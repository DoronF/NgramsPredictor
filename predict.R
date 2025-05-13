library(tokenizers)
library(stringr)
library(textstem)
library(stringi)
library(textclean) # For replace_contraction
library(DBI)
library(RSQLite)
library(stopwords)
library(dplyr)
library(tidyr)

# Constants
NGRAM_DB_PATH <- "ngrams.db"

# Stopwords
stop_words <- stopwords("en") %>%
    textclean::replace_contraction() %>%
    lemmatize_strings()

normalize_input <- function(text) {
    if (is.null(text) || is.na(text)) {
        #warning("Invalid or empty input")
        return(NULL)
    }
    text <- trimws(tolower(text))
    # remove hash tags 
    text <- str_remove_all(text, "\\S*_\\S*") #underscore
    text <- str_remove_all(text, "#[a-z0-9_]+") # hashtag
    text <- str_remove_all(text, "\\$[a-z0-9_]+") # $dollar sign
    text <- str_remove_all(text, "@[a-z0-9_]+")  # @ tag
    text <- str_remove_all(text, "\\S*\\d\\S*") # words with numbers
    # 1. Normalize Unicode form
    text <- stri_trans_nfkc(text)
    
    # 2. Remove URLs (before tokenization)
    text <- str_remove_all(text, "(https?://[^\\s]+)")
    
    # 4. Tokenize using tokenizers::tokenize_words
    tokens <- tokenize_words(text)[[1]]
    
    normalized_tokens <- character(0)
    for (token in tokens) {
        
        processed_token <- trimws(token) # Add trimws here
        
        # Remove emojis
        processed_token <- stri_replace_all_regex(processed_token, "\\p{Emoji}", "")
        
        # Normalize foreign characters to ASCII
        processed_token <- iconv(processed_token, "UTF-8", "ASCII//TRANSLIT", sub = "")
        
        processed_token <- str_replace_all(processed_token, "\\?", "")
        processed_token <- processed_token %>%
            textclean::replace_contraction() %>%
            lemmatize_strings()
        # Remove all punctuation
        processed_token <- str_remove_all(processed_token, "[[:punct:]]")
        # print(processed_token)
        # Keep only if it consists entirely of lowercase letters and is not empty
        if (nchar(processed_token) > 0) {
            
            normalized_tokens <- c(normalized_tokens, trimws(processed_token))
        }
    }
    
    if (length(normalized_tokens) == 0) {
        return(NULL)
    } else {
        return(normalized_tokens)
    }
}

# Interpolation weights (must sum to 1)
lambda <- c("5" = 0.5, "4" = 0.3, "3" = 0.15, "2" = 0.05)

# Main prediction function
predict_next_word <- function(input_text, n_top = 5) {
    con <- dbConnect(SQLite(), NGRAM_DB_PATH)
    on.exit(dbDisconnect(con), add = TRUE)
    
    tokens <- normalize_input(input_text)
    if (is.null(tokens) || length(tokens) == 0) {
        return(data.frame(word = character(0), probability = numeric(0)))
    }
    
    predictions <- data.frame(word = character(0), 
                              probability = numeric(0),
                              ngram_source = character(0),
                              stringsAsFactors = FALSE)
    seen_words <- character(0)  # For deduplication across all levels
    
    # Loop through n-grams in order (5-gram to bigram)
    for (n in 5:2) {
        if (length(tokens) < n - 1) next
        context <- tail(tokens, n - 1)
        table <- switch(n,
                        "5" = "fivegrams_kn",
                        "4" = "fourgrams_kn",
                        "3" = "trigrams_kn",
                        "2" = "bigrams_kn")
        where_clause <- paste(sprintf("w%d = '%s'", 1:(n - 1), context), collapse = " AND ")
        predicted_col <- paste0("w", n)
        
        query <- sprintf("
            SELECT %s AS word, probability, '%s' AS ngram_source 
            FROM %s 
            WHERE %s 
            ORDER BY probability DESC 
            LIMIT %d
        ", predicted_col, paste0(n, "-gram"), table, where_clause, n_top * 5)  # Overfetch for deduplication
        
        results <- tryCatch(dbGetQuery(con, query), error = function(e) {
            return(NULL)
        })
        
        if (!is.null(results) && nrow(results) > 0) {
            results <- results[!results$word %in% stop_words & results$probability > 1e-6, ]
            results <- results[!results$word %in% seen_words, ]
            
            if (nrow(results) > 0) {
                seen_words <- c(seen_words, results$word)
                predictions <- rbind(predictions, results)
            }
        }
    }
    
    # Fallback: top bigrams if still under quota
    if (nrow(predictions) < n_top) {
        fallback_query <- sprintf("
            SELECT w2 AS word, probability , 'fallback' AS ngram_source
            FROM bigrams_kn 
            ORDER BY probability DESC 
            LIMIT %d
        ", n_top - nrow(predictions))  # Only fetch what's needed
        
        fallback_results <- tryCatch(dbGetQuery(con, fallback_query), error = function(e) {
            return(data.frame(word = character(0), probability = numeric(0),ngram_source = character(0) ))
        })
        
        fallback_results <- fallback_results[!fallback_results$word %in% stop_words & fallback_results$probability > 1e-6, ]
        fallback_results <- fallback_results[!fallback_results$word %in% seen_words, ]
        
        if (nrow(fallback_results) > 0) {
            predictions <- rbind(predictions, fallback_results)
        }
    }
    ngram_labels <- c("2-gram" = "bigram", "3-gram" = "trigram", 
                      "4-gram" = "fourgram", "5-gram" = "fivegram", "fallback" = "fallback (bigram)")
    predictions$ngram_source <- ngram_labels[predictions$ngram_source]
    # Final deduplication (extra safety)
    predictions <- predictions[!duplicated(predictions$word), ]
    
    # Ensure output size is exactly n_top
    if (nrow(predictions) > n_top) {
        predictions <- predictions[1:n_top, ]
    }
    
    return(predictions)
}

calculate_perplexity <- function(input_text) {
    con <- dbConnect(SQLite(), NGRAM_DB_PATH)
    on.exit(dbDisconnect(con), add = TRUE)
    
    tokens <- normalize_input(input_text)
    if (is.null(tokens) || length(tokens) == 0) {
        return(NA)
    }
    
    total_log_prob <- 0  # To accumulate the log probabilities
    total_tokens <- length(tokens)
    
    # If there's only one token, handle it separately
    if (total_tokens == 1) {
        query <- sprintf("
            SELECT probability
            FROM bigrams_kn 
            WHERE w1 = '%s'
            ORDER BY probability DESC 
            LIMIT 1
        ", tokens[1])  # Get the probability for this single word
        
        result <- tryCatch(dbGetQuery(con, query), error = function(e) {
            return(NULL)
        })
        
        if (!is.null(result) && nrow(result) > 0) {
            prob <- result$probability[1]
            if (prob > 0) {
                total_log_prob <- log(prob)  # Directly use the log probability
            } else {
                total_log_prob <- log(1e-6)  # Handle zero probability
            }
        } else {
            total_log_prob <- log(1e-6)  # Handle missing n-gram match
        }
        
    } else {
        # Loop through n-grams (5-gram to bigram) for multiple tokens
        for (n in 5:2) {
            if (length(tokens) < n - 1) next
            context <- tail(tokens, n - 1)
            table <- switch(n,
                            "5" = "fivegrams_kn",
                            "4" = "fourgrams_kn",
                            "3" = "trigrams_kn",
                            "2" = "bigrams_kn")
            where_clause <- paste(sprintf("w%d = '%s'", 1:(n - 1), context), collapse = " AND ")
            predicted_col <- paste0("w", n)
            
            query <- sprintf("
                SELECT probability 
                FROM %s 
                WHERE %s 
                ORDER BY probability DESC 
                LIMIT 1
            ", table, where_clause)  # Get the top prediction
            
            result <- tryCatch(dbGetQuery(con, query), error = function(e) {
                return(NULL)
            })
            
            if (!is.null(result) && nrow(result) > 0) {
                prob <- result$probability[1]
                if (prob > 0) {
                    total_log_prob <- total_log_prob + log(prob)  # Accumulate log probabilities
                } else {
                    total_log_prob <- total_log_prob + log(1e-6)  # Handle zero probability
                }
            } else {
                total_log_prob <- total_log_prob + log(1e-6)  # Handle missing n-gram match
            }
        }
        
    }
    
    # Compute perplexity: exp(-1/N * log(P1 * P2 * ... * PN))
    perplexity <- exp(-total_log_prob / total_tokens)
    return(perplexity)
}

