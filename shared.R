library(tokenizers)
library(stringr)
library(textstem)
library(stringi)
library(textclean) # For replace_contraction
library(DBI)
library(RSQLite)
library(stopwords)

# Constants
NGRAM_DB_PATH <- "ngrams.db"
#NLP_DB_PATH <- "en_nlp.db"
ERROR_LOG_PATH <- "errors.log"

# Logging
log_message <- function(message) {
    cat(sprintf("[%s] %s\n", Sys.time(), message))
    #cat(sprintf("[%s] %s\n", Sys.time(), message), file = log_file, append = TRUE)
}

log_error <- function(message) {
    cat(sprintf("[%s] ERROR: %s\n", Sys.time(), message), file = ERROR_LOG_PATH, append = TRUE)
}

# Database connection
connect_db <- function() {
    tryCatch({
        con <- dbConnect(SQLite(), DB_PATH)
        cat(sprintf("[DEBUG] Connected to database: %s\n", DB_PATH))
        con
    }, error = function(e) {
        log_error(sprintf("Database connection failed: %s", e$message))
        stop(e)
    })
}

# Stopwords
stop_words <- stopwords("en") %>%
    textclean::replace_contraction() %>%
    lemmatize_strings()

exclude_stopwords_in_targets <- TRUE



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