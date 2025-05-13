library(DBI)
library(RSQLite)
library(data.table)
library(tokenizers)
library(textstem)
library(parallel)
library(quanteda)
library(textclean)


source("shared.R")

# Configuration
n_samples <- 100000  # Test with 1000; set to 500000 for full run
chunk_size <- 1000
en_nlp_db_path <- NLP_DB_PATH
ngrams_db_path <- "/ngrams.db"
log_file <- ERROR_LOG_PATH

num_cores <- detectCores() - 1

# Generate ngrams
generate_ngrams_chunk <- function(tokens, n) {
    if (length(tokens) < n) return(NULL)
    ngrams <- data.table()
    for (i in 1:(length(tokens) - n + 1)) {
        ngram <- tokens[i:(i + n - 1)]
        if (any(is.na(ngram))) next
        if (n == 2) {
            w1 <- ngram[1]
            w2 <- ngram[2]
            if (w1 %in% stop_words || (exclude_stopwords_in_targets && w2 %in% stop_words)) next
            ngrams <- rbindlist(list(ngrams, data.table(w1 = w1, w2 = w2, frequency = 1)))
        } else if (n == 3) {
            w1 <- ngram[1]
            w2 <- ngram[2]
            w3 <- ngram[3]
            if (exclude_stopwords_in_targets && w3 %in% stop_words) next
            ngrams <- rbindlist(list(ngrams, data.table(w1 = w1, w2 = w2, w3 = w3, frequency = 1)))
        } else if (n == 4) {
            w1 <- ngram[1]
            w2 <- ngram[2]
            w3 <- ngram[3]
            w4 <- ngram[4]
            if (exclude_stopwords_in_targets && w4 %in% stop_words) next
            ngrams <- rbindlist(list(ngrams, data.table(w1 = w1, w2 = w2, w3 = w3, w4 = w4, frequency = 1)))
        } else if (n == 5) {
            w1 <- ngram[1]
            w2 <- ngram[2]
            w3 <- ngram[3]
            w4 <- ngram[4]
            w5 <- ngram[5]
            if (exclude_stopwords_in_targets && w5 %in% stop_words) next
            ngrams <- rbindlist(list(ngrams, data.table(w1 = w1, w2 = w2, w3 = w3, w4 = w4, w5 = w5, frequency = 1)))
        }
    }
    ngrams
}

apply_kneser_ney <- function(ngrams, n, discount = 0.75) {
    if (nrow(ngrams) == 0) return(data.table())
    
    # Calculate the total frequency for normalization
    total_freq <- sum(ngrams$frequency)
    
    # Apply Kneser-Ney smoothing for the n-gram frequencies
    ngrams[, probability := pmax(frequency - discount, 0) / total_freq]
    
    # Calculate continuation count (words that appear in different contexts)
    group_cols <- paste0("w", 1:n)
    continuation_counts <- ngrams[, .(cont_count = .N), by = group_cols]
    
    # Merge continuation counts with ngrams
    ngrams <- merge(ngrams, continuation_counts, by = group_cols, all.x = TRUE)
    ngrams[is.na(cont_count), cont_count := 0]
    
    # Update the probability with the continuation probability
    ngrams[, probability := probability + (discount * cont_count / total_freq)]
    
    # Normalize probabilities to ensure they sum to 1
    total_prob <- sum(ngrams$probability)
    if (total_prob > 0) {
        ngrams[, probability := probability / total_prob]
    }
    
    # Remove the continuation count column
    ngrams[, cont_count := NULL]
    
    return(ngrams)
}

# Main function
generate_ngrams <- function(n_samples, chunk_size) {
    tryCatch({
        log_message(sprintf("Starting n-gram generation: n_samples=%d, chunk_size=%d", n_samples, chunk_size))
        
        # Connect to databases
        en_con <- dbConnect(SQLite(), en_nlp_db_path)
        ng_con <- dbConnect(SQLite(), ngrams_db_path)
        on.exit({
            if (dbIsValid(en_con)) dbDisconnect(en_con)
            if (dbIsValid(ng_con)) dbDisconnect(ng_con)
            log_message("Cleaned up connections")
        }, add = TRUE)
        
        # Check sentences table
        sentence_count <- dbGetQuery(en_con, "SELECT COUNT(*) AS count FROM sentences")$count
        if (sentence_count == 0) stop("No sentences found in en_nlp.db")
        
        # Create tables
        for (table in c("bigrams_kn", "trigrams_kn", "fourgrams_kn", "fivegrams_kn")) {
            dbExecute(ng_con, sprintf("DROP TABLE IF EXISTS %s", table))
        }
        dbExecute(ng_con, "CREATE TABLE bigrams_kn (w1 TEXT, w2 TEXT, frequency INTEGER, probability REAL, PRIMARY KEY (w1, w2))")
        dbExecute(ng_con, "CREATE TABLE trigrams_kn (w1 TEXT, w2 TEXT, w3 TEXT, frequency INTEGER, probability REAL, PRIMARY KEY (w1, w2, w3))")
        dbExecute(ng_con, "CREATE TABLE fourgrams_kn (w1 TEXT, w2 TEXT, w3 TEXT, w4 TEXT, frequency INTEGER, probability REAL, PRIMARY KEY (w1, w2, w3, w4))")
        dbExecute(ng_con, "CREATE TABLE fivegrams_kn (w1 TEXT, w2 TEXT, w3 TEXT, w4 TEXT, w5 TEXT, frequency INTEGER, probability REAL, PRIMARY KEY (w1, w2, w3, w4, w5))")
        
        # Fetch sentences
        file_counts <- dbGetQuery(en_con, "
      SELECT file_id, sentence_count AS count 
      FROM files 
      WHERE sentence_count IS NOT NULL AND sentence_count > 0")
        if (nrow(file_counts) == 0) {
            file_counts <- dbGetQuery(en_con, "
        SELECT file_id, COUNT(*) AS count 
        FROM sentences 
        GROUP BY file_id")
        }
        if (nrow(file_counts) == 0) stop("No files with valid sentences")
        samples_per_file <- ceiling(n_samples / nrow(file_counts))
        sentence_batches <- list()
        
        for (i in 1:nrow(file_counts)) {
            file_id <- file_counts$file_id[i]
            file_total <- file_counts$count[i]
            file_samples <- min(samples_per_file, file_total)
            query <- sprintf("SELECT text_content FROM sentences WHERE file_id = %d ORDER BY RANDOM() LIMIT %d", file_id, file_samples)
            batch_data <- dbGetQuery(en_con, query)
            sentence_batches[[i]] <- batch_data$text_content
            log_message(sprintf("feched %d from file_id:%d", file_samples, file_id))
        }
        sentences <- unlist(sentence_batches)
        if (length(sentences) == 0) stop("No sentences retrieved")
        
        # Process chunks in parallel
        all_ngrams <- list()
        for (n in 2:5) all_ngrams[[n-1]] <- data.table()
        chunks <- split(sentences, ceiling(seq_along(sentences) / chunk_size))
        for (n in 2:5) {
            log_message(sprintf("Processing %d-grams", n))
            ngrams_list <- mclapply(chunks, function(chunk) {
                tokens_list <- lapply(chunk, normalize_input)
                tokens_list <- tokens_list[!sapply(tokens_list, is.null)]
                rbindlist(lapply(tokens_list, function(tokens) generate_ngrams_chunk(tokens, n)))
            }, mc.cores = num_cores)
            all_ngrams[[n-1]] <- rbindlist(ngrams_list)
            log_message(sprintf("Generated %d %sgrams", nrow(all_ngrams[[n-1]]), c("bi", "tri", "four", "five")[n-1]))
        }
        
        # Aggregate and smooth
        for (n in 2:5) {
            ngrams <- all_ngrams[[n-1]]
            if (nrow(ngrams) > 0) {
                group_cols <- paste0("w", 1:n)
                ngrams <- ngrams[, .(frequency = sum(frequency)), by = group_cols]
                ngrams <- apply_kneser_ney(ngrams, n)
                table_name <- switch(n - 1, "bigrams_kn", "trigrams_kn", "fourgrams_kn", "fivegrams_kn")
                dbWriteTable(ng_con, table_name, ngrams, overwrite = TRUE, row.names = FALSE)
                log_message(sprintf("Wrote %d %sgrams to %s", nrow(ngrams), c("bi", "tri", "four", "five")[n-1], table_name))
            }
        }
        log_message("Completed n-gram generation")
    }, error = function(e) {
        log_error(sprintf("Failed: %s", conditionMessage(e)))
        stop(e)
    })
}

# Run
generate_ngrams(n_samples, chunk_size)