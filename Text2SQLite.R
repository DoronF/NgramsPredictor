library(RSQLite)
library(readr)
library(tokenizers)

create_db <- function(lang) {
    con <- dbConnect(SQLite(), paste0(lang, "_nlp.db"))
    
    # Files table
    dbExecute(con, "
    CREATE TABLE files (
      file_id INTEGER PRIMARY KEY AUTOINCREMENT,
      file_name TEXT,
      file_size_mb REAL
    )")
    
    # Sentences table
    dbExecute(con, "
    CREATE TABLE sentences (
      sentence_id INTEGER PRIMARY KEY AUTOINCREMENT,
      file_id INTEGER,
      position INTEGER,
      text_content TEXT,
      word_count INTEGER,
      FOREIGN KEY (file_id) REFERENCES files(file_id)
    )")
    
    # Index for performance
    dbExecute(con, "CREATE INDEX idx_file_id ON sentences (file_id)")
    
    dbDisconnect(con)
}

languages <- c("en", "fi", "ru", "de")
lapply(languages, create_db)

files <- list.files("final", pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)


for (file_path in files) {
    # Extract language
    file_name <- basename(file_path)
    lang <- substr(file_name, 1, 2)  # e.g., "en" from "en_1.txt"
    file_size_mb <- file.size(file_path) / 1024^2
    
    # Connect to language-specific DB
    con <- dbConnect(SQLite(), paste0(lang, "_nlp.db"))
    dbBegin(con)  # Start transaction for speed
    
    # Insert file metadata
    dbExecute(con, "INSERT INTO files (file_name, file_size_mb) VALUES (?, ?)",
              params = list(file_name, file_size_mb))
    file_id <- dbGetQuery(con, "SELECT last_insert_rowid()")[[1]]
    
    # Read and tokenize in chunks
    chunk_size <- 1e6  # 1 MB chunks
    con_file <- file(file_path, "r")
    position <- 1
    
    while (length(lines <- readLines(con_file, n = chunk_size, warn = FALSE)) > 0) {
        text <- paste(lines, collapse = " ")
        sentences <- tokenize_sentences(text)[[1]]
        if (length(sentences) > 0) {
            data <- data.frame(
                file_id = file_id,
                position = position:(position + length(sentences) - 1),
                text_content = sentences,
                word_count = lengths(tokenize_words(sentences))
            )
            dbWriteTable(con, "sentences", data, append = TRUE)
            position <- position + length(sentences)
        }
    }
    close(con_file)
    dbCommit(con)  # Commit transaction
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_word_count ON sentences(word_count)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_file_id ON sentences(file_id)")
    
    dbDisconnect(con)
    print(paste("Processed:", file_name, "into", paste0(lang, "_nlp.db")))
}
