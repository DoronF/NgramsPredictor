library(DBI)
library(RSQLite)

# Connect to the SQLite database
db_path <- "en_nlp.db"  # Replace with your actual path
con <- dbConnect(SQLite(), dbname = db_path, synchronous = NULL)

cat("Connected to database:", db_path, "\n")
cat("Starting safe deletion of out-of-range sentences...\n")

start_time <- Sys.time()

# Wrap in transaction
dbExecute(con, "BEGIN TRANSACTION")

# Perform deletion
rows_deleted <- dbExecute(con, "
  DELETE FROM sentences 
  WHERE word_count < 5 OR word_count > 50
")

# Commit transaction
dbExecute(con, "COMMIT")

end_time <- Sys.time()
duration <- round(difftime(end_time, start_time, units = "secs"), 2)

cat("Deleted", rows_deleted, "rows in", duration, "seconds.\n")

# Optional: Reclaim disk space
cat("Running VACUUM to optimize database size...\n")
dbExecute(con, "VACUUM")
cat("VACUUM complete.\n")

# Disconnect safely
dbDisconnect(con)
cat("Disconnected from database.\n")
