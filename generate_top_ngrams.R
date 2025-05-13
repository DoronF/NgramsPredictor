library(DBI)
library(RSQLite)

# Path to your SQLite database
con <- dbConnect(SQLite(), "ngrams.db")

# Load top 10 n-grams from each level
top_bigrams     <- dbGetQuery(con, "SELECT * FROM bigrams_kn ORDER BY probability DESC LIMIT 10")
top_trigrams    <- dbGetQuery(con, "SELECT * FROM trigrams_kn ORDER BY probability DESC LIMIT 10")
top_fourgrams   <- dbGetQuery(con, "SELECT * FROM fourgrams_kn ORDER BY probability DESC LIMIT 10")
top_fivegrams   <- dbGetQuery(con, "SELECT * FROM fivegrams_kn ORDER BY probability DESC LIMIT 10")

# Count total rows (i.e., total n-grams at each level)
count_bigrams   <- dbGetQuery(con, "SELECT COUNT(*) AS count FROM bigrams_kn")$count
count_trigrams  <- dbGetQuery(con, "SELECT COUNT(*) AS count FROM trigrams_kn")$count
count_fourgrams <- dbGetQuery(con, "SELECT COUNT(*) AS count FROM fourgrams_kn")$count
count_fivegrams <- dbGetQuery(con, "SELECT COUNT(*) AS count FROM fivegrams_kn")$count

dbDisconnect(con)

# Save all to RData
save(
    top_bigrams, top_trigrams, top_fourgrams, top_fivegrams,
    count_bigrams, count_trigrams, count_fourgrams, count_fivegrams,
    file = "top_ngrams.RData"
)
