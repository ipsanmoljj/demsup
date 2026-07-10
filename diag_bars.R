install.packages(c("DBI","RSQLite"), repos="https://cloud.r-project.org", quiet=TRUE)
library(DBI); library(RSQLite); library(data.table); library(lubridate)

db_path <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup/backtesting/extra/bars_1min_20260624.db"
con <- dbConnect(SQLite(), db_path)
cat("Tables:\n"); cat(paste(dbListTables(con), collapse="\n"), "\n\n")
tbls <- dbListTables(con)
for (tbl in head(tbls, 10)) {
  cat("--- Table:", tbl, "---\n")
  cat("Cols:", paste(dbListFields(con, tbl), collapse=", "), "\n")
  d <- dbGetQuery(con, sprintf("SELECT * FROM \"%s\" LIMIT 3", tbl))
  print(d); cat("\n")
}
dbDisconnect(con)
