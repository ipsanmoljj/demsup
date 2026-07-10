library(data.table)
setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")

ms <- readRDS("output/sfm_models.rds")
keys <- ls(ms)
cat("Total models:", length(keys), "\n")
cat(paste(head(keys, 30), collapse="\n"), "\n")

cat("\n--- First model object structure ---\n")
m1 <- get(keys[1], envir=ms)
cat("type:", m1$type, "\n")
cat("xcols:", paste(head(m1$xcols, 10), collapse=", "), "\n")

cat("\n--- Training stats check ---\n")
fac <- fread("output/factors_extended.csv")
fac[, date := as.Date(date)]
fac <- fac[order(date)]
cat("crude_stocks cols:", paste(grep("crude_stock", names(fac), value=TRUE), collapse=", "), "\n")
tr <- fac[weekdays(date)=="Wednesday" & date <= as.Date("2024-10-09")]
cat("Training Wednesday rows:", nrow(tr), "\n")
if ("crude_stocks_chg" %in% names(tr)) {
  cat("crude_stocks_chg sample:", paste(round(tail(tr$crude_stocks_chg, 5), 1), collapse=", "), "\n")
  cat("crude_stocks_chg mean:", round(mean(tr$crude_stocks_chg, na.rm=TRUE), 2), "\n")
  cat("crude_stocks_chg sd  :", round(sd(tr$crude_stocks_chg, na.rm=TRUE), 2), "\n")
} else {
  cat("crude_stocks_chg NOT FOUND in factors_extended\n")
  cat("All cols:\n")
  cat(paste(names(fac), collapse=", "), "\n")
}
