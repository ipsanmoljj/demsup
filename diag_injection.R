setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
library(data.table)
source("R/spread_factor_model.R")

cons <- fread("output/eia_consensus.csv"); cons[, date := as.Date(date)]

cat("=== 1. Factor file - crude_stocks_surprise before injection ===\n")
factors <- .sfm_load_factors(getwd())
weds <- factors[weekdays(date) == "Wednesday" & !is.na(crude_stocks_chg)]
cat("  Wednesday rows with crude_stocks_chg:", nrow(weds), "\n")
if ("crude_stocks_surprise" %in% names(weds)) {
  cat("  crude_stocks_surprise exists in factor file\n")
  x <- as.numeric(weds$crude_stocks_surprise)
  cat("  Range:", round(range(x, na.rm=T)), "  Mean:", round(mean(x, na.rm=T)), "  NA:", sum(is.na(x)), "\n")
  cat("  First 5 values:", head(round(x), 5), "\n")
} else {
  cat("  crude_stocks_surprise NOT in factor file\n")
}

cat("\n=== 2. After injection ===\n")
factors <- merge(factors,
                 cons[, .(date, cons_surp_kb = surprise_kb)],
                 by = "date", all.x = TRUE)
n_inj <- sum(!is.na(factors$cons_surp_kb))
factors[!is.na(cons_surp_kb), crude_stocks_surprise := cons_surp_kb]
factors[, cons_surp_kb := NULL]
cat("  Injected rows:", n_inj, "\n")

weds2 <- factors[weekdays(date) == "Wednesday" & !is.na(crude_stocks_chg)]
if ("crude_stocks_surprise" %in% names(weds2)) {
  x2 <- as.numeric(weds2$crude_stocks_surprise)
  cat("  crude_stocks_surprise after injection:\n")
  cat("  Range:", round(range(x2, na.rm=T)), "  Mean:", round(mean(x2, na.rm=T)), "  NA:", sum(is.na(x2)), "\n")
  cat("  First 5 values:", head(round(x2), 5), "\n")
  cat("  Consensus CSV mean:", round(mean(cons$surprise_kb)), "  (should match above)\n")
}

cat("\n=== 3. Check specific dates match ===\n")
chk <- weds2[date %in% as.Date(c("2026-06-17","2026-06-03","2025-12-31","2021-01-06"))]
merge_chk <- merge(chk[, .(date, crude_stocks_surprise)],
                   cons[, .(date, cons_surprise_kb=surprise_kb)],
                   by="date", all.x=TRUE)
print(merge_chk)
