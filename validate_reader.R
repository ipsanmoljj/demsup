# validate_reader.R
# ------------------
# Run this to confirm the R reader works against your actual CSV files.
#
# Usage (in Codespace terminal):
#   Rscript validate_reader.R /path/to/CL_outrights_1min_t.csv

library(data.table)
library(lubridate)
library(zoo)

source("R/futures_reader.R")

args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args) > 0) args[1] else stop("Usage: Rscript validate_reader.R <path_to_csv>")

cat("\n", strrep("=", 60), "\n")
cat("STEP 1: Raw reader\n")
cat(strrep("=", 60), "\n")

ff <- read_futures_csv(path)
cat("\nShape      :", nrow(ff), "rows x", ncol(ff), "cols\n")
cat("Date range :", format(min(ff$timestamp)), "->", format(max(ff$timestamp)), "\n")
cat("First 3 col names:", paste(colnames(ff)[1:4], collapse=", "), "...\n")

cat("\n", strrep("=", 60), "\n")
cat("STEP 2: FuturesFile summary\n")
cat(strrep("=", 60), "\n\n")
print(ff)

cat("\n", strrep("=", 60), "\n")
cat("STEP 3: Price extraction (hourly)\n")
cat(strrep("=", 60), "\n")

prices <- get_prices(ff, resample_to = "1 hour")
cat("\nShape    :", nrow(prices), "rows x", ncol(prices), "cols\n")
cat("Contracts:", paste(setdiff(colnames(prices), "timestamp"), collapse=", "), "\n")
cat("\nLast 3 rows:\n")
print(tail(prices[, 1:7], 3))

cat("\n", strrep("=", 60), "\n")
cat("STEP 4: Time spreads\n")
cat(strrep("=", 60), "\n")

spds <- get_spreads(ff, resample_to = "1 hour")
cat("\nSpreads computed:", paste(setdiff(colnames(spds), "timestamp"), collapse=", "), "\n")
cat("\nLast 5 rows:\n")
print(tail(spds, 5))

cat("\n", strrep("=", 60), "\n")
cat("STEP 5: Curve metrics\n")
cat(strrep("=", 60), "\n")

curve <- get_curve_metrics(ff, resample_to = "1 hour")
cat("\nMetrics:", paste(setdiff(colnames(curve), "timestamp"), collapse=", "), "\n")
cat("\nLast 5 rows:\n")
print(tail(curve, 5))

cat("\n", strrep("=", 60), "\n")
cat("STEP 6: Spread z-scores (63-bar rolling window)\n")
cat(strrep("=", 60), "\n")

# Rolling z-score for M1M2
window <- 63
m1m2   <- spds$M1M2
roll_mean <- zoo::rollmean(m1m2, window, fill = NA, align = "right")
roll_sd   <- zoo::rollapply(m1m2, window, sd, fill = NA, align = "right")
z_score   <- (m1m2 - roll_mean) / roll_sd
pct_rank  <- rank(m1m2, na.last = "keep") / sum(!is.na(m1m2))

latest <- list(
  value    = round(tail(m1m2[!is.na(m1m2)], 1), 4),
  z_score  = round(tail(z_score[!is.na(z_score)], 1), 3),
  pct_rank = round(tail(pct_rank[!is.na(pct_rank)], 1), 3)
)

cat("\nM1M2 current snapshot:\n")
cat("  Value    :", latest$value, "\n")
cat("  Z-score  :", latest$z_score, "\n")
cat("  Pct rank :", scales::percent(latest$pct_rank, accuracy=1)
    |> tryCatch(error = function(e) paste0(round(latest$pct_rank*100), "%")), "\n")

# Curve regime signal
last_contango <- tail(curve$contango[!is.na(curve$contango)], 1)
last_slope    <- tail(curve$slope[!is.na(curve$slope)], 1)
regime_signal <- if (last_contango == 1) "CONTANGO" else "BACKWARDATION"

cat("\nCurve regime signal :", regime_signal, "\n")
cat("Current slope (M1-M6):", round(last_slope, 3), "\n")

cat("\n", strrep("=", 60), "\n")
cat("All steps complete. R reader working correctly.\n")
cat(strrep("=", 60), "\n\n")