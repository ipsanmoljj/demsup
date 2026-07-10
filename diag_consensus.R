setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
source("R/factor_loader_consensus.R")

cons <- load_eia_consensus(
  start      = "2021-01-01",
  end        = Sys.Date(),
  output_dir = "output",
  save       = TRUE,
  verbose    = TRUE
)

cat("\n=== First 10 rows ===\n"); print(head(cons, 10))
cat("\n=== Last 10 rows ===\n");  print(tail(cons, 10))
cat("\nTotal rows:", nrow(cons), "\n")
cat("Date range:", format(min(cons$date)), "to", format(max(cons$date)), "\n")
cat("\nSurprise distribution (million barrels):\n")
cat("  Mean:", round(mean(cons$surprise_mbbls, na.rm=TRUE), 3), "\n")
cat("  SD  :", round(sd(cons$surprise_mbbls, na.rm=TRUE), 3), "\n")
cat("  Min :", round(min(cons$surprise_mbbls, na.rm=TRUE), 3), "\n")
cat("  Max :", round(max(cons$surprise_mbbls, na.rm=TRUE), 3), "\n")

cat("\n=== Most recent 5 EIA releases ===\n")
print(tail(cons[, .(date, actual_mbbls, forecast_mbbls, surprise_mbbls)], 5))
