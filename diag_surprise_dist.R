setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
library(data.table)
cons <- fread("output/eia_consensus.csv")
cons[, date := as.Date(date)]
x <- cons$surprise_kb

cat("Full distribution (", nrow(cons), "events):\n")
cat("  Mean:", round(mean(x),1), "kb  SD:", round(sd(x),1), "kb\n")
cat("  Median:", round(median(x),1), "kb  MAD:", round(mad(x),1), "kb\n")
cat("  Quantiles:", paste(round(quantile(x, c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)),0), collapse=" | "), "\n")
cat("  Outliers (|surprise| > 10000 kb):\n")
big <- cons[abs(surprise_kb) > 10000][order(-abs(surprise_kb))]
for (i in seq_len(nrow(big)))
  cat(sprintf("    %s  actual=%+.1f M  forecast=%+.1f M  surprise=%+.0f kb\n",
              big$date[i], big$actual_mbbls[i], big$forecast_mbbls[i], big$surprise_kb[i]))

cat("\nJune 24 surprise = -2188 kb\n")
cat(strrep("-", 60), "\n")

# Approach 1: winsorized (clip at 5th/95th percentile)
q5 <- quantile(x, 0.05); q95 <- quantile(x, 0.95)
xw <- pmax(pmin(x, q95), q5)
cat(sprintf("Winsorized [%d, %d] kb:\n", round(q5), round(q95)))
cat(sprintf("  Mean=%+.0f  SD=%.0f  surprise_z=%+.3f\n",
            mean(xw), sd(xw), (-2188 - mean(xw))/sd(xw)))

# Approach 2: trim storm outliers (|surprise| <= 10000 kb)
xt <- x[abs(x) <= 10000]
cat(sprintf("Trimmed (|surprise|<=10000): N=%d\n", length(xt)))
cat(sprintf("  Mean=%+.0f  SD=%.0f  surprise_z=%+.3f\n",
            mean(xt), sd(xt), (-2188 - mean(xt))/sd(xt)))

# Approach 3: robust z-score (median/MAD)
med <- median(x); madv <- mad(x)
cat(sprintf("Robust (median/MAD):\n"))
cat(sprintf("  Median=%+.0f  MAD=%.0f  surprise_z=%+.3f\n",
            med, madv, (-2188 - med)/madv))

# Approach 4: trailing 2-year window only
recent <- cons[date >= as.Date("2024-06-24") & date < as.Date("2026-06-24"), surprise_kb]
cat(sprintf("2-year trailing window: N=%d\n", length(recent)))
cat(sprintf("  Mean=%+.0f  SD=%.0f  surprise_z=%+.3f\n",
            mean(recent), sd(recent), (-2188 - mean(recent))/sd(recent)))

cat("\nSummary: Which approach is most appropriate?\n")
cat("  Full dist    : z=", round((-2188-mean(x))/sd(x), 3), " (inflated by 2021 outliers)\n")
cat("  Winsorized   : z=", round((-2188-mean(xw))/sd(xw), 3), "\n")
cat("  Trimmed      : z=", round((-2188-mean(xt))/sd(xt), 3), "\n")
cat("  Robust(MAD)  : z=", round((-2188-med)/madv, 3), "\n")
cat("  Trailing 2yr : z=", round((-2188-mean(recent))/sd(recent), 3), "\n")
cat("\nConclusion: with storm outliers removed, June 24 surprise is moderately bearish\n")
cat("  (less inventory draw than typical consensus beat → mildly bearish surprise)\n")
