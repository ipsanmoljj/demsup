# R/build_lco_curve.R
# Builds lco_curve_daily.csv from LCO_final.csv (1-min bar data)
#
# Logic: the first contract in each row is treated as M1 (front month),
# subsequent contracts as M2..M12. For each trading day, take the last
# 1-min bar as the daily close. Compute spreads same as cl_curve_daily.csv.
#
# m2_fly = m1 - 2*m2 + m3  (butterfly M2)
# m3_fly = m2 - 2*m3 + m5  (butterfly M3, skips m4 — consistent with CL)

suppressPackageStartupMessages({ library(data.table); library(lubridate) })

ROOT <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"
SRC  <- file.path(ROOT, "strategy_live/final data/tent_data/LCO_final.csv")
OUT  <- file.path(ROOT, "strategy_live/final data/tent_data/lco_curve_daily.csv")

cat("Reading LCO_final.csv (2.37M rows) — may take ~30 seconds...\n")
t0 <- proc.time()

# Read raw (skipping the #meta comment header)
raw <- fread(SRC, header = FALSE, skip = 1, fill = TRUE,
             colClasses = "character", showProgress = FALSE)
cat(sprintf("Read %d rows in %.1f s\n", nrow(raw), (proc.time()-t0)[3]))

# V1 = timestamp; V2,V4,V6... = contract names; V3,V5,V7... = prices
# Extract timestamp and the first 13 contract-price pairs (M1..M13 → use M1-M12)
ts_col <- 1
c_cols  <- seq(2, min(ncol(raw), 26), by = 2)  # col 2,4,6,...,26 → up to 13 contracts
p_cols  <- seq(3, min(ncol(raw), 27), by = 2)  # col 3,5,7,...,27

cat(sprintf("Using up to %d contract columns\n", length(c_cols)))

# Parse date from timestamp (format: 2021-01-04T01:00:00Z or 2021-01-04 01:00:00+00:00)
raw[, date := as.Date(substr(get(paste0("V",ts_col)), 1, 10))]

# For each date, keep only the last row (daily close)
setorder(raw, date, V1)
close_rows <- raw[, .SD[.N], by = date]

cat(sprintf("Unique trading dates: %d (%s → %s)\n",
            nrow(close_rows), min(close_rows$date), max(close_rows$date)))

# Extract M1..M12 prices
n_cont <- min(length(c_cols), 12)
result <- data.table(date = close_rows$date)

for (i in seq_len(n_cont)) {
  pc  <- paste0("V", p_cols[i])
  col <- paste0("m", i)
  result[, (col) := suppressWarnings(as.numeric(close_rows[[pc]]))]
}

# Drop rows where m1 is missing
result <- result[!is.na(m1)]

# ── Compute spreads ───────────────────────────────────────────────────────────
result[, m1m2  := m1 - m2]
result[, m1m3  := m1 - m3]
result[, m1m6  := m1 - m6]
result[, m1m12 := m1 - m12]
result[, m2m3  := m2 - m3]
result[, m3m6  := m3 - m6]
result[, m6m12 := m6 - m12]

# Butterflies (same formula as CL): skip m4 for m3_fly
result[, m2_fly := m1 - 2*m2 + m3]
result[, m3_fly := m2 - 2*m3 + m5]   # skips m4 — consistent with CL methodology
result[, m6_fly := m5 - 2*m6 + m7]
result[, m_condor := m2_fly + m3_fly]

result[, curve_slope_short := m1m3]
result[, curve_slope_long  := m1m12]

setorder(result, date)

fwrite(result, OUT)
cat(sprintf("\nWrote %d rows to %s\n", nrow(result), OUT))
cat(sprintf("Date range: %s → %s\n", min(result$date), max(result$date)))
cat(sprintf("Latest m1: %.2f  m1m2: %.4f  m2_fly: %.4f\n",
            result[.N, m1], result[.N, m1m2], result[.N, m2_fly]))
