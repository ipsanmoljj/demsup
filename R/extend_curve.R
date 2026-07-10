# Extends cl_curve_daily.csv from Jul 2 to Jul 3 & Jul 7 using I-drive DB
# Formula: m2_fly = m1 - 2*m2 + m3;  m3_fly = m2 - 2*m3 + m5

suppressPackageStartupMessages({
  library(data.table); library(DBI); library(RSQLite); library(lubridate)
})

DB_PATH   <- "I:/Public/Summer Interns Energy/DB/bars_15min_20260701.db"
CURVE_CSV <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup/strategy_live/final data/tent_data/cl_curve_daily.csv"

# ── Contract ordering for M1-M12 starting from Aug 2026 ──────────────────────
# In July 2026, front month = CL_Q26 (Aug 2026 delivery)
# Order: Q26, U26, V26, X26, Z26, F27, G27, H27, J27, K27, M27, N27, Q27
MONTH_CONTRACTS <- c("CL_Q26","CL_U26","CL_V26","CL_X26","CL_Z26",
                     "CL_F27","CL_G27","CL_H27","CL_J27","CL_K27",
                     "CL_M27","CL_N27","CL_Q27")
# Positions 1-8 = m1-m8, position 12 = m12 (CL_Q27 = Aug 2027)

con  <- dbConnect(SQLite(), DB_PATH)
tbls <- dbListTables(con)

# ── Pull close prices per table per day ───────────────────────────────────────
get_daily_close <- function(tbl) {
  q  <- paste0("SELECT timestamp, close FROM `", tbl, "` ORDER BY timestamp")
  dt <- as.data.table(dbGetQuery(con, q))
  if (nrow(dt) == 0) return(NULL)
  dt[, ts  := as.POSIXct(timestamp, tz = "UTC")]
  dt[, date := as.Date(ts)]
  # Last bar of each day = closing price
  dt[, .SD[.N], by = date][, .(date, close)]
}

avail <- intersect(MONTH_CONTRACTS, tbls)
cat("Available contracts:", paste(avail, collapse = ", "), "\n")

closes <- lapply(avail, function(tb) {
  d <- get_daily_close(tb)
  if (is.null(d)) return(NULL)
  d[, contract := tb]
  d
})
closes <- rbindlist(closes[!sapply(closes, is.null)])
dbDisconnect(con)

# Pivot wide: date × contract
wide <- dcast(closes, date ~ contract, value.var = "close")

# Identify trading days not yet in curve
curve <- fread(CURVE_CSV)
curve[, date := as.Date(date)]
existing_dates <- curve$date

new_dates <- wide$date[!wide$date %in% existing_dates]
cat("New trading dates:", paste(new_dates, collapse = ", "), "\n")

if (length(new_dates) == 0) {
  cat("No new dates to add.\n"); quit(status = 0)
}

new_wide <- wide[date %in% new_dates]

# ── Helper: get price for a contract at a date ────────────────────────────────
get_px <- function(dt_row, col) {
  v <- dt_row[[col]]
  if (is.null(v) || length(v) == 0 || is.na(v)) NA_real_ else as.numeric(v)
}

# ── Build new rows ────────────────────────────────────────────────────────────
new_rows <- lapply(seq_len(nrow(new_wide)), function(i) {
  row <- new_wide[i]
  m   <- setNames(
    sapply(MONTH_CONTRACTS, function(c) { v <- row[[c]]; if (is.null(v)) NA_real_ else as.numeric(v) }),
    MONTH_CONTRACTS
  )
  m1  <- m["CL_Q26"]; m2 <- m["CL_U26"]; m3 <- m["CL_V26"]
  m4  <- m["CL_X26"]; m5 <- m["CL_Z26"]; m6 <- m["CL_F27"]
  m7  <- m["CL_G27"]; m8 <- m["CL_H27"]; m12<- m["CL_Q27"]

  m1m2  <- m1 - m2
  m1m3  <- m1 - m3
  m1m6  <- m1 - m6
  m1m12 <- m1 - m12
  m2m3  <- m2 - m3
  m3m6  <- m3 - m6
  m6m12 <- m6 - m12
  m2_fly <- m1 - 2*m2 + m3
  m3_fly <- m2 - 2*m3 + m5
  m6_fly <- m5 - 2*m6 + m7
  m_condor <- m2_fly + m3_fly  # simple condor proxy
  curve_slope_short <- m1m3
  curve_slope_long  <- m1m12

  data.table(
    date = row$date,
    m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5, m6 = m6, m7 = m7, m8 = m8, m12 = m12,
    m1m2 = m1m2, m1m3 = m1m3, m1m6 = m1m6, m1m12 = m1m12,
    m2m3 = m2m3, m3m6 = m3m6, m6m12 = m6m12,
    m2_fly = m2_fly, m3_fly = m3_fly, m6_fly = m6_fly, m_condor = m_condor,
    curve_slope_short = curve_slope_short, curve_slope_long = curve_slope_long
  )
})

new_rows_dt <- rbindlist(new_rows)
cat("\nNew rows computed:\n"); print(new_rows_dt[, .(date, m1, m2, m1m2, m2_fly, m3_fly)])

# ── Append and write ──────────────────────────────────────────────────────────
curve_updated <- rbindlist(list(curve, new_rows_dt), fill = TRUE)
setorder(curve_updated, date)
fwrite(curve_updated, CURVE_CSV)
cat(sprintf("\nWrote %d rows to %s\n", nrow(curve_updated), CURVE_CSV))
cat(sprintf("New date range: %s → %s\n", min(curve_updated$date), max(curve_updated$date)))
