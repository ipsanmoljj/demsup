# R/phase2_build.R
# CFTC Phase 2 — Expiry Week Spread Dataset
# Builds: expiry_dates, spread_windows_raw, expiry_summary,
#         expiry_cftc_merged, phase2_dataset, quality report,
#         descriptive stats
#
# Data sources:
#   CL_data.csv          : 1-min bars, 2021-01-04 to 2024-02-01 (TWAP spreads)
#   regime_labels_CL.csv : daily M1M2, 2021-01-04 to 2026-05-20 (extends CL_data)
#   CFTC 2016-2026 CL.xlsx
#   output/cftc/yahoo_ms_states.csv
#   output/wti_weekly.csv

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(readxl)
  library(timeDate)
})

setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
dir.create("output/cftc_phase2", showWarnings = FALSE, recursive = TRUE)

cat("=== CFTC Phase 2 Build ===\n")
cat("Started:", format(Sys.time()), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# HELPERS: Business day calendar
# ─────────────────────────────────────────────────────────────────────────────
cat("Building NYSE holiday calendar 2015-2027...\n")
# timeDate::holidayNYSE returns Date objects for each year
nyse_hols <- as.Date(
  as.character(
    do.call(c, lapply(2015:2027, function(y)
      tryCatch(holidayNYSE(y), error = function(e) timeDate())
    ))
  )
)
nyse_hols <- sort(unique(nyse_hols))

# NYMEX-specific early-close days not in the NYSE calendar
# CME/NYMEX treats Black Friday and Christmas Eve as non-business days for LTD calculation
# %w: 0=Sun,1=Mon,2=Tue,3=Wed,4=Thu,5=Fri,6=Sat
thanksgiving_date <- function(yr) {
  nov1 <- as.Date(sprintf("%d-11-01", yr))
  # First Thursday of November
  first_thu <- nov1 + (4L - as.integer(format(nov1, "%w")) + 7L) %% 7L
  first_thu + 21L  # 4th Thursday
}
nymex_extra <- as.Date(unlist(lapply(2015:2027, function(yr) {
  # Black Friday (day after Thanksgiving)
  bf <- thanksgiving_date(yr) + 1L
  # Christmas Eve: Dec 24 if business day, else Dec 23 (Sat) or Dec 22 (Sun)
  dec24 <- as.Date(sprintf("%d-12-24", yr))
  dow24 <- as.integer(format(dec24, "%w"))
  xe <- if (dow24 == 0L) dec24 - 2L  # Sunday → Friday Dec 22
        else if (dow24 == 6L) dec24 - 1L  # Saturday → Friday Dec 23
        else dec24                          # Weekday → Dec 24 itself
  c(bf, xe)
})))
nymex_extra <- sort(unique(nymex_extra))
nyse_hols <- sort(unique(c(nyse_hols, nymex_extra)))

is_bizday <- function(d) {
  !weekdays(as.Date(d)) %in% c("Saturday","Sunday") & !as.Date(d) %in% nyse_hols
}

# Move to prior business day if d is not a bizday
prev_bizday <- function(d) {
  d <- as.Date(d) - 1L
  while (!is_bizday(d)) d <- d - 1L
  d
}

# n-th prior bizday from d (exclusive of d)
nth_prev_bizday <- function(d, n) {
  d <- as.Date(d)
  count <- 0L
  while (count < n) {
    d <- d - 1L
    if (is_bizday(d)) count <- count + 1L
  }
  d
}

# Add n business days to d
add_bizdays <- function(d, n) {
  d <- as.Date(d)
  count <- 0L
  while (count < n) {
    d <- d + 1L
    if (is_bizday(d)) count <- count + 1L
  }
  d
}

# All business days in a range
bizdays_in_range <- function(d_start, d_end) {
  all_days <- seq(as.Date(d_start), as.Date(d_end), by = "day")
  all_days[sapply(all_days, is_bizday)]
}

cat(sprintf("  %d NYSE + NYMEX early-close holidays loaded (2015-2027)\n\n", length(nyse_hols)))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 1: Expiry Date Table
# Rule: LTD = 3 bizdays before the prior-bizday-adjusted 25th of the month
#        preceding delivery
# ─────────────────────────────────────────────────────────────────────────────
cat("=== STEP 1: Building expiry date table ===\n")

month_to_code <- c("F","G","H","J","K","M","N","Q","U","V","X","Z")

compute_ltd <- function(del_year, del_month) {
  ref_month <- if (del_month == 1L) 12L else del_month - 1L
  ref_year  <- if (del_month == 1L) del_year - 1L else del_year
  ref_25    <- as.Date(sprintf("%04d-%02d-25", ref_year, ref_month))
  # If 25th is not a bizday, move to the nearest prior bizday
  ref <- ref_25
  while (!is_bizday(ref)) ref <- ref - 1L
  # 3rd bizday prior to ref
  nth_prev_bizday(ref, 3L)
}

expiry_rows <- list()
for (yr in 2016:2026) {
  for (mo in 1:12) {
    code <- month_to_code[mo]
    yr2  <- yr %% 100L
    cc   <- sprintf("CL%s%02d", code, yr2)
    ltd  <- compute_ltd(yr, mo)
    expiry_rows[[length(expiry_rows) + 1L]] <- list(
      contract_code  = cc,
      delivery_month = sprintf("%04d-%02d", yr, mo),
      expiry_date    = ltd,
      expiry_year    = year(ltd),
      expiry_month   = month(ltd)
    )
  }
}
expiry_tbl <- rbindlist(expiry_rows)
expiry_tbl[, expiry_date := as.Date(expiry_date)]
# Keep Jan 2016 – Jul 2026 expiries
expiry_tbl <- expiry_tbl[
  expiry_date >= as.Date("2016-01-01") &
  expiry_date <= as.Date("2026-07-31")
][order(expiry_date)]

cat(sprintf("  Generated %d expiry dates\n", nrow(expiry_tbl)))

# Verification: cross-check 5 known dates
cat("\n  Verification vs known CME expiries:\n")
checks <- data.table(
  contract = c("CLF17","CLK20","CLM20","CLZ22"),
  known    = as.Date(c("2016-12-19","2020-04-21","2020-05-19","2022-11-18"))
)
for (i in seq_len(nrow(checks))) {
  computed <- expiry_tbl[contract_code == checks$contract[i], expiry_date]
  match_flag <- if (length(computed) > 0 && computed == checks$known[i]) "MATCH" else
                if (length(computed) > 0) sprintf("OFF BY %+d days", as.integer(computed - checks$known[i])) else "NOT FOUND"
  cat(sprintf("    %-8s  known=%s  computed=%s  [%s]\n",
              checks$contract[i], checks$known[i],
              if (length(computed) > 0) as.character(computed) else "NA", match_flag))
}
# Compute CLM26
clm26 <- expiry_tbl[contract_code == "CLM26", expiry_date]
cat(sprintf("    %-8s  computed=%s  (verify vs CME)\n", "CLM26",
            if (length(clm26) > 0) as.character(clm26) else "NA"))

fwrite(expiry_tbl, "output/cftc_phase2/expiry_dates.csv")
cat(sprintf("\n  Saved: output/cftc_phase2/expiry_dates.csv (%d rows)\n\n",
            nrow(expiry_tbl)))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 2: Extract M1-M2 Spread Windows
# Sources:
#   A. CL_data.csv (TWAP): 2021-01-04 to 2024-02-01
#   B. regime_labels_CL.csv (M1M2 daily): 2021-01-04 to 2026-05-20
#   C. Pre-2021: NA (no EIA key)
# ─────────────────────────────────────────────────────────────────────────────
cat("=== STEP 2: Building spread windows ===\n")

# ── 2A: Load CL_data.csv and compute daily TWAP spreads ──────────────────────
cat("  Loading CL_data.csv (1M rows, may take ~30s)...\n")
source("R/futures_reader.R")
ff_raw <- fread("CL_data.csv", skip = 1L, header = TRUE, na.strings = c("","NA"))
setnames(ff_raw, 1L, "timestamp")
# Parse timestamp: "2021-01-04 01:00:00+00:00"
ff_raw[, ts_utc := ymd_hms(timestamp, tz = "UTC")]
ff_raw[, ts_et  := with_tz(ts_utc, "America/New_York")]
ff_raw[, date_et := as.Date(ts_et)]
ff_raw[, hhmm_et := hour(ts_et) * 60L + minute(ts_et)]  # minutes since midnight

# Market hours: 09:00–14:30 ET = 540–870 minutes
ff_mkt <- ff_raw[hhmm_et >= 540L & hhmm_et <= 870L]

# Rename spread columns safely
c1p <- "c1||weighted_mid"; c2p <- "c2||weighted_mid"; c1t <- "c1||contract"
setnames(ff_mkt, c(c1p, c2p, c1t), c("c1_px", "c2_px", "c1_ticker"))
ff_mkt[, M1M2_bar := c1_px - c2_px]

# Daily TWAP spread
twap_daily <- ff_mkt[!is.na(M1M2_bar), .(
  M1M2_twap  = mean(M1M2_bar),
  c1_contract = tail(c1_ticker[!is.na(c1_ticker)], 1L),
  c1_vol_day  = .N,          # proxy: bar count
  price_source = "CL_data_TWAP"
), by = date_et]
setnames(twap_daily, "date_et", "trading_date")

# Close spread: 14:15–14:30 ET = 855–870 minutes
ff_close <- ff_mkt[hhmm_et >= 855L]
close_daily <- ff_close[!is.na(M1M2_bar), .(
  M1M2_close = mean(M1M2_bar)
), by = date_et]
setnames(close_daily, "date_et", "trading_date")

twap_daily <- merge(twap_daily, close_daily, by = "trading_date", all.x = TRUE)
setorder(twap_daily, trading_date)

cat(sprintf("  CL_data TWAP: %d trading days (%s to %s)\n",
            nrow(twap_daily), min(twap_daily$trading_date), max(twap_daily$trading_date)))

# Low-c1-volume flag: c1 daily bars < 20% of 20-day rolling mean
twap_daily[, c1_vol_roll20 := frollmean(c1_vol_day, 20L, align="right", na.rm=TRUE)]
twap_daily[, low_c1_volume := c1_vol_day < 0.2 * c1_vol_roll20]
twap_daily[is.na(low_c1_volume), low_c1_volume := FALSE]

# ── 2B: regime_labels_CL.csv daily M1M2 (extends to 2026-05) ─────────────────
cat("  Loading regime_labels_CL.csv (per-product, post-2026-06-17 fix)...\n")
# Use output/CL/regime_labels_CL.csv (canonical per-product file from classify_regimes("CL"))
# NOT output/regime_labels_CL.csv which is the stale pre-fix root file with corrupted M1M2 units
rl <- if (file.exists("output/CL/regime_labels_CL.csv"))
  fread("output/CL/regime_labels_CL.csv") else fread("output/regime_labels_CL.csv")
rl[, trading_date := as.Date(date)]
rl_spread <- rl[!is.na(M1M2), .(trading_date, M1M2_regime = M1M2,
                                  price_source_rl = "regime_labels_daily")]
setorder(rl_spread, trading_date)
cat(sprintf("  regime_labels M1M2: %d trading days (%s to %s)\n",
            nrow(rl_spread), min(rl_spread$trading_date), max(rl_spread$trading_date)))

# ── 2C: Combined daily spread table ──────────────────────────────────────────
# Priority: CL_data TWAP where available; regime_labels M1M2 for rest
all_trading_dates <- sort(unique(c(twap_daily$trading_date, rl_spread$trading_date)))

spread_combined <- merge(
  data.table(trading_date = all_trading_dates),
  twap_daily[, .(trading_date, M1M2_twap, M1M2_close, c1_contract, c1_vol_day, low_c1_volume, price_source)],
  by = "trading_date", all.x = TRUE
)
spread_combined <- merge(spread_combined, rl_spread, by = "trading_date", all.x = TRUE)

# Build unified M1M2_daily: prefer TWAP, fall back to regime_labels
spread_combined[, M1M2_daily := fifelse(!is.na(M1M2_twap), M1M2_twap, M1M2_regime)]
spread_combined[, price_source := fifelse(!is.na(M1M2_twap), "CL_data_TWAP",
                                   fifelse(!is.na(M1M2_regime), "regime_labels_daily",
                                           "unavailable"))]
spread_combined[is.na(low_c1_volume), low_c1_volume := FALSE]

# ── 2D: Extract windows per expiry ───────────────────────────────────────────
PRE_DAYS  <- 15L
POST_DAYS <- 10L

# We need business-day-indexed windows
# For each expiry, find the trading dates in the combined dataset
all_td_sorted <- sort(unique(spread_combined$trading_date))

window_rows <- list()

for (i in seq_len(nrow(expiry_tbl))) {
  e_date <- expiry_tbl$expiry_date[i]
  e_code <- expiry_tbl$contract_code[i]

  # Find expiry in trading dates (might not be in data if pre-2021)
  e_idx <- which(all_td_sorted == e_date)
  if (length(e_idx) == 0L) {
    # Try nearest business day in data
    e_idx <- which.min(abs(as.integer(all_td_sorted - e_date)))
    if (abs(as.integer(all_td_sorted[e_idx] - e_date)) > 5L) {
      # No data within 5 days — use a theoretical window of bizdays
      bizdays_win <- bizdays_in_range(e_date - 25L, e_date + 15L)
      for (bd in seq_along(bizdays_win)) {
        rel <- as.integer(bizdays_win[bd] - e_date)
        # Business day relative (approximate — no market data to anchor to)
        biz_before <- sum(bizdays_win < e_date)
        biz_idx    <- bd - biz_before - 1L
        window_rows[[length(window_rows) + 1L]] <- list(
          contract_code = e_code,
          expiry_date   = e_date,
          trading_date  = bizdays_win[bd],
          trading_day_relative = biz_idx,
          M1M2_daily    = NA_real_,
          M1M2_close    = NA_real_,
          c1_contract   = NA_character_,
          c1_volume     = NA_integer_,
          c2_volume     = NA_integer_,
          low_c1_volume = FALSE,
          price_source  = "unavailable"
        )
      }
      next
    }
  }

  # Extract window indices
  win_start <- max(1L, e_idx - PRE_DAYS)
  win_end   <- min(length(all_td_sorted), e_idx + POST_DAYS)
  win_dates <- all_td_sorted[win_start:win_end]
  win_rels  <- (win_start:win_end) - e_idx[1L]

  # Join with spread data
  win_data <- spread_combined[trading_date %in% win_dates]

  for (j in seq_along(win_dates)) {
    td  <- win_dates[j]
    rel <- win_rels[j]
    row <- win_data[trading_date == td]
    window_rows[[length(window_rows) + 1L]] <- list(
      contract_code = e_code,
      expiry_date   = e_date,
      trading_date  = td,
      trading_day_relative = rel,
      M1M2_daily    = if (nrow(row) > 0L) row$M1M2_daily   else NA_real_,
      M1M2_close    = if (nrow(row) > 0L) row$M1M2_close   else NA_real_,
      c1_contract   = if (nrow(row) > 0L) row$c1_contract  else NA_character_,
      c1_volume     = if (nrow(row) > 0L) row$c1_vol_day   else NA_integer_,
      c2_volume     = NA_integer_,       # not separately tracked
      low_c1_volume = if (nrow(row) > 0L) row$low_c1_volume else FALSE,
      price_source  = if (nrow(row) > 0L) row$price_source  else "unavailable"
    )
  }
}

spread_windows <- rbindlist(window_rows, fill = TRUE)
spread_windows[, expiry_date  := as.Date(expiry_date)]
spread_windows[, trading_date := as.Date(trading_date)]

# Special flag: CLK20 (WTI negative price event)
spread_windows[, covid_negative_price := contract_code == "CLK20"]

fwrite(spread_windows, "output/cftc_phase2/spread_windows_raw.csv")
cat(sprintf("\n  Saved: output/cftc_phase2/spread_windows_raw.csv (%d rows)\n\n",
            nrow(spread_windows)))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 3: Expiry-Level Summary Statistics
# ─────────────────────────────────────────────────────────────────────────────
cat("=== STEP 3: Computing expiry summary statistics ===\n")

get_spread_at <- function(dt, rel_days, tol = 1L) {
  # Get M1M2_daily at a specific relative day (with tolerance ±tol)
  for (d in rel_days) {
    for (delta in 0L:tol) {
      for (sign in c(0L, 1L, -1L)) {
        target <- d + sign * delta
        val <- dt[trading_day_relative == target, M1M2_daily]
        if (length(val) > 0L && !is.na(val[1L])) return(val[1L])
      }
    }
  }
  NA_real_
}

expiry_sum_rows <- list()
for (i in seq_len(nrow(expiry_tbl))) {
  e_code <- expiry_tbl$contract_code[i]
  e_date <- expiry_tbl$expiry_date[i]

  win <- spread_windows[contract_code == e_code & !is.na(M1M2_daily)]

  # Pre-baseline: days -10 to -6
  pre_win  <- win[trading_day_relative %in% (-10L):(-6L), M1M2_daily]
  # Expiry week: days -5 to -1
  exp_win  <- win[trading_day_relative %in% (-5L):(-1L), M1M2_daily]
  # Post: days +1 to +5
  post_win <- win[trading_day_relative %in% 1L:5L, M1M2_daily]

  M1M2_pre_baseline       <- if (length(pre_win)  >= 2L) mean(pre_win,  na.rm=TRUE) else NA_real_
  M1M2_pre_vol            <- if (length(pre_win)  >= 2L) sd(pre_win,    na.rm=TRUE) else NA_real_
  M1M2_entry              <- get_spread_at(win, -5L)
  M1M2_expiry_week_mean   <- if (length(exp_win)  >= 2L) mean(exp_win,  na.rm=TRUE) else NA_real_
  M1M2_expiry_week_end    <- get_spread_at(win, -1L)
  M1M2_on_expiry          <- get_spread_at(win,  0L)
  M1M2_post_1d            <- get_spread_at(win,  1L)
  M1M2_post_5d            <- get_spread_at(win,  5L)

  M1M2_expiry_week_chg    <- if (!is.na(M1M2_expiry_week_end) && !is.na(M1M2_entry))
    M1M2_expiry_week_end - M1M2_entry else NA_real_
  M1M2_expiry_week_chg_pct <- if (!is.na(M1M2_expiry_week_chg) && !is.na(M1M2_entry) && M1M2_entry != 0)
    M1M2_expiry_week_chg / abs(M1M2_entry) else NA_real_

  M1M2_post_chg     <- if (!is.na(M1M2_post_5d) && !is.na(M1M2_on_expiry))
    M1M2_post_5d - M1M2_on_expiry else NA_real_
  M1M2_post_chg_pct <- if (!is.na(M1M2_post_chg) && !is.na(M1M2_on_expiry) && M1M2_on_expiry != 0)
    M1M2_post_chg / abs(M1M2_on_expiry) else NA_real_

  roll_compression  <- if (!is.na(M1M2_expiry_week_end) && !is.na(M1M2_pre_baseline))
    M1M2_expiry_week_end - M1M2_pre_baseline else NA_real_
  roll_reversion    <- if (!is.na(M1M2_post_5d) && !is.na(M1M2_expiry_week_end))
    M1M2_post_5d - M1M2_expiry_week_end else NA_real_

  n_days_with_data  <- nrow(win)
  primary_source    <- if (nrow(win) > 0L) names(sort(-table(win$price_source)))[1L] else "unavailable"

  expiry_sum_rows[[i]] <- list(
    contract_code             = e_code,
    delivery_month            = expiry_tbl$delivery_month[i],
    expiry_date               = e_date,
    n_days_with_data          = n_days_with_data,
    price_source              = primary_source,
    covid_negative_price      = e_code == "CLK20",
    M1M2_pre_baseline         = round(M1M2_pre_baseline, 4),
    M1M2_pre_vol              = round(M1M2_pre_vol, 4),
    M1M2_entry                = round(M1M2_entry, 4),
    M1M2_expiry_week_mean     = round(M1M2_expiry_week_mean, 4),
    M1M2_expiry_week_end      = round(M1M2_expiry_week_end, 4),
    M1M2_expiry_week_chg      = round(M1M2_expiry_week_chg, 4),
    M1M2_expiry_week_chg_pct  = round(M1M2_expiry_week_chg_pct, 4),
    M1M2_on_expiry            = round(M1M2_on_expiry, 4),
    M1M2_post_1d              = round(M1M2_post_1d, 4),
    M1M2_post_5d              = round(M1M2_post_5d, 4),
    M1M2_post_chg             = round(M1M2_post_chg, 4),
    M1M2_post_chg_pct         = round(M1M2_post_chg_pct, 4),
    roll_compression          = round(roll_compression, 4),
    roll_reversion            = round(roll_reversion, 4)
  )
}

expiry_sum <- rbindlist(expiry_sum_rows, fill = TRUE)
expiry_sum[, expiry_date := as.Date(expiry_date)]
fwrite(expiry_sum, "output/cftc_phase2/expiry_summary.csv")
cat(sprintf("  Saved: output/cftc_phase2/expiry_summary.csv (%d rows)\n\n",
            nrow(expiry_sum)))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 4: Merge CFTC Positioning
# ─────────────────────────────────────────────────────────────────────────────
cat("=== STEP 4: Merging CFTC positioning ===\n")

cftc_raw <- as.data.table(read_excel("CFTC 2016-2026 CL.xlsx"))
cftc_raw[, release_date  := as.Date(releasedate)]
cftc_raw[, cutoff_date   := as.Date(date)]
cftc_raw[, net_long      := as.numeric(actual)]
cftc <- unique(cftc_raw[!is.na(net_long), .(release_date, cutoff_date, net_long)],
               by = "release_date")[order(release_date)]

cat(sprintf("  CFTC: %d weekly obs (%s to %s)\n",
            nrow(cftc), min(cftc$release_date), max(cftc$release_date)))

# ── Gross long / short from raw CFTC txt files (WTI-PHYSICAL NYMEX = 067651) ──
txt_files <- list.files("output", pattern = "cftc_\\d{4}\\.txt", full.names = TRUE)
gross_list <- lapply(txt_files, function(f) {
  dt <- fread(f, select = c("Report_Date_as_YYYY-MM-DD", "CFTC_Contract_Market_Code",
                             "M_Money_Positions_Long_All", "M_Money_Positions_Short_All"))
  dt[CFTC_Contract_Market_Code == "067651",
     .(cutoff_date  = as.Date(`Report_Date_as_YYYY-MM-DD`),
       mm_long_raw  = as.numeric(M_Money_Positions_Long_All),
       mm_short_raw = as.numeric(M_Money_Positions_Short_All))]
})
gross <- unique(rbindlist(gross_list)[order(cutoff_date)], by = "cutoff_date")
cat(sprintf("  Gross L/S: %d weekly obs from txt (%s to %s)\n",
            nrow(gross), min(gross$cutoff_date), max(gross$cutoff_date)))

cftc <- merge(cftc, gross, by = "cutoff_date", all.x = TRUE)
cftc[, mm_long  := mm_long_raw]
cftc[, mm_short := mm_short_raw]
cftc[, ls_ratio := round(mm_long / mm_short, 4)]
cftc[, mm_long_raw  := NULL]
cftc[, mm_short_raw := NULL]
cat(sprintf("  Gross L/S matched: %d of %d CFTC weeks\n",
            sum(!is.na(cftc$mm_long)), nrow(cftc)))

# Positioning features
cftc[, pos_z_full   := (net_long - mean(net_long)) / sd(net_long)]
cftc[, mm_chg       := net_long - shift(net_long, 1L)]
cftc[, mm_chg_z     := (mm_chg - mean(mm_chg, na.rm=TRUE)) / sd(mm_chg, na.rm=TRUE)]
# Rolling 52-week z-score (look-ahead-free)
cftc[, pos_z_roll52 := {
  vals <- net_long
  n    <- length(vals)
  z52  <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (i >= 52L) {
      w <- vals[(i-51L):(i-1L)]
      z52[i] <- (vals[i] - mean(w)) / sd(w)
    }
  }
  z52
}]
# Percentile ranks
cftc[, pos_pct_full  := rank(net_long) / .N]
cftc[, pos_pct_roll52 := {
  vals <- net_long
  n    <- length(vals)
  pct  <- rep(NA_real_, n)
  for (i in 52L:n) {
    w <- vals[(i-51L):(i-1L)]
    pct[i] <- mean(w < vals[i])
  }
  pct
}]
cftc[, extreme_long  := as.integer(pos_pct_full > 0.90)]
cftc[, extreme_short := as.integer(pos_pct_full < 0.10)]

# For each expiry: find last CFTC release_date <= (expiry - 5 bizdays)
cftc_merged_rows <- list()
for (i in seq_len(nrow(expiry_sum))) {
  e_date <- expiry_sum$expiry_date[i]
  e_code <- expiry_sum$contract_code[i]

  # Entry day = expiry - 5 trading days
  entry_day <- nth_prev_bizday(e_date, 5L)

  # Last CFTC release on or before entry_day
  valid_cftc <- cftc[release_date <= entry_day]
  if (nrow(valid_cftc) == 0L) {
    cftc_merged_rows[[i]] <- list(
      contract_code = e_code, expiry_date = e_date,
      cftc_releasedate = NA, cftc_cutoff_date = NA,
      cftc_net_long = NA_real_, cftc_days_before_expiry = NA_integer_,
      pos_z_full = NA_real_, pos_z_roll52 = NA_real_,
      pos_pct_full = NA_real_, pos_pct_roll52 = NA_real_,
      mm_chg = NA_real_, mm_chg_z = NA_real_,
      extreme_long = NA_integer_, extreme_short = NA_integer_,
      mm_long = NA_real_, mm_short = NA_real_, ls_ratio = NA_real_
    )
    next
  }
  last_cftc <- valid_cftc[.N]
  cftc_merged_rows[[i]] <- list(
    contract_code           = e_code,
    expiry_date             = e_date,
    cftc_releasedate        = last_cftc$release_date,
    cftc_cutoff_date        = last_cftc$cutoff_date,
    cftc_net_long           = last_cftc$net_long,
    cftc_days_before_expiry = as.integer(e_date - last_cftc$release_date),
    pos_z_full              = round(last_cftc$pos_z_full, 4),
    pos_z_roll52            = round(last_cftc$pos_z_roll52, 4),
    pos_pct_full            = round(last_cftc$pos_pct_full, 4),
    pos_pct_roll52          = round(last_cftc$pos_pct_roll52, 4),
    mm_chg                  = last_cftc$mm_chg,
    mm_chg_z                = round(last_cftc$mm_chg_z, 4),
    extreme_long            = last_cftc$extreme_long,
    extreme_short           = last_cftc$extreme_short,
    mm_long                 = last_cftc$mm_long,
    mm_short                = last_cftc$mm_short,
    ls_ratio                = last_cftc$ls_ratio
  )
}
cftc_merged <- rbindlist(cftc_merged_rows, fill = TRUE)
cftc_merged[, expiry_date := as.Date(expiry_date)]
cftc_merged[, cftc_releasedate := as.Date(cftc_releasedate)]
cftc_merged[, cftc_cutoff_date := as.Date(cftc_cutoff_date)]

expiry_cftc <- merge(expiry_sum, cftc_merged, by = c("contract_code","expiry_date"), all.x = TRUE)
fwrite(expiry_cftc, "output/cftc_phase2/expiry_cftc_merged.csv")
cat(sprintf("  Saved: output/cftc_phase2/expiry_cftc_merged.csv (%d rows)\n\n",
            nrow(expiry_cftc)))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 5: Add Regime, Markov, Seasonal, Price Context
# ─────────────────────────────────────────────────────────────────────────────
cat("=== STEP 5: Adding context variables ===\n")

# ── Markov state ──────────────────────────────────────────────────────────────
ms <- fread("output/cftc/yahoo_ms_states.csv")
ms[, release_date := as.Date(release_date)]
setorder(ms, release_date)

# ── Regime labels ─────────────────────────────────────────────────────────────
rl2 <- if (file.exists("output/CL/regime_labels_CL.csv"))
  fread("output/CL/regime_labels_CL.csv") else fread("output/regime_labels_CL.csv")
rl2[, date := as.Date(date)]
setorder(rl2, date)

# ── WTI weekly ───────────────────────────────────────────────────────────────
wti_wk <- fread("output/wti_weekly.csv")
wti_wk[, date := as.Date(date)]
wti_wk[, ret_4w_prior := (wti_close - shift(wti_close, 4L)) / shift(wti_close, 4L)]
wti_wk[, wti_vol_4w := {
  vals <- wti_close / shift(wti_close, 1L) - 1
  frollapply(vals, 4L, sd, align="right", na.rm=TRUE)
}]
setorder(wti_wk, date)

phase2_rows <- list()
for (i in seq_len(nrow(expiry_cftc))) {
  e_date <- expiry_cftc$expiry_date[i]
  e_code <- expiry_cftc$contract_code[i]

  # Entry day = expiry - 5 trading days
  entry_day <- nth_prev_bizday(e_date, 5L)

  # Markov state: nearest release_date to entry_day
  ms_idx <- which.min(abs(as.integer(ms$release_date - entry_day)))
  ms_row <- if (length(ms_idx) > 0L && abs(as.integer(ms$release_date[ms_idx] - entry_day)) <= 10L)
    ms[ms_idx] else NULL

  # Regime: nearest date <= entry_day
  rl_valid <- rl2[date <= entry_day]
  rl_row   <- if (nrow(rl_valid) > 0L) rl_valid[.N] else NULL

  # WTI: nearest Friday <= entry_day
  wti_valid <- wti_wk[date <= entry_day]
  wti_row   <- if (nrow(wti_valid) > 0L) wti_valid[.N] else NULL

  # Seasonal
  exp_month_num  <- as.integer(format(e_date, "%m"))
  quarter_num    <- ceiling(exp_month_num / 3L)
  driving_season <- as.integer(exp_month_num %in% 4L:9L)
  heating_season <- as.integer(exp_month_num %in% c(10L:12L, 1L:3L))
  dec_roll       <- as.integer(exp_month_num == 12L)

  phase2_rows[[i]] <- c(
    as.list(expiry_cftc[i]),
    list(
      markov_state   = if (!is.null(ms_row)) ms_row$state_filtered else NA_integer_,
      p_lowvol       = if (!is.null(ms_row)) ms_row$p_bull_filtered else NA_real_,
      curve_regime   = if (!is.null(rl_row)) rl_row$regime_label else NA_character_,
      level_z_126    = if (!is.null(rl_row)) rl_row$level_z_126 else NA_real_,
      wti_price_entry    = if (!is.null(wti_row)) wti_row$wti_close else NA_real_,
      wti_ret_4w_prior   = if (!is.null(wti_row)) round(wti_row$ret_4w_prior, 6) else NA_real_,
      wti_vol_4w         = if (!is.null(wti_row)) round(wti_row$wti_vol_4w, 6) else NA_real_,
      expiry_month_num   = exp_month_num,
      quarter            = paste0("Q", quarter_num),
      driving_season     = driving_season,
      heating_season     = heating_season,
      dec_roll           = dec_roll
    )
  )
}

phase2 <- rbindlist(phase2_rows, fill = TRUE)
phase2[, expiry_date      := as.Date(expiry_date)]
phase2[, cftc_releasedate := as.Date(cftc_releasedate)]
phase2[, cftc_cutoff_date := as.Date(cftc_cutoff_date)]

fwrite(phase2, "output/cftc_phase2/phase2_dataset.csv")
cat(sprintf("  Saved: output/cftc_phase2/phase2_dataset.csv (%d rows, %d cols)\n\n",
            nrow(phase2), ncol(phase2)))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 6: Data Quality Report
# ─────────────────────────────────────────────────────────────────────────────
cat("=== STEP 6: Data quality checks ===\n")

qr <- character(0)
add_qr <- function(...) {
  msg <- paste0(...)
  cat(" ", msg, "\n")
  qr <<- c(qr, msg)
}

add_qr("=== CFTC Phase 2 Data Quality Report ===")
add_qr("Generated: ", format(Sys.time()))
add_qr("")

# Check 1: Coverage
add_qr("--- CHECK 1: Expiry Date Coverage ---")
add_qr("Total expiry events generated : ", nrow(expiry_tbl))
n_with_data <- sum(phase2$price_source %in% c("CL_data_TWAP","regime_labels_daily"))
n_unavail   <- sum(phase2$price_source == "unavailable" | is.na(phase2$price_source))
n_twap      <- sum(phase2$price_source == "CL_data_TWAP", na.rm=TRUE)
n_rl        <- sum(phase2$price_source == "regime_labels_daily", na.rm=TRUE)
add_qr("Events with spread data      : ", n_with_data)
add_qr("  - CL_data TWAP (2021-2024) : ", n_twap)
add_qr("  - regime_labels daily      : ", n_rl)
add_qr("Events with NO spread data   : ", n_unavail, " (pre-2021, no EIA key)")

# Check 2: CFTC merge
add_qr("")
add_qr("--- CHECK 2: CFTC Merge Quality ---")
n_cftc_matched <- sum(!is.na(phase2$cftc_net_long))
add_qr("Events with CFTC match       : ", n_cftc_matched, " / ", nrow(phase2))
days_gap <- phase2[!is.na(cftc_days_before_expiry), cftc_days_before_expiry]
add_qr("CFTC days before expiry  mean: ", round(mean(days_gap), 1),
       "  range: ", min(days_gap), " - ", max(days_gap))
stale <- sum(days_gap > 14L, na.rm=TRUE)
add_qr("CFTC readings > 14 days old  : ", stale)
if (stale > 0L) {
  stale_rows <- phase2[!is.na(cftc_days_before_expiry) & cftc_days_before_expiry > 14L,
                       .(contract_code, expiry_date, cftc_days_before_expiry)]
  add_qr("  Stale CFTC events: ", paste(stale_rows$contract_code, collapse=", "))
}

# Check 3: Spread plausibility
add_qr("")
add_qr("--- CHECK 3: Spread Plausibility ---")
pre_vals <- phase2[!is.na(M1M2_pre_baseline), M1M2_pre_baseline]
if (length(pre_vals) > 5L) {
  add_qr(sprintf("M1M2_pre_baseline distribution: min=%.3f  p10=%.3f  median=%.3f  p90=%.3f  max=%.3f",
                 min(pre_vals), quantile(pre_vals,.1), median(pre_vals), quantile(pre_vals,.9), max(pre_vals)))
}
comp_vals <- phase2[!is.na(roll_compression), roll_compression]
if (length(comp_vals) > 5L) {
  add_qr(sprintf("roll_compression distribution  : min=%.3f  p10=%.3f  median=%.3f  p90=%.3f  max=%.3f",
                 min(comp_vals), quantile(comp_vals,.1), median(comp_vals), quantile(comp_vals,.9), max(comp_vals)))
  large_comp <- sum(abs(comp_vals) > 5L, na.rm=TRUE)
  add_qr(sprintf("|roll_compression| > $5 events : %d (flagged as potential data error)", large_comp))
}
clk20_row <- phase2[contract_code == "CLK20"]
if (nrow(clk20_row) > 0L) {
  add_qr(sprintf("CLK20 (WTI negative price): pre_baseline=%.3f  compression=%.3f  [OUTLIER FLAGGED]",
                 clk20_row$M1M2_pre_baseline, clk20_row$roll_compression))
}

# Check 4: Markov state
add_qr("")
add_qr("--- CHECK 4: Markov State Merge ---")
n_ms <- sum(!is.na(phase2$markov_state))
pct_hv <- mean(phase2$markov_state == 1L, na.rm=TRUE) * 100
pct_lv <- mean(phase2$markov_state == 2L, na.rm=TRUE) * 100
add_qr("Events with Markov state     : ", n_ms, " / ", nrow(phase2))
add_qr(sprintf("High-Vol (state=1): %.1f%%  Low-Vol (state=2): %.1f%%  (Phase 1: 63%%/37%%)", pct_hv, pct_lv))
no_state <- phase2[is.na(markov_state), contract_code]
if (length(no_state) > 0L) add_qr("Missing state: ", paste(head(no_state,10), collapse=", "))

# Check 5: Regime merge
add_qr("")
add_qr("--- CHECK 5: Regime Label Merge ---")
n_regime <- sum(!is.na(phase2$curve_regime))
add_qr("Events with regime label     : ", n_regime, " / ", nrow(phase2))
if (n_regime > 0L) {
  rt <- table(phase2$curve_regime)
  add_qr("Regime label distribution:")
  for (nm in names(sort(-rt))) add_qr(sprintf("  %-30s : %d", nm, rt[[nm]]))
}

# Check 6: Seasonal balance
add_qr("")
add_qr("--- CHECK 6: Seasonal Balance ---")
mo_counts <- table(phase2$expiry_month_num)
add_qr("Expiries per calendar month:")
for (m in 1:12) {
  n_m <- if (!is.na(mo_counts[as.character(m)])) mo_counts[[as.character(m)]] else 0L
  flag <- if (n_m < 6L || n_m > 12L) " [FLAG]" else ""
  add_qr(sprintf("  Month %2d: %2d%s", m, n_m, flag))
}

writeLines(qr, "output/cftc_phase2/data_quality_report.txt")
cat("  Saved: output/cftc_phase2/data_quality_report.txt\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 7: Descriptive Statistics
# ─────────────────────────────────────────────────────────────────────────────
cat("=== STEP 7: Descriptive statistics ===\n")

desc_stats <- function(dt, label) {
  n <- nrow(dt)
  if (n == 0L) return(NULL)
  data.table(
    group                   = label,
    n                       = n,
    n_with_spread           = sum(!is.na(dt$roll_compression)),
    mean_roll_compression   = round(mean(dt$roll_compression,  na.rm=TRUE), 4),
    sd_roll_compression     = round(sd(dt$roll_compression,    na.rm=TRUE), 4),
    median_roll_compression = round(median(dt$roll_compression,na.rm=TRUE), 4),
    pct_neg_compression     = round(mean(dt$roll_compression < 0L, na.rm=TRUE) * 100L, 1),
    mean_roll_reversion     = round(mean(dt$roll_reversion,   na.rm=TRUE), 4),
    sd_roll_reversion       = round(sd(dt$roll_reversion,     na.rm=TRUE), 4),
    pct_pos_reversion       = round(mean(dt$roll_reversion > 0L, na.rm=TRUE) * 100L, 1),
    mean_cftc_net_long      = round(mean(dt$cftc_net_long,    na.rm=TRUE), 0),
    mean_pos_z_full         = round(mean(dt$pos_z_full,       na.rm=TRUE), 4),
    mean_M1M2_pre_baseline  = round(mean(dt$M1M2_pre_baseline,na.rm=TRUE), 4)
  )
}

desc_list <- list(
  desc_stats(phase2,                                        "Full Sample"),
  desc_stats(phase2[markov_state == 1L],                   "High-Vol (Markov=1)"),
  desc_stats(phase2[markov_state == 2L],                   "Low-Vol (Markov=2)"),
  desc_stats(phase2[extreme_long  == 1L],                  "Extreme Long (>p90)"),
  desc_stats(phase2[extreme_short == 1L],                  "Extreme Short (<p10)"),
  desc_stats(phase2[driving_season == 1L],                 "Driving Season (Apr-Sep)"),
  desc_stats(phase2[heating_season == 1L],                 "Heating Season (Oct-Mar)"),
  desc_stats(phase2[dec_roll == 1L],                       "December Roll"),
  desc_stats(phase2[covid_negative_price == FALSE | is.na(covid_negative_price)], "Excl. CLK20")
)
desc_tbl <- rbindlist(Filter(Negate(is.null), desc_list), fill = TRUE)

cat("\n  Descriptive stats preview:\n")
print(desc_tbl[, .(group, n, n_with_spread, mean_roll_compression,
                   pct_neg_compression, mean_roll_reversion, pct_pos_reversion)],
      digits = 3)

fwrite(desc_tbl, "output/cftc_phase2/descriptive_stats.csv")
cat("\n  Saved: output/cftc_phase2/descriptive_stats.csv\n")

cat("\n=== Phase 2 Build COMPLETE ===\n")
cat("Outputs in output/cftc_phase2/:\n")
for (f in list.files("output/cftc_phase2")) cat(" ", f, "\n")
