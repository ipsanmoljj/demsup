# =============================================================================
# intraday_signal_engine.R
# Intraday 15-min regime-conditional mean-reversion signal engine
# Products: CL (WTI M1M2), WTCL_LCO (Brent-WTI spread M1M2)
#
# BUG FIX (v2): Loop variable collision fixed.
#   Previous bug: filter condition used `product == product` (always TRUE)
#   because the loop variable name collided with the column name.
#   Fix: loop variable renamed to `prod` throughout; column filter is
#   now `bars$product == prod` which correctly compares column to variable.
#
# Data sources:
#   Historical backtest : data/intraday/CL_m1m2_15min.csv
#                         data/intraday/WTCL_LCO_SPREAD_m1m2_15min.csv
#   Live feed test      : called separately via run_live_validation()
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(zoo)
})

# -----------------------------------------------------------------------------
# 0. Configuration
# -----------------------------------------------------------------------------

PRODUCTS <- list(
  CL = list(
    csv_path      = "data/intraday/CL_m1m2_15min.csv",
    liquid_start  = 13L,   # UTC hour (inclusive)
    liquid_end    = 20L,   # UTC hour (exclusive)
    tick_value    = 10.0,  # USD per 0.01 move (1 lot)
    has_volume    = TRUE
  ),
  WTCL_LCO = list(
    csv_path      = "data/intraday/WTCL_LCO_SPREAD_m1m2_15min.csv",
    liquid_start  = 8L,
    liquid_end    = 16L,
    tick_value    = 10.0,
    has_volume    = FALSE
  )
)

# Regime-conditional signal parameters
# Each entry: list(z_thresh, atr_mult, confidence)
#   z_thresh   – minimum |z_score| required to fire a signal
#   atr_mult   – stop distance = atr_mult * ATR14
#   confidence – position size scalar (0–1); keeps weak regimes dormant
REGIME_PARAMS <- list(
  "Deep-Backwardation"    = list(z_thresh = 1.0, atr_mult = 1.25, confidence = 1.0),
  "Backwardation"         = list(z_thresh = 2.0, atr_mult = 1.10, confidence = 0.6),
  "Backwardation-Deficit" = list(z_thresh = 2.5, atr_mult = 1.00, confidence = 0.1),
  "Easing-Backwardation"  = list(z_thresh = 4.0, atr_mult = 0.75, confidence = 0.1),
  "Flat"                  = list(z_thresh = 4.0, atr_mult = 0.75, confidence = 0.1),
  "Easing-Contango"       = list(z_thresh = 4.0, atr_mult = 0.75, confidence = 0.1),
  "Contango"              = list(z_thresh = 4.0, atr_mult = 0.75, confidence = 0.1),
  "Deep-Contango"         = list(z_thresh = 4.0, atr_mult = 0.75, confidence = 0.1),
  "Warm-Up"               = list(z_thresh = Inf, atr_mult = 1.00, confidence = 0.0),
  "Unknown"               = list(z_thresh = Inf, atr_mult = 1.00, confidence = 0.0)
)

# Signal windows (in 15-min bars)
Z_LONG_WINDOW  <- 120L   # ~30 hours: slow baseline
Z_SHORT_WINDOW <- 48L    # ~12 hours: fast signal
ATR_WINDOW     <- 14L
RSI_WINDOW     <- 14L
RR_RATIO       <- 2.0    # reward:risk for target
BE_THRESHOLD   <- 0.5    # fraction of stop to move to break-even

# Data split
TRAIN_END      <- as.Date("2023-12-31")
VALID_END      <- as.Date("2024-06-30")
# Test window: 2024-07-01 onwards

# -----------------------------------------------------------------------------
# 1. Helper: ATR14
# -----------------------------------------------------------------------------
.atr14 <- function(high, low, close, n = ATR_WINDOW) {
  tr <- pmax(
    high - low,
    abs(high - shift(close, 1L, type = "lag")),
    abs(low  - shift(close, 1L, type = "lag")),
    na.rm = FALSE
  )
  # Wilder smoothing
  atr <- numeric(length(tr))
  first_valid <- which(!is.na(tr))[1L]
  if (is.na(first_valid) || (first_valid + n - 1L) > length(tr)) return(rep(NA_real_, length(tr)))
  atr[first_valid + n - 1L] <- mean(tr[first_valid:(first_valid + n - 1L)], na.rm = TRUE)
  for (i in seq(first_valid + n, length(tr))) {
    atr[i] <- (atr[i - 1L] * (n - 1L) + tr[i]) / n
  }
  atr[atr == 0] <- NA_real_
  atr
}

# -----------------------------------------------------------------------------
# 2. Helper: Wilder RSI
# -----------------------------------------------------------------------------
.rsi_wilder <- function(close, n = RSI_WINDOW) {
  chg   <- diff(close, lag = 1L)
  gains <- pmax(chg, 0)
  losses <- pmax(-chg, 0)
  rsi <- rep(NA_real_, length(close))
  if (length(gains) < n) return(rsi)
  # seed
  avg_g <- mean(gains[1:n], na.rm = TRUE)
  avg_l <- mean(losses[1:n], na.rm = TRUE)
  for (i in seq(n + 1L, length(gains))) {
    avg_g <- (avg_g * (n - 1L) + gains[i]) / n
    avg_l <- (avg_l * (n - 1L) + losses[i]) / n
  }
  # final RSI values
  for (i in seq(n + 1L, length(close))) {
    idx_chg <- i - 1L
    avg_g <- (avg_g * (n - 1L) + pmax(chg[idx_chg], 0)) / n
    avg_l <- (avg_l * (n - 1L) + pmax(-chg[idx_chg], 0)) / n
    rs <- if (avg_l == 0) 100 else avg_g / avg_l
    rsi[i] <- 100 - 100 / (1 + rs)
  }
  rsi
}

# -----------------------------------------------------------------------------
# 3. Feature engineering for one product's bar series
# -----------------------------------------------------------------------------
.build_features <- function(dt) {
  # dt must have columns: timestamp, open, high, low, close, volume (or NA)
  # Returns dt with signal columns appended

  dt <- copy(dt)
  setorder(dt, timestamp)

  n <- nrow(dt)
  cl <- dt$close

  # Rolling z-scores (long and short window)
  roll_mean_long  <- frollmean(cl, Z_LONG_WINDOW,  align = "right", na.rm = TRUE)
  roll_sd_long    <- frollapply(cl, Z_LONG_WINDOW,  sd,  align = "right")
  roll_mean_short <- frollmean(cl, Z_SHORT_WINDOW, align = "right", na.rm = TRUE)
  roll_sd_short   <- frollapply(cl, Z_SHORT_WINDOW, sd,  align = "right")

  dt[, z_long  := fifelse(roll_sd_long  > 0, (cl - roll_mean_long)  / roll_sd_long,  NA_real_)]
  dt[, z_short := fifelse(roll_sd_short > 0, (cl - roll_mean_short) / roll_sd_short, NA_real_)]

  # ATR14
  dt[, atr14 := .atr14(high, low, close)]

  # RSI14
  dt[, rsi14 := .rsi_wilder(close)]

  # VWAP deviation (only meaningful when volume > 0)
  if ("volume" %in% names(dt) && any(!is.na(dt$volume) & dt$volume > 0)) {
    dt[, session_date := as.Date(timestamp)]
    dt[, cum_pv := cumsum(close * pmax(volume, 0, na.rm = TRUE)), by = session_date]
    dt[, cum_v  := cumsum(pmax(volume, 0, na.rm = TRUE)),         by = session_date]
    dt[, vwap   := fifelse(cum_v > 0, cum_pv / cum_v, NA_real_)]
    dt[, vwap_dev := fifelse(!is.na(vwap) & atr14 > 0,
                             (close - vwap) / atr14, NA_real_)]
    dt[, c("cum_pv", "cum_v", "session_date") := NULL]
  } else {
    dt[, vwap     := NA_real_]
    dt[, vwap_dev := NA_real_]
  }

  # Session open gap (first bar of each UTC session day)
  dt[, session_date := as.Date(timestamp)]
  dt[, session_open := close[1L], by = session_date]
  dt[, open_gap := fifelse(atr14 > 0, (open - session_open) / atr14, NA_real_)]
  dt[, session_date := NULL]

  # UTC hour for liquid hours filter
  dt[, utc_hour := hour(timestamp)]

  dt[]
}

# -----------------------------------------------------------------------------
# 4. Merge daily regime labels onto intraday bars
# -----------------------------------------------------------------------------
.merge_regimes <- function(bars, regime_dt, prod_name) {
  # regime_dt: data.table with columns date, product, regime_label
  # bars:      data.table with column timestamp

  prod_regimes <- regime_dt[product == prod_name, .(date, regime_label)]
  bars[, date := as.Date(timestamp)]
  bars <- merge(bars, prod_regimes, by = "date", all.x = TRUE)
  bars[is.na(regime_label), regime_label := "Unknown"]
  bars[, date := NULL]
  bars[]
}

# -----------------------------------------------------------------------------
# 5. Signal generation (one bar)
# -----------------------------------------------------------------------------
.get_signal_params <- function(regime) {
  p <- REGIME_PARAMS[[regime]]
  if (is.null(p)) p <- REGIME_PARAMS[["Unknown"]]
  p
}

# Returns: 1 (long), -1 (short), 0 (no signal)
.signal_direction <- function(z_short, vwap_dev, rsi14, has_volume, params) {
  if (params$confidence == 0) return(0L)

  # Primary signal: z_short (mean reversion)
  # Confirmation: RSI or VWAP deviation in same direction
  z  <- z_short
  if (is.na(z)) return(0L)

  thresh <- params$z_thresh

  direction <- 0L
  if (z >  thresh) direction <- -1L   # spread too high → short (expect reversion)
  if (z < -thresh) direction <-  1L   # spread too low  → long  (expect reversion)

  if (direction == 0L) return(0L)

  # Confirmation gate: RSI must not contradict
  # Long signal needs RSI < 70 (not overbought); short needs RSI > 30 (not oversold)
  if (!is.na(rsi14)) {
    if (direction ==  1L && rsi14 > 70) return(0L)
    if (direction == -1L && rsi14 < 30) return(0L)
  }

  # If VWAP deviation available, require it to agree in direction
  if (has_volume && !is.na(vwap_dev)) {
    if (direction ==  1L && vwap_dev >  0.5) return(0L)  # price above VWAP, contradicts long
    if (direction == -1L && vwap_dev < -0.5) return(0L)  # price below VWAP, contradicts short
  }

  direction
}

# -----------------------------------------------------------------------------
# 6. Backtest loop for one product
#    BUG FIX: loop variable is `prod` (not `product`) to avoid colliding with
#    the `product` column name in any data.table inside the function.
# -----------------------------------------------------------------------------
.backtest_product <- function(prod, cfg, bars_all, regime_dt, split) {
  cat(sprintf("\n[%s] Starting backtest (split: %s)\n", prod, split))

  # --- Filter to this product's bars only ---
  # FIX: `prod` is the loop variable; `bars_all$product` is the column.
  # Previously written as `bars_all$product == product` which always evaluated
  # to a logical vector compared to itself → always TRUE → all rows matched.
  bars <- bars_all[product == prod]   # data.table syntax; `prod` is unambiguous

  if (nrow(bars) == 0L) {
    cat(sprintf("[%s] No bars found. Check product name.\n", prod))
    return(NULL)
  }

  # Merge regime labels
  bars <- .merge_regimes(bars, regime_dt, prod)

  # Apply liquid hours filter
  bars <- bars[utc_hour >= cfg$liquid_start & utc_hour < cfg$liquid_end]

  # Split selection
  bars[, bar_date := as.Date(timestamp)]
  if (split == "train") {
    bars <- bars[bar_date <= TRAIN_END]
  } else if (split == "validation") {
    bars <- bars[bar_date > TRAIN_END & bar_date <= VALID_END]
  } else if (split == "test") {
    bars <- bars[bar_date > VALID_END]
  }

  if (nrow(bars) < Z_LONG_WINDOW + 10L) {
    cat(sprintf("[%s] Insufficient bars for split '%s'.\n", prod, split))
    return(NULL)
  }

  cat(sprintf("[%s] %d bars in '%s' window (%s to %s)\n",
              prod, nrow(bars), split,
              min(bars$bar_date), max(bars$bar_date)))

  # ---- Trade simulation ----
  trades       <- list()
  in_trade     <- FALSE
  entry_price  <- NA_real_
  stop_price   <- NA_real_
  target_price <- NA_real_
  be_triggered <- FALSE
  direction    <- 0L
  entry_bar    <- NA_integer_

  for (i in seq_len(nrow(bars))) {
    row <- bars[i]

    # Close open trade at session end (last liquid bar of each session date)
    if (in_trade) {
      is_last_bar <- (i == nrow(bars)) ||
        (as.Date(bars[i + 1L]$timestamp) != as.Date(row$timestamp))

      if (is_last_bar) {
        pnl <- (row$close - entry_price) * direction
        trades[[length(trades) + 1L]] <- list(
          product     = prod,
          entry_time  = bars[entry_bar]$timestamp,
          exit_time   = row$timestamp,
          direction   = direction,
          entry_price = entry_price,
          exit_price  = row$close,
          exit_reason = "session_close",
          pnl_pts     = pnl,
          regime      = bars[entry_bar]$regime_label
        )
        in_trade <- FALSE
        next
      }

      # Break-even stop: once price moves BE_THRESHOLD * stop_dist in our favour,
      # move stop to entry price
      stop_dist <- abs(entry_price - stop_price)
      if (!be_triggered) {
        profit_so_far <- (row$close - entry_price) * direction
        if (profit_so_far >= BE_THRESHOLD * stop_dist) {
          stop_price   <- entry_price
          be_triggered <- TRUE
        }
      }

      # Check stop
      hit_stop <- (direction ==  1L && row$low  <= stop_price) ||
                  (direction == -1L && row$high >= stop_price)
      # Check target
      hit_target <- (direction ==  1L && row$high >= target_price) ||
                    (direction == -1L && row$low  <= target_price)

      if (hit_stop || hit_target) {
        exit_price  <- if (hit_target) target_price else stop_price
        exit_reason <- if (hit_target) "target" else "stop"
        pnl <- (exit_price - entry_price) * direction
        trades[[length(trades) + 1L]] <- list(
          product     = prod,
          entry_time  = bars[entry_bar]$timestamp,
          exit_time   = row$timestamp,
          direction   = direction,
          entry_price = entry_price,
          exit_price  = exit_price,
          exit_reason = exit_reason,
          pnl_pts     = pnl,
          regime      = bars[entry_bar]$regime_label
        )
        in_trade <- FALSE
      }
      next
    }

    # ---- No trade open: check for entry ----
    if (is.na(row$z_short) || is.na(row$atr14) || row$atr14 <= 0) next

    params <- .get_signal_params(row$regime_label)
    if (params$confidence == 0) next

    sig <- .signal_direction(
      z_short    = row$z_short,
      vwap_dev   = row$vwap_dev,
      rsi14      = row$rsi14,
      has_volume = cfg$has_volume,
      params     = params
    )

    if (sig == 0L) next

    # First reversion bar confirmation:
    # Signal fires on bar i, entry on close of bar i+1 if direction unchanged
    if (i >= nrow(bars)) next
    next_row <- bars[i + 1L]
    if (as.Date(next_row$timestamp) != as.Date(row$timestamp)) next  # no overnight entry

    # Confirm: z_short should be pulling back (absolute z smaller on next bar)
    if (is.na(next_row$z_short)) next
    if (abs(next_row$z_short) >= abs(row$z_short)) next  # not reverting yet

    # Entry
    atr         <- row$atr14
    in_trade     <- TRUE
    direction    <- sig
    entry_price  <- next_row$close
    stop_price   <- entry_price - direction * params$atr_mult * atr
    target_price <- entry_price + direction * RR_RATIO * params$atr_mult * atr
    be_triggered <- FALSE
    entry_bar    <- i + 1L
  }

  if (length(trades) == 0L) {
    cat(sprintf("[%s] No trades generated.\n", prod))
    return(NULL)
  }

  trades_dt <- rbindlist(lapply(trades, as.data.table))
  trades_dt[, split := split]

  cat(sprintf("[%s] %d trades | Hit rate: %.1f%% | Total P&L: %+.2f pts\n",
              prod,
              nrow(trades_dt),
              100 * mean(trades_dt$pnl_pts > 0),
              sum(trades_dt$pnl_pts)))

  trades_dt[]
}

# -----------------------------------------------------------------------------
# 7. Load and prepare all bar data
# -----------------------------------------------------------------------------
.load_bars <- function() {
  bar_list <- list()

  for (prod_name in names(PRODUCTS)) {
    cfg  <- PRODUCTS[[prod_name]]
    path <- cfg$csv_path

    if (!file.exists(path)) {
      warning(sprintf("CSV not found: %s — skipping %s", path, prod_name))
      next
    }

    dt <- fread(path)

    # Standardise column names
    setnames(dt,
             old = intersect(c("Timestamp", "TIMESTAMP", "time", "Time"), names(dt)),
             new = rep("timestamp", length(intersect(c("Timestamp","TIMESTAMP","time","Time"), names(dt)))))

    if (!"timestamp" %in% names(dt)) stop(sprintf("[%s] No timestamp column found.", prod_name))

    dt[, timestamp := as.POSIXct(timestamp, tz = "UTC")]
    if (!"volume" %in% names(dt)) dt[, volume := NA_real_]

    # Ensure numeric OHLC
    for (col in c("open","high","low","close")) {
      if (col %in% names(dt)) dt[, (col) := as.numeric(get(col))]
    }

    # Add product tag
    dt[, product := prod_name]

    # Build features
    cat(sprintf("[%s] Building features on %d bars...\n", prod_name, nrow(dt)))
    dt <- .build_features(dt)

    bar_list[[prod_name]] <- dt
  }

  rbindlist(bar_list, fill = TRUE)
}

# -----------------------------------------------------------------------------
# 8. Load regime labels
#    Reads per-product CSV files: output/regime_labels_CL.csv etc.
#    Columns used: date, product, regime_label
#    The intraday engine maps:  CL       → regime_labels_CL.csv
#                               WTCL_LCO → regime_labels_CL.csv (CL drives the spread)
# -----------------------------------------------------------------------------

# Map each intraday product to its regime label CSV
REGIME_CSV_MAP <- list(
  CL       = "output/regime_labels_CL.csv",
  WTCL_LCO = "output/regime_labels_CL.csv"   # spread uses CL regime
)

.load_regimes <- function() {
  parts <- list()

  for (prod_name in names(REGIME_CSV_MAP)) {
    path <- REGIME_CSV_MAP[[prod_name]]

    if (!file.exists(path)) {
      stop(sprintf(
        "Regime labels file not found for %s: %s\nCheck the output/ folder.",
        prod_name, path
      ))
    }

    dt <- fread(path, select = c("date", "product", "regime_label"))
    dt[, date := as.Date(date)]

    # Override product column to match the intraday product name
    # (e.g. the CSV has product="CL" but we need "WTCL_LCO" for the spread)
    dt[, product := prod_name]

    parts[[prod_name]] <- dt
  }

  rbindlist(parts)
}

# -----------------------------------------------------------------------------
# 9. Summary report
# -----------------------------------------------------------------------------
.print_summary <- function(all_trades) {
  cat("\n", strrep("=", 60), "\n")
  cat("BACKTEST SUMMARY\n")
  cat(strrep("=", 60), "\n\n")

  for (sp in c("train", "validation", "test")) {
    sub <- all_trades[split == sp]
    if (nrow(sub) == 0L) next

    cat(sprintf("--- %s window ---\n", toupper(sp)))

    for (prod_name in unique(sub$product)) {
      p <- sub[product == prod_name]
      hit  <- mean(p$pnl_pts > 0)
      pnl  <- sum(p$pnl_pts)
      n    <- nrow(p)
      cat(sprintf("  %-12s  Trades: %3d  Hit: %5.1f%%  P&L: %+7.3f pts\n",
                  prod_name, n, 100*hit, pnl))
    }

    cat(sprintf("\n  Regime breakdown (test window):\n"))
    if (sp == "test") {
      reg_tbl <- sub[, .(
        trades   = .N,
        hit_rate = round(100 * mean(pnl_pts > 0), 1),
        total_pnl = round(sum(pnl_pts), 3)
      ), by = .(product, regime)][order(product, -trades)]
      print(reg_tbl)
    }
    cat("\n")
  }
}

# -----------------------------------------------------------------------------
# 10. Main entry point — historical backtest
# -----------------------------------------------------------------------------
run_backtest <- function(splits = c("train", "validation", "test")) {
  cat("Loading bar data...\n")
  bars_all   <- .load_bars()

  cat("Loading regime labels...\n")
  regime_dt  <- .load_regimes()

  all_trades <- list()

  for (sp in splits) {
    for (prod_name in names(PRODUCTS)) {  # loop variable is `prod_name` — no collision
      cfg    <- PRODUCTS[[prod_name]]
      result <- .backtest_product(prod_name, cfg, bars_all, regime_dt, sp)
      if (!is.null(result)) all_trades[[length(all_trades) + 1L]] <- result
    }
  }

  if (length(all_trades) == 0L) {
    cat("No trades produced across any product/split.\n")
    return(invisible(NULL))
  }

  all_trades_dt <- rbindlist(all_trades, fill = TRUE)
  .print_summary(all_trades_dt)

  invisible(all_trades_dt)
}

# -----------------------------------------------------------------------------
# 11. Live feed validation runner
#     Reads SQLite .db files from the Lightstreamer folder and runs signals.
#     Called separately — does not affect historical backtest.
# -----------------------------------------------------------------------------
run_live_validation <- function(
    db_folder    = "I:/Public/Siddharth Raj/lightstreamer_data",
    date_range   = NULL,   # e.g. c("2026-06-12", "2026-06-16")
    products     = c("CL", "WTCL_LCO")
) {
  if (!requireNamespace("RSQLite", quietly = TRUE) ||
      !requireNamespace("DBI",     quietly = TRUE)) {
    stop("Install RSQLite and DBI first:\n  install.packages(c('RSQLite','DBI'))")
  }
  library(RSQLite)
  library(DBI)

  # Find all .db files in the folder
  db_files <- list.files(db_folder, pattern = "bars_15min_\\d{8}\\.db$", full.names = TRUE)

  if (length(db_files) == 0L) {
    stop(sprintf("No .db files found in: %s\nCheck the path is accessible.", db_folder))
  }

  # Filter to date range if provided
  if (!is.null(date_range)) {
    date_from <- as.Date(date_range[1])
    date_to   <- as.Date(date_range[2])
    # Extract date from filename: bars_15min_20260612.db → 20260612
    file_dates <- as.Date(
      sub(".*bars_15min_(\\d{8})\\.db$", "\\1", basename(db_files)),
      format = "%Y%m%d"
    )
    db_files <- db_files[file_dates >= date_from & file_dates <= date_to]
  }

  if (length(db_files) == 0L) {
    stop("No .db files match the requested date range.")
  }

  cat(sprintf("Found %d .db file(s): %s\n", length(db_files),
              paste(basename(db_files), collapse = ", ")))

  # Read all bars from all files
  bar_list <- list()
  for (db_path in db_files) {
    con <- dbConnect(SQLite(), db_path)
    tables <- dbListTables(con)
    cat(sprintf("  %s — tables: %s\n", basename(db_path), paste(tables, collapse = ", ")))

    for (tbl in tables) {
      dt <- as.data.table(dbReadTable(con, tbl))

      # Standardise timestamp
      if ("timestamp" %in% names(dt)) {
        dt[, timestamp := as.POSIXct(timestamp, tz = "UTC", origin = "1970-01-01")]
      }

      # Determine which product this table represents
      # Convention: table name contains "CL" or "LCO" etc.
      matched_prod <- NA_character_
      for (p in products) {
        if (grepl(p, tbl, ignore.case = TRUE)) { matched_prod <- p; break }
      }
      if (is.na(matched_prod)) {
        cat(sprintf("    Table '%s' doesn't match any product — skipping.\n", tbl))
        next
      }

      dt[, product := matched_prod]
      bar_list[[length(bar_list) + 1L]] <- dt
    }
    dbDisconnect(con)
  }

  if (length(bar_list) == 0L) {
    cat("No matching product tables found in the .db files.\n")
    return(invisible(NULL))
  }

  bars_all <- rbindlist(bar_list, fill = TRUE)
  if (!"volume" %in% names(bars_all)) bars_all[, volume := NA_real_]

  # Ensure OHLC numeric
  for (col in c("open","high","low","close")) {
    if (col %in% names(bars_all)) bars_all[, (col) := as.numeric(get(col))]
  }

  # Build features
  bars_by_prod <- list()
  for (prod_name in products) {
    sub <- bars_all[product == prod_name]
    if (nrow(sub) == 0L) next
    cat(sprintf("[%s] Building features on %d live bars...\n", prod_name, nrow(sub)))
    sub <- .build_features(sub)
    bars_by_prod[[prod_name]] <- sub
  }
  bars_featured <- rbindlist(bars_by_prod, fill = TRUE)

  # Load regime labels and merge
  cat("Loading regime labels...\n")
  regime_dt <- .load_regimes()
  for (prod_name in products) {
    bars_featured[product == prod_name,
                  regime_label := .merge_regimes(
                    bars_featured[product == prod_name], regime_dt, prod_name
                  )$regime_label]
  }

  # Apply liquid hours filter and generate signals bar-by-bar
  cat("\n--- LIVE SIGNAL SCAN ---\n")
  signals_out <- list()

  for (prod_name in products) {
    cfg  <- PRODUCTS[[prod_name]]
    bars <- bars_featured[product == prod_name &
                            utc_hour >= cfg$liquid_start &
                            utc_hour <  cfg$liquid_end]

    if (nrow(bars) == 0L) next

    for (i in seq_len(nrow(bars))) {
      row <- bars[i]
      if (is.na(row$z_short) || is.na(row$atr14) || row$atr14 <= 0) next

      params <- .get_signal_params(row$regime_label)
      if (params$confidence == 0) next

      sig <- .signal_direction(
        z_short    = row$z_short,
        vwap_dev   = row$vwap_dev,
        rsi14      = row$rsi14,
        has_volume = cfg$has_volume,
        params     = params
      )

      if (sig != 0L) {
        signals_out[[length(signals_out) + 1L]] <- list(
          product    = prod_name,
          timestamp  = row$timestamp,
          regime     = row$regime_label,
          z_short    = round(row$z_short, 3),
          vwap_dev   = round(row$vwap_dev %||% NA_real_, 3),
          rsi14      = round(row$rsi14, 1),
          atr14      = round(row$atr14, 4),
          direction  = ifelse(sig == 1L, "LONG", "SHORT"),
          confidence = params$confidence,
          stop_dist  = round(params$atr_mult * row$atr14, 4),
          target_dist= round(RR_RATIO * params$atr_mult * row$atr14, 4)
        )
      }
    }
  }

  if (length(signals_out) == 0L) {
    cat("No signals fired on live data in the selected date range.\n")
    return(invisible(NULL))
  }

  sig_dt <- rbindlist(lapply(signals_out, as.data.table))
  cat(sprintf("\n%d signal(s) found:\n", nrow(sig_dt)))
  print(sig_dt[, .(product, timestamp, regime, direction, z_short, confidence,
                   stop_dist, target_dist)])

  invisible(sig_dt)
}

# Null-coalescing helper (base R doesn't have %||%)
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

# =============================================================================
# HOW TO USE
# =============================================================================
# 1. Historical backtest (all splits):
#      source("R/intraday_signal_engine.R")
#      results <- run_backtest()
#
# 2. Test only (clean unseen data, Jul 2024 → May 2026):
#      results <- run_backtest(splits = "test")
#
# 3. Live feed validation (Jun 12–16 SQLite files):
#      run_live_validation(
#        db_folder  = "I:/Public/Siddharth Raj/lightstreamer_data",
#        date_range = c("2026-06-12", "2026-06-16")
#      )
# =============================================================================