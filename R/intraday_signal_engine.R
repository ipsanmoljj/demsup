# =============================================================================
# intraday_signal_engine.R
# =============================================================================
# Intraday mean-reversion signal engine for energy futures M1M2 spreads.
# Structurally independent from the daily signal engine (Stage 3).
#
# Products:
#   CL          — WTI M1M2 spread        (data/intraday/CL_m1m2_15min.csv)
#   WTCL_LCO    — Brent-WTI inter-market (data/intraday/WTCL_LCO_SPREAD_m1m2_15min.csv)
#
# Four signals evaluated by IC, best selected per regime x horizon:
#   1. Intraday z-score     — rolling z of spread on 15-min bars
#   2. VWAP deviation       — spread vs session VWAP (CL only; NA for WTCL_LCO)
#   3. RSI (14-bar)         — Wilder RSI on spread close
#   4. Session open gap     — spread open vs prior session last close
#
# Entry:  time filter (liquid hours) + first reversion bar confirmation
# Stop:   1.25 x ATR14 on 15-min bars (intraday ATR, not daily)
# Exit:   hard stop | break-even stop | session close (no overnight holds)
# Target: 2.0 x ATR14 from entry
#
# Data split (matches daily model):
#   Training   : start -> Dec 2023   (IC computation, parameter estimation)
#   Validation : Jan 2024 -> Jun 2024 (threshold tuning only)
#   Test       : Jul 2024 -> May 2026 (opened once, results accepted as-is)
# =============================================================================

library(data.table)
library(lubridate)

# ── 0. Configuration ----------------------------------------------------------

DATA_ROOT  <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"
INTRA_DIR  <- file.path(DATA_ROOT, "data", "intraday")
REGIME_DIR <- file.path(DATA_ROOT, "output")       # daily regime label CSVs
OUT_DIR    <- file.path(DATA_ROOT, "output", "intraday")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Signal parameters
Z_WINDOW_LONG  <- 120L   # 5 sessions x 24 bars/session
Z_WINDOW_SHORT <-  48L   # 2 sessions
RSI_PERIOD     <-  14L
ATR_PERIOD     <-  14L
ATR_MULT_STOP  <-   1.25
ATR_MULT_TGT   <-   2.0
BE_FRAC        <-   0.5  # break-even triggers at 50% of stop distance

# Liquid session windows (UTC)
SESSION_HOURS <- list(
  CL       = list(start = 13L, end = 20L),   # CME liquid window
  WTCL_LCO = list(start =  8L, end = 16L)    # ICE liquid window
)

# Data split boundaries
TRAIN_END <- as.Date("2023-12-31")
VAL_END   <- as.Date("2024-06-30")
TEST_END  <- as.Date("2026-05-31")

# Products
PRODUCTS <- list(
  CL = list(
    file        = "CL_m1m2_15min.csv",
    regime_file = "regime_labels_CL.csv",
    has_volume  = TRUE
  ),
  WTCL_LCO = list(
    file        = "WTCL_LCO_SPREAD_m1m2_15min.csv",
    regime_file = "regime_labels_LCO.csv",   # use LCO regime labels
    has_volume  = FALSE
  )
)

# Regime-conditional parameters
# Instead of hard exclusions, each regime gets its own threshold, stop multiplier,
# and confidence weight. High threshold + low confidence = effectively inactive
# but still alive if the regime character changes.
REGIME_PARAMS <- data.table(
  regime      = c("Deep-Backwardation", "Backwardation-Deficit",
                  "Stable-Elevated",    "Transition-Tightening",
                  "Contango-Surplus",   "Easing-Contango",
                  "Deep-Contango",      "Warm-Up",
                  "Unknown"),
  # Deep-Backwardation: full signal — only regime with confirmed intraday edge
  # Backwardation-Deficit: raised to 2.5 SD, low confidence — only extreme events
  # All others: 4.0 SD threshold — virtually inactive, door stays open for extremes
  z_thresh    = c(1.0,   2.5,   4.0,   4.0,   4.0,   4.0,   4.0,   99.0,  99.0),
  atr_mult    = c(1.25,  1.00,  0.75,  0.75,  0.75,  0.75,  0.75,   0.0,   0.0),
  confidence  = c(1.0,   0.1,   0.1,   0.1,   0.1,   0.1,   0.1,    0.0,   0.0)
)

# ── 1. Utility functions ------------------------------------------------------

# Wilder RSI
.rsi <- function(x, n = 14L) {
  n   <- as.integer(n)
  out <- rep(NA_real_, length(x))
  if (length(x) <= n) return(out)
  dx  <- diff(x)
  for (i in seq(n, length(dx))) {
    gains  <- pmax(dx[(i - n + 1):i], 0)
    losses <- pmax(-dx[(i - n + 1):i], 0)
    ag     <- mean(gains,  na.rm = TRUE)
    al     <- mean(losses, na.rm = TRUE)
    out[i + 1] <- if (al == 0) 100 else 100 - 100 / (1 + ag / al)
  }
  out
}

# Rolling mean and SD (population, min_obs enforced)
.roll_mean_sd <- function(x, n, min_obs = 10L) {
  len  <- length(x)
  rmean <- rep(NA_real_, len)
  rsd   <- rep(NA_real_, len)
  for (i in seq_len(len)) {
    w <- x[max(1L, i - n + 1L):i]
    w <- w[!is.na(w)]
    if (length(w) >= min_obs) {
      rmean[i] <- mean(w)
      rsd[i]   <- sd(w)
    }
  }
  list(mean = rmean, sd = rsd)
}

# Rolling ATR on spread (uses high - low as the range proxy)
.atr14 <- function(high, low, n = 14L) {
  tr  <- high - low
  out <- rep(NA_real_, length(tr))
  for (i in seq(n, length(tr))) {
    out[i] <- mean(tr[(i - n + 1):i], na.rm = TRUE)
  }
  out
}

# Spearman IC
.ic <- function(signal, fwd_ret) {
  ok <- !is.na(signal) & !is.na(fwd_ret)
  if (sum(ok) < 10L) return(NA_real_)
  cor(signal[ok], fwd_ret[ok], method = "spearman")
}

# ── 2. Feature engineering ----------------------------------------------------

.build_features <- function(dt, has_volume = TRUE) {

  dt <- copy(dt)
  dt[, timestamp := as.POSIXct(timestamp, tz = "UTC")]
  dt[, date      := as.Date(timestamp)]
  dt[, hour_utc  := hour(timestamp)]

  setorder(dt, timestamp)

  # ── Signal 1: Intraday z-score (long window) ──────────────────────────────
  ms_long  <- .roll_mean_sd(dt$close, Z_WINDOW_LONG)
  ms_short <- .roll_mean_sd(dt$close, Z_WINDOW_SHORT)

  dt[, z_long  := (close - ms_long$mean)  / ms_long$sd]
  dt[, z_short := (close - ms_short$mean) / ms_short$sd]

  # ── Signal 2: VWAP deviation (session-level, reset each date) ─────────────
  if (has_volume && "volume" %in% names(dt) && !all(is.na(dt$volume))) {
    dt[, vwap := cumsum(close * volume) / cumsum(volume), by = date]
    dt[, vwap_dev := (close - vwap) / abs(vwap)]
  } else {
    dt[, vwap     := NA_real_]
    dt[, vwap_dev := NA_real_]
  }

  # ── Signal 3: RSI (14-bar) ────────────────────────────────────────────────
  dt[, rsi := .rsi(close, RSI_PERIOD)]

  # ── Signal 4: Session open gap ────────────────────────────────────────────
  # Last close of each session
  session_close <- dt[, .(session_last_close = last(close)), by = date]
  session_close[, prior_session_close := shift(session_last_close, 1L)]

  # First bar of each session
  dt[, bar_rank := seq_len(.N), by = date]
  dt <- merge(dt, session_close[, .(date, prior_session_close)],
              by = "date", all.x = TRUE)
  dt[bar_rank == 1L,
     open_gap := open - prior_session_close]
  dt[bar_rank != 1L, open_gap := NA_real_]
  # Forward-fill gap within session so every bar has the day's gap value
  dt[, open_gap := zoo::na.locf(open_gap, na.rm = FALSE), by = date]

  # ── ATR14 on 15-min bars ──────────────────────────────────────────────────
  dt[, atr14_intra := .atr14(high, low, ATR_PERIOD)]

  # ── Forward returns for IC computation ────────────────────────────────────
  dt[, fwd_ret_1  := shift(close, -1L)  - close]
  dt[, fwd_ret_4  := shift(close, -4L)  - close]
  dt[, fwd_ret_8  := shift(close, -8L)  - close]

  dt
}

# ── 3. Regime label join ------------------------------------------------------

.join_regime <- function(dt, regime_path) {
  if (!file.exists(regime_path)) {
    message("  Regime file not found: ", regime_path,
            " — all bars treated as unclassified")
    dt[, regime := "Unknown"]
    return(dt)
  }
  reg <- fread(regime_path)
  message("  Regime file columns: ", paste(names(reg), collapse = ", "))

  # Detect date column
  date_col <- intersect(c("date", "Date", "DATE"), names(reg))[1]
  # Detect regime label column — try common names from Stage 2 output
  label_col <- intersect(
    c("regime_label", "regime", "label", "modal_label", "final_label"),
    names(reg)
  )[1]

  if (!is.na(date_col) && !is.na(label_col)) {
    reg[, date_key := as.Date(get(date_col))]
    dt <- merge(dt, reg[, .(date = date_key, regime = get(label_col))],
                by = "date", all.x = TRUE)
    message("  Joined on '", date_col, "' + '", label_col,
            "' — matched ", sum(!is.na(dt$regime)), " bars")
  } else {
    message("  Could not detect date/label columns — treating all bars as Unknown")
    message("  Available columns: ", paste(names(reg), collapse = ", "))
    dt[, regime := "Unknown"]
  }
  dt
}

# ── 4. IC analysis (training window only) ------------------------------------

.compute_ic_table <- function(dt) {
  signals  <- c("z_long", "z_short", "vwap_dev", "rsi", "open_gap")
  horizons <- c("fwd_ret_1", "fwd_ret_4", "fwd_ret_8")
  regimes  <- unique(dt$regime)
  regimes  <- regimes[!is.na(regimes)]

  rows <- list()
  for (sig in signals) {
    for (hz in horizons) {
      # Overall IC
      rows[[length(rows) + 1]] <- data.table(
        signal  = sig,
        horizon = hz,
        regime  = "ALL",
        ic      = .ic(dt[[sig]], dt[[hz]]),
        n_bars  = sum(!is.na(dt[[sig]]) & !is.na(dt[[hz]]))
      )
      # Per-regime IC
      for (reg in regimes) {
        sub <- dt[regime == reg]
        rows[[length(rows) + 1]] <- data.table(
          signal  = sig,
          horizon = hz,
          regime  = reg,
          ic      = .ic(sub[[sig]], sub[[hz]]),
          n_bars  = sum(!is.na(sub[[sig]]) & !is.na(sub[[hz]]))
        )
      }
    }
  }
  rbindlist(rows)
}

# Best signal per regime x horizon (highest |IC| with >= 10 bars)
.best_signal_map <- function(ic_table) {
  ic_table[n_bars >= 10L][
    ,
    .SD[which.max(abs(ic))],
    by = .(regime, horizon)
  ][, .(regime, horizon, best_signal = signal, ic)]
}

# ── 5. Entry logic ------------------------------------------------------------

.entry_filter <- function(dt, product) {
  sess  <- SESSION_HOURS[[product]]
  # Time filter: liquid hours only
  dt[, in_session := hour_utc >= sess$start & hour_utc < sess$end]

  # Join regime-conditional parameters
  dt <- merge(dt, REGIME_PARAMS, by = "regime", all.x = TRUE)
  dt[is.na(z_thresh),   z_thresh   := 99.0]
  dt[is.na(atr_mult),   atr_mult   := 0.0 ]
  dt[is.na(confidence), confidence := 0.0 ]

  # Regime ok: confidence > 0 (Warm-Up and Unknown always blocked)
  dt[, regime_ok := confidence > 0 & !is.na(regime)]

  # Signal direction: regime-specific z threshold
  dt[, signal_dir := fcase(
    z_long < -z_thresh,  1L,   # buy signal
    z_long >  z_thresh, -1L,   # sell signal
    rep(TRUE, .N),       0L    # default
  )]

  # First reversion bar: bar where close moves in signal direction
  # i.e. after a sell signal, first bar that closes lower than previous bar
  dt[, prev_close  := shift(close, 1L)]
  dt[, rev_bar := fcase(
    signal_dir ==  1L & close > prev_close,  TRUE,
    signal_dir == -1L & close < prev_close,  TRUE,
    rep(TRUE, .N)                          , FALSE
  )]

  # Entry bar = next bar after first reversion bar where all filters pass
  dt[, entry_trigger := in_session & regime_ok & signal_dir != 0L & rev_bar]

  dt
}

# ── 6. Trade simulation -------------------------------------------------------

.simulate_trades <- function(dt, product, window_label) {

  dt <- copy(dt)
  dt <- .entry_filter(dt, product)

  trades     <- list()
  in_trade   <- FALSE
  entry_px   <- NA_real_
  direction  <- 0L
  stop_px    <- NA_real_
  target_px  <- NA_real_
  be_px      <- NA_real_    # break-even stop level
  be_triggered <- FALSE
  entry_time <- NA
  entry_atr  <- NA_real_
  entry_sig  <- NA_real_
  entry_conf <- NA_real_
  entry_reg  <- NA_character_
  trade_date <- NA

  n <- nrow(dt)

  for (i in seq_len(n)) {
    row <- dt[i]

    # ── Session end: force close any open position ───────────────────────────
    if (in_trade) {
      # Check if this is last bar of the session date
      is_last_bar <- (i == n) ||
        (as.Date(dt[i + 1L, timestamp]) != as.Date(row$timestamp))

      if (is_last_bar) {
        exit_px  <- row$close
        pnl_raw  <- direction * (exit_px - entry_px)
        pnl_net  <- pnl_raw - 0.04  # $0.04/bbl bid-offer cost
        trades[[length(trades) + 1]] <- data.table(
          product    = product,
          window     = window_label,
          entry_time = entry_time,
          exit_time  = row$timestamp,
          trade_date = trade_date,
          direction  = direction,
          entry_px   = entry_px,
          exit_px    = exit_px,
          stop_px    = stop_px,
          target_px  = target_px,
          atr_entry   = entry_atr,
          exit_reason = "session_close",
          pnl_raw     = pnl_raw,
          pnl_net     = pnl_net,
          regime      = entry_reg,
          signal_z    = entry_sig,
          confidence  = entry_conf
        )
        in_trade     <- FALSE
        be_triggered <- FALSE
        next
      }

      px <- row$close

      # ── Stop check ──────────────────────────────────────────────────────────
      stop_hit <- (direction ==  1L && px <= stop_px) ||
                  (direction == -1L && px >= stop_px)

      # ── Target check ────────────────────────────────────────────────────────
      tgt_hit  <- (direction ==  1L && px >= target_px) ||
                  (direction == -1L && px <= target_px)

      # ── Break-even: move stop to entry once 50% of stop distance gained ────
      if (!be_triggered) {
        gain <- direction * (px - entry_px)
        if (!is.na(gain) && gain >= BE_FRAC * abs(entry_px - stop_px)) {
          stop_px      <- entry_px
          be_triggered <- TRUE
        }
      }

      if (stop_hit || tgt_hit) {
        exit_px     <- if (stop_hit) stop_px else target_px
        pnl_raw     <- direction * (exit_px - entry_px)
        pnl_net     <- pnl_raw - 0.04
        exit_reason <- if (stop_hit) "stop" else "target"

        trades[[length(trades) + 1]] <- data.table(
          product     = product,
          window      = window_label,
          entry_time  = entry_time,
          exit_time   = row$timestamp,
          trade_date  = trade_date,
          direction   = direction,
          entry_px    = entry_px,
          exit_px     = exit_px,
          stop_px     = stop_px,
          target_px   = target_px,
          atr_entry   = entry_atr,
          exit_reason = exit_reason,
          pnl_raw     = pnl_raw,
          pnl_net     = pnl_net,
          regime      = entry_reg,
          signal_z    = entry_sig,
          confidence  = entry_conf
        )
        in_trade     <- FALSE
        be_triggered <- FALSE
      }

      next  # don't look for new entry while in trade
    }

    # ── Entry ─────────────────────────────────────────────────────────────────
    if (!in_trade && isTRUE(row$entry_trigger) && !is.na(row$atr14_intra)) {
      atr        <- row$atr14_intra
      if (is.na(atr) || atr <= 0) next

      direction  <- row$signal_dir
      entry_px   <- row$open   # enter on open of bar after trigger
      r_atr_mult <- if (!is.na(row$atr_mult) && row$atr_mult > 0) row$atr_mult else ATR_MULT_STOP
      stop_px    <- entry_px - direction * r_atr_mult   * atr
      target_px  <- entry_px + direction * ATR_MULT_TGT * atr
      be_px      <- entry_px + direction * BE_FRAC * r_atr_mult * atr
      entry_time <- row$timestamp
      entry_atr  <- atr
      entry_sig  <- row$z_long
      entry_reg  <- row$regime
      entry_conf <- if (!is.na(row$confidence)) row$confidence else 0.0
      trade_date <- as.Date(row$timestamp)
      in_trade   <- TRUE
      be_triggered <- FALSE
    }
  }

  if (length(trades) == 0) return(data.table())
  rbindlist(trades)
}

# ── 7. Performance summary ----------------------------------------------------

.summarise <- function(trades) {
  if (nrow(trades) == 0) return(data.table())
  trades[, .(
    n_trades   = .N,
    hit_rate   = mean(pnl_net > 0, na.rm = TRUE),
    avg_pnl    = mean(pnl_net, na.rm = TRUE),
    total_pnl  = sum(pnl_net, na.rm = TRUE),
    avg_win    = mean(pnl_net[pnl_net > 0], na.rm = TRUE),
    avg_loss   = mean(pnl_net[pnl_net < 0], na.rm = TRUE),
    pct_stop   = mean(exit_reason == "stop",          na.rm = TRUE),
    pct_target = mean(exit_reason == "target",        na.rm = TRUE),
    pct_eod    = mean(exit_reason == "session_close", na.rm = TRUE)
  ), by = .(product, window)]
}

# ── 8. Main runner ------------------------------------------------------------

run_intraday_engine <- function(products = c("CL", "WTCL_LCO")) {

  all_trades <- list()
  ic_tables  <- list()

  for (product in products) {
    cfg       <- PRODUCTS[[product]]
    data_path <- file.path(INTRA_DIR, cfg$file)
    reg_path  <- file.path(REGIME_DIR, cfg$regime_file)

    message("\n", strrep("=", 65))
    message("Product: ", product)
    message(strrep("=", 65))

    if (!file.exists(data_path)) {
      warning("Data file not found: ", data_path); next
    }

    # Load and build features
    dt <- fread(data_path)
    message("  Bars loaded: ", format(nrow(dt), big.mark = ","))

    dt <- .build_features(dt, has_volume = cfg$has_volume)
    dt <- .join_regime(dt, reg_path)

    # Assign window labels
    dt[, window := fcase(
      as.Date(timestamp) <= TRAIN_END, "train",
      as.Date(timestamp) <= VAL_END,   "validation",
      as.Date(timestamp) <= TEST_END,  "test",
      rep(TRUE, .N)                  , "oos"
    )]

    # ── IC analysis on training data only ─────────────────────────────────
    message("\n  --- IC Analysis (training window) ---")
    train_dt <- dt[window == "train"]
    ic_tbl   <- .compute_ic_table(train_dt)
    ic_tbl[, product := product]
    ic_tables[[product]] <- ic_tbl

    best_map <- .best_signal_map(ic_tbl)
    message("  Best signals per horizon (ALL regimes):")
    print(best_map[regime == "ALL"][order(horizon)])

    # ── Backtest on validation + test ─────────────────────────────────────
    for (w in c("validation", "test")) {
      message("\n  --- Window: ", w, " ---")
      sub    <- dt[window == w]
      if (nrow(sub) == 0) { message("  No data"); next }

      trades <- .simulate_trades(sub, product, w)
      if (nrow(trades) == 0) {
        message("  No trades generated")
        next
      }

      all_trades[[paste(product, w, sep = "_")]] <- trades

      summ <- .summarise(trades)
      message("  Trades    : ", summ$n_trades)
      message("  Hit rate  : ", round(summ$hit_rate * 100, 1), "%")
      message("  Avg P&L   : ", round(summ$avg_pnl, 4))
      message("  Total P&L : ", round(summ$total_pnl, 4))
      message("  Exit mix  : stop=", round(summ$pct_stop * 100, 1), "%",
              "  target=", round(summ$pct_target * 100, 1), "%",
              "  eod=", round(summ$pct_eod * 100, 1), "%")
    }
  }

  # ── Write outputs ──────────────────────────────────────────────────────────
  message("\n", strrep("=", 65))
  message("Writing outputs to: ", OUT_DIR)

  if (length(all_trades) > 0) {
    all_trades_dt <- rbindlist(all_trades, fill = TRUE)

    # Per product trade files
    for (prod in products) {
      pt <- all_trades_dt[product == prod]
      if (nrow(pt) > 0) {
        fwrite(pt, file.path(OUT_DIR,
               paste0("intraday_trades_", prod, ".csv")))
      }
    }

    # Summary
    summ_all <- .summarise(all_trades_dt)
    fwrite(summ_all, file.path(OUT_DIR, "intraday_summary.csv"))
    message("  intraday_summary.csv written")
    cat("\n")
    print(summ_all)
  }

  # IC table
  if (length(ic_tables) > 0) {
    ic_all <- rbindlist(ic_tables, fill = TRUE)
    fwrite(ic_all, file.path(OUT_DIR, "intraday_signal_ic.csv"))
    message("  intraday_signal_ic.csv written")

    # Print top signals overall
    message("\n  --- Top signals by |IC| (training, ALL regimes, 4-bar horizon) ---")
    top <- ic_all[horizon == "fwd_ret_4" & regime == "ALL"][order(-abs(ic))]
    print(top)
  }

  message("\nDone.")
  invisible(list(trades = all_trades, ic = ic_tables))
}

# ── Run -----------------------------------------------------------------------
# source("R/intraday_signal_engine.R")
# results <- run_intraday_engine(products = c("CL", "WTCL_LCO"))