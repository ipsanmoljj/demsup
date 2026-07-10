# =============================================================================
# intraday_signal_engine_v3.R  — Dual-layer intraday execution engine
#                                 with episode-based refit/validation
#
# v3 adds: episode-split refitting. The signal clusters into two separate
# historical episodes within the ORIGINAL training data (2021-2023) rather
# than firing continuously. v3 lets you fit stop/target on episode1 and
# check generalisation on episode2 — without ever touching the original
# test window (Jul 2024+), which remains reserved for a final check later.
#
# Layer 1 — Daily gate (derived from regime_labels_CL.csv):
#   level_z_126 > SELL_THRESH → SELL signal day (spread too high, expect decline)
#   level_z_126 < -SELL_THRESH → BUY signal day (not seen in current data)
#   Threshold derived from training window (z > 1.0, matching signal_engine.R)
#   No intraday activity on FLAT days.
#
# Layer 2 — Intraday entry trigger (15-min bars):
#   Three triggers tested; IC computed on the fitting window to rank them.
#   Trigger A: VWAP reversion  — spread extended from VWAP, enter on pull-back
#   Trigger B: EMA pullback    — 8/21 EMA trend confirmed, enter on retracement
#   Trigger C: Gap fill        — session gap against signal direction filling back
#
# Stop / Target: independently configurable in ATR units (see overrides below)
# Exit:   stop / target / session close
#
# Signal source: output/regime_labels_CL.csv  (covers 2021-2026)
# Splits:
#   Original  — train (->Dec23) | validation (Jan-Jun24) | test (Jul24+, opened once)
#   Episode   — episode1 (Oct21-Jul22, fit) | episode2 (Jul23-Dec23, check)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

# -----------------------------------------------------------------------------
# 0. Configuration
# -----------------------------------------------------------------------------

PRODUCTS <- list(
  CL = list(
    bars_path    = "data/intraday/CL_m1m2_15min.csv",
    regime_path  = "output/regime_labels_CL.csv",
    liquid_start = 13L,
    liquid_end   = 20L,
    has_volume   = TRUE,
    atr_stop_mult= 1.0
  ),
  WTCL_LCO = list(
    bars_path    = "data/intraday/WTCL_LCO_SPREAD_m1m2_15min.csv",
    regime_path  = "output/regime_labels_CL.csv",   # CL regime drives spread
    liquid_start = 8L,
    liquid_end   = 16L,
    has_volume   = FALSE,
    atr_stop_mult= 1.0
  )
)

# Signal threshold — derived from training window
# Matches signal_engine.R: level_z_126 > 1.0 = SELL
SELL_THRESH <- 1.0

# EMA windows
EMA_FAST <- 8L
EMA_SLOW <- 21L

# ATR
ATR_N <- 14L

# Trigger A — VWAP
VWAP_EXTEND_THRESH <- 0.5    # spread must be > this many ATRs from VWAP
VWAP_REVERT_THRESH <- 0.25   # and now closing back within this

# Trigger B — EMA pullback tolerance (ATR units)
EMA_PULLBACK_TOL <- 0.3

# Trigger C — Gap fill
GAP_MIN_ATR  <- 0.4
GAP_FILL_PCT <- 0.3

# Override hooks — set via run_backtest_v3(vwap_extend=, ema_tol=, gap_min=)
# to tighten/loosen trigger conditions without editing constants each time.
# NULL = use the defaults above.
.VWAP_EXTEND_OVERRIDE <- NULL
.EMA_TOL_OVERRIDE     <- NULL
.GAP_MIN_OVERRIDE     <- NULL

# R:R
RR_RATIO <- 2.0

# Break-even: move stop to entry after 0.5x stop distance in our favour
BE_FRAC <- 0.5

# Splits (ORIGINAL — used for the first round of tuning, already opened once)
TRAIN_END <- as.Date("2023-12-31")
VALID_END <- as.Date("2024-06-30")

# ── RESPLIT of the original test window (Jul 2024 - May 2026) ──────────────
# Parked for now — too thin to fit regime-specific params reliably.
RESPLIT_TRAIN_END <- as.Date("2025-12-31")
RESPLIT_VALID_END <- as.Date("2026-03-31")

# ── EPISODE SPLIT within ORIGINAL TRAINING DATA (2021-2023) ────────────────
# The signal clusters into two separate episodes rather than firing
# continuously. Splitting by episode (not by arbitrary date) gives a fairer
# generalisation check than slicing the tail of one continuous block.
#   Episode 1 (fit):   2021-10-01 to 2022-07-31  (203 signal days)
#   Episode 2 (check):  2023-07-01 to 2023-12-31  (87 signal days)
# This keeps the ORIGINAL test window (Jul 2024+) completely untouched.
EPISODE1_START <- as.Date("2021-10-01")
EPISODE1_END   <- as.Date("2022-07-31")
EPISODE2_START <- as.Date("2023-07-01")
EPISODE2_END   <- as.Date("2023-12-31")

# Minimum volume filter (CL only)
MIN_VOLUME <- 50L

# Stop multiplier override — set externally via run_backtest_v3(atr_stop_mult = X)
# to test different stop widths without editing PRODUCTS config each time
.ATR_STOP_OVERRIDE <- NULL

# Target multiplier override (in ATR units, NOT relative to stop).
# When set, target distance = target_atr_mult * ATR14, independent of stop width.
# When NULL, falls back to the old behaviour: target = RR_RATIO * stop_distance.
.TARGET_ATR_OVERRIDE <- NULL

# -----------------------------------------------------------------------------
# 1. ATR14 — Wilder smoothing
# -----------------------------------------------------------------------------
.atr14 <- function(high, low, close, n = ATR_N) {
  m  <- length(close)
  tr <- numeric(m)
  tr[1] <- high[1] - low[1]
  for (i in 2:m) {
    tr[i] <- max(high[i] - low[i],
                 abs(high[i] - close[i-1]),
                 abs(low[i]  - close[i-1]))
  }
  atr <- numeric(m)
  s   <- which(!is.na(tr) & tr > 0)[1L]
  if (is.na(s) || s + n - 1L > m) return(rep(NA_real_, m))
  atr[s + n - 1L] <- mean(tr[s:(s + n - 1L)])
  for (i in (s + n):m) atr[i] <- (atr[i-1L] * (n-1L) + tr[i]) / n
  atr[atr == 0] <- NA_real_
  atr
}

# -----------------------------------------------------------------------------
# 2. EMA
# -----------------------------------------------------------------------------
.ema <- function(x, n) {
  k   <- 2 / (n + 1)
  out <- rep(NA_real_, length(x))
  s   <- which(!is.na(x))[1L]
  if (is.na(s) || s + n - 1L > length(x)) return(out)
  out[s + n - 1L] <- mean(x[s:(s + n - 1L)], na.rm = TRUE)
  for (i in (s + n):length(x)) out[i] <- x[i] * k + out[i-1L] * (1 - k)
  out
}

# -----------------------------------------------------------------------------
# 3. Feature engineering
# -----------------------------------------------------------------------------
.build_features <- function(dt, has_volume) {
  dt <- copy(dt)
  setorder(dt, timestamp)

  dt[, atr14    := .atr14(high, low, close)]
  dt[, ema_fast := .ema(close, EMA_FAST)]
  dt[, ema_slow := .ema(close, EMA_SLOW)]
  dt[, session_date := as.Date(timestamp)]
  dt[, utc_hour     := hour(timestamp)]

  # VWAP (session-level, reset each date)
  if (has_volume && "volume" %in% names(dt)) {
    dt[, vol_clean := pmax(as.numeric(volume), 0, na.rm = TRUE)]
    dt[, cum_pv := cumsum(close * vol_clean), by = session_date]
    dt[, cum_v  := cumsum(vol_clean),         by = session_date]
    dt[, vwap         := fifelse(cum_v > 0, cum_pv / cum_v, NA_real_)]
    dt[, vwap_dev_atr := fifelse(!is.na(vwap) & !is.na(atr14) & atr14 > 0,
                                 (close - vwap) / atr14, NA_real_)]
    dt[, c("cum_pv","cum_v","vol_clean") := NULL]
  } else {
    dt[, vwap         := NA_real_]
    dt[, vwap_dev_atr := NA_real_]
  }

  # Session open price (first bar of each date)
  dt[, session_open_price := close[1L], by = session_date]

  # Prior session close (last bar of previous date)
  prior_dt <- dt[, .(prior_close = last(close)), by = session_date]
  prior_dt[, next_date := shift(session_date, -1L, type = "lead")]
  dt <- merge(dt,
              prior_dt[!is.na(next_date), .(next_date, prior_close)],
              by.x = "session_date", by.y = "next_date",
              all.x = TRUE)

  # Open gap in ATR units
  dt[, open_gap_atr := fifelse(
    !is.na(prior_close) & !is.na(atr14) & atr14 > 0,
    (session_open_price - prior_close) / atr14,
    NA_real_)]

  # Trigger tag (filled during backtest loop)
  dt[, trigger_used := NA_character_]

  dt[]
}

# -----------------------------------------------------------------------------
# 4. Build daily signal from regime labels
#    Uses level_z_126 directly — no dependency on signal_engine.R
# -----------------------------------------------------------------------------
.build_signal_from_labels <- function(regime_path) {
  if (!file.exists(regime_path)) stop(paste("Regime file not found:", regime_path))

  reg <- fread(regime_path,
               select = c("date","product","regime_label",
                          "level_z_126","in_warmup","confidence_score"))
  reg[, date := as.Date(date)]

  # Derive signal from level_z_126
  reg[, signal := fcase(
    in_warmup == TRUE,              "FLAT",
    is.na(level_z_126),             "FLAT",
    level_z_126 >  SELL_THRESH,     "SELL",
    level_z_126 < -SELL_THRESH,     "BUY",
    default =                       "FLAT"
  )]

  # Label the original split
  reg[, window := fcase(
    date <= TRAIN_END, "train",
    date <= VALID_END, "validation",
    default =          "test"
  )]

  # Label the episode split (only meaningful within original training data)
  reg[, episode_window := fcase(
    date >= EPISODE1_START & date <= EPISODE1_END, "episode1",
    date >= EPISODE2_START & date <= EPISODE2_END, "episode2",
    default = NA_character_
  )]

  reg[]
}

# -----------------------------------------------------------------------------
# 5. IC scanner — training window only
# -----------------------------------------------------------------------------
.compute_ic <- function(bars_train, has_volume) {

  if (nrow(bars_train) < 100L) {
    message("Too few bars for IC computation."); return(NULL)
  }

  # Forward returns (in price points)
  bars_train[, fwd1 := shift(close, -1L, type = "lead") - close]
  bars_train[, fwd4 := shift(close, -4L, type = "lead") - close]
  bars_train[, fwd8 := shift(close, -8L, type = "lead") - close]

  # All signal days here are SELL (negative fwd return = correct)
  # So IC sign: feature positively correlated with -fwd = good

  results <- list()

  # Trigger A: VWAP deviation
  if (has_volume && "vwap_dev_atr" %in% names(bars_train) &&
      !all(is.na(bars_train$vwap_dev_atr))) {
    for (hz in c("fwd1","fwd4","fwd8")) {
      fwd  <- bars_train[[hz]]
      feat <- bars_train$vwap_dev_atr
      ok   <- !is.na(feat) & !is.na(fwd)
      if (sum(ok) > 30L) {
        ic <- cor(feat[ok], -fwd[ok], method = "spearman")
        results[[length(results)+1L]] <- data.table(
          trigger="A_vwap_dev", horizon=hz, IC=round(ic,4), n=sum(ok))
      }
    }
  }

  # Trigger B: EMA spread (fast - slow)
  if (!all(is.na(bars_train$ema_fast))) {
    bars_train[, ema_spread := ema_fast - ema_slow]
    for (hz in c("fwd1","fwd4","fwd8")) {
      fwd  <- bars_train[[hz]]
      feat <- bars_train$ema_spread
      ok   <- !is.na(feat) & !is.na(fwd)
      if (sum(ok) > 30L) {
        ic <- cor(feat[ok], -fwd[ok], method = "spearman")
        results[[length(results)+1L]] <- data.table(
          trigger="B_ema_spread", horizon=hz, IC=round(ic,4), n=sum(ok))
      }
    }
  }

  # Trigger C: open gap
  if (!all(is.na(bars_train$open_gap_atr))) {
    for (hz in c("fwd1","fwd4","fwd8")) {
      fwd  <- bars_train[[hz]]
      feat <- bars_train$open_gap_atr
      ok   <- !is.na(feat) & !is.na(fwd)
      if (sum(ok) > 30L) {
        ic <- cor(feat[ok], -fwd[ok], method = "spearman")
        results[[length(results)+1L]] <- data.table(
          trigger="C_open_gap", horizon=hz, IC=round(ic,4), n=sum(ok))
      }
    }
  }

  if (length(results) == 0L) return(NULL)
  rbindlist(results)[order(trigger, horizon)]
}

# -----------------------------------------------------------------------------
# 6. Backtest loop — one product, one split
# -----------------------------------------------------------------------------
.backtest_product <- function(prod_name, cfg, bars_all, signal_dt, split) {

  cat(sprintf("\n[%s] Backtest split: %s\n", prod_name, split))

  # Filter bars — prod_name is loop variable, not a column name (no collision)
  bars <- bars_all[product == prod_name]
  if (nrow(bars) == 0L) { cat("  No bars found.\n"); return(NULL) }

  is_episode_split <- split %in% c("episode1", "episode2")

  # Filter signal to this split
  if (is_episode_split) {
    sig_split <- signal_dt[episode_window == split & signal != "FLAT",
                           .(date, signal, level_z_126, regime_label)]
  } else {
    sig_split <- signal_dt[window == split & signal != "FLAT",
                           .(date, signal, level_z_126, regime_label)]
  }

  if (nrow(sig_split) == 0L) {
    cat(sprintf("  No active signal days in %s window.\n", split))
    return(NULL)
  }

  # Filter bars to split dates
  if (is_episode_split) {
    ep_start <- if (split == "episode1") EPISODE1_START else EPISODE2_START
    ep_end   <- if (split == "episode1") EPISODE1_END   else EPISODE2_END
    bars <- bars[session_date >= ep_start & session_date <= ep_end]
  } else {
    if (split == "train")      bars <- bars[session_date <= TRAIN_END]
    if (split == "validation") bars <- bars[session_date > TRAIN_END &
                                            session_date <= VALID_END]
    if (split == "test")       bars <- bars[session_date > VALID_END]
  }

  # Liquid hours + volume filter
  bars <- bars[utc_hour >= cfg$liquid_start & utc_hour < cfg$liquid_end]
  if (cfg$has_volume) bars <- bars[is.na(volume) | as.integer(volume) >= MIN_VOLUME]

  # Keep only signal days
  bars <- bars[session_date %in% sig_split$date]

  if (nrow(bars) < 50L) { cat("  Insufficient bars.\n"); return(NULL) }

  cat(sprintf("  %d bars | %d signal days | %s → %s\n",
              nrow(bars), length(unique(bars$session_date)),
              min(bars$session_date), max(bars$session_date)))

  # Merge signal info
  bars <- merge(bars, sig_split,
                by.x = "session_date", by.y = "date",
                all.x = TRUE)

  # IC scan on the fitting window (original train OR episode1)
  if (split == "train" || split == "episode1") {
    cat(sprintf("\n  [%s] IC scan on %s signal days:\n", prod_name, split))
    ic_tbl <- .compute_ic(copy(bars), cfg$has_volume)
    if (!is.null(ic_tbl)) {
      print(ic_tbl)
      best <- ic_tbl[which.max(IC)]
      cat(sprintf("\n  Best trigger: %s at horizon %s (IC = %.4f)\n\n",
                  best$trigger, best$horizon, best$IC))
    } else {
      cat("  Could not compute IC.\n\n")
    }
  }

  # ---- Trade simulation ----
  trades      <- list()
  in_trade    <- FALSE
  entry_price <- NA_real_
  stop_price  <- NA_real_
  tgt_price   <- NA_real_
  direction   <- 0L
  entry_bar   <- NA_integer_
  be_done     <- FALSE

  for (i in seq_len(nrow(bars))) {
    row <- bars[i]
    if (is.na(row$atr14) || row$atr14 <= 0) next

    day_dir <- if (!is.na(row$signal) && row$signal == "SELL") -1L else
               if (!is.na(row$signal) && row$signal == "BUY")   1L else 0L
    if (day_dir == 0L) next

    atr <- row$atr14

    # ── Manage open trade ──────────────────────────────────────────────────
    if (in_trade) {

      is_session_end <- (i == nrow(bars)) ||
        (bars[i+1L]$session_date != row$session_date)

      if (is_session_end) {
        pnl <- (row$close - entry_price) * direction
        trades[[length(trades)+1L]] <- list(
          product      = prod_name,
          entry_time   = bars[entry_bar]$timestamp,
          exit_time    = row$timestamp,
          direction    = direction,
          entry_price  = entry_price,
          exit_price   = row$close,
          exit_reason  = "session_close",
          pnl_pts      = pnl,
          trigger_used = bars[entry_bar]$trigger_used,
          regime       = bars[entry_bar]$regime_label,
          level_z      = row$level_z_126,
          split        = split
        )
        in_trade <- FALSE
        next
      }

      # Break-even
      stop_dist <- abs(entry_price - stop_price)
      if (!be_done && (row$close - entry_price) * direction >= BE_FRAC * stop_dist) {
        stop_price <- entry_price
        be_done    <- TRUE
      }

      # Stop / target
      hit_stop <- (direction ==  1L && row$low  <= stop_price) ||
                  (direction == -1L && row$high >= stop_price)
      hit_tgt  <- (direction ==  1L && row$high >= tgt_price) ||
                  (direction == -1L && row$low  <= tgt_price)

      if (hit_stop || hit_tgt) {
        ep  <- if (hit_tgt) tgt_price else stop_price
        pnl <- (ep - entry_price) * direction
        trades[[length(trades)+1L]] <- list(
          product      = prod_name,
          entry_time   = bars[entry_bar]$timestamp,
          exit_time    = row$timestamp,
          direction    = direction,
          entry_price  = entry_price,
          exit_price   = ep,
          exit_reason  = if (hit_tgt) "target" else "stop",
          pnl_pts      = pnl,
          trigger_used = bars[entry_bar]$trigger_used,
          regime       = bars[entry_bar]$regime_label,
          level_z      = row$level_z_126,
          split        = split
        )
        in_trade <- FALSE
      }
      next
    }

    # ── Look for entry ─────────────────────────────────────────────────────
    trigger_fired <- NA_character_

    eff_vwap_extend <- if (!is.null(.VWAP_EXTEND_OVERRIDE)) .VWAP_EXTEND_OVERRIDE else VWAP_EXTEND_THRESH
    eff_ema_tol      <- if (!is.null(.EMA_TOL_OVERRIDE))     .EMA_TOL_OVERRIDE     else EMA_PULLBACK_TOL
    eff_gap_min      <- if (!is.null(.GAP_MIN_OVERRIDE))     .GAP_MIN_OVERRIDE     else GAP_MIN_ATR

    # Trigger A — VWAP reversion
    if (cfg$has_volume && !is.na(row$vwap_dev_atr)) {
      vd <- row$vwap_dev_atr
      if (abs(vd) > eff_vwap_extend && i > 1L) {
        prior_vd <- bars[i-1L]$vwap_dev_atr
        if (!is.na(prior_vd)) {
          reverting <- (day_dir == -1L && prior_vd > vd && vd > 0) ||
                       (day_dir ==  1L && prior_vd < vd && vd < 0)
          if (reverting) trigger_fired <- "A_vwap_reversion"
        }
      }
    }

    # Trigger B — EMA pullback
    if (is.na(trigger_fired) &&
        !is.na(row$ema_fast) && !is.na(row$ema_slow)) {
      trend_ok  <- (day_dir == -1L && row$ema_fast > row$ema_slow) ||
                   (day_dir ==  1L && row$ema_fast < row$ema_slow)
      near_ema  <- abs(row$close - row$ema_fast) <= eff_ema_tol * atr
      if (trend_ok && near_ema) trigger_fired <- "B_ema_pullback"
    }

    # Trigger C — Gap fill
    if (is.na(trigger_fired) && !is.na(row$open_gap_atr) &&
        !is.na(row$prior_close)) {
      gap <- row$open_gap_atr
      gap_against <- (day_dir == -1L && gap < -eff_gap_min) ||
                     (day_dir ==  1L && gap >  eff_gap_min)
      if (gap_against) {
        gap_raw       <- row$session_open_price - row$prior_close
        filled        <- (row$close - row$session_open_price) * (-sign(gap_raw))
        fill_pct      <- filled / abs(gap_raw)
        if (!is.na(fill_pct) && fill_pct >= GAP_FILL_PCT)
          trigger_fired <- "C_gap_fill"
      }
    }

    if (is.na(trigger_fired)) next

    # Tag bar with trigger and enter
    bars[i, trigger_used := trigger_fired]

    in_trade     <- TRUE
    direction    <- day_dir
    entry_price  <- row$close
    eff_stop_mult <- if (!is.null(.ATR_STOP_OVERRIDE)) .ATR_STOP_OVERRIDE else cfg$atr_stop_mult
    stop_price   <- entry_price - direction * eff_stop_mult * atr
    # Target sizing: independent ATR-based target if override set,
    # otherwise fall back to RR_RATIO * stop distance (legacy behaviour)
    eff_target_dist <- if (!is.null(.TARGET_ATR_OVERRIDE)) {
      .TARGET_ATR_OVERRIDE * atr
    } else {
      RR_RATIO * eff_stop_mult * atr
    }
    tgt_price    <- entry_price + direction * eff_target_dist
    be_done      <- FALSE
    entry_bar    <- i
  }

  if (length(trades) == 0L) {
    cat(sprintf("  [%s] No trades generated.\n", prod_name))
    return(NULL)
  }

  out <- rbindlist(lapply(trades, as.data.table))

  cat(sprintf("  Trades: %d | Hit: %.1f%% | P&L: %+.3f pts\n",
              nrow(out), 100 * mean(out$pnl_pts > 0), sum(out$pnl_pts)))

  trig <- out[, .(trades=.N,
                  hit=round(100*mean(pnl_pts>0),1),
                  pnl=round(sum(pnl_pts),3)),
              by=trigger_used][order(-trades)]
  cat("  By trigger:\n"); print(trig)

  out[]
}

# -----------------------------------------------------------------------------
# 7. Load and feature-engineer all bar data
# -----------------------------------------------------------------------------
.load_all_bars <- function() {
  bar_list <- list()
  for (pn in names(PRODUCTS)) {
    cfg  <- PRODUCTS[[pn]]
    path <- cfg$bars_path
    if (!file.exists(path)) { warning("Not found: ", path); next }
    dt <- fread(path)
    ts_col <- intersect(c("timestamp","Timestamp","TIMESTAMP","time"), names(dt))[1L]
    setnames(dt, ts_col, "timestamp")
    dt[, timestamp := as.POSIXct(timestamp, tz = "UTC")]
    if (!"volume" %in% names(dt)) dt[, volume := NA_integer_]
    for (col in c("open","high","low","close"))
      if (col %in% names(dt)) dt[, (col) := as.numeric(get(col))]
    dt[, product := pn]
    cat(sprintf("[%s] Building features on %d bars...\n", pn, nrow(dt)))
    dt <- .build_features(dt, cfg$has_volume)
    bar_list[[pn]] <- dt
  }
  rbindlist(bar_list, fill = TRUE)
}

# -----------------------------------------------------------------------------
# 8. Summary printer
# -----------------------------------------------------------------------------
.print_summary <- function(all_trades) {
  cat("\n", strrep("=", 60), "\n")
  cat("DUAL-LAYER ENGINE v2 — BACKTEST SUMMARY\n")
  cat(strrep("=", 60), "\n\n")

  for (sp in c("train","validation","test","episode1","episode2")) {
    sub <- all_trades[split == sp]
    if (nrow(sub) == 0L) next
    cat(sprintf("--- %s ---\n", toupper(sp)))
    for (pn in unique(sub$product)) {
      p <- sub[product == pn]
      cat(sprintf("  %-12s  Trades:%3d  Hit:%5.1f%%  P&L:%+7.3f pts\n",
                  pn, nrow(p), 100*mean(p$pnl_pts>0), sum(p$pnl_pts)))
    }
    cat("\n")
  }

  detail_split <- if ("episode2" %in% all_trades$split) "episode2" else "test"
  detail <- all_trades[split == detail_split]
  if (nrow(detail) > 0L) {
    cat(sprintf("%s detail — by trigger:\n", toupper(detail_split)))
    print(detail[, .(trades=.N,
                   hit=round(100*mean(pnl_pts>0),1),
                   pnl=round(sum(pnl_pts),3)),
               by=.(product,trigger_used)][order(product,-trades)])
    cat(sprintf("\n%s detail — by exit reason:\n", toupper(detail_split)))
    print(detail[, .(trades=.N,
                   hit=round(100*mean(pnl_pts>0),1),
                   pnl=round(sum(pnl_pts),3)),
               by=.(product,exit_reason)][order(product,-trades)])
  }
}

# -----------------------------------------------------------------------------
# 9. Main entry point
# -----------------------------------------------------------------------------
run_backtest_v3 <- function(splits = c("train","validation","test"),
                            atr_stop_mult  = NULL,
                            target_atr_mult = NULL,
                            vwap_extend   = NULL,
                            ema_tol       = NULL,
                            gap_min       = NULL) {

  .ATR_STOP_OVERRIDE    <<- atr_stop_mult
  .TARGET_ATR_OVERRIDE  <<- target_atr_mult
  .VWAP_EXTEND_OVERRIDE <<- vwap_extend
  .EMA_TOL_OVERRIDE     <<- ema_tol
  .GAP_MIN_OVERRIDE     <<- gap_min

  cat("Loading bars and building features...\n")
  bars_all <- .load_all_bars()

  all_trades <- list()

  for (pn in names(PRODUCTS)) {
    cfg <- PRODUCTS[[pn]]
    cat(sprintf("\n[%s] Loading signal from regime labels...\n", pn))
    signal_dt <- .build_signal_from_labels(cfg$regime_path)

    # Print signal day counts per split
    if (any(splits %in% c("episode1","episode2"))) {
      ep_counts <- signal_dt[!is.na(episode_window) & signal != "FLAT",
                             .N, by = episode_window]
      print(ep_counts)
    } else {
      sig_counts <- signal_dt[signal != "FLAT", .N, by = window]
      print(sig_counts)
    }

    for (sp in splits) {
      result <- .backtest_product(pn, cfg, bars_all, signal_dt, sp)
      if (!is.null(result)) all_trades[[length(all_trades)+1L]] <- result
    }
  }

  if (length(all_trades) == 0L) {
    cat("No trades produced.\n"); return(invisible(NULL))
  }

  all_trades_dt <- rbindlist(all_trades, fill = TRUE)
  .print_summary(all_trades_dt)
  invisible(all_trades_dt)
}

# =============================================================================
# USAGE
# =============================================================================
# source("R/intraday_signal_engine_v2.R")
#
# EPISODE-BASED REFIT (uses ONLY the original training data, 2021-2023;
# the original test window Jul-2024+ remains completely untouched):
#
# # Step 1 — refit stop/target on episode1 (Oct 2021 - Jul 2022):
# r_fit <- run_backtest_v3(splits = "episode1", atr_stop_mult = 2.40, target_atr_mult = 3.00)
#
# # Step 2 — check the SAME params on episode2 (Jul 2023 - Dec 2023), unseen:
# r_check <- run_backtest_v3(splits = "episode2", atr_stop_mult = 2.40, target_atr_mult = 3.00)
#
# # Compare:
# cat("Episode1 (fit):   trades=", nrow(r_fit),   " hit=", round(100*mean(r_fit$pnl_pts>0),1),
#     " pnl=", round(sum(r_fit$pnl_pts),3), "\n")
# cat("Episode2 (check): trades=", nrow(r_check), " hit=", round(100*mean(r_check$pnl_pts>0),1),
#     " pnl=", round(sum(r_check$pnl_pts),3), "\n")
#
# Original test window (Jul 2024+) remains available, untouched, for later.
# =============================================================================