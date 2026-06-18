# R/signal_engine.R
# -----------------
# Complete signal engine for M1M2 calendar spread trading.
#
# Signal source:  level_z_126 from regime_classifier.R
# Stop loss:      ATR-based (product-specific multipliers)
# Trailing stop:  activates after break-even threshold is reached
# ATR filter:     volatility gate — each product has its own ATR condition
# Regime gating:  excludes regimes where signal is systematically wrong
# EV calculation: per-trade expected value using realised hit rate and R:R
#
# STABILITY FINDINGS (from test window analysis):
#   CL:  Both vol regimes work — no ATR filter needed
#   LCO: Signal breaks in extreme vol — only trade ATR14 < 2x training median
#   HO:  Signal collapses in high vol — only trade ATR14 < p75 training ATR
#   LGO: Signal only works IN high vol — only trade ATR14 > p75 training ATR
#
# DATA SPLIT:
#   Training   : warm-up → Dec 2023
#   Validation : Jan 2024 → Jun 2024
#   Test       : Jul 2024 → May 2026 (final performance — opened once)
#
# Usage:
#   source("R/signal_engine.R")
#   results <- run_signal_engine(products = c("CL","LCO","HO","LGO"))
#   print(results$summary)

suppressPackageStartupMessages({
  library(data.table)
  library(zoo)
})

# ── Split boundaries ───────────────────────────────────────────────────────────
TRAIN_END <- as.Date("2023-12-31")
VAL_END   <- as.Date("2024-06-30")

# ── Per-product configuration ──────────────────────────────────────────────────
#
# atr_multiplier : stop distance = ATR14 × multiplier
# be_move_frac   : BE threshold = 1/(1+R:R) per EV document
# atr_filter_min : only trade when ATR14 >= this (NULL = no lower bound)
# atr_filter_max : only trade when ATR14 <= this (NULL = no upper bound)
# unit           : native price unit
# bid_offer      : round-trip transaction cost
# excluded_regimes: regimes excluded from signal generation
#
# ATR filter thresholds derived from TRAINING window ATR14 distributions:
#   CL  training: median=0.0510, p75=0.1409, 2×median=0.1020
#   LCO training: median=0.0690, p75=0.1547, 2×median=0.1381
#   HO  training: median=0.0063, p75=0.0189, 2×median=0.0126
#   LGO training: median=0.9763, p75=3.1000, 2×median=1.9526

PRODUCT_CONFIG <- list(

  CL = list(
    atr_multiplier   = 2.5,
    atr_period       = 14L,
    be_move_frac     = 0.48,       # 1/(1+R:R=1.05)
    atr_filter_min   = NULL,       # no lower bound — both vol regimes work
    atr_filter_max   = NULL,       # no upper bound
    unit             = "$/bbl",
    bid_offer        = 0.04,
    excluded_regimes = c("Warm-Up","Transition-Tightening","Transition-Loosening")
  ),

  LCO = list(
    atr_multiplier   = 2.5,
    atr_period       = 14L,
    be_move_frac     = 0.56,       # 1/(1+R:R=0.78)
    atr_filter_min   = NULL,
    atr_filter_max   = 0.1381,     # 2x training median — exclude extreme vol
    unit             = "$/bbl",    # signal breaks in Feb 2026 crisis
    bid_offer        = 0.04,
    excluded_regimes = c("Warm-Up","Transition-Tightening","Transition-Loosening",
                          "Deep-Backwardation")  # 48.7% hit — not tradeable
  ),

  HO = list(
    atr_multiplier   = 2.5,
    atr_period       = 14L,
    be_move_frac     = 0.57,       # 1/(1+R:R=0.75)
    atr_filter_min   = NULL,
    atr_filter_max   = 0.0189,     # p75 training ATR — exclude high vol
    unit             = "$/gal",    # hit rate 38.5% in high-vol → exclude
    bid_offer        = 0.002,
    excluded_regimes = c("Warm-Up","Transition-Tightening","Transition-Loosening",
                          "Easing-Backwardation")  # 0% hit rate
  ),

  LGO = list(
    atr_multiplier   = 3.0,        # wider stop — structurally high vol
    atr_period       = 14L,
    be_move_frac     = 0.56,       # 1/(1+R:R=0.79)
    atr_filter_min   = 3.1000,     # p75 training ATR — only trade in high vol
    atr_filter_max   = NULL,       # signal works ONLY when vol is elevated
    unit             = "$/mt",
    bid_offer        = 0.50,
    excluded_regimes = c("Warm-Up","Transition-Tightening","Transition-Loosening",
                          "Easing-Backwardation",   # 0% hit rate
                          "Deep-Contango")           # 13.6% hit rate
  )
)

# Tuned thresholds from validation window
SIGNAL_THRESHOLDS <- c(CL=1.00, LCO=1.50, HO=1.75, LGO=1.75)

# ── Compute ATR14 ──────────────────────────────────────────────────────────────
.compute_atr <- function(m1m2_vec, period = 14L) {
  tr <- abs(c(NA, diff(m1m2_vec)))
  zoo::rollmean(tr, period, fill=NA, align="right")
}

# ── Simulate single trade with ATR stop + trailing stop ───────────────────────
.simulate_trade <- function(entry_price, direction, atr_at_entry,
                             future_prices, config) {
  stop_dist       <- atr_at_entry * config$atr_multiplier
  stop_loss       <- entry_price - direction * stop_dist
  be_move         <- stop_dist * config$be_move_frac
  be_level        <- entry_price + direction * be_move
  trailing_active <- FALSE
  best_price      <- entry_price
  exit_price      <- NA_real_
  exit_reason     <- NA_character_
  bars_held       <- 0L

  for (i in seq_along(future_prices)) {
    px <- future_prices[i]
    if (is.na(px)) next
    bars_held <- i

    # Update best price
    if (direction ==  1L && px > best_price) best_price <- px
    if (direction == -1L && px < best_price) best_price <- px

    # Break-even trigger
    be_triggered <- (direction ==  1L && px >= be_level) ||
                    (direction == -1L && px <= be_level)
    if (be_triggered && !trailing_active) {
      stop_loss       <- entry_price
      trailing_active <- TRUE
    }

    # Trailing stop (only moves in favourable direction)
    if (trailing_active) {
      trail_stop <- best_price - direction * stop_dist
      if (direction ==  1L) stop_loss <- max(stop_loss, trail_stop)
      if (direction == -1L) stop_loss <- min(stop_loss, trail_stop)
    }

    # Stop hit check
    stop_hit <- (direction ==  1L && px <= stop_loss) ||
                (direction == -1L && px >= stop_loss)
    if (stop_hit) {
      exit_price  <- stop_loss
      exit_reason <- if (trailing_active) "trailing_stop" else "stop_loss"
      break
    }

    # Max hold: 21 days
    if (i == min(21L, length(future_prices))) {
      exit_price  <- px
      exit_reason <- "time_exit"
      break
    }
  }

  if (is.na(exit_price)) {
    exit_price  <- tail(future_prices[!is.na(future_prices)], 1)
    exit_reason <- "end_of_data"
  }

  pnl_gross <- direction * (exit_price - entry_price)
  pnl_net   <- pnl_gross - config$bid_offer

  list(
    exit_price      = exit_price,
    exit_reason     = exit_reason,
    bars_held       = bars_held,
    pnl_gross       = pnl_gross,
    pnl_net         = pnl_net,
    stop_dist       = stop_dist,
    be_triggered    = trailing_active
  )
}

# ── EV calculation per document ────────────────────────────────────────────────
# EV       = (WR × R_profit) - ((1-WR) × R_loss)
# EV_with_BE = WR × (1-P_BE) × R_profit - (1-WR) × R_loss
.compute_ev <- function(trades_dt) {
  if (nrow(trades_dt) == 0)
    return(list(ev=NA, ev_be=NA, rr=NA, wr=NA,
                r_profit=NA, r_loss=NA))

  wins   <- trades_dt[pnl_net > 0,  pnl_net]
  losses <- trades_dt[pnl_net <= 0, pnl_net]
  wr       <- length(wins) / nrow(trades_dt)
  r_profit <- if (length(wins)   > 0) mean(wins)        else 0
  r_loss   <- if (length(losses) > 0) mean(abs(losses)) else 0
  rr       <- if (r_loss > 0) r_profit / r_loss else NA_real_
  ev       <- wr * r_profit - (1 - wr) * r_loss
  p_be     <- if ("be_triggered" %in% names(trades_dt))
                mean(trades_dt$be_triggered, na.rm=TRUE) else 0.3
  ev_be    <- wr * (1 - p_be) * r_profit - (1 - wr) * r_loss

  list(ev       = round(ev,      4),
       ev_be    = round(ev_be,   4),
       rr       = round(ifelse(is.na(rr),0,rr), 3),
       wr       = round(wr,      3),
       r_profit = round(r_profit,4),
       r_loss   = round(r_loss,  4))
}

# ── Stability metrics across sub-periods ──────────────────────────────────────
.stability_summary <- function(trades_dt) {
  trades_dt[, period := cut(date,
    breaks = c(as.Date("2024-07-01"), as.Date("2024-12-31"),
               as.Date("2025-06-30"), as.Date("2025-12-31"),
               as.Date("2026-12-31")),
    labels = c("H2-2024","H1-2025","H2-2025","2026"),
    include.lowest = TRUE
  )]

  cat(sprintf("    %-10s %5s %7s %8s %8s %8s\n",
              "Period","N","Hit%","Mean","SD","MaxDD"))
  cat("    ", strrep("-", 52), "\n")

  hit_rates <- numeric()
  for (p in c("H2-2024","H1-2025","H2-2025","2026")) {
    sub <- trades_dt[period == p]
    if (nrow(sub) < 3) next
    cum <- cumsum(sub$pnl_net)
    dd  <- min(cum - cummax(cum))
    hr  <- mean(sub$pnl_net > 0) * 100
    hit_rates <- c(hit_rates, hr)
    cat(sprintf("    %-10s %5d %7.1f %8.4f %8.4f %8.4f\n",
                p, nrow(sub), hr,
                mean(sub$pnl_net, na.rm=TRUE),
                sd(sub$pnl_net, na.rm=TRUE), dd))
  }

  # Coefficient of variation of hit rates = stability measure
  cv_hit <- if (length(hit_rates) > 1)
    round(sd(hit_rates) / mean(hit_rates) * 100, 1) else NA
  cat(sprintf("    Hit rate CV across periods: %.1f%% (lower = more stable)\n",
              ifelse(is.na(cv_hit), 0, cv_hit)))
}

# ═════════════════════════════════════════════════════════════════════════════
# MAIN SIGNAL ENGINE
# ═════════════════════════════════════════════════════════════════════════════

run_signal_engine <- function(products   = c("CL","LCO","HO","LGO"),
                               output_dir = "output",
                               verbose    = TRUE) {

  cat("\n", strrep("=", 65), "\n")
  cat("SIGNAL ENGINE — ATR Stop + Trailing Stop + Vol Gate + EV\n")
  cat(strrep("=", 65), "\n\n")

  all_results  <- list()
  summary_rows <- list()

  for (prod in products) {
    cat(strrep("-", 65), "\n")
    cat("Product:", prod, "\n")
    cat(strrep("-", 65), "\n\n")

    cfg    <- PRODUCT_CONFIG[[prod]]
    thresh <- SIGNAL_THRESHOLDS[prod]

    # classify_regimes() (regime_classifier.R) now defaults to writing
    # regime_labels_<product>.csv under output/<product>/ rather than
    # directly under output/ — fixed 2026-06-17 alongside the cross-product
    # model-file collision bug (see regime_classifier.R's classify_regimes
    # docstring). Check the new per-product path first; fall back to the old
    # shared-output path for anyone running this against pre-fix CSVs that
    # haven't been regenerated yet, so this doesn't silently SKIP a product
    # just because the directory convention moved.
    labels_path_new <- file.path(output_dir, prod,
                                  paste0("regime_labels_", prod, ".csv"))
    labels_path_old <- file.path(output_dir,
                                  paste0("regime_labels_", prod, ".csv"))
    labels_path <- if (file.exists(labels_path_new)) labels_path_new else labels_path_old

    if (!file.exists(labels_path)) {
      cat("  SKIP: regime labels not found (checked", labels_path_new, "and", labels_path_old, ")\n\n"); next
    }

    dt <- fread(labels_path)
    dt[, date := as.Date(date)]
    setorder(dt, date)

    dt[, window := fcase(
      in_warmup == TRUE,   "warmup",
      date <= TRAIN_END,   "train",
      date <= VAL_END,     "validation",
      default =            "test"
    )]

    dt[, atr14 := .compute_atr(M1M2, cfg$atr_period)]

    # ── ATR filter ─────────────────────────────────────────────────────────
    atr_ok <- rep(TRUE, nrow(dt))
    if (!is.null(cfg$atr_filter_min))
      atr_ok <- atr_ok & (!is.na(dt$atr14) & dt$atr14 >= cfg$atr_filter_min)
    if (!is.null(cfg$atr_filter_max))
      atr_ok <- atr_ok & (!is.na(dt$atr14) & dt$atr14 <= cfg$atr_filter_max)
    dt[, atr_filter_pass := atr_ok]

    # ── Raw signal (before vol filter) ─────────────────────────────────────
    dt[, signal := fcase(
      in_warmup == TRUE,                           "FLAT",
      window == "train",                           "FLAT",
      regime_label %in% cfg$excluded_regimes,      "FLAT",
      is.na(level_z_126),                          "FLAT",
      !atr_filter_pass,                            "FLAT",
      level_z_126 < -thresh,                       "BUY",
      level_z_126 >  thresh,                       "SELL",
      default =                                    "FLAT"
    )]

    # Config summary
    atr_gate <- if (!is.null(cfg$atr_filter_min) && !is.null(cfg$atr_filter_max))
      sprintf("%.4f ≤ ATR14 ≤ %.4f", cfg$atr_filter_min, cfg$atr_filter_max)
    else if (!is.null(cfg$atr_filter_max))
      sprintf("ATR14 ≤ %.4f (low-vol only)", cfg$atr_filter_max)
    else if (!is.null(cfg$atr_filter_min))
      sprintf("ATR14 ≥ %.4f (high-vol only)", cfg$atr_filter_min)
    else "none"

    excl_display <- cfg$excluded_regimes[
      !grepl("Warm-Up|Transition", cfg$excluded_regimes)]

    cat(sprintf("  Threshold:   %.2f\n", thresh))
    cat(sprintf("  ATR stop:    %.1fx ATR14  |  BE frac: %.2f\n",
                cfg$atr_multiplier, cfg$be_move_frac))
    cat(sprintf("  Vol gate:    %s\n", atr_gate))
    if (length(excl_display) > 0)
      cat(sprintf("  Excl regime: %s\n", paste(excl_display, collapse=", ")))
    cat("\n")

    # ── Trade simulation ────────────────────────────────────────────────────
    signal_idx <- which(dt$signal != "FLAT")
    trades     <- list()

    for (idx in signal_idx) {
      if (is.na(dt$atr14[idx]) || dt$atr14[idx] <= 0) next
      direction   <- if (dt$signal[idx] == "BUY") 1L else -1L
      entry_price <- dt$M1M2[idx]
      atr_entry   <- dt$atr14[idx]
      future_idx  <- seq(idx + 1L, min(idx + 21L, nrow(dt)))
      future_px   <- dt$M1M2[future_idx]
      if (length(future_px) < 2) next

      trade <- .simulate_trade(entry_price, direction, atr_entry,
                                future_px, cfg)

      trades[[length(trades) + 1]] <- data.table(
        date          = dt$date[idx],
        window        = dt$window[idx],
        product       = prod,
        regime        = dt$regime_label[idx],
        signal        = dt$signal[idx],
        level_z       = round(dt$level_z_126[idx], 3),
        entry_price   = round(entry_price, 4),
        exit_price    = round(trade$exit_price, 4),
        exit_reason   = trade$exit_reason,
        bars_held     = trade$bars_held,
        stop_dist     = round(trade$stop_dist, 4),
        atr14         = round(atr_entry, 4),
        be_triggered  = trade$be_triggered,
        pnl_gross     = round(trade$pnl_gross, 4),
        pnl_net       = round(trade$pnl_net, 4)
      )
    }

    if (length(trades) == 0) { cat("  No trades generated\n\n"); next }
    trades_dt <- rbindlist(trades)

    # ── Performance by window ───────────────────────────────────────────────
    for (win in c("validation","test")) {
      wt <- trades_dt[window == win]
      if (nrow(wt) == 0) next

      ev      <- .compute_ev(wt)
      stops_n <- sum(wt$exit_reason == "stop_loss")
      trail_n <- sum(wt$exit_reason == "trailing_stop")
      be_n    <- sum(wt$be_triggered)
      cum_pnl <- cumsum(wt[order(date), pnl_net])
      max_dd  <- min(cum_pnl - cummax(cum_pnl))

      cat(sprintf("  [%s]\n", toupper(win)))
      cat(sprintf("    Trades:       %d  (W:%d / L:%d)\n",
                  nrow(wt), sum(wt$pnl_net>0), sum(wt$pnl_net<=0)))
      cat(sprintf("    Hit rate:     %.1f%%\n",     ev$wr*100))
      cat(sprintf("    R:R ratio:    %.2f\n",       ev$rr))
      cat(sprintf("    EV/trade:     %+.4f %s\n",  ev$ev,    cfg$unit))
      cat(sprintf("    EV with BE:   %+.4f %s\n",  ev$ev_be, cfg$unit))
      cat(sprintf("    Mean PnL net: %+.4f %s\n",
                  mean(wt$pnl_net,na.rm=TRUE), cfg$unit))
      cat(sprintf("    Total PnL:    %+.4f %s\n",
                  sum(wt$pnl_net,na.rm=TRUE), cfg$unit))
      cat(sprintf("    Max drawdown: %+.4f %s\n",  max_dd, cfg$unit))
      cat(sprintf("    Stops: %d hard  %d trailing  |  BE: %d (%.0f%%)\n",
                  stops_n, trail_n, be_n, be_n/nrow(wt)*100))
      cat(sprintf("    Avg hold:     %.1f bars\n",
                  mean(wt$bars_held,na.rm=TRUE)))

      if (win == "test") {
        cat("\n    Regime breakdown:\n")
        cat(sprintf("    %-25s %5s %6s %8s %8s\n",
                    "Regime","N","Hit%","EV_net","Tot_PnL"))
        cat("    ", strrep("-", 57), "\n")
        rs <- wt[, .(n=.N, hit=round(mean(pnl_net>0)*100,1),
                      ev=round(mean(pnl_net,na.rm=TRUE),4),
                      tot=round(sum(pnl_net,na.rm=TRUE),4)),
                  by=regime][order(-tot)]
        for (i in seq_len(nrow(rs)))
          cat(sprintf("    %-25s %5d %6.1f %8.4f %8.4f\n",
                      rs$regime[i], rs$n[i], rs$hit[i],
                      rs$ev[i], rs$tot[i]))

        cat("\n    Stability across sub-periods:\n")
        .stability_summary(wt[order(date)])
      }
      cat("\n")
    }

    # ── Live signal ─────────────────────────────────────────────────────────
    last <- tail(dt[!in_warmup & !is.na(level_z_126)], 1)
    live_signal <- NULL
    if (nrow(last) > 0) {
      cat(sprintf("  LIVE SIGNAL (%s):\n", format(last$date)))
      cat(sprintf("    Regime:    %s\n",      last$regime_label))
      cat(sprintf("    M1M2:      %.4f %s\n", last$M1M2, cfg$unit))
      cat(sprintf("    Z-score:   %+.3f\n",   last$level_z_126))
      cat(sprintf("    ATR14:     %.4f  [gate: %s]\n",
                  ifelse(is.na(last$atr14),0,last$atr14), atr_gate))
      cat(sprintf("    Vol gate:  %s\n",
                  ifelse(last$atr_filter_pass,"PASS","BLOCKED")))
      sig <- last$signal
      # Structured version of the same live-signal block, returned to the
      # caller (e.g. plumber's /signal endpoint) instead of only printed —
      # this guarantees a served "live signal" is always identical to what
      # this validated function itself computed, with no separate
      # reimplementation of the gating logic anywhere else.
      live_signal <- list(
        product          = prod,
        date             = as.character(last$date),
        regime           = last$regime_label,
        m1m2             = round(last$M1M2, 4),
        level_z          = round(last$level_z_126, 3),
        atr14            = if (is.na(last$atr14)) NA else round(last$atr14, 4),
        vol_gate_pass    = isTRUE(last$atr_filter_pass),
        vol_gate         = atr_gate,
        threshold        = thresh,
        signal           = sig,
        unit             = cfg$unit
      )
      if (sig != "FLAT") {
        stop_dist <- last$atr14 * cfg$atr_multiplier
        hard_stop <- last$M1M2 - ifelse(sig=="BUY",1,-1) * stop_dist
        cat(sprintf("    Signal:    %s\n",          sig))
        cat(sprintf("    Stop:      %.4f %s (%.1fx ATR)\n",
                    stop_dist, cfg$unit, cfg$atr_multiplier))
        cat(sprintf("    Hard stop: %.4f %s\n", hard_stop, cfg$unit))
        live_signal$stop_dist <- round(stop_dist, 4)
        live_signal$hard_stop <- round(hard_stop, 4)
        live_signal$atr_multiplier <- cfg$atr_multiplier
      } else {
        cat(sprintf("    Signal:    FLAT  (z=%+.3f  thresh=%.2f  gate=%s)\n",
                    last$level_z_126, thresh,
                    ifelse(last$atr_filter_pass,"pass","blocked")))
        live_signal$stop_dist <- NA
        live_signal$hard_stop <- NA
        live_signal$atr_multiplier <- cfg$atr_multiplier
      }
      cat("\n")
    }

    # ── Save ────────────────────────────────────────────────────────────────
    fwrite(trades_dt,
           file.path(output_dir, paste0("trades_", prod, ".csv")))
    cat(sprintf("  Saved: output/trades_%s.csv\n\n", prod))

    # Collect summary row
    test_t <- trades_dt[window == "test"]
    if (nrow(test_t) > 0) {
      ev_t <- .compute_ev(test_t)
      cum  <- cumsum(test_t[order(date), pnl_net])
      summary_rows[[prod]] <- data.table(
        product    = prod,
        unit       = cfg$unit,
        n_trades   = nrow(test_t),
        hit_pct    = round(ev_t$wr*100, 1),
        rr         = ev_t$rr,
        ev_trade   = ev_t$ev,
        ev_be      = ev_t$ev_be,
        total_pnl  = round(sum(test_t$pnl_net,na.rm=TRUE), 4),
        max_dd     = round(min(cum-cummax(cum)), 4),
        vol_gate   = atr_gate
      )
    }

    all_results[[prod]] <- list(trades=trades_dt, config=cfg, thresh=thresh, live_signal=live_signal)
  }

  # ── Cross-product summary ──────────────────────────────────────────────────
  cat("\n", strrep("=", 65), "\n")
  cat("CROSS-PRODUCT SUMMARY — TEST WINDOW\n")
  cat(strrep("=", 65), "\n\n")

  if (length(summary_rows) > 0) {
    sum_dt <- rbindlist(summary_rows, fill=TRUE)

    cat(sprintf("  %-6s %-10s %7s %7s %6s %8s %8s %10s\n",
                "Prod","Unit","N","Hit%","R:R","EV/trade","Tot_PnL","MaxDD"))
    cat("  ", strrep("-", 70), "\n")
    for (i in seq_len(nrow(sum_dt))) {
      r <- sum_dt[i]
      cat(sprintf("  %-6s %-10s %7d %7.1f %6.2f %8.4f %8.4f %10.4f\n",
                  r$product, r$unit, r$n_trades, r$hit_pct, r$rr,
                  r$ev_trade, r$total_pnl, r$max_dd))
    }

    cat("\n  Vol gate summary:\n")
    for (i in seq_len(nrow(sum_dt)))
      cat(sprintf("    %-6s %s\n", sum_dt$product[i], sum_dt$vol_gate[i]))

    fwrite(sum_dt, file.path(output_dir, "signal_summary.csv"))
    cat("\n  Saved: output/signal_summary.csv\n")
  }

  invisible(all_results)
}