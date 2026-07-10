# Fix M2-fly direction in phase3 backtest outputs
#
# Background: The original backtest used flip=TRUE for M2-fly throughout (Long
# when bearish). Regime analysis shows that in Deep-Backwardation, Easing-
# Backwardation, Stable-Depressed, and Transition-Tightening, the M2-fly
# rises when the signal is BULLISH (M1 gets squeezed more than M2), so
# flip=FALSE (Long when bullish) is correct in those 4 regimes.
#
# Fix: For rows in the 4 affected regimes, pos_m2fly = -1 * original pos_m2fly.
# Then recompute per-regime-instrument performance metrics and update
# regime_instrument_detail.csv, so phase3c_strategy.R can use correct weights.

suppressPackageStartupMessages({ library(data.table); library(zoo) })

P3   <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup/strategy_live/final data/phase3"
P3C  <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup/strategy_live/final data/phase3c"

FLIP_REGIMES <- c("Deep-Backwardation","Easing-Backwardation",
                  "Stable-Depressed","Transition-Tightening")

# ── Load backtest ─────────────────────────────────────────────────────────────
bt <- fread(file.path(P3, "backtest_full.csv"))
bt[, date := as.Date(date)]
cat(sprintf("Loaded %d rows (%s → %s)\n", nrow(bt), min(bt$date), max(bt$date)))

# ── Apply M2-fly direction correction ─────────────────────────────────────────
bt[, pos_m2fly_orig := pos_m2fly]
bt[cl_regime %in% FLIP_REGIMES, pos_m2fly := -1L * pos_m2fly]

n_fixed <- bt[cl_regime %in% FLIP_REGIMES & pos_m2fly != 0, .N]
cat(sprintf("Corrected %d non-zero M2-fly positions in 4 regimes\n", n_fixed))

# ── Performance metric helpers ────────────────────────────────────────────────
ann_sharpe <- function(pnl, min_obs = 10) {
  pnl <- pnl[!is.na(pnl)]
  if (length(pnl) < min_obs) return(NA_real_)
  m <- mean(pnl); s <- sd(pnl)
  if (is.na(s) || s == 0) return(NA_real_)
  (m / s) * sqrt(252)
}

max_dd <- function(pnl) {
  pnl <- pnl[!is.na(pnl)]
  if (length(pnl) == 0) return(NA_real_)
  cum <- cumsum(pnl)
  peak <- cummax(cum)
  dd  <- cum - peak
  round(min(dd) * 100, 1)
}

calmar <- function(pnl, min_obs = 10) {
  pnl <- pnl[!is.na(pnl)]
  if (length(pnl) < min_obs) return(NA_real_)
  ann_ret <- mean(pnl) * 252
  mdd <- min(cumsum(pnl) - cummax(cumsum(pnl)))
  if (mdd == 0) return(NA_real_)
  round(ann_ret / abs(mdd), 2)
}

hit_pct <- function(pos, pnl) {
  pnl <- pnl[!is.na(pnl) & !is.na(pos) & pos != 0]
  pos <- pos[!is.na(pnl) & !is.na(pos) & pos != 0]  # match filtering
  # re-filter
  idx <- !is.na(pnl)
  pnl2 <- pnl[idx]; pos2 <- pos[idx]
  if (length(pnl2) == 0) return(NA_real_)
  round(mean((pos2 > 0 & pnl2 > 0) | (pos2 < 0 & pnl2 < 0)) * 100, 1)
}

avg_win <- function(pos, pnl) {
  idx <- !is.na(pnl) & !is.na(pos) & pos != 0
  if (sum(idx) == 0) return(NA_real_)
  round(mean(pos[idx] * pnl[idx]), 6)
}

# ── Compute per-regime per-instrument metrics ─────────────────────────────────
instruments <- list(
  "Outright M1" = list(pos = "pos_out",   ret = "ret_out"),
  "M1-M2"       = list(pos = "pos_m1m2",  ret = "ret_m1m2"),
  "M1-M3"       = list(pos = "pos_m1m3",  ret = "ret_m1m3"),
  "M1-M6"       = list(pos = "pos_m1m6",  ret = "ret_m1m6"),
  "M1-M12"      = list(pos = "pos_m1m12", ret = "ret_m1m12"),
  "M2 fly"      = list(pos = "pos_m2fly", ret = "ret_m2fly"),
  "M3 fly"      = list(pos = "pos_m3fly", ret = "ret_m3fly")
)

regimes <- sort(unique(bt$cl_regime))
rows <- list()

for (reg in regimes) {
  sub <- bt[cl_regime == reg]
  n_obs <- nrow(sub)
  for (nm in names(instruments)) {
    pc <- instruments[[nm]]$pos
    rc <- instruments[[nm]]$ret
    if (!pc %in% names(sub) || !rc %in% names(sub)) next
    pos_v <- sub[[pc]]
    ret_v <- sub[[rc]]
    pnl_v <- pos_v * ret_v  # daily P&L = position × return
    n_sig  <- sum(!is.na(pos_v) & pos_v != 0, na.rm = TRUE)

    rows[[length(rows)+1]] <- data.table(
      regime     = reg,
      instrument = nm,
      n_obs      = n_obs,
      n_sig      = n_sig,
      sharpe     = round(ann_sharpe(pnl_v), 2),
      hit_pct    = hit_pct(pos_v, ret_v),
      max_dd_pct = max_dd(pnl_v),
      calmar     = calmar(pnl_v),
      avg_win    = avg_win(pos_v, ret_v)
    )
  }
}

detail <- rbindlist(rows)

# ── Print summary for M2 fly (the corrected instrument) ───────────────────────
cat("\n=== M2 Fly performance after correction ===\n")
m2_summary <- detail[instrument == "M2 fly", .(regime, sharpe, hit_pct, max_dd_pct, calmar)]
print(m2_summary)

# ── Save corrected detail ─────────────────────────────────────────────────────
fwrite(detail, file.path(P3, "regime_instrument_detail.csv"))
cat(sprintf("\nSaved corrected regime_instrument_detail.csv (%d rows)\n", nrow(detail)))

# ── Save corrected backtest (with pos_m2fly fixed) ───────────────────────────
bt[, pos_m2fly_orig := NULL]  # drop temp col
fwrite(bt, file.path(P3, "backtest_full_corrected.csv"))
cat("Saved backtest_full_corrected.csv\n")

# ── Generate updated weight recommendations ───────────────────────────────────
weight_from_dd_cal <- function(dd, cal) {
  if (is.na(dd) || is.na(cal)) return(0.5)  # conservative
  if (dd < -50 || cal < 10)    return(0.0)
  if (dd < -35 || cal < 15)    return(0.25)
  if (dd < -10 || cal < 50)    return(0.5)
  return(1.0)
}

m2_weights <- m2_summary[, .(
  regime,
  sharpe,
  max_dd_pct,
  calmar,
  recommended_weight = mapply(weight_from_dd_cal, max_dd_pct, calmar)
)]
cat("\n=== Recommended M2 fly weights (corrected) ===\n")
print(m2_weights)

# Save for reference
fwrite(m2_weights, file.path(P3, "m2fly_corrected_weights.csv"))
cat("\nSaved m2fly_corrected_weights.csv\n")
