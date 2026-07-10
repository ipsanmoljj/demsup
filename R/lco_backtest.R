# R/lco_backtest.R
# LCO regime × instrument backtest — mirrors the CL phase3 backtest
# Uses the CL signal (sig_ens) and CL regime as signal source (CL/Brent highly correlated)
# Computes LCO spread returns and derives regime-specific weights for the LCO strategy
#
# Output: strategy_live/final data/phase3_lco/
#   lco_backtest_full.csv           — daily positions × returns for all LCO instruments
#   lco_regime_instrument_detail.csv — per-regime per-instrument Sharpe/DD/Calmar
#   lco_weights.csv                  — weight table ready for lco_strategy.R

suppressPackageStartupMessages({ library(data.table); library(zoo) })

REPO <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"
OUTD <- file.path(REPO, "strategy_live/final data/phase2")
TENT <- file.path(REPO, "strategy_live/final data/tent_data")
SAVE <- file.path(REPO, "strategy_live/final data/phase3_lco")
dir.create(SAVE, showWarnings = FALSE)

cat("=================================================================\n")
cat("LCO BACKTEST: Regime × Instrument Performance\n")
cat("=================================================================\n\n")

# ── 1. Load data ──────────────────────────────────────────────────────────────
oos     <- fread(file.path(OUTD, "oos_signals_v2.csv"))
lco     <- fread(file.path(TENT, "lco_curve_daily.csv"))
lco_reg <- fread(file.path(REPO, "output/LCO/regime_labels_LCO.csv"))

oos[,     date := as.Date(date)]
lco[,     date := as.Date(date)]
lco_reg[, date := as.Date(date)]
setorder(oos, date); setorder(lco, date)

# Rename LCO spread cols to avoid clash with any CL columns
lco_spread_cols <- c("m1m2","m1m3","m1m6","m1m12","m2_fly","m3_fly")
setnames(lco, lco_spread_cols,
         paste0("lco_", c("m1m2","m1m3","m1m6","m1m12","m2fly","m3fly")))

oos <- merge(oos, lco[, .(date, lco_m1 = m1, lco_m1m2, lco_m1m3,
                           lco_m1m6, lco_m1m12, lco_m2fly, lco_m3fly)],
             by = "date", all.x = TRUE)
oos <- merge(oos, lco_reg[, .(date, lco_regime = regime_label)],
             by = "date", all.x = TRUE)
oos[, lco_m1 := zoo::na.locf(lco_m1, na.rm = FALSE)]

cat(sprintf("OOS rows: %d  |  %s → %s\n", nrow(oos), min(oos$date), max(oos$date)))
cat(sprintf("LCO rows: %d  |  %s → %s\n", nrow(lco), min(lco$date), max(lco$date)))

# ── 2. Thresholds based on LCO regime ────────────────────────────────────────
REG_THR <- list(
  "Deep-Backwardation"   = 0.04,
  "Easing-Backwardation" = 0.04,
  "Stable-Depressed"     = 0.04,
  "Stable-Elevated"      = 0.04,
  "default"              = 0.10
)
get_thr <- function(r) { v <- REG_THR[[r]]; if (is.null(v)) REG_THR$default else v }
oos[, threshold := sapply(lco_regime, get_thr)]
oos[, sig := sig_ens]

# ── 3. LCO forward returns (20-day, normalised by LCO m1) ────────────────────
spread_ret <- function(spread, m1, h = 20)
  (shift(spread, -h, type = "lead") - spread) / pmax(m1, 10)

oos[, ret_lco_m1m2  := spread_ret(lco_m1m2,  lco_m1)]
oos[, ret_lco_m1m3  := spread_ret(lco_m1m3,  lco_m1)]
oos[, ret_lco_m1m6  := spread_ret(lco_m1m6,  lco_m1)]
oos[, ret_lco_m1m12 := spread_ret(lco_m1m12, lco_m1)]
oos[, ret_lco_m2fly := spread_ret(lco_m2fly, lco_m1)]
oos[, ret_lco_m3fly := spread_ret(lco_m3fly, lco_m1)]

# ── 4. Positions ──────────────────────────────────────────────────────────────
# Spreads: Long when bullish (fly falls → back gets steeper → m1 rises vs m2/m3)
# m3fly: flip=TRUE (same as CL)
# m2fly: regime-specific flip (same as CL logic)
M2FLY_USE_FLIP <- c(
  "Backwardation-Deficit" = TRUE,
  "Deep-Backwardation"    = FALSE,
  "Easing-Backwardation"  = FALSE,
  "Contango-Surplus"      = TRUE,
  "Deep-Contango"         = TRUE,
  "Easing-Contango"       = TRUE,
  "Stable-Depressed"      = FALSE,
  "Stable-Elevated"       = TRUE,
  "Transition-Tightening" = FALSE
)

raw_pos <- function(sig, thr, flip) {
  r <- fifelse(sig >  thr,  1L, fifelse(sig < -thr, -1L, 0L))
  if (flip) r * -1L else r
}

oos[, pos_lco_m1m2  := raw_pos(sig, threshold, flip = FALSE)]
oos[, pos_lco_m1m3  := raw_pos(sig, threshold, flip = FALSE)]
oos[, pos_lco_m1m6  := raw_pos(sig, threshold, flip = FALSE)]
oos[, pos_lco_m1m12 := raw_pos(sig, threshold, flip = FALSE)]
oos[, pos_lco_m3fly := raw_pos(sig, threshold, flip = TRUE)]

oos[, m2fly_flip := M2FLY_USE_FLIP[lco_regime]]
oos[is.na(m2fly_flip), m2fly_flip := TRUE]
oos[, raw_sig := fifelse(sig > threshold, 1L, fifelse(sig < -threshold, -1L, 0L))]
oos[, pos_lco_m2fly := fifelse(m2fly_flip, raw_sig * -1L, raw_sig)]

# ── 5. Performance metric helpers ─────────────────────────────────────────────
ann_sharpe <- function(pnl, min_obs = 10) {
  pnl <- pnl[!is.na(pnl) & is.finite(pnl)]
  if (length(pnl) < min_obs) return(NA_real_)
  m <- mean(pnl); s <- sd(pnl)
  if (is.na(s) || s == 0) return(NA_real_)
  (m / s) * sqrt(252)
}

max_dd <- function(pnl) {
  pnl <- pnl[!is.na(pnl) & is.finite(pnl)]
  if (length(pnl) == 0) return(NA_real_)
  cum <- cumsum(pnl)
  peak <- cummax(cum)
  round(min(cum - peak) * 100, 1)
}

calmar <- function(pnl, min_obs = 10) {
  pnl <- pnl[!is.na(pnl) & is.finite(pnl)]
  if (length(pnl) < min_obs) return(NA_real_)
  ann_ret <- mean(pnl) * 252
  mdd <- min(cumsum(pnl) - cummax(cumsum(pnl)))
  if (mdd == 0) return(NA_real_)
  round(ann_ret / abs(mdd), 2)
}

hit_pct <- function(pos, ret) {
  ok <- !is.na(pos) & !is.na(ret) & is.finite(ret) & pos != 0
  if (!any(ok)) return(NA_real_)
  round(mean((pos[ok] > 0 & ret[ok] > 0) | (pos[ok] < 0 & ret[ok] < 0)) * 100, 1)
}

avg_win <- function(pos, ret) {
  ok <- !is.na(pos) & !is.na(ret) & is.finite(ret) & pos != 0
  if (!any(ok)) return(NA_real_)
  round(mean(pos[ok] * ret[ok]), 6)
}

# ── 6. Regime × instrument metrics ────────────────────────────────────────────
instruments <- list(
  "LCO M1-M2"  = list(pos = "pos_lco_m1m2",  ret = "ret_lco_m1m2"),
  "LCO M1-M3"  = list(pos = "pos_lco_m1m3",  ret = "ret_lco_m1m3"),
  "LCO M1-M6"  = list(pos = "pos_lco_m1m6",  ret = "ret_lco_m1m6"),
  "LCO M1-M12" = list(pos = "pos_lco_m1m12", ret = "ret_lco_m1m12"),
  "LCO M2 fly" = list(pos = "pos_lco_m2fly", ret = "ret_lco_m2fly"),
  "LCO M3 fly" = list(pos = "pos_lco_m3fly", ret = "ret_lco_m3fly")
)

regimes <- sort(unique(oos$lco_regime[!is.na(oos$lco_regime) &
                                       oos$lco_regime != "Warm-Up"]))
rows <- list()

for (reg in regimes) {
  sub <- oos[lco_regime == reg & !is.na(ret_lco_m1m2)]
  n_obs <- nrow(sub)
  for (nm in names(instruments)) {
    pc <- instruments[[nm]]$pos
    rc <- instruments[[nm]]$ret
    pos_v <- sub[[pc]]
    ret_v <- sub[[rc]]
    pnl_v <- pos_v * ret_v
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

cat("\n=== LCO Regime × Instrument Performance ===\n")
print(detail[, .(regime, instrument, sharpe, hit_pct, max_dd_pct, calmar)])

# ── 7. Derive weights from Calmar / Max DD ────────────────────────────────────
# LCO uses Sharpe-based weights (calmar naturally small for normalized spreads)
weight_lco <- function(shr, dd) {
  if (is.na(shr) || is.na(dd)) return(0.0)
  if (dd < -50)    return(0.0)   # catastrophic drawdown
  if (shr >= 3.0)  return(1.0)
  if (shr >= 1.5)  return(0.5)
  if (shr >= 0.5)  return(0.25)
  return(0.0)
}

detail[, w := mapply(weight_lco, sharpe, max_dd_pct)]

# Pivot to wide weight table (one row per regime)
wt_wide <- dcast(detail, regime ~ instrument, value.var = "w")
setnames(wt_wide,
  c("LCO M1-M2","LCO M1-M3","LCO M1-M6","LCO M1-M12","LCO M2 fly","LCO M3 fly"),
  c("w_lco_m1m2","w_lco_m1m3","w_lco_m1m6","w_lco_m1m12","w_lco_m2fly","w_lco_m3fly"),
  skip_absent = TRUE)

cat("\n=== LCO Weight Table ===\n")
print(wt_wide)

# ── 8. Save outputs ───────────────────────────────────────────────────────────
bt_cols <- c("date","cl_regime","sig","threshold",
             "lco_m1","lco_m1m2","lco_m1m3","lco_m1m6","lco_m1m12","lco_m2fly","lco_m3fly",
             "pos_lco_m1m2","pos_lco_m1m3","pos_lco_m1m6","pos_lco_m1m12","pos_lco_m2fly","pos_lco_m3fly",
             "ret_lco_m1m2","ret_lco_m1m3","ret_lco_m1m6","ret_lco_m1m12","ret_lco_m2fly","ret_lco_m3fly")
bt_cols <- intersect(bt_cols, names(oos))

fwrite(oos[, ..bt_cols],  file.path(SAVE, "lco_backtest_full.csv"))
fwrite(detail,            file.path(SAVE, "lco_regime_instrument_detail.csv"))
fwrite(wt_wide,           file.path(SAVE, "lco_weights.csv"))

cat(sprintf("\nSaved to %s\n", SAVE))
cat("  lco_backtest_full.csv\n")
cat("  lco_regime_instrument_detail.csv\n")
cat("  lco_weights.csv\n")
cat("\n=================================================================\n")
cat("LCO BACKTEST COMPLETE\n")
cat("=================================================================\n")
