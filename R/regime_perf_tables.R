# R/regime_perf_tables.R
# Computes regime × instrument performance tables for CL and LCO separately,
# using each product's own regime classification labels.
#
# CL:  output/CL/regime_labels_CL.csv  + oos_signals_v2 + cl_curve_daily
# LCO: output/LCO/regime_labels_LCO.csv + oos_signals_v2 + lco_curve_daily
#
# Outputs (to strategy_live/final data/phase3c/):
#   cl_regime_perf.csv
#   lco_regime_perf.csv

suppressPackageStartupMessages({ library(data.table); library(zoo) })

REPO <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"
OUTD <- file.path(REPO, "strategy_live/final data/phase2")
TENT <- file.path(REPO, "strategy_live/final data/tent_data")
SAVE <- file.path(REPO, "strategy_live/final data/phase3c")
dir.create(SAVE, showWarnings = FALSE)

# ── Performance helpers ───────────────────────────────────────────────────────
ann_sharpe <- function(pnl, min_obs = 10) {
  pnl <- pnl[!is.na(pnl) & is.finite(pnl)]
  if (length(pnl) < min_obs) return(NA_real_)
  m <- mean(pnl); s <- sd(pnl)
  if (is.na(s) || s == 0) return(NA_real_)
  round((m / s) * sqrt(252), 2)
}
max_dd <- function(pnl) {
  pnl <- pnl[!is.na(pnl) & is.finite(pnl)]
  if (length(pnl) == 0) return(NA_real_)
  cum <- cumsum(pnl); pk <- cummax(cum)
  round(min(cum - pk) * 100, 1)
}
calmar <- function(pnl, min_obs = 10) {
  pnl <- pnl[!is.na(pnl) & is.finite(pnl)]
  if (length(pnl) < min_obs) return(NA_real_)
  ann_ret <- mean(pnl) * 252
  mdd     <- min(cumsum(pnl) - cummax(cumsum(pnl)))
  if (mdd == 0) return(NA_real_)
  round(ann_ret / abs(mdd), 2)
}
hit_pct <- function(pos, ret) {
  ok <- !is.na(pos) & !is.na(ret) & is.finite(ret) & pos != 0
  if (!any(ok)) return(NA_real_)
  round(mean((pos[ok] > 0 & ret[ok] > 0) | (pos[ok] < 0 & ret[ok] < 0)) * 100, 1)
}

spread_ret <- function(spread, m1, h = 20)
  (shift(spread, -h, type = "lead") - spread) / pmax(m1, 10)

# M2-fly direction per regime
M2FLY_USE_FLIP <- c(
  "Backwardation-Deficit" = TRUE,
  "Deep-Backwardation"    = FALSE,
  "Easing-Backwardation"  = FALSE,
  "Contango-Surplus"      = TRUE,
  "Deep-Contango"         = TRUE,
  "Easing-Contango"       = TRUE,
  "Stable-Depressed"      = FALSE,
  "Stable-Elevated"       = TRUE,
  "Transition-Tightening" = FALSE,
  "Transition-Loosening"  = TRUE
)

# Signal thresholds per regime
REG_THR <- list(
  "Deep-Backwardation"   = 0.04,
  "Easing-Backwardation" = 0.04,
  "Stable-Depressed"     = 0.04,
  "default"              = 0.10
)
get_thr <- function(r) { v <- REG_THR[[r]]; if (is.null(v)) REG_THR$default else v }

# Compute positions given signal, threshold per row, and regime-specific flip for m2fly
make_positions <- function(dt, sig_col = "sig") {
  dt[, thr := sapply(regime, get_thr)]
  dt[, raw_sig := fifelse(get(sig_col) >  thr,  1L,
                  fifelse(get(sig_col) < -thr, -1L, 0L))]
  dt[, pos_m1m2  := raw_sig]
  dt[, pos_m1m3  := raw_sig]
  dt[, pos_m1m6  := raw_sig]
  dt[, pos_m1m12 := raw_sig]
  dt[, pos_out   := raw_sig]
  dt[, pos_m3fly := raw_sig * -1L]    # flip=TRUE always for m3fly

  dt[, m2fly_flip := M2FLY_USE_FLIP[regime]]
  dt[is.na(m2fly_flip), m2fly_flip := TRUE]
  dt[, pos_m2fly := fifelse(m2fly_flip, raw_sig * -1L, raw_sig)]
  dt
}

# Compute regime × instrument metrics
compute_regime_perf <- function(dt, regimes_col, instruments, weight_from_calmar = TRUE) {
  regimes <- sort(unique(dt[[regimes_col]][!is.na(dt[[regimes_col]]) &
                                            dt[[regimes_col]] != "Warm-Up" &
                                            !grepl("Transition", dt[[regimes_col]])]))
  rows <- list()
  for (reg in regimes) {
    sub <- dt[get(regimes_col) == reg & !is.na(get(instruments[[1]]$ret))]
    n_obs <- nrow(sub)
    for (nm in names(instruments)) {
      pc <- instruments[[nm]]$pos
      rc <- instruments[[nm]]$ret
      if (!pc %in% names(sub) || !rc %in% names(sub)) next
      pv <- sub[[pc]]; rv <- sub[[rc]]; pnl <- pv * rv
      n_sig <- sum(!is.na(pv) & pv != 0)
      shr <- ann_sharpe(pnl)
      mdd <- max_dd(pnl)
      cal <- calmar(pnl)
      hit <- hit_pct(pv, rv)
      w   <- if (weight_from_calmar) {
        if (is.na(mdd) || is.na(cal)) 0.5
        else if (mdd == 0 && shr > 5) 1.0
        else if (mdd < -50 || cal < 10) 0.0
        else if (mdd < -35 || cal < 15) 0.25
        else if (mdd < -10 || cal < 50) 0.5
        else 1.0
      } else NA_real_
      rows[[length(rows)+1]] <- data.table(
        regime = reg, instrument = nm,
        n_obs = n_obs, n_sig = n_sig,
        sharpe = shr, hit_pct = hit,
        max_dd_pct = mdd, calmar = cal, weight = w
      )
    }
  }
  rbindlist(rows)
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. CL: regime × instrument using CL's own regime labels
# ─────────────────────────────────────────────────────────────────────────────
cat("=== CL regime × instrument ===\n")

oos    <- fread(file.path(OUTD, "oos_signals_v2.csv"))
curve  <- fread(file.path(TENT, "cl_curve_daily.csv"))
cl_reg <- fread(file.path(REPO, "output/CL/regime_labels_CL.csv"))

oos[,    date := as.Date(date)]
curve[,  date := as.Date(date)]
cl_reg[, date := as.Date(date)]
setorder(oos, date); setorder(curve, date)

cl_dt <- merge(oos[, .(date, sig = sig_ens, ret_20d, wti_spot)], curve, by = "date", all.x = TRUE)
cl_dt <- merge(cl_dt, cl_reg[, .(date, regime = regime_label)], by = "date", all.x = TRUE)
cl_dt <- cl_dt[!is.na(regime) & regime != "Warm-Up"]

cl_dt[, wti_spot := as.numeric(wti_spot)]
cl_dt[, m1_px    := zoo::na.locf(wti_spot, na.rm = FALSE)]
cl_dt[, ret_out   := ret_20d]
cl_dt[, ret_m1m2  := spread_ret(m1m2,   m1_px)]
cl_dt[, ret_m1m3  := spread_ret(m1m3,   m1_px)]
cl_dt[, ret_m1m6  := spread_ret(m1m6,   m1_px)]
cl_dt[, ret_m1m12 := spread_ret(m1m12,  m1_px)]
cl_dt[, ret_m2fly := spread_ret(m2_fly, m1_px)]
cl_dt[, ret_m3fly := spread_ret(m3_fly, m1_px)]

cl_dt <- make_positions(cl_dt)

cl_instruments <- list(
  "Outright M1" = list(pos = "pos_out",   ret = "ret_out"),
  "M1-M2"       = list(pos = "pos_m1m2",  ret = "ret_m1m2"),
  "M1-M3"       = list(pos = "pos_m1m3",  ret = "ret_m1m3"),
  "M1-M6"       = list(pos = "pos_m1m6",  ret = "ret_m1m6"),
  "M1-M12"      = list(pos = "pos_m1m12", ret = "ret_m1m12"),
  "M2 fly"      = list(pos = "pos_m2fly", ret = "ret_m2fly"),
  "M3 fly"      = list(pos = "pos_m3fly", ret = "ret_m3fly")
)

cl_perf <- compute_regime_perf(cl_dt, "regime", cl_instruments)
cat("\nCL regime × instrument (first 21 rows):\n")
print(cl_perf[, .(regime, instrument, sharpe, hit_pct, max_dd_pct, calmar, weight)])

fwrite(cl_perf, file.path(SAVE, "cl_regime_perf.csv"))
cat("Saved cl_regime_perf.csv\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 2. LCO: regime × instrument using LCO's own regime labels
# ─────────────────────────────────────────────────────────────────────────────
cat("=== LCO regime × instrument ===\n")

lco     <- fread(file.path(TENT, "lco_curve_daily.csv"))
lco_reg <- fread(file.path(REPO, "output/LCO/regime_labels_LCO.csv"))

lco[,     date := as.Date(date)]
lco_reg[, date := as.Date(date)]
setorder(lco, date)

lco_spread_cols <- c("m1m2","m1m3","m1m6","m1m12","m2_fly","m3_fly")
setnames(lco, lco_spread_cols,
         paste0("lco_", c("m1m2","m1m3","m1m6","m1m12","m2fly","m3fly")))

lco_dt <- merge(oos[, .(date, sig = sig_ens)],
                lco[, .(date, lco_m1 = m1, lco_m1m2, lco_m1m3,
                        lco_m1m6, lco_m1m12, lco_m2fly, lco_m3fly)],
                by = "date", all.x = TRUE)
lco_dt <- merge(lco_dt, lco_reg[, .(date, regime = regime_label)], by = "date", all.x = TRUE)
lco_dt <- lco_dt[!is.na(regime) & regime != "Warm-Up" & !is.na(lco_m1)]
lco_dt[, lco_m1 := zoo::na.locf(lco_m1, na.rm = FALSE)]

lco_dt[, ret_lco_m1m2  := spread_ret(lco_m1m2,  lco_m1)]
lco_dt[, ret_lco_m1m3  := spread_ret(lco_m1m3,  lco_m1)]
lco_dt[, ret_lco_m1m6  := spread_ret(lco_m1m6,  lco_m1)]
lco_dt[, ret_lco_m1m12 := spread_ret(lco_m1m12, lco_m1)]
lco_dt[, ret_lco_m2fly := spread_ret(lco_m2fly, lco_m1)]
lco_dt[, ret_lco_m3fly := spread_ret(lco_m3fly, lco_m1)]

lco_dt <- make_positions(lco_dt)
# Rename positions to lco_ prefix for clarity
setnames(lco_dt,
  c("pos_m1m2","pos_m1m3","pos_m1m6","pos_m1m12","pos_m2fly","pos_m3fly"),
  c("pos_lco_m1m2","pos_lco_m1m3","pos_lco_m1m6","pos_lco_m1m12","pos_lco_m2fly","pos_lco_m3fly"))

lco_instruments <- list(
  "M1-M2"  = list(pos = "pos_lco_m1m2",  ret = "ret_lco_m1m2"),
  "M1-M3"  = list(pos = "pos_lco_m1m3",  ret = "ret_lco_m1m3"),
  "M1-M6"  = list(pos = "pos_lco_m1m6",  ret = "ret_lco_m1m6"),
  "M1-M12" = list(pos = "pos_lco_m1m12", ret = "ret_lco_m1m12"),
  "M2 fly" = list(pos = "pos_lco_m2fly", ret = "ret_lco_m2fly"),
  "M3 fly" = list(pos = "pos_lco_m3fly", ret = "ret_lco_m3fly")
)

lco_perf <- compute_regime_perf(lco_dt, "regime", lco_instruments)
cat("\nLCO regime × instrument:\n")
print(lco_perf[, .(regime, instrument, sharpe, hit_pct, max_dd_pct, calmar, weight)])

fwrite(lco_perf, file.path(SAVE, "lco_regime_perf.csv"))
cat("Saved lco_regime_perf.csv\n")

cat("\n=== DONE ===\n")
cat("cl_regime_perf.csv and lco_regime_perf.csv saved to phase3c/\n")
