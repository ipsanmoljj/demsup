# R/phase3c_strategy.R
# ─────────────────────────────────────────────────────────────────────────────
# PHASE 3C: Regime-Adaptive Multi-Instrument Strategy
#
# Two independent sub-strategies, each with their own backtest-derived weights:
#
#   CL  strategy: out, m1m2, m1m3, m1m6, m1m12, m2fly, m3fly
#   LCO strategy: m1m2, m1m3, m1m6, m1m12, m2fly, m3fly   (no LCO outright)
#
# Reporting: CL-only  |  LCO-only  |  Combined (CL+LCO equal-notional)
#
# CFTC overlay (WTI managed-money positioning):
#   pos_z > +1.5 → overcrowded long  → contrarian, multiply position by ×0.5 if confirming crowd
#   pos_z < -1.5 → overcrowded short → contrarian, multiply position by ×0.5 if confirming crowd
#   When contrarian signal agrees with our position → ×1.5 boost
#
# Weights:
#   CL : from regime_instrument_detail.csv (corrected M2-fly 2026-07-09)
#   LCO: from phase3_lco/lco_weights.csv   (computed by lco_backtest.R)
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({ library(data.table); library(zoo) })

REPO    <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"
OUTD    <- file.path(REPO, "strategy_live/final data/phase2")
TENT    <- file.path(REPO, "strategy_live/final data/tent_data")
P3LCO   <- file.path(REPO, "strategy_live/final data/phase3_lco")
SAVE    <- file.path(REPO, "strategy_live/final data/phase3c")
dir.create(SAVE, showWarnings = FALSE)

cat("=================================================================\n")
cat("PHASE 3C: CL + LCO Regime-Adaptive Strategy (Separate)\n")
cat("=================================================================\n\n")

# ── 1. Load data ──────────────────────────────────────────────────────────────
oos     <- fread(file.path(OUTD, "oos_signals_v2.csv"))
curve   <- fread(file.path(TENT, "cl_curve_daily.csv"))
lco     <- fread(file.path(TENT, "lco_curve_daily.csv"))
cftc    <- fread(file.path(TENT, "cftc_daily.csv"))
lco_reg <- fread(file.path(REPO, "output/LCO/regime_labels_LCO.csv"))

for (dt in list(oos, curve, lco, cftc, lco_reg)) dt[, date := as.Date(date)]
setorder(oos, date); setorder(curve, date)

oos <- merge(oos,   curve, by = "date", all.x = TRUE)
oos <- merge(oos,   cftc[, .(date, pos_z, cftc_regime, cftc_signal, cftc_multiplier)],
             by = "date", all.x = TRUE)
oos <- merge(oos,   lco_reg[, .(date, lco_regime = regime_label)],
             by = "date", all.x = TRUE)

lco_spread_cols <- c("m1m2","m1m3","m1m6","m1m12","m2_fly","m3_fly")
setnames(lco, lco_spread_cols,
         paste0("lco_", c("m1m2","m1m3","m1m6","m1m12","m2fly","m3fly")))
oos <- merge(oos, lco[, .(date, lco_m1 = m1, lco_m1m2, lco_m1m3,
                           lco_m1m6, lco_m1m12, lco_m2fly, lco_m3fly)],
             by = "date", all.x = TRUE)
oos[, lco_m1 := zoo::na.locf(lco_m1, na.rm = FALSE)]

cat(sprintf("OOS rows: %d  |  %s → %s\n", nrow(oos), min(oos$date), max(oos$date)))

# ── 2. Signal thresholds (same for CL and LCO) ───────────────────────────────
REG_THR <- list(
  "Deep-Backwardation"   = 0.04,
  "Easing-Backwardation" = 0.04,
  "Stable-Depressed"     = 0.04,
  "default"              = 0.10
)
get_thr <- function(r) { v <- REG_THR[[r]]; if (is.null(v)) REG_THR$default else v }
LCO_THR <- list(
  "Deep-Backwardation"   = 0.04,
  "Easing-Backwardation" = 0.04,
  "Stable-Depressed"     = 0.04,
  "Stable-Elevated"      = 0.04,   # stable backwardation: weak signal still informative
  "default"              = 0.10
)
get_lco_thr <- function(r) { v <- LCO_THR[[r]]; if (is.null(v)) LCO_THR$default else v }
oos[, threshold     := sapply(cl_regime, get_thr)]
oos[, lco_threshold := sapply(fifelse(is.na(lco_regime), "default", lco_regime), get_lco_thr)]
oos[, sig := sig_ens]

# ── 3. Forward returns ────────────────────────────────────────────────────────
oos[, wti_spot := as.numeric(wti_spot)]
oos[, m1_px   := zoo::na.locf(wti_spot, na.rm = FALSE)]

spread_ret <- function(spread, m1, h = 20)
  (shift(spread, -h, type = "lead") - spread) / pmax(m1, 10)

# CL returns
oos[, ret_out   := ret_20d]
oos[, ret_m1m2  := spread_ret(m1m2,   m1_px)]
oos[, ret_m1m3  := spread_ret(m1m3,   m1_px)]
oos[, ret_m1m6  := spread_ret(m1m6,   m1_px)]
oos[, ret_m1m12 := spread_ret(m1m12,  m1_px)]
oos[, ret_m2fly := spread_ret(m2_fly, m1_px)]
oos[, ret_m3fly := spread_ret(m3_fly, m1_px)]

# LCO returns (normalised by LCO m1 price)
oos[, ret_lco_m1m2  := spread_ret(lco_m1m2,  lco_m1)]
oos[, ret_lco_m1m3  := spread_ret(lco_m1m3,  lco_m1)]
oos[, ret_lco_m1m6  := spread_ret(lco_m1m6,  lco_m1)]
oos[, ret_lco_m1m12 := spread_ret(lco_m1m12, lco_m1)]
oos[, ret_lco_m2fly := spread_ret(lco_m2fly, lco_m1)]
oos[, ret_lco_m3fly := spread_ret(lco_m3fly, lco_m1)]

# ── 4 & 5. Load regularised weights (elastic net, ridge fallback) ─────────────
# Built by optimise_weights.R using glmnet (alpha=0.5 enet, lower.limits=0)
# For regimes where enet gives all-zero weights, use ridge.
load_enet_weights <- function(path, inst_names, prefix = "") {
  dt <- fread(path)
  enet_cols  <- paste0("w_enet_",  inst_names)
  ridge_cols <- paste0("w_ridge_", inst_names)
  out_cols   <- paste0(prefix, inst_names)
  for (i in seq_along(inst_names)) {
    ec <- enet_cols[i]; rc <- ridge_cols[i]; oc <- out_cols[i]
    # Use enet; fall back to ridge if that regime's enet row sums to 0
    dt[, enet_sum := rowSums(.SD, na.rm=TRUE), .SDcols = enet_cols]
    dt[, (oc) := fifelse(enet_sum > 0,
                          fifelse(is.na(get(ec)), 0, get(ec)),
                          fifelse(is.na(get(rc)), 0, get(rc)))]
    dt[, enet_sum := NULL]
  }
  dt[, c("regime", out_cols), with = FALSE]
}

cl_inst  <- c("out","m1m2","m1m3","m1m6","m1m12","m2fly","m3fly")
lco_inst <- c("m1m2","m1m3","m1m6","m1m12","m2fly","m3fly")

CL_RW  <- load_enet_weights(file.path(SAVE, "cl_weights_enet.csv"),
                             cl_inst,  prefix = "w_")
LCO_RW <- load_enet_weights(file.path(SAVE, "lco_weights_enet.csv"),
                             lco_inst, prefix = "w_lco_")

# Fill missing regimes with 0 (regimes not in training get no weight)
cl_wcols  <- paste0("w_",     cl_inst)
lco_wcols <- paste0("w_lco_", lco_inst)

# ── 6. M2-fly direction by regime ─────────────────────────────────────────────
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

# ── 7. Merge weight tables onto OOS ──────────────────────────────────────────
oos <- merge(oos, CL_RW,  by.x = "cl_regime",  by.y = "regime", all.x = TRUE)
oos <- merge(oos, LCO_RW, by.x = "lco_regime", by.y = "regime", all.x = TRUE)

for (wc in c(cl_wcols, lco_wcols))
  oos[is.na(get(wc)), (wc) := 0]

# ── 8. CL positions ───────────────────────────────────────────────────────────
oos[, pos_out   := raw_pos(sig, threshold, flip = FALSE)]
oos[, pos_m1m2  := raw_pos(sig, threshold, flip = FALSE)]
oos[, pos_m1m3  := raw_pos(sig, threshold, flip = FALSE)]
oos[, pos_m1m6  := raw_pos(sig, threshold, flip = FALSE)]
oos[, pos_m1m12 := raw_pos(sig, threshold, flip = FALSE)]
oos[, pos_m3fly := raw_pos(sig, threshold, flip = TRUE)]

oos[, m2fly_flip := M2FLY_USE_FLIP[cl_regime]]
oos[is.na(m2fly_flip), m2fly_flip := TRUE]
oos[, raw_sig := fifelse(sig >  threshold,  1L, fifelse(sig < -threshold, -1L, 0L))]
oos[, pos_m2fly := fifelse(m2fly_flip, raw_sig * -1L, raw_sig)]

# ── 9. LCO positions (LCO threshold + LCO regime flip) ───────────────────────
oos[, lco_raw_sig := fifelse(sig >  lco_threshold,  1L,
                     fifelse(sig < -lco_threshold, -1L, 0L))]
oos[, pos_lco_m1m2  := lco_raw_sig]
oos[, pos_lco_m1m3  := lco_raw_sig]
oos[, pos_lco_m1m6  := lco_raw_sig]
oos[, pos_lco_m1m12 := lco_raw_sig]
oos[, pos_lco_m3fly := lco_raw_sig * -1L]

oos[, lco_m2fly_flip := M2FLY_USE_FLIP[lco_regime]]
oos[is.na(lco_m2fly_flip), lco_m2fly_flip := TRUE]
oos[, pos_lco_m2fly := fifelse(lco_m2fly_flip, lco_raw_sig * -1L, lco_raw_sig)]

# ── 10. CFTC overlay multiplier ───────────────────────────────────────────────
oos[, cftc_signal     := zoo::na.locf(cftc_signal,     na.rm = FALSE)]
oos[, cftc_multiplier := zoo::na.locf(cftc_multiplier, na.rm = FALSE)]
oos[is.na(cftc_signal),     cftc_signal     := 0L]
oos[is.na(cftc_multiplier), cftc_multiplier := 1.0]

cftc_pos_mult <- function(pos, cftc_sig) {
  fifelse(cftc_sig == 0L, 1.0,
  fifelse(sign(pos) == cftc_sig, 1.5,
  fifelse(pos == 0L, 1.0, 0.5)))
}
oos[, cftc_mult := cftc_pos_mult(raw_sig, cftc_signal)]

# ── 11. CL P&L ───────────────────────────────────────────────────────────────
oos[, c_out   := w_out   * pos_out   * cftc_mult * ret_out  ]
oos[, c_m1m2  := w_m1m2  * pos_m1m2  * cftc_mult * ret_m1m2 ]
oos[, c_m1m3  := w_m1m3  * pos_m1m3  * cftc_mult * ret_m1m3 ]
oos[, c_m1m6  := w_m1m6  * pos_m1m6  * cftc_mult * ret_m1m6 ]
oos[, c_m1m12 := w_m1m12 * pos_m1m12 * cftc_mult * ret_m1m12]
oos[, c_m2fly := w_m2fly * pos_m2fly * cftc_mult * ret_m2fly]
oos[, c_m3fly := w_m3fly * pos_m3fly * cftc_mult * ret_m3fly]

oos[, cl_wsum := w_out*abs(pos_out)*cftc_mult + w_m1m2*abs(pos_m1m2)*cftc_mult +
                 w_m1m3*abs(pos_m1m3)*cftc_mult + w_m1m6*abs(pos_m1m6)*cftc_mult +
                 w_m1m12*abs(pos_m1m12)*cftc_mult + w_m2fly*abs(pos_m2fly)*cftc_mult +
                 w_m3fly*abs(pos_m3fly)*cftc_mult]
oos[, cl_raw  := c_out + c_m1m2 + c_m1m3 + c_m1m6 + c_m1m12 + c_m2fly + c_m3fly]
oos[, cl_port_ret := fifelse(cl_wsum > 0, cl_raw / cl_wsum, NA_real_)]

# CL-only no CFTC (baseline)
oos[, cl_wsum_nc := w_out*abs(pos_out) + w_m1m2*abs(pos_m1m2) + w_m1m3*abs(pos_m1m3) +
                    w_m1m6*abs(pos_m1m6) + w_m1m12*abs(pos_m1m12) +
                    w_m2fly*abs(pos_m2fly) + w_m3fly*abs(pos_m3fly)]
oos[, cl_raw_nc  := w_out*pos_out*ret_out + w_m1m2*pos_m1m2*ret_m1m2 +
                    w_m1m3*pos_m1m3*ret_m1m3 + w_m1m6*pos_m1m6*ret_m1m6 +
                    w_m1m12*pos_m1m12*ret_m1m12 + w_m2fly*pos_m2fly*ret_m2fly +
                    w_m3fly*pos_m3fly*ret_m3fly]
oos[, cl_nc_ret  := fifelse(cl_wsum_nc > 0, cl_raw_nc / cl_wsum_nc, NA_real_)]

# ── 12. LCO P&L (using LCO-specific weights) ─────────────────────────────────
oos[, c_lco_m1m2  := w_lco_m1m2  * pos_lco_m1m2  * cftc_mult * ret_lco_m1m2 ]
oos[, c_lco_m1m3  := w_lco_m1m3  * pos_lco_m1m3  * cftc_mult * ret_lco_m1m3 ]
oos[, c_lco_m1m6  := w_lco_m1m6  * pos_lco_m1m6  * cftc_mult * ret_lco_m1m6 ]
oos[, c_lco_m1m12 := w_lco_m1m12 * pos_lco_m1m12 * cftc_mult * ret_lco_m1m12]
oos[, c_lco_m2fly := w_lco_m2fly * pos_lco_m2fly * cftc_mult * ret_lco_m2fly]
oos[, c_lco_m3fly := w_lco_m3fly * pos_lco_m3fly * cftc_mult * ret_lco_m3fly]

oos[, lco_wsum := w_lco_m1m2*abs(pos_lco_m1m2)*cftc_mult + w_lco_m1m3*abs(pos_lco_m1m3)*cftc_mult +
                  w_lco_m1m6*abs(pos_lco_m1m6)*cftc_mult + w_lco_m1m12*abs(pos_lco_m1m12)*cftc_mult +
                  w_lco_m2fly*abs(pos_lco_m2fly)*cftc_mult + w_lco_m3fly*abs(pos_lco_m3fly)*cftc_mult]
oos[, lco_raw  := c_lco_m1m2 + c_lco_m1m3 + c_lco_m1m6 + c_lco_m1m12 +
                  c_lco_m2fly + c_lco_m3fly]
oos[, lco_port_ret := fifelse(lco_wsum > 0, lco_raw / lco_wsum, NA_real_)]

# ── 13. Combined P&L (CL + LCO, equal notional) ──────────────────────────────
oos[, comb_wsum := cl_wsum + lco_wsum]
oos[, comb_raw  := cl_raw  + lco_raw]
oos[, port_ret  := fifelse(comb_wsum > 0, comb_raw / comb_wsum, NA_real_)]

# Outright M1 benchmark
oos[, bench_ret := pos_out * ret_out]

# ── 14. Performance helpers ───────────────────────────────────────────────────
ann_sharpe <- function(r, ann = 252) {
  r <- r[!is.na(r) & is.finite(r)]
  if (length(r) < 10) return(NA_real_)
  mean(r) / sd(r) * sqrt(ann)
}
hit_rate <- function(r) {
  r <- r[!is.na(r) & is.finite(r) & r != 0]
  if (length(r) == 0) return(NA_real_)
  mean(r > 0) * 100
}
max_dd <- function(r) {
  r <- r[!is.na(r) & is.finite(r)]
  if (length(r) == 0) return(NA_real_)
  eq <- cumprod(1 + r); pk <- cummax(eq)
  min((eq - pk) / pk) * 100
}
calmar <- function(r) {
  s <- ann_sharpe(r); d <- max_dd(r)
  if (is.na(s) || is.na(d) || d == 0) NA_real_ else s / abs(d)
}

perf <- function(r, lbl = "") {
  data.table(
    label   = lbl,
    n       = sum(!is.na(r) & is.finite(r)),
    sharpe  = round(ann_sharpe(r), 3),
    hit_pct = round(hit_rate(r),   1),
    max_dd  = round(max_dd(r),     1),
    calmar  = round(calmar(r),     2)
  )
}

# ── 15. Results ───────────────────────────────────────────────────────────────
cat("\n--- Overall performance (OOS 2021-2026) ---\n")
res_overall <- rbind(
  perf(oos$cl_port_ret,  "CL strategy (with CFTC)"),
  perf(oos$cl_nc_ret,    "CL strategy (no CFTC)"),
  perf(oos$lco_port_ret, "LCO strategy (with CFTC)"),
  perf(oos$port_ret,     "Combined CL+LCO (with CFTC)"),
  perf(oos$bench_ret,    "Outright CL M1 (baseline)")
)
print(res_overall, digits = 4)

# ── 16. Per-regime breakdown ──────────────────────────────────────────────────
cat("\n--- Per-regime: CL vs LCO vs Combined Sharpe ---\n")
reg_perf <- oos[!is.na(cl_regime), .(
  n_obs      = .N,
  sharpe_cl  = round(ann_sharpe(cl_port_ret),  2),
  sharpe_lco = round(ann_sharpe(lco_port_ret), 2),
  sharpe_comb= round(ann_sharpe(port_ret),     2),
  dd_cl      = round(max_dd(cl_port_ret),      1),
  dd_lco     = round(max_dd(lco_port_ret),     1),
  dd_comb    = round(max_dd(port_ret),         1)
), by = cl_regime][order(cl_regime)]
print(reg_perf, digits = 4)

# ── 17. CFTC overlay analysis ─────────────────────────────────────────────────
cat("\n--- CFTC regime: CL strategy performance ---\n")
cftc_perf <- oos[!is.na(cftc_regime) & !is.na(cl_port_ret), .(
  n_obs   = .N,
  sharpe_cl  = round(ann_sharpe(cl_port_ret),  2),
  sharpe_lco = round(ann_sharpe(lco_port_ret), 2),
  pos_z_avg  = round(mean(pos_z, na.rm = TRUE), 2)
), by = cftc_regime][order(-sharpe_cl)]
print(cftc_perf)

# ── 18. Annual performance ────────────────────────────────────────────────────
cat("\n--- Annual Sharpe ---\n")
oos[, year := format(date, "%Y")]
ann <- oos[, .(
  cl   = round(ann_sharpe(cl_port_ret),  2),
  lco  = round(ann_sharpe(lco_port_ret), 2),
  comb = round(ann_sharpe(port_ret),     2),
  bch  = round(ann_sharpe(bench_ret),    2)
), by = year][order(year)]
cat(sprintf("  %-6s  %8s  %8s  %8s  %8s\n", "Year","CL","LCO","Combined","Outright"))
for (i in seq_len(nrow(ann))) {
  r <- ann[i]
  fmt <- function(x) if (is.na(x)) "      NA" else sprintf("%+8.2f", x)
  cat(sprintf("  %-6s  %8s  %8s  %8s  %8s\n",
              r$year, fmt(r$cl), fmt(r$lco), fmt(r$comb), fmt(r$bch)))
}

# ── 19. CL instrument contribution by regime ──────────────────────────────────
cat("\n--- CL: avg weighted contribution per active day by regime (bps) ---\n")
cat(sprintf("  %-26s  %8s  %8s  %8s  %8s  %8s  %8s  %8s\n",
            "Regime","Outright","M1-M2","M1-M3","M1-M6","M1-M12","M2-fly","M3-fly"))
cl_contrib <- c("c_out","c_m1m2","c_m1m3","c_m1m6","c_m1m12","c_m2fly","c_m3fly")
cl_weights  <- c("w_out","w_m1m2","w_m1m3","w_m1m6","w_m1m12","w_m2fly","w_m3fly")
cl_pos      <- c("pos_out","pos_m1m2","pos_m1m3","pos_m1m6","pos_m1m12","pos_m2fly","pos_m3fly")

for (reg in sort(unique(oos$cl_regime[!is.na(oos$cl_regime)]))) {
  sub <- oos[cl_regime == reg & !is.na(cl_port_ret)]
  vals <- mapply(function(cc, wc, pc) {
    aw <- sub[[wc]] * abs(sub[[pc]])
    tw <- sum(aw, na.rm = TRUE)
    if (tw == 0) return(NA_real_)
    sum(sub[[cc]], na.rm = TRUE) / tw * 10000
  }, cl_contrib, cl_weights, cl_pos)
  cat(sprintf("  %-26s  %8.1f  %8.1f  %8.1f  %8.1f  %8.1f  %8.1f  %8.1f\n",
              reg, vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7]))
}

# ── 20. LCO instrument contribution by regime ─────────────────────────────────
cat("\n--- LCO: avg weighted contribution per active day by regime (bps) ---\n")
cat(sprintf("  %-26s  %8s  %8s  %8s  %8s  %8s  %8s\n",
            "Regime","M1-M2","M1-M3","M1-M6","M1-M12","M2-fly","M3-fly"))
lco_contrib <- c("c_lco_m1m2","c_lco_m1m3","c_lco_m1m6","c_lco_m1m12","c_lco_m2fly","c_lco_m3fly")
lco_wcols2  <- c("w_lco_m1m2","w_lco_m1m3","w_lco_m1m6","w_lco_m1m12","w_lco_m2fly","w_lco_m3fly")
lco_pcols   <- c("pos_lco_m1m2","pos_lco_m1m3","pos_lco_m1m6","pos_lco_m1m12","pos_lco_m2fly","pos_lco_m3fly")

for (reg in sort(unique(oos$cl_regime[!is.na(oos$cl_regime)]))) {
  sub <- oos[cl_regime == reg & !is.na(lco_port_ret)]
  vals <- mapply(function(cc, wc, pc) {
    aw <- sub[[wc]] * abs(sub[[pc]])
    tw <- sum(aw, na.rm = TRUE)
    if (tw == 0) return(NA_real_)
    sum(sub[[cc]], na.rm = TRUE) / tw * 10000
  }, lco_contrib, lco_wcols2, lco_pcols)
  cat(sprintf("  %-26s  %8.1f  %8.1f  %8.1f  %8.1f  %8.1f  %8.1f\n",
              reg, vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]))
}

# ── 21. Quarterly cumulative P&L ─────────────────────────────────────────────
cat("\n--- Quarterly cumulative P&L ---\n")
oos[, qtr := paste0(format(date,"%Y"),"Q",ceiling(as.integer(format(date,"%m"))/3))]
qpnl <- oos[, .(
  cl   = sum(cl_port_ret,  na.rm = TRUE),
  lco  = sum(lco_port_ret, na.rm = TRUE),
  comb = sum(port_ret,     na.rm = TRUE),
  bch  = sum(bench_ret,    na.rm = TRUE)
), by = .(year, qtr)][order(year, qtr)]
qpnl[, `:=`(cum_cl   = cumsum(cl),
             cum_lco  = cumsum(lco),
             cum_comb = cumsum(comb),
             cum_bch  = cumsum(bch))]
cat(sprintf("  %-7s  %10s  %10s  %10s  %10s\n","Quarter","CL","LCO","Combined","Outright"))
for (i in seq_len(nrow(qpnl))) {
  r <- qpnl[i]
  cat(sprintf("  %-7s  %+10.3f  %+10.3f  %+10.3f  %+10.3f\n",
              r$qtr, r$cum_cl, r$cum_lco, r$cum_comb, r$cum_bch))
}

# ── 22. Save ──────────────────────────────────────────────────────────────────
cat("\n[22] Saving output...\n")

save_cols <- c(
  "date","cl_regime","sig","threshold",
  "cftc_regime","pos_z","cftc_signal","cftc_mult",
  # CL positions
  "pos_out","pos_m1m2","pos_m1m3","pos_m1m6","pos_m1m12","pos_m2fly","pos_m3fly",
  # LCO positions
  "pos_lco_m1m2","pos_lco_m1m3","pos_lco_m1m6","pos_lco_m1m12","pos_lco_m2fly","pos_lco_m3fly",
  # CL weights
  "w_out","w_m1m2","w_m1m3","w_m1m6","w_m1m12","w_m2fly","w_m3fly",
  # LCO weights
  "w_lco_m1m2","w_lco_m1m3","w_lco_m1m6","w_lco_m1m12","w_lco_m2fly","w_lco_m3fly",
  # CL contributions
  "c_out","c_m1m2","c_m1m3","c_m1m6","c_m1m12","c_m2fly","c_m3fly",
  # LCO contributions
  "c_lco_m1m2","c_lco_m1m3","c_lco_m1m6","c_lco_m1m12","c_lco_m2fly","c_lco_m3fly",
  # P&L
  "cl_port_ret","lco_port_ret","port_ret","bench_ret",
  # returns
  "ret_out","ret_m1m2","ret_m1m3","ret_m1m6","ret_m1m12","ret_m2fly","ret_m3fly",
  "ret_lco_m1m2","ret_lco_m1m3","ret_lco_m1m6","ret_lco_m1m12","ret_lco_m2fly","ret_lco_m3fly",
  # prices / spreads
  "wti_spot","m1m2","m1m3","m1m6","m1m12","m2_fly","m3_fly",
  "lco_m1","lco_m1m2","lco_m1m3","lco_m1m6","lco_m1m12","lco_m2fly","lco_m3fly"
)
save_cols <- intersect(save_cols, names(oos))

fwrite(oos[, ..save_cols],  file.path(SAVE, "portfolio_daily.csv"))
fwrite(res_overall,          file.path(SAVE, "overall_comparison.csv"))
fwrite(reg_perf,             file.path(SAVE, "regime_performance.csv"))
fwrite(ann,                  file.path(SAVE, "annual_performance.csv"))
fwrite(qpnl,                 file.path(SAVE, "cumulative_quarterly.csv"))

cat("  portfolio_daily.csv       — full daily P&L (CL + LCO separate columns)\n")
cat("  overall_comparison.csv    — 5-way headline metrics\n")
cat("  regime_performance.csv    — per-regime CL vs LCO vs Combined Sharpe\n")
cat("  annual_performance.csv    — year-by-year CL / LCO / Combined\n")
cat("  cumulative_quarterly.csv  — equity curve by quarter\n")
cat("\n=================================================================\n")
cat("PHASE 3C COMPLETE\n")
cat("=================================================================\n")
