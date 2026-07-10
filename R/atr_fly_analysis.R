# R/atr_fly_analysis.R
# ATR analysis for all computable butterfly spreads on CL and LCO.
#
# ATR (close-only) = rolling_mean(|fly_t - fly_{t-1}|, N=14)
#
# Outputs:
#   strategy_live/final data/phase3c/atr_flies_daily.csv  — daily ATR per fly
#   strategy_live/final data/phase3c/atr_flies_regime.csv — avg ATR by regime

suppressPackageStartupMessages({ library(data.table); library(zoo) })

REPO <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"
TENT <- file.path(REPO, "strategy_live/final data/tent_data")
OUTD <- file.path(REPO, "strategy_live/final data/phase2")
SAVE <- file.path(REPO, "strategy_live/final data/phase3c")

cl      <- fread(file.path(TENT, "cl_curve_daily.csv"))
lco     <- fread(file.path(TENT, "lco_curve_daily.csv"))
oos     <- fread(file.path(OUTD, "oos_signals_v2.csv"))
lco_reg <- fread(file.path(REPO, "output/LCO/regime_labels_LCO.csv"))

cl[,      date := as.Date(date)]
lco[,     date := as.Date(date)]
oos[,     date := as.Date(date)]
lco_reg[, date := as.Date(date)]
setorder(cl, date); setorder(lco, date)

# ── Build fly ladder ──────────────────────────────────────────────────────────
# Butterfly: fly(n) = m(n-1) - 2*m(n) + m(n+1)
# We name by the middle leg: m2_fly has m2 as middle

# CL: m4 is sparse (357/1920 rows) so only adjacent-month flies that avoid m4
# fly_m2 = m1-2*m2+m3, fly_m6 = m5-2*m6+m7, fly_m7 = m6-2*m7+m8
cl[, fly_m2 := m1 - 2*m2 + m3]
cl[, fly_m6 := m5 - 2*m6 + m7]
cl[, fly_m7 := m6 - 2*m7 + m8]

# LCO: all months m1-m12 available; compute M2-M8 adjacent butterflies
lco[, fly_m2 := m1 - 2*m2  + m3]
lco[, fly_m3 := m2 - 2*m3  + m4]
lco[, fly_m4 := m3 - 2*m4  + m5]
lco[, fly_m5 := m4 - 2*m5  + m6]
lco[, fly_m6 := m5 - 2*m6  + m7]
lco[, fly_m7 := m6 - 2*m7  + m8]
lco[, fly_m8 := m7 - 2*m8  + m9]

# ── ATR(14): rolling mean of |daily change| ───────────────────────────────────
atr <- function(x, n = 14) {
  chg <- abs(diff(x, lag = 1))
  c(NA, zoo::rollmean(chg, k = n, fill = NA, align = "right"))
}

cl_fly_cols  <- c("fly_m2","fly_m6","fly_m7")
lco_fly_cols <- paste0("fly_m", 2:8)

for (col in cl_fly_cols)  { atr_col <- paste0("atr_", col); cl[,  (atr_col) := atr(get(col))] }
for (col in lco_fly_cols) { atr_col <- paste0("atr_", col); lco[, (atr_col) := atr(get(col))] }

# ── Merge regime labels ───────────────────────────────────────────────────────
cl_reg_oos <- oos[, .(date, cl_regime)]
cl  <- merge(cl,  cl_reg_oos,                              by = "date", all.x = TRUE)
lco <- merge(lco, lco_reg[, .(date, lco_regime = regime_label)], by = "date", all.x = TRUE)

# ── Combine into long daily ATR table ────────────────────────────────────────
cl_atr_cols  <- paste0("atr_", c("fly_m2","fly_m6","fly_m7"))
lco_atr_cols <- paste0("atr_fly_m", 2:8)

cl_long <- melt(
  cl[, c("date","cl_regime", cl_atr_cols), with = FALSE],
  id.vars = c("date","cl_regime"),
  variable.name = "fly", value.name = "atr"
)
cl_long[, product := "CL"]
cl_long[, fly := gsub("atr_fly_", "", fly)]   # → m2, m3, m6, m7
setnames(cl_long, "cl_regime", "regime")

lco_long <- melt(
  lco[, c("date","lco_regime", lco_atr_cols), with = FALSE],
  id.vars = c("date","lco_regime"),
  variable.name = "fly", value.name = "atr"
)
lco_long[, product := "LCO"]
lco_long[, fly := gsub("atr_fly_", "", fly)]
setnames(lco_long, "lco_regime", "regime")

daily <- rbindlist(list(cl_long, lco_long), fill = TRUE)
daily <- daily[!is.na(atr)]
daily[, year := format(date, "%Y")]

fwrite(daily, file.path(SAVE, "atr_flies_daily.csv"))
cat(sprintf("Saved atr_flies_daily.csv  (%d rows)\n", nrow(daily)))

# ── Summary: average ATR by product × fly × regime ───────────────────────────
regime_atr <- daily[!is.na(regime) & regime != "Warm-Up", .(
  n_days   = .N,
  atr_mean = round(mean(atr, na.rm=TRUE), 4),
  atr_med  = round(median(atr, na.rm=TRUE), 4),
  atr_p90  = round(quantile(atr, 0.9, na.rm=TRUE), 4)
), by = .(product, fly, regime)][order(product, fly, regime)]

fwrite(regime_atr, file.path(SAVE, "atr_flies_regime.csv"))
cat(sprintf("Saved atr_flies_regime.csv (%d rows)\n", nrow(regime_atr)))

# ── Summary: average ATR by product × fly (overall) ──────────────────────────
cat("\n=== Overall ATR by fly ===\n")
overall <- daily[, .(
  n_days   = .N,
  atr_mean = round(mean(atr, na.rm=TRUE), 4),
  atr_med  = round(median(atr, na.rm=TRUE), 4)
), by = .(product, fly)][order(product, fly)]
print(overall)

# ── Summary: average ATR by regime (all flies combined) ──────────────────────
cat("\n=== ATR by regime (mean across flies) ===\n")
by_regime <- daily[!is.na(regime) & regime != "Warm-Up", .(
  atr_mean = round(mean(atr, na.rm=TRUE), 4)
), by = .(product, regime)][order(product, -atr_mean)]
print(by_regime)

cat("\n=== DONE ===\n")
