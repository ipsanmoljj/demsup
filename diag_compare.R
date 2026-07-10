library(DBI)
library(RSQLite)
library(data.table)
library(lubridate)

root  <- "C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup"
db1m  <- file.path(root, "backtesting/extra/bars_1min_20260624.db")
db15m <- file.path(root, "backtesting/bars_15min_20260623.db")

# ── Helpers ───────────────────────────────────────────────────────────────────

load_db_contracts <- function(db_path, prefixes = c("CL_","CO_")) {
  con  <- dbConnect(SQLite(), db_path)
  tbls <- dbListTables(con)
  keep <- tbls[Reduce(`|`, lapply(prefixes, function(p) startsWith(tbls, p)))]
  dt_list <- lapply(keep, function(t) {
    d <- as.data.table(dbGetQuery(con,
      sprintf("SELECT timestamp, close FROM \"%s\" ORDER BY timestamp", t)))
    d[, ts       := as.POSIXct(timestamp, tz = "UTC")]
    d[, contract := t]
    d[, .(contract, ts, close)]
  })
  dbDisconnect(con)
  rbindlist(dt_list)
}

last_price_before <- function(data, contract_name, cutoff_utc) {
  rows <- data[contract == contract_name & ts <= cutoff_utc]
  if (nrow(rows) == 0) return(NA_real_)
  tail(rows$close, 1)
}

spreads_from_outright <- function(p, M) {
  # p must be a named list/vector: names = M1,M2,...
  m1 <- p["M1"]; m2 <- p["M2"]; m3 <- p["M3"]; m6 <- p["M6"]
  c(m1m2  = unname(m1 - m2),
    m2m3  = unname(m2 - m3),
    m1m6  = unname(m1 - m6),
    fly123 = unname(m1 - 2*m2 + m3),
    fly136 = unname(m1 - 2*m3 + m6))
}

# ── Load bar data ─────────────────────────────────────────────────────────────
cat("Loading bar data...\n")
bars1m  <- load_db_contracts(db1m)
bars15m <- load_db_contracts(db15m)
bars_all <- rbindlist(list(bars1m, bars15m))
bars_all <- bars_all[order(contract, ts)]
bars_all <- bars_all[, .SD[!duplicated(ts)], by = contract]  # drop dups

cat("1-min DB range :", format(min(bars1m$ts)), "to", format(max(bars1m$ts)), "\n")
cat("15-min DB range:", format(min(bars15m$ts)), "to", format(max(bars15m$ts)), "\n")

# ── Contract mapping for June 24 2026 ────────────────────────────────────────
# July 2026 (N26) contract expired; front month = August 2026 (Q26)
# M1=Aug(Q26), M2=Sep(U26), M3=Oct(V26), M4=Nov(X26), M5=Dec(Z26), M6=Jan27(F27)

CL_M <- c(M1="CL_Q26", M2="CL_U26", M3="CL_V26", M4="CL_X26", M5="CL_Z26", M6="CL_F27")
CO_M <- c(M1="CO_Q26", M2="CO_U26", M3="CO_V26", M4="CO_X26", M5="CO_Z26", M6="CO_F27")

# ── Time windows (all UTC) ────────────────────────────────────────────────────
# EIA releases 10:30 ET = 14:30 UTC on June 24
t_pre   <- as.POSIXct("2026-06-24 14:29:00", tz = "UTC")  # 10:29 ET (just before)
t_5m    <- as.POSIXct("2026-06-24 14:35:00", tz = "UTC")  # 10:35 ET (+5 min)
t_1h    <- as.POSIXct("2026-06-24 15:30:00", tz = "UTC")  # 11:30 ET (+1 hr)
t_eod24 <- as.POSIXct("2026-06-24 19:30:00", tz = "UTC")  # ~15:30 ET (close Jun 24)
# June 25 close ~00:30 UTC (=20:30 ET Jun 24 electronic) - max available
t_jun25 <- as.POSIXct("2026-06-25 00:29:00", tz = "UTC")

cat("\n=== CL OHLC prices at key windows ===\n")
windows <- list(
  pre   = t_pre,
  p5m   = t_5m,
  p1h   = t_1h,
  eod24 = t_eod24,
  jun25 = t_jun25
)

# Fetch prices for each CL leg at each window
cl_px <- lapply(windows, function(t) {
  setNames(sapply(names(CL_M), function(mn) {
    last_price_before(bars_all, CL_M[mn], t)
  }), names(CL_M))
})

co_px <- lapply(windows, function(t) {
  setNames(sapply(names(CO_M), function(mn) {
    last_price_before(bars_all, CO_M[mn], t)
  }), names(CO_M))
})

cat("\nCL individual contract prices:\n")
cat(sprintf("  %-4s %-12s %6s %6s %6s %6s %6s\n",
            "Leg","Contract","Pre","5min","1hr","EOD24","Jun25"))
for (mn in names(CL_M)) {
  cat(sprintf("  %-4s %-12s %6.2f %6.2f %6.2f %6.2f %6.2f\n",
              mn, CL_M[mn],
              ifelse(is.na(cl_px$pre[mn]),   0, cl_px$pre[mn]),
              ifelse(is.na(cl_px$p5m[mn]),   0, cl_px$p5m[mn]),
              ifelse(is.na(cl_px$p1h[mn]),   0, cl_px$p1h[mn]),
              ifelse(is.na(cl_px$eod24[mn]), 0, cl_px$eod24[mn]),
              ifelse(is.na(cl_px$jun25[mn]), 0, cl_px$jun25[mn])))
}

# ── Compute spreads for each window ──────────────────────────────────────────
cat("\n=== CL Calendar Spreads ($/bbl) ===\n")
cat(sprintf("  %-8s %7s %7s %7s %7s %7s | %7s %7s %7s %7s\n",
            "Spread", "Pre", "+5min", "+1hr", "EOD24", "Jun25",
            "Ch5m", "Ch1h", "ChEOD", "ChJ25"))

cl_spr <- lapply(cl_px, spreads_from_outright, M = CL_M)
spr_names <- c("m1m2","m2m3","m1m6","fly123","fly136")
cl_actuals <- data.table()

for (spr in spr_names) {
  pre_v  <- cl_spr$pre[spr]
  p5_v   <- cl_spr$p5m[spr]
  p1h_v  <- cl_spr$p1h[spr]
  eod_v  <- cl_spr$eod24[spr]
  j25_v  <- cl_spr$jun25[spr]
  ch5m   <- p5_v  - pre_v
  ch1h   <- p1h_v - pre_v
  cheod  <- eod_v - pre_v
  chj25  <- j25_v - pre_v
  cat(sprintf("  %-8s %7.4f %7.4f %7.4f %7.4f %7.4f | %7.4f %7.4f %7.4f %7.4f\n",
              spr, pre_v, p5_v, p1h_v, eod_v, j25_v, ch5m, ch1h, cheod, chj25))
  cl_actuals <- rbind(cl_actuals, data.table(
    product=1, spread=spr, pre=pre_v, p5m=p5_v, p1h=p1h_v, eod24=eod_v, jun25=j25_v,
    ch_5m=ch5m, ch_1h=ch1h, ch_eod=cheod, ch_j25=chj25
  ))
}

cat("\n=== CO (Brent/LCO) Calendar Spreads ($/bbl) ===\n")
cat(sprintf("  %-8s %7s %7s %7s %7s | %7s %7s %7s\n",
            "Spread","Pre","+5min","+1hr","Jun25","Ch5m","Ch1h","ChJ25"))
co_spr <- lapply(co_px, spreads_from_outright, M = CO_M)
for (spr in c("m1m2","m2m3","m1m6")) {
  pre_v <- co_spr$pre[spr]; p5_v  <- co_spr$p5m[spr]
  p1h_v <- co_spr$p1h[spr]; j25_v <- co_spr$jun25[spr]
  cat(sprintf("  %-8s %7.4f %7.4f %7.4f %7.4f | %7.4f %7.4f %7.4f\n",
              spr, pre_v, p5_v, p1h_v, j25_v,
              p5_v-pre_v, p1h_v-pre_v, j25_v-pre_v))
}

# ── Load model predictions ────────────────────────────────────────────────────
pred_path <- file.path(root, "output/live_eia_test_20260624.csv")
if (!file.exists(pred_path)) stop("Run live_eia_test.R first.")
preds <- fread(pred_path)

# Best tier: sfm_t5_enhanced
best_tier <- "sfm_t5_enhanced"
p <- preds[tier == best_tier & product == "CL"]

# ── Side-by-side comparison ───────────────────────────────────────────────────
cat("\n")
cat(strrep("=", 75), "\n")
cat("  MODEL vs ACTUAL COMPARISON -- June 24 2026 EIA Release\n")
cat(strrep("=", 75), "\n")
cat("  EIA data: Crude -15,148 kb draw  |  Cushing -9,060 kb\n")
cat("  Model surprise_z = -0.31 (NEUTRAL -- below |0.4| filter threshold)\n")
cat("  This means the draw was expected given where stocks are vs 5yr avg\n")
cat(strrep("-", 75), "\n")

cat(sprintf("\n  CL (WTI) -- tier: %s\n", best_tier))
cat(sprintf("  %-8s  %8s  %8s  %8s  %8s  %8s  %s\n",
            "Spread","Pred(dir)","Actual5m","Actual1h","ActualEOD","ActualJ25","HIT?"))

for (spr in spr_names) {
  pr <- p[spread == spr]
  if (!nrow(pr)) next
  actual5m  <- cl_spr$p5m[spr]  - cl_spr$pre[spr]
  actual1h  <- cl_spr$p1h[spr]  - cl_spr$pre[spr]
  actualeod <- cl_spr$eod24[spr] - cl_spr$pre[spr]
  actualj25 <- cl_spr$jun25[spr] - cl_spr$pre[spr]
  pred_sign <- sign(pr$pred_val)
  hit5m  <- if(!is.na(actual5m))  sign(actual5m)  == pred_sign else NA
  hit1h  <- if(!is.na(actual1h))  sign(actual1h)  == pred_sign else NA
  hiteod <- if(!is.na(actualeod)) sign(actualeod) == pred_sign else NA
  hitj25 <- if(!is.na(actualj25)) sign(actualj25) == pred_sign else NA
  cat(sprintf("  %-8s  %+8.4f  %+8.4f  %+8.4f  %+8.4f  %+8.4f  %s/%s/%s/%s\n",
              spr, pr$pred_val,
              ifelse(is.na(actual5m),0,actual5m),
              ifelse(is.na(actual1h),0,actual1h),
              ifelse(is.na(actualeod),0,actualeod),
              ifelse(is.na(actualj25),0,actualj25),
              ifelse(is.na(hit5m),"?",ifelse(hit5m,"Y","N")),
              ifelse(is.na(hit1h),"?",ifelse(hit1h,"Y","N")),
              ifelse(is.na(hiteod),"?",ifelse(hiteod,"Y","N")),
              ifelse(is.na(hitj25),"?",ifelse(hitj25,"Y","N"))))
}

# LCO
cat(sprintf("\n  LCO (Brent/CO) -- tier: %s\n", best_tier))
cat(sprintf("  %-8s  %8s  %8s  %8s  %8s  %s\n",
            "Spread","Pred(dir)","Actual5m","Actual1h","ActualJ25","HIT(5m/1h/J25)?"))
p_lco <- preds[tier == best_tier & product == "LCO"]
for (spr in c("m1m2","m2m3","m1m6")) {
  pr <- p_lco[spread == spr]
  if (!nrow(pr)) next
  a5m  <- co_spr$p5m[spr]  - co_spr$pre[spr]
  a1h  <- co_spr$p1h[spr]  - co_spr$pre[spr]
  aj25 <- co_spr$jun25[spr] - co_spr$pre[spr]
  psgn <- sign(pr$pred_val)
  cat(sprintf("  %-8s  %+8.4f  %+8.4f  %+8.4f  %+8.4f  %s/%s/%s\n",
              spr, pr$pred_val,
              ifelse(is.na(a5m),0,a5m), ifelse(is.na(a1h),0,a1h),
              ifelse(is.na(aj25),0,aj25),
              ifelse(is.na(a5m),"?",ifelse(sign(a5m)==psgn,"Y","N")),
              ifelse(is.na(a1h),"?",ifelse(sign(a1h)==psgn,"Y","N")),
              ifelse(is.na(aj25),"?",ifelse(sign(aj25)==psgn,"Y","N"))))
}

# ── Summary hit table ─────────────────────────────────────────────────────────
cat("\n", strrep("-", 75), "\n")
cat("  HIT SUMMARY across all tiers (CL m1m2, EOD24 as reference actual):\n")
cat(sprintf("  %-22s  %8s  %8s  %8s\n", "Tier","Pred","Actual(EOD)","HIT?"))
for (tnm in unique(preds$tier)) {
  pr <- preds[tier==tnm & product=="CL" & spread=="m1m2"]
  if (!nrow(pr)) next
  actualeod <- cl_spr$eod24["m1m2"] - cl_spr$pre["m1m2"]
  hit <- if(!is.na(actualeod)) sign(pr$pred_val)==sign(actualeod) else NA
  cat(sprintf("  %-22s  %+8.4f  %+8.4f  %s\n",
              tnm, pr$pred_val, actualeod,
              ifelse(is.na(hit),"?",ifelse(hit,"YES","NO"))))
}

# ── Price trajectory ──────────────────────────────────────────────────────────
cat("\n", strrep("-", 75), "\n")
cat("  CL M1 (Aug 2026 front month) price trajectory:\n")
cl_m1 <- bars_all[contract == "CL_Q26"][order(ts)]
key_ts <- c("2026-06-24 13:30:00","2026-06-24 14:25:00","2026-06-24 14:30:00",
            "2026-06-24 14:35:00","2026-06-24 15:30:00","2026-06-24 19:30:00",
            "2026-06-25 00:00:00")
key_labels <- c("09:30ET(open)","10:25ET(pre-EIA)","10:30ET(EIA out)",
                "10:35ET(+5min)","11:30ET(+1hr)","15:30ET(EOD)","next-day")
for (i in seq_along(key_ts)) {
  t  <- as.POSIXct(key_ts[i], tz="UTC")
  px <- last_price_before(cl_m1, "CL_Q26", t)
  cat(sprintf("    %-20s  CL M1 = %.2f\n", key_labels[i], ifelse(is.na(px),0,px)))
}

cat(strrep("=", 75), "\n")
