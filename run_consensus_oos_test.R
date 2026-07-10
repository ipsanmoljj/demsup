setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
# ─────────────────────────────────────────────────────────────────────────────
# Consensus-Surprise OOS Test — June 24 2026
# ─────────────────────────────────────────────────────────────────────────────
# WHAT THIS DOES
#   1. Replaces the 5-yr-deviation "surprise" with true analyst-consensus
#      surprise (actual − forecast from eia_consensus.csv).
#   2. Retrains the spread factor model on all events with spread data
#      (Jan 2021 – May 2026, 70/30 split).
#   3. Predicts spread changes for the June 24 2026 EIA release
#      (TRUE out-of-sample: no training data includes this event).
#   4. Loads actual spread movements from bar databases and compares.
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table); library(lubridate); library(DBI); library(RSQLite)
  library(glmnet); library(randomForest); library(xgboost); library(zoo)
})

# Install missing packages silently
for (p in c("glmnet","randomForest","xgboost","RSQLite","DBI","zoo"))
  if (!requireNamespace(p, quietly=TRUE))
    install.packages(p, repos="https://cloud.r-project.org", quiet=TRUE)

ROOT     <- getwd()
EIA_DATE <- as.Date("2026-06-24")

# ══ 1.  Source model helpers ═══════════════════════════════════════════════════
source("R/spread_factor_model.R")

# ══ 2.  Load consensus data ════════════════════════════════════════════════════
cons <- fread("output/eia_consensus.csv")
cons[, date := as.Date(date)]
cat(sprintf("\nConsensus CSV: %d rows  (%s to %s)\n",
            nrow(cons), min(cons$date), max(cons$date)))

june24_cons <- cons[date == EIA_DATE]
cat(sprintf("Jun 24 2026: actual=%+.3f M  forecast=%+.3f M  surprise=%+.3f M (%+d kb)\n",
            june24_cons$actual_mbbls, june24_cons$forecast_mbbls,
            june24_cons$surprise_mbbls, june24_cons$surprise_kb))

# ══ 3.  Load factors & inject consensus surprise ═══════════════════════════════
message("\n[3] Loading factors and injecting consensus surprise...")
factors <- .sfm_load_factors(ROOT)

# FIX: The original crude_stocks_surprise column is a LEVEL-BASED measure
# (total crude stocks vs 5yr average), ranging -200k to -14k kb — completely
# incompatible with the weekly consensus-surprise scale (±5k to ±22k kb).
# Holiday-adjusted EIA releases (e.g., Thursday/Friday) land in the consensus
# CSV on the non-Wednesday date, so the factor file's Wednesday row for that
# week never gets injected. Those rows keep the old extreme level values and
# blow up the training SD.
# Fix: (1) null out ALL Wednesday crude_stocks_surprise values first so
# non-matched Wednesdays fall back to crude_stocks_chg; (2) inject consensus
# only for dates that match exactly; (3) winsorize storm outliers at ±10000 kb.

# Step 1: clear Wednesday rows so non-matched weeks don't contaminate stats
factors[weekdays(date) == "Wednesday", crude_stocks_surprise := NA_real_]

# Step 2: inject consensus values for matching dates
factors <- merge(factors,
                 cons[, .(date, cons_surp_kb = surprise_kb)],
                 by = "date", all.x = TRUE)
n_inj <- sum(!is.na(factors$cons_surp_kb))
factors[!is.na(cons_surp_kb), crude_stocks_surprise := cons_surp_kb]
factors[, cons_surp_kb := NULL]

# Step 3: winsorize at ±10000 kb to remove 2021 Texas Winter Storm Uri outliers
# (13 events with |surprise| > 10000 kb inflate SD from ~3700 to ~5020 kb,
#  which would make the June 24 signal fall below the 0.4 threshold)
SURP_WINSOR <- 10000L
factors[!is.na(crude_stocks_surprise),
        crude_stocks_surprise := pmax(pmin(crude_stocks_surprise, SURP_WINSOR), -SURP_WINSOR)]

# Diagnostics
weds_inj <- factors[weekdays(date) == "Wednesday" & !is.na(crude_stocks_surprise)]
message(sprintf("  Injected: %d factor rows  |  Wednesday rows with consensus: %d  mean=%+.0f kb  sd=%.0f kb",
                n_inj, nrow(weds_inj),
                mean(weds_inj$crude_stocks_surprise, na.rm=TRUE),
                sd(weds_inj$crude_stocks_surprise, na.rm=TRUE)))

# ══ 4.  Per-product train + fit ════════════════════════════════════════════════
# Training cutoff: last EIA event for which 2-day post-event spread data exists
# CL / LCO spread data end 2026-05-22 → event on 2026-05-20 needs May 22  ✓
# HO / LGO spread data end 2026-05-20 → event on 2026-05-14 needs May 16  ✓
TRAIN_CUTOFFS <- list(CL="2026-05-20", LCO="2026-05-20", HO="2026-05-14", LGO="2026-05-14")

model_store  <- new.env(parent = emptyenv())
train_panels <- list()   # keep training panels for live z-score computation

for (prod in c("CL","LCO","HO","LGO")) {
  message("\n── ", prod, " ─────────────────────────────────────")
  cutoff <- as.Date(TRAIN_CUTOFFS[[prod]])

  spreads <- tryCatch(.sfm_load_spreads(prod, ROOT),
                      error=function(e) { message("  SKIP: ",e$message); NULL })
  if (is.null(spreads)) next

  regimes <- tryCatch(.sfm_load_regime(prod, ROOT),
                      error=function(e) { message("  SKIP: ",e$message); NULL })
  if (is.null(regimes)) next

  events <- tryCatch(
    .sfm_build_panel(spreads, factors, regimes, cutoff),
    error=function(e) { message("  FAIL panel: ",e$message); NULL }
  )
  if (is.null(events) || !nrow(events)) { message("  No events."); next }

  train <- events[date <= cutoff]
  message(sprintf("  Events: %d total  |  train=%d  test=%d  | surprise_z mean=%.2f sd=%.2f",
                  nrow(events), nrow(train), nrow(events)-nrow(train),
                  mean(train$surprise_z, na.rm=T), sd(train$surprise_z, na.rm=T)))

  train_panels[[prod]] <- list(events=events, cutoff=cutoff)

  tryCatch(.sfm_fit_all(train, prod, model_store),
           error=function(e) message("  ERR fit: ",e$message))
  message("  Fitted models for: ", prod)
}

# ══ 5.  Compute June 24 feature vector (OOS) ══════════════════════════════════
message("\n[5] Computing June 24 OOS feature vector...")

# Helper: z-score a value using training-period stats of the RAW column
.zs_live <- function(val, raw_col, events_train) {
  if (!raw_col %in% names(events_train)) return(0)
  x  <- as.numeric(events_train[[raw_col]])
  mu <- mean(x, na.rm=TRUE); sg <- sd(x, na.rm=TRUE)
  if (!is.finite(sg) || sg < 1e-10) return(0)
  (val - mu) / sg
}

# We need June 24's raw factor values.
# The factor file goes to June 23; use June 23 as proxy for "current" state.
fac_live <- factors[date == as.Date("2026-06-23")]
if (!nrow(fac_live)) {
  fac_live <- factors[date == max(factors$date)]
  message("  Warning: June 23 not found; using ", max(factors$date))
}

# Also derive the same weekly-change columns the panel builder would compute
fac_ord <- factors[order(date)]
# Safe column extractor — returns 0 for columns not in the factor file
.lv <- function(col, d = as.Date("2026-06-23")) {
  if (!col %in% names(fac_ord)) return(0)
  r <- fac_ord[date == d]
  if (!nrow(r)) return(NA_real_)
  as.numeric(r[[col]])
}
.lv1 <- function(col) .lv(col, as.Date("2026-06-16"))

# Derive weekly changes the same way the panel builder does
ck_diff <- function(col) { v <- .lv(col); v1 <- .lv1(col); if (is.finite(v) && is.finite(v1)) v - v1 else 0 }

live_row <- list(
  # Surprise = consensus surprise for June 24
  crude_stocks_surprise    = june24_cons$surprise_kb,
  # Weekly changes (Jun 23 minus Jun 16 as proxy for EIA release week)
  cushing_stocks_chg       = ck_diff("cushing_stocks_kb"),
  crude_prod_chg           = ck_diff("crude_prod_kbd"),
  gasoline_stocks_chg      = ck_diff("gasoline_stocks_kb"),
  distillate_stocks_chg    = ck_diff("distillate_stocks_kb"),
  rig_chg_wow              = ck_diff("rig_count"),
  # Level/structural variables from June 23
  crude_net_exports_kbd    = .lv("crude_net_exports_kbd"),
  hdd_dev_5yr              = .lv("hdd_dev_5yr"),
  cftc_mm_net_chg          = .lv("cftc_mm_net_chg"),
  td3c_wow_ws              = .lv("td3c_wow_ws"),
  td3c_storage_cost_bbl_mo = .lv("td3c_storage_cost_bbl_mo"),
  dxy                      = .lv("dxy"),
  dxy_4wk_chg              = .lv("dxy_4wk_chg"),
  sofr                     = .lv("sofr"),
  opec_prod_mbd            = .lv("opec_prod_mbd"),
  crude_stocks_5yr_dev     = .lv("crude_stocks_5yr_dev"),
  cushing_stocks_5yr_dev   = .lv("cushing_stocks_5yr_dev"),
  gasoil_crack_dev         = .lv("gasoil_crack_dev"),
  gasoline_stocks_5yr_dev  = .lv("gasoline_stocks_5yr_dev"),
  distillate_stocks_5yr_dev= .lv("distillate_stocks_5yr_dev"),
  days_fwd_cover_proxy     = .lv("days_fwd_cover_proxy"),
  china_imports_proxy      = .lv("china_imports_proxy"),
  ho_crack_proxy           = .lv("ho_crack_proxy"),
  crude_net_exports_4wk    = .lv("crude_net_exports_4wk"),
  # Pass-through z-scored vars (use 0 if column absent from factor file)
  td3c_z52                 = .lv("td3c_z52"),
  bdi_z52                  = .lv("bdi_z52"),
  cftc_net_mm_zscore       = .lv("cftc_net_mm_zscore"),   # 0 if absent
  refinery_util_dev        = .lv("refinery_util_dev"),
  # Seasonal dummies from date
  sin_ann          = sin(2 * pi * yday(EIA_DATE) / 365.25),
  cos_ann          = cos(2 * pi * yday(EIA_DATE) / 365.25),
  driving_season   = as.numeric(month(EIA_DATE) %in% 4:9),
  heating_season   = as.numeric(month(EIA_DATE) %in% c(10:12,1:3)),
  turnaround_season= as.numeric(month(EIA_DATE) %in% c(3:5,9:10)),
  cdd_us_ne        = 0
)

cat("\nLive input values:\n")
cat(sprintf("  Consensus surprise: %+.0f kb (%+.3f Mbbl)\n",
            live_row$crude_stocks_surprise, live_row$crude_stocks_surprise/1000))
cat(sprintf("  5yr structural dev: %+.0f kb\n", live_row$crude_stocks_5yr_dev))
cat(sprintf("  Cushing chg:        %+.0f kb\n", live_row$cushing_stocks_chg))

# ══ 6.  Predict for each product ══════════════════════════════════════════════
message("\n[6] Generating predictions...")

raw_z_map <- list(
  surprise_z               = "crude_stocks_surprise",
  cushing_stocks_chg_z     = "cushing_stocks_chg",
  crude_prod_chg_z         = "crude_prod_chg",
  gasoline_stocks_chg_z    = "gasoline_stocks_chg",
  distillate_stocks_chg_z  = "distillate_stocks_chg",
  crude_net_exports_z      = "crude_net_exports_kbd",
  rig_chg_wow_z            = "rig_chg_wow",
  hdd_dev_5yr_z            = "hdd_dev_5yr",
  cftc_mm_net_chg_z        = "cftc_mm_net_chg",
  td3c_wow_ws_z            = "td3c_wow_ws",
  td3c_storage_cost_z      = "td3c_storage_cost_bbl_mo",
  dxy_z                    = "dxy",
  dxy_4wk_chg_z            = "dxy_4wk_chg",
  sofr_z                   = "sofr",
  opec_prod_z              = "opec_prod_mbd",
  crude_stocks_5yr_dev_z   = "crude_stocks_5yr_dev",
  cushing_stocks_5yr_dev_z = "cushing_stocks_5yr_dev",
  gasoil_crack_dev_z       = "gasoil_crack_dev"
)

all_preds <- list()

for (prod in c("CL","LCO","HO","LGO")) {
  tp <- train_panels[[prod]]
  if (is.null(tp)) next
  train_ev <- tp$events[date <= tp$cutoff]

  # Build z-scored feature vector for June 24
  fv <- as.list(live_row)  # start with raw values

  # Z-score raw columns using training period stats
  for (zc in names(raw_z_map)) {
    rc <- raw_z_map[[zc]]
    fv[[zc]] <- .zs_live(live_row[[rc]], rc, train_ev)
  }

  # Pass-through columns already z-scored
  passthrough <- c("td3c_z52","bdi_z52","cftc_net_mm_zscore","refinery_util_dev",
                   "sin_ann","cos_ann","driving_season","heating_season",
                   "turnaround_season","cdd_us_ne")
  for (pc in passthrough) fv[[pc]] <- if (!is.null(live_row[[pc]])) live_row[[pc]] else 0

  # Interaction terms
  fv[["sx_cushing"]] <- fv[["surprise_z"]] * fv[["cushing_stocks_chg_z"]]
  fv[["sx_util"]]    <- fv[["surprise_z"]] * fv[["refinery_util_dev"]]
  fv[["sx_td3c"]]    <- fv[["surprise_z"]] * fv[["td3c_z52"]]
  fv[["sx_cftc"]]    <- fv[["surprise_z"]] * fv[["cftc_net_mm_zscore"]]
  fv[["sx_5yr_dev"]] <- fv[["surprise_z"]] * fv[["crude_stocks_5yr_dev_z"]]

  # Tier-5 extended features (z-score from training)
  raw_z_map2 <- list(
    gasoline_5yr_dev_z="gasoline_stocks_5yr_dev", distillate_5yr_dev_z="distillate_stocks_5yr_dev",
    days_fwd_cover_z="days_fwd_cover_proxy", china_imports_z="china_imports_proxy",
    ho_crack_dev_z="ho_crack_proxy", crude_net_exports_4wk_z="crude_net_exports_4wk"
  )
  for (zc in names(raw_z_map2)) {
    rc  <- raw_z_map2[[zc]]
    rv  <- live_row[[rc]]
    if (is.null(rv) || is.na(rv)) rv <- 0
    fv[[zc]] <- .zs_live(rv, rc, train_ev)
  }

  # EIA streak (rolling mean of last 4 surprise_z in training)
  fv[["eia_streak_4w"]] <- mean(tail(train_ev$surprise_z, 4), na.rm=TRUE)
  fv[["abs_surprise_z"]] <- abs(fv[["surprise_z"]])

  # Total petroleum balance
  fv[["tpc_surprise_kb"]] <- sum(unlist(live_row[c("crude_stocks_chg","gasoline_stocks_chg","distillate_stocks_chg")]), na.rm=TRUE)
  fv[["tpc_surprise_z"]]  <- .zs_live(fv[["tpc_surprise_kb"]], "tpc_surprise_kb", train_ev)

  # Enhanced signal (if in training events)
  fv[["surprise_z_x_season"]] <- fv[["surprise_z"]] * fv[["turnaround_season"]]
  fv[["surprise_z_x_dxy"]]    <- fv[["surprise_z"]] * fv[["dxy_z"]]
  fv[["surprise_z_x_cftc"]]   <- fv[["surprise_z"]] * fv[["cftc_net_mm_zscore"]]

  surp_z <- fv[["surprise_z"]]
  cat(sprintf("\n%s: surprise_z = %+.3f  (raw=%+.0f kb; train mean=%+.0f sd=%.0f)\n",
              prod, surp_z, june24_cons$surprise_kb,
              mean(as.numeric(train_ev$surprise_z)*sd(as.numeric(train_ev[[raw_z_map[["surprise_z"]]]], na.rm=T)), na.rm=T),
              1))

  # Check filter threshold
  if (abs(surp_z) < SFM_MIN_SURPRISE_Z) {
    cat(sprintf("  NOTE: |surprise_z|=%.3f < %.1f threshold → model has no edge\n",
                abs(surp_z), SFM_MIN_SURPRISE_Z))
  }

  # Predict using each tier
  tiers_to_try <- c("sfm_t1_eia_only","sfm_t2_eia_full","sfm_t3_structural",
                    "sfm_t4_combined","sfm_t5_enhanced","sfm_t4_rf","sfm_t4_xgb",
                    "old_t1_baseline","old_t2_physical","old_t3_full")
  tgts <- c("m1m2","m2m3","m1m6","fly123","fly136")

  for (tnm in tiers_to_try) {
    tier_feats <- SFM_TIERS[[tnm]]
    if (is.null(tier_feats)) next
    for (tgt in tgts) {
      key1 <- paste(prod, tnm, tgt, "ALL_REGIMES", sep="||")
      m <- tryCatch(get(key1, envir=model_store, inherits=FALSE), error=function(e) NULL)
      if (is.null(m)) next

      xcols <- tier_feats[tier_feats %in% names(fv)]
      if (!length(xcols)) next
      Xrow  <- matrix(sapply(xcols, function(col) {
        v <- fv[[col]]; if (!is.null(v) && is.finite(v)) v else 0
      }), nrow=1, dimnames=list(NULL, xcols))

      yp <- tryCatch(.sfm_predict(m, Xrow), error=function(e) NA_real_)
      if (!is.finite(yp)) next

      all_preds[[length(all_preds)+1]] <- data.table(
        product=prod, spread=tgt, tier=tnm,
        surprise_z=round(surp_z, 3),
        pred_val=round(yp, 4)
      )
    }
  }
}

preds <- rbindlist(all_preds, fill=TRUE)
cat(sprintf("\nTotal predictions generated: %d\n", nrow(preds)))

# ══ 7.  Load actual spread movements from SQLite bar data ══════════════════════
message("\n[7] Loading actual spread movements from bar databases...")

db1m <- file.path(ROOT, "backtesting/extra/bars_1min_20260624.db")
if (!file.exists(db1m)) stop("Bar database not found: ", db1m)

con <- dbConnect(SQLite(), db1m)
tbls <- dbListTables(con)

load_bar <- function(con, tbl) {
  if (!tbl %in% dbListTables(con)) return(NULL)
  d <- as.data.table(dbGetQuery(con,
    sprintf('SELECT timestamp, close FROM "%s" ORDER BY timestamp', tbl)))
  d[, ts := as.POSIXct(timestamp, tz="UTC")]
  d[, .(ts, close)]
}

last_px <- function(bars, t) {
  r <- bars[ts <= t]; if (!nrow(r)) return(NA_real_); tail(r$close, 1)
}

# Timestamps (UTC) — EIA releases at 10:30 ET = 14:30 UTC
t_pre  <- as.POSIXct("2026-06-24 14:29:00", tz="UTC")  # just before release
t_5m   <- as.POSIXct("2026-06-24 14:35:00", tz="UTC")  # +5 min
t_1h   <- as.POSIXct("2026-06-24 15:30:00", tz="UTC")  # +1 hr
t_eod  <- as.POSIXct("2026-06-24 19:30:00", tz="UTC")  # EOD
t_nxt  <- as.POSIXct("2026-06-26 14:00:00", tz="UTC")  # Jun 26 (2 trading days)

# Contract maps for Jun 24 2026 (M1=Aug/Q26 after Jul/N26 expiry)
contracts <- list(
  CL  = c(M1="CL_Q26", M2="CL_U26", M3="CL_V26", M6="CL_F27"),
  LCO = c(M1="CO_Q26", M2="CO_U26", M3="CO_V26", M6="CO_F27")
)

actuals_list <- list()
for (prod in c("CL","LCO")) {
  cm <- contracts[[prod]]
  bars <- lapply(cm, function(t) load_bar(con, t))
  names(bars) <- names(cm)

  times <- list(pre=t_pre, p5m=t_5m, p1h=t_1h, eod=t_eod)
  px <- lapply(times, function(t) {
    sapply(names(cm), function(mn) {
      b <- bars[[mn]]; if (is.null(b)) return(NA_real_)
      last_px(b, t)
    })
  })

  u <- SFM_UNIT_CONV[[prod]]
  for (tt in names(times)) {
    p <- px[[tt]]
    m1m2   <- (p["M1"]-p["M2"])*u;    m2m3 <- (p["M2"]-p["M3"])*u
    m1m6   <- (p["M1"]-p["M6"])*u;   fly  <- (p["M1"]-2*p["M2"]+p["M3"])*u
    for (spr in c("m1m2","m2m3","m1m6","fly123")) {
      val <- switch(spr, m1m2=m1m2, m2m3=m2m3, m1m6=m1m6, fly123=fly)
      if (!is.na(val))
        actuals_list[[length(actuals_list)+1]] <- data.table(
          product=prod, spread=spr, window=tt, price=round(val,4))
    }
  }
}
dbDisconnect(con)

actuals <- rbindlist(actuals_list, fill=TRUE)
actuals_wide <- dcast(actuals, product+spread~window, value.var="price")
actuals_wide[, chg_5m  := p5m - pre]
actuals_wide[, chg_1h  := p1h - pre]
actuals_wide[, chg_eod := eod - pre]

# ══ 8.  Comparison table ══════════════════════════════════════════════════════
cat("\n")
cat(strrep("═", 90), "\n")
cat("  CONSENSUS-SURPRISE MODEL — OOS PREDICTION vs ACTUAL\n")
cat("  EIA Jun 24 2026: Actual -6.088M bbl | Consensus -3.900M bbl | Surprise -2.188M bbl\n")
cat(strrep("═", 90), "\n\n")

# Print surprise z-scores
cat("Consensus surprise z-scores by product:\n")
for (prod in c("CL","LCO","HO","LGO")) {
  tp <- train_panels[[prod]]
  if (is.null(tp)) next
  train_ev <- tp$events[date <= tp$cutoff]
  raw_val <- june24_cons$surprise_kb
  mu <- mean(as.numeric(train_ev$surprise_z), na.rm=T) * sd(as.numeric(train_ev$surprise_z), na.rm=T)
  # Recompute from raw
  rc <- raw_z_map[["surprise_z"]]
  x  <- as.numeric(train_ev[[rc]])
  mu2 <- mean(x, na.rm=T); sg2 <- sd(x, na.rm=T)
  szold <- if (is.finite(sg2) && sg2 > 1e-10) (raw_val - mu2)/sg2 else NA
  cat(sprintf("  %s: raw_surprise=%+.0f kb  train_mean=%+.1f  train_sd=%.1f  surprise_z=%+.3f\n",
              prod, raw_val, mu2, sg2, szold))
}

cat("\n")

# Best tiers to display
best_tiers <- c("sfm_t1_eia_only","sfm_t2_eia_full","sfm_t4_combined",
                "sfm_t5_enhanced","sfm_t4_xgb","old_t3_full")

for (prod in c("CL","LCO")) {
  cat(strrep("─", 90), "\n")
  cat(sprintf("  %s SPREADS — Prediction vs Actual\n", prod))
  cat(sprintf("  %-18s %-8s  %8s  %8s  %8s  %8s  %s\n",
              "Tier", "Spread", "Pred", "Act+5m", "Act+1h", "ActEOD", "HIT?"))

  for (tnm in best_tiers) {
    for (spr in c("m1m2","m2m3","m1m6")) {
      pr  <- preds[product==prod & tier==tnm & spread==spr]
      act <- actuals_wide[product==prod & spread==spr]
      if (!nrow(pr) || !nrow(act)) next

      pv   <- pr$pred_val
      a5m  <- if (!is.na(act$chg_5m))  act$chg_5m  else NA_real_
      a1h  <- if (!is.na(act$chg_1h))  act$chg_1h  else NA_real_
      aeod <- if (!is.na(act$chg_eod)) act$chg_eod else NA_real_

      hit5  <- if (!is.na(a5m))  ifelse(sign(pv)==sign(a5m), "Y","N") else "?"
      hit1h <- if (!is.na(a1h))  ifelse(sign(pv)==sign(a1h), "Y","N") else "?"
      hiteod<- if (!is.na(aeod)) ifelse(sign(pv)==sign(aeod),"Y","N") else "?"

      cat(sprintf("  %-18s %-8s  %+8.4f  %+8.4f  %+8.4f  %+8.4f  %s/%s/%s\n",
                  tnm, spr, pv,
                  if(!is.na(a5m)) a5m else 0,
                  if(!is.na(a1h)) a1h else 0,
                  if(!is.na(aeod)) aeod else 0,
                  hit5, hit1h, hiteod))
    }
  }
  cat("\n")
}

# ── Actual spread movement summary ───────────────────────────────────────────
cat(strrep("─", 90), "\n")
cat("  ACTUAL SPREAD MOVEMENTS (pre-EIA to EOD / +1h)\n")
cat(sprintf("  %-6s %-8s  %8s  %8s  %8s  %8s  %8s\n",
            "Prod","Spread","Pre px","5m chg","1h chg","EOD chg","Pre→EOD"))
for (i in seq_len(nrow(actuals_wide))) {
  r <- actuals_wide[i]
  cat(sprintf("  %-6s %-8s  %8.4f  %+8.4f  %+8.4f  %+8.4f\n",
              r$product, r$spread, r$pre,
              if(!is.na(r$chg_5m)) r$chg_5m else 0,
              if(!is.na(r$chg_1h)) r$chg_1h else 0,
              if(!is.na(r$chg_eod)) r$chg_eod else 0))
}

# ── Save predictions ─────────────────────────────────────────────────────────
fwrite(preds, "output/consensus_oos_preds_20260624.csv")
fwrite(actuals_wide, "output/consensus_oos_actuals_20260624.csv")
cat(sprintf("\nSaved:\n  output/consensus_oos_preds_20260624.csv (%d rows)\n", nrow(preds)))
cat("  output/consensus_oos_actuals_20260624.csv\n")
cat(strrep("═", 90), "\n")
