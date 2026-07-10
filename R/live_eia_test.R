# R/live_eia_test.R
# ─────────────────────────────────────────────────────────────────────────────
# Live EIA Test -- model predictions vs actual spread moves for any EIA release
#
# ARCHITECTURE
#   Sources spread_factor_model.R for panel-building and fitting helpers.
#   Fetches EIA inventory actuals from the EIA API.
#   Computes the feature vector for the live date directly from the factors
#   data (replicating the panel builder's z-score normalization without
#   requiring post-event spread data -- which we don't have yet).
#   Trains the best tiers on the historical training subset, then predicts.
#   Existing files are never modified. Results: output/live_eia_test_YYYYMMDD.csv
#
# USAGE
#   source("R/live_eia_test.R")
#
#   # Without actual spreads -- just get predictions:
#   run_live_eia_test(eia_date = "2026-06-24")
#
#   # With actual spread levels -- compute hit rate too:
#   run_live_eia_test(
#     eia_date = "2026-06-24",
#     actual_spreads = list(
#       CL  = list(m1m2_pre=0.72, m1m2_post=0.91,
#                  m2m3_pre=0.35, m2m3_post=0.41,
#                  m1m6_pre=1.80, m1m6_post=2.10),
#       LCO = list(m1m2_pre=0.55, m1m2_post=0.71),
#       HO  = list(m1m2_pre=0.012, m1m2_post=0.018),
#       LGO = list(m1m2_pre=6.50,  m1m6_post=8.20)
#     )
#   )
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table); library(lubridate)
  library(httr); library(jsonlite)
})

# ── Fetch EIA actuals from API ────────────────────────────────────────────────

.live_fetch_eia <- function(eia_date) {
  readRenviron(path.expand("~/.Renviron"))
  eia_key <- Sys.getenv("EIA_API_KEY")
  if (nchar(eia_key) == 0) stop("EIA_API_KEY not set in ~/.Renviron")
  event_date   <- as.Date(eia_date)
  search_start <- format(event_date - 21, "%Y-%m-%d")
  search_end   <- format(event_date + 3,  "%Y-%m-%d")

  fetch_stoc <- function(sid) {
    resp <- tryCatch(GET("https://api.eia.gov/v2/petroleum/stoc/wstk/data/",
      query = list(api_key=eia_key, frequency="weekly", `data[0]`="value",
        `facets[series][]`=sid, start=search_start, end=search_end,
        `sort[0][column]`="period", `sort[0][direction]`="desc", length=6),
      timeout(30)), error=function(e) NULL)
    if (is.null(resp) || status_code(resp)!=200) return(NULL)
    p <- tryCatch(fromJSON(rawToChar(resp$content)), error=function(e) NULL)
    if (is.null(p)||is.null(p$response$data)) return(NULL)
    d <- as.data.table(p$response$data)
    d[, .(date=as.Date(period), value=suppressWarnings(as.numeric(value)))]
  }
  fetch_move <- function(sid) {
    resp <- tryCatch(GET("https://api.eia.gov/v2/petroleum/move/wkly/data/",
      query = list(api_key=eia_key, frequency="weekly", `data[0]`="value",
        `facets[series][]`=sid, start=search_start, end=search_end,
        `sort[0][column]`="period", `sort[0][direction]`="desc", length=6),
      timeout(30)), error=function(e) NULL)
    if (is.null(resp)||status_code(resp)!=200) return(NULL)
    p <- tryCatch(fromJSON(rawToChar(resp$content)), error=function(e) NULL)
    if (is.null(p)||is.null(p$response$data)) return(NULL)
    d <- as.data.table(p$response$data)
    d[, .(date=as.Date(period), value=suppressWarnings(as.numeric(value)))]
  }
  sm <- list(crude_stocks_kb="WCRSTUS1", cushing_stocks_kb="WCSSTUS1",
             gasoline_stocks_kb="WGTSTUS1", distillate_kb="WDISTUS1")
  rows <- Filter(Negate(is.null), lapply(names(sm), function(nm) {
    d <- fetch_stoc(sm[[nm]]); if (!is.null(d)) { setnames(d,"value",nm); d } else NULL
  }))
  if (!length(rows)) stop("Failed to fetch EIA stock data.")
  stocks <- Reduce(function(a,b) merge(a,b,by="date",all=TRUE), rows)
  stocks <- stocks[order(date)]
  n <- nrow(stocks)
  if (n < 2) stop("Need at least 2 EIA weeks.")
  cur <- stocks[n]; prev <- stocks[n-1]
  ed <- fetch_move("WCREXUS2"); id <- fetch_move("WCRIMUS2")
  net_exp <- tryCatch(
    ed[date==max(ed$date)]$value - id[date==max(id$date)]$value,
    error=function(e) NA_real_)
  list(release_date=eia_date, survey_week_ending=as.character(cur$date),
       crude_stocks_kb=cur$crude_stocks_kb, cushing_stocks_kb=cur$cushing_stocks_kb,
       gasoline_stocks_kb=cur$gasoline_stocks_kb, distillate_kb=cur$distillate_kb,
       crude_chg   =cur$crude_stocks_kb    - prev$crude_stocks_kb,
       cushing_chg =cur$cushing_stocks_kb  - prev$cushing_stocks_kb,
       gasoline_chg=cur$gasoline_stocks_kb - prev$gasoline_stocks_kb,
       distillate_chg=cur$distillate_kb    - prev$distillate_kb,
       crude_net_exports=net_exp)
}

# ── Compute live feature vector directly from training normalisers ─────────────
# Replicates .sfm_build_panel's z-scoring WITHOUT requiring post-event spreads.

.live_compute_features <- function(ea, factors, train_cutoff) {
  fac <- copy(factors)[order(date)]

  # Replicate the panel builder's deriv() call (recompute only if column absent)
  deriv_if_missing <- function(chg, lvl) {
    if (!chg %in% names(fac) && lvl %in% names(fac))
      fac[, (chg) := get(lvl) - shift(get(lvl), 1L)]
  }
  deriv_if_missing("crude_stocks_chg",      "crude_stocks_kb")
  deriv_if_missing("gasoline_stocks_chg",   "gasoline_stocks_kb")
  deriv_if_missing("distillate_stocks_chg", "distillate_stocks_kb")
  deriv_if_missing("cushing_stocks_chg",    "cushing_stocks_kb")
  deriv_if_missing("crude_prod_chg",        "crude_prod_kbd")
  if ("rig_count" %in% names(fac)) {
    fac[, rig_count := suppressWarnings(as.numeric(rig_count))]
    deriv_if_missing("rig_chg_wow", "rig_count")
  }

  # Wednesday rows used by panel builder
  inv <- fac[weekdays(date) == "Wednesday" & !is.na(crude_stocks_chg)]
  if (!nrow(inv)) inv <- fac[weekdays(date) == "Wednesday"]
  inv <- inv[order(date)]
  tr  <- inv$date <= as.Date(train_cutoff)

  # Training statistics extractor
  ts <- function(col) {
    if (!col %in% names(inv)) return(list(mu=0, sg=1))
    v  <- suppressWarnings(as.numeric(inv[[col]]))
    mu <- mean(v[tr], na.rm=TRUE)
    sg <- sd(v[tr],   na.rm=TRUE)
    list(mu = if(is.finite(mu)) mu else 0,
         sg = if(is.finite(sg) && sg>1e-10) sg else 1)
  }
  z_scale <- function(col, val) {
    s <- ts(col); (val - s$mu) / s$sg
  }

  # Surprise proxy (same logic as panel builder)
  surp_col <- if ("crude_stocks_surprise" %in% names(inv) &&
                  !all(is.na(inv$crude_stocks_surprise)))
                "crude_stocks_surprise" else "crude_stocks_chg"
  cat(sprintf("    Using '%s' as surprise proxy\n", surp_col))
  cat(sprintf("    Training mean: %.1f   sd: %.1f\n",
              ts(surp_col)$mu, ts(surp_col)$sg))

  # Compute crude_stocks_surprise for the live event
  # = current_stocks - 5yr_seasonal_avg (same week of year)
  release_wk <- week(as.Date(ea$release_date))
  release_yr <- year(as.Date(ea$release_date))
  hist_same_wk <- fac[
    abs(week(date) - release_wk) <= 1 &
    year(date) %in% (release_yr-5):(release_yr-1) &
    !is.na(crude_stocks_kb)
  ]
  if (nrow(hist_same_wk) >= 2) {
    avg5yr <- mean(as.numeric(hist_same_wk$crude_stocks_kb), na.rm=TRUE)
  } else {
    # Fallback: estimate from most recent known level + surprise
    last_known <- inv[!is.na(get(surp_col))][.N]
    avg5yr <- as.numeric(last_known$crude_stocks_kb) -
              as.numeric(last_known[[surp_col]])
  }
  live_surprise_val <- ea$crude_stocks_kb - avg5yr
  cat(sprintf("    5yr avg (est): %.0f kb  =>  crude_stocks_surprise: %.0f kb\n",
              avg5yr, live_surprise_val))

  surprise_z <- z_scale(surp_col, live_surprise_val)

  # Remaining inventory z-scores (WoW changes from EIA API)
  cushing_stocks_chg_z    <- z_scale("cushing_stocks_chg",    ea$cushing_chg)
  gasoline_stocks_chg_z   <- z_scale("gasoline_stocks_chg",   ea$gasoline_chg)
  distillate_stocks_chg_z <- z_scale("distillate_stocks_chg", ea$distillate_chg)
  crude_prod_chg_z        <- 0   # not yet released on EIA day
  crude_net_exports_z     <- z_scale("crude_net_exports_kbd",
                                     if(is.finite(ea$crude_net_exports)) ea$crude_net_exports else 0)

  # Total petroleum balance
  total_raw           <- ea$crude_chg + ea$gasoline_chg + ea$distillate_chg
  tp_tr_vals          <- suppressWarnings(
    as.numeric(inv$crude_stocks_chg) +
    as.numeric(inv$gasoline_stocks_chg) +
    as.numeric(inv$distillate_stocks_chg))
  tp_mu <- mean(tp_tr_vals[tr], na.rm=TRUE)
  tp_sg <- sd(tp_tr_vals[tr],   na.rm=TRUE)
  total_petrol_surprise_z <- if(is.finite(tp_sg)&&tp_sg>1e-10)
                               (total_raw-tp_mu)/tp_sg else 0

  abs_surprise_z <- abs(surprise_z)

  # EIA streak: mean of last 4 surprise_z values from training rows
  streak_4w <- mean(tail(suppressWarnings(
    z_scale(surp_col, as.numeric(inv[[surp_col]])[tr]), 4)), na.rm=TRUE)
  if (!is.finite(streak_4w)) streak_4w <- 0

  # Structural 5yr deviations (most recent factor row values, z-scored)
  last_inv <- inv[.N]
  live_vals_5yr <- list(
    crude_stocks_5yr_dev   = if("crude_stocks_5yr_dev"    %in% names(last_inv))
                               as.numeric(last_inv$crude_stocks_5yr_dev)    + z_scale("crude_stocks_chg", ea$crude_chg) * ts("crude_stocks_chg")$sg
                             else 0,
    cushing_stocks_5yr_dev = if("cushing_stocks_5yr_dev"  %in% names(last_inv))
                               as.numeric(last_inv$cushing_stocks_5yr_dev)  + ea$cushing_chg
                             else 0,
    gasoline_stocks_5yr_dev= if("gasoline_stocks_5yr_dev" %in% names(last_inv))
                               as.numeric(last_inv$gasoline_stocks_5yr_dev) + ea$gasoline_chg
                             else 0,
    distillate_stocks_5yr_dev=if("distillate_stocks_5yr_dev"%in% names(last_inv))
                               as.numeric(last_inv$distillate_stocks_5yr_dev)+ ea$distillate_chg
                             else 0
  )
  crude_stocks_5yr_dev_z   <- z_scale("crude_stocks_5yr_dev",   live_vals_5yr$crude_stocks_5yr_dev)
  cushing_stocks_5yr_dev_z <- z_scale("cushing_stocks_5yr_dev", live_vals_5yr$cushing_stocks_5yr_dev)
  gasoline_5yr_dev_z       <- z_scale("gasoline_stocks_5yr_dev",live_vals_5yr$gasoline_stocks_5yr_dev)
  distillate_5yr_dev_z     <- z_scale("distillate_stocks_5yr_dev",live_vals_5yr$distillate_stocks_5yr_dev)

  # Macro (most recent factor row)
  last_fac <- fac[.N]
  gv <- function(col) if(col %in% names(last_fac)) suppressWarnings(as.numeric(last_fac[[col]])) else 0
  dxy_z          <- z_scale("dxy",           gv("dxy"))
  dxy_4wk_chg_z  <- z_scale("dxy_4wk_chg",  gv("dxy_4wk_chg"))
  sofr_z         <- z_scale("sofr",          gv("sofr"))
  opec_prod_z    <- z_scale("opec_prod_mbd", gv("opec_prod_mbd"))
  gasoil_crack_dev_z <- z_scale("gasoil_crack_dev", gv("gasoil_crack_dev"))
  ho_crack_dev_z     <- z_scale("ho_crack_proxy",   gv("ho_crack_proxy"))
  rig_chg_wow_z      <- z_scale("rig_chg_wow",      gv("rig_chg_wow"))
  hdd_dev_5yr_z      <- z_scale("hdd_dev_5yr",      gv("hdd_dev_5yr"))
  td3c_storage_cost_z<- z_scale("td3c_storage_cost_bbl_mo", gv("td3c_storage_cost_bbl_mo"))
  td3c_wow_ws_z      <- z_scale("td3c_wow_ws",      gv("td3c_wow_ws"))
  cftc_mm_net_chg_z  <- z_scale("cftc_mm_net_chg",  gv("cftc_mm_net_chg"))
  china_imports_z    <- z_scale("china_imports_proxy", gv("china_imports_proxy"))
  days_fwd_cover_z   <- z_scale("days_fwd_cover_proxy", gv("days_fwd_cover_proxy"))
  crude_net_exports_4wk_z <- z_scale("crude_net_exports_4wk", gv("crude_net_exports_4wk"))

  # Passthrough (already normalised or pure seasonal)
  td3c_z52          <- gv("td3c_z52")
  bdi_z52           <- gv("bdi_z52")
  cftc_net_mm_zscore<- gv("cftc_net_mm_zscore")
  refinery_util_dev <- gv("refinery_util_dev")
  cdd_us_ne         <- gv("cdd_us_ne")
  ev_penetration_pct<- gv("ev_penetration_pct")

  # Seasonality
  doy   <- yday(as.Date(ea$release_date))
  sin_ann          <- sin(2*pi*doy/365.25)
  cos_ann          <- cos(2*pi*doy/365.25)
  mo               <- month(as.Date(ea$release_date))
  driving_season   <- as.numeric(mo %in% 5:9)
  heating_season   <- as.numeric(mo %in% c(10,11,12,1,2,3))
  turnaround_season<- as.numeric(mo %in% c(3,4,9,10))

  # Interactions
  sx_cushing <- surprise_z * cushing_stocks_chg_z
  sx_util    <- surprise_z * refinery_util_dev
  sx_td3c    <- surprise_z * td3c_z52
  sx_cftc    <- surprise_z * cftc_net_mm_zscore
  sx_5yr_dev <- surprise_z * crude_stocks_5yr_dev_z
  sx_streak  <- surprise_z * streak_4w
  sx_abs     <- surprise_z * abs_surprise_z
  for (ic in c("sx_cushing","sx_util","sx_td3c","sx_cftc","sx_5yr_dev","sx_streak","sx_abs"))
    if (!is.finite(get(ic))) assign(ic, 0)

  m1m2_mom_z <- 0   # spread data ends May 22; neutral assumption

  # Named vector -- all features used across all tiers
  c(
    surprise_z              = surprise_z,
    cushing_stocks_chg_z    = cushing_stocks_chg_z,
    gasoline_stocks_chg_z   = gasoline_stocks_chg_z,
    distillate_stocks_chg_z = distillate_stocks_chg_z,
    crude_prod_chg_z        = crude_prod_chg_z,
    crude_net_exports_z     = crude_net_exports_z,
    total_petrol_surprise_z = total_petrol_surprise_z,
    abs_surprise_z          = abs_surprise_z,
    eia_streak_4w           = streak_4w,
    crude_stocks_5yr_dev_z  = crude_stocks_5yr_dev_z,
    cushing_stocks_5yr_dev_z= cushing_stocks_5yr_dev_z,
    gasoline_5yr_dev_z      = gasoline_5yr_dev_z,
    distillate_5yr_dev_z    = distillate_5yr_dev_z,
    days_fwd_cover_z        = days_fwd_cover_z,
    cftc_net_mm_zscore      = cftc_net_mm_zscore,
    cftc_mm_net_chg_z       = cftc_mm_net_chg_z,
    td3c_z52                = td3c_z52,
    td3c_wow_ws_z           = td3c_wow_ws_z,
    td3c_storage_cost_z     = td3c_storage_cost_z,
    bdi_z52                 = bdi_z52,
    dxy_z                   = dxy_z,
    dxy_4wk_chg_z           = dxy_4wk_chg_z,
    sofr_z                  = sofr_z,
    china_imports_z         = china_imports_z,
    opec_prod_z             = opec_prod_z,
    rig_chg_wow_z           = rig_chg_wow_z,
    refinery_util_dev       = refinery_util_dev,
    hdd_dev_5yr_z           = hdd_dev_5yr_z,
    cdd_us_ne               = cdd_us_ne,
    ho_crack_dev_z          = ho_crack_dev_z,
    gasoil_crack_dev_z      = gasoil_crack_dev_z,
    m1m2_mom_z              = m1m2_mom_z,
    sin_ann                 = sin_ann,
    cos_ann                 = cos_ann,
    driving_season          = driving_season,
    heating_season          = heating_season,
    turnaround_season       = turnaround_season,
    sx_cushing              = sx_cushing,
    sx_td3c                 = sx_td3c,
    sx_cftc                 = sx_cftc,
    sx_5yr_dev              = sx_5yr_dev,
    sx_util                 = sx_util,
    sx_streak               = sx_streak,
    sx_abs                  = sx_abs,
    crude_net_exports_4wk_z = crude_net_exports_4wk_z,
    ev_penetration_pct      = ev_penetration_pct
  )
}

# ── Actual spread change from manual inputs ────────────────────────────────────
.live_actual_chg <- function(product, spread, actual_spreads) {
  if (is.null(actual_spreads) || !product %in% names(actual_spreads)) return(NA_real_)
  pr <- actual_spreads[[product]]
  pre <- paste0(spread, "_pre"); post <- paste0(spread, "_post")
  if (!pre %in% names(pr) || !post %in% names(pr)) return(NA_real_)
  as.numeric(pr[[post]]) - as.numeric(pr[[pre]])
}

# ── Master function ───────────────────────────────────────────────────────────

run_live_eia_test <- function(eia_date       = as.character(Sys.Date()),
                               actual_spreads = NULL,
                               live_tiers     = c("sfm_t5_enhanced",
                                                   "sfm_t4_combined",
                                                   "sfm_t3_structural"),
                               train_cutoff   = "2024-10-09",
                               root           = NULL) {

  if (!exists(".sfm_build_panel", mode = "function")) {
    dir0 <- if (!is.null(root)) root else getwd()
    source(file.path(dir0, "R", "spread_factor_model.R"))
  }
  if (is.null(root)) root <- .sfm_root()
  odir <- file.path(root, "output")

  cat(strrep("=", 65), "\n")
  cat("  LIVE EIA TEST --", eia_date, "\n")
  cat(strrep("=", 65), "\n\n")

  # 1. Fetch EIA actuals
  cat("[1] Fetching EIA inventory actuals from API...\n")
  ea <- tryCatch(.live_fetch_eia(eia_date), error = function(e) {
    cat("    ERROR:", conditionMessage(e), "\n"); NULL
  })
  if (is.null(ea)) stop("Could not fetch EIA data.")

  cat(sprintf("    Survey week ending  : %s\n",   ea$survey_week_ending))
  cat(sprintf("    Crude WoW change    : %+.0f kb  (%+.2f mmbbls)\n",
              ea$crude_chg, ea$crude_chg/1000))
  cat(sprintf("    Cushing WoW change  : %+.0f kb\n",  ea$cushing_chg))
  cat(sprintf("    Gasoline WoW change : %+.0f kb\n",  ea$gasoline_chg))
  cat(sprintf("    Distillate WoW chg  : %+.0f kb\n",  ea$distillate_chg))
  cat(sprintf("    Net exports (kbd)   : %s\n",
              if(is.finite(ea$crude_net_exports)) sprintf("%+.0f",ea$crude_net_exports) else "N/A"))

  # 2. Load historical factors (unchanged)
  cat("\n[2] Loading historical factors...\n")
  factors <- .sfm_load_factors(root)

  # 3. Build live feature vector
  cat("\n[3] Computing live feature vector (training normalizers anchored to",
      train_cutoff, ")...\n")
  feat <- .live_compute_features(ea, factors, train_cutoff)

  surp_z  <- feat["surprise_z"]
  signal  <- if      (surp_z < -1.5) "STRONGLY BULLISH (large draw)"
             else if (surp_z < -0.4) "BULLISH (draw)"
             else if (surp_z >  1.5) "STRONGLY BEARISH (large build)"
             else if (surp_z >  0.4) "BEARISH (build)"
             else                    "NEUTRAL"
  cat(sprintf("    surprise_z = %.2f  -->  %s\n", surp_z, signal))
  cat(sprintf("    |surprise_z| >= %.1f filter: %s\n",
              SFM_MIN_SURPRISE_Z,
              if(abs(surp_z) >= SFM_MIN_SURPRISE_Z) "PASSES" else "BELOW THRESHOLD (weaker signal)"))

  # 4. For each product: build historical panel, train, predict
  cat("\n[4] Training models and generating predictions...\n")

  products       <- c("CL", "LCO", "HO", "LGO")
  spread_targets <- c("m1m2", "m2m3", "m1m6", "fly123", "fly136")
  all_rows       <- list()

  for (prod in products) {
    cat(sprintf("\n  -- %s ---\n", prod))

    spreads <- tryCatch(.sfm_load_spreads(prod, root), error = function(e) {
      cat("    Spread file missing:", e$message, "\n"); NULL
    })
    if (is.null(spreads)) next

    regimes <- tryCatch(.sfm_load_regime(prod, root), error = function(e) {
      cat("    Regime file missing, using ALL_REGIMES dummy.\n")
      data.table(date = factors$date, regime = "ALL_REGIMES")
    })

    events <- tryCatch(
      .sfm_build_panel(spreads, factors, regimes, train_cutoff),
      error = function(e) { cat("    Panel error:", e$message, "\n"); NULL }
    )
    if (is.null(events) || !nrow(events)) next

    sig_train <- events[date <= as.Date(train_cutoff) &
                        is.finite(surprise_z) & abs(surprise_z) >= SFM_MIN_SURPRISE_Z]
    cat(sprintf("    Historical train events (|z|>=%.1f): %d\n",
                SFM_MIN_SURPRISE_Z, nrow(sig_train)))
    if (!nrow(sig_train)) { cat("    No training events -- skipping.\n"); next }

    model_store <- new.env(parent = emptyenv())
    tryCatch(.sfm_fit_all(sig_train, prod, model_store),
             error = function(e) cat("    Fit error:", e$message, "\n"))

    # Detect current regime from most recent regime label
    regime_col <- tryCatch(
      regimes[date <= as.Date(eia_date)][.N]$regime,
      error = function(e) "ALL_REGIMES"
    )
    live_regime <- if (!is.null(regime_col) && !is.na(regime_col) && nchar(regime_col) > 0)
                    regime_col else "ALL_REGIMES"
    cat(sprintf("    Current regime: %s\n", live_regime))

    for (spr in spread_targets) {
      for (tnm in live_tiers) {
        if (!tnm %in% names(SFM_TIERS)) next

        xcols <- SFM_TIERS[[tnm]]
        # Build feature matrix: use feat for known columns, 0 for missing
        X <- matrix(0, nrow=1, ncol=length(xcols)); colnames(X) <- xcols
        for (xc in xcols[xcols %in% names(feat)])
          X[, xc] <- suppressWarnings(as.numeric(feat[xc]))
        X[!is.finite(X)] <- 0

        # Prefer regime-specific model, fall back to ALL_REGIMES
        key1 <- paste(prod, tnm, spr, live_regime,   sep="||")
        key2 <- paste(prod, tnm, spr, "ALL_REGIMES", sep="||")
        m <- if (exists(key1, envir=model_store, inherits=FALSE))
               get(key1, envir=model_store, inherits=FALSE)
             else if (exists(key2, envir=model_store, inherits=FALSE))
               get(key2, envir=model_store, inherits=FALSE)
             else NULL
        if (is.null(m)) next

        pred_val <- tryCatch({
          if (m$type == "lm") {
            cf <- coef(m$obj); as.numeric(cf[1] + X %*% cf[-1])
          } else if (m$type %in% c("ridge","lasso")) {
            as.numeric(predict(m$obj, newx=X, s=m$obj$lambda[1]))
          } else if (m$type == "rf") {
            as.numeric(predict(m$obj, newdata=as.data.frame(X)))
          } else if (m$type == "xgb") {
            as.numeric(predict(m$obj, newdata=xgboost::xgb.DMatrix(X)))
          } else NA_real_
        }, error=function(e) NA_real_)
        if (!is.finite(pred_val)) next

        actual_chg <- .live_actual_chg(prod, spr, actual_spreads)
        hit <- if (is.finite(actual_chg)) (sign(pred_val)==sign(actual_chg)) else NA

        all_rows[[length(all_rows)+1]] <- data.table(
          eia_date   = eia_date,
          product    = prod,
          spread     = spr,
          tier       = tnm,
          regime     = live_regime,
          surprise_z = round(surp_z, 2),
          signal     = signal,
          pred_val   = round(pred_val, 4),
          pred_dir   = if(pred_val>0) "UP" else "DOWN",
          actual_chg = if(is.finite(actual_chg)) round(actual_chg,4) else NA_real_,
          actual_dir = if(is.finite(actual_chg)) (if(actual_chg>0) "UP" else "DOWN") else "PENDING",
          hit        = hit
        )
      }
    }
  }

  if (!length(all_rows)) {
    message("\nNo predictions generated.")
    return(invisible(NULL))
  }
  result <- rbindlist(all_rows, fill=TRUE)

  # 5. Print summary
  cat("\n", strrep("=", 65), "\n")
  cat("PREDICTIONS SUMMARY\n")
  cat(strrep("-", 65), "\n")

  best_tier <- live_tiers[live_tiers %in% result$tier][1]
  disp      <- result[tier == best_tier]

  for (prod in products) {
    sub <- disp[product == prod]
    if (!nrow(sub)) next
    cat(sprintf("\n  %s  [%s]  surprise_z = %.2f\n", prod, sub$signal[1], sub$surprise_z[1]))
    for (i in seq_len(nrow(sub))) {
      r <- sub[i]
      astr <- if (!is.na(r$actual_chg))
        sprintf("  actual: %+.4f (%s)  HIT: %s",
                r$actual_chg, r$actual_dir, if(isTRUE(r$hit)) "YES" else "NO")
      else "  actual: PENDING"
      cat(sprintf("    %-8s  pred: %+.4f (%s)%s\n",
                  r$spread, r$pred_val, r$pred_dir, astr))
    }
  }

  # Cross-tier comparison for m1m2
  cat("\n", strrep("-", 65), "\n")
  cat("TIER COMPARISON (m1m2 only):\n")
  for (prod in products) {
    sub <- result[product==prod & spread=="m1m2"]
    if (!nrow(sub)) next
    cat(sprintf("  %-4s  ", prod))
    for (i in seq_len(nrow(sub)))
      cat(sprintf("[%s: %+.4f %s] ", sub$tier[i], sub$pred_val[i], sub$pred_dir[i]))
    cat("\n")
  }

  # Overall hit rate
  has_actuals <- result[!is.na(hit)]
  if (nrow(has_actuals)) {
    cat(sprintf("\n  Overall hit rate: %.1f%%  (%d/%d predictions)\n",
                mean(has_actuals$hit)*100, sum(has_actuals$hit), nrow(has_actuals)))
  }

  # 6. Save
  fname <- paste0("live_eia_test_", gsub("-","",eia_date), ".csv")
  fwrite(result, file.path(odir, fname))
  cat(sprintf("\n  Saved: output/%s\n", fname))

  if (all(is.na(result$actual_chg))) {
    cat("\n  To add actual spread prices and compute hit rate, re-run:\n")
    cat("  run_live_eia_test('", eia_date, "',\n", sep="")
    cat("    actual_spreads = list(\n")
    cat("      CL  = list(m1m2_pre=<before_EIA>, m1m2_post=<2days_after>,\n")
    cat("                 m2m3_pre=<before_EIA>, m2m3_post=<2days_after>,\n")
    cat("                 m1m6_pre=<before_EIA>, m1m6_post=<2days_after>),\n")
    cat("      LCO = list(m1m2_pre=<before_EIA>, m1m2_post=<2days_after>),\n")
    cat("      HO  = list(m1m2_pre=<before_EIA>, m1m2_post=<2days_after>),\n")
    cat("      LGO = list(m1m2_pre=<before_EIA>, m1m2_post=<2days_after>)\n")
    cat("    ))\n")
    cat("  Pre-price  = spread level just BEFORE EIA release (10:29 ET Jun 24)\n")
    cat("  Post-price = spread level 2 trading days AFTER release (Jun 26 close)\n")
    cat("  Spreads in: $/bbl for CL/LCO, $/gallon for HO, $/tonne for LGO\n")
  }

  cat(strrep("=", 65), "\n")
  invisible(result)
}

# ── Auto-run when sourced directly ────────────────────────────────────────────
if (!exists(".live_eia_test_loaded")) {
  .live_eia_test_loaded <- TRUE
  run_live_eia_test(eia_date = "2026-06-24")
}
