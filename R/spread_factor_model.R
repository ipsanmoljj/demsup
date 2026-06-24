# R/spread_factor_model.R
# ─────────────────────────────────────────────────────────────────────────────
# MOTIVATION
#   inventory_shock_model.R treats EIA as the sole driver and uses other
#   factors only as interactions with the surprise. This means the surprise
#   coefficient absorbs ALL mid-week noise (dollar moves, OPEC headlines,
#   positioning) that happened to coincide with the EIA release.
#
#   This script adds STRUCTURAL factors as standalone regressors so the
#   surprise coefficient captures the PURE incremental EIA effect after
#   controlling for everything else that moves spreads on any given Wednesday.
#
# MODEL EQUATION (sfm_t4_combined tier)
#   d_spread_t =
#     α₁·surprise_z + α₂·cushing_z + α₃·gas_z + α₄·dist_z  ← EIA inventory
#   + β₁·cftc_pos + β₂·dxy + β₃·sofr + β₄·td3c             ← macro / freight
#   + β₅·5yr_dev + β₆·cushing_5yr + β₇·opec_prod            ← structural
#   + β₈·hdd_dev + β₉·crack_dev + β₁₀·seasonality           ← demand
#   + γ₁·(surprise×td3c) + γ₂·(surprise×cftc) + ...         ← amplifiers
#   + ε_t
#
# OUTPUTS
#   output/sfm_results.csv            — model metrics + coefficients
#   output/sfm_oos.csv                — event-level OOS predictions
#   output/sfm_report.csv             — cleaned comparison table
#   output/sfm_comparison.csv         — head-to-head vs old model
#   output/sfm_models.rds             — saved model objects
# ─────────────────────────────────────────────────────────────────────────────


# ── Packages ──────────────────────────────────────────────────────────────────
.ensure_sfm <- function() {
  req <- c("data.table","lubridate","zoo","glmnet")
  mis <- req[!vapply(req, requireNamespace, logical(1), quietly=TRUE)]
  if (length(mis)) install.packages(mis, repos="https://cloud.r-project.org", quiet=TRUE)
}
.ensure_sfm()
suppressPackageStartupMessages({ library(data.table); library(lubridate); library(zoo); library(glmnet) })


# ── Constants (mirror inventory_shock_model.R) ────────────────────────────────
SFM_PRODUCTS       <- c("CL","LCO","HO","LGO")
SFM_OOS_START      <- as.Date("2026-03-01")
SFM_OOS_END        <- as.Date("2026-06-12")
SFM_EVENT_WINDOW   <- 2L
SFM_MIN_OBS        <- 15L
SFM_MIN_SURPRISE_Z <- 0.4
SFM_OLS_MIN_N      <- 30L
SFM_OLS_DOF_RATIO  <- 10L
SFM_SIG_THRESH     <- 0.5
SFM_TRIM_QUANTILE  <- 0.05
SFM_UNIT_CONV      <- list(CL=1.0, LCO=1.0, HO=42.0, LGO=1/7.45)


# ── Factor tier definitions ───────────────────────────────────────────────────
# Four tiers from narrow (EIA only) to full combined model.
# Each tier is tested against all 5 spread targets so we can see which
# factors add explanatory power BEYOND the raw inventory signal.

SFM_TIERS <- list(

  # T1 — identical to old tier1_baseline: single-factor benchmark
  sfm_t1_eia_only = c(
    "surprise_z"
  ),

  # T2 — all EIA product-level inventory components (crude + gasoline + distillate + production + exports)
  # Old model never put all 6 EIA components together without also adding interactions
  sfm_t2_eia_full = c(
    "surprise_z",
    "cushing_stocks_chg_z",
    "gasoline_stocks_chg_z",
    "distillate_stocks_chg_z",
    "crude_prod_chg_z",
    "crude_net_exports_z"
  ),

  # T3 — structural factors ONLY (no EIA surprise at all)
  # Benchmark: how much of Wednesday spread moves can structural factors explain
  # without knowing the EIA number?
  sfm_t3_structural_only = c(
    "crude_stocks_5yr_dev_z",    # structural surplus/deficit vs 5yr avg
    "cushing_stocks_5yr_dev_z",  # Cushing structural position
    "cftc_net_mm_zscore",        # speculative net length (level)
    "cftc_mm_net_chg_z",         # week-on-week positioning change
    "td3c_z52",                  # VLCC freight level
    "td3c_wow_ws_z",             # freight momentum
    "bdi_z52",                   # dry bulk (demand barometer)
    "dxy_z",                     # dollar strength
    "dxy_4wk_chg_z",             # dollar 4-week trend
    "sofr_z",                    # interest rates / carry cost
    "opec_prod_z",               # OPEC supply volume
    "rig_chg_wow_z",             # US rig momentum
    "refinery_util_dev",         # refinery crude demand
    "hdd_dev_5yr_z",             # heating demand deviation
    "cdd_us_ne",                 # cooling demand
    "gasoil_crack_dev_z",        # crack spread deviation (demand signal)
    "sin_ann", "cos_ann",
    "driving_season", "heating_season", "turnaround_season"
  ),

  # T4 — full combined: EIA + structural + interaction amplifiers
  # This is the main new model. surprise_z coefficient = PURE EIA effect
  sfm_t4_combined = c(
    # EIA inventory components
    "surprise_z",
    "cushing_stocks_chg_z",
    "gasoline_stocks_chg_z",
    "distillate_stocks_chg_z",
    "crude_prod_chg_z",
    "crude_net_exports_z",
    # Structural market factors (standalone — new vs old model)
    "crude_stocks_5yr_dev_z",
    "cushing_stocks_5yr_dev_z",
    "cftc_net_mm_zscore",
    "cftc_mm_net_chg_z",
    "td3c_z52",
    "td3c_wow_ws_z",
    "bdi_z52",
    "dxy_z",
    "dxy_4wk_chg_z",
    "sofr_z",
    "opec_prod_z",
    "rig_chg_wow_z",
    "refinery_util_dev",
    "hdd_dev_5yr_z",
    "cdd_us_ne",
    "gasoil_crack_dev_z",
    "sin_ann", "cos_ann",
    "driving_season", "heating_season", "turnaround_season",
    # EIA × structural interaction terms (amplifiers)
    "sx_cushing",
    "sx_td3c",
    "sx_cftc",
    "sx_5yr_dev",
    "sx_util"
  )
)


# ── Helpers ───────────────────────────────────────────────────────────────────
.sfm_root <- function() {
  path <- getwd()
  for (i in seq_len(10)) {
    if (file.exists(file.path(path, ".git"))) return(path)
    parent <- dirname(path); if (parent == path) break; path <- parent
  }
  getwd()
}

.sfm_zscore <- function(x, train_mask) {
  mu <- mean(x[train_mask], na.rm=TRUE)
  sg <- sd(x[train_mask], na.rm=TRUE)
  if (!is.finite(sg) || sg < 1e-10) return(rep(0, length(x)))
  (x - mu) / sg
}

.to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))


# ── Data loaders ──────────────────────────────────────────────────────────────
.sfm_load_factors <- function(root) {
  for (nm in c("factors_extended.csv","factors_combined.csv")) {
    p <- file.path(root, "output", nm)
    if (file.exists(p)) {
      dt <- fread(p); dt[, date := as.Date(date)]
      # Coerce character columns that should be numeric
      char_to_num <- c("td3c_z52","td3c_wow_ws","td3c_storage_cost_bbl_mo",
                       "bdi_z52","bdi","bdi_4wk_chg","rig_count",
                       "opec_spare_cap_mbd","cftc_mm_net_chg","cftc_prod_short",
                       "cftc_swap_net","cl_lz")
      for (col in intersect(char_to_num, names(dt)))
        set(dt, j=col, value=.to_num(dt[[col]]))
      message("  Factor file: ", nm, " (", nrow(dt), " rows, ", ncol(dt), " cols)")
      return(dt)
    }
  }
  stop("No factor file found. Run R/factor_loader_extended.R first.")
}

.sfm_load_spreads <- function(product, root) {
  fname <- file.path(root, paste0(product, "_data.csv"))
  if (!file.exists(fname)) stop("Cannot find ", fname)
  raw   <- fread(fname, skip=1L, header=TRUE)
  cols  <- colnames(raw)
  get_mid <- function(cn) {
    idx <- which(cols == paste0(cn,"||weighted_mid"))
    if (!length(idx)) idx <- grep(paste0("^",cn,"\\|\\|weighted_mid$"), cols)
    if (!length(idx)) return(NULL)
    as.numeric(raw[[cols[idx]]])
  }
  c1 <- get_mid("c1"); c2 <- get_mid("c2")
  c3 <- get_mid("c3"); c6 <- get_mid("c6")
  if (is.null(c1)||is.null(c2)) stop("c1/c2 not found in ", fname)
  u  <- SFM_UNIT_CONV[[product]]
  dt <- data.table(
    date = as.Date(raw[["timestamp"]]),
    m1=as.numeric(c1), m2=as.numeric(c2),
    m3=if(!is.null(c3)) as.numeric(c3) else NA_real_,
    m6=if(!is.null(c6)) as.numeric(c6) else NA_real_
  )
  dt <- dt[order(date)][, .SD[.N], by=date]
  dt[, `:=`(
    m1m2   = (m1-m2)*u,
    m2m3   = if(any(!is.na(m3))) (m2-m3)*u else NA_real_,
    m1m6   = if(any(!is.na(m6))) (m1-m6)*u else NA_real_,
    fly123 = if(any(!is.na(m3))) (m1-2*m2+m3)*u else NA_real_,
    fly136 = if(any(!is.na(m3))&&any(!is.na(m6))) (m1-2*m3+m6)*u else NA_real_
  )]
  dt[, c("m1","m2","m3","m6") := NULL]
  dt
}

.sfm_load_regime <- function(product, root) {
  for (nm in paste0(c("regime_labels_","signal_","signals_","classifier_"), product, ".csv")) {
    p <- file.path(root, "output", nm)
    if (!file.exists(p)) next
    dt <- fread(p); dt[, date := as.Date(date)]
    rc <- intersect(c("regime_label","regime","label"), names(dt))[1]
    if (!is.na(rc)) return(dt[, .(date, regime=get(rc))])
  }
  stop("No regime file for ", product)
}


# ── Event panel builder ───────────────────────────────────────────────────────
.sfm_build_panel <- function(spreads, factors, regimes) {
  fac <- copy(factors)[order(date)]

  # Derive WoW changes if only levels present
  deriv <- function(chg, lvl) {
    if (!chg %in% names(fac) && lvl %in% names(fac))
      fac[, (chg) := get(lvl) - shift(get(lvl), 1)]
  }
  deriv("crude_stocks_chg",      "crude_stocks_kb")
  deriv("gasoline_stocks_chg",   "gasoline_stocks_kb")
  deriv("distillate_stocks_chg", "distillate_stocks_kb")
  deriv("cushing_stocks_chg",    "cushing_stocks_kb")
  deriv("crude_prod_chg",        "crude_prod_kbd")
  if ("rig_count" %in% names(fac))
    set(fac, j="rig_count", value=.to_num(fac[["rig_count"]]))
  deriv("rig_chg_wow",           "rig_count")

  # Wednesday EIA events
  inv <- fac[weekdays(date)=="Wednesday" & !is.na(crude_stocks_chg)]
  if (!nrow(inv)) {
    inv <- fac[weekdays(date)=="Wednesday"]
    message("  WARN: no crude_stocks_chg — using all Wednesdays")
  }
  inv <- merge(inv, regimes, by="date", all.x=TRUE)
  tr  <- inv$date < SFM_OOS_START   # training mask

  # ── Z-score raw columns on training window ────────────────────────────────
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
    # NEW standalone structural z-scores
    dxy_z                    = "dxy",
    dxy_4wk_chg_z            = "dxy_4wk_chg",
    sofr_z                   = "sofr",
    opec_prod_z              = "opec_prod_mbd",
    crude_stocks_5yr_dev_z   = "crude_stocks_5yr_dev",
    cushing_stocks_5yr_dev_z = "cushing_stocks_5yr_dev",
    gasoil_crack_dev_z       = "gasoil_crack_dev"
  )

  # Use crude_stocks_chg as surprise proxy if no consensus column
  if (!"crude_stocks_surprise" %in% names(inv) ||
      all(is.na(inv$crude_stocks_surprise))) {
    raw_z_map[["surprise_z"]] <- "crude_stocks_chg"
    message("  NOTE: using crude_stocks_chg as surprise proxy")
  }

  for (z_col in names(raw_z_map)) {
    raw_col <- raw_z_map[[z_col]]
    if (raw_col %in% names(inv))
      inv[, (z_col) := .sfm_zscore(get(raw_col), tr)]
    else
      inv[, (z_col) := 0]
  }

  # Pre-z-scored columns: use as-is
  for (col in c("td3c_z52","bdi_z52","cftc_net_mm_zscore","refinery_util_dev",
                "sin_ann","cos_ann","driving_season","heating_season",
                "turnaround_season","cdd_us_ne"))
    if (!col %in% names(inv)) inv[, (col) := 0]

  # Interaction terms
  inv[, sx_cushing := surprise_z * cushing_stocks_chg_z]
  inv[, sx_util    := surprise_z * refinery_util_dev]
  inv[, sx_td3c    := surprise_z * td3c_z52]
  inv[, sx_cftc    := surprise_z * cftc_net_mm_zscore]
  inv[, sx_5yr_dev := surprise_z * crude_stocks_5yr_dev_z]

  for (ic in c("sx_cushing","sx_util","sx_td3c","sx_cftc","sx_5yr_dev"))
    inv[!is.finite(get(ic)), (ic) := 0]

  # Compute 2-day spread changes around each EIA event
  tgts   <- c("m1m2","m2m3","m1m6","fly123","fly136")
  sp_dt  <- spreads[, c("date", tgts), with=FALSE]

  rbindlist(lapply(seq_len(nrow(inv)), function(i) {
    rd      <- inv$date[i]
    idx_pre <- which(sp_dt$date <= rd)
    idx_pos <- which(sp_dt$date >  rd)
    if (!length(idx_pre) || length(idx_pos) < SFM_EVENT_WINDOW) return(NULL)
    pre <- sp_dt[tail(idx_pre, 1)]
    pst <- sp_dt[idx_pos[SFM_EVENT_WINDOW]]
    row <- copy(inv[i])
    for (tgt in tgts) row[, paste0("d_", tgt) := pst[[tgt]] - pre[[tgt]]]
    row
  }), fill=TRUE)
}


# ── Fitting (same ridge/lasso/OLS logic as inventory_shock_model.R) ───────────
.sfm_fit <- function(sub, xcols, y_col, filter_sig=TRUE, use_ols=FALSE, trim_y=FALSE) {
  if (!y_col %in% names(sub)) return(NULL)
  if (filter_sig && "surprise_z" %in% names(sub))
    sub <- sub[abs(surprise_z) >= SFM_MIN_SURPRISE_Z]
  y    <- sub[[y_col]]
  Xdt  <- sub[, xcols, with=FALSE]
  for (col in names(Xdt)) set(Xdt, which(!is.finite(Xdt[[col]])), col, 0)
  X    <- as.matrix(Xdt)
  keep <- is.finite(y) & apply(is.finite(X),1,all)
  if (sum(keep) < SFM_MIN_OBS) return(NULL)
  Xk <- X[keep,,drop=FALSE]; yk <- y[keep]; n <- nrow(Xk); p <- ncol(Xk)

  ols_ok <- use_ols && n >= SFM_OLS_MIN_N && p <= floor(n / SFM_OLS_DOF_RATIO)

  if (ols_ok) {
    if (trim_y) {
      ql <- quantile(yk, SFM_TRIM_QUANTILE); qh <- quantile(yk, 1-SFM_TRIM_QUANTILE)
      tr <- yk >= ql & yk <= qh
      if (sum(tr) >= SFM_MIN_OBS) { Xk <- Xk[tr,,drop=FALSE]; yk <- yk[tr]; n <- nrow(Xk) }
    }
    fit  <- tryCatch(lm.fit(cbind(1,Xk), yk), error=function(e) NULL)
    if (is.null(fit)) return(NULL)
    yh   <- as.numeric(cbind(1,Xk) %*% fit$coefficients)
    sst  <- sum((yk-mean(yk))^2); ssr <- sum((yk-yh)^2)
    r2   <- if (sst>0) 1-ssr/sst else NA_real_
    r2a  <- if (!is.na(r2)&&n>p+1) 1-(1-r2)*(n-1)/(n-p-1) else NA_real_
    sig  <- if ("surprise_z" %in% colnames(Xk)) abs(Xk[,"surprise_z"])>SFM_SIG_THRESH else rep(TRUE,n)
    coefs <- fit$coefficients
    cr   <- as.data.table(t(coefs)); setnames(cr, paste0("coef_",c("intercept",colnames(Xk))))
    return(list(n_events=n, r2_insample=round(r2,4), r2_cv=round(r2a,4),
                rmse_bbl=round(sqrt(mean((yk-yh)^2)),4),
                hit_rate=round(mean(sign(yh)==sign(yk),na.rm=TRUE),4),
                hit_sig=round(if(sum(sig)>=5) mean(sign(yh[sig])==sign(yk[sig]),na.rm=TRUE) else NA_real_,4),
                n_sig=sum(sig), coef_row=cr, estimator="OLS"))
  }

  alpha <- if (n>=40) 1 else 0
  nf    <- min(5, max(3, floor(n/4)))
  cvf   <- tryCatch(cv.glmnet(Xk, yk, alpha=alpha, nfolds=nf), error=function(e) NULL)
  if (is.null(cvf)) return(NULL)
  fit  <- glmnet(Xk, yk, alpha=alpha, lambda=cvf$lambda.min)
  yh   <- as.numeric(predict(fit, Xk))
  sst  <- sum((yk-mean(yk))^2); ssr <- sum((yk-yh)^2)
  r2   <- if (sst>0) 1-ssr/sst else NA_real_
  r2cv <- tryCatch(1-min(cvf$cvm)/mean((yk-mean(yk))^2), error=function(e) NA_real_)
  sig  <- if ("surprise_z" %in% colnames(Xk)) abs(Xk[,"surprise_z"])>SFM_SIG_THRESH else rep(TRUE,n)
  coefs <- as.numeric(coef(fit))
  cr   <- as.data.table(t(coefs)); setnames(cr, paste0("coef_",c("intercept",colnames(Xk))))
  list(n_events=n, r2_insample=round(r2,4), r2_cv=round(r2cv,4),
       rmse_bbl=round(sqrt(mean((yk-yh)^2)),4),
       hit_rate=round(mean(sign(yh)==sign(yk),na.rm=TRUE),4),
       hit_sig=round(if(sum(sig)>=5) mean(sign(yh[sig])==sign(yk[sig]),na.rm=TRUE) else NA_real_,4),
       n_sig=sum(sig), coef_row=cr, estimator=if(alpha==0)"Ridge" else "Lasso")
}


# ── Fit all tiers × regimes for one product ───────────────────────────────────
.sfm_fit_all <- function(events_train, product) {
  tgts    <- c("m1m2","m2m3","m1m6","fly123","fly136")
  regimes <- c(as.list(unique(na.omit(events_train$regime))), list("ALL_REGIMES"))

  rbindlist(lapply(regimes, function(reg) {
    sub <- if (identical(reg,"ALL_REGIMES")) events_train
           else events_train[regime==reg]
    if (nrow(sub) < SFM_MIN_OBS) return(NULL)

    rbindlist(lapply(names(SFM_TIERS), function(tnm) {
      xcols <- SFM_TIERS[[tnm]]
      xcols <- xcols[xcols %in% names(sub)]
      if (!length(xcols)) return(NULL)

      rbindlist(lapply(tgts, function(tgt) {
        res <- .sfm_fit(sub, xcols, paste0("d_",tgt), filter_sig=TRUE,
                        use_ols=TRUE, trim_y=FALSE)
        if (is.null(res)) return(NULL)
        cbind(data.table(product=product, regime=as.character(reg), tier=tnm,
                         spread_target=tgt, estimator=res$estimator,
                         n_events=res$n_events, r2_insample=res$r2_insample,
                         r2_cv=res$r2_cv, rmse_bbl=res$rmse_bbl,
                         hit_rate=res$hit_rate, hit_sig=res$hit_sig,
                         n_sig=res$n_sig),
              res$coef_row)
      }), fill=TRUE)
    }), fill=TRUE)
  }), fill=TRUE)
}


# ── OOS evaluation ────────────────────────────────────────────────────────────
.sfm_oos <- function(events_all, models, product) {
  oos  <- events_all[date >= SFM_OOS_START & date <= SFM_OOS_END]
  tgts <- c("m1m2","m2m3","m1m6","fly123","fly136")
  if (!nrow(oos)) return(data.table())

  rbindlist(lapply(seq_len(nrow(oos)), function(i) {
    ev  <- oos[i]
    if (abs(ev$surprise_z) < SFM_MIN_SURPRISE_Z) return(NULL)
    reg <- if (is.na(ev$regime)) "ALL_REGIMES" else ev$regime

    rbindlist(lapply(tgts, function(tgt) {
      yact <- ev[[paste0("d_",tgt)]]
      if (!is.finite(yact)) return(NULL)
      prod_key <- product

      rbindlist(lapply(names(SFM_TIERS), function(tnm) {
        xcols <- SFM_TIERS[[tnm]]
        xcols <- xcols[xcols %in% names(ev)]
        mod <- models[product==prod_key & spread_target==tgt & tier==tnm & regime==reg]
        if (!nrow(mod)) mod <- models[product==prod_key & spread_target==tgt & tier==tnm & regime=="ALL_REGIMES"]
        if (!nrow(mod)) return(NULL)
        fv  <- sapply(xcols, function(col) { v <- ev[[col]]; if (is.finite(v)) v else 0 })
        cc  <- paste0("coef_", xcols)
        cc  <- cc[cc %in% names(mod)]
        cv  <- as.numeric(mod[1, cc, with=FALSE])
        ic  <- if ("coef_intercept" %in% names(mod)) as.numeric(mod$coef_intercept[1]) else 0
        yp  <- ic + sum(fv[sub("coef_","",cc)] * cv, na.rm=TRUE)
        data.table(product=product, date=ev$date, regime=reg, tier=tnm,
                   spread_target=tgt, surprise_z=round(ev$surprise_z,3),
                   y_actual=round(yact,4), y_pred=round(yp,4),
                   correct_sign=(sign(yp)==sign(yact)),
                   abs_error=round(abs(yp-yact),4), error=round(yp-yact,4))
      }), fill=TRUE)
    }), fill=TRUE)
  }), fill=TRUE)
}

.sfm_summarise_oos <- function(oos) {
  if (!nrow(oos)) return(data.table())
  rbindlist(list(
    oos[, .(n_events=.N, hit_rate=round(mean(correct_sign,na.rm=TRUE),4),
            rmse=round(sqrt(mean(error^2,na.rm=TRUE)),4),
            mae=round(mean(abs_error,na.rm=TRUE),4)),
        by=.(product,tier,spread_target,regime)],
    oos[, .(regime="ALL_REGIMES", n_events=.N,
            hit_rate=round(mean(correct_sign,na.rm=TRUE),4),
            rmse=round(sqrt(mean(error^2,na.rm=TRUE)),4),
            mae=round(mean(abs_error,na.rm=TRUE),4)),
        by=.(product,tier,spread_target)]
  ), fill=TRUE)
}


# ── Comparison helper ─────────────────────────────────────────────────────────
.load_old_oos <- function(root) {
  # Load all old OOS variants and pick the best (highest hit rate) per cell
  variants <- c("v1_ridge_all","v2_ridge_sig","v3_ols_sig","v4_ols_sig_trim")
  dts <- lapply(variants, function(v) {
    p <- file.path(root, "output", paste0("ism_oos_",v,".csv"))
    if (!file.exists(p)) return(NULL)
    dt <- fread(p); dt[, variant := v]; dt
  })
  dts <- dts[!sapply(dts, is.null)]
  if (!length(dts)) return(NULL)
  rbindlist(dts, fill=TRUE)
}

.build_comparison <- function(sfm_oos_sum, old_oos, root) {
  # New model: one row per (product, tier, spread_target) across all regimes
  sfm_all <- sfm_oos_sum[regime=="ALL_REGIMES",
                          .(product, tier, spread_target,
                            n_new=n_events, hit_new=hit_rate, rmse_new=rmse)]

  if (is.null(old_oos) || !nrow(old_oos)) return(sfm_all)

  # Old model: best hit rate across all tiers and variants per (product, spread)
  old_best <- old_oos[abs(surprise_z) >= SFM_MIN_SURPRISE_Z,
    .(hit_old=round(mean(correct_sign,na.rm=TRUE),4),
      rmse_old=round(sqrt(mean(error^2,na.rm=TRUE)),4),
      n_old=.N, best_variant=paste(unique(variant),collapse="|")),
    by=.(product, spread_target, tier, variant)][
    order(-hit_old)][
    , .SD[1], by=.(product, spread_target)]   # best tier×variant per cell
  old_best[, c("tier","variant") := NULL]

  merge(sfm_all, old_best, by=c("product","spread_target"), all.x=TRUE)
}


# ══ MAIN RUNNER ═══════════════════════════════════════════════════════════════
run_spread_factor_model <- function(root=NULL) {
  root <- if (!is.null(root)) root else .sfm_root()
  odir <- file.path(root, "output")
  if (!dir.exists(odir)) dir.create(odir, recursive=TRUE)

  message("══ Spread Factor Model ══════════════════════════════════════════")
  message("Root     : ", root)
  message("OOS      : ", SFM_OOS_START, " to ", SFM_OOS_END)
  message("Tiers    : ", paste(names(SFM_TIERS), collapse=" | "))
  message("Key new factors (vs old model): dxy_z, sofr_z, opec_prod_z,")
  message("  crude_stocks_5yr_dev_z (standalone), cushing_stocks_5yr_dev_z,")
  message("  cftc_net_mm_zscore (standalone), gasoil_crack_dev_z,")
  message("  all EIA product components (gas, distillate) in same regression")

  message("\n[1] Loading factors...")
  factors <- .sfm_load_factors(root)

  all_models <- list(); all_oos <- list()

  for (prod in SFM_PRODUCTS) {
    message("\n── ", prod, " ──────────────────────────────────────────────────")

    spreads <- tryCatch(.sfm_load_spreads(prod, root),
                        error=function(e){message("  SKIP: ",e$message); NULL})
    if (is.null(spreads)) next

    regimes <- tryCatch(.sfm_load_regime(prod, root),
                        error=function(e){message("  SKIP: ",e$message); NULL})
    if (is.null(regimes)) next

    message("  Building event panel...")
    events <- tryCatch(.sfm_build_panel(spreads, factors, regimes),
                       error=function(e){message("  SKIP: ",e$message); NULL})
    if (is.null(events)||!nrow(events)) {message("  No events."); next}

    n_tr  <- nrow(events[date <  SFM_OOS_START])
    n_oos <- nrow(events[date >= SFM_OOS_START & date <= SFM_OOS_END])
    n_sig <- nrow(events[date >= SFM_OOS_START & date <= SFM_OOS_END & abs(surprise_z)>=SFM_MIN_SURPRISE_Z])
    message(sprintf("  Events: train=%d  OOS=%d  (sig OOS: %d)", n_tr, n_oos, n_sig))

    train  <- events[date < SFM_OOS_START]
    message("  Fitting models...")
    models <- tryCatch(.sfm_fit_all(train, prod),
                       error=function(e){message("  ERR fit: ",e$message); NULL})
    if (is.null(models)||!nrow(models)) next
    all_models[[prod]] <- models

    oos <- tryCatch(.sfm_oos(events, models, prod),
                    error=function(e){message("  ERR oos: ",e$message); data.table()})
    if (nrow(oos)) {
      all_oos[[prod]] <- oos
      # Quick OOS summary for t4_combined
      q <- oos[tier=="sfm_t4_combined" & spread_target=="m1m2"]
      if (nrow(q))
        message(sprintf("  t4_combined m1m2 OOS: hit=%.1f%%  RMSE=%.3f  n=%d",
                        mean(q$correct_sign,na.rm=TRUE)*100,
                        sqrt(mean(q$error^2,na.rm=TRUE)), nrow(q)))
    }
  }

  # ── Save outputs ─────────────────────────────────────────────────────────────
  message("\n── Saving outputs ───────────────────────────────────────────────")
  models_dt <- rbindlist(all_models, fill=TRUE)
  oos_dt    <- rbindlist(all_oos,    fill=TRUE)
  oos_sum   <- .sfm_summarise_oos(oos_dt)
  old_oos   <- .load_old_oos(root)
  comp      <- .build_comparison(oos_sum, old_oos, root)

  fwrite(models_dt, file.path(odir, "sfm_results.csv"))
  fwrite(oos_dt,    file.path(odir, "sfm_oos.csv"))
  fwrite(oos_sum,   file.path(odir, "sfm_report.csv"))
  fwrite(comp,      file.path(odir, "sfm_comparison.csv"))
  saveRDS(list(models=models_dt, oos=oos_dt, summary=oos_sum, comparison=comp),
          file.path(odir, "sfm_models.rds"))
  message("  Saved: sfm_results.csv, sfm_oos.csv, sfm_report.csv,")
  message("         sfm_comparison.csv, sfm_models.rds")

  # ── Print comparison table ────────────────────────────────────────────────
  cat("\n══ OOS HIT RATE COMPARISON — ALL_REGIMES, sig surprises only ════\n")
  cat(sprintf("  %-6s %-8s %-26s %8s %8s  %5s\n",
              "Prod","Spread","Tier","New hit%","Old hit%","Δ"))
  cat("  ", strrep("─",68), "\n")

  if (nrow(comp)) {
    comp_show <- comp[spread_target %in% c("m1m2","m2m3","m1m6") &
                      tier %in% c("sfm_t2_eia_full","sfm_t4_combined")]
    setorder(comp_show, product, spread_target, tier)
    for (j in seq_len(nrow(comp_show))) {
      r  <- comp_show[j]
      hn <- if (!is.na(r$hit_new)) sprintf("%.1f%%", r$hit_new*100) else "  n/a"
      ho <- if ("hit_old" %in% names(r) && !is.na(r$hit_old))
              sprintf("%.1f%%", r$hit_old*100) else "  n/a"
      dh <- if ("hit_old" %in% names(r) && !is.na(r$hit_old) && !is.na(r$hit_new))
              sprintf("%+.1f%%",(r$hit_new-r$hit_old)*100) else "   n/a"
      cat(sprintf("  %-6s %-8s %-26s %8s %8s  %5s\n",
                  r$product, r$spread_target, r$tier, hn, ho, dh))
    }
  }
  cat("  ", strrep("─",68), "\n\n")

  # ── Coefficient spotlight for t4_combined ─────────────────────────────────
  cat("══ LASSO COEFFICIENTS — CL m1m2, ALL_REGIMES, t4_combined ══════\n")
  cl_coef <- models_dt[product=="CL" & regime=="ALL_REGIMES" &
                        tier=="sfm_t4_combined" & spread_target=="m1m2"]
  if (nrow(cl_coef)) {
    coef_cols <- grep("^coef_", names(cl_coef), value=TRUE)
    vals <- as.numeric(cl_coef[1, coef_cols, with=FALSE])
    names(vals) <- sub("^coef_","",coef_cols)
    vals <- sort(vals[abs(vals)>1e-6], decreasing=TRUE)
    cat("  Non-zero coefficients (sorted by magnitude):\n")
    for (nm in names(vals))
      cat(sprintf("    %-30s  %+.4f\n", nm, vals[nm]))
  } else {
    cat("  (No CL m1m2 model found)\n")
  }
  cat("═════════════════════════════════════════════════════════════════\n\n")

  invisible(list(models=models_dt, oos=oos_dt, summary=oos_sum,
                 comparison=comp))
}

# results <- run_spread_factor_model()
