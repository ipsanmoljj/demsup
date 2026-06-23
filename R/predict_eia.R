# R/predict_eia.R
# ─────────────────────────────────────────────────────────────────────────────
# Live forward prediction for upcoming EIA inventory releases.
#
# Takes today's factor snapshot + an expected/actual EIA surprise and returns
# predicted spread changes ($/bbl) for every product × spread target combination.
#
# How to use
# ──────────
# 1. Make sure the models have been trained:
#      source("R/inventory_shock_model.R")
#      run_inventory_shock_model()
#
# 2. Source this file and predict:
#      source("R/predict_eia.R")
#
#      # After the EIA number drops: crude surprise = -3.5 mmb vs consensus
#      result <- predict_eia(crude_surprise_mb = -3.5)
#      print(result)
#
#      # Pre-release scenario table: sweep -5 to +5 mmb
#      tbl <- sweep_eia_scenarios(seq(-5, 5, by = 1))
#      print(tbl)
#
#      # Quick regime + factor summary for today
#      show_current_conditions()
#
# Surprise sign convention:
#   Negative = more crude was DRAWN than consensus (bullish for crude)
#   Positive = more crude was BUILT than consensus (bearish for crude)
#
# surprise_z is the z-score relative to the historical distribution of
# weekly stock changes in the training data (pre-2026-03-01).
# |surprise_z| ≥ 0.4 = model considers this "significant" (matches training filter)
# |surprise_z| ≥ 1.0 = strong directional signal
# ─────────────────────────────────────────────────────────────────────────────


# ── Package bootstrap ─────────────────────────────────────────────────────────
.ensure_pkgs_pred <- function() {
  req <- c("data.table","lubridate")
  mis <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(mis)) install.packages(mis, repos = "https://cloud.r-project.org", quiet = TRUE)
}
.ensure_pkgs_pred()

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})


# ── Internal: repo root ───────────────────────────────────────────────────────
.pred_root <- function(root = NULL) {
  if (!is.null(root)) return(root)
  path <- getwd()
  for (i in seq_len(10)) {
    if (file.exists(file.path(path, ".git"))) return(path)
    parent <- dirname(path)
    if (parent == path) break
    path <- parent
  }
  getwd()
}


# ── Factor tier definitions (must match inventory_shock_model.R) ──────────────
.PRED_TIERS <- list(
  tier1_baseline = c("surprise_z"),
  tier2_physical = c("surprise_z","cushing_stocks_chg_z","refinery_util_dev",
                      "crude_prod_chg_z","crude_net_exports_z",
                      "sx_cushing","sx_util"),
  tier2_freight  = c("surprise_z","cushing_stocks_chg_z","refinery_util_dev",
                      "crude_net_exports_z","td3c_z52","td3c_wow_ws_z",
                      "sx_cushing","sx_td3c"),
  tier3_full     = c("surprise_z","cushing_stocks_chg_z","refinery_util_dev",
                      "crude_prod_chg_z","crude_net_exports_z",
                      "gasoline_stocks_chg_z","distillate_stocks_chg_z",
                      "rig_chg_wow_z","td3c_z52","td3c_wow_ws_z",
                      "td3c_storage_cost_z","bdi_z52","hdd_dev_5yr_z",
                      "cdd_us_ne","cftc_mm_net_chg_z",
                      "driving_season","heating_season","turnaround_season",
                      "sin_ann","cos_ann",
                      "sx_cushing","sx_util","sx_td3c","sx_cftc","sx_5yr_dev")
)

# Columns that come pre-z-scored from factors_extended.csv (use as-is)
.PRE_ZSCORED <- c("td3c_z52","bdi_z52","cftc_net_mm_zscore",
                  "sin_ann","cos_ann","sin_semi","cos_semi",
                  "driving_season","heating_season","turnaround_season",
                  "cdd_us_ne","doy_z")

# Raw → z-scored mapping (need training mean/sd)
.RAW_TO_Z <- list(
  cushing_stocks_chg_z    = "cushing_stocks_chg",
  crude_prod_chg_z        = "crude_prod_chg",
  gasoline_stocks_chg_z   = "gasoline_stocks_chg",
  distillate_stocks_chg_z = "distillate_stocks_chg",
  crude_net_exports_z     = "crude_net_exports_kbd",
  rig_chg_wow_z           = "rig_chg_wow",
  hdd_dev_5yr_z           = "hdd_dev_5yr",
  cftc_mm_net_chg_z       = "cftc_mm_net_chg",
  td3c_wow_ws_z           = "td3c_wow_ws",
  td3c_storage_cost_z     = "td3c_storage_cost_bbl_mo"
)


# ── Step 1: Load training normalization parameters ────────────────────────────
# norm_recent_years: use only the most-recent N years before oos_start for the
# surprise normalization. Avoids the SPR-release period (2022-23) biasing the
# mean toward a massive draw, which would make typical EIA surprises look tiny.
.load_training_norms <- function(root,
                                  oos_start         = as.Date("2026-03-01"),
                                  norm_recent_years = 3L) {
  fac_path <- file.path(root, "output", "factors_extended.csv")
  if (!file.exists(fac_path))
    fac_path <- file.path(root, "output", "factors_combined.csv")
  if (!file.exists(fac_path))
    stop("factors_extended.csv not found. Run R/factor_loader_extended.R first.")

  fac <- fread(fac_path)
  fac[, date := as.Date(date)]

  # Full training Wednesdays (for non-surprise factors)
  train_full <- fac[weekdays(date) == "Wednesday" & date < oos_start]
  if (nrow(train_full) == 0) train_full <- fac[date < oos_start]

  # Recent window for crude surprise normalization (avoids SPR-era distortion)
  recent_start <- oos_start - lubridate::years(norm_recent_years)
  train_recent <- train_full[date >= recent_start]
  if (nrow(train_recent) < 20L) train_recent <- train_full  # fallback

  compute_norms <- function(dt) {
    num_cols <- names(dt)[sapply(dt, is.numeric)]
    norms <- lapply(num_cols, function(col) {
      x  <- dt[[col]]
      mu <- mean(x, na.rm = TRUE)
      sg <- sd(x, na.rm = TRUE)
      list(mu = mu, sigma = if (is.finite(sg) && sg > 1e-10) sg else NA_real_)
    })
    names(norms) <- num_cols
    norms
  }

  full_norms   <- compute_norms(train_full)
  recent_norms <- compute_norms(train_recent)

  # Merge: use recent norms for the surprise column, full norms for everything else
  surp_col <- if ("crude_stocks_surprise" %in% names(recent_norms))
                "crude_stocks_surprise" else "crude_stocks_chg"

  list(
    full          = full_norms,
    recent        = recent_norms,
    surprise_col  = surp_col,
    recent_start  = recent_start,
    oos_start     = oos_start
  )
}

.apply_zscore <- function(value, raw_col, norms_obj, use_recent = FALSE) {
  norms <- if (use_recent) norms_obj$recent else norms_obj$full
  if (!raw_col %in% names(norms)) return(0)
  n <- norms[[raw_col]]
  if (is.null(n) || is.na(n$sigma)) return(0)
  (value - n$mu) / n$sigma
}

# Helper: convert raw crude draw/build deviation from consensus → surprise_z
# Uses the recent SD (not the SPR-distorted full-history SD) centered at zero
# so that "−3 mmb bigger draw than consensus" = a properly scaled negative z.
.consensus_surprise_to_z <- function(surprise_mb, norms_obj) {
  surp_col <- norms_obj$surprise_col
  n        <- norms_obj$recent[[surp_col]]
  if (is.null(n) || is.na(n$sigma)) {
    warning("Cannot compute surprise_z: normalization params missing.")
    return(NA_real_)
  }
  # Centre at 0 (no surprise vs consensus), scale by recent SD
  surprise_kb <- surprise_mb * 1000
  surprise_kb / n$sigma
}


# ── Step 2: Load today's factor snapshot ──────────────────────────────────────
.load_current_snapshot <- function(root) {
  fac_path <- file.path(root, "output", "factors_extended.csv")
  if (!file.exists(fac_path))
    fac_path <- file.path(root, "output", "factors_combined.csv")
  fac <- fread(fac_path)
  fac[, date := as.Date(date)]
  tail(fac[order(date)], 1)
}


# ── Step 3: Get current regime for a product ──────────────────────────────────
.load_current_regime <- function(product, root) {
  candidates <- paste0(
    c("regime_labels_","signal_","signals_","classifier_"), product, ".csv"
  )
  for (fname in candidates) {
    path <- file.path(root, "output", fname)
    if (!file.exists(path)) next
    dt <- fread(path)
    dt[, date := as.Date(date)]
    rcol <- intersect(c("regime_label","regime","label"), names(dt))[1]
    if (!is.na(rcol)) {
      latest <- tail(dt[order(date)], 1)
      return(as.character(latest[[rcol]]))
    }
  }
  message("  No regime file found for ", product, " — using ALL_REGIMES")
  "ALL_REGIMES"
}


# ── Step 4: Build the full feature vector ─────────────────────────────────────
.build_feature_vector <- function(snap, norms_obj, surprise_z,
                                  gasoline_surprise_kb = NULL,
                                  distillate_surprise_kb = NULL) {
  feat <- list(surprise_z = surprise_z)

  # Z-score raw columns from snapshot using full training norms
  for (z_col in names(.RAW_TO_Z)) {
    raw_col <- .RAW_TO_Z[[z_col]]
    if (raw_col %in% names(snap)) {
      val <- as.numeric(snap[[raw_col]])
      feat[[z_col]] <- if (is.finite(val))
                         .apply_zscore(val, raw_col, norms_obj, use_recent = FALSE)
                       else 0
    } else {
      feat[[z_col]] <- 0
    }
  }

  # Override gasoline/distillate if caller provided current surprises
  if (!is.null(gasoline_surprise_kb)) {
    feat[["gasoline_stocks_chg_z"]] <-
      .apply_zscore(gasoline_surprise_kb, "gasoline_stocks_chg", norms_obj)
  }
  if (!is.null(distillate_surprise_kb)) {
    feat[["distillate_stocks_chg_z"]] <-
      .apply_zscore(distillate_surprise_kb, "distillate_stocks_chg", norms_obj)
  }

  # Pre-z-scored columns — use snapshot values directly
  for (col in .PRE_ZSCORED) {
    if (col %in% names(snap)) {
      val <- as.numeric(snap[[col]])
      feat[[col]] <- if (is.finite(val)) val else 0
    } else {
      feat[[col]] <- 0
    }
  }

  # refinery_util_dev not in factors — zero-fill
  feat[["refinery_util_dev"]] <- 0

  # 5yr deviation (already in factors, z-score using training norms)
  if ("crude_stocks_5yr_dev" %in% names(snap)) {
    val5 <- as.numeric(snap$crude_stocks_5yr_dev)
    dev_z <- if (is.finite(val5))
               .apply_zscore(val5, "crude_stocks_5yr_dev", norms_obj) else 0
  } else {
    dev_z <- 0
  }

  # Interaction terms
  feat[["sx_cushing"]] <- surprise_z * (feat[["cushing_stocks_chg_z"]] %||% 0)
  feat[["sx_util"]]    <- surprise_z * feat[["refinery_util_dev"]]
  feat[["sx_td3c"]]    <- surprise_z * (feat[["td3c_z52"]] %||% 0)
  feat[["sx_cftc"]]    <- surprise_z * (feat[["cftc_net_mm_zscore"]] %||% 0)
  feat[["sx_5yr_dev"]] <- surprise_z * dev_z

  # Replace any non-finite values
  feat <- lapply(feat, function(v) if (is.finite(v)) v else 0)
  feat
}

`%||%` <- function(a, b) if (!is.null(a)) a else b


# ── Step 5: Apply model coefficients → predicted spread change ────────────────
.apply_coefficients <- function(feat, tier_name, models_dt, product, regime) {
  tier_cols <- .PRED_TIERS[[tier_name]]
  prod_key  <- product

  mod <- models_dt[product == prod_key & tier == tier_name & regime == regime]
  if (nrow(mod) == 0)
    mod <- models_dt[product == prod_key & tier == tier_name & regime == "ALL_REGIMES"]
  if (nrow(mod) == 0) return(NULL)

  intercept <- if ("coef_intercept" %in% names(mod))
                 as.numeric(mod$coef_intercept[1]) else 0

  coef_names  <- paste0("coef_", tier_cols)
  present     <- coef_names[coef_names %in% names(mod)]
  feat_names  <- sub("^coef_", "", present)

  feat_vals <- sapply(feat_names, function(col) {
    v <- feat[[col]]
    if (is.null(v) || !is.finite(v)) 0 else v
  })
  coef_vals <- as.numeric(mod[1, present, with = FALSE])

  intercept + sum(feat_vals * coef_vals, na.rm = TRUE)
}


# ══ MAIN PREDICTION FUNCTION ══════════════════════════════════════════════════

#' Predict EIA inventory shock impact on commodity spreads
#'
#' PRIMARY INPUTS — choose one:
#'
#' @param surprise_z         Recommended. The surprise in standard-deviation units.
#'                           Negative = bigger crude draw than market expected (bullish).
#'                           Positive = bigger build than market expected (bearish).
#'                           Rule of thumb:  |z| < 0.4 = muted signal
#'                                           |z| 0.4-1.0 = moderate surprise
#'                                           |z| > 1.0  = strong surprise
#'                           Use sweep_eia_scenarios() to see the full sensitivity table.
#'
#' @param crude_surprise_mb  Alternative. Deviation of actual EIA release from
#'                           consensus estimate, in million barrels.
#'                           Negative = bigger draw than consensus (bullish crude).
#'                           Positive = bigger build than consensus (bearish crude).
#'                           Converted to z-score using the recent (3yr) training SD,
#'                           centred at zero deviation from consensus.
#'
#' @param products           Products to forecast. Default: all four.
#' @param spread_targets     Spread targets. Default: all five.
#' @param variant            Model variant. "v2_ridge_sig" recommended.
#' @param tiers              Which factor tiers to report.
#' @param gasoline_surprise_mb  Optional concurrent gasoline surprise (mmb).
#' @param distillate_surprise_mb Optional concurrent distillate surprise (mmb).
#' @param root               Repo root path. Auto-detected if NULL.
#'
#' @return data.table: one row per product × spread × tier with predicted_bbl and direction.
predict_eia <- function(
  surprise_z             = NULL,
  crude_surprise_mb      = NULL,
  products               = c("CL","LCO","HO","LGO"),
  spread_targets         = c("m1m2","m2m3","m1m6","fly123","fly136"),
  variant                = "v2_ridge_sig",
  tiers                  = c("tier1_baseline","tier2_physical",
                               "tier2_freight","tier3_full"),
  gasoline_surprise_mb   = NULL,
  distillate_surprise_mb = NULL,
  root                   = NULL
) {
  root <- .pred_root(root)

  # ── Load models ──────────────────────────────────────────────────────────────
  rds_path <- file.path(root, "output", paste0("ism_", variant, ".rds"))
  if (!file.exists(rds_path))
    stop("Model not found: ", rds_path,
         "\nRun run_inventory_shock_model() in R/inventory_shock_model.R first.")
  saved     <- readRDS(rds_path)
  models_dt <- saved$models

  # ── Load training norms ──────────────────────────────────────────────────────
  message("Loading training normalization parameters...")
  norms_obj <- .load_training_norms(root)
  message(sprintf("  Surprise norm window: %s to %s (n-years = 3)",
                  norms_obj$recent_start, norms_obj$oos_start))

  # ── Compute surprise_z ───────────────────────────────────────────────────────
  if (!is.null(surprise_z)) {
    sz <- surprise_z
    message(sprintf("  surprise_z supplied: %.3f", sz))
  } else if (!is.null(crude_surprise_mb)) {
    # Centre at zero (no deviation from consensus), scale by recent SD
    sz <- .consensus_surprise_to_z(crude_surprise_mb, norms_obj)
    message(sprintf("  crude_surprise_mb = %.2f → surprise_z = %.3f  (SD=%.0f kb)",
                    crude_surprise_mb, sz,
                    norms_obj$recent[[norms_obj$surprise_col]]$sigma))
  } else {
    stop("Supply either surprise_z or crude_surprise_mb.")
  }

  if (is.na(sz)) stop("surprise_z is NA — check training data normalization.")

  # ── Current factor snapshot ───────────────────────────────────────────────────
  snap <- .load_current_snapshot(root)
  message("  Snapshot date: ", snap$date)

  # ── Gasoline/distillate surprises (optional) ─────────────────────────────────
  gas_kb <- if (!is.null(gasoline_surprise_mb))   gasoline_surprise_mb   * 1000 else NULL
  dis_kb <- if (!is.null(distillate_surprise_mb)) distillate_surprise_mb * 1000 else NULL

  # ── Feature vector (shared across all products) ───────────────────────────────
  feat <- .build_feature_vector(snap, norms_obj, sz, gas_kb, dis_kb)

  # ── Predict for each product × spread × tier ─────────────────────────────────
  results <- rbindlist(lapply(products, function(prod) {
    regime <- .load_current_regime(prod, root)
    message(sprintf("  %s — regime: %s", prod, regime))

    rbindlist(lapply(spread_targets, function(tgt) {
      rbindlist(lapply(tiers, function(tier_name) {
        # Filter models to this product × spread × tier
        prod_key  <- prod
        tier_mods <- models_dt[product == prod_key & spread_target == tgt &
                                  tier == tier_name]
        if (nrow(tier_mods) == 0) return(NULL)

        y_pred <- .apply_coefficients(feat, tier_name, tier_mods, prod, regime)
        if (is.null(y_pred)) return(NULL)

        data.table(
          product         = prod,
          spread_target   = tgt,
          tier            = tier_name,
          regime          = regime,
          surprise_z      = round(sz, 3),
          crude_surp_mmb  = if (!is.null(crude_surprise_mb)) crude_surprise_mb else NA_real_,
          predicted_bbl   = round(y_pred, 4),
          direction       = fifelse(y_pred >  0.02, "Bullish",
                            fifelse(y_pred < -0.02, "Bearish", "Neutral")),
          variant         = variant
        )
      }), fill = TRUE)
    }), fill = TRUE)
  }), fill = TRUE)

  results[order(product, spread_target, tier)]
}


# ══ SCENARIO SWEEP ═══════════════════════════════════════════════════════════

#' Sweep surprise scenarios and return a sensitivity table
#'
#' By default sweeps surprise_z from -2 to +2 in steps of 0.5.
#' You can also provide surprise_mb values for the deviation-from-consensus approach.
#'
#' @param surprise_z_range   Numeric vector of z-score surprises. Default: seq(-2,2,0.5).
#'                           Set to NULL to use surprise_mb_range instead.
#' @param surprise_mb_range  Alternative: vector of mmb deviations from consensus.
#' @param products,spread_targets,variant,tiers  Passed through to predict_eia.
#'
#' @return data.table with one row per scenario × product × spread × tier.
sweep_eia_scenarios <- function(
  surprise_z_range  = seq(-2, 2, by = 0.5),
  surprise_mb_range = NULL,
  products          = c("CL","LCO"),
  spread_targets    = c("m1m2","m2m3","m1m6"),
  variant           = "v2_ridge_sig",
  tiers             = c("tier2_freight"),
  root              = NULL
) {
  root      <- .pred_root(root)
  norms_obj <- .load_training_norms(root)
  rds_path  <- file.path(root, "output", paste0("ism_", variant, ".rds"))
  if (!file.exists(rds_path)) stop("Model not found: ", rds_path)
  saved     <- readRDS(rds_path)
  models_dt <- saved$models
  snap      <- .load_current_snapshot(root)
  regimes   <- setNames(lapply(products, .load_current_regime, root = root), products)

  # Build scenario list — either z-scores or mb
  if (!is.null(surprise_mb_range)) {
    scenarios <- lapply(surprise_mb_range, function(mb) {
      list(mb = mb, sz = .consensus_surprise_to_z(mb, norms_obj))
    })
  } else {
    scenarios <- lapply(surprise_z_range, function(sz) {
      # Back-calculate approximate mb for display
      n  <- norms_obj$recent[[norms_obj$surprise_col]]
      mb <- if (!is.null(n) && !is.na(n$sigma)) sz * n$sigma / 1000 else NA_real_
      list(mb = mb, sz = sz)
    })
  }

  rbindlist(lapply(scenarios, function(sc) {
    sz   <- sc$sz
    feat <- .build_feature_vector(snap, norms_obj, sz)

    rbindlist(lapply(products, function(prod) {
      regime <- regimes[[prod]]
      rbindlist(lapply(spread_targets, function(tgt) {
        rbindlist(lapply(tiers, function(tier_name) {
          prod_key  <- prod
          tier_mods <- models_dt[product == prod_key & spread_target == tgt &
                                    tier == tier_name]
          if (nrow(tier_mods) == 0) return(NULL)
          y_pred <- .apply_coefficients(feat, tier_name, tier_mods, prod, regime)
          if (is.null(y_pred)) return(NULL)
          data.table(
            surprise_z    = round(sz, 2),
            surp_mmb_approx = round(sc$mb, 2),
            product       = prod,
            spread_target = tgt,
            tier          = tier_name,
            predicted_bbl = round(y_pred, 4),
            direction     = fifelse(y_pred >  0.02, "Bullish",
                            fifelse(y_pred < -0.02, "Bearish", "Neutral"))
          )
        }), fill = TRUE)
      }), fill = TRUE)
    }), fill = TRUE)
  }), fill = TRUE)
}


# ══ CURRENT CONDITIONS SUMMARY ════════════════════════════════════════════════

#' Print a summary of today's conditioning factors that the model uses
show_current_conditions <- function(products = c("CL","LCO","HO","LGO"),
                                    root = NULL) {
  root      <- .pred_root(root)
  snap      <- .load_current_snapshot(root)
  norms_obj <- .load_training_norms(root)

  cat("\n══ Current Factor Snapshot ══════════════════════════════════════\n")
  cat(sprintf("  Date              : %s\n", snap$date))

  disp <- function(label, col, raw_col = NULL, unit = "") {
    val <- if (col %in% names(snap)) as.numeric(snap[[col]]) else NA_real_
    if (!is.finite(val)) { cat(sprintf("  %-26s: n/a\n", label)); return() }
    zval <- if (!is.null(raw_col))
              .apply_zscore(val, raw_col, norms_obj, use_recent = FALSE)
            else val
    cat(sprintf("  %-26s: %8.3f %s   (z = %.2f)\n", label, val, unit, zval))
  }

  cat("\n  --- Inventory & Supply ---\n")
  disp("Crude 5yr dev (kb)",    "crude_stocks_5yr_dev",    "crude_stocks_5yr_dev",  "kb")
  disp("Cushing chg (kb)",      "cushing_stocks_chg",      "cushing_stocks_chg",    "kb")
  disp("Crude prod chg (kbd)",  "crude_prod_chg",          "crude_prod_chg",        "kbd")
  disp("Net exports (kbd)",     "crude_net_exports_kbd",   "crude_net_exports_kbd", "kbd")
  disp("Gasoline chg (kb)",     "gasoline_stocks_chg",     "gasoline_stocks_chg",   "kb")
  disp("Distillate chg (kb)",   "distillate_stocks_chg",   "distillate_stocks_chg", "kb")

  cat("\n  --- Freight & Positioning ---\n")
  disp("TD3C z52",              "td3c_z52")
  disp("TD3C wow WS",           "td3c_wow_ws",             "td3c_wow_ws",           "WS")
  disp("BDI z52",               "bdi_z52")
  disp("CFTC net MM zscore",    "cftc_net_mm_zscore")

  cat("\n  --- Weather & Seasonal ---\n")
  disp("HDD dev 5yr",           "hdd_dev_5yr",             "hdd_dev_5yr")
  disp("Driving season",        "driving_season")
  disp("Heating season",        "heating_season")
  disp("Turnaround season",     "turnaround_season")

  cat("\n  --- Regime Labels ---\n")
  for (prod in products) {
    regime <- tryCatch(.load_current_regime(prod, root), error = function(e) "n/a")
    cat(sprintf("  %-6s: %s\n", prod, regime))
  }
  cat("═════════════════════════════════════════════════════════════════\n\n")
  invisible(snap)
}
