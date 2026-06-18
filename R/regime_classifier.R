# R/regime_classifier.R
# ----------------------
# Unified regime classifier: produces a single narrative regime label
# + confidence score for every bar, for each product individually,
# and a cross-product consensus layer to distinguish local vs global breaks.
#
# Inputs  (from output/ after running regime_models.R):
#   output/model_kf.rds        — Kalman filter states per bar
#   output/model_ms.rds        — Markov switching states per bar
#   output/model_arima.rds     — ARIMA level z-scores per bar
#   output/model_bp_breaks.rds — Bai-Perron break dates
#   output/model_signals.rds   — combined signals table
#
# Outputs:
#   output/regime_labels_per_product.csv  — per-bar labels for one product run
#   output/regime_consensus.csv           — cross-product consensus (run once per product, combine)
#
# Usage (run once per product, then combine):
#   source("R/futures_reader.R")
#   source("R/structural_breaks.R")
#   source("R/regime_models.R")
#   source("R/regime_classifier.R")
#
#   # For a single product:
#   cl_labels <- classify_regimes(product = "CL")
#   print(cl_labels$summary)
#
#   # For all products and cross-product consensus:
#   all_labels    <- classify_all_products(products = c("CL","LCO","HO","LGO"))
#   consensus_tbl <- build_cross_product_consensus(all_labels)
#   print(consensus_tbl)

library(data.table)
library(zoo)

# ═════════════════════════════════════════════════════════════════════════════
# SECTION 1 — NARRATIVE LABEL LOGIC
# ═════════════════════════════════════════════════════════════════════════════
#
# Labels are assigned per bar using three inputs:
#   1. Kalman slope   — direction of the time-varying mean (rising/falling/flat)
#   2. Markov state   — which state the bar belongs to (ranked by mean level)
#   3. Level z-score  — how far the current level is from its rolling mean
#
# Label taxonomy (nine labels):
#
# Two dimensions combined:
#   1. M1M2 sign — physical direction (absolute, never overridden)
#        M1M2 > 0 = backwardation family
#        M1M2 < 0 = contango family
#   2. Z-score + slope — intensity and trend within that direction
#
#   Deep-Backwardation    : M1M2 > 0, z > deep_high          → crisis-level squeeze
#   Backwardation-Deficit : M1M2 > 0, z > high, slope rising  → active tightening
#   Easing-Backwardation  : M1M2 > 0, z < low,  slope falling → still tight, loosening
#   Stable-Elevated       : M1M2 > 0, z mid or flat slope     → tight, stable plateau
#   Stable-Depressed      : M1M2 < 0, z mid or flat slope     → loose, stable floor
#   Easing-Contango       : M1M2 < 0, z > high, slope rising  → still loose, tightening
#   Contango-Surplus      : M1M2 < 0, z < low,  slope falling → active loosening
#   Deep-Contango         : M1M2 < 0, z < deep_low            → crisis-level oversupply
#   Transition-Tightening : near break, direction up
#   Transition-Loosening  : near break, direction down
#
# Thresholds derived empirically per product via BIC (select_thresholds()).

TRANSITION_WINDOW    <- 5    # bars either side of a break date = transition zone
KALMAN_SLOPE_WINDOW  <- 10   # bars to compute Kalman slope over
KALMAN_SLOPE_THRESH  <- 0.05 # minimum slope magnitude to count as rising/falling
LEVEL_Z_WINDOW       <- 126  # fallback window if window_selector not sourced
LEVEL_Z_HIGH_THRESH  <-  0.5 # fallback high threshold
LEVEL_Z_LOW_THRESH   <- -0.5 # fallback low threshold
LEVEL_Z_DEEP_HIGH    <-  1.5 # fallback deep-high threshold
LEVEL_Z_DEEP_LOW     <- -1.5 # fallback deep-low threshold
LOOKBACK_LAG         <-  63  # fallback lag: baseline excludes most recent N bars

# ── Assign narrative label to a single bar ────────────────────────────────────

.assign_label <- function(kalman_slope,
                           level_z_126,         # rolling z-score of M1M2 (scale-independent)
                           m1m2,                # raw M1M2 value (determines physical direction)
                           near_break,          # logical: within transition window
                           break_direction,     # "up" or "down" if near_break
                           z_high   = LEVEL_Z_HIGH_THRESH,
                           z_low    = LEVEL_Z_LOW_THRESH,
                           z_deep_h = LEVEL_Z_DEEP_HIGH,
                           z_deep_l = LEVEL_Z_DEEP_LOW) {

  if (near_break) {
    return(ifelse(break_direction == "up",
                  "Transition-Tightening",
                  "Transition-Loosening"))
  }

  # Dimension 1: physical direction from M1M2 sign (absolute, never overridden)
  is_backwardated <- m1m2 >= 0   # front month premium = backwardation

  # Dimension 2: intensity tier from z-score
  level_tier <- ifelse(level_z_126 >= z_deep_h, "deep_high",
                ifelse(level_z_126 >= z_high,    "high",
                ifelse(level_z_126 <= z_deep_l,  "deep_low",
                ifelse(level_z_126 <= z_low,      "low", "mid"))))

  # Combine: direction + intensity + slope → label
  #
  # Logic:
  #   Deep tier    → always gets deep label regardless of slope
  #   High tier    → Backwardation-Deficit if slope rising OR flat (sustained tight)
  #                  Easing-Backwardation  if slope falling (tightness unwinding)
  #   Low tier     → Easing-Backwardation  if still positive M1M2 (coming off a high)
  #   Mid tier     → Stable-Elevated (balanced within backwardation)
  #
  # Same logic mirrored for contango family.

  # Backwardation family (M1M2 >= 0)
  if (is_backwardated) {
    if (level_tier == "deep_high")                               return("Deep-Backwardation")
    if (level_tier == "high" && kalman_slope >= -KALMAN_SLOPE_THRESH) return("Backwardation-Deficit")
    if (level_tier == "high" && kalman_slope <  -KALMAN_SLOPE_THRESH) return("Easing-Backwardation")
    if (level_tier == "low"  || level_tier == "deep_low")        return("Easing-Backwardation")
    return("Stable-Elevated")   # mid tier
  }

  # Contango family (M1M2 < 0)
  if (!is_backwardated) {
    if (level_tier == "deep_low")                                return("Deep-Contango")
    if (level_tier == "low"  && kalman_slope <= KALMAN_SLOPE_THRESH) return("Contango-Surplus")
    if (level_tier == "low"  && kalman_slope >  KALMAN_SLOPE_THRESH) return("Easing-Contango")
    if (level_tier == "high" || level_tier == "deep_high")       return("Easing-Contango")
    return("Stable-Depressed")  # mid tier
  }

  "Stable-Elevated"  # fallback
}

# ═════════════════════════════════════════════════════════════════════════════
# SECTION 2 — CONFIDENCE SCORE CONSTRUCTION
# ═════════════════════════════════════════════════════════════════════════════
#
# Confidence score = weighted sum of three components (all scaled 0–1):
#
#   w1 = 0.40 × model_agreement   : how many models confirmed the epoch boundary
#                                    from the consensus matrix
#   w2 = 0.35 × temporal_stability: 0 at a break date, rises to 1 at mid-epoch
#   w3 = 0.25 × kalman_certainty  : 1 − normalised Kalman z-score magnitude
#                                    (high |z| = uncertain, near mean = certain)
#
# This gives a number in [0, 1] per bar. Interpretation:
#   > 0.75  HIGH confidence
#   0.5–0.75 MEDIUM confidence
#   < 0.5   LOW confidence (likely near a transition)

.compute_confidence <- function(model_agreement_weight,  # from consensus matrix: 0.25–1.0
                                 days_since_break,
                                 days_to_next_break,
                                 kalman_z_abs) {          # |kf_z| for this bar

  w1 <- 0.40
  w2 <- 0.35
  w3 <- 0.25

  # Component 1: model agreement (already 0–1 from consensus weight)
  c1 <- model_agreement_weight

  # Component 2: temporal stability
  # Minimum of days since / days to next break, scaled by epoch half-length
  # Peaks at 1.0 at the midpoint of an epoch, drops to 0 at break dates
  epoch_half <- pmax(1, pmin(days_since_break, days_to_next_break))
  c2 <- pmin(1, epoch_half / 30)  # reaches full confidence after 30 days in epoch

  # Component 3: Kalman certainty
  # |z| of 0 = maximum certainty; |z| >= 3 = minimum certainty
  c3 <- pmax(0, 1 - (kalman_z_abs / 3))

  score <- w1 * c1 + w2 * c2 + w3 * c3
  round(pmin(1, pmax(0, score)), 4)
}

.confidence_label <- function(score) {
  ifelse(score > 0.75, "HIGH",
  ifelse(score > 0.50, "MEDIUM", "LOW"))
}

# ═════════════════════════════════════════════════════════════════════════════
# SECTION 3 — PER-PRODUCT CLASSIFIER
# ═════════════════════════════════════════════════════════════════════════════

classify_regimes <- function(product        = "CL",
                              output_dir     = file.path("output", product),
                              level_z_window = NULL,
                              lookback_lag   = NULL) {
  # level_z_window: override the rolling window for level z-score.
  #   If NULL (default), auto-selects using BIC via select_level_z_window().
  # lookback_lag: bars to exclude before the window starts (pre-crisis baseline).
  #   If NULL (default), auto-selects using BIC via select_level_z_window().
  #   Ensures z-score measures deviation from pre-current-regime levels.
  #
  # output_dir defaults to "output/<product>" — e.g. "output/CL", "output/LCO" —
  # matching the per-product save path now used by run_parallel_models(). This
  # fixes a confirmed bug (2026-06-17): previously both functions defaulted to
  # the shared "output/" directory, so classify_regimes("LCO") called any time
  # after a run_parallel_models() call for a DIFFERENT product would silently
  # read that other product's model_kf.rds/model_signals.rds/etc. and return
  # regime labels for the wrong underlying data, with no warning. Verified via
  # identical(classify_regimes("CL")$labels$level_z_126,
  #           classify_regimes("LCO")$labels$level_z_126) returning TRUE before
  # this fix. If you have existing files directly under "output/" from before
  # this change, either move them into "output/CL/" (etc.) or pass
  # output_dir = "output" explicitly to keep the old (shared, unsafe) behavior.

  cat("\n", strrep("=", 60), "\n")
  cat("REGIME CLASSIFIER —", product, "\n")
  cat(strrep("=", 60), "\n\n")

  if (!dir.exists(output_dir)) {
    stop(sprintf(
      "output_dir '%s' does not exist. Did you run run_parallel_models(..., product = \"%s\") first? Each product must have its own model fit saved before classify_regimes() can read it back.",
      output_dir, product
    ))
  }

  # ── Load model outputs ───────────────────────────────────────────────────
  kf        <- readRDS(file.path(output_dir, "model_kf.rds"))
  ms        <- readRDS(file.path(output_dir, "model_ms.rds"))
  ar        <- readRDS(file.path(output_dir, "model_arima.rds"))
  bp_breaks <- readRDS(file.path(output_dir, "model_bp_breaks.rds"))
  signals   <- readRDS(file.path(output_dir, "model_signals.rds"))
  cm_path   <- file.path(output_dir, "consensus_matrix.csv")

  # Load consensus matrix if it exists, else derive weights from n_models
  if (file.exists(cm_path)) {
    cm <- fread(cm_path)
    cm[, break_date := as.Date(break_date)]
  } else {
    # Fallback: build uniform weights from bp_breaks
    cm <- data.table(
      break_date = as.Date(bp_breaks),
      n_models   = 3L,
      confidence = "HIGH"
    )
  }

  # ── Derive model agreement weights from consensus matrix ─────────────────
  # n_models=4 → 1.0, n_models=3 → 0.75, n_models=2 → 0.50, n_models=1 → 0.25
  cm[, agreement_weight := n_models / 4]

  # ── Compute Kalman slope (rolling slope of kf_mean) ──────────────────────
  kf[, kf_slope := c(rep(NA, KALMAN_SLOPE_WINDOW),
                      diff(kf_mean, lag = KALMAN_SLOPE_WINDOW))]

  # ── Auto-select or use provided rolling window for level z-score ────────
  # Window selection uses BIC via window_selector.R (must be sourced first).
  # If level_z_window is provided directly, that value is used instead.
  y_raw <- signals$M1M2

  if (is.null(level_z_window)) {
    # Check if select_level_z_window() is available
    if (exists("select_level_z_window", mode = "function")) {
      cat("  Auto-selecting level z-score window via BIC...\n")
      window_result  <- select_level_z_window(
        product    = product,
        output_dir = output_dir,
        windows    = CANDIDATE_WINDOWS_EXTENDED,
        plot       = FALSE   # suppress plot during classification
      )
      chosen_window <- window_result$optimal_window
      cat("  Selected window:", chosen_window, "days (",
          round(chosen_window / 21, 1), "months ) —",
          window_result$selection_method, "\n")
    } else {
      # Fallback if window_selector.R not sourced
      chosen_window <- LEVEL_Z_WINDOW
      cat("  window_selector.R not sourced — using default LEVEL_Z_WINDOW:",
          chosen_window, "days\n")
      cat("  Tip: source('R/window_selector.R') before classify_regimes()\n")
    }
  } else {
    chosen_window <- level_z_window
    cat("  Using provided level_z_window:", chosen_window, "days\n")
  }

  # ── Auto-select or use provided lookback lag ─────────────────────────────
  if (is.null(lookback_lag)) {
    if (exists("select_level_z_window", mode = "function")) {
      cat("  Auto-selecting lookback lag via BIC...\n")
      lag_result   <- select_lookback_lag(
        product    = product,
        output_dir = output_dir,
        window     = chosen_window,
        plot       = FALSE
      )
      chosen_lag <- lag_result$optimal_lag
      cat("  Selected lag:", chosen_lag, "days (", round(chosen_lag/21,1),
          "months ) —", lag_result$selection_method, "\n")
    } else {
      chosen_lag <- LOOKBACK_LAG
      cat("  Using default LOOKBACK_LAG:", chosen_lag, "days\n")
    }
  } else {
    chosen_lag <- lookback_lag
    cat("  Using provided lookback_lag:", chosen_lag, "days\n")
  }

  # ── Compute lagged rolling z-score (pre-crisis baseline) ─────────────────
  # Baseline window: from (t - chosen_window - chosen_lag) to (t - chosen_lag)
  # This means the reference is always computed from BEFORE the current period,
  # so a sustained crisis doesn't inflate the baseline mean.
  n_raw <- length(y_raw)

  # Vectorised lagged rolling statistics using zoo
  # Lag the series by chosen_lag positions, then apply standard rolling window
  # This is equivalent to: baseline_mean[t] = mean(y[t-lag-window+1 : t-lag])
  if (chosen_lag == 0) {
    # No lag: standard rolling window
    roll_mean <- as.numeric(zoo::rollmean(y_raw, chosen_window, fill = NA, align = "right"))
    roll_sd   <- as.numeric(zoo::rollapply(y_raw, chosen_window, sd, fill = NA, align = "right"))
  } else {
    # Lagged baseline: shift series forward by lag, then compute rolling stats
    y_lagged  <- c(rep(NA_real_, chosen_lag), y_raw[1:(n_raw - chosen_lag)])
    roll_mean <- as.numeric(zoo::rollmean(y_lagged, chosen_window, fill = NA, align = "right"))
    roll_sd   <- as.numeric(zoo::rollapply(y_lagged, chosen_window, sd, fill = NA, align = "right"))
  }

  # ── Warm-up period exclusion ─────────────────────────────────────────────
  # Bars before (chosen_window + chosen_lag) have no valid pre-lag baseline.
  # Comparing them against future data would introduce look-ahead bias.
  # These bars are marked as warm-up and excluded from regime label assignment.
  warmup_bars <- chosen_window + chosen_lag
  warmup_mask <- seq_len(n_raw) <= warmup_bars

  cat("  Warm-up period:", warmup_bars, "bars —",
      sum(warmup_mask), "bars excluded from classification\n")

  # For bars outside warm-up: fill any remaining NAs with nearest valid value
  # (should be rare — only affects edge cases at boundary)
  if (any(is.na(roll_mean) & !warmup_mask)) {
    last_valid_mean <- mean(y_raw[1:warmup_bars], na.rm = TRUE)
    last_valid_sd   <- sd(y_raw[1:warmup_bars],   na.rm = TRUE)
    for (i in which(is.na(roll_mean) & !warmup_mask)) {
      roll_mean[i] <- last_valid_mean
      roll_sd[i]   <- last_valid_sd
    }
  }

  # Floor sd to prevent division issues (use 1% of post-warmup sd)
  post_warmup_sd <- sd(y_raw[!warmup_mask], na.rm = TRUE)
  roll_sd <- pmax(roll_sd, post_warmup_sd * 0.01, na.rm = TRUE)
  level_z_126 <- (y_raw - roll_mean) / roll_sd

  cat("  Level z-score range:", round(min(level_z_126, na.rm=TRUE), 2),
      "to", round(max(level_z_126, na.rm=TRUE), 2),
      "| Lag:", chosen_lag, "days | Window:", chosen_window, "days\n")

  # ── Auto-select or use provided threshold values ──────────────────────────
  if (exists("select_thresholds", mode = "function")) {
    cat("  Auto-selecting z-score thresholds via BIC...\n")
    thresh_result <- select_thresholds(
      product    = product,
      output_dir = output_dir,
      window     = chosen_window,
      plot       = FALSE
    )
    z_high   <- thresh_result$z_high
    z_low    <- thresh_result$z_low
    z_deep_h <- thresh_result$z_deep_high
    z_deep_l <- thresh_result$z_deep_low
    cat("  Thresholds — Deep-Back:", z_deep_h,
        "| Back:", z_high,
        "| Cont:", z_low,
        "| Deep-Cont:", z_deep_l, "\n")
  } else {
    z_high   <- LEVEL_Z_HIGH_THRESH
    z_low    <- LEVEL_Z_LOW_THRESH
    z_deep_h <- LEVEL_Z_DEEP_HIGH
    z_deep_l <- LEVEL_Z_DEEP_LOW
    cat("  Using fallback thresholds (source window_selector.R for auto-selection)\n")
  }

  # ── Build base table ──────────────────────────────────────────────────────
  n <- nrow(signals)
  dt <- data.table(
    date          = signals$date,
    product       = product,
    M1M2          = signals$M1M2,
    kf_mean       = kf$kf_mean,
    kf_z          = kf$kf_z,
    kf_slope      = kf$kf_slope,
    level_z_126   = level_z_126,
    level_z       = ar$level_z
  )

  # ── Identify break dates and epoch boundaries ─────────────────────────────
  bp_dates <- sort(as.Date(bp_breaks))
  # Epoch boundaries: add sentinel dates at start and end
  boundaries <- c(as.Date("2000-01-01"), bp_dates, as.Date("2100-01-01"))

  # For each bar: which epoch, days since break, days to next break
  dt[, epoch_id       := NA_integer_]
  dt[, days_since_break := NA_real_]
  dt[, days_to_next_break := NA_real_]
  dt[, near_break     := FALSE]
  dt[, break_direction := NA_character_]
  dt[, epoch_agreement_weight := 0.5]  # default

  for (i in seq_len(length(boundaries) - 1)) {
    epoch_start <- boundaries[i]
    epoch_end   <- boundaries[i + 1]
    idx <- which(dt$date > epoch_start & dt$date <= epoch_end)
    if (length(idx) == 0) next

    dt[idx, epoch_id := i]
    dt[idx, days_since_break  := as.numeric(date - epoch_start)]
    dt[idx, days_to_next_break := as.numeric(epoch_end - date)]

    # Near-break flag: adaptive window = min(TRANSITION_WINDOW, 15% of epoch length)
    # Prevents short epochs being entirely consumed by transition labels
    epoch_len        <- as.numeric(epoch_end - epoch_start)
    adaptive_window  <- min(TRANSITION_WINDOW, max(2, floor(epoch_len * 0.15)))
    dt[idx, near_break := (days_since_break  <= adaptive_window |
                            days_to_next_break <= adaptive_window)]

    # Break direction: M1M2 mean before vs after epoch_end
    if (i < length(boundaries) - 1 && epoch_end < as.Date("2099-01-01")) {
      before_mean <- mean(dt[date > epoch_start & date <= epoch_end, M1M2], na.rm = TRUE)
      after_idx   <- which(dt$date > epoch_end &
                            dt$date <= boundaries[min(i + 2, length(boundaries))])
      after_mean  <- if (length(after_idx) > 0) mean(dt[after_idx, M1M2], na.rm = TRUE) else before_mean
      direction   <- ifelse(after_mean > before_mean, "up", "down")
      dt[idx, break_direction := direction]
    }

    # Agreement weight from consensus matrix for this epoch's entry break
    if (i > 1) {  # epoch 1 has no entry break
      entry_break <- boundaries[i]
      cm_row <- cm[abs(as.numeric(break_date - entry_break)) <= 3]
      if (nrow(cm_row) > 0) {
        dt[idx, epoch_agreement_weight := cm_row$agreement_weight[1]]
      }
    }
  }

  # ── Assign regime label per bar ───────────────────────────────────────────
  cat("Assigning narrative regime labels...\n")

  # Mark warm-up bars — these have no valid pre-lag baseline
  dt[, in_warmup := warmup_mask]

  # Work on explicit vectors to avoid data.table scoping issues in mapply
  v_slope     <- ifelse(is.na(dt$kf_slope),     0,   dt$kf_slope)
  v_lz126     <- ifelse(is.na(dt$level_z_126),  0,   dt$level_z_126)
  v_m1m2      <- dt$M1M2
  v_near      <- dt$near_break
  v_dir       <- ifelse(is.na(dt$break_direction), "up", dt$break_direction)

  dt[, regime_label := mapply(
    .assign_label,
    kalman_slope    = v_slope,
    level_z_126     = v_lz126,
    m1m2            = v_m1m2,
    near_break      = v_near,
    break_direction = v_dir,
    z_high          = z_high,
    z_low           = z_low,
    z_deep_h        = z_deep_h,
    z_deep_l        = z_deep_l
  )]

  # Override warm-up bars — exclude from regime classification
  dt[in_warmup == TRUE, regime_label := "Warm-Up"]

  # ── Override with z-score refinement ─────────────────────────────────────
  # If level_z_126 is extreme AND not near a break, upgrade label strength
  dt[near_break == FALSE & !is.na(level_z_126) & level_z_126 > 2.0 & regime_label == "Stable-Elevated",
     regime_label := "Backwardation-Deficit"]
  dt[near_break == FALSE & !is.na(level_z_126) & level_z_126 < -2.0 & regime_label == "Stable-Depressed",
     regime_label := "Contango-Surplus"]

  # Physical direction is now baked into .assign_label() directly via m1m2 sign
  # No post-hoc override needed

  # ── Diagnostic: print near_break breakdown per epoch ─────────────────────
  cat("\nNear-break diagnostic:\n")
  print(dt[, .(
    total_bars    = .N,
    near_break_n  = sum(near_break),
    near_break_pct = round(mean(near_break)*100,1)
  ), by = epoch_id][order(epoch_id)])

  # ── Assign regime_id (integer epoch counter) ──────────────────────────────
  dt[, regime_id := .GRP, by = .(epoch_id)]

  # ── Compute confidence score ──────────────────────────────────────────────
  cat("Computing confidence scores...\n")

  dt[, confidence_score := .compute_confidence(
    model_agreement_weight = epoch_agreement_weight,
    days_since_break       = ifelse(is.na(days_since_break), 30, days_since_break),
    days_to_next_break     = ifelse(is.na(days_to_next_break), 30, days_to_next_break),
    kalman_z_abs           = abs(ifelse(is.na(kf_z), 0, kf_z))
  )]

  dt[, confidence_band := .confidence_label(confidence_score)]

  # ── Clean output columns ──────────────────────────────────────────────────
  out <- dt[, .(
    date,
    product,
    regime_label,
    regime_id,
    in_warmup,
    confidence_score,
    confidence_band,
    days_since_break,
    days_to_next_break,
    kf_mean,
    kf_z,
    level_z_126,
    level_z,
    M1M2
  )]
  out[, level_z_window := chosen_window]
  out[, lookback_lag   := chosen_lag]

  # ── Save ──────────────────────────────────────────────────────────────────
  out_path <- file.path(output_dir, paste0("regime_labels_", product, ".csv"))
  fwrite(out, out_path)
  cat("Saved:", out_path, "\n")

  # ── Summary table (excludes warm-up bars) ────────────────────────────────
  n_active    <- nrow(out[regime_label != "Warm-Up"])
  summary_tbl <- out[regime_label != "Warm-Up", .(
    n_bars       = .N,
    pct_of_total = round(.N / n_active * 100, 1),
    mean_conf    = round(mean(confidence_score), 3),
    mean_M1M2    = round(mean(M1M2, na.rm = TRUE), 3)
  ), by = regime_label][order(-n_bars)]

  cat("  Active bars (post warm-up):", n_active, "of", nrow(out), "total\n")

  cat("\n--- REGIME SUMMARY:", product, "---\n\n")
  print(summary_tbl)

  list(
    labels          = out,
    summary         = summary_tbl,
    product         = product,
    level_z_window  = chosen_window,
    lookback_lag    = chosen_lag
  )
}

# ═════════════════════════════════════════════════════════════════════════════
# SECTION 4 — CROSS-PRODUCT CONSENSUS
# ═════════════════════════════════════════════════════════════════════════════
#
# After running classify_regimes() for each product separately,
# combine results to identify whether a given regime is:
#
#   GLOBAL  : 4–5 products share the same label  → macro / supply event
#   BROAD   : 3 products share the same label    → widespread but not universal
#   LOCAL   : 1–2 products share the same label  → product-specific story
#   DIVERGENT: no majority label                 → unstable / transitional
#
# This is the key diagnostic for distinguishing:
#   - A global supply shock (all products tighten simultaneously)
#   - A product-specific event (e.g. HO heating season, LGO refinery turnaround)

build_cross_product_consensus <- function(all_labels_list,
                                           output_dir = "output") {

  cat("\n", strrep("=", 60), "\n")
  cat("CROSS-PRODUCT CONSENSUS\n")
  cat(strrep("=", 60), "\n\n")

  # Combine all per-product label tables
  combined <- rbindlist(lapply(all_labels_list, function(x) x$labels))

  # Get the set of dates present in all products
  all_dates <- Reduce(intersect, lapply(all_labels_list, function(x) as.character(x$labels$date)))
  all_dates <- as.Date(all_dates)

  cat("Common date range:", format(min(all_dates)), "to", format(max(all_dates)),
      "(", length(all_dates), "bars )\n\n")

  # For each date, find the modal regime label and count agreements
  consensus_dt <- combined[date %in% all_dates, {

    label_counts <- sort(table(regime_label), decreasing = TRUE)
    modal_label  <- names(label_counts)[1]
    n_agree      <- as.integer(label_counts[1])
    n_products   <- .N

    scope <- ifelse(n_agree >= 4, "GLOBAL",
             ifelse(n_agree == 3, "BROAD",
             ifelse(n_agree >= 2, "LOCAL", "DIVERGENT")))

    # Mean confidence across agreeing products
    agreeing_conf <- mean(confidence_score[regime_label == modal_label], na.rm = TRUE)

    # Flag if any product is in a Transition label
    any_transition <- any(grepl("Transition", regime_label))

    list(
      consensus_label      = modal_label,
      n_products_agreeing  = n_agree,
      n_products_total     = n_products,
      regime_scope         = scope,
      consensus_confidence = round(agreeing_conf, 4),
      any_transition       = any_transition
    )
  }, by = date][order(date)]

  # ── Add regime_scope_id for consecutive same-scope periods ───────────────
  consensus_dt[, scope_change := c(TRUE, diff(as.integer(factor(regime_scope))) != 0)]
  consensus_dt[, scope_epoch  := cumsum(scope_change)]

  # ── Save ──────────────────────────────────────────────────────────────────
  out_path <- file.path(output_dir, "regime_consensus.csv")
  fwrite(consensus_dt, out_path)
  cat("Saved:", out_path, "\n")

  # ── Scope summary ─────────────────────────────────────────────────────────
  scope_summary <- consensus_dt[, .(
    n_bars       = .N,
    pct_of_total = round(.N / nrow(consensus_dt) * 100, 1)
  ), by = regime_scope][order(-n_bars)]

  cat("\n--- REGIME SCOPE SUMMARY ---\n\n")
  print(scope_summary)

  # ── Global regime periods (most useful for trading) ───────────────────────
  global_periods <- consensus_dt[regime_scope == "GLOBAL", .(
    start_date = min(date),
    end_date   = max(date),
    n_bars     = .N,
    label      = consensus_label[1],
    mean_conf  = round(mean(consensus_confidence), 3)
  ), by = scope_epoch][order(start_date)]

  if (nrow(global_periods) > 0) {
    cat("\n--- GLOBAL REGIME PERIODS (all products agree) ---\n\n")
    print(global_periods[, .(start_date, end_date, n_bars, label, mean_conf)])
  }

  list(
    consensus     = consensus_dt,
    scope_summary = scope_summary,
    global_periods = global_periods
  )
}

# ═════════════════════════════════════════════════════════════════════════════
# SECTION 5 — MULTI-PRODUCT WRAPPER
# ═════════════════════════════════════════════════════════════════════════════
#
# Convenience wrapper: re-run the full pipeline (read data → break detection
# → parallel models → classify) for each product, then build consensus.
#
# NOTE: This assumes that for each product, you have already run
#   run_parallel_models() and the output/*.rds files exist.
#   If running fresh, you need to source the upstream scripts first.
#
# For a multi-product run where each product has its OWN model outputs,
# you need to save/load per-product .rds files. See note below.

classify_all_products <- function(products    = c("CL", "LCO", "HO", "LGO"),
                                   output_dir  = "output") {

  cat("\n", strrep("=", 60), "\n")
  cat("MULTI-PRODUCT CLASSIFICATION\n")
  cat(strrep("=", 60), "\n\n")

  # Each product's model outputs live under output_dir/<product>/ (see
  # run_parallel_models()'s product-namespaced save path). classify_regimes()
  # now defaults its own output_dir to file.path("output", product), so calling
  # it per-product here is safe as long as run_parallel_models(..., product = p)
  # was run for every product in `products` beforehand. This used to silently
  # read whichever product's model files happened to be sitting in a single
  # shared "output/" dir — fixed 2026-06-17, see classify_regimes()'s docstring.
  #
  # Per-product workflow (run once per product before calling this):
  #   models_cl  <- run_parallel_models(cl_data,  bp_cl,  series = "M1M2", product = "CL")
  #   models_lco <- run_parallel_models(lco_data, bp_lco, series = "M1M2", product = "LCO")
  #   ... etc for HO, LGO ...
  #   all <- classify_all_products()

  all_labels <- lapply(products, function(p) {
    cat("\n--- Processing product:", p, "---\n")
    classify_regimes(product = p, output_dir = file.path(output_dir, p))
  })

  names(all_labels) <- products

  consensus <- build_cross_product_consensus(all_labels, output_dir = output_dir)

  list(
    per_product = all_labels,
    consensus   = consensus
  )
}

# ═════════════════════════════════════════════════════════════════════════════
# SECTION 6 — DIAGNOSTIC PLOT
# ═════════════════════════════════════════════════════════════════════════════

plot_regime_labels <- function(labels_result,
                                save_path = NULL) {

  out     <- labels_result$labels
  product <- labels_result$product

  if (is.null(save_path)) {
    save_path <- paste0("output/regime_labels_", product, "_plot_v3.png")
  }

  # Colour map for narrative labels (11 labels including Warm-Up)
  label_colours <- c(
    "Warm-Up"                = "#DDDDDD",   # light grey — excluded warm-up period
    "Deep-Backwardation"     = "#7B0000",   # dark crimson
    "Backwardation-Deficit"  = "#C0392B",   # deep red
    "Easing-Backwardation"   = "#E8927C",   # salmon — still tight but loosening
    "Stable-Elevated"        = "#E67E22",   # amber
    "Transition-Tightening"  = "#F39C12",   # yellow-orange
    "Transition-Loosening"   = "#2980B9",   # mid blue
    "Stable-Depressed"       = "#1ABC9C",   # teal
    "Easing-Contango"        = "#76B7A0",   # light teal — still loose but tightening
    "Contango-Surplus"       = "#2C3E50",   # dark navy
    "Deep-Contango"          = "#0A0A2E"    # near black navy
  )

  # Two-line label text
  label_line1 <- c(
    "Warm-Up"                = "WARM",
    "Deep-Backwardation"     = "DEEP",
    "Backwardation-Deficit"  = "BACK-",
    "Easing-Backwardation"   = "EASING",
    "Stable-Elevated"        = "STABLE",
    "Transition-Tightening"  = "TRANS",
    "Transition-Loosening"   = "TRANS",
    "Stable-Depressed"       = "STABLE",
    "Easing-Contango"        = "EASING",
    "Contango-Surplus"       = "CONT-",
    "Deep-Contango"          = "DEEP"
  )
  label_line2 <- c(
    "Warm-Up"                = "UP",
    "Deep-Backwardation"     = "BACKW.",
    "Backwardation-Deficit"  = "DEFICIT",
    "Easing-Backwardation"   = "BACK.",
    "Stable-Elevated"        = "HIGH",
    "Transition-Tightening"  = "TIGHTEN",
    "Transition-Loosening"   = "LOOSEN",
    "Stable-Depressed"       = "LOW",
    "Easing-Contango"        = "CONT.",
    "Contango-Surplus"       = "SURPLUS",
    "Deep-Contango"          = "CONTANGO"
  )

  png(save_path, width = 1800, height = 1100, res = 120)
  par(mfrow = c(2, 1), mar = c(2, 4.5, 5, 2), oma = c(3, 0, 3, 0), bg = "white")

  times  <- as.POSIXct(out$date)

  # ── Panel 1: M1M2 with coloured bands + text labels ──────────────────────
  y_range <- range(out$M1M2, na.rm = TRUE)
  y_span  <- diff(y_range)
  y_min   <- y_range[1] - y_span * 0.05
  y_max   <- y_range[2] + y_span * 0.55   # large headroom above price for labels
  line_h  <- y_span * 0.09                # vertical gap between two text lines

  plot(times, out$M1M2, type = "n",
       main = paste0(product, " — M1M2 spread with regime labels"),
       xlab = "", ylab = "M1M2 ($/bbl)", xaxt = "n", las = 1,
       ylim = c(y_min, y_max), cex.main = 0.9)

  unique_epochs <- unique(out$regime_id)

  for (ep in unique_epochs) {
    ep_rows  <- out[regime_id == ep]
    if (nrow(ep_rows) == 0) next

    # Use modal label (most frequent) not first bar — first bars may be Transition
    ep_label <- names(sort(table(ep_rows$regime_label), decreasing = TRUE))[1]
    ep_col   <- label_colours[ep_label]
    if (is.na(ep_col)) ep_col <- "gray80"

    t_start  <- as.POSIXct(min(ep_rows$date)) - 43200
    t_end    <- as.POSIXct(max(ep_rows$date)) + 43200
    t_mid    <- as.POSIXct(mean(as.numeric(c(t_start, t_end)),
                                na.rm = TRUE), origin = "1970-01-01")

    # Coloured background band (full height including label headroom)
    rect(t_start, y_min, t_end, y_max,
         col = adjustcolor(ep_col, 0.13), border = NA)

    # Thin vertical border at epoch start
    abline(v = t_start, col = adjustcolor(ep_col, 0.45), lwd = 0.6, lty = 1)

    # Text labels — only for epochs wide enough to fit (>= 10 bars)
    if (nrow(ep_rows) >= 10) {
      l1      <- label_line1[ep_label]
      l2      <- label_line2[ep_label]
      txt_col <- ep_col
      y_top   <- y_range[2] + y_span * 0.42

      # Line 1 (e.g. "BACK-")
      text(t_mid, y_top,
           labels = l1, cex = 0.60, col = txt_col, font = 2, adj = c(0.5, 0.5))

      # Line 2 (e.g. "DEFICIT")
      text(t_mid, y_top - line_h,
           labels = l2, cex = 0.60, col = txt_col, font = 2, adj = c(0.5, 0.5))

      # Confidence score below
      if (nrow(ep_rows) >= 15) {
        mean_conf <- round(mean(ep_rows$confidence_score, na.rm = TRUE), 2)
        text(t_mid, y_top - line_h * 2.1,
             labels = paste0("conf:", mean_conf),
             cex = 0.46, col = "gray40", adj = c(0.5, 0.5))
      }

      # Small tick line to separate label zone from price zone
      segments(t_start, y_range[2] + y_span * 0.07,
               t_end,   y_range[2] + y_span * 0.07,
               col = adjustcolor(ep_col, 0.4), lwd = 0.5)
    }
  }

  # Price line and zero reference
  lines(times, out$M1M2, col = "gray20", lwd = 0.9)
  abline(h = 0, lty = 2, col = "gray55", lwd = 0.5)

  # Kalman mean overlay (dashed, same panel)
  lines(times, out$kf_mean, col = "#185FA5", lwd = 1.0, lty = 2)

  axis.POSIXct(1, at = seq(min(times), max(times), by = "6 months"),
               format = "%b %Y", cex.axis = 0.7, las = 2)

  # Legend — bottom left to avoid overlap with labels at top
  present_labels <- intersect(names(label_colours), unique(out$regime_label))
  legend("bottomleft",
         legend = c(present_labels, "Kalman mean"),
         fill   = c(adjustcolor(label_colours[present_labels], 0.5), NA),
         lty    = c(rep(NA, length(present_labels)), 2),
         lwd    = c(rep(NA, length(present_labels)), 1.2),
         col    = c(rep(NA, length(present_labels)), "#185FA5"),
         border = c(rep("gray70", length(present_labels)), NA),
         cex    = 0.6, bty = "n", ncol = 3)

  # ── Panel 2: Confidence score coloured by regime ──────────────────────────
  plot(times, out$confidence_score, type = "n",
       main = paste0(product, " — Confidence score per bar (coloured by regime)"),
       xlab = "", ylab = "Confidence (0–1)", xaxt = "n", las = 1,
       ylim = c(0, 1.05), cex.main = 0.9)

  # Shade confidence area with regime colour
  for (ep in unique_epochs) {
    ep_rows <- out[regime_id == ep]
    if (nrow(ep_rows) == 0) next
    # Use modal label (most frequent) not first bar — first bars may be Transition
    ep_label <- names(sort(table(ep_rows$regime_label), decreasing = TRUE))[1]
    ep_col   <- label_colours[ep_label]
    if (is.na(ep_col)) ep_col <- "gray70"
    ep_times <- as.POSIXct(ep_rows$date)
    ep_conf  <- ep_rows$confidence_score
    polygon(c(ep_times, rev(ep_times)),
            c(ep_conf, rep(0, length(ep_conf))),
            col = adjustcolor(ep_col, 0.25), border = NA)
  }

  # Confidence line on top
  lines(times, out$confidence_score, col = "gray25", lwd = 0.7)

  # Threshold lines
  abline(h = c(0.50, 0.75), lty = 2,
         col = c("gray55", "gray30"), lwd = 0.6)
  text(max(times), 0.77, "HIGH",   cex = 0.6, col = "gray30", adj = 1)
  text(max(times), 0.52, "MEDIUM", cex = 0.6, col = "gray30", adj = 1)
  text(max(times), 0.27, "LOW",    cex = 0.6, col = "gray30", adj = 1)

  axis.POSIXct(1, at = seq(min(times), max(times), by = "6 months"),
               format = "%b %Y", cex.axis = 0.7, las = 2)

  mtext(paste0("Regime classification — ", product),
        side = 3, outer = TRUE, cex = 1.0, font = 2, line = 1.5)
  mtext("Top panel: coloured bands = regime epoch; dashed blue = Kalman mean | Bottom: confidence score shaded by regime",
        side = 1, outer = TRUE, cex = 0.65, line = 1.5)

  dev.off()
  cat("Saved:", save_path, "\n")
}