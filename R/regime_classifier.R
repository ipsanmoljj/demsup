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
# Label taxonomy (six labels):
#
#   Backwardation-Deficit   : Kalman rising  + Markov high state
#   Stable-Elevated         : Kalman flat    + Markov high state
#   Contango-Surplus        : Kalman falling + Markov low state
#   Stable-Depressed        : Kalman flat    + Markov low state
#   Transition-Tightening   : Within ±TRANSITION_WINDOW bars of a break, direction up
#   Transition-Loosening    : Within ±TRANSITION_WINDOW bars of a break, direction down
#
# Transition labels override the directional labels when a bar is close to a
# confirmed break date. This prevents the classifier from confidently labelling
# a period that is structurally changing.

TRANSITION_WINDOW <- 5   # bars either side of a break date = transition zone
KALMAN_SLOPE_WINDOW <- 10  # bars to compute Kalman slope over
KALMAN_SLOPE_THRESH <- 0.05  # minimum slope magnitude to count as rising/falling
HIGH_STATE_RANK <- 0.6    # top 40% of Markov states by mean = "high"
LOW_STATE_RANK  <- 0.4    # bottom 40% = "low"

# ── Assign narrative label to a single bar ────────────────────────────────────

.assign_label <- function(kalman_slope,
                           markov_state_rank,   # 0=lowest mean, 1=highest mean
                           near_break,          # logical: within transition window
                           break_direction) {   # "up" or "down" if near_break

  if (near_break) {
    return(ifelse(break_direction == "up",
                  "Transition-Tightening",
                  "Transition-Loosening"))
  }

  # Determine level tier from Markov state rank
  level_tier <- ifelse(markov_state_rank >= HIGH_STATE_RANK, "high",
                ifelse(markov_state_rank <= LOW_STATE_RANK,  "low", "mid"))

  # Combine slope + level tier
  if (kalman_slope > KALMAN_SLOPE_THRESH  && level_tier == "high") return("Backwardation-Deficit")
  if (kalman_slope > KALMAN_SLOPE_THRESH  && level_tier == "mid")  return("Backwardation-Deficit")
  if (kalman_slope < -KALMAN_SLOPE_THRESH && level_tier == "low")  return("Contango-Surplus")
  if (kalman_slope < -KALMAN_SLOPE_THRESH && level_tier == "mid")  return("Contango-Surplus")
  if (level_tier == "high")                                         return("Stable-Elevated")
  if (level_tier == "low")                                          return("Stable-Depressed")

  # Middle tier, flat slope → use level z-score direction
  "Stable-Elevated"   # default (will be overridden by z-score layer below)
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

classify_regimes <- function(product = "CL",
                              output_dir = "output") {

  cat("\n", strrep("=", 60), "\n")
  cat("REGIME CLASSIFIER —", product, "\n")
  cat(strrep("=", 60), "\n\n")

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

  # ── Normalise Markov state to 0–1 rank ───────────────────────────────────
  if ("ms_state" %in% names(ms) && !all(is.na(ms$ms_state))) {
    n_states   <- max(ms$ms_state, na.rm = TRUE)
    ms[, ms_state_rank := (ms_state - 1) / pmax(1, n_states - 1)]
  } else {
    # No Markov states available — derive rank from Kalman z-score
    ms[, ms_state_rank := pmin(1, pmax(0, (kf$kf_z + 3) / 6))]
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
    ms_state_rank = ms$ms_state_rank,
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

  # Work on explicit vectors to avoid data.table scoping issues in mapply
  v_slope     <- ifelse(is.na(dt$kf_slope),       0,    dt$kf_slope)
  v_ms_rank   <- ifelse(is.na(dt$ms_state_rank),  0.5,  dt$ms_state_rank)
  v_near      <- dt$near_break
  v_dir       <- ifelse(is.na(dt$break_direction), "up", dt$break_direction)

  dt[, regime_label := mapply(
    .assign_label,
    kalman_slope      = v_slope,
    markov_state_rank = v_ms_rank,
    near_break        = v_near,
    break_direction   = v_dir
  )]

  # ── Override with z-score refinement ─────────────────────────────────────
  # If level_z is strongly positive/negative AND not near a break,
  # override ambiguous mid-tier labels for stronger signal
  dt[near_break == FALSE & !is.na(level_z) & level_z > 2.0  & regime_label == "Stable-Elevated",
     regime_label := "Backwardation-Deficit"]
  dt[near_break == FALSE & !is.na(level_z) & level_z < -2.0 & regime_label == "Stable-Depressed",
     regime_label := "Contango-Surplus"]

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
    confidence_score,
    confidence_band,
    days_since_break,
    days_to_next_break,
    kf_mean,
    kf_z,
    level_z,
    M1M2
  )]

  # ── Save ──────────────────────────────────────────────────────────────────
  out_path <- file.path(output_dir, paste0("regime_labels_", product, ".csv"))
  fwrite(out, out_path)
  cat("Saved:", out_path, "\n")

  # ── Summary table ─────────────────────────────────────────────────────────
  summary_tbl <- out[, .(
    n_bars       = .N,
    pct_of_total = round(.N / nrow(out) * 100, 1),
    mean_conf    = round(mean(confidence_score), 3),
    mean_M1M2    = round(mean(M1M2, na.rm = TRUE), 3)
  ), by = regime_label][order(-n_bars)]

  cat("\n--- REGIME SUMMARY:", product, "---\n\n")
  print(summary_tbl)

  list(
    labels  = out,
    summary = summary_tbl,
    product = product
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

  # NOTE: This wrapper classifies all products using the SAME model outputs
  # currently in output/. If each product was modelled separately (recommended),
  # run classify_regimes() per product after each run_parallel_models() call,
  # then pass the results list to build_cross_product_consensus().
  #
  # Single-product workflow (recommended):
  #   models_cl  <- run_parallel_models(cl_data,  bp_cl,  series = "M1M2")
  #   cl_labels  <- classify_regimes("CL")
  #   models_lco <- run_parallel_models(lco_data, bp_lco, series = "M1M2")
  #   lco_labels <- classify_regimes("LCO")
  #   ... etc for HO, LGO ...
  #   consensus  <- build_cross_product_consensus(list(cl_labels, lco_labels, ho_labels, lgo_labels))

  all_labels <- lapply(products, function(p) {
    cat("\n--- Processing product:", p, "---\n")
    classify_regimes(product = p, output_dir = output_dir)
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

  # Colour map for narrative labels
  label_colours <- c(
    "Backwardation-Deficit"  = "#C0392B",   # deep red
    "Stable-Elevated"        = "#E67E22",   # amber
    "Transition-Tightening"  = "#F39C12",   # yellow-orange
    "Transition-Loosening"   = "#2980B9",   # mid blue
    "Stable-Depressed"       = "#1ABC9C",   # teal
    "Contango-Surplus"       = "#2C3E50"    # dark navy
  )

  # Two-line label text — rendered as two separate text() calls
  # (base R does not support \n in text(); stacking done via y offset)
  label_line1 <- c(
    "Backwardation-Deficit"  = "BACK-",
    "Stable-Elevated"        = "STABLE",
    "Transition-Tightening"  = "TRANS",
    "Transition-Loosening"   = "TRANS",
    "Stable-Depressed"       = "STABLE",
    "Contango-Surplus"       = "CONT-"
  )
  label_line2 <- c(
    "Backwardation-Deficit"  = "DEFICIT",
    "Stable-Elevated"        = "HIGH",
    "Transition-Tightening"  = "TIGHTEN",
    "Transition-Loosening"   = "LOOSEN",
    "Stable-Depressed"       = "LOW",
    "Contango-Surplus"       = "SURPLUS"
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