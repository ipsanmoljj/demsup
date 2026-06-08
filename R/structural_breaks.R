# R/structural_breaks.R
# ----------------------
# Structural break detection for regime identification.
#
# Runs 4 parallel models and compares break dates for robustness:
#   Model 1: M1M2 only
#   Model 2: M1M2 + slope (bivariate)
#   Model 3: M1M2 + slope + roll_yield + volatility (full)
#   Model 4: All spreads (M1M2, M1M3, M1M6, M1M12)
#
# Break dates confirmed by 2+ models = high-confidence regime boundary.
#
# Usage:
#   source("R/futures_reader.R")
#   source("R/structural_breaks.R")
#   ff      <- read_futures_csv("path/to/CL_outrights_1min_t.csv")
#   results <- run_break_detection(ff, resample_to = "1 day")
#   print(results$summary)
#   plot_breaks(results)

library(data.table)
library(lubridate)
library(zoo)
library(strucchange)

# ── Install strucchange if missing ────────────────────────────────────────────
if (!requireNamespace("strucchange", quietly = TRUE)) {
  install.packages("strucchange", repos = "https://cloud.r-project.org")
  library(strucchange)
}

# ── Product configuration ────────────────────────────────────────────────────
# Each product has:
#   unit        : native trading unit
#   to_bbl      : multiply M1M2 by this to get $/bbl equivalent
#   thresholds  : M1M2 thresholds in NATIVE units for regime labelling

PRODUCT_CONFIG <- list(
  CL = list(
    unit       = "$/bbl",
    to_bbl     = 1.0,
    thresholds = list(deep_back = 2.0, mild_back = 0.5, flat_lo = -0.5,
                      mild_cont = -0.5, deep_cont = -2.0,
                      slope_deep_back = 10, slope_mild_back = 2,
                      slope_mild_cont = -2, slope_deep_cont = -10)
  ),
  LCO = list(
    unit       = "$/bbl",
    to_bbl     = 1.0,
    thresholds = list(deep_back = 2.0, mild_back = 0.5, flat_lo = -0.5,
                      mild_cont = -0.5, deep_cont = -2.0,
                      slope_deep_back = 10, slope_mild_back = 2,
                      slope_mild_cont = -2, slope_deep_cont = -10)
  ),
  HO = list(
    unit       = "$/gallon",
    to_bbl     = 42.0,          # 42 gallons per barrel
    thresholds = list(deep_back = 0.048, mild_back = 0.012, flat_lo = -0.012,
                      mild_cont = -0.012, deep_cont = -0.048,
                      slope_deep_back = 0.24, slope_mild_back = 0.05,
                      slope_mild_cont = -0.05, slope_deep_cont = -0.24)
  ),
  LGO = list(
    unit       = "$/mt",
    to_bbl     = 1/7.45,        # ~7.45 barrels per metric tonne of gasoil
    thresholds = list(deep_back = 14.9, mild_back = 3.7, flat_lo = -3.7,
                      mild_cont = -3.7, deep_cont = -14.9,
                      slope_deep_back = 74.5, slope_mild_back = 14.9,
                      slope_mild_cont = -14.9, slope_deep_cont = -74.5)
  )
)

# Get config for a product (defaults to CL if unknown)
.get_product_config <- function(product_name) {
  key <- toupper(gsub("_data|_outrights.*", "", basename(product_name)))
  key <- gsub(".csv", "", key, fixed = TRUE)
  # Match known products
  if (grepl("^HO", key))  return(PRODUCT_CONFIG$HO)
  if (grepl("^LGO", key)) return(PRODUCT_CONFIG$LGO)
  if (grepl("^LCO", key)) return(PRODUCT_CONFIG$LCO)
  PRODUCT_CONFIG$CL  # default
}

# ── Main entry point ──────────────────────────────────────────────────────────

run_break_detection <- function(ff,
                                resample_to   = "1 day",
                                max_breaks    = 8,
                                min_seg_frac  = 0.05) {
  # min_seg_frac: minimum segment size as fraction of total obs
  # 0.05 = at least 5% of data between breaks (avoids spurious micro-breaks)

  cat("\nPreparing data (resampling to", resample_to, ")...\n")
  data <- .prepare_data(ff, resample_to)
  n    <- nrow(data)
  cat("  Observations:", n, "\n")
  cat("  Date range  :", format(min(data$timestamp)), "->",
      format(max(data$timestamp)), "\n\n")

  min_seg <- max(floor(n * min_seg_frac), 10)

  # ── Run 4 parallel models ─────────────────────────────────────────────────

  cat("Running Model 1: M1M2 only...\n")
  m1 <- .run_bai_perron(data, vars = "M1M2",
                         max_breaks = max_breaks, min_seg = min_seg)

  cat("Running Model 2: M1M2 + slope...\n")
  m2 <- .run_bai_perron(data, vars = c("M1M2", "slope"),
                         max_breaks = max_breaks, min_seg = min_seg)

  cat("Running Model 3: M1M2 + slope + roll_yield + vol...\n")
  m3 <- .run_bai_perron(data, vars = c("M1M2", "slope", "roll_yield_ann", "vol_M1M2"),
                         max_breaks = max_breaks, min_seg = min_seg)

  cat("Running Model 4: All spreads...\n")
  m4 <- .run_bai_perron(data, vars = c("M1M2", "M1M3", "M1M6", "M1M12"),
                         max_breaks = max_breaks, min_seg = min_seg)

  models <- list(
    "M1: M1M2 only"          = m1,
    "M2: M1M2 + slope"       = m2,
    "M3: Full (4 vars)"      = m3,
    "M4: All spreads"        = m4
  )

  # ── Consensus break dates ─────────────────────────────────────────────────
  consensus <- .find_consensus_breaks(models, data$timestamp, window_days = 30)

  # ── Label regimes ─────────────────────────────────────────────────────────
  cfg           <- .get_product_config(attr(ff, "source_path"))
  regime_labels <- .label_regimes(data, consensus$high_confidence, cfg)
  cat("  Product config:", cfg$unit, "(1 unit =", cfg$to_bbl, "bbl equiv)
")

  # ── Summary table ─────────────────────────────────────────────────────────
  summary_tbl <- .build_summary(models, consensus, regime_labels, data)

  cat("\n", strrep("=", 60), "\n")
  cat("STRUCTURAL BREAK DETECTION COMPLETE\n")
  cat(strrep("=", 60), "\n\n")
  print(summary_tbl)

  list(
    data          = data,
    models        = models,
    consensus     = consensus,
    regime_labels = regime_labels,
    summary       = summary_tbl
  )
}

# ── Prepare data ──────────────────────────────────────────────────────────────

.prepare_data <- function(ff, resample_to) {
  spds  <- get_spreads(ff,        resample_to = resample_to)
  curve <- get_curve_metrics(ff,  resample_to = resample_to)

  dt <- merge(spds, curve, by = "timestamp")

  # Rolling 21-bar volatility of M1M2
  dt[, vol_M1M2 := zoo::rollapply(M1M2, 21, sd, fill = NA, align = "right")]

  # Drop rows with too many NAs (start of series)
  core_cols <- c("M1M2", "slope", "roll_yield_ann")
  dt <- dt[complete.cases(dt[, ..core_cols])]

  dt
}

# ── Bai-Perron breakpoint detection ──────────────────────────────────────────

.run_bai_perron <- function(data, vars, max_breaks, min_seg) {
  # Use first variable as dependent, rest as additional signals
  # For single variable: OLS mean-shift model (y ~ 1)
  # For multiple variables: use first var, condition on others

  y <- as.numeric(data[[vars[1]]])

  # Remove any remaining NAs
  valid <- !is.na(y)
  if (sum(valid) < 30) {
    cat("  WARNING: insufficient data for", vars[1], "\n")
    return(list(break_dates = as.Date(character(0)), bp_obj = NULL))
  }

  y_clean <- y[valid]
  ts_clean <- data$timestamp[valid]

  tryCatch({
    # Bai-Perron test: structural change in mean (intercept-only model)
    bp <- breakpoints(y_clean ~ 1,
                      h     = min_seg,
                      breaks = max_breaks)

    # Select optimal number of breaks by BIC
    bp_summary <- summary(bp)
    opt_breaks <- .select_optimal_breaks(bp)

    if (is.na(opt_breaks) || opt_breaks == 0) {
      cat("  No significant breaks found\n")
      return(list(break_dates = as.Date(character(0)), bp_obj = bp,
                  n_breaks = 0, criterion = "BIC"))
    }

    # Extract break indices and convert to dates
    bp_final   <- breakpoints(bp, breaks = opt_breaks)
    break_idx  <- bp_final$breakpoints
    break_idx  <- break_idx[!is.na(break_idx)]
    break_dates <- as.Date(ts_clean[break_idx])

    cat("  Found", length(break_dates), "break(s):",
        paste(format(break_dates), collapse = ", "), "\n")

    list(
      break_dates = break_dates,
      bp_obj      = bp,
      bp_final    = bp_final,
      n_breaks    = length(break_dates),
      criterion   = "BIC",
      timestamps  = ts_clean,
      y           = y_clean,
      vars        = vars
    )
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
    list(break_dates = as.Date(character(0)), bp_obj = NULL, n_breaks = 0)
  })
}

# ── Select optimal breaks via BIC ─────────────────────────────────────────────

.select_optimal_breaks <- function(bp) {
  tryCatch({
    s <- summary(bp)
    # BIC is in RSS column of summary — find minimum
    bic_vals <- s$RSS["BIC",]
    bic_vals <- bic_vals[!is.na(bic_vals)]
    if (length(bic_vals) == 0) return(0)
    opt <- as.integer(names(which.min(bic_vals)))
    if (is.na(opt)) 0 else opt
  }, error = function(e) 0)
}

# ── Find consensus break dates ────────────────────────────────────────────────

.find_consensus_breaks <- function(models, timestamps, window_days = 30) {
  # Collect all break dates from all models
  all_breaks <- lapply(names(models), function(name) {
    dates <- models[[name]]$break_dates
    if (length(dates) == 0) return(data.table())
    data.table(date = dates, model = name)
  })
  all_breaks <- rbindlist(all_breaks[sapply(all_breaks, nrow) > 0])

  if (nrow(all_breaks) == 0) {
    cat("WARNING: No breaks found in any model\n")
    return(list(all = all_breaks, high_confidence = as.Date(character(0)),
                medium_confidence = as.Date(character(0))))
  }

  # Cluster breaks within window_days of each other
  all_breaks <- all_breaks[order(date)]
  all_dates  <- sort(unique(all_breaks$date))

  clusters <- list()
  used      <- rep(FALSE, length(all_dates))

  for (i in seq_along(all_dates)) {
    if (used[i]) next
    cluster_dates <- all_dates[abs(as.numeric(all_dates - all_dates[i])) <= window_days]
    cluster_models <- all_breaks[date %in% cluster_dates, unique(model)]
    consensus_date <- median(cluster_dates)
    clusters[[length(clusters) + 1]] <- list(
      consensus_date  = consensus_date,
      model_count     = length(cluster_models),
      models          = cluster_models,
      date_range      = range(cluster_dates)
    )
    used[all_dates %in% cluster_dates] <- TRUE
  }

  # Classify by confidence
  .extract_dates <- function(min_count) {
    matched <- clusters[sapply(clusters, `[[`, "model_count") >= min_count &
                        sapply(clusters, `[[`, "model_count") < (min_count + 10)]
    if (length(matched) == 0) return(as.Date(character(0)))
    as.Date(as.numeric(sapply(matched, `[[`, "consensus_date")), origin = "1970-01-01")
  }
  high_conf    <- .extract_dates(3)
  medium_conf  <- as.Date(as.numeric(sapply(
    clusters[sapply(clusters, `[[`, "model_count") == 2], `[[`, "consensus_date"
  )), origin = "1970-01-01")
  if (length(medium_conf) == 0) medium_conf <- as.Date(character(0))
  low_conf     <- as.Date(as.numeric(sapply(
    clusters[sapply(clusters, `[[`, "model_count") == 1], `[[`, "consensus_date"
  )), origin = "1970-01-01")
  if (length(low_conf) == 0) low_conf <- as.Date(character(0))

  cat("\nConsensus break dates:\n")
  cat("  High confidence (3-4 models agree)  :", length(high_conf), "breaks\n")
  cat("  Medium confidence (2 models agree)  :", length(medium_conf), "breaks\n")
  cat("  Low confidence (1 model only)       :", length(low_conf), "breaks\n")

  list(
    all              = all_breaks,
    clusters         = clusters,
    high_confidence  = sort(high_conf),
    medium_confidence= sort(medium_conf),
    low_confidence   = sort(low_conf)
  )
}

# ── Label regimes between break dates ────────────────────────────────────────

.label_regimes <- function(data, break_dates, cfg = PRODUCT_CONFIG$CL) {
  # Use high-confidence breaks to define periods
  # Label each period by its dominant curve characteristics

  boundaries <- sort(c(as.Date(min(data$timestamp)) - 1,
                        break_dates,
                        as.Date(max(data$timestamp)) + 1))

  result <- copy(data[, .(timestamp, M1M2, slope, roll_yield_ann,
                           contango, m1_level)])
  result[, date := as.Date(timestamp)]
  result[, regime_id := NA_integer_]
  result[, curve_regime := NA_character_]
  result[, vol_regime   := NA_character_]

  for (i in seq_len(length(boundaries) - 1)) {
    start_d <- boundaries[i]
    end_d   <- boundaries[i + 1]
    idx     <- result$date > start_d & result$date <= end_d

    if (sum(idx, na.rm = TRUE) == 0) next

    result[idx, regime_id := i]

    # Curve regime label based on median M1M2 and slope in period
    med_m1m2  <- median(result$M1M2[idx],  na.rm = TRUE)
    med_slope <- median(result$slope[idx], na.rm = TRUE)

    curve_lbl <- dplyr::case_when(
      med_m1m2 >  cfg$thresholds$deep_back  & med_slope >  cfg$thresholds$slope_deep_back  ~ "deep_backwardation",
      med_m1m2 >  cfg$thresholds$mild_back  & med_slope >  cfg$thresholds$slope_mild_back  ~ "mild_backwardation",
      med_m1m2 >= cfg$thresholds$flat_lo    & med_m1m2 <= cfg$thresholds$mild_back         ~ "flat",
      med_m1m2 <  cfg$thresholds$mild_cont  & med_slope <  cfg$thresholds$slope_mild_cont  ~ "mild_contango",
      med_m1m2 <  cfg$thresholds$deep_cont  & med_slope <  cfg$thresholds$slope_deep_cont  ~ "deep_contango",
      TRUE                                                                                   ~ "transitional"
    )
    result[idx, curve_regime := curve_lbl]
  }

  # Vol regime based on rolling vol percentile (full history)
  vol_series <- zoo::rollapply(result$M1M2, 63, sd, fill = NA, align = "right")
  vol_pct    <- rank(vol_series, na.last = "keep") / sum(!is.na(vol_series))
  result[, vol_regime := dplyr::case_when(
    vol_pct < 0.25 ~ "low_vol",
    vol_pct < 0.75 ~ "normal_vol",
    vol_pct < 0.95 ~ "high_vol",
    TRUE           ~ "crisis_vol"
  )]

  result[, combined_regime := paste(curve_regime, vol_regime, sep = " | ")]
  result[, date := NULL]
  result
}

# ── Summary table ─────────────────────────────────────────────────────────────

.build_summary <- function(models, consensus, regime_labels, data) {
  # Per-model break count
  model_summary <- rbindlist(lapply(names(models), function(name) {
    m <- models[[name]]
    data.table(
      model        = name,
      n_breaks     = ifelse(is.null(m$n_breaks), 0, m$n_breaks),
      break_dates  = paste(format(m$break_dates), collapse = " | ")
    )
  }))

  # Consensus summary
  cat("\n--- Break dates by confidence ---\n")
  if (length(consensus$high_confidence) > 0) {
    cat("HIGH   :", paste(format(consensus$high_confidence), collapse = ", "), "\n")
  }
  if (length(consensus$medium_confidence) > 0) {
    cat("MEDIUM :", paste(format(consensus$medium_confidence), collapse = ", "), "\n")
  }

  # Regime duration summary
  if (!is.null(regime_labels) && nrow(regime_labels) > 0) {
    cat("\n--- Regime periods ---\n")
    reg_summary <- regime_labels[!is.na(regime_id),
                                  .(start = min(as.Date(timestamp)),
                                    end   = max(as.Date(timestamp)),
                                    days  = as.integer(max(as.Date(timestamp)) -
                                                       min(as.Date(timestamp))),
                                    med_M1M2  = round(median(M1M2,  na.rm=TRUE), 3),
                                    med_slope = round(median(slope, na.rm=TRUE), 3)),
                                  by = .(regime_id, curve_regime)]
    setorder(reg_summary, regime_id)
    print(reg_summary)
  }

  model_summary
}

# ── Plot breaks ───────────────────────────────────────────────────────────────

plot_breaks <- function(results, series = "M1M2") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cat("Install ggplot2 for plots: install.packages('ggplot2')\n")
    return(invisible(NULL))
  }
  library(ggplot2)

  data    <- results$data
  regimes <- results$regime_labels
  hc      <- results$consensus$high_confidence
  mc      <- results$consensus$medium_confidence

  p <- ggplot(data, aes(x = timestamp, y = .data[[series]])) +
    geom_line(colour = "#378ADD", linewidth = 0.4, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "gray50", linewidth = 0.3)

  # Add high-confidence breaks (solid red)
  if (length(hc) > 0) {
    p <- p + geom_vline(xintercept = as.POSIXct(hc),
                        colour = "#E24B4A", linewidth = 0.8, linetype = "solid") +
      annotate("text", x = as.POSIXct(hc), y = max(data[[series]], na.rm=TRUE) * 0.9,
               label = "HC", colour = "#E24B4A", size = 2.5, hjust = -0.1)
  }

  # Add medium-confidence breaks (dashed orange)
  if (length(mc) > 0) {
    p <- p + geom_vline(xintercept = as.POSIXct(mc),
                        colour = "#EF9F27", linewidth = 0.6, linetype = "dashed") +
      annotate("text", x = as.POSIXct(mc), y = max(data[[series]], na.rm=TRUE) * 0.75,
               label = "MC", colour = "#EF9F27", size = 2.5, hjust = -0.1)
  }

  # Shade regime periods
  if (!is.null(regimes) && nrow(regimes) > 0) {
    regime_bands <- regimes[!is.na(regime_id),
                             .(start = min(timestamp), end = max(timestamp),
                               curve_regime = first(curve_regime)),
                             by = regime_id]

    regime_colours <- c(
      "deep_backwardation" = "#FAECE7",
      "mild_backwardation" = "#EAF3DE",
      "flat"               = "#F1EFE8",
      "mild_contango"      = "#E6F1FB",
      "deep_contango"      = "#EEEDFE",
      "transitional"       = "#FAEEDA"
    )

    for (i in seq_len(nrow(regime_bands))) {
      band  <- regime_bands[i]
      color <- regime_colours[band$curve_regime]
      if (is.na(color)) color <- "#F1EFE8"
      p <- p + annotate("rect",
                         xmin = band$start, xmax = band$end,
                         ymin = -Inf, ymax = Inf,
                         fill = color, alpha = 0.3)
    }
  }

  p <- p +
    labs(
      title    = paste("Structural breaks —", series),
      subtitle = "Red = high confidence (3-4 models), Orange = medium (2 models)",
      x        = NULL,
      y        = paste(series, "($/bbl)")
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(size = 13, face = "bold"))

  print(p)
  invisible(p)
}