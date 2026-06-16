# R/factor_loader.R
# -----------------
# Fetches, cleans, and constructs all external factor variables
# needed for the Stage 3 regime-conditional elastic net model.
#
# Data sources:
#   FRED API  — SOFR, DXY, BDI
#   EIA API v2 — crude stocks, Cushing, gasoline, distillates,
#                 crude production, refinery utilisation
#   CFTC CSV  — net managed money positions (WTI + Brent)
#   Internal  — crack spreads derived from existing futures CSVs
#
# Output:
#   output/factors_daily.csv   — daily factor panel (SOFR, DXY, BDI,
#                                 crack spreads, internal curve vars)
#   output/factors_weekly.csv  — weekly factor panel (EIA, CFTC)
#   output/factors_combined.csv — merged daily panel (weekly data
#                                  forward-filled to daily frequency)
#
# Usage:
#   source("R/factor_loader.R")
#   factors <- load_all_factors(start = "2021-01-01", end = "2026-05-31")
#   print(head(factors$combined))

suppressPackageStartupMessages({
  library(data.table)
  library(httr)
  library(jsonlite)
  library(zoo)
})

# ── API keys (read from environment — set via .Renviron) ──────────────────
.get_fred_key <- function() {
  key <- Sys.getenv("FRED_API_KEY")
  if (nchar(key) == 0) stop("FRED_API_KEY not set. Run setup_api_keys() first.")
  key
}
.get_eia_key <- function() {
  key <- Sys.getenv("EIA_API_KEY")
  if (nchar(key) == 0) stop("EIA_API_KEY not set. Run setup_api_keys() first.")
  key
}

# ── One-time setup: write keys to .Renviron ───────────────────────────────

setup_api_keys <- function(fred_key, eia_key,
                            renviron_path = file.path(Sys.getenv("HOME"),
                                                       ".Renviron")) {
  # Read existing .Renviron if it exists
  existing <- if (file.exists(renviron_path)) {
    readLines(renviron_path)
  } else {
    character(0)
  }

  # Remove any existing key lines
  existing <- existing[!grepl("^FRED_API_KEY=|^EIA_API_KEY=", existing)]

  # Append new keys
  new_lines <- c(existing,
                 paste0("FRED_API_KEY=", fred_key),
                 paste0("EIA_API_KEY=",  eia_key))

  writeLines(new_lines, renviron_path)

  # Load into current session immediately
  Sys.setenv(FRED_API_KEY = fred_key)
  Sys.setenv(EIA_API_KEY  = eia_key)

  cat("API keys written to:", renviron_path, "\n")
  cat("Keys loaded into current session.\n")
  cat("Note: .Renviron is gitignored — keys will not be committed.\n")

  # Ensure .gitignore covers .Renviron
  gitignore_path <- ".gitignore"
  if (file.exists(gitignore_path)) {
    gi <- readLines(gitignore_path)
    if (!any(grepl("^\\.Renviron$", gi))) {
      write(".Renviron", gitignore_path, append = TRUE)
      cat(".Renviron added to .gitignore\n")
    }
  } else {
    writeLines(".Renviron", gitignore_path)
    cat(".gitignore created with .Renviron entry\n")
  }

  invisible(TRUE)
}

# ═════════════════════════════════════════════════════════════════════════════
# SECTION 1 — FRED FETCHER
# ═════════════════════════════════════════════════════════════════════════════

.fetch_fred <- function(series_id, start, end, label) {
  cat("  Fetching FRED:", series_id, "(", label, ")...\n")

  url <- "https://api.stlouisfed.org/fred/series/observations"
  resp <- GET(url, query = list(
    series_id         = series_id,
    observation_start = as.character(start),
    observation_end   = as.character(end),
    api_key           = .get_fred_key(),
    file_type         = "json"
  ))

  if (http_error(resp)) {
    cat("  WARN: FRED", series_id, "returned HTTP", status_code(resp), "\n")
    return(NULL)
  }

  data <- fromJSON(rawToChar(resp$content))$observations
  if (is.null(data) || nrow(data) == 0) {
    cat("  WARN: FRED", series_id, "returned no data\n")
    return(NULL)
  }

  dt <- data.table(
    date  = as.Date(data$date),
    value = suppressWarnings(as.numeric(data$value))
  )
  dt <- dt[!is.na(value)]
  setnames(dt, "value", label)
  cat("    Got", nrow(dt), "observations:", format(min(dt$date)),
      "to", format(max(dt$date)), "\n")
  dt
}

fetch_fred_factors <- function(start = "2021-01-01", end = "2026-05-31") {
  cat("\n--- FRED factors ---\n")

  start <- as.Date(start)
  end   <- as.Date(end)

  sofr <- .fetch_fred("SOFR",     start, end, "sofr")
  dxy  <- .fetch_fred("DTWEXBGS", start, end, "dxy")
  bdi  <- .fetch_fred("BDIY",     start, end, "bdi")

  # Merge on date — outer join, then forward-fill gaps
  all_dates <- data.table(date = seq(start, end, by = "day"))

  merged <- all_dates
  for (dt in list(sofr, dxy, bdi)) {
    if (!is.null(dt)) merged <- merge(merged, dt, by = "date", all.x = TRUE)
  }

  # Forward-fill (SOFR and DXY have weekday gaps, BDI has weekend gaps)
  num_cols <- setdiff(names(merged), "date")
  for (col in num_cols) {
    merged[[col]] <- zoo::na.locf(merged[[col]], na.rm = FALSE)
  }

  cat("  FRED panel:", nrow(merged), "rows,",
      ncol(merged) - 1, "series\n")
  merged
}

# ═════════════════════════════════════════════════════════════════════════════
# SECTION 2 — EIA FETCHER
# ═════════════════════════════════════════════════════════════════════════════
#
# EIA API v2 endpoint structure:
#   /petroleum/stoc/wstk/data/ — weekly stocks
#   /petroleum/sum/sndw/data/  — weekly supply
#
# Key series (weekly, thousand barrels or percent):
#   WCRSTUS1  — US crude oil stocks total (kb)
#   WCSSTUS1  — Cushing OK crude stocks (kb)
#   WGTSTUS1  — US total gasoline stocks (kb)
#   WDISTUS1  — US distillate fuel oil stocks (kb)
#   WCRFPUS2  — US crude oil field production (kb/d)
#   WPULEUS2  — US refinery utilisation rate (%)

.fetch_eia <- function(series_id, start, end, label, facet_key = "series") {
  cat("  Fetching EIA:", series_id, "(", label, ")...\n")

  url  <- "https://api.eia.gov/v2/petroleum/stoc/wstk/data/"

  # Different endpoints for different series types
  if (grepl("WCRFP", series_id)) {
    url <- "https://api.eia.gov/v2/petroleum/sum/sndw/data/"
  } else if (grepl("WPULE", series_id)) {
    url <- "https://api.eia.gov/v2/petroleum/opec/opprod/data/"
  }

  resp <- GET(url, query = list(
    api_key         = .get_eia_key(),
    frequency       = "weekly",
    `data[0]`       = "value",
    `facets[series][]` = series_id,
    start           = format(as.Date(start), "%Y-%m-%d"),
    end             = format(as.Date(end),   "%Y-%m-%d"),
    sort            = "period",
    offset          = 0,
    length          = 5000
  ))

  if (http_error(resp)) {
    cat("  WARN: EIA", series_id, "HTTP", status_code(resp), "\n")
    return(NULL)
  }

  parsed <- tryCatch(fromJSON(rawToChar(resp$content)),
                      error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$response$data)) {
    cat("  WARN: EIA", series_id, "returned no data\n")
    return(NULL)
  }

  raw <- as.data.table(parsed$response$data)
  if (nrow(raw) == 0 || !"period" %in% names(raw)) {
    cat("  WARN: EIA", series_id, "empty response\n")
    return(NULL)
  }

  dt <- data.table(
    date  = as.Date(raw$period),
    value = suppressWarnings(as.numeric(raw$value))
  )
  dt <- dt[!is.na(value)][order(date)]
  setnames(dt, "value", label)
  cat("    Got", nrow(dt), "observations:", format(min(dt$date)),
      "to", format(max(dt$date)), "\n")
  dt
}

.fetch_eia_v2 <- function(route, series_id, start, end, label) {
  # Generic EIA v2 fetcher with explicit route
  cat("  Fetching EIA:", series_id, "(", label, ")...\n")

  base_url <- paste0("https://api.eia.gov/v2/", route, "/data/")

  resp <- GET(base_url,
              add_headers(`X-Params` = ""),
              query = list(
                api_key              = .get_eia_key(),
                frequency            = "weekly",
                `data[0]`            = "value",
                `facets[series][]`   = series_id,
                start                = format(as.Date(start), "%Y-%m-%d"),
                end                  = format(as.Date(end),   "%Y-%m-%d"),
                `sort[0][column]`    = "period",
                `sort[0][direction]` = "asc",
                offset               = 0,
                length               = 5000
              ))

  if (http_error(resp)) {
    cat("  WARN:", series_id, "HTTP", status_code(resp), "\n")
    return(NULL)
  }

  parsed <- tryCatch(fromJSON(rawToChar(resp$content)), error = function(e) NULL)
  if (is.null(parsed)) return(NULL)

  raw <- tryCatch(as.data.table(parsed$response$data), error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) {
    cat("  WARN:", series_id, "empty\n")
    return(NULL)
  }

  # Find value column
  val_col <- intersect(c("value", "Value"), names(raw))[1]
  if (is.na(val_col)) {
    cat("  WARN:", series_id, "no value column. Columns:", paste(names(raw), collapse=", "), "\n")
    return(NULL)
  }

  dt <- data.table(
    date  = as.Date(raw$period),
    value = suppressWarnings(as.numeric(raw[[val_col]]))
  )
  dt <- dt[!is.na(value)][order(date)]
  setnames(dt, "value", label)
  cat("    Got", nrow(dt), "obs:", format(min(dt$date)),
      "to", format(max(dt$date)), "\n")
  dt
}

fetch_eia_factors <- function(start = "2021-01-01", end = "2026-05-31") {
  cat("\n--- EIA factors ---\n")

  # Stocks — petroleum/stoc/wstk
  crude_stocks   <- .fetch_eia_v2("petroleum/stoc/wstk", "WCRSTUS1", start, end, "crude_stocks_kb")
  cushing_stocks <- .fetch_eia_v2("petroleum/stoc/wstk", "WCSSTUS1", start, end, "cushing_stocks_kb")
  gasoline_stocks<- .fetch_eia_v2("petroleum/stoc/wstk", "WGTSTUS1", start, end, "gasoline_stocks_kb")
  distillate_stocks <- .fetch_eia_v2("petroleum/stoc/wstk", "WDISTUS1", start, end, "distillate_stocks_kb")

  # Production — petroleum/sum/sndw
  crude_prod     <- .fetch_eia_v2("petroleum/sum/sndw", "WCRFPUS2", start, end, "crude_prod_kbd")

  # Refinery utilisation — petroleum/sum/rup
  refinery_util  <- .fetch_eia_v2("petroleum/sum/rup",  "WPULEUS2", start, end, "refinery_util_pct")

  # Merge all weekly EIA series
  series_list <- list(crude_stocks, cushing_stocks, gasoline_stocks,
                      distillate_stocks, crude_prod, refinery_util)
  series_list <- series_list[!sapply(series_list, is.null)]

  if (length(series_list) == 0) {
    cat("  WARN: No EIA series fetched successfully\n")
    return(NULL)
  }

  merged <- Reduce(function(a, b) merge(a, b, by = "date", all = TRUE),
                   series_list)
  setorder(merged, date)

  # Compute 5-year seasonal averages for surprise calculation
  # Surprise = actual - rolling 5yr same-week average
  if ("crude_stocks_kb" %in% names(merged)) {
    merged[, week_of_year := as.integer(format(date, "%V"))]
    merged[, crude_stocks_5yr_avg := {
      sapply(seq_len(.N), function(i) {
        w   <- week_of_year[i]
        d   <- date[i]
        # Use same week in prior 5 years
        prior <- merged[week_of_year == w & date < d &
                          date >= (d - 365*5), crude_stocks_kb]
        if (length(prior) >= 3) mean(prior, na.rm=TRUE) else NA_real_
      })
    }]
    merged[, crude_stocks_surprise := crude_stocks_kb - crude_stocks_5yr_avg]
    merged[, week_of_year := NULL]
    merged[, crude_stocks_5yr_avg := NULL]
  }

  cat("  EIA panel:", nrow(merged), "rows,",
      ncol(merged) - 1, "series\n")
  merged
}

# ═════════════════════════════════════════════════════════════════════════════
# SECTION 3 — CFTC COT FETCHER
# ═════════════════════════════════════════════════════════════════════════════
#
# CFTC publishes COT reports as annual CSV files.
# WTI contract code:  067651  (CFTC)
# Brent contract:     06765A  (ICE, in disaggregated report)
# We use the Disaggregated Futures Only report for managed money positions.

fetch_cftc_factors <- function(start = "2021-01-01", end = "2026-05-31",
                                output_dir = "output") {

  cat("\n--- CFTC COT factors ---\n")

  start_yr <- as.integer(format(as.Date(start), "%Y"))
  end_yr   <- as.integer(format(as.Date(end),   "%Y"))

  # CFTC disaggregated COT — annual ZIP files
  base_url <- "https://www.cftc.gov/files/dea/history/fut_disagg_txt_"

  all_rows <- list()

  for (yr in start_yr:end_yr) {
    url      <- paste0(base_url, yr, ".zip")
    zip_path <- file.path(output_dir, paste0("cftc_cot_", yr, ".zip"))
    csv_path <- file.path(output_dir, paste0("cftc_cot_", yr, ".csv"))

    cat("  Downloading CFTC COT", yr, "...\n")

    resp <- tryCatch(
      GET(url, write_disk(zip_path, overwrite = TRUE), timeout(60)),
      error = function(e) { cat("  WARN: CFTC", yr, "download failed\n"); NULL }
    )

    if (is.null(resp) || http_error(resp)) {
      cat("  WARN: CFTC", yr, "not available\n")
      next
    }

    # Unzip
    tryCatch({
      unzip(zip_path, exdir = output_dir, overwrite = TRUE)
      # Find the extracted CSV
      extracted <- list.files(output_dir, pattern = "f_year\\.txt|disagg.*\\.txt",
                              full.names = TRUE, ignore.case = TRUE)
      if (length(extracted) == 0) {
        extracted <- list.files(output_dir, pattern = "\\.txt$",
                                full.names = TRUE)
      }
      if (length(extracted) > 0) {
        file.copy(extracted[1], csv_path, overwrite = TRUE)
      }
    }, error = function(e) {
      cat("  WARN: CFTC", yr, "unzip failed:", conditionMessage(e), "\n")
    })

    if (!file.exists(csv_path)) next

    cot <- tryCatch(
      fread(csv_path, select = c(
        "Market_and_Exchange_Names",
        "As_of_Date_in_Form_YYMMDD",
        "CFTC_Commodity_Code",
        "M_Money_Positions_Long_All",
        "M_Money_Positions_Short_All"
      )),
      error = function(e) {
        cat("  WARN: CFTC", yr, "parse failed\n"); NULL
      }
    )
    if (is.null(cot)) next

    # Filter for WTI (067651) and Brent (06765A)
    cot_filtered <- cot[CFTC_Commodity_Code %in% c("067651", "06765A")]

    if (nrow(cot_filtered) == 0) {
      # Try name-based filter
      cot_filtered <- cot[grepl("CRUDE OIL|BRENT", Market_and_Exchange_Names,
                                 ignore.case = TRUE)]
    }

    if (nrow(cot_filtered) > 0) {
      cot_filtered[, date := as.Date(
        as.character(As_of_Date_in_Form_YYMMDD), format = "%y%m%d"
      )]
      all_rows[[as.character(yr)]] <- cot_filtered
      cat("    Got", nrow(cot_filtered), "COT rows for", yr, "\n")
    }
  }

  if (length(all_rows) == 0) {
    cat("  WARN: No CFTC data fetched\n")
    return(NULL)
  }

  cot_all <- rbindlist(all_rows, fill = TRUE)
  cot_all <- cot_all[date >= as.Date(start) & date <= as.Date(end)]

  # Compute net managed money position and z-score
  cot_all[, net_mm := M_Money_Positions_Long_All - M_Money_Positions_Short_All]

  # Aggregate across WTI + Brent (sum net positions)
  cot_agg <- cot_all[, .(
    net_mm_total = sum(net_mm, na.rm = TRUE)
  ), by = date][order(date)]

  # Z-score of net position vs 52-week rolling window
  cot_agg[, cftc_net_mm_zscore := {
    n <- .N
    z <- rep(NA_real_, n)
    for (i in 52:n) {
      w <- net_mm_total[(i-51):i]
      z[i] <- (net_mm_total[i] - mean(w, na.rm=TRUE)) / sd(w, na.rm=TRUE)
    }
    z
  }]

  result <- cot_agg[, .(date, cftc_net_mm_total = net_mm_total, cftc_net_mm_zscore)]
  cat("  CFTC panel:", nrow(result), "rows\n")
  result
}

# ═════════════════════════════════════════════════════════════════════════════
# SECTION 4 — INTERNAL CRACK SPREAD FACTORS
# ═════════════════════════════════════════════════════════════════════════════
#
# Crack spreads derived from existing futures CSV data.
# Gasoil crack = LGO M1 level - LCO M1 level (converted to $/bbl)
# HO crack proxy = HO M1 level * 42 - CL M1 level ($/bbl equivalent)
# Seasonal deviation = actual - rolling 5yr same-day-of-year average

fetch_internal_factors <- function(start = "2021-01-01", end = "2026-05-31") {
  cat("\n--- Internal crack spread factors ---\n")

  # Load daily regime labels which contain M1 level for each product
  labels_cl  <- tryCatch(fread("output/regime_labels_CL.csv"),  error=function(e) NULL)
  labels_lco <- tryCatch(fread("output/regime_labels_LCO.csv"), error=function(e) NULL)
  labels_ho  <- tryCatch(fread("output/regime_labels_HO.csv"),  error=function(e) NULL)
  labels_lgo <- tryCatch(fread("output/regime_labels_LGO.csv"), error=function(e) NULL)

  # Also load structural break outputs which contain m1_level and curve vars
  signals_path <- "output/model_signals.rds"

  dt <- data.table(date = seq(as.Date(start), as.Date(end), by = "day"))

  # ── Gasoil crack: LGO M1 ($/mt ÷ 7.45 for $/bbl) - LCO M1 ($/bbl) ──────
  if (!is.null(labels_lgo) && !is.null(labels_lco)) {
    lgo_m1 <- labels_lgo[, .(date = as.Date(date), M1M2_lgo = M1M2)]
    lco_m1 <- labels_lco[, .(date = as.Date(date), M1M2_lco = M1M2)]

    # Use M1 level if available, else proxy from M1M2 + Kalman mean
    if ("kf_mean" %in% names(labels_lgo)) {
      lgo_m1[, lgo_spread_bbl := labels_lgo$kf_mean / 7.45]
      lco_m1[, lco_spread_bbl := labels_lco$kf_mean]
    }

    crack_lgo <- merge(
      labels_lgo[, .(date = as.Date(date), M1M2_lgo = M1M2,
                      kf_mean_lgo = kf_mean)],
      labels_lco[, .(date = as.Date(date), M1M2_lco = M1M2,
                      kf_mean_lco = kf_mean)],
      by = "date"
    )

    # Gasoil crack proxy: LGO M1M2 vs LCO M1M2 (relative tightness)
    crack_lgo[, gasoil_crack_proxy := (kf_mean_lgo / 7.45) - kf_mean_lco]
    dt <- merge(dt, crack_lgo[, .(date, gasoil_crack_proxy)],
                by = "date", all.x = TRUE)
    cat("  Gasoil crack proxy: OK\n")
  }

  # ── HO crack: HO M1M2 relative to CL M1M2 (both in $/bbl equiv) ─────────
  if (!is.null(labels_ho) && !is.null(labels_cl)) {
    crack_ho <- merge(
      labels_ho[, .(date = as.Date(date), kf_mean_ho = kf_mean)],
      labels_cl[, .(date = as.Date(date), kf_mean_cl = kf_mean)],
      by = "date"
    )
    crack_ho[, ho_crack_proxy := (kf_mean_ho * 42) - kf_mean_cl]
    dt <- merge(dt, crack_ho[, .(date, ho_crack_proxy)],
                by = "date", all.x = TRUE)
    cat("  HO crack proxy: OK\n")
  }

  # ── Curve shape variables (from CL as benchmark) ─────────────────────────
  if (!is.null(labels_cl)) {
    curve_vars <- labels_cl[, .(
      date      = as.Date(date),
      cl_M1M2   = M1M2,
      cl_kf_z   = kf_z,
      cl_lz     = level_z_126,
      cl_conf   = confidence_score
    )]
    dt <- merge(dt, curve_vars, by = "date", all.x = TRUE)
    cat("  CL curve vars: OK\n")
  }

  # ── Seasonal deviations: compute 5yr rolling same-day average ────────────
  if ("gasoil_crack_proxy" %in% names(dt)) {
    dt[!is.na(gasoil_crack_proxy), gasoil_crack_dev := {
      n <- .N
      dev <- rep(NA_real_, n)
      doy <- as.integer(format(date, "%j"))
      for (i in seq_len(n)) {
        same_doy <- which(abs(doy - doy[i]) <= 7 &
                            date < date[i] &
                            date >= (date[i] - 365*5))
        if (length(same_doy) >= 20) {
          dev[i] <- gasoil_crack_proxy[i] -
                    mean(gasoil_crack_proxy[same_doy], na.rm=TRUE)
        }
      }
      dev
    }]
  }

  cat("  Internal factors panel:", nrow(dt[!is.na(cl_M1M2)]), "non-NA rows\n")
  dt
}

# ═════════════════════════════════════════════════════════════════════════════
# SECTION 5 — MASTER LOADER
# ═════════════════════════════════════════════════════════════════════════════

load_all_factors <- function(start      = "2021-01-01",
                              end        = "2026-05-31",
                              output_dir = "output",
                              save       = TRUE) {

  cat("\n", strrep("=", 60), "\n")
  cat("FACTOR LOADER\n")
  cat(strrep("=", 60), "\n")
  cat("Date range:", start, "to", end, "\n\n")

  dir.create(output_dir, showWarnings = FALSE)

  # ── 1. FRED factors ───────────────────────────────────────────────────────
  fred_dt  <- tryCatch(fetch_fred_factors(start, end),
                        error = function(e) {
                          cat("FRED fetch error:", conditionMessage(e), "\n")
                          NULL
                        })

  # ── 2. EIA factors ────────────────────────────────────────────────────────
  eia_dt   <- tryCatch(fetch_eia_factors(start, end),
                        error = function(e) {
                          cat("EIA fetch error:", conditionMessage(e), "\n")
                          NULL
                        })

  # ── 3. CFTC factors ───────────────────────────────────────────────────────
  cftc_dt  <- tryCatch(fetch_cftc_factors(start, end, output_dir),
                        error = function(e) {
                          cat("CFTC fetch error:", conditionMessage(e), "\n")
                          NULL
                        })

  # ── 4. Internal crack/curve factors ───────────────────────────────────────
  int_dt   <- tryCatch(fetch_internal_factors(start, end),
                        error = function(e) {
                          cat("Internal fetch error:", conditionMessage(e), "\n")
                          NULL
                        })

  # ── 5. Merge everything onto a daily spine ────────────────────────────────
  cat("\n--- Merging all factor panels ---\n")
  spine <- data.table(date = seq(as.Date(start), as.Date(end), by = "day"))

  for (panel in list(fred_dt, eia_dt, cftc_dt, int_dt)) {
    if (!is.null(panel)) {
      spine <- merge(spine, panel, by = "date", all.x = TRUE)
    }
  }

  # Forward-fill weekly/monthly data to daily
  weekly_cols <- c("crude_stocks_kb", "cushing_stocks_kb", "gasoline_stocks_kb",
                   "distillate_stocks_kb", "crude_prod_kbd", "refinery_util_pct",
                   "crude_stocks_surprise", "cftc_net_mm_total", "cftc_net_mm_zscore")

  for (col in intersect(weekly_cols, names(spine))) {
    spine[[col]] <- zoo::na.locf(spine[[col]], na.rm = FALSE)
  }

  # ── 6. Derived features for the model ─────────────────────────────────────
  cat("\n--- Computing derived features ---\n")

  # BDI 4-week rate of change
  if ("bdi" %in% names(spine)) {
    spine[, bdi_4wk_chg := (bdi / shift(bdi, 20) - 1) * 100]
  }

  # SOFR level (already a level — use directly)
  # DXY 4-week rate of change
  if ("dxy" %in% names(spine)) {
    spine[, dxy_4wk_chg := (dxy / shift(dxy, 20) - 1) * 100]
  }

  # Refinery utilisation deviation from 5yr seasonal avg
  if ("refinery_util_pct" %in% names(spine)) {
    spine[, week_num := as.integer(format(date, "%V"))]
    spine[, refinery_util_dev := {
      n   <- .N
      dev <- rep(NA_real_, n)
      for (i in seq_len(n)) {
        w <- week_num[i]
        d <- date[i]
        prior <- spine[week_num == w & date < d & date >= (d - 365*5),
                       refinery_util_pct]
        if (length(prior) >= 3)
          dev[i] <- refinery_util_pct[i] - mean(prior, na.rm=TRUE)
      }
      dev
    }]
    spine[, week_num := NULL]
  }

  # ── 7. Summary ────────────────────────────────────────────────────────────
  cat("\n--- Factor panel summary ---\n")
  cat("  Total rows:", nrow(spine), "\n")
  cat("  Columns:", ncol(spine) - 1, "factors\n\n")

  factor_cols <- setdiff(names(spine), "date")
  coverage <- spine[, lapply(.SD, function(x) {
    n_valid <- sum(!is.na(x))
    round(n_valid / .N * 100, 1)
  }), .SDcols = factor_cols]

  cat(sprintf("  %-35s %s\n", "Factor", "Coverage %"))
  cat("  ", strrep("-", 45), "\n")
  for (col in factor_cols) {
    cat(sprintf("  %-35s %s%%\n", col, coverage[[col]]))
  }

  # ── 8. Save ───────────────────────────────────────────────────────────────
  if (save) {
    # Daily factors
    daily_cols <- c("date", "sofr", "dxy", "dxy_4wk_chg", "bdi", "bdi_4wk_chg",
                    grep("crack|cl_|gasoil", names(spine), value=TRUE))
    daily_cols <- intersect(daily_cols, names(spine))
    if (length(daily_cols) > 1)
      fwrite(spine[, ..daily_cols], file.path(output_dir, "factors_daily.csv"))

    # Weekly factors
    weekly_cols_out <- c("date", intersect(weekly_cols, names(spine)))
    if (length(weekly_cols_out) > 1)
      fwrite(spine[!is.na(crude_stocks_kb) | is.na(crude_stocks_kb),
                   ..weekly_cols_out],
             file.path(output_dir, "factors_weekly.csv"))

    # Combined
    fwrite(spine, file.path(output_dir, "factors_combined.csv"))
    cat("\nSaved:\n")
    cat("  output/factors_daily.csv\n")
    cat("  output/factors_weekly.csv\n")
    cat("  output/factors_combined.csv\n")
  }

  list(
    combined = spine,
    fred     = fred_dt,
    eia      = eia_dt,
    cftc     = cftc_dt,
    internal = int_dt
  )
}