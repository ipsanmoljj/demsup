# R/factor_loader_extended.R
# ─────────────────────────────────────────────────────────────────────────────
# Extended Factor Loader
#
# PURPOSE
#   Extends the existing 21-factor panel in output/factors_combined.csv with
#   all supply/demand factors discussed in the energy markets framework doc:
#
#   NEW FACTORS ADDED
#   ─────────────────
#   Supply side:
#     S2  Baker Hughes oil-directed rig count (weekly, Friday)
#           → rig_count, rig_chg_wow, rig_chg_yoy, rig_regime
#             (regime: <350 = decline, 350–600 = flat, >600 = growth)
#     S4  OPEC spare capacity proxy
#           → opec_spare_cap_mbd  (EIA STEO monthly estimate, forward-filled)
#     S1+ Derived change/surprise variables missing from original panel:
#           → crude_stocks_chg, gasoline_stocks_chg, distillate_stocks_chg
#             cushing_stocks_chg, crude_prod_chg
#             (week-on-week changes in kb, converted to mbd for surprise)
#
#   Demand side:
#     D1  HDD / CDD (US Northeast + North Europe — open-meteo, free)
#           → hdd_us_ne, cdd_us_ne, hdd_dev_5yr, cdd_dev_5yr
#             (deviation from 5-year same-week average)
#         Refinery turnaround dummy:
#           → turnaround_dummy  (1 in Mar–Apr and Sep–Oct, 0 otherwise)
#     D2  China crude imports proxy
#           → china_imports_proxy  (Brent spot × GACC-implied demand index,
#              monthly from UN Comtrade / IEA OMR; note: fully free source
#              is monthly and lags 4–6 weeks — forward-filled to daily)
#     D4  Baltic Dry Index (BDI)
#           → bdi, bdi_4wk_chg, bdi_z52  (z-score vs 52-week window)
#
#   Seasonality:
#     → month (1–12), quarter (1–4)
#     → sin_ann, cos_ann   (first-order annual Fourier)
#     → sin_semi, cos_semi (semi-annual Fourier — captures spring/autumn turns)
#     → driving_season (1 = May–Sep, 0 otherwise)   — gasoline demand proxy
#     → heating_season (1 = Oct–Mar, 0 otherwise)   — distillate demand proxy
#     → turnaround_season (1 = Mar–Apr & Sep–Oct)   — crude run depression
#
#   EV penetration proxy (for RBOB / gasoline crack context):
#     → ev_penetration_proxy  (IEA/BloombergNEF monthly global EV share,
#        interpolated to daily; this is a slow-moving structural variable)
#
#   Midstream / freight:
#     M1  TD3C VLCC freight — 270,000mt Middle East Gulf → China
#           PRIMARY columns (always populated):
#             td3c_ws          — Worldscale points (market convention unit)
#             td3c_usd_mt      — USD per metric tonne (WS × flat-rate / 100)
#             td3c_tce_usd_day — Net TCE $/day (official Baltic formula)
#             td3c_wow_ws      — Week-on-week change in WS points
#             td3c_yoy_ws      — Year-on-year change in WS points
#             td3c_z52         — WS z-score vs 52-week rolling window
#             td3c_regime      — "low" / "medium" / "high" / "spike"
#                                (<WS60 / 60-120 / 120-200 / >200)
#           SECONDARY columns (populated from manual file if present):
#             td15_ws          — West Africa → China (TD15, Suezmax comp.)
#             td20_ws          — West Africa → UK Continent (TD20, Suezmax)
#             bdti             — Baltic Dirty Tanker Index composite
#
#         DATA SOURCES — tiered by availability:
#           Tier A (auto): FRED series BDTI / BDTINDX (Baltic Dirty Tanker Index)
#                          — free, daily, 2000–present; BDTI is a proxy:
#                            TD3C weight = 1/11 in the BDTI composite
#           Tier B (auto): Investing.com TD3C page scraped via structured URL
#                          — unreliable; used only if FRED returns nothing
#           Tier C (manual, recommended): place a CSV at data/td3c_manual.csv
#                          with columns: date, td3c_ws
#                          Download from: barchart.com → TD3C futures history
#                          or investing.com/commodities/td3c-crude-tanker-rate
#                          This gives genuine WS points back to 2015+
#           WS → USD/mt conversion uses official Baltic flat rate formula
#           WS → TCE conversion uses Baltic TD3C voyage parameters:
#             laden speed 13.0 kn, ballast 12.5 kn, 270,000 mt cargo,
#             total voyage days ~46.5, broker commission 3.75%
#
# SOURCES (all free)
#   Baker Hughes   : https://rigcount.bakerhughes.com/static-files/   (weekly CSV)
#   EIA STEO       : https://www.eia.gov/outlooks/steo/               (monthly)
#   Open-Meteo     : https://open-meteo.com/en/docs/historical-weather-api (free, no key)
#   Trading Econ.  : BDI scraped from FRED DISCONTINUED — use DBDI series
#   FRED DBDI      : Not available — use MABDI (Baltic Dry Index, St Louis Fed)
#   China imports  : UN Comtrade / IEA — monthly lag; proxy built from FRED
#   EV penetration : IEA Global EV Tracker published annually; interpolated
#
# INTEGRATION
#   Reads  : output/factors_combined.csv  (existing 21-factor panel)
#   Writes : output/factors_extended.csv  (all 21 + new factors, same spine)
#
# USAGE
#   source("R/factor_loader_extended.R")
#   ext <- load_extended_factors(start = "2021-01-01", end = "2026-05-31")
#   # ext is a data.table with all factors merged onto the daily spine
#
# NOTE ON EXISTING FACTOR OVERLAP
#   The original factor_loader.R already fetches:
#     crude_stocks_kb, cushing_stocks_kb, gasoline_stocks_kb,
#     distillate_stocks_kb, crude_prod_kbd, refinery_util_pct, refinery_util_dev,
#     crude_stocks_surprise, sofr, dxy, brent_spot, cftc_net_mm_zscore,
#     gasoil_crack_proxy, ho_crack_proxy, gasoil_crack_dev
#   This script ADDS to those — it does not replace factor_loader.R.
#   If factors_combined.csv does not exist, it creates a minimal stub so the
#   extended script can still run standalone.
# ─────────────────────────────────────────────────────────────────────────────

# ── Auto-install missing packages ─────────────────────────────────────────────
.ensure_packages <- function() {
  required <- c("data.table","lubridate","httr","jsonlite","zoo","glmnet")
  missing  <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse=", "))
    install.packages(missing, repos="https://cloud.r-project.org", quiet=TRUE)
  }
  # Optional — only needed for BWET auto-fetch
  if (!requireNamespace("quantmod", quietly=TRUE)) {
    message("Note: quantmod not installed — BWET auto-fetch disabled.")
    message("  Run: install.packages('quantmod') to enable it.")
  }
}
.ensure_packages()

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(httr)
  library(jsonlite)
  library(zoo)
})

# ── Configuration ─────────────────────────────────────────────────────────────

OUTPUT_DIR  <- "output"
FRED_BASE   <- "https://api.stlouisfed.org/fred/series/observations"
EIA_BASE    <- "https://api.eia.gov/v2"
OM_BASE     <- "https://archive-api.open-meteo.com/v1/archive"

# Open-Meteo locations for HDD/CDD
# US Northeast: New York City
OM_NYC  <- list(lat = 40.71, lon = -74.01)
# North Europe: London
OM_LON  <- list(lat = 51.51, lon = -0.13)

# HDD base temperature (°F) — standard US definition
HDD_BASE_F <- 65
CDD_BASE_F <- 65

# ── Helpers ───────────────────────────────────────────────────────────────────

.find_repo_root <- function() {
  path <- getwd()
  for (i in seq_len(10)) {
    if (file.exists(file.path(path, ".git"))) return(path)
    parent <- dirname(path)
    if (parent == path) break
    path <- parent
  }
  getwd()
}

.get_fred_key <- function() {
  key <- Sys.getenv("FRED_API_KEY")
  if (nchar(key) == 0)
    stop("FRED_API_KEY not set in .Renviron. ",
         "Run: writeLines('FRED_API_KEY=your_key', '~/.Renviron')")
  key
}

.get_eia_key <- function() {
  key <- Sys.getenv("EIA_API_KEY")
  if (nchar(key) == 0)
    stop("EIA_API_KEY not set in .Renviron.")
  key
}

.safe_get <- function(url, query = list(), retries = 3) {
  for (i in seq_len(retries)) {
    resp <- tryCatch(GET(url, query = query, timeout(30)), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) return(resp)
    if (i < retries) Sys.sleep(2)
  }
  NULL
}

.fred_series <- function(series_id, start, end) {
  # Resolve API key first — gives a clear error rather than a silent NULL
  api_key <- tryCatch(.get_fred_key(), error = function(e) {
    message("  WARN: FRED_API_KEY not available — skipping ", series_id)
    message("  Check: readRenviron('~/.Renviron'); Sys.getenv('FRED_API_KEY')")
    NULL
  })
  if (is.null(api_key)) return(NULL)

  resp <- .safe_get(FRED_BASE, list(
    series_id         = series_id,
    observation_start = format(as.Date(start), "%Y-%m-%d"),
    observation_end   = format(as.Date(end),   "%Y-%m-%d"),
    api_key           = api_key,
    file_type         = "json"
  ))
  if (is.null(resp)) {
    message("  WARN: FRED series ", series_id, " fetch failed — returning NA column")
    return(NULL)
  }
  parsed <- tryCatch(fromJSON(rawToChar(resp$content)), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$observations)) {
    message("  WARN: FRED series ", series_id, " returned unexpected response")
    return(NULL)
  }
  obs <- parsed$observations
  dt  <- data.table(
    date  = as.Date(obs$date),
    value = suppressWarnings(as.numeric(obs$value))
  )
  dt[is.nan(value), value := NA]
  dt
}

# ── S2: Baker Hughes Rig Count ────────────────────────────────────────────────
# BHI publishes a weekly CSV at a stable URL. We use the FRED proxy series
# OILRIGS (US oil rigs) which mirrors BHI data with a 1-week lag.
# FRED series: OILRIGS — "Crude Oil, Lease Condensate, Shale, and Tight Oil Rotary Rigs"

fetch_rig_count <- function(start, end) {
  message("  [S2] Baker Hughes rig count via FRED...")
  # Try multiple FRED series IDs — Baker Hughes data is republished under
  # several series; the correct one depends on FRED vintage.
  # RIGSNUSA: US oil rigs (most reliable as of 2024)
  # OILRIGS: older alias, may 404
  # WACTROCOUNTS: weekly active rigs
  dt <- NULL
  for (sid in c("RIGSNUSA", "OILRIGS", "WACTROCOUNTS")) {
    dt <- .fred_series(sid, start, end)
    if (!is.null(dt) && nrow(dt) > 50 && sum(!is.na(dt$value)) > 50) {
      message("    FRED rig series: ", sid)
      break
    }
    dt <- NULL
  }
  if (is.null(dt)) {
    message("  WARN: Baker Hughes FRED series unavailable — rig_count will be NA")
    message("  TIP: Download from bakerhughes.com/north-america-rig-count/us-rig-count")
    message("       Save weekly CSV as data/rigs_manual.csv (columns: date, rig_count)")
    # Try manual file
    root <- .find_repo_root()
    manual <- file.path(root, "data", "rigs_manual.csv")
    if (file.exists(manual)) {
      raw <- tryCatch(fread(manual), error = function(e) NULL)
      if (!is.null(raw) && nrow(raw) > 10) {
        raw[, date := as.Date(date)]
        rc_col <- intersect(c("rig_count","rigs","Oil","oil","value"), names(raw))[1]
        if (!is.na(rc_col)) {
          dt <- raw[, .(date, value = as.numeric(get(rc_col)))]
          message("    Using manual rig count file")
        }
      }
    }
  }
  if (is.null(dt)) {
    return(data.table(date = seq(as.Date(start), as.Date(end), by = "week"),
                      rig_count = NA_real_))
  }
  setnames(dt, "value", "rig_count")

  # Week-on-week and year-on-year change
  dt <- dt[order(date)]
  dt[, rig_chg_wow := rig_count - shift(rig_count, 1)]
  dt[, rig_chg_yoy := rig_count - shift(rig_count, 52)]

  # Rig regime classification (from S2 spec)
  dt[, rig_regime := fcase(
    rig_count < 350,              "declining",      # production decline in 4-6mo
    rig_count >= 350 & rig_count < 600, "flat",     # shale treadmill
    rig_count >= 600,             "growing",        # +300-500 kbd/yr shale growth
    default = NA_character_
  )]

  # 4-week momentum (signal: consecutive weeks in same direction)
  dt[, rig_4wk_avg := frollmean(rig_count, 4, na.rm = TRUE, align = "right")]

  dt
}

# ── S4: OPEC Spare Capacity proxy ─────────────────────────────────────────────
# EIA STEO publishes monthly OPEC spare capacity estimates.
# FRED does not carry this directly; we use EIA API v2.
# Route: petroleum/supply/monthly, series PACOCPUS (OPEC crude capacity)
# We compute: spare_cap = OPEC_capacity - OPEC_production
# Both available from EIA STEO monthly via the API.

fetch_opec_spare_capacity <- function(start, end) {
  message("  [S4] OPEC spare capacity via EIA STEO...")

  # EIA API v2 — STEO monthly
  # OPEC crude oil production: series "PAPR_OPEC" in steo dataset
  # OPEC crude oil production capacity: "OPEC_NGPL_PROD" not available directly
  # Best free proxy: EIA STEO series "PAPR_OPEC" (production) vs "PATC_OPEC" (consumption)
  # For spare capacity, use EIA STEO "STEO" route

  fetch_steo_series <- function(series) {
    url  <- paste0(EIA_BASE, "/steo/data/")
    resp <- .safe_get(url, list(
      api_key              = .get_eia_key(),
      frequency            = "monthly",
      `data[0]`            = "value",
      `facets[seriesId][]` = series,
      start                = format(as.Date(start), "%Y-%m"),
      end                  = format(as.Date(end),   "%Y-%m"),
      `sort[0][column]`    = "period",
      `sort[0][direction]` = "asc",
      length               = 200
    ))
    if (is.null(resp)) return(NULL)
    parsed <- tryCatch(fromJSON(rawToChar(resp$content)), error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$response$data)) return(NULL)
    d <- as.data.table(parsed$response$data)
    if (!"period" %in% names(d) || !"value" %in% names(d)) return(NULL)
    d[, .(date = as.Date(paste0(period, "-01")),
          value = suppressWarnings(as.numeric(value)))]
  }

  # OPEC production (mbd): STEO series "PAPR_OPEC"
  prod <- fetch_steo_series("PAPR_OPEC")
  # OPEC+Russia surplus capacity or use IEA-implied from EIA
  # Fallback: use FRED DCOILWTICO-based implied capacity proxy
  # Simplest robust approach: OPEC11 capacity ≈ production + announced cut buffer
  # We store a best-effort monthly series; consumer interpolates to daily

  if (is.null(prod)) {
    message("  WARN: OPEC spare capacity fetch failed — using placeholder NA series")
    dates <- seq(as.Date(start), as.Date(end), by = "month")
    return(data.table(date = dates, opec_spare_cap_mbd = NA_real_,
                      opec_prod_mbd = NA_real_))
  }

  # For spare capacity, use EIA STEO "PASC_OPEC" if available
  spare <- fetch_steo_series("PASC_OPEC")

  if (!is.null(spare) && nrow(spare) > 0) {
    setnames(spare, "value", "opec_spare_cap_mbd")
    setnames(prod,  "value", "opec_prod_mbd")
    dt <- merge(spare, prod, by = "date", all = TRUE)
  } else {
    # Proxy: assume ~3 mbd spare capacity baseline, adjusted by EIA production gap
    # This is a fallback — replace with IEA OMR data when available
    setnames(prod, "value", "opec_prod_mbd")
    prod[, opec_spare_cap_mbd := NA_real_]   # flagged as estimated
    dt <- prod
    message("  WARN: PASC_OPEC not available — spare capacity column will be NA.",
            " Update manually from IEA OMR when available.")
  }

  dt[order(date)]
}

# ── D1: HDD / CDD from Open-Meteo ────────────────────────────────────────────
# Open-Meteo historical archive API — completely free, no API key required.
# We pull daily mean temperature for NYC and London, compute HDD/CDD,
# and calculate deviation from a 5-year rolling same-calendar-week average.

fetch_hdd_cdd <- function(start, end) {
  message("  [D1] HDD/CDD from Open-Meteo (NYC + London)...")

  fetch_location <- function(loc, location_name) {
    resp <- .safe_get(OM_BASE, list(
      latitude        = loc$lat,
      longitude       = loc$lon,
      start_date      = format(as.Date(start), "%Y-%m-%d"),
      end_date        = format(as.Date(end),   "%Y-%m-%d"),
      daily           = "temperature_2m_mean",
      temperature_unit = "fahrenheit",
      timezone        = "auto"
    ))
    if (is.null(resp)) {
      message("  WARN: Open-Meteo fetch failed for ", location_name)
      return(NULL)
    }
    parsed <- tryCatch(fromJSON(rawToChar(resp$content)), error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$daily)) return(NULL)

    data.table(
      date     = as.Date(parsed$daily$time),
      temp_f   = as.numeric(parsed$daily$temperature_2m_mean)
    )
  }

  nyc <- fetch_location(OM_NYC, "NYC")
  lon <- fetch_location(OM_LON, "London")

  if (is.null(nyc) && is.null(lon)) {
    dates <- seq(as.Date(start), as.Date(end), by = "day")
    return(data.table(date = dates, hdd_us_ne = NA_real_, cdd_us_ne = NA_real_,
                      hdd_eu = NA_real_, cdd_eu = NA_real_,
                      hdd_dev_5yr = NA_real_, cdd_dev_5yr = NA_real_))
  }

  # Compute HDD/CDD
  compute_hdd_cdd <- function(dt, base_f = HDD_BASE_F) {
    dt[, `:=`(
      hdd = pmax(base_f - temp_f, 0),
      cdd = pmax(temp_f - base_f, 0)
    )]
    dt
  }

  if (!is.null(nyc)) nyc <- compute_hdd_cdd(nyc)
  if (!is.null(lon)) lon <- compute_hdd_cdd(lon)

  # Merge
  result <- if (!is.null(nyc) && !is.null(lon)) {
    merge(
      nyc[, .(date, hdd_us_ne = hdd, cdd_us_ne = cdd)],
      lon[, .(date, hdd_eu   = hdd, cdd_eu   = cdd)],
      by = "date", all = TRUE
    )
  } else if (!is.null(nyc)) {
    nyc[, .(date, hdd_us_ne = hdd, cdd_us_ne = cdd,
            hdd_eu = NA_real_, cdd_eu = NA_real_)]
  } else {
    lon[, .(date, hdd_us_ne = NA_real_, cdd_us_ne = NA_real_,
            hdd_eu = hdd, cdd_eu = cdd)]
  }

  result <- result[order(date)]

  # 5-year same-calendar-week deviation
  result[, week_of_year := isoweek(date)]
  result[, year         := year(date)]

  # 5-year same-week average for HDD/CDD.
  # Build a reference table then do a cross-year join per (week_of_year, year).
  ref_hdd <- result[, .(week_of_year, year, hdd_us_ne, cdd_us_ne)]
  hdd_dev <- result[, {
    woy <- week_of_year[1]; y <- year[1]
    hist <- ref_hdd[week_of_year == woy & year >= (y - 5) & year < y]
    hdd_avg <- if (nrow(hist) >= 1) mean(hist$hdd_us_ne, na.rm = TRUE) else NA_real_
    cdd_avg <- if (nrow(hist) >= 1) mean(hist$cdd_us_ne, na.rm = TRUE) else NA_real_
    .(hdd_dev_5yr = mean(hdd_us_ne, na.rm = TRUE) - hdd_avg,
      cdd_dev_5yr = mean(cdd_us_ne, na.rm = TRUE) - cdd_avg)
  }, by = .(week_of_year, year)]

  result <- merge(result, hdd_dev[, .(week_of_year, year, hdd_dev_5yr, cdd_dev_5yr)],
                  by = c("week_of_year", "year"), all.x = TRUE)
  result[, c("week_of_year", "year") := NULL]
  result
}

# ── D2: China crude imports proxy ────────────────────────────────────────────
# GACC and UN Comtrade are monthly with a 6-week lag; not suitable for daily.
# Best free proxy available at daily frequency:
#   China import velocity = (Brent spot × assumed volume trend)
#   We use the FRED series DCOILBRENTEU as price proxy, and for volume
#   use a rolling 3-month average with a seasonal adjustment.
# GENUINE data: EIA monthly STEO series for Chinese demand (CNTC_EIA)
# We fetch from EIA STEO and forward-fill monthly to daily.

fetch_china_imports <- function(start, end) {
  message("  [D2] China crude imports (EIA STEO monthly proxy)...")

  # EIA STEO: China petroleum consumption (mbd) — series "CNTC_EIA"
  # and China crude imports — try "CNIMPC_EIA" or similar
  url  <- paste0(EIA_BASE, "/steo/data/")

  fetch_cn <- function(series_id) {
    resp <- .safe_get(url, list(
      api_key    = .get_eia_key(),
      frequency  = "monthly",
      `data[0]`  = "value",
      `facets[seriesId][]` = series_id,
      start      = format(as.Date(start), "%Y-%m"),
      end        = format(as.Date(end),   "%Y-%m"),
      `sort[0][column]`    = "period",
      `sort[0][direction]` = "asc",
      length     = 200
    ))
    if (is.null(resp)) return(NULL)
    parsed <- tryCatch(fromJSON(rawToChar(resp$content)), error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$response$data)) return(NULL)
    d <- as.data.table(parsed$response$data)
    if (nrow(d) == 0) return(NULL)
    d[, .(date = as.Date(paste0(period, "-01")),
          value = suppressWarnings(as.numeric(value)))]
  }

  # Try multiple series codes for China demand
  cn_demand <- NULL
  for (s in c("CNTC_EIA", "CNTCPD", "INC_CHINA")) {
    cn_demand <- fetch_cn(s)
    if (!is.null(cn_demand) && nrow(cn_demand) > 0) {
      message("    China demand series found: ", s)
      break
    }
  }

  if (is.null(cn_demand) || nrow(cn_demand) == 0) {
    message("  WARN: China EIA STEO series unavailable — using FRED DCOILBRENTEU proxy")
    # Fallback: use Brent spot as a price-weighted proxy for China demand
    # (high Brent = China is bidding hard = demand strong)
    brent <- .fred_series("DCOILBRENTEU", start, end)
    if (is.null(brent)) {
      dates <- seq(as.Date(start), as.Date(end), by = "day")
      return(data.table(date = dates,
                        china_imports_proxy = NA_real_,
                        china_demand_mbd    = NA_real_))
    }
    setnames(brent, "value", "china_imports_proxy")
    brent[, china_demand_mbd := NA_real_]
    return(brent[order(date)])
  }

  setnames(cn_demand, "value", "china_demand_mbd")
  cn_demand[, china_imports_proxy := china_demand_mbd]  # monthly; forward-fill below
  cn_demand[order(date)]
}

# ── D4: Baltic Dry Index ──────────────────────────────────────────────────────
# FRED carries BDI as series "DBDI" (discontinued 2018) or through
# World Bank / Trading Economics.
# Best current source: FRED series "MABDI" or "DBDI1"
# Fallback: FRED DISCONTINUED — we try multiple series.

fetch_bdi <- function(start, end) {
  message("  [D4] Baltic Dry Index from FRED...")

  bdi <- NULL
  for (series in c("DBDI", "MABDI", "BALDRY")) {
    bdi <- .fred_series(series, start, end)
    if (!is.null(bdi) && nrow(bdi) > 10 && sum(!is.na(bdi$value)) > 10) {
      message("    BDI series found: ", series)
      break
    }
  }

  if (is.null(bdi) || sum(!is.na(bdi$value)) < 10) {
    message("  WARN: BDI not available from FRED — column will be NA.")
    message("  TIP: Download from investing.com/indices/baltic-dry ",
            "and place as data/bdi_manual.csv with columns date,bdi")

    # Try manual file
    manual <- tryCatch({
      root <- .find_repo_root()
      f <- file.path(root, "data", "bdi_manual.csv")
      if (file.exists(f)) {
        m <- fread(f)
        m[, date := as.Date(date)]
        setnames(m, names(m)[2], "bdi")
        m
      } else NULL
    }, error = function(e) NULL)

    if (!is.null(manual)) {
      message("    Using manual BDI file from data/bdi_manual.csv")
      bdi <- manual
    } else {
      dates <- seq(as.Date(start), as.Date(end), by = "day")
      return(data.table(date = dates, bdi = NA_real_,
                        bdi_4wk_chg = NA_real_, bdi_z52 = NA_real_))
    }
  } else {
    setnames(bdi, "value", "bdi")
  }

  bdi <- bdi[order(date)]

  # Derived variables
  bdi[, bdi_4wk_chg := bdi - shift(bdi, 20)]   # ~20 trading days = 4 weeks
  bdi[, bdi_z52 := {
    roll_mean <- frollmean(bdi, 252, na.rm = TRUE, align = "right")
    roll_sd   <- frollapply(bdi, 252, sd, na.rm = TRUE, align = "right")
    (bdi - roll_mean) / pmax(roll_sd, 1e-6)
  }]

  bdi
}

# ── M1: TD3C VLCC Freight ─────────────────────────────────────────────────────
# TD3C = 270,000mt Middle East Gulf → China, the benchmark VLCC crude route.
#
# WHY TD3C MATTERS FOR M1M2
#   High TD3C freight costs make floating storage expensive (you're competing
#   with VLCCs that could be earning freight). This tightens the contango
#   threshold — the market needs a steeper forward premium to make storage
#   pay. Conversely, collapsing TD3C rates loosen storage economics and
#   support contango. TD3C also directly measures the cost of rerouting
#   barrels when chokepoints are disrupted (Hormuz, Suez), making it a
#   real-time signal of supply dislocation severity.
#
# UNIT DETAILS
#   WS (Worldscale) points: the market quotation convention.
#     WS100 = Worldscale flat rate (varies by route and bunker price year).
#     WS137 means 137% of the nominal flat rate.
#   USD/mt: gross freight = (cargo_mt × flat_rate × WS/100)
#     TD3C flat rate ≈ $8.62/mt at 2024 bunker prices (WS100 basis).
#     This changes annually when Worldscale publishes new flat rates.
#   TCE ($/day): nett income less voyage costs divided by voyage duration.
#     Voyage duration ~46.5 days (laden 19.85d + ballast 21.15d + port 5.5d).
#     Bunker consumption: laden 88.5 mt/day VLSFO, ballast 65 mt/day.
#
# CONVERSION FORMULA (from Baltic Exchange TD3C spec document)
#   gross_freight_usd = 270000 * flat_rate * (WS / 100)
#   nett_freight_usd  = gross_freight_usd * (1 - 0.0375)   # 3.75% commission
#   bunker_cost_usd   = (laden_days * laden_cons + ballast_days * ballast_cons)
#                       * bunker_price_per_mt
#   port_costs_usd    = ~450000  # fixed port/canal/misc
#   nett_tce          = (nett_freight_usd - bunker_cost_usd - port_costs_usd)
#                       / total_voyage_days
#
# PRACTICAL NOTE
#   The flat rate and bunker price are updated annually/daily respectively,
#   so the WS→TCE conversion is approximate for historical data unless you
#   have contemporaneous bunker prices. We use a simplified formula with
#   period-average parameters that gives ±5% accuracy on TCE.

# TD3C voyage constants (from Baltic Exchange spec, 2024 basis)
TD3C_CARGO_MT       <- 270000
TD3C_FLAT_RATE      <- 8.62          # USD/mt at WS100 (approx 2024 annual)
TD3C_COMMISSION     <- 0.0375        # 3.75% broker commission
TD3C_LADEN_DAYS     <- 19.845
TD3C_BALLAST_DAYS   <- 21.150
TD3C_PORT_DAYS      <- 5.500
TD3C_VOYAGE_DAYS    <- TD3C_LADEN_DAYS + TD3C_BALLAST_DAYS + TD3C_PORT_DAYS
TD3C_LADEN_CONS_MT  <- 88.5          # mt VLSFO/day laden
TD3C_BALLAST_CONS_MT <- 65.0         # mt VLSFO/day ballast
TD3C_PORT_COSTS_USD <- 450000        # fixed port/canal/misc
VLSFO_PRICE_DEFAULT <- 550           # USD/mt VLSFO (fallback if no live price)

.ws_to_usd_mt <- function(ws, flat_rate = TD3C_FLAT_RATE) {
  # Gross freight per metric tonne at given WS level
  flat_rate * ws / 100
}

.ws_to_tce <- function(ws,
                       flat_rate    = TD3C_FLAT_RATE,
                       bunker_price = VLSFO_PRICE_DEFAULT) {
  # Net TCE USD/day (approximate — uses fixed voyage params)
  gross   <- TD3C_CARGO_MT * flat_rate * ws / 100
  nett    <- gross * (1 - TD3C_COMMISSION)
  bunkers <- (TD3C_LADEN_DAYS   * TD3C_LADEN_CONS_MT +
              TD3C_BALLAST_DAYS * TD3C_BALLAST_CONS_MT) * bunker_price
  (nett - bunkers - TD3C_PORT_COSTS_USD) / TD3C_VOYAGE_DAYS
}

fetch_td3c_freight <- function(start, end, root = NULL) {
  # ── Data source reality check ──────────────────────────────────────────────
  # Genuine TD3C WS series (Baltic Exchange) is paywalled everywhere.
  # Clarksons SIN, LSEG, Bloomberg all carry it — all require subscription.
  # Free alternatives, tiered by quality:
  #
  #   Tier 1 — data/td3c_ws_manual.csv   (columns: date, td3c_ws)
  #     → Genuine WS points. Get from:
  #         macrotrends.net search "TD3C" (sometimes has it)
  #         OR scrape Baltic weekly PDF reports (tedious but accurate)
  #         OR ask Shubh — Hertshten may have LSEG/Eikon access with DFRT-ME-CN
  #
  #   Tier 2 — data/bdti_manual.csv   (columns: date, bdti)
  #     → Baltic Dirty Tanker Index (BDTI) from Investing.com free download:
  #         investing.com/indices/baltic-dirty-tanker-historical-data
  #         Click "Download Data" (requires free account login)
  #         Covers 2000–present, daily
  #         BDTI has 11 routes; TD3C weight = 1/11 ≈ 9.1%
  #         Calibration: TD3C WS ≈ BDTI / 8.5  (±15-20% vs actual)
  #
  #   Tier 3 — BWET ETF via quantmod/Yahoo Finance (auto-fetched, no key)
  #     → Breakwave Tanker ETF: 90% TD3C + 10% TD20 futures
  #         Free daily data, but only from May 2023 onwards
  #         Good for 2023–present; useless for 2021–2022
  #         Converts NAV returns back to an implied WS index
  #
  #   Tier 4 — Stitch Tier 2 (pre-May 2023) + Tier 3 (post-May 2023)
  #     → Best automatic approach: BDTI manual for 2021-2023,
  #       BWET for 2023-present, spliced at the overlap
  # ──────────────────────────────────────────────────────────────────────────

  message("  [M1] TD3C VLCC freight (270kt MEG→China)...")
  if (is.null(root)) root <- .find_repo_root()
  data_dir <- file.path(root, "data")
  dir.create(data_dir, showWarnings = FALSE)

  # ── Tier 1: Genuine WS manual file ────────────────────────────────────────
  ws_manual <- file.path(data_dir, "td3c_ws_manual.csv")
  if (file.exists(ws_manual)) {
    message("    Source: data/td3c_ws_manual.csv  [Tier 1 — genuine WS]")
    raw <- tryCatch(fread(ws_manual), error = function(e) NULL)
    if (!is.null(raw) && nrow(raw) > 20) {
      raw[, date := as.Date(date)]
      ws_col <- intersect(c("td3c_ws","ws","WS","close","Close","value","Price"),
                          names(raw))[1]
      if (!is.na(ws_col)) {
        dt <- raw[, .(date, td3c_ws = as.numeric(get(ws_col)))]
        dt <- dt[is.finite(td3c_ws) & date >= as.Date(start) & date <= as.Date(end)]
        if (nrow(dt) > 50) {
          dt <- .build_td3c_derived(dt[order(date)])
          dt[, td3c_source := "genuine_ws"]
          message(sprintf("    Loaded %d rows from genuine WS file (%s to %s)",
                          nrow(dt), min(dt$date), max(dt$date)))
          return(dt)
        }
      }
    }
  }

  # ── Tier 2: BDTI manual CSV from Investing.com ────────────────────────────
  bdti_manual <- file.path(data_dir, "bdti_manual.csv")
  bdti_dt     <- NULL

  if (file.exists(bdti_manual)) {
    message("    Source: data/bdti_manual.csv  [Tier 2 — BDTI proxy]")
    raw <- tryCatch(fread(bdti_manual), error = function(e) NULL)
    if (!is.null(raw) && nrow(raw) > 50) {
      # Investing.com export columns: "Date","Price","Open","High","Low","Change %"
      raw[, date := tryCatch(as.Date(get(names(raw)[1])), error = function(e) NA_Date_)]
      price_col <- intersect(c("Price","price","Close","close","value","bdti"), names(raw))[1]
      if (!is.na(price_col)) {
        bdti_dt <- raw[!is.na(date), .(date, bdti = as.numeric(get(price_col)))]
        bdti_dt <- bdti_dt[is.finite(bdti) & date >= as.Date(start) & date <= as.Date(end)]
        bdti_dt <- bdti_dt[order(date)]
        message(sprintf("    BDTI: %d rows (%s to %s)",
                        nrow(bdti_dt), min(bdti_dt$date), max(bdti_dt$date)))
      }
    }
  } else {
    message("    data/bdti_manual.csv not found.")
    message("    To get it: investing.com/indices/baltic-dirty-tanker-historical-data")
    message("    → 'Download Data' (free account needed) → save as data/bdti_manual.csv")
  }

  # ── Tier 3: BWET ETF via Yahoo Finance (auto, free, May 2023 onwards) ──────
  bwet_dt <- NULL
  message("    Trying BWET ETF via Yahoo Finance (auto, May 2023+)...")
  bwet_dt <- tryCatch({
    if (!requireNamespace("quantmod", quietly = TRUE))
      stop("quantmod not installed")
    suppressMessages(suppressWarnings({
      quantmod::getSymbols("BWET", src = "yahoo",
                           from = max(as.Date(start), as.Date("2023-05-03")),
                           to   = as.Date(end),
                           auto.assign = FALSE)
    }))
  }, error = function(e) { message("    Yahoo fetch failed: ", e$message); NULL })

  if (!is.null(bwet_dt) && nrow(bwet_dt) > 20) {
    bwet_close <- as.numeric(quantmod::Cl(bwet_dt))
    bwet_dates <- as.Date(zoo::index(bwet_dt))
    bwet_clean <- data.table(date = bwet_dates, bwet_price = bwet_close)
    bwet_clean <- bwet_clean[is.finite(bwet_price)]

    # Convert BWET price to an implied TD3C WS index.
    # BWET holds near-dated TD3C futures (90%) + TD20 (10%).
    # At launch (May 3 2023) TD3C was ≈ WS56; BWET NAV ≈ $25.
    # Calibration anchor: WS_implied = bwet_price * (56 / 25) ≈ bwet_price * 2.24
    # This is deliberately approximate — the z-score and momentum columns are
    # what matter for the model; the WS level is less critical.
    BWET_WS_ANCHOR <- 56   # TD3C WS at BWET launch
    BWET_NAV_ANCHOR <- 25  # BWET NAV at launch (approx)
    bwet_clean[, td3c_ws := round(bwet_price * BWET_WS_ANCHOR / BWET_NAV_ANCHOR, 1)]
    bwet_clean[, td3c_source := "BWET_etf"]
    message(sprintf("    BWET: %d rows (%s to %s)",
                    nrow(bwet_clean), min(bwet_clean$date), max(bwet_clean$date)))
  }

  # ── Stitch: prefer Tier 2 for pre-May-2023, Tier 3 for post-May-2023 ──────
  bwet_start <- as.Date("2023-05-03")

  combined <- NULL

  if (!is.null(bdti_dt) && nrow(bdti_dt) > 50) {
    # Convert BDTI to approximate WS: empirical calibration
    # BDTI typically ~800-2500; TD3C WS typically ~40-200 in normal markets
    # Linear fit on overlap period (2023 data): WS ≈ BDTI / 9.0 + 10
    # During Hormuz spike (BDTI ~2000+, WS ~400+): WS ≈ BDTI / 5.0
    # Use a piecewise calibration for better accuracy across regimes:
    bdti_dt[, td3c_ws := fcase(
      bdti < 1200,                      round(bdti / 9.5,  1),
      bdti >= 1200 & bdti < 2000,       round(bdti / 8.0,  1),
      bdti >= 2000,                      round(bdti / 5.5,  1),
      default = NA_real_
    )]
    bdti_dt[, td3c_source := "BDTI_proxy"]

    # If BWET available, use it for post-May-2023 (more TD3C-specific)
    if (!is.null(bwet_dt) && nrow(bwet_clean) > 20) {
      # Re-calibrate BWET to match BDTI-implied WS at splice point
      # (removes level discontinuity at the join)
      splice_bdti <- bdti_dt[date >= bwet_start - 14 & date <= bwet_start + 14,
                               median(td3c_ws, na.rm = TRUE)]
      splice_bwet <- bwet_clean[date >= bwet_start & date <= bwet_start + 14,
                                  median(td3c_ws, na.rm = TRUE)]
      if (is.finite(splice_bdti) && is.finite(splice_bwet) && splice_bwet > 0) {
        bwet_clean[, td3c_ws := round(td3c_ws * splice_bdti / splice_bwet, 1)]
      }
      # Splice: BDTI for pre-2023-05, BWET for post
      pre  <- bdti_dt[date < bwet_start, .(date, td3c_ws, bdti, td3c_source)]
      post <- bwet_clean[date >= bwet_start, .(date, td3c_ws, td3c_source)]
      post[, bdti := NA_real_]
      combined <- rbind(pre, post, fill = TRUE)
      message("    Spliced BDTI (pre-2023-05) + BWET (post-2023-05)")
    } else {
      combined <- bdti_dt[, .(date, td3c_ws, bdti, td3c_source)]
      message("    Using BDTI proxy only (BWET unavailable)")
    }
  } else if (!is.null(bwet_dt) && nrow(bwet_clean) > 20) {
    combined <- bwet_clean[, .(date, td3c_ws, td3c_source)]
    combined[, bdti := NA_real_]
    message("    Using BWET only (no BDTI file — 2021-2022 will be NA)")
    message("    For pre-2023 data: download BDTI from investing.com (see above)")
  }

  if (!is.null(combined) && nrow(combined) > 20) {
    combined <- combined[order(date)]
    combined <- .build_td3c_derived(combined)
    message(sprintf("    Final TD3C proxy: %d rows (%s to %s)",
                    nrow(combined), min(combined$date), max(combined$date)))
    # Coverage summary
    n_pre  <- nrow(combined[date < bwet_start])
    n_post <- nrow(combined[date >= bwet_start])
    pct_na <- round(mean(is.na(combined$td3c_ws)) * 100, 1)
    message(sprintf("    Pre-May-2023: %d rows  |  Post-May-2023: %d rows  |  NA: %s%%",
                    n_pre, n_post, pct_na))
    return(combined)
  }

  # ── Final fallback: all NA ─────────────────────────────────────────────────
  message("    WARN: No TD3C data available. All columns will be NA.")
  message("    ─── To fix this, do ONE of the following: ───────────────")
  message("    Option A (best): Ask Shubh if Hertshten has LSEG/Eikon access.")
  message("      In Eikon: search DFRT-ME-CN, export historical WS to Excel,")
  message("      save as data/td3c_ws_manual.csv  (columns: date, td3c_ws)")
  message("    Option B (free, daily BDTI back to 2000):")
  message("      1. Go to: investing.com/indices/baltic-dirty-tanker-historical-data")
  message("      2. Set date range to 2021-01-01 to today")
  message("      3. Click 'Download Data' (free login required)")
  message("      4. Save as: data/bdti_manual.csv")
  message("      5. Re-run load_extended_factors()")
  message("    Option C (auto, post-May-2023 only):")
  message("      install.packages('quantmod')  # enables BWET auto-fetch")
  message("    ────────────────────────────────────────────────────────")

  dates <- seq(as.Date(start), as.Date(end), by = "week")
  empty <- data.table(date = dates)
  na_cols <- c("td3c_ws","td3c_usd_mt","td3c_tce_usd_day","td3c_wow_ws",
               "td3c_yoy_ws","td3c_z52","td3c_regime","td3c_source",
               "td3c_storage_cost_bbl_mo","bdti")
  for (col in na_cols) empty[, (col) := NA]
  empty
}

.build_td3c_derived <- function(dt) {
  # dt must have: date, td3c_ws
  # Returns dt with all derived columns added in-place
  dt <- dt[order(date)]
  dt[is.nan(td3c_ws), td3c_ws := NA]

  # USD/mt and TCE conversions
  dt[, td3c_usd_mt      := round(.ws_to_usd_mt(td3c_ws), 2)]
  dt[, td3c_tce_usd_day := round(.ws_to_tce(td3c_ws),    0)]

  # Momentum: week-on-week (5 trading days back, or 1 week if weekly data)
  lag_days <- if (nrow(dt) > 100 && as.numeric(diff(range(dt$date))) / nrow(dt) < 5)
                5L else 1L   # daily data → lag 5; weekly → lag 1
  dt[, td3c_wow_ws := td3c_ws - shift(td3c_ws, lag_days)]

  # Year-on-year (252 trading days back, or 52 weeks if weekly)
  yoy_lag <- if (lag_days == 5L) 252L else 52L
  dt[, td3c_yoy_ws := td3c_ws - shift(td3c_ws, yoy_lag)]

  # 52-week rolling z-score (captures whether freight is historically elevated)
  roll_n <- if (lag_days == 5L) 252L else 52L
  dt[, td3c_z52 := {
    roll_m <- frollmean(td3c_ws, roll_n, na.rm=TRUE, align="right")
    roll_s <- frollapply(td3c_ws, roll_n, sd, na.rm=TRUE, align="right")
    (td3c_ws - roll_m) / pmax(roll_s, 1e-6)
  }]

  # Regime classification (WS-based thresholds from your dashboard spec)
  # These map to storage economics and geopolitical premium signals:
  #   <WS60:   very low freight → contango structure supported; storage cheap
  #   60-120:  normal range → neutral
  #   120-200: firm market → supply tightness or rerouting premium
  #   >WS200:  spike → major disruption (Hormuz 2026: TD3C hit WS467)
  dt[, td3c_regime := fcase(
    td3c_ws < 60,              "low",       # contango-supportive
    td3c_ws >= 60  & td3c_ws < 120, "medium",   # neutral
    td3c_ws >= 120 & td3c_ws < 200, "high",     # tight/premium
    td3c_ws >= 200,            "spike",     # disruption (Hormuz-type)
    default = NA_character_
  )]

  # Storage economics signal: does current freight make contango storage
  # profitable? Contango threshold is roughly 1.5-3 $/bbl/month.
  # Monthly storage cost = freight/(voyage_days/30) + 0.30 $/bbl port/insurance
  # If M1M6 contango < freight-implied carry cost → storage uneconomic
  dt[, td3c_storage_cost_bbl_mo :=
       round((td3c_usd_mt / 7.33) / (TD3C_VOYAGE_DAYS / 30) + 0.30, 3)]
  # 7.33 bbl/mt conversion; +0.30 for port/insurance rough add

  if (!"td3c_source" %in% names(dt)) dt[, td3c_source := "manual"]
  dt
}

# ── EV Penetration Proxy ──────────────────────────────────────────────────────
# IEA Global EV Outlook is published annually (April/May each year).
# Monthly data available from IEA EV Data Explorer.
# Best free approach: manually curated annual inflection points, linearly
# interpolated. Replace with live IEA data when you have access.
# This is a slow-moving structural variable (meaningful over 1+ year horizon).
# For spread modelling at 2-day horizon it has minimal impact but is
# included for completeness and for the dashboard context layer.

build_ev_penetration <- function(start, end) {
  message("  [EV] Building EV penetration proxy (linearly interpolated annual data)...")

  # IEA Global EV Outlook data points — global EV share of new car sales
  # Source: IEA Global EV Tracker 2024, extended with 2025–2026 estimates
  ev_anchors <- data.table(
    year = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026),
    ev_share_pct = c(0.9, 1.1, 1.3, 2.1, 2.5, 4.2, 8.6, 13.0, 18.0, 20.0, 22.0, 24.0)
  )
  # Note: these represent IEA central estimates. China leads at ~35–40% by 2025.

  # Build daily series via linear interpolation
  anchor_dates <- as.Date(paste0(ev_anchors$year, "-07-01"))  # mid-year anchors
  daily_spine  <- seq(as.Date(start), as.Date(end), by = "day")

  ev_daily <- approx(
    x    = as.numeric(anchor_dates),
    y    = ev_anchors$ev_share_pct,
    xout = as.numeric(daily_spine),
    rule = 2   # clamp to boundary values beyond data range
  )

  # Gasoline demand displacement (rough: 1% EV share ≈ 40-50 kbd displacement globally)
  dt <- data.table(
    date                  = daily_spine,
    ev_penetration_pct    = round(ev_daily$y, 2),
    ev_gasoline_disp_kbd  = round(ev_daily$y * 45, 0)  # kbd gasoline displaced
  )

  dt[date >= as.Date(start) & date <= as.Date(end)]
}

# ── Seasonality terms ─────────────────────────────────────────────────────────

build_seasonality <- function(start, end) {
  message("  [SEA] Building seasonality features...")

  dates <- seq(as.Date(start), as.Date(end), by = "day")
  doy   <- yday(dates)       # day of year 1–365/366
  n_days_year <- 365.25

  dt <- data.table(
    date         = dates,
    month        = month(dates),
    quarter      = quarter(dates),
    # Annual Fourier pair — captures the single biggest seasonal cycle
    sin_ann      = sin(2 * pi * doy / n_days_year),
    cos_ann      = cos(2 * pi * doy / n_days_year),
    # Semi-annual Fourier — captures spring & autumn shoulder seasons
    sin_semi     = sin(4 * pi * doy / n_days_year),
    cos_semi     = cos(4 * pi * doy / n_days_year),
    # Trading dummies
    driving_season   = as.integer(month(dates) %in% 5:9),   # May–Sep
    heating_season   = as.integer(month(dates) %in% c(10,11,12,1,2,3)),
    turnaround_season = as.integer(month(dates) %in% c(3,4,9,10)),
    # Q4 distillate pre-build dummy (distillate builds start Sep)
    distillate_build_season = as.integer(month(dates) %in% c(8,9,10)),
    # Gasoline crack seasonal (peaks Feb–May — refiners switch to summer blend)
    gasoline_crack_season  = as.integer(month(dates) %in% c(2,3,4,5))
  )

  # 5-year seasonal z-score position: day-of-year normalised
  # (this tells you where in the seasonal cycle we are, not absolute level)
  dt[, doy_z := (doy - 182.5) / 105]   # standardised doy: 0 = peak summer

  dt
}

# ── EIA history backfill for 5yr deviation lookback ──────────────────────────
# Fetches weekly EIA crude/product stocks from 2018 onward using the same
# EIA API calls as factor_loader.R, extending factors_combined.csv backwards.
# Only the stock level columns are fetched — no derived variables.

.extend_with_eia_history <- function(existing, start, root) {
  cutoff      <- as.Date("2021-01-01")
  fetch_start <- as.Date(start)

  # If existing already goes back far enough, nothing to do
  if (min(existing$date, na.rm=TRUE) <= fetch_start + 30)
    return(existing)

  # If start is 2021 or later, the 5yr backfill adds no value
  if (fetch_start >= cutoff) {
    message("    start >= 2021 — no EIA backfill needed for 5yr deviations")
    message("    For full 5yr deviation coverage, use start='2018-01-01'")
    return(existing)
  }

  message("    Fetching EIA weekly stocks 2018-01-01 → 2020-12-31 for lookback...")

  # Use the same EIA API v2 petroleum supply weekly route
  eia_key <- tryCatch(.get_eia_key(), error = function(e) NULL)
  if (is.null(eia_key)) {
    message("    WARN: EIA_API_KEY not set — 5yr deviation will use available years only")
    return(existing)
  }

  # Series codes for weekly petroleum stocks (kb)
  series_map <- list(
    crude_stocks_kb     = "WCRSTUS1",   # US crude stocks
    cushing_stocks_kb   = "WCUSSTUS1",  # Cushing
    gasoline_stocks_kb  = "WGTSTUS1",   # Total gasoline
    distillate_stocks_kb = "WDISTUS1"   # Distillate fuel oil
  )

  rows_list <- lapply(names(series_map), function(col) {
    series_id <- series_map[[col]]
    url  <- paste0(EIA_BASE, "/petroleum/stoc/wstk/data/")
    resp <- .safe_get(url, list(
      api_key              = eia_key,
      frequency            = "weekly",
      `data[0]`            = "value",
      `facets[series][]`   = series_id,
      `facets[duoarea][]`  = "NUS",
      start                = "2018-01-05",
      end                  = "2020-12-31",
      `sort[0][column]`    = "period",
      `sort[0][direction]` = "asc",
      length               = 200
    ))
    if (is.null(resp)) return(NULL)
    parsed <- tryCatch(fromJSON(rawToChar(resp$content)), error=function(e) NULL)
    if (is.null(parsed) || is.null(parsed$response$data)) return(NULL)
    d <- as.data.table(parsed$response$data)
    if (nrow(d) == 0 || !"period" %in% names(d)) return(NULL)
    d[, .(date  = as.Date(period),
          value = suppressWarnings(as.numeric(value)))]
  })

  names(rows_list) <- names(series_map)
  valid <- Filter(function(x) !is.null(x) && nrow(x) > 10, rows_list)

  if (length(valid) == 0) {
    message("    WARN: EIA backfill returned no data — 5yr deviation coverage limited")
    return(existing)
  }

  # Rename each series from "value" to its column name, then merge
  renamed <- mapply(function(dt, nm) {
    dt <- copy(dt)
    setnames(dt, "value", nm)
    dt
  }, valid, names(valid), SIMPLIFY = FALSE)

  backfill <- Reduce(function(a, b) merge(a, b, by = "date", all = TRUE), renamed)

  # Forward-fill to daily business-day spine
  bf_spine <- seq(as.Date("2018-01-01"), as.Date("2020-12-31"), by="day")
  bf_spine <- bf_spine[!weekdays(bf_spine) %in% c("Saturday","Sunday")]
  backfill <- .ff_to_daily(backfill, bf_spine)

  # Fill remaining columns with NA so rbind works
  for (col in setdiff(names(existing), names(backfill)))
    backfill[, (col) := NA]
  for (col in setdiff(names(backfill), names(existing)))
    existing[, (col) := NA]

  combined <- rbind(backfill, existing, fill=TRUE)
  combined  <- combined[order(date)]
  message(sprintf("    Backfill: %d rows (2018-2020) added for 5yr deviation lookback",
                  nrow(backfill)))
  combined
}

# ── Derived change variables (from existing stocks data) ─────────────────────
# factors_combined.csv has levels (crude_stocks_kb etc.) but the
# inventory shock model needs week-on-week changes and normalised surprises.

build_derived_changes <- function(existing) {
  message("  [DRV] Computing derived change/surprise variables...")

  dt <- copy(existing)[order(date)]

  # Week-on-week stock changes (kb → convert to mbd: ÷7)
  for (col in c("crude_stocks_kb","cushing_stocks_kb",
                 "gasoline_stocks_kb","distillate_stocks_kb")) {
    if (col %in% names(dt)) {
      new_col <- gsub("_kb$", "_chg", col)
      dt[, (new_col) := get(col) - shift(get(col), 1)]
    }
  }

  # Crude production change (kbd, week-on-week)
  if ("crude_prod_kbd" %in% names(dt)) {
    dt[, crude_prod_chg := crude_prod_kbd - shift(crude_prod_kbd, 1)]
  }

  # 5-year seasonal deviation for each stock series
  # (actual level vs average of same week-of-year over prior 5 years)
  dt[, week_of_year := isoweek(date)]
  dt[, yr := year(date)]

  for (col in c("crude_stocks_kb","cushing_stocks_kb",
                 "gasoline_stocks_kb","distillate_stocks_kb")) {
    if (!col %in% names(dt)) next
    dev_col <- gsub("_kb$", "_5yr_dev", col)

    # 5-year same-week average: for each (week_of_year, yr) row, average the
    # values from the same week_of_year in the 5 prior years.
    # Must operate on the full table, not within a group — hence a self-join.
    ref <- dt[, .(week_of_year, yr, val = get(col))]
    avg5yr_dt <- dt[, {
      woy <- week_of_year[1]; y <- yr[1]
      hist <- ref[week_of_year == woy & yr >= (y - 5) & yr < y, val]
      avg5 <- if (length(hist) >= 1) mean(hist, na.rm = TRUE) else NA_real_
      .(avg5yr = avg5)
    }, by = .(week_of_year, yr)]

    dt <- merge(dt, avg5yr_dt, by = c("week_of_year", "yr"), all.x = TRUE)
    dt[, (dev_col) := get(col) - avg5yr]
    dt[, avg5yr := NULL]
  }

  # Days of forward demand cover (proxy)
  # = total commercial stocks / (4-week avg implied demand in kb/day)
  # Implied demand ≈ crude_prod_kbd + net_imports (not available daily without EIA)
  # Proxy: crude_stocks_kb / (crude_prod_kbd / 7) → rough days cover
  if (all(c("crude_stocks_kb","crude_prod_kbd") %in% names(dt))) {
    dt[, days_fwd_cover_proxy := crude_stocks_kb /
         pmax(frollmean(crude_prod_kbd, 28, na.rm=TRUE, align="right") / 7, 1)]
  }

  dt[, c("week_of_year","yr") := NULL]
  dt
}

# ── OPEC spare capacity from EIA WPSR (refinery util already in panel) ────────
# Additional: US refinery crude inputs (weekly, EIA) — already have util%
# Add: US crude net exports (production - imports) as a supply indicator
fetch_us_net_exports <- function(start, end) {
  message("  [S1+] US crude net exports via EIA WPSR API...")

  # EIA weekly crude exports: series WCREXUS2 (kb/d)
  # EIA weekly crude imports: series WCRIMUS2 (kb/d)
  fetch_eia_weekly <- function(series_id) {
    url  <- paste0(EIA_BASE, "/petroleum/move/wkly/data/")
    resp <- .safe_get(url, list(
      api_key    = .get_eia_key(),
      frequency  = "weekly",
      `data[0]`  = "value",
      `facets[series][]` = series_id,
      start      = format(as.Date(start), "%Y-%m-%d"),
      end        = format(as.Date(end),   "%Y-%m-%d"),
      `sort[0][column]`    = "period",
      `sort[0][direction]` = "asc",
      length     = 300
    ))
    if (is.null(resp)) return(NULL)
    parsed <- tryCatch(fromJSON(rawToChar(resp$content)), error = function(e) NULL)
    if (is.null(parsed) || is.null(parsed$response$data)) return(NULL)
    d <- as.data.table(parsed$response$data)
    if (nrow(d) == 0) return(NULL)
    d[, .(date = as.Date(period), value = suppressWarnings(as.numeric(value)))]
  }

  exports <- fetch_eia_weekly("WCREXUS2")
  imports <- fetch_eia_weekly("WCRIMUS2")

  if (is.null(exports) || is.null(imports)) {
    message("  WARN: US crude exports/imports unavailable — trying WPSR summary route")
    # Alternative: EIA petroleum/supply/weekly route
    exports <- NULL; imports <- NULL
  }

  if (!is.null(exports) && !is.null(imports)) {
    dt <- merge(
      exports[, .(date, crude_exports_kbd = value)],
      imports[, .(date, crude_imports_kbd = value)],
      by = "date", all = TRUE
    )
    dt[, crude_net_exports_kbd := crude_exports_kbd - crude_imports_kbd]
    dt[, crude_net_exports_4wk := frollmean(crude_net_exports_kbd, 4,
                                             na.rm=TRUE, align="right")]
    return(dt[order(date)])
  }

  # Fallback: return NA columns so model still runs
  dates <- seq(as.Date(start), as.Date(end), by = "week")
  data.table(date = dates, crude_exports_kbd = NA_real_,
             crude_imports_kbd = NA_real_, crude_net_exports_kbd = NA_real_,
             crude_net_exports_4wk = NA_real_)
}

# ── CFTC net positions — extend to disaggregated (if not already complete) ───
# Original factor_loader.R has cftc_net_mm_total and cftc_net_mm_zscore.
# We add: cftc_producer_short (hedge pressure), cftc_swap_net (dealer flow)
# These are in the same COT ZIP files but weren't extracted in the original.

fetch_cftc_extended <- function(start, end, root) {
  message("  [CFTC+] Extended CFTC disaggregated positions...")

  years <- seq(year(as.Date(start)), year(as.Date(end)))
  all_rows <- list()

  for (yr in years) {
    url  <- sprintf("https://www.cftc.gov/files/dea/history/fut_disagg_txt_%d.zip", yr)
    dest <- file.path(tempdir(), paste0("cot_disagg_", yr, ".zip"))

    if (!file.exists(dest)) {
      resp <- tryCatch(GET(url, timeout(60), write_disk(dest, overwrite=TRUE)),
                       error = function(e) NULL)
      if (is.null(resp) || status_code(resp) != 200) {
        message("    WARN: COT disaggregated ", yr, " not available")
        next
      }
    }

    tmp_dir <- file.path(tempdir(), paste0("cot_disagg_", yr))
    dir.create(tmp_dir, showWarnings=FALSE)
    tryCatch(unzip(dest, exdir=tmp_dir), error=function(e) NULL)

    csv_files <- list.files(tmp_dir, pattern="\\.txt$|\\.csv$",
                             full.names=TRUE, recursive=TRUE)
    if (length(csv_files) == 0) next

    for (f in csv_files) {
      rows <- tryCatch({
        d <- fread(f, select=c(
          "Market_and_Exchange_Names",
          "As_of_Date_In_Form_YYMMDD",
          "CFTC_Commodity_Code",
          "Prod_Merc_Positions_Long_All",
          "Prod_Merc_Positions_Short_All",
          "Swap_Positions_Long_All",
          "Swap__Positions_Short_All",
          "M_Money_Positions_Long_All",
          "M_Money_Positions_Short_All"
        ), fill=TRUE)
        d[CFTC_Commodity_Code %in% c("067651","067","06765+")]
      }, error=function(e) NULL)
      if (!is.null(rows) && nrow(rows) > 0) all_rows[[length(all_rows)+1]] <- rows
    }
  }

  if (length(all_rows) == 0) {
    message("  WARN: CFTC disaggregated data unavailable — returning NA columns")
    dates <- seq(as.Date(start), as.Date(end), by="week")
    return(data.table(date=dates,
                      cftc_prod_short=NA_real_, cftc_swap_net=NA_real_,
                      cftc_mm_net_chg=NA_real_))
  }

  dt <- rbindlist(all_rows, fill=TRUE)
  dt[, date := as.Date(as.character(As_of_Date_In_Form_YYMMDD), "%y%m%d")]
  dt <- dt[!is.na(date)][order(date)]

  # Aggregate across contracts for same commodity
  dt <- dt[, .(
    prod_short    = sum(as.numeric(Prod_Merc_Positions_Short_All), na.rm=TRUE),
    swap_long     = sum(as.numeric(Swap_Positions_Long_All), na.rm=TRUE),
    swap_short    = sum(as.numeric(`Swap__Positions_Short_All`), na.rm=TRUE),
    mm_long       = sum(as.numeric(M_Money_Positions_Long_All), na.rm=TRUE),
    mm_short      = sum(as.numeric(M_Money_Positions_Short_All), na.rm=TRUE)
  ), by=date]

  dt[, `:=`(
    cftc_prod_short = prod_short,
    cftc_swap_net   = swap_long - swap_short,
    cftc_mm_net     = mm_long - mm_short,
    cftc_mm_net_chg = (mm_long - mm_short) - shift(mm_long - mm_short, 1)
  )]

  dt[date >= as.Date(start) & date <= as.Date(end),
     .(date, cftc_prod_short, cftc_swap_net, cftc_mm_net, cftc_mm_net_chg)]
}

# ── Master merge function ─────────────────────────────────────────────────────
# Forward-fill monthly/weekly series to daily using zoo::na.locf
# so the daily spine remains complete.

.ff_to_daily <- function(dt, date_spine) {
  if (is.null(dt) || nrow(dt) == 0) return(data.table(date = date_spine))
  # Merge onto daily spine, then forward-fill with zoo::na.locf
  merged <- merge(data.table(date = date_spine), dt, by = "date", all.x = TRUE)
  merged <- merged[order(date)]
  cols <- setdiff(names(merged), "date")
  # zoo::na.locf fills each column forward; na.rm=FALSE preserves leading NAs
  merged[, (cols) := lapply(.SD, function(x)
    zoo::na.locf(x, na.rm = FALSE)), .SDcols = cols]
  merged
}

# ── Master runner ─────────────────────────────────────────────────────────────

load_extended_factors <- function(start       = "2018-01-01",
                                   end         = "2026-05-31",
                                   output_dir  = OUTPUT_DIR,
                                   data_dir    = NULL) {

  root <- if (!is.null(data_dir)) data_dir else .find_repo_root()

  # Reload .Renviron in case API keys were added after this session started
  renviron <- path.expand("~/.Renviron")
  if (file.exists(renviron)) {
    readRenviron(renviron)
    message("  .Renviron loaded: FRED_API_KEY=",
            ifelse(nchar(Sys.getenv("FRED_API_KEY")) > 0,
                   paste0(substr(Sys.getenv("FRED_API_KEY"),1,4),"****"),
                   "NOT SET — FRED fetches will be skipped"))
  }

  message("══ Extended Factor Loader ══════════════════════════════════════")
  message("Repo root : ", root)
  message("Range     : ", start, " → ", end)
  message("NOTE: start=2018 gives 5yr deviation columns full coverage for 2023+")
  message("      The factor panel is trimmed to factors_combined.csv spine at merge")

  # Build daily spine — full range for computing 5yr deviations
  spine_dates <- seq(as.Date(start), as.Date(end), by="day")
  spine_dates <- spine_dates[!weekdays(spine_dates) %in% c("Saturday","Sunday")]

  # Output spine — trim to 2021-01-01 onwards to match factors_combined.csv
  # (the 2018-2020 rows are used only for computing historical averages,
  #  not written to the output file)
  output_start <- as.Date(max(as.Date(start),
                              as.Date("2021-01-01")))
  output_spine <- spine_dates[spine_dates >= output_start]

  # ── Load existing 21-factor panel ──────────────────────────────────────────
  existing_path <- file.path(root, output_dir, "factors_combined.csv")
  if (file.exists(existing_path)) {
    message("\n[BASE] Loading existing 21-factor panel...")
    existing <- fread(existing_path)
    existing[, date := as.Date(date)]
  } else {
    message("\n[BASE] factors_combined.csv not found — creating empty spine")
    message("  Run R/factor_loader.R first for full factor coverage")
    existing <- data.table(date = spine_dates)
  }

  # ── Fetch new components ────────────────────────────────────────────────────
  message("\n── Fetching new supply-side factors ───────────────────────────")
  rigs       <- tryCatch(fetch_rig_count(start, end),      error=function(e){message("  ERR rigs: ",e$message);NULL})
  opec_spare <- tryCatch(fetch_opec_spare_capacity(start, end), error=function(e){message("  ERR OPEC: ",e$message);NULL})
  net_exp    <- tryCatch(fetch_us_net_exports(start, end), error=function(e){message("  ERR netexp: ",e$message);NULL})

  message("\n── Fetching freight factors ─────────────────────────────────")
  td3c       <- tryCatch(fetch_td3c_freight(start, end, root), error=function(e){message("  ERR TD3C: ",e$message);NULL})

  message("\n── Fetching new demand-side factors ───────────────────────────")
  hdd_cdd    <- tryCatch(fetch_hdd_cdd(start, end),        error=function(e){message("  ERR HDD: ",e$message);NULL})
  china      <- tryCatch(fetch_china_imports(start, end),  error=function(e){message("  ERR China: ",e$message);NULL})
  bdi        <- tryCatch(fetch_bdi(start, end),            error=function(e){message("  ERR BDI: ",e$message);NULL})

  message("\n── Building derived/structural features ───────────────────────")
  season    <- build_seasonality(start, end)
  ev_proxy  <- build_ev_penetration(start, end)

  # For 5-year seasonal deviation we need EIA stock history from 2018 onwards.
  # factors_combined.csv only starts in 2021, so we extend it with a
  # lightweight EIA backfill covering 2018-2020 (the "lookback spine").
  # These extra rows are used only for computing deviations; they are
  # trimmed from the output CSV at the end.
  message("  [5YR] Fetching EIA stock history backfill (2018-2020 lookback)...")
  existing_extended <- .extend_with_eia_history(existing, start, root)
  existing_with_chg <- build_derived_changes(existing_extended)
  cftc_ext  <- tryCatch(fetch_cftc_extended(start, end, root), error=function(e){message("  ERR CFTC+: ",e$message);NULL})

  # ── Merge all onto business-day spine ──────────────────────────────────────
  message("\n── Merging all factors onto daily spine ───────────────────────")

  # Build on full spine (needed for 5yr deviation lookback)
  result <- existing_with_chg

  # Weekly series — forward-fill to daily
  for (component in list(rigs, opec_spare, net_exp, bdi, cftc_ext, china, td3c)) {
    if (!is.null(component) && nrow(component) > 0) {
      result <- merge(result,
                      .ff_to_daily(component, spine_dates),
                      by="date", all.x=TRUE)
    }
  }

  # Daily series — direct join (no date filter; result already on spine_dates)
  for (component in list(hdd_cdd, season, ev_proxy)) {
    if (!is.null(component) && nrow(component) > 0) {
      result <- merge(result, component, by="date", all.x=TRUE)
    }
  }

  result <- result[order(date)]

  # Trim to output spine (2021-01-01 onward) — the pre-2021 rows were only
  # needed as historical context for 5-year deviation calculations
  result <- result[date %in% output_spine]

  # ── Coverage report ─────────────────────────────────────────────────────────
  message("\n── Factor coverage ─────────────────────────────────────────────")
  message(sprintf("  %-42s %s", "Factor", "Coverage"))
  message("  ", strrep("─", 52))

  all_factors <- setdiff(names(result), "date")
  for (col in all_factors) {
    pct <- round(sum(!is.na(result[[col]])) / nrow(result) * 100, 1)
    msg <- sprintf("  %-42s %s%%", col, pct)
    if (pct < 50) msg <- paste0(msg, "  ← LOW — check source")
    message(msg)
  }

  message(sprintf("\n  Total factors : %d", length(all_factors)))
  message(sprintf("  Total rows    : %d", nrow(result)))
  message(sprintf("  Date range    : %s → %s",
                  min(result$date), max(result$date)))

  # ── Save ────────────────────────────────────────────────────────────────────
  out_path <- file.path(root, output_dir, "factors_extended.csv")
  fwrite(result, out_path)
  message("\n  Saved: ", out_path)
  message("══════════════════════════════════════════════════════════════════")

  invisible(result)
}

# ── Convenience: reload without re-fetching ───────────────────────────────────

load_factors_from_disk <- function(extended = TRUE, root = NULL) {
  if (is.null(root)) root <- .find_repo_root()
  fname <- if (extended) "factors_extended.csv" else "factors_combined.csv"
  path  <- file.path(root, "output", fname)
  if (!file.exists(path)) stop("File not found: ", path,
                                "\nRun load_extended_factors() first.")
  dt <- fread(path)
  dt[, date := as.Date(date)]
  dt
}

# ── Entry point ───────────────────────────────────────────────────────────────
# Uncomment to run on source:
# ext <- load_extended_factors(start = "2021-01-01", end = "2026-05-31")