# R/factor_loader_consensus.R
# ─────────────────────────────────────────────────────────────────────────────
# Fetches EIA Crude Oil Inventories: actual vs analyst consensus (forecast)
# Source: Forex Factory economic calendar (USD events)
#
# Returns data.table with columns:
#   date             <Date>  EIA release date (Wednesday)
#   actual_mbbls     <num>   actual inventory change (Mbbls, draw = negative)
#   forecast_mbbls   <num>   analyst consensus (Mbbls)
#   previous_mbbls   <num>   prior week actual (Mbbls)
#   surprise_mbbls   <num>   actual - forecast (negative = bullish)
#   actual_kb / forecast_kb / surprise_kb  <num>  same in thousand barrels
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(data.table)
  library(lubridate)
  library(zoo)
})

.FF_UA  <- paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
                  "(KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36")
.FF_BASE <- "https://www.forexfactory.com"

# ── Helpers ───────────────────────────────────────────────────────────────────
.ff_parse_val <- function(x) {
  x <- trimws(gsub("−", "-", as.character(x)))  # unicode minus
  if (is.na(x) || x %in% c("", "-", "N/A", "/", " ")) return(NA_real_)
  neg  <- startsWith(x, "-")
  x    <- sub("^-", "", x)
  mult <- if (grepl("[Bb]$", x)) 1e3 else
          if (grepl("[Mm]$", x)) 1.0  else
          if (grepl("[Kk]$", x)) 1e-3 else 1.0
  val  <- suppressWarnings(as.numeric(gsub("[BMKbmk]$", "", trimws(x)))) * mult
  if (!is.finite(val)) return(NA_real_)
  if (neg) -val else val
}

.ff_get <- function(url, sleep = 1.2) {
  Sys.sleep(sleep)
  tryCatch(
    GET(url,
        add_headers("User-Agent"      = .FF_UA,
                    "Accept"          = "text/html,application/xhtml+xml,*/*;q=0.8",
                    "Accept-Language" = "en-US,en;q=0.9",
                    "Referer"         = .FF_BASE),
        timeout(25)),
    error = function(e) NULL
  )
}

# Build the list of Forex Factory week URLs between start and end
.ff_week_urls <- function(start_dt, end_dt) {
  # FF uses Monday as week start; URL format: calendar?week=jan1.2023
  mondays <- seq(floor_date(start_dt, "week", week_start = 1),
                 floor_date(end_dt,   "week", week_start = 1),
                 by = "week")
  month_abbr <- tolower(month.abb)
  urls <- vapply(mondays, function(d) {
    paste0(.FF_BASE, "/calendar?week=",
           month_abbr[month(d)], day(d), ".", year(d))
  }, character(1))
  unique(urls)
}

# Parse one week's FF calendar page → data.table of EIA crude events
.ff_parse_week <- function(html) {
  # FF calendar table: date col may be blank (carry-forward within week)
  rows <- html_nodes(html, "tr.calendar__row")
  if (!length(rows)) return(NULL)

  results <- lapply(rows, function(row) {
    # Only keep rows tagged as USD
    currency <- trimws(html_text(html_nodes(row, ".calendar__currency")))
    if (!length(currency) || !grepl("^USD$", currency, ignore.case=TRUE))
      return(NULL)

    event_text <- trimws(html_text(html_nodes(row, ".calendar__event-title")))
    if (!length(event_text)) return(NULL)
    # Must contain "Crude Oil Inventories" (and NOT gasoline, distillate, etc)
    if (!grepl("crude oil inv", event_text, ignore.case=TRUE)) return(NULL)
    if (grepl("gasoline|distillate|cushing|refiner", event_text, ignore.case=TRUE))
      return(NULL)

    date_text  <- trimws(html_text(html_nodes(row, ".calendar__date")))
    actual_txt <- trimws(html_text(html_nodes(row, ".calendar__actual")))
    fore_txt   <- trimws(html_text(html_nodes(row, ".calendar__forecast")))
    prev_txt   <- trimws(html_text(html_nodes(row, ".calendar__previous")))

    list(
      date_str     = if (length(date_text))  date_text[1]  else NA_character_,
      event        = event_text[1],
      actual_raw   = if (length(actual_txt)) actual_txt[1] else NA_character_,
      forecast_raw = if (length(fore_txt))   fore_txt[1]   else NA_character_,
      previous_raw = if (length(prev_txt))   prev_txt[1]   else NA_character_
    )
  })
  results <- Filter(Negate(is.null), results)
  if (!length(results)) return(NULL)
  rbindlist(lapply(results, as.data.table), fill = TRUE)
}

# Resolve dates: FF date column shows "Wed Jan 11" style, year from URL
.ff_resolve_dates <- function(dt, week_url) {
  # Extract year from URL  …jan1.2023
  yr <- as.integer(sub(".*\\.(\\d{4})$", "\\1", week_url))

  dt[, date_str := trimws(date_str)]
  dt[date_str == "" | is.na(date_str), date_str := NA_character_]
  dt[, date_str := na.locf(date_str, na.rm = FALSE)]

  dt[, date := {
    sapply(date_str, function(s) {
      if (is.na(s)) return(NA_real_)
      # Try "Wed Jan 11" or "Jan 11" or just "11"
      d <- suppressWarnings(
        tryCatch(as.Date(paste(s, yr), format = "%a %b %d %Y"), error=function(e) NA_Date_))
      if (!is.na(d)) return(as.numeric(d))
      d2 <- suppressWarnings(
        tryCatch(as.Date(paste(s, yr), format = "%b %d %Y"), error=function(e) NA_Date_))
      if (!is.na(d2)) return(as.numeric(d2))
      NA_real_
    })
  }]
  dt[, date := as.Date(date, origin="1970-01-01")]
  dt
}

# ── Master loader ─────────────────────────────────────────────────────────────
load_eia_consensus <- function(start      = "2021-01-01",
                                end        = Sys.Date(),
                                output_dir = NULL,
                                save       = TRUE,
                                verbose    = FALSE) {

  start_dt <- as.Date(start)
  end_dt   <- as.Date(end)

  message("Fetching EIA consensus from Forex Factory (",
          start_dt, " to ", end_dt, ")...")

  urls <- .ff_week_urls(start_dt, end_dt)
  message("  Weeks to fetch: ", length(urls))

  all_chunks <- list()
  n_ok <- 0L; n_fail <- 0L

  for (i in seq_along(urls)) {
    url <- urls[i]
    if (verbose) message("  [", i, "/", length(urls), "] ", url)

    resp <- .ff_get(url, sleep = if (i == 1) 0.3 else 1.1)
    if (is.null(resp) || status_code(resp) != 200) {
      n_fail <- n_fail + 1L
      if (verbose) message("    FAIL (status=",
                           if (!is.null(resp)) status_code(resp) else "NULL", ")")
      next
    }

    html <- tryCatch(read_html(rawToChar(resp$content)), error=function(e) NULL)
    if (is.null(html)) { n_fail <- n_fail + 1L; next }

    chunk <- tryCatch(.ff_parse_week(html), error=function(e) NULL)
    if (!is.null(chunk) && nrow(chunk)) {
      chunk <- .ff_resolve_dates(chunk, url)
      chunk <- chunk[!is.na(date) & date >= start_dt & date <= end_dt]
      if (nrow(chunk)) {
        all_chunks[[length(all_chunks) + 1]] <- chunk
        n_ok <- n_ok + nrow(chunk)
        if (verbose) message("    -> ", nrow(chunk), " EIA crude rows")
      }
    }

    # Progress ping every 10 weeks
    if (i %% 10 == 0)
      message("  ... ", i, "/", length(urls), " weeks done | rows so far: ", n_ok)
  }

  message("  Fetched: ", n_ok, " rows across ", length(urls), " weeks  (",
          n_fail, " failed)")

  if (!n_ok) {
    message("  WARNING: No data fetched.")
    return(data.table(date=as.Date(character(0)), actual_mbbls=numeric(0),
                      forecast_mbbls=numeric(0), previous_mbbls=numeric(0),
                      surprise_mbbls=numeric(0), actual_kb=numeric(0),
                      forecast_kb=numeric(0), surprise_kb=numeric(0)))
  }

  raw <- unique(rbindlist(all_chunks, fill=TRUE), by="date")[order(date)]

  raw[, actual_mbbls   := sapply(actual_raw,   .ff_parse_val)]
  raw[, forecast_mbbls := sapply(forecast_raw, .ff_parse_val)]
  raw[, previous_mbbls := sapply(previous_raw, .ff_parse_val)]

  final <- raw[!is.na(actual_mbbls)]

  final[, surprise_mbbls := actual_mbbls - forecast_mbbls]
  final[, actual_kb      := actual_mbbls   * 1000]
  final[, forecast_kb    := forecast_mbbls * 1000]
  final[, surprise_kb    := surprise_mbbls * 1000]

  final <- final[, .(date, actual_mbbls, forecast_mbbls, previous_mbbls,
                     surprise_mbbls, actual_kb, forecast_kb, surprise_kb)]

  message("  Total rows: ", nrow(final),
          "  (", min(final$date), " to ", max(final$date), ")")
  msg_s <- round(mean(final$surprise_mbbls, na.rm=TRUE), 2)
  msg_sd <- round(sd(final$surprise_mbbls, na.rm=TRUE), 2)
  message("  Surprise: mean=", msg_s, " M bbl  sd=", msg_sd, " M bbl")

  if (save && !is.null(output_dir)) {
    path <- file.path(output_dir, "eia_consensus.csv")
    if (file.exists(path)) {
      existing <- fread(path)
      existing[, date := as.Date(date)]
      final <- unique(rbind(existing, final), by="date")[order(date)]
    }
    fwrite(final, path)
    message("  Saved: ", path)
  }

  final
}
