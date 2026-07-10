setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")

for (pkg in c("httr","rvest","data.table","lubridate","zoo"))
  if (!requireNamespace(pkg, quietly=TRUE))
    install.packages(pkg, repos="https://cloud.r-project.org", quiet=TRUE)

library(httr); library(rvest); library(data.table); library(lubridate); library(zoo)

UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"
FF <- "https://www.forexfactory.com"

# ── Test 2 specific weeks known to have EIA releases ─────────────────────────
test_urls <- c(
  paste0(FF, "/calendar?week=jun18.2026"),   # should have Jun 24 2026 release
  paste0(FF, "/calendar?week=jan4.2021"),    # first week of 2021
  paste0(FF, "/calendar?week=jun21.2023")    # mid-2023
)

for (url in test_urls) {
  cat("\n=== URL:", url, "===\n")
  resp <- tryCatch(
    GET(url, add_headers(
      "User-Agent"      = UA,
      "Accept"          = "text/html,application/xhtml+xml,*/*;q=0.8",
      "Accept-Language" = "en-US,en;q=0.9",
      "Referer"         = FF
    ), timeout(25)),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )

  if (is.null(resp)) { cat("  NULL response\n"); next }
  cat("  Status:", status_code(resp), "  Bytes:", length(resp$content), "\n")

  if (status_code(resp) != 200) {
    cat("  Body (first 300):", substr(rawToChar(resp$content), 1, 300), "\n")
    next
  }

  html <- tryCatch(read_html(rawToChar(resp$content)), error=function(e) NULL)
  if (is.null(html)) { cat("  Failed to parse HTML\n"); next }

  # How many calendar rows total?
  all_rows <- html_nodes(html, "tr.calendar__row")
  cat("  Total calendar__row elements:", length(all_rows), "\n")

  # Find USD rows
  usd_count <- 0L
  crude_rows <- list()

  for (row in all_rows) {
    curr <- trimws(html_text(html_nodes(row, ".calendar__currency")))
    if (!length(curr) || !grepl("^USD$", curr)) next
    usd_count <- usd_count + 1L

    ev <- trimws(html_text(html_nodes(row, ".calendar__event-title")))
    if (!length(ev)) next
    if (!grepl("crude oil inv", ev, ignore.case=TRUE)) next
    if (grepl("gasoline|distillate|cushing|refiner", ev, ignore.case=TRUE)) next

    date_txt <- trimws(html_text(html_nodes(row, ".calendar__date")))
    act_txt  <- trimws(html_text(html_nodes(row, ".calendar__actual")))
    fore_txt <- trimws(html_text(html_nodes(row, ".calendar__forecast")))
    prev_txt <- trimws(html_text(html_nodes(row, ".calendar__previous")))

    crude_rows[[length(crude_rows)+1]] <- list(
      date     = if(length(date_txt)) date_txt[1] else "",
      event    = ev[1],
      actual   = if(length(act_txt))  act_txt[1]  else "",
      forecast = if(length(fore_txt)) fore_txt[1] else "",
      previous = if(length(prev_txt)) prev_txt[1] else ""
    )
  }

  cat("  USD rows:", usd_count, "\n")
  cat("  EIA crude rows found:", length(crude_rows), "\n")
  if (length(crude_rows)) {
    for (r in crude_rows) {
      cat(sprintf("    date=%-12s  actual=%-8s  forecast=%-8s  prev=%-8s  event=%s\n",
                  r$date, r$actual, r$forecast, r$previous, r$event))
    }
  } else {
    # Try broader search - show all USD events
    cat("  All USD events found (sample):\n")
    n <- 0L
    for (row in all_rows) {
      curr <- trimws(html_text(html_nodes(row, ".calendar__currency")))
      if (!length(curr) || !grepl("^USD$", curr)) next
      ev <- trimws(html_text(html_nodes(row, ".calendar__event-title")))
      if (!length(ev) || nchar(trimws(ev[1])) == 0) next
      cat("    USD:", ev[1], "\n")
      n <- n + 1L
      if (n > 10) { cat("    ...(truncated)\n"); break }
    }
  }

  Sys.sleep(1.5)
}

cat("\n=== DONE ===\n")
