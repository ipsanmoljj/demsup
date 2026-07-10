setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
library(httr); library(rvest); library(jsonlite); library(data.table)

UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"
BASE <- "https://tradingeconomics.com"
SYM  <- "UNITEDSTACRUOILSTOCH"

do_get <- function(url, sleep=1.2, raw=FALSE) {
  Sys.sleep(sleep)
  r <- tryCatch(GET(url, add_headers(
    "User-Agent"="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
    "Accept"="text/html,application/xhtml+xml,application/json,*/*;q=0.8",
    "Accept-Language"="en-US,en;q=0.9",
    "Referer"=BASE,
    "X-Requested-With"="XMLHttpRequest"
  ), timeout(25)), error=function(e) NULL)
  if (raw) return(r)
  if (is.null(r)) { cat("  NULL response\n"); return(NULL) }
  cat("  Status:", status_code(r), "  Bytes:", length(r$content), "\n")
  txt <- tryCatch(rawToChar(r$content), error=function(e) "")
  cat("  First 300 chars:", substr(txt, 1, 300), "\n")
  invisible(r)
}

# ── 1. TradingEconomics calendar with country/indicator filter ─────────────────
cat("=== 1. TE Calendar with EIA filter ===\n")
urls <- c(
  paste0(BASE, "/calendar?i=crude-oil-stocks-change&c=united-states"),
  paste0(BASE, "/calendar#united-states"),
  paste0(BASE, "/calendar?i=", SYM)
)
for (u in urls) {
  cat("  URL:", substr(u,1,100), "\n")
  r <- do_get(u)
}

# ── 2. Try the download/export endpoint ───────────────────────────────────────
cat("\n=== 2. TE Download endpoints ===\n")
dl_urls <- c(
  paste0(BASE, "/united-states/crude-oil-stocks-change?format=csv"),
  paste0(BASE, "/united-states/crude-oil-stocks-change?format=json"),
  paste0(BASE, "/indicators/download.aspx?url=united-states/crude-oil-stocks-change&format=csv"),
  paste0(BASE, "/indicators/download.aspx?symbol=", SYM, "&format=json")
)
for (u in dl_urls) {
  cat("  URL:", substr(u,1,100), "\n")
  r <- do_get(u)
}

# ── 3. TradingEconomics internal AJAX data endpoint ──────────────────────────
cat("\n=== 3. TE AJAX chart/data endpoints ===\n")
# These are common TE internal endpoints
ajax_urls <- c(
  paste0(BASE, "/embeds/chart/?s=", SYM, "&d1=2021-01-01&d2=2026-06-30"),
  paste0(BASE, "/charts/", tolower(SYM), ".png?d1=2021-01-01&d2=2026-06-30&v1=1"),
  paste0("https://d3fy651gv2fhd3.cloudfront.net/charts/united-states-crude-oil-stocks-change.png?s=", tolower(SYM), "&d1=2021-01-01&d2=2026-06-30"),
  paste0(BASE, "/api/historical/?symbol=", SYM, "&d1=2021-01-01&d2=2026-06-30"),
  paste0(BASE, "/data/historical/?s=", SYM, "&d1=20210101&d2=20261231")
)
for (u in ajax_urls) {
  cat("  URL:", substr(u,1,100), "\n")
  r <- do_get(u)
}

# ── 4. Calendar page - look for AJAX request that loads calendar events ────────
cat("\n=== 4. TE calendar AJAX (common patterns) ===\n")
cal_ajax <- c(
  paste0(BASE, "/calendar/download.aspx?d1=2021-01-01&d2=2022-01-01&category=crude-oil-stocks-change&country=united+states"),
  paste0(BASE, "/api/calendar.aspx?d1=2021-01-01&d2=2022-01-01&indicator=crude-oil-stocks-change"),
  paste0(BASE, "/calendar/country/United+States/indicator/Crude+Oil+Stocks+Change?d1=2021-01-01&d2=2022-01-01")
)
for (u in cal_ajax) {
  cat("  URL:", substr(u,1,100), "\n")
  r <- do_get(u)
}

cat("\n=== DONE ===\n")
