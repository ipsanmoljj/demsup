setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
library(httr); library(rvest); library(jsonlite); library(data.table)

UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"
BASE <- "https://tradingeconomics.com"

do_get <- function(url, sleep=1.2) {
  Sys.sleep(sleep)
  tryCatch(GET(url, add_headers(
    "User-Agent"="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
    "Accept"="text/html,application/xhtml+xml,*/*;q=0.8",
    "Accept-Language"="en-US,en;q=0.9",
    "Referer"=BASE
  ), timeout(25)), error=function(e) NULL)
}

# ── 1. Read full main page and look for embedded JSON / additional tables ──────
cat("=== 1. Main page full parse ===\n")
r1 <- do_get(paste0(BASE, "/united-states/crude-oil-stocks-change"), sleep=0.3)
if (!is.null(r1) && status_code(r1) == 200) {
  html <- read_html(rawToChar(r1$content))
  tbls <- html_table(html_nodes(html, "table"), fill=TRUE)
  cat("  Tables found:", length(tbls), "\n")
  for (i in seq_along(tbls)) {
    cat("  Table", i, "rows:", nrow(tbls[[i]]), " cols:", ncol(tbls[[i]]), "\n")
    print(head(tbls[[i]], 6))
  }

  # Look for JSON in script tags
  scripts <- html_nodes(html, "script")
  cat("\n  Script tags:", length(scripts), "\n")
  for (sc in scripts) {
    txt <- html_text(sc)
    if (grepl("Crude|crude|consensus|Consensus", txt, ignore.case=FALSE) &&
        nchar(txt) > 50 && nchar(txt) < 20000) {
      cat("  Found relevant script (", nchar(txt), "chars):\n")
      cat(substr(txt, 1, 500), "\n")
    }
  }
}

# ── 2. Historical data page ────────────────────────────────────────────────────
cat("\n=== 2. Historical data page ===\n")
r2 <- do_get(paste0(BASE, "/united-states/crude-oil-stocks-change/historical-data"))
if (!is.null(r2)) {
  cat("  Status:", status_code(r2), "\n")
  if (status_code(r2) == 200) {
    html2 <- read_html(rawToChar(r2$content))
    tbls2 <- html_table(html_nodes(html2, "table"), fill=TRUE)
    cat("  Tables:", length(tbls2), "\n")
    for (i in seq_along(tbls2)) {
      cat("  Table", i, "rows:", nrow(tbls2[[i]]), "\n")
      print(head(tbls2[[i]], 6))
    }
  }
}

# ── 3. Try TradingEconomics public API endpoint (no key needed?) ───────────────
cat("\n=== 3. TE public API ===\n")
api_urls <- c(
  paste0(BASE, "/api/indicator/?country=united+states&indicator=Crude+Oil+Stocks+Change"),
  "https://api.tradingeconomics.com/calendar/country/United%20States?c=guest:guest",
  paste0(BASE, "/graphs/historical-data.ashx?url=united-states/crude-oil-stocks-change&sid=0")
)
for (u in api_urls) {
  cat("  URL:", u, "\n")
  r <- do_get(u)
  if (!is.null(r)) {
    cat("  Status:", status_code(r), "  Bytes:", length(r$content), "\n")
    txt <- tryCatch(rawToChar(r$content), error=function(e) "")
    cat("  Body (200 chars):", substr(txt, 1, 200), "\n")
  }
}

# ── 4. Look for the actual JSON data feed used by the chart ───────────────────
cat("\n=== 4. Chart data endpoint ===\n")
chart_urls <- c(
  paste0(BASE, "/charts/united-states/crude-oil-stocks-change?d1=2021-01-01&d2=2026-06-30&type=column&title=United+States+Crude+Oil+Stocks+Change"),
  paste0(BASE, "/charts/united-states-crude-oil-stocks-change-area-chart-chart?d1=2021-01-01&d2=2026-06-30")
)
for (u in chart_urls) {
  cat("  URL:", substr(u, 1, 90), "\n")
  r <- do_get(u)
  if (!is.null(r)) {
    cat("  Status:", status_code(r), "  Bytes:", length(r$content), "\n")
    txt <- tryCatch(rawToChar(r$content), error=function(e) "")
    cat("  Body (300 chars):", substr(txt, 1, 300), "\n")
  }
}

cat("\n=== DONE ===\n")
