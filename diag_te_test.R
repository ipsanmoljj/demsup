setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
library(httr); library(rvest); library(jsonlite); library(data.table)

UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"

try_get <- function(url, desc="") {
  cat("\n---", desc, "---\n  URL:", url, "\n")
  r <- tryCatch(GET(url, add_headers(
    "User-Agent"      = UA,
    "Accept"          = "text/html,application/xhtml+xml,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.9"
  ), timeout(20)), error = function(e) { cat("  ERROR:", e$message, "\n"); NULL })
  if (is.null(r)) return(invisible(NULL))
  cat("  Status:", status_code(r), "  Bytes:", length(r$content), "\n")
  txt <- tryCatch(rawToChar(r$content), error = function(e) "")
  cat("  First 300 chars:", substr(txt, 1, 300), "\n")
  if (status_code(r) == 200) {
    html <- tryCatch(read_html(txt), error = function(e) NULL)
    if (!is.null(html)) {
      tbls <- html_table(html_nodes(html, "table"), fill=TRUE)
      cat("  Tables found:", length(tbls), "\n")
      if (length(tbls)) { cat("  First table head:\n"); print(head(tbls[[1]], 4)) }
    }
  }
  Sys.sleep(1.5)
  invisible(r)
}

# ── TradingEconomics: US crude oil stocks change ──────────────────────────────
try_get("https://tradingeconomics.com/united-states/crude-oil-stocks-change",
        "TradingEconomics - main page")

# ── TradingEconomics API (public endpoint) ────────────────────────────────────
try_get("https://tradingeconomics.com/charts/united-states/crude-oil-stocks-change.png?d1=20210101&d2=20261231",
        "TradingEconomics chart PNG")

# ── OECD or other open sources ────────────────────────────────────────────────
try_get("https://api.eia.gov/v2/petroleum/stoc/wstk/data/?api_key=DEMO_KEY&facets[series][]=W_EPC0_SAX_YCUOK_MMBBLS&frequency=weekly&data[0]=value&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=20",
        "EIA API - crude stocks PADD2")

# ── EIA weekly petroleum status report - public ───────────────────────────────
try_get("https://www.eia.gov/petroleum/supply/weekly/",
        "EIA weekly supply page")

cat("\n=== DONE ===\n")
