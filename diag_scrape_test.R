for (pkg in c("httr","rvest","jsonlite","data.table"))
  if (!requireNamespace(pkg, quietly=TRUE))
    install.packages(pkg, repos="https://cloud.r-project.org", quiet=TRUE)
library(httr); library(rvest); library(jsonlite); library(data.table)

# ── Test 1: Direct page GET ───────────────────────────────────────────────────
cat("=== Test 1: GET event page directly ===\n")
url <- "https://in.investing.com/economic-calendar/eia-crude-oil-inventories-75"
resp <- tryCatch(GET(url,
  add_headers(
    "User-Agent"      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
    "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.9",
    "Connection"      = "keep-alive"
  ), timeout(20)), error = function(e) { cat("ERROR:", e$message, "\n"); NULL })

if (!is.null(resp)) {
  cat("Status:", status_code(resp), "\n")
  html <- tryCatch(read_html(rawToChar(resp$content)), error = function(e) NULL)
  if (!is.null(html)) {
    tbls <- html_table(html_nodes(html, "table"), fill = TRUE)
    cat("Tables found:", length(tbls), "\n")
    if (length(tbls) > 0) { cat("First table head:\n"); print(head(tbls[[1]], 4)) }
    # Also look for any div or section with historical data
    rows <- html_nodes(html, "#eventHistoryTable tr")
    cat("eventHistoryTable rows:", length(rows), "\n")
    if (length(rows) > 0) print(html_text(head(rows, 3)))
  }
}

# ── Test 2: investing.com calendar POST API ───────────────────────────────────
cat("\n=== Test 2: POST to calendar API ===\n")
# First GET the calendar page to obtain a session/cookies
sess <- tryCatch(GET("https://www.investing.com/economic-calendar/",
  add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/125.0.0.0"),
  timeout(20)), error = function(e) NULL)
cat("Session status:", if (!is.null(sess)) status_code(sess) else "FAIL", "\n")

api_resp <- tryCatch(POST(
  "https://www.investing.com/economic-calendar/Service/getCalendarFilteredData",
  body = list(
    "country[]"    = "5",
    "eventIds[]"   = "75",
    "dateFrom"     = "2023-01-01",
    "dateTo"       = "2024-01-01",
    "timeZone"     = "55",
    "currentTab"   = "custom",
    "limit_from"   = "0"
  ),
  encode = "form",
  add_headers(
    "User-Agent"      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/125.0.0.0",
    "X-Requested-With"= "XMLHttpRequest",
    "Accept"          = "application/json, text/javascript, */*; q=0.01",
    "Referer"         = "https://www.investing.com/economic-calendar/",
    "Content-Type"    = "application/x-www-form-urlencoded; charset=UTF-8"
  ),
  if (!is.null(sess)) config(cookies = cookies(sess)),
  timeout(30)
), error = function(e) { cat("API ERROR:", e$message, "\n"); NULL })

if (!is.null(api_resp)) {
  cat("API Status:", status_code(api_resp), "\n")
  raw_text <- rawToChar(api_resp$content)
  cat("Response (first 500 chars):", substr(raw_text, 1, 500), "\n")
  parsed <- tryCatch(fromJSON(raw_text), error = function(e) NULL)
  if (!is.null(parsed)) {
    cat("JSON keys:", paste(names(parsed), collapse=", "), "\n")
    if ("data" %in% names(parsed)) {
      html2 <- tryCatch(read_html(parsed$data), error = function(e) NULL)
      if (!is.null(html2)) {
        tbls2 <- html_table(html_nodes(html2, "table"), fill = TRUE)
        cat("Tables in API response:", length(tbls2), "\n")
        if (length(tbls2) > 0) print(head(tbls2[[1]], 5))
      }
    }
  }
}
