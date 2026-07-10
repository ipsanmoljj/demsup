setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")

for (pkg in c("httr","rvest","jsonlite","data.table","zoo"))
  if (!requireNamespace(pkg, quietly=TRUE))
    install.packages(pkg, repos="https://cloud.r-project.org", quiet=TRUE)

library(httr); library(rvest); library(jsonlite); library(data.table); library(zoo)

UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"

# ── Step 1: Warm-up GET to get session cookies ────────────────────────────────
cat("=== Step 1: Warm-up GET for session cookies ===\n")
warm <- tryCatch(
  GET("https://www.investing.com/economic-calendar/",
      add_headers("User-Agent" = UA,
                  "Accept"     = "text/html,application/xhtml+xml,*/*;q=0.8",
                  "Accept-Language" = "en-US,en;q=0.9",
                  "Connection" = "keep-alive"),
      timeout(20)),
  error = function(e) { cat("  FAIL:", e$message, "\n"); NULL }
)
if (!is.null(warm)) {
  cat("  Status:", status_code(warm), "\n")
  ck <- cookies(warm)
  cat("  Cookies captured:", nrow(ck), "\n")
  if (nrow(ck)) print(ck[, c("name","value","domain")])
  ck_str <- if (nrow(ck)) paste(paste0(ck$name, "=", ck$value), collapse="; ") else NULL
  cat("  Cookie string length:", if (!is.null(ck_str)) nchar(ck_str) else 0, "\n")
} else {
  ck_str <- NULL
}

Sys.sleep(2)

# ── Step 2: Direct event page ─────────────────────────────────────────────────
cat("\n=== Step 2: Direct event page GET ===\n")
for (url in c(
  "https://in.investing.com/economic-calendar/eia-crude-oil-inventories-75",
  "https://www.investing.com/economic-calendar/eia-crude-oil-inventories-75"
)) {
  cat("  URL:", url, "\n")
  r <- tryCatch(
    GET(url, add_headers("User-Agent"=UA,"Accept"="text/html,*/*","Connection"="keep-alive"), timeout(20)),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(r)) {
    cat("  Status:", status_code(r), "  Content-length:", length(r$content), "\n")
    if (status_code(r) == 200) {
      html <- tryCatch(read_html(rawToChar(r$content)), error = function(e) NULL)
      if (!is.null(html)) {
        tbls <- html_table(html_nodes(html, "table"), fill=TRUE)
        cat("  Tables found:", length(tbls), "\n")
        if (length(tbls)) { cat("  First table:\n"); print(head(tbls[[1]], 6)) }
        # Check for eventHistoryTable specifically
        hist_rows <- html_nodes(html, "#eventHistoryTable tr, .historyTable tr, [class*='history'] tr")
        cat("  History rows found:", length(hist_rows), "\n")
      }
      break
    }
  }
}

Sys.sleep(2)

# ── Step 3: POST to calendar API with correct headers ─────────────────────────
cat("\n=== Step 3: POST to getCalendarFilteredData API ===\n")
post_url <- "https://www.investing.com/economic-calendar/Service/getCalendarFilteredData"

hdrs_list <- list(
  "User-Agent"        = UA,
  "X-Requested-With"  = "XMLHttpRequest",
  "Accept"            = "application/json, text/javascript, */*; q=0.01",
  "Referer"           = "https://www.investing.com/economic-calendar/",
  "Content-Type"      = "application/x-www-form-urlencoded; charset=UTF-8"
)
if (!is.null(ck_str)) hdrs_list[["Cookie"]] <- ck_str

cat("  Sending POST to:", post_url, "\n")
api_resp <- tryCatch(
  POST(post_url,
       body   = list(
         "country[]"  = "5",
         "eventIds[]" = "75",
         "dateFrom"   = "2023-01-01",
         "dateTo"     = "2023-06-30",
         "timeZone"   = "55",
         "currentTab" = "custom",
         "limit_from" = "0"
       ),
       encode = "form",
       do.call(add_headers, hdrs_list),
       timeout(30)),
  error = function(e) { cat("  POST ERROR:", e$message, "\n"); NULL }
)

if (!is.null(api_resp)) {
  cat("  Status:", status_code(api_resp), "\n")
  raw_text <- tryCatch(content(api_resp, "text", encoding="UTF-8"), error = function(e) "")
  cat("  Response length:", nchar(raw_text), "\n")
  cat("  First 600 chars:\n", substr(raw_text, 1, 600), "\n")

  parsed <- tryCatch(fromJSON(raw_text, simplifyVector=FALSE), error = function(e) NULL)
  if (!is.null(parsed)) {
    cat("  JSON keys:", paste(names(parsed), collapse=", "), "\n")
    if ("data" %in% names(parsed)) {
      html2 <- tryCatch(read_html(parsed$data), error = function(e) NULL)
      if (!is.null(html2)) {
        tbls2 <- html_table(html_nodes(html2, "table"), fill=TRUE)
        cat("  Tables in API response:", length(tbls2), "\n")
        if (length(tbls2)) print(head(tbls2[[1]], 6))
      }
    }
    if ("rows" %in% names(parsed)) cat("  'rows' value:", parsed$rows, "\n")
  }
}

Sys.sleep(1)

# ── Step 4: Try the in.investing.com POST ─────────────────────────────────────
cat("\n=== Step 4: POST via in.investing.com ===\n")
post_url_in <- "https://in.investing.com/economic-calendar/Service/getCalendarFilteredData"
api_resp2 <- tryCatch(
  POST(post_url_in,
       body   = list(
         "country[]"  = "5",
         "eventIds[]" = "75",
         "dateFrom"   = "2023-01-01",
         "dateTo"     = "2023-06-30",
         "timeZone"   = "55",
         "currentTab" = "custom",
         "limit_from" = "0"
       ),
       encode = "form",
       add_headers(
         "User-Agent"       = UA,
         "X-Requested-With" = "XMLHttpRequest",
         "Accept"           = "application/json, text/javascript, */*; q=0.01",
         "Referer"          = "https://in.investing.com/economic-calendar/",
         "Content-Type"     = "application/x-www-form-urlencoded; charset=UTF-8"
       ),
       timeout(30)),
  error = function(e) { cat("  POST ERROR:", e$message, "\n"); NULL }
)
if (!is.null(api_resp2)) {
  cat("  Status:", status_code(api_resp2), "\n")
  raw2 <- tryCatch(content(api_resp2, "text", encoding="UTF-8"), error = function(e) "")
  cat("  First 400 chars:\n", substr(raw2, 1, 400), "\n")
}

# ── Summary ───────────────────────────────────────────────────────────────────
cat("\n=== DONE ===\n")
cat("Warm-up status:", if (!is.null(warm)) status_code(warm) else "FAIL", "\n")
cat("Cookies obtained:", if (!is.null(ck_str)) "YES" else "NO", "\n")
cat("POST (www) status:", if (!is.null(api_resp)) status_code(api_resp) else "FAIL", "\n")
cat("POST (in) status:", if (!is.null(api_resp2)) status_code(api_resp2) else "FAIL", "\n")
