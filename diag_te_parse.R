setwd("C:/Users/kanwar.singh/OneDrive - hertshtengroup.com/Documents/demsup")
library(httr); library(rvest); library(jsonlite); library(data.table); library(lubridate)

UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"
BASE <- "https://tradingeconomics.com"

do_get <- function(url, sleep=1.2) {
  Sys.sleep(sleep)
  tryCatch(GET(url, add_headers(
    "User-Agent"=UA, "Accept"="text/html,*/*;q=0.8",
    "Accept-Language"="en-US,en;q=0.9", "Referer"=BASE
  ), timeout(30)), error=function(e) { cat("  ERROR:", e$message, "\n"); NULL })
}

parse_te_page <- function(r, label="") {
  if (is.null(r) || status_code(r) != 200) {
    cat("  FAIL (status=", if(!is.null(r)) status_code(r) else "NULL", ")\n"); return(NULL)
  }
  html <- tryCatch(read_html(rawToChar(r$content)), error=function(e) NULL)
  if (is.null(html)) { cat("  HTML parse failed\n"); return(NULL) }

  # Try standard table parse first
  tbls <- html_table(html_nodes(html, "table"), fill=TRUE)
  cat("  Tables found:", length(tbls), "\n")
  for (i in seq_along(tbls)) {
    nm <- tolower(names(tbls[[i]]))
    cat("  Table", i, ":", nrow(tbls[[i]]), "rows, cols:", paste(nm, collapse="|"), "\n")
    if (any(grepl("actual|consensus", nm))) {
      cat("  -> HAS ACTUAL/CONSENSUS COLUMNS\n")
      print(head(tbls[[i]], 10))
      return(as.data.table(tbls[[i]]))
    }
  }

  # Try looking for data embedded in <script> tags as JSON arrays
  scripts <- html_text(html_nodes(html, "script"))
  for (sc in scripts) {
    # Look for arrays like [["date","actual","consensus"],...]
    if (grepl("consensus|Consensus|\\[\\[\\d{4}", sc) && nchar(sc) > 200) {
      m <- regmatches(sc, regexpr("\\[\\[.*?\\]\\]", sc, perl=TRUE))
      if (length(m) && nchar(m) > 100) {
        cat("  -> Found JSON array in script (", nchar(m), " chars):\n")
        cat(substr(m, 1, 300), "\n")
        parsed <- tryCatch(fromJSON(m), error=function(e) NULL)
        if (!is.null(parsed)) { cat("  Parsed rows:", nrow(parsed), "\n"); print(head(parsed, 5)) }
        break
      }
    }
  }
  NULL
}

# ── 1. Main indicator-specific calendar ───────────────────────────────────────
cat("=== 1. TE Calendar filtered (indicator=crude-oil-stocks-change) ===\n")
r1 <- do_get(paste0(BASE, "/calendar?i=crude-oil-stocks-change&c=united-states"), sleep=0.3)
cat("  Status:", if(!is.null(r1)) status_code(r1) else "NULL", "  Bytes:", if(!is.null(r1)) length(r1$content) else 0, "\n")
tbl1 <- parse_te_page(r1)
if (!is.null(tbl1)) cat("  Rows captured:", nrow(tbl1), "\n")

# ── 2. Try date-ranged calendar URLs ─────────────────────────────────────────
cat("\n=== 2. TE Calendar with date ranges ===\n")
yr_urls <- list(
  "2021" = paste0(BASE, "/calendar?d1=2021-01-01&d2=2021-12-31&c=united+states&i=crude+oil+stocks+change"),
  "2022" = paste0(BASE, "/calendar?d1=2022-01-01&d2=2022-12-31&c=united+states&i=crude+oil+stocks+change"),
  "2023" = paste0(BASE, "/calendar?d1=2023-01-01&d2=2023-12-31&c=united+states&i=crude+oil+stocks+change"),
  "2024" = paste0(BASE, "/calendar?d1=2024-01-01&d2=2024-12-31&c=united+states&i=crude+oil+stocks+change")
)
all_rows <- list()
for (yr in names(yr_urls)) {
  cat("\n  Year:", yr, "\n")
  r <- do_get(yr_urls[[yr]])
  tbl <- parse_te_page(r)
  if (!is.null(tbl)) {
    all_rows[[yr]] <- tbl
    cat("  Year", yr, "rows:", nrow(tbl), "\n")
  }
}

# ── 3. Parse the indicator page calendar section directly ─────────────────────
cat("\n=== 3. Parse indicator page calendar section ===\n")
r3 <- do_get(paste0(BASE, "/united-states/crude-oil-stocks-change"))
if (!is.null(r3) && status_code(r3) == 200) {
  html3 <- read_html(rawToChar(r3$content))
  # Find the calendar table specifically
  cal_tbls <- html_nodes(html3, "table")
  cat("  Tables found:", length(cal_tbls), "\n")
  for (i in seq_along(cal_tbls)) {
    hdrs <- html_text(html_nodes(cal_tbls[[i]], "th"))
    cat("  Table", i, "headers:", paste(hdrs, collapse=" | "), "\n")
    tbl_data <- html_table(cal_tbls[[i]], fill=TRUE)
    print(head(tbl_data, 6))
  }

  # Look for any section with "Calendar" heading
  h4s <- html_text(html_nodes(html3, "h4, h3, h2"))
  cat("  Section headings:", paste(h4s, collapse=" | "), "\n")
}

cat("\n=== DONE ===\n")
