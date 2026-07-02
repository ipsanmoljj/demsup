# =============================================================================
# app.R — Demsup Energy Markets Dashboard (Stage 4)
#
# Interactive Shiny dashboard for the regime-classification + signal engine.
# Covers CL (WTI M1M2) and LCO (Brent M1M2) — the two products with the
# strongest validated daily signal performance:
#   CL:  310 trades, 57.4% hit rate (test window Jul 2024 - May 2026)
#   LCO:  18 trades, 77.8% hit rate (same window)
#
# Answers the five dashboard questions from the project philosophy doc:
#   1. What regime are we in?
#   2. Why are we in this regime?
#   3. How have similar periods behaved historically?
#   4. Which spreads/signals are currently interesting?
#   5. What's driving those opportunities?
#
# Data sources (read-only, relative to repo root):
#   output/regime_labels_CL.csv
#   output/regime_labels_LCO.csv
#   output/signal_CL.csv
#   output/signal_LCO.csv
#   output/trades_CL.csv
#   output/trades_LCO.csv
#
# Run with:  shiny::runApp("dashboard")        (from repo root)
#   or open this file in RStudio/VSCode and click "Run App"
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(ggplot2)
  library(scales)
  library(DT)
})

# -----------------------------------------------------------------------------
# 0. Configuration
# -----------------------------------------------------------------------------

PRODUCTS <- c("CL" = "CL — WTI (M1M2)", "LCO" = "LCO — Brent (M1M2)")

# ── Repo root resolution ─────────────────────────────────────────────────
# Shiny sets the working directory to the app's OWN folder (e.g. "dashboard/")
# regardless of where runApp() was called from. Since output/ lives one level
# up at the repo root, we resolve it explicitly here rather than relying on
# a relative "output/..." path, which would silently look in dashboard/output/.
#
# This walks up from the app's working directory until it finds an "output"
# folder, so it works whether you run via runApp("dashboard") from the repo
# root, or open app.R directly and click "Run App" from inside dashboard/.
.find_repo_root <- function() {
  candidates <- c(getwd(), dirname(getwd()))
  for (d in candidates) {
    if (dir.exists(file.path(d, "output"))) return(d)
  }
  # Fallback: assume cwd already IS repo root
  getwd()
}

REPO_ROOT <- .find_repo_root()

PATHS <- list(
  CL  = list(regime  = file.path(REPO_ROOT, "output", "regime_labels_CL.csv"),
             signal  = file.path(REPO_ROOT, "output", "signal_CL.csv"),
             trades  = file.path(REPO_ROOT, "output", "trades_CL.csv")),
  LCO = list(regime  = file.path(REPO_ROOT, "output", "regime_labels_LCO.csv"),
             signal  = file.path(REPO_ROOT, "output", "signal_LCO.csv"),
             trades  = file.path(REPO_ROOT, "output", "trades_LCO.csv"))
)

# Regime colour mapping — consistent across all panels
REGIME_COLORS <- c(
  "Deep-Backwardation"     = "#7a0d14",
  "Backwardation"          = "#b22222",
  "Backwardation-Deficit"  = "#d9683c",
  "Easing-Backwardation"   = "#e8a35a",
  "Transition-Tightening"  = "#d4b35a",
  "Stable-Elevated"        = "#c9a227",
  "Flat"                   = "#9a9a9a",
  "Stable-Depressed"       = "#7a9bc4",
  "Transition-Loosening"   = "#5a8fc4",
  "Easing-Contango"        = "#3f6fae",
  "Contango"               = "#2a4f8a",
  "Contango-Surplus"       = "#1d3a6e",
  "Deep-Contango"          = "#10254a",
  "Warm-Up"                = "#d9d9d9",
  "Unknown"                = "#c0c0c0"
)

SIGNAL_COLORS <- c("SELL" = "#b22222", "BUY" = "#1d6e3a", "FLAT" = "#9a9a9a")

# Startup diagnostics — printed to the R console (not the browser) so any
# path problem is immediately visible when you launch the app.
message("---- demsup dashboard startup ----")
message("Resolved REPO_ROOT: ", REPO_ROOT)
for (p in names(PATHS)) {
  for (kind in names(PATHS[[p]])) {
    pth <- PATHS[[p]][[kind]]
    message(sprintf("  %s %-7s -> %s  [exists: %s]",
                    p, kind, pth, file.exists(pth)))
  }
}
message("-----------------------------------")
# -----------------------------------------------------------------------------
# 1. Data loading helpers
# -----------------------------------------------------------------------------

load_regime_data <- function(prod) {
  if (is.null(prod) || length(prod) == 0L) return(NULL)
  # Defensive: if a display label slipped through instead of the key, map it back
  if (!prod %in% names(PATHS) && prod %in% PRODUCTS) {
    prod <- names(PRODUCTS)[match(prod, PRODUCTS)]
  }
  if (!prod %in% names(PATHS)) return(NULL)
  path <- PATHS[[prod]]$regime
  if (is.null(path) || !file.exists(path)) return(NULL)
  dt <- fread(path)
  dt[, date := as.Date(date)]
  setorder(dt, date)
  dt
}

load_signal_data <- function(prod) {
  if (is.null(prod) || length(prod) == 0L) return(NULL)
  if (!prod %in% names(PATHS) && prod %in% PRODUCTS) {
    prod <- names(PRODUCTS)[match(prod, PRODUCTS)]
  }
  if (!prod %in% names(PATHS)) return(NULL)
  path <- PATHS[[prod]]$signal
  if (is.null(path) || !file.exists(path)) return(NULL)
  dt <- fread(path)
  dt[, date := as.Date(date)]
  setorder(dt, date)
  dt
}

# Build a simple per-signal trade record from fwd10 (10-day forward M1M2 change)
build_trade_table <- function(sig_dt) {
  trades <- sig_dt[signal != "FLAT" & !is.na(fwd10)]
  if (nrow(trades) == 0L) return(data.table())
  trades[, correct := fifelse(signal == "SELL", fwd10 < 0, fwd10 > 0)]
  trades[, .(date, regime_label, signal, level_z_126, fwd10, fwd21, correct)]
}

load_trades_data <- function(prod) {
  if (!prod %in% names(PATHS)) return(NULL)
  path <- PATHS[[prod]]$trades
  if (is.null(path) || !file.exists(path)) return(NULL)
  dt <- fread(path)
  dt[, date := as.Date(date)]
  setorder(dt, date)
  dt
}

build_logs_table <- function(dt, prod) {
  if (is.null(dt) || nrow(dt) == 0L) return(data.table())

  dt <- copy(dt)

  # Sequential trade name
  dt[, trade_name := sprintf("%s-%03d", prod, seq_len(.N))]

  # Result
  dt[, result := fifelse(pnl_net > 0, "Win", "Loss")]

  # Planned target: entry ± 2 × stop_dist (2R target)
  dt[, planned_target := fifelse(
    signal == "BUY",
    entry_price + 2 * stop_dist,
    entry_price - 2 * stop_dist
  )]

  # Slippage: not captured in signal engine → flag N/A
  dt[, slippage := NA_real_]

  # Running cumulative PnL and max drawdown
  dt[, cum_pnl := cumsum(pnl_net)]
  dt[, run_max := cummax(cum_pnl)]
  dt[, max_drawdown := round(cum_pnl - run_max, 4)]

  # Rolling Sharpe (20-trade window, annualised assuming ~252 trades/yr)
  roll_sharpe <- function(x, w = 20L) {
    n <- length(x)
    out <- rep(NA_real_, n)
    for (i in seq(w, n)) {
      sl <- x[(i - w + 1L):i]
      sd_sl <- sd(sl)
      out[i] <- if (!is.na(sd_sl) && sd_sl > 0) (mean(sl) / sd_sl) * sqrt(252) else NA_real_
    }
    out
  }
  dt[, rolling_sharpe := round(roll_sharpe(pnl_net), 2)]

  dt[, .(
    Name           = trade_name,
    Timestamp      = date,
    Window         = window,
    Signal         = signal,
    Regime         = regime,
    `Entry Price`  = round(entry_price, 4),
    `Exit Price`   = round(exit_price, 4),
    `Stop Loss`    = round(entry_price - fifelse(signal=="BUY", stop_dist, -stop_dist), 4),
    `Planned Target` = round(planned_target, 4),
    `Slippage`     = slippage,
    `PnL Net`      = round(pnl_net, 4),
    `Max Drawdown` = max_drawdown,
    `Rolling Sharpe (20T)` = rolling_sharpe,
    `Bars Held`    = bars_held,
    `Exit Reason`  = exit_reason,
    Result         = result
  )]
}

# -----------------------------------------------------------------------------
# 2. UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
  title = "Demsup — Energy Markets Regime & Signal Dashboard",

  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', Helvetica, Arial, sans-serif; background-color: #f7f7f5; }
    .header-bar { background-color: #10254a; color: white; padding: 16px 24px; margin-bottom: 18px; border-radius: 4px; }
    .header-bar h2 { margin: 0; font-weight: 600; }
    .header-bar .subtitle { opacity: 0.8; font-size: 13px; margin-top: 2px; }
    .badge-panel { background: white; border-radius: 6px; padding: 14px 18px; box-shadow: 0 1px 3px rgba(0,0,0,0.08); margin-bottom: 14px; }
    .badge-label { font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em; color: #888; margin-bottom: 4px; }
    .badge-value { font-size: 22px; font-weight: 700; }
    .signal-SELL { color: #b22222; }
    .signal-BUY  { color: #1d6e3a; }
    .signal-FLAT { color: #9a9a9a; }
    .section-title { font-size: 15px; font-weight: 600; color: #10254a; margin: 18px 0 8px 0; border-bottom: 2px solid #eee; padding-bottom: 6px; }
  "))),

  div(class = "header-bar",
      h2("Demsup — Energy Markets Regime & Signal Dashboard"),
      div(class = "subtitle", "Daily M1M2 calendar spread regime classification and signal engine — CL and LCO")
  ),

  fluidRow(
    column(3,
      selectInput("product", "Product", choices = PRODUCTS, selected = "CL")
    ),
    column(3,
      dateRangeInput("date_range", "Date range",
                     start = "2021-01-01", end = as.character(Sys.Date()),
                     min = "2021-01-01", max = as.character(Sys.Date()))
    ),
    column(3,
      checkboxInput("show_warmup", "Include warm-up period", value = FALSE)
    ),
    column(3,
      uiOutput("data_freshness")
    )
  ),

  fluidRow(
    column(3, uiOutput("badge_regime")),
    column(3, uiOutput("badge_signal")),
    column(3, uiOutput("badge_zscore")),
    column(3, uiOutput("badge_confidence"))
  ),

  div(class = "section-title", "1-2. Current Regime & Why"),
  fluidRow(
    column(8, plotOutput("regime_timeline", height = "110px")),
    column(4, htmlOutput("regime_explainer"))
  ),

  div(class = "section-title", "Signal — level_z_126 vs SELL/BUY thresholds"),
  fluidRow(
    column(12, plotOutput("signal_chart", height = "320px"))
  ),

  div(class = "section-title", "3-4. Historical Analogues & Performance by Regime"),
  fluidRow(
    column(6, plotOutput("regime_perf_chart", height = "300px")),
    column(6, DTOutput("regime_perf_table"))
  ),

  div(class = "section-title", "5. Logs — Trade-Level History"),
  fluidRow(
    column(12, DTOutput("logs_table"))
  ),

  br()
)

# -----------------------------------------------------------------------------
# 3. Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  # ---- Reactive data loads ----
  regime_data <- reactive({
    req(input$product)
    message("DEBUG input$product = '", input$product, "'")  # remove after debugging
    dt <- load_regime_data(input$product)
    validate(need(!is.null(dt), paste0(
      "Could not find regime_labels_", input$product, ".csv in output/. ",
      "Run the dashboard from your repo root (where output/ lives).")))
    if (!input$show_warmup) dt <- dt[in_warmup == FALSE]
    req(input$date_range)
    dt[date >= input$date_range[1] & date <= input$date_range[2]]
  })

  signal_data <- reactive({
    req(input$product)
    dt <- load_signal_data(input$product)
    validate(need(!is.null(dt), paste0(
      "Could not find signal_", input$product, ".csv in output/. ",
      "Run the dashboard from your repo root (where output/ lives).")))
    req(input$date_range)
    dt[date >= input$date_range[1] & date <= input$date_range[2]]
  })

  trade_data <- reactive({
    build_trade_table(signal_data())
  })

  logs_data <- reactive({
    req(input$product)
    dt <- load_trades_data(input$product)
    if (is.null(dt)) return(data.table())
    req(input$date_range)
    dt <- dt[date >= input$date_range[1] & date <= input$date_range[2]]
    build_logs_table(dt, input$product)
  })

  latest_regime_row <- reactive({
    dt <- regime_data()
    if (nrow(dt) == 0L) return(NULL)
    dt[which.max(date)]
  })

  latest_signal_row <- reactive({
    dt <- signal_data()
    if (nrow(dt) == 0L) return(NULL)
    dt[which.max(date)]
  })

  # ---- Data freshness note ----
  output$data_freshness <- renderUI({
    rr <- latest_regime_row()
    if (is.null(rr)) return(NULL)
    div(style = "padding-top: 25px; color: #888; font-size: 12px;",
        paste0("Latest data: ", format(rr$date, "%d %b %Y")))
  })

  # ---- Badges ----
  output$badge_regime <- renderUI({
    rr <- latest_regime_row()
    if (is.null(rr)) return(NULL)
    col <- REGIME_COLORS[[rr$regime_label]]
    if (is.null(col)) col <- "#444"
    div(class = "badge-panel",
        div(class = "badge-label", "Current Regime"),
        div(class = "badge-value", style = paste0("color:", col, ";"), rr$regime_label)
    )
  })

  output$badge_signal <- renderUI({
    sr <- latest_signal_row()
    if (is.null(sr)) return(NULL)
    div(class = "badge-panel",
        div(class = "badge-label", "Current Signal"),
        div(class = paste0("badge-value signal-", sr$signal), sr$signal)
    )
  })

  output$badge_zscore <- renderUI({
    sr <- latest_signal_row()
    if (is.null(sr) || is.na(sr$level_z_126)) return(NULL)
    div(class = "badge-panel",
        div(class = "badge-label", "level_z_126"),
        div(class = "badge-value", sprintf("%.2f", sr$level_z_126))
    )
  })

  output$badge_confidence <- renderUI({
    rr <- latest_regime_row()
    if (is.null(rr) || !"confidence_score" %in% names(rr)) return(NULL)
    div(class = "badge-panel",
        div(class = "badge-label", "Confidence"),
        div(class = "badge-value", sprintf("%.0f%%", rr$confidence_score * 100)),
        div(style = "font-size: 12px; color: #888; margin-top: 2px;",
            if ("confidence_band" %in% names(rr)) rr$confidence_band else "")
    )
  })

  # ---- Regime timeline strip ----
  output$regime_timeline <- renderPlot({
    dt <- regime_data()
    if (nrow(dt) == 0L) return(NULL)

    ggplot(dt, aes(x = date, y = 1, fill = regime_label)) +
      geom_tile(height = 1) +
      scale_fill_manual(values = REGIME_COLORS, na.value = "#d9d9d9") +
      scale_x_date(expand = c(0, 0)) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.y = element_blank(), axis.title = element_blank(),
        axis.ticks.y = element_blank(), panel.grid = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        legend.key.size = unit(10, "pt"), legend.text = element_text(size = 8)
      ) +
      guides(fill = guide_legend(nrow = 2))
  })

  # ---- Regime explainer text ----
  output$regime_explainer <- renderUI({
    rr <- latest_regime_row()
    if (is.null(rr)) return(NULL)

    explain <- switch(rr$regime_label,
      "Deep-Backwardation"    = "Spread is sharply positive and well above its lagged baseline — strong physical tightness, sustained scarcity premium.",
      "Backwardation"         = "Spread is positive with moderate intensity — front-month premium reflecting near-term tightness.",
      "Backwardation-Deficit" = "Spread positive but easing from a backwardation peak — tightness still present but moderating.",
      "Easing-Backwardation"  = "Spread declining from a backwardated state — transition toward flat/balanced conditions.",
      "Flat"                  = "Spread near zero — no strong directional pricing of storage economics either way.",
      "Easing-Contango"       = "Spread negative but improving from a deeper contango — oversupply easing.",
      "Contango"              = "Spread negative — market pricing in storage economics, mild oversupply signal.",
      "Deep-Contango"         = "Spread sharply negative — strong oversupply / storage-is-profitable signal.",
      "Stable-Elevated"       = "Spread elevated but range-bound — persistent mild tightness without a clear trend.",
      "Stable-Depressed"      = "Spread depressed but range-bound — persistent mild oversupply without a clear trend.",
      "Transition-Tightening" = "Market is moving from looser toward tighter conditions — early-stage regime shift.",
      "Transition-Loosening"  = "Market is moving from tighter toward looser conditions — early-stage regime shift.",
      "Contango-Surplus"      = "Spread negative with surplus characteristics — oversupply with storage economics confirming it.",
      "Warm-Up"               = "Insufficient history for this date to classify reliably — excluded from signal generation.",
      paste0("Regime label: ", rr$regime_label)
    )

    days_txt <- if (!is.null(rr$days_since_break) && !is.na(rr$days_since_break)) {
      sprintf("%d days since last structural break.", rr$days_since_break)
    } else ""

    HTML(paste0(
      "<div style='background:white; border-radius:6px; padding:14px 18px; ",
      "box-shadow:0 1px 3px rgba(0,0,0,0.08); font-size:13px; line-height:1.5;'>",
      "<b>M1M2 = ", sprintf("%.3f", rr$M1M2), "</b><br>",
      explain, "<br><span style='color:#888;'>", days_txt, "</span>",
      "</div>"
    ))
  })

  # ---- Signal chart ----
  output$signal_chart <- renderPlot({
    dt <- signal_data()
    if (nrow(dt) == 0L) return(NULL)

    sell_thresh <- 1.0
    buy_thresh  <- -1.0

    ggplot(dt, aes(x = date, y = level_z_126)) +
      geom_hline(yintercept = sell_thresh, linetype = "dashed", color = "#b22222", linewidth = 0.5) +
      geom_hline(yintercept = buy_thresh,  linetype = "dashed", color = "#1d6e3a", linewidth = 0.5) +
      geom_hline(yintercept = 0, color = "#cccccc", linewidth = 0.4) +
      geom_line(color = "#10254a", linewidth = 0.4, na.rm = TRUE) +
      geom_point(data = dt[signal != "FLAT"], aes(color = signal), size = 1.6, na.rm = TRUE) +
      scale_color_manual(values = SIGNAL_COLORS) +
      labs(x = NULL, y = "level_z_126", color = "Signal") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top", panel.grid.minor = element_blank())
  })

  # ---- Regime performance chart ----
  output$regime_perf_chart <- renderPlot({
    td <- trade_data()
    if (nrow(td) == 0L) return(NULL)

    perf <- td[, .(trades = .N, hit_rate = mean(correct) * 100), by = regime_label]
    perf <- perf[order(-trades)]

    ggplot(perf, aes(x = reorder(regime_label, trades), y = hit_rate, fill = regime_label)) +
      geom_col() +
      geom_text(aes(label = sprintf("%.0f%% (n=%d)", hit_rate, trades)), hjust = -0.05, size = 3.2) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "#888") +
      scale_fill_manual(values = REGIME_COLORS, guide = "none") +
      coord_flip(clip = "off") +
      ylim(0, max(perf$hit_rate, 100) * 1.15) +
      labs(x = NULL, y = "Hit rate (%, 10-day forward)") +
      theme_minimal(base_size = 12) +
      theme(plot.margin = margin(5, 40, 5, 5))
  })

  output$regime_perf_table <- renderDT({
    td <- trade_data()
    if (nrow(td) == 0L) return(NULL)

    perf <- td[, .(
      Trades   = .N,
      `Hit Rate %` = round(mean(correct) * 100, 1),
      `Avg fwd10` = round(mean(fwd10, na.rm = TRUE), 3),
      `Avg fwd21` = round(mean(fwd21, na.rm = TRUE), 3)
    ), by = .(Regime = regime_label)][order(-Trades)]

    datatable(perf, options = list(pageLength = 10, dom = "tp"), rownames = FALSE)
  })

  # ---- Logs table (trade-level history from trades CSV) ----
  output$logs_table <- renderDT({
    ld <- logs_data()
    if (is.null(ld) || nrow(ld) == 0L) {
      return(datatable(data.frame(Message = "No trades found for this product / date range."),
                       rownames = FALSE))
    }
    datatable(
      ld[order(-Timestamp)],
      options  = list(pageLength = 20, scrollX = TRUE,
                      order = list(list(1, "desc"))),
      rownames = FALSE
    ) |>
      formatStyle("Result",
                  backgroundColor = styleEqual(c("Win", "Loss"),
                                               c("#e3f3e6", "#fbe4e4"))) |>
      formatStyle("Max Drawdown",
                  color = styleInterval(0, c("#b22222", "#333"))) |>
      formatCurrency(c("Entry Price","Exit Price","Stop Loss",
                       "Planned Target","PnL Net"), currency = "", digits = 4)
  })
}

# -----------------------------------------------------------------------------
# 4. Run
# -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)