# ==============================================================================
# PHASE 1 SMOKE TEST
# ==============================================================================
# Run with:
#   app <- shiny::shinyAppFile("smoke_test_phase1.R")
#   shiny::runApp(app)
# (runApp("smoke_test_phase1.R") only works when the file is named app.R.)
#
# Verifies the foundation pieces work end-to-end:
#   * R/theme.R tokens load without errors
#   * autoscale_range() produces sensible bounds for a flat-ish series
#   * format_delta() distinguishes "+0.0" from "no change"
#   * KPI card renders with accent color, delta pill, and sparkline
#   * apply_gcc_theme() stamps plotly defaults onto a chart
#   * design-tokens.css and design-system.css load and resolve
#
# This is NOT the new dashboard. It is a minimal 2-card + 1-chart page
# used only to sanity-check Phase 1 before any migration begins.
# ==============================================================================

library(shiny)
library(htmltools)
library(plotly)

# Source portable Phase 1 modules
source("R/theme.R")
source("R/axis_helpers.R")
source("R/chart_defaults.R")
source("R/mod_kpi_card.R")

# ----- Fake data mirroring the real GCCEII shape ------------------------------
fake_ts <- data.frame(
  year  = 2015:2024,
  trade = c(62, 58, 55, 51, 48, 42, 45, 51, 55, 52),
  gcc   = c(45.4, 44.0, 43.2, 41.7, 42.9, 40.3, 42.0, 45.9, 47.4, 47.4)
)
latest <- fake_ts[nrow(fake_ts), ]
prev   <- fake_ts[nrow(fake_ts) - 1, ]

# ----- UI --------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "fonts.css"),
    tags$link(rel = "stylesheet", href = "design-tokens.css"),
    tags$link(rel = "stylesheet", href = "design-system.css"),
    tags$script(src = "theme.js")
  ),
  tags$div(
    class = "gcc-section",
    style = "padding: 24px; background: var(--gcc-surface-background); min-height: 100vh;",
    tags$h1(class = "gcc-section-title", "Phase 1 — Smoke Test"),
    tags$p(class = "gcc-section-subtitle",
           "Confirms theme tokens, auto-scaling, and the KPI card all work."),

    # --- Insight strip ---
    tags$div(
      class = "gcc-insight-strip",
      "The GCC integration score is ",
      tags$span(class = "highlight", "47.4"),
      ", unchanged from 2023. The previous axis showed ▲0 — the new ",
      "format_delta() helper correctly reports ",
      tags$span(class = "gcc-text-neutral", "“no change”"),
      " instead."
    ),

    # --- Two KPI cards side by side ---
    fluidRow(
      column(6, kpi_card_ui("card_gcc")),
      column(6, kpi_card_ui("card_trade"))
    ),

    tags$br(),

    # --- A full chart applying apply_gcc_theme() ---
    tags$div(
      class = "gcc-card",
      tags$div(class = "gcc-card-header",
               tags$h3(class = "gcc-card-title",
                       "GCC overall score 2015–2024 (auto-scaled y-axis)"),
               tags$span(class = "gcc-card-action",
                         "Observe the range — not 0–100")),
      plotlyOutput("gcc_trend", height = "360px")
    ),

    # --- Level badges ---
    tags$div(
      style = "margin-top: 16px;",
      tags$span(class = "gcc-level-badge weak", "Weak"),
      " ",
      tags$span(class = "gcc-level-badge moderate", "Moderate"),
      " ",
      tags$span(class = "gcc-level-badge good", "Good")
    )
  )
)

# ----- Server ----------------------------------------------------------------
server <- function(input, output, session) {

  # GCC overall card
  kpi_card_server(
    id      = "card_gcc",
    label   = reactive("GCC Overall"),
    value   = reactive(latest$gcc),
    delta   = reactive(latest$gcc - prev$gcc),     # = 0, should say "no change"
    history = reactive(data.frame(year = fake_ts$year, value = fake_ts$gcc)),
    accent  = gcc_theme$brand$primary,
    unit    = "/100"
  )

  # Trade dimension card
  kpi_card_server(
    id      = "card_trade",
    label   = reactive("Trade Integration"),
    value   = reactive(latest$trade),
    delta   = reactive(latest$trade - prev$trade), # = -3, negative pill
    history = reactive(data.frame(year = fake_ts$year, value = fake_ts$trade)),
    accent  = gcceii_dimension_palette()[["Trade"]],
    unit    = "/100"
  )

  # The auto-scaled chart
  output$gcc_trend <- renderPlotly({
    plot_ly(
      fake_ts, x = ~year, y = ~gcc,
      type = "scatter", mode = "lines+markers",
      line = list(color = gcc_theme$brand$primary, width = 3),
      marker = list(color = gcc_theme$brand$primary, size = 8)
    ) |>
      autoscale_plotly_y(fake_ts$gcc, pad = 0.12) |>
      apply_gcc_theme(show_legend = FALSE) |>
      add_source_annotation("Source: synthetic smoke-test data")
  })
}

shinyApp(ui, server)
