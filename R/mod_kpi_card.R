# ==============================================================================
# KPI CARD — REUSABLE SHINY MODULE
# ==============================================================================
# @portable:    yes
# @phase:       1 (foundation)
# @shared-with: GCCEDI
# @depends-on:  R/theme.R, R/axis_helpers.R (format_delta, delta_color),
#               R/chart_defaults.R (apply_gcc_theme, for the sparkline)
# ==============================================================================
#
# A small headline card designed to live in the top row of the Executive
# Pulse tab and in Country Profile. Shows:
#   - label        (e.g. "Trade Integration")
#   - value        (e.g. 47.4)
#   - unit         (e.g. "/100")
#   - delta pill   (e.g. "+2.3 pts" in green, "-0.8 pts" in red, "no change")
#   - sparkline    (last ~10 years, auto-scaled so the trend is visible)
#   - accent bar   (color-coded by dimension)
#
# Usage:
#   # UI
#   kpi_card_ui("trade_card")
#
#   # Server
#   kpi_card_server(
#     id      = "trade_card",
#     label   = reactive(t("trade", lang())),
#     value   = reactive(latest$trade_score),
#     delta   = reactive(latest$trade_score - prev$trade_score),
#     history = reactive(gcc_ts[, c("year", "trade_score")]),
#     accent  = gcceii_dimension_palette()[["Trade"]],
#     lang    = lang
#   )
# ==============================================================================


# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

#' UI for a KPI card
#' @param id module id
#' @param height fixed card height in px
#' @export
kpi_card_ui <- function(id, height = 140) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "gcc-kpi-card",
    style = sprintf("height: %dpx;", height),
    htmltools::div(class = "gcc-kpi-accent", id = ns("accent")),
    htmltools::div(
      class = "gcc-kpi-body",
      htmltools::div(class = "gcc-kpi-label", shiny::textOutput(ns("label"), inline = TRUE)),
      htmltools::div(
        class = "gcc-kpi-value-row",
        htmltools::span(class = "gcc-kpi-value", shiny::textOutput(ns("value"), inline = TRUE)),
        htmltools::span(class = "gcc-kpi-unit",  shiny::textOutput(ns("unit"),  inline = TRUE))
      ),
      htmltools::div(class = "gcc-kpi-delta", shiny::uiOutput(ns("delta")))
    ),
    htmltools::div(
      class = "gcc-kpi-spark",
      plotly::plotlyOutput(ns("spark"), height = "46px")
    )
  )
}


# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------

#' Server for a KPI card
#'
#' @param id module id
#' @param label    reactive -> character (card title, already translated)
#' @param value    reactive -> numeric (headline number)
#' @param delta    reactive -> numeric (change vs prior period; NA ok)
#' @param history  reactive -> data.frame(year, value) for the sparkline
#' @param accent   single hex color for the left accent bar
#' @param unit     character (default "/100" for scored indices)
#' @param digits   numeric precision (default 1)
#' @param lang     reactive -> "en" | "ar"
#' @export
kpi_card_server <- function(id,
                            label,
                            value,
                            delta,
                            history,
                            accent  = "#003366",
                            unit    = "/100",
                            digits  = 1,
                            lang    = shiny::reactive("en")) {

  shiny::moduleServer(id, function(input, output, session) {

    # Accent bar color (applied via inline style — not a re-render concern)
    output$label <- shiny::renderText({ label() })

    output$value <- shiny::renderText({
      v <- value()
      if (is.null(v) || is.na(v)) return("\u2014")
      formatC(v, digits = digits, format = "f")
    })

    output$unit <- shiny::renderText({ unit })

    output$delta <- shiny::renderUI({
      d <- delta()
      color <- delta_color(d)
      # No decoration when delta is missing
      if (is.null(d) || !is.finite(d)) {
        return(htmltools::span(class = "gcc-kpi-delta-pill gcc-kpi-delta-neutral", "\u2014"))
      }
      text <- format_delta(d, digits = digits, unit = "pts")
      # Arrow glyph
      arrow <- if (abs(round(d, digits)) < 10 ^ -digits) "\u25CF"
               else if (d > 0) "\u25B2"
               else "\u25BC"
      htmltools::span(
        class = "gcc-kpi-delta-pill",
        style = sprintf("color: %s; background: %s;",
                        color,
                        delta_soft_bg(d)),
        htmltools::span(class = "gcc-kpi-delta-arrow", arrow),
        " ",
        text
      )
    })

    output$spark <- plotly::renderPlotly({
      df <- history()
      if (is.null(df) || nrow(df) < 2) {
        return(plotly::plotly_empty())
      }
      # Expect columns named year + value OR (year + <anything numeric>)
      xcol <- "year"
      ycol <- setdiff(names(df), "year")[1]
      rng  <- autoscale_range(df[[ycol]], pad = 0.12, nice_round = FALSE)

      plotly::plot_ly(
        df,
        x = stats::as.formula(paste0("~", xcol)),
        y = stats::as.formula(paste0("~", ycol)),
        type = "scatter", mode = "lines",
        line = list(color = accent, width = 2),
        hoverinfo = "x+y"
      ) |>
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE, range = rng),
          margin = list(t = 0, r = 0, b = 0, l = 0),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          showlegend = FALSE
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # Inject accent color via a one-off JS observer (avoids re-render)
    shiny::observe({
      session$sendCustomMessage(
        "gcc_set_accent",
        list(id = session$ns("accent"), color = accent)
      )
    })
  })
}


# ------------------------------------------------------------------------------
# INTERNAL: SOFT BACKGROUND FOR DELTA PILL
# ------------------------------------------------------------------------------

delta_soft_bg <- function(d, threshold = 0.05, theme = gcc_theme) {
  if (is.na(d))                return(theme$semantic$neutral_soft)
  if (abs(d) < threshold)      return(theme$semantic$neutral_soft)
  if (d > 0)                   return(theme$semantic$positive_soft)
  theme$semantic$negative_soft
}
