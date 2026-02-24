# =============================================================================
# R/mod_charts.R
# GCCEII Dashboard – Bilingual Plotly Chart Functions
# =============================================================================
# Every function accepts a `lang` parameter ("en" or "ar") and uses
# translations.R helpers to localise all user-visible text.
#
# Dependencies: R/translations.R must be sourced before this file.
#
# Plotly note: chart containers stay direction:ltr (SVG renders internally).
# Arabic labels are passed as character strings into Plotly layout / trace
# arguments — the SVG engine handles bidirectional text correctly.
#
# Functions exported:
#   chart_gcc_gauge()              – GCC overall indicator gauge
#   chart_dimension_bars()         – Horizontal bar: latest dimension scores
#   chart_dimension_scoreboxes()   – renderUI infoBox grid for 6 dimensions
#   chart_gcc_trend()              – GCC overall index trend line (with ribbon)
#   chart_gcc_dimension_ts()       – Multi-line: GCC dimension timeseries
#   chart_overall_ts_countries()   – Multi-line: overall index all countries
#   chart_radar()                  – Radar / scatterpolar: dimension profile
#   chart_correlation()            – Heatmap: dimension correlation matrix
#   chart_country_trend()          – Country overall index trend line
#   chart_country_dims_current()   – Country dimension bar (latest year)
#   chart_country_all_dims()       – Country all dimensions over time
#   chart_country_vs_gcc()         – Country vs GCC aggregate comparison
#   chart_country_heatmap()        – Country × dimension score heatmap
#   chart_rankings()               – Dimension rankings bump chart
#   chart_yoy_overall()            – YoY overall change horizontal bar
#   chart_yoy_dimensions()         – YoY change by dimension scatter
#   chart_contribution()           – Contribution to total change grouped bar
#   chart_annual_trends()          – Annual change trends (mean / min / max)
#   chart_change_heatmap()         – Country × year change heatmap
# =============================================================================

library(plotly)
library(dplyr)
library(tidyr)
library(tibble)
library(viridis)
library(shiny)
library(shinydashboard)

# ── Shared constants ──────────────────────────────────────────────────────────

.dim_cols <- c(
  "trade_score", "financial_score", "labor_score",
  "infrastructure_score", "sustainability_score", "convergence_score"
)

.dim_labels_en <- c(
  "Trade", "Financial", "Labor",
  "Infrastructure", "Sustainability", "Convergence"
)

.country_colors <- c(
  "Bahrain"      = "#E41A1C",
  "Kuwait"       = "#377EB8",
  "Oman"         = "#4DAF4A",
  "Qatar"        = "#984EA3",
  "Saudi Arabia" = "#FF7F00",
  "UAE"          = "#A65628"
)

# ── Internal helpers ──────────────────────────────────────────────────────────

# Return translated dimension labels vector (length 6, in standard order)
.dlabels <- function(lang) {
  translate_dimensions(.dim_labels_en, lang = lang)
}

# Return a standard Plotly font list appropriate for the language
.plotly_font <- function(lang) {
  list(
    family = if (lang == "ar") {
      "Cairo, Tajawal, Arial, sans-serif"
    } else {
      "Source Sans Pro, Lato, Arial, sans-serif"
    },
    size = 12
  )
}

# Standard Plotly layout defaults shared by all charts
.base_layout <- function(lang, ...) {
  list(
    font        = .plotly_font(lang),
    plot_bgcolor  = "white",
    paper_bgcolor = "white",
    margin      = list(t = 40, b = 60, l = 80, r = 20),
    ...
  )
}

# Translate a country name that may appear as a trace name / axis tick
.cn <- function(name, lang) translate_country(name, lang)

# Translate country names for colour lookup (colors are keyed on English names)
.country_color <- function(name) {
  unname(.country_colors[name])
}

# =============================================================================
# 1. GCC Overall Gauge
# =============================================================================

#' GCC overall integration gauge (indicator chart)
#'
#' @param gcc_ts   Data frame with GCC aggregate timeseries (cols: year, overall).
#' @param lang     "en" or "ar".
chart_gcc_gauge <- function(gcc_ts, lang = "en") {
  latest    <- gcc_ts %>% filter(year == max(year))
  prev_val  <- gcc_ts %>% filter(year == max(year) - 1) %>% pull(overall)
  yr        <- max(gcc_ts$year)

  title_text <- paste(
    t("app_title", lang), "\u2014",
    if (lang == "ar") paste0("\u0639\u0627\u0645 ", yr) else yr   # "عام" in Arabic
  )

  plot_ly(
    type  = "indicator",
    mode  = "gauge+number+delta",
    value = latest$overall,
    title = list(text = title_text, font = .plotly_font(lang)),
    delta = list(reference = prev_val),
    gauge = list(
      axis = list(range = list(0, 100)),
      bar  = list(color = "#1a3a5c"),
      steps = list(
        list(range = c(0, 40),  color = "#f5b7b1"),   # Weak  – soft red
        list(range = c(40, 60), color = "#fdebd0"),   # Moderate – soft amber
        list(range = c(60, 100), color = "#a9dfbf")   # Good  – soft green
      ),
      threshold = list(
        line      = list(color = "red", width = 4),
        thickness = 0.75,
        value     = 70
      )
    )
  ) %>%
    layout(
      font   = .plotly_font(lang),
      margin = list(l = 20, r = 20, t = 50, b = 20)
    )
}

# =============================================================================
# 2. Dimension Bar Chart (latest year, horizontal)
# =============================================================================

#' Horizontal bar chart of latest GCC dimension scores
#'
#' @param gcc_ts  Data frame with GCC aggregate timeseries.
#' @param lang    "en" or "ar".
chart_dimension_bars <- function(gcc_ts, lang = "en") {
  latest <- gcc_ts %>% filter(year == max(year))

  dim_data <- data.frame(
    Dimension = .dlabels(lang),
    Score     = c(
      latest$trade_score, latest$financial_score, latest$labor_score,
      latest$infrastructure_score, latest$sustainability_score,
      latest$convergence_score
    )
  ) %>% arrange(Score)

  plot_ly(
    dim_data,
    x           = ~Score,
    y           = ~reorder(Dimension, Score),
    type        = "bar",
    orientation = "h",
    marker      = list(color = viridis(6)),
    hovertemplate = paste0("%{y}: %{x:.1f}<extra></extra>")
  ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis  = list(title = t("axis_score", lang), range = c(0, 100)),
        yaxis  = list(title = ""),
        margin = list(l = 160, r = 20, t = 40, b = 60)
      )))
    )
}

# =============================================================================
# 3. Dimension Score Boxes (renderUI)
# =============================================================================

#' infoBox grid for 6 dimension scores
#'
#' @param gcc_ts  Data frame with GCC aggregate timeseries.
#' @param lang    "en" or "ar".
#' @return        tagList suitable for renderUI.
chart_dimension_scoreboxes <- function(gcc_ts, lang = "en") {
  latest <- gcc_ts %>% filter(year == max(year))
  scores <- c(
    latest$trade_score, latest$financial_score, latest$labor_score,
    latest$infrastructure_score, latest$sustainability_score,
    latest$convergence_score
  )
  labels <- .dlabels(lang)

  box_list <- lapply(1:6, function(i) {
    color <- if (scores[i] >= 60) "green" else if (scores[i] >= 40) "yellow" else "red"
    infoBox(
      labels[i],
      round(scores[i], 1),
      icon  = icon("chart-bar"),
      color = color,
      fill  = FALSE,
      width = 6
    )
  })

  tagList(
    fluidRow(box_list[[1]], box_list[[2]]),
    fluidRow(box_list[[3]], box_list[[4]]),
    fluidRow(box_list[[5]], box_list[[6]])
  )
}

# =============================================================================
# 4. GCC Overall Trend Line (with confidence ribbon)
# =============================================================================

#' GCC overall integration index trend
#'
#' @param gcc_ts  Data frame with cols: year, overall.
#' @param lang    "en" or "ar".
chart_gcc_trend <- function(gcc_ts, lang = "en") {
  plot_ly(gcc_ts, x = ~year) %>%
    add_ribbons(
      ymin      = ~overall - 2,
      ymax      = ~overall + 2,
      line      = list(color = "rgba(22, 96, 167, 0.0)"),
      fillcolor = "rgba(22, 96, 167, 0.15)",
      name      = if (lang == "ar") "\u0646\u0637\u0627\u0642 \u0627\u0644\u062a\u0642\u062f\u064a\u0631"
                  else "Confidence Band",
      showlegend = TRUE,
      hoverinfo  = "skip"
    ) %>%
    add_trace(
      y    = ~overall,
      type = "scatter", mode = "lines+markers",
      name = t("app_title", lang),
      line = list(color = "rgb(22, 96, 167)", width = 3),
      marker = list(size = 9, color = "rgb(22, 96, 167)"),
      hovertemplate = paste0(
        t("axis_year", lang), ": %{x}<br>",
        t("axis_overall_index", lang), ": %{y:.1f}<extra></extra>"
      )
    ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis     = list(title = t("axis_year", lang)),
        yaxis     = list(title = t("axis_overall_index", lang), range = c(0, 100)),
        hovermode = "x unified",
        showlegend = TRUE,
        legend    = list(orientation = "h", y = -0.2)
      )))
    )
}

# =============================================================================
# 5. GCC Dimension Timeseries (multi-line, user-selected dimensions)
# =============================================================================

#' GCC integration timeseries for selected dimensions
#'
#' @param gcc_ts          Data frame with GCC aggregate timeseries.
#' @param selected_cols   Character vector of dimension column names to plot.
#' @param show_overall    Logical. Whether to add the overall index line.
#' @param lang            "en" or "ar".
chart_gcc_dimension_ts <- function(gcc_ts, selected_cols, show_overall = TRUE,
                                    lang = "en") {
  p <- plot_ly()

  for (col in selected_cols) {
    en_label  <- .dim_labels_en[which(.dim_cols == col)]
    dim_label <- translate_dimension(en_label, lang)

    p <- p %>%
      add_trace(
        data = gcc_ts,
        x    = ~year,
        y    = gcc_ts[[col]],
        type = "scatter", mode = "lines+markers",
        name = dim_label,
        line   = list(width = 2),
        marker = list(size = 8),
        hovertemplate = paste0(dim_label, ": %{y:.1f}<extra></extra>")
      )
  }

  if (show_overall) {
    overall_label <- if (lang == "ar") "\u0627\u0644\u0645\u0624\u0634\u0631 \u0627\u0644\u0625\u062c\u0645\u0627\u0644\u064a"
                     else "Overall Index"
    p <- p %>%
      add_trace(
        data = gcc_ts,
        x    = ~year,
        y    = ~overall,
        type = "scatter", mode = "lines+markers",
        name = overall_label,
        line   = list(width = 3, dash = "dash", color = "black"),
        marker = list(size = 10, color = "black"),
        hovertemplate = paste0(overall_label, ": %{y:.1f}<extra></extra>")
      )
  }

  p %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis     = list(title = t("axis_year", lang)),
        yaxis     = list(title = t("axis_score", lang), range = c(0, 100)),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )))
    )
}

# =============================================================================
# 6. Overall Index Timeseries – All Countries
# =============================================================================

#' Overall index over time for all GCC countries
#'
#' @param dimension_scores  Data frame with cols: country, year, overall_index.
#' @param country_colors    Named character vector of country colours (English names as keys).
#' @param lang              "en" or "ar".
chart_overall_ts_countries <- function(dimension_scores, country_colors = .country_colors,
                                        lang = "en") {
  countries <- unique(dimension_scores$country)
  p <- plot_ly()

  for (ctry in countries) {
    ctry_data  <- dimension_scores %>% filter(country == ctry)
    ctry_label <- translate_country(ctry, lang)
    col        <- unname(country_colors[ctry])

    p <- p %>%
      add_trace(
        data = ctry_data,
        x    = ~year,
        y    = ~overall_index,
        type = "scatter", mode = "lines+markers",
        name = ctry_label,
        line   = list(color = col, width = 2),
        marker = list(size = 8, color = col),
        hovertemplate = paste0(ctry_label, ": %{y:.1f}<extra></extra>")
      )
  }

  p %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis     = list(title = t("axis_year", lang)),
        yaxis     = list(title = t("axis_overall_index", lang), range = c(0, 100)),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )))
    )
}

# =============================================================================
# 7. Radar / Scatterpolar Chart
# =============================================================================

#' Radar chart of dimension scores for a single entity (GCC or country)
#'
#' @param scores  Named numeric vector with names matching .dim_labels_en.
#'                OR a length-6 numeric vector in standard dimension order.
#' @param label   Character. Name for the trace (entity name).
#' @param color   Character. Hex / rgb colour for this trace.
#' @param lang    "en" or "ar".
chart_radar <- function(scores, label = "GCC", color = "rgb(22, 96, 167)",
                         lang = "en") {
  theta <- c(.dlabels(lang), .dlabels(lang)[1])  # close polygon
  r     <- c(as.numeric(scores), as.numeric(scores)[1])

  plot_ly(
    type  = "scatterpolar",
    r     = r,
    theta = theta,
    fill  = "toself",
    name  = if (lang == "ar") translate_country(label, lang) else label,
    fillcolor = paste0(
      gsub("^rgb\\(", "rgba(", gsub("\\)$", "", color)), ", 0.25)"
    ),
    line  = list(color = color, width = 2),
    hovertemplate = paste0("%{theta}: %{r:.1f}<extra></extra>")
  ) %>%
    layout(
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0, 100))
      ),
      font       = .plotly_font(lang),
      showlegend = FALSE,
      margin     = list(t = 40, b = 40, l = 60, r = 60)
    )
}

# =============================================================================
# 8. Dimension Correlation Heatmap
# =============================================================================

#' Correlation matrix heatmap across the 6 dimensions
#'
#' @param gcc_ts  Data frame with GCC aggregate timeseries.
#' @param lang    "en" or "ar".
chart_correlation <- function(gcc_ts, lang = "en") {
  cor_data   <- gcc_ts %>% select(all_of(.dim_cols)) %>% cor(use = "complete.obs")
  dim_labels <- .dlabels(lang)

  plot_ly(
    x    = dim_labels,
    y    = dim_labels,
    z    = cor_data,
    type = "heatmap",
    colorscale = "RdBu",
    zmid = 0,
    text = round(cor_data, 2),
    hovertemplate = "%{y} \u00d7 %{x}: %{z:.2f}<extra></extra>"
  ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )))
    )
}

# =============================================================================
# 9. Country Overall Trend Line
# =============================================================================

#' Single-country overall integration index over time
#'
#' @param country_ts     Data frame for one country (cols: year, overall_index).
#' @param country_name   English name of the country (for colour lookup).
#' @param country_colors Named colour vector.
#' @param lang           "en" or "ar".
chart_country_trend <- function(country_ts, country_name,
                                 country_colors = .country_colors, lang = "en") {
  col        <- unname(country_colors[country_name])
  ctry_label <- translate_country(country_name, lang)

  plot_ly(
    country_ts,
    x    = ~year,
    y    = ~overall_index,
    type = "scatter", mode = "lines+markers",
    name = ctry_label,
    line   = list(color = col, width = 3),
    marker = list(size = 10, color = col),
    hovertemplate = paste0(
      t("axis_year", lang), ": %{x}<br>",
      t("axis_overall_index", lang), ": %{y:.1f}<extra></extra>"
    )
  ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis     = list(title = t("axis_year", lang)),
        yaxis     = list(title = t("axis_overall_index", lang), range = c(0, 100)),
        hovermode = "x unified"
      )))
    )
}

# =============================================================================
# 10. Country Dimension Bar (latest year, horizontal)
# =============================================================================

#' Horizontal bar of a country's latest dimension scores
#'
#' @param country_latest  One-row data frame with the 6 dimension score columns.
#' @param country_name    English country name (for colour).
#' @param country_colors  Named colour vector.
#' @param lang            "en" or "ar".
chart_country_dims_current <- function(country_latest, country_name,
                                        country_colors = .country_colors, lang = "en") {
  col <- unname(country_colors[country_name])

  dim_data <- data.frame(
    Dimension = .dlabels(lang),
    Score = c(
      country_latest$trade_score, country_latest$financial_score,
      country_latest$labor_score, country_latest$infrastructure_score,
      country_latest$sustainability_score, country_latest$convergence_score
    )
  ) %>% arrange(Score)

  plot_ly(
    dim_data,
    x           = ~Score,
    y           = ~reorder(Dimension, Score),
    type        = "bar",
    orientation = "h",
    marker      = list(color = col),
    hovertemplate = "%{y}: %{x:.1f}<extra></extra>"
  ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis  = list(title = t("axis_score", lang), range = c(0, 100)),
        yaxis  = list(title = ""),
        margin = list(l = 160, r = 20, t = 40, b = 60)
      )))
    )
}

# =============================================================================
# 11. Country All Dimensions Over Time (multi-line)
# =============================================================================

#' All 6 dimension timeseries for a single country
#'
#' @param country_ts  Data frame for one country (cols: year, + 6 dim score cols).
#' @param lang        "en" or "ar".
chart_country_all_dims <- function(country_ts, lang = "en") {
  plot_data <- country_ts %>%
    select(year, all_of(.dim_cols)) %>%
    pivot_longer(
      cols      = all_of(.dim_cols),
      names_to  = "dimension",
      values_to = "score"
    ) %>%
    mutate(
      dim_label = translate_dimensions(
        .dim_labels_en[match(dimension, .dim_cols)],
        lang = lang
      )
    )

  plot_ly(
    plot_data,
    x     = ~year,
    y     = ~score,
    color = ~dim_label,
    type  = "scatter", mode = "lines+markers",
    line   = list(width = 2),
    marker = list(size = 8),
    hovertemplate = "%{fullData.name}: %{y:.1f}<extra></extra>"
  ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis     = list(title = t("axis_year", lang)),
        yaxis     = list(title = t("axis_score", lang), range = c(0, 100)),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )))
    )
}

# =============================================================================
# 12. Country vs GCC Average
# =============================================================================

#' Country overall index vs GCC aggregate trend comparison
#'
#' @param dimension_scores  Full country-level data frame.
#' @param gcc_ts            GCC aggregate timeseries (col: overall).
#' @param country_name      English country name.
#' @param country_colors    Named colour vector.
#' @param lang              "en" or "ar".
chart_country_vs_gcc <- function(dimension_scores, gcc_ts, country_name,
                                  country_colors = .country_colors, lang = "en") {
  country_ts <- dimension_scores %>%
    filter(country == country_name) %>%
    select(year, overall_index)

  gcc_overall <- gcc_ts %>% select(year, overall)

  comparison <- left_join(country_ts, gcc_overall, by = "year")
  col        <- unname(country_colors[country_name])
  ctry_label <- translate_country(country_name, lang)
  gcc_label  <- t("label_gcc_average", lang)

  plot_ly(comparison) %>%
    add_trace(
      x    = ~year, y = ~overall_index,
      type = "scatter", mode = "lines+markers",
      name = ctry_label,
      line   = list(color = col, width = 3),
      marker = list(size = 10, color = col),
      hovertemplate = paste0(ctry_label, ": %{y:.1f}<extra></extra>")
    ) %>%
    add_trace(
      x    = ~year, y = ~overall,
      type = "scatter", mode = "lines+markers",
      name = gcc_label,
      line   = list(color = "black", width = 3, dash = "dash"),
      marker = list(size = 10, color = "black", symbol = "square"),
      hovertemplate = paste0(gcc_label, ": %{y:.1f}<extra></extra>")
    ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis     = list(title = t("axis_year", lang)),
        yaxis     = list(title = t("axis_overall_index", lang), range = c(0, 100)),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )))
    )
}

# =============================================================================
# 13. Country × Dimension Heatmap
# =============================================================================

#' Country × dimension score heatmap for a given year
#'
#' @param dimension_scores   Full country-level data frame.
#' @param selected_year      Integer year.
#' @param selected_countries Character vector of country names (English).
#' @param selected_dims      Character vector of dimension column names.
#' @param lang               "en" or "ar".
chart_country_heatmap <- function(dimension_scores, selected_year,
                                   selected_countries, selected_dims,
                                   lang = "en") {
  heatmap_data <- dimension_scores %>%
    filter(year == selected_year, country %in% selected_countries) %>%
    select(country, all_of(selected_dims)) %>%
    pivot_longer(
      cols      = all_of(selected_dims),
      names_to  = "dimension",
      values_to = "score"
    ) %>%
    mutate(
      dim_label     = translate_dimensions(
        .dim_labels_en[match(dimension, .dim_cols)], lang = lang
      ),
      country_label = translate_countries(country, lang = lang)
    )

  heat_matrix <- heatmap_data %>%
    select(country_label, dim_label, score) %>%
    pivot_wider(names_from = dim_label, values_from = score) %>%
    column_to_rownames("country_label") %>%
    as.matrix()

  plot_ly(
    x    = colnames(heat_matrix),
    y    = rownames(heat_matrix),
    z    = heat_matrix,
    type = "heatmap",
    colorscale = "Viridis",
    text = round(heat_matrix, 1),
    hovertemplate = paste0(
      t("axis_country", lang), ": %{y}<br>",
      t("axis_dimension", lang), ": %{x}<br>",
      t("axis_score", lang), ": %{z:.1f}<extra></extra>"
    )
  ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis  = list(title = t("axis_dimension", lang)),
        yaxis  = list(title = t("axis_country", lang)),
        margin = list(l = 160, r = 20, t = 40, b = 80)
      )))
    )
}

# =============================================================================
# 14. Rankings Bump Chart
# =============================================================================

#' Country rankings across dimensions for a given year
#'
#' @param dimension_scores   Full country-level data frame.
#' @param selected_year      Integer year.
#' @param selected_dims      Character vector of dimension column names.
#' @param country_colors     Named colour vector.
#' @param lang               "en" or "ar".
chart_rankings <- function(dimension_scores, selected_year, selected_dims,
                            country_colors = .country_colors, lang = "en") {
  ranking_data <- dimension_scores %>%
    filter(year == selected_year) %>%
    select(country, all_of(selected_dims)) %>%
    pivot_longer(
      cols      = all_of(selected_dims),
      names_to  = "dimension",
      values_to = "score"
    ) %>%
    mutate(
      dim_label     = translate_dimensions(
        .dim_labels_en[match(dimension, .dim_cols)], lang = lang
      ),
      country_label = translate_countries(country, lang = lang)
    ) %>%
    group_by(dim_label) %>%
    arrange(desc(score)) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  countries_en <- unique(ranking_data$country)
  p <- plot_ly()

  for (ctry in countries_en) {
    ctry_data  <- ranking_data %>% filter(country == ctry)
    ctry_label <- translate_country(ctry, lang)
    col        <- unname(country_colors[ctry])

    p <- p %>%
      add_trace(
        data = ctry_data,
        x    = ~dim_label, y = ~rank,
        type = "scatter", mode = "markers+lines",
        name = ctry_label,
        line   = list(color = col, width = 2),
        marker = list(color = col, size = 12),
        hovertemplate = paste0(ctry_label, " \u2014 ", t("axis_rank", lang), ": %{y}<extra></extra>")
      )
  }

  p %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis  = list(title = t("axis_dimension", lang)),
        yaxis  = list(title = t("axis_rank", lang), autorange = "reversed"),
        hovermode = "closest",
        legend = list(orientation = "h", y = -0.2)
      )))
    )
}

# =============================================================================
# 15. YoY Overall Change (horizontal bar)
# =============================================================================

#' Year-over-year change in overall index for a given year
#'
#' @param yoy_changes    Data frame with cols: year, country, overall_change.
#' @param selected_year  Integer.
#' @param lang           "en" or "ar".
chart_yoy_overall <- function(yoy_changes, selected_year, lang = "en") {
  changes <- yoy_changes %>%
    filter(year == selected_year) %>%
    mutate(country_label = translate_countries(country, lang = lang)) %>%
    arrange(desc(overall_change))

  max_abs <- max(abs(changes$overall_change), na.rm = TRUE)

  plot_ly(
    changes,
    x           = ~overall_change,
    y           = ~reorder(country_label, overall_change),
    type        = "bar",
    orientation = "h",
    marker      = list(
      color      = ~overall_change,
      colorscale = list(c(0, "red"), c(0.5, "white"), c(1, "green")),
      cmin       = -max_abs,
      cmax       =  max_abs,
      colorbar   = list(title = t("axis_colorbar_change", lang))
    ),
    hovertemplate = paste0(
      "%{y}: %{x:+.2f}<extra></extra>"
    )
  ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis  = list(title = t("axis_change_overall", lang)),
        yaxis  = list(title = ""),
        margin = list(l = 160, r = 20, t = 40, b = 60)
      )))
    )
}

# =============================================================================
# 16. YoY Changes by Dimension (scatter / connected dots)
# =============================================================================

#' Year-over-year change by dimension for all countries, given year
#'
#' @param yoy_changes    Data frame with _change suffix columns.
#' @param selected_year  Integer.
#' @param country_colors Named colour vector.
#' @param lang           "en" or "ar".
chart_yoy_dimensions <- function(yoy_changes, selected_year,
                                  country_colors = .country_colors, lang = "en") {
  changes <- yoy_changes %>%
    filter(year == selected_year) %>%
    select(country, ends_with("_change"), -overall_change) %>%
    pivot_longer(
      cols      = ends_with("_change"),
      names_to  = "dimension",
      values_to = "change"
    ) %>%
    mutate(
      dim_col   = sub("_change$", "", dimension),
      dim_label = translate_dimensions(
        .dim_labels_en[match(dim_col, .dim_cols)], lang = lang
      )
    )

  countries_en <- unique(changes$country)
  p <- plot_ly()

  for (ctry in countries_en) {
    ctry_data  <- changes %>% filter(country == ctry)
    ctry_label <- translate_country(ctry, lang)
    col        <- unname(country_colors[ctry])

    p <- p %>%
      add_trace(
        data = ctry_data,
        x    = ~dim_label, y = ~change,
        type = "scatter", mode = "markers+lines",
        name = ctry_label,
        line   = list(color = col, width = 2),
        marker = list(color = col, size = 10),
        hovertemplate = paste0(ctry_label, " \u2014 %{x}: %{y:+.2f}<extra></extra>")
      )
  }

  n_dims <- length(unique(changes$dim_label))
  p %>%
    add_segments(
      x = 0, xend = n_dims - 1, y = 0, yend = 0,
      line      = list(color = "black", dash = "dash", width = 1),
      showlegend = FALSE,
      inherit    = FALSE
    ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis     = list(title = t("axis_dimension", lang)),
        yaxis     = list(title = t("axis_yoy_change", lang)),
        hovermode = "closest",
        legend    = list(orientation = "h", y = -0.2)
      )))
    )
}

# =============================================================================
# 17. Contribution to Total Change (grouped bar)
# =============================================================================

#' Dimension-level contribution to GCC index change for a given year
#'
#' @param dimension_scores  Full country-level data frame.
#' @param selected_year     Integer.
#' @param country_colors    Named colour vector.
#' @param lang              "en" or "ar".
chart_contribution <- function(dimension_scores, selected_year,
                                country_colors = .country_colors, lang = "en") {
  prev_year <- selected_year - 1

  current <- dimension_scores %>%
    filter(year == selected_year) %>%
    select(country, all_of(.dim_cols))

  prev <- dimension_scores %>%
    filter(year == prev_year) %>%
    select(country, all_of(.dim_cols))

  changes <- current %>%
    left_join(prev, by = "country", suffix = c("_cur", "_prv")) %>%
    pivot_longer(
      cols      = -country,
      names_to  = "metric",
      values_to = "value"
    ) %>%
    mutate(
      period    = ifelse(grepl("_cur$", metric), "cur", "prv"),
      dim_col   = sub("_(cur|prv)$", "", metric)
    ) %>%
    pivot_wider(names_from = period, values_from = value) %>%
    mutate(
      change    = cur - prv,
      dim_label = translate_dimensions(
        .dim_labels_en[match(dim_col, .dim_cols)], lang = lang
      ),
      country_label = translate_countries(country, lang = lang)
    )

  countries_en <- unique(changes$country)
  p <- plot_ly()

  for (ctry in countries_en) {
    ctry_data  <- changes %>% filter(country == ctry)
    ctry_label <- translate_country(ctry, lang)
    col        <- unname(country_colors[ctry])

    p <- p %>%
      add_trace(
        data = ctry_data,
        x    = ~dim_label, y = ~change,
        type = "bar",
        name = ctry_label,
        marker = list(color = col),
        hovertemplate = paste0(ctry_label, " \u2014 %{x}: %{y:+.2f}<extra></extra>")
      )
  }

  p %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis   = list(title = t("axis_dimension", lang)),
        yaxis   = list(title = t("axis_contribution", lang)),
        barmode = "group",
        legend  = list(orientation = "h", y = -0.2)
      )))
    )
}

# =============================================================================
# 18. Annual Change Trends (mean / min / max across all years)
# =============================================================================

#' GCC-wide annual change trends across all available years
#'
#' @param yoy_changes  Data frame with cols: year, country, overall_change.
#' @param lang         "en" or "ar".
chart_annual_trends <- function(yoy_changes, lang = "en") {
  all_changes <- yoy_changes %>%
    group_by(year) %>%
    summarise(
      mean_change = mean(overall_change, na.rm = TRUE),
      max_change  = max(overall_change,  na.rm = TRUE),
      min_change  = min(overall_change,  na.rm = TRUE),
      .groups     = "drop"
    )

  avg_label <- if (lang == "ar") "\u0645\u062a\u0648\u0633\u0637 \u0627\u0644\u062a\u063a\u064a\u0631"
               else "Average Change"
  max_label <- if (lang == "ar") "\u0623\u0642\u0635\u0649 \u062a\u063a\u064a\u0631"
               else "Max Change"
  min_label <- if (lang == "ar") "\u0623\u062f\u0646\u0649 \u062a\u063a\u064a\u0631"
               else "Min Change"

  plot_ly(all_changes, x = ~year) %>%
    add_trace(
      y    = ~mean_change, type = "scatter", mode = "lines+markers",
      name = avg_label,
      line   = list(color = "blue", width = 3),
      marker = list(size = 10, color = "blue"),
      hovertemplate = paste0(avg_label, ": %{y:+.2f}<extra></extra>")
    ) %>%
    add_trace(
      y    = ~max_change, type = "scatter", mode = "lines",
      name = max_label,
      line = list(color = "green", width = 2, dash = "dash"),
      hovertemplate = paste0(max_label, ": %{y:+.2f}<extra></extra>")
    ) %>%
    add_trace(
      y    = ~min_change, type = "scatter", mode = "lines",
      name = min_label,
      line = list(color = "red", width = 2, dash = "dash"),
      hovertemplate = paste0(min_label, ": %{y:+.2f}<extra></extra>")
    ) %>%
    add_segments(
      x         = ~min(year), xend = ~max(year), y = 0, yend = 0,
      line      = list(color = "black", dash = "dot", width = 1),
      showlegend = FALSE,
      inherit    = FALSE
    ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis     = list(title = t("axis_year", lang)),
        yaxis     = list(title = t("axis_change_overall", lang)),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )))
    )
}

# =============================================================================
# 19. Change Heatmap (country × year, overall_change)
# =============================================================================

#' Heatmap of overall index year-on-year change, all countries × all years
#'
#' @param yoy_changes  Data frame with cols: year, country, overall_change.
#' @param lang         "en" or "ar".
chart_change_heatmap <- function(yoy_changes, lang = "en") {
  change_wide <- yoy_changes %>%
    mutate(country_label = translate_countries(country, lang = lang)) %>%
    select(country_label, year, overall_change) %>%
    pivot_wider(names_from = year, values_from = overall_change) %>%
    column_to_rownames("country_label") %>%
    as.matrix()

  plot_ly(
    x    = colnames(change_wide),
    y    = rownames(change_wide),
    z    = change_wide,
    type = "heatmap",
    colorscale = list(c(0, "red"), c(0.5, "white"), c(1, "green")),
    zmid = 0,
    text = round(change_wide, 1),
    hovertemplate = paste0(
      t("axis_country", lang), ": %{y}<br>",
      t("axis_year", lang),    ": %{x}<br>",
      t("axis_change", lang),  ": %{z:+.2f}<extra></extra>"
    )
  ) %>%
    layout(
      do.call(.base_layout, c(list(lang), list(
        xaxis  = list(title = t("axis_year", lang)),
        yaxis  = list(title = t("axis_country", lang)),
        margin = list(l = 160, r = 20, t = 40, b = 60)
      )))
    )
}
