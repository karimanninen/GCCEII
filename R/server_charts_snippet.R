# =============================================================================
# server_charts_snippet.R
# Drop-in replacements for all renderPlotly / renderUI chart calls in app.R
# =============================================================================
# Assumes:
#   source("R/translations.R")
#   source("R/mod_charts.R")
#   current_lang <- reactiveVal("en")   # set up per app_wiring_snippet.R
# =============================================================================

# ── Overview tab ──────────────────────────────────────────────────────────────

output$latest_overall <- renderValueBox({
  latest_gcc <- gcc_ts %>% filter(year == max(year))
  valueBox(
    value    = round(latest_gcc$overall, 1),
    subtitle = t("vb_gcc_index", current_lang()),
    icon     = icon("chart-line"),
    color    = "blue"
  )
})

output$latest_year <- renderValueBox({
  valueBox(
    value    = max(gcc_ts$year),
    subtitle = t("vb_latest_year", current_lang()),
    icon     = icon("calendar"),
    color    = "green"
  )
})

output$num_countries <- renderValueBox({
  valueBox(
    value    = length(countries),
    subtitle = t("vb_members", current_lang()),
    icon     = icon("flag"),
    color    = "yellow"
  )
})

# ── GCC Overall tab ───────────────────────────────────────────────────────────

output$gcc_overall_gauge <- renderPlotly({
  chart_gcc_gauge(gcc_ts, lang = current_lang())
})

output$gcc_dimension_bars <- renderPlotly({
  chart_dimension_bars(gcc_ts, lang = current_lang())
})

output$dimension_boxes <- renderUI({
  chart_dimension_scoreboxes(gcc_ts, lang = current_lang())
})

output$gcc_overall_trend <- renderPlotly({
  chart_gcc_trend(gcc_ts, lang = current_lang())
})

# ── GCC Timeseries tab ────────────────────────────────────────────────────────

output$gcc_dimension_timeseries <- renderPlotly({
  req(input$dimension_select)
  chart_gcc_dimension_ts(
    gcc_ts,
    selected_cols = input$dimension_select,
    show_overall  = input$show_overall,
    lang          = current_lang()
  )
})

output$overall_index_timeseries <- renderPlotly({
  chart_overall_ts_countries(dimension_scores, lang = current_lang())
})

output$dimension_radar <- renderPlotly({
  latest_gcc <- gcc_ts %>% filter(year == max(year))
  scores <- c(
    latest_gcc$trade_score, latest_gcc$financial_score,
    latest_gcc$labor_score, latest_gcc$infrastructure_score,
    latest_gcc$sustainability_score, latest_gcc$convergence_score
  )
  chart_radar(scores, label = "GCC", lang = current_lang())
})

output$dimension_correlation <- renderPlotly({
  chart_correlation(gcc_ts, lang = current_lang())
})

# ── Country Profiles tab ──────────────────────────────────────────────────────

country_data_reactive <- reactive({
  req(input$country_select)
  dimension_scores %>% filter(country == input$country_select)
})

output$country_overall <- renderValueBox({
  latest <- country_data_reactive() %>% filter(year == max(year))
  valueBox(
    value    = round(latest$overall_index, 1),
    subtitle = t("vb_overall_index", current_lang()),
    icon     = icon("chart-line"),
    color    = "blue"
  )
})

output$country_rank <- renderValueBox({
  latest_year <- max(dimension_scores$year)
  rank_data <- dimension_scores %>%
    filter(year == latest_year) %>%
    arrange(desc(overall_index)) %>%
    mutate(rank = row_number())
  country_rank <- rank_data %>%
    filter(country == input$country_select) %>%
    pull(rank)
  valueBox(
    value    = paste0("#", country_rank, " / ", nrow(rank_data)),
    subtitle = t("vb_country_rank", current_lang()),
    icon     = icon("trophy"),
    color    = "yellow"
  )
})

output$country_level <- renderValueBox({
  latest <- country_data_reactive() %>% filter(year == max(year))
  valueBox(
    value    = translate_level(latest$integration_level, current_lang()),
    subtitle = t("vb_integration_lvl", current_lang()),
    icon     = icon("signal"),
    color    = if (latest$integration_level == "Good") "green" else "orange"
  )
})

output$country_change <- renderValueBox({
  data <- country_data_reactive()
  if (nrow(data) >= 2) {
    latest   <- data %>% filter(year == max(year)) %>% pull(overall_index)
    previous <- data %>% filter(year == max(year) - 1) %>% pull(overall_index)
    change   <- latest - previous
    valueBox(
      value    = paste0(ifelse(change > 0, "+", ""), round(change, 1)),
      subtitle = t("vb_yoy_overall", current_lang()),
      icon     = icon(ifelse(change > 0, "arrow-up", "arrow-down")),
      color    = ifelse(change > 0, "green", "red")
    )
  } else {
    valueBox(
      value    = "N/A",
      subtitle = t("vb_yoy_overall", current_lang()),
      icon     = icon("minus"),
      color    = "gray"
    )
  }
})

output$country_overall_trend <- renderPlotly({
  chart_country_trend(
    country_data_reactive(),
    country_name = input$country_select,
    lang         = current_lang()
  )
})

output$country_dimension_current <- renderPlotly({
  latest <- country_data_reactive() %>% filter(year == max(year))
  chart_country_dims_current(
    latest,
    country_name = input$country_select,
    lang         = current_lang()
  )
})

output$country_all_dimensions <- renderPlotly({
  chart_country_all_dims(country_data_reactive(), lang = current_lang())
})

output$country_vs_gcc <- renderPlotly({
  chart_country_vs_gcc(
    dimension_scores,
    gcc_ts,
    country_name = input$country_select,
    lang         = current_lang()
  )
})

# ── Country Heatmap tab ───────────────────────────────────────────────────────

output$country_dimension_heatmap <- renderPlotly({
  req(input$heatmap_countries, input$heatmap_dimensions)
  chart_country_heatmap(
    dimension_scores,
    selected_year      = input$heatmap_year,
    selected_countries = input$heatmap_countries,
    selected_dims      = input$heatmap_dimensions,
    lang               = current_lang()
  )
})

output$dimension_rankings <- renderPlotly({
  req(input$heatmap_dimensions)
  chart_rankings(
    dimension_scores,
    selected_year = input$heatmap_year,
    selected_dims = input$heatmap_dimensions,
    lang          = current_lang()
  )
})

# ── GCC Analytics tab ─────────────────────────────────────────────────────────

output$yoy_overall_changes <- renderPlotly({
  chart_yoy_overall(yoy_changes, selected_year = input$analytics_year,
                    lang = current_lang())
})

output$yoy_dimension_changes <- renderPlotly({
  chart_yoy_dimensions(yoy_changes, selected_year = input$analytics_year,
                        lang = current_lang())
})

output$contribution_analysis <- renderPlotly({
  chart_contribution(dimension_scores, selected_year = input$analytics_year,
                     lang = current_lang())
})

output$annual_change_trends <- renderPlotly({
  chart_annual_trends(yoy_changes, lang = current_lang())
})

output$change_heatmap <- renderPlotly({
  chart_change_heatmap(yoy_changes, lang = current_lang())
})
