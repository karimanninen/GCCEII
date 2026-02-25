# ==============================================================================
# GCC ECONOMIC INTEGRATION DASHBOARD
# Integration Pathway | GCC Economic Observatory
# Version: 4.0 - COINr Pipeline Integration
# Date: February 2026
# ==============================================================================
#
# A comprehensive Shiny dashboard for analyzing GCC economic integration
# across six member states (Bahrain, Kuwait, Oman, Qatar, Saudi Arabia, UAE)
#
# Features:
# - ARIIP-style landing page with rotating quotes carousel
# - 7 interactive tabs with 25+ visualizations
# - Time series analysis (2015-2024)
# - 32 indicators across 6 dimensions (COINr framework)
# - Country comparisons and rankings
# - Year-over-year change analytics
# - Dual data source: gcceii_scores.csv (primary) or Fusion Registry
#
# ==============================================================================

# Load required packages
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(plotly)
library(tidyr)
library(DT)
library(scales)
library(viridis)

# Source modular components
source("R/utils.R")
source("R/data_loading.R")
source("R/mod_landing_page.R")
source("R/mod_metadata.R")
source("R/mod_charts.R")
source("R/translations.R")

# ==============================================================================
# LOAD DATA
# ==============================================================================

gcc_data <- load_gcc_data("output")
dimension_scores <- gcc_data$dimension_scores
gcc_ts <- gcc_data$gcc_ts
yoy_changes <- gcc_data$yoy_changes
country_data <- gcc_data$country_data
indicator_detail <- gcc_data$indicator_detail  # NULL for legacy data sources

# Get unique countries
countries <- get_countries(dimension_scores)

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(

  # Link external CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "rtl.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Noto+Sans+Arabic:wght@400;600;700&display=swap")
  ),

  # Conditional display: Landing Page OR Dashboard
  uiOutput("main_ui"),

  # Language direction JavaScript
  tags$script(HTML("
    Shiny.addCustomMessageHandler('setLang', function(lang) {
      if (lang === 'ar') {
        $('body').addClass('lang-ar').attr('dir', 'rtl');
      } else {
        $('body').removeClass('lang-ar').attr('dir', 'ltr');
      }
    });
  "))
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Navigation State
  # ---------------------------------------------------------------------------

  entered_dashboard <- reactiveVal(FALSE)
  current_lang <- reactiveVal("en")

  observeEvent(input$lang_toggle, {
    new_lang <- if (current_lang() == "en") "ar" else "en"
    current_lang(new_lang)
  })

  observe({
    session$sendCustomMessage("setLang", current_lang())
  })

  observeEvent(input$enter_dashboard, { entered_dashboard(TRUE) })
  observeEvent(input$back_to_welcome, { entered_dashboard(FALSE) })

  # ---------------------------------------------------------------------------
  # Main UI Renderer
  # ---------------------------------------------------------------------------

  output$main_ui <- renderUI({
    lang <- current_lang()
    if (!entered_dashboard()) {
      landing_page_ui(lang)
    } else {
      dashboard_ui(lang)
    }
  })

  # ---------------------------------------------------------------------------
  # Overview Tab Outputs
  # ---------------------------------------------------------------------------

  output$latest_overall <- renderValueBox({
    latest <- gcc_ts %>% filter(year == max(year))
    valueBox(
      value = round(latest$overall, 1),
      subtitle = t("vb_overall_score", current_lang()),
      icon = icon("chart-line"),
      color = "blue"
    )
  })

  output$latest_year <- renderValueBox({
    valueBox(
      value = max(dimension_scores$year),
      subtitle = t("vb_latest_year", current_lang()),
      icon = icon("calendar"),
      color = "green"
    )
  })

  output$num_countries <- renderValueBox({
    valueBox(
      value = length(countries),
      subtitle = t("vb_member_states", current_lang()),
      icon = icon("flag"),
      color = "purple"
    )
  })

  # ---------------------------------------------------------------------------
  # Metadata Tab Outputs
  # ---------------------------------------------------------------------------

  output$dimension_weights_table <- renderTable({
    get_dimension_weights_table(lang = current_lang())
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$trade_indicators_table <- renderTable({
    get_trade_indicators_table(lang = current_lang())
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$financial_indicators_table <- renderTable({
    get_financial_indicators_table(lang = current_lang())
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$labor_indicators_table <- renderTable({
    get_labor_indicators_table(lang = current_lang())
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$infrastructure_indicators_table <- renderTable({
    get_infrastructure_indicators_table(lang = current_lang())
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$sustainability_indicators_table <- renderTable({
    get_sustainability_indicators_table(lang = current_lang())
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$convergence_indicators_table <- renderTable({
    get_convergence_indicators_table(lang = current_lang())
  }, striped = TRUE, hover = TRUE, width = "100%")

  # ---------------------------------------------------------------------------
  # GCC Overall Tab Outputs
  # ---------------------------------------------------------------------------

  output$gcc_overall_gauge <- renderPlotly({
    lang <- current_lang()
    latest <- gcc_ts %>% filter(year == max(year))
    prev <- gcc_ts %>% filter(year == max(year) - 1) %>% pull(overall)
    create_gauge_chart(latest$overall, prev, paste(t("gcc_gauge_title", lang), max(gcc_ts$year)))
  })

  output$gcc_dimension_bars <- renderPlotly({
    latest <- gcc_ts %>% filter(year == max(year))
    dim_data <- create_dimension_df(latest)
    create_dimension_bar_chart(dim_data, lang = current_lang())
  })

  output$dimension_boxes <- renderUI({
    lang <- current_lang()
    latest <- gcc_ts %>% filter(year == max(year))
    scores <- get_scores_vector(latest)

    tagList(
      fluidRow(
        lapply(1:3, function(i) {
          column(4,
            div(class = "info-box",
              span(class = paste("info-box-icon bg", DIMENSION_BOX_COLORS[i], sep = "-"),
                   icon(DIMENSION_ICONS[i])),
              div(class = "info-box-content",
                span(class = "info-box-text", translate_dimension(DIMENSION_LABELS[i], lang)),
                span(class = "info-box-number", round(scores[i], 1))
              )
            )
          )
        })
      ),
      fluidRow(
        lapply(4:6, function(i) {
          column(4,
            div(class = "info-box",
              span(class = paste("info-box-icon bg", DIMENSION_BOX_COLORS[i], sep = "-"),
                   icon(DIMENSION_ICONS[i])),
              div(class = "info-box-content",
                span(class = "info-box-text", translate_dimension(DIMENSION_LABELS[i], lang)),
                span(class = "info-box-number", round(scores[i], 1))
              )
            )
          )
        })
      )
    )
  })

  output$country_ranking <- renderPlotly({
    latest_year <- max(dimension_scores$year)
    ranking <- dimension_scores %>%
      filter(year == latest_year) %>%
      arrange(desc(overall_index))
    create_ranking_chart(ranking, title = paste(t("gcc_ranking_title", current_lang()), "-", latest_year), lang = current_lang())
  })

  # ---------------------------------------------------------------------------
  # GCC Timeseries Tab Outputs
  # ---------------------------------------------------------------------------

  # -- Reactive: selected dimension (default: Trade) --
  selected_dimension <- reactiveVal("Trade")

  observeEvent(input$dim_btn_Trade, { selected_dimension("Trade") })
  observeEvent(input$dim_btn_Financial, { selected_dimension("Financial") })
  observeEvent(input$dim_btn_Labor, { selected_dimension("Labor") })
  observeEvent(input$dim_btn_Infrastructure, { selected_dimension("Infrastructure") })
  observeEvent(input$dim_btn_Sustainability, { selected_dimension("Sustainability") })
  observeEvent(input$dim_btn_Convergence, { selected_dimension("Convergence") })

  # -- Summary header bar --
  output$ts_summary_bar <- renderUI({
    lang <- current_lang()
    latest <- gcc_ts %>% filter(year == max(year))
    prev <- gcc_ts %>% filter(year == max(year) - 1)
    change <- round(latest$overall - prev$overall, 1)
    change_class <- if (change >= 0) "positive" else "negative"
    change_arrow <- if (change >= 0) "\u25b2" else "\u25bc"
    level <- latest$integration_level
    level_class <- tolower(level)

    div(class = "ts-summary-bar",
      div(class = "ts-summary-item",
        div(class = "ts-summary-value", round(latest$overall, 1)),
        div(class = "ts-summary-label", paste(t("ts_summary_gcc_score", lang), max(gcc_ts$year)))
      ),
      div(class = "ts-summary-item",
        div(class = paste("ts-summary-change", change_class),
          paste(change_arrow, abs(change), t("label_pts_from", lang), max(gcc_ts$year) - 1)),
        div(class = "ts-summary-label", t("ts_summary_yoy", lang))
      ),
      div(class = "ts-summary-item",
        span(class = paste("ts-level-badge", level_class), translate_level(level, lang)),
        div(class = "ts-summary-label", t("ts_summary_level", lang))
      ),
      div(class = "ts-summary-item",
        div(class = "ts-summary-value", paste0(min(gcc_ts$year), "\u2013", max(gcc_ts$year))),
        div(class = "ts-summary-label", t("ts_summary_coverage", lang))
      )
    )
  })

  # -- Overall trend (compact) --
  output$gcc_overall_trend <- renderPlotly({
    create_line_chart(gcc_ts, "year", "overall", lang = current_lang())
  })

  # -- Dynamic titles --
  output$ts_dimension_title <- renderUI({
    dim <- selected_dimension()
    lang <- current_lang()
    tags$span(paste(translate_dimension(dim, lang), t("ts_dim_country_compare", lang)))
  })

  output$ts_indicators_title <- renderUI({
    dim <- selected_dimension()
    lang <- current_lang()
    tags$span(paste(translate_dimension(dim, lang), t("ts_dim_indicators", lang)))
  })

  # -- Dimension country comparison chart --
  output$ts_dimension_countries <- renderPlotly({
    dim <- selected_dimension()
    lang <- current_lang()
    col_name <- DIMENSION_CODE_MAP[dim]

    p <- plot_ly()

    # Country lines (thin, colored)
    for (ctry in countries) {
      ctry_data <- dimension_scores %>% dplyr::filter(country == ctry)
      p <- p %>% add_trace(
        data = ctry_data, x = ~year,
        y = as.formula(paste0("~", col_name)),
        name = translate_country(ctry, lang), type = 'scatter', mode = 'lines+markers',
        line = list(color = COUNTRY_COLORS[ctry], width = 1.5),
        marker = list(size = 5),
        legendgroup = ctry
      )
    }

    # GCC aggregate (thick, gold, dashed)
    p <- p %>% add_trace(
      data = gcc_ts, x = ~year,
      y = as.formula(paste0("~", col_name)),
      name = translate_country("GCC", lang), type = 'scatter', mode = 'lines+markers',
      line = list(color = COUNTRY_COLORS_WITH_GCC["GCC"], width = 3.5, dash = "dash"),
      marker = list(size = 9, symbol = "diamond",
                    color = COUNTRY_COLORS_WITH_GCC["GCC"]),
      legendgroup = "GCC"
    )

    p %>% layout(
      xaxis = list(title = t("axis_year", lang), dtick = 1),
      yaxis = list(title = paste(translate_dimension(dim, lang), t("axis_score", lang)), range = c(0, 100)),
      hovermode = 'x unified',
      legend = list(orientation = 'h', y = -0.15)
    )
  })

  # -- Indicator small multiples (GCC aggregate, subplot grid) --
  output$ts_indicator_multiples <- renderPlotly({
    dim <- selected_dimension()
    lang <- current_lang()

    if (is.null(indicator_detail) || nrow(indicator_detail) == 0) {
      return(
        plot_ly() %>%
          layout(title = t("label_no_data", lang),
                 xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
      )
    }

    # Filter to GCC aggregate for the selected dimension
    gcc_ind <- indicator_detail %>%
      dplyr::filter(dimension == dim, country == "GCC") %>%
      dplyr::arrange(indicator_code, year)

    # Get unique indicators and their labels
    ind_info <- gcc_ind %>%
      dplyr::distinct(indicator_code, indicator_label) %>%
      dplyr::arrange(indicator_code)

    n_ind <- nrow(ind_info)
    if (n_ind == 0) {
      return(
        plot_ly() %>%
          layout(title = t("label_no_data", lang),
                 xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
      )
    }

    # Dimension-specific accent color for the small multiples
    dim_colors <- c(
      "Trade" = "#1565c0", "Financial" = "#f57f17",
      "Labor" = "#2e7d32", "Infrastructure" = "#c62828",
      "Sustainability" = "#6a1b9a", "Convergence" = "#00838f"
    )
    accent <- dim_colors[dim]

    # Build one subplot per indicator
    plots <- lapply(seq_len(n_ind), function(i) {
      ind_code <- ind_info$indicator_code[i]
      ind_label <- translate_indicator(ind_info$indicator_label[i], lang)
      ind_data <- gcc_ind %>% dplyr::filter(indicator_code == ind_code)

      plot_ly(ind_data, x = ~year, y = ~normalized_value,
              type = 'scatter', mode = 'lines+markers',
              line = list(color = accent, width = 2),
              marker = list(size = 5, color = accent),
              hovertemplate = paste0(
                "<b>", ind_label, "</b><br>",
                t("axis_year", lang), ": %{x}<br>",
                t("axis_score", lang), ": %{y:.1f}<br>",
                "<extra></extra>"
              ),
              showlegend = FALSE) %>%
        layout(
          annotations = list(
            list(text = ind_label, x = 0.5, y = 1.08,
                 xref = "paper", yref = "paper",
                 showarrow = FALSE,
                 font = list(size = 11, color = "#333"))
          ),
          xaxis = list(dtick = 2),
          yaxis = list(range = c(0, 100))
        )
    })

    n_cols <- min(3, n_ind)
    n_rows <- ceiling(n_ind / n_cols)

    # Calculate dynamic height: 250px per row
    chart_height <- n_rows * 250

    subplot(plots, nrows = n_rows, shareX = TRUE, shareY = TRUE,
            titleX = FALSE, titleY = FALSE, margin = 0.06) %>%
      layout(
        yaxis = list(title = paste0(t("axis_score", lang), " (0\u2013100)"), range = c(0, 100)),
        margin = list(t = 30, b = 40),
        height = chart_height
      )
  })

  # ---------------------------------------------------------------------------
  # Country Profiles Tab Outputs
  # ---------------------------------------------------------------------------

  output$country_overall <- renderValueBox({
    latest <- dimension_scores %>%
      filter(country == input$selected_country, year == max(year))
    valueBox(
      value = round(latest$overall_index, 1),
      subtitle = paste(translate_country(input$selected_country, current_lang()), t("vb_country_overall", current_lang())),
      icon = icon("chart-line"),
      color = "blue"
    )
  })

  output$country_rank <- renderValueBox({
    latest_year <- max(dimension_scores$year)
    ranking <- dimension_scores %>%
      filter(year == latest_year) %>%
      arrange(desc(overall_index)) %>%
      mutate(rank = row_number())

    rank_val <- ranking %>% filter(country == input$selected_country) %>% pull(rank)
    valueBox(
      value = paste("#", rank_val),
      subtitle = paste(t("vb_country_rank", current_lang()), latest_year),
      icon = icon("trophy"),
      color = "green"
    )
  })

  output$country_radar <- renderPlotly({
    latest <- dimension_scores %>%
      filter(country == input$selected_country, year == max(year))
    scores <- get_scores_vector(latest)
    create_radar_chart(scores, lang = current_lang())
  })

  # -- Combined Country vs GCC trend (replaces two separate charts) --
  output$country_vs_gcc_combined <- renderPlotly({
    ctry <- input$selected_country
    lang <- current_lang()
    ctry_data <- dimension_scores %>% filter(country == ctry)

    plot_ly() %>%
      add_trace(data = ctry_data, x = ~year, y = ~overall_index,
                name = translate_country(ctry, lang), type = 'scatter', mode = 'lines+markers',
                line = list(color = COUNTRY_COLORS[ctry], width = 3),
                marker = list(size = 10)) %>%
      add_trace(data = gcc_ts, x = ~year, y = ~overall,
                name = t("label_gcc_average", lang), type = 'scatter', mode = 'lines+markers',
                line = list(color = COUNTRY_COLORS_WITH_GCC["GCC"], width = 3, dash = 'dash'),
                marker = list(size = 8, symbol = "diamond")) %>%
      layout(
        xaxis = list(title = t("axis_year", lang), dtick = 1),
        yaxis = list(title = t("axis_overall", lang), range = c(0, 100)),
        hovermode = 'x unified',
        legend = list(orientation = 'h', y = -0.15)
      )
  })

  # -- Lollipop chart: country vs GCC indicator comparison --
  output$cp_indicator_lollipop <- renderPlotly({
    ctry <- input$selected_country
    dim <- input$cp_dimension_select
    lang <- current_lang()

    if (is.null(indicator_detail) || nrow(indicator_detail) == 0) {
      return(plot_ly() %>% layout(title = t("label_no_data", lang)))
    }

    latest_year <- max(indicator_detail$year)

    # Get country and GCC scores for each indicator in the dimension
    ctry_ind <- indicator_detail %>%
      dplyr::filter(country == ctry, dimension == dim, year == latest_year) %>%
      dplyr::select(indicator_code, indicator_label, ctry_score = normalized_value)

    gcc_ind <- indicator_detail %>%
      dplyr::filter(country == "GCC", dimension == dim, year == latest_year) %>%
      dplyr::select(indicator_code, gcc_score = normalized_value)

    combined <- ctry_ind %>%
      dplyr::left_join(gcc_ind, by = "indicator_code") %>%
      dplyr::arrange(ctry_score)

    if (nrow(combined) == 0) {
      return(plot_ly() %>% layout(title = t("label_no_data", lang)))
    }

    # Translate indicator labels and create ordered factor for y-axis
    combined$indicator_label <- translate_indicators(combined$indicator_label, lang)
    combined$indicator_label <- factor(combined$indicator_label,
                                       levels = combined$indicator_label)

    ctry_color <- COUNTRY_COLORS[ctry]
    ctry_label <- translate_country(ctry, lang)
    gcc_label <- t("label_gcc_average", lang)

    p <- plot_ly()

    # Connecting lines (gap segments)
    for (i in seq_len(nrow(combined))) {
      p <- p %>% add_trace(
        x = c(combined$ctry_score[i], combined$gcc_score[i]),
        y = c(combined$indicator_label[i], combined$indicator_label[i]),
        type = 'scatter', mode = 'lines',
        line = list(color = '#cccccc', width = 2),
        showlegend = FALSE, hoverinfo = 'skip'
      )
    }

    # Country dots
    p <- p %>% add_trace(
      data = combined, x = ~ctry_score, y = ~indicator_label,
      type = 'scatter', mode = 'markers',
      marker = list(size = 14, color = ctry_color,
                    line = list(color = 'white', width = 1.5)),
      name = ctry_label,
      hovertemplate = paste0("<b>%{y}</b><br>", ctry_label, ": %{x:.1f}<extra></extra>")
    )

    # GCC dots
    p <- p %>% add_trace(
      data = combined, x = ~gcc_score, y = ~indicator_label,
      type = 'scatter', mode = 'markers',
      marker = list(size = 14, color = COUNTRY_COLORS_WITH_GCC["GCC"],
                    symbol = "diamond",
                    line = list(color = 'white', width = 1.5)),
      name = gcc_label,
      hovertemplate = paste0("<b>%{y}</b><br>", gcc_label, ": %{x:.1f}<extra></extra>")
    )

    p %>% layout(
      xaxis = list(title = paste(t("axis_score", lang), "(", latest_year, ")"),
                   range = c(0, 105)),
      yaxis = list(title = "", automargin = TRUE),
      legend = list(orientation = 'h', y = -0.15),
      margin = list(l = 160)
    )
  })

  # -- Small multiples: indicator trends for selected country + GCC overlay --
  output$cp_indicator_trends <- renderPlotly({
    ctry <- input$selected_country
    dim <- input$cp_dimension_select
    lang <- current_lang()

    if (is.null(indicator_detail) || nrow(indicator_detail) == 0) {
      return(plot_ly() %>% layout(title = t("label_no_data", lang)))
    }

    # Filter indicator data for this dimension (country + GCC)
    ind_data <- indicator_detail %>%
      dplyr::filter(dimension == dim,
                    country %in% c(ctry, "GCC")) %>%
      dplyr::arrange(indicator_code, year)

    ind_info <- ind_data %>%
      dplyr::distinct(indicator_code, indicator_label) %>%
      dplyr::arrange(indicator_code)

    n_ind <- nrow(ind_info)
    if (n_ind == 0) {
      return(plot_ly() %>% layout(title = t("label_no_data", lang)))
    }

    ctry_color <- COUNTRY_COLORS[ctry]
    gcc_color <- COUNTRY_COLORS_WITH_GCC["GCC"]
    ctry_label <- translate_country(ctry, lang)
    gcc_label <- translate_country("GCC", lang)

    plots <- lapply(seq_len(n_ind), function(i) {
      ind_code <- ind_info$indicator_code[i]
      ind_label <- translate_indicator(ind_info$indicator_label[i], lang)

      ctry_series <- ind_data %>%
        dplyr::filter(indicator_code == ind_code, country == ctry)
      gcc_series <- ind_data %>%
        dplyr::filter(indicator_code == ind_code, country == "GCC")

      plot_ly() %>%
        add_trace(data = ctry_series, x = ~year, y = ~normalized_value,
                  type = 'scatter', mode = 'lines+markers',
                  line = list(color = ctry_color, width = 2),
                  marker = list(size = 4, color = ctry_color),
                  name = ctry_label, showlegend = (i == 1),
                  legendgroup = "ctry",
                  hovertemplate = paste0(
                    "<b>", ind_label, "</b><br>",
                    ctry_label, ": %{y:.1f}<extra></extra>")) %>%
        add_trace(data = gcc_series, x = ~year, y = ~normalized_value,
                  type = 'scatter', mode = 'lines+markers',
                  line = list(color = gcc_color, width = 2, dash = "dash"),
                  marker = list(size = 4, color = gcc_color, symbol = "diamond"),
                  name = gcc_label, showlegend = (i == 1),
                  legendgroup = "gcc",
                  hovertemplate = paste0(
                    "<b>", ind_label, "</b><br>",
                    gcc_label, ": %{y:.1f}<extra></extra>")) %>%
        layout(
          annotations = list(
            list(text = ind_label, x = 0.5, y = 1.08,
                 xref = "paper", yref = "paper",
                 showarrow = FALSE,
                 font = list(size = 11, color = "#333"))
          ),
          xaxis = list(dtick = 2),
          yaxis = list(range = c(0, 100))
        )
    })

    n_cols <- min(3, n_ind)
    n_rows <- ceiling(n_ind / n_cols)
    chart_height <- n_rows * 250

    subplot(plots, nrows = n_rows, shareX = TRUE, shareY = TRUE,
            titleX = FALSE, titleY = FALSE, margin = 0.06) %>%
      layout(
        yaxis = list(title = paste0(t("axis_score", lang), " (0\u2013100)"), range = c(0, 100)),
        margin = list(t = 30, b = 40),
        height = chart_height,
        legend = list(orientation = 'h', y = -0.08)
      )
  })

  # -- Dynamic titles for indicator detail section --
  output$cp_lollipop_title <- renderUI({
    dim <- input$cp_dimension_select
    ctry <- input$selected_country
    lang <- current_lang()
    tags$span(paste0(translate_country(ctry, lang), " vs ", t("label_gcc_average", lang),
                     " \u2014 ", translate_dimension(dim, lang)))
  })

  output$cp_trends_title <- renderUI({
    dim <- input$cp_dimension_select
    lang <- current_lang()
    tags$span(translate_dimension(dim, lang))
  })

  # ---------------------------------------------------------------------------
  # Country Heatmap Tab Outputs
  # ---------------------------------------------------------------------------

  output$country_heatmap <- renderPlotly({
    lang <- current_lang()
    heatmap_data <- dimension_scores %>%
      filter(year == input$heatmap_year) %>%
      select(country, all_of(DIMENSION_COLS))

    colnames(heatmap_data) <- c("country", translate_dimensions(DIMENSION_LABELS, lang))

    heatmap_data$country <- translate_countries(heatmap_data$country, lang)

    heatmap_matrix <- heatmap_data %>%
      column_to_rownames("country") %>%
      as.matrix()

    create_heatmap(heatmap_matrix, lang = lang)
  })

  output$integration_levels <- renderPlotly({
    lang <- current_lang()
    translated_levels <- c(
      translate_level("Weak", lang),
      translate_level("Moderate", lang),
      translate_level("Good", lang)
    )
    level_data <- dimension_scores %>%
      filter(year == input$heatmap_year) %>%
      mutate(
        level_color = INTEGRATION_LEVEL_COLORS[integration_level],
        integration_level = factor(translate_level(integration_level, lang),
                                   levels = translated_levels),
        country_label = translate_countries(country, lang)
      )

    plot_ly(level_data, x = ~country_label, y = ~overall_index, type = 'bar',
            marker = list(color = ~level_color)) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = t("axis_overall", lang), range = c(0, 100)),
        shapes = list(
          list(type = "line", x0 = -0.5, x1 = 5.5, y0 = 40, y1 = 40,
               line = list(color = "orange", dash = "dash")),
          list(type = "line", x0 = -0.5, x1 = 5.5, y0 = 60, y1 = 60,
               line = list(color = "green", dash = "dash"))
        )
      )
  })

  # ---------------------------------------------------------------------------
  # GCC Analytics Tab Outputs
  # ---------------------------------------------------------------------------

  # -- Dynamic titles --
  output$analytics_waterfall_title <- renderUI({
    lang <- current_lang()
    tags$span(paste0(t("an_waterfall_title", lang), " (",
                     input$analytics_from_year, " \u2192 ", input$analytics_to_year, ")"))
  })
  output$analytics_country_contrib_title <- renderUI({
    lang <- current_lang()
    tags$span(paste0(t("an_country_contrib_title", lang), " (",
                     input$analytics_from_year, " \u2192 ", input$analytics_to_year, ")"))
  })
  output$analytics_spread_title <- renderUI({
    lang <- current_lang()
    tags$span(paste(t("an_spread_title", lang), "\u2014", max(dimension_scores$year)))
  })
  output$analytics_movers_title <- renderUI({
    lang <- current_lang()
    ly <- max(dimension_scores$year)
    tags$span(paste0(t("an_movers_title", lang), " (", ly - 1, " \u2192 ", ly, ")"))
  })

  # ===== Section 1: What Changed? =====

  # -- Waterfall chart: dimension-level decomposition --
  output$analytics_waterfall <- renderPlotly({
    lang <- current_lang()
    yr_from <- as.integer(input$analytics_from_year)
    yr_to <- as.integer(input$analytics_to_year)

    gcc_from <- gcc_ts %>% dplyr::filter(year == yr_from)
    gcc_to <- gcc_ts %>% dplyr::filter(year == yr_to)

    if (nrow(gcc_from) == 0 || nrow(gcc_to) == 0) {
      return(plot_ly() %>% layout(title = t("label_no_data", lang)))
    }

    # Weighted dimension changes
    dim_changes <- data.frame(
      dimension = translate_dimensions(DIMENSION_LABELS, lang),
      change = sapply(DIMENSION_COLS, function(col) {
        (gcc_to[[col]] - gcc_from[[col]])
      }) * DIMENSION_WEIGHTS,
      stringsAsFactors = FALSE
    )
    # Sort by absolute change descending for visual clarity
    dim_changes <- dim_changes[order(-abs(dim_changes$change)), ]

    # Build waterfall data
    from_val <- gcc_from$overall
    to_val <- gcc_to$overall

    # Construct the bar positions
    labels <- c(as.character(yr_from), dim_changes$dimension, as.character(yr_to))
    n <- length(labels)

    base_vals <- numeric(n)
    bar_vals <- numeric(n)
    bar_colors <- character(n)

    # First bar: starting index
    base_vals[1] <- 0
    bar_vals[1] <- from_val
    bar_colors[1] <- "#003366"

    # Middle bars: dimension changes
    running <- from_val
    for (i in seq_len(nrow(dim_changes))) {
      ch <- dim_changes$change[i]
      if (ch >= 0) {
        base_vals[i + 1] <- running
        bar_vals[i + 1] <- ch
        bar_colors[i + 1] <- "#4caf50"
      } else {
        base_vals[i + 1] <- running + ch
        bar_vals[i + 1] <- abs(ch)
        bar_colors[i + 1] <- "#f44336"
      }
      running <- running + ch
    }

    # Last bar: ending index
    base_vals[n] <- 0
    bar_vals[n] <- to_val
    bar_colors[n] <- "#003366"

    labels_factor <- factor(labels, levels = labels)

    # Invisible base bars
    plot_ly() %>%
      add_trace(x = labels_factor, y = base_vals,
                type = 'bar', marker = list(color = 'rgba(0,0,0,0)'),
                showlegend = FALSE, hoverinfo = 'skip') %>%
      # Visible bars
      add_trace(x = labels_factor, y = bar_vals,
                type = 'bar', marker = list(color = bar_colors),
                showlegend = FALSE,
                text = round(bar_vals, 1),
                textposition = 'outside',
                hovertemplate = "<b>%{x}</b><br>Value: %{y:.1f}<extra></extra>") %>%
      layout(
        barmode = 'stack',
        xaxis = list(title = ""),
        yaxis = list(title = t("axis_score", lang), range = c(0, max(from_val, to_val) + 15)),
        margin = list(b = 80)
      )
  })

  # -- Country contribution: GDP-weighted bars --
  output$analytics_country_contribution <- renderPlotly({
    lang <- current_lang()
    yr_from <- as.integer(input$analytics_from_year)
    yr_to <- as.integer(input$analytics_to_year)

    scores_from <- dimension_scores %>% dplyr::filter(year == yr_from)
    scores_to <- dimension_scores %>% dplyr::filter(year == yr_to)

    if (nrow(scores_from) == 0 || nrow(scores_to) == 0) {
      return(plot_ly() %>% layout(title = t("label_no_data", lang)))
    }

    # Join and calculate weighted overall contribution
    contrib <- scores_to %>%
      dplyr::select(country, overall_to = overall_index) %>%
      dplyr::left_join(
        scores_from %>% dplyr::select(country, overall_from = overall_index),
        by = "country"
      ) %>%
      dplyr::left_join(GDP_WEIGHTS, by = "country") %>%
      dplyr::mutate(
        raw_change = overall_to - overall_from,
        weighted_contrib = raw_change * weight,
        country_label = translate_countries(country, lang)
      ) %>%
      dplyr::arrange(weighted_contrib)

    contrib$country_label <- factor(contrib$country_label, levels = contrib$country_label)
    bar_colors <- ifelse(contrib$weighted_contrib >= 0, "#4caf50", "#f44336")

    plot_ly(contrib, y = ~country_label, x = ~weighted_contrib,
            type = 'bar', orientation = 'h',
            marker = list(color = bar_colors),
            text = ~paste0(country_label, ": ", sprintf("%+.2f", weighted_contrib),
                          " (", sprintf("%+.1f", raw_change), ")"),
            hovertemplate = "%{text}<extra></extra>") %>%
      layout(
        xaxis = list(title = t("axis_gdp_weighted_contrib", lang),
                     zeroline = TRUE),
        yaxis = list(title = "", automargin = TRUE),
        shapes = list(
          list(type = "line", x0 = 0, x1 = 0,
               y0 = -0.5, y1 = nrow(contrib) - 0.5,
               line = list(color = "black", width = 1, dash = "dot"))
        ),
        margin = list(l = 100)
      )
  })

  # ===== Section 2: Annual Dynamics =====

  # -- Stacked area: GCC score composition by dimension --
  output$analytics_stacked_area <- renderPlotly({
    lang <- current_lang()
    # Build weighted dimension scores for GCC aggregate
    area_data <- gcc_ts %>%
      dplyr::select(year, all_of(DIMENSION_COLS)) %>%
      tidyr::pivot_longer(cols = -year, names_to = "dim_col", values_to = "score") %>%
      dplyr::mutate(
        dimension = DIMENSION_COL_TO_LABEL[dim_col],
        weighted = score * DIMENSION_WEIGHTS[dimension]
      )

    area_data$dimension <- factor(area_data$dimension, levels = rev(DIMENSION_LABELS))

    p <- plot_ly()
    for (i in rev(seq_along(DIMENSION_LABELS))) {
      dim_name <- DIMENSION_LABELS[i]
      dim_label <- translate_dimension(dim_name, lang)
      d <- area_data %>% dplyr::filter(dimension == dim_name)
      p <- p %>% add_trace(
        data = d, x = ~year, y = ~weighted,
        type = 'scatter', mode = 'lines',
        fill = 'tonexty', fillcolor = paste0(DIMENSION_COLORS[i], "88"),
        line = list(color = DIMENSION_COLORS[i], width = 1),
        name = dim_label,
        stackgroup = 'one',
        hovertemplate = paste0("<b>", dim_label, "</b><br>",
                              "%{y:.1f}<extra></extra>")
      )
    }

    p %>% layout(
      xaxis = list(title = t("axis_year", lang), dtick = 1),
      yaxis = list(title = t("axis_weighted_dim_score", lang)),
      hovermode = 'x unified',
      legend = list(orientation = 'h', y = -0.15)
    )
  })

  # -- Annual dimension change bars: what drove each year's change --
  output$analytics_annual_dim_bars <- renderPlotly({
    lang <- current_lang()
    # GCC aggregate dimension changes year-over-year
    gcc_dim_long <- gcc_ts %>%
      dplyr::arrange(year) %>%
      dplyr::mutate(across(all_of(DIMENSION_COLS), ~(.x - dplyr::lag(.x)))) %>%
      dplyr::filter(!is.na(trade_score)) %>%
      tidyr::pivot_longer(cols = all_of(DIMENSION_COLS),
                          names_to = "dim_col", values_to = "change") %>%
      dplyr::mutate(
        dimension = DIMENSION_COL_TO_LABEL[dim_col],
        weighted_change = change * DIMENSION_WEIGHTS[dimension]
      )

    gcc_dim_long$dimension <- factor(gcc_dim_long$dimension, levels = DIMENSION_LABELS)

    p <- plot_ly()
    for (i in seq_along(DIMENSION_LABELS)) {
      dim_label <- translate_dimension(DIMENSION_LABELS[i], lang)
      d <- gcc_dim_long %>% dplyr::filter(dimension == DIMENSION_LABELS[i])
      p <- p %>% add_trace(
        data = d, x = ~year, y = ~weighted_change,
        type = 'bar', name = dim_label,
        marker = list(color = DIMENSION_COLORS[i]),
        hovertemplate = paste0("<b>", dim_label, "</b><br>",
                              "%{y:+.2f}<extra></extra>")
      )
    }

    p %>% layout(
      barmode = 'relative',
      xaxis = list(title = t("axis_year", lang), dtick = 1),
      yaxis = list(title = t("axis_weighted_change", lang)),
      hovermode = 'x unified',
      legend = list(orientation = 'h', y = -0.15),
      shapes = list(
        list(type = "line",
             x0 = min(gcc_dim_long$year) - 0.5,
             x1 = max(gcc_dim_long$year) + 0.5,
             y0 = 0, y1 = 0,
             line = list(color = "black", width = 1, dash = "dot"))
      )
    )
  })

  # ===== Section 3: Cross-Country Spread =====

  output$analytics_spread_chart <- renderPlotly({
    lang <- current_lang()
    latest_year <- max(dimension_scores$year)
    latest <- dimension_scores %>% dplyr::filter(year == latest_year)

    # Also get GCC aggregate
    gcc_latest <- gcc_ts %>% dplyr::filter(year == latest_year)

    translated_dims <- translate_dimensions(DIMENSION_LABELS, lang)
    spread_data <- lapply(seq_along(DIMENSION_COLS), function(i) {
      col <- DIMENSION_COLS[i]
      dim_label <- translated_dims[i]
      vals <- latest[[col]]

      data.frame(
        dimension = dim_label,
        country = latest$country,
        score = vals,
        gcc_avg = gcc_latest[[col]],
        dim_min = min(vals, na.rm = TRUE),
        dim_max = max(vals, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }) %>% dplyr::bind_rows()

    spread_data$dimension <- factor(spread_data$dimension, levels = rev(translated_dims))

    gcc_label <- t("label_gcc_average", lang)
    p <- plot_ly()

    # Range lines (min to max)
    range_summary <- spread_data %>%
      dplyr::distinct(dimension, dim_min, dim_max, gcc_avg)

    for (j in seq_len(nrow(range_summary))) {
      p <- p %>% add_trace(
        x = c(range_summary$dim_min[j], range_summary$dim_max[j]),
        y = c(range_summary$dimension[j], range_summary$dimension[j]),
        type = 'scatter', mode = 'lines',
        line = list(color = '#cccccc', width = 6),
        showlegend = FALSE, hoverinfo = 'skip'
      )
    }

    # Individual country dots
    for (ctry in countries) {
      ctry_label <- translate_country(ctry, lang)
      ctry_data <- spread_data %>% dplyr::filter(country == ctry)
      p <- p %>% add_trace(
        data = ctry_data, x = ~score, y = ~dimension,
        type = 'scatter', mode = 'markers',
        marker = list(size = 12, color = COUNTRY_COLORS[ctry],
                      line = list(color = 'white', width = 1)),
        name = ctry_label, legendgroup = ctry,
        hovertemplate = paste0("<b>", ctry_label, "</b><br>",
                              "%{y}: %{x:.1f}<extra></extra>")
      )
    }

    # GCC average diamonds
    p <- p %>% add_trace(
      data = range_summary, x = ~gcc_avg, y = ~dimension,
      type = 'scatter', mode = 'markers',
      marker = list(size = 14, color = COUNTRY_COLORS_WITH_GCC["GCC"],
                    symbol = "diamond",
                    line = list(color = 'white', width = 1.5)),
      name = gcc_label, legendgroup = "gcc",
      hovertemplate = paste0("<b>", gcc_label, "</b><br>%{y}: %{x:.1f}<extra></extra>")
    )

    p %>% layout(
      xaxis = list(title = t("axis_score", lang), range = c(0, 105)),
      yaxis = list(title = "", automargin = TRUE),
      legend = list(orientation = 'h', y = -0.1),
      margin = list(l = 120)
    )
  })

  # ===== Section 4: Biggest Movers =====

  output$analytics_biggest_movers <- renderPlotly({
    lang <- current_lang()
    latest_year <- max(dimension_scores$year)
    prev_year <- latest_year - 1

    curr <- dimension_scores %>% dplyr::filter(year == latest_year)
    prev <- dimension_scores %>% dplyr::filter(year == prev_year)

    if (nrow(curr) == 0 || nrow(prev) == 0) {
      return(plot_ly() %>% layout(title = t("label_no_data", lang)))
    }

    # Calculate dimension-level changes for each country
    movers <- curr %>%
      dplyr::select(country, all_of(DIMENSION_COLS)) %>%
      tidyr::pivot_longer(cols = -country, names_to = "dim_col", values_to = "score_to") %>%
      dplyr::left_join(
        prev %>%
          dplyr::select(country, all_of(DIMENSION_COLS)) %>%
          tidyr::pivot_longer(cols = -country, names_to = "dim_col",
                              values_to = "score_from"),
        by = c("country", "dim_col")
      ) %>%
      dplyr::mutate(
        dimension = DIMENSION_COL_TO_LABEL[dim_col],
        change = score_to - score_from,
        label = paste(translate_countries(country, lang), "\u2014", translate_dimensions(dimension, lang))
      ) %>%
      dplyr::arrange(change)

    # Take top 8 gains and top 8 declines (or fewer if not available)
    top_gains <- movers %>% dplyr::filter(change > 0) %>%
      dplyr::arrange(desc(change)) %>% utils::head(8)
    top_declines <- movers %>% dplyr::filter(change < 0) %>%
      dplyr::arrange(change) %>% utils::head(8)
    top_movers <- dplyr::bind_rows(top_declines, top_gains)

    if (nrow(top_movers) == 0) {
      return(plot_ly() %>% layout(title = t("label_no_data", lang)))
    }

    top_movers$label <- factor(top_movers$label, levels = top_movers$label)
    bar_colors <- ifelse(top_movers$change >= 0, "#4caf50", "#f44336")

    plot_ly(top_movers, y = ~label, x = ~change,
            type = 'bar', orientation = 'h',
            marker = list(color = bar_colors),
            text = ~sprintf("%+.1f", change),
            textposition = 'outside',
            hovertemplate = paste0(
              "<b>%{y}</b><br>",
              "%{x:+.1f}<br>",
              "<extra></extra>")) %>%
      layout(
        xaxis = list(title = t("axis_score_change", lang), zeroline = TRUE),
        yaxis = list(title = "", automargin = TRUE,
                     categoryorder = "array",
                     categoryarray = levels(top_movers$label)),
        shapes = list(
          list(type = "line", x0 = 0, x1 = 0,
               y0 = -0.5, y1 = nrow(top_movers) - 0.5,
               line = list(color = "black", width = 1, dash = "dot"))
        ),
        margin = list(l = 180)
      )
  })

  # ---------------------------------------------------------------------------
  # Data Explorer Tab Outputs
  # ---------------------------------------------------------------------------

  output$data_table <- renderDT({
    lang <- current_lang()
    data_to_show <- switch(input$data_table_select,
                           "dimension" = dimension_scores,
                           "gcc" = gcc_ts,
                           "yoy" = yoy_changes,
                           "indicator" = indicator_detail)

    # Translate country names in the data
    if ("country" %in% names(data_to_show)) {
      data_to_show$country <- translate_countries(data_to_show$country, lang)
    }
    # Translate indicator labels in the data
    if ("indicator_label" %in% names(data_to_show)) {
      data_to_show$indicator_label <- translate_indicators(data_to_show$indicator_label, lang)
    }
    # Translate dimension names in the data
    if ("dimension" %in% names(data_to_show) && input$data_table_select == "indicator") {
      data_to_show$dimension <- translate_dimensions(data_to_show$dimension, lang)
    }

    # Remember numeric columns before renaming
    num_cols <- names(data_to_show)[sapply(data_to_show, is.numeric) & names(data_to_show) != "year"]

    # Translate column headers
    data_to_show <- translate_colnames(data_to_show, lang)
    num_cols_display <- translate_colnames(
      setNames(data.frame(matrix(ncol = length(num_cols), nrow = 0)), num_cols), lang
    ) |> names()

    dt_options <- list(
      pageLength = 25,
      scrollX = TRUE,
      searchHighlight = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    )
    if (lang == "ar") {
      dt_options$language <- list(
        url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/ar.json"
      )
    }

    datatable(
      data_to_show,
      options = dt_options,
      extensions = 'Buttons',
      filter = 'top',
      rownames = FALSE
    ) %>%
      formatRound(columns = num_cols_display, digits = 2)
  })
}

# ==============================================================================
# Dashboard UI Definition
# ==============================================================================

dashboard_ui <- function(lang = "en") {
  tagList(
    dashboardPage(
      skin = "blue",

      dashboardHeader(
        title = t("app_title", lang),
        titleWidth = 380,
        tags$li(class = "dropdown",
          actionButton("lang_toggle",
            label = if (lang == "ar") "English" else "\u0639\u0631\u0628\u064a",
            class = "lang-toggle-btn",
            style = "margin-top: 8px; margin-right: 10px; color: #fff; background: transparent; border: 1px solid rgba(255,255,255,0.5); padding: 4px 12px; font-size: 13px;"
          )
        )
      ),

      dashboardSidebar(
        width = 250,
        sidebarMenu(
          id = "tabs",
          menuItem(t("menu_overview", lang), tabName = "overview", icon = icon("dashboard")),
          menuItem(t("menu_metadata", lang), tabName = "metadata", icon = icon("info-circle")),
          menuItem(t("menu_gcc_overall", lang), tabName = "gcc_overall", icon = icon("chart-line")),
          menuItem(t("menu_gcc_timeseries", lang), tabName = "gcc_timeseries", icon = icon("chart-area")),
          menuItem(t("menu_country_profiles", lang), tabName = "country_profiles", icon = icon("flag")),
          menuItem(t("menu_country_heatmap", lang), tabName = "country_heatmap", icon = icon("th")),
          menuItem(t("menu_gcc_analytics", lang), tabName = "gcc_analytics", icon = icon("chart-bar")),
          menuItem(t("menu_data_explorer", lang), tabName = "data_explorer", icon = icon("table"))
        )
      ),

      dashboardBody(
        tabItems(
          # Overview Tab
          overview_tab_ui(lang),

          # Metadata Tab
          metadata_tab_ui(lang),

          # GCC Overall Tab
          gcc_overall_tab_ui(lang),

          # GCC Timeseries Tab
          gcc_timeseries_tab_ui(lang),

          # Country Profiles Tab
          country_profiles_tab_ui(lang),

          # Country Heatmap Tab
          country_heatmap_tab_ui(lang),

          # GCC Analytics Tab
          gcc_analytics_tab_ui(lang),

          # Data Explorer Tab
          data_explorer_tab_ui(lang)
        )
      )
    ),

    # Back to Welcome button
    actionButton("back_to_welcome",
                 label = tagList(icon("home"), paste0(" ", t("btn_back_welcome", lang))),
                 class = "back-to-welcome")
  )
}

# ==============================================================================
# Tab UI Functions
# ==============================================================================

overview_tab_ui <- function(lang = "en") {
  tabItem(
    tabName = "overview",
    fluidRow(
      box(
        width = 12,
        title = t("overview_title", lang),
        status = "primary",
        solidHeader = TRUE,
        h4(t("overview_welcome", lang)),
        p(t("overview_description", lang)),
        hr(),
        h5(strong(t("overview_features_title", lang))),
        tags$ul(
          tags$li(strong(t("menu_gcc_overall", lang), ":"), t("feat_gcc_overall", lang)),
          tags$li(strong(t("menu_gcc_timeseries", lang), ":"), t("feat_gcc_timeseries", lang)),
          tags$li(strong(t("menu_country_profiles", lang), ":"), t("feat_country_profiles", lang)),
          tags$li(strong(t("menu_country_heatmap", lang), ":"), t("feat_country_heatmap", lang)),
          tags$li(strong(t("menu_gcc_analytics", lang), ":"), t("feat_gcc_analytics", lang)),
          tags$li(strong(t("menu_data_explorer", lang), ":"), t("feat_data_explorer", lang))
        ),
        hr(),
        h5(strong(t("overview_dimensions_title", lang))),
        fluidRow(
          column(4, tags$div(icon("exchange-alt"), strong(paste0(" ", t("dim_trade", lang))))),
          column(4, tags$div(icon("university"), strong(paste0(" ", t("dim_financial", lang))))),
          column(4, tags$div(icon("users"), strong(paste0(" ", t("dim_labor", lang)))))
        ),
        br(),
        fluidRow(
          column(4, tags$div(icon("road"), strong(paste0(" ", t("dim_infrastructure", lang))))),
          column(4, tags$div(icon("leaf"), strong(paste0(" ", t("dim_sustainability", lang))))),
          column(4, tags$div(icon("chart-line"), strong(paste0(" ", t("dim_convergence", lang)))))
        )
      )
    ),
    fluidRow(
      valueBoxOutput("latest_overall", width = 4),
      valueBoxOutput("latest_year", width = 4),
      valueBoxOutput("num_countries", width = 4)
    )
  )
}

gcc_overall_tab_ui <- function(lang = "en") {
  tabItem(
    tabName = "gcc_overall",
    fluidRow(
      box(width = 12, title = t("gcc_box_aggregate", lang),
          status = "primary", solidHeader = TRUE,
          plotlyOutput("gcc_overall_gauge", height = "300px"))
    ),
    fluidRow(
      box(width = 6, title = t("gcc_box_dim_scores", lang),
          status = "info", solidHeader = TRUE,
          plotlyOutput("gcc_dimension_bars", height = "400px")),
      box(width = 6, title = t("gcc_box_score_cards", lang),
          status = "info", solidHeader = TRUE,
          uiOutput("dimension_boxes"))
    ),
    fluidRow(
      box(width = 12, title = t("gcc_box_ranking", lang),
          status = "success", solidHeader = TRUE,
          plotlyOutput("country_ranking", height = "350px"))
    )
  )
}

gcc_timeseries_tab_ui <- function(lang = "en") {
  tabItem(
    tabName = "gcc_timeseries",

    # --- Tier 1: Summary header + compact overall trend ---
    fluidRow(
      column(12, uiOutput("ts_summary_bar"))
    ),
    fluidRow(
      box(width = 12, title = t("ts_box_overall_trend", lang),
          status = "primary", solidHeader = TRUE,
          plotlyOutput("gcc_overall_trend", height = "250px"))
    ),

    # --- Tier 2: Dimension selector pills ---
    fluidRow(
      column(12,
        div(class = "dimension-selector",
          tags$label(t("ts_pills_label", lang), class = "dim-selector-label"),
          div(class = "dim-pills",
            actionButton("dim_btn_Trade", tagList(icon("exchange-alt"), paste0(" ", translate_dimension("Trade", lang))),
                         class = "dim-pill dim-pill-trade"),
            actionButton("dim_btn_Financial", tagList(icon("university"), paste0(" ", translate_dimension("Financial", lang))),
                         class = "dim-pill dim-pill-financial"),
            actionButton("dim_btn_Labor", tagList(icon("users"), paste0(" ", translate_dimension("Labor", lang))),
                         class = "dim-pill dim-pill-labor"),
            actionButton("dim_btn_Infrastructure", tagList(icon("road"), paste0(" ", translate_dimension("Infrastructure", lang))),
                         class = "dim-pill dim-pill-infrastructure"),
            actionButton("dim_btn_Sustainability", tagList(icon("leaf"), paste0(" ", translate_dimension("Sustainability", lang))),
                         class = "dim-pill dim-pill-sustainability"),
            actionButton("dim_btn_Convergence", tagList(icon("chart-line"), paste0(" ", translate_dimension("Convergence", lang))),
                         class = "dim-pill dim-pill-convergence")
          )
        ),
        # JS to toggle active class on pill buttons
        tags$script(HTML("
          $(document).on('click', '.dim-pill', function() {
            $('.dim-pill').removeClass('active');
            $(this).addClass('active');
          });
          $(document).on('shiny:sessioninitialized', function() {
            $('#dim_btn_Trade').addClass('active');
          });
        "))
      )
    ),

    # --- Tier 3: Selected dimension — country comparison ---
    fluidRow(
      box(width = 12,
          title = uiOutput("ts_dimension_title"),
          status = "info", solidHeader = TRUE,
          plotlyOutput("ts_dimension_countries", height = "350px"))
    ),

    # --- Tier 4: Indicator small multiples (GCC aggregate) ---
    fluidRow(
      box(width = 12,
          title = uiOutput("ts_indicators_title"),
          status = "success", solidHeader = TRUE,
          plotlyOutput("ts_indicator_multiples", height = "500px"))
    )
  )
}

country_profiles_tab_ui <- function(lang = "en") {
  tabItem(
    tabName = "country_profiles",

    # --- Row 1: Country selector + value boxes ---
    fluidRow(
      box(width = 4, title = t("cp_box_select", lang),
          status = "primary", solidHeader = TRUE,
          selectInput("selected_country", t("cp_label_country", lang),
                      choices = setNames(countries, translate_countries(countries, lang)),
                      selected = countries[1])),
      valueBoxOutput("country_overall", width = 4),
      valueBoxOutput("country_rank", width = 4)
    ),

    # --- Row 2: Radar chart + combined Country vs GCC trend ---
    fluidRow(
      box(width = 5, title = t("cp_box_radar", lang),
          status = "info", solidHeader = TRUE,
          plotlyOutput("country_radar", height = "380px")),
      box(width = 7, title = t("cp_box_vs_gcc", lang),
          status = "info", solidHeader = TRUE,
          plotlyOutput("country_vs_gcc_combined", height = "380px"))
    ),

    # --- Row 3: Dimension selector for indicator drill-down ---
    fluidRow(
      box(width = 12, title = t("cp_box_indicator_detail", lang),
          status = "primary", solidHeader = TRUE,
          fluidRow(
            column(4,
              selectInput("cp_dimension_select", t("cp_dim_select_label", lang),
                          choices = setNames(DIMENSION_LABELS, translate_dimensions(DIMENSION_LABELS, lang)),
                          selected = "Trade")
            ),
            column(8,
              div(style = "padding-top: 25px; color: #666; font-size: 0.9rem;",
                icon("info-circle"),
                t("cp_dim_info", lang)
              )
            )
          )
      )
    ),

    # --- Row 4: Lollipop comparison + small multiples side by side ---
    fluidRow(
      box(width = 5,
          title = uiOutput("cp_lollipop_title"),
          status = "success", solidHeader = TRUE,
          plotlyOutput("cp_indicator_lollipop", height = "450px")),
      box(width = 7,
          title = uiOutput("cp_trends_title"),
          status = "success", solidHeader = TRUE,
          plotlyOutput("cp_indicator_trends", height = "450px"))
    )
  )
}

country_heatmap_tab_ui <- function(lang = "en") {
  tabItem(
    tabName = "country_heatmap",
    fluidRow(
      box(width = 4, title = t("hm_box_year", lang),
          status = "primary", solidHeader = TRUE,
          selectInput("heatmap_year", t("hm_label_year", lang),
                      choices = sort(unique(dimension_scores$year), decreasing = TRUE),
                      selected = max(dimension_scores$year)))
    ),
    fluidRow(
      box(width = 12, title = t("hm_box_heatmap", lang),
          status = "info", solidHeader = TRUE,
          plotlyOutput("country_heatmap", height = "500px"))
    ),
    fluidRow(
      box(width = 12, title = t("hm_box_levels", lang),
          status = "success", solidHeader = TRUE,
          plotlyOutput("integration_levels", height = "350px"))
    )
  )
}

gcc_analytics_tab_ui <- function(lang = "en") {
  tabItem(
    tabName = "gcc_analytics",

    # ===== Section 1: What Changed? (Period Decomposition) =====
    fluidRow(
      box(width = 12, title = t("an_section1_title", lang),
          status = "primary", solidHeader = TRUE,
          fluidRow(
            column(3,
              selectInput("analytics_from_year", t("an_label_from", lang),
                          choices = sort(unique(dimension_scores$year)),
                          selected = max(dimension_scores$year) - 1)
            ),
            column(3,
              selectInput("analytics_to_year", t("an_label_to", lang),
                          choices = sort(unique(dimension_scores$year)),
                          selected = max(dimension_scores$year))
            ),
            column(6,
              div(style = "padding-top: 25px; color: #666; font-size: 0.9rem;",
                icon("info-circle"),
                t("an_section1_info", lang)
              )
            )
          )
      )
    ),
    fluidRow(
      box(width = 7, title = uiOutput("analytics_waterfall_title"),
          status = "info", solidHeader = TRUE,
          plotlyOutput("analytics_waterfall", height = "420px")),
      box(width = 5, title = uiOutput("analytics_country_contrib_title"),
          status = "info", solidHeader = TRUE,
          plotlyOutput("analytics_country_contribution", height = "420px"))
    ),

    # ===== Section 2: Annual Dynamics (All Years) =====
    fluidRow(
      box(width = 12,
          title = t("an_section2_title", lang),
          status = "primary", solidHeader = TRUE,
          p(style = "color: #666; font-size: 0.9rem; margin-bottom: 0;",
            icon("info-circle"),
            t("an_section2_info", lang))
      )
    ),
    fluidRow(
      box(width = 6, title = t("an_box_stacked_area", lang),
          status = "success", solidHeader = TRUE,
          plotlyOutput("analytics_stacked_area", height = "400px")),
      box(width = 6, title = t("an_box_annual_dim_bars", lang),
          status = "success", solidHeader = TRUE,
          plotlyOutput("analytics_annual_dim_bars", height = "400px"))
    ),

    # ===== Section 3: Cross-Country Spread =====
    fluidRow(
      box(width = 12,
          title = uiOutput("analytics_spread_title"),
          status = "primary", solidHeader = TRUE,
          p(style = "color: #666; font-size: 0.9rem; margin-bottom: 5px;",
            icon("info-circle"),
            t("an_spread_info", lang)),
          plotlyOutput("analytics_spread_chart", height = "380px"))
    ),

    # ===== Section 4: Biggest Movers =====
    fluidRow(
      box(width = 12,
          title = uiOutput("analytics_movers_title"),
          status = "primary", solidHeader = TRUE,
          p(style = "color: #666; font-size: 0.9rem; margin-bottom: 5px;",
            icon("info-circle"),
            t("an_movers_info", lang)),
          plotlyOutput("analytics_biggest_movers", height = "450px"))
    )
  )
}

data_explorer_tab_ui <- function(lang = "en") {
  tabItem(
    tabName = "data_explorer",
    fluidRow(
      box(width = 12, title = t("de_box_title", lang),
          status = "primary", solidHeader = TRUE,
          selectInput("data_table_select", t("de_label_dataset", lang),
                      choices = setNames(
                        c("dimension", "gcc", "yoy", "indicator"),
                        c(t("de_dataset_dimension", lang), t("de_dataset_gcc", lang),
                          t("de_dataset_yoy", lang), t("de_dataset_indicator", lang))
                      )),
          DTOutput("data_table"))
    )
  )
}

# ==============================================================================
# Run the Application
# ==============================================================================

shinyApp(ui = ui, server = server)
