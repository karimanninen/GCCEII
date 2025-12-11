# ==============================================================================
# GCC ECONOMIC INTEGRATION INDEX DASHBOARD
# Version: 3.0 - Modular Architecture
# Date: December 2025
# ==============================================================================
#
# A comprehensive Shiny dashboard for analyzing GCC economic integration
# across six member states (Bahrain, Kuwait, Oman, Qatar, Saudi Arabia, UAE)
#
# Features:
# - ARIIP-style landing page with rotating quotes carousel
# - 7 interactive tabs with 25+ visualizations
# - Time series analysis (2015-2023)
# - Country comparisons and rankings
# - Year-over-year change analytics
# - Six integration dimensions: Trade, Financial, Labor, Infrastructure,
#   Sustainability, and Economic Convergence
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

# ==============================================================================
# LOAD DATA
# ==============================================================================

gcc_data <- load_gcc_data("output")
dimension_scores <- gcc_data$dimension_scores
gcc_ts <- gcc_data$gcc_ts
yoy_changes <- gcc_data$yoy_changes
country_data <- gcc_data$country_data

# Get unique countries
countries <- get_countries(dimension_scores)

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(

  # Link external CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # Conditional display: Landing Page OR Dashboard
  uiOutput("main_ui"),

  # Carousel JavaScript
  carousel_js()
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Navigation State
  # ---------------------------------------------------------------------------

  entered_dashboard <- reactiveVal(FALSE)

  observeEvent(input$enter_dashboard, { entered_dashboard(TRUE) })
  observeEvent(input$back_to_welcome, { entered_dashboard(FALSE) })

  # ---------------------------------------------------------------------------
  # Main UI Renderer
  # ---------------------------------------------------------------------------

  output$main_ui <- renderUI({
    if (!entered_dashboard()) {
      landing_page_ui()
    } else {
      dashboard_ui()
    }
  })

  # ---------------------------------------------------------------------------
  # Overview Tab Outputs
  # ---------------------------------------------------------------------------

  output$latest_overall <- renderValueBox({
    latest <- gcc_ts %>% filter(year == max(year))
    valueBox(
      value = round(latest$overall, 1),
      subtitle = "Overall GCC Index",
      icon = icon("chart-line"),
      color = "blue"
    )
  })

  output$latest_year <- renderValueBox({
    valueBox(
      value = max(dimension_scores$year),
      subtitle = "Latest Year",
      icon = icon("calendar"),
      color = "green"
    )
  })

  output$num_countries <- renderValueBox({
    valueBox(
      value = length(countries),
      subtitle = "Member States",
      icon = icon("flag"),
      color = "purple"
    )
  })

  # ---------------------------------------------------------------------------
  # Metadata Tab Outputs
  # ---------------------------------------------------------------------------

  output$dimension_weights_table <- renderTable({
    get_dimension_weights_table()
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$trade_indicators_table <- renderTable({
    get_trade_indicators_table()
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$financial_indicators_table <- renderTable({
    get_financial_indicators_table()
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$labor_indicators_table <- renderTable({
    get_labor_indicators_table()
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$infrastructure_indicators_table <- renderTable({
    get_infrastructure_indicators_table()
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$sustainability_indicators_table <- renderTable({
    get_sustainability_indicators_table()
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$convergence_indicators_table <- renderTable({
    get_convergence_indicators_table()
  }, striped = TRUE, hover = TRUE, width = "100%")

  # ---------------------------------------------------------------------------
  # GCC Overall Tab Outputs
  # ---------------------------------------------------------------------------

  output$gcc_overall_gauge <- renderPlotly({
    latest <- gcc_ts %>% filter(year == max(year))
    prev <- gcc_ts %>% filter(year == max(year) - 1) %>% pull(overall)
    create_gauge_chart(latest$overall, prev, paste("GCC Integration Index", max(gcc_ts$year)))
  })

  output$gcc_dimension_bars <- renderPlotly({
    latest <- gcc_ts %>% filter(year == max(year))
    dim_data <- create_dimension_df(latest)
    create_dimension_bar_chart(dim_data)
  })

  output$dimension_boxes <- renderUI({
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
                span(class = "info-box-text", DIMENSION_LABELS[i]),
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
                span(class = "info-box-text", DIMENSION_LABELS[i]),
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
    create_ranking_chart(ranking, title = paste("Country Rankings -", latest_year))
  })

  # ---------------------------------------------------------------------------
  # GCC Timeseries Tab Outputs
  # ---------------------------------------------------------------------------

  output$gcc_overall_trend <- renderPlotly({
    create_line_chart(gcc_ts, "year", "overall")
  })

  output$gcc_dimension_trends <- renderPlotly({
    p <- plot_ly(gcc_ts, x = ~year)

    for (i in seq_along(DIMENSION_COLS)) {
      p <- p %>% add_trace(
        y = as.formula(paste0("~", DIMENSION_COLS[i])),
        name = DIMENSION_LABELS[i],
        type = 'scatter', mode = 'lines+markers',
        line = list(color = DIMENSION_COLORS[i], width = 2),
        marker = list(size = 8)
      )
    }

    p %>% layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = "Score", range = c(0, 100)),
      hovermode = 'x unified',
      legend = list(orientation = 'h', y = -0.15)
    )
  })

  output$all_countries_trend <- renderPlotly({
    p <- plot_ly()

    for (ctry in countries) {
      ctry_data <- dimension_scores %>% filter(country == ctry)
      p <- p %>% add_trace(
        data = ctry_data, x = ~year, y = ~overall_index,
        name = ctry, type = 'scatter', mode = 'lines+markers',
        line = list(color = COUNTRY_COLORS[ctry], width = 2),
        marker = list(size = 8)
      )
    }

    p %>% layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = "Overall Index", range = c(0, 100)),
      hovermode = 'x unified',
      legend = list(orientation = 'h', y = -0.15)
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
      subtitle = paste(input$selected_country, "Overall Index"),
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
      subtitle = paste("Rank in", latest_year),
      icon = icon("trophy"),
      color = "green"
    )
  })

  output$country_radar <- renderPlotly({
    latest <- dimension_scores %>%
      filter(country == input$selected_country, year == max(year))
    scores <- get_scores_vector(latest)
    create_radar_chart(scores)
  })

  output$country_dimension_bars <- renderPlotly({
    latest <- dimension_scores %>%
      filter(country == input$selected_country, year == max(year))
    dim_data <- create_dimension_df(latest)
    create_dimension_bar_chart(dim_data, COUNTRY_COLORS[input$selected_country])
  })

  output$country_trend <- renderPlotly({
    ctry_data <- dimension_scores %>% filter(country == input$selected_country)
    create_line_chart(ctry_data, "year", "overall_index",
                      line_color = COUNTRY_COLORS[input$selected_country])
  })

  output$country_vs_gcc <- renderPlotly({
    ctry_data <- dimension_scores %>% filter(country == input$selected_country)

    plot_ly() %>%
      add_trace(data = ctry_data, x = ~year, y = ~overall_index,
                name = input$selected_country, type = 'scatter', mode = 'lines+markers',
                line = list(color = COUNTRY_COLORS[input$selected_country], width = 3),
                marker = list(size = 10)) %>%
      add_trace(data = gcc_ts, x = ~year, y = ~overall,
                name = "GCC Average", type = 'scatter', mode = 'lines+markers',
                line = list(color = '#003366', width = 3, dash = 'dash'),
                marker = list(size = 10)) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Overall Index", range = c(0, 100)),
        hovermode = 'x unified',
        legend = list(orientation = 'h', y = -0.15)
      )
  })

  # ---------------------------------------------------------------------------
  # Country Heatmap Tab Outputs
  # ---------------------------------------------------------------------------

  output$country_heatmap <- renderPlotly({
    heatmap_data <- dimension_scores %>%
      filter(year == input$heatmap_year) %>%
      select(country, all_of(DIMENSION_COLS))

    colnames(heatmap_data) <- c("country", DIMENSION_LABELS)

    heatmap_matrix <- heatmap_data %>%
      column_to_rownames("country") %>%
      as.matrix()

    create_heatmap(heatmap_matrix)
  })

  output$integration_levels <- renderPlotly({
    level_data <- dimension_scores %>%
      filter(year == input$heatmap_year) %>%
      mutate(integration_level = factor(integration_level, levels = c("Weak", "Moderate", "Good")))

    plot_ly(level_data, x = ~country, y = ~overall_index, type = 'bar',
            marker = list(color = ~INTEGRATION_LEVEL_COLORS[integration_level])) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Overall Index", range = c(0, 100)),
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

  output$yoy_dimension_changes <- renderPlotly({
    changes <- yoy_changes %>%
      filter(year >= input$analytics_year_range[1] & year <= input$analytics_year_range[2]) %>%
      select(country, trade_change, financial_change, labor_change,
             infrastructure_change, sustainability_change, convergence_change) %>%
      group_by(country) %>%
      summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(cols = -country, names_to = "dimension", values_to = "change")

    changes$dimension_label <- DIMENSION_CHANGE_TO_LABEL[changes$dimension]
    changes$dimension_label <- factor(changes$dimension_label, levels = DIMENSION_LABELS)

    create_yoy_scatter(changes)
  })

  output$contribution_analysis <- renderPlotly({
    selected_year <- input$analytics_year_range[2]
    prev_year <- input$analytics_year_range[1]

    current_scores <- dimension_scores %>%
      filter(year == selected_year) %>%
      select(country, trade_score, financial_score, labor_score,
             infrastructure_score, sustainability_score, convergence_score)

    prev_scores <- dimension_scores %>%
      filter(year == prev_year) %>%
      select(country, trade_score, financial_score, labor_score,
             infrastructure_score, sustainability_score, convergence_score)

    if (nrow(prev_scores) == 0 || nrow(current_scores) == 0) {
      return(plot_ly() %>% layout(title = "No data for selected year range"))
    }

    changes <- current_scores %>%
      left_join(prev_scores, by = "country", suffix = c("_end", "_start")) %>%
      left_join(GDP_WEIGHTS, by = "country")

    contribution_data <- data.frame(
      country = rep(changes$country, 6),
      dimension = rep(DIMENSION_LABELS, each = nrow(changes)),
      contribution = c(
        (changes$trade_score_end - changes$trade_score_start) * changes$weight,
        (changes$financial_score_end - changes$financial_score_start) * changes$weight,
        (changes$labor_score_end - changes$labor_score_start) * changes$weight,
        (changes$infrastructure_score_end - changes$infrastructure_score_start) * changes$weight,
        (changes$sustainability_score_end - changes$sustainability_score_start) * changes$weight,
        (changes$convergence_score_end - changes$convergence_score_start) * changes$weight
      )
    )

    create_contribution_chart(contribution_data)
  })

  output$annual_change_trends <- renderPlotly({
    all_changes <- yoy_changes %>%
      group_by(year) %>%
      summarise(
        mean_change = mean(overall_change, na.rm = TRUE),
        max_change = max(overall_change, na.rm = TRUE),
        min_change = min(overall_change, na.rm = TRUE)
      )

    plot_ly(all_changes) %>%
      add_trace(x = ~year, y = ~mean_change, type = 'scatter', mode = 'lines+markers',
                name = 'Average Change', line = list(color = 'blue', width = 3),
                marker = list(size = 10)) %>%
      add_trace(x = ~year, y = ~max_change, type = 'scatter', mode = 'lines',
                name = 'Max Change', line = list(color = 'green', width = 2, dash = 'dash')) %>%
      add_trace(x = ~year, y = ~min_change, type = 'scatter', mode = 'lines',
                name = 'Min Change', line = list(color = 'red', width = 2, dash = 'dash')) %>%
      add_segments(x = min(all_changes$year), xend = max(all_changes$year),
                   y = 0, yend = 0, line = list(color = 'black', dash = 'dot'),
                   showlegend = FALSE) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Change in Overall Index"),
        hovermode = 'x unified',
        legend = list(orientation = 'h', y = -0.15)
      )
  })

  output$change_heatmap <- renderPlotly({
    dim_changes <- yoy_changes %>%
      filter(year >= input$analytics_year_range[1] & year <= input$analytics_year_range[2]) %>%
      select(country, trade_change, financial_change, labor_change,
             infrastructure_change, sustainability_change, convergence_change) %>%
      group_by(country) %>%
      summarise(across(everything(), ~mean(.x, na.rm = TRUE)))

    colnames(dim_changes) <- c("country", DIMENSION_LABELS)

    change_matrix <- dim_changes %>%
      column_to_rownames("country") %>%
      as.matrix()

    create_heatmap(change_matrix, CHANGE_COLORSCALE, zmin = -10, zmax = 10, zmid = 0)
  })

  # ---------------------------------------------------------------------------
  # Data Explorer Tab Outputs
  # ---------------------------------------------------------------------------

  output$data_table <- renderDT({
    data_to_show <- switch(input$data_table_select,
                           "dimension" = dimension_scores,
                           "gcc" = gcc_ts,
                           "yoy" = yoy_changes)

    datatable(
      data_to_show,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        searchHighlight = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      filter = 'top',
      rownames = FALSE
    ) %>%
      formatRound(
        columns = names(data_to_show)[sapply(data_to_show, is.numeric) & names(data_to_show) != "year"],
        digits = 2
      )
  })
}

# ==============================================================================
# Dashboard UI Definition
# ==============================================================================

dashboard_ui <- function() {
  tagList(
    dashboardPage(
      skin = "blue",

      dashboardHeader(
        title = "GCC Economic Integration Index",
        titleWidth = 350
      ),

      dashboardSidebar(
        width = 250,
        sidebarMenu(
          id = "tabs",
          menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
          menuItem("Metadata & Analysis", tabName = "metadata", icon = icon("info-circle")),
          menuItem("GCC Overall", tabName = "gcc_overall", icon = icon("chart-line")),
          menuItem("GCC Timeseries", tabName = "gcc_timeseries", icon = icon("chart-area")),
          menuItem("Country Profiles", tabName = "country_profiles", icon = icon("flag")),
          menuItem("Country Heatmap", tabName = "country_heatmap", icon = icon("th")),
          menuItem("GCC Analytics", tabName = "gcc_analytics", icon = icon("chart-bar")),
          menuItem("Data Explorer", tabName = "data_explorer", icon = icon("table"))
        )
      ),

      dashboardBody(
        tabItems(
          # Overview Tab
          overview_tab_ui(),

          # Metadata Tab
          metadata_tab_ui(),

          # GCC Overall Tab
          gcc_overall_tab_ui(),

          # GCC Timeseries Tab
          gcc_timeseries_tab_ui(),

          # Country Profiles Tab
          country_profiles_tab_ui(),

          # Country Heatmap Tab
          country_heatmap_tab_ui(),

          # GCC Analytics Tab
          gcc_analytics_tab_ui(),

          # Data Explorer Tab
          data_explorer_tab_ui()
        )
      )
    ),

    # Back to Welcome button
    actionButton("back_to_welcome",
                 label = tagList(icon("home"), " Welcome"),
                 class = "back-to-welcome")
  )
}

# ==============================================================================
# Tab UI Functions
# ==============================================================================

overview_tab_ui <- function() {
  tabItem(
    tabName = "overview",
    fluidRow(
      box(
        width = 12,
        title = "GCC Economic Integration Index Dashboard",
        status = "primary",
        solidHeader = TRUE,
        h4("Welcome to the GCC Economic Integration Index Dashboard"),
        p("This dashboard provides comprehensive analysis of economic integration across GCC member states from 2015 to 2023."),
        hr(),
        h5(strong("Dashboard Features:")),
        tags$ul(
          tags$li(strong("GCC Overall:"), "View aggregate GCC integration metrics and latest performance"),
          tags$li(strong("GCC Timeseries:"), "Analyze trends across all six dimensions over time"),
          tags$li(strong("Country Profiles:"), "Deep dive into individual country performance with timeseries"),
          tags$li(strong("Country Heatmap:"), "Compare countries across dimensions visually"),
          tags$li(strong("GCC Analytics:"), "Examine year-over-year changes and contributions to total GCC index"),
          tags$li(strong("Data Explorer:"), "Access and export underlying data")
        ),
        hr(),
        h5(strong("Six Integration Dimensions:")),
        fluidRow(
          column(4, tags$div(icon("exchange-alt"), strong(" Trade Integration"))),
          column(4, tags$div(icon("university"), strong(" Financial Integration"))),
          column(4, tags$div(icon("users"), strong(" Labor Mobility")))
        ),
        br(),
        fluidRow(
          column(4, tags$div(icon("road"), strong(" Infrastructure Connectivity"))),
          column(4, tags$div(icon("leaf"), strong(" Sustainability"))),
          column(4, tags$div(icon("chart-line"), strong(" Economic Convergence")))
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

gcc_overall_tab_ui <- function() {
  tabItem(
    tabName = "gcc_overall",
    fluidRow(
      box(width = 12, title = "GCC Aggregate Integration Performance",
          status = "primary", solidHeader = TRUE,
          plotlyOutput("gcc_overall_gauge", height = "300px"))
    ),
    fluidRow(
      box(width = 6, title = "Current Dimension Scores",
          status = "info", solidHeader = TRUE,
          plotlyOutput("gcc_dimension_bars", height = "400px")),
      box(width = 6, title = "Dimension Score Cards",
          status = "info", solidHeader = TRUE,
          uiOutput("dimension_boxes"))
    ),
    fluidRow(
      box(width = 12, title = "Country Ranking",
          status = "success", solidHeader = TRUE,
          plotlyOutput("country_ranking", height = "350px"))
    )
  )
}

gcc_timeseries_tab_ui <- function() {
  tabItem(
    tabName = "gcc_timeseries",
    fluidRow(
      box(width = 12, title = "GCC Overall Index Trend",
          status = "primary", solidHeader = TRUE,
          plotlyOutput("gcc_overall_trend", height = "350px"))
    ),
    fluidRow(
      box(width = 12, title = "GCC Dimension Trends",
          status = "info", solidHeader = TRUE,
          plotlyOutput("gcc_dimension_trends", height = "400px"))
    ),
    fluidRow(
      box(width = 12, title = "All Countries: Overall Index Trend",
          status = "success", solidHeader = TRUE,
          plotlyOutput("all_countries_trend", height = "400px"))
    )
  )
}

country_profiles_tab_ui <- function() {
  tabItem(
    tabName = "country_profiles",
    fluidRow(
      box(width = 4, title = "Select Country",
          status = "primary", solidHeader = TRUE,
          selectInput("selected_country", "Country:",
                      choices = countries, selected = countries[1])),
      valueBoxOutput("country_overall", width = 4),
      valueBoxOutput("country_rank", width = 4)
    ),
    fluidRow(
      box(width = 6, title = "Dimension Profile",
          status = "info", solidHeader = TRUE,
          plotlyOutput("country_radar", height = "400px")),
      box(width = 6, title = "Dimension Scores",
          status = "info", solidHeader = TRUE,
          plotlyOutput("country_dimension_bars", height = "400px"))
    ),
    fluidRow(
      box(width = 12, title = "Country Trend Over Time",
          status = "success", solidHeader = TRUE,
          plotlyOutput("country_trend", height = "350px"))
    ),
    fluidRow(
      box(width = 12, title = "Country vs GCC Average",
          status = "warning", solidHeader = TRUE,
          plotlyOutput("country_vs_gcc", height = "350px"))
    )
  )
}

country_heatmap_tab_ui <- function() {
  tabItem(
    tabName = "country_heatmap",
    fluidRow(
      box(width = 4, title = "Select Year",
          status = "primary", solidHeader = TRUE,
          selectInput("heatmap_year", "Year:",
                      choices = sort(unique(dimension_scores$year), decreasing = TRUE),
                      selected = max(dimension_scores$year)))
    ),
    fluidRow(
      box(width = 12, title = "Country-Dimension Heatmap",
          status = "info", solidHeader = TRUE,
          plotlyOutput("country_heatmap", height = "500px"))
    ),
    fluidRow(
      box(width = 12, title = "Integration Levels by Country",
          status = "success", solidHeader = TRUE,
          plotlyOutput("integration_levels", height = "350px"))
    )
  )
}

gcc_analytics_tab_ui <- function() {
  tabItem(
    tabName = "gcc_analytics",
    fluidRow(
      box(width = 12, title = "Analysis Period",
          status = "primary", solidHeader = TRUE,
          sliderInput("analytics_year_range", "Year Range:",
                      min = min(dimension_scores$year),
                      max = max(dimension_scores$year),
                      value = c(min(dimension_scores$year), max(dimension_scores$year)),
                      step = 1, sep = ""))
    ),
    fluidRow(
      box(width = 6, title = "Year-over-Year Changes by Dimension",
          status = "info", solidHeader = TRUE,
          plotlyOutput("yoy_dimension_changes", height = "400px")),
      box(width = 6, title = "Contribution to GCC Index Change",
          status = "info", solidHeader = TRUE,
          plotlyOutput("contribution_analysis", height = "400px"))
    ),
    fluidRow(
      box(width = 6, title = "Annual Change Trends",
          status = "success", solidHeader = TRUE,
          plotlyOutput("annual_change_trends", height = "350px")),
      box(width = 6, title = "Dimension Change Heatmap",
          status = "success", solidHeader = TRUE,
          plotlyOutput("change_heatmap", height = "350px"))
    )
  )
}

data_explorer_tab_ui <- function() {
  tabItem(
    tabName = "data_explorer",
    fluidRow(
      box(width = 12, title = "Data Explorer",
          status = "primary", solidHeader = TRUE,
          selectInput("data_table_select", "Select Dataset:",
                      choices = c("Country Dimension Scores" = "dimension",
                                  "GCC Aggregate" = "gcc",
                                  "Year-over-Year Changes" = "yoy")),
          DTOutput("data_table"))
    )
  )
}

# ==============================================================================
# Run the Application
# ==============================================================================

shinyApp(ui = ui, server = server)
