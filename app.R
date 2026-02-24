# =============================================================================
# app.R  —  GCC Economic Integration Index Dashboard
# Bilingual (English / Arabic) | Shiny + shinydashboard
# =============================================================================
#
# Required file layout
# ├── app.R                          ← this file
# ├── R/
# │   ├── translations.R
# │   ├── mod_landing_page.R
# │   ├── mod_charts.R
# │   └── mod_metadata.R
# ├── www/
# │   ├── rtl.css
# │   └── gccstat_logo.png           ← place actual logo here
# └── output/
#     └── time_series_complete.csv   ← or gcc_integration_workspace.RData
#
# =============================================================================

# =============================================================================
# SECTION 1  Libraries
# =============================================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(plotly)
library(DT)
library(viridis)
library(scales)

# =============================================================================
# SECTION 2  Source modules
# =============================================================================

source("R/translations.R")
source("R/mod_landing_page.R")
source("R/mod_charts.R")
source("R/mod_metadata.R")

# =============================================================================
# SECTION 3  Data loading
# =============================================================================

OUTPUT_DIR <- "output"

.load_data <- function() {
  ws_path  <- file.path(OUTPUT_DIR, "gcc_integration_workspace.RData")
  csv_path <- file.path(OUTPUT_DIR, "time_series_complete.csv")

  if (file.exists(ws_path)) {
    message("Loading data from workspace ...")
    e <- new.env()
    load(ws_path, envir = e)
    ts <- if (exists("time_series_complete", envir = e)) e$time_series_complete
          else if (exists("country_index_2023", envir = e)) e$country_index_2023
          else stop("Cannot locate time-series data in workspace.")
    message("\u2714 Workspace loaded")
    return(ts)
  } else if (file.exists(csv_path)) {
    message("Loading data from CSV ...")
    ts <- read_csv(csv_path, show_col_types = FALSE)
    message("\u2714 CSV loaded")
    return(ts)
  } else {
    stop(
      "ERROR: Data files not found.\n",
      "  Expected: ", ws_path, "\n",
      "  Or:       ", csv_path, "\n",
      "Run GCC_Integration_Master.R first to produce these files."
    )
  }
}

time_series_complete <- .load_data()

# ── Extract analytic frames ───────────────────────────────────────────────────

dimension_scores <- time_series_complete %>%
  filter(country != "GCC AGGREGATE") %>%
  select(country, year,
         overall_index, trade_score, financial_score, labor_score,
         infrastructure_score, sustainability_score, convergence_score) %>%
  mutate(integration_level = case_when(
    overall_index >= 60 ~ "Good",
    overall_index >= 40 ~ "Moderate",
    TRUE                ~ "Weak"
  ))

gcc_ts <- time_series_complete %>%
  filter(country == "GCC AGGREGATE") %>%
  mutate(overall = overall_index) %>%
  select(country, year, overall,
         trade_score, financial_score, labor_score,
         infrastructure_score, sustainability_score, convergence_score)

yoy_changes <- dimension_scores %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(across(
    c(overall_index, trade_score, financial_score, labor_score,
      infrastructure_score, sustainability_score, convergence_score),
    ~ . - lag(.),
    .names = "{.col}_change"
  )) %>%
  ungroup() %>%
  filter(!is.na(overall_index_change)) %>%
  rename(overall_change = overall_index_change) %>%
  select(year, country, ends_with("_change"))

# =============================================================================
# SECTION 4  Derived constants
# =============================================================================

.dim_cols <- c(
  "trade_score", "financial_score", "labor_score",
  "infrastructure_score", "sustainability_score", "convergence_score"
)
.dim_labels_en <- c(
  "Trade", "Financial", "Labor",
  "Infrastructure", "Sustainability", "Convergence"
)

countries <- dimension_scores %>%
  filter(country != "GCC AGGREGATE") %>%
  pull(country) %>% unique() %>% sort()

country_colors <- c(
  "Bahrain"      = "#E41A1C",
  "Kuwait"       = "#377EB8",
  "Oman"         = "#4DAF4A",
  "Qatar"        = "#984EA3",
  "Saudi Arabia" = "#FF7F00",
  "UAE"          = "#A65628"
)

yr_min     <- min(dimension_scores$year, na.rm = TRUE)
yr_max     <- max(dimension_scores$year, na.rm = TRUE)
yr_min_yoy <- yr_min + 1L

# ── Input choice helpers ──────────────────────────────────────────────────────

.dim_choices <- function(lang) {
  setNames(.dim_cols, translate_dimensions(.dim_labels_en, lang))
}

.country_choices <- function(lang) {
  setNames(countries, translate_countries(countries, lang))
}

# Preserve a selectInput selection across renderUI re-renders
.keep <- function(val, default) {
  v <- isolate(val)
  if (!is.null(v) && length(v) > 0) v else default
}

# =============================================================================
# SECTION 5  Tab UI builder functions
# =============================================================================
# Each function is called inside renderUI (server) so it re-executes when
# current_lang() changes, giving fully bilingual tab bodies and box titles.
# All input IDs are stable across re-renders; current selections are preserved
# via .keep() + isolate().
# =============================================================================

.tab_overview <- function(lang, input) {
  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = t("overview_box_dashboard", lang),
          tags$p(style = "font-size:1rem; line-height:1.75;",
                 t("meta_overview_p1", lang)),
          tags$hr(),
          tags$h5(tags$strong(
            if (lang == "ar")
              "\u0645\u0632\u0627\u064a\u0627 \u0644\u0648\u062d\u0629 \u0627\u0644\u062a\u062d\u0643\u0645"
            else "Dashboard Sections:"
          )),
          tags$ul(
            tags$li(tags$strong(t("menu_gcc_overall",      lang)), " \u2014 ",
                    t("overview_box_aggregate",        lang)),
            tags$li(tags$strong(t("menu_gcc_timeseries",   lang)), " \u2014 ",
                    t("gcc_box_dimensions_time",       lang)),
            tags$li(tags$strong(t("menu_country_profiles", lang)), " \u2014 ",
                    t("cp_box_all_dim",                lang)),
            tags$li(tags$strong(t("menu_country_heatmap",  lang)), " \u2014 ",
                    t("hm_box_heatmap",                lang)),
            tags$li(tags$strong(t("menu_gcc_analytics",    lang)), " \u2014 ",
                    t("an_box_contribution",           lang)),
            tags$li(tags$strong(t("menu_data_explorer",    lang)), " \u2014 ",
                    t("de_box_explorer",               lang))
          ),
          tags$hr(),
          tags$h5(tags$strong(
            if (lang == "ar")
              "\u0633\u062a\u0629 \u0623\u0628\u0639\u0627\u062f \u0644\u0644\u062a\u0643\u0627\u0645\u0644"
            else "Six Integration Dimensions:"
          )),
          fluidRow(
            column(4, icon("exchange-alt"), " ",
                   tags$strong(translate_dimension("Trade",          lang))),
            column(4, icon("university"),   " ",
                   tags$strong(translate_dimension("Financial",      lang))),
            column(4, icon("users"),        " ",
                   tags$strong(translate_dimension("Labor",          lang)))
          ),
          tags$br(),
          fluidRow(
            column(4, icon("road"),       " ",
                   tags$strong(translate_dimension("Infrastructure", lang))),
            column(4, icon("leaf"),       " ",
                   tags$strong(translate_dimension("Sustainability", lang))),
            column(4, icon("chart-line"), " ",
                   tags$strong(translate_dimension("Convergence",    lang)))
          )
      )
    ),
    fluidRow(
      valueBoxOutput("vb_overall",   width = 4),
      valueBoxOutput("vb_year",      width = 4),
      valueBoxOutput("vb_countries", width = 4)
    )
  )
}

.tab_gcc_overall <- function(lang, input) {
  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = t("overview_box_aggregate", lang),
          plotlyOutput("gcc_overall_gauge", height = "300px"))
    ),
    fluidRow(
      box(width = 6, status = "info", solidHeader = TRUE,
          title = t("overview_box_dimension_scores", lang),
          plotlyOutput("gcc_dimension_bars", height = "400px")),
      box(width = 6, status = "info", solidHeader = TRUE,
          title = t("overview_box_score_cards", lang),
          uiOutput("dimension_boxes"))
    ),
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = t("gcc_box_trend", lang),
          plotlyOutput("gcc_overall_trend", height = "350px"))
    )
  )
}

.tab_gcc_timeseries <- function(lang, input) {
  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = t("gcc_box_dimensions_time", lang),
          fluidRow(
            column(9,
              selectInput("dimension_select",
                label    = if (lang == "ar")
                             "\u0627\u062e\u062a\u0631 \u0627\u0644\u0623\u0628\u0639\u0627\u062f"
                           else "Select Dimension(s):",
                choices  = .dim_choices(lang),
                selected = .keep(input$dimension_select, .dim_cols),
                multiple = TRUE)
            ),
            column(3,
              tags$br(),
              checkboxInput("show_overall",
                label = if (lang == "ar")
                          "\u0625\u0638\u0647\u0627\u0631 \u0627\u0644\u0645\u0624\u0634\u0631 \u0627\u0644\u0625\u062c\u0645\u0627\u0644\u064a"
                        else "Show Overall Index",
                value = .keep(input$show_overall, TRUE))
            )
          )
      )
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = TRUE,
          title = t("ts_box_dimension_ts", lang),
          plotlyOutput("gcc_dimension_timeseries", height = "450px"))
    ),
    fluidRow(
      box(width = 6, status = "success", solidHeader = TRUE,
          title = t("ts_box_dimension_compare", lang),
          plotlyOutput("dimension_radar", height = "400px")),
      box(width = 6, status = "warning", solidHeader = TRUE,
          title = t("ts_box_correlation", lang),
          plotlyOutput("dimension_correlation", height = "400px"))
    )
  )
}

.tab_country_profiles <- function(lang, input) {
  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = t("cp_box_select", lang),
          selectInput("country_select",
            label    = t("cp_label_select", lang),
            choices  = .country_choices(lang),
            selected = .keep(input$country_select, countries[1]))
      )
    ),
    fluidRow(
      valueBoxOutput("vb_country_overall", width = 3),
      valueBoxOutput("vb_country_rank",    width = 3),
      valueBoxOutput("vb_country_level",   width = 3),
      valueBoxOutput("vb_country_change",  width = 3)
    ),
    fluidRow(
      box(width = 6, status = "info", solidHeader = TRUE,
          title = t("cp_box_trend", lang),
          plotlyOutput("country_overall_trend", height = "350px")),
      box(width = 6, status = "info", solidHeader = TRUE,
          title = t("cp_box_radar", lang),
          plotlyOutput("country_dimension_current", height = "350px"))
    ),
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = t("cp_box_all_dim", lang),
          plotlyOutput("country_all_dimensions", height = "400px"))
    ),
    fluidRow(
      box(width = 12, status = "warning", solidHeader = TRUE,
          title = t("cp_box_vs_gcc", lang),
          plotlyOutput("country_vs_gcc", height = "350px"))
    )
  )
}

.tab_country_heatmap <- function(lang, input) {
  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = t("hm_box_controls", lang),
          fluidRow(
            column(5,
              selectInput("heatmap_countries",
                label    = t("axis_country", lang),
                choices  = .country_choices(lang),
                selected = .keep(input$heatmap_countries, countries),
                multiple = TRUE)
            ),
            column(5,
              selectInput("heatmap_dimensions",
                label    = t("axis_dimension", lang),
                choices  = .dim_choices(lang),
                selected = .keep(input$heatmap_dimensions, .dim_cols),
                multiple = TRUE)
            ),
            column(2,
              sliderInput("heatmap_year",
                label = t("hm_label_year", lang),
                min   = yr_min, max = yr_max,
                value = .keep(input$heatmap_year, yr_max),
                step  = 1, sep = "")
            )
          )
      )
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = TRUE,
          title = t("hm_box_heatmap", lang),
          plotlyOutput("country_dimension_heatmap", height = "500px"))
    ),
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = t("hm_box_rankings", lang),
          plotlyOutput("dimension_rankings", height = "400px"))
    )
  )
}

.tab_gcc_analytics <- function(lang, input) {
  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = t("an_box_year_select", lang),
          sliderInput("analytics_year",
            label = t("an_label_year", lang),
            min   = yr_min_yoy, max = yr_max,
            value = .keep(input$analytics_year, yr_max),
            step  = 1, sep = "")
      )
    ),
    fluidRow(
      box(width = 6, status = "info", solidHeader = TRUE,
          title = t("an_box_yoy_overall", lang),
          plotlyOutput("yoy_overall_changes", height = "400px")),
      box(width = 6, status = "info", solidHeader = TRUE,
          title = t("an_box_yoy_dim", lang),
          plotlyOutput("yoy_dimension_changes", height = "400px"))
    ),
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = t("an_box_contribution", lang),
          tags$p(class = "text-muted small",
                 t("an_info_contribution", lang)),
          plotlyOutput("contribution_analysis", height = "400px"))
    ),
    fluidRow(
      box(width = 6, status = "warning", solidHeader = TRUE,
          title = t("an_box_trends", lang),
          plotlyOutput("annual_change_trends", height = "400px")),
      box(width = 6, status = "warning", solidHeader = TRUE,
          title = t("an_box_change_heatmap", lang),
          plotlyOutput("change_heatmap", height = "400px"))
    )
  )
}

.tab_data_explorer <- function(lang, input) {
  de_choices <- c(
    "overall"     = t("de_dataset_overall",     lang),
    "trade"       = t("de_dataset_trade",        lang),
    "financial"   = t("de_dataset_financial",    lang),
    "labor"       = t("de_dataset_labor",        lang),
    "infra"       = t("de_dataset_infra",        lang),
    "sustain"     = t("de_dataset_sustain",      lang),
    "convergence" = t("de_dataset_convergence",  lang)
  )
  # flip to named-value format expected by selectInput
  de_choices_named <- setNames(names(de_choices), de_choices)

  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = t("de_box_explorer", lang),
          fluidRow(
            column(6,
              selectInput("data_table_select",
                label    = t("de_label_dataset", lang),
                choices  = de_choices_named,
                selected = .keep(input$data_table_select, "overall"))
            ),
            column(3,
              tags$br(),
              downloadButton("data_download",
                             t("de_btn_download", lang),
                             class = "btn-sm")
            )
          )
      )
    ),
    fluidRow(
      box(width = 12, status = "info", solidHeader = TRUE,
          title = t("de_box_table", lang),
          DTOutput("data_table"))
    )
  )
}

# =============================================================================
# SECTION 6  UI skeleton
# =============================================================================
# The full shinydashboard structure is rendered once at startup.
# The landing page sits on top as a fixed full-screen uiOutput overlay;
# it returns NULL once the user enters the dashboard.
# Tab bodies are uiOutput placeholders refreshed when language changes.
# =============================================================================

ui <- dashboardPage(
  skin = "blue",

  # ── Header ──────────────────────────────────────────────────────────────────
  dashboardHeader(
    title     = "GCCEII",
    titleWidth = 220,
    # Language toggle in the header bar
    tags$li(
      class = "dropdown",
      style = "padding: 8px 14px 0;",
      tags$button(
        id      = "dash_lang_en",
        class   = "btn btn-xs btn-default lang-btn active",
        onclick = "setLang('en')",
        "EN"
      ),
      tags$span("\u00a0"),
      tags$button(
        id      = "dash_lang_ar",
        class   = "btn btn-xs btn-default lang-btn",
        onclick = "setLang('ar')",
        "\u0639\u0631"          # عر
      )
    )
  ),

  # ── Sidebar ──────────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 240,
    sidebarMenuOutput("sidebar_menu")
  ),

  # ── Body ─────────────────────────────────────────────────────────────────────
  dashboardBody(

    # ── <head> assets ──────────────────────────────────────────────────────────
    tags$head(
      # Google Fonts (Arabic-capable)
      tags$link(rel = "preconnect",  href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect",  href = "https://fonts.gstatic.com",
                crossorigin = NA),
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Cairo:wght@300;400;600;700&family=Tajawal:wght@300;400;500;700&display=swap"),
      # RTL stylesheet (www/rtl.css)
      tags$link(rel = "stylesheet",  type = "text/css", href = "rtl.css"),
      # Inline fixes
      tags$style(HTML("
        .content-wrapper, .right-side { background: #f4f6f9; }
        .box       { box-shadow: 0 1px 3px rgba(0,0,0,.12); }
        .small-box { box-shadow: 0 2px 4px rgba(0,0,0,.15); }
        .info-box  { box-shadow: 0 1px 3px rgba(0,0,0,.12); }
        .lang-btn          { min-width:36px; font-weight:600; margin:0 2px; }
        .lang-btn.active   { background:#3c8dbc !important;
                             color:white       !important;
                             border-color:#367fa9 !important; }
      ")),
      # Shared setLang() JS — works on both landing page and dashboard
      tags$script(HTML("
        function setLang(lang) {
          Shiny.setInputValue('selected_lang', lang, {priority: 'event'});
          document.body.classList.toggle('lang-ar', lang === 'ar');
          ['btn-lang-en','btn-lang-ar','dash_lang_en','dash_lang_ar'].forEach(function(id) {
            var el = document.getElementById(id);
            if (el) el.classList.remove('active');
          });
          var toActivate = lang === 'ar'
            ? ['btn-lang-ar', 'dash_lang_ar']
            : ['btn-lang-en', 'dash_lang_en'];
          toActivate.forEach(function(id) {
            var el = document.getElementById(id);
            if (el) el.classList.add('active');
          });
        }
      "))
    ),

    # ── Landing page overlay ────────────────────────────────────────────────────
    # Renders a full-screen fixed div on startup; becomes NULL after entry.
    uiOutput("landing_overlay"),

    # ── Tab items (bodies filled reactively in server) ──────────────────────────
    tabItems(
      tabItem(tabName = "overview",         uiOutput("tab_overview")),
      tabItem(tabName = "gcc_overall",      uiOutput("tab_gcc_overall")),
      tabItem(tabName = "gcc_timeseries",   uiOutput("tab_gcc_timeseries")),
      tabItem(tabName = "country_profiles", uiOutput("tab_country_profiles")),
      tabItem(tabName = "country_heatmap",  uiOutput("tab_country_heatmap")),
      tabItem(tabName = "gcc_analytics",    uiOutput("tab_gcc_analytics")),
      tabItem(tabName = "data_explorer",    uiOutput("tab_data_explorer")),
      tabItem(tabName = "metadata",         uiOutput("tab_metadata"))
    )
  )
)

# =============================================================================
# SECTION 7  Server
# =============================================================================

server <- function(input, output, session) {

  # ── Reactive state ────────────────────────────────────────────────────────────

  current_lang <- reactiveVal("en")
  current_page <- reactiveVal("landing")

  observeEvent(input$selected_lang, {
    req(input$selected_lang %in% c("en", "ar"))
    current_lang(input$selected_lang)
  })

  # ── Landing page module ────────────────────────────────────────────────────────

  landing <- landing_page_server("landing")

  observeEvent(landing$enter(), {
    req(landing$enter())
    current_page("dashboard")
    updateTabItems(session, "tabs", selected = "overview")
  })

  # ── Landing overlay ────────────────────────────────────────────────────────────

  output$landing_overlay <- renderUI({
    if (current_page() == "landing") {
      tags$div(
        style = paste0(
          "position:fixed; top:0; left:0;",
          "width:100vw; height:100vh;",
          "z-index:9999; overflow-y:auto; background:white;"
        ),
        landing_page_ui("landing", lang = current_lang())
      )
    } else {
      NULL
    }
  })

  # ── Sidebar (reactive labels) ──────────────────────────────────────────────────

  output$sidebar_menu <- renderMenu({
    l <- current_lang()
    sidebarMenu(
      id = "tabs",
      menuItem(t("menu_overview",         l), tabName = "overview",         icon = icon("dashboard")),
      menuItem(t("menu_gcc_overall",      l), tabName = "gcc_overall",      icon = icon("chart-line")),
      menuItem(t("menu_gcc_timeseries",   l), tabName = "gcc_timeseries",   icon = icon("chart-area")),
      menuItem(t("menu_country_profiles", l), tabName = "country_profiles", icon = icon("flag")),
      menuItem(t("menu_country_heatmap",  l), tabName = "country_heatmap",  icon = icon("th")),
      menuItem(t("menu_gcc_analytics",    l), tabName = "gcc_analytics",    icon = icon("chart-bar")),
      menuItem(t("menu_data_explorer",    l), tabName = "data_explorer",    icon = icon("table")),
      menuItem(t("menu_metadata",         l), tabName = "metadata",         icon = icon("book"))
    )
  })

  # ── Tab bodies (re-render on language change) ──────────────────────────────────

  output$tab_overview         <- renderUI(.tab_overview(current_lang(), input))
  output$tab_gcc_overall      <- renderUI(.tab_gcc_overall(current_lang(), input))
  output$tab_gcc_timeseries   <- renderUI(.tab_gcc_timeseries(current_lang(), input))
  output$tab_country_profiles <- renderUI(.tab_country_profiles(current_lang(), input))
  output$tab_country_heatmap  <- renderUI(.tab_country_heatmap(current_lang(), input))
  output$tab_gcc_analytics    <- renderUI(.tab_gcc_analytics(current_lang(), input))
  output$tab_data_explorer    <- renderUI(.tab_data_explorer(current_lang(), input))

  # Metadata: module handles its own internal DTs
  output$tab_metadata <- renderUI(metadata_ui("metadata", lang = current_lang()))
  metadata_server("metadata", lang = current_lang)

  # ============================================================================
  # CHART OUTPUTS
  # All chart functions are sourced from R/mod_charts.R.
  # ============================================================================

  # ── Overview KPI value boxes ──────────────────────────────────────────────────

  output$vb_overall <- renderValueBox({
    latest_gcc <- gcc_ts %>% filter(year == max(year))
    valueBox(round(latest_gcc$overall, 1),
             t("vb_gcc_index",  current_lang()), icon("chart-line"), color = "blue")
  })

  output$vb_year <- renderValueBox({
    valueBox(max(gcc_ts$year),
             t("vb_latest_year", current_lang()), icon("calendar"), color = "green")
  })

  output$vb_countries <- renderValueBox({
    valueBox(length(countries),
             t("vb_members", current_lang()), icon("flag"), color = "yellow")
  })

  # ── GCC Overall ───────────────────────────────────────────────────────────────

  output$gcc_overall_gauge <- renderPlotly(
    chart_gcc_gauge(gcc_ts, lang = current_lang())
  )
  output$gcc_dimension_bars <- renderPlotly(
    chart_dimension_bars(gcc_ts, lang = current_lang())
  )
  output$dimension_boxes <- renderUI(
    chart_dimension_scoreboxes(gcc_ts, lang = current_lang())
  )
  output$gcc_overall_trend <- renderPlotly(
    chart_gcc_trend(gcc_ts, lang = current_lang())
  )

  # ── GCC Timeseries ────────────────────────────────────────────────────────────

  output$gcc_dimension_timeseries <- renderPlotly({
    req(input$dimension_select)
    chart_gcc_dimension_ts(gcc_ts,
                           selected_cols = input$dimension_select,
                           show_overall  = isTRUE(input$show_overall),
                           lang          = current_lang())
  })

  output$dimension_radar <- renderPlotly({
    latest <- gcc_ts %>% filter(year == max(year))
    scores <- c(latest$trade_score, latest$financial_score,
                latest$labor_score, latest$infrastructure_score,
                latest$sustainability_score, latest$convergence_score)
    chart_radar(scores, label = "GCC", lang = current_lang())
  })

  output$dimension_correlation <- renderPlotly(
    chart_correlation(gcc_ts, lang = current_lang())
  )

  # ── Country Profiles ──────────────────────────────────────────────────────────

  # country_select value is always an English country name (it is the *value*
  # of the named choice vector, not the displayed label).
  country_data_r <- reactive({
    req(input$country_select)
    dimension_scores %>% filter(country == input$country_select)
  })

  output$vb_country_overall <- renderValueBox({
    latest <- country_data_r() %>% filter(year == max(year))
    valueBox(round(latest$overall_index, 1),
             t("vb_overall_index", current_lang()), icon("chart-line"), color = "blue")
  })

  output$vb_country_rank <- renderValueBox({
    ly   <- max(dimension_scores$year)
    rnks <- dimension_scores %>%
      filter(year == ly) %>%
      arrange(desc(overall_index)) %>%
      mutate(rank = row_number())
    r <- rnks %>% filter(country == input$country_select) %>% pull(rank)
    valueBox(paste0("#", r, " / ", nrow(rnks)),
             t("vb_country_rank", current_lang()), icon("trophy"), color = "yellow")
  })

  output$vb_country_level <- renderValueBox({
    latest <- country_data_r() %>% filter(year == max(year))
    valueBox(translate_level(latest$integration_level, current_lang()),
             t("vb_integration_lvl", current_lang()),
             icon("signal"),
             color = if (latest$integration_level == "Good") "green" else "orange")
  })

  output$vb_country_change <- renderValueBox({
    data <- country_data_r()
    if (nrow(data) >= 2) {
      now  <- data %>% filter(year == max(year))     %>% pull(overall_index)
      prev <- data %>% filter(year == max(year) - 1) %>% pull(overall_index)
      chg  <- now - prev
      valueBox(paste0(if (chg > 0) "+" else "", round(chg, 1)),
               t("vb_yoy_overall", current_lang()),
               icon(if (chg > 0) "arrow-up" else "arrow-down"),
               color = if (chg > 0) "green" else "red")
    } else {
      valueBox("N/A", t("vb_yoy_overall", current_lang()),
               icon("minus"), color = "gray")
    }
  })

  output$country_overall_trend <- renderPlotly(
    chart_country_trend(country_data_r(),
                        country_name = input$country_select,
                        lang         = current_lang())
  )

  output$country_dimension_current <- renderPlotly({
    latest <- country_data_r() %>% filter(year == max(year))
    chart_country_dims_current(latest,
                               country_name = input$country_select,
                               lang         = current_lang())
  })

  output$country_all_dimensions <- renderPlotly(
    chart_country_all_dims(country_data_r(), lang = current_lang())
  )

  output$country_vs_gcc <- renderPlotly(
    chart_country_vs_gcc(dimension_scores, gcc_ts,
                         country_name = input$country_select,
                         lang         = current_lang())
  )

  # ── Country Heatmap ───────────────────────────────────────────────────────────

  output$country_dimension_heatmap <- renderPlotly({
    req(input$heatmap_countries, input$heatmap_dimensions, input$heatmap_year)
    chart_country_heatmap(dimension_scores,
                          selected_year      = input$heatmap_year,
                          selected_countries = input$heatmap_countries,
                          selected_dims      = input$heatmap_dimensions,
                          lang               = current_lang())
  })

  output$dimension_rankings <- renderPlotly({
    req(input$heatmap_dimensions, input$heatmap_year)
    chart_rankings(dimension_scores,
                   selected_year = input$heatmap_year,
                   selected_dims = input$heatmap_dimensions,
                   lang          = current_lang())
  })

  # ── GCC Analytics ─────────────────────────────────────────────────────────────

  output$yoy_overall_changes <- renderPlotly({
    req(input$analytics_year)
    chart_yoy_overall(yoy_changes,
                      selected_year = input$analytics_year,
                      lang          = current_lang())
  })

  output$yoy_dimension_changes <- renderPlotly({
    req(input$analytics_year)
    chart_yoy_dimensions(yoy_changes,
                         selected_year = input$analytics_year,
                         lang          = current_lang())
  })

  output$contribution_analysis <- renderPlotly({
    req(input$analytics_year)
    if (input$analytics_year <= yr_min) return(plotly_empty())
    chart_contribution(dimension_scores,
                       selected_year = input$analytics_year,
                       lang          = current_lang())
  })

  output$annual_change_trends <- renderPlotly(
    chart_annual_trends(yoy_changes, lang = current_lang())
  )

  output$change_heatmap <- renderPlotly(
    chart_change_heatmap(yoy_changes, lang = current_lang())
  )

  # ── Data Explorer ─────────────────────────────────────────────────────────────

  .col_names <- function(lang) {
    c(t("col_country",     lang), t("col_year",     lang),
      t("col_overall",     lang), t("col_trade",    lang),
      t("col_financial",   lang), t("col_labor",    lang),
      t("col_infra",       lang), t("col_sustain",  lang),
      t("col_convergence", lang), t("col_level",    lang))
  }

  .explorer_df <- reactive({
    l  <- current_lang()
    ds <- input$data_table_select %||% "overall"

    base <- dimension_scores %>%
      mutate(
        country           = translate_countries(country, l),
        integration_level = translate_level(integration_level, l)
      ) %>%
      select(country, year,
             overall_index, trade_score, financial_score, labor_score,
             infrastructure_score, sustainability_score, convergence_score,
             integration_level)

    # Filter columns by selected dataset
    cols_to_show <- switch(ds,
      "trade"       = c(1, 2, 4),        # country, year, trade_score
      "financial"   = c(1, 2, 5),        # country, year, financial_score
      "labor"       = c(1, 2, 6),        # country, year, labor_score
      "infra"       = c(1, 2, 7),        # country, year, infrastructure_score
      "sustain"     = c(1, 2, 8),        # country, year, sustainability_score
      "convergence" = c(1, 2, 9),        # country, year, convergence_score
      seq_len(ncol(base))                # "overall" → all columns
    )

    setNames(base[, cols_to_show, drop = FALSE], .col_names(l)[cols_to_show])
  })

  output$data_table <- renderDT({
    datatable(
      .explorer_df(),
      filter     = "top",
      extensions = "Buttons",
      options    = list(
        pageLength      = 25,
        scrollX         = TRUE,
        searchHighlight = TRUE,
        dom             = "Bfrtip",
        buttons         = c("copy", "csv", "excel"),
        language        = if (current_lang() == "ar")
          list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/ar.json")
        else list()
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = which(sapply(.explorer_df(), is.numeric)), digits = 2)
  })

  output$data_download <- downloadHandler(
    filename = function()
      paste0("GCCEII_", input$data_table_select %||% "data",
             "_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) write_csv(.explorer_df(), file)
  )

}  # end server

# =============================================================================
# SECTION 8  Run
# =============================================================================

shinyApp(ui = ui, server = server)
