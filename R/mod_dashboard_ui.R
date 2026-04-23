# ==============================================================================
# DASHBOARD UI — FIVE-TAB STRUCTURE
# ==============================================================================
# @phase: 2 — Information architecture and navigation
#
# Replaces the old 8-tab dashboard_ui() defined inline in app.R.
# Source this file from app.R BEFORE shinyApp():
#   source("R/mod_dashboard_ui.R")
# Then call dashboard_ui_new(lang, countries) instead of dashboard_ui(lang).
#
# Tab mapping (old -> new):
#   overview         -> retired; value boxes promoted to Executive Pulse
#   gcc_overall      -> Executive Pulse
#   gcc_analytics    -> Biggest movers -> Dimension Deep-Dive
#                       Waterfall      -> Data Lab
#                       Rest           -> Dimension Deep-Dive
#   gcc_timeseries   -> Dimension Deep-Dive
#   country_profiles -> Country Profiles  (same content, country selector global)
#   country_heatmap  -> Data Lab
#   data_explorer    -> Data Lab
#   metadata         -> Methodology
#
# Input IDs are PRESERVED so the server section of app.R needs no changes.
# ==============================================================================

# ------------------------------------------------------------------------------
# Helper: tab narrative subtitle
# ------------------------------------------------------------------------------
tab_subtitle <- function(en, ar, lang) {
  tags$p(
    class = "gcc-tab-subtitle",
    if (identical(lang, "ar")) ar else en
  )
}


# ------------------------------------------------------------------------------
# Main entry point
# ------------------------------------------------------------------------------

#' Build the dashboard UI with the new 5-tab structure
#'
#' @param lang "en" | "ar"
#' @param countries character vector of country names (from get_countries())
dashboard_ui_new <- function(lang = "en", countries = character(0)) {
  tagList(
    dashboardPage(
      skin = "blue",

      # --- Header -----------------------------------------------------------
      dashboardHeader(
        title    = t("app_title", lang),
        titleWidth = 380,
        tags$li(class = "dropdown",
          actionButton(
            "lang_toggle",
            label = if (identical(lang, "ar")) "English" else "عربي",
            class = "lang-toggle-btn",
            style = paste(
              "margin-top: 8px; margin-right: 10px; color: #fff;",
              "background: transparent;",
              "border: 1px solid rgba(255,255,255,0.5);",
              "padding: 4px 12px; font-size: 13px;"
            )
          )
        )
      ),

      # --- Sidebar with global controls ------------------------------------
      dashboardSidebar(
        width = 260,
        sidebarMenu(
          id = "tabs",
          menuItem(
            if (identical(lang, "ar")) "النبض التنفيذي"
            else "Executive Pulse",
            tabName = "executive_pulse", icon = icon("tachometer-alt")
          ),
          menuItem(
            if (identical(lang, "ar")) "الملفات القُطرية"
            else "Country Profiles",
            tabName = "country_profiles", icon = icon("flag")
          ),
          menuItem(
            if (identical(lang, "ar")) "التحليل العميق"
            else "Dimension Deep-Dive",
            tabName = "dimension_deep_dive", icon = icon("chart-area")
          ),
          menuItem(
            if (identical(lang, "ar")) "مختبر البيانات"
            else "Data Lab",
            tabName = "data_lab", icon = icon("flask")
          ),
          menuItem(
            if (identical(lang, "ar")) "المنهجية"
            else "Methodology",
            tabName = "methodology", icon = icon("info-circle")
          )
        ),

        hr(),

        # Global country selector — same input ID as before so server is unchanged
        div(
          class = "sidebar-global-control",
          selectInput(
            "selected_country",
            label = if (identical(lang, "ar")) "الدولة" else "Country",
            choices = if (length(countries) > 0)
              setNames(countries, translate_countries(countries, lang))
            else c("UAE" = "UAE"),
            selected = if (length(countries) > 0) countries[1] else "UAE"
          )
        ),

        # Global year selector — same input ID as heatmap_year so server is unchanged
        div(
          class = "sidebar-global-control",
          selectInput(
            "heatmap_year",
            label = if (identical(lang, "ar")) "السنة" else "Year",
            choices = sort(unique(dimension_scores$year), decreasing = TRUE),
            selected = max(dimension_scores$year)
          )
        )
      ),

      # --- Body -------------------------------------------------------------
      dashboardBody(
        tabItems(
          executive_pulse_tab(lang),
          country_profiles_tab(lang),
          dimension_deep_dive_tab(lang),
          data_lab_tab(lang),
          methodology_tab(lang)
        )
      )
    ),

    # Back-to-welcome button (outside dashboardPage, always visible)
    actionButton(
      "back_to_welcome",
      label = tagList(icon("home"), paste0(" ", t("btn_back_welcome", lang))),
      class = "back-to-welcome"
    )
  )
}


# ------------------------------------------------------------------------------
# Tab 1: Executive Pulse
# "How integrated is the GCC, and who is leading?"
# Content: value boxes (ex-overview) + gauge + dim bars + dim cards + ranking
# ------------------------------------------------------------------------------
executive_pulse_tab <- function(lang = "en") {
  tabItem(
    tabName = "executive_pulse",
    tab_subtitle(
      "How integrated is the GCC, and who is leading?",
      "ما مدى تكامل دول مجلس التعاون؟ ومن يتصدّر المشهد؟",
      lang
    ),
    # Headline KPIs (from retired overview tab)
    fluidRow(
      valueBoxOutput("latest_overall",  width = 4),
      valueBoxOutput("latest_year",     width = 4),
      valueBoxOutput("num_countries",   width = 4)
    ),
    # Gauge + dimension score cards
    fluidRow(
      box(width = 6, title = t("gcc_box_aggregate", lang),
          status = "primary", solidHeader = TRUE,
          plotlyOutput("gcc_overall_gauge", height = "300px")),
      box(width = 6, title = t("gcc_box_score_cards", lang),
          status = "info", solidHeader = TRUE,
          uiOutput("dimension_boxes"))
    ),
    # Dimension bar chart
    fluidRow(
      box(width = 12, title = t("gcc_box_dim_scores", lang),
          status = "info", solidHeader = TRUE,
          plotlyOutput("gcc_dimension_bars", height = "350px"))
    ),
    # Country ranking
    fluidRow(
      box(width = 12, title = t("gcc_box_ranking", lang),
          status = "success", solidHeader = TRUE,
          plotlyOutput("country_ranking", height = "350px"))
    )
  )
}


# ------------------------------------------------------------------------------
# Tab 2: Country Profiles
# "How does each member state perform across dimensions?"
# Country selector moved to global sidebar; content otherwise unchanged.
# ------------------------------------------------------------------------------
country_profiles_tab <- function(lang = "en") {
  tabItem(
    tabName = "country_profiles",
    tab_subtitle(
      "How does each member state perform across dimensions?",
      "كيف يؤدي كل عضو عبر الأبعاد؟",
      lang
    ),
    # Value boxes (country selector is now in the global sidebar)
    fluidRow(
      valueBoxOutput("country_overall", width = 6),
      valueBoxOutput("country_rank",    width = 6)
    ),
    # Radar + country vs GCC trend
    fluidRow(
      box(width = 5, title = t("cp_box_radar", lang),
          status = "info", solidHeader = TRUE,
          plotlyOutput("country_radar", height = "380px")),
      box(width = 7, title = t("cp_box_vs_gcc", lang),
          status = "info", solidHeader = TRUE,
          plotlyOutput("country_vs_gcc_combined", height = "380px"))
    ),
    # Dimension selector for indicator drill-down
    fluidRow(
      box(width = 12, title = t("cp_box_indicator_detail", lang),
          status = "primary", solidHeader = TRUE,
          fluidRow(
            column(4,
              selectInput("cp_dimension_select", t("cp_dim_select_label", lang),
                          choices = setNames(DIMENSION_LABELS,
                                            translate_dimensions(DIMENSION_LABELS, lang)),
                          selected = "Trade")
            ),
            column(8,
              div(style = "padding-top: 25px; color: #666; font-size: 0.9rem;",
                  icon("info-circle"), t("cp_dim_info", lang))
            )
          )
      )
    ),
    # Lollipop + indicator trend subplots
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


# ------------------------------------------------------------------------------
# Tab 3: Dimension Deep-Dive
# "Which dimensions drive or drag integration over time?"
# Content: timeseries + analytics (stacked area, spread, contribution, movers)
# ------------------------------------------------------------------------------
dimension_deep_dive_tab <- function(lang = "en") {
  tabItem(
    tabName = "dimension_deep_dive",
    tab_subtitle(
      "Which dimensions drive or drag integration over time?",
      "أيّ الأبعاد تدفع التكامل أو تُعيقه؟",
      lang
    ),
    # Summary bar + GCC overall trend
    fluidRow(column(12, uiOutput("ts_summary_bar"))),
    fluidRow(
      box(width = 12, title = t("ts_box_overall_trend", lang),
          status = "primary", solidHeader = TRUE,
          plotlyOutput("gcc_overall_trend", height = "250px"))
    ),
    # Dimension selector pills
    fluidRow(
      column(12,
        div(class = "dimension-selector",
          tags$label(t("ts_pills_label", lang), class = "dim-selector-label"),
          div(class = "dim-pills",
            actionButton("dim_btn_Trade",
              tagList(icon("exchange-alt"), paste0(" ", translate_dimension("Trade", lang))),
              class = "dim-pill dim-pill-trade"),
            actionButton("dim_btn_Financial",
              tagList(icon("university"), paste0(" ", translate_dimension("Financial", lang))),
              class = "dim-pill dim-pill-financial"),
            actionButton("dim_btn_Labor",
              tagList(icon("users"), paste0(" ", translate_dimension("Labor", lang))),
              class = "dim-pill dim-pill-labor"),
            actionButton("dim_btn_Infrastructure",
              tagList(icon("road"), paste0(" ", translate_dimension("Infrastructure", lang))),
              class = "dim-pill dim-pill-infrastructure"),
            actionButton("dim_btn_Sustainability",
              tagList(icon("leaf"), paste0(" ", translate_dimension("Sustainability", lang))),
              class = "dim-pill dim-pill-sustainability"),
            actionButton("dim_btn_Convergence",
              tagList(icon("chart-line"), paste0(" ", translate_dimension("Convergence", lang))),
              class = "dim-pill dim-pill-convergence")
          )
        ),
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
    # Dimension country comparison
    fluidRow(
      box(width = 12,
          title = uiOutput("ts_dimension_title"),
          status = "info", solidHeader = TRUE,
          plotlyOutput("ts_dimension_countries", height = "350px"))
    ),
    # Indicator small multiples
    fluidRow(
      box(width = 12,
          title = uiOutput("ts_indicators_title"),
          status = "success", solidHeader = TRUE,
          plotlyOutput("ts_indicator_multiples", height = "500px"))
    ),
    # Annual dynamics (stacked area + annual bars)
    fluidRow(
      box(width = 12, title = t("an_section2_title", lang),
          status = "primary", solidHeader = TRUE,
          p(style = "color: #666; font-size: 0.9rem; margin-bottom: 0;",
            icon("info-circle"), t("an_section2_info", lang)))
    ),
    fluidRow(
      box(width = 6, title = t("an_box_stacked_area", lang),
          status = "success", solidHeader = TRUE,
          plotlyOutput("analytics_stacked_area", height = "400px")),
      box(width = 6, title = t("an_box_annual_dim_bars", lang),
          status = "success", solidHeader = TRUE,
          plotlyOutput("analytics_annual_dim_bars", height = "400px"))
    ),
    # Cross-country spread
    fluidRow(
      box(width = 12,
          title = uiOutput("analytics_spread_title"),
          status = "primary", solidHeader = TRUE,
          p(style = "color: #666; font-size: 0.9rem; margin-bottom: 5px;",
            icon("info-circle"), t("an_spread_info", lang)),
          plotlyOutput("analytics_spread_chart", height = "380px"))
    ),
    # Country contribution
    fluidRow(
      box(width = 12,
          title = uiOutput("analytics_country_contrib_title"),
          status = "info", solidHeader = TRUE,
          plotlyOutput("analytics_country_contribution", height = "420px"))
    ),
    # Biggest movers
    fluidRow(
      box(width = 12,
          title = uiOutput("analytics_movers_title"),
          status = "primary", solidHeader = TRUE,
          p(style = "color: #666; font-size: 0.9rem; margin-bottom: 5px;",
            icon("info-circle"), t("an_movers_info", lang)),
          plotlyOutput("analytics_biggest_movers", height = "450px"))
    )
  )
}


# ------------------------------------------------------------------------------
# Tab 4: Data Lab
# "Explore the underlying scores and year-on-year shifts."
# Content: waterfall decomposition + heatmap + data explorer table
# Year selector in global sidebar controls the heatmap snapshot.
# ------------------------------------------------------------------------------
data_lab_tab <- function(lang = "en") {
  tabItem(
    tabName = "data_lab",
    tab_subtitle(
      "Explore the underlying scores and year-on-year shifts.",
      "استكشف الدرجات والتغيّرات السنوية.",
      lang
    ),
    # Period decomposition (waterfall)
    fluidRow(
      box(width = 12, title = t("an_section1_title", lang),
          status = "primary", solidHeader = TRUE,
          fluidRow(
            column(3,
              selectInput("analytics_from_year", t("an_label_from", lang),
                          choices  = sort(unique(dimension_scores$year)),
                          selected = max(dimension_scores$year) - 1)
            ),
            column(3,
              selectInput("analytics_to_year", t("an_label_to", lang),
                          choices  = sort(unique(dimension_scores$year)),
                          selected = max(dimension_scores$year))
            ),
            column(6,
              div(style = "padding-top: 25px; color: #666; font-size: 0.9rem;",
                  icon("info-circle"), t("an_section1_info", lang))
            )
          )
      )
    ),
    fluidRow(
      box(width = 12,
          title = uiOutput("analytics_waterfall_title"),
          status = "info", solidHeader = TRUE,
          plotlyOutput("analytics_waterfall", height = "420px"))
    ),
    # Heatmap (year controlled by global sidebar heatmap_year selector)
    fluidRow(
      box(width = 12, title = t("hm_box_heatmap", lang),
          status = "info", solidHeader = TRUE,
          plotlyOutput("country_heatmap", height = "500px"))
    ),
    fluidRow(
      box(width = 12, title = t("hm_box_levels", lang),
          status = "success", solidHeader = TRUE,
          plotlyOutput("integration_levels", height = "350px"))
    ),
    # Data explorer table
    fluidRow(
      box(width = 12, title = t("de_box_title", lang),
          status = "primary", solidHeader = TRUE,
          selectInput("data_table_select", t("de_label_dataset", lang),
                      choices = setNames(
                        c("dimension", "gcc", "yoy", "indicator"),
                        c(t("de_dataset_dimension", lang), t("de_dataset_gcc", lang),
                          t("de_dataset_yoy",       lang), t("de_dataset_indicator", lang))
                      )),
          DTOutput("data_table"))
    )
  )
}


# ------------------------------------------------------------------------------
# Tab 5: Methodology
# "How the index is constructed and what each indicator measures."
# Delegates to existing metadata_content_ui() from mod_metadata.R.
# ------------------------------------------------------------------------------
methodology_tab <- function(lang = "en") {
  tabItem(
    tabName = "methodology",
    tab_subtitle(
      "How the index is constructed and what each indicator measures.",
      "كيفية بناء المؤشر وما يقيسه كل مؤشر.",
      lang
    ),
    metadata_content_ui(lang)
  )
}
