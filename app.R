# ==============================================================================
# GCC ECONOMIC INTEGRATION INDEX DASHBOARD
# Version: 2.0 - With Landing Page
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

# Load data from master script outputs
OUTPUT_DIR <- "output"

# Option 1: Load from saved workspace (fastest)
if (file.exists(file.path(OUTPUT_DIR, "gcc_integration_workspace.RData"))) {
  message("Loading data from workspace file...")
  load(file.path(OUTPUT_DIR, "gcc_integration_workspace.RData"))
  
  # Extract dimension scores for COUNTRIES ONLY (exclude GCC aggregate)
  if (exists("time_series_complete")) {
    dimension_scores <- time_series_complete %>%
      filter(country != "GCC") %>%
      select(country, year, overall_index,
             trade_score, financial_score, labor_score,
             infrastructure_score, sustainability_score, convergence_score) %>%
      mutate(integration_level = case_when(
        overall_index >= 60 ~ "Good",
        overall_index >= 40 ~ "Moderate",
        TRUE ~ "Weak"
      ))
    
    # Extract GCC aggregate timeseries (GDP-weighted only for display)
    gcc_ts <- time_series_complete %>%
      filter(country == "GCC")
    
    # If method column exists, filter to GDP-weighted
    if ("method" %in% names(gcc_ts)) {
      gcc_ts <- gcc_ts %>% filter(method == "gdp")
    }
    
    # Rename overall_index to overall for consistency
    gcc_ts <- gcc_ts %>%
      mutate(overall = overall_index) %>%
      select(country, year, overall, trade_score, financial_score, labor_score,
             infrastructure_score, sustainability_score, convergence_score,
             integration_level)
    
    # Country vs GCC comparison (countries + GCC aggregate)
    if ("method" %in% names(time_series_complete)) {
      country_data <- time_series_complete %>%
        filter(country != "GCC AGGREGATE" | (country == "GCC AGGREGATE" & method == "gdp")) %>%
        select(year, country, trade_score, financial_score, labor_score,
               infrastructure_score, sustainability_score, convergence_score)
    } else {
      country_data <- time_series_complete %>%
        filter(country != "GCC AGGREGATE" | country == "GCC AGGREGATE") %>%
        select(year, country, trade_score, financial_score, labor_score,
               infrastructure_score, sustainability_score, convergence_score)
    }
    
  } else {
    # Fallback to country_index_2023 if time_series_complete doesn't exist
    dimension_scores <- country_index_2023 %>%
      mutate(year = 2023) %>%
      select(country, year, overall_index,
             trade_score, financial_score, labor_score,
             infrastructure_score, sustainability_score, convergence_score,
             integration_level)
    
    # Create GCC timeseries from 2023 data
    gcc_ts <- gcc_aggregate_2023 %>%
      filter(method == "gdp") %>%
      mutate(overall = overall_index, year = 2023) %>%
      select(country, year, overall, trade_score, financial_score, labor_score,
             infrastructure_score, sustainability_score, convergence_score,
             integration_level)
    
    country_data <- dimension_scores %>%
      select(year, country, trade_score, financial_score, labor_score,
             infrastructure_score, sustainability_score, convergence_score)
  }
  
  # Calculate year-over-year changes for COUNTRIES ONLY
  yoy_changes <- dimension_scores %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      overall_change = overall_index - lag(overall_index),
      trade_change = trade_score - lag(trade_score),
      financial_change = financial_score - lag(financial_score),
      labor_change = labor_score - lag(labor_score),
      infrastructure_change = infrastructure_score - lag(infrastructure_score),
      sustainability_change = sustainability_score - lag(sustainability_score),
      convergence_change = convergence_score - lag(convergence_score)
    ) %>%
    ungroup() %>%
    filter(!is.na(overall_change)) %>%
    select(year, country, ends_with("_change"))
  
  message("✓ Data loaded from workspace")
  message(paste("  - Countries:", n_distinct(dimension_scores$country)))
  message(paste("  - Years:", paste(range(dimension_scores$year), collapse = "-")))
  
} else if (file.exists(file.path(OUTPUT_DIR, "time_series_complete.csv"))) {
  # Option 2: Load from CSV files
  message("Loading data from CSV files...")
  
  time_series_complete <- read_csv(file.path(OUTPUT_DIR, "time_series_complete.csv"),
                                   show_col_types = FALSE)
  
  # Extract dimension scores for COUNTRIES ONLY
  dimension_scores <- time_series_complete %>%
    filter(country != "GCC") %>%
    select(country, year, overall_index,
           trade_score, financial_score, labor_score,
           infrastructure_score, sustainability_score, convergence_score) %>%
    mutate(integration_level = case_when(
      overall_index >= 60 ~ "Good",
      overall_index >= 40 ~ "Moderate",
      TRUE ~ "Weak"
    ))
  
  # Extract GCC aggregate (GDP-weighted if method exists)
  gcc_ts <- time_series_complete %>%
    filter(country == "GCC")
  
  if ("method" %in% names(gcc_ts)) {
    gcc_ts <- gcc_ts %>% filter(method == "gdp")
  }
  
  gcc_ts <- gcc_ts %>%
    mutate(overall = overall_index) %>%
    select(country, year, overall, trade_score, financial_score, labor_score,
           infrastructure_score, sustainability_score, convergence_score,
           integration_level)
  
  # Country vs GCC comparison
  country_data <- time_series_complete %>%
    filter((country != "GCC") | 
             (country == "GCC" & if_else("method" %in% names(.), method == "gdp", TRUE))) %>%
    select(year, country, trade_score, financial_score, labor_score,
           infrastructure_score, sustainability_score, convergence_score)
  
  # Calculate YoY changes for COUNTRIES ONLY
  yoy_changes <- dimension_scores %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      overall_change = overall_index - lag(overall_index),
      trade_change = trade_score - lag(trade_score),
      financial_change = financial_score - lag(financial_score),
      labor_change = labor_score - lag(labor_score),
      infrastructure_change = infrastructure_score - lag(infrastructure_score),
      sustainability_change = sustainability_score - lag(sustainability_score),
      convergence_change = convergence_score - lag(convergence_score)
    ) %>%
    ungroup() %>%
    filter(!is.na(overall_change)) %>%
    select(year, country, ends_with("_change"))
  
  message("✓ Data loaded from CSV files")
  message(paste("  - Countries:", n_distinct(dimension_scores$country)))
  message(paste("  - Years:", paste(range(dimension_scores$year), collapse = "-")))
  
} else {
  stop("ERROR: Could not find data files. Please ensure either:\n",
       "  1. gcc_integration_workspace.RData exists in output/ directory, OR\n",
       "  2. time_series_complete.csv exists in output/ directory\n\n",
       "Run the GCC_Integration_Master.R script first to generate these files.")
}

# Define dimension columns
dimension_cols <- c("trade_score", "financial_score", "labor_score", 
                    "infrastructure_score", "sustainability_score", "convergence_score")
dimension_labels <- c("Trade", "Financial", "Labor", 
                      "Infrastructure", "Sustainability", "Convergence")

# Get unique countries (excluding GCC aggregate)
countries <- dimension_scores %>% 
  filter(country != "GCC") %>% 
  pull(country) %>% 
  unique() %>% 
  sort()

# Color palette for countries
country_colors <- c(
  "UAE" = "#000000",
  "Qatar" = "#99154C",
  "Saudi Arabia" = "#008035",
  "Bahrain" = "#E20000",
  "Oman" = "#a3a3a3",
  "Kuwait" = "#00B1E6"
)

# ==============================================================================
# UI - With Landing Page
# ==============================================================================

ui <- fluidPage(
  
  # Custom CSS for landing page and dashboard
 tags$head(
    tags$style(HTML("
      /* ===== LANDING PAGE STYLES ===== */
      
      /* Hide body scrollbar when on landing page */
      body {
        margin: 0;
        padding: 0;
      }
      
      .landing-container {
        min-height: 100vh;
        background: #f8f9fa;
      }
      
      /* Hero Section with rotating backgrounds */
      .landing-hero {
        height: 75vh;
        min-height: 500px;
        position: relative;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        color: white;
        text-align: center;
        padding: 40px 20px;
        overflow: hidden;
        /* FALLBACK background if images don't load */
        background: linear-gradient(135deg, #003366 0%, #005082 50%, #003366 100%);
      }

      /* Background image layers */
      .hero-bg {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background-size: cover;
        background-position: center;
        transition: opacity 1.5s ease-in-out;
        opacity: 0;
        z-index: 0;
      }

      .hero-bg.active {
        opacity: 1;
      }

      /* Dark overlay for text readability */
      .hero-overlay {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: linear-gradient(135deg, rgba(0,51,102,0.85) 0%, rgba(0,80,130,0.80) 50%, rgba(0,51,102,0.85) 100%);
        z-index: 1;
      }

      /* Content sits above background */
      .landing-hero > *:not(.hero-bg):not(.hero-overlay) {
        position: relative;
        z-index: 2;
      }

      /* Background images - using local files from www/ folder */
      .hero-bg-1 { background-image: url('images/1.jpg'); }
      .hero-bg-2 { background-image: url('images/2.jpg'); }
      .hero-bg-3 { background-image: url('images/3.jpg'); }
      .hero-bg-4 { background-image: url('images/4.jpg'); }

      .landing-logo {
        height: 90px;
        margin-bottom: 25px;
      }
      
      .landing-title {
        font-size: 2.8rem;
        font-weight: 700;
        margin-bottom: 10px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
        letter-spacing: 1px;
      }
      
      .landing-subtitle {
        font-size: 1.3rem;
        font-weight: 300;
        margin-bottom: 30px;
        opacity: 0.9;
      }
      
      /* Quote Carousel */
      .quote-carousel {
        max-width: 850px;
        padding: 35px 50px;
        font-size: 1.35rem;
        font-style: italic;
        line-height: 1.7;
        min-height: 100px;
        background: rgba(255,255,255,0.08);
        border-radius: 15px;
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255,255,255,0.1);
        margin: 20px 0;
      }
      
      .carousel-dots {
        display: flex;
        gap: 12px;
        margin-top: 25px;
        justify-content: center;
      }
      
      .carousel-dot {
        width: 12px;
        height: 12px;
        border-radius: 50%;
        background: rgba(255,255,255,0.4);
        cursor: pointer;
        transition: all 0.3s ease;
        border: none;
        padding: 0;
      }
      
      .carousel-dot:hover {
        background: rgba(255,255,255,0.7);
        transform: scale(1.2);
      }
      
      .carousel-dot.active {
        background: white;
        transform: scale(1.2);
      }
      
      /* Enter Button */
      .enter-btn {
        margin-top: 35px;
        padding: 18px 60px;
        font-size: 1.2rem;
        font-weight: 600;
        background: linear-gradient(135deg, #008035 0%, #006028 100%);
        border: none;
        color: white;
        border-radius: 50px;
        cursor: pointer;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(0,128,53,0.4);
        text-transform: uppercase;
        letter-spacing: 2px;
      }
      
      .enter-btn:hover {
        background: linear-gradient(135deg, #009940 0%, #007030 100%);
        transform: translateY(-3px);
        box-shadow: 0 6px 25px rgba(0,128,53,0.5);
      }
      
      .enter-btn:active {
        transform: translateY(-1px);
      }
      
      /* About Section */
      .landing-about {
        padding: 70px 10%;
        background: white;
        text-align: center;
      }
      
      .landing-about h2 {
        color: #003366;
        font-size: 2.2rem;
        margin-bottom: 25px;
        font-weight: 600;
      }
      
      .landing-about-text {
        max-width: 950px;
        margin: 0 auto;
        font-size: 1.15rem;
        line-height: 1.9;
        color: #444;
      }
      
      /* Stats Row */
      .stats-row {
        display: flex;
        justify-content: center;
        gap: 35px;
        margin-top: 50px;
        flex-wrap: wrap;
        padding: 0 20px;
      }
      
      .stat-box {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
        padding: 30px 45px;
        border-radius: 15px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        transition: all 0.3s ease;
        border: 1px solid #e9ecef;
        min-width: 140px;
      }
      
      .stat-box:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 30px rgba(0,0,0,0.12);
      }
      
      .stat-box .number {
        font-size: 2.8rem;
        font-weight: 700;
        color: #003366;
        line-height: 1;
        margin-bottom: 8px;
      }
      
      .stat-box .label {
        color: #666;
        font-size: 0.95rem;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      /* Dimensions Preview */
      .dimensions-section {
        padding: 60px 10%;
        background: #f8f9fa;
      }
      
      .dimensions-section h3 {
        text-align: center;
        color: #003366;
        font-size: 1.8rem;
        margin-bottom: 40px;
        font-weight: 600;
      }
      
      .dimension-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
        gap: 25px;
        max-width: 1200px;
        margin: 0 auto;
      }
      
      .dimension-card {
        background: white;
        padding: 25px;
        border-radius: 12px;
        box-shadow: 0 2px 15px rgba(0,0,0,0.06);
        display: flex;
        align-items: center;
        gap: 18px;
        transition: all 0.3s ease;
        border-left: 4px solid #003366;
      }
      
      .dimension-card:hover {
        transform: translateX(5px);
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
      }
      
      .dimension-card i {
        font-size: 2rem;
        color: #003366;
        width: 50px;
        text-align: center;
      }
      
      .dimension-card .dim-text {
        flex: 1;
      }
      
      .dimension-card .dim-title {
        font-weight: 600;
        color: #003366;
        font-size: 1.1rem;
        margin-bottom: 5px;
      }
      
      .dimension-card .dim-desc {
        font-size: 0.9rem;
        color: #666;
        line-height: 1.5;
      }
      
      /* Footer */
      .landing-footer {
        padding: 30px;
        background: #003366;
        color: rgba(255,255,255,0.8);
        text-align: center;
        font-size: 0.95rem;
      }
      
      .landing-footer a {
        color: white;
        text-decoration: none;
      }
      
      /* Back to Welcome link in dashboard */
      .back-to-welcome {
        position: fixed;
        bottom: 20px;
        right: 20px;
        background: #003366;
        color: white;
        padding: 10px 20px;
        border-radius: 25px;
        font-size: 0.9rem;
        cursor: pointer;
        z-index: 9999;
        box-shadow: 0 2px 10px rgba(0,0,0,0.2);
        transition: all 0.3s ease;
        border: none;
      }
      
      .back-to-welcome:hover {
        background: #004488;
        transform: translateY(-2px);
      }
      
      /* ===== DASHBOARD STYLES ===== */
      .content-wrapper { background-color: #f4f6f9; }
      .box { box-shadow: 0 1px 3px rgba(0,0,0,0.12); }
      .small-box { box-shadow: 0 2px 4px rgba(0,0,0,0.15); }
      .info-box { box-shadow: 0 1px 3px rgba(0,0,0,0.12); }
      
      /* Responsive adjustments */
      @media (max-width: 768px) {
        .landing-title { font-size: 2rem; }
        .landing-subtitle { font-size: 1.1rem; }
        .quote-carousel { 
          font-size: 1.1rem; 
          padding: 25px 30px;
          margin: 15px;
        }
        .stats-row { gap: 20px; }
        .stat-box { padding: 20px 30px; }
        .stat-box .number { font-size: 2.2rem; }
        .enter-btn { padding: 15px 40px; }
      }
    "))
  ),
  
  # Conditional display: Landing Page OR Dashboard
  uiOutput("main_ui"),
  
  # JavaScript for carousel rotation
 tags$script(HTML("
  var quotes = [
    'Economic integration is a strategic priority for the GCC. All member states have a role in building a unified, diversified regional economy.',
    'The GCC common market represents one of the most ambitious regional integration projects, supporting Vision 2030 transformation goals across member states.',
    'Measuring integration helps identify progress and opportunities for deeper economic cooperation, guiding evidence-based policymaking.',
    'From trade and investment to labor mobility and infrastructure connectivity, the Index captures the full picture of regional integration.'
  ];
  
  var currentQuote = 0;
  var autoRotate;
  
  function showQuote(index) {
    currentQuote = index;
    
    // Update quote text with fade
    var display = document.getElementById('quote-display');
    if (display) {
      display.style.opacity = 0;
      setTimeout(function() {
        display.innerText = quotes[index];
        display.style.opacity = 1;
      }, 300);
    }
    
    // Update background images
    var bgs = document.querySelectorAll('.hero-bg');
    bgs.forEach(function(bg, i) {
      if (i === index) {
        bg.classList.add('active');
      } else {
        bg.classList.remove('active');
      }
    });
    
    // Update dots
    var dots = document.querySelectorAll('.carousel-dot');
    dots.forEach(function(dot, i) {
      if (i === index) {
        dot.classList.add('active');
      } else {
        dot.classList.remove('active');
      }
    });
  }
  
  function startCarousel() {
    if (autoRotate) clearInterval(autoRotate);
    autoRotate = setInterval(function() {
      currentQuote = (currentQuote + 1) % quotes.length;
      showQuote(currentQuote);
    }, 7000);
  }
  
  $(document).on('shiny:value', function(event) {
    if (event.name === 'main_ui') {
      setTimeout(function() {
        var display = document.getElementById('quote-display');
        if (display) {
          display.style.transition = 'opacity 0.3s ease';
          startCarousel();
        }
      }, 100);
    }
  });
"))
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # Reactive value to track if user has entered the dashboard
  entered_dashboard <- reactiveVal(FALSE)
  
  # Observer for Enter Dashboard button
  observeEvent(input$enter_dashboard, {
    entered_dashboard(TRUE)
  })
  
  # Observer for Back to Welcome button
  observeEvent(input$back_to_welcome, {
    entered_dashboard(FALSE)
  })
  
  # Main UI renderer - shows either landing page or dashboard
  output$main_ui <- renderUI({
    if (!entered_dashboard()) {
      # ===== LANDING PAGE =====
      div(class = "landing-container",
          
          # Hero Section
          div(class = "landing-hero",
              
              # ===== BACKGROUNDS MUST BE FIRST =====
              div(class = "hero-bg hero-bg-1 active"),
              div(class = "hero-bg hero-bg-2"),
              div(class = "hero-bg hero-bg-3"),
              div(class = "hero-bg hero-bg-4"),
              
              # Dark overlay (ALSO REQUIRED)
              div(class = "hero-overlay"),
              # ======================================
              
              # GCC-Stat Logo (placeholder - replace with actual logo path)
              img(src = "images/GCC-MAIN-01-WHITE.png", class = "landing-logo"),
          
              
              # Title
              h1(class = "landing-title", "GCC Economic Integration Index"),
              
              # Subtitle
              p(class = "landing-subtitle", "Measuring Regional Integration Across the Gulf Cooperation Council"),
              
              # Rotating Quote Carousel
              div(class = "quote-carousel", id = "quote-display",
                  "Economic integration is a strategic priority for the GCC. All member states have a role in building a unified, diversified regional economy."
              ),
              
              # Carousel navigation dots
              div(class = "carousel-dots",
                  tags$button(class = "carousel-dot active", onclick = "showQuote(0)"),
                  tags$button(class = "carousel-dot", onclick = "showQuote(1)"),
                  tags$button(class = "carousel-dot", onclick = "showQuote(2)"),
                  tags$button(class = "carousel-dot", onclick = "showQuote(3)")
              ),
              
              # Enter Dashboard button
              actionButton("enter_dashboard", "Enter Dashboard", 
                           class = "enter-btn",
                           icon = icon("arrow-right"))
          ),
          
          # About Section
          div(class = "landing-about",
              h2("About the GCC Economic Integration Index"),
              p(class = "landing-about-text",
                "As the GCC region continues to pursue deeper economic, social, and institutional integration,
       the need for robust measurement tools becomes increasingly essential. Inspired by global models
       such as the Asia-Pacific Regional Cooperation and Integration Index (ARCII) and the Global
       Integration Index (GII), the GCC Regional Cooperation and Integration Index (GCC-RCII) offers
       a comprehensive, multidimensional framework to assess the progress of regional integration across
       GCC economies."
              ),
              p(class = "landing-about-text",
                "The GCC-RCII tracks integration across key areas including trade, finance, value-chain linkages,
       infrastructure connectivity, movement of people, institutional alignment, digital integration,
       and environmental cooperation. It provides policymakers, researchers, and practitioners with
       evidence-based insights to identify strengths, monitor gaps, and guide regional policy coordination."
              ),
              
              # Key statistics
              div(class = "stats-row",
                  div(class = "stat-box",
                      div(class = "number", "6"),
                      div(class = "label", "Member States")
                  ),
                  div(class = "stat-box",
                      div(class = "number", "6"),
                      div(class = "label", "Dimensions")
                  ),
                  div(class = "stat-box",
                      div(class = "number", "40+"),
                      div(class = "label", "Indicators")
                  ),
                  div(class = "stat-box",
                      div(class = "number", "2015-2023"),
                      div(class = "label", "Time Series")
                  )
              )
          ),
          
          # Dimensions Preview Section
          div(class = "dimensions-section",
              h3("Six Integration Dimensions"),
              div(class = "dimension-grid",
                  div(class = "dimension-card",
                      icon("exchange-alt"),
                      div(class = "dim-text",
                          div(class = "dim-title", "Trade Integration"),
                          div(class = "dim-desc", "Intra-GCC merchandise and services trade intensity")
                      )
                  ),
                  div(class = "dimension-card",
                      icon("university"),
                      div(class = "dim-text",
                          div(class = "dim-title", "Financial Integration"),
                          div(class = "dim-desc", "Cross-border financial flows and monetary convergence")
                      )
                  ),
                  div(class = "dimension-card",
                      icon("users"),
                      div(class = "dim-text",
                          div(class = "dim-title", "Labor Mobility"),
                          div(class = "dim-desc", "Movement of workers, tourists, and students")
                      )
                  ),
                  div(class = "dimension-card",
                      icon("road"),
                      div(class = "dim-text",
                          div(class = "dim-title", "Infrastructure Connectivity"),
                          div(class = "dim-desc", "Transport, energy, and digital infrastructure")
                      )
                  ),
                  div(class = "dimension-card",
                      icon("leaf"),
                      div(class = "dim-text",
                          div(class = "dim-title", "Sustainability"),
                          div(class = "dim-desc", "Economic diversification and green transition")
                      )
                  ),
                  div(class = "dimension-card",
                      icon("chart-line"),
                      div(class = "dim-text",
                          div(class = "dim-title", "Economic Convergence"),
                          div(class = "dim-desc", "Macroeconomic alignment across member states")
                      )
                  )
              )
          ),
          
          # Footer
          div(class = "landing-footer",
              p(
                "© 2025 GCC Statistical Center (GCC-Stat) | ",
                tags$a(href = "https://gccstat.org", target = "_blank", "www.gccstat.org")
              )
          )
      )
      
    } else {
      # ===== DASHBOARD =====
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
              ),
              
              # Metadata & Analysis Tab
              tabItem(
                tabName = "metadata",
                
                # Overview Section
                fluidRow(
                  box(
                    width = 12,
                    title = "About the GCC Economic Integration Index",
                    status = "primary",
                    solidHeader = TRUE,
                    
                    h4("Purpose"),
                    p("The GCC Economic Integration Index (EII) is a composite indicator designed to measure 
                      the depth and progress of economic integration among the six Gulf Cooperation Council 
                      member states: Bahrain, Kuwait, Oman, Qatar, Saudi Arabia, and the United Arab Emirates."),
                    
                    p("The index supports Vision 2030 transformation goals by providing an authoritative, 
                      data-driven assessment of regional integration across multiple economic dimensions. 
                      It enables policymakers to identify strengths, gaps, and opportunities for deeper 
                      cooperation within the GCC common market framework."),
                    
                    h4("Scope"),
                    p("The index covers the period 2015-2023 and aggregates performance across six dimensions, 
                      each capturing a distinct aspect of economic integration. Country-level scores are 
                      aggregated to a GCC-wide index using GDP-weighted averaging to reflect the relative 
                      economic significance of each member state."),
                    
                    h4("Scoring"),
                    p("All dimension and overall scores are presented on a 0-100 scale. The methodology varies by indicator type:"),
                    tags$ul(
                      tags$li(strong("Min-max normalization:"), " Most indicators are scaled using min-max normalization, 
                              where the best performer scores 100 and the lowest scores 0."),
                      tags$li(strong("Direct measures:"), " Some indicators (e.g., trade intensity, FDI shares) are 
                              direct percentage measures normalized to the 0-100 scale."),
                      tags$li(strong("Convergence measures:"), " Indicators measuring regional convergence use the 
                              coefficient of variation (CV) across countries. These produce the ", em("same score for all countries"), 
                              " in a given year, reflecting GCC-wide convergence rather than individual country performance. 
                              The score is calculated as: 100 minus the CV (capped at 0-100).")
                    ),
                    p("Integration levels are classified as:"),
                    tags$ul(
                      tags$li(strong("Good (60-100):"), " Strong integration with well-functioning mechanisms"),
                      tags$li(strong("Moderate (40-59):"), " Partial integration with room for improvement"),
                      tags$li(strong("Weak (0-39):"), " Limited integration requiring policy attention")
                    )
                  )
                ),
                
                # Dimension Weights
                fluidRow(
                  box(
                    width = 12,
                    title = "Dimension Weights",
                    status = "info",
                    solidHeader = TRUE,
                    
                    p("The overall index combines six dimensions with the following weights:"),
                    tableOutput("dimension_weights_table")
                  )
                ),
                
                # Trade Integration Indicators
                fluidRow(
                  box(
                    width = 12,
                    title = "Dimension 1: Trade Integration (20%)",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    p("Measures the intensity and composition of intra-GCC merchandise and services trade, 
                      with emphasis on non-oil diversification and value chain participation."),
                    tableOutput("trade_indicators_table")
                  )
                ),
                
                # Financial Integration Indicators
                fluidRow(
                  box(
                    width = 12,
                    title = "Dimension 2: Financial & Monetary Integration (20%)",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    p("Assesses convergence in monetary conditions, banking sector integration, 
                      and cross-border financial flows within the GCC."),
                    tableOutput("financial_indicators_table")
                  )
                ),
                
                # Labor Integration Indicators
                fluidRow(
                  box(
                    width = 12,
                    title = "Dimension 3: Labor & Human Capital (20%)",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    p("Captures the mobility of labor and people across GCC borders, including 
                      worker movements, tourism, and student exchanges."),
                    tableOutput("labor_indicators_table")
                  )
                ),
                
                # Infrastructure Indicators
                fluidRow(
                  box(
                    width = 12,
                    title = "Dimension 4: Infrastructure Connectivity (20%)",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    p("Tracks physical and digital connectivity infrastructure that enables 
                      cross-border economic activity, including transport, energy, and telecommunications."),
                    tableOutput("infrastructure_indicators_table")
                  )
                ),
                
                # Sustainability Indicators
                fluidRow(
                  box(
                    width = 12,
                    title = "Dimension 5: Sustainability & Diversification (10%)",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    p("Measures progress toward economic diversification away from hydrocarbon 
                      dependence, aligned with national Vision 2030 strategies."),
                    tableOutput("sustainability_indicators_table")
                  )
                ),
                
                # Convergence Indicators
                fluidRow(
                  box(
                    width = 12,
                    title = "Dimension 6: Macroeconomic Convergence (10%)",
                    status = "success",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    p("Assesses the degree to which GCC economies are converging in key macroeconomic 
                      variables, a prerequisite for deeper monetary integration."),
                    tableOutput("convergence_indicators_table")
                  )
                ),
                
                # Data Sources
                fluidRow(
                  box(
                    width = 12,
                    title = "Data Sources",
                    status = "warning",
                    solidHeader = TRUE,
                    
                    tags$ul(
                      tags$li(strong("Trade data:"), " UN Comtrade (merchandise), GCC-Stat estimates (services)"),
                      tags$li(strong("National accounts:"), " GCC-Stat national accounts database"),
                      tags$li(strong("Financial data:"), " GCC-Stat monetary database"),
                      tags$li(strong("Labor & mobility:"), " GCC-Stat labor statistics,  tourism and common market databases"),
                      tags$li(strong("Infrastructure:"), " GCC-Stat common market, energy and tourism databases"),
                      tags$li(strong("Price data:"), " GCC-Stat national CPI database, ICP (International Comparison Program) ")
                    ),
                    
                    p(em("Note: Some indicators use proxy measures or estimates where direct data is unavailable. 
                         Methodology documentation is available upon request."))
                  )
                )
              ),
              
              # GCC Overall Tab
              tabItem(
                tabName = "gcc_overall",
                fluidRow(
                  box(
                    width = 12,
                    title = "GCC Aggregate Integration Performance",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("gcc_overall_gauge", height = "300px")
                  )
                ),
                fluidRow(
                  box(
                    width = 6, 
                    title = "Current Dimension Scores",
                    status = "info",
                    solidHeader = TRUE,
                    plotlyOutput("gcc_dimension_bars", height = "400px")
                  ),
                  box(
                    width = 6, 
                    title = "Dimension Score Cards",
                    status = "info",
                    solidHeader = TRUE,
                    uiOutput("dimension_boxes")
                  )
                ),
                fluidRow(
                  box(
                    width = 12,
                    title = "Country Ranking",
                    status = "success",
                    solidHeader = TRUE,
                    plotlyOutput("country_ranking", height = "350px")
                  )
                )
              ),
              
              # GCC Timeseries Tab
              tabItem(
                tabName = "gcc_timeseries",
                fluidRow(
                  box(
                    width = 12,
                    title = "GCC Overall Index Trend",
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("gcc_overall_trend", height = "350px")
                  )
                ),
                fluidRow(
                  box(
                    width = 12,
                    title = "GCC Dimension Trends",
                    status = "info",
                    solidHeader = TRUE,
                    plotlyOutput("gcc_dimension_trends", height = "400px")
                  )
                ),
                fluidRow(
                  box(
                    width = 12,
                    title = "All Countries: Overall Index Trend",
                    status = "success",
                    solidHeader = TRUE,
                    plotlyOutput("all_countries_trend", height = "400px")
                  )
                )
              ),
              
              # Country Profiles Tab
              tabItem(
                tabName = "country_profiles",
                fluidRow(
                  box(
                    width = 4,
                    title = "Select Country",
                    status = "primary",
                    solidHeader = TRUE,
                    selectInput("selected_country", "Country:",
                                choices = countries,
                                selected = countries[1])
                  ),
                  valueBoxOutput("country_overall", width = 4),
                  valueBoxOutput("country_rank", width = 4)
                ),
                fluidRow(
                  box(
                    width = 6,
                    title = "Dimension Profile",
                    status = "info",
                    solidHeader = TRUE,
                    plotlyOutput("country_radar", height = "400px")
                  ),
                  box(
                    width = 6,
                    title = "Dimension Scores",
                    status = "info",
                    solidHeader = TRUE,
                    plotlyOutput("country_dimension_bars", height = "400px")
                  )
                ),
                fluidRow(
                  box(
                    width = 12,
                    title = "Country Trend Over Time",
                    status = "success",
                    solidHeader = TRUE,
                    plotlyOutput("country_trend", height = "350px")
                  )
                ),
                fluidRow(
                  box(
                    width = 12,
                    title = "Country vs GCC Average",
                    status = "warning",
                    solidHeader = TRUE,
                    plotlyOutput("country_vs_gcc", height = "350px")
                  )
                )
              ),
              
              # Country Heatmap Tab
              tabItem(
                tabName = "country_heatmap",
                fluidRow(
                  box(
                    width = 4,
                    title = "Select Year",
                    status = "primary",
                    solidHeader = TRUE,
                    selectInput("heatmap_year", "Year:",
                                choices = sort(unique(dimension_scores$year), decreasing = TRUE),
                                selected = max(dimension_scores$year))
                  )
                ),
                fluidRow(
                  box(
                    width = 12,
                    title = "Country-Dimension Heatmap",
                    status = "info",
                    solidHeader = TRUE,
                    plotlyOutput("country_heatmap", height = "500px")
                  )
                ),
                fluidRow(
                  box(
                    width = 12,
                    title = "Integration Levels by Country",
                    status = "success",
                    solidHeader = TRUE,
                    plotlyOutput("integration_levels", height = "350px")
                  )
                )
              ),
              
              # GCC Analytics Tab
              tabItem(
                tabName = "gcc_analytics",
                fluidRow(
                  box(
                    width = 12,
                    title = "Analysis Period",
                    status = "primary",
                    solidHeader = TRUE,
                    sliderInput("analytics_year_range", "Year Range:",
                                min = min(dimension_scores$year),
                                max = max(dimension_scores$year),
                                value = c(min(dimension_scores$year), max(dimension_scores$year)),
                                step = 1,
                                sep = "")
                  )
                ),
                fluidRow(
                  box(
                    width = 6,
                    title = "Year-over-Year Changes by Dimension",
                    status = "info",
                    solidHeader = TRUE,
                    plotlyOutput("yoy_dimension_changes", height = "400px")
                  ),
                  box(
                    width = 6,
                    title = "Contribution to GCC Index Change",
                    status = "info",
                    solidHeader = TRUE,
                    plotlyOutput("contribution_analysis", height = "400px")
                  )
                ),
                fluidRow(
                  box(
                    width = 6,
                    title = "Annual Change Trends",
                    status = "success",
                    solidHeader = TRUE,
                    plotlyOutput("annual_change_trends", height = "350px")
                  ),
                  box(
                    width = 6,
                    title = "Dimension Change Heatmap",
                    status = "success",
                    solidHeader = TRUE,
                    plotlyOutput("change_heatmap", height = "350px")
                  )
                )
              ),
              
              # Data Explorer Tab
              tabItem(
                tabName = "data_explorer",
                fluidRow(
                  box(
                    width = 12,
                    title = "Data Explorer",
                    status = "primary",
                    solidHeader = TRUE,
                    selectInput("data_table_select", "Select Dataset:",
                                choices = c("Country Dimension Scores" = "dimension",
                                            "GCC Aggregate" = "gcc",
                                            "Year-over-Year Changes" = "yoy")),
                    DTOutput("data_table")
                  )
                )
              )
            )
          )
        ),
        
        # Back to Welcome button (floating)
        actionButton("back_to_welcome", 
                     label = tagList(icon("home"), " Welcome"),
                     class = "back-to-welcome")
      )
    }
  })
  
  # ===========================================================================
  # DASHBOARD SERVER OUTPUTS
  # ===========================================================================
  
  # Overview Tab Outputs
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
  
  # Metadata Tab Tables
  output$dimension_weights_table <- renderTable({
    data.frame(
      Dimension = c("Trade Integration", "Financial & Monetary", "Labor & Human Capital",
                    "Infrastructure Connectivity", "Sustainability & Diversification", 
                    "Macroeconomic Convergence", "TOTAL"),
      Weight = c("20%", "20%", "20%", "20%", "10%", "10%", "100%"),
      `Key Focus` = c("Intra-GCC trade intensity, non-oil exports",
                      "Interest rate convergence, cross-border finance",
                      "Worker mobility, tourism, education exchange",
                      "Transport, energy, digital connectivity",
                      "Economic diversification, renewables",
                      "Macroeconomic alignment across member states",
                      "Composite integration measure")
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  output$trade_indicators_table <- renderTable({
    data.frame(
      Indicator = c("Intra-GCC Trade Intensity", 
                    "Non-Oil Trade Share",
                    "Trade Complementarity Index",
                    "Services Trade Intensity"),
      Description = c("Intra-GCC exports as % of total exports",
                      "Non-hydrocarbon exports as share of intra-GCC trade",
                      "Alignment of export/import structures across countries",
                      "Cross-border services trade relative to GDP"),
      Weight = c("30%", "25%", "25%", "20%")
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  output$financial_indicators_table <- renderTable({
    data.frame(
      Indicator = c("Interest Rate Convergence", 
                    "Inflation Convergence",
                    "Intra-GCC FDI Share",
                    "Banking Sector Integration"),
      Description = c("Coefficient of variation in policy/lending rates",
                      "Similarity in inflation rates across countries",
                      "Share of FDI originating from other GCC states",
                      "Cross-border banking assets and liabilities"),
      Weight = c("30%", "25%", "25%", "20%")
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  output$labor_indicators_table <- renderTable({
    data.frame(
      Indicator = c("GCC National Worker Mobility", 
                    "Intra-GCC Tourism Share",
                    "Student Exchange Intensity",
                    "Professional Qualification Recognition"),
      Description = c("GCC nationals employed in other member states",
                      "Inbound tourists from other GCC states as share of total",
                      "Students studying in other GCC countries",
                      "Mutual recognition of professional credentials"),
      Weight = c("35%", "30%", "20%", "15%")
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  output$infrastructure_indicators_table <- renderTable({
    data.frame(
      Indicator = c("Transport Connectivity Index", 
                    "Energy Grid Integration",
                    "Digital Infrastructure Score",
                    "Logistics Performance"),
      Description = c("Road/rail/air connections between member states",
                      "GCC Interconnection Grid participation and flows",
                      "Broadband, mobile coverage, and e-government readiness",
                      "Cross-border logistics efficiency"),
      Weight = c("30%", "25%", "25%", "20%")
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  output$sustainability_indicators_table <- renderTable({
    data.frame(
      Indicator = c("Economic Diversification Index", 
                    "Renewable Energy Share",
                    "Green Finance Development"),
      Description = c("Non-oil GDP share and sectoral diversification",
                      "Renewable energy as % of total energy production",
                      "Green bonds, sustainable finance instruments"),
      Weight = c("45%", "35%", "20%")
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  output$convergence_indicators_table <- renderTable({
    data.frame(
      Indicator = c("GDP Growth Convergence", 
                    "Inflation Rate Convergence",
                    "Fiscal Balance Convergence",
                    "GDP per Capita Convergence"),
      Description = c("Similarity in real GDP growth rates",
                      "Alignment of consumer price inflation",
                      "Convergence in government budget balances (% GDP)",
                      "Reduction in income disparities across member states"),
      Weight = c("25%", "25%", "25%", "25%")
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  # GCC Overall Tab Outputs
  output$gcc_overall_gauge <- renderPlotly({
    latest <- gcc_ts %>% filter(year == max(year))
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number+delta",
      value = latest$overall,
      delta = list(reference = gcc_ts %>% filter(year == max(year) - 1) %>% pull(overall)),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = "#003366"),
        steps = list(
          list(range = c(0, 40), color = "#ffcccc"),
          list(range = c(40, 60), color = "#ffffcc"),
          list(range = c(60, 100), color = "#ccffcc")
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 60
        )
      ),
      title = list(text = paste("GCC Integration Index", max(gcc_ts$year)))
    ) %>%
      layout(margin = list(t = 50, b = 20))
  })
  
  output$gcc_dimension_bars <- renderPlotly({
    latest <- gcc_ts %>% filter(year == max(year))
    
    dim_data <- data.frame(
      dimension = factor(dimension_labels, levels = dimension_labels),
      score = c(latest$trade_score, latest$financial_score, latest$labor_score,
                latest$infrastructure_score, latest$sustainability_score, latest$convergence_score)
    )
    
    plot_ly(dim_data, x = ~dimension, y = ~score, type = 'bar',
            marker = list(color = '#003366')) %>%
      layout(
        xaxis = list(title = "", categoryorder = "array", categoryarray = dimension_labels),
        yaxis = list(title = "Score", range = c(0, 100)),
        margin = list(b = 100)
      )
  })
  
  output$dimension_boxes <- renderUI({
    latest <- gcc_ts %>% filter(year == max(year))
    
    scores <- c(latest$trade_score, latest$financial_score, latest$labor_score,
                latest$infrastructure_score, latest$sustainability_score, latest$convergence_score)
    
    icons <- c("exchange-alt", "university", "users", "road", "leaf", "chart-line")
    colors <- c("blue", "green", "orange", "purple", "olive", "maroon")
    
    tagList(
      fluidRow(
        lapply(1:3, function(i) {
          column(4,
                 div(class = "info-box",
                     span(class = paste("info-box-icon bg", colors[i], sep = "-"), icon(icons[i])),
                     div(class = "info-box-content",
                         span(class = "info-box-text", dimension_labels[i]),
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
                     span(class = paste("info-box-icon bg", colors[i], sep = "-"), icon(icons[i])),
                     div(class = "info-box-content",
                         span(class = "info-box-text", dimension_labels[i]),
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
    
    plot_ly(ranking, x = ~reorder(country, overall_index), y = ~overall_index,
            type = 'bar',
            marker = list(color = ~country_colors[country])) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Overall Index", range = c(0, 100)),
        title = paste("Country Rankings -", latest_year)
      )
  })
  
  # GCC Timeseries Tab Outputs
  output$gcc_overall_trend <- renderPlotly({
    plot_ly(gcc_ts, x = ~year, y = ~overall, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#003366', width = 3),
            marker = list(size = 10)) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Overall Index", range = c(0, 100)),
        hovermode = 'x unified'
      )
  })
  
  output$gcc_dimension_trends <- renderPlotly({
    dim_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628")
    
    p <- plot_ly(gcc_ts, x = ~year)
    
    for (i in 1:length(dimension_cols)) {
      p <- p %>% add_trace(y = as.formula(paste0("~", dimension_cols[i])),
                           name = dimension_labels[i],
                           type = 'scatter', mode = 'lines+markers',
                           line = list(color = dim_colors[i], width = 2),
                           marker = list(size = 8))
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
      p <- p %>% add_trace(data = ctry_data, x = ~year, y = ~overall_index,
                           name = ctry, type = 'scatter', mode = 'lines+markers',
                           line = list(color = country_colors[ctry], width = 2),
                           marker = list(size = 8))
    }
    
    p %>% layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = "Overall Index", range = c(0, 100)),
      hovermode = 'x unified',
      legend = list(orientation = 'h', y = -0.15)
    )
  })
  
  # Country Profiles Tab Outputs
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
    
    scores <- c(latest$trade_score, latest$financial_score, latest$labor_score,
                latest$infrastructure_score, latest$sustainability_score, latest$convergence_score,
                latest$trade_score)  # Close the radar
    
    labels <- c(dimension_labels, dimension_labels[1])
    
    plot_ly(
      type = 'scatterpolar',
      r = scores,
      theta = labels,
      fill = 'toself',
      fillcolor = 'rgba(0, 51, 102, 0.3)',
      line = list(color = '#003366')
    ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 100))
        ),
        showlegend = FALSE
      )
  })
  
  output$country_dimension_bars <- renderPlotly({
    latest <- dimension_scores %>%
      filter(country == input$selected_country, year == max(year))
    
    dim_data <- data.frame(
      dimension = factor(dimension_labels, levels = dimension_labels),
      score = c(latest$trade_score, latest$financial_score, latest$labor_score,
                latest$infrastructure_score, latest$sustainability_score, latest$convergence_score)
    )
    
    plot_ly(dim_data, x = ~dimension, y = ~score, type = 'bar',
            marker = list(color = country_colors[input$selected_country])) %>%
      layout(
        xaxis = list(title = "", categoryorder = "array", categoryarray = dimension_labels),
        yaxis = list(title = "Score", range = c(0, 100)),
        margin = list(b = 100)
      )
  })
  
  output$country_trend <- renderPlotly({
    ctry_data <- dimension_scores %>% filter(country == input$selected_country)
    
    plot_ly(ctry_data, x = ~year, y = ~overall_index, type = 'scatter', mode = 'lines+markers',
            line = list(color = country_colors[input$selected_country], width = 3),
            marker = list(size = 10)) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Overall Index", range = c(0, 100)),
        hovermode = 'x unified'
      )
  })
  
  output$country_vs_gcc <- renderPlotly({
    ctry_data <- dimension_scores %>% filter(country == input$selected_country)
    
    plot_ly() %>%
      add_trace(data = ctry_data, x = ~year, y = ~overall_index,
                name = input$selected_country, type = 'scatter', mode = 'lines+markers',
                line = list(color = country_colors[input$selected_country], width = 3),
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
  
  # Country Heatmap Tab Outputs
  output$country_heatmap <- renderPlotly({
    heatmap_data <- dimension_scores %>%
      filter(year == input$heatmap_year) %>%
      select(country, all_of(dimension_cols))
    
    # Rename columns for display
    colnames(heatmap_data) <- c("country", dimension_labels)
    
    # Convert to matrix
    heatmap_matrix <- heatmap_data %>%
      column_to_rownames("country") %>%
      as.matrix()
    
    plot_ly(
      x = colnames(heatmap_matrix),
      y = rownames(heatmap_matrix),
      z = heatmap_matrix,
      type = "heatmap",
      colorscale = list(c(0, '#fee0d2'), c(0.5, '#fcbba1'), c(1, '#006d2c')),
      text = round(heatmap_matrix, 1),
      hovertemplate = '%{y}<br>%{x}: %{z:.1f}<extra></extra>'
    ) %>%
      layout(
        xaxis = list(title = "Dimension"),
        yaxis = list(title = "Country"),
        margin = list(l = 100)
      )
  })
  
  output$integration_levels <- renderPlotly({
    level_data <- dimension_scores %>%
      filter(year == input$heatmap_year) %>%
      mutate(integration_level = factor(integration_level, levels = c("Weak", "Moderate", "Good")))
    
    level_colors <- c("Weak" = "#E41A1C", "Moderate" = "#FFFF33", "Good" = "#4DAF4A")
    
    plot_ly(level_data, x = ~country, y = ~overall_index, type = 'bar',
            marker = list(color = ~level_colors[integration_level])) %>%
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
  
  # GCC Analytics Tab Outputs
  output$yoy_dimension_changes <- renderPlotly({
    changes <- yoy_changes %>%
      filter(year >= input$analytics_year_range[1] & year <= input$analytics_year_range[2]) %>%
      select(country, trade_change, financial_change, labor_change,
             infrastructure_change, sustainability_change, convergence_change) %>%
      group_by(country) %>%
      summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(cols = -country,
                   names_to = "dimension",
                   values_to = "change")
    
    dim_change_to_label <- c(
      "trade_change" = "Trade",
      "financial_change" = "Financial",
      "labor_change" = "Labor",
      "infrastructure_change" = "Infrastructure",
      "sustainability_change" = "Sustainability",
      "convergence_change" = "Convergence"
    )
    
    changes$dimension_label <- dim_change_to_label[changes$dimension]
    changes$dimension_label <- factor(changes$dimension_label, levels = dimension_labels)
    
    plot_ly(changes, x = ~dimension_label, y = ~change, color = ~country,
            colors = country_colors,
            type = 'scatter', mode = 'markers',
            marker = list(size = 14)) %>%
      layout(
        xaxis = list(title = "Dimension", categoryorder = "array", categoryarray = dimension_labels),
        yaxis = list(title = "Year-over-Year Change"),
        hovermode = 'closest',
        legend = list(orientation = 'h', y = -0.2),
        shapes = list(
          list(type = "line", x0 = -0.5, x1 = 5.5, y0 = 0, y1 = 0,
               line = list(color = "black", dash = "dash"))
        )
      )
  })
  
  output$contribution_analysis <- renderPlotly({
    selected_year <- input$analytics_year_range[2]
    prev_year <- input$analytics_year_range[1]
    
    gdp_weights <- data.frame(
      country = c("Bahrain", "Kuwait", "Oman", "Qatar", "Saudi Arabia", "UAE"),
      weight = c(0.02, 0.07, 0.04, 0.10, 0.42, 0.35)
    )
    
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
      left_join(gdp_weights, by = "country")
    
    contribution_data <- data.frame(
      country = rep(changes$country, 6),
      dimension = rep(dimension_labels, each = nrow(changes)),
      contribution = c(
        (changes$trade_score_end - changes$trade_score_start) * changes$weight,
        (changes$financial_score_end - changes$financial_score_start) * changes$weight,
        (changes$labor_score_end - changes$labor_score_start) * changes$weight,
        (changes$infrastructure_score_end - changes$infrastructure_score_start) * changes$weight,
        (changes$sustainability_score_end - changes$sustainability_score_start) * changes$weight,
        (changes$convergence_score_end - changes$convergence_score_start) * changes$weight
      )
    )
    
    contribution_data$dimension <- factor(contribution_data$dimension, levels = dimension_labels)
    
    plot_ly(contribution_data, x = ~dimension, y = ~contribution, color = ~country,
            colors = country_colors,
            type = 'bar') %>%
      layout(
        xaxis = list(title = "Dimension", categoryorder = "array", categoryarray = dimension_labels),
        yaxis = list(title = "Contribution to GCC Index (weighted)"),
        barmode = 'stack',
        legend = list(orientation = 'h', y = -0.2)
      )
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
                name = 'Average Change',
                line = list(color = 'blue', width = 3),
                marker = list(size = 10)) %>%
      add_trace(x = ~year, y = ~max_change, type = 'scatter', mode = 'lines',
                name = 'Max Change',
                line = list(color = 'green', width = 2, dash = 'dash')) %>%
      add_trace(x = ~year, y = ~min_change, type = 'scatter', mode = 'lines',
                name = 'Min Change',
                line = list(color = 'red', width = 2, dash = 'dash')) %>%
      add_segments(x = min(all_changes$year), xend = max(all_changes$year),
                   y = 0, yend = 0,
                   line = list(color = 'black', dash = 'dot'),
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
    
    colnames(dim_changes) <- c("country", dimension_labels)
    
    change_matrix <- dim_changes %>%
      column_to_rownames("country") %>%
      as.matrix()
    
    plot_ly(
      x = colnames(change_matrix),
      y = rownames(change_matrix),
      z = change_matrix,
      type = "heatmap",
      colorscale = list(c(0, 'red'), c(0.5, 'white'), c(1, 'green')),
      zmid = 0,
      text = round(change_matrix, 1),
      hovertemplate = '%{y}<br>%{x}: %{z:.1f}<extra></extra>'
    ) %>%
      layout(
        title = "Average Dimension Change by Country",
        xaxis = list(title = "Dimension"),
        yaxis = list(title = "Country"),
        margin = list(l = 100)
      )
  })
  
  # Data Explorer Tab Outputs
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
      formatRound(columns = which(sapply(data_to_show, is.numeric)), digits = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
