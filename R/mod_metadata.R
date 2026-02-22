# ==============================================================================
# METADATA TABLES MODULE
# ==============================================================================
# Defines all metadata tables for the Metadata & Analysis tab.
# Updated to match the COINr pipeline (32 indicators, 6 dimensions).
# ==============================================================================

#' Get Dimension Weights Table Data
#'
#' @return Data frame with dimension weights
get_dimension_weights_table <- function() {
  data.frame(
    Dimension = c("Trade Integration", "Financial & Monetary",
                  "Labor & Human Capital", "Infrastructure Connectivity",
                  "Sustainability & Diversification",
                  "Macroeconomic Convergence", "TOTAL"),
    Weight = c("20%", "20%", "20%", "20%", "10%", "10%", "100%"),
    Indicators = c("6", "8", "5", "2", "4", "7", "32"),
    `Key Focus` = c(
      "Intra-GCC trade intensity, non-oil exports, value chain participation",
      "OCA criteria, banking integration, capital markets, fiscal position",
      "Worker mobility, tourism, student exchange, labor market indicators",
      "Aviation connectivity, energy infrastructure",
      "Economic diversification, non-oil GDP, manufacturing, revenue diversification",
      "Distance-based convergence across macroeconomic variables",
      "Composite integration measure"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Get Trade Indicators Table Data
#'
#' @return Data frame with trade indicators
get_trade_indicators_table <- function() {
  data.frame(
    Code = c("ind_51", "ind_52", "ind_55", "ind_56", "ind_63", "ind_64"),
    Indicator = c(
      "Intra-GCC Trade Intensity",
      "Services Trade Share",
      "Non-oil Trade Intensity",
      "Services as % of Total Trade",
      "Intermediate Goods Share",
      "Trade Diversification"
    ),
    Description = c(
      "Total intra-GCC trade (exports + imports) as percentage of GDP",
      "Intra-GCC services trade as percentage of GDP",
      "Intra-GCC non-oil merchandise trade as percentage of GDP",
      "Share of services in total intra-GCC trade",
      "Share of intermediate goods in intra-GCC trade (value chain indicator)",
      "Trade diversification index (100 minus HHI)"
    ),
    Direction = c("+", "+", "+", "+", "+", "+"),
    Normalization = c(
      "Winsorize + Min-max",
      "Winsorize + Min-max",
      "Winsorize + Min-max",
      "Min-max",
      "Min-max",
      "Min-max"
    ),
    stringsAsFactors = FALSE
  )
}

#' Get Financial Indicators Table Data
#'
#' @return Data frame with financial indicators
get_financial_indicators_table <- function() {
  data.frame(
    Code = c("ind_inflation", "ind_m2_growth", "ind_gdp_growth",
             "ind_31_fdi", "ind_39_banking", "ind_44_stock",
             "ind_bank_depth", "ind_fiscal_balance"),
    Indicator = c(
      "Annual Inflation Rate",
      "M2 Money Supply Growth",
      "Real GDP Growth Rate",
      "Intra-GCC FDI Share",
      "GCC Banking Penetration",
      "Stock Market Openness",
      "Banking Sector Depth",
      "Fiscal Balance Ratio"
    ),
    Description = c(
      "Annual CPI inflation rate (OCA criterion: lower divergence = better)",
      "Growth rate of M2 money supply (OCA criterion: monetary convergence)",
      "Real GDP growth rate (OCA criterion: business cycle synchronization)",
      "Foreign direct investment from other GCC states as share of GDP",
      "Number of GCC banks operating in other member states (% of total)",
      "Cross-listing of companies and market openness across GCC exchanges",
      "Banking sector assets relative to GDP",
      "Government fiscal balance as percentage of GDP"
    ),
    Direction = c("-", "-", "+", "+", "+", "+", "+", "+"),
    Normalization = c(
      "Z-score + Rescale",
      "Z-score + Rescale",
      "Z-score + Rescale",
      "Winsorize + Min-max",
      "Bounded min-max [50,100]",
      "Goalpost [0,100]",
      "Min-max",
      "Z-score + Rescale"
    ),
    stringsAsFactors = FALSE
  )
}

#' Get Labor Indicators Table Data
#'
#' @return Data frame with labor indicators
get_labor_indicators_table <- function() {
  data.frame(
    Code = c("ind_69_labor", "ind_71_student", "ind_72_tourism",
             "ind_lfpr", "ind_unemployment"),
    Indicator = c(
      "GCC National Worker Mobility",
      "Student Exchange Mobility",
      "Intra-GCC Tourism Intensity",
      "Labor Force Participation Rate",
      "Unemployment Rate"
    ),
    Description = c(
      "GCC nationals employed in other member states (per 1,000 population)",
      "GCC students enrolled in universities in other member states (per 1,000 population)",
      "Inbound tourists from other GCC states as share of total inbound tourism",
      "Total labor force participation rate (all nationalities)",
      "Citizen unemployment rate"
    ),
    Direction = c("+", "+", "+", "+", "-"),
    Normalization = c(
      "Winsorize + Min-max",
      "Winsorize + Min-max",
      "Min-max",
      "Min-max",
      "Winsorize + Min-max"
    ),
    stringsAsFactors = FALSE
  )
}

#' Get Infrastructure Indicators Table Data
#'
#' @return Data frame with infrastructure indicators
get_infrastructure_indicators_table <- function() {
  data.frame(
    Code = c("ind_3_aviation", "ind_elec_pc",
             "ind_1_railway", "ind_2_port", "ind_6_gccia", "ind_8_digital"),
    Indicator = c(
      "Aviation Connectivity",
      "Electricity Per Capita",
      "GCC Railway Connectivity",
      "Port Connectivity Index",
      "Power Grid Interconnection",
      "Digital Infrastructure Index"
    ),
    Description = c(
      "Intra-GCC air passenger arrivals (thousands)",
      "Per capita electricity production (kWh/person)",
      "GCC Railway network connectivity (placeholder - awaiting data)",
      "Port connectivity index (placeholder - awaiting data)",
      "GCCIA power grid interconnection capacity (placeholder - awaiting data)",
      "Digital infrastructure index (placeholder - awaiting data)"
    ),
    Direction = c("+", "+", "+", "+", "+", "+"),
    Status = c("Operational", "Operational",
               "Placeholder", "Placeholder", "Placeholder", "Placeholder"),
    stringsAsFactors = FALSE
  )
}

#' Get Sustainability Indicators Table Data
#'
#' @return Data frame with sustainability indicators
get_sustainability_indicators_table <- function() {
  data.frame(
    Code = c("ind_non_oil_share", "ind_oil_share",
             "ind_manufacturing_share", "ind_nonoil_rev_share"),
    Indicator = c(
      "Non-oil GDP Share",
      "Oil Dependency",
      "Manufacturing Share",
      "Non-oil Revenue Share"
    ),
    Description = c(
      "Direct measure of non-oil sector as percentage of total GDP",
      "Oil sector as percentage of total GDP (lower = more diversified)",
      "Manufacturing sector value added as percentage of GDP",
      "Non-oil government revenue as percentage of total revenue"
    ),
    Direction = c("+", "-", "+", "+"),
    Normalization = c("Min-max", "Min-max", "Min-max", "Min-max"),
    stringsAsFactors = FALSE
  )
}

#' Get Convergence Indicators Table Data
#'
#' @return Data frame with convergence indicators
get_convergence_indicators_table <- function() {
  data.frame(
    Code = c("ind_conv_income", "ind_conv_price", "ind_conv_non_oil",
             "ind_conv_manufacturing", "ind_conv_oil",
             "ind_conv_fiscal", "ind_conv_interest"),
    Indicator = c(
      "Real Income Convergence",
      "Price Level Convergence",
      "Non-oil GDP Convergence",
      "Manufacturing Convergence",
      "Oil Dependency Convergence",
      "Fiscal Balance Convergence",
      "Interest Rate Convergence"
    ),
    Description = c(
      "Convergence in PPP-adjusted GDP per capita across GCC states",
      "Convergence in price level indices (World = 100)",
      "Convergence in non-oil GDP shares across member states",
      "Convergence in manufacturing sector shares across member states",
      "Convergence in oil sector dependency across member states",
      "Convergence in government fiscal balance positions",
      "Convergence in central bank policy interest rates"
    ),
    Direction = rep("+", 7),
    Normalization = rep("Min-max", 7),
    stringsAsFactors = FALSE
  )
}

#' Metadata Tab UI
#'
#' @return Shiny UI element for metadata tab
metadata_tab_ui <- function() {
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
        p("The index covers the period 2015-2024 and aggregates performance across six dimensions,
          each capturing a distinct aspect of economic integration. Country-level scores are
          aggregated to a GCC-wide index using GDP-weighted averaging to reflect the relative
          economic significance of each member state."),

        h4("Methodology"),
        p("The GCCEII is constructed using the ", strong("COINr framework"),
          " (Composite Indicator Numerical Repository), following the JRC/OECD Handbook on
          Constructing Composite Indicators. The pipeline involves:"),
        tags$ol(
          tags$li(strong("Data extraction:"), " Raw indicator values from GCC-Stat databases and external sources"),
          tags$li(strong("Imputation:"), " Two-pass approach: linear interpolation for temporal gaps,
                  followed by EM (Expectation-Maximization) algorithm for remaining missing data"),
          tags$li(strong("Normalization:"), " Multi-strategy pipeline adapted to each indicator's distribution
                  (see normalization details below)"),
          tags$li(strong("Aggregation:"), " Arithmetic weighted mean within dimensions, then across dimensions
                  using the weights shown below"),
          tags$li(strong("GCC Aggregate:"), " GDP-weighted average across the six member states")
        ),

        h4("Scoring"),
        p("All dimension and overall scores are presented on a 0-100 scale. The normalization methodology varies by indicator type:"),
        tags$ul(
          tags$li(strong("Min-max normalization:"), " Most indicators are scaled using pooled min-max normalization
                  across all country-year observations (2015-2024). Best observation scores 100, lowest scores 0."),
          tags$li(strong("Winsorize + min-max:"), " Indicators with extreme outliers are first winsorized at the
                  5th/95th percentile, then min-max normalized. This prevents single outliers from compressing
                  the scale."),
          tags$li(strong("Z-score + rescale:"), " OCA criteria (inflation, M2 growth) and fiscal balance use
                  z-score normalization to handle distributions that cross zero, then rescaled to 0-100."),
          tags$li(strong("Goalpost:"), " Stock market openness (already on a 0-100% scale) uses goalpost
                  normalization with fixed bounds."),
          tags$li(strong("Bounded min-max:"), " Banking penetration uses min-max within a bounded range [50, 100].")
        ),
        p("Integration levels are classified as:"),
        tags$ul(
          tags$li(strong("Good (60-100):"), " Strong integration with well-functioning mechanisms"),
          tags$li(strong("Moderate (40-59):"), " Partial integration with room for improvement"),
          tags$li(strong("Weak (0-39):"), " Limited integration requiring policy attention")
        ),

        h4("Convergence Indicators"),
        p("Convergence indicators use a ", strong("distance-based"), " methodology, measuring each
          country's deviation from the GCC mean:"),
        tags$pre("score = 100 - |value - GCC_mean| / GCC_mean * 100   (capped at 0-100)"),
        p("Unlike the coefficient-of-variation approach, this produces ", em("country-specific"),
          " convergence scores: countries closer to the regional mean score higher.
          Seven convergence indicators cover income, prices, non-oil GDP, manufacturing,
          oil dependency, fiscal balance, and interest rates.")
      )
    ),

    # Dimension Weights
    fluidRow(
      box(
        width = 12,
        title = "Dimension Weights & Structure",
        status = "info",
        solidHeader = TRUE,
        p("The overall index combines six dimensions with the following weights.
          The index covers ", strong("32 operational indicators"), " across six dimensions:"),
        tableOutput("dimension_weights_table")
      )
    ),

    # Dimension indicator boxes
    fluidRow(
      box(width = 12, title = "Dimension 1: Trade Integration (20%) - 6 Indicators",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Measures the intensity and composition of intra-GCC merchandise and services trade,
            with emphasis on non-oil diversification and value chain participation."),
          tableOutput("trade_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = "Dimension 2: Financial & Monetary Integration (20%) - 8 Indicators",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Assesses convergence in monetary conditions (OCA criteria), banking sector integration,
            cross-border financial flows, and fiscal positioning within the GCC. Includes three OCA
            (Optimal Currency Area) readiness indicators: inflation convergence, monetary supply
            convergence, and GDP growth synchronization."),
          tableOutput("financial_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = "Dimension 3: Labor & Human Capital (20%) - 5 Indicators",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Captures the mobility of labor and people across GCC borders, including
            worker movements, tourism, and student exchanges, as well as labor market
            indicators. Worker and student mobility use the host-country perspective:
            how many GCC nationals does each country host?"),
          tableOutput("labor_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = "Dimension 4: Infrastructure Connectivity (20%) - 2 Indicators",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Tracks physical connectivity infrastructure that enables cross-border economic activity.
            Currently driven by two operational indicators (aviation and energy). Four additional
            indicators (railway, port, power grid, digital) are defined but awaiting external data sources."),
          tableOutput("infrastructure_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = "Dimension 5: Sustainability & Diversification (10%) - 4 Indicators",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Measures progress toward economic diversification away from hydrocarbon
            dependence, aligned with national Vision 2030 strategies. Covers both
            GDP composition and government revenue diversification."),
          tableOutput("sustainability_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = "Dimension 6: Macroeconomic Convergence (10%) - 7 Indicators",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Assesses the degree to which GCC economies are converging in key macroeconomic
            variables, a prerequisite for deeper monetary integration and potential currency union.
            Uses distance-based scores: each country is scored based on its deviation from the
            GCC mean for each variable."),
          tableOutput("convergence_indicators_table"))
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
          tags$li(strong("Financial data:"), " GCC-Stat monetary & financial database"),
          tags$li(strong("Labor & mobility:"), " GCC-Stat labor statistics, common market database"),
          tags$li(strong("Tourism:"), " GCC-Stat tourism database"),
          tags$li(strong("Infrastructure:"), " GCC-Stat common market and energy databases"),
          tags$li(strong("Price data:"), " GCC-Stat CPI database, World Bank ICP (2017, 2021)"),
          tags$li(strong("Fiscal data:"), " GCC-Stat government finance statistics"),
          tags$li(strong("Interest rates:"), " Central bank policy rates (harmonized panel)"),
          tags$li(strong("FDI data:"), " UNCTAD / national sources")
        ),
        p(em("Note: Some indicators use proxy measures or estimates where direct data is unavailable.
             The COINr pipeline applies two-pass imputation (linear interpolation + EM algorithm)
             to handle remaining gaps."))
      )
    )
  )
}
