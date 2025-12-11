# ==============================================================================
# METADATA TABLES MODULE
# ==============================================================================
# Defines all metadata tables for the Metadata & Analysis tab
# ==============================================================================

#' Get Dimension Weights Table Data
#'
#' @return Data frame with dimension weights
get_dimension_weights_table <- function() {
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
                    "Composite integration measure"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Get Trade Indicators Table Data
#'
#' @return Data frame with trade indicators
get_trade_indicators_table <- function() {
  data.frame(
    Indicator = c("Intra-GCC Trade Intensity",
                  "Services Trade Share",
                  "Non-oil Trade Intensity",
                  "Services as % of Total Trade",
                  "Intermediate Goods Share",
                  "Trade by Product Type"),
    Description = c("Total intra-GCC trade (exports + imports) as percentage of GDP",
                    "Intra-GCC services trade as percentage of GDP",
                    "Intra-GCC non-oil merchandise trade as percentage of GDP",
                    "Share of services in total intra-GCC trade",
                    "Share of intermediate goods in intra-GCC trade (value chain indicator)",
                    "Composition of trade by BEC category (primary, intermediate, final goods)"),
    Weight = c("16.7%", "16.7%", "16.7%", "16.7%", "16.7%", "16.7%"),
    stringsAsFactors = FALSE
  )
}

#' Get Financial Indicators Table Data
#'
#' @return Data frame with financial indicators
get_financial_indicators_table <- function() {
  data.frame(
    Indicator = c("OCA Readiness",
                  "Intra-GCC FDI Share",
                  "Banking Sector Penetration",
                  "Stock Market Integration",
                  "Financial Depth Convergence"),
    Description = c("Optimal Currency Area composite: inflation convergence, monetary (M2) convergence, and GDP growth synchronization",
                    "Foreign direct investment originating from other GCC states as share of GDP",
                    "Number of GCC banks operating in other member states",
                    "Cross-listing of companies and market openness across GCC exchanges",
                    "Convergence in banking sector assets relative to GDP across member states"),
    Weight = c("20%", "20%", "20%", "20%", "20%"),
    stringsAsFactors = FALSE
  )
}

#' Get Labor Indicators Table Data
#'
#' @return Data frame with labor indicators
get_labor_indicators_table <- function() {
  data.frame(
    Indicator = c("GCC National Worker Mobility",
                  "Intra-GCC Tourism Intensity",
                  "Student Exchange Mobility",
                  "Labor Force Participation Convergence",
                  "Unemployment Rate Convergence"),
    Description = c("GCC nationals employed in other member states",
                    "Inbound tourists from other GCC states as share of total tourism",
                    "GCC students enrolled in universities in other member states",
                    "Convergence in labor force participation rates across GCC",
                    "Convergence in unemployment rates across member states"),
    Weight = c("30%", "30%", "20%", "10%", "10"),
    stringsAsFactors = FALSE
  )
}

#' Get Infrastructure Indicators Table Data
#'
#' @return Data frame with infrastructure indicators
get_infrastructure_indicators_table <- function() {
  data.frame(
    Indicator = c("Aviation Connectivity",
                  "Electricity Production Convergence"),
    Description = c("Intra-GCC air passenger arrivals relative to population",
                    "Convergence in per capita electricity production across member states"),
    Weight = c("50%", "50%"),
    stringsAsFactors = FALSE
  )
}

#' Get Sustainability Indicators Table Data
#'
#' @return Data frame with sustainability indicators
get_sustainability_indicators_table <- function() {
  data.frame(
    Indicator = c("Non-oil GDP Share",
                  "Non-oil GDP Convergence",
                  "Manufacturing Share Convergence"),
    Description = c("Direct measure of non-oil sector as percentage of total GDP",
                    "Convergence in non-oil GDP shares across member states",
                    "Convergence in manufacturing sector shares across member states"),
    Weight = c("30%", "40%", "30%"),
    stringsAsFactors = FALSE
  )
}

#' Get Convergence Indicators Table Data
#'
#' @return Data frame with convergence indicators
get_convergence_indicators_table <- function() {
  data.frame(
    Indicator = c("Real Income Convergence",
                  "Price Level Convergence",
                  "Financial Depth Convergence",
                  "LFPR Convergence",
                  "Unemployment Convergence",
                  "Non-oil GDP Convergence",
                  "Manufacturing Convergence",
                  "Oil Dependency Convergence"),
    Description = c("Convergence in PPP-adjusted GDP per capita",
                    "Convergence in price level indices (World=100)",
                    "Convergence in banking assets/GDP ratios",
                    "Convergence in labor force participation rates",
                    "Convergence in unemployment rates",
                    "Convergence in non-oil GDP shares",
                    "Convergence in manufacturing sector shares",
                    "Convergence in oil sector dependency"),
    Weight = c("12,5%", "12,5%", "12,5%", "12,5%", "12,5%", "12,5%", "12,5%", "12,5%"),
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

    # Dimension indicator boxes
    fluidRow(
      box(width = 12, title = "Dimension 1: Trade Integration (20%)",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Measures the intensity and composition of intra-GCC merchandise and services trade,
            with emphasis on non-oil diversification and value chain participation."),
          tableOutput("trade_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = "Dimension 2: Financial & Monetary Integration (20%)",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Assesses convergence in monetary conditions, banking sector integration,
            and cross-border financial flows within the GCC."),
          tableOutput("financial_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = "Dimension 3: Labor & Human Capital (20%)",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Captures the mobility of labor and people across GCC borders, including
            worker movements, tourism, and student exchanges."),
          tableOutput("labor_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = "Dimension 4: Infrastructure Connectivity (20%)",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Tracks physical and digital connectivity infrastructure that enables
            cross-border economic activity, including transport, energy, and telecommunications."),
          tableOutput("infrastructure_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = "Dimension 5: Sustainability & Diversification (10%)",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Measures progress toward economic diversification away from hydrocarbon
            dependence, aligned with national Vision 2030 strategies."),
          tableOutput("sustainability_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = "Dimension 6: Macroeconomic Convergence (10%)",
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p("Assesses the degree to which GCC economies are converging in key macroeconomic
            variables, a prerequisite for deeper monetary integration."),
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
          tags$li(strong("Financial data:"), " GCC-Stat monetary database"),
          tags$li(strong("Labor & mobility:"), " GCC-Stat labor statistics, tourism and common market databases"),
          tags$li(strong("Infrastructure:"), " GCC-Stat common market, energy and tourism databases"),
          tags$li(strong("Price data:"), " GCC-Stat national CPI database, ICP (International Comparison Program)")
        ),
        p(em("Note: Some indicators use proxy measures or estimates where direct data is unavailable.
             Methodology documentation is available upon request."))
      )
    )
  )
}
