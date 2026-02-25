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
#' @param lang Language code ("en" or "ar")
#' @return Shiny UI element for metadata tab
metadata_tab_ui <- function(lang = "en") {
  tabItem(
    tabName = "metadata",

    # Overview Section
    fluidRow(
      box(
        width = 12,
        title = t("meta_about_title", lang),
        status = "primary",
        solidHeader = TRUE,

        h4(t("meta_purpose", lang)),
        p(t("meta_purpose_p1", lang)),
        p(t("meta_purpose_p2", lang)),

        h4(t("meta_scope", lang)),
        p(t("meta_scope_p1", lang)),

        h4(t("meta_methodology", lang)),
        p(t("meta_methodology_intro", lang)),
        tags$ol(
          tags$li(strong(t("meta_label_data_extraction", lang)), " ", t("meta_step_extraction", lang)),
          tags$li(strong(t("meta_label_imputation", lang)), " ", t("meta_step_imputation", lang)),
          tags$li(strong(t("meta_label_normalization", lang)), " ", t("meta_step_normalization", lang)),
          tags$li(strong(t("meta_label_aggregation", lang)), " ", t("meta_step_aggregation", lang)),
          tags$li(strong(t("meta_label_gcc_aggregate", lang)), " ", t("meta_step_gcc_agg", lang))
        ),

        h4(t("meta_scoring", lang)),
        p(t("meta_scoring_intro", lang)),
        tags$ul(
          tags$li(strong(t("meta_norm_label_minmax", lang)), " ", t("meta_norm_minmax", lang)),
          tags$li(strong(t("meta_norm_label_winsorize", lang)), " ", t("meta_norm_winsorize", lang)),
          tags$li(strong(t("meta_norm_label_zscore", lang)), " ", t("meta_norm_zscore", lang)),
          tags$li(strong(t("meta_norm_label_goalpost", lang)), " ", t("meta_norm_goalpost", lang)),
          tags$li(strong(t("meta_norm_label_bounded", lang)), " ", t("meta_norm_bounded", lang))
        ),
        p(t("meta_levels_intro", lang)),
        tags$ul(
          tags$li(strong(t("meta_label_level_good", lang)), " ", t("meta_level_good", lang)),
          tags$li(strong(t("meta_label_level_moderate", lang)), " ", t("meta_level_moderate", lang)),
          tags$li(strong(t("meta_label_level_weak", lang)), " ", t("meta_level_weak", lang))
        ),

        h4(t("meta_convergence", lang)),
        p(t("meta_convergence_p1", lang)),
        tags$pre("score = 100 - |value - GCC_mean| / GCC_mean * 100   (capped at 0-100)"),
        p(t("meta_convergence_p2", lang))
      )
    ),

    # Dimension Weights
    fluidRow(
      box(
        width = 12,
        title = t("meta_weights_title", lang),
        status = "info",
        solidHeader = TRUE,
        p(t("meta_weights_intro", lang)),
        tableOutput("dimension_weights_table")
      )
    ),

    # Dimension indicator boxes
    fluidRow(
      box(width = 12, title = t("meta_dim1_title", lang),
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p(t("meta_dim1_desc", lang)),
          tableOutput("trade_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = t("meta_dim2_title", lang),
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p(t("meta_dim2_desc", lang)),
          tableOutput("financial_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = t("meta_dim3_title", lang),
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p(t("meta_dim3_desc", lang)),
          tableOutput("labor_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = t("meta_dim4_title", lang),
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p(t("meta_dim4_desc", lang)),
          tableOutput("infrastructure_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = t("meta_dim5_title", lang),
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p(t("meta_dim5_desc", lang)),
          tableOutput("sustainability_indicators_table"))
    ),
    fluidRow(
      box(width = 12, title = t("meta_dim6_title", lang),
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          p(t("meta_dim6_desc", lang)),
          tableOutput("convergence_indicators_table"))
    ),

    # Data Sources
    fluidRow(
      box(
        width = 12,
        title = t("meta_data_sources", lang),
        status = "warning",
        solidHeader = TRUE,
        tags$ul(
          tags$li(strong(t("meta_src_label_trade", lang)), " ", t("meta_src_trade", lang)),
          tags$li(strong(t("meta_src_label_national", lang)), " ", t("meta_src_national", lang)),
          tags$li(strong(t("meta_src_label_financial", lang)), " ", t("meta_src_financial", lang)),
          tags$li(strong(t("meta_src_label_labor", lang)), " ", t("meta_src_labor", lang)),
          tags$li(strong(t("meta_src_label_tourism", lang)), " ", t("meta_src_tourism", lang)),
          tags$li(strong(t("meta_src_label_infra", lang)), " ", t("meta_src_infra", lang)),
          tags$li(strong(t("meta_src_label_price", lang)), " ", t("meta_src_price", lang)),
          tags$li(strong(t("meta_src_label_fiscal", lang)), " ", t("meta_src_fiscal", lang)),
          tags$li(strong(t("meta_src_label_interest", lang)), " ", t("meta_src_interest", lang)),
          tags$li(strong(t("meta_src_label_fdi", lang)), " ", t("meta_src_fdi", lang))
        ),
        p(em(t("meta_data_note", lang)))
      )
    )
  )
}
