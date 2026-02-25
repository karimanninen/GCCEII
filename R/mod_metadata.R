# ==============================================================================
# METADATA TABLES MODULE
# ==============================================================================
# Defines all metadata tables for the Metadata & Analysis tab.
# Updated to match the COINr pipeline (32 indicators, 6 dimensions).
# Bilingual: all content uses t() from R/translations.R
# ==============================================================================

#' Get Dimension Weights Table Data
#'
#' @param lang Language code ("en" or "ar")
#' @return Data frame with dimension weights
get_dimension_weights_table <- function(lang = "en") {
  df <- data.frame(
    col1 = c(t("meta_wt_trade", lang), t("meta_wt_financial", lang),
             t("meta_wt_labor", lang), t("meta_wt_infra", lang),
             t("meta_wt_sustain", lang), t("meta_wt_convergence", lang),
             t("meta_wt_total", lang)),
    col2 = c("20%", "20%", "20%", "20%", "10%", "10%", "100%"),
    col3 = c("6", "8", "5", "2", "4", "7", "32"),
    col4 = c(t("meta_focus_trade", lang), t("meta_focus_financial", lang),
             t("meta_focus_labor", lang), t("meta_focus_infra", lang),
             t("meta_focus_sustain", lang), t("meta_focus_convergence", lang),
             t("meta_focus_total", lang)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(df) <- c(t("meta_col_dimension", lang), t("meta_col_weight", lang),
                 t("meta_col_indicators", lang), t("meta_col_focus", lang))
  df
}

#' Get Trade Indicators Table Data
#'
#' @param lang Language code ("en" or "ar")
#' @return Data frame with trade indicators
get_trade_indicators_table <- function(lang = "en") {
  ind_names <- c("Intra-GCC Trade Intensity", "Services Trade Share",
                 "Non-oil Trade Intensity", "Services as % of Total Trade",
                 "Intermediate Goods Share", "Trade Diversification")
  desc_keys <- c("meta_desc_ind_51", "meta_desc_ind_52", "meta_desc_ind_55",
                 "meta_desc_ind_56", "meta_desc_ind_63", "meta_desc_ind_64")
  norm_keys <- c("meta_norm_val_winsorize_minmax", "meta_norm_val_winsorize_minmax",
                 "meta_norm_val_winsorize_minmax", "meta_norm_val_minmax",
                 "meta_norm_val_minmax", "meta_norm_val_minmax")
  df <- data.frame(
    col1 = c("ind_51", "ind_52", "ind_55", "ind_56", "ind_63", "ind_64"),
    col2 = translate_indicators(ind_names, lang),
    col3 = vapply(desc_keys, t, character(1), lang = lang),
    col4 = c("+", "+", "+", "+", "+", "+"),
    col5 = vapply(norm_keys, t, character(1), lang = lang),
    stringsAsFactors = FALSE
  )
  names(df) <- c(t("meta_col_code", lang), t("meta_col_indicator", lang),
                 t("meta_col_description", lang), t("meta_col_direction", lang),
                 t("meta_col_normalization", lang))
  rownames(df) <- NULL
  df
}

#' Get Financial Indicators Table Data
#'
#' @param lang Language code ("en" or "ar")
#' @return Data frame with financial indicators
get_financial_indicators_table <- function(lang = "en") {
  ind_names <- c("Annual Inflation Rate", "M2 Money Supply Growth",
                 "Real GDP Growth Rate", "Intra-GCC FDI Share",
                 "GCC Banking Penetration", "Stock Market Openness",
                 "Banking Sector Depth", "Fiscal Balance Ratio")
  desc_keys <- c("meta_desc_ind_inflation", "meta_desc_ind_m2",
                 "meta_desc_ind_gdp", "meta_desc_ind_fdi",
                 "meta_desc_ind_banking", "meta_desc_ind_stock",
                 "meta_desc_ind_bank_depth", "meta_desc_ind_fiscal")
  norm_keys <- c("meta_norm_val_zscore", "meta_norm_val_zscore",
                 "meta_norm_val_zscore", "meta_norm_val_winsorize_minmax",
                 "meta_norm_val_bounded", "meta_norm_val_goalpost",
                 "meta_norm_val_minmax", "meta_norm_val_zscore")
  df <- data.frame(
    col1 = c("ind_inflation", "ind_m2_growth", "ind_gdp_growth",
             "ind_31_fdi", "ind_39_banking", "ind_44_stock",
             "ind_bank_depth", "ind_fiscal_balance"),
    col2 = translate_indicators(ind_names, lang),
    col3 = vapply(desc_keys, t, character(1), lang = lang),
    col4 = c("-", "-", "+", "+", "+", "+", "+", "+"),
    col5 = vapply(norm_keys, t, character(1), lang = lang),
    stringsAsFactors = FALSE
  )
  names(df) <- c(t("meta_col_code", lang), t("meta_col_indicator", lang),
                 t("meta_col_description", lang), t("meta_col_direction", lang),
                 t("meta_col_normalization", lang))
  rownames(df) <- NULL
  df
}

#' Get Labor Indicators Table Data
#'
#' @param lang Language code ("en" or "ar")
#' @return Data frame with labor indicators
get_labor_indicators_table <- function(lang = "en") {
  ind_names <- c("GCC National Worker Mobility", "Student Exchange Mobility",
                 "Intra-GCC Tourism Intensity", "Labor Force Participation Rate",
                 "Unemployment Rate")
  desc_keys <- c("meta_desc_ind_labor", "meta_desc_ind_student",
                 "meta_desc_ind_tourism", "meta_desc_ind_lfpr",
                 "meta_desc_ind_unemp")
  norm_keys <- c("meta_norm_val_winsorize_minmax", "meta_norm_val_winsorize_minmax",
                 "meta_norm_val_minmax", "meta_norm_val_minmax",
                 "meta_norm_val_winsorize_minmax")
  df <- data.frame(
    col1 = c("ind_69_labor", "ind_71_student", "ind_72_tourism",
             "ind_lfpr", "ind_unemployment"),
    col2 = translate_indicators(ind_names, lang),
    col3 = vapply(desc_keys, t, character(1), lang = lang),
    col4 = c("+", "+", "+", "+", "-"),
    col5 = vapply(norm_keys, t, character(1), lang = lang),
    stringsAsFactors = FALSE
  )
  names(df) <- c(t("meta_col_code", lang), t("meta_col_indicator", lang),
                 t("meta_col_description", lang), t("meta_col_direction", lang),
                 t("meta_col_normalization", lang))
  rownames(df) <- NULL
  df
}

#' Get Infrastructure Indicators Table Data
#'
#' @param lang Language code ("en" or "ar")
#' @return Data frame with infrastructure indicators
get_infrastructure_indicators_table <- function(lang = "en") {
  ind_names <- c("Aviation Connectivity", "Electricity Per Capita",
                 "GCC Railway Connectivity", "Port Connectivity Index",
                 "Power Grid Interconnection", "Digital Infrastructure Index")
  desc_keys <- c("meta_desc_ind_aviation", "meta_desc_ind_elec",
                 "meta_desc_ind_railway", "meta_desc_ind_port",
                 "meta_desc_ind_grid", "meta_desc_ind_digital")
  df <- data.frame(
    col1 = c("ind_3_aviation", "ind_elec_pc",
             "ind_1_railway", "ind_2_port", "ind_6_gccia", "ind_8_digital"),
    col2 = translate_indicators(ind_names, lang),
    col3 = vapply(desc_keys, t, character(1), lang = lang),
    col4 = c("+", "+", "+", "+", "+", "+"),
    col5 = c(t("meta_status_operational", lang), t("meta_status_operational", lang),
             t("meta_status_placeholder", lang), t("meta_status_placeholder", lang),
             t("meta_status_placeholder", lang), t("meta_status_placeholder", lang)),
    stringsAsFactors = FALSE
  )
  names(df) <- c(t("meta_col_code", lang), t("meta_col_indicator", lang),
                 t("meta_col_description", lang), t("meta_col_direction", lang),
                 t("meta_col_status", lang))
  rownames(df) <- NULL
  df
}

#' Get Sustainability Indicators Table Data
#'
#' @param lang Language code ("en" or "ar")
#' @return Data frame with sustainability indicators
get_sustainability_indicators_table <- function(lang = "en") {
  ind_names <- c("Non-oil GDP Share", "Oil Dependency",
                 "Manufacturing Share", "Non-oil Revenue Share")
  desc_keys <- c("meta_desc_ind_nonoil_gdp", "meta_desc_ind_oil_dep",
                 "meta_desc_ind_manuf", "meta_desc_ind_nonoil_rev")
  df <- data.frame(
    col1 = c("ind_non_oil_share", "ind_oil_share",
             "ind_manufacturing_share", "ind_nonoil_rev_share"),
    col2 = translate_indicators(ind_names, lang),
    col3 = vapply(desc_keys, t, character(1), lang = lang),
    col4 = c("+", "-", "+", "+"),
    col5 = rep(t("meta_norm_val_minmax", lang), 4),
    stringsAsFactors = FALSE
  )
  names(df) <- c(t("meta_col_code", lang), t("meta_col_indicator", lang),
                 t("meta_col_description", lang), t("meta_col_direction", lang),
                 t("meta_col_normalization", lang))
  rownames(df) <- NULL
  df
}

#' Get Convergence Indicators Table Data
#'
#' @param lang Language code ("en" or "ar")
#' @return Data frame with convergence indicators
get_convergence_indicators_table <- function(lang = "en") {
  ind_names <- c("Real Income Convergence", "Price Level Convergence",
                 "Non-oil GDP Convergence", "Manufacturing Convergence",
                 "Oil Dependency Convergence", "Fiscal Balance Convergence",
                 "Interest Rate Convergence")
  desc_keys <- c("meta_desc_ind_conv_income", "meta_desc_ind_conv_price",
                 "meta_desc_ind_conv_nonoil", "meta_desc_ind_conv_manuf",
                 "meta_desc_ind_conv_oil", "meta_desc_ind_conv_fiscal",
                 "meta_desc_ind_conv_interest")
  df <- data.frame(
    col1 = c("ind_conv_income", "ind_conv_price", "ind_conv_non_oil",
             "ind_conv_manufacturing", "ind_conv_oil",
             "ind_conv_fiscal", "ind_conv_interest"),
    col2 = translate_indicators(ind_names, lang),
    col3 = vapply(desc_keys, t, character(1), lang = lang),
    col4 = rep("+", 7),
    col5 = rep(t("meta_norm_val_minmax", lang), 7),
    stringsAsFactors = FALSE
  )
  names(df) <- c(t("meta_col_code", lang), t("meta_col_indicator", lang),
                 t("meta_col_description", lang), t("meta_col_direction", lang),
                 t("meta_col_normalization", lang))
  rownames(df) <- NULL
  df
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
