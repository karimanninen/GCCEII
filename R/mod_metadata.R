# =============================================================================
# R/mod_metadata.R
# GCCEII Dashboard – Bilingual Metadata & Methodology Tab
# =============================================================================
# Exports:
#   metadata_ui(id, lang)      – Full tab UI
#   metadata_server(id, lang)  – Server (reactive DT rendering)
#
# Dependencies: R/translations.R must be sourced first.
#
# Content structure:
#   Section 1 – Index Overview          (bilingual text + key-facts table)
#   Section 2 – Dimension Framework     (bilingual dimension table)
#   Section 3 – Full Indicator List     (bilingual DT, filterable)
#   Section 4 – Methodology             (bilingual accordion: normalisation,
#                                        weighting, aggregation)
#   Section 5 – Data Sources            (bilingual sources table)
#   Section 6 – References & Citation   (bilingual)
# =============================================================================

library(shiny)
library(shinydashboard)
library(DT)

# ── Static bilingual data tables ──────────────────────────────────────────────

# Dimension summary table (one row per dimension)
.dim_framework <- data.frame(
  dim_num   = 1:6,
  dim_en    = c(
    "Infrastructure Connectivity",
    "Energy & Resource Integration",
    "Financial Integration",
    "Trade Integration",
    "Labor & Human Capital Integration",
    "Sustainability & Diversification"
  ),
  dim_ar    = c(
    "\u062a\u0631\u0627\u0628\u0637 \u0627\u0644\u0628\u0646\u064a\u0629 \u0627\u0644\u062a\u062d\u062a\u064a\u0629",
    "\u062a\u0643\u0627\u0645\u0644 \u0627\u0644\u0637\u0627\u0642\u0629 \u0648\u0627\u0644\u0645\u0648\u0627\u0631\u062f",
    "\u0627\u0644\u062a\u0643\u0627\u0645\u0644 \u0627\u0644\u0645\u0627\u0644\u064a",
    "\u062a\u0643\u0627\u0645\u0644 \u0627\u0644\u062a\u062c\u0627\u0631\u0629",
    "\u062a\u0643\u0627\u0645\u0644 \u0627\u0644\u0639\u0645\u0627\u0644\u0629 \u0648\u0631\u0623\u0633 \u0627\u0644\u0645\u0627\u0644 \u0627\u0644\u0628\u0634\u0631\u064a",
    "\u0627\u0644\u0627\u0633\u062a\u062f\u0627\u0645\u0629 \u0648\u0627\u0644\u062a\u0646\u0648\u064a\u0639"
  ),
  weight    = c("30%", "25%", "20%", "15%", "10%", "Overlay"),
  n_cats    = c(4L, 4L, 3L, 3L, 3L, 3L),
  n_ind_poc = c(10L, 8L, 11L, 9L, 6L, 6L),   # Proof-of-concept indicators
  n_ind_all = c(16L, 14L, 20L, 18L, 10L, 12L), # Full set
  source_en = c(
    "GCC Secretariat, GCCIA, ITU, IATA",
    "IRENA, IEA, OPEC, GPCA",
    "IMF, BIS, Central Banks, Exchanges",
    "UN Comtrade, GCC-Stat, WTO",
    "GCC-Stat, ILO, UNESCO",
    "IRENA, World Bank, GCC-Stat"
  ),
  source_ar = c(
    "\u0627\u0644\u0623\u0645\u0627\u0646\u0629 \u0627\u0644\u0639\u0627\u0645\u0629 \u0644\u0645\u062c\u0644\u0633 \u0627\u0644\u062a\u0639\u0627\u0648\u0646\u060c GCCIA\u060c ITU\u060c IATA",
    "IRENA\u060c IEA\u060c \u0623\u0648\u0628\u0643\u060c GPCA",
    "\u0635\u0646\u062f\u0648\u0642 \u0627\u0644\u0646\u0642\u062f \u0627\u0644\u062f\u0648\u0644\u064a\u060c BIS\u060c \u0627\u0644\u0628\u0646\u0648\u0643 \u0627\u0644\u0645\u0631\u0643\u0632\u064a\u0629\u060c \u0627\u0644\u0628\u0648\u0631\u0635\u0627\u062a",
    "UN Comtrade\u060c GCC-Stat\u060c \u0645\u0646\u0638\u0645\u0629 \u0627\u0644\u062a\u062c\u0627\u0631\u0629 \u0627\u0644\u0639\u0627\u0644\u0645\u064a\u0629",
    "GCC-Stat\u060c \u0645\u0646\u0638\u0645\u0629 \u0627\u0644\u0639\u0645\u0644 \u0627\u0644\u062f\u0648\u0644\u064a\u0629\u060c \u064a\u0648\u0646\u0633\u0643\u0648",
    "IRENA\u060c \u0627\u0644\u0628\u0646\u0643 \u0627\u0644\u062f\u0648\u0644\u064a\u060c GCC-Stat"
  ),
  stringsAsFactors = FALSE
)

# Full indicator list (key subset; extend by appending more rows)
.indicators <- data.frame(
  dim_num  = c(
    1,1,1,1,1,1,
    2,2,2,2,
    3,3,3,3,3,
    4,4,4,4,4,4,
    5,5,5,
    6,6,6
  ),
  ind_num  = c(
    1,2,3,6,8,9,
    17,18,26,27,
    33,34,39,40,44,
    51,52,55,57,58,63,
    69,70,71,
    37,38,50
  ),
  indicator_en = c(
    # Infrastructure
    "GCC Railway Project completion rate",
    "Port connectivity & freight efficiency index",
    "Aviation hub development metrics",
    "Power grid interconnection capacity utilisation",
    "Cross-border data flows",
    "Digital government service interoperability",
    # Energy
    "Joint renewable energy projects capacity",
    "Regional renewable electricity trade flows",
    "Commodity price stabilisation mechanisms",
    "OPEC+ coordination effectiveness",
    # Financial
    "Currency coordination mechanisms readiness",
    "Interest rate convergence",
    "Cross-border banking services penetration",
    "Payment system integration (Buna)",
    "Cross-listing of securities",
    # Trade
    "Intra-GCC merchandise trade intensity",
    "Trade in services intra-GCC share",
    "Non-oil intra-GCC merchandise trade intensity",
    "Customs procedures harmonisation index",
    "Common external tariff implementation rate",
    "Intra-GCC intermediate goods trade share",
    # Labor
    "Intra-GCC employment of nationals",
    "Brain circulation indicators",
    "Intra-GCC student mobility rate",
    # Sustainability (selected)
    "Non-oil sector cross-border investments",
    "Green/sustainable finance flows",
    "Sukuk cross-listing volume"
  ),
  indicator_ar = c(
    # Infrastructure
    "\u0646\u0633\u0628\u0629 \u0625\u0646\u062c\u0627\u0632 \u0645\u0634\u0631\u0648\u0639 \u0633\u0643\u0629 \u062d\u062f\u064a\u062f \u0645\u062c\u0644\u0633 \u0627\u0644\u062a\u0639\u0627\u0648\u0646",
    "\u0645\u0624\u0634\u0631 \u062a\u0631\u0627\u0628\u0637 \u0627\u0644\u0645\u0648\u0627\u0646\u0626 \u0648\u0643\u0641\u0627\u0621\u0629 \u0627\u0644\u0634\u062d\u0646",
    "\u0645\u0642\u0627\u064a\u064a\u0633 \u062a\u0637\u0648\u064a\u0631 \u0645\u062d\u0627\u0648\u0631 \u0627\u0644\u0637\u064a\u0631\u0627\u0646",
    "\u0645\u0639\u062f\u0644 \u0627\u0633\u062a\u062e\u062f\u0627\u0645 \u0637\u0627\u0642\u0629 \u0627\u0644\u0631\u0628\u0637 \u0627\u0644\u0643\u0647\u0631\u0628\u0627\u0626\u064a",
    "\u062a\u062f\u0641\u0642\u0627\u062a \u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a \u0639\u0628\u0631 \u0627\u0644\u062d\u062f\u0648\u062f",
    "\u062a\u0634\u063a\u064a\u0644\u064a\u0629 \u062e\u062f\u0645\u0627\u062a \u0627\u0644\u062d\u0643\u0648\u0645\u0629 \u0627\u0644\u0631\u0642\u0645\u064a\u0629 \u0639\u0628\u0631 \u0627\u0644\u062d\u062f\u0648\u062f",
    # Energy
    "\u0637\u0627\u0642\u0629 \u0645\u0634\u0627\u0631\u064a\u0639 \u0627\u0644\u0637\u0627\u0642\u0629 \u0627\u0644\u0645\u062a\u062c\u062f\u062f\u0629 \u0627\u0644\u0645\u0634\u062a\u0631\u0643\u0629",
    "\u062a\u062f\u0641\u0642\u0627\u062a \u0627\u0644\u0643\u0647\u0631\u0628\u0627\u0621 \u0627\u0644\u0645\u062a\u062c\u062f\u062f\u0629 \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a\u0629",
    "\u0622\u0644\u064a\u0627\u062a \u0627\u0633\u062a\u0642\u0631\u0627\u0631 \u0623\u0633\u0639\u0627\u0631 \u0627\u0644\u0633\u0644\u0639",
    "\u0641\u0639\u0627\u0644\u064a\u0629 \u0627\u0644\u062a\u0646\u0633\u064a\u0642 \u0636\u0645\u0646 \u0625\u0637\u0627\u0631 \u0623\u0648\u0628\u0643+",
    # Financial
    "\u062c\u0627\u0647\u0632\u064a\u0629 \u0622\u0644\u064a\u0627\u062a \u062a\u0646\u0633\u064a\u0642 \u0627\u0644\u0639\u0645\u0644\u0629",
    "\u062a\u0642\u0627\u0631\u0628 \u0623\u0633\u0639\u0627\u0631 \u0627\u0644\u0641\u0627\u0626\u062f\u0629",
    "\u0627\u062e\u062a\u0631\u0627\u0642 \u0627\u0644\u062e\u062f\u0645\u0627\u062a \u0627\u0644\u0645\u0635\u0631\u0641\u064a\u0629 \u0639\u0628\u0631 \u0627\u0644\u062d\u062f\u0648\u062f",
    "\u062a\u0643\u0627\u0645\u0644 \u0646\u0638\u0627\u0645 \u0627\u0644\u0645\u062f\u0641\u0648\u0639\u0627\u062a (\u0628\u0648\u0646\u0627)",
    "\u0627\u0644\u0625\u062f\u0631\u0627\u062c \u0627\u0644\u0645\u0632\u062f\u0648\u062c \u0644\u0644\u0623\u0648\u0631\u0627\u0642 \u0627\u0644\u0645\u0627\u0644\u064a\u0629",
    # Trade
    "\u0643\u062b\u0627\u0641\u0629 \u0627\u0644\u062a\u062c\u0627\u0631\u0629 \u0627\u0644\u0633\u0644\u0639\u064a\u0629 \u0627\u0644\u0628\u064a\u0646\u064a\u0629",
    "\u062d\u0635\u0629 \u062a\u062c\u0627\u0631\u0629 \u0627\u0644\u062e\u062f\u0645\u0627\u062a \u0627\u0644\u0628\u064a\u0646\u064a\u0629",
    "\u0643\u062b\u0627\u0641\u0629 \u0627\u0644\u062a\u062c\u0627\u0631\u0629 \u063a\u064a\u0631 \u0627\u0644\u0646\u0641\u0637\u064a\u0629 \u0627\u0644\u0628\u064a\u0646\u064a\u0629",
    "\u0645\u0624\u0634\u0631 \u062a\u0648\u062d\u064a\u062f \u0625\u062c\u0631\u0627\u0621\u0627\u062a \u0627\u0644\u062c\u0645\u0627\u0631\u0643",
    "\u0645\u0639\u062f\u0644 \u062a\u0637\u0628\u064a\u0642 \u0627\u0644\u062a\u0639\u0631\u064a\u0641\u0629 \u0627\u0644\u062c\u0645\u0631\u0643\u064a\u0629 \u0627\u0644\u062e\u0627\u0631\u062c\u064a\u0629 \u0627\u0644\u0645\u0648\u062d\u062f\u0629",
    "\u062d\u0635\u0629 \u0627\u0644\u0633\u0644\u0639 \u0627\u0644\u0648\u0633\u064a\u0637\u0629 \u0641\u064a \u0627\u0644\u062a\u062c\u0627\u0631\u0629 \u0627\u0644\u0628\u064a\u0646\u064a\u0629",
    # Labor
    "\u062a\u0648\u0638\u064a\u0641 \u0627\u0644\u0645\u0648\u0627\u0637\u0646\u064a\u0646 \u0641\u064a \u062f\u0648\u0644 \u0627\u0644\u0645\u062c\u0644\u0633 \u0627\u0644\u0623\u062e\u0631\u0649",
    "\u0645\u0624\u0634\u0631\u0627\u062a \u062a\u062f\u0627\u0648\u0644 \u0627\u0644\u0643\u0641\u0627\u0621\u0627\u062a",
    "\u0645\u0639\u062f\u0644 \u062a\u0646\u0642\u0644 \u0627\u0644\u0637\u0644\u0627\u0628 \u0627\u0644\u0628\u064a\u0646\u064a",
    # Sustainability
    "\u0627\u0633\u062a\u062b\u0645\u0627\u0631\u0627\u062a \u0627\u0644\u0642\u0637\u0627\u0639\u0627\u062a \u063a\u064a\u0631 \u0627\u0644\u0646\u0641\u0637\u064a\u0629 \u0639\u0628\u0631 \u0627\u0644\u062d\u062f\u0648\u062f",
    "\u062a\u062f\u0641\u0642\u0627\u062a \u0627\u0644\u062a\u0645\u0648\u064a\u0644 \u0627\u0644\u0623\u062e\u0636\u0631 \u0648\u0627\u0644\u0645\u0633\u062a\u062f\u0627\u0645",
    "\u062d\u062c\u0645 \u0627\u0644\u0635\u0643\u0648\u0643 \u0627\u0644\u0645\u062f\u0631\u062c\u0629 \u0639\u0644\u0649 \u0628\u0648\u0631\u0635\u0627\u062a \u0645\u062a\u0639\u062f\u062f\u0629"
  ),
  poc_set  = c(
    TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,
    TRUE,FALSE,TRUE,TRUE,
    FALSE,TRUE,TRUE,TRUE,TRUE,
    TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
    TRUE,FALSE,TRUE,
    TRUE,FALSE,TRUE
  ),
  final_set = c(
    TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,
    TRUE,TRUE,TRUE,TRUE,
    FALSE,TRUE,TRUE,TRUE,TRUE,
    TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
    TRUE,TRUE,TRUE,
    TRUE,TRUE,TRUE
  ),
  source_en = c(
    "GCC Secretariat / Transport Ministries",
    "UNCTAD LSCI / GCC Customs",
    "IATA / Civil Aviation Authorities",
    "GCCIA",
    "Telecom Regulators / ITU",
    "UN e-Government Survey",
    "IRENA / National Renewable Agencies",
    "GCCIA / IRENA",
    "OPEC / IMF Fiscal Monitor",
    "OPEC Monthly Reports / IEA",
    "Central Banks / IMF Article IV",
    "Central Banks / Bloomberg / IMF IFS",
    "Central Banks / BIS",
    "Arab Monetary Fund (Buna)",
    "Stock Exchanges / CMAs",
    "UN Comtrade / GCC-Stat / IMF DOTS",
    "IMF BOP (BPM6) / GCC-Stat",
    "UN Comtrade / GCC-Stat",
    "GCC Customs Union / WTO TFA",
    "GCC Secretariat / WTO",
    "UN Comtrade (BEC) / GCC-Stat",
    "Labor Surveys / GCC-Stat",
    "Labor Surveys / Professional Associations",
    "Education Ministries / UNESCO",
    "Central Banks / FDI Agencies",
    "Climate Bonds Initiative / Bloomberg ESG",
    "Bloomberg Sukuk Indices / Exchanges"
  ),
  stringsAsFactors = FALSE
)

# Data sources reference table
.data_sources <- data.frame(
  acronym = c(
    "UN Comtrade", "GCC-Stat / MARSA", "IMF BOP / DOTS",
    "World Bank", "ILO", "UNESCO", "IRENA", "IEA",
    "OPEC", "GCCIA", "BIS", "Arab Monetary Fund"
  ),
  name_en = c(
    "United Nations Comtrade Database",
    "GCC-Stat MARSA Dissemination Platform",
    "IMF Balance of Payments / Direction of Trade Statistics",
    "World Bank Open Data",
    "International Labour Organization ILOSTAT",
    "UNESCO Institute for Statistics",
    "International Renewable Energy Agency",
    "International Energy Agency",
    "Organization of the Petroleum Exporting Countries",
    "Gulf Cooperation Council Interconnection Authority",
    "Bank for International Settlements Statistics",
    "Arab Monetary Fund – Buna Payment Platform"
  ),
  name_ar = c(
    "\u0642\u0627\u0639\u062f\u0629 \u0628\u064a\u0627\u0646\u0627\u062a \u062a\u062c\u0627\u0631\u0629 \u0627\u0644\u0623\u0645\u0645 \u0627\u0644\u0645\u062a\u062d\u062f\u0629",
    "\u0645\u0646\u0635\u0629 \u0645\u0631\u0633\u0649 \u0644\u0644\u0628\u064a\u0627\u0646\u0627\u062a - GCC-Stat",
    "\u0645\u064a\u0632\u0627\u0646 \u0627\u0644\u0645\u062f\u0641\u0648\u0639\u0627\u062a / \u0625\u062d\u0635\u0627\u0621\u0627\u062a \u0627\u062a\u062c\u0627\u0647 \u0627\u0644\u062a\u062c\u0627\u0631\u0629 - \u0635\u0646\u062f\u0648\u0642 \u0627\u0644\u0646\u0642\u062f \u0627\u0644\u062f\u0648\u0644\u064a",
    "\u0628\u064a\u0627\u0646\u0627\u062a \u0627\u0644\u0628\u0646\u0643 \u0627\u0644\u062f\u0648\u0644\u064a \u0627\u0644\u0645\u0641\u062a\u0648\u062d\u0629",
    "\u0642\u0627\u0639\u062f\u0629 ILOSTAT - \u0645\u0646\u0638\u0645\u0629 \u0627\u0644\u0639\u0645\u0644 \u0627\u0644\u062f\u0648\u0644\u064a\u0629",
    "\u0645\u0639\u0647\u062f \u0627\u0644\u064a\u0648\u0646\u0633\u0643\u0648 \u0644\u0644\u0625\u062d\u0635\u0627\u0621",
    "\u0627\u0644\u0648\u0643\u0627\u0644\u0629 \u0627\u0644\u062f\u0648\u0644\u064a\u0629 \u0644\u0644\u0637\u0627\u0642\u0629 \u0627\u0644\u0645\u062a\u062c\u062f\u062f\u0629",
    "\u0648\u0643\u0627\u0644\u0629 \u0627\u0644\u0637\u0627\u0642\u0629 \u0627\u0644\u062f\u0648\u0644\u064a\u0629",
    "\u0645\u0646\u0638\u0645\u0629 \u0627\u0644\u062f\u0648\u0644 \u0627\u0644\u0645\u0635\u062f\u0631\u0629 \u0644\u0644\u0628\u062a\u0631\u0648\u0644",
    "\u0647\u064a\u0626\u0629 \u0627\u0644\u0631\u0628\u0637 \u0627\u0644\u0643\u0647\u0631\u0628\u0627\u0626\u064a \u0644\u062f\u0648\u0644 \u0645\u062c\u0644\u0633 \u0627\u0644\u062a\u0639\u0627\u0648\u0646",
    "\u0625\u062d\u0635\u0627\u0621\u0627\u062a \u0628\u0646\u0643 \u0627\u0644\u062a\u0633\u0648\u064a\u0627\u062a \u0627\u0644\u062f\u0648\u0644\u064a\u0629",
    "\u0645\u0646\u0635\u0629 \u0628\u0648\u0646\u0627 \u0644\u0644\u0645\u062f\u0641\u0648\u0639\u0627\u062a - \u0635\u0646\u062f\u0648\u0642 \u0627\u0644\u0646\u0642\u062f \u0627\u0644\u0639\u0631\u0628\u064a"
  ),
  coverage = c(
    "2010\u2013present",
    "2015\u2013present",
    "2000\u2013present",
    "2000\u2013present",
    "2000\u2013present",
    "2000\u2013present",
    "2010\u2013present",
    "1990\u2013present",
    "1970\u2013present",
    "2009\u2013present",
    "2000\u2013present",
    "2020\u2013present"
  ),
  stringsAsFactors = FALSE
)

# ── UI helper functions ───────────────────────────────────────────────────────

# A styled section heading
.meta_h2 <- function(text, icon_name = NULL, color = "#1a3a5c") {
  tags$h4(
    style = paste0(
      "color:", color, ";",
      "border-bottom: 2px solid ", color, ";",
      "padding-bottom: 6px; margin-top: 28px; margin-bottom: 14px;"
    ),
    if (!is.null(icon_name)) tagList(icon(icon_name), " "),
    text
  )
}

# A styled key-value row for the key facts box
.kv_row <- function(label, value) {
  tags$tr(
    tags$td(style = "font-weight:600; padding: 4px 12px 4px 0; white-space:nowrap;", label),
    tags$td(style = "padding: 4px 0;", value)
  )
}

# Methodology step box
.method_step <- function(number, title_en, title_ar, body_en, body_ar, lang,
                          icon_name, color = "#2e86c1") {
  title <- if (lang == "ar") title_ar else title_en
  body  <- if (lang == "ar") body_ar  else body_en
  dir   <- get_direction(lang)

  tags$div(
    class = "method-step",
    style = paste0(
      "border-left: 4px solid ", color, ";",
      "padding: 12px 16px; margin-bottom: 14px;",
      "background: #f8fbff; border-radius: 0 6px 6px 0;"
    ),
    tags$div(
      style = "display:flex; align-items:center; gap:10px; margin-bottom:6px;",
      tags$span(
        style = paste0(
          "background:", color, "; color:white; border-radius:50%;",
          "width:26px; height:26px; display:inline-flex;",
          "align-items:center; justify-content:center;",
          "font-weight:700; font-size:13px; flex-shrink:0;"
        ),
        number
      ),
      tags$strong(style = paste0("color:", color, "; direction:", dir), title)
    ),
    tags$p(style = paste0("margin:0; direction:", dir, "; font-size:0.92rem;"), body)
  )
}

# ── Main UI ───────────────────────────────────────────────────────────────────

#' Metadata tab UI
#'
#' @param id   Shiny module namespace.
#' @param lang "en" or "ar".
metadata_ui <- function(id, lang = "en") {
  ns  <- NS(id)
  dir <- get_direction(lang)

  tags$div(
    class = paste("metadata-body", if (lang == "ar") "lang-ar" else ""),
    style = paste0("direction:", dir, "; padding: 10px 4px;"),

    # ── Section 1: Index Overview ────────────────────────────────────────────
    .meta_h2(t("meta_section_overview", lang), "info-circle"),

    fluidRow(
      column(8,
        tags$p(style = "font-size:1rem; line-height:1.75;",
          t("meta_overview_p1", lang)
        ),
        tags$p(style = "font-size:1rem; line-height:1.75; margin-top:10px;",
          if (lang == "ar") {
            paste(
              "\u064a\u0633\u062a\u0646\u062f \u0627\u0644\u0645\u0624\u0634\u0631 \u0625\u0644\u0649 \u0625\u0637\u0627\u0631 \u0645\u0641\u0627\u0647\u064a\u0645\u064a \u064a\u063a\u0637\u064a \u0633\u062a\u0629 \u0623\u0628\u0639\u0627\u062f",
              "\u0644\u0644\u062a\u0643\u0627\u0645\u0644: \u062a\u0631\u0627\u0628\u0637 \u0627\u0644\u0628\u0646\u064a\u0629 \u0627\u0644\u062a\u062d\u062a\u064a\u0629\u060c \u0627\u0644\u0637\u0627\u0642\u0629 \u0648\u0627\u0644\u0645\u0648\u0627\u0631\u062f\u060c \u0627\u0644\u062a\u0643\u0627\u0645\u0644 \u0627\u0644\u0645\u0627\u0644\u064a\u060c",
              "\u062a\u0643\u0627\u0645\u0644 \u0627\u0644\u062a\u062c\u0627\u0631\u0629\u060c \u062a\u0643\u0627\u0645\u0644 \u0627\u0644\u0639\u0645\u0627\u0644\u0629\u060c \u0648\u0627\u0644\u0627\u0633\u062a\u062f\u0627\u0645\u0629 \u0648\u0627\u0644\u062a\u0646\u0648\u064a\u0639.",
              "\u064a\u0648\u0644\u062f \u0643\u0644 \u0628\u064f\u0639\u062f \u062f\u0631\u062c\u0629 \u0641\u0631\u0639\u064a\u0629\u060c \u062a\u064f\u062f\u0645\u062c \u062c\u0645\u064a\u0639\u0647\u0627 \u0641\u064a \u0645\u0624\u0634\u0631 \u0625\u062c\u0645\u0627\u0644\u064a \u0645\u0648\u062d\u062f."
            )
          } else {
            paste(
              "The index is built on a theoretical framework covering six dimensions of integration:",
              "Infrastructure Connectivity, Energy & Resource Integration, Financial Integration,",
              "Trade Integration, Labor & Human Capital Integration, and Sustainability & Diversification.",
              "Each dimension produces a sub-index score, which are combined into a single overall index."
            )
          }
        )
      ),
      column(4,
        tags$div(
          class = "well",
          style = "background:#f0f6ff; border:1px solid #c3d9f5; border-radius:8px; padding:16px;",
          tags$strong(
            style = "color:#1a3a5c; font-size:0.95rem;",
            if (lang == "ar") "\u062d\u0642\u0627\u0626\u0642 \u0631\u0626\u064a\u0633\u064a\u0629" else "Key Facts"
          ),
          tags$hr(style = "margin:8px 0;"),
          tags$table(
            style = "width:100%; font-size:0.88rem;",
            .kv_row(if (lang == "ar") "\u0627\u0644\u062f\u0648\u0644 \u0627\u0644\u0623\u0639\u0636\u0627\u0621" else "Member States", "6 GCC States"),
            .kv_row(if (lang == "ar") "\u0627\u0644\u0623\u0628\u0639\u0627\u062f" else "Dimensions", "6"),
            .kv_row(if (lang == "ar") "\u0645\u062c\u0645\u0648\u0639\u0629 \u0627\u0644\u0645\u0624\u0634\u0631\u0627\u062a" else "Total Indicators", "90 (full set)"),
            .kv_row(if (lang == "ar") "\u0645\u0624\u0634\u0631\u0627\u062a \u0627\u0644\u062a\u062c\u0631\u064a\u0628\u064a\u0629" else "Proof-of-Concept", "50 indicators"),
            .kv_row(if (lang == "ar") "\u0627\u0644\u062a\u063a\u0637\u064a\u0629 \u0627\u0644\u0632\u0645\u0646\u064a\u0629" else "Time Coverage", "2015\u20132023"),
            .kv_row(if (lang == "ar") "\u0627\u0644\u062f\u0648\u0631\u064a\u0629" else "Frequency", if (lang == "ar") "\u0633\u0646\u0648\u064a" else "Annual"),
            .kv_row(if (lang == "ar") "\u0627\u0644\u0645\u0646\u0647\u062c\u064a\u0629" else "Methodology", "JRC/OECD 2008"),
            .kv_row(if (lang == "ar") "\u062c\u0647\u0629 \u0627\u0644\u0625\u0635\u062f\u0627\u0631" else "Produced by", "GCC-Stat")
          )
        )
      )
    ),

    # ── Section 2: Dimension Framework ──────────────────────────────────────
    .meta_h2(t("meta_section_dimensions", lang), "th-list"),

    tags$p(
      style = "margin-bottom:12px; font-size:0.92rem;",
      if (lang == "ar") {
        "\u064a\u0639\u0631\u0636 \u0627\u0644\u062c\u062f\u0648\u0644 \u0623\u062f\u0646\u0627\u0647 \u0627\u0644\u0623\u0628\u0639\u0627\u062f \u0627\u0644\u0633\u062a\u0629 \u0645\u0639 \u0623\u0648\u0632\u0627\u0646\u0647\u0627 \u0648\u0639\u062f\u062f \u0627\u0644\u0645\u0624\u0634\u0631\u0627\u062a \u0648\u0627\u0644\u0645\u0635\u0627\u062f\u0631 \u0627\u0644\u0631\u0626\u064a\u0633\u064a\u0629."
      } else {
        "The table below presents the six dimensions with their weights, category counts, indicator counts, and primary data sources."
      }
    ),
    DTOutput(ns("dim_table")),

    tags$br(),

    # Dimension description boxes
    fluidRow(
      lapply(1:6, function(i) {
        d   <- .dim_framework[i, ]
        ttl <- if (lang == "ar") d$dim_ar else d$dim_en
        column(4,
          tags$div(
            class = "metadata-dim-card",
            style = paste0(
              "border-left: 4px solid #2e86c1;",
              "padding: 12px; margin-bottom: 12px;",
              "background: white; border-radius: 0 6px 6px 0;",
              "box-shadow: 0 1px 4px rgba(0,0,0,0.08);"
            ),
            tags$div(
              style = "display:flex; justify-content:space-between; align-items:center;",
              tags$strong(style = paste0("direction:", dir, "; font-size:0.88rem; color:#1a3a5c;"), ttl),
              tags$span(
                class = "label label-primary",
                style = "font-size:0.78rem; padding:2px 8px;",
                d$weight
              )
            )
          )
        )
      })
    ),

    # ── Section 3: Indicator List ────────────────────────────────────────────
    .meta_h2(
      if (lang == "ar") "\u0642\u0627\u0626\u0645\u0629 \u0627\u0644\u0645\u0624\u0634\u0631\u0627\u062a"
      else "Indicator List",
      "list"
    ),

    tags$p(
      style = "font-size:0.92rem; margin-bottom:10px;",
      if (lang == "ar") {
        "\u064a\u0639\u0631\u0636 \u0627\u0644\u062c\u062f\u0648\u0644 \u0627\u0644\u0645\u0624\u0634\u0631\u0627\u062a \u0627\u0644\u0645\u062f\u0631\u062c\u0629 \u0641\u064a \u0645\u062c\u0645\u0648\u0639\u0629 \u0627\u0644\u062a\u062c\u0631\u064a\u0628\u064a\u0629 (\u0645\u062d\u062f\u062f\u0629 \u0628\u0639\u0644\u0627\u0645\u0629 \u2714) \u0648\u0627\u0644\u0645\u062c\u0645\u0648\u0639\u0629 \u0627\u0644\u0646\u0647\u0627\u0626\u064a\u0629 \u0627\u0644\u0645\u0642\u062a\u0631\u062d\u0629."
      } else {
        "Indicators marked \u2714 are included in the Proof-of-Concept set; the full indicator set covers 90 indicators across six dimensions."
      }
    ),
    DTOutput(ns("ind_table")),

    tags$br(),

    # ── Section 4: Methodology ───────────────────────────────────────────────
    .meta_h2(t("meta_section_methodology", lang), "cogs"),

    tags$p(style = "font-size:1rem; line-height:1.75;", t("meta_methodology_p1", lang)),

    tags$br(),

    # Four-step methodology accordion
    .method_step(
      "1",
      "Theoretical Framework",
      "\u0627\u0644\u0625\u0637\u0627\u0631 \u0627\u0644\u0646\u0638\u0631\u064a",
      paste(
        "The GCCEII framework is grounded in the theory of regional economic integration",
        "(Balassa, 1961) and adapted for the specific structural characteristics of GCC economies.",
        "Six dimensions are identified through expert consultation, literature review, and",
        "benchmarking against the ARII (Africa) and ARCII (Asia-Pacific) indices."
      ),
      paste(
        "\u064a\u0633\u062a\u0646\u062f \u0625\u0637\u0627\u0631 GCCEII \u0625\u0644\u0649 \u0646\u0638\u0631\u064a\u0629 \u0627\u0644\u062a\u0643\u0627\u0645\u0644 \u0627\u0644\u0627\u0642\u062a\u0635\u0627\u062f\u064a \u0627\u0644\u0625\u0642\u0644\u064a\u0645\u064a (\u0628\u0627\u0644\u0627\u0633\u0627\u060c 1961)",
        "\u0648\u064a\u062a\u0643\u064a\u0641 \u0645\u0639 \u0627\u0644\u062e\u0635\u0627\u0626\u0635 \u0627\u0644\u0647\u064a\u0643\u0644\u064a\u0629 \u0644\u0627\u0642\u062a\u0635\u0627\u062f\u064a\u0627\u062a \u062f\u0648\u0644 \u0645\u062c\u0644\u0633 \u0627\u0644\u062a\u0639\u0627\u0648\u0646.",
        "\u062a\u0645 \u062a\u062d\u062f\u064a\u062f \u0633\u062a\u0629 \u0623\u0628\u0639\u0627\u062f \u0639\u0628\u0631 \u0627\u0644\u062a\u0634\u0627\u0648\u0631 \u0645\u0639 \u0627\u0644\u062e\u0628\u0631\u0627\u0621 \u0648\u0645\u0631\u0627\u062c\u0639\u0629 \u0627\u0644\u0623\u062f\u0628\u064a\u0627\u062a",
        "\u0648\u0627\u0644\u0645\u0642\u0627\u0631\u0646\u0629 \u0645\u0639 \u0645\u0624\u0634\u0631\u064a ARII (\u0623\u0641\u0631\u064a\u0642\u064a\u0627) \u0648ARCII (\u0622\u0633\u064a\u0627 \u0648\u0627\u0644\u0645\u062d\u064a\u0637 \u0627\u0644\u0647\u0627\u062f\u0626)."
      ),
      lang, "sitemap", "#1a6fa8"
    ),

    .method_step(
      "2",
      "Data Selection & Quality",
      "\u0627\u062e\u062a\u064a\u0627\u0631 \u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a \u0648\u062c\u0648\u062f\u062a\u0647\u0627",
      paste(
        "Indicators are selected based on four criteria: relevance to the integration dimension,",
        "data availability across all six GCC states, time coverage from 2015 onward, and",
        "methodological comparability. Missing values are imputed using last-observation-carried-forward",
        "(LOCF) for gaps of one year; longer gaps are flagged and excluded from scoring."
      ),
      paste(
        "\u062a\u064f\u062e\u062a\u0627\u0631 \u0627\u0644\u0645\u0624\u0634\u0631\u0627\u062a \u0648\u0641\u0642 \u0623\u0631\u0628\u0639\u0629 \u0645\u0639\u0627\u064a\u064a\u0631: \u0627\u0644\u0635\u0644\u0629 \u0628\u0627\u0644\u0628\u064f\u0639\u062f\u060c \u062a\u0648\u0641\u0631 \u0627\u0644\u0628\u064a\u0627\u0646\u0627\u062a",
        "\u0639\u0628\u0631 \u062f\u0648\u0644 \u0627\u0644\u0645\u062c\u0644\u0633 \u0627\u0644\u0633\u062a\u060c \u0627\u0644\u062a\u063a\u0637\u064a\u0629 \u0627\u0644\u0632\u0645\u0646\u064a\u0629 \u0645\u0646\u0630 2015\u060c",
        "\u0648\u0642\u0627\u0628\u0644\u064a\u0629 \u0627\u0644\u0645\u0642\u0627\u0631\u0646\u0629 \u0627\u0644\u0645\u0646\u0647\u062c\u064a\u0629. \u062a\u064f\u0639\u0627\u062f\u0644 \u0627\u0644\u0642\u064a\u0645 \u0627\u0644\u0645\u0641\u0642\u0648\u062f\u0629 \u0628\u0627\u0633\u062a\u062e\u062f\u0627\u0645 \u0622\u062e\u0631 \u0645\u0644\u0627\u062d\u0638\u0629",
        "\u0644\u0641\u062c\u0648\u0627\u062a \u0645\u062f\u062a\u0647\u0627 \u0633\u0646\u0629 \u0648\u0627\u062d\u062f\u0629\u061b \u0623\u0645\u0627 \u0627\u0644\u0641\u062c\u0648\u0627\u062a \u0627\u0644\u0623\u0637\u0648\u0644 \u0641\u062a\u064f\u0639\u0644\u0651\u0645 \u0648\u062a\u064f\u0633\u062a\u0628\u0639\u062f \u0645\u0646 \u0627\u0644\u062a\u0642\u064a\u064a\u0645."
      ),
      lang, "filter", "#1e8449"
    ),

    .method_step(
      "3",
      "Normalisation",
      "\u0627\u0644\u062a\u0637\u0628\u064a\u0639",
      t("meta_normalisation_p1", "en"),
      t("meta_normalisation_p1", "ar"),
      lang, "sliders", "#7d3c98"
    ),

    .method_step(
      "4",
      "Weighting & Aggregation",
      "\u0627\u0644\u0623\u0648\u0632\u0627\u0646 \u0648\u0627\u0644\u062a\u062c\u0645\u064a\u0639",
      paste(t("meta_weighting_p1", "en"), t("meta_aggregation_p1", "en")),
      paste(t("meta_weighting_p1", "ar"), t("meta_aggregation_p1", "ar")),
      lang, "balance-scale", "#c0392b"
    ),

    # ── Section 5: Data Sources ──────────────────────────────────────────────
    .meta_h2(t("meta_section_data_sources", lang), "database"),

    DTOutput(ns("sources_table")),

    tags$br(),

    # ── Section 6: References & Citation ────────────────────────────────────
    .meta_h2(
      if (lang == "ar") "\u0627\u0644\u0645\u0631\u0627\u062c\u0639 \u0648\u0627\u0644\u0627\u0633\u062a\u0634\u0647\u0627\u062f"
      else "References & How to Cite",
      "book"
    ),

    tags$div(
      class = "well",
      style = "background:#f9f9f9; border-radius:6px; font-size:0.9rem;",
      tags$strong(if (lang == "ar") "\u0627\u0633\u062a\u0634\u0647\u0627\u062f \u0628\u0627\u0644\u0645\u0635\u062f\u0631:" else "Suggested citation:"),
      tags$p(
        style = "font-style:italic; margin:8px 0;",
        "GCC-Stat (2025). GCC Economic Integration Index: Framework and Results 2015\u20132023.",
        "Statistical Centre for the Cooperation Council for the Arab States of the Gulf. Muscat."
      ),
      tags$hr(style = "margin:10px 0;"),
      tags$strong(if (lang == "ar") "\u0645\u0631\u062c\u0639 \u0627\u0644\u0645\u0646\u0647\u062c\u064a\u0629:" else "Methodology reference:"),
      tags$p(
        style = "margin:8px 0;",
        "Nardo, M., Saisana, M., Saltelli, A., Tarantola, S., Hoffmann, A., & Giovannini, E. (2008).",
        tags$em("Handbook on Constructing Composite Indicators: Methodology and User Guide."),
        "OECD Publishing / European Commission JRC."
      ),
      tags$hr(style = "margin:10px 0;"),
      tags$strong(if (lang == "ar") "\u0645\u0624\u0634\u0631\u0627\u062a \u0645\u0631\u062c\u0639\u064a\u0629 \u0644\u0644\u0645\u0642\u0627\u0631\u0646\u0629:" else "Comparable indices:"),
      tags$ul(
        style = "margin:6px 0;",
        tags$li(
          "Africa Regional Integration Index (ARII) – African Development Bank / AU / ECA (2016)"
        ),
        tags$li(
          "Asia-Pacific Regional Cooperation and Integration Index (ARCII) – ADB (2019)"
        )
      )
    ),

    tags$br()

  ) # end metadata-body
}

# ── Server ────────────────────────────────────────────────────────────────────

#' Metadata tab server
#'
#' @param id    Shiny module namespace.
#' @param lang  Reactive expression returning "en" or "ar".
metadata_server <- function(id, lang = reactive("en")) {
  moduleServer(id, function(input, output, session) {

    # ── Dimension framework table ──────────────────────────────────────────

    output$dim_table <- renderDT({
      l <- lang()
      df <- .dim_framework

      display <- data.frame(
        `#`              = df$dim_num,
        Dimension        = if (l == "ar") df$dim_ar       else df$dim_en,
        Weight           = df$weight,
        Categories       = df$n_cats,
        `PoC Indicators` = df$n_ind_poc,
        `Full Set`       = df$n_ind_all,
        `Primary Sources`= if (l == "ar") df$source_ar   else df$source_en,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      # Column header translations
      if (l == "ar") {
        names(display) <- c(
          "#",
          "\u0627\u0644\u0628\u064f\u0639\u062f",
          "\u0627\u0644\u0648\u0632\u0646",
          "\u0627\u0644\u0641\u0626\u0627\u062a",
          "\u0645\u0624\u0634\u0631\u0627\u062a \u0627\u0644\u062a\u062c\u0631\u064a\u0628\u064a\u0629",
          "\u0627\u0644\u0645\u062c\u0645\u0648\u0639\u0629 \u0627\u0644\u0643\u0627\u0645\u0644\u0629",
          "\u0627\u0644\u0645\u0635\u0627\u062f\u0631 \u0627\u0644\u0631\u0626\u064a\u0633\u064a\u0629"
        )
      }

      datatable(
        display,
        options   = list(
          dom        = "t",
          pageLength = 10,
          scrollX    = TRUE,
          language   = if (l == "ar") list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/ar.json") else list()
        ),
        rownames  = FALSE,
        class     = "stripe hover compact",
        selection = "none"
      ) %>%
        formatStyle(
          if (l == "ar") "\u0627\u0644\u0648\u0632\u0646" else "Weight",
          fontWeight = "bold",
          color      = "#2e86c1"
        )
    })

    # ── Indicator list table ───────────────────────────────────────────────

    output$ind_table <- renderDT({
      l  <- lang()
      df <- .indicators

      # Dimension label lookup
      dim_labels <- if (l == "ar") .dim_framework$dim_ar else .dim_framework$dim_en

      display <- data.frame(
        `Dim`       = df$dim_num,
        Dimension   = dim_labels[df$dim_num],
        `Ind #`     = df$ind_num,
        Indicator   = if (l == "ar") df$indicator_ar else df$indicator_en,
        PoC         = ifelse(df$poc_set,   "\u2714", "\u2013"),
        `Full Set`  = ifelse(df$final_set, "\u2714", "\u2013"),
        Source      = df$source_en,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      if (l == "ar") {
        names(display) <- c(
          "\u062f",
          "\u0627\u0644\u0628\u064f\u0639\u062f",
          "\u0631\u0642\u0645",
          "\u0627\u0644\u0645\u0624\u0634\u0631",
          "\u062a\u062c\u0631\u064a\u0628\u064a",
          "\u0646\u0647\u0627\u0626\u064a",
          "\u0627\u0644\u0645\u0635\u062f\u0631"
        )
      }

      datatable(
        display,
        filter  = "top",
        options = list(
          pageLength = 15,
          scrollX    = TRUE,
          language   = if (l == "ar") list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/ar.json") else list(),
          columnDefs = list(
            list(width = "35%", targets = if (l == "ar") 3 else 3)
          )
        ),
        rownames  = FALSE,
        class     = "stripe hover compact",
        selection = "none"
      ) %>%
        formatStyle(
          if (l == "ar") "\u062a\u062c\u0631\u064a\u0628\u064a" else "PoC",
          color      = styleEqual(c("\u2714", "\u2013"), c("#1e8449", "#aaa")),
          fontWeight = styleEqual(c("\u2714", "\u2013"), c("bold", "normal"))
        ) %>%
        formatStyle(
          if (l == "ar") "\u0646\u0647\u0627\u0626\u064a" else "Full Set",
          color      = styleEqual(c("\u2714", "\u2013"), c("#1a6fa8", "#aaa")),
          fontWeight = styleEqual(c("\u2714", "\u2013"), c("bold", "normal"))
        )
    })

    # ── Data sources table ─────────────────────────────────────────────────

    output$sources_table <- renderDT({
      l  <- lang()
      df <- .data_sources

      display <- data.frame(
        Acronym      = df$acronym,
        Organisation = if (l == "ar") df$name_ar else df$name_en,
        Coverage     = df$coverage,
        check.names  = FALSE,
        stringsAsFactors = FALSE
      )

      if (l == "ar") {
        names(display) <- c(
          "\u0627\u0644\u0631\u0645\u0632",
          "\u0627\u0644\u0645\u0646\u0638\u0645\u0629",
          "\u0627\u0644\u062a\u063a\u0637\u064a\u0629"
        )
      }

      datatable(
        display,
        options = list(
          dom        = "ft",
          pageLength = 15,
          scrollX    = TRUE,
          language   = if (l == "ar") list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/ar.json") else list()
        ),
        rownames  = FALSE,
        class     = "stripe hover compact",
        selection = "none"
      )
    })

  }) # end moduleServer
}
