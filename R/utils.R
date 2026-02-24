# ==============================================================================
# SHARED UTILITIES AND CONSTANTS
# ==============================================================================
# Common constants, color palettes, and helper functions for GCC EII Dashboard
# ==============================================================================

# ------------------------------------------------------------------------------
# DIMENSION DEFINITIONS
# ------------------------------------------------------------------------------

#' Dimension column names (as they appear in data)
DIMENSION_COLS <- c(
  "trade_score",
  "financial_score",

  "labor_score",
  "infrastructure_score",
  "sustainability_score",
  "convergence_score"
)

#' Dimension display labels
DIMENSION_LABELS <- c(
  "Trade",
  "Financial",
  "Labor",

  "Infrastructure",
  "Sustainability",
  "Convergence"
)

#' Named vector mapping columns to labels
DIMENSION_COL_TO_LABEL <- setNames(DIMENSION_LABELS, DIMENSION_COLS)

#' Named vector mapping change columns to labels
DIMENSION_CHANGE_TO_LABEL <- c(
  "trade_change" = "Trade",
  "financial_change" = "Financial",
  "labor_change" = "Labor",
  "infrastructure_change" = "Infrastructure",
  "sustainability_change" = "Sustainability",
  "convergence_change" = "Convergence"
)

#' Dimension weights for overall index calculation
DIMENSION_WEIGHTS <- c(
  Trade = 0.20,
  Financial = 0.20,
  Labor = 0.20,
  Infrastructure = 0.20,
  Sustainability = 0.10,
  Convergence = 0.10
)

# ------------------------------------------------------------------------------
# COLOR PALETTES
# ------------------------------------------------------------------------------

#' Country color palette (based on national colors)
COUNTRY_COLORS <- c(
  "UAE" = "#000000",
  "Qatar" = "#99154C",
  "Saudi Arabia" = "#008035",
  "Bahrain" = "#E20000",
  "Oman" = "#a3a3a3",
  "Kuwait" = "#00B1E6"
)

#' Extended colors including GCC aggregate
COUNTRY_COLORS_WITH_GCC <- c(COUNTRY_COLORS, "GCC" = "#DAA520")

#' Dimension colors for charts
DIMENSION_COLORS <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628")

#' Integration level colors
INTEGRATION_LEVEL_COLORS <- c(
  "Weak" = "#E41A1C",
  "Moderate" = "#FFFF33",
  "Good" = "#4DAF4A"
)

#' Heatmap color scale (red to green)
HEATMAP_COLORSCALE <- list(
  c(0, '#d73027'),
  c(0.25, '#fc8d59'),
  c(0.5, '#fee08b'),
  c(0.75, '#d9ef8b'),
  c(1, '#1a9850')
)

#' Change heatmap color scale (diverging)
CHANGE_COLORSCALE <- list(
  c(0, 'red'),
  c(0.5, 'white'),
  c(1, 'green')
)

# ------------------------------------------------------------------------------
# GDP WEIGHTS FOR AGGREGATION
# ------------------------------------------------------------------------------

#' GDP weights for GCC countries (approximate shares)
GDP_WEIGHTS <- data.frame(
  country = c("Bahrain", "Kuwait", "Oman", "Qatar", "Saudi Arabia", "UAE"),
  weight = c(0.02, 0.07, 0.04, 0.10, 0.42, 0.35),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# ICON MAPPINGS
# ------------------------------------------------------------------------------

#' Dimension icons for UI
DIMENSION_ICONS <- c(
  "Trade" = "exchange-alt",
  "Financial" = "university",
  "Labor" = "users",
  "Infrastructure" = "road",
  "Sustainability" = "leaf",
  "Convergence" = "chart-line"
)

#' Info box colors for dimensions
DIMENSION_BOX_COLORS <- c("blue", "green", "orange", "purple", "olive", "maroon")

# ------------------------------------------------------------------------------
# HELPER FUNCTIONS
# ------------------------------------------------------------------------------

#' Get unique countries from dimension scores (excluding GCC)
#'
#' @param dimension_scores Data frame with dimension scores
#' @return Character vector of sorted country names
get_countries <- function(dimension_scores) {
  dimension_scores %>%
    dplyr::filter(country != "GCC") %>%
    dplyr::pull(country) %>%
    unique() %>%
    sort()
}

#' Get integration level based on score
#'
#' @param score Numeric score
#' @return Character integration level
get_integration_level <- function(score) {
  dplyr::case_when(
    score >= 60 ~ "Good",
    score >= 40 ~ "Moderate",
    TRUE ~ "Weak"
  )
}

#' Get color for integration level
#'
#' @param level Character integration level
#' @return Color string
get_level_color <- function(level) {
  colors <- c("Good" = "green", "Moderate" = "yellow", "Weak" = "red")
  colors[level]
}

#' Format score for display
#'
#' @param score Numeric score
#' @param digits Number of decimal places
#' @return Formatted string
format_score <- function(score, digits = 1) {
  round(score, digits)
}

#' Create dimension data frame from latest GCC data
#'
#' @param gcc_data Single row of GCC data
#' @return Data frame with dimension labels and scores
create_dimension_df <- function(gcc_data) {
  data.frame(
    dimension = factor(DIMENSION_LABELS, levels = DIMENSION_LABELS),
    score = c(
      gcc_data$trade_score,
      gcc_data$financial_score,
      gcc_data$labor_score,
      gcc_data$infrastructure_score,
      gcc_data$sustainability_score,
      gcc_data$convergence_score
    )
  )
}

#' Get scores vector from data row
#'
#' @param data_row Single row of data with dimension scores
#' @return Numeric vector of scores
get_scores_vector <- function(data_row) {
  c(
    data_row$trade_score,
    data_row$financial_score,
    data_row$labor_score,
    data_row$infrastructure_score,
    data_row$sustainability_score,
    data_row$convergence_score
  )
}
