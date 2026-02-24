# ==============================================================================
# DATA LOADING MODULE
# ==============================================================================
# Handles all data import and transformation for the GCC EII Dashboard
#
# Data sources (in priority order):
#   1. gcceii_scores.csv  — unified long-format export from COINr pipeline
#   2. Fusion Registry    — PostgreSQL dataflow with identical schema
#   3. time_series_complete.csv / RData — legacy wide-format (fallback)
# ==============================================================================

# ------------------------------------------------------------------------------
# Country name mapping: engine → dashboard
# ------------------------------------------------------------------------------

#' Map country identifiers to dashboard display names
#'
#' The COINr engine uses ISO3 codes (ARE, BHR, ...) while the dashboard
#' displays short names (UAE, Qatar, ...). This function handles the mapping.
#'
#' @param x Character vector of country codes or full names
#' @return Character vector of dashboard display names
map_country_names <- function(x) {
  lookup <- c(
    "ARE" = "UAE", "BHR" = "Bahrain", "SAU" = "Saudi Arabia",
    "OMN" = "Oman", "QAT" = "Qatar", "KWT" = "Kuwait", "GCC" = "GCC",
    "United Arab Emirates" = "UAE"
  )
  mapped <- lookup[x]
  # Pass through any names not in lookup (already correct)
  ifelse(is.na(mapped), x, mapped)
}

# ------------------------------------------------------------------------------
# Dimension code → dashboard column name mapping
# ------------------------------------------------------------------------------

DIMENSION_CODE_MAP <- c(
  "Trade"          = "trade_score",
  "Financial"      = "financial_score",
  "Labor"          = "labor_score",
  "Infrastructure" = "infrastructure_score",
  "Sustainability" = "sustainability_score",
  "Convergence"    = "convergence_score"
)

# ==============================================================================
# MAIN ENTRY POINT
# ==============================================================================

#' Load GCC Integration Data
#'
#' Loads data and returns a list with the four data frames the dashboard expects.
#' Tries sources in order: gcceii_scores.csv, Fusion Registry, legacy files.
#'
#' @param output_dir Path to the output directory containing data files
#' @param use_fusion Logical, whether to attempt Fusion Registry loading
#' @return List containing dimension_scores, gcc_ts, yoy_changes, country_data,
#'         and optionally indicator_detail
load_gcc_data <- function(output_dir = "output", use_fusion = FALSE) {

  scores_csv <- file.path(output_dir, "gcceii_scores.csv")
  scores_rds <- file.path(output_dir, "gcceii_scores.rds")
  legacy_rdata <- file.path(output_dir, "gcc_integration_workspace.RData")
  legacy_csv <- file.path(output_dir, "time_series_complete.csv")

  # ------------------------------------------------------------------
  # Source 1: gcceii_scores.csv / .rds (preferred)
  # ------------------------------------------------------------------
  if (file.exists(scores_rds)) {
    message("Loading data from gcceii_scores.rds ...")
    scores_long <- readRDS(scores_rds)
    result <- transform_scores_to_dashboard(scores_long)
    message("Data loaded from gcceii_scores.rds")
    return(log_and_return(result))
  }

  if (file.exists(scores_csv)) {
    message("Loading data from gcceii_scores.csv ...")
    scores_long <- readr::read_csv(scores_csv, show_col_types = FALSE)
    result <- transform_scores_to_dashboard(scores_long)
    message("Data loaded from gcceii_scores.csv")
    return(log_and_return(result))
  }

  # ------------------------------------------------------------------
  # Source 2: Fusion Registry (PostgreSQL)
  # ------------------------------------------------------------------
  if (use_fusion) {
    tryCatch({
      message("Attempting Fusion Registry data load ...")
      scores_long <- load_scores_from_fusion()
      result <- transform_scores_to_dashboard(scores_long)
      message("Data loaded from Fusion Registry")
      return(log_and_return(result))
    }, error = function(e) {
      message("Fusion Registry unavailable: ", e$message)
      message("Falling back to legacy files ...")
    })
  }

  # ------------------------------------------------------------------
  # Source 3: Legacy wide-format files (backward compatibility)
  # ------------------------------------------------------------------
  if (file.exists(legacy_rdata)) {
    message("Loading data from legacy workspace file ...")
    temp_env <- new.env()
    load(legacy_rdata, envir = temp_env)
    if (exists("time_series_complete", envir = temp_env)) {
      ts_data <- get("time_series_complete", envir = temp_env)
      result <- process_legacy_wide_data(ts_data)
      message("Data loaded from legacy workspace")
      return(log_and_return(result))
    }
  }

  if (file.exists(legacy_csv)) {
    message("Loading data from legacy CSV ...")
    ts_data <- readr::read_csv(legacy_csv, show_col_types = FALSE)
    result <- process_legacy_wide_data(ts_data)
    message("Data loaded from legacy CSV")
    return(log_and_return(result))
  }

  stop("ERROR: Could not find data files. Please ensure one of:\n",
       "  1. gcceii_scores.csv or .rds exists in ", output_dir, "/, OR\n",
       "  2. time_series_complete.csv or .RData exists in ", output_dir, "/\n\n",
       "Run the build_gcceii_coin.R pipeline first to generate these files.")
}

# ==============================================================================
# TRANSFORM: long-format scores → dashboard data structures
# ==============================================================================

#' Transform unified long-format scores to dashboard data frames
#'
#' Converts the gcceii_scores long format (one row per country × year × code)
#' into the wide-format data frames the dashboard expects.
#'
#' @param scores_long Data frame in gcceii_scores format
#' @return List with dimension_scores, gcc_ts, yoy_changes, country_data,
#'         indicator_detail
transform_scores_to_dashboard <- function(scores_long) {

  # Map country identifiers to dashboard display names
  scores_long <- scores_long %>%
    dplyr::mutate(display_name = map_country_names(country))

  # --- Level 2 (dimensions) → wide format ---
  dim_wide <- scores_long %>%
    dplyr::filter(level == 2) %>%
    dplyr::select(country = display_name, year, code, score) %>%
    tidyr::pivot_wider(names_from = code, values_from = score)

  # Rename dimension columns to dashboard conventions
  for (dim_name in names(DIMENSION_CODE_MAP)) {
    col_name <- DIMENSION_CODE_MAP[dim_name]
    if (dim_name %in% names(dim_wide)) {
      names(dim_wide)[names(dim_wide) == dim_name] <- col_name
    }
  }

  # --- Level 3 (overall index) ---
  idx_wide <- scores_long %>%
    dplyr::filter(level == 3) %>%
    dplyr::select(country = display_name, year, overall_index = score,
                  integration_level)

  # --- Combine dimensions + index for countries (exclude GCC) ---
  dimension_scores <- dim_wide %>%
    dplyr::inner_join(idx_wide, by = c("country", "year")) %>%
    dplyr::filter(country != "GCC") %>%
    dplyr::mutate(
      integration_level = dplyr::case_when(
        overall_index >= 60 ~ "Good",
        overall_index >= 40 ~ "Moderate",
        TRUE ~ "Weak"
      )
    )

  # --- GCC aggregate time series ---
  gcc_ts <- dim_wide %>%
    dplyr::inner_join(idx_wide, by = c("country", "year")) %>%
    dplyr::filter(country == "GCC") %>%
    dplyr::mutate(
      overall = overall_index,
      integration_level = dplyr::case_when(
        overall_index >= 60 ~ "Good",
        overall_index >= 40 ~ "Moderate",
        TRUE ~ "Weak"
      )
    ) %>%
    dplyr::select(country, year, overall, trade_score, financial_score,
                  labor_score, infrastructure_score, sustainability_score,
                  convergence_score, integration_level)

  # --- Country data (countries + GCC, dimension scores only) ---
  country_data <- dim_wide %>%
    dplyr::select(year, country, trade_score, financial_score, labor_score,
                  infrastructure_score, sustainability_score, convergence_score)

  # --- Year-over-year changes ---
  yoy_changes <- calculate_yoy_changes(dimension_scores)

  # --- Indicator-level detail (Level 1) ---
  indicator_detail <- scores_long %>%
    dplyr::filter(level == 1) %>%
    dplyr::mutate(country = display_name) %>%
    dplyr::select(country, year, dimension = parent, indicator_code = code,
                  indicator_label = label, raw_value, normalized_value = score)

  list(
    dimension_scores = dimension_scores,
    gcc_ts = gcc_ts,
    yoy_changes = yoy_changes,
    country_data = country_data,
    indicator_detail = indicator_detail
  )
}

# ==============================================================================
# FUSION REGISTRY LOADER (placeholder)
# ==============================================================================

#' Load scores from Fusion Registry
#'
#' Connects to the MARSA Dissemination Warehouse and loads the
#' gcceii_scores dataflow. Requires RPostgres and database credentials
#' in environment variables (MARSA_HOST, MARSA_PORT, etc.).
#'
#' @return Data frame in gcceii_scores long format
load_scores_from_fusion <- function() {

  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("RPostgres package required for Fusion Registry access")
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("DBI package required for Fusion Registry access")
  }

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("MARSA_HOST", "localhost"),
    port     = as.integer(Sys.getenv("MARSA_PORT", "5434")),
    dbname   = Sys.getenv("MARSA_DB", "dissemination"),
    user     = Sys.getenv("MARSA_USER", "marsa_reader"),
    password = Sys.getenv("MARSA_PASSWORD")
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Query the gcceii_scores view/table
  # Schema and table name should match the FR dataflow configuration
  query <- "SELECT country, country_name, year, level, code, label,
                   parent, direction, weight, raw_value, score, integration_level
            FROM gcceii_scores
            ORDER BY year, country, level, code"

  scores_long <- DBI::dbGetQuery(con, query)
  scores_long <- tibble::as_tibble(scores_long)

  message(paste("  Loaded", nrow(scores_long), "rows from Fusion Registry"))
  return(scores_long)
}

# ==============================================================================
# LEGACY WIDE-FORMAT LOADER (backward compatibility)
# ==============================================================================

#' Process legacy wide-format time series data
#'
#' Handles the old time_series_complete.csv format for backward compatibility.
#'
#' @param time_series_complete Data frame in legacy wide format
#' @return List of processed data frames
process_legacy_wide_data <- function(time_series_complete) {

  # Extract dimension scores for COUNTRIES ONLY (exclude GCC aggregate)
  dimension_scores <- time_series_complete %>%
    dplyr::filter(country != "GCC") %>%
    dplyr::select(country, year, overall_index,
                  trade_score, financial_score, labor_score,
                  infrastructure_score, sustainability_score, convergence_score) %>%
    dplyr::mutate(integration_level = dplyr::case_when(
      overall_index >= 60 ~ "Good",
      overall_index >= 40 ~ "Moderate",
      TRUE ~ "Weak"
    ))

  # Extract GCC aggregate timeseries (GDP-weighted only for display)
  gcc_ts <- time_series_complete %>%
    dplyr::filter(country == "GCC")

  # If method column exists, filter to GDP-weighted
  if ("method" %in% names(gcc_ts)) {
    gcc_ts <- gcc_ts %>% dplyr::filter(method == "gdp")
  }

  gcc_ts <- gcc_ts %>%
    dplyr::mutate(overall = overall_index) %>%
    dplyr::select(country, year, overall, trade_score, financial_score, labor_score,
                  infrastructure_score, sustainability_score, convergence_score,
                  integration_level)

  # Country data (countries + GCC)
  country_data <- time_series_complete %>%
    dplyr::select(year, country, trade_score, financial_score, labor_score,
                  infrastructure_score, sustainability_score, convergence_score)

  # Filter GCC AGGREGATE duplicates if method column exists
  if ("method" %in% names(time_series_complete)) {
    country_data <- time_series_complete %>%
      dplyr::filter(!(country == "GCC" & method != "gdp")) %>%
      dplyr::select(year, country, trade_score, financial_score, labor_score,
                    infrastructure_score, sustainability_score, convergence_score)
  }

  yoy_changes <- calculate_yoy_changes(dimension_scores)

  list(
    dimension_scores = dimension_scores,
    gcc_ts = gcc_ts,
    yoy_changes = yoy_changes,
    country_data = country_data,
    indicator_detail = NULL
  )
}

# ==============================================================================
# SHARED HELPERS
# ==============================================================================

#' Calculate Year-over-Year Changes
#'
#' @param dimension_scores Data frame with dimension scores
#' @return Data frame with YoY changes
calculate_yoy_changes <- function(dimension_scores) {
  dimension_scores %>%
    dplyr::arrange(country, year) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(
      overall_change = overall_index - dplyr::lag(overall_index),
      trade_change = trade_score - dplyr::lag(trade_score),
      financial_change = financial_score - dplyr::lag(financial_score),
      labor_change = labor_score - dplyr::lag(labor_score),
      infrastructure_change = infrastructure_score - dplyr::lag(infrastructure_score),
      sustainability_change = sustainability_score - dplyr::lag(sustainability_score),
      convergence_change = convergence_score - dplyr::lag(convergence_score)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(overall_change)) %>%
    dplyr::select(year, country, dplyr::ends_with("_change"))
}

#' Log data summary and return result
#'
#' @param result List from any loader
#' @return Same list, after logging
log_and_return <- function(result) {
  message(paste("  - Countries:",
                dplyr::n_distinct(result$dimension_scores$country)))
  message(paste("  - Years:",
                paste(range(result$dimension_scores$year), collapse = "-")))
  if (!is.null(result$indicator_detail)) {
    message(paste("  - Indicators:",
                  dplyr::n_distinct(result$indicator_detail$indicator_code)))
  }
  return(result)
}
