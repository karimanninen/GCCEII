# ==============================================================================
# DATA LOADING MODULE
# ==============================================================================
# Handles all data import and transformation for the GCC EII Dashboard
# ==============================================================================

#' Load GCC Integration Data
#'
#' Loads data from workspace file or CSV files and returns a list of data frames
#'
#' @param output_dir Path to the output directory containing data files
#' @return List containing dimension_scores, gcc_ts, yoy_changes, and country_data
load_gcc_data <- function(output_dir = "output") {

  # Initialize result list
  result <- list(
    dimension_scores = NULL,
    gcc_ts = NULL,
    yoy_changes = NULL,
    country_data = NULL
  )

  # Option 1: Load from saved workspace (fastest)
  workspace_path <- file.path(output_dir, "gcc_integration_workspace.RData")
  csv_path <- file.path(output_dir, "time_series_complete.csv")

  if (file.exists(workspace_path)) {
    message("Loading data from workspace file...")

    # Load into temporary environment to avoid polluting global namespace
    temp_env <- new.env()
    load(workspace_path, envir = temp_env)

    # Extract time_series_complete from loaded workspace
    if (exists("time_series_complete", envir = temp_env)) {
      time_series_complete <- get("time_series_complete", envir = temp_env)
      result <- process_time_series_data(time_series_complete)
    } else if (exists("country_index_2023", envir = temp_env)) {
      # Fallback to country_index_2023 if time_series_complete doesn't exist
      country_index_2023 <- get("country_index_2023", envir = temp_env)
      gcc_aggregate_2023 <- get("gcc_aggregate_2023", envir = temp_env)
      result <- process_fallback_data(country_index_2023, gcc_aggregate_2023)
    }

    message("Data loaded from workspace")

  } else if (file.exists(csv_path)) {
    # Option 2: Load from CSV files
    message("Loading data from CSV files...")

    time_series_complete <- readr::read_csv(csv_path, show_col_types = FALSE)
    result <- process_time_series_data(time_series_complete)

    message("Data loaded from CSV files")

  } else {
    stop("ERROR: Could not find data files. Please ensure either:\n",
         "  1. gcc_integration_workspace.RData exists in output/ directory, OR\n",
         "  2. time_series_complete.csv exists in output/ directory\n\n",
         "Run the GCC_Integration_Master.R script first to generate these files.")
  }

  # Log summary
  message(paste("  - Countries:", dplyr::n_distinct(result$dimension_scores$country)))
  message(paste("  - Years:", paste(range(result$dimension_scores$year), collapse = "-")))

  return(result)
}

#' Process Time Series Data
#'
#' @param time_series_complete Data frame with complete time series
#' @return List of processed data frames
process_time_series_data <- function(time_series_complete) {

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

  # Rename overall_index to overall for consistency
  gcc_ts <- gcc_ts %>%
    dplyr::mutate(overall = overall_index) %>%
    dplyr::select(country, year, overall, trade_score, financial_score, labor_score,
                  infrastructure_score, sustainability_score, convergence_score,
                  integration_level)

  # Country vs GCC comparison (countries + GCC aggregate)
  if ("method" %in% names(time_series_complete)) {
    country_data <- time_series_complete %>%
      dplyr::filter(country != "GCC AGGREGATE" | (country == "GCC AGGREGATE" & method == "gdp")) %>%
      dplyr::select(year, country, trade_score, financial_score, labor_score,
                    infrastructure_score, sustainability_score, convergence_score)
  } else {
    country_data <- time_series_complete %>%
      dplyr::filter(country != "GCC AGGREGATE" | country == "GCC AGGREGATE") %>%
      dplyr::select(year, country, trade_score, financial_score, labor_score,
                    infrastructure_score, sustainability_score, convergence_score)
  }

  # Calculate year-over-year changes for COUNTRIES ONLY
  yoy_changes <- calculate_yoy_changes(dimension_scores)

  list(
    dimension_scores = dimension_scores,
    gcc_ts = gcc_ts,
    yoy_changes = yoy_changes,
    country_data = country_data
  )
}

#' Process Fallback Data (2023 only)
#'
#' @param country_index_2023 Country index data for 2023
#' @param gcc_aggregate_2023 GCC aggregate data for 2023
#' @return List of processed data frames
process_fallback_data <- function(country_index_2023, gcc_aggregate_2023) {

  dimension_scores <- country_index_2023 %>%
    dplyr::mutate(year = 2023) %>%
    dplyr::select(country, year, overall_index,
                  trade_score, financial_score, labor_score,
                  infrastructure_score, sustainability_score, convergence_score,
                  integration_level)

  gcc_ts <- gcc_aggregate_2023 %>%
    dplyr::filter(method == "gdp") %>%
    dplyr::mutate(overall = overall_index, year = 2023) %>%
    dplyr::select(country, year, overall, trade_score, financial_score, labor_score,
                  infrastructure_score, sustainability_score, convergence_score,
                  integration_level)

  country_data <- dimension_scores %>%
    dplyr::select(year, country, trade_score, financial_score, labor_score,
                  infrastructure_score, sustainability_score, convergence_score)

  yoy_changes <- calculate_yoy_changes(dimension_scores)

  list(
    dimension_scores = dimension_scores,
    gcc_ts = gcc_ts,
    yoy_changes = yoy_changes,
    country_data = country_data
  )
}

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
