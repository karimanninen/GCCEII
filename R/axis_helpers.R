# ==============================================================================
# AXIS AUTO-SCALING HELPERS
# ==============================================================================
# @portable:    yes
# @phase:       1 (foundation)
# @shared-with: GCCEDI
# @depends-on:  R/theme.R (for semantic colors in format_delta())
# ==============================================================================
#
# Replaces the hardcoded `y_range = c(0, 100)` pattern found throughout
# mod_charts.R. The problem: when the GCC integration score moves from 47.4
# to 47.4 (e.g. 2023 -> 2024), a 0-100 y-axis makes the change invisible.
# The fix: axes should pad the observed range by a configurable amount so
# movement is visible, with optional anchoring when the zero or a reference
# line is meaningful.
#
# Rules of thumb used below:
#   * Score axes (0-100) — use observed range + 5-10% padding, not [0,100]
#   * Change axes (delta scores) — symmetric around 0 so sign is readable
#   * Indicator axes (raw units) — observed range + 10% padding, anchor at 0
#     only if the variable is definitionally non-negative
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. CORE RANGE CALCULATOR
# ------------------------------------------------------------------------------

#' Compute a padded axis range from observed values
#'
#' @param values numeric vector of observations to cover
#' @param pad fraction of observed range to add on each side (default 0.08 = 8%)
#' @param include optional numeric vector of values that MUST lie inside the
#'   returned range (e.g. the GCC average reference line)
#' @param anchor_zero if TRUE, snap the lower bound to 0 when observations
#'   are all non-negative (useful for proportions / counts but NOT for scores
#'   that happen to stay above 0 by coincidence)
#' @param min_span minimum span the range must cover; prevents axes collapsing
#'   to a hair's-breadth when all values are nearly identical
#' @param nice_round if TRUE, round endpoints to multiples of a "nice" step
#'   so tick labels stay tidy
#' @return length-2 numeric vector c(lower, upper)
#' @export
#'
#' @examples
#' autoscale_range(c(45.2, 47.1, 47.4, 47.4))
#' # -> c(44.94, 47.66) approximately — the flat YoY change is now visible
#'
#' autoscale_range(c(-2.3, 1.1, 0.5), include = 0)
#' # -> symmetric-ish around 0 because 0 is in the 'include' set
autoscale_range <- function(values,
                            pad         = 0.08,
                            include     = NULL,
                            anchor_zero = FALSE,
                            min_span    = 1.0,
                            nice_round  = TRUE) {

  values <- c(values, include)
  values <- values[is.finite(values)]
  if (length(values) == 0L) return(c(0, 1))

  lo <- min(values)
  hi <- max(values)
  span <- hi - lo

  # Guard against collapsed ranges (all values ~equal)
  if (span < min_span) {
    mid  <- (hi + lo) / 2
    half <- min_span / 2
    lo <- mid - half
    hi <- mid + half
    span <- hi - lo
  }

  padding <- span * pad
  lo_out <- lo - padding
  hi_out <- hi + padding

  if (anchor_zero && lo >= 0) {
    lo_out <- 0
  }

  if (nice_round) {
    step <- nice_step(span)
    lo_out <- floor(lo_out / step) * step
    hi_out <- ceiling(hi_out / step) * step
  }

  c(lo_out, hi_out)
}


#' Compute a symmetric range centered on zero
#'
#' Used for change / delta axes so that a +2 and a -2 move appear equal in
#' magnitude. Always includes zero.
#'
#' @param values numeric vector
#' @param pad fraction padding on each side
#' @param min_half_span minimum half-range (defaults to 1.0)
#' @param nice_round round endpoints to nice numbers
#' @return c(-h, h) where h is the padded max absolute value
#' @export
autoscale_symmetric <- function(values, pad = 0.10,
                                min_half_span = 1.0,
                                nice_round = TRUE) {
  values <- values[is.finite(values)]
  if (length(values) == 0L) return(c(-1, 1))

  h <- max(abs(values))
  if (h < min_half_span) h <- min_half_span
  h <- h * (1 + pad)

  if (nice_round) {
    step <- nice_step(h * 2)
    h <- ceiling(h / step) * step
  }

  c(-h, h)
}


# ------------------------------------------------------------------------------
# 2. PLOTLY WRAPPERS
# ------------------------------------------------------------------------------

#' Apply an auto-scaled y-axis to a plotly chart
#'
#' @param p a plotly object
#' @param values the data series the axis covers
#' @param ... extra args forwarded to autoscale_range()
#' @return the plotly object with yaxis.range updated
#' @export
autoscale_plotly_y <- function(p, values, ...) {
  rng <- autoscale_range(values, ...)
  plotly::layout(p, yaxis = list(range = rng))
}

#' Apply an auto-scaled symmetric y-axis (for change / delta charts)
#' @export
autoscale_plotly_y_symmetric <- function(p, values, ...) {
  rng <- autoscale_symmetric(values, ...)
  plotly::layout(p, yaxis = list(range = rng, zeroline = TRUE))
}


# ------------------------------------------------------------------------------
# 3. DELTA FORMATTING
# ------------------------------------------------------------------------------

#' Format a score change as a human-readable string with sign
#'
#' Produces strings like "+2.3 pts", "-0.8 pts", "no change" suitable for
#' KPI card deltas. Uses the rounding rule that if abs(delta) rounds to 0
#' at the requested digits, we explicitly say "no change" rather than
#' showing a misleading "+0.0".
#'
#' @param delta numeric (single value)
#' @param digits rounding digits (default 1)
#' @param unit suffix (default "pts"; pass "" for unit-less)
#' @param no_change_text string returned when the delta rounds to 0
#' @return a length-1 character string
#' @export
#'
#' @examples
#' format_delta(2.34)        # "+2.3 pts"
#' format_delta(-0.84)       # "-0.8 pts"
#' format_delta(0.02)        # "no change"
#' format_delta(NA)          # "—"
format_delta <- function(delta,
                         digits         = 1,
                         unit           = "pts",
                         no_change_text = "no change") {
  if (is.na(delta) || !is.finite(delta)) return("\u2014")  # em dash
  rounded <- round(delta, digits)
  if (rounded == 0) return(no_change_text)
  sign_char <- if (rounded > 0) "+" else ""
  suffix <- if (nzchar(unit)) paste0(" ", unit) else ""
  sprintf("%s%.*f%s", sign_char, digits, rounded, suffix)
}


#' Return the semantic color matching a delta's sign
#'
#' Wrapper around gcc_delta_color() (defined in R/theme.R) so calling code
#' can do `style = sprintf("color: %s", delta_color(d))` without thinking
#' about which palette key to use.
#'
#' @param delta numeric
#' @param threshold absolute value below which we treat as neutral
#' @param theme gcc_theme
#' @return hex color string
#' @export
delta_color <- function(delta, threshold = 0.05, theme = gcc_theme) {
  # theme must already be loaded via source("R/theme.R")
  if (!exists("gcc_delta_color", mode = "function")) {
    stop("delta_color(): R/theme.R must be sourced before using this helper.")
  }
  gcc_delta_color(delta, threshold = threshold, theme = theme)
}


# ------------------------------------------------------------------------------
# 4. INTERNAL: NICE-NUMBER ROUNDING
# ------------------------------------------------------------------------------
# Snaps axis endpoints to multiples of a "nice" step (1, 2, 2.5, 5, 10, ...).
# Keeps tick labels tidy without the caller picking step sizes by hand.

nice_step <- function(span, target_ticks = 5) {
  if (!is.finite(span) || span <= 0) return(1)
  raw_step <- span / target_ticks
  magnitude <- 10 ^ floor(log10(raw_step))
  residual  <- raw_step / magnitude
  nice <- if      (residual < 1.5)  1
          else if (residual < 3)    2
          else if (residual < 7)    5
          else                      10
  nice * magnitude
}
