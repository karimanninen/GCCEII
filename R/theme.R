# ==============================================================================
# GCC DASHBOARD DESIGN SYSTEM — THEME REGISTRY
# ==============================================================================
# @portable:      yes
# @phase:         1 (foundation)
# @shared-with:   GCCEDI (drop-in; no GCCEII-specific coupling)
# @generated-by:  none (hand-authored source of truth)
# @generates:     www/design-tokens.css (via write_design_tokens_css())
# ==============================================================================
#
# SINGLE SOURCE OF TRUTH for colors, typography, spacing, and chart defaults
# used across all GCC-Stat composite-index dashboards (GCCEII, GCCEDI, future).
#
# How it works:
#   * R code reads tokens directly from `gcc_theme`
#   * CSS reads the SAME values via :root custom properties written by
#     write_design_tokens_css() into www/design-tokens.css
#   * Because both sides are generated from one definition, they cannot drift
#
# Design principles for the ministerial audience:
#   * Institutional colors (deep blue, GCC green) preserved from existing brand
#   * Muted state colors — no garish reds/yellows that signal alarm incorrectly
#   * Dimension colors indexed by name, parameterizable (GCCEDI has a different
#     set of dimensions than GCCEII — don't hard-code the six EII dimensions)
#   * Typography pairs Inter (Latin) with Cairo/Tajawal (Arabic) for bilingual
#     consistency; numerals render identically in both scripts
#
# Usage:
#   source("R/theme.R")
#   gcc_theme$brand$primary                           # access a token in R
#   gcc_dimension_palette(c("Trade", "Labor", "..."))  # map dims to colors
#   write_design_tokens_css()                         # regenerate CSS side
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. THEME REGISTRY
# ------------------------------------------------------------------------------

gcc_theme <- list(

  # --- 1.1 BRAND -----------------------------------------------------------
  brand = list(
    primary       = "#003366",
    primary_light = "#005082",
    primary_dark  = "#002244",
    accent        = "#008035",
    accent_light  = "#009940",
    accent_dark   = "#006028"
  ),

  # --- 1.2 SURFACE ---------------------------------------------------------
  surface = list(
    background    = "#f4f6f9",
    canvas        = "#ffffff",
    subtle        = "#f8f9fa",
    border        = "#e8edf4",
    border_strong = "#d0d7e0"
  ),

  # --- 1.3 INK (TEXT) ------------------------------------------------------
  ink = list(
    primary   = "#1a1a1a",
    secondary = "#444444",
    muted     = "#666666",
    inverse   = "#ffffff",
    brand     = "#003366"
  ),

  # --- 1.4 SEMANTIC (STATE & TREND) ----------------------------------------
  semantic = list(
    positive      = "#2e7d32",
    positive_soft = "#e8f5e9",
    negative      = "#c62828",
    negative_soft = "#ffebee",
    neutral       = "#616161",
    neutral_soft  = "#f5f5f5",
    success       = "#2e7d32",
    warning       = "#b8860b",
    danger        = "#c62828",
    info          = "#005082"
  ),

  # --- 1.5 INTEGRATION-LEVEL BADGES ----------------------------------------
  level = list(
    weak     = "#c14953",
    moderate = "#d4a028",
    good     = "#5ba265"
  ),

  # --- 1.6 DIMENSION BASE PALETTE -----------------------------------------
  dimension_base = c(
    "#1e88e5",
    "#f9a825",
    "#43a047",
    "#e53935",
    "#8e24aa",
    "#00acc1",
    "#6d4c41",
    "#546e7a"
  ),

  # --- 1.7 COUNTRY PALETTE -------------------------------------------------
  country = c(
    "UAE"          = "#000000",
    "Qatar"        = "#99154c",
    "Saudi Arabia" = "#008035",
    "Bahrain"      = "#e20000",
    "Oman"         = "#7a7a7a",
    "Kuwait"       = "#009cd8",
    "GCC"          = "#b8860b"
  ),

  # --- 1.8 TYPOGRAPHY ------------------------------------------------------
  typography = list(
    font_latin  = "'Inter', system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
    font_arabic = "'Cairo', 'Tajawal', 'Noto Sans Arabic', Tahoma, Arial, sans-serif",
    font_mono   = "'JetBrains Mono', 'SF Mono', Consolas, monospace",

    size_micro   = 11,
    size_small   = 12,
    size_body    = 14,
    size_lead    = 16,
    size_h4      = 18,
    size_h3      = 22,
    size_h2      = 28,
    size_h1      = 34,
    size_display = 48,

    # Unitless in CSS — must NOT get "px" suffix in the generator
    weight_regular  = 400,
    weight_medium   = 500,
    weight_semibold = 600,
    weight_bold     = 700,

    # Unitless in CSS — must NOT get "px" suffix in the generator
    line_tight   = 1.2,
    line_normal  = 1.5,
    line_relaxed = 1.8
  ),

  # --- 1.9 SPACING ---------------------------------------------------------
  spacing = list(
    xxs = 2, xs = 4, sm = 8, md = 12, lg = 16, xl = 24, xxl = 32, xxxl = 48
  ),

  # --- 1.10 RADIUS ---------------------------------------------------------
  radius = list(
    sm = 4, md = 8, lg = 12, xl = 16, pill = 999
  ),

  # --- 1.11 SHADOW ---------------------------------------------------------
  shadow = list(
    sm = "0 1px 3px rgba(0,0,0,0.08)",
    md = "0 4px 12px rgba(0,0,0,0.10)",
    lg = "0 8px 24px rgba(0,0,0,0.14)",
    xl = "0 16px 40px rgba(0,0,0,0.18)"
  ),

  # --- 1.12 HEATMAP / DIVERGING SCALES -------------------------------------
  scale = list(
    sequential = list(
      c(0.00, "#fdae61"),
      c(0.25, "#fee08b"),
      c(0.50, "#ffffbf"),
      c(0.75, "#a6d96a"),
      c(1.00, "#1a9850")
    ),
    diverging = list(
      c(0.0, "#c62828"),
      c(0.5, "#f5f5f5"),
      c(1.0, "#2e7d32")
    )
  ),

  # --- 1.13 CHART DEFAULTS -------------------------------------------------
  chart = list(
    margin        = list(t = 40, r = 20, b = 40, l = 50),
    grid_color    = "#e8edf4",
    zeroline_color = "#d0d7e0",
    axis_color    = "#666666",
    tick_color    = "#666666",
    hover_bg      = "rgba(255,255,255,0.95)",
    hover_border  = "#d0d7e0",
    plot_bg       = "#ffffff",
    paper_bg      = "#ffffff"
  )
)


# ------------------------------------------------------------------------------
# 2. PALETTE ACCESSORS
# ------------------------------------------------------------------------------

#' Assign a color to each dimension
#' @export
gcc_dimension_palette <- function(dimension_names, theme = gcc_theme) {
  n <- length(dimension_names)
  if (n == 0L) return(character(0))
  if (n > length(theme$dimension_base)) {
    stop(sprintf(
      "gcc_dimension_palette: %d dimensions requested, theme provides %d base colors.",
      n, length(theme$dimension_base)
    ))
  }
  setNames(theme$dimension_base[seq_len(n)], dimension_names)
}

#' GCCEII six-dimension palette (convenience wrapper)
#' @export
gcceii_dimension_palette <- function(theme = gcc_theme) {
  gcc_dimension_palette(
    c("Trade", "Financial", "Labor",
      "Infrastructure", "Sustainability", "Convergence"),
    theme = theme
  )
}

#' Pick the right semantic color for a numeric delta
#' @export
gcc_delta_color <- function(delta, threshold = 0.05, theme = gcc_theme) {
  if (is.na(delta))           return(theme$semantic$neutral)
  if (abs(delta) < threshold) return(theme$semantic$neutral)
  if (delta > 0)              return(theme$semantic$positive)
  theme$semantic$negative
}


# ------------------------------------------------------------------------------
# 3. CSS CUSTOM PROPERTY GENERATOR
# ------------------------------------------------------------------------------

#' Generate design-tokens.css from the R theme
#'
#' @param path target path for the CSS file
#' @param theme gcc_theme list
#' @return invisibly, the CSS string written
#' @export
write_design_tokens_css <- function(path = "www/design-tokens.css",
                                    theme = gcc_theme) {

  css_var <- function(name, value) {
    sprintf("  --gcc-%s: %s;", name, value)
  }

  # Recursively flatten a nested list into dashed keys.
  # weight_* and line_* typography tokens are unitless in CSS and must
  # NOT receive a "px" suffix; everything else numeric gets "px".
  flatten_tokens <- function(x, prefix = "") {
    out <- character(0)
    for (nm in names(x)) {
      key <- if (nzchar(prefix)) paste(prefix, nm, sep = "-") else nm
      val <- x[[nm]]
      if (is.list(val) && !is.null(names(val))) {
        out <- c(out, flatten_tokens(val, key))
      } else if (is.character(val) && length(val) == 1L) {
        out <- c(out, css_var(key, val))
      } else if (is.numeric(val) && length(val) == 1L) {
        unitless <- grepl("(weight|line)_", nm)
        val_str  <- if (unitless) as.character(val) else paste0(val, "px")
        out <- c(out, css_var(key, val_str))
      }
    }
    out
  }

  scalars <- list(
    brand      = theme$brand,
    surface    = theme$surface,
    ink        = theme$ink,
    semantic   = theme$semantic,
    level      = theme$level,
    spacing    = theme$spacing,
    radius     = theme$radius,
    shadow     = theme$shadow,
    typography = theme$typography
  )
  scalar_lines <- flatten_tokens(scalars)

  dim_lines <- sprintf("  --gcc-dim-%d: %s;",
                       seq_along(theme$dimension_base),
                       theme$dimension_base)

  eii_dims  <- c("trade", "financial", "labor", "infrastructure",
                 "sustainability", "convergence")
  eii_lines <- sprintf("  --gcc-eii-%s: %s;",
                       eii_dims,
                       theme$dimension_base[seq_along(eii_dims)])

  country_key   <- function(nm) tolower(gsub(" ", "-", nm))
  country_lines <- sprintf("  --gcc-country-%s: %s;",
                           country_key(names(theme$country)),
                           unname(theme$country))

  header <- c(
    "/* =============================================================",
    "   design-tokens.css",
    sprintf("   Generated from R/theme.R at %s",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "   DO NOT EDIT BY HAND — edit R/theme.R and regenerate via",
    "       source('R/theme.R'); write_design_tokens_css()",
    "   @portable: yes",
    "   ============================================================= */",
    "",
    ":root {",
    "  /* --- Brand / surface / ink / semantic / level ----------- */"
  )

  body <- c(
    scalar_lines,
    "",
    "  /* --- Dimension base palette (index-agnostic slots) ------ */",
    dim_lines,
    "",
    "  /* --- GCCEII dimension aliases (convenience) ------------- */",
    eii_lines,
    "",
    "  /* --- Country palette ------------------------------------ */",
    country_lines
  )

  # Footer utility classes use underscore var names to match what the
  # generator emits (e.g. --gcc-semantic-positive_soft, not -positive-soft).
  footer <- c(
    "}",
    "",
    "/* --- Utility classes consuming tokens ---------------------- */",
    ".gcc-text-positive { color: var(--gcc-semantic-positive); }",
    ".gcc-text-negative { color: var(--gcc-semantic-negative); }",
    ".gcc-text-neutral  { color: var(--gcc-semantic-neutral); }",
    ".gcc-bg-positive-soft { background: var(--gcc-semantic-positive_soft); }",
    ".gcc-bg-negative-soft { background: var(--gcc-semantic-negative_soft); }",
    ".gcc-font-latin  { font-family: var(--gcc-typography-font_latin); font-variant-numeric: tabular-nums; }",
    ".gcc-font-arabic { font-family: var(--gcc-typography-font_arabic); font-variant-numeric: tabular-nums; }",
    ""
  )

  css <- paste(c(header, body, footer), collapse = "\n")

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(css, path)
  message(sprintf("Wrote %d lines of CSS tokens to %s",
                  length(strsplit(css, "\n")[[1]]), path))
  invisible(css)
}


# ------------------------------------------------------------------------------
# 4. PORTABILITY NOTE
# ------------------------------------------------------------------------------
# This file is marked @portable: it does not depend on any GCCEII-specific
# constant. To use it in GCCEDI:
#   1. copy R/theme.R, R/axis_helpers.R, R/chart_defaults.R into GCCEDI/R/
#   2. copy www/design-tokens.css into GCCEDI/www/
#   3. call gcc_dimension_palette() with GCCEDI's own dimension names
# ==============================================================================
