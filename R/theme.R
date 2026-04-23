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
  # Deep institutional blue + GCC green. Kept aligned with the existing
  # landing-page identity so the new system does not break visual continuity.
  brand = list(
    primary       = "#003366",  # Deep navy — headings, primary buttons, nav
    primary_light = "#005082",  # Hovers, subtle emphasis
    primary_dark  = "#002244",  # Focus states, pressed states
    accent        = "#008035",  # GCC green — CTAs, confirm, positive actions
    accent_light  = "#009940",
    accent_dark   = "#006028"
  ),

  # --- 1.2 SURFACE ---------------------------------------------------------
  # Page background, card canvas, borders. Kept very light; charts and content
  # carry the color, the surface stays out of the way.
  surface = list(
    background    = "#f4f6f9",  # Page body
    canvas        = "#ffffff",  # Card / panel / chart background
    subtle        = "#f8f9fa",  # Subtle fill (striped rows, secondary cards)
    border        = "#e8edf4",  # Default card / divider border
    border_strong = "#d0d7e0"   # Focused inputs, emphasized dividers
  ),

  # --- 1.3 INK (TEXT) ------------------------------------------------------
  # Text colors with enough contrast for WCAG AA at body sizes.
  ink = list(
    primary   = "#1a1a1a",  # Body text on white / subtle surfaces
    secondary = "#444444",  # Supporting text, captions
    muted     = "#666666",  # Metadata, small labels, axis ticks
    inverse   = "#ffffff",  # Text on dark / brand-colored backgrounds
    brand     = "#003366"   # Section headings in brand color
  ),

  # --- 1.4 SEMANTIC (STATE & TREND) ----------------------------------------
  # Used consistently across EVERY chart and every card so that "green means
  # improved, red means declined" never gets inverted anywhere in the app.
  # Muted tones chosen deliberately — ministerial audiences read saturated
  # reds as alarms. Keep soft variants for background fills (e.g. a positive
  # delta pill has dark-green text on a soft-green background).
  semantic = list(
    positive      = "#2e7d32",  # Score improved YoY
    positive_soft = "#e8f5e9",
    negative      = "#c62828",  # Score declined YoY
    negative_soft = "#ffebee",
    neutral       = "#616161",  # No meaningful change
    neutral_soft  = "#f5f5f5",
    # Generic UI state (not tied to score direction)
    success       = "#2e7d32",
    warning       = "#b8860b",  # Goldenrod, not yellow — readable on white
    danger        = "#c62828",
    info          = "#005082"
  ),

  # --- 1.5 INTEGRATION-LEVEL BADGES ----------------------------------------
  # Qualitative labels shown under the headline gauge ("Weak / Moderate /
  # Good"). Desaturated variants replace the previous bright red/yellow/green
  # which read as alarm rather than as classification.
  level = list(
    weak     = "#c14953",  # Muted crimson
    moderate = "#d4a028",  # Muted amber (darker than yellow for readability)
    good     = "#5ba265"   # Muted sage green
  ),

  # --- 1.6 DIMENSION BASE PALETTE -----------------------------------------
  # Eight colors kept in a fixed order. Dimensions are assigned from this
  # palette at runtime via gcc_dimension_palette() — so GCCEII's six
  # integration dimensions and GCCEDI's (different) diversification
  # dimensions can share the base without collisions.
  # First six match the colors already used in the existing GCCEII CSS so
  # we preserve visual continuity for returning users.
  dimension_base = c(
    "#1e88e5",  # 1  blue         (GCCEII: Trade)
    "#f9a825",  # 2  amber        (GCCEII: Financial)
    "#43a047",  # 3  green        (GCCEII: Labor)
    "#e53935",  # 4  red          (GCCEII: Infrastructure)
    "#8e24aa",  # 5  purple       (GCCEII: Sustainability)
    "#00acc1",  # 6  teal         (GCCEII: Convergence)
    "#6d4c41",  # 7  brown        (reserved — GCCEDI / future)
    "#546e7a"   # 8  blue-gray    (reserved — GCCEDI / future)
  ),

  # --- 1.7 COUNTRY PALETTE -------------------------------------------------
  # Based on national flag colors. Kept intact from utils.R but hex values
  # normalized to lowercase and Oman darkened slightly for better contrast
  # on light backgrounds (was #a3a3a3 which is below AA against white text).
  country = c(
    "UAE"          = "#000000",  # Black
    "Qatar"        = "#99154c",  # Maroon
    "Saudi Arabia" = "#008035",  # Green
    "Bahrain"      = "#e20000",  # Red
    "Oman"         = "#7a7a7a",  # Dark gray (darkened from #a3a3a3 for AA)
    "Kuwait"       = "#009cd8",  # Light blue
    "GCC"          = "#b8860b"   # Goldenrod — aggregate / reference line
  ),

  # --- 1.8 TYPOGRAPHY ------------------------------------------------------
  # Inter pairs cleanly with Cairo/Tajawal: both are humanist sans-serifs
  # with similar x-height and stroke contrast. Tabular figures (tnum)
  # should be enabled for all KPI numerals — critical for scorecards.
  typography = list(
    font_latin  = "'Inter', system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif",
    font_arabic = "'Cairo', 'Tajawal', 'Noto Sans Arabic', Tahoma, Arial, sans-serif",
    font_mono   = "'JetBrains Mono', 'SF Mono', Consolas, monospace",

    # Type scale (pixels) — modular scale with ratio ~1.25
    size_micro   = 11,  # Chart axis ticks
    size_small   = 12,  # Captions, metadata
    size_body    = 14,  # Body copy, default
    size_lead    = 16,  # Lead paragraphs, card subtitles
    size_h4      = 18,
    size_h3      = 22,
    size_h2      = 28,
    size_h1      = 34,
    size_display = 48,  # Hero number on Executive Pulse tab

    # Weights
    weight_regular  = 400,
    weight_medium   = 500,
    weight_semibold = 600,
    weight_bold     = 700,

    # Line heights
    line_tight   = 1.2,   # Display / h1 / h2
    line_normal  = 1.5,   # Body (Latin)
    line_relaxed = 1.8    # Body (Arabic — needs more vertical breathing room)
  ),

  # --- 1.9 SPACING ---------------------------------------------------------
  # 4px base scale. Use these tokens instead of arbitrary pixel values so
  # rhythm stays consistent across cards, charts, and the landing page.
  spacing = list(
    xxs = 2, xs = 4, sm = 8, md = 12, lg = 16, xl = 24, xxl = 32, xxxl = 48
  ),

  # --- 1.10 RADIUS ---------------------------------------------------------
  radius = list(
    sm = 4, md = 8, lg = 12, xl = 16, pill = 999
  ),

  # --- 1.11 SHADOW ---------------------------------------------------------
  # Softer than the existing styles.css drops. Ministerial UIs should feel
  # calm, not floaty.
  shadow = list(
    sm = "0 1px 3px rgba(0,0,0,0.08)",
    md = "0 4px 12px rgba(0,0,0,0.10)",
    lg = "0 8px 24px rgba(0,0,0,0.14)",
    xl = "0 16px 40px rgba(0,0,0,0.18)"
  ),

  # --- 1.12 HEATMAP / DIVERGING SCALES -------------------------------------
  # Used by plotly colorscale arguments. Kept softer than the existing
  # viridis+RdYlGn mix to suit the brand.
  scale = list(
    # Sequential 0-100 score scale (low = warm muted, high = cool green)
    sequential = list(
      c(0.00, "#fdae61"),
      c(0.25, "#fee08b"),
      c(0.50, "#ffffbf"),
      c(0.75, "#a6d96a"),
      c(1.00, "#1a9850")
    ),
    # Diverging scale — anchor at 0 for "change" heatmaps
    diverging = list(
      c(0.0, "#c62828"),
      c(0.5, "#f5f5f5"),
      c(1.0, "#2e7d32")
    )
  ),

  # --- 1.13 CHART DEFAULTS -------------------------------------------------
  # Plotly layout defaults consumed by R/chart_defaults.R. Centralized here
  # so chart styling never has to be re-specified in individual viz code.
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
#'
#' Takes a character vector of dimension names (as they appear in your index)
#' and returns a named vector mapping each to a color drawn from the base
#' palette, in order. Works for GCCEII (six integration dimensions), GCCEDI
#' (whatever the diversification dimensions turn out to be), or any future
#' index as long as it has \le 8 dimensions.
#'
#' @param dimension_names character vector of dimension display names
#' @param theme gcc_theme list (defaults to the global)
#' @return named character vector: names = dimensions, values = hex colors
#' @examples
#' gcc_dimension_palette(c("Trade", "Financial", "Labor",
#'                         "Infrastructure", "Sustainability", "Convergence"))
#' @export
gcc_dimension_palette <- function(dimension_names, theme = gcc_theme) {
  n <- length(dimension_names)
  if (n == 0L) return(character(0))
  if (n > length(theme$dimension_base)) {
    stop(sprintf(
      "gcc_dimension_palette: %d dimensions requested, theme provides %d base colors. Extend gcc_theme$dimension_base before adding more dimensions.",
      n, length(theme$dimension_base)
    ))
  }
  setNames(theme$dimension_base[seq_len(n)], dimension_names)
}

#' GCCEII's six-dimension palette (convenience wrapper)
#' @export
gcceii_dimension_palette <- function(theme = gcc_theme) {
  gcc_dimension_palette(
    c("Trade", "Financial", "Labor",
      "Infrastructure", "Sustainability", "Convergence"),
    theme = theme
  )
}

#' Pick the right semantic color for a numeric delta
#'
#' @param delta numeric (positive = improvement, negative = decline)
#' @param threshold absolute value below which we treat the change as neutral
#' @param theme gcc_theme
#' @return single hex color string
#' @export
gcc_delta_color <- function(delta, threshold = 0.05, theme = gcc_theme) {
  if (is.na(delta))          return(theme$semantic$neutral)
  if (abs(delta) < threshold) return(theme$semantic$neutral)
  if (delta > 0)              return(theme$semantic$positive)
  theme$semantic$negative
}


# ------------------------------------------------------------------------------
# 3. CSS CUSTOM PROPERTY GENERATOR
# ------------------------------------------------------------------------------
# Writes www/design-tokens.css using the SAME values as the R theme so the
# two sides cannot drift. Call this whenever gcc_theme is edited; the
# output file is committed to the repo so production Shiny does not need
# to regenerate at startup.
#
# The emitted CSS exposes tokens as :root custom properties, e.g.:
#   --gcc-brand-primary: #003366;
#   --gcc-semantic-positive: #2e7d32;
#   --gcc-font-latin: 'Inter', ...;
# Consumers reference them as  color: var(--gcc-brand-primary);
# ------------------------------------------------------------------------------

#' Generate design-tokens.css from the R theme
#'
#' @param path target path for the CSS file
#' @param theme gcc_theme list
#' @return invisibly, the CSS string written
#' @export
write_design_tokens_css <- function(path = "www/design-tokens.css",
                                    theme = gcc_theme) {

  # ---- helpers ----
  css_var <- function(name, value) {
    sprintf("  --gcc-%s: %s;", name, value)
  }
  # Recursively flatten a nested list into dashed keys (brand$primary -> brand-primary)
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
        # Spacing / radius / typography sizes — emit as px
        out <- c(out, css_var(key, paste0(val, "px")))
      }
      # Lists of colors (country, dimension_base) and colorscales are
      # handled separately below — skipped here on purpose.
    }
    out
  }

  # ---- 1. scalar tokens (brand, surface, ink, semantic, level, spacing, radius, shadow, typography) ----
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

  # ---- 2. dimension base palette (emit as numbered slots) ----
  dim_lines <- sprintf("  --gcc-dim-%d: %s;",
                       seq_along(theme$dimension_base),
                       theme$dimension_base)
  # Plus convenience aliases for the GCCEII default mapping
  eii_dims <- c("trade", "financial", "labor", "infrastructure",
                "sustainability", "convergence")
  eii_lines <- sprintf("  --gcc-eii-%s: %s;",
                       eii_dims,
                       theme$dimension_base[seq_along(eii_dims)])

  # ---- 3. country palette ----
  country_key <- function(nm) tolower(gsub(" ", "-", nm))
  country_lines <- sprintf("  --gcc-country-%s: %s;",
                           country_key(names(theme$country)),
                           unname(theme$country))

  # ---- 4. assemble ----
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

  footer <- c(
    "}",
    "",
    "/* --- Utility classes consuming tokens ---------------------- */",
    ".gcc-text-positive { color: var(--gcc-semantic-positive); }",
    ".gcc-text-negative { color: var(--gcc-semantic-negative); }",
    ".gcc-text-neutral  { color: var(--gcc-semantic-neutral); }",
    ".gcc-bg-positive-soft { background: var(--gcc-semantic-positive-soft); }",
    ".gcc-bg-negative-soft { background: var(--gcc-semantic-negative-soft); }",
    ".gcc-font-latin  { font-family: var(--gcc-typography-font-latin); font-variant-numeric: tabular-nums; }",
    ".gcc-font-arabic { font-family: var(--gcc-typography-font-arabic); font-variant-numeric: tabular-nums; }",
    ""
  )

  css <- paste(c(header, body, footer), collapse = "\n")

  # Ensure parent dir exists
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(css, path)
  message(sprintf("Wrote %d lines of CSS tokens to %s",
                  length(strsplit(css, "\n")[[1]]), path))
  invisible(css)
}


# ------------------------------------------------------------------------------
# 4. PORTABILITY NOTE
# ------------------------------------------------------------------------------
# This file is marked @portable: it intentionally does not depend on any
# GCCEII-specific constant (dimension names, country list, data columns).
# To use it in GCCEDI:
#   1. copy R/theme.R, R/axis_helpers.R, R/chart_defaults.R into GCCEDI/R/
#   2. copy www/design-tokens.css into GCCEDI/www/
#   3. call gcc_dimension_palette() with GCCEDI's own dimension names
# Any GCCEII-specific helper (e.g. the gcceii_dimension_palette() wrapper)
# can be left behind or replaced with a GCCEDI equivalent.
# ==============================================================================
