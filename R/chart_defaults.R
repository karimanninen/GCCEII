# ==============================================================================
# PLOTLY CHART DEFAULTS
# ==============================================================================
# @portable:    yes
# @phase:       1 (foundation)
# @shared-with: GCCEDI
# @depends-on:  R/theme.R, R/axis_helpers.R
# ==============================================================================
#
# apply_gcc_theme(p) stamps our design-system defaults onto any plotly
# object: fonts, gridlines, margins, hover styling, legend placement.
# Every chart in the app should end with `%>% apply_gcc_theme()` so style
# drift becomes impossible.
#
# For bilingual rendering, pass lang = "ar" and the function substitutes the
# Arabic font stack and RTL legend/axis orientation.
# ==============================================================================


#' Apply GCC-Stat design-system defaults to a plotly chart
#'
#' @param p a plotly object
#' @param lang "en" or "ar" — switches font stack and RTL details
#' @param show_legend TRUE / FALSE / "auto" (auto = show only if > 1 trace)
#' @param legend_position "bottom" (default) / "top" / "right"
#' @param theme gcc_theme list
#' @return the plotly object with layout defaults applied
#' @export
apply_gcc_theme <- function(p,
                            lang            = "en",
                            show_legend     = "auto",
                            legend_position = "bottom",
                            theme           = gcc_theme) {

  if (!inherits(p, c("plotly", "htmlwidget"))) {
    stop("apply_gcc_theme(): expected a plotly object, got ", class(p)[1])
  }

  # --- font family ------------------------------------------------------
  font_family <- if (identical(lang, "ar")) {
    theme$typography$font_arabic
  } else {
    theme$typography$font_latin
  }

  # --- legend placement ------------------------------------------------
  legend_cfg <- switch(
    legend_position,
    bottom = list(orientation = "h", y = -0.20, x = 0.5, xanchor = "center",
                  bgcolor = "rgba(0,0,0,0)"),
    top    = list(orientation = "h", y = 1.10,  x = 0.5, xanchor = "center",
                  bgcolor = "rgba(0,0,0,0)"),
    right  = list(orientation = "v", y = 1.0,   x = 1.02, xanchor = "left",
                  bgcolor = "rgba(0,0,0,0)"),
    list(orientation = "h", y = -0.20)
  )

  # --- resolve showlegend ----------------------------------------------
  legend_show <- if (identical(show_legend, "auto")) {
    # let plotly decide based on trace count — leave unset
    NULL
  } else {
    isTRUE(show_legend)
  }

  # --- axis styling (applied to all x/y axes) --------------------------
  axis_default <- list(
    gridcolor    = theme$chart$grid_color,
    zerolinecolor = theme$chart$zeroline_color,
    linecolor    = theme$chart$axis_color,
    tickcolor    = theme$chart$tick_color,
    tickfont     = list(family = font_family,
                        size   = theme$typography$size_small,
                        color  = theme$ink$muted),
    titlefont    = list(family = font_family,
                        size   = theme$typography$size_body,
                        color  = theme$ink$secondary),
    showline     = FALSE,
    showgrid     = TRUE,
    zeroline     = FALSE
  )

  # --- assemble layout --------------------------------------------------
  layout_args <- list(
    font       = list(family = font_family,
                      size   = theme$typography$size_body,
                      color  = theme$ink$primary),
    paper_bgcolor = theme$chart$paper_bg,
    plot_bgcolor  = theme$chart$plot_bg,
    margin     = theme$chart$margin,
    hoverlabel = list(bgcolor     = theme$chart$hover_bg,
                      bordercolor = theme$chart$hover_border,
                      font = list(family = font_family,
                                  size = theme$typography$size_small,
                                  color = theme$ink$primary)),
    legend     = legend_cfg,
    xaxis      = axis_default,
    yaxis      = axis_default
  )
  if (!is.null(legend_show)) layout_args$showlegend <- legend_show

  # Call plotly::layout with our assembled args
  do.call(plotly::layout, c(list(p), layout_args))
}


#' Apply a consistent hover template
#'
#' Use when you want every chart to say "Dimension: Trade\\nScore: 47.4"
#' rather than letting plotly pick a format. Saves per-chart boilerplate.
#'
#' @param p a plotly object
#' @param template hover template string (plotly %{...} syntax)
#' @return the plotly object with hovertemplate applied to all traces
#' @export
apply_gcc_hover <- function(p, template) {
  plotly::style(p, hovertemplate = template)
}


#' Add a subtle annotation for the data source
#'
#' Small gray text at the bottom-right of a chart — useful on export.
#' Avoids cluttering the main chart but ensures provenance travels with
#' any screenshot.
#'
#' @param p a plotly object
#' @param text the annotation string (e.g. "Source: COINr, GCC-Stat")
#' @param theme gcc_theme
#' @return plotly with annotation added
#' @export
add_source_annotation <- function(p, text, theme = gcc_theme) {
  plotly::layout(p,
    annotations = list(
      list(
        text      = text,
        showarrow = FALSE,
        xref      = "paper", yref = "paper",
        x = 1.0, y = -0.28,
        xanchor = "right", yanchor = "top",
        font = list(
          size   = theme$typography$size_micro,
          color  = theme$ink$muted,
          family = theme$typography$font_latin
        )
      )
    )
  )
}
