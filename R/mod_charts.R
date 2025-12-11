# ==============================================================================
# CHART HELPERS MODULE
# ==============================================================================
# Reusable chart creation functions for the GCC EII Dashboard
# ==============================================================================

#' Create Gauge Chart for Overall Index
#'
#' @param current_value Current overall index value
#' @param reference_value Previous year's value for delta
#' @param title Chart title
#' @return plotly object
create_gauge_chart <- function(current_value, reference_value, title) {
  plotly::plot_ly(
    type = "indicator",
    mode = "gauge+number+delta",
    value = current_value,
    delta = list(reference = reference_value),
    gauge = list(
      axis = list(range = list(0, 100)),
      bar = list(color = "#003366"),
      steps = list(
        list(range = c(0, 40), color = "#ffcccc"),
        list(range = c(40, 60), color = "#ffffcc"),
        list(range = c(60, 100), color = "#ccffcc")
      ),
      threshold = list(
        line = list(color = "red", width = 4),
        thickness = 0.75,
        value = 60
      )
    ),
    title = list(text = title)
  ) %>%
    plotly::layout(margin = list(t = 50, b = 20))
}

#' Create Horizontal Bar Chart for Dimensions
#'
#' @param dim_data Data frame with dimension and score columns
#' @param bar_color Color for bars (default #003366)
#' @param y_range Y-axis range (default c(0, 100))
#' @return plotly object
create_dimension_bar_chart <- function(dim_data, bar_color = "#003366", y_range = c(0, 100)) {
  plotly::plot_ly(
    dim_data,
    x = ~dimension,
    y = ~score,
    type = 'bar',
    marker = list(color = bar_color)
  ) %>%
    plotly::layout(
      xaxis = list(title = "", categoryorder = "array", categoryarray = DIMENSION_LABELS),
      yaxis = list(title = "Score", range = y_range),
      margin = list(b = 100)
    )
}

#' Create Line Chart for Time Series
#'
#' @param data Data frame with year and value columns
#' @param x_col Column name for x-axis (typically "year")
#' @param y_col Column name for y-axis
#' @param line_color Line color
#' @param line_width Line width
#' @param marker_size Marker size
#' @param y_range Y-axis range
#' @return plotly object
create_line_chart <- function(data, x_col, y_col, line_color = "#003366",
                               line_width = 3, marker_size = 10, y_range = c(0, 100)) {
  plotly::plot_ly(
    data,
    x = as.formula(paste0("~", x_col)),
    y = as.formula(paste0("~", y_col)),
    type = 'scatter',
    mode = 'lines+markers',
    line = list(color = line_color, width = line_width),
    marker = list(size = marker_size)
  ) %>%
    plotly::layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = "Score", range = y_range),
      hovermode = 'x unified'
    )
}

#' Create Multi-Line Chart for Multiple Countries/Dimensions
#'
#' @param data Data frame
#' @param x_col X-axis column
#' @param y_col Y-axis column
#' @param color_col Column to use for coloring
#' @param colors Named vector of colors
#' @param y_range Y-axis range
#' @return plotly object
create_multi_line_chart <- function(data, x_col, y_col, color_col, colors, y_range = c(0, 100)) {
  plotly::plot_ly(
    data,
    x = as.formula(paste0("~", x_col)),
    y = as.formula(paste0("~", y_col)),
    color = as.formula(paste0("~", color_col)),
    colors = colors,
    type = 'scatter',
    mode = 'lines+markers',
    marker = list(size = 8),
    line = list(width = 2)
  ) %>%
    plotly::layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = "Score", range = y_range),
      hovermode = 'x unified',
      legend = list(orientation = 'h', y = -0.15)
    )
}

#' Create Radar Chart for Dimension Profile
#'
#' @param scores Numeric vector of 6 dimension scores
#' @param fill_color Fill color with alpha
#' @param line_color Line color
#' @return plotly object
create_radar_chart <- function(scores, fill_color = 'rgba(0, 51, 102, 0.3)',
                                line_color = '#003366') {
  # Close the polygon by repeating first value
  radar_scores <- c(scores, scores[1])
  radar_labels <- c(DIMENSION_LABELS, DIMENSION_LABELS[1])

  plotly::plot_ly(
    type = 'scatterpolar',
    r = radar_scores,
    theta = radar_labels,
    fill = 'toself',
    fillcolor = fill_color,
    line = list(color = line_color)
  ) %>%
    plotly::layout(
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0, 100))
      ),
      showlegend = FALSE
    )
}

#' Create Heatmap Chart
#'
#' @param matrix_data Matrix with values
#' @param colorscale Color scale list
#' @param zmin Minimum z value
#' @param zmax Maximum z value
#' @param zmid Middle z value (for diverging scales)
#' @return plotly object
create_heatmap <- function(matrix_data, colorscale = NULL, zmin = 0, zmax = 100, zmid = NULL) {
  if (is.null(colorscale)) {
    colorscale <- HEATMAP_COLORSCALE
  }

  p <- plotly::plot_ly(
    x = colnames(matrix_data),
    y = rownames(matrix_data),
    z = matrix_data,
    type = "heatmap",
    colorscale = colorscale,
    zmin = zmin,
    zmax = zmax,
    text = round(matrix_data, 1),
    hovertemplate = '%{y}<br>%{x}: %{z:.1f}<extra></extra>'
  )

  if (!is.null(zmid)) {
    p <- p %>% plotly::layout(coloraxis = list(cmid = zmid))
  }

  p %>% plotly::layout(
    xaxis = list(title = "Dimension"),
    yaxis = list(title = "Country"),
    margin = list(l = 100)
  )
}

#' Create Country Ranking Bar Chart
#'
#' @param ranking_data Data frame with country and overall_index columns
#' @param colors Named color vector
#' @param title Chart title
#' @return plotly object
create_ranking_chart <- function(ranking_data, colors = COUNTRY_COLORS, title = NULL) {
  plotly::plot_ly(
    ranking_data,
    x = ~reorder(country, overall_index),
    y = ~overall_index,
    type = 'bar',
    marker = list(color = ~colors[country])
  ) %>%
    plotly::layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Overall Index", range = c(0, 100)),
      title = title
    )
}

#' Create Stacked Bar Chart for Contribution Analysis
#'
#' @param data Data frame with dimension, contribution, and country columns
#' @param colors Named color vector
#' @return plotly object
create_contribution_chart <- function(data, colors = COUNTRY_COLORS) {
  data$dimension <- factor(data$dimension, levels = DIMENSION_LABELS)

  plotly::plot_ly(
    data,
    x = ~dimension,
    y = ~contribution,
    color = ~country,
    colors = colors,
    type = 'bar'
  ) %>%
    plotly::layout(
      xaxis = list(title = "Dimension", categoryorder = "array", categoryarray = DIMENSION_LABELS),
      yaxis = list(title = "Contribution to GCC Index (weighted)"),
      barmode = 'stack',
      legend = list(orientation = 'h', y = -0.2)
    )
}

#' Create Scatter Plot for YoY Changes
#'
#' @param data Data frame with dimension_label, change, and country columns
#' @param colors Named color vector
#' @return plotly object
create_yoy_scatter <- function(data, colors = COUNTRY_COLORS) {
  plotly::plot_ly(
    data,
    x = ~dimension_label,
    y = ~change,
    color = ~country,
    colors = colors,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 14)
  ) %>%
    plotly::layout(
      xaxis = list(title = "Dimension", categoryorder = "array", categoryarray = DIMENSION_LABELS),
      yaxis = list(title = "Year-over-Year Change"),
      hovermode = 'closest',
      legend = list(orientation = 'h', y = -0.2),
      shapes = list(
        list(type = "line", x0 = -0.5, x1 = 5.5, y0 = 0, y1 = 0,
             line = list(color = "black", dash = "dash"))
      )
    )
}
