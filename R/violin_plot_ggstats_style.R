#' Create a stylized violin plot with jittered points and boxplots
#'
#' This function creates a violin plot overlaid with jittered data points and a boxplot,
#' using a consistent visual style suitable for statistical reporting. It also supports
#' custom axis labels, titles, captions, and color palettes.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param x A categorical variable (unquoted) to be used on the x-axis.
#' @param y A numeric variable (unquoted) to be used on the y-axis.
#' @param xlab Optional custom label for the x-axis. Defaults to the variable name.
#' @param ylab Optional custom label for the y-axis. Defaults to the variable name.
#' @param caption Optional caption to be displayed at the bottom of the plot.
#' @param title Optional main title of the plot.
#' @param subtitle Optional subtitle of the plot.
#' @param point.args A list of arguments passed to `geom_point()` to control jittered point appearance.
#' @param boxplot.args A list of arguments passed to `geom_boxplot()` to control boxplot appearance.
#' @param violin.args A list of arguments passed to `geom_violin()` to control violin plot appearance.
#' @param color_values Optional named vector of colors used to manually set fill and point colors
#'   (e.g., `c("Group1" = "blue", "Group2" = "red")`).
#'
#' @return A `ggplot` object representing the combined violin-box-jitter plot.
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'   violin_plot_ggstats_style(antibody_dataset, y = `HLA PRS 4 SNP`, x = Subgroup,
#'                              xlab = "HLA PRS", ylab = "Subgroups",
#'                              title = "Distribution of HLA PRA across subgroups",
#'                              color_values = acr_subgroups_paper_colors())
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_boxplot geom_violin labs scale_color_manual scale_fill_manual
#' @importFrom rlang ensym as_name `!!`
#' @importFrom ggplot2 position_jitterdodge
#' @importFrom purrr %||%
#' @importFrom dplyr %>%
#' @importFrom rlang exec
#' @export
violin_plot_ggstats_style <- function(data, x, y,
                                      xlab = NULL, ylab = NULL,
                                      caption = NULL, title = NULL, subtitle = NULL,
                                      point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                                                        alpha = 0.4, size = 3, stroke = 0, na.rm = TRUE),
                                      boxplot.args = list(width = 0.3, alpha = 0.2, na.rm = TRUE),
                                      violin.args = list(width = 0.5, alpha = 0.2, na.rm = TRUE),
                                      color_values = NULL) {

  # Use tidy evaluation for input variables
  x_sym <- rlang::ensym(x)
  y_sym <- rlang::ensym(y)

  # Base plot with jittered points, boxplot, and violin plot
  plot_comparison <- ggplot(data, aes(x = !!x_sym, y = !!y_sym)) +
    exec(geom_point, aes(color = !!x_sym), !!!point.args) +
    exec(geom_boxplot, outlier.shape = NA, !!!boxplot.args) +
    exec(geom_violin, !!!violin.args) +
    labs(
      x = xlab %||% rlang::as_name(x_sym),
      y = ylab %||% rlang::as_name(y_sym),
      title = title,
      subtitle = subtitle,
      caption = caption
    )

  # Apply custom color scheme if provided
  if (!is.null(color_values)) {
    plot_comparison <- plot_comparison +
      scale_color_manual(values = color_values) +
      scale_fill_manual(values = color_values)
  }

  return(plot_comparison)
}

violin_plot_ggstats_style <- function(data, x, y,
                                      xlab = NULL, ylab = NULL,
                                      caption = NULL, title = NULL, subtitle = NULL,
                                      point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                                                        alpha = 0.4, size = 3, stroke = 0, na.rm = TRUE),
                                      boxplot.args = list(width = 0.3, alpha = 0.2, na.rm = TRUE),
                                      violin.args = list(width = 0.5, alpha = 0.2, na.rm = TRUE),
                                      color_values = NULL) {

  # Use tidy evaluation
  x_sym <- rlang::ensym(x)
  y_sym <- rlang::ensym(y)

  plot_comparison <- ggplot(data, aes(x = !!x_sym, y = !!y_sym)) +
    exec(geom_point, aes(color = !!x_sym), !!!point.args) +
    exec(geom_boxplot, outlier.shape = NA, !!!boxplot.args) +
    exec(geom_violin, !!!violin.args) +
    labs(
      x = xlab %||% rlang::as_name(x_sym),
      y = ylab %||% rlang::as_name(y_sym),
      title = title,
      subtitle = subtitle,
      caption = caption
    )

  # Add custom colors if provided
  if (!is.null(color_values)) {
    plot_comparison <- plot_comparison +
      scale_color_manual(values = color_values) +
      scale_fill_manual(values = color_values)
  }

  return(plot_comparison)
}
