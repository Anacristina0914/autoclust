#' Custom ggplot2 theme for subgroups SLE
#'
#' Returns a `ggplot2` theme object with a tailored style optimized for clustering
#' or publication-quality plots. Allows full customization of font family, size,
#' color, and positioning for all plot elements.
#'
#' @param text_family Font family for general plot text.
#' @param text_size Base text size.
#' @param text_color Color for general plot text.
#' @param plot_title_family Font family for the plot title.
#' @param plot_title_size Font size for the plot title.
#' @param plot_title_face Font face for the plot title (e.g., `"bold"`, `"italic"`).
#' @param plot_title_color Color for the plot title.
#' @param axis_title_size Font size for axis titles.
#' @param axis_text_size Font size for axis tick labels.
#' @param axis_text_color Color for axis tick labels.
#' @param caption_text_color Color of the caption text.
#' @param caption_text_size Size of the caption text.
#' @param caption_face_style Face style of the caption text (e.g., `"bold"`).
#' @param legend_text_size Font size for legend text.
#' @param legend_text_color Color of legend text.
#' @param legend_title_face_style Face style for the legend title (e.g., `"bold"`).
#' @param legend_position Position of the legend (e.g., `"top"`, `"bottom"`, `"none"`).
#'
#' @return A `ggplot2` theme object to be added to a plot.
#'
#' @examples
#' library(ggplot2)
#' ggplot(antibody_dataset, aes(x = Subgroup, y = `HLA PRS 4 SNP`)) +
#'   geom_boxplot() +
#'   theme_clustering()
#'
#' @importFrom ggplot2 theme_bw theme element_text
#' @export

theme_clustering <- function(text_family = "Roboto", text_size = 8, text_color = "black",
                             plot_title_family = "Lobster Two", plot_title_size = 20,
                             plot_title_face = "bold", plot_title_color ="#2a475e",
                             axis_title_size = 12, axis_text_size = 10, axis_text_color = "black",
                             caption_text_color = "black", caption_text_size = 8, caption_face_style = "bold",
                             legend_text_size = 12, legend_text_color = "black", legend_title_face_style = "bold",
                             legend_position = "top"){
  theme_bw() +
  theme(
    # This is the new default font in the plot
    text = element_text(family = text_family, size = text_size, color = text_color),
    plot.title = element_text(family = plot_title_family, size = plot_title_size,
      face = plot_title_face,
      color = plot_title_color
    ),
    axis.title = element_text(size = axis_text_size),
    axis.text = element_text(size = axis_text_size, color = axis_text_color),
    plot.caption = element_text(color = caption_text_color, size = caption_text_size,
                                face = caption_face_style),
    legend.text = element_text(size = legend_text_size, color = legend_text_color),
    legend.title = element_text(face = legend_title_face_style, size = legend_text_size),
    legend.position = legend_position
  )
}
