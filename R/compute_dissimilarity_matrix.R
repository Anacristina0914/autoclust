#' Compute and Visualize a Dissimilarity Matrix
#'
#' Computes a dissimilarity matrix using the \code{\link[cluster]{daisy}} function and
#' optionally visualizes it using a heatmap.
#'
#' @param x A data matrix or data frame.
#' @param metric Character. Dissimilarity metric to use: \code{"euclidean"}, \code{"manhattan"}, or \code{"gower"}.
#' @param stand Logical. Whether to standardize variables (used for non-Gower metrics).
#' @param return_dis Logical. If \code{TRUE}, returns the dissimilarity object; else returns a matrix.
#' @param plot_dissim Logical. Whether to generate a dissimilarity heatmap.
#' @param heatmap_title Character. Title of the heatmap plot.
#' @param heatmap_axis_text_size Numeric. Axis text size for the heatmap.
#' @param plot_title_size Numeric. Plot title font size.
#' @param save_plots Logical. If \code{TRUE}, saves the heatmap.
#' @param plot_outpath Character. Directory where the plot is saved.
#'
#' @return A list containing:
#' \describe{
#'   \item{dissimilarity_matrix}{Either a dissimilarity object or a matrix, depending on \code{return_dis}.}
#'   \item{heatmap_plot}{(Optional) ggplot2 heatmap plot of the dissimilarity matrix.}
#' }
#' @importFrom cluster daisy
#' @importFrom factoextra fviz_dist
#' @import ggplot2
#' @export

compute_dissimilarity_matrix <- function(x, metric = c("euclidean", "manhattan", "gower"), stand = FALSE, return_dis = FALSE,
                                         plot_dissim = TRUE, heatmap_title = paste0(metric, " Dissimilarity Matrix"), heatmap_axis_text_size = 4,
                                         plot_title_size = 20, save_plots = FALSE, plot_outpath = ".") {

  # Calculate the dissimilarity matrix
  x_disim <- daisy(x, metric = metric, stand = stand)
  x_dissim_matrix <- as.matrix(x_disim)

  if (return_dis){
    # Initialize a list to store results
    result <- list(dissimilarity_matrix = x_disim)
  } else {
    result <- list(dissimilarity_matrix = x_dissim_matrix)
  }

  # Optionally create a heatmap plot
  if (plot_dissim) {
    pheatmap_dissim <- factoextra::fviz_dist(x_disim) +
      theme(axis.text = element_text(size = heatmap_axis_text_size), plot.title = element_text(size = plot_title_size)) +
      ggtitle(heatmap_title)

    # Add the plot to the list
    result$heatmap_plot <- pheatmap_dissim

    if (save_plots){
      ggsave(plot = pheatmap_dissim, filename = sprintf("dissim_%s.jpeg", metric), path = plot_outpath)
    }
  }

  # Return the list containing the matrix and, if requested, the plots
  return(result)
}
