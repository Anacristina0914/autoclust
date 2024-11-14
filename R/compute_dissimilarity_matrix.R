#' Compute dissimilarity matrices
#'
#' Wrapper function (daisy) for computing dissimilarity matrices from matrix or datasets of continuous, discrete, or categorical variables.
#' The function also creates a heatmap using the dissimilarity matrix if specified.
#'
#' @param x numeric matrix or data frame (n x p). Dissimilarities are computed between the rows of x. Dataframe must contain headers.
#' For further details, see help(daisy).
#' @param metric Metric to be used to compute matrix. Possible options include "gower" (default), "euclidean", or "manhattan".
#' @param stand if TRUE, measurements in x are standardized before calculating dissimilarities. For further details, see help(daisy).
#' @param return_dis if TRUE function returns dissimilarity object rather than matrix.
#' @param plot_dissim if TRUE, plots the dissimilarity matrix as a heatmap and shows hierarchical clustering on the plot.
#' @param heatmap_title Title for the heatmap plot.
#' @param heatmap_axis_text_size Heatmap x-y axis text size.
#' @param plot_title_size Font size for the plot title.
#' @param save_plots If TRUE plots produced are saved in the working directory.
#' @param plot_outpath Output pathway for plots. If not specified and save_plots is TRUE they are saved in the current directory.
#' @return A list containing the dissimilarity matrix, optionally the heatmap plot if specified.
#'
#' @importFrom cluster daisy
#' @examples
#' # Example of how to use the function
#' result <- compute_dissimilarity_matrix(x = autoant_dataset, metric = "gower")
#' result$dissimilarity_matrix  # Access the matrix
#' result$heatmap_plot  # Access the plot (if plot_dissim = TRUE)
#'
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
