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
#' @param sillplot if TRUE, calculates and stores a silhouette plot for varying numbers of clusters.
#' @param ndim Maximum number of clusters to consider for the silhouette plot.
#' @param save_plots If TRUE plots produced are saved in the working directory.
#' @return A list containing the dissimilarity matrix, optionally the heatmap plot, and the silhouette plot if specified.
#' @examples
#' # Example of how to use the function
#' result <- compute_dissimilarity_matrix(x = autoant_dataset, metric = "gower")
#' result$dissimilarity_matrix  # Access the matrix
#' result$heatmap_plot  # Access the plot (if plot_dissim = TRUE)
#' result$silhouette_plot  # Access the silhouette plot (if sillplot = TRUE)
#'
#' @export
compute_dissimilarity_matrix <- function(x, metric = c("euclidean", "manhattan", "gower"), stand = FALSE, return_dis = FALSE,
                                         plot_dissim = TRUE, heatmap_title = paste0(metric, " Dissimilarity Matrix"), heatmap_axis_text_size = 4,
                                         plot_title_size = 20, sillplot = TRUE, ndim = 10,
                                         save_plots = FALSE) {

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
      ggsave(plot = pheatmap_dissim, filename = sprintf("dissim_%s.jpeg", metric))
    }
  }

  # Plot silhouette if specified by user
  if (sillplot) {
    # Calculate silhouette widths for cluster sizes 2 to ndim
    sil_width <- numeric(ndim)
    for (i in 2:ndim) {
      pam_fit <- pam(x_disim, diss = TRUE, k = i)
      sil_width[i] <- pam_fit$silinfo$avg.width
    }

    # Create silhouette plot with ggplot2
    sil_data <- data.frame(Clusters = factor(1:ndim), Avg_Silhouette_Width = sil_width)
    sillplot <- ggplot(sil_data, aes(x = Clusters, y = Avg_Silhouette_Width, group = 1)) +
      geom_line() +
      geom_point(colour = "steelblue", size = 6) +
      labs(x = "Number of clusters", y = "Average Silhouette Width") +
      theme_linedraw()

    # Add silhouette plot to the list
    result$silhouette_plot <- sillplot

    # If save_plots = TRUE save them
    if (save_plots){
      ggsave(plot = sillplot, filename = sprintf("Sillplot_n%s_%s.jpeg", ndim, metric), path =".")
    }
  }

  # Return the list containing the matrix and, if requested, the plots
  return(result)
}
