
#' Evaluate Clustering with Silhouette Scores
#'
#' This function performs clustering evaluation by calculating silhouette widths for a specified range of cluster numbers and selecting the optimal cluster number based on the highest average silhouette width. The function can also plot silhouette widths and save the plot.
#'
#' @param x A dissimilarity object of class dissimilarity.
#' @param ndim Integer. The maximum number of clusters to consider for silhouette analysis. Default is 10.
#' @param sillplot_dot_size Numeric. The size of the points in the silhouette plot. Default is 6.
#' @param save_plot Logical. If TRUE, saves the silhouette plot as a JPEG file. Default is TRUE.
#' @param nclust Optional. An integer specifying the desired number of clusters. If not provided, the function uses the number of clusters with the highest silhouette width.
#'
#' @return A list containing:
#' \item{silhouette_plot}{A ggplot2 object for the silhouette plot.}
#' \item{pam_fit}{The best PAM clustering model based on the optimal number of clusters determined by silhouette width.}
#'
#' @examples
#' # Example usage with a dissimilarity matrix `diss_matrix`
#' results <- cluster_eval(x = diss_matrix, ndim = 10)
#' results$silhouette_plot  # Access the silhouette plot
#' results$pam_fit  # Access the best PAM clustering model
#'
#' @importFrom cluster pam
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_linedraw ggsave
#' @importFrom dplyr filter arrange slice_head pull
#' @export

cluster_eval <- function(x, ndim = 10, sillplot_dot_size = 6,
                         save_plot = TRUE, nclust = NA){
  # Plot silhouette if specified by user
  results <- list()

  # Calculate silhouette widths for cluster sizes 2 to ndim
  sil_width <- numeric(ndim-1)

  pam_fit <- list()
  for (i in 2:ndim) {
    pam_fit[[as.character(i)]] <- pam(x, diss = TRUE, k = i)
    sil_width[i] <- pam_fit[[as.character(i)]]$silinfo$avg.width
  }

  # Create silhouette plot with ggplot2
  sil_data <- data.frame(Clusters = factor(1:ndim), Avg_Silhouette_Width = sil_width)
  sil_data <- sil_data %>% filter(Clusters != 1)

  sillplot <- ggplot(sil_data, aes(x = Clusters, y = Avg_Silhouette_Width, group = 1)) +
    geom_line() +
    geom_point(colour = "#870052", size = sillplot_dot_size) +
    labs(x = "Number of clusters", y = "Average Silhouette Width") +
    theme_linedraw()

  # Add silhouette plot to the list
  results$silhouette_plot <- sillplot

  # If save_plots = TRUE save them
  if (save_plot){
    ggsave(plot = sillplot, filename = sprintf("Sillplot_n%s_%s.jpeg", ndim, metric), path =".")
    }

  # Cluster based on best silhouette
  if (is.na(nclust)){
    # Cluster using number of clusters corresponding to max silhouette
    n_clust <- sil_data %>%
      arrange(desc(Avg_Silhouette_Width)) %>%
      slice_head(n = 1) %>%
      pull(Clusters)
  }

  # Run clustering with best number of clusters
  best_pam_fit <- pam_fit[[as.character(n_clust)]]

  # Add clustering to results
  results$pam_fit <- best_pam_fit

  # Return best pam fit
  return(results)
}
