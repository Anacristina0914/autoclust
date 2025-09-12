#' Compute Bootstrap Clustering Stability
#'
#' Computes clustering stability using bootstrap resampling and Jaccard similarity
#' based on the \code{\link[fpc]{clusterboot}} function. Returns the bootstrap mean
#' Jaccard index for each cluster and a bar plot summarizing cluster stability.
#'
#' @param x A dissimilarity matrix (if \code{diss = TRUE}) or data matrix (if \code{diss = FALSE}).
#' @param nclust Integer. The number of clusters to evaluate.
#' @param plot_dir Character. Directory where plots will be saved.
#' @param plots_suffix Character. Suffix used for naming output plots.
#' @param B Integer. Number of bootstrap resampling iterations. Default is 100.
#' @param bootmethod Character. Resampling method. Typically \code{"boot"} or \code{"subset"}.
#' @param seed Integer. Random seed for reproducibility. Default is 123.
#' @param clustermethod A clustering function in the format expected by \code{fpc::clusterboot}.
#' @param diss Logical. Whether \code{x} is a dissimilarity matrix (\code{TRUE}) or a data matrix (\code{FALSE}).
#' @param theme A ggplot2 theme object to customize the stability plot. Default is \code{theme_minimal()}.
#' @param save_plot Logical. Whether to save the plot as a JPEG file. Default is \code{FALSE}.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{stats}{The full result from \code{fpc::clusterboot}.}
#'   \item{plot}{A ggplot2 object showing the mean Jaccard similarity per cluster.}
#' }
#' If \code{save_plot = TRUE}, the plot is also saved as a JPEG file in the specified directory.
#'
#' @importFrom fpc clusterboot
#' @importFrom ggplot2 ggplot aes geom_col labs theme ggsave
#' @importFrom rcartocolor scale_fill_carto_d
#' @export

compute_bootstrap_stability <- function(x, nclust, plot_dir, plots_suffix, B = 100, bootmethod = "boot",
                                        seed = 123, clustermethod = NULL, diss = TRUE,
                                        theme = theme_minimal(), save_plot = FALSE){
  message("Computing bootstrap stability...")
  set.seed(seed)

  clusterboot_res <- clusterboot(x, B = B, bootmethod = bootmethod,
                                   clustermethod = clustermethod,
                                   k = nclust, diss = diss)

  stability_df <- data.frame(
    Cluster = 1:length(clusterboot_res$bootmean),
    Jaccard_Mean = clusterboot_res$bootmean
  )

  stability_plot <- ggplot(stability_df, aes(x = factor(Cluster), y = Jaccard_Mean, fill = factor(Cluster))) +
    geom_col() +
    scale_fill_carto_d(name = "cluster", palette = "Safe") +
    labs(x = "Cluster", y = "Mean Jaccard Similarity",
         title = paste("Bootstrap Stability (Jaccard) for k =", nclust)) +
    ylim(0,1) + theme

  results <- list(
    stats = clusterboot_res,
    plot = stability_plot
  )

  # If save plots
  if (save_plot) {
    if (!dir.exists(file.path(plot_dir, "clust_eval_results", "bootstrap_stability_results"))) {
      dir.create(file.path(plot_dir, "clust_eval_results", "bootstrap_stability_results"))
    }
    ggsave(plot = stability_plot, filename = sprintf("bootstrap_stability_n%s_%s.jpeg", nclust, plots_suffix),
           path = file.path(plot_dir, "clust_eval_results", "bootstrap_stability_results"))

  }

  return(results)

}
