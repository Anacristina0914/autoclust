#' Plot UMAP across different neighbors and min_distance values.
#'
#'
#' @param umap_coords umap coordinates for plots.
#' @param clusters vector containing clusters for each point.
#' @param neighbors number of neighbors used to compute umap coords.
#' @param min_dist minimum distance used to compute umap coords.
#' @param pt.size point size for umap plot.

#' @return a ggplot object of UMAP plot.
#' @examples
#' # Example of how to use the function
#' umap_plot <- plot_custom_umap(umap_coords = coords, neighbors = 10,
#' min_dist = 0.4, pt.size = 1)
#'
#' @export
plot_custom_umap <- function(umap_coords, clusters, neighbors, min_dist, pt.size = 0.5) {
  umap_coords$Cluster <- as.factor(clusters)

  ggplot_umap <- ggplot(umap_coords, aes(x = UMAP1, y = UMAP2, color = Cluster)) +
    geom_point(alpha = 0.7, size = pt.size) +
    theme_linedraw() +
    labs(
      title = sprintf("UMAP Plot nneigh %s, min_dist %s", neighbors, min_dist),
      x = "UMAP Dimension 1",
      y = "UMAP Dimension 2"
    )

  return(ggplot_umap)
}
