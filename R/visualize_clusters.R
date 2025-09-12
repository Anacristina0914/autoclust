#' Visualize Clustering Results with t-SNE and UMAP
#'
#' Visualizes cluster assignments from a PAM model using t-SNE and UMAP dimensionality
#' reduction techniques. UMAP is computed for several combinations of \code{n_neighbors}
#' and \code{min_dist} values. The function returns plots and optionally saves them.
#'
#' @param x A dissimilarity matrix (must be of class "dissimilarity").
#' @param pam_fit A fitted PAM object (e.g., from \code{cluster::pam()}).
#' @param df Data frame containing metadata including IDs and stability scores.
#' @param id_col Character. Name of the ID column in \code{df}.
#' @param umap_nneight Integer vector. Values for the UMAP \code{n_neighbors} parameter.
#' @param umap_min_dist Numeric vector. Values for the UMAP \code{min_dist} parameter.
#' @param point_size Numeric. Size of points in the plots. Default is 2.
#' @param stability_col Character. Column name in \code{df} that stores cluster stability metrics.
#' @param save_plots Logical. If \code{TRUE}, plots are saved to \code{plot_outpath}.
#' @param plot_outpath Character. Directory path for saving plots. Default is current directory.
#'
#' @return A list with:
#' \describe{
#'   \item{tsne_plot}{A ggplot2 object visualizing clusters via t-SNE.}
#'   \item{umap_plots}{A nested list of ggplot2 UMAP plots indexed by neighbor and distance parameters.}
#' }
#' @import ggplot2
#' @importFrom Rtsne Rtsne
#' @importFrom randomcoloR distinctColorPalette
#' @export

visualize_clusters <- function(x, pam_fit, df, id_col, umap_nneight = c(10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L),
                               umap_min_dist = c(0.1, 0.25, 0.3, 0.5, 0.75, 0.99), point_size = 2, stability_col = "stability",
                               save_plots = FALSE,
                               plot_outpath = "."){
  # Make sure x is a dissimilarity matrix
  if (class(x)[1] != "dissimilarity"){
    stop("x object must be a dissimilarity matrix")
  }

  # Make different colors for clusters
  num_clusters = length(unique(pam_fit$clustering))
  cluster_colors = randomcoloR::distinctColorPalette(num_clusters)
  names(cluster_colors) = unique(pam_fit$clustering)

  # Make tsne plot
  tsne <- Rtsne(x, is_distance=TRUE)
  tsne_plot_data <- tsne$Y %>%
    data.frame()%>%
    setNames(c("tsne1","tsne2"))%>%
    mutate(cluster=factor(pam_fit$clustering),
           ID=df[[id_col]],
           stability = df[[stability_col]])

  tsne_plot <- ggplot(data = tsne_plot_data, aes(x=tsne1,y=tsne2))+
    geom_point(aes(color=cluster, alpha = stability), size = point_size) + #TODO adjust to generalize
    theme_linedraw()
  # If save_plots TRUE save tsne plot
  if (save_plots){
    ggsave(filename = "tsne_dist.jpeg", plot = tsne_plot,
           bg = "white", path = plot_outpath)
  }
  # Save results in a list
  results <- list()
  results$tsne_plot <- tsne_plot

  # Make UMAP
  umap_plots <- list()
  for (neigh in umap_nneight) { # Loop over neighbor values
    for (dist in umap_min_dist) { # Loop over distance values
      message(sprintf("Computing UMAP for nneigh = %s, min_dist = %s", neigh, dist))

      umap_coords <- run_custom_umap(x, neighbors = neigh, min_dist = dist)
      ggplot_umap <- plot_custom_umap(umap_coords, pam_fit$clustering, neigh, dist, pt.size = point_size)

      umap_plots[[as.character(neigh)]][[as.character(dist)]] <- ggplot_umap

      if (save_plots){
        ggsave(filename = sprintf("umap_neigh%s_dist%s.png", as.character(neigh), dist),
          plot = ggplot_umap, bg = "white", path = plot_outpath)
      }
    }
  }

  results$umap_plots <- umap_plots

  # Return results
  return(results)

}
