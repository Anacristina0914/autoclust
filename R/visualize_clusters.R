visualize_clusters <- function(x, pam_fit, df, id_col, umap_nneight = c(10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L),
                               umap_min_dist = c(0.1, 0.25, 0.3, 0.5, 0.75, 0.99), point_size = 2, save_plots = FALSE, plot_outpath = "."){
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
           ID=df[[id_col]])

  tsne_plot <- ggplot(data = tsne_plot_data, aes(x=tsne1,y=tsne2))+
    geom_point(aes(color=cluster), alpha = 0.8, size = point_size) +
    theme_linedraw()
  # If save_plots TRUE save tsne plot
  if (save_plots){
    ggsave(filename = "tsne_dist.jpeg", plot = tsne_plot,
           bg = "white", path = plot_outpath)
  }
  # Save results in a list
  results <- list()
  results$tnse_plot <- tsne_plot

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

  # Make line plot

  # TODO


  # Return results
  return(results)

}
