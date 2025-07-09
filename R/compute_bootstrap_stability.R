
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
    if (!dir.exists(file.path(plot_dir, "clust_eval_results", paste0("bootstrap_stability_results_", plots_suffix)))) {
      dir.create(file.path(plot_dir, "clust_eval_results", paste0("bootstrap_stability_results_", plots_suffix)))
    }
    ggsave(plot = stability_plot, filename = sprintf("BootstrapStability_n%s_%s.jpeg", nclust, plots_suffix),
           path = file.path(plot_dir, "clust_eval_results", paste0("bootstrap_stability_results_", plots_suffix)))

  }

  return(results)

}
