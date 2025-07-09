
#' Evaluate Clustering with Silhouette Scores
#'
#' This function performs clustering evaluation by calculating silhouette widths for a specified range of cluster numbers and selecting the optimal cluster number based on the highest average silhouette width. The function can also plot silhouette widths and save the plot.
#'
#' @param x A dissimilarity object of class dissimilarity.
#' @param ndim Integer. The maximum number of clusters to consider for silhouette analysis. Default is 10.
#' @param sillplot_dot_size Numeric. The size of the points in the silhouette plot. Default is 6.
#' @param save_plot Logical. If TRUE, saves the silhouette plot as a JPEG file. Default is TRUE.
#' @param nclust Optional. An integer specifying the desired number of clusters. If not provided, the function uses the number of clusters with the highest silhouette width.
#' @param interactive Logical. If TRUE, the results return interactive silhouette plots for each number of clusters given by ndim.
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

cluster_eval <- function(x, ndim = 10, clust_method = "pam", distance = "auto", data_type = "binary",
                        sillplot_dot_size = 6, save_plot = TRUE, nclust = NA, interactive = FALSE,
                         boot_runs = 100, seed = 123, compute_consensus = TRUE,
                         compute_jaccard = TRUE, consensus_runs = 100,
                         plots_suffix = paste0(distance, "_", clust_method), plots_dir = getwd()){

  results <- list()
  results$silhouette_plots <- list()

  # ---- Distance Calculation ----
  if (distance == "auto") {
    if (data_type == "binary") {
      dist_obj <- daisy(x, metric = "gower", stand = FALSE)
      diss <- TRUE
      distance <- "binary"
    } else {
      dist_obj <- daisy(x, metric = "euclidean", stand = FALSE)
      diss <- TRUE
      distance <- "euclidean"
    }
  } else {
    dist_obj <- daisy(x, metric = distance, stand = FALSE)
    diss <- TRUE
  }

  # ---- Clustering Method Wrapper ----
  cluster_fn <- switch(clust_method,
                       pam = function(x, k) pam(dist_obj, diss = diss, k = k),
                       kmeans = function(x, k) kmeans(dist_obj, centers = k),
                       clara = function(x, k) clara(dist_obj, k = k),
                       hclust = function(x, k) cutree(hclust(dist_obj, method = "average"), k = k),
                       stop("Unsupported clustering method") #TODO add other supported cluster methods (density based and )
  )


  # ---- Silhouette Scores ----
  sil_width <- numeric(ndim - 1)
  message("Computing silhouette scores...")

  for (i in 2:ndim) {
    fit <- cluster_fn(dist_obj, i)
    # Save cluster assignments for the different methods
    cluster_assignment <- if (clust_method == "kmeans") fit$cluster else if (clust_method == "clara") fit$clustering else fit$clustering

    sil <- silhouette(cluster_assignment, dist_obj)
    sil_width[i] <- mean(sil[, 3])

    sil_df <- as.data.frame(sil[, 1:3])
    sil_df$obs <- 1:nrow(sil_df)

    sil_df <- sil_df %>%
      arrange(cluster, -sil_width, .by_group = TRUE) %>%
      mutate(order = factor(row_number()))

    p <- ggplot(sil_df, aes(y = order, x = sil_width, fill = factor(cluster))) +
      geom_bar(stat = "identity", width = 0.8) +
      scale_fill_carto_d(name = "cluster", palette = "Safe") +
      labs(x = "Silhouette Width", y = "Observation", fill = "Cluster") +
      ggtitle(paste("Silhouette plot for k =", i, clust_method)) +
      theme_minimal() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

    if (interactive) {
      p <- ggplotly(p, tooltip = c("x", "fill"))
    }

    results$silhouette_plots[[paste0(i, "_clusters")]] <- p
    results$fit[[as.character(i)]] <- fit
  }

  sil_data <- data.frame(Clusters = 2:ndim, Avg_Silhouette_Width = sil_width[2:ndim])

  sillplot <- ggplot(sil_data, aes(x = Clusters, y = Avg_Silhouette_Width, group = 1)) +
    geom_line() +
    geom_point(colour = "#870052", size = sillplot_dot_size) +
    labs(x = "Number of clusters", y = "Average Silhouette Width") +
    geom_hline(yintercept = c(0.25, 0.5, 0.7), linetype = "dashed", color = c("#CC0000", "#FFCC33", "#669933")) +
    theme_linedraw()

  results$silhouette_plots$avg_silhouette_plot <- sillplot

  if (save_plot) {
    if (!dir.exists(file.path(plots_dir, "clust_eval_results", paste0("silhouette_results_", plots_suffix)))) {
      dir.create(file.path(plots_dir, "clust_eval_results", paste0("silhouette_results_", plots_suffix)))
    }

    ggsave(plot = sillplot, filename = sprintf("sillplot_n%s_%s.jpeg", ndim, plots_suffix), path = file.path(plots_dir, "clust_eval_results", paste0("silhouette_results_", plots_suffix)))
  }


  # ---- Determine Optimal k ----
  if (is.na(nclust)) {
    n_clust <- sil_data %>%
      arrange(desc(Avg_Silhouette_Width)) %>%
      slice_head(n = 1) %>%
      pull(Clusters)
  } else {
    n_clust <- nclust
  }

  # ---- Bootstrap Stability (Optional) ----
  if (compute_jaccard) {
    cluster_fn_jaccard <- switch(clust_method,
                         pam = pamkCBI,
                         kmeans = kmeansCBI,
                         clara = claraCBI,
                         hclust = hclustCBI,
                         stop("Unsupported clustering method") #TODO add other supported cluster methods (density based and )
    )
    message("Computing bootstrap stability...")
    results$bootstrap <- list()

    for (i in 2:ndim) {
      message(sprintf("Computing jaccard index for %s clusters...", i))

      results$bootstrap[[paste0(i, "_clusters")]] <- compute_bootstrap_stability(
        x = dist_obj, nclust = i, B = boot_runs, bootmethod = "boot", seed = seed,
        clustermethod = cluster_fn_jaccard, diss = diss,
        theme = theme_minimal(), save_plot = save_plot,
        plot_dir = file.path(plots_dir), plots_suffix = plots_suffix
      )
    }
  }

  # ---- Consensus Clustering ----
  if (compute_consensus) {
    message("Computing consensus clustering...")
    if (!clust_method %in% c("pam", "kmeans", "hc")) {
      stop("Method must be set to pam, kmeans or hc")
    }

    # Return the right name for the clustering algorithm
    clust_method_consensus_clust <- switch(clust_method,
                                 pam = "pam",
                                 kmeans = "km",
                                 hclust = "hc",
                                 stop("Unsupported clustering method for Consensus Cluster Plus") #TODO add other supported cluster methods (density based and )
    )


    if (!dir.exists(file.path(plots_dir, "clust_eval_results", paste0("consensus_results_", plots_suffix)))) {
        dir.create(file.path(plots_dir, "clust_eval_results", paste0("consensus_results_", plots_suffix)))
      }

    consensus_res <- ConsensusClusterPlus(
      d = as.matrix(t(x)), maxK = ndim, reps = consensus_runs,
      pItem = 0.8, pFeature = 1, clusterAlg = clust_method_consensus_clust,
      distance = distance, innerLinkage = "average",
      seed = seed, plot = "pdf", title = file.path(plots_dir, "clust_eval_results", paste0("consensus_results_", plots_suffix))
    )

    icl <- calcICL(consensus_res, title = file.path(plots_dir, "clust_eval_results", paste0("consensus_results_", plots_suffix)), plot = "pdf")

    results$consensus_clustering <- list(result = consensus_res, consensus_calc = icl)
  }

  results$best_nclust <- n_clust
  results$method <- clust_method
  if (save_plot) {
    message(sprintf("Plots saved to %s", paste0(plots_dir, "/clust_eval_results/")))
  }
  message("Completed..Hej då!")
  return(results)
}

