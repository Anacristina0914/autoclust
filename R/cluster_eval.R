
#' Evaluate Clustering Performance Using Mixed Data
#'
#' Performs comprehensive evaluation of clustering performance using silhouette scores, consensus clustering, and bootstrap stability.
#' Supports both continuous and binary data and multiple clustering algorithms.
#'
#' @param x A data frame or matrix of observations (rows = samples, columns = features). Can include binary or continuous variables.
#' @param ndim Integer. Maximum number of clusters to evaluate (default = 10).
#' @param clust_method Clustering method to use. Options are `"pam"`, `"kmeans"`, `"clara"`, or `"hclust"`.
#' @param distance Distance metric to use. `"auto"` (default) uses `"gower"` for binary data and `"euclidean"` for continuous.
#'        Other options can be passed to `cluster::daisy()`, e.g., `"manhattan"`, `"gower"`, `"euclidean"`.
#' @param data_type Type of data: `"binary"` or `"continuous"`. Used only when `distance = "auto"`.
#' @param sillplot_dot_size Size of dots in silhouette width plot (default = 6).
#' @param save_plot Logical. Save plots to disk? (default = `TRUE`).
#' @param nclust Optional. Number of clusters to use for evaluation. If `NA`, selects best k based on silhouette score (default = `NA`).
#' @param interactive Logical. If `TRUE`, returns interactive silhouette plots using `plotly` (default = `FALSE`).
#' @param boot_runs Integer. Number of bootstrap resamples for Jaccard stability index (default = 100).
#' @param seed Random seed for reproducibility (default = 123).
#' @param compute_consensus Logical. If `TRUE`, performs consensus clustering (default = `TRUE`).
#' @param compute_jaccard Logical. If `TRUE`, computes bootstrap-based Jaccard index for cluster stability (default = `TRUE`).
#' @param consensus_runs Integer. Number of resamples for consensus clustering (default = 100).
#' @param plots_suffix Character string added to filenames when saving plots (default = `paste0(distance, "_", clust_method)`).
#' @param plots_dir Directory where plots are saved (default = current working directory).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{silhouette_plots}{A list of silhouette plots for each evaluated number of clusters, and a summary plot.}
#'   \item{fit}{Fitted clustering objects for each k.}
#'   \item{bootstrap}{Bootstrap cluster stability results for each k (if `compute_jaccard = TRUE`).}
#'   \item{consensus_clustering}{ConsensusClusterPlus results and ICL scores (if `compute_consensus = TRUE`).}
#'   \item{best_nclust}{The optimal number of clusters based on average silhouette width.}
#'   \item{method}{The clustering method used.}
#' }
#'
#' @details
#' This function streamlines the evaluation of clustering strategies on binary or continuous datasets using silhouette scores,
#' consensus clustering, and cluster stability (via bootstrapped Jaccard index). Useful for benchmarking clustering algorithms and selecting optimal k.
#'
#' @import cluster
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @importFrom dplyr arrange mutate slice_head pull
#' @importFrom ConsensusClusterPlus ConsensusClusterPlus calcICL
#' @importFrom stats hclust cutree kmeans
#' @importFrom utils head
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- generate_synthetic_mixed_data(n_samples = 300, n_clusters = 3,
#' n_continuous = 4, n_binary = 4)
#' x <- data[, -ncol(data)]  # remove true cluster labels
#' result <- cluster_eval(x, data_type = "binary", clust_method = "pam", distance = "gower")
#' result$best_nclust
#' }
#'
cluster_eval <- function(x, ndim = 10, clust_method = "pam", distance = "auto", data_type = "binary",
                        sillplot_dot_size = 6, save_plot = TRUE, nclust = NA, interactive = FALSE,
                         boot_runs = 100, seed = 123, compute_consensus = TRUE,
                         compute_jaccard = TRUE, consensus_runs = 100, stand = FALSE,
                         plots_suffix = paste0(distance, "_", clust_method), plots_dir = getwd()){

  results <- list()
  results$silhouette_plots <- list()

  # ---- Distance/dissimilarity Calculation ----
  calculate_distance <- function(data_matrix, method = distance, stand = stand) {
    distance_results <- list()
    if (distance == "auto") {
      if (data_type == "binary") {
        # if data is binary compute dissimilarities
        distance_results[["dist_obj"]] <- daisy(x, metric = "gower", stand = stand)
        distance_results[["diss"]] <- TRUE
        distance_results[["distance"]] <- "gower"
      } else {
        distance_results[["dist_obj"]] <- dist(x, method = "euclidean")
        distance_results[["diss"]] <- FALSE
        distance_results[["distance"]] <- "euclidean"
      }
    } else {
      if (data_type == "binary" || distance == "gower") {
        # If distance is not gower or data is binary
        distance_results[["dist_obj"]] <- daisy(x, metric = distance, stand = stand)
        distance_results[["diss"]] <- TRUE
        distance_results[["distance"]] <- distance
      } else {
        distance_results[["dist_obj"]] <- dist(x, method = distance)
        distance_results[["diss"]] <- FALSE
        distance_results[["distance"]] <- distance
      }
    }
    return(distance_results)
  }

  distance_results <- calculate_distance(data_matrix = x, method = distance, stand = stand)

  # assign variables
  dist_obj <- distance_results[["dist_obj"]]
  diss <- distance_results[["diss"]]

  # ---- Clustering Method Wrapper ----
  cluster_fn <- switch(clust_method,
                       pam = function(x, k) pam(dist_obj, diss = diss, k = k),
                       kmeans = function(x, k) kmeans(dist_obj, centers = k),
                       clara = function(x, k) clara(dist_obj, k = k),
                       hclust = function(x, k) cutree(hclust(dist_obj, method = "average"), k = k),
                       stop("Unsupported clustering method") #TODO add other supported cluster methods (density based)
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
      ggtitle(sprintf("Silhouette plot for k = %s %s", i, clust_method)) +
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
    if (!dir.exists(file.path(plots_dir, "clust_eval_results", "silhouette_results"))) {
      dir.create(file.path(plots_dir, "clust_eval_results", "silhouette_results"), recursive = T)
    }

    ggsave(plot = sillplot, filename = sprintf("sillplot_n%s_%s.jpeg", ndim, plots_suffix), path = file.path(plots_dir, "clust_eval_results", "silhouette_results"))
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


    if (!dir.exists(file.path(plots_dir, "clust_eval_results", "consensus_results"))) {
        dir.create(file.path(plots_dir, "clust_eval_results", "consensus_results"), recursive = T)
      }


    calculate_distance_consensus <- function(x){
      return(function(x){
        x
      })
    }

    # Return function to calculate distance
    message(sprintf("Calculating consensus clustering of %s data using %s distance and %s algorithm...", data_type, distance_results[["distance"]], clust_method))

    custom_distance_consensus <- calculate_distance_consensus(x = distance_results[["dist_obj"]])

    consensus_res <- ConsensusClusterPlus(
      d = distance_results[["dist_obj"]], maxK = ndim, reps = consensus_runs,
      pItem = 0.8, pFeature = 1, clusterAlg = clust_method_consensus_clust,
      distance = "custom_distance_consensus", innerLinkage = "average",
      seed = seed, plot = "pdf", title = file.path(plots_dir, "clust_eval_results", "consensus_results", plots_suffix)
    )

    icl <- calcICL(consensus_res, title = file.path(plots_dir, "clust_eval_results", "consensus_results", plots_suffix), plot = "pdf")

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

