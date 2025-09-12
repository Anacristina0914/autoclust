#' Generate Synthetic Clustered Datasets
#'
#' Simulates clustered datasets (binary or continuous) with optional batch effects.
#' Useful for testing and benchmarking clustering algorithms.
#'
#' @param n_samples Integer. Total number of samples.
#' @param n_vars Integer. Number of features (variables).
#' @param n_clusters Integer. Number of clusters to generate.
#' @param cluster_sep Numeric. Controls separation between cluster centers.
#' @param data_type Character. Either \code{"continuous"} or \code{"binary"}.
#' @param df Optional reference dataframe. If provided, sampling and perturbation is based on this data.
#' @param noise_sd Numeric. Standard deviation of Gaussian noise (only for continuous data).
#' @param noise_prob Numeric. Probability of flipping bits (only for binary data).
#' @param n_batches Integer. Number of batches to simulate. If 0, no batch effect is added.
#' @param batch_effect_strength Numeric. Controls strength of batch effect.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return A data frame with synthetic data and a \code{Cluster} column. If \code{n_batches > 0}, includes a \code{Batch} column.
#'
#' @export

generate_synthetic_clusters <- function(n_samples = 100,
                                        n_vars = 10,
                                        n_clusters = 3,
                                        n_batches = 1,
                                        data_type = c("binary", "continuous"),
                                        cluster_sep = 1,
                                        flip_prob = 0.05,
                                        random_state = NULL) {
  set.seed(random_state)
  data_type <- match.arg(data_type)

  # Assign number of samples per cluster (accounting for uneven division)
  samples_per_cluster <- rep(n_samples %/% n_clusters, n_clusters)
  if (n_samples %% n_clusters > 0) {
    samples_per_cluster[1:(n_samples %% n_clusters)] <-
      samples_per_cluster[1:(n_samples %% n_clusters)] + 1
  }
  stopifnot(sum(samples_per_cluster) == n_samples)

  # Predefine cluster centers
  if (data_type == "binary") {
    centers <- matrix(rbinom(n_vars * n_clusters, 1, 0.5), nrow = n_clusters)
  } else {
    centers <- matrix(rnorm(n_vars * n_clusters, mean = 0, sd = cluster_sep),
                      nrow = n_clusters)
  }

  data_list <- list()
  cluster_labels <- c()
  sample_idx <- 1

  for (k in 1:n_clusters) {
    n_k <- samples_per_cluster[k]
    cluster_center <- centers[k, ]

    if (data_type == "binary") {
      # Generate data based on cluster center with noise
      sampled <- matrix(rep(cluster_center, each = n_k), nrow = n_k)
      flip_mask <- matrix(rbinom(n_k * n_vars, 1, flip_prob), nrow = n_k)
      cluster_data <- abs(sampled - flip_mask)
    } else {
      # Add Gaussian noise around cluster center
      cluster_data <- matrix(rnorm(n_k * n_vars, mean = 0, sd = 1), nrow = n_k)
      cluster_data <- cluster_data + matrix(rep(cluster_center, each = n_k), nrow = n_k)
    }

    data_list[[k]] <- cluster_data
    cluster_labels <- c(cluster_labels, rep(k, n_k))
  }

  # Combine data from all clusters
  data_all <- do.call(rbind, data_list)
  stopifnot(nrow(data_all) == n_samples)

  # Assign batch labels randomly
  batch_labels <- sample(1:max(n_batches, 1), n_samples, replace = TRUE)

  # Create final data frame
  df_out <- as.data.frame(data_all)
  colnames(df_out) <- paste0("var", 1:n_vars)
  df_out$cluster <- as.factor(cluster_labels)
  df_out$batch <- as.factor(batch_labels)
  rownames(df_out) <- paste0("sample", seq_len(n_samples))

  return(df_out)
}
