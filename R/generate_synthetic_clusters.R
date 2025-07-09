generate_synthetic_clusters <- function(n_samples = 300,
                                        n_vars = 10,
                                        n_clusters = 3,
                                        cluster_sep = 1,
                                        data_type = c("continuous", "binary"),
                                        df = NULL,
                                        noise_sd = 0.1,
                                        noise_prob = 0.05,
                                        n_batches = 0,
                                        batch_effect_strength = 0.5,
                                        seed = 123) {
  set.seed(seed)
  data_type <- match.arg(data_type)
  samples_per_cluster <- rep(n_samples %/% n_clusters, n_clusters)
  samples_per_cluster[1:(n_samples %% n_clusters)] <- samples_per_cluster[1:(n_samples %% n_clusters)] + 1

  data_list <- list()
  cluster_labels <- rep(1:n_clusters, times = samples_per_cluster)
  batch_labels <- sample(1:max(n_batches, 1), n_samples, replace = TRUE)

  idx <- 1

  for (k in 1:n_clusters) {
    n_k <- samples_per_cluster[k]

    if (is.null(df)) {
      # --- From scratch ---
      if (data_type == "continuous") {
        means <- rnorm(n_vars, mean = k * cluster_sep, sd = 1)
        cluster_data <- matrix(rnorm(n_k * n_vars, mean = rep(means, each = n_k), sd = 1),
                               nrow = n_k, byrow = FALSE)
      } else if (data_type == "binary") {
        spread <- min(0.4, cluster_sep * 0.1)
        probs <- runif(n_vars, min = 0.5 - spread * k, max = 0.5 + spread * k)
        cluster_data <- matrix(rbinom(n_k * n_vars, 1, prob = rep(probs, each = n_k)),
                               nrow = n_k, byrow = FALSE)
      }
    } else {
      # --- Based on reference dataframe ---
      sampled <- df[sample(1:nrow(df), n_k, replace = TRUE), ]
      sampled <- as.matrix(sampled)

      if (data_type == "continuous") {
        shifts <- rnorm(n_vars, mean = k * cluster_sep, sd = 0.5)
        cluster_data <- sweep(sampled, 2, shifts, "+")
      } else if (data_type == "binary") {
        flip_prob <- min(0.5, cluster_sep * (k - 1) * 0.1 + 0.1)
        flip_mask <- matrix(rbinom(n_k * n_vars, 1, flip_prob), nrow = n_k)
        cluster_data <- abs(sampled[1:n_k, ] - flip_mask)
      }
    }

    # --- Add noise ---
    if (data_type == "continuous" && noise_sd > 0) {
      noise <- matrix(rnorm(n_k * n_vars, mean = 0, sd = noise_sd), nrow = n_k)
      cluster_data <- cluster_data + noise
    }
    if (data_type == "binary" && noise_prob > 0) {
      flip_mask <- matrix(rbinom(n_k * n_vars, 1, noise_prob), nrow = n_k)
      cluster_data <- abs(cluster_data - flip_mask)
    }

    data_list[[k]] <- cluster_data
  }

  # --- Combine all clusters ---
  data_all <- do.call(rbind, data_list)
  colnames(data_all) <- paste0("Var", 1:n_vars)
  df_out <- as.data.frame(data_all)
  df_out$Cluster <- factor(cluster_labels)

  # --- Add batch effect ---
  if (n_batches > 0) {
    if (data_type == "continuous") {
      batch_shifts <- matrix(0, n_batches, n_vars)
      for (b in 1:n_batches) {
        affected_vars <- sample(n_vars, round(n_vars / 3))
        batch_shifts[b, affected_vars] <- rnorm(length(affected_vars), mean = 0, sd = batch_effect_strength)
      }
      for (i in 1:n_samples) {
        df_out[i, 1:n_vars] <- df_out[i, 1:n_vars] + batch_shifts[batch_labels[i], ]
      }
    } else if (data_type == "binary") {
      for (i in 1:n_samples) {
        flip_rate <- min(0.5, batch_effect_strength)
        flip_mask <- rbinom(n_vars, 1, flip_rate)
        df_out[i, 1:n_vars] <- abs(df_out[i, 1:n_vars] - flip_mask)
      }
    }
    df_out$Batch <- factor(batch_labels)
  }
  # Add rownames to df (needed for clust_eval)
  rownames(df) <- 1:length(rownames(df))


  return(df_out)
}
