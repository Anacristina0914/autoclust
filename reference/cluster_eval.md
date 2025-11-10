# Evaluate Clustering Performance Using Mixed Data

Performs comprehensive evaluation of clustering performance using
silhouette scores, consensus clustering, and bootstrap stability.
Supports both continuous and binary data and multiple clustering
algorithms.

## Usage

``` r
cluster_eval(
  x,
  ndim = 10,
  clust_method = "pam",
  distance = "auto",
  data_type = "binary",
  sillplot_dot_size = 6,
  save_plot = TRUE,
  nclust = NA,
  interactive = FALSE,
  boot_runs = 100,
  seed = 123,
  compute_consensus = TRUE,
  compute_jaccard = TRUE,
  consensus_runs = 100,
  stand = FALSE,
  plots_suffix = paste0(distance, "_", clust_method),
  plots_dir = getwd()
)
```

## Arguments

- x:

  A data frame or matrix of observations (rows = samples, columns =
  features). Can include binary or continuous variables.

- ndim:

  Integer. Maximum number of clusters to evaluate (default = 10).

- clust_method:

  Clustering method to use. Options are `"pam"`, `"kmeans"`, `"clara"`,
  or `"hclust"`.

- distance:

  Distance metric to use. `"auto"` (default) uses `"gower"` for binary
  data and `"euclidean"` for continuous. Other options can be passed to
  [`cluster::daisy()`](https://rdrr.io/pkg/cluster/man/daisy.html),
  e.g., `"manhattan"`, `"gower"`, `"euclidean"`.

- data_type:

  Type of data: `"binary"` or `"continuous"`. Used only when
  `distance = "auto"`.

- sillplot_dot_size:

  Size of dots in silhouette width plot (default = 6).

- save_plot:

  Logical. Save plots to disk? (default = `TRUE`).

- nclust:

  Optional. Number of clusters to use for evaluation. If `NA`, selects
  best k based on silhouette score (default = `NA`).

- interactive:

  Logical. If `TRUE`, returns interactive silhouette plots using
  `plotly` (default = `FALSE`).

- boot_runs:

  Integer. Number of bootstrap resamples for Jaccard stability index
  (default = 100).

- seed:

  Random seed for reproducibility (default = 123).

- compute_consensus:

  Logical. If `TRUE`, performs consensus clustering (default = `TRUE`).

- compute_jaccard:

  Logical. If `TRUE`, computes bootstrap-based Jaccard index for cluster
  stability (default = `TRUE`).

- consensus_runs:

  Integer. Number of resamples for consensus clustering (default = 100).

- plots_suffix:

  Character string added to filenames when saving plots (default =
  `paste0(distance, "_", clust_method)`).

- plots_dir:

  Directory where plots are saved (default = current working directory).

## Value

A list with the following elements:

- silhouette_plots:

  A list of silhouette plots for each evaluated number of clusters, and
  a summary plot.

- fit:

  Fitted clustering objects for each k.

- bootstrap:

  Bootstrap cluster stability results for each k (if
  `compute_jaccard = TRUE`).

- consensus_clustering:

  ConsensusClusterPlus results and ICL scores (if
  `compute_consensus = TRUE`).

- best_nclust:

  The optimal number of clusters based on average silhouette width.

- method:

  The clustering method used.

## Details

This function streamlines the evaluation of clustering strategies on
binary or continuous datasets using silhouette scores, consensus
clustering, and cluster stability (via bootstrapped Jaccard index).
Useful for benchmarking clustering algorithms and selecting optimal k.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- generate_synthetic_mixed_data(n_samples = 300, n_clusters = 3,
n_continuous = 4, n_binary = 4)
x <- data[, -ncol(data)]  # remove true cluster labels
result <- cluster_eval(x, data_type = "binary", clust_method = "pam", distance = "gower")
result$best_nclust
} # }
```
