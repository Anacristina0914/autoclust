# Compute Bootstrap Clustering Stability

Computes clustering stability using bootstrap resampling and Jaccard
similarity based on the
[`clusterboot`](https://rdrr.io/pkg/fpc/man/clusterboot.html) function.
Returns the bootstrap mean Jaccard index for each cluster and a bar plot
summarizing cluster stability.

## Usage

``` r
compute_bootstrap_stability(
  x,
  nclust,
  plot_dir,
  plots_suffix,
  B = 100,
  bootmethod = "boot",
  seed = 123,
  clustermethod = NULL,
  diss = TRUE,
  theme = theme_minimal(),
  save_plot = FALSE
)
```

## Arguments

- x:

  A dissimilarity matrix (if `diss = TRUE`) or data matrix (if
  `diss = FALSE`).

- nclust:

  Integer. The number of clusters to evaluate.

- plot_dir:

  Character. Directory where plots will be saved.

- plots_suffix:

  Character. Suffix used for naming output plots.

- B:

  Integer. Number of bootstrap resampling iterations. Default is 100.

- bootmethod:

  Character. Resampling method. Typically `"boot"` or `"subset"`.

- seed:

  Integer. Random seed for reproducibility. Default is 123.

- clustermethod:

  A clustering function in the format expected by
  [`fpc::clusterboot`](https://rdrr.io/pkg/fpc/man/clusterboot.html).

- diss:

  Logical. Whether `x` is a dissimilarity matrix (`TRUE`) or a data
  matrix (`FALSE`).

- theme:

  A ggplot2 theme object to customize the stability plot. Default is
  `theme_minimal()`.

- save_plot:

  Logical. Whether to save the plot as a JPEG file. Default is `FALSE`.

## Value

A list with the following components:

- stats:

  The full result from
  [`fpc::clusterboot`](https://rdrr.io/pkg/fpc/man/clusterboot.html).

- plot:

  A ggplot2 object showing the mean Jaccard similarity per cluster.

If `save_plot = TRUE`, the plot is also saved as a JPEG file in the
specified directory.
