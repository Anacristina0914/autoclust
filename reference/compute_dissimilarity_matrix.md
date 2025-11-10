# Compute and Visualize a Dissimilarity Matrix

Computes a dissimilarity matrix using the
[`daisy`](https://rdrr.io/pkg/cluster/man/daisy.html) function and
optionally visualizes it using a heatmap.

## Usage

``` r
compute_dissimilarity_matrix(
  x,
  metric = c("euclidean", "manhattan", "gower"),
  stand = FALSE,
  return_dis = FALSE,
  plot_dissim = TRUE,
  heatmap_title = paste0(metric, " Dissimilarity Matrix"),
  heatmap_axis_text_size = 4,
  plot_title_size = 20,
  save_plots = FALSE,
  plot_outpath = "."
)
```

## Arguments

- x:

  A data matrix or data frame.

- metric:

  Character. Dissimilarity metric to use: `"euclidean"`, `"manhattan"`,
  or `"gower"`.

- stand:

  Logical. Whether to standardize variables (used for non-Gower
  metrics).

- return_dis:

  Logical. If `TRUE`, returns the dissimilarity object; else returns a
  matrix.

- plot_dissim:

  Logical. Whether to generate a dissimilarity heatmap.

- heatmap_title:

  Character. Title of the heatmap plot.

- heatmap_axis_text_size:

  Numeric. Axis text size for the heatmap.

- plot_title_size:

  Numeric. Plot title font size.

- save_plots:

  Logical. If `TRUE`, saves the heatmap.

- plot_outpath:

  Character. Directory where the plot is saved.

## Value

A list containing:

- dissimilarity_matrix:

  Either a dissimilarity object or a matrix, depending on `return_dis`.

- heatmap_plot:

  (Optional) ggplot2 heatmap plot of the dissimilarity matrix.
