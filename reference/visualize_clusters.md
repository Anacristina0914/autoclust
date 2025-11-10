# Visualize Clustering Results with t-SNE and UMAP

Visualizes cluster assignments from a PAM model using t-SNE and UMAP
dimensionality reduction techniques. UMAP is computed for several
combinations of `n_neighbors` and `min_dist` values. The function
returns plots and optionally saves them.

## Usage

``` r
visualize_clusters(
  x,
  pam_fit,
  df,
  id_col,
  umap_nneight = c(10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L),
  umap_min_dist = c(0.1, 0.25, 0.3, 0.5, 0.75, 0.99),
  point_size = 2,
  stability_col = "stability",
  save_plots = FALSE,
  plot_outpath = "."
)
```

## Arguments

- x:

  A dissimilarity matrix (must be of class "dissimilarity").

- pam_fit:

  A fitted PAM object (e.g., from
  [`cluster::pam()`](https://rdrr.io/pkg/cluster/man/pam.html)).

- df:

  Data frame containing metadata including IDs and stability scores.

- id_col:

  Character. Name of the ID column in `df`.

- umap_nneight:

  Integer vector. Values for the UMAP `n_neighbors` parameter.

- umap_min_dist:

  Numeric vector. Values for the UMAP `min_dist` parameter.

- point_size:

  Numeric. Size of points in the plots. Default is 2.

- stability_col:

  Character. Column name in `df` that stores cluster stability metrics.

- save_plots:

  Logical. If `TRUE`, plots are saved to `plot_outpath`.

- plot_outpath:

  Character. Directory path for saving plots. Default is current
  directory.

## Value

A list with:

- tsne_plot:

  A ggplot2 object visualizing clusters via t-SNE.

- umap_plots:

  A nested list of ggplot2 UMAP plots indexed by neighbor and distance
  parameters.
