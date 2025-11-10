# Plot UMAP across different neighbors and min_distance values.

Plot UMAP across different neighbors and min_distance values.

## Usage

``` r
plot_custom_umap(umap_coords, clusters, neighbors, min_dist, pt.size = 0.5)
```

## Arguments

- umap_coords:

  umap coordinates for plots.

- clusters:

  vector containing clusters for each point.

- neighbors:

  number of neighbors used to compute umap coords.

- min_dist:

  minimum distance used to compute umap coords.

- pt.size:

  point size for umap plot.

## Value

a ggplot object of UMAP plot.

## Examples

``` r
# Example of how to use the function
umap_plot <- plot_custom_umap(umap_coords = coords, neighbors = 10,
min_dist = 0.4, pt.size = 1)
#> Error in plot_custom_umap(umap_coords = coords, neighbors = 10, min_dist = 0.4,     pt.size = 1): argument "clusters" is missing, with no default
```
