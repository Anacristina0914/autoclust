# Run UMAP with Custom Parameters

This function performs Uniform Manifold Approximation and Projection
(UMAP) on the input data, allowing customization of UMAP parameters,
such as the number of neighbors, minimum distance, and the number of
training epochs.

## Usage

``` r
run_custom_umap(data, neighbors, min_dist, n_epochs = 200)
```

## Arguments

- data:

  A matrix or dataframe of numeric values where rows represent samples
  and columns represent features.

- neighbors:

  An integer specifying the number of neighbors to consider for each
  point in the UMAP algorithm. Higher values result in capturing broader
  patterns, while lower values capture finer details.

- min_dist:

  A numeric value indicating the minimum distance between points in the
  UMAP embedding. Smaller values keep similar points close, while larger
  values increase separation.

- n_epochs:

  An integer specifying the number of training epochs for UMAP. The
  default value is 200. Higher values may improve stability of the
  embedding at the cost of computation time.

## Value

A dataframe with two columns (`UMAP1`, `UMAP2`) representing the UMAP
embedding coordinates for each sample.

## Details

The function allows for flexible UMAP configurations to be used with
distance-based input. It leverages `umap.defaults` as a base and
overrides specific parameters based on function inputs.

## Examples

``` r
# Example usage:
# Assuming `data_matrix` is a matrix of distances or a high-dimensional dataset:
run_custom_umap(data = data_matrix, neighbors = 15, min_dist = 0.1, n_epochs = 300)
#> Error: object 'data_matrix' not found
```
