# Plot Most Similar and Most Dissimilar Pairs from a Dissimilarity Matrix

This function visualizes the most similar and most dissimilar pairs of
individuals based on a dissimilarity matrix. It returns an interactive
heatmap showing the dissimilarity values, with the most similar pair
highlighted in blue and the most dissimilar pair highlighted in red.

## Usage

``` r
plot_most_similar_dissimilar(x, df, id_col)
```

## Arguments

- x:

  A dissimilarity matrix or an object of class `dissimilarity`.

- df:

  A dataframe containing individual IDs.

- id_col:

  A string specifying the column in `df` with individual IDs.

## Value

A `ggplotly` object displaying the heatmap of the dissimilarity matrix.
The most similar and most dissimilar pairs are highlighted.

## Examples

``` r
# Assuming `dissim_matrix` is your dissimilarity matrix and `id_df` is your ID dataframe
# plot_most_similar_dissimilar(dissim_matrix, id_df, "ID")
```
