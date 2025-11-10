# Generate Synthetic Clustered Datasets

Simulates clustered datasets (binary or continuous) with optional batch
effects. Useful for testing and benchmarking clustering algorithms.

## Usage

``` r
generate_synthetic_clusters(
  n_samples = 100,
  n_vars = 10,
  n_clusters = 3,
  n_batches = 1,
  data_type = c("binary", "continuous"),
  cluster_sep = 1,
  flip_prob = 0.05,
  random_state = NULL
)
```

## Arguments

- n_samples:

  Integer. Total number of samples.

- n_vars:

  Integer. Number of features (variables).

- n_clusters:

  Integer. Number of clusters to generate.

- n_batches:

  Integer. Number of batches to simulate. If 0, no batch effect is
  added.

- data_type:

  Character. Either `"continuous"` or `"binary"`.

- cluster_sep:

  Numeric. Controls separation between cluster centers.

- df:

  Optional reference dataframe. If provided, sampling and perturbation
  is based on this data.

- noise_sd:

  Numeric. Standard deviation of Gaussian noise (only for continuous
  data).

- noise_prob:

  Numeric. Probability of flipping bits (only for binary data).

- batch_effect_strength:

  Numeric. Controls strength of batch effect.

- seed:

  Integer. Random seed for reproducibility.

## Value

A data frame with synthetic data and a `Cluster` column. If
`n_batches > 0`, includes a `Batch` column.
