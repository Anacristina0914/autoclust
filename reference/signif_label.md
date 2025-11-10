# Convert p-value to significance label

Returns a conventional significance label (e.g., `"*"`, `"**"`, `"***"`,
or `"ns"`) based on the provided p-value.

## Usage

``` r
signif_label(pval)
```

## Arguments

- pval:

  A numeric p-value.

## Value

A character string representing the significance level:

- `"***"` for p \< 0.001

- `"**"` for p ≤ 0.01

- `"*"` for p ≤ 0.05

- `"ns"` (not significant) otherwise

## Examples

``` r
signif_label(0.0005)  # returns "***"
#> [1] "***"
signif_label(0.02)    # returns "*"
#> [1] "*"
signif_label(0.3)     # returns "ns"
#> [1] "ns"
```
