# Transform Autoantibody Data for Line Plot

Prepares a long-format data frame showing the percentage frequency of
positive autoantibody results per subgroup, suitable for plotting with
`ggplot2`.

## Usage

``` r
transform_data_for_line_plot(
  df,
  autoantibody_columns,
  subgroup_column = "Clustering"
)
```

## Arguments

- df:

  A data frame containing the autoantibody data and subgroup
  information.

- autoantibody_columns:

  A character vector of column names or an integer vector of column
  positions corresponding to the autoantibody variables to include.

- subgroup_column:

  A character string or integer indicating the subgroup column to group
  by. Defaults to `"Clustering"`.

## Value

A long-format data frame with the following columns:

- Subgroup column:

  The grouping variable (e.g., Clustering).

- Autoantibody:

  The name of the autoantibody.

- Frequency:

  The percentage of positive results within each subgroup.

## Details

The function:

1.  Extracts the specified autoantibody columns and the subgroup column.

2.  Converts the autoantibody binary values to numeric (if needed).

3.  Calculates the sum of positives per subgroup.

4.  Divides by the subgroup size to obtain the percentage positivity.

5.  Returns the data in a melted long format suitable for plotting.

## See also

[`reshape2::melt()`](https://rdrr.io/pkg/reshape2/man/melt.html),
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage
df <- your_data_frame
autoantibody_vars <- c("anti-dsDNA", "anti-Sm", "anti-RNP")
freq_long <- transform_data_for_line_plot(df, autoantibody_columns = autoantibody_vars)

library(ggplot2)
ggplot(freq_long, aes(x = Autoantibody, y = Frequency, group = Clustering, color = Clustering)) +
  geom_line() + geom_point()
} # }
```
