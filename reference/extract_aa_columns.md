# Extract Autoantibody and Subgroup Columns

Extracts a subset of a data frame containing specified autoantibody
columns and a subgroup column. Supports input as either column names or
column positions.

## Usage

``` r
extract_aa_columns(data_frame, autoantibody_columns, subgroup_column)
```

## Arguments

- data_frame:

  A data frame containing autoantibody measurements and subgroup
  information.

- autoantibody_columns:

  A character vector of column names or an integer vector of column
  positions corresponding to the autoantibody variables to extract.

- subgroup_column:

  A character string or integer indicating the subgroup column to
  extract.

## Value

A list with three elements:

- aa_data:

  A data frame containing only the selected autoantibody columns and the
  subgroup column.

- antibody_cols:

  The column positions (indices) of the selected autoantibody columns.

- subgroup_col_number:

  The column position (index) of the selected subgroup column.

## Details

This function checks the type of `autoantibody_columns` and
`subgroup_column`:

- If character, it matches them to the column names.

- If integer, it uses them directly as positions.

If the input is neither character nor integer, an error is thrown.

## See also

[`base::subset()`](https://rdrr.io/r/base/subset.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage
df <- your_data_frame
autoantibody_vars <- c("anti-dsDNA", "anti-Sm", "anti-RNP")
result <- extract_aa_columns(df, autoantibody_columns = autoantibody_vars,
subgroup_column = "Clustering")
head(result[[1]]) # View extracted data
} # }
```
