# Create a Forest Plot of Association Results

This function generates a publication-ready forest plot summarizing the
odds ratios (ORs), 95% confidence intervals, and p-values (or adjusted
p-values) from association analyses, such as those produced by
[`run_associations()`](run_associations.md). It filters, ranks, and
visualizes the most significant associations across clinical
manifestations and predictor variables.

## Usage

``` r
make_forest_plot(
  results_df,
  clinical_manifestation,
  predictor_var_colname,
  adjust_vars,
  clinical_manifestation_colname = "clinical_manifestation",
  ntop = NULL,
  adjust_p = adjust_p,
  p_adj_method = p_adj_method,
  pt.size = pt.size,
  y.axis.text.size,
  x.axis.text.size,
  or.label.size,
  plot_title = "",
  name_mapping = NULL,
  add_xylabs = FALSE,
  filter_sig_OR = TRUE
)
```

## Arguments

- results_df:

  A data frame containing association results, including columns for
  `OR`, `2.5 %`, `97.5 %`, p-values (e.g., `Pr\>|z\|`), and optionally
  adjusted p-values (`adj_p`).

- clinical_manifestation:

  Character vector of clinical manifestation names to include in the
  plot.

- predictor_var_colname:

  Character string specifying the column name for the predictor
  variable.

- adjust_vars:

  Character vector or string of adjustment variables used in the model
  (e.g., `"Age_dx + sex"`). Used to remove adjusted terms from the
  plotted results.

- clinical_manifestation_colname:

  Character string indicating the column name in `results_df`
  corresponding to the clinical manifestation variable (default:
  `"clinical_manifestation"`).

- ntop:

  Integer specifying the number of top associations (by OR) to include
  in the plot (default: `NULL`, plots all).

- adjust_p:

  Logical indicating whether adjusted p-values (`adj_p`) should be used
  for coloring points (default: inherits from calling environment).

- p_adj_method:

  Character string specifying the p-value adjustment method (default:
  inherits from calling environment).

- pt.size:

  Numeric value controlling point size in the forest plot.

- y.axis.text.size:

  Numeric value specifying text size for the y-axis labels.

- x.axis.text.size:

  Numeric value specifying text size for the x-axis labels.

- or.label.size:

  Numeric value controlling the size of the odds ratio text labels.

- plot_title:

  Character string specifying the plot title (default: `""`).

- name_mapping:

  Optional named vector or list mapping variable names to human-readable
  labels.

- add_xylabs:

  Logical; if `TRUE`, adds axis labels and title to the plot (default:
  `FALSE`).

- filter_sig_OR:

  Logical; if `TRUE`, filters results to include only significant ORs
  (i.e., 95% CI does not cross 1) (default: `TRUE`).

## Value

A `ggplot2` object representing the forest plot, ready for further
customization or saving.

## Details

The function removes intercepts and adjustment variables from the
results before plotting. If `filter_sig_OR = TRUE`, only terms with 95%
confidence intervals entirely above or below 1 are included.
Associations are colored by their p-value or adjusted p-value, depending
on user settings. The x-axis represents the odds ratio (OR) with 95%
confidence intervals, and the y-axis shows the clinical manifestation
and predictor variable combinations.

When `name_mapping` is provided, both the clinical manifestation and
predictor names are replaced by human-readable labels. This is
particularly useful for figure preparation in publications.

## See also

[`run_associations()`](run_associations.md),
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)

## Examples

``` r
if (FALSE) { # \dontrun{
forest <- make_forest_plot(
  results_df = association_results,
  clinical_manifestation = c("arthritis", "nephritis"),
  predictor_var_colname = "cluster",
  adjust_vars = "Age_dx + sex",
  ntop = 10,
  adjust_p = TRUE,
  pt.size = 2,
  y.axis.text.size = 10,
  x.axis.text.size = 8,
  or.label.size = 3,
  plot_title = "Top 10 Cluster Associations",
  add_xylabs = TRUE
)
print(forest)
} # }
```
