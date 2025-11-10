# Run Association Analyses Between Clinical Manifestations (outcome) and a predictor variable (e.g. patient subgroup)

This function performs a series of (generalized) linear regression
models to test associations between one or more clinical manifestations
and a categorical predictor variable (e.g., subgroup or cluster),
optionally adjusting for covariates such as age and sex. It supports
logistic regression (default) and returns model objects, ANOVA tables,
and a combined summary of odds ratios, confidence intervals, and
p-values. Optionally, it can generate a forest plot of the top
associations.

## Usage

``` r
run_associations(
  clinical_data_df,
  clinical_data_cols,
  predictor_var_colname = "subgroup",
  adjust_by = "Age_dx + male.female_dx",
  family = binomial,
  link = "logit",
  plot = FALSE,
  y.axis.text.size = 12,
  or.label.size = 3,
  ntop = 10,
  order = FALSE,
  adjust_p = TRUE,
  p_adj_method = "BH",
  add_xylabs = FALSE,
  name_mapping = NULL,
  x.axis.text.size = 8,
  pt.size = 2,
  filter_sig_OR = TRUE,
  forest_plot_title = "Associations"
)
```

## Arguments

- clinical_data_df:

  A data frame containing clinical variables (outcomes), the predictor
  variable, and optional adjustment covariates.

- clinical_data_cols:

  Character or numeric vector specifying the column names or indices of
  the clinical manifestations (outcome variables) to test.

- predictor_var_colname:

  Character string specifying the name of the predictor variable column
  (default: `"subgroup"`).

- adjust_by:

  Character string defining adjustment variables in formula format
  (e.g., `"Age_dx + male.female_dx"`). Can include multiple covariates
  separated by `"+"`.

- family:

  A family function for the regression (default: `binomial`).

- link:

  Character string specifying the link function (default: `"logit"`).

- plot:

  Logical; if `TRUE`, generates a forest plot of the top associations
  (default: `FALSE`).

- y.axis.text.size:

  Numeric; text size for the y-axis in the forest plot (default: `12`).

- or.label.size:

  Numeric; text size for the odds ratio labels in the forest plot
  (default: `3`).

- ntop:

  Integer; number of top associations to include in the forest plot
  (default: `10`).

- order:

  Logical; if `TRUE`, orders the output data frame by p-value (default:
  `FALSE`).

- adjust_p:

  Logical; whether to perform multiple testing correction (default:
  `TRUE`).

- p_adj_method:

  Character; method for p-value adjustment (default: `"BH"` for
  Benjamini–Hochberg).

- add_xylabs:

  Logical; whether to add x and y axis labels to the forest plot
  (default: `FALSE`).

- name_mapping:

  Optional named vector or data frame for mapping variable names to
  human-readable labels in the plot (default: `NULL`).

- x.axis.text.size:

  Numeric; text size for the x-axis in the forest plot (default: `8`).

- pt.size:

  Numeric; point size in the forest plot (default: `2`).

- filter_sig_OR:

  Logical; if `TRUE`, filters forest plot to only show significant
  associations (based on adjusted p-values) (default: `TRUE`).

- forest_plot_title:

  Character string; title for the forest plot (default:
  `"Associations"`).

## Value

A named list containing:

- `models` — Nested list of fitted model objects per group and outcome.

- `anova` — Nested list of ANOVA tables for each model.

- `summarized_results` — Combined data frame of model estimates, odds
  ratios, confidence intervals, p-values, and (if enabled) adjusted
  p-values.

- `df` — Input data frame with additional binary indicator columns for
  each predictor group.

- `forest_plot` — (Optional) ggplot object with a forest plot of top
  associations.

## Details

For each clinical manifestation and each level of the predictor
variable, the function fits a model of the form: \$\$outcome ~
predictor_level + adjustment_covariates\$\$ where `predictor_level` is a
binary variable indicating membership in a given subgroup. Odds ratios,
95% confidence intervals, and p-values are extracted from each model.
P-values can optionally be adjusted for multiple testing using the
specified method.

## See also

[`glm()`](https://rdrr.io/r/stats/glm.html),
[`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html),
[`make_forest_plot()`](make_forest_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
results <- run_associations(
  clinical_data_df = lupus_df,
  clinical_data_cols = c("nephritis", "arthritis", "rash"),
  predictor_var_colname = "cluster",
  adjust_by = "Age_dx + sex",
  plot = TRUE,
  ntop = 15
)
} # }
```
