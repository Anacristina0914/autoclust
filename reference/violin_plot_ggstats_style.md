# Create a stylized violin plot with jittered points and boxplots

This function creates a violin plot overlaid with jittered data points
and a boxplot, using a consistent visual style suitable for statistical
reporting. It also supports custom axis labels, titles, captions, and
color palettes.

## Usage

``` r
violin_plot_ggstats_style(
  data,
  x,
  y,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  title = NULL,
  subtitle = NULL,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha =
    0.4, size = 3, stroke = 0, na.rm = TRUE),
  boxplot.args = list(width = 0.3, alpha = 0.2, na.rm = TRUE),
  violin.args = list(width = 0.5, alpha = 0.2, na.rm = TRUE),
  color_values = NULL
)
```

## Arguments

- data:

  A data frame containing the variables to be plotted.

- x:

  A categorical variable (unquoted) to be used on the x-axis.

- y:

  A numeric variable (unquoted) to be used on the y-axis.

- xlab:

  Optional custom label for the x-axis. Defaults to the variable name.

- ylab:

  Optional custom label for the y-axis. Defaults to the variable name.

- caption:

  Optional caption to be displayed at the bottom of the plot.

- title:

  Optional main title of the plot.

- subtitle:

  Optional subtitle of the plot.

- point.args:

  A list of arguments passed to
  [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  to control jittered point appearance.

- boxplot.args:

  A list of arguments passed to
  [`geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html)
  to control boxplot appearance.

- violin.args:

  A list of arguments passed to
  [`geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html)
  to control violin plot appearance.

- color_values:

  Optional named vector of colors used to manually set fill and point
  colors (e.g., `c("Group1" = "blue", "Group2" = "red")`).

## Value

A `ggplot` object representing the combined violin-box-jitter plot.

## Examples

``` r
if (FALSE) { # \dontrun{
  library(ggplot2)
  violin_plot_ggstats_style(antibody_dataset, y = `HLA PRS 4 SNP`, x = Subgroup,
                             xlab = "HLA PRS", ylab = "Subgroups",
                             title = "Distribution of HLA PRA across subgroups",
                             color_values = acr_subgroups_paper_colors())
} # }
```
