# Make line plot for autoantibody frequencies per subgroup

Make line plot for autoantibody frequencies per subgroup

## Usage

``` r
make_aa_lineplot(
  x,
  aa_colnames,
  subgroup_column,
  subgroup_colors = distinctColorPalette(k = length(unique(x %>%
    pull(subgroup_column)))),
  line_width = 1.5,
  geom_point = 2,
  plot_theme = autoclust::theme_clustering(text_size = 8, plot_title_size = 18),
  ...
)
```

## Arguments

- x:

  Data frame with autoantibody and subgroup columns.

- aa_colnames:

  Character vector of antibody columns.

- subgroup_column:

  Name of the column defining subgroups.

- subgroup_colors:

  Optional custom colors.

- line_width:

  Line thickness.

- geom_point:

  Point size.

- plot_theme:

  Plot theme (default: autoclust theme).

- ...:

  Additional arguments passed to ggplot2::labs.

## Value

A ggplot2 object showing antibody frequencies.
