# Custom ggplot2 theme for subgroups SLE

Returns a `ggplot2` theme object with a tailored style optimized for
clustering or publication-quality plots. Allows full customization of
font family, size, color, and positioning for all plot elements.

## Usage

``` r
theme_clustering(
  text_family = "sans",
  text_size = 8,
  text_color = "black",
  plot_title_family = "Lobster Two",
  plot_title_size = 20,
  plot_title_face = "bold",
  plot_title_color = "#2a475e",
  axis_title_size = 12,
  axis_text_size = 10,
  axis_text_color = "black",
  caption_text_color = "black",
  caption_text_size = 8,
  caption_face_style = "bold",
  legend_text_size = 12,
  legend_text_color = "black",
  legend_title_face_style = "bold",
  legend_position = "top"
)
```

## Arguments

- text_family:

  Font family for general plot text.

- text_size:

  Base text size.

- text_color:

  Color for general plot text.

- plot_title_family:

  Font family for the plot title.

- plot_title_size:

  Font size for the plot title.

- plot_title_face:

  Font face for the plot title (e.g., `"bold"`, `"italic"`).

- plot_title_color:

  Color for the plot title.

- axis_title_size:

  Font size for axis titles.

- axis_text_size:

  Font size for axis tick labels.

- axis_text_color:

  Color for axis tick labels.

- caption_text_color:

  Color of the caption text.

- caption_text_size:

  Size of the caption text.

- caption_face_style:

  Face style of the caption text (e.g., `"bold"`).

- legend_text_size:

  Font size for legend text.

- legend_text_color:

  Color of legend text.

- legend_title_face_style:

  Face style for the legend title (e.g., `"bold"`).

- legend_position:

  Position of the legend (e.g., `"top"`, `"bottom"`, `"none"`).

## Value

A `ggplot2` theme object to be added to a plot.

## Examples

``` r
library(ggplot2)
ggplot(antibody_dataset, aes(x = Subgroup, y = `HLA PRS 4 SNP`)) +
  geom_boxplot() +
  theme_clustering()
#> Error: object 'antibody_dataset' not found
```
