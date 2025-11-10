# Retrieves KI Colors

Returns a named vector of Karolinska Institutet's palette.

## Usage

``` r
ki_colors(color_palette = "primary", palette_for_printing = FALSE)
```

## Arguments

- color_palette:

  Color palette names can be set to primary or functional (default:
  `"primary"`)

- print_palette:

  To display color for printing or web designs (default: `"FALSE"`)

## Value

A named vector of hex color codes.

## Examples

``` r
ki_colors = ki_colors(color_palette = "primary", palette_for_printing = FALSE)
#> Error in ki_colors(color_palette = "primary", palette_for_printing = FALSE): could not find function "ki_colors"
```
