# Retrieve Colors for ACR Subgroups and Other Variables

Returns a named vector of colors associated with subgroups, sex, and
case-control status.

## Usage

``` r
acr_subgroups_paper_colors(
  sg1_var_name = "1",
  sg2_var_name = "2",
  sg3_var_name = "3",
  sg4_var_name = "4",
  fem_var_name = "Female",
  mas_var_name = "Male",
  case_var_name = "case",
  control_var_name = "control"
)
```

## Arguments

- sg1_var_name:

  Name for subgroup 1 (default: `"1"`)

- sg2_var_name:

  Name for subgroup 2 (default: `"2"`)

- sg3_var_name:

  Name for subgroup 3 (default: `"3"`)

- sg4_var_name:

  Name for subgroup 4 (default: `"4"`)

- fem_var_name:

  Name for Female (default: `"Female"`)

- mas_var_name:

  Name for Male (default: `"Male"`)

- case_var_name:

  Name for Cases (default: `"case"`)

- control_var_name:

  Name for Controls (default: `"control"`)

## Value

A named vector of hex color codes.

## Examples

``` r
subgroup_colors = acr_subgroups_paper_colors()
#> Error in acr_subgroups_paper_colors(): could not find function "acr_subgroups_paper_colors"
```
