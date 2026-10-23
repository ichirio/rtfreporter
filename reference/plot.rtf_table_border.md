# Visualise an `rtf_table_border`

Draws a schematic table with the four zones (header, body, first_row,
last_row) coloured to show which one provides which border.

## Usage

``` r
# S3 method for class 'rtf_table_border'
plot(x, ...)
```

## Arguments

- x:

  An
  [`rtf_table_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_border.md)
  object.

- ...:

  Unused.

## Value

Invisibly returns `x`.

## Examples

``` r
plot(rtf_border_tfl())          # preview the clinical TFL border zones
#> Warning: `rtf_border_tfl()` is deprecated: the clinical TFL rules are already reachable as
#>   `rtftable(border = "tfl")`, and as a reusable value from `rtf_table_style_tfl()`.
```
