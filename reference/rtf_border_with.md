# Return a copy of an `rtf_border` with selected sides replaced

Non-mutating: returns a new `rtf_border` with the supplied side(s) set
on top of `border`. `NULL` arguments leave the corresponding side
unchanged.

## Usage

``` r
rtf_border_with(
  border,
  top = NULL,
  bottom = NULL,
  left = NULL,
  right = NULL,
  inside_h = NULL,
  inside_v = NULL
)
```

## Arguments

- border:

  An
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  object. `NULL` is accepted and treated as an empty border.

- top, bottom, left, right:

  Replacement side values, or `NULL` to leave a side unchanged.

- inside_h, inside_v:

  Replacement side values for the between-rows and between-cells rules,
  or `NULL` to leave them unchanged.

## Value

A new `rtf_border` object.

## Examples

``` r
b <- rtf_border(top = TRUE, bottom = TRUE)
rtf_border(top = b$top, bottom = rtf_border_side(color = "#003366"))
#> <rtf_border>
#>   top     : single, 15 twips
#>   bottom  : single, 15 twips, color=#003366
#>   left    : none
#>   right   : none
rtf_border(top = "none", bottom = b$bottom)   # an explicit no-line on top
#> <rtf_border>
#>   top     : none, 15 twips
#>   bottom  : single, 15 twips
#>   left    : none
#>   right   : none
```
