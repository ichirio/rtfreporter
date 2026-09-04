# Visualise an `rtf_border`

Draws a 1cm square showing which of the four edges (top, bottom, left,
right) carry a border, in each side's own style.

## Usage

``` r
# S3 method for class 'rtf_border'
plot(x, ...)
```

## Arguments

- x:

  An
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  object.

- ...:

  Unused.

## Value

Invisibly returns `x`.

## Examples

``` r
plot(rtf_border(top = TRUE, bottom = rtf_border_side(color = "#003366")))
```
