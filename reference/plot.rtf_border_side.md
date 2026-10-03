# Visualise an `rtf_border_side`

Draws a 1cm-wide swatch of the line in the side's own style, width and
colour. Useful for previewing the exact look of a border before it is
baked into an RTF document.

## Usage

``` r
# S3 method for class 'rtf_border_side'
plot(x, ...)
```

## Arguments

- x:

  A border side object.

- ...:

  Unused.

## Value

Invisibly returns `x`.

## Examples

``` r
plot(rtf_border_side(style = "double", width = 30L, color = "#003366"))
```
