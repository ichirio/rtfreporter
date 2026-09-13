# Estimate the display width of a text string

Returns an estimated display width in **inches** for a character string
rendered in the given font and size. For Courier New (monospace) the
estimate is reliable; for proportional fonts (Arial) it is an
average-width approximation.

## Usage

``` r
text_width_in(text, font = "courier_new", size_half_points = 18L)
```

## Arguments

- text:

  A character vector. `NA` is treated as `""`.

- font:

  Font name. One of `"courier_new"` (default), `"courier"`, or
  `"arial"`. Unrecognised values fall back to Courier New.

- size_half_points:

  Font size in **half-points** (the unit used by RTF and the document's
  `default_format$font_size_half_points`). Default `18` = 9 pt.

## Value

A numeric vector of estimated widths in inches (same length as `text`).

## Examples

``` r
text_width_in("Hello, World!")          # ~0.88 inches at 9pt Courier New
#> [1] 0.9777083
text_width_in("abc", size_half_points = 24)   # 12pt
#> [1] 0.3008333
```
