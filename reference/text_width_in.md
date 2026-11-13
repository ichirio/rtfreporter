# Estimate the display width of a text string

Returns an estimated display width in **inches** for a character string
rendered in the given font and size. For Courier New (monospace) every
character is one width. For Arial and Times New Roman (proportional)
each character has its own width, from the font metrics R ships
(grDevices' AFM files: Helvetica, whose widths are Arial's, and
Times-Roman): an `i` is narrower than a `W`. Characters outside
printable ASCII take the width of an `n`.

## Usage

``` r
text_width_in(text, font = NULL, font_size_half_points = 18L)
```

## Arguments

- text:

  A character vector. `NA` is treated as `""`.

- font:

  Font name: `"Courier New"` / `"courier_new"`, `"Courier"`, `"Arial"`
  (or `"Helvetica"`) or `"Times New Roman"` (or `"Times"`); case, spaces
  and hyphens do not matter. `NULL` (default) is the package's font,
  `getOption("rtfreporter.font")` ("Courier New"), as for every other
  `font =`. Unrecognised values fall back to Courier New.

- font_size_half_points:

  Font size in **half-points** (the unit used by RTF and the document's
  `rtf_style(font_size_half_points = )`). Default `18` = 9 pt.

## Value

A numeric vector of estimated widths in inches (same length as `text`).

## Examples

``` r
text_width_in("Hello, World!")          # ~0.88 inches at 9pt Courier New
#> [1] 0.9777083
text_width_in("abc", font_size_half_points = 24)   # 12pt
#> [1] 0.3008333
```
