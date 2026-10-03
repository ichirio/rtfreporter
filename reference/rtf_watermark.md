# Build a page watermark

Builds the `watermark` setting for
[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
– a diagonal word such as `"DRAFT"` or `"CONFIDENTIAL"` drawn behind the
page body, on every page.

## Usage

``` r
rtf_watermark(
  text,
  font_size_half_points = 144L,
  color = "#C8C8C8",
  angle = -45,
  font = NULL,
  width_in = 6,
  height_in = 2
)
```

## Arguments

- text:

  The watermark word. A zero-length or empty string means no watermark.

- font_size_half_points:

  Size in half-points, as everywhere else in the package (`144L` = 72
  pt, the default).

- color:

  Fill colour as `"#RRGGBB"`. The default is a light grey that stays
  readable behind body text.

- angle:

  Rotation in degrees, counter-clockwise negative. `-45` (the default)
  is the conventional bottom-left to top-right diagonal.

- font:

  Font family. `NULL` (default) uses the document font.

- width_in, height_in:

  Size of the shape's bounding box in inches. The defaults suit a single
  word on a Letter/A4 page; a long phrase needs a wider box.

## Value

An `rtf_watermark` object for `rtf_document(watermark = )`.

## Details

The watermark is written into the section header, so it repeats per page
and stays scoped to its section: concatenating with
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
cannot leak one document's watermark into the next. A section can
override the document-wide setting by passing `watermark` in its
`rtf_section(secinfo = )`, including `watermark = NA` to turn it off for
that section alone.

## See also

[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md),
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md),
[`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md).

## Examples

``` r
rtf_watermark("DRAFT")
#> $text
#> [1] "DRAFT"
#> 
#> $font_size_half_points
#> [1] 144
#> 
#> $color
#> [1] "#C8C8C8"
#> 
#> $angle
#> [1] -45
#> 
#> $font
#> NULL
#> 
#> $width_in
#> [1] 6
#> 
#> $height_in
#> [1] 2
#> 
#> attr(,"class")
#> [1] "rtf_watermark"

# Bigger, redder, horizontal:
rtf_watermark("DO NOT DISTRIBUTE", font_size_half_points = 96L,
              color = "#E8B4B4", angle = 0, width_in = 7)
#> $text
#> [1] "DO NOT DISTRIBUTE"
#> 
#> $font_size_half_points
#> [1] 96
#> 
#> $color
#> [1] "#E8B4B4"
#> 
#> $angle
#> [1] 0
#> 
#> $font
#> NULL
#> 
#> $width_in
#> [1] 7
#> 
#> $height_in
#> [1] 2
#> 
#> attr(,"class")
#> [1] "rtf_watermark"
```
