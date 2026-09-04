# Create an RTF figure object

Embeds a PNG or JPEG image into the RTF output. The result can be passed
directly to
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
in a pipe chain.

## Usage

``` r
rtfplot(path, width_twips = NULL, height_twips = NULL, align = "center")
```

## Arguments

- path:

  Path to a PNG or JPEG image file.

- width_twips:

  Display width in twips. `NULL` (default) uses the image's **native
  size at its embedded DPI** (100% scale). If only `height_twips` is
  given, the width is derived from the native aspect ratio.

- height_twips:

  Display height in twips. `NULL` (default) uses the native size at the
  image's DPI, or – when `width_twips` is given – the height derived
  from the native aspect ratio.

- align:

  Horizontal alignment: `"center"` (default), `"left"`, or `"right"`.

## Value

An `rtfplot` (S3) object suitable for use in
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md).

## Details

The native size is read from the file's resolution metadata (PNG `pHYs`
chunk, JPEG JFIF density). A 2500 x 1438 px image saved at 300 DPI
therefore embeds at 2500/300 x 1438/300 in. When the file carries no
DPI, the `rtfreporter.figure.default_dpi` option (factory `96`) is
assumed. There is no automatic page-fit cap; give an explicit
`width_twips` to shrink a figure that is wider than the page.

## Examples

``` r
if (FALSE) { # \dontrun{
fig <- rtfplot("scatter.png", width_twips = 9000L)

doc <- rtf_document() %>%
  rtf_section(page = 1, secinfo = list(
    header = rtf_header(rows = list(c(l = "Figure 14.1")))
  )) %>%
  rtf_tables(list(fig))

generate_rtfreport(doc, "output.rtf", overwrite = TRUE)
} # }
```
