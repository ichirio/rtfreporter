# Create an RTF figure object

Embeds a figure into the RTF output – a PNG or JPEG file, or a plot
object drawn here and then embedded. The result can be passed directly
to
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
or
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
in a pipe chain.

## Usage

``` r
rtfplot(
  x,
  width_twips = NULL,
  height_twips = NULL,
  align = "center",
  render_width = 6.5,
  render_height = 4.5,
  render_dpi = 300
)
```

## Arguments

- x:

  A figure. Either the path to a **PNG or JPEG** file (RTF's `\pngblip`
  and `\jpegblip`; no other format is emitted), or a plot object to
  draw:

  - anything that draws when printed – a **ggplot2** plot, a **lattice**
    trellis object, a **patchwork**;

  - a **grid** grob or `gtable`, drawn with
    [`grid::grid.draw()`](https://rdrr.io/r/grid/grid.draw.html);

  - a base plot recorded with
    [`grDevices::recordPlot()`](https://rdrr.io/r/grDevices/recordplot.html);

  - a **function of no arguments** that draws – the escape hatch for
    anything not covered above.

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

- render_width, render_height:

  Size **in inches** to draw a plot object at (default 6.5 x 4.5, which
  fits a portrait letter page). Because the drawn PNG records its own
  resolution, this is also the size the figure takes on the page unless
  `width_twips`/`height_twips` say otherwise. Refused for a file, which
  has its size already.

- render_dpi:

  Resolution to draw a plot object at (default `300`). It decides how
  sharp the figure is, **not** how big: a plot drawn at 600 dpi occupies
  the same inches on the page as one drawn at 150.

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
