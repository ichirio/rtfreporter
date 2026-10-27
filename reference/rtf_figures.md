# Add figure content to document

Append one or more figures as content pages – image files (PNG/JPEG) or
plot objects. Each figure creates one new page. Display dimensions and
alignment apply to every path and plot object in `figures`; elements
already constructed via
[`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
keep their own settings.

## Usage

``` r
rtf_figures(
  doc,
  figures,
  width_twips = NULL,
  height_twips = NULL,
  align = "center",
  titles = NULL,
  footnotes = NULL,
  page_break = TRUE,
  space_before = 0L,
  render_width = NULL,
  render_height = NULL,
  render_dpi = NULL,
  png_device = NULL
)
```

## Arguments

- doc:

  An rtf_document object.

- figures:

  A figure, or a list of them: a character file path to an image file
  (PNG/JPEG), a plot object (a ggplot2 plot, a patchwork, a grob, a
  recorded base plot, a function that draws – see
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)),
  or a pre-built `rtfplot`. One figure is given as it is –
  `rtf_figures(doc, plot)` – and several as a list. A plot object is
  drawn with `png_device` (default: the `rtfreporter.png_device` option,
  which follows
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html):
  ragg when installed,
  [`grDevices::png()`](https://rdrr.io/r/grDevices/png.html) otherwise);
  build an
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
  to choose per figure.

- width_twips:

  Display width in twips of a figure that is not an `rtfplot` already (a
  file or a plot object), as
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
  takes it. `NULL` (default): the image's native size – a file at its
  embedded DPI, a plot object at its `render_width` – with no fit to the
  page; give a width to shrink a figure wider than the page.

- height_twips:

  Display height in twips, as
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
  takes it. `NULL`: the native height, or – with `width_twips` – the
  height the image's aspect ratio gives.

- align:

  Horizontal alignment for bare paths: `"center"` (default), `"left"`,
  or `"right"`.

- titles, footnotes:

  Optional lists of length `length(figures)` or length 1 (common to all
  figures). See
  [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
  for the block structure (character vectors or per-row styled lists).

- page_break:

  `TRUE` (the default): each figure begins a new page. `FALSE`: a figure
  goes on under the content before it; nothing is measured – one that
  does not fit what is left of the page, the reader moves to the next,
  its title left behind. One value for every figure, or one each; the
  document's first content always begins a page. As
  [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)'s
  `page_break`.

- space_before:

  With `page_break = FALSE`, empty lines between the content before and
  this figure.

- render_width, render_height, render_dpi, png_device:

  How to draw the **plot objects** in `figures`, as
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
  takes them: size in inches, resolution, and PNG device. `NULL`
  (default) is
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)'s
  own default (6.5 x 4.5 in, 300 dpi, the `rtfreporter.png_device`
  option). A file path ignores them (a file has its size already), and a
  figure given as an
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
  keeps the settings it was built with.

## Value

Modified rtf_document with appended figure contents.

## Examples

``` r
png_path <- tempfile(fileext = ".png")
grDevices::png(png_path, width = 600, height = 400)
graphics::plot(1:10, main = "A figure")
grDevices::dev.off()
#> agg_record_2073307f13a3 
#>                       2 
doc <- rtf_document() |>
  rtf_figures(list(png_path), width_twips = 6000L, align = "center")

# one figure, as it is: a path, or a plot object
doc <- rtf_document() |> rtf_figures(png_path)
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  doc <- rtf_document() |> rtf_figures(p)
}
# }
```
