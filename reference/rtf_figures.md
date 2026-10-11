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
  footnotes = NULL
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
  `rtf_figures(doc, plot)` – and several as a list.

- width_twips:

  Display width in twips for bare paths. `NULL` = full writable width.

- height_twips:

  Display height in twips for bare paths. `NULL` = derived from the
  image's aspect ratio.

- align:

  Horizontal alignment for bare paths: `"center"` (default), `"left"`,
  or `"right"`.

- titles, footnotes:

  Optional lists of length `length(figures)` or length 1 (common to all
  figures). See
  [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
  for the block structure (character vectors or per-row styled lists).

## Value

Modified rtf_document with appended figure contents.

## Examples

``` r
png_path <- tempfile(fileext = ".png")
grDevices::png(png_path, width = 600, height = 400)
graphics::plot(1:10, main = "A figure")
grDevices::dev.off()
#> agg_record_1e5936ffec40 
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
