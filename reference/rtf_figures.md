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

  A list whose elements are character file paths to image files
  (PNG/JPEG), plot objects (a ggplot2 plot, a grob, a recorded base
  plot, a function that draws – see
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)),
  or pre-built `rtfplot` objects.

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
if (FALSE) { # \dontrun{
doc <- rtf_document() |>
  rtf_figures(list("scatter.png"), width_twips = 6000L, align = "center")
} # }
```
