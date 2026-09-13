# Assign content footnotes to pages

Same shape as
[`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md):
a list with one block per page (or length 1, common to all). Each block
is a character vector or a list of rows (a string or
`list(text=, align=, bold=, italic=, underline=, color=, border=)`).
Footnote rows default to left-aligned, and the first row carries a top
rule (the separator) unless that row sets its own `border`. `NULL` per
element suppresses the footnote for that page.

## Usage

``` r
rtf_footnotes(
  doc,
  footnotes,
  font_size_half_points = NULL,
  row_height_twips = NULL,
  markup = NULL,
  font = NULL,
  align = NULL,
  border = NULL
)
```

## Arguments

- doc:

  An rtf_document object.

- footnotes:

  A list of length = number of pages, or length 1 (common). Each page's
  element is a list of rows. A row is a single string, or a named vector
  `c(l = , c = , r = )` giving up to three cells positioned left /
  centre / right – the same row model
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  uses. A bare string lands in whichever slot `align` selects.

- font_size_half_points, row_height_twips, markup, align, font:

  Style for this block, overriding the document default from
  [`rtf_default_format()`](https://ichirio.github.io/rtfreporter/reference/rtf_default_format.md).
  `font` names a family (e.g. `"Arial"`); one not already in the
  document's `font_table` is added to it automatically, the way a colour
  is. Anything left `NULL` is inherited. Font size and row height
  resolve **together**: a size given without a height recomputes the
  height from that size rather than inheriting one chosen for a
  different size, and an explicit height always wins. `align` sets the
  block's default row alignment; a per-row `align` still beats it.

- border:

  `NULL` (default) for **no rule** – unlike the page footer, the
  footnote draws none unless asked. An
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  puts one on the first row, e.g. `border = rtf_border(top = TRUE)`.

## Value

Modified rtf_document.

## Examples

``` r
df <- data.frame(A = 1:2, B = c("x", "y"))
doc <- rtf_document() |>
  rtf_tables(df) |>
  rtf_footnotes(list(c("Source: ADaM ADSL")))
```
