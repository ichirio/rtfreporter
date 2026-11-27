# Assign content footnotes to pages

Same shape as
[`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md):
a list with one block per content (table or figure page; or length 1,
common to all). Each block is a character vector or a list of rows (a
string, a named vector `c(l = , c = , r = )`, or the page footer's row
list `list(columns = , row_height_twips = )`, whose `row_height_twips`
is that row's height over the block's; it takes `border =` and, as a
band row does, `bold =`, `italic =` and `underline =` (each `TRUE` or
`FALSE`) for every cell of the row). Footnote rows default to
left-aligned and draw no rule unless `border` (or a row's own `border`)
asks for one. `NULL` per element suppresses the footnote for that
content.

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

  A list of length = number of contents, or length 1 (common). Each
  content's element is a list of rows. A row is a single string, or a
  named vector `c(l = , c = , r = )` giving up to three cells positioned
  left / centre / right – the same row model
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  uses. A bare string lands in whichever slot `align` selects.

- font_size_half_points, row_height_twips, markup, align, font:

  Style for this block, over the document's
  [`rtf_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_style.md)
  (its section, then its shared level). Anything left `NULL` is
  inherited.

  `font`

  :   names a family (e.g. `"Arial"`); one not already in the document's
      `font_table` is added to it automatically, the way a colour is. A
      named vector gives one family per script slot (`latin`,
      `east_asian`, `complex`, optional `hansi`), e.g.
      `c(latin = "Times New Roman", east_asian = "MS Mincho")`; see
      [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md).

  `font_size_half_points`, `row_height_twips`

  :   resolve **together**: a size given without a height recomputes the
      height from that size rather than inheriting one chosen for a
      different size, and an explicit height always wins.

  `align`

  :   sets the block's default row alignment; a per-row `align` still
      beats it.

- border:

  `NULL` (default) for **no rule** – unlike the page footer, the
  footnote draws none unless asked. An
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  is the border of the block as a whole, read as
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)'s
  is: `top` above the first row, `bottom` below the last row, `inside_h`
  between rows, `left` / `right` on the outer edges of every row,
  `inside_v` between the cells of a row; e.g.
  `border = rtf_border(top = TRUE)` for a separator rule above the
  block. A row given as a list may carry its own `border =`
  (`list(columns = , border = )`), merged side by side over the block's.
  It is drawn only with `rtf_style(footnote_format = "table")` (the
  default); the text form ignores it and warns.

## Value

Modified rtf_document.

## Examples

``` r
df <- data.frame(A = 1:2, B = c("x", "y"))
doc <- rtf_document() |>
  rtf_tables(df) |>
  rtf_footnotes(list(c("Source: ADaM ADSL")))
```
