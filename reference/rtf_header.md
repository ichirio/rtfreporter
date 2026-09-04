# Create a header or footer object for a section

`rtf_header()` and `rtf_footer()` create structured header/footer
objects that can be passed to
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md).
Use
[`update_header_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
/
[`update_footer_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
to add or replace individual rows after creation.

## Usage

``` r
rtf_header(
  rows,
  border = NULL,
  width_twips = NULL,
  width = NULL,
  row_height_twips = NULL,
  font_size_half_points = NULL,
  font = NULL,
  markup = NULL,
  cell_padding_left_twips = NULL,
  cell_padding_right_twips = NULL
)

rtf_footer(
  rows,
  border = rtf_border(top = TRUE),
  width_twips = NULL,
  width = NULL,
  row_height_twips = NULL,
  font_size_half_points = NULL,
  font = NULL,
  markup = NULL,
  cell_padding_left_twips = NULL,
  cell_padding_right_twips = NULL
)
```

## Arguments

- rows:

  The header (or footer) content, row by row: a single named character
  vector for one row, or a `list` of them for several rows. Within a
  row, the name of each element chooses its column:

  `l`

  :   left-aligned text

  `c`

  :   centred text

  `r`

  :   right-aligned text

  e.g. `c(l = "Protocol XYZ-001", r = "Page {AUTO_PAGE}")`. Cell text
  may contain **page-number tokens** that the renderer substitutes:
  `{AUTO_PAGE}` (the current page, updated live by the viewer),
  `{AUTO_TOTAL_PAGES}` (the document total), and `{PAGE}` /
  `{TOTAL_PAGES}` (static numbers baked in at render time).

- border:

  An
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  object controlling the border applied to all rows of the header/footer
  table. `NULL` = no border (default for header). Use
  `rtf_border(top = TRUE)` for a horizontal dividing line (default for
  footer).

- width_twips:

  Integer. Table width in twips. `NULL` (default) uses the full writable
  width (page width minus margins).

- width:

  Band width in the shared vocabulary: `"page"` (the writable width, the
  default), a fraction in `(0, 1]` of it, or twips. `width_twips` is the
  older absolute-only form and wins when both are given.

- row_height_twips:

  Integer. Row height in twips. `NULL` (default) reads the value from
  `inst/resources/rtfreporter_defaults.R`.

- font_size_half_points:

  Font size for this band, in half-points. `NULL` (default) inherits the
  document size. Setting it also recomputes the row height from that
  size unless `row_height_twips` is given.

- font:

  Font family for this band, e.g. `"Arial"`. `NULL` (default) uses the
  document font. A family not already in the document's `font_table` is
  added to it automatically, the way a colour is.

- markup:

  Cell-text markup for this band; `NULL` (default) inherits the document
  setting. See
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).

- cell_padding_left_twips, cell_padding_right_twips:

  Integer cell padding on the left / right side of each header (or
  footer) cell, matching the content-table convention. `NULL` (default)
  reads from `inst/resources/rtfreporter_defaults.R` (0L for both since
  v0.0.21).

## Value

A named list with elements `rows`, `border`, `width_twips`, and
`row_height_twips`.

## Examples

``` r
hdr <- rtf_header(
  rows = list(
    c(l = "Protocol: RTF-101", r = "ACME Pharma"),
    c(l = "Table 14.1.1",     r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
  )
)
ftr <- rtf_footer(c(l = "Confidential"))

hdr <- update_header_row(hdr, row = 3, content = c(c = "Draft"))
```
