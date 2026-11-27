# Create a header or footer object for a section

`rtf_header()` and `rtf_footer()` create structured header/footer
objects that can be passed to
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md).

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
  cell_padding_right_twips = NULL,
  cell_padding_top_twips = NULL,
  cell_padding_bottom_twips = NULL,
  drop_empty_rows = FALSE,
  color = NULL
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
  cell_padding_right_twips = NULL,
  cell_padding_top_twips = NULL,
  cell_padding_bottom_twips = NULL,
  drop_empty_rows = FALSE,
  color = NULL
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

  e.g. `c(l = "Protocol XYZ-001", r = "Page {AUTO_PAGE}")`. A row may
  also be a list,
  `list(columns = c(l = , c = , r = ), row_height_twips = 400L, color = "#C00000", border = rtf_border(bottom = TRUE), bold = TRUE)`,
  whose `row_height_twips` is that row's height (over the band's
  `row_height_twips`), `color` its text colour (over the band's
  `color`), `border` its own
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  (see `border`), and `bold`, `italic` and `underline` (each `TRUE` or
  `FALSE`, default `FALSE`) the emphasis of every cell of the row, the
  words a title row uses
  ([`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)).
  Tokens and `^{}` / `_{}` markup work inside them. A value that is not
  one `TRUE` / `FALSE` is an error naming the row. Cell text may contain
  **page-number tokens** that the renderer substitutes: `{AUTO_PAGE}`
  (the current page, updated live by the viewer), `{AUTO_TOTAL_PAGES}`
  (the document total), and `{PAGE}` / `{TOTAL_PAGES}` (static numbers
  baked in at render time). A table paginated on a column
  (`as_rtftables(page_by = )`) gives each page its value of that column,
  written `{<column name>}` (`{PARAM}`): the band then shows the page's
  value, and is written for each page. See *Page values* in
  [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md).
  The run tokens (`{PROGRAM}`, `{DATETIME}`), the `{BOOK_PAGE}` slot and
  tokens of one's own
  ([`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)`(tokens = )`)
  are filled too
  ([`rtf_text_tokens()`](https://ichirio.github.io/rtfreporter/reference/rtf_text_tokens.md)
  lists them); a token's value is written as it is, and an unknown token
  is printed as written. `{{` and `}}` print a literal `{` and `}` (as
  in a column header), so `"{{PAGE}}"` prints `{PAGE}` and fills
  nothing. With `markup = "script"`, the braces of `^{...}` / `_{...}`
  are the markup's own: `^{STUDY}` raises the word STUDY. To raise a
  token's value, write the token inside the braces, `^{{STUDY}}`, or
  after a character, `^{n={STUDY}}`.

- border:

  An
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  for the band as a whole, read as a table zone's is: `top` above the
  first row, `bottom` below the last row, `inside_h` between rows,
  `left` / `right` on the outer edges of every row, `inside_v` between
  the cells of a row (`all` is the four outer edges). The rows are the
  ones written, after `drop_empty_rows`. A row given as a list may carry
  its own `border =`, merged side by side over the band's, so the sides
  it sets win; it applies to that row's outer edges and its `inside_v`.
  A line both of two rows draw between them is drawn once, as the upper
  row's bottom. `NULL` = no border (default for the header);
  `rtf_border(top = TRUE)`, a rule above the band, is the footer's
  default.

- width_twips:

  Integer. Table width in twips. `NULL` (default) uses the full writable
  width (page width minus margins); so does `0`, as
  `rtftable(table_width_twips = 0)` does, since a band of no width
  cannot be read (`width`, when given, then sets the width). A value
  that is not one non-negative integer is an error.

- width:

  Band width in the shared vocabulary: `"page"` (the writable width, the
  default), a fraction in `(0, 1]` of it, or twips (a whole number above
  1; `1.5` or `Inf` is an error). `width_twips` is the older
  absolute-only form and wins when both are given.

- row_height_twips:

  Integer. Row height in twips. `NULL` (default) inherits the document's
  row height (its
  [`rtf_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_style.md),
  else the `rtfreporter.row_height_twips` option), else the height the
  band's font size implies. A value that is not one non-negative integer
  is an error.

- font_size_half_points:

  Font size for this band, in half-points. `NULL` (default) inherits the
  document size. Setting it also recomputes the row height from that
  size unless `row_height_twips` is given.

- font:

  Font family for this band, e.g. `"Arial"`. `NULL` (default) uses the
  document font. A family not already in the document's `font_table` is
  added to it automatically, the way a colour is. A named vector gives
  one family per script slot (`latin`, `east_asian`, `complex`, optional
  `hansi`), e.g.
  `c(latin = "Times New Roman", east_asian = "MS Mincho")`; see
  [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md).

- markup:

  Cell-text markup for this band; `NULL` (default) inherits the document
  setting. See
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).

- cell_padding_left_twips, cell_padding_right_twips:

  Integer cell padding on the left / right side of each header (or
  footer) cell, matching the content-table convention. `NULL` (default)
  inherits the document's
  [`rtf_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_style.md),
  else the resource baseline (0). A value that is not one non-negative
  integer is an error.

- cell_padding_top_twips, cell_padding_bottom_twips:

  Integer cell padding above / below the text of each header (or footer)
  cell, in twips, written into the cell definitions. `NULL` (default)
  inherits the document's
  [`rtf_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_style.md),
  else the resource baseline (0); 0 writes nothing.

- drop_empty_rows:

  Logical. `TRUE`: a row whose tokens of one's own
  (`rtf_document(tokens = )`, `options(rtfreporter.tokens = )`) are all
  empty when the file is written, and which says nothing else but blanks
  and brackets (`"<{POPULATION}>"`), is left out. One header can then
  serve every report of a study, a report with no value for a line going
  without it. `FALSE` (default): every row is written.

- color:

  The band's text colour, a 6-digit hex string such as `"#7F7F7F"`;
  `NULL` (default) leaves the text in the document's colour (black). A
  row given as `list(columns = , color = )` has its own. The colour is
  added to the document's colour table.

## Value

A named list of the band's settings, one element per argument (`rows`,
`border`, `width_twips`, `width`, `row_height_twips`, ...), for
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)`(header = , footer = )`.

## Examples

``` r
hdr <- rtf_header(
  rows = list(
    c(l = "Protocol: RTF-101", r = "ACME Pharma"),
    c(l = "Table 14.1.1",     r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
  )
)
ftr <- rtf_footer(c(l = "Confidential"))
```
