# One style object for every module of a document

`rtf_style()` gathers the look of a document – font, size, row height,
cell padding, markup, the header / footer colour – in one object, given
to
[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
or
[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
as `style = `. It is an **addition**: every style argument of
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
[`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md),
[`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md),
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
and
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
keeps working, and wins over the style. A study standard is written
once, here; a report changes what it needs with the module's own
argument.

## Usage

``` r
rtf_style(
  font = NULL,
  font_size_half_points = NULL,
  row_height_twips = NULL,
  cell_padding_left_twips = NULL,
  cell_padding_right_twips = NULL,
  cell_padding_top_twips = NULL,
  cell_padding_bottom_twips = NULL,
  markup = NULL,
  color = NULL,
  title_format = NULL,
  footnote_format = NULL,
  title_width = NULL,
  footnote_width = NULL,
  table = NULL,
  titles = NULL,
  footnotes = NULL,
  header = NULL,
  footer = NULL,
  ...
)
```

## Arguments

- font:

  Font family for the text of every module, e.g. `"Arial"`; a named
  vector gives one family per script slot (see
  [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)).
  A family not in the document's `font_table` is added to it.

- font_size_half_points:

  The document's font size, in half-points (`18` = 9 pt).

- row_height_twips:

  The row height of every table-shaped element (content table, title /
  footnote, header / footer), in twips.

- cell_padding_left_twips, cell_padding_right_twips,
  cell_padding_top_twips, cell_padding_bottom_twips:

  The cell padding of every table-shaped element, in twips.

- markup:

  Cell-text markup: `"script"`, `"relational"`, `"all"` or `"none"` (see
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)).

- color:

  Text colour, a 6-digit hex string such as `"#7F7F7F"`. For now the
  shared colour reaches the page header and footer only.

- title_format, footnote_format:

  How the title / footnote block is written: `"text"` (plain paragraphs)
  or `"table"` (a single-column table). The defaults are `"text"` for
  the title and `"table"` for the footnote. A `border` on a title or
  footnote needs the `"table"` form; the `"text"` form ignores it and
  warns.

- title_width, footnote_width:

  The width of the title / footnote block: `"content"` (the table's),
  `"page"` (the writable width), a fraction in `(0, 1]` of the writable
  width, or twips (a whole number above 1).

- table:

  A named list of
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  arguments for every content table: `font`, `font_size_half_points`,
  `row_height_twips`, `row_height_exact`, `header_row_height_twips`,
  `blank_row_height_twips`, the four `cell_padding_*_twips`,
  `cell_valign`, `table_align`, `markup`.

- titles:

  A named list of
  [`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)'s
  style arguments: `font`, `font_size_half_points`, `row_height_twips`,
  `markup`, `align`, `border`.

- footnotes:

  A named list of
  [`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md)'s
  style arguments: `font`, `font_size_half_points`, `row_height_twips`,
  `markup`, `align`, `border`.

- header, footer:

  A named list of
  [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)'s
  /
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)'s
  style arguments: `font`, `font_size_half_points`, `row_height_twips`,
  `markup`, the four `cell_padding_*_twips`, `border`, `color`.

- ...:

  Not used: an argument not named above is an error.

## Value

An `rtf_style` object for `rtf_document(style = )` /
`rtf_config(style = )`. Every argument left `NULL` is unset.

## Details

The **shared** arguments reach every module that has the argument. A
**section** (`table`, `titles`, `footnotes`, `header`, `footer`) is a
named list that reaches its module only, under that module's own
argument names; a section key beats the shared one. The order, highest
first:

**the module's own argument \> the `rtf_style()` section \> the
`rtf_style()` shared level \> the `rtfreporter.*` option \> the resource
baseline.**

A section key reaches an element only where the element left the
argument unset: `NULL`, or, for
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
`table_align`, `cell_valign` and `row_height_exact` and for a band's
`border`, not passed. As for the module arguments, a font size and a row
height resolve together: an element that sets its own size and no height
takes no height from the style, its height following its size. A table
is cut into pages
([`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md))
before the style reaches it, so a section's row height does not change
where the pages break.

Row height, cell padding, vertical alignment and font size apply to the
whole table (the body, and the header for its own settings); they are
not set per row or per cell.

Several `rtf_style()` objects given through
[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
merge key by key, the later winning.

## See also

[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md),
[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md),
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md).

## Examples

``` r
study <- rtf_style(
  font = "Arial", font_size_half_points = 18L,
  cell_padding_left_twips = 40L, cell_padding_right_twips = 40L,
  table     = list(header_row_height_twips = 260L, cell_valign = "center"),
  header    = list(color = "#7F7F7F"),
  footnotes = list(font_size_half_points = 16L)
)
study
#> <rtf_style>
#>   font = "Arial"; font_size_half_points = 18; cell_padding_left_twips =
#>     40; cell_padding_right_twips = 40
#>   table: header_row_height_twips = 260; cell_valign = "center"
#>   footnotes: font_size_half_points = 16
#>   header: color = "#7F7F7F"

adsl_tbl <- data.frame(Parameter = c("Age, Mean (SD)", "Sex: Female, n (%)"),
                       Placebo = c("75.2 (8.6)", "53 (61.6)"),
                       Active  = c("74.4 (7.9)", "50 (59.5)"))
doc <- rtf_document(style = study) |>
  rtf_tables(as_rtftables(adsl_tbl), titles = "Table 14.1.1") |>
  rtf_footnotes("Source: ADSL", font_size_half_points = 14L) |> # this report: 7 pt wins
  rtf_section(header = rtf_header(c(l = "Study XYZ", r = "Page {AUTO_PAGE}")))
f <- tempfile(fileext = ".rtf")
generate_rtfreport(doc, f)
unlink(f)

# A later style merges key by key over the document's:
doc <- rtf_config(doc, style = rtf_style(table = list(cell_valign = "top")))
```
