# Add content pages to document

Append one or more content items as pages. **Each element of `tables`
becomes exactly one page**, holding a single table or figure.

## Usage

``` r
rtf_tables(
  doc,
  tables,
  col_header = NULL,
  col_header_align = NULL,
  spanning_header = NULL,
  col_spec = NULL,
  row_title = NULL,
  border = "tfl",
  blank_rows = NULL,
  read_attributes = TRUE,
  style = NULL,
  col_rel_width = NULL,
  column_widths_twips = NULL,
  table_width_twips = NULL,
  table_width_pct_of_writable = NULL,
  table_width_pct = NULL,
  table_align = "left",
  row_height_twips = NULL,
  row_height_exact = FALSE,
  header_row_height_twips = NULL,
  blank_row_height_twips = NULL,
  cell_padding_left_twips = NULL,
  cell_padding_right_twips = NULL,
  cell_valign = "bottom",
  blank_row_normalize = c("detect", "collapse"),
  markup = NULL,
  font_size_half_points = NULL,
  font = NULL,
  titles = NULL,
  footnotes = NULL,
  auto_section = FALSE,
  section_label_align = "left",
  auto_title = FALSE,
  title_label_align = "left",
  read_gt = FALSE
)
```

## Arguments

- doc:

  An rtf_document object.

- tables:

  A list where each element is one page's content (a single content per
  page). Each element is one of:

  a `data.frame`

  :   a simple table; the table-format arguments below apply to it.

  an `rtftable` object

  :   (from
      [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md))
      a table with full formatting – usually the output of
      [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).

  an `rtfplot` object

  :   (from
      [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md))
      an embedded figure.

  a `gt_tbl` object

  :   (from the gt package) treated like a `data.frame`; pass
      `read_gt = TRUE` (or a token vector) to also pull through gt's
      column labels, alignment, title / subtitle and source notes (see
      `read_gt`).

  a gtsummary table

  :   (`tbl_summary`, `tbl_regression`, ...) auto-converted to a
      `gt_tbl` first; `read_gt = TRUE` pulls through its labels, titles,
      source notes, footnotes and spanning headers.

  Note: cell-level formatting (row indentation, bold group-header rows,
  footnote marks in cells) is **not** transferred to RTF. See
  [`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md)
  for details on gtsummary limitations.

- col_header, col_header_align, spanning_header, col_spec, row_title,
  border, blank_rows, read_attributes, style:

  Per-table content settings applied to bare `data.frame` elements.
  `row_title` names the row-heading columns (default: column 1) and sets
  the per-column default alignment (heading columns left, others
  centre). See
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  for details.

- col_rel_width, column_widths_twips, table_width_twips,
  table_width_pct_of_writable, table_width_pct, table_align:

  Column-width and table-width settings applied to bare `data.frame`
  elements. See
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  for details.

- row_height_twips, row_height_exact, header_row_height_twips,
  blank_row_height_twips:

  Row-height settings applied to bare `data.frame` elements. See
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  for details.

- cell_padding_left_twips, cell_padding_right_twips, cell_valign:

  Cell layout settings applied to bare `data.frame` elements. See
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  for details.

- blank_row_normalize:

  Blank-row normalisation applied to bare `data.frame` elements (default
  `c("detect", "collapse")`): `"detect"` renders an all-empty data row
  as a single full-width blank row, `"collapse"` reduces a run of
  consecutive blank rows to one. Pre-built
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  pages keep their own setting. See
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  for details.

- markup:

  Cell-text markup applied to bare `data.frame` elements (`"script"`
  super/subscript, `"relational"` `>=`/`<=` symbols, `"all"`, `"none"`).
  `NULL` (default) inherits the document default (`"script"`). Pre-built
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  pages keep their own setting. See
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).

- font_size_half_points, font:

  Typography for these tables, in half-points and as a family name. Both
  **override** whatever a pre-built
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  carries, so a table can be built once and placed in documents of
  different sizes. Passing a size without `row_height_twips` recomputes
  the row height from it, exactly as on
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).

- titles:

  `NULL` (default) or a list of length `length(tables)` **or length 1**
  (a single block applied to every page). Each element is a title
  **block**: either a character vector (one entry per row, default
  styling) or a list of rows, where a row is a string or a styled
  `list(text=, align=, bold=, italic=, underline=, color=, border=)`. An
  empty string (`""`) is a blank row. The block renders as a
  single-column table the same width as the content (so it lines up);
  title rows default to centred + bold.

- footnotes:

  `NULL` (default) or a list of length `length(tables)` or length 1
  (common to all). Same block structure as `titles`; footnote rows
  default to left-aligned, and the first row carries a top rule (the
  separator) unless that row sets its own `border`.

- auto_section:

  Logical. When `TRUE` and `tables` is a **named** list, each name is
  used as a per-section heading appended to the common header defined by
  `rtf_section(secinfo = ...)` (called without a `page` argument). The
  document is then automatically split into one RTF section per named
  element. Unnamed items fall through to the previous section. Default
  `FALSE`.

- section_label_align:

  Alignment for the auto-appended section label row. One of `"left"`
  (default), `"center"`, or `"right"`.

- auto_title:

  Logical. When `TRUE` and `tables` is a **named** list, each name is
  appended as the **last row of that table's title block**, so it prints
  immediately above the table rather than in the page header. Titles
  already present – from `titles =`, or carried on the object as the
  `rtf_titles` attribute by
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  – are kept and the name goes after them; a table with no titles gets
  the name as its whole block. Unnamed items are untouched. Behaves
  identically under either `title_format`. Composes with `auto_section`,
  which puts the same name in the section header instead. Default
  `FALSE`.

- title_label_align:

  Alignment for the auto-appended title label row. One of `"left"`
  (default), `"center"`, or `"right"`.

- read_gt:

  **Legacy.** Controls metadata extraction when a raw `gt_tbl` is handed
  *directly* to `rtf_tables()` (no pagination). For new code, prefer
  converting up front with
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
  which paginates *and* reads metadata, then pass the resulting list
  here – the page-level titles / footnotes flow through automatically
  (via the `rtf_titles` / `rtf_footnotes` attributes) with `read_gt`
  left at its default. Allowed values:

  `FALSE` (default)

  :   treat `gt_tbl` items as a rendered body only; ignore titles /
      labels / source notes.

  `TRUE`

  :   read the render-relevant metadata: column labels, per-column
      alignment, spanning headers, widths, plus the page-level title /
      subtitle and footnote / source notes. See
      [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
      for the full *What is carried, by source* table.

  a character vector of tokens

  :   selective opt-in. See
      [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
      for the token list.

  Explicit `rtf_tables()` /
  [`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)
  /
  [`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md)
  values always override gt-extracted ones.

## Value

Modified rtf_document with appended contents.

## Details

Table-formatting arguments (`col_rel_width`, `border`,
`row_height_twips`, ...) accepted by this function are used to build any
bare `data.frame` element of `tables`, and – when passed **explicitly**
– also override the matching field of any pre-built
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
element (for example the output of
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).
Arguments left at their default are not applied to pre-built tables, so
those keep their own / gt-derived settings.
[`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
elements are never modified. (The `style` argument seeds
construction-time defaults only; it is not applied as an override to a
pre-built table.)

## Examples

``` r
# Two clinical tables in one document, each on its own page with its own
# title; shared TFL borders and a wide row-label column are applied to both
# bare data.frames, and a footnote is attached to the first page only.
t1 <- data.frame(Parameter = c("Age (years)", "Sex, n (%)"),
                 Value = c("75.1 (8.2)", "120 (53%)"))
t2 <- data.frame(Parameter = c("Weight (kg)", "Height (cm)"),
                 Value = c("78.0 (12.1)", "170 (9.5)"))

doc <- rtf_document() |>
  rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
  rtf_tables(
    list(t1, t2),
    border        = "tfl",
    col_rel_width = c(2, 1),
    titles    = list("Table 14.1.1", "Table 14.1.2"),
    footnotes = list("Source: ADSL", NULL)
  )
f <- tempfile(fileext = ".rtf")
generate_rtfreport(doc, f, overwrite = TRUE)
unlink(f)
```
