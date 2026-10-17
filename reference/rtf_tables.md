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

  The pages to add. The usual forms are:

  a `table_plan`

  :   (from
      [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md))
      the plan of an ARD or a data.frame; it becomes its pages, as
      [`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md)
      would make them.

  the output of [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)

  :   the pages of a gt, gtsummary, rtables/tern, rlistings, flextable
      or huxtable table, or of a `data.frame` paged and styled there.

  a bare `data.frame`

  :   one simple page; the table-format arguments below apply to it.

  More generally, `tables` is one page or a list of pages, **each
  element one page** with a single content: a `data.frame`, an
  `rtftable` (from
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  or
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)),
  an `rtfplot` (from
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)),
  or a `gt_tbl` / gtsummary table. A `gt_tbl` or gtsummary table is best
  passed through
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
  which reads its labels, alignment, titles and source notes; placed
  here directly it is treated like a `data.frame` unless `read_gt` is
  set (see `read_gt`).

  `tables` may also be a single object of a class another package has
  registered an
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  method for; it is converted to its pages first (see "Supplying a
  source from another package" in
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).

- col_header, col_header_align, spanning_header, col_spec, row_title,
  border, blank_rows, read_attributes, style, col_rel_width,
  column_widths_twips, table_width_twips, table_width_pct_of_writable,
  table_width_pct, table_align, row_height_twips, row_height_exact,
  header_row_height_twips, blank_row_height_twips,
  cell_padding_left_twips, cell_padding_right_twips, cell_valign,
  blank_row_normalize, markup, font_size_half_points, font:

  **Deprecated** (removed in 0.9.0): the same arguments of
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
  written there (or in
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  /
  [`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md))
  instead. See "Deprecated arguments" below.

- titles:

  `NULL` (default) or a list of length `length(tables)` **or length 1**
  (a single block applied to every page). Each element is a title
  **block**: either a character vector (one entry per row, default
  styling) or a list of rows, where a row is a string or a styled
  `list(text=, align=, bold=, italic=, underline=, color=, border=)`. An
  empty string (`""`) is a blank row. The block renders as a
  single-column table the same width as the content (so it lines up);
  title rows default to centred + bold.

  Title and footnote text may carry the same **page-number tokens** as a
  header or footer band – `{AUTO_PAGE}`, `{AUTO_TOTAL_PAGES}`, `{PAGE}`,
  `{TOTAL_PAGES}` – which is how a per-table "Page 1 of 3" sits next to
  the table instead of at the page margin. A static `{PAGE}` costs
  nothing here: these bands are re-emitted for every page, so each page
  bakes its own number without the extra RTF section the same token
  forces in a header.

- footnotes:

  `NULL` (default) or a list of length `length(tables)` or length 1
  (common to all). Same block structure as `titles`, and the same
  page-number tokens; footnote rows default to left-aligned, and the
  first row carries a top rule (the separator) unless that row sets its
  own `border`.

- auto_section:

  Logical. When `TRUE` and `tables` is a **named** list, each name is
  used as a per-section heading appended to the common header defined by
  `rtf_section(secinfo = ...)` (called without a `page` argument). A
  section opens where the name **changes**, so a run of pages carrying
  the same name – the pages of one split table, which all share its
  heading – is **one** section of several pages. A `"...n"` tail (the
  serial
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  adds so a page list with repeated headings stays addressable by name)
  is **not** part of the name here: it is stripped before both the
  comparison and the printed heading. An item with an empty name falls
  through into the open section, which is how pages whose headings
  *differ* are put in one section (see
  [`combine_sections()`](https://ichirio.github.io/rtfreporter/reference/combine_sections.md)).
  Default `FALSE`.

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

  **Deprecated** (removed in 0.9.0): pass a gt or gtsummary table
  through
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
  which reads its labels, alignment, titles and source notes, and place
  the pages it returns here.

## Value

Modified rtf_document with appended contents.

## Details

A table's appearance is written where the table is made: in
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
or
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md).
A bare `data.frame` placed here gets their defaults.

## Deprecated arguments

The 26 table-format arguments that copy
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
(`col_header`, `border`, `col_rel_width`, `row_height_twips`,
`font_size_half_points`, `font`, ... – see the usage) and `read_gt` are
**deprecated** in 0.8.x: each one passed says so once a session and
still works as before, and they are removed in 0.9.0, the first CRAN
release. Write the same settings on the table:
`rtftable(df, border = "tfl", col_rel_width = c(2, 1))`, or
`as_rtftables(x, ...)` for a paged or converted table; pass a gt or
gtsummary table through
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
instead of `read_gt`. Until then they behave as they always did: used to
build a bare `data.frame` element, and, when passed explicitly,
overriding the matching field of a pre-built
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
page
([`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
pages are never modified).

## Examples

``` r
# Two clinical tables in one document, each on its own page with its own
# title; each table carries its own appearance (rtftable()), and a footnote
# is attached to the first page only.
t1 <- data.frame(Parameter = c("Age (years)", "Sex, n (%)"),
                 Value = c("75.1 (8.2)", "120 (53%)"))
t2 <- data.frame(Parameter = c("Weight (kg)", "Height (cm)"),
                 Value = c("78.0 (12.1)", "170 (9.5)"))

doc <- rtf_document() |>
  rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
  rtf_tables(
    list(rtftable(t1, border = "tfl", col_rel_width = c(2, 1)),
         rtftable(t2, border = "tfl", col_rel_width = c(2, 1))),
    titles    = list("Table 14.1.1", "Table 14.1.2"),
    footnotes = list("Source: ADSL", NULL)
  )
f <- tempfile(fileext = ".rtf")
generate_rtfreport(doc, f)
unlink(f)
```
