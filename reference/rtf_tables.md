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
  auto_section = NULL,
  section_label_align = "left",
  auto_title = NULL,
  title_label_align = "left",
  read_gt = FALSE,
  page_break = TRUE,
  first_rows = NULL,
  space_before = 0L,
  cont_title = NULL
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
  forces in a header. A table paginated on a column fills
  `{<column name>}` with its page's value (see *Page values*).

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
  `NULL` (default): what the pages say — a plan's
  `plan_paginate_group(heading_at = "section")` — else `FALSE`.

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
  which puts the same name in the section header instead. `NULL`
  (default): what the pages say — a plan's
  `plan_paginate_group(heading_at = "title")` — else `FALSE`.

- title_label_align:

  Alignment for the auto-appended title label row. One of `"left"`
  (default), `"center"`, or `"right"`.

- read_gt:

  **Deprecated** (removed in 0.9.0): pass a gt or gtsummary table
  through
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
  which reads its labels, alignment, titles and source notes, and place
  the pages it returns here.

- page_break:

  `TRUE` (the default): each table begins a new page. `FALSE`: a table
  goes on under the content before it, on the page that one ends on – a
  listing under its summary table. Nothing is measured: what does not
  fit, the reader (Word, LibreOffice) carries to the next page by itself
  – with no title, no column header and no footnote there; fitting the
  page is the program's to arrange. To go on with the title, column
  header and footnote on pages of its own, give `first_rows`. One value
  for every table, or one each. A table's own later pages (a plan's, or
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s)
  always begin a page; so does the document's first content. Unlike
  reporter's `add_content(page_break = )` this is the break *before* the
  table. A page has one `auto_section` heading: that of the first table
  on it that has one; a later table on the page with another heading
  gets a warning (`auto_title` names it on its title), and no page break
  is added for it. The page header and footer are the section's; a
  static `{PAGE}` / `{TOTAL_PAGES}` counts the pages as written, which
  are right when each page holds what fits it: the program counts the
  rows.
  [`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)`(page = n)`
  counts the pages as written;
  [`rtf_layout()`](https://ichirio.github.io/rtfreporter/reference/rtf_layout.md)
  shows them.

- first_rows:

  With `page_break = FALSE`, the rows of the table that go on the page
  before it; the rest is paginated as the table says – a plan's
  `plan_paginate_rows(max_rows = , split = )` – each page with its
  title, column header and footnote. The program counts what fits:
  rtfreporter does not measure the page. `tables` is then one table: a
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md),
  a data frame (its rest one page; for `max_rows` give
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)`(first_rows = )`
  the pages) or an
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  (cut there, its rest one page). `NULL` (the default): the whole table
  goes on.

- space_before:

  With `page_break = FALSE`, empty lines between the content before and
  this table (0: the title's own blank line alone).

- cont_title:

  A line under the title of each later page of a table
  (`"(continued)"`): the pages after the shared one of `first_rows`, and
  a table's own later pages (a plan's, or
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `max_rows` pages). `NULL`: none.
  [`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)
  replaces the titles whole, this line too. Not
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `cont_label`, which marks a group cut across pages.

## Value

Modified rtf_document with appended contents.

## Details

A table's appearance is written where the table is made: in
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
or
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md).
A bare `data.frame` placed here gets their defaults.

## Page values

A table cut into pages on a column –
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)`(page_by = )`,
a value split on a named group column
(`split = "by_value", group_col = `), a plan's
[`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
– knows each page's value of that column. A title, footnote,
`auto_section` heading, header or footer
([`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md))
then writes `{<column name>}` (`{PARAM}`, `{SEX}`) and gets that page's
value when the file is written:

- a title or footnote takes the value of its own table, so on a page of
  several tables (`page_break = FALSE`) each names its own;

- a header or footer belongs to the page: each column takes the first
  table on the page with a value for it (the rule of the `auto_section`
  heading); a later table with another value gets a warning. A band that
  uses one is written for each page, as one with a static `{PAGE}` is;

- a `{NAME}` the page has no value for is printed as written, as any
  unknown token is (a title's `{N}` stays `{N}`); rtfreporter's own
  tokens (`{PAGE}`, `{PROGRAM}`, ...) and a token of one's own
  (`rtf_document(tokens = )`) of the same name win over a column's.

The values are on each page's `rtf_paginate_meta` as `page_values`
(`c(PARAM = "ALT")`), and
[`rtf_layout()`](https://ichirio.github.io/rtfreporter/reference/rtf_layout.md)
shows them. An rtables page from
[`rtables::paginate_table()`](https://insightsengineering.github.io/rtables/latest-tag/reference/paginate.html)
carries its `split_rows_by(page_by = TRUE)` values by their page-title
prefix; a gt_group member of tfrmt's `print_to_gt()` the
`page_structure()` columns of a `page_plan(note_loc = )` that writes the
page note.

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
