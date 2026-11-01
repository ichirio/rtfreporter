# Declare the table, one layer at a time

Each verb adds a layer to an
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).
**A later layer wins.** The roles — which column goes across, which go
down, which carries the row identity — are said once, on
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md);
these verbs are the things a report really does declare twice. Their
arguments are the ones
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
and
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
already take, so the layering is the only new idea. Nothing runs until
the plan is handed to
[`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md)
or
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md).

This page is the overview of the verbs, in the order a report is usually
written; each has a page of its own. Every verb takes the plan first, as
`.plan`: the pipe passes it by position, and the dot keeps a name in
`...` such as `p` (cards' percentage) from being taken for it.

**The cells**

- [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md):
  the cell templates, how each cell is made from the statistics.

- [`plan_digits()`](https://ichirio.github.io/rtfreporter/reference/plan_digits.md):
  the decimals (or significant digits) of the open tokens.

- [`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md):
  one record's statistics split into named groups.

**The rows**

- [`plan_filter()`](https://ichirio.github.io/rtfreporter/reference/plan_filter.md):
  drop rows by a condition over their statistics.

- [`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_sort.md):
  the row order, alphabetical or by frequency.

- [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md):
  the order of values (and of the analysis variables).

- [`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md):
  the text values and variables print as.

- [`plan_nest()`](https://ichirio.github.io/rtfreporter/reference/plan_nest.md):
  one variable's rows under one level of another.

- [`plan_empty()`](https://ichirio.github.io/rtfreporter/reference/plan_empty.md):
  what a table with no events says.

**More columns**

- [`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md):
  a Total column (or row) from the ARD's overall rows.

- [`plan_compare()`](https://ichirio.github.io/rtfreporter/reference/plan_compare.md):
  columns read from comparison records (a p-value, a difference).

**The layout**

- [`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md):
  fold the row keys into one row-heading column.

- [`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md):
  how a run of rows sharing a value shows.

- [`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md):
  where the blank rows go.

- [`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md):
  columns that do their work without being printed.

- [`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_columns.md):
  widths, decimal alignment and row titles, by name.

- [`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md):
  bold, italic, colour, alignment of cells.

- [`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md):
  the whole table's borders, font and row heights.

**The pages**

- [`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md):
  a page per value of a column.

- [`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md):
  the row budget, and what a break may cut.

- [`plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_cols.md):
  column blocks, and how the page axes nest.

**The header and the text around the table**

- [`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md):
  the column header and the numbers it prints.

- [`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md):
  the block of titles above the table.

- [`plan_footnotes()`](https://ichirio.github.io/rtfreporter/reference/plan_footnotes.md):
  the block of footnotes below the table.

**The rest**

- [`plan_listing()`](https://ichirio.github.io/rtfreporter/reference/plan_listing.md):
  lay a frame of records out as a listing.

- [`plan_append()`](https://ichirio.github.io/rtfreporter/reference/plan_append.md):
  another plan's rows under this table's.

- [`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md):
  your own functions, run on the finished pages.

## Where each argument goes

Each verb has **one job**, and its arguments keep the names of the
function that does it. This is what each one hands on.

- `plan_cells(..., stats, value, na, notes)`: how a cell is made. Goes
  to
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  `cells`, `stats`, `value`, `na`, `notes`; a finished table
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `na`.

- `plan_digits(..., rounding)`: the digits. Goes to the open tokens of
  the templates; on a finished table
  [`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md).

- [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md),
  [`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md):
  the order and text of values. Goes to
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  `levels`, `labels`. A named element of
  [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md)
  is a value and its text, as
  [`base::factor()`](https://rdrr.io/r/base/factor.html)'s `levels` and
  `labels` (`plan_levels(SEX = c(F = "Female", M = "Male"))`,
  `variable = c(AGE = "Age (years)", "SEX")`);
  [`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md)
  names without ordering, and wins value by value over a name
  [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md)
  gives.

- `plan_total(label, position)`: a Total column from the ARD's overall
  rows. Goes to
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  those rows as one more value of the column key, and its place in
  `levels`; the header's `{n}` there is the study total.
  `plan_levels(.drop_empty = )` leaves out the levels no record has,
  before the table is made.

- `plan_stat_groups(...)`: the statistics in named groups. Goes to
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  `stat_groups` (the key `.stat_group`).

- `plan_filter(..., any, parents, by)`: rows dropped by a condition over
  the record. Goes to
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  `filter`.

- `plan_compare(..., by, position)`: columns read from comparison
  records (a test's p-value, a difference and its CI). Goes to
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  `compare`.

- `plan_sort(..., stat, keep)`: the row order. Goes to
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  `sort`, `sort_stat`; a finished table
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `sort_by`, `sort_desc` from `-name`.

- `plan_stub(vars, name, indent, group_summary, before)`: the row
  headings. Goes to
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md):
  `vars`, `label`, `indent`, `group_summary`.

- `plan_cell_style(cols, header, where, bold, italic, align, color, background, border, underline, indent_twips)`:
  how cells look. Goes to
  [`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md),
  [`style_cols()`](https://ichirio.github.io/rtfreporter/reference/style_header.md),
  or
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  `cell_styles` for a condition.

- `plan_paginate_group(col, keep, heading, heading_at)`: a page per
  value. Goes to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `split = "by_value"`, `group_col`; `keep = FALSE` adds it to
  `drop_cols`; `heading` names each page from a template (`{value}`,
  `{n}`), the name `rtf_tables(auto_section = TRUE)` prints;
  `heading_at` says where it prints (`"section"`, `"title"`, `"none"`):
  the default of `rtf_tables(auto_section = , auto_title = )`.

- `plan_row_group(mode, collapse, group_col)`: groups down the body.
  Goes to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `group_by`, `collapse_repeats`, `group_col` (a finished table).

- `plan_hide(...)`: columns not printed. Goes to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `drop_cols`.

- `plan_empty(text, when, position, footnote)`: a table of no events:
  one row saying so (`"No subjects met the criteria."`), and a footnote,
  only when the table has no row or its overall row is 0.

- `plan_blanks(where, first, last, counted)`: blank rows. Goes to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `blank_rows`, `blank_row_first`, `blank_row_end`, `count_blank_rows`;
  a listing's `where = "records"` / `"none"` is
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)'s
  `blank_row = TRUE` / `FALSE`.

- `plan_paginate_rows(max_rows, split, break_before, min_group_rows, cont_label, page_by, heading, heading_at)`:
  the row budget, inside the BY pages. Goes to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `max_rows`, `split`, `split_rows`, `min_group_rows`, `cont_label`,
  `page_by`; `heading` and `heading_at` name each BY page, and say where
  the name prints, as
  [`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md)'s
  do.

- `plan_paginate_cols(at, cut_by, every, keep, col_header, fit, allow_span_break, order)`:
  column blocks. Goes to
  [`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md),
  whose arguments have the same names.

- `plan_style(border, ..., border_header, ..., header_bold, ..., table_width_twips, ...)`:
  the whole table. Goes to
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  /
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  by the same names; `border_*` and the default look (`header_align`,
  `header_bold`, `header_italic`, `align`, `bold`, `italic`,
  `underline`) via
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md).

- `plan_columns(widths, decimal, row_title, auto_width, sep, cell_format, column_widths_twips)`:
  the columns. Goes to
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md):
  `col_rel_width`, `row_title`, `column_widths_twips`;
  [`set_decimal_split()`](https://ichirio.github.io/rtfreporter/reference/set_decimal_split.md):
  `cols`;
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `auto_width`, `cell_format`;
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
  `sep`.

- `plan_col_header(header, values, header_sep, col_header_align, lines, span)`:
  the column header. Goes to
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md):
  the header (or `lines`, the header a row at a time) and a data frame
  of `values`; a population fills its `{n}` tokens;
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
  `header_sep`;
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md):
  `col_header_align`.

- `plan_listing(..., type, sep, spacer, spacer_rel_width, layout, wrap, align, record)`:
  a listing. Goes to
  [`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md),
  the same names.

- [`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md),
  [`plan_footnotes()`](https://ichirio.github.io/rtfreporter/reference/plan_footnotes.md):
  the blocks above and below. Goes to
  [`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md),
  [`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md).

- `plan_after(...)`: anything else. Goes to your functions of the pages.

[`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_columns.md)
declares the columns **of this table** — widths, alignment, the key
separator — and is not
[`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md),
which addresses the columns of finished pages by their printed names.

## Two kinds of names

[`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md),
[`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md),
[`plan_filter()`](https://ichirio.github.io/rtfreporter/reference/plan_filter.md),
[`plan_nest()`](https://ichirio.github.io/rtfreporter/reference/plan_nest.md),
[`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md)
and
[`plan_compare()`](https://ichirio.github.io/rtfreporter/reference/plan_compare.md)
read the normalized ARD's columns (`variable`, the keys as the ARD names
them);
[`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md),
`plan_paginate_group(col = )`,
[`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md),
[`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md),
[`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_columns.md)
and
[`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md)
read the finished table's columns (the names `table_plan(rows = )` gave,
`row_label`, the arms).

The styles
[`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md)
declares run on the pages in the order written (after the header and the
decimal alignment, before any
[`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md)
step). Its `cols` may be column names, and `.values` stands for every
value column, so a reordered table keeps them.

## See also

[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md),
[`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md),
[`plan_layers()`](https://ichirio.github.io/rtfreporter/reference/plan_layers.md)

## Examples

``` r
# Invented summary statistics of two arms, one row a statistic
st <- data.frame(
  ARM       = rep(c("Placebo", "Active"), each = 4),
  variable  = rep(c("AGE", "AGE", "SEX", "SEX"), 2),
  .label    = rep(c(NA, NA, "F", "M"), 2),
  .kind     = rep(c("continuous", "continuous", "categorical",
                    "categorical"), 2),
  stat_name = rep(c("mean", "sd", "n", "n"), 2),
  stat      = c(61.2, 9.81, 12, 8, 63.5, 10.2, 9, 11))

# The roles once, then one verb a declaration; plan_apply() runs it
pages <- table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(continuous  = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
             categorical = "{n}") |>
  plan_labels(AGE = "Age (years)", SEX = "Sex") |>
  plan_stub(name = "Characteristic") |>
  plan_col_header(rtf_col_header(c("Characteristic", "{col}"))) |>
  plan_titles("Table 14.1.1", "Demographics") |>
  plan_apply()
pages[[1]]
#>               Table 14.1.1               
#>               Demographics               
#> ─────────────────────────────────────────
#> Characteristic    Placebo       Active   
#> ─────────────────────────────────────────
#> Age (years)                              
#>     Mean (SD)   61.2 (9.81)  63.5 (10.20)
#> Sex                                      
#>     F               12            9      
#>     M                8            11     
#> 
#> <rtftable> 5 rows x 3 columns
#>   Columns:    Characteristic | Placebo | Active
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#>   Titles:     2 line(s)
#> 
```
