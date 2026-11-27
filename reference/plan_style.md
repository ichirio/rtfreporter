# Set the look of the whole table

`plan_style()` gives the settings that belong to the **whole table**:
its rules, font, row heights, padding and width, and the default look of
the header and the body. The arguments keep the names
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
and
[`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)
give them. What a table has per column or per cell is another verb's —
[`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_columns.md),
[`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md),
[`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md),
[`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md)
— so this list is the whole of it.

## Usage

``` r
plan_style(
  .plan,
  border = NULL,
  align_count_pct = NULL,
  font = NULL,
  font_size_half_points = NULL,
  row_height_twips = NULL,
  row_height_exact = NULL,
  header_row_height_twips = NULL,
  blank_row_height_twips = NULL,
  cell_padding_left_twips = NULL,
  cell_padding_right_twips = NULL,
  cell_padding_top_twips = NULL,
  cell_padding_bottom_twips = NULL,
  cell_valign = NULL,
  table_align = NULL,
  markup = NULL,
  blank_row_normalize = NULL,
  border_header = NULL,
  border_spanning = NULL,
  border_body = NULL,
  border_first_row = NULL,
  border_last_row = NULL,
  border_group = NULL,
  header_align = NULL,
  header_bold = NULL,
  header_italic = NULL,
  align = NULL,
  bold = NULL,
  italic = NULL,
  underline = NULL,
  table_width_twips = NULL,
  table_width_pct = NULL,
  table_width_pct_of_writable = NULL
)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- border:

  The table's rules,
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  `border`: `"tfl"` (the clinical preset), `"none"`, or an
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  object. Given with the `border_*` zones below (in this call or
  another), the preset is the start and each zone is merged onto its
  zone side by side, as
  [`style_zone()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
  merges: `border = "tfl", border_last_row = rtf_border(bottom = TRUE)`
  is the clinical rules with a rule under the last row. A side a zone
  leaves `NULL` keeps the preset's. A whole-table
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  is expanded by
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  first and the zones merged onto that, the pages
  [`style_zone()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
  gives. Given with the look fields (`header_bold`, `align`, ...), the
  preset's rules stay the table's, and so they do with `border_group`,
  which adds a rule under each group; with no `border` and no zone of
  the table's rows, the default `"tfl"` keeps its rules the same way.

- align_count_pct:

  `as_rtftables(align_count_pct = )`: `TRUE` realigns `"n (xx.x)"` cells
  to one width before the pages are cut, or the name of another built-in
  cell format (`"count_paren"`, ...).

- font, font_size_half_points:

  The table's font family (`"Arial"`; a named vector gives one a script
  slot) and size in half-points. `NULL` takes the document's.

- row_height_twips, row_height_exact, header_row_height_twips,
  blank_row_height_twips:

  The heights of the body rows, the header rows and the blank rows, in
  twips (1/1440 inch); `row_height_exact = TRUE` makes a height exact
  (clipped) rather than a minimum.

- cell_padding_left_twips, cell_padding_right_twips:

  The cells' left and right padding, in twips.

- cell_padding_top_twips, cell_padding_bottom_twips:

  The cells' top and bottom padding, in twips
  ([`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s).

- cell_valign:

  The cells' vertical alignment: `"bottom"` (default), `"top"` or
  `"center"`.

- table_align:

  Where the table sits across the page: `"left"` (default), `"center"`
  or `"right"`.

- markup, blank_row_normalize:

  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  `markup` (the cell-text markup applied when the page is rendered, such
  as `"script"`) and `blank_row_normalize` (how blank rows are
  normalised then).

- border_header, border_spanning, border_body, border_first_row,
  border_last_row:

  The rules of one kind of row (the column header, a spanning header
  row, the body, its first and its last row), each an
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md),
  by the names
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)
  gives them.

- border_group:

  An
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md)
  drawn under the **last row of each row group**, the groups
  [`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md)'s
  `mode` finds. A plan with no
  [`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md)
  has no groups, so nothing is drawn. `NULL` (the default) draws
  nothing, and the table is written exactly as before. It is one of the
  `border_*` zones, and goes with `border` as they do. The rule goes to
  the group's last body row, not to a blank row between groups, and a
  group cut by a page break has it under its real last row only, not at
  the break. The last row of a page takes `border_last_row` when that is
  given: it wins, and the group rule is not written on that row, so
  there is no double line. The table's final row ends the last group:
  with no `border_last_row` it takes the group rule like any other
  group's. A table with no groups, or one, changes nothing.

- header_align, header_bold, header_italic:

  The whole header's default look,
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)'s
  fields.

- align, bold, italic, underline:

  The body's default look,
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)'s
  fields. One column or one cell is
  [`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md).
  The flags (`header_bold`, `header_italic`, `bold`, `italic`,
  `underline`) mean what they mean there: only `TRUE` turns the look on,
  and any other value, `NA` included, is the same as `FALSE` (not an
  error, as `row_height_exact = NA` is). `align` and `header_align` must
  be `"left"`, `"center"` or `"right"`.

- table_width_twips, table_width_pct, table_width_pct_of_writable:

  The table's width: in twips, as a percentage (0–100) or a fraction
  (0–1) of the writable page width. Give one; if `table_width_twips` is
  given with a percentage, the twips win, and given both percentages,
  `table_width_pct` wins (as in
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)).

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
and
[`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md)
for the settings in full.

Other plan verbs:
[`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md),
[`plan_append()`](https://ichirio.github.io/rtfreporter/reference/plan_append.md),
[`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md),
[`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md),
[`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md),
[`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md),
[`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_columns.md),
[`plan_compare()`](https://ichirio.github.io/rtfreporter/reference/plan_compare.md),
[`plan_digits()`](https://ichirio.github.io/rtfreporter/reference/plan_digits.md),
[`plan_empty()`](https://ichirio.github.io/rtfreporter/reference/plan_empty.md),
[`plan_filter()`](https://ichirio.github.io/rtfreporter/reference/plan_filter.md),
[`plan_footnotes()`](https://ichirio.github.io/rtfreporter/reference/plan_footnotes.md),
[`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md),
[`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md),
[`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md),
[`plan_listing()`](https://ichirio.github.io/rtfreporter/reference/plan_listing.md),
[`plan_nest()`](https://ichirio.github.io/rtfreporter/reference/plan_nest.md),
[`plan_paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_cols.md),
[`plan_paginate_group()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_group.md),
[`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_paginate_rows.md),
[`plan_row_group()`](https://ichirio.github.io/rtfreporter/reference/plan_row_group.md),
[`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_sort.md),
[`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md),
[`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md),
[`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md),
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md)

## Examples

``` r
tbl <- data.frame(
  Parameter = c("Subjects", "Age, mean (SD)", "Female, n (%)"),
  Placebo   = c("86", "75.2 (8.59)", "53 (61.6)"),
  Active    = c("84", "75.7 (8.29)", "50 (59.5)"))

# Minimal: the clinical rules (a rule over and under the header, none
# under the last row)
pages <- table_plan(tbl) |> plan_style(border = "tfl") |> plan_apply()
class(pages[[1]]$border)
#> [1] "rtf_table_border"

# A house style: Arial 9 pt, taller rows, the table centred at 80% of
# the page width, its counts and percentages lined up
pages <- table_plan(tbl) |>
  plan_style(border = "tfl", font = "Arial", font_size_half_points = 18,
             row_height_twips = 300, table_align = "center",
             table_width_pct = 80, align_count_pct = TRUE) |>
  plan_apply()
pages[[1]][c("font", "font_size_half_points", "row_height_twips",
             "table_align", "table_width_pct_of_writable")]
#> $font
#> [1] "Arial"
#> 
#> $font_size_half_points
#> [1] 18
#> 
#> $row_height_twips
#> [1] 300
#> 
#> $table_align
#> [1] "center"
#> 
#> $table_width_pct_of_writable
#> [1] 0.8
#> 
```
