# Style cells by column, in the header, or by a condition

`plan_cell_style()` says how cells look — bold, italic, underline,
colour, background, alignment, indent, border — and which cells: the
column header (`header = TRUE`), whole columns (`cols`), or the rows a
condition chooses (`where`, or an attribute written as a formula). It is
SAS's `call define()` in a `compute` block: a cell looks at its own row
and decides how it is printed. Columns are named, so a reordered table
keeps its styles; the styles run on the pages in the order written.

## Usage

``` r
plan_cell_style(
  .plan,
  cols = NULL,
  header = FALSE,
  where = NULL,
  bold = NULL,
  italic = NULL,
  align = NULL,
  color = NULL,
  background = NULL,
  border = NULL,
  underline = NULL,
  indent_twips = NULL
)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- cols:

  The columns styled, by name (`.values` for every value column); left
  out, every column.

- header:

  `TRUE` styles the column header
  ([`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md))
  instead of the body; it takes `bold`, `italic`, `underline`, `align`
  and `border`, and no `where`.

- where:

  A one-sided formula over the table's columns choosing the rows,
  `where = ~ is.na(label)`; the attributes given apply there and leave
  the other rows alone. Left out, the whole of `cols` is styled
  ([`style_cols()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)).

- bold, italic, align, color, background, underline:

  How the cells look. Each is one of:

  a value

  :   `bold = TRUE`, `align = "center"`, `color = "#CC0000"`: applied to
      the cells `cols` / `header` / `where` choose.

  a one-sided formula

  :   the value computed row by row over the table's columns, `NA`
      leaving the column default alone: `bold = ~ is.na(label)`. A call
      whose attributes are formulas takes neither `where` nor `header`.

  a named list of formulas

  :   one a column, by name:
      `color = list(Placebo = ~ ifelse(n > 50, "#CC0000", NA))`.

  A formula sees the printed rows only with `plan_stub(before = TRUE)`.

- border:

  A column's border, an
  [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md):
  on whole columns or the header, not with `where`.

- indent_twips:

  The left indent of the cells' text,
  [`style_cols()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)'s
  `indent_twips` (not on the header).

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md)
for the whole table's look.

Other plan verbs:
[`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md),
[`plan_append()`](https://ichirio.github.io/rtfreporter/reference/plan_append.md),
[`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md),
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
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md),
[`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md),
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md)

## Examples

``` r
tbl <- data.frame(
  Parameter = c("Subjects", "Age, mean (SD)", "Female, n (%)"),
  Placebo   = c("86", "75.2 (8.59)", "53 (61.6)"),
  Active    = c("84", "75.7 (8.29)", "50 (59.5)"))

# Minimal: the active arm's column in bold
pages <- table_plan(tbl) |>
  plan_cell_style(cols = "Active", bold = TRUE) |>
  plan_apply()
pages[[1]]$col_spec[[3]]$bold
#> [1] TRUE

# The header in bold, the arms centred, the count row in italics and
# the active arm's female cell in red
pages <- table_plan(tbl) |>
  plan_col_header(rtf_col_header(c("Parameter", "Placebo", "Active"))) |>
  plan_cell_style(header = TRUE, bold = TRUE) |>
  plan_cell_style(cols = c("Placebo", "Active"), align = "center") |>
  plan_cell_style(where = ~ Parameter == "Subjects", italic = TRUE) |>
  plan_cell_style(color = list(
    Active = ~ ifelse(startsWith(Parameter, "Female"), "#CC0000", NA))) |>
  plan_apply()
pages[[1]]
#> ────────────────────────────────────────
#> Parameter         Placebo      Active   
#> ────────────────────────────────────────
#> Subjects            86           84     
#> Age, mean (SD)  75.2 (8.59)  75.7 (8.29)
#> Female, n (%)    53 (61.6)    50 (59.5) 
#> 
#> <rtftable> 3 rows x 3 columns
#>   Columns:    Parameter | Placebo | Active
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#>   Cell styles: set (3 rows)
#> 
```
