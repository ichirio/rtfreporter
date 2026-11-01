# Declare the columns: widths, decimal alignment and row titles

`plan_columns()` declares the columns **of this table** by name: their
relative widths, the ones whose numbers line up at the decimal point,
the row-heading columns, and the separator several `cols` keys are
joined with. Named by column, the settings follow a column when the
table is reordered or a study has more arms, and they are what a
workbook's `columns` sheet holds. It is not
[`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md),
which addresses the columns of finished pages by their printed names.

## Usage

``` r
plan_columns(
  .plan,
  widths = NULL,
  decimal = NULL,
  row_title = NULL,
  auto_width = NULL,
  sep = NULL,
  cell_format = NULL,
  column_widths_twips = NULL
)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- widths:

  The relative column widths, `rtftable(col_rel_width = )`. **Named by
  column** (`c(row_label = 5, .values = 2)`, `.values` for every value
  column) a reordered table keeps them; unnamed, they are one a column
  in order, as `col_rel_width` itself. A name that is not a column of
  the table stops with the names the table has.

- decimal:

  The columns whose numbers line up at the decimal point (`.values` for
  every value column),
  [`set_decimal_split()`](https://ichirio.github.io/rtfreporter/reference/set_decimal_split.md)'s
  `cols`.

- row_title:

  The row-heading columns, `rtftable(row_title = )`, by name as `widths`
  are: a row key's name as `table_plan(rows = )` gives it, `.values` for
  every value column. A position counts the **printed** columns: a key
  hidden with
  [`plan_hide()`](https://ichirio.github.io/rtfreporter/reference/plan_hide.md)
  or taken out by `page_by` is left out.

- auto_width:

  `TRUE` sizes each column to its widest content,
  `as_rtftables(auto_width = )`.

- sep:

  The separator several `cols` keys are joined with in the value
  columns' names, `"____"` by default (`"Placebo____F"`). The spanning
  header is built by splitting on it, so a key value that contains it is
  refused.

- cell_format, column_widths_twips:

  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `cell_format` (a formatter, or a list of them one a column) and
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
  `column_widths_twips` (absolute widths), as they are. A plan gives
  `column_widths_twips` or `widths`, not both.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md)
for the table's width.

Other plan verbs:
[`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md),
[`plan_append()`](https://ichirio.github.io/rtfreporter/reference/plan_append.md),
[`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md),
[`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md),
[`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md),
[`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_col_header.md),
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
st <- data.frame(
  ARM       = rep(c("Placebo", "Active"), each = 3),
  variable  = rep(c("AGE", "WEIGHT", "SEX"), 2),
  .label    = rep(c(NA, NA, "F"), 2),
  .kind     = rep(c("continuous", "continuous", "categorical"), 2),
  stat_name = rep(c("mean", "mean", "n"), 2),
  stat      = c(61.2, 72.43, 12, 63.5, 101.8, 9))
demog <- function(st) {
  table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(Mean = "{mean:.1f}"), categorical = "{n}") |>
    plan_labels(AGE = "Age (years)", WEIGHT = "Weight (kg)", SEX = "Sex") |>
    plan_stub(name = "Characteristic")
}

# Minimal: the heading column three parts wide, each arm two
pages <- demog(st) |>
  plan_columns(widths = c(Characteristic = 3, .values = 2)) |>
  plan_apply()
pages[[1]]$col_rel_width
#> [1] 3 2 2

# The arms' numbers lined up at the decimal point too.  A third arm
# needs no change: the names reach it
st3 <- rbind(st, data.frame(
  ARM = "High dose", variable = c("AGE", "WEIGHT", "SEX"),
  .label = c(NA, NA, "F"),
  .kind = c("continuous", "continuous", "categorical"),
  stat_name = c("mean", "mean", "n"), stat = c(62.0, 88.6, 10)))
pages <- demog(st3) |>
  plan_columns(widths = c(Characteristic = 3, .values = 2),
               decimal = ".values") |>
  plan_apply()
pages[[1]]$col_rel_width
#> [1] 3 2 2 2
pages[[1]]$decimal_split$cols
#> [1] 2 3 4
pages[[1]]
#> ──────────────────────────────────────────
#> Characteristic  Placebo  Active  High dose
#> ──────────────────────────────────────────
#> Age (years)                               
#>     Mean         61.2     63.5     62.0   
#> Weight (kg)                               
#>     Mean         72.4    101.8     88.6   
#> Sex                                       
#>     F             12       9        10    
#> 
#> <rtftable> 6 rows x 4 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     relative 3:2:2:2
#> 
```
