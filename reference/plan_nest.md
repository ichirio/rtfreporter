# Put one variable's rows under one level of another

`plan_nest()` places the rows of one analysis variable under a single
row of another: the sub-categories of a race under its `"Asian"` row.
They are two analyses of the same data — a hierarchical tabulation would
drop every level that has no sub-level — and the layout puts them
together: the rows move, nothing is counted again. The nested rows
follow that level's row, one indent step deeper, in the parent's group.

## Usage

``` r
plan_nest(.plan, ...)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- ...:

  One entry per nested variable, named by it, whose value is the
  variable and level its rows go under, named by the variable:
  `plan_nest(RACESUB = c(RACE = "Asian"))`. The value is one level (not
  `NA`) of one variable other than the nested one, and a variable is
  nested once; another value stops at `plan_nest()`, and a variable or
  level the table does not have stops when the plan is applied.

  The level

  :   is matched as the table shows it (the ARD's level: a code list's
      label).

  The nested variable

  :   Its own heading is dropped; its order and text stay
      [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md)'
      and
      [`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md)'.

  What is read

  :   The rows key that carries the variable
      (`table_plan(rows = c(group = "variable"))`) is what is read, and
      the indent is the stub's (`plan_stub(indent = )`, default 4).

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb.

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
# Invented counts: RACE, and the Asian subjects' origin as its own
# variable
race <- data.frame(
  ARM       = rep(c("Placebo", "Active"), each = 5),
  variable  = rep(c("RACE", "RACE", "RACE", "ASIANORG", "ASIANORG"), 2),
  .label    = rep(c("White", "Asian", "Black", "Chinese", "Japanese"), 2),
  .kind     = "categorical",
  stat_name = "n",
  stat      = c(10, 6, 4, 2, 4, 9, 7, 4, 3, 4))
p <- table_plan(race, cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(categorical = "{n}")

# Minimal: the origins follow the "Asian" row, in RACE's block
p |> plan_nest(ASIANORG = c(RACE = "Asian")) |> plan_apply("table")
#>   group        label Placebo Active
#> 1  RACE        White      10      9
#> 2  RACE        Asian       6      7
#> 3  RACE      Chinese       2      3
#> 4  RACE     Japanese       4      4
#> 5  RACE        Black       4      4

# As printed: one stub, the origins indented under "Asian", Japanese
# first
p |>
  plan_nest(ASIANORG = c(RACE = "Asian")) |>
  plan_levels(ASIANORG = c("Japanese", "Chinese")) |>
  plan_labels(RACE = "Race") |>
  plan_stub(name = "Characteristic") |>
  plan_apply("pages")
#> [[1]]
#> ─────────────────────────────────
#> Characteristic    Placebo  Active
#> ─────────────────────────────────
#> Race                             
#>     White           10       9   
#>     Asian            6       7   
#>         Japanese     4       4   
#>         Chinese      2       3   
#>     Black            4       4   
#> 
#> <rtftable> 6 rows x 3 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
#> 
```
