# Split one record's statistics into named groups

`plan_stat_groups()` puts the statistics of one record into named groups
— its N in one, its rate and interval in another — and gives each row of
the ARD its group in the key `.stat_group`, which `table_plan(cols = )`
or `rows = ` names like any other key. So one analysis lays out as **n
rows x m columns** without a
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) of
`variable` before
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md):
the groups go across (`cols = c("ARM", ".stat_group")`) or down
(`rows = c(..., stat = ".stat_group")`), and
[`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md)
keys a template by a group's name.

## Usage

``` r
plan_stat_groups(.plan, ..., .variables = NULL, .others = NULL)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- ...:

  The groups, each named, the value the statistics in it:
  `n = "N", orr_ci = c("n", "estimate", "conf.low", "conf.high")`. A
  statistic in no group is not read (and noted). Down the rows the
  groups come in the data's order;
  [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md)
  (`.stat_group = `) orders them. With
  [`cell_rows()`](https://ichirio.github.io/rtfreporter/reference/cell_rows.md)
  or named templates for the rows inside a cell, a group with fewer rows
  leaves the rest empty, aligned row by row. A later call replaces the
  groups whole.

- .variables:

  The variables whose records are split into the groups; `NULL`
  (default): all.

- .others:

  The group the other variables' records go to **whole**, every
  statistic with them — so a block of SEX's n (%) is not pulled into a
  response rate's groups by its `n`. `NULL` (default): none. `.others`
  needs `.variables`.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md)
for how a template is chosen for a group.

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
[`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_stub.md),
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md),
[`plan_titles()`](https://ichirio.github.io/rtfreporter/reference/plan_titles.md),
[`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_total.md)

## Examples

``` r
# Invented response rates: one record per arm, with its N, the number of
# responders, the rate and its 95% CI
orr <- data.frame(
  ARM       = rep(c("Placebo", "Active"), each = 5),
  variable  = "RESP",
  .label    = "Responders",
  .kind     = "categorical",
  stat_name = rep(c("N", "n", "estimate", "conf.low", "conf.high"), 2),
  stat      = c(40, 8, 0.20, 0.09, 0.36, 41, 17, 0.415, 0.263, 0.579))

# Minimal: each arm's N in one column and n (%) in the next
table_plan(orr, cols = c("ARM", ".stat_group"), rows = c(group = "variable")) |>
  plan_stat_groups(N = "N", rate = c("n", "estimate")) |>
  plan_cells(N = "{N}", rate = "{n} ({estimate:.1f%})", notes = FALSE) |>
  plan_apply("table")
#>   group      label Placebo____N Placebo____rate Active____N Active____rate
#> 1  RESP Responders           40        8 (20.0)          41      17 (41.5)

# The same record down the rows: the rate first, then its CI
table_plan(orr, cols = "ARM",
           rows = c(group = "variable", stat = ".stat_group")) |>
  plan_stat_groups(ci = c("conf.low", "conf.high"),
                   rate = c("n", "estimate")) |>
  plan_levels(.stat_group = c(rate = "n (%)", ci = "95% CI")) |>
  plan_cells(rate = "{n} ({estimate:.1f%})",
             ci   = "({conf.low:.1f%}, {conf.high:.1f%})",
             notes = FALSE) |>
  plan_apply("table")
#>   group   stat      label     Placebo       Active
#> 1  RESP  n (%) Responders    8 (20.0)    17 (41.5)
#> 2  RESP 95% CI Responders (9.0, 36.0) (26.3, 57.9)
```
