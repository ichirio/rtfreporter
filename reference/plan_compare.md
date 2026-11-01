# Add columns read from comparison records

`plan_compare()` adds **comparison columns** — a test's p-value, a
difference and its CI — read from the ARD's comparison records: the
column key with **no value**, in an analysis the body does not have
(cardx's `ard_stats_t_test()`, `ard_stats_chisq_test()`,
`ard_stats_fisher_test()`, ...). Each goes on its **variable's block**:
the row of its level when it has one, else the block's first row as the
table is sorted. Nothing is computed; the numbers are the ARD's.

## Usage

``` r
plan_compare(.plan, ..., by = NULL, position = c("last", "first"))
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).
  The dot keeps `p = `, the commonest column here, from being taken for
  it.

- ...:

  The columns, each named (the column's name, any name) and each what
  [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md)
  takes for one cell:

  a template

  :   `p = "{p.value:.3f}"`, reading only the statistics it names.

  guards

  :   `p = c(p.value < 0.001 ~ "<0.001", "{p.value:.3f}")`.

  a map by context / kind / `default`

  :   a named vector: `p = c(stats_t_test = "{p.value:.3f}")` reads the
      t-test and leaves a Wilcoxon of the same variable (noted, as every
      record no column takes). Two tests one column reads both of are an
      error, never one picked.

  [`cell_rows()`](https://ichirio.github.io/rtfreporter/reference/cell_rows.md)

  :   lines: each goes on the variable's row of that label, so a p-value
      on one row only is `cell_rows("Mean (SD)" = "{p.value:.3f}")`.

  A test whose record names no variable (`ard_stats_aov()`'s term) has
  no row; use `ard_stats_oneway_test()`. The columns have no population:
  the header's `{col}` / `{n}` and its short rows leave them alone, so
  the header names them (`rtf_col_header(c(..., "p-value"))`).

- by:

  The column that splits the comparison records into columns, one a
  level (`by = "COMPARISON"`: each active arm against placebo, the ARD
  made by `ard_strata(.strata = COMPARISON)` over two-arm subsets). With
  several columns in `...` they are named `<level>____<column>`. `NULL`:
  one comparison a variable.

- position:

  Where the columns go: after the column key's (`"last"`, the default)
  or before them (`"first"`).

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
# Invented statistics: AGE by arm, and a t-test of the two arms (the
# record with no ARM)
st <- data.frame(
  ARM       = c(rep(c("Placebo", "Active"), each = 3), NA, NA, NA, NA),
  variable  = "AGE",
  context   = c(rep("summary", 6), rep("stats_t_test", 4)),
  .label    = NA,
  .kind     = "continuous",
  stat_name = c("N", "mean", "sd", "N", "mean", "sd",
                "p.value", "estimate", "conf.low", "conf.high"),
  stat      = c(20, 61.2, 9.81, 20, 63.5, 10.2,
                0.0312, -2.3, -4.4, -0.2))
p <- table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(continuous = c(n = "{N}",
                            "Mean (SD)" = "{mean:.1f} ({sd:.2f})"))

# Minimal: the p-value, on the block's first row
p |> plan_compare(p = "{p.value:.3f}") |> plan_apply("table")
#>   group     label     Placebo       Active     p
#> 1   AGE         n          20           20 0.031
#> 2   AGE Mean (SD) 61.2 (9.81) 63.5 (10.20)  <NA>

# The difference with its CI and the p-value, both on the Mean row,
# a small p-value printed as "<0.001"
p |>
  plan_compare(
    diff = cell_rows("Mean (SD)" =
      "{estimate:.1f} ({conf.low:.1f}, {conf.high:.1f})"),
    p    = cell_rows("Mean (SD)" = c(p.value < 0.001 ~ "<0.001",
                                     "{p.value:.3f}"))) |>
  plan_apply("table")
#>   group     label     Placebo       Active              diff     p
#> 1   AGE         n          20           20              <NA>  <NA>
#> 2   AGE Mean (SD) 61.2 (9.81) 63.5 (10.20) -2.3 (-4.4, -0.2) 0.031
```
