# Order the values of a key, and the analysis variables

`plan_levels()` says the order the values of a column or analysis
variable appear in — the arms across the top, the categories of SEX down
the side, or the blocks of rows themselves — and may give each value its
text on the way. It is a layer like every verb after
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md):
it merges one key at a time, so a later call adds a variable without
restating the rest. Values left out follow in the data's order.

## Usage

``` r
plan_levels(.plan, ..., .drop_empty = NULL)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- ...:

  One entry per column or analysis variable, named by it; the value is
  the order. An order may name its values' text, as
  [`base::factor()`](https://rdrr.io/r/base/factor.html)'s `levels` and
  `labels` do:

  `<key> = c("M", "F")`

  :   the order of a key's values (`ARM`, `SEX`, a value of
      `table_plan(cols = )` or `rows = `).

  `<key> = c(F = "Female", M = "Male")`

  :   the order F, M, and the text each prints as; an element without a
      name is the value as it is.

  `variable = c("AGE", "SEX")`

  :   the order of the analysis variables (the blocks of rows); a named
      element also gives the block's heading,
      `variable = c(AGE = "Age (years)", "SEX")`.

  A single unnamed argument that is itself a named list or vector is
  taken as the whole map.
  [`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md)
  names values without ordering them, and wins value by value over a
  text given here.

- .drop_empty:

  Variables whose levels **no record has** are not shown: a level whose
  `n` is 0 (or missing) in every column, as an ARD made with a code
  list's full set of levels has (a factor's unused level, counted 0).
  `NULL` (default): every level the data has is shown. A level that some
  column counts stays; a variable summarised without `n` is untouched.
  Several calls add up.

## Value

The plan, with one more layer.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_labels.md)
for text without order.

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
# Invented counts, one row a statistic: SEX has an "Unknown" level
# that no subject has.
st <- data.frame(
  ARM       = rep(c("Active", "Placebo"), each = 4),
  variable  = rep(c("AGE", "SEX", "SEX", "SEX"), 2),
  .label    = rep(c(NA, "F", "M", "U"), 2),
  .kind     = rep(c("continuous", "categorical", "categorical",
                    "categorical"), 2),
  stat_name = rep(c("mean", "n", "n", "n"), 2),
  stat      = c(63.5, 9, 11, 0, 61.2, 12, 8, 0))
p <- table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(continuous = c(Mean = "{mean:.1f}"), categorical = "{n}")

# Minimal: males first
p |> plan_levels(SEX = c("M", "F", "U")) |> plan_apply("table")
#>   group label Active Placebo
#> 1   AGE  Mean   63.5    61.2
#> 2   SEX     M     11       8
#> 3   SEX     F      9      12
#> 4   SEX     U      0       0

# Placebo on the left, the SEX block first with its heading, each sex
# spelled out, and the empty "Unknown" row dropped
p |>
  plan_levels(ARM = c("Placebo", "Active"),
              variable = c(SEX = "Sex", AGE = "Age (years)"),
              SEX = c(F = "Female", M = "Male", U = "Unknown"),
              .drop_empty = "SEX") |>
  plan_apply("table")
#> widen_ard(): `levels` names values the ARD does not have; they make no row or column:
#>   SEX: "U"
#>   A level shown with no record has to be in the ARD: a factor level (the study's code list
#>   applied when the ARD is made), which cards counts as 0.
#>         group  label Placebo Active
#> 1         Sex Female      12      9
#> 2         Sex   Male       8     11
#> 3 Age (years)   Mean    61.2   63.5
```
