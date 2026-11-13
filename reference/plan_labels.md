# Give values and variables the text they print as

`plan_labels()` says the text a value, a column or an analysis variable
prints as, without changing the order
([`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md)
does that). Write the plan with the data's own codes and rename at the
end: one entry per key, merged one key at a time, so a later call
relabels one variable and leaves the others. It is read when the table
is made, so it can come anywhere in the pipe after
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

## Usage

``` r
plan_labels(.plan, ...)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- ...:

  One entry per value, column or analysis variable, named by it. The
  shape of the value says which:

  `<value> = "text"`

  :   one text: that value's text wherever it appears. An analysis
      variable's name is a value too, so `AGE = "Age (years)"` is the
      heading of AGE's block of rows.

  `<key> = c(code = "text", ...)`

  :   a named vector: a dictionary for that key only — a column key
      (`ARM = c(Active = "Drug X 10 mg")`) or an analysis variable's
      levels (`SEX = c(F = "Female", M = "Male")`). Its entry under the
      variable's own name is the variable's heading:
      `SEX = c(SEX = "Sex", F = "Female")`. So the same code can mean
      two things on two axes: a shift table's `"0"` is `"Grade 0"` down
      the side and `"Baseline 0"` across the top.

  A value may be named by itself or by the text a
  [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_levels.md)
  order gave it; this text wins over that one. A later call replaces an
  entry of the same name whole. A single unnamed argument that is itself
  a named vector is taken as the whole map: `plan_labels(study_labels)`.
  An entry for a value, column or variable the table does not have is
  not used, so one study-wide map serves every table. Each entry is text
  (`""` prints an empty text); an entry that is `NA`, `NULL`, a number,
  a factor, `character(0)`, two texts without names or a dictionary with
  an `NA` or an unnamed text stops.

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
  variable  = rep(c("AGE", "SEX", "SEX"), 2),
  .label    = rep(c(NA, "F", "M"), 2),
  .kind     = rep(c("continuous", "categorical", "categorical"), 2),
  stat_name = rep(c("mean", "n", "n"), 2),
  stat      = c(61.2, 12, 8, 63.5, 9, 11))
p <- table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(continuous = c(Mean = "{mean:.1f}"), categorical = "{n}")

# Minimal: the headings of the two blocks
p |> plan_labels(AGE = "Age (years)", SEX = "Sex") |> plan_apply("table")
#>         group label Placebo Active
#> 1 Age (years)  Mean    61.2   63.5
#> 2         Sex     F      12      9
#> 3         Sex     M       8     11

# The study keeps its labels in one vector.  This table also spells
# out the sexes and names the active arm's dose; AGE keeps the study's
study_labels <- c(AGE = "Age (years)", SEX = "Sex")
p |>
  plan_labels(study_labels) |>
  plan_labels(SEX = c(SEX = "Sex, n", F = "Female", M = "Male"),
              ARM = c(Active = "Drug X 10 mg")) |>
  plan_apply("table")
#>         group  label Placebo Drug X 10 mg
#> 1 Age (years)   Mean    61.2         63.5
#> 2      Sex, n Female      12            9
#> 3      Sex, n   Male       8           11
```
