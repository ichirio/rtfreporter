# Set the digits of the numbers in the cells

`plan_digits()` gives the decimals (or significant digits) the open
tokens of the
[`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md)
templates take — `{mean}`, `{p:%}`, as opposed to `{mean:.2f}`, which
already says. It is the verb a study writes twice: a house rule for
every table, then one variable's own. A later layer wins key by key, and
a statistic the narrower entry says nothing about falls through to the
wider one. On a plan of a finished table it formats the numbers
themselves, with
[`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md).

## Usage

``` r
plan_digits(.plan, ..., rounding = NULL)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- ...:

  The digits, keyed as
  [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_cells.md)'
  templates are (an analysis variable, a stat group, a context,
  `continuous` / `categorical`, or one unnamed entry for every cell).
  Each value is:

  one number

  :   the decimals of every open token in that entry, `plan_digits(1)`,
      `plan_digits(AGE = 0)`.

  a vector named by statistic

  :   each statistic its own,
      `continuous = c(mean = 1, sd = 2, median = 1)`.

  `"4s"`

  :   4 **significant** digits instead of decimals, as the token
      grammar's `{mean:.4s}`; it may be mixed in a named vector,
      `c(mean = "4s", n = 0)`.

  A key may also be a **row key** (a column of the long frame, such as
  `PARAMCD`), its digits given **value by value**: one of the values
  above for each, `PARAMCD = c(ALT = 1, BILI = 3)` or
  `PARAMCD = list(ALT = c(mean = 1, sd = 2), BILI = 3)` — a define's
  `SignificantDigits` one parameter at a time. The rows carrying that
  value take them after the analysis variable's own and before the
  context's, the kind's and the default; a value the rows do not carry
  is ignored, so one map can serve every table. On a **finished table**
  (a source that is already the table, or `plan_cells(stats = "rows")`)
  a key that names a **column** formats that column, and `.rows` formats
  the value columns by the text of the label column:
  `plan_digits(.rows = c(N = 0, Mean = 1, SD = "3s"))`. A key that
  reaches neither a variable nor a column is an error.

  A number of decimals is one whole number, 0 or more; a significant
  count is `"<n>s"` with `n` 1 or more (`"0s"` is an error). A vector of
  more than one value is named (by statistic, or by value for a row
  key). Anything else — `1.5`, `-1`, `Inf`, `NA`, `TRUE`, `"2"` on its
  own, `"x"`, `c(1, 2)` unnamed, a zero-length vector — stops at the
  verb, naming the key. (A whole number as text, `"0"`, is read as
  decimals inside a named vector, as `c(mean = "4s", n = 0)` makes it.)

  A later call replaces a key's entry as a whole:
  `AGE = c(mean = 1, sd = 2)` followed by `AGE = c(mean = 0)` leaves
  `mean` only for `AGE`, and its `sd` then takes the next wider key's
  digits.

- rounding:

  The tie-breaking family for the run, as `widen_ard(rounding = )` takes
  it. Last wins. A study sets the rule once with
  `options(rtfreporter.rounding = )`; give it here only for a table that
  departs from it.

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
  ARM       = rep(c("Placebo", "Active"), each = 6),
  variable  = rep(c("AGE", "AGE", "WEIGHT", "WEIGHT", "SEX", "SEX"), 2),
  .label    = rep(c(NA, NA, NA, NA, "F", "M"), 2),
  .kind     = rep(c(rep("continuous", 4), rep("categorical", 2)), 2),
  stat_name = rep(c("mean", "sd", "mean", "sd", "n", "n"), 2),
  stat      = c(61.24, 9.812, 72.456, 12.345, 12, 8,
                63.51, 10.218, 70.12, 11.874, 9, 11))
p <- table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(continuous = c("Mean (SD)" = "{mean} ({sd})"),
             categorical = "{n}")

# Minimal: one decimal for every open token (the counts too)
p |> plan_digits(1) |> plan_apply("table")
#>    group     label     Placebo      Active
#> 1    AGE Mean (SD)  61.2 (9.8) 63.5 (10.2)
#> 2 WEIGHT Mean (SD) 72.5 (12.3) 70.1 (11.9)
#> 3    SEX         F        12.0         9.0
#> 4    SEX         M         8.0        11.0

# The house rule (SD one more than the mean, counts whole), then
# WEIGHT, measured more finely, one more again
p |>
  plan_digits(continuous = c(mean = 1, sd = 2), categorical = c(n = 0)) |>
  plan_digits(WEIGHT = c(mean = 2, sd = 3)) |>
  plan_apply("table")
#>    group     label        Placebo         Active
#> 1    AGE Mean (SD)    61.2 (9.81)   63.5 (10.22)
#> 2 WEIGHT Mean (SD) 72.46 (12.345) 70.12 (11.874)
#> 3    SEX         F             12              9
#> 4    SEX         M              8             11

# A lab table: the digits by parameter, as a define gives them
lb <- data.frame(
  ARM = rep(c("Placebo", "Active"), each = 4),
  PARAMCD = rep(rep(c("ALT", "BILI"), each = 2), 2),
  variable = "AVAL", .label = NA, .kind = "continuous",
  stat_name = rep(c("mean", "sd"), 4),
  stat = c(31.234, 4.102, 0.612, 0.1234, 33.111, 5.303, 0.707, 0.1515))
table_plan(lb, cols = "ARM", rows = c(param = "PARAMCD")) |>
  plan_cells(continuous = c("Mean (SD)" = "{mean} ({sd})")) |>
  plan_digits(PARAMCD = list(ALT = c(mean = 1, sd = 2), BILI = c(mean = 3, sd = 4))) |>
  plan_apply("table")
#>   param     label        Placebo         Active
#> 1   ALT Mean (SD)    31.2 (4.10)    33.1 (5.30)
#> 2  BILI Mean (SD) 0.612 (0.1234) 0.707 (0.1515)
```
