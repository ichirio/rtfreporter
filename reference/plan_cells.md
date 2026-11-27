# Say how each cell is made from the statistics

`plan_cells()` gives the **cell templates**: the text a cell prints,
written with the statistics' names in braces (`"{mean} ({sd})"`,
`"{n} ({p:%})"`). It is usually the first verb after
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md),
since it decides what the rows of a block are;
[`plan_digits()`](https://ichirio.github.io/rtfreporter/reference/plan_digits.md)
then fills in the decimals the templates leave open. It also takes the
settings of how cells are made — a row per statistic instead of a
template, which number a token reads, what an empty cell prints — as
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
takes them.

## Usage

``` r
plan_cells(.plan, ..., stats = NULL, value = NULL, na = NULL, notes = NULL)
```

## Arguments

- .plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).
  The pipe passes it by position; the dot keeps a name in `...` such as
  `p` (cards' percentage) from being taken for it.

- ...:

  The templates, as `widen_ard(cells = )` takes them: one bare entry for
  every cell, or entries named by what they apply to. Each entry is a
  template, a named vector of templates (one row each, the names the
  rows' labels), guards (`c(n == 0 ~ "0", "{n} ({p})")`) or
  [`cell_rows()`](https://ichirio.github.io/rtfreporter/reference/cell_rows.md).
  The names, from the most specific:

  `<variable>`

  :   an analysis variable, `AGE = "{mean} ({sd})"`.

  `<stat group>`

  :   a group of
      [`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md),
      `orr_ci = cell_rows(...)`.

  `<context>`

  :   an ARD context, `tabulate = "{n}"`.

  `continuous`, `categorical`

  :   a kind of variable.

  `default`

  :   everything else (the unnamed entry).

  In a table of
  [`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_stat_groups.md),
  a record of a split variable (`.variables`) takes its variable's
  entry, else its **group's**, before its context or kind; a record that
  went whole to `.others` takes its variable's, then its context's or
  kind's, its group's only after them. The names of the arguments after
  `...` are reserved: a variable called `stats`, `value`, `na` or
  `notes` cannot be keyed here; name it otherwise in the ARD.

  A later call replaces a key's entry as a whole, a named vector of
  templates too: its rows are not merged with the earlier call's.

  An unnamed vector of several templates, `c("{n} ({p})", "{n}")`, is
  one row: the first template whose statistics the record has wins, as
  with guards. A template with no token prints as written, so a constant
  cell is text (`"-"`, and `""` an empty cell). A list holds the same
  things (`list(n = "{N}", Mean = "{mean}")` is the named vector).
  Anything else — `NULL`, `NA`, a number, `TRUE`, a factor,
  `character(0)`, a template that is `NA` — stops at the verb, naming
  the key.

- stats:

  `"cells"` (default) fills a template per cell; `"rows"` makes each
  statistic a row of its own (no template).

- value:

  With `stats = "rows"`: the raw number (`"stat"`) or cards' formatted
  text (`"stat_fmt"`) in each row. With templates an open `{x}` takes
  `stat_fmt` when the ARD has it and `stat` otherwise, and
  `{x:stat_fmt}` / `{x:.1f}` say so per token.

- na:

  What fills a cell no template could (a statistic the ARD does not
  have). On a plan of a table that is already built (no statistics),
  `na` alone is taken:
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s
  `na`, what a missing value prints as.

- notes:

  `FALSE` stops the message listing the statistics no template used;
  `TRUE` (or `NULL`, the default) reports them.

## Value

The plan, with one more layer. The settings hold however the plan is
run, by
[`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md)
or by `rtf_tables(doc, plan)`.

## See also

[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
for every verb;
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
for the template language.

Other plan verbs:
[`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_after.md),
[`plan_append()`](https://ichirio.github.io/rtfreporter/reference/plan_append.md),
[`plan_blanks()`](https://ichirio.github.io/rtfreporter/reference/plan_blanks.md),
[`plan_cell_style()`](https://ichirio.github.io/rtfreporter/reference/plan_cell_style.md),
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
# Invented summary statistics, one row a statistic (no cards needed)
st <- data.frame(
  ARM       = rep(c("Placebo", "Active"), each = 7),
  variable  = rep(c("AGE", "AGE", "AGE", "SEX", "SEX", "SEX", "SEX"), 2),
  .label    = rep(c(NA, NA, NA, "F", "F", "M", "M"), 2),
  .kind     = rep(c(rep("continuous", 3), rep("categorical", 4)), 2),
  stat_name = rep(c("mean", "sd", "median", "n", "p", "n", "p"), 2),
  stat      = c(61.2, 9.81, 60, 12, 0.6, 8, 0.4,
                63.5, 10.2, NA, 20, 1, 0, 0))
p <- table_plan(st, cols = "ARM", rows = c(group = "variable"))

# Minimal: one template per kind, the digits written in the template;
# the name of a continuous template is its row's label
p |>
  plan_cells(continuous  = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
             categorical = "{n} ({p:.1f%})", notes = FALSE) |>
  plan_apply("table")
#>   group     label     Placebo       Active
#> 1   AGE Mean (SD) 61.2 (9.81) 63.5 (10.20)
#> 2   SEX         F   12 (60.0)   20 (100.0)
#> 3   SEX         M    8 (40.0)      0 (0.0)

# A demographics block: AGE as two named rows, a zero count printed
# as "0" rather than "0 (0.0)", and a median the ARD lacks as "-"
p |>
  plan_cells(continuous  = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})",
                             "Median"    = "{median:.1f}"),
             categorical = c(n == 0 ~ "0", "{n} ({p:.1f%})"),
             na = "-") |>
  plan_apply("table")
#>   group     label     Placebo       Active
#> 1   AGE Mean (SD) 61.2 (9.81) 63.5 (10.20)
#> 2   AGE    Median        60.0            -
#> 3   SEX         F   12 (60.0)   20 (100.0)
#> 4   SEX         M    8 (40.0)            0
```
