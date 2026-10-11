# A deferred, last-wins plan for a table

`table_plan()` starts a plan, and takes the **roles**: which column goes
across the table, which go down it, which carries the row identity. This
is `ggplot(data, aes(x, y))` — the names must be columns of the data you
hand it, so you can check them by looking. Every `plan_*()` verb after
it adds a declaration, and nothing runs until
[`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md).

## Usage

``` r
table_plan(x = NULL, cols = NULL, rows = NULL, label = NULL, stat = NULL)
```

## Arguments

- x:

  What the table is built from. A plan does **not** flatten: `cols` /
  `rows` / `label` name columns of what you hand it, so flatten first
  and look at the result.

  - a frame through
    [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
    — the ordinary case;

  - **any long frame of statistics**: keys, a `stat_name` and a `stat`,
    built with dplyr and no cards anywhere.
    [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
    does the work;

  - a frame that is already the table, for the display half on its own;

  - a frame of subject records, with
    [`plan_listing()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md).

  A raw cards ARD is refused, with the line to write.

- cols:

  The key that goes **across** the table, as `widen_ard(cols = )` takes
  it: one or more columns, optionally renamed `c(new = old)`.

- rows:

  The keys that go **down** it, likewise. Left out, the analysis
  variable is used (as `group`).

- label:

  The column carrying the **row identity** — the text in the label
  column. `".label"` by default (what
  [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
  builds), `NA` for a table that has none, or a guarded template.

- stat:

  Which of **your** columns play the three parts
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
  reads by name, as a named vector: `variable` (the analysis variable,
  what
  [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  and
  [`plan_digits()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  key on), `name` (which statistic a row is) and `value` (what it is
  worth) —
  `stat = c(variable = "PARAMCD", name = "STAT", value = "AVAL")`. A
  frame from cards already calls them `variable`, `stat_name` and `stat`
  and needs none of this; a summary somebody built with dplyr says so
  here, once, instead of being asked again by every verb. Name only the
  parts your frame calls something else.

## Value

An object of class `table_plan`.

## Details

The rule is **last wins** — a later layer overwrites what an earlier one
said about the same key — so "set everything, then fix one variable" is
a two-line edit:

    plan_digits(2) |> plan_digits(AGE = 0)

This is tfrmt's `frmt_structure` rule. Within one layer the keys are
picked by specificity, as
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)'s
`cells` are: an analysis variable before a kind, a kind before the
default.

## Where the other settings went

`table_plan()` takes the **roles** and nothing else. Every other setting
belongs to the verb whose job it is:

- a template per cell or a row per statistic, `stat` / `stat_fmt`, the
  empty-cell text:
  [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  (`stats =`, `value =`, `na =`)

- the statistic a frequency order totals:
  [`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  (`stat =`)

- reporting what the templates did not use:
  [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  (`notes =`)

- the separator several `cols` keys are joined with in the column names:
  [`plan_columns()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  (`sep =`)

## See also

[`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md),
[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md),
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_stack(
    cards::ADSL, .by = ARM,
    cards::ard_summary(variables = c(AGE, BMIBL)),
    cards::ard_tabulate(variables = SEX))

  ard |>
    normalize_ard() |>
    table_plan(cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous  = c("Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n} ({p:%})") |>
    plan_digits(2) |>
    plan_digits(AGE = 0, SEX = 1) |>
    plan_apply()
}
#> widen_ard(): 51 ARD rows were not used.
#>   summary        N                6  no template named it
#>   summary        max              6  no template named it
#>   summary        median           6  no template named it
#>   summary        min              6  no template named it
#>   summary        p25              6  no template named it
#>   summary        p75              6  no template named it
#>   tabulate       N                6  no template named it
#>   tabulate       N                3  a key variable's own tabulation
#>   ... and 2 more; see attr(, "ard_ignored")
#>   (notes = FALSE to silence; attr(, "ard_ignored") has the detail)
#>   group     label      Placebo Xanomeline High Dose Xanomeline Low Dose
#> 1   AGE Mean (SD)       75 (9)               74 (8)              76 (8)
#> 2 BMIBL Mean (SD) 23.64 (3.67)         25.35 (4.16)        25.06 (4.27)
#> 3   SEX         F  53.0 (61.6)          40.0 (47.6)         50.0 (59.5)
#> 4   SEX         M  33.0 (38.4)          44.0 (52.4)         34.0 (40.5)
```
