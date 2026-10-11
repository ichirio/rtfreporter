# Run a plan, or look inside it

Resolves an
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)'s
layers and runs the conversion. `stage` stops it early, so the same one
pass answers "what does this do" and "what did it do" — there is no
second code path that could disagree with the first.

## Usage

``` r
plan_apply(plan, stage = c("auto", "input", "args", "table", "pages"))
```

## Arguments

- plan:

  An
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

- stage:

  How far to go. `"auto"`, the default, is **as far as the plan
  declares**: a plan that says nothing about the display stops at the
  table `data.frame`; one that carries a display verb goes on to the RTF
  pages. You rarely need this function at all —
  [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
  takes a plan directly — and naming a stage is for looking inside:
  `"input"`, `"args"`, `"table"`, `"pages"`.

  The named stages: `"table"` returns the table `data.frame`, the same
  object
  [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
  returns. `"input"` returns the frame going in, with the ARD column
  names the roles renamed. `"args"` returns the resolved argument lists
  without running anything — the call the plan amounts to, as `$widen`
  ([`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)'s)
  and `$rtf`
  ([`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s).
  Both are resolved from layers and either can be the one that
  surprises: a page budget declared twice is last-wins, and the call you
  are editing may not be the one that decides, so
  `plan_apply(p, "args")$rtf$max_rows` is the way to ask. `"pages"` goes
  all the way:
  [`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md),
  [`stub_cols()`](https://ichirio.github.io/rtfreporter/reference/stub_cols.md),
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
  and whatever
  [`plan_after()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  declared, giving the RTF pages.

## Value

A data frame; for `stage = "args"` the resolved argument list; for
`stage = "pages"` what
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
and the steps after it return.

## See also

[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md),
[plan_verbs](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  p <- cards::ard_stack(
         cards::ADSL, .by = ARM,
         cards::ard_summary(variables = AGE)) |>
    normalize_ard() |>
    table_plan(cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c("Mean (SD)" = "{mean} ({sd})")) |>
    plan_digits(2) |>
    plan_digits(AGE = 0)

  str(plan_apply(p, "args")$cells)   # AGE won
  plan_apply(p)
}
#>  NULL
#> widen_ard(): 27 ARD rows were not used.
#>   tabulate       N                3  a key variable's own tabulation
#>   tabulate       n                3  a key variable's own tabulation
#>   tabulate       p                3  a key variable's own tabulation
#>   summary        N                3  no template named it
#>   summary        max              3  no template named it
#>   summary        median           3  no template named it
#>   summary        min              3  no template named it
#>   summary        p25              3  no template named it
#>   ... and 1 more; see attr(, "ard_ignored")
#>   (notes = FALSE to silence; attr(, "ard_ignored") has the detail)
#>   group     label Placebo Xanomeline High Dose Xanomeline Low Dose
#> 1   AGE Mean (SD)  75 (9)               74 (8)              76 (8)
```
