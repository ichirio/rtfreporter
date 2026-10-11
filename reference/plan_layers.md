# What a plan declares, and what it resolved to

Reads a plan from the outside: its roles, the layers each verb declared
(merged per kind, a later layer winning, as the resolver merges them),
and — because reading a plan means running it — what its pages came out
as: the column names, the value columns, the widths, the column header
as cell rows, the pages themselves. It is the one way another package
reads a plan: `tflspec::tfl_as_table_spec()` writes a workbook from
nothing but this and
[`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md).

## Usage

``` r
plan_layers(plan)
```

## Arguments

- plan:

  An
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

## Value

A list: `data` (the plan's data); `roles`; `label` (the label column's
name, or none); `group_col`; `declared` (the kinds of layer, in the
order declared); `layers` (by kind, the merged fields; for `after` and
`restyle` one entry per call); `cells` (the cell templates, each parsed
into its label rows and template chains — `NULL` when the statistics are
rows); `columns` (`names`, `spread`, `page_names`, `widths`); `header`
(`source`: `"cells"` for a header given as cell rows, `"resolved"` for
one the plan built; `cells`: the header as cell rows, a data frame;
`n_text`: the `{n}` scope as text, or `NULL`; `literal_n`: whether `n`
was a number written in); and `pages`.

## See also

[`plan_apply()`](https://ichirio.github.io/rtfreporter/reference/plan_apply.md),
`tflspec::tfl_as_table_spec()`

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_stack(
    cards::ADSL, .by = ARM,
    cards::ard_summary(variables = AGE),
    cards::ard_tabulate(variables = SEX))
  p <- table_plan(normalize_ard(ard), cols = "ARM",
                  rows = c(group = "variable")) |>
    plan_cells(continuous  = c("Mean (SD)" = "{mean} ({sd})"),
               categorical = "{n} ({p:%})", notes = FALSE) |>
    plan_digits(1)
  lay <- plan_layers(p)
  lay$declared                  # the kinds of layer, in the order declared
  lay$columns$names             # the columns the pages print
}
#> [1] "group"                "label"                "Placebo"             
#> [4] "Xanomeline High Dose" "Xanomeline Low Dose" 
```
