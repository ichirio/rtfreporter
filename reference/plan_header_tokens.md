# The tokens a plan's column header may carry, with their values

What `print(plan)` lists under "header tokens", as data: one row per
token a
[`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
cell may carry, with the values it resolves to here. A program that
offers them (a GUI's "insert" menu that shows each token's values) reads
this instead of the printed text.

## Usage

``` r
plan_header_tokens(plan)
```

## Arguments

- plan:

  A
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md).

## Value

A data frame: `token`, `kind`, `values` (a list column: the values,
named by column where they are), `text` (as
[`print()`](https://rdrr.io/r/base/print.html) shows them), `resolved`,
`note`.

## Details

- `kind = "column"`: `{col}` (the leaf of each spread column's name)
  and, with several `cols` keys, `{col1}`, `{col2}`, ... (each depth's
  values) – known once the table has been made, which this does.

- `kind = "population"`: `{n}` (and the other names of
  `plan_col_header(values = )`), each column's population from the ARD;
  `resolved = FALSE` with the reason in `note` when the ARD does not
  state it.

- `kind = "sum"`: `{n:sum}`, the total over every column (over a
  spanner, over its own columns).

- `kind = "one"`: `{n:<column>}`, one column's value.

## See also

[`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- normalize_ard(cards::ard_stack(cards::ADSL, .by = ARM,
    cards::ard_summary(variables = AGE)))
  p <- table_plan(ard, cols = "ARM") |>
    plan_col_header(values = list(n = TRUE))
  plan_header_tokens(p)[, c("token", "text")]
}
#>                      token
#> 1                    {col}
#> 2                      {n}
#> 3                  {n:sum}
#> 4              {n:Placebo}
#> 5 {n:Xanomeline High Dose}
#> 6  {n:Xanomeline Low Dose}
#>                                                                     text
#> 1                = c(Placebo, Xanomeline High Dose, Xanomeline Low Dose)
#> 2 = c(Placebo = 86, Xanomeline High Dose = 84, Xanomeline Low Dose = 84)
#> 3         = 254 over every column (less over a spanner: its own columns)
#> 4                                                                   = 86
#> 5                                                                   = 84
#> 6                                                                   = 84
```
