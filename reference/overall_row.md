# Where the table's overall row comes from

An adverse-events table opens with a "subjects with at least one event"
row, and where that row lives in the ARD depends on how the ARD was
built. `cards::ard_stack_hierarchical(over_variables = TRUE)` writes it
as rows whose variable is the sentinel `..ard_hierarchical_overall..`.
Build the same table by summarising each level separately and binding
the results, and there is no sentinel: the overall block counts the
**treatment itself**, so the arm sits in `variable` / `variable_level`
with no grouping pair at all. The two are indistinguishable from the
shape of the ARD, so which one this is has to be said.

## Usage

``` r
overall_row(label, from = NULL)
```

## Arguments

- label:

  Text for the overall row, e.g. `"Any TEAE"`. It is written into the
  first `hierarchy` column, so the row sits at the top level beside the
  other outermost rows.

- from:

  The analysis variable that block summarised – the treatment variable,
  usually. `NULL` (default) means the cards sentinel.

## Value

An object of class `overall_row`, for
[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)'s
`overall` argument. That argument also takes a bare string, which is
`overall_row(label)`.

## See also

[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md),
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)

## Examples

``` r
overall_row("Any TEAE")                      # the cards sentinel rows
#> $label
#> [1] "Any TEAE"
#> 
#> $from
#> character(0)
#> 
#> attr(,"class")
#> [1] "overall_row"
overall_row("Any TEAE", from = "TRT01P")     # a separately-built block
#> $label
#> [1] "Any TEAE"
#> 
#> $from
#> [1] "TRT01P"
#> 
#> attr(,"class")
#> [1] "overall_row"
```
