# What is actually inside an ARD

Returns the structural facts you need in order to call
[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
and
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md):
the grouping-variable names that appear in the `group1..groupN` columns,
the analysis variables, the `context` values and the statistics each
context carries. All of it is read from the tibble's rows – never from
the object's attributes.

## Usage

``` r
list_ard_keys(x)
```

## Arguments

- x:

  A cards/cardx ARD (any data frame with the ARD columns).

## Value

A list of class `"rtfreporter_ard_keys"` with elements `keys`,
`variables`, `contexts`, `kinds` (the structural kinds, a data frame of
variable / kind / context, or `NULL` when the ARD cannot be normalised)
and `stats` (a data frame of context / stat_name / stat_label). Its
print method shows them, so typing the call prints them and assigning
the result prints nothing.

## See also

[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md),
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md),
`plan_template(form = "widen")`

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_stack(
    cards::ADSL, .by = ARM,
    cards::ard_summary(variables = AGE),
    cards::ard_tabulate(variables = SEX))
  list_ard_keys(ard)            # prints the keys, variables and statistics
  keys <- list_ard_keys(ard)    # prints nothing
  keys$keys
}
#> [1] "ARM"
```
