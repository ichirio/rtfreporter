# What is actually inside an ARD

Prints, and returns invisibly, the structural facts you need in order to
call
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

Invisibly, a list with elements `keys`, `variables`, `contexts` and
`stats` (a data frame of context / stat_name / stat_label).

## See also

[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md),
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md),
`plan_template(form = "widen")`
