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

## Examples

``` r
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_stack(
    cards::ADSL, .by = ARM,
    cards::ard_summary(variables = AGE),
    cards::ard_tabulate(variables = SEX))
  keys <- list_ard_keys(ard)    # prints the keys, variables and statistics
  keys$keys
}
#> ARD keys (group1..groupN values) : ARM 
#> Analysis variables               : AGE, SEX, ARM 
#> Contexts (cards-version specific): summary, tabulate 
#> Structural kinds (stable)        : continuous, categorical 
#> 
#> Per variable -- key `cells` on `kind` unless you need the context:
#>  variable        kind  context
#>       AGE  continuous  summary
#>       SEX categorical tabulate
#>       ARM categorical tabulate
#> 
#> Statistics per context:
#>   context stat_name stat_label
#>   summary         N          N
#>   summary      mean       Mean
#>   summary        sd         SD
#>   summary    median     Median
#>   summary       p25         Q1
#>   summary       p75         Q3
#>   summary       min        Min
#>   summary       max        Max
#>  tabulate         n          n
#>  tabulate         N          N
#>  tabulate         p          %
#> [1] "ARM"
```
