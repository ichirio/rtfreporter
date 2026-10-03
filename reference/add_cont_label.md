# Prepend a continuation label row to a paginated chunk

A small helper for writing custom `split=` functions for
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).
When a group is split across pages, clinical tables repeat the group
label at the top of the continuation page with a `" (Cont.)"` suffix.
`add_cont_label()` builds that row: it prepends a blank row to `chunk`
and places `paste0(label, cont_label)` in column `col`, leaving every
other cell empty (`""` for character columns, `NA` otherwise). A
**factor** `col` is coerced to character first, so it can hold the
suffixed label.

## Usage

``` r
add_cont_label(chunk, label, cont_label = " (Cont.)", col = 1L)
```

## Arguments

- chunk:

  A data.frame – a single continuation page produced by your split
  function.

- label:

  Character scalar: the group label to repeat (without the continuation
  suffix).

- cont_label:

  Character scalar appended to `label`. Default `" (Cont.)"`, matching
  the built-in group strategies.

- col:

  Integer or character column where the label is placed. Default `1`
  (the row-label column).

## Value

`chunk` with one extra row prepended.

## See also

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
for the `split=` custom-function contract.

## Examples

``` r
df <- data.frame(group = c("B", "B"), value = c("3", "4"))
add_cont_label(df, label = "Group B")
#>             group value
#> 1 Group B (Cont.)      
#> 2               B     3
#> 3               B     4
```
