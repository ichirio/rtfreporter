# Several rows in one cell recipe, each with its own fallback chain

`cells` already reads three containers: one template is one row, a
*named* character vector is one row per element, and a
[`list()`](https://rdrr.io/r/base/list.html) is a map looked up by
analysis variable. The one shape those cannot spell is a **named row
whose value is itself a chain** – `c("1" = c(a, b))` is flattened by
[`c()`](https://rdrr.io/r/base/c.html) before
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
ever sees it. `cell_rows()` is that shape, and only that shape.

## Usage

``` r
cell_rows(...)
```

## Arguments

- ...:

  One argument per output row. Names become row labels; an unnamed
  argument takes its label from `label`, as a bare template does.

## Value

An object of class `cell_rows`, for `cells` in
[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md).

## Details

Each argument is one output row: its name is the row label, its value is
a template, a chain of templates, or a chain with guards. Reading a
recipe then goes [`list()`](https://rdrr.io/r/base/list.html) for *which
variable*, `cell_rows()` for *which row*,
[`c()`](https://rdrr.io/r/base/c.html) for *which template to try
first*.

## See also

[`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md),
[ard-tables](https://ichirio.github.io/rtfreporter/reference/ard-tables.md)

## Examples

``` r
# an estimate line and a confidence-interval line, the first guarded so a
# count of zero prints as "0" rather than "0 (0.0)"
cell_rows(
  "1" = c(n == 0 ~ "0", "{n:.0f} ({estimate:.1f%})"),
  "2" = "{conf.low:.1f%}, {conf.high:.1f%}")
#> <cell_rows> 2 row(s)
#>   1              n == 0 ~ "0"  |  {n:.0f} ({estimate:.1f%})
#>   2              {conf.low:.1f%}, {conf.high:.1f%}
```
