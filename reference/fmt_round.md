# Format numbers to a fixed number of decimal places

Rounds to `digits` decimal places and prints them all, trailing zeros
included – `fmt_round(2.5, 2)` is `"2.50"`, not `"2.5"`.

## Usage

``` r
fmt_round(x, digits = 2L, rounding = c("r", "sas"), na = "")
```

## Arguments

- x:

  A numeric vector. Character input is an error – character columns are
  taken as already formatted.

- digits:

  Decimal places. Default `2`.

- rounding:

  `"r"` (default) rounds with
  [`base::round()`](https://rdrr.io/r/base/Round.html), which is
  banker's rounding; `"sas"` rounds half away from zero as SAS does.
  They differ on exact halves – `23.445` is `"23.44"` under `"r"` and
  `"23.45"` under `"sas"`.

- na:

  Text for `NA` and `NaN`. Default `""` (an empty cell, which is how
  rtfreporter renders a missing value anyway).

## Value

A character vector the same length as `x`. `Inf` / `-Inf` pass through
as `"Inf"` / `"-Inf"`.

## See also

[`fmt_signif()`](https://ichirio.github.io/rtfreporter/reference/fmt_signif.md),
[`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md).

## Examples

``` r
fmt_round(c(2.5, 20.333, 100), digits = 2)
#> [1] "2.50"   "20.33"  "100.00"
fmt_round(23.445, digits = 2)                   # "23.44" -- banker's
#> [1] "23.44"
fmt_round(23.445, digits = 2, rounding = "sas") # "23.45"
#> [1] "23.45"
```
