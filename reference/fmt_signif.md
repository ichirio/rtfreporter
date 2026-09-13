# Format numbers to a number of significant digits

Renders a numeric vector as the text a clinical table should show,
counting **total printed digits including the integer part** – the
convention a SAP means by "report to 4 significant digits", which is not
the same as [`base::signif()`](https://rdrr.io/r/base/Round.html):


    fmt_signif(c(0, 10.2, 103.4, 20.333333, 23.4463), digits = 4)
    #> "0.000" "10.20" "103.4" "20.33" "23.45"

The decimal places are `digits` minus the number of digits in the
integer part, never below zero – so an integer part longer than `digits`
simply prints whole (`12345.6` at 4 digits is `"12346"`; integer digits
are never dropped). When rounding carries the value into another decade
the decimals are recomputed, so the result really does carry `digits`
digits (`99.995` is `"100.0"`, not `"100.00"`).

## Usage

``` r
fmt_signif(
  x,
  digits = 3L,
  rounding = c("r", "sas"),
  small = c("signif", "fixed"),
  na = ""
)
```

## Arguments

- x:

  A numeric vector. Character input is an error – character columns are
  taken as already formatted.

- digits:

  Total significant digits. Default `3`.

- rounding:

  `"r"` (default) rounds with
  [`base::round()`](https://rdrr.io/r/base/Round.html), which is
  banker's rounding; `"sas"` rounds half away from zero as SAS does.
  They differ on exact halves – `23.445` is `"23.44"` under `"r"` and
  `"23.45"` under `"sas"`.

- small:

  `"signif"` (default) or `"fixed"`; see *Values below 1*.

- na:

  Text for `NA` and `NaN`. Default `""` (an empty cell, which is how
  rtfreporter renders a missing value anyway).

## Value

A character vector the same length as `x`. `Inf` / `-Inf` pass through
as `"Inf"` / `"-Inf"`.

## Values below 1

`small` decides what a value with no integer part means. `"signif"` (the
default) counts from the first significant digit, so precision is kept;
`"fixed"` applies the integer-counting rule everywhere, which can erase
a small value entirely:

|             |                |               |
|-------------|----------------|---------------|
| **x**       | **`"signif"`** | **`"fixed"`** |
| `0.333333`  | `0.3333`       | `0.333`       |
| `0.0004567` | `0.0004567`    | `0.000`       |
| `0.00998`   | `0.009980`     | `0.010`       |

`0.000` for a concentration near the limit of quantitation is the reason
`"signif"` is the default. Zero itself prints with `digits - 1` decimals
under both (`"0.000"` at 4), having no significant digits to count.

## See also

[`fmt_round()`](https://ichirio.github.io/rtfreporter/reference/fmt_round.md)
for plain decimal places,
[`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md)
to apply either across a data frame, and
[`set_decimal_split()`](https://ichirio.github.io/rtfreporter/reference/set_decimal_split.md)
to line the printed decimal points up.

## Examples

``` r
fmt_signif(c(0, 10.2, 103.4, 20.333333, 23.4463), digits = 4)
#> [1] "0.000" "10.20" "103.4" "20.33" "23.45"
fmt_signif(0.0004567, digits = 4)                      # "0.0004567"
#> [1] "0.0004567"
fmt_signif(0.0004567, digits = 4, small = "fixed")     # "0.000"
#> [1] "0.000"
fmt_signif(23.445, digits = 4, rounding = "sas")       # "23.45"
#> [1] "23.45"
```
