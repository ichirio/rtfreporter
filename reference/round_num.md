# Round the way R rounds, or the way SAS rounds

The two disagree on an exact half. Base R rounds a half to the **even**
digit (`round(0.5)` is `0`, `round(2.5)` is `2`); SAS's `ROUND()` rounds
it **away from zero** (`0.5` is `1`, `2.5` is `3`, `-0.5` is `-1`).
Every formatter in the package –
[`fmt_signif()`](https://ichirio.github.io/rtfreporter/reference/fmt_signif.md),
[`fmt_round()`](https://ichirio.github.io/rtfreporter/reference/fmt_round.md),
[`fmt_numeric()`](https://ichirio.github.io/rtfreporter/reference/fmt_numeric.md),
[`format_count_pct()`](https://ichirio.github.io/rtfreporter/reference/format_count_pct.md)
– rounds with this same rule, so a study that has to match a
SAS-produced table sets it **once**:


    options(rtfreporter.rounding = "sas")

An explicit `rounding =` on any call still wins over the option.

`"sas"` also absorbs a binary representation error the way SAS's own
fuzz does: `2.675` is stored as `2.67499999999999982`, which base R
rounds to `2.67` and SAS to `2.68`.

## Usage

``` r
round_num(x, digits = 0, rounding = NULL)
```

## Arguments

- x:

  A numeric vector.

- digits:

  Decimal places. Default `0`.

- rounding:

  `"r"` (base R, half to even) or `"sas"` (half away from zero). `NULL`,
  the default, reads `getOption("rtfreporter.rounding")`, which is `"r"`
  unless the session or site set it.

## Value

A numeric vector the same length as `x`.

## See also

[`fmt_round()`](https://ichirio.github.io/rtfreporter/reference/fmt_round.md)
for the same rounding as printed text.

## Examples

``` r
round_num(c(0.5, 1.5, 2.5, -0.5))                    # 0 2 2 0
#> [1] 0 2 2 0
round_num(c(0.5, 1.5, 2.5, -0.5), rounding = "sas")  # 1 2 3 -1
#> [1]  1  2  3 -1
round_num(2.675, 2, rounding = "sas")                # 2.68
#> [1] 2.68
```
