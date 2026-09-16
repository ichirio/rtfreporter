# Format the numeric columns of a table for display

Applies
[`fmt_signif()`](https://ichirio.github.io/rtfreporter/reference/fmt_signif.md)
or
[`fmt_round()`](https://ichirio.github.io/rtfreporter/reference/fmt_round.md)
across a data frame, turning the selected **numeric** columns into the
text the report should show. Run it before building the table; character
columns are taken as already formatted and are never touched, and
neither are columns outside `cols`.

## Usage

``` r
fmt_numeric(
  data,
  cols,
  by = NULL,
  formats = NULL,
  signif = NULL,
  digits = NULL,
  rounding = c("r", "sas"),
  small = c("signif", "fixed"),
  na = ""
)
```

## Arguments

- data:

  A data frame.

- cols:

  Columns to format: names or positions. A selected column that is not
  numeric is left alone.

- by:

  Optional carrier column (name or position) keying into `formats`. May
  be the row-heading column. Values are matched after trimming.

- formats:

  Named list of rules, used with `by`. Each entry gives exactly one of
  `signif` or `digits`, and may add `small`. The reserved name
  `.default` covers unmatched keys.

- signif, digits:

  Used **without** `by` to format every selected cell the same way. Give
  exactly one of them.

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

`data` with the selected numeric columns replaced by character columns.
Row count, column order and every other column are unchanged.

## One column, several formats – the carrier column

A clinical column holds several statistics, each with its own format, so
a per-column setting is not enough. `by` names a **carrier column**
whose values key into `formats`. It can be the **row-heading (stub)
column itself**: values are matched after trimming, so an indented
`" Mean"` keys on `"Mean"` and no extra column is needed.


    pk |> fmt_numeric(
      cols = visits, by = "Nominal Time (h)",
      formats = list(
        n        = list(digits = 0),
        Mean     = list(signif = 4),
        `CV%`    = list(digits = 1),
        .default = list(signif = 4)))

A row whose selected cells are all missing needs no key, so group-label
and blank rows fall out on their own. A **non-missing** cell whose key
matches nothing and has no `.default` is an error naming the unmatched
keys – quietly printing 15 digits into a submission table is the worse
outcome.

## See also

[`fmt_signif()`](https://ichirio.github.io/rtfreporter/reference/fmt_signif.md),
[`fmt_round()`](https://ichirio.github.io/rtfreporter/reference/fmt_round.md);
[`set_decimal_split()`](https://ichirio.github.io/rtfreporter/reference/set_decimal_split.md)
to line the printed decimal points up afterwards.

## Examples

``` r
df <- data.frame(
  stat = c("n", "Mean", "SD"),
  trt  = c(24, 902.3312, 230.1234)
)
# one rule for the whole column
fmt_numeric(df, cols = "trt", signif = 4)
#>   stat   trt
#> 1    n 24.00
#> 2 Mean 902.3
#> 3   SD 230.1

# per-statistic, keyed on the row-heading column
fmt_numeric(df, cols = "trt", by = "stat",
            formats = list(n    = list(digits = 0),
                           Mean = list(signif = 4),
                           SD   = list(signif = 4)))
#>   stat   trt
#> 1    n    24
#> 2 Mean 902.3
#> 3   SD 230.1
```
