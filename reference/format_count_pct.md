# Format count + percent cells (old name)

**Deprecated** in 0.8.x (it says so once a session and still works),
removed in 0.9.0: the function is
[`fmt_count_pct()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_pct.md)
now, named like the other cell formatters.

## Usage

``` r
format_count_pct(
  count,
  pct,
  pct_unit = c("fraction", "percent"),
  nbsp = " ",
  pct_sign = FALSE,
  na = "",
  rounding = NULL
)
```

## Arguments

- count:

  Integer / numeric vector of counts. A `0` count produces the
  count-only branch (no parentheses); a **missing** count produces the
  `na` token instead.

- pct:

  Numeric vector of percentages. By default expressed as a *fraction* in
  `[0, 1]`; pass `pct_unit = "percent"` if your values are already in
  `[0, 100]`. Recycled against `count` if one argument is length 1. A
  fraction above 1 says once a session that the values look like
  percentages (`5` prints as `500`).

- pct_unit:

  Either `"fraction"` (default, `0..1`) or `"percent"` (`0..100`).

- nbsp:

  Character used to replace the padding spaces. Default is the
  non-breaking space (Unicode code point U+00A0) so that RTF and Word do
  not collapse leading whitespace. Pass `" "` (regular space) for
  plain-text output.

- pct_sign:

  Logical (default `FALSE`). When `TRUE`, a literal `%` is placed before
  the closing parenthesis (e.g. `" 14 (50.0%)"`) and every branch is one
  character wider so the `)` still aligns.

- na:

  Text to print when the **count** is missing (`NA`, or `NaN` – R counts
  it as missing). Right-justified in the count field, so its right edge
  lands on the ones digit, then padded to the branch width. The default
  `""` returns an empty cell. A missing *percent* alongside a real count
  is not missing data: the count still prints, on its own. `Inf` /
  `-Inf` counts print as `"Inf"` / `"-Inf"` – an infinity means a
  division by zero upstream, and hiding it would hide the bug.

- rounding:

  How the percent is rounded to one decimal: `"r"` (half to even) or
  `"sas"` (half away from zero). `NULL`, the default, reads
  `getOption("rtfreporter.rounding")`; see
  [`round_num()`](https://ichirio.github.io/rtfreporter/reference/round_num.md).
  `1/16` is `6.25%`, printed `"6.2"` under `"r"` and `"6.3"` under
  `"sas"`.

## Value

As
[`fmt_count_pct()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_pct.md).

## Examples

``` r
suppressWarnings(format_count_pct(5L, 0.05))   # fmt_count_pct(5L, 0.05)
#> [1] "  5  (5.0)"
```
