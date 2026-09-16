# Format count + percent cells to a uniform display width

Returns each pair `(count[i], pct[i])` as a padded `"n (xx.x)"` string
suitable for monospaced clinical TFL alignment. The four width branches
match the convention in the rtfreporter Issue \#2 reference helper.

## Usage

``` r
format_count_pct(
  count,
  pct,
  pct_unit = c("fraction", "percent"),
  nbsp = " ",
  pct_sign = FALSE,
  na = ""
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
  `[0, 100]`. Recycled against `count` if one argument is length 1.

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

## Value

Character vector the same length as `count` / `pct`.

## See also

[`realign_count_pct()`](https://ichirio.github.io/rtfreporter/reference/realign_count_pct.md)
for the same widths starting from already-formatted strings.

## Examples

``` r
# Fractions (the default)
format_count_pct(c(5L, 14L, 30L), c(0.05, 0.50, 1.00))
#> [1] "  5  (5.0)" " 14 (50.0)" " 30  (100)"

# Percent values
format_count_pct(c(5L, 14L, 30L), c(5, 50, 100), pct_unit = "percent")
#> [1] "  5  (5.0)" " 14 (50.0)" " 30  (100)"

# Plain spaces if the output is going to plain text rather than RTF
format_count_pct(7L, 0.333, nbsp = " ")
#> [1] "  7 (33.3)"

# A missing count, shown as "-" under the ones digit
format_count_pct(c(5L, NA), c(0.05, NA), na = "-", nbsp = " ")
#> [1] "  5  (5.0)" "  -       "
```
