# Align "count (parenthetical)" cells, including bare counts

Like
[`fmt_count_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren.md),
but a **bare integer count** with no parentheses (a lone `"0"` for a
zero count, or a raw event total) is also padded into the same count
field, so it lines up under the parenthetical cells instead of drifting
out of line. Cells that do not start with an integer (text, decimals,
empty cells) are still returned unchanged.

## Usage

``` r
fmt_count_paren_bare(x, nbsp = " ", na = "")
```

## Arguments

- x:

  Character vector (one table column).

- nbsp:

  Padding character; defaults to the non-breaking space (U+00A0) so RTF
  / Word keep the alignment. Pass `" "` for plain text.

- na:

  Text to print for a missing value (`NA`, and `NaN` – R counts it as
  missing). The default `""` leaves the cell empty, as before. A
  non-empty token is right-justified with the other cells, so its right
  edge lines up with theirs. `Inf` / `-Inf` are **not** missing and
  print as `"Inf"` / `"-Inf"`. See the `na` argument of
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).

## Value

Character vector the same length as `x`.

## See also

[`fmt_count_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren.md)
(parenthetical cells only).

## Examples

``` r
# The lone "0" is padded to share the column width.
fmt_count_paren_bare(c("1 (1.2%)", "0", "11 (3.6%)", "108 (35.3%)"))
#> [1] "  1 ( 1.2%)" "  0        " " 11 ( 3.6%)" "108 (35.3%)"
```
