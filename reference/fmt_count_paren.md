# Align "count (parenthetical)" cells

Aligns clinical cells made of an integer **count** followed by a
**parenthetical** part – e.g. `"69 (80.2%)"`, `"3 (<1%)"`,
`"70 (100%)"`. It scans the whole column, then right-justifies the count
to the widest count and right-justifies the text *inside* the
parentheses to the widest one, so the count digit **and** the percentage
line up across rows.

## Usage

``` r
fmt_count_paren(x, nbsp = " ", na = "")
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

## Details

Only cells that have parentheses are touched; cells **without** them – a
lone count such as `"0"` or a raw total, a continuous statistic like
`"75.2 (8.6)"` whose "count" is not an integer, free text, or empty
group-label cells – are returned **unchanged**. Use
[`fmt_count_paren_bare()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren_bare.md)
if you also want bare integer counts padded into the same column.

Unlike the fixed-width
[`realign_count_pct()`](https://ichirio.github.io/rtfreporter/reference/realign_count_pct.md)
this adapts to the column's actual digit counts and does not care what
is *inside* the parentheses, coping with mixed notations like `"(<1%)"`,
`"(100%)"` and `"( 2.8%)"` in one column (e.g. tables produced by
`tfrmt`).

With `na` set, a missing cell prints that token right-justified in the
count field – its right edge under the ones digit – so it stays in line
instead of falling out of the column.

## See also

[`fmt_count_paren_bare()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren_bare.md),
[`fmt_right_align()`](https://ichirio.github.io/rtfreporter/reference/fmt_right_align.md),
[`realign_count_pct()`](https://ichirio.github.io/rtfreporter/reference/realign_count_pct.md),
and the `cell_format` argument of
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).

## Examples

``` r
# Only the parenthetical cells are aligned; the lone "0" is left as-is.
fmt_count_paren(c("1 (1.2%)", "0", "11 (3.6%)", "108 (35.3%)"))
#> [1] "  1 ( 1.2%)" "0"           " 11 ( 3.6%)" "108 (35.3%)"

# A missing cell lines up under the counts.
fmt_count_paren(c("1 (1.2%)", NA, "108 (35.3%)"), na = "-", nbsp = " ")
#> [1] "  1 ( 1.2%)" "  -        " "108 (35.3%)"
```
