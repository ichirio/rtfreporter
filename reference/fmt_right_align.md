# Right-align the cells of a column to a common width

A minimal cell-format function (see *The contract* in the `vignette` /
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)):
every non-empty cell is right-justified to the width of the widest cell,
padding on the left with non-breaking spaces. Empty cells are left
empty. This is the simplest useful formatter and a good template for
writing your own.

## Usage

``` r
fmt_right_align(x, nbsp = " ", na = "")
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

[`fmt_count_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren.md),
[`realign_count_pct()`](https://ichirio.github.io/rtfreporter/reference/realign_count_pct.md),
and the `cell_format` argument of
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).

## Examples

``` r
fmt_right_align(c("5", "120", "7"))
#> [1] "  5" "120" "  7"
```
