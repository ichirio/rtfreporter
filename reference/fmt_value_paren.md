# Align "value (parenthetical)" cells, text values included

The general form of
[`fmt_count_paren_bare()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren_bare.md):
the value before the parenthesis may be **any text**, and cells with and
without a parenthesis are both padded into one column.

## Usage

``` r
fmt_value_paren(x, nbsp = " ", na = "")
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

Three rules, applied to the whole column at once:

- the **value** is right-justified, so its rightmost character lines up
  – whether it is a count (`"86"`), a fraction (`"3/12"`), an `"n=3"` or
  a word;

- the whole `(...)` block is right-justified to a common right edge, so
  the padding falls **before** the `(` and never inside the parentheses.
  Every `)` lands in the same column, and cells sharing a decimal layout
  line up on the decimal point and the ones digit – the rule
  [`format_count_pct()`](https://ichirio.github.io/rtfreporter/reference/format_count_pct.md)
  has always used. A parenthetical with no decimals, `"(100)"`,
  therefore sits flush against its `)` and is *not* digit-aligned with
  the decimal cells;

- a **zero count drops its parenthetical**: `"0 (0.0)"` prints as `"0"`,
  again as
  [`format_count_pct()`](https://ichirio.github.io/rtfreporter/reference/format_count_pct.md)
  does. Only an all-zero parenthetical is dropped, so `"0 (BLQ)"` keeps
  what it says;

- **100% loses its decimals**: `"(100.0)"` prints as `"(100)"` and
  `"(100.0%)"` as `"(100%)"` –
  [`format_count_pct()`](https://ichirio.github.io/rtfreporter/reference/format_count_pct.md)'s
  `pct >= 100` branch, which formats that one as an integer. Only a
  plain number is rewritten; `"<100.0"` and any other notation is left
  exactly as written.

    12  (100)          <- no decimals: flush against the ")", digits not aligned
     6 (50.0)
     1  (8.3)          <- the "8" under the "0" of 50.0
     0                 <- a zero count, and cells with no parenthesis at all,
                           padded into the same field

Empty cells, and cells whose parenthesis is not the last thing in the
cell (`"Mean (SD) by visit"`), are returned unchanged – as is a cell
with nothing before the parenthesis, since there is no value to line up.
Nested parentheses are not parsed.

Compared with the other built-ins:
[`fmt_count_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren.md)
touches only parenthetical cells and only when the value is a bare
integer, and pads *inside* the parentheses;
[`fmt_count_paren_bare()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren_bare.md)
adds bare integers to the same field; this one drops the integer
requirement, pads before the `(` as
[`format_count_pct()`](https://ichirio.github.io/rtfreporter/reference/format_count_pct.md)
does, and prints a zero count on its own.

## See also

[`fmt_count_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren.md),
[`fmt_count_paren_bare()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren_bare.md),
[`format_count_pct()`](https://ichirio.github.io/rtfreporter/reference/format_count_pct.md)
for the same parenthesis rules from numbers, and the `cell_format`
argument of
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).

## Examples

``` r
# A zero count prints on its own; "(100)" is flush against its ")".
fmt_value_paren(c("12 (100)", "6 (50.0)", "1 (8.3)", "0 (0.0)"), nbsp = " ")
#> [1] "12  (100)" " 6 (50.0)" " 1  (8.3)" " 0       "

# The value may be any text -- its rightmost character is what lines up.
fmt_value_paren(c("86", "12 (14.0)", "n=3 (3.5)"), nbsp = " ")
#> [1] " 86       " " 12 (14.0)" "n=3  (3.5)"
```
