# Line up the decimal points of a numeric column

Marks one or more body columns to be rendered as **two adjacent RTF
cells**: the part before the first decimal separator (right-aligned) and
the separator plus everything after it (left-aligned). The decimal
points then line up exactly, whatever the font, instead of merely
right-aligning the last character.

## Usage

``` r
set_decimal_split(x, ...)

# S3 method for class 'rtftable'
set_decimal_split(
  x,
  cols = NULL,
  ratio = NULL,
  decimal_mark = ".",
  pad_chars = c(1, 1),
  min_chars = c(4, 6),
  max_chars = 10,
  include_compound = FALSE,
  ...
)

# S3 method for class 'list'
set_decimal_split(x, ...)
```

## Arguments

- x:

  An
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
  or a list of them (pages from
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).

- ...:

  Unused.

- cols:

  Columns to split: names or positions. `NULL` clears the setting.

- ratio:

  `NULL` (the default) to size the two halves automatically from the
  widest left and right part actually present, or a number in `(0, 1)`
  giving the left half's share of the column width. Pass an explicit
  value to keep the split point identical across pages, whose data
  differ.

- decimal_mark:

  The separator to split at. Default `"."`.

- pad_chars:

  Breathing room added to each measured half before the ratio is taken,
  in character-width units, integer half first. Default `c(1, 1)`.

- min_chars:

  Floor for each half, in the same units, applied after `pad_chars`: the
  effective width of a half is `max(measured + pad_chars, min_chars)`.
  Default `c(4, 6)` – a three-digit integer part and a five-character
  decimal part, each plus its padding character – so anything at or
  below that size holds a `4 : 6` baseline and anything larger scales
  from its own measurement. Without a floor a column of `0.0000` values
  measures 1 against 5 and leaves the integer half cramped. Pass
  `pad_chars = c(0, 0), min_chars = c(0, 0)` for the raw measured ratio.

- max_chars:

  Total measured width, in the same units, above which the allowance is
  dropped and the raw measured proportions are used however lopsided.
  Default `10`: a column that long needs every twip it has, and
  reserving room it does not use would squeeze the digits it does. `Inf`
  never drops the allowance.

- include_compound:

  Split values carrying a whitespace- or parenthesis-separated companion
  (`"12.3 (4.56)"`) too. Default `FALSE`.

## Value

An object of the same shape as `x`.

## Details

This is an **RTF-output option**, not a data transformation. The table's
data frame is never rewritten, the total table width is unchanged (only
the column's own width is divided), and the column-header block keeps
rendering over the original single column. The console preview
([`print()`](https://rdrr.io/r/base/print.html) /
[`format()`](https://rdrr.io/r/base/format.html)) therefore does not
show the split.

## Which cells are split

A cell is split when it starts with an optional relational or sign
prefix (`<`, `>`, `>=`, `<=`, `~`, `+`, `-`) followed by a digit or the
separator, and contains at least one digit. The prefix travels with the
left half and any suffix (`%`, a `^{a}` footnote marker, ...) with the
right half, so both hang outside the aligned point:

|          |          |           |
|----------|----------|-----------|
| **cell** | **left** | **right** |
| `3.45`   | `3`      | `.45`     |
| `-0.7`   | `-0`     | `.7`      |
| `<0.001` | `<0`     | `.001`    |
| `45.6%`  | `45`     | `.6%`     |
| `100`    | `100`    | (empty)   |

Everything else – free text such as `"n (%)"` or a group label – is
rendered as **one cell across the pair**, exactly as it looks without
the option. Leading and trailing spaces (non-breaking ones included, as
left by
[`fmt_right_align()`](https://ichirio.github.io/rtfreporter/reference/fmt_right_align.md))
are dropped from split cells, since the split supplies the alignment
they were emulating.

Compound values such as `"12.3 (4.56)"` (mean (SD)) are excluded by
default: the trailing group would dominate the right half and drag the
split point far to the left. Set `include_compound = TRUE` to split them
anyway.

A selected column in which no cell carries a separator is left
untouched. That decision is made per table, so across paginated pages a
column may be split on one page and not on another – a PK visit column
that is all `BLQ` in one time band, for instance. Each page stays
internally consistent, which is what the geometry needs.

## See also

[`style_cols()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
for ordinary column alignment;
[`fmt_right_align()`](https://ichirio.github.io/rtfreporter/reference/fmt_right_align.md)
and
[`fmt_count_paren()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_paren.md)
for the text-padding alternatives.

## Examples

``` r
df  <- data.frame(
  Statistic = c("Mean", "SD", "p-value"),
  Value     = c("12.3", "1.05", "<0.001"),
  stringsAsFactors = FALSE
)
tbl <- rtftable(df) |> set_decimal_split(cols = "Value")
```
