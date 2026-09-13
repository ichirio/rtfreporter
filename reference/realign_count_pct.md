# Re-align existing "n (xx.x)" strings to a uniform display width

Scans `x` for cells matching the clinical-TFL pattern `"n (xx.x)"` (e.g.
`"5 (33.3)"`), parses the count and percent, and reformats them through
[`format_count_pct()`](https://ichirio.github.io/rtfreporter/reference/format_count_pct.md)
so every cell is the same width. Cells that do not match are returned
unchanged.

## Usage

``` r
realign_count_pct(x, nbsp = " ", na = "")
```

## Arguments

- x:

  Character vector. Cells that match the regex
  `^\\d+ \\(\\d+(\\.\\d+)?\\)$` are reformatted; all others are returned
  unchanged.

- nbsp:

  Padding character (see
  [`format_count_pct()`](https://ichirio.github.io/rtfreporter/reference/format_count_pct.md)).

- na:

  Text to print for a missing cell – an `NA` in `x`, or a cell that
  already holds this token because
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  substituted it. It is right-justified in the count field, so its right
  edge lands on the ones digit. The default `""` leaves the cell empty,
  as before. Text that is neither the token nor a count/percent cell
  (`"NE"`, `"n/a"`, free text) is still returned unchanged and unpadded.

## Value

Character vector the same length as `x`.

## Details

This is the function
[`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)
invokes internally when `align_count_pct = TRUE` (see
[`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)).
It is exported so it can be applied directly to a data.frame column
outside of any pagination context.

## See also

[`format_count_pct()`](https://ichirio.github.io/rtfreporter/reference/format_count_pct.md)
for the numeric -\> string variant.

## Examples

``` r
realign_count_pct(c("5 (33.3)", "12 (100.0)", "0 (0.0)",
                    "not a count", "1 (5.0)", "1 (50.0)"))
#> [1] "  5 (33.3)"  " 12  (100)"  "  0       "  "not a count" "  1  (5.0)" 
#> [6] "  1 (50.0)" 

# A missing cell, shown as "-" under the ones digit
realign_count_pct(c("5 (33.3)", NA, "12 (100.0)"), na = "-", nbsp = " ")
#> [1] "  5 (33.3)" "  -       " " 12  (100)"
```
