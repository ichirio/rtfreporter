# Re-align existing "n (xx.x)" strings to a uniform display width

**Deprecated** in 0.8.x (it says so once a session and still works),
removed in 0.9.0: the same alignment is
`as_rtftables(align_count_pct = TRUE)`, or the built-in cell format
`"count_pct"` (`as_rtftables(cell_format = "count_pct")`).

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
  [`fmt_count_pct()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_pct.md)).

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

Scans `x` for cells matching the clinical-TFL pattern `"n (xx.x)"` (e.g.
`"5 (33.3)"`), parses the count and percent, and reformats them through
[`fmt_count_pct()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_pct.md)
so every cell is the same width. Cells that do not match are returned
unchanged.

## See also

[`fmt_count_pct()`](https://ichirio.github.io/rtfreporter/reference/fmt_count_pct.md)
for the numeric -\> string variant.

## Examples

``` r
# deprecated: the same as as_rtftables(align_count_pct = TRUE) does
suppressWarnings(
  realign_count_pct(c("5 (33.3)", "12 (100.0)", "0 (0.0)",
                      "not a count", "1 (5.0)", "1 (50.0)")))
#> [1] "  5 (33.3)"  " 12  (100)"  "  0       "  "not a count" "  1  (5.0)" 
#> [6] "  1 (50.0)" 
```
