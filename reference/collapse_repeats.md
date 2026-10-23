# Blank out consecutive repeated values in a finished table

Suppresses consecutive repeated values in the chosen columns of a built
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
keeping only the first row of each run; suppressed cells become `NA`. No
rows are removed – only the display value is blanked (the renderer draws
`NA` as an empty cell). This is the classic "don't repeat the group
label on every row" layout for clinical listings.

## Usage

``` r
collapse_repeats(x, cols)

# S3 method for class 'rtftable'
collapse_repeats(x, cols)

# S3 method for class 'list'
collapse_repeats(x, cols)

# Default S3 method
collapse_repeats(x, cols)
```

## Arguments

- x:

  An
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
  or a list of them (pages from
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).

- cols:

  Columns to collapse: character names and/or integer positions in the
  table's **final** body columns. Mix names and positions with a
  [`list()`](https://rdrr.io/r/base/list.html) (a bare
  [`c()`](https://rdrr.io/r/base/c.html) would coerce the numbers to
  strings). Processed in the supplied order (outermost group first).

## Value

An object of the same shape as `x` (rtftable, or list of pages), with
suppressed cells set to `NA` in the selected columns.

## Details

Like the other post-hoc verbs it is an S3 generic with an `rtftable`
method and a **list** method. Given a **list of pages** (an
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
result) it collapses **each page independently**, so a run restarts at
every page break – the first row of each page shows its value again.
This makes `collapse_repeats(as_rtftables(x, ...), cols)` equivalent to
`as_rtftables(x, collapse_repeats = cols, ...)`.

Suppression is **hierarchical**: columns are processed in the order
given, so a change in any earlier-listed column resets the run of every
later one.

## See also

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
(the equivalent per-page `collapse_repeats` argument);
[`style_body()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
and the other post-hoc verbs.

## Examples

``` r
tbl <- rtftable(data.frame(
  grp = c("A", "A", "A", "B", "B"),
  sub = c("x", "x", "y", "x", "x"),
  n   = 1:5,
  stringsAsFactors = FALSE
))
out <- collapse_repeats(tbl, cols = c("grp", "sub"))
out$data
#>    grp  sub n
#> 1    A    x 1
#> 2 <NA> <NA> 2
#> 3 <NA>    y 3
#> 4    B    x 4
#> 5 <NA> <NA> 5
```
