# Blank-row specification: insert when a variable's value changes

Constructor for a blank-row spec that inserts a blank separator row each
time the *group* of any column in `cols` changes from the previous row.
Pass the result to `rtftable(blank_rows = ...)` or
`as_rtftables(blank_rows = ...)`, optionally combined with other specs
via a list.

## Usage

``` r
blank_rows_by_change(
  cols,
  group_by = c("value", "indent", "filled", "auto"),
  include_before_first = TRUE,
  include_after_last = TRUE
)
```

## Arguments

- cols:

  Character vector of column names in the data frame.

- group_by:

  How a "group" is recognised in each column – the same detection the
  pagination splits and `blank_rows = "between_groups"` use:

  `"value"`

  :   (default) a group is a run of equal values; a blank is inserted
      whenever a value differs from the previous row (the classic
      behaviour).

  `"indent"`

  :   a non-indented, non-empty cell starts a group; indented / empty
      cells are members.

  `"filled"`

  :   a non-empty cell starts a group; `NA` / `""` cells are members.

  `"auto"`

  :   pick one of the above from each column's content.

  With several `cols`, a blank is inserted where **any** column's group
  changes.

- include_before_first:

  Logical. When `TRUE` (default), also insert a blank row before the
  first data row.

- include_after_last:

  Logical. When `TRUE` (default), also insert a blank row after the last
  data row.

## Value

An object of class `rtf_blank_rows_by_change`.

## Examples

``` r
df <- data.frame(Treatment = c("A", "A", "B", "B"),
                 Visit = c("Week 1", "Week 2", "Week 1", "Week 2"),
                 label = c("Age", "  Mean", "Sex", "  F"),
                 Value = c("", "75.1", "", "53%"))
rtftable(df, blank_rows = blank_rows_by_change(c("Treatment", "Visit")))
#> ────────────────────────────────
#> Treatment  Visit   label   Value
#> ────────────────────────────────
#> A          Week 1   Age         
#> A          Week 2    Mean  75.1 
#> B          Week 1   Sex         
#> B          Week 2     F     53% 
#> 
#> <rtftable> 4 rows x 4 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
# indent-based groups (a stub/label column):
rtftable(df, blank_rows = blank_rows_by_change("label", group_by = "indent"))
#> ────────────────────────────────
#> Treatment  Visit   label   Value
#> ────────────────────────────────
#> A          Week 1   Age         
#> A          Week 2    Mean  75.1 
#> B          Week 1   Sex         
#> B          Week 2     F     53% 
#> 
#> <rtftable> 4 rows x 4 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#> 
```
