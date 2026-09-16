# Blank-row specification: insert before/after rows matching a pattern

Constructor for a blank-row spec that inserts a blank separator row
before or after every data row whose value in `col` matches the regular
expression `pattern`.

## Usage

``` r
blank_rows_by_rule(col, pattern, where = c("before", "after"))
```

## Arguments

- col:

  Name of the column to test.

- pattern:

  A regular expression (POSIX extended via
  [`base::grepl()`](https://rdrr.io/r/base/grep.html)).

- where:

  Either `"before"` (default) or `"after"`.

## Value

An object of class `rtf_blank_rows_by_rule`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Blank row before every row whose Parameter does NOT start with a space
rtftable(df, blank_rows = blank_rows_by_rule(
  col = "Parameter", pattern = "^[^ ]", where = "before"))
} # }
```
