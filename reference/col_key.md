# Select header columns by a column-name segment

Returns a **column selector** for
[`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
`pos =`: a function that is handed the data column names when the header
is attached to a table and answers which columns the cell covers.

## Usage

``` r
col_key(key, sep = NULL, part = 1L)
```

## Arguments

- key:

  Character. One or more segment values to match.

- sep:

  Single string, or `NULL` (default) to auto-detect the separator that
  occurs in the column names (`"____"`, then tfrmt's delimiter).

- part:

  Integer. Which segment to compare, counting from the left (`1`, the
  default, is the value before the first separator). A negative value
  counts from the right, so `part = -1` is the last segment.

## Value

A function of the column names, tagged for use as `col_cell(pos =)`.

## Details

Data columns produced by a wide pivot are usually named by rule, as
`<group><sep><sub-group>` – for example `"Placebo____Day 1"`.
`col_key()` splits each name on `sep` and keeps the columns whose
`part`-th segment is one of `key`, so a spanning header cell is written
in terms of the *value* it labels rather than of column numbers that
shift whenever a column is added, dropped or reordered.

The separator is the one already used to rebuild a spanning header from
delimited column names (see `as_rtftables(header_sep = )`) and to split
a table column-wise (see
[`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md)
`by = `), so the three stay in step.

A selector may also be written directly as a plain function of the
column names, which covers glob and regular-expression matching without
any further vocabulary:

    col_cell(function(nm) grepl(glob2rx("Placebo____*"), nm), "Placebo")
    col_cell(function(nm) grepl("^Placebo____", nm),          "Placebo")

Such a function may return a logical vector the length of the column
names, an integer vector of positions, or a character vector of column
names. It is an error for a selector to match no column, or to match
columns that are not adjacent – a header cell can only span a contiguous
range.

## See also

[`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md),
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md),
[`paginate_cols()`](https://ichirio.github.io/rtfreporter/reference/paginate_cols.md)

## Examples

``` r
# Data columns: Placebo____Day 1, Placebo____Day 7, TAK-003____Day 1, ...
col_cell(col_key("Placebo"), "Placebo\n(N=60)")
#> <col_cell pos=col_key("Placebo") label="Placebo
#> (N=60)">

# Every "Day 1" column, whichever arm it belongs to:
col_cell(col_key("Day 1", part = 2), "Day 1")
#> <col_cell pos=col_key("Day 1") label="Day 1">
```
