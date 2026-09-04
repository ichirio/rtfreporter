# Set the whole column header of a finished table (final-table coordinates)

`set_col_header()` replaces the column header of an already-built
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
– typically the output of
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
– resolving every cell against the **final, printed table**: the columns
you actually see, addressed by **name** or by **visible position** (`1`
= first printed column). Because it runs on the finished table, there is
no "intermediate" column layout to reason about – no hidden `drop_cols`,
no `stub_vars` bookkeeping, no position shifting. This is the
recommended way to attach a multi-row / spanning header on top of an
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
pipeline; pair it with
[`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md)
to see the exact column names first.

## Usage

``` r
set_col_header(x, ...)

# S3 method for class 'rtftable'
set_col_header(x, ..., align = NULL)

# S3 method for class 'list'
set_col_header(x, ...)
```

## Arguments

- x:

  An
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
  or a list of them (pages from
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).

- ...:

  The header rows, in render order (top first) – each a character vector
  (a label row, optionally named to place labels by column name) or a
  list of
  [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
  cells (a spanning / cell row, whose `pos` may be column names or final
  positions). Alternatively a single pre-built
  [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
  object. Passing nothing clears the header.

- align:

  Optional column-header text alignment for the final columns:
  `"left"`/`"center"`/`"right"` (applied to every column) or a character
  vector of length `ncol` (one per printed column). `NULL` (default)
  leaves the current header alignment untouched.

## Value

An object of the same shape as `x` (rtftable, or list of pages).

## Details

Contrast with the `col_header =` argument of
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
/
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
whose positions refer to the source body **before** `drop_cols` /
`stub_vars` are applied. `set_col_header()` always speaks the final
table's coordinates.

Like the other post-hoc verbs it is an S3 generic with an `rtftable`
method and a **list** method (every page of an
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
result), and it chains with the native pipe.

## See also

[`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md)
to list the final column names;
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
/
[`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
to build header rows;
[`add_header_row()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
to add a single row;
[`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
to restyle existing header cells.

## Examples

``` r
df <- data.frame(row_label = c("A", "B"),
                 g1 = 1:2, g2 = 3:4, Total = 5:6)
tbl <- rtftable(df)
tbl <- set_col_header(
  tbl,
  list(col_cell("row_label", ""), col_cell(c("g1", "g2"), "Treatment")),
  c(row_label = "Category", g1 = "Low", g2 = "High", Total = "Total")
)
```
