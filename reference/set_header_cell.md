# Set or merge individual column-header cells (spanning, borders, alignment)

Edits **specific cells of one header row** of a finished
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
(or a list of pages) without rebuilding the whole header: place one or
more
[`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)s
– by column **name or position**, spanning via `c(a, b)` – into row
`row`, keeping the other cells of that row intact. Merging
currently-separate cells into one spanning cell is the core use.
Borders, alignment and text decorations are carried natively by
[`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md).

## Usage

``` r
set_header_cell(x, ...)

# S3 method for class 'rtftable'
set_header_cell(x, ..., row)

# S3 method for class 'list'
set_header_cell(x, ...)
```

## Arguments

- x:

  An
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
  or a list of them (pages from
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).

- ...:

  One or more
  [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
  objects to place in `row`. Their `pos` may be column names or
  positions (spanning via `c(a, b)`), resolved against the final
  columns; `align` / `border` / `bold` / `italic` / `underline` are
  applied to the cell.

- row:

  The header row to edit (1 = top). Must be an existing row – add a new
  row with
  [`add_header_row()`](https://ichirio.github.io/rtfreporter/reference/style_header.md).

## Value

An object of the same shape as `x`.

## Details

A **label row** is promoted to cells first (as in
[`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)),
so the row then renders through the `spanning` border zone (which falls
back to the `header` zone when unset). Merging cells in one row replaces
their individual labels with the single span label – to keep the
sub-labels, put the span on a separate upper row (e.g. via
[`add_header_row()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)).

## Rules

Each target span must align to existing cell boundaries in that row (it
cannot split an existing spanning cell – an error is raised otherwise),
and the requested cells must not overlap one another.

## See also

[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
to set the whole header;
[`style_header()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
to restyle existing cells;
[`add_header_row()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)
to add a row;
[`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md).

## Examples

``` r
df  <- data.frame(Item = "x", g1 = 1, g2 = 2, g3 = 3, Total = 4)
tbl <- rtftable(df, col_header = c("Item", "N", "Mean", "SD", "Total"))
# Merge g1..g3 under one spanning "Statistics" cell on the top row:
tbl <- set_header_cell(tbl, col_cell(c("g1", "g3"), "Statistics"), row = 1)
```
