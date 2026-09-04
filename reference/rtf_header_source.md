# Deparse a table's column header back to editable `rtf_col_header()` source

Renders the **current** column header of a finished
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
(or the first page of an
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
list) as `rtf_col_header(...)` source text, addressed by **column
name**, so you can copy it, edit the labels / spans, and re-apply with
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md).
It is the inverse companion of
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
and pairs with
[`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md)
(which lists the final column names).

## Usage

``` r
rtf_header_source(
  x,
  level = c("explicit", "default", "all"),
  snippet = TRUE,
  add_span_level = FALSE,
  stub = 1
)
```

## Arguments

- x:

  An
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
  or a list of them (pages from
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md);
  the first page is used).

- level:

  Verbosity of the emitted cells: `"explicit"` (default – only fields
  that differ from the defaults), `"default"` (also show each cell's
  effective `align` and default border widths, but not the `FALSE`
  decoration flags), or `"all"` (everything, including `bold = FALSE`
  etc.).

- snippet:

  Logical (default `TRUE`). When `TRUE`, wrap the header in a pipeable
  `set_col_header(...)` call that also reproduces the header text
  alignment (from `col_spec`) and the header-related zone borders
  (`header` / `spanning`, via
  [`style_zone()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)).
  There is no leading `tbl |>` – the first argument is meant to arrive
  through the pipe (e.g. `my_tbl |> ` followed by the snippet). When
  `FALSE`, return the bare `rtf_col_header(...)` value.

- add_span_level:

  Logical (default `FALSE`). When `TRUE`, prepend a scaffold spanning
  row grouping the non-`stub` columns (see *Details*).

- stub:

  Column(s) to keep un-spanned when `add_span_level = TRUE`: integer
  position(s) and/or column name(s). Default `1` (the first column,
  where `as_rtftables(stub_vars = )` places the stub).

## Value

A single character string (the source). Use
[`cat()`](https://rdrr.io/r/base/cat.html) to print it with the line
breaks rendered.

## Details

The output is name-based (cell positions are written as column names,
which survive reordering), keeps the header's empty gap cells, and
backtick-quotes non-syntactic column names. Per-cell text decorations
and borders are reproduced faithfully.

`add_span_level = TRUE` previews **adding a second hierarchy level**:
the `stub` column(s) stay as single (empty) cells and every other column
is bundled under one empty spanning cell whose label you fill in – a
quick scaffold for turning a one-row header into a grouped, two-row
header.

## See also

[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
to apply an edited header;
[`rtf_columns()`](https://ichirio.github.io/rtfreporter/reference/rtf_columns.md)
for the final column names;
[`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md)
/
[`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
for the pieces.

## Examples

``` r
tbl <- rtftable(
  data.frame(row_label = "x", g1 = 1, g2 = 2, Total = 3),
  col_header = c("Category", "Low", "High", "Total")
)
cat(rtf_header_source(tbl, snippet = FALSE))
#> rtf_col_header(
#>     c(row_label = "Category", g1 = "Low", g2 = "High", Total = "Total")
#>   )

# Preview adding a spanning level over the non-stub columns:
cat(rtf_header_source(tbl, snippet = FALSE, add_span_level = TRUE))
#> rtf_col_header(
#>     list(col_cell("row_label", ""), col_cell(c("g1", "Total"), "")),
#>     c(row_label = "Category", g1 = "Low", g2 = "High", Total = "Total")
#>   )
```
