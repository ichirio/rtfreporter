# Update a specific row in an `rtf_header()` object (deprecated)

**Deprecated** in 0.8.x (warns once a session, still works); removed in
0.9.0. Make the header (or footer) again with
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md):
its rows are a list, and a list is edited with R.

## Usage

``` r
update_header_row(header, row, content)

update_footer_row(footer, row, content)
```

## Arguments

- header:

  An
  [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  object (returned by
  [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)).

- row:

  Integer. Target row number (1-based).

- content:

  A named character vector for the row (e.g.
  `c(l = "Left", r = "Right")`). See
  [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  for column rules.

- footer:

  An
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  object (returned by
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)).

## Value

A modified
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
object.

## Details

Adds a new row or replaces an existing row in a header/footer object. If
`row` is beyond the current number of rows, intermediate rows are
auto-filled with empty center-aligned rows (`c(c = "")`).

## Examples

``` r
# instead: the rows as a list, made again
rows <- list(c(l = "Protocol: XXX-001", r = "Company"),
             c(l = "Table 14.1.1", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"))
rows[[3]] <- c(c = "Draft - Confidential")
hdr <- rtf_header(rows = rows)
```
