# Append (or prepend) a row to an `rtf_col_header`

Append (or prepend) a row to an `rtf_col_header`

## Usage

``` r
add_col_header_row(hdr, row, .position = c("bottom", "top"))
```

## Arguments

- hdr:

  An
  [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md),
  or any value accepted by `rtftable(col_header = ...)`.
  Non-`rtf_col_header` inputs are promoted automatically.

- row:

  One header row: a character vector or a list of cell specs.

- .position:

  `"bottom"` (default) appends below the existing rows; `"top"` prepends
  above.

## Value

A new `rtf_col_header`.

## Examples

``` r
if (FALSE) { # \dontrun{
hdr <- rtf_col_header(c("Item", "N", "Mean", "N", "Mean"))   # bottom row
hdr <- add_col_header_row(
  hdr,
  list(col_cell(1, ""),
       col_cell(c(2, 3), "Drug A"),
       col_cell(c(4, 5), "Drug B")),
  .position = "top"
)
} # }
```
