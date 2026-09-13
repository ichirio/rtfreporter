# Automatically calculate column widths for a data.frame

Scans the content of each column (header labels and data values) to
derive suggested column widths in **twips**. The widths can be passed
directly to
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
`column_widths_twips` argument.

## Usage

``` r
auto_col_widths(
  df,
  col_header = NULL,
  font = "courier_new",
  size_half_points = 18L,
  table_width_twips = NULL,
  min_col_width_twips = 720L,
  col_padding_twips = 288L,
  protect_cols = integer(0)
)
```

## Arguments

- df:

  A `data.frame`.

- col_header:

  Column header labels. `NULL` (default) uses `names(df)`. Accepts the
  same formats as `rtftable(col_header = ...)`: a character vector, a
  pipe-delimited string, or a list of character vectors (only the first
  row is used for width estimation).

- font:

  Font name passed to
  [`text_width_in()`](https://ichirio.github.io/rtfreporter/reference/text_width_in.md).
  Default `"courier_new"`.

- size_half_points:

  Font size in half-points. Default `18` (= 9 pt).

- table_width_twips:

  If not `NULL`, the column widths are scaled so that their sum equals
  this value. Useful for fitting tables to a fixed page width.

- min_col_width_twips:

  Minimum width per column in twips. Default `720` (= 0.5 inch).

- col_padding_twips:

  Extra twips added to each column width to account for cell padding and
  inter-column spacing. Default `288` (= 0.2 inch).

- protect_cols:

  Integer column indices to keep at their natural (content) width when
  `table_width_twips` forces the table *narrower* than its natural
  width. Only the remaining columns are shrunk to fit, so e.g. a
  row-label column (`protect_cols = 1`) stays readable while the data
  columns absorb the squeeze. Protection is dropped if it would push the
  scalable columns below `min_col_width_twips`. Has no effect when
  scaling *up* or when `table_width_twips` is `NULL`. Default none.

## Value

An integer vector of column widths in twips, one per column of `df`.

## Examples

``` r
df <- data.frame(
  USUBJID  = c("SUBJ-001", "SUBJ-002"),
  TREATMENT = c("Placebo", "Active"),
  AGE      = c(45L, 62L)
)
widths <- auto_col_widths(df, table_width_twips = 14400L)
tbl <- rtftable(df, column_widths_twips = widths)
```
