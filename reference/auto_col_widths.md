# Automatically calculate column widths for a data.frame

Scans the content of each column (header labels and data values) to
derive suggested column widths in **twips**. The widths can be passed
directly to
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)'s
`column_widths_twips` argument.

## Usage

``` r
auto_col_widths(
  data,
  col_header = NULL,
  font = NULL,
  font_size_half_points = 18L,
  table_width_twips = NULL,
  min_col_width_twips = 720L,
  col_padding_twips = 288L,
  protect_cols = integer(0)
)
```

## Arguments

- data:

  A `data.frame`.

- col_header:

  Column header labels. `NULL` (default) uses `names(data)`. Accepts the
  same formats as `rtftable(col_header = ...)`: a character vector, a
  pipe-delimited string, or a list of character vectors (only the first
  row is used for width estimation).

- font:

  Font name. The width is an estimate from **one average character
  width** for the font and size, the same for every character (it is not
  [`text_width_in()`](https://ichirio.github.io/rtfreporter/reference/text_width_in.md)'s
  per-character measure): the width of a Courier New character (exact,
  since Courier is monospaced) for `"Courier New"`, `"Courier"` and for
  any name it does not know, and a flat average for `"Arial"`. Case,
  spaces and hyphens in the name do not matter. `NULL` (default) is the
  package's font, `getOption("rtfreporter.font")`.

- font_size_half_points:

  Font size in half-points. Default `18` (= 9 pt).

- table_width_twips:

  If not `NULL`, the column widths are scaled so that their sum equals
  this value exactly, the last column taking the rounding. A column the
  scaling would take below `min_col_width_twips` is held at the minimum
  and the others are scaled to what is left, so the minimum holds and
  the sum is still exact. When the value is below the sum of the minima
  (`min_col_width_twips` times the number of columns) both cannot hold:
  the widths are then scaled in proportion to their natural widths,
  ignoring the minimum, so that the sum is still the value. Useful for
  fitting tables to a fixed page width.

- min_col_width_twips:

  Minimum width per column in twips. Default `720` (= 0.5 inch). With
  `table_width_twips` it holds unless that is below the sum of the
  minima (see there).

- col_padding_twips:

  Extra twips added to each column width to account for cell padding and
  inter-column spacing. Default `288` (= 0.2 inch).

- protect_cols:

  Integer column indices to keep at their natural (content) width when
  `table_width_twips` forces the table *narrower* than its natural
  width. Default none. No effect when `table_width_twips` is `NULL`.

  Squeezed

  :   Only the remaining columns are shrunk to fit, so e.g. a row-label
      column (`protect_cols = 1`) stays readable while the data columns
      absorb the squeeze. Protection is dropped if the room left for the
      scalable columns is less than `min_col_width_twips` each.

  Scaled up

  :   When the table is scaled *up*, the protected columns likewise keep
      their natural width and only the others grow to the target.

## Value

An integer vector of column widths in twips, one per column of `data`.

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
