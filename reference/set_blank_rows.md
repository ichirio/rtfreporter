# Attach blank-row positions to a data.frame

Resolves a `blank_rows` specification (the same one
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
accepts) into integer positions and stores them on
`attr(data, "rtf_blank_rows")`. Use this when you already have a
page-sized data.frame and only need to add blank rows – no pagination
required.

## Usage

``` r
set_blank_rows(
  data,
  blank_rows = NULL,
  blank_row_first = FALSE,
  blank_row_end = FALSE,
  group_col = NULL,
  group_by = c("auto", "indent", "value", "filled")
)
```

## Arguments

- data:

  A data.frame (or tibble).

- blank_rows:

  Blank-row specification. One of – or a
  [`list()`](https://rdrr.io/r/base/list.html) combining any of
  (positions are unioned):

  `NULL`

  :   (default) no positions from this argument.

  an integer vector

  :   explicit positions: `0` = before the first row, `k` = after row
      `k`. Positions out of range, `NA` and negative ones are dropped
      without a warning.

  `"between_groups"`

  :   insert a blank at every group transition, using `group_by` (the
      same detection as the pagination splits).

  a [`blank_rows_by_change()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_change.md) or [`blank_rows_by_rule()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_rule.md) spec

  :   resolved per page (each carries its own rule / `group_by`).

- blank_row_first:

  Logical, default `FALSE`. When `TRUE`, also adds position `0` (blank
  row at the top of `data`).

- blank_row_end:

  Logical, default `FALSE`. When `TRUE`, also adds position `nrow(data)`
  (blank row at the bottom of `data`).

- group_col:

  Column name or 1-based index identifying the group, used only when
  `blank_rows = "between_groups"`. `NULL` (default) means detection on
  column 1 – see
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).

- group_by:

  How groups are recognised when `blank_rows = "between_groups"`:
  `"auto"` (default), `"indent"`, `"value"`, or `"filled"` – the same
  detection as the pagination splits (see
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).

## Value

`data` with `attr(., "rtf_blank_rows")` updated. The attribute is left
absent when the resolved position set is empty.

## Details

The positions count rows of `data` as a whole. Given to
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
with `blank_rows` left `NULL`, a data frame that carries the attribute
keeps that meaning across the pages: position `k` is a blank after row
`k` of the whole data frame, on the page that row lands on (once, and at
the foot of that page when the row ends it, not carried to the top of
the next); `0` is before the first row and the last row's position after
the last. A page that no position falls on gets no blank.

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
calls this function on every page it produces for the separator rules
(`"between_groups"`,
[`blank_rows_by_change()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_change.md),
[`blank_rows_by_rule()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_rule.md))
and for `blank_row_first` and `blank_row_end`; its integer positions are
the whole body's, as above.

## See also

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
for the paginated version;
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
(`read_attributes = TRUE`) which consumes the attribute.

## Examples

``` r
df <- data.frame(
  label = c("Demographics", "  Age", "  Sex",
            "Vitals",       "  HR",  "  BP"),
  v = 1:6,
  stringsAsFactors = FALSE
)
out <- set_blank_rows(df,
                      blank_rows      = "between_groups",
                      blank_row_first = TRUE,
                      blank_row_end   = TRUE)
attr(out, "rtf_blank_rows")
#> [1] 0 3 6
```
