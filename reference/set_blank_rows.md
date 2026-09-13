# Attach blank-row positions to a data.frame

Resolves a `blank_rows` specification (the same one
[`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)
accepts) into integer positions and stores them on
`attr(df, "rtf_blank_rows")`. Use this when you already have a
page-sized data.frame and only need to add blank rows — no pagination
required.

## Usage

``` r
set_blank_rows(
  df,
  blank_rows = NULL,
  blank_row_first = FALSE,
  blank_row_end = FALSE,
  group_col = NULL,
  group_by = c("auto", "indent", "value", "filled")
)
```

## Arguments

- df:

  A data.frame (or tibble).

- blank_rows:

  Blank-row specification. One of – or a
  [`list()`](https://rdrr.io/r/base/list.html) combining any of
  (positions are unioned):

  `NULL`

  :   (default) no positions from this argument.

  an integer vector

  :   explicit positions: `0` = before the first row, `k` = after row
      `k`.

  `"between_groups"`

  :   insert a blank at every group transition, using `group_by` (the
      same detection as the pagination splits).

  a [`blank_rows_by_change()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_change.md) or [`blank_rows_by_rule()`](https://ichirio.github.io/rtfreporter/reference/blank_rows_by_rule.md) spec

  :   resolved per page (each carries its own rule / `group_by`).

- blank_row_first:

  Logical, default `FALSE`. When `TRUE`, also adds position `0` (blank
  row at the top of `df`).

- blank_row_end:

  Logical, default `FALSE`. When `TRUE`, also adds position `nrow(df)`
  (blank row at the bottom of `df`).

- group_col:

  Column name or 1-based index identifying the group, used only when
  `blank_rows = "between_groups"`. `NULL` (default) means detection on
  column 1 — see
  [`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md).

- group_by:

  How groups are recognised when `blank_rows = "between_groups"`:
  `"auto"` (default), `"indent"`, `"value"`, or `"filled"` — the same
  detection as the pagination splits (see
  [`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)).

## Value

`df` with `attr(., "rtf_blank_rows")` updated. The attribute is left
absent when the resolved position set is empty.

## Details

[`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)
calls this function on every chunk it produces, so the behaviour here
defines what `paginate(blank_rows = ...)`,
`paginate(blank_row_first = ...)` and `paginate(blank_row_end = ...)`
actually do.

## See also

[`paginate()`](https://ichirio.github.io/rtfreporter/reference/paginate.md)
for the per-page version;
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
