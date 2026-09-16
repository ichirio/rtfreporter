# Convert one table object to a single rtftable

Single-page convenience wrapper around
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):
takes a `gt_tbl`, a
[gtsummary](https://www.danieldsjoberg.com/gtsummary/) table, an
rtables/tern `VTableTree`, an rlistings `listing_df`, a `flextable`, a
`huxtable`, or a plain `data.frame` / tibble, and returns one `rtftable`
(rather than a list of pages). It is exactly
`as_rtftables(x, read_meta = read_meta, split = "none", ...)[[1]]`.

## Usage

``` r
as_rtftable(gt_obj, read_meta = TRUE, ...)
```

## Arguments

- gt_obj:

  A `gt_tbl`, a gtsummary table, an rtables/tern `VTableTree`, an
  rlistings `listing_df`, a `flextable`, a `huxtable`, or a plain
  `data.frame` / tibble. A `gt_group`
  ([`gt::gt_group()`](https://gt.rstudio.com/reference/gt_group.html) /
  [`gt::gt_split()`](https://gt.rstudio.com/reference/gt_split.html), or
  a tfrmt `page_plan` render) or a gtsummary `tbl_split` container is
  accepted when it holds exactly one table; multi-member containers
  error – convert those with
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).

- read_meta:

  `TRUE` (default, read all render-relevant metadata), `FALSE` (rendered
  body only), or a character vector of tokens. See
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).

- ...:

  Passed to
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  (and on to
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).
  Explicit values always win over the values extracted from the source
  table.

## Value

An `rtftable` S3 object.

## Details

The body is the table's *rendered* body (gt via
[`gt::extract_body()`](https://gt.rstudio.com/reference/extract_body.html),
rtables via
[`formatters::matrix_form()`](https://rdrr.io/pkg/formatters/man/matrix_form.html));
only render-relevant metadata is read. See
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
for the full *What is carried, by source* table – in short: column
labels, alignment, spanning headers, widths, titles, footnotes, in-cell
footnote marks and (for gt/gtsummary) explicit
[`tab_style()`](https://gt.rstudio.com/reference/tab_style.html) borders
and text styles are carried; cell fills, fonts, gt theme borders and
Markdown are not.

## See also

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
for the paginating, list-returning version and the per-source metadata
table.

## Examples

``` r
if (FALSE) { # \dontrun{
library(gt)
g <- gt(head(mtcars, 5)) |>
  cols_label(mpg = "MPG", cyl = "Cyl") |>
  cols_align("right", columns = c(mpg, cyl))
tbl <- as_rtftable(g)
} # }
```
