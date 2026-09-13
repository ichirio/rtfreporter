# Paginate a table horizontally, by columns

Splits a table across pages **by column** – the horizontal counterpart
of the row pagination
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
performs – repeating the row-heading column(s) on every page so each one
can be read on its own.

Row splitting happens first; this verb then splits the resulting pages.
The **row page is the outer level**: a row band sweeps every column
block before the next band starts, so the reader goes across the table
first and then down it. Two row pages by three column blocks come out as
`row1/col1`, `row1/col2`, `row1/col3`, `row2/col1`, `row2/col2`,
`row2/col3`.

    as_rtftables(x, max_rows = 20) |> paginate_cols(at = c(4, 6))

Because it runs on **built** tables, the positions refer to the final
printed columns – after `drop_cols`, `stub_vars` and any user
`col_header` – the same convention
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
uses.

## Usage

``` r
paginate_cols(x, ...)

# S3 method for class 'rtftable'
paginate_cols(
  x,
  at = NULL,
  cols = NULL,
  carry = NULL,
  allow_span_break = TRUE,
  width = c("fill", "keep"),
  ...
)

# S3 method for class 'list'
paginate_cols(
  x,
  at = NULL,
  cols = NULL,
  carry = NULL,
  allow_span_break = TRUE,
  width = c("fill", "keep"),
  ...
)
```

## Arguments

- x:

  An
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md),
  or a list of them (pages from
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)).
  Every page in a list must have the same columns.

- ...:

  Unused.

- at:

  Columns to cut **before**, as names or positions (symmetric with
  `split_rows`). `at = c(4, 6)` yields blocks `1:3`, `4:5`, `6:ncol`.

- cols:

  Explicit column blocks as a list, e.g. `list(2:3, 4:5)`. Give either
  `at` or `cols`.

- carry:

  Row-heading columns repeated on every page. Defaults to the table's
  `row_title` (column 1 unless set). `carry = integer(0)` repeats
  nothing. Carry columns are removed from the blocks automatically, so
  they are never printed twice on a page.

- allow_span_break:

  Allow a cut inside a spanning header cell. Default `TRUE`.

- width:

  How relative widths are rescaled after the split. `"fill"` (default)
  fixes the twips-per-ratio unit on **page 1** and reuses it on every
  page, so page 1 fills the sheet and a given ratio is the same width
  throughout; `"keep"` gives each kept column exactly the width it had
  before the split. No effect under `column_widths_twips`. See *Column
  widths*.

## Value

A list of
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
pages. Names are carried through unchanged, so
`rtf_tables(auto_section = TRUE)` keeps a table's column pages in one
section.

## Column widths

`column_widths_twips` is absolute, so a subset already carries the right
widths and `width` has no effect. Relative widths (and the default equal
distribution) need a rule, because `.compute_cellx()` re-normalises
whatever it is given across the page: a bare subset would stretch the
kept columns to refill the sheet, making a ratio-1 column a different
size on every page.

`width = "fill"` (the default) fixes the **twips per ratio unit on page
1** and reuses it everywhere. Page 1 – and any block with the same ratio
total – fills the sheet, while a given ratio is the same width on every
page. With `rel = c(3, 1, 1, 1, 1, 1, 1, 1, 1)` on a 13680-twip page:

|         |        |                  |
|---------|--------|------------------|
| blocks  | unit   | page widths      |
| 4 + 4   | 1954.3 | 100% / 100%      |
| 4 + 3   | 1954.3 | 100% / 85.7%     |
| 2+2+2+2 | 2736.0 | 100% on all four |

`width = "keep"` measures against the whole table instead, so a kept
column has exactly the width it had before the split and a partial block
yields a proportionally shorter page.

A block totalling **more** ratio than page 1 scales past the sheet under
`"fill"`; a warning names the pages. Order the blocks so the widest
comes first, or use `"keep"`.

## Spanning headers

Spanning cells are clipped to each page's columns. By default a cut may
fall inside a spanning group, and the group's label is repeated over its
remaining columns on each page; `allow_span_break = FALSE` rejects such
a cut instead.

## See also

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
for row pagination;
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
for the same final-column addressing.

## Examples

``` r
df <- data.frame(Parameter = c("Mean", "SD"),
                 A_n = c("86", "86"), A_mean = c("45.2", "12.3"),
                 B_n = c("84", "84"), B_mean = c("44.8", "11.9"),
                 stringsAsFactors = FALSE)
pages <- rtftable(df) |> paginate_cols(at = 4)
length(pages)                 # 2 column pages
#> [1] 2
names(pages[[1]]$data)        # Parameter repeated on both
#> [1] "Parameter" "A_n"       "A_mean"   
```
