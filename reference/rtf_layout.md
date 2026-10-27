# Lay a document's contents out as printed pages

Shows how the contents added by
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
and
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
fall on the pages
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
writes. Each content is a block; a block begins a page unless it was
added with `page_break = FALSE`, in which case it goes on the page
before it, under what is there. A table's own later pages (a plan's, or
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)'s)
always begin a page, and so does the document's first content.

## Usage

``` r
rtf_layout(doc)

# S3 method for class 'rtf_pages'
print(x, ...)
```

## Arguments

- doc:

  An
  [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md).

- x:

  An `rtf_pages` object.

- ...:

  Unused.

## Value

An `rtf_pages` object: a list with

- `pages`:

  one entry per printed page, each a list with `number` (the page
  number, as
  [`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)`(page = )`
  counts it), `section` (the section the page is in, counted from 1),
  `section_start` (`TRUE` where a section begins: page 1, a page
  [`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)
  starts one on, or a page an `auto_section` heading opens),
  `section_label` (that heading, or `NULL`) and `segments`: the blocks
  on the page, top to bottom, each a list with `block` (its position in
  the document), `name`, `content` (an `rtftable` or `rtfplot`), `title`
  (with `cont_title`'s line on a table's later pages), `footnote`,
  `space_before`, `group` and `page_in_group` (which table, and which
  page of it), `section` (the `auto_section` heading the block opens, or
  `NULL`) and `values` (the columns its table was cut on and this page's
  value of each, a named character vector, or `NULL`: see *Page values*
  in
  [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md));
  and the page's own `values`: each column that of the first block on
  the page with a value for it.

- `n_blocks`:

  the number of blocks.

- `paper`:

  the page size and font size, for printing.

[`print()`](https://rdrr.io/r/base/print.html) shows one line per block.

## Details

A page has one `auto_section` heading (see
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)):
that of the first block on it that has one. A later block on the same
page with a different heading gets a warning, and its heading is not
used; no page break is added for it. A page whose blocks have none stays
in the section before it. An
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)`(page = n)`
on the page wins over its heading.

Nothing is measured: the pages are decided by `page_break` and by the
tables' own pagination (`max_rows`, `split`, `first_rows`) alone. A page
holding more than fits is carried on by the reader (Word, LibreOffice),
and the page numbers written into the file are then off by those pages.
Fitting a page is the program's to arrange;
[`print()`](https://rdrr.io/r/base/print.html) of the result shows the
rows of each table and the size of each figure to arrange it by.

## See also

[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
/
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
(`page_break`, `space_before`, `first_rows`),
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md).

## Examples

``` r
df <- function(n) data.frame(Item = sprintf("Row %02d", seq_len(n)), N = seq_len(n))
doc <- rtf_document() |>
  rtf_tables(list(Summary = df(8)), titles = "Table 1") |>
  rtf_tables(list(Listing = df(12)), titles = "Listing 1", page_break = FALSE) |>
  rtf_tables(list(Other = df(5)), titles = "Table 2")
lay <- rtf_layout(doc)
length(lay$pages)      # 2: the listing goes on the first page
#> [1] 2
lay
#> rtf_pages: 3 blocks on 2 pages (letter landscape, 9 pt)
#>  page 1  [sec 1]  Summary  rtftable 8 rows   title 1
#>       +           Listing  rtftable 12 rows  title 1
#>  page 2           Other    rtftable 5 rows   title 1
```
