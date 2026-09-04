# Headers and footers (sections)

``` r

library(rtfreporter)
```

A rtfreporter document is a flat sequence of content pages (see [Adding
tables and
figures](https://ichirio.github.io/rtfreporter/articles/adding-content.md)).
The running **header** and **footer** – the lines repeated at the top
and bottom of every page – are not attached to individual pages. Instead
they are defined by **sections**.

## The model: sections overlay headers/footers onto page ranges

A **section** assigns a header and footer starting at a given page; that
header/footer applies to every page **from there until the next
section**. There is no separate “per-page header” mechanism – a single
page’’s header is simply whatever the section covering that page
defines. You create sections with
`rtf_section(doc, page = , secinfo = list(header = , footer = ))`.

Two consequences worth remembering:

- Passing `header = NULL` (or `footer = NULL`) for a section means
  **inherit** the previous section’’s header (footer) – not “blank”.
- Sections are an overlay on the flat page list; they do **not** contain
  or group the content. Content order alone determines page numbers.

## Building a header or footer

[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
build the header/footer content. Rows are named character vectors using
`l` / `c` / `r` for the left / centre / right cell; pass a `list` of
them for a multi-row header. Page-number tokens are substituted at
render time:

- `{AUTO_PAGE}` – the current page number (updates in the viewer);
- `{AUTO_TOTAL_PAGES}` – the total page count;
- `{PAGE}` – the static first-page number of the section.

``` r

hdr <- rtf_header(rows = list(
  c(l = "Protocol XYZ-001", r = "Confidential"),
  c(l = "Table 14.1.1",     r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
))
ftr <- rtf_footer(c(c = "ACME Pharma, Inc."))
```

[`update_header_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
/
[`update_footer_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
add or replace one row after creation (handy when a row depends on
logic):

``` r

hdr <- update_header_row(hdr, row = 3, content = c(c = "DRAFT"))
```

A header/footer can also take a `border` (e.g. a rule under the footer)
– see
[`?rtf_header`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md).

## One header/footer for the whole document

For a single running header/footer, define **one** section starting at
page 1:

``` r

df <- data.frame(A = c("1", "2"), B = c("x", "y"), stringsAsFactors = FALSE)

doc <- rtf_document() |>
  rtf_tables(list(df, df, df)) |>                      # three pages
  rtf_section(page = 1, secinfo = list(header = hdr, footer = ftr))
```

The section at page 1 covers all subsequent pages, so every page gets
the same header and footer.

## Different headers/footers for different page ranges

Define more sections, each starting where its header/footer should take
over. Here pages 1-2 use one header and pages 3+ use another:

``` r

hdr_b <- rtf_header(rows = list(c(l = "Protocol XYZ-001", r = "Table 14.2.1")))

doc <- rtf_document() |>
  rtf_tables(list(df, df, df, df)) |>
  rtf_section(page = 1, secinfo = list(header = hdr,   footer = ftr)) |>
  rtf_section(page = 3, secinfo = list(header = hdr_b, footer = NULL))
#                                                       ^ NULL = keep ftr
```

`footer = NULL` on the second section inherits the first section’’s
footer, so only the header changes at page 3. You can also assign
several sections in one call by passing a vector of `page`s and a list
of `secinfo` objects – see
[`?rtf_section`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md).

## Per-content automatic headers – `auto_section`

When each table should start its own section with a heading derived from
its name, use `auto_section = TRUE` with a **named** list of content.
Define the common header once with
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)
*without* a `page` (the base template), and each name is appended to it
as a per-section line:

``` r

doc <- rtf_document() |>
  rtf_section(secinfo = list(header = hdr, footer = ftr)) |>   # base (no page)
  rtf_tables(
    list("Demographics" = df, "Adverse events" = df),
    auto_section        = TRUE,
    section_label_align = "center"
  )
```

This splits the document into one section per named item, each carrying
the base header plus its own label – a compact way to get per-table
headings without writing a
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)
call for every table. `auto_section` is sugar built on the same section
mechanism described above.

### Sectioning converted (multi-page) tables

`auto_section` reads the list **names**: a non-empty name starts a new
section (its text goes in the header), and an empty / unnamed element
falls through into the current section. The example above works because
each item is a single table. But
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
returns **one element per page**, so a paginated table is already a list
– naming the whole list once is not enough, and naming *every* page
would start a fresh section on each page.

Two ways to get clean sections from converted tables:

- **Group is a real column** (a `data.frame` / Tplyr body you assemble,
  or a `cards` + `tfrmt` table with `label_loc = "column"`): let
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  do it. `split = "by_value"` names each page by the group value, so
  `auto_section = TRUE` gives one section per group with no extra step:

  ``` r

  pages <- as_rtftables(lab_df, split = "by_value", group_col = "PARAM",
                        drop_cols = "PARAM")
  doc <- rtf_document() |>
    rtf_section(secinfo = list(header = hdr)) |>
    rtf_tables(pages, auto_section = TRUE)        # one section per parameter
  ```

- **Group is row-grouped** (gt, gtsummary, rtables/tern, or `tfrmt`
  indented – the group is rendered as label rows, not a column), **or
  you are combining several distinct tables**: use
  \[[`combine_sections()`](https://ichirio.github.io/rtfreporter/reference/combine_sections.md)\].
  Each argument’s name becomes one section, applied to that argument’s
  first page with the remaining pages falling through:

  ``` r

  dm <- as_rtftables(dm_gt)                         # may be several pages
  ae <- as_rtftables(ae_tbl, split = "group_force", max_rows = 30)

  doc <- rtf_document() |>
    rtf_section(secinfo = list(header = hdr)) |>
    rtf_tables(
      combine_sections(Demographics = dm, `Adverse Events` = ae),
      auto_section = TRUE)                          # one clean section per table
  ```

Leaving the pages named one-per-page is also fine – it simply yields one
RTF section per page, which is harmless (it matches the common
one-page-one-section convention).
[`combine_sections()`](https://ichirio.github.io/rtfreporter/reference/combine_sections.md)
is just for the tidier one-section-per-table layout.

## Page numbering

The tokens render to dynamic RTF fields, so the page numbers are correct
in the word processor even after the document is re-paginated:

``` r

doc <- rtf_document() |>
  rtf_tables(list(df, df)) |>
  rtf_section(page = 1, secinfo = list(
    header = NULL,
    footer = rtf_footer(c(c = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"))
  ))

rtf <- {
  f <- tempfile(fileext = ".rtf"); generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}
grepl("\\chpgn", rtf, fixed = TRUE)        # {AUTO_PAGE} -> \chpgn field
#> [1] TRUE
```

See
[`?rtf_section`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md),
[`?rtf_header`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md),
and
[`?rtf_footer`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
for the full reference.

## Where next

- [Sections](https://ichirio.github.io/rtfreporter/articles/section-splitting.md)
  — applying different headers to different pages

The four recipes (`?rtfreporter-recipes`) are the same ground covered as
runnable help-page examples: demographics, adverse events, PK and
laboratory, each data-in to RTF-out.
