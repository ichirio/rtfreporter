# Paginate a table horizontally, by columns

Splits a table across pages **by column** – the horizontal counterpart
of the row pagination
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
performs – repeating the row-heading column(s) on every page so each one
can be read on its own.

Row splitting happens first; this verb then splits the resulting pages,
and `page_order` says which axis the **page number advances along
first** – `"across"` the columns (the default: the column block
advances, the row pages running inside it) or `"down"` the rows (the row
page advances, its column blocks following it). Two row pages by three
column blocks come out as `col1/row1`, `col1/row2`, `col2/row1`, ...
under `"across"` and `row1/col1`, `row1/col2`, `row1/col3`, `row2/col1`,
... under `"down"`. A **group** is above both: see *Page order*.

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
  by = NULL,
  carry = NULL,
  col_header = NULL,
  allow_span_break = TRUE,
  width = c("fill", "keep"),
  page_order = c("across", "down"),
  ...
)

# S3 method for class 'list'
paginate_cols(
  x,
  at = NULL,
  cols = NULL,
  by = NULL,
  carry = NULL,
  col_header = NULL,
  allow_span_break = TRUE,
  width = c("fill", "keep"),
  page_order = c("across", "down"),
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

- by:

  Where to cut, taken from the **columns themselves** instead of
  positions: either a **separator** found in the column names (a single
  string – `"Placebo____Day 1"` with `by = "____"` keys on `"Placebo"`),
  or one **key per column**. Each run of one key becomes a block, so a
  wide table laid out as `<group>____<visit>` splits per group with no
  positions to count. Carry columns belong to no block. Give one of
  `at`, `cols` or `by`.

- carry:

  Row-heading columns repeated on every page. Defaults to the table's
  `row_title` (column 1 unless set). `carry = integer(0)` repeats
  nothing. Carry columns are removed from the blocks automatically, so
  they are never printed twice on a page.

- col_header:

  The header each page should carry, written **once** for the whole
  table:

  a header

  :   in the **full table's** coordinates – a label row, or rows of
      [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md)
      – sliced to each page's columns, spanning cells clipped, exactly
      as a header already on the table is.

  `"names"`

  :   build the **two-level** header the column names already carry: the
      `by` key on top (the group), the rest of the name below (the
      visit). Needs `by` to be a separator. A carry column keeps its own
      label and sits under no spanning cell.

  `NULL`

  :   (default) leave the table's own header, which is sliced per page
      either way.

  This is the place to put it: a header written for the whole table does
  **not** fit a page that kept only some of its columns, so applying one
  after the split
  ([`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
  on the page list, `rtf_tables(col_header = )`) is an error.

- allow_span_break:

  Allow a cut inside a spanning header cell. Default `TRUE`.

- width:

  How relative widths are rescaled after the split. `"fill"` (default)
  fixes the twips-per-ratio unit on **page 1** and reuses it on every
  page, so page 1 fills the sheet and a given ratio is the same width
  throughout; `"keep"` gives each kept column exactly the width it had
  before the split. No effect under `column_widths_twips`. See *Column
  widths*.

- page_order:

  The order the pages come out in, as the **axes** of the split,
  outermost first: any of `"group"` (the pages a value-based split made,
  `as_rtftables(split = "by_value", group_col = )`), `"rows"` (the row
  pages – `page_by` and every other row split) and `"cols"` (the column
  blocks this verb cuts). Axes left out are appended in that default
  order, and an axis the table does not have never varies.

  `"across"` and `"down"` are the two usual orders, kept as shorthands:
  `"across"` is `c("group", "cols", "rows")` – the page number advances
  across the columns first – and `"down"` is
  `c("group", "rows", "cols")`, advancing down the rows. The pages
  themselves are identical whatever the order; only the sequence
  differs. See *Page order*.

## Value

A list of
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
pages. Names are carried through unchanged – each column page keeps its
row page's name (see *Page names*).

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

## Columns named `<group>__<item>`

The common wide layout – one column per treatment x visit – splits per
group and wants a two-level header, group over visit. `by` and
`col_header = "names"` do both from the names:

    as_rtftables(df, split = "by_value", group_col = "period",
                 stub_vars = c("row_grp1", "label")) |>
      paginate_cols(by = "____", carry = 1, col_header = "names")

    page 1                        page 2
    |            | Placebo      | |            | HOGE-001     |
    | Group      | D1 | D2 | D8 | | Group      | D1 | D2 | D8 |

## Page order

A table can be split on three axes, and `page_order` names them in the
order they should nest, **outermost first**:

|  |  |
|----|----|
| axis | what it is |
| `"group"` | the pages a value-based split made – `as_rtftables(split = "by_value", group_col = )` |
| `"rows"` | the row pages: `page_by`, and every other row split (`max_rows` continuation pages, `split = "rows"`) |
| `"cols"` | the column blocks this verb cuts |

With a group `G`, two row pages `P1` / `P2` and two column blocks `C1` /
`C2`, all six orders are expressible; the two usual ones have
shorthands:

|  |  |
|----|----|
| `page_order` | page sequence |
| `"across"` = `c("group", "cols", "rows")` | `G1 C1 P1`, `G1 C1 P2`, `G1 C2 P1`, ... |
| `"down"` = `c("group", "rows", "cols")` | `G1 P1 C1`, `G1 P1 C2`, `G1 P2 C1`, ... |
| `c("cols", "group", "rows")` | `C1 G1 P1`, `C1 G1 P2`, `C1 G2 P1`, ... |

An axis left out of the vector is appended in the default order
(`"group"`, `"rows"`, `"cols"`), so `page_order = "cols"` means "column
blocks outermost, everything else as usual".

Each page records its own group in `rtf_paginate_meta$page_group` (and
its `page_by` value beside it), so none of this depends on reading a
page name – which carries the group alone.

One consequence worth knowing: `rtf_tables(auto_section = TRUE)` opens a
section where the page **name** changes, and a page is named by its
group. So a group's pages land in one section only while `"group"` comes
first; put `"cols"` outside it and each column block starts the groups
again, giving a section per block and group.

## Page names

A column page inherits the name of the row page it was cut from,
whatever `page_order` is – that is the only name there is. Under
`"across"` pages that share a name are therefore no longer adjacent.

Pages sharing a heading are numbered with a `"...n"` tail
(`"Period 1...1"`, `"Period 1...2"`) so the returned list stays
addressable by name; the tail is stripped wherever the name is used as a
heading.

That is worth knowing because `rtf_tables(auto_section = TRUE)` opens a
section where the name **changes**: a run of pages sharing a name is one
section, and a page whose name is `""` joins the section before it. So
`"down"` keeps a table's column pages in one section, while `"across"`
interleaves the names and gives a section per page. An **unnamed** list
– what
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
returns for a single table, and what `paginate_cols()` then passes
through – opens none at all, leaving every page in the document's own
section.

To put pages with *different* names in one section, blank the ones that
should not open a new one
([`combine_sections()`](https://ichirio.github.io/rtfreporter/reference/combine_sections.md)
does that bookkeeping when you are assembling whole tables).

## See also

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
for row pagination;
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)
for the same final-column addressing;
[`combine_sections()`](https://ichirio.github.io/rtfreporter/reference/combine_sections.md)
for the page names `rtf_tables(auto_section = TRUE)` reads.

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
