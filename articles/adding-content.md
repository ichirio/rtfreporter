# Adding tables and figures

``` r

library(rtfreporter)
```

A rtfreporter document is a **flat sequence of pages**, and the rule is
simple: **one page holds exactly one content item** – a table or a
figure – plus an optional title and footnote block. You add content with
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
(and
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
for images); each element you pass becomes one page.

This article is the deep dive on content. For document-wide settings
(page size, fonts) see [Page & document
setup](https://ichirio.github.io/rtfreporter/articles/page-setup.md);
for converting `gt` / `gtsummary` / `rtables` objects see [Importing
tables](https://ichirio.github.io/rtfreporter/articles/importing-tables.md);
for splitting a long table across pages see [Paginating with
as_rtftables()](https://ichirio.github.io/rtfreporter/articles/pagination.md).

``` r

df1 <- data.frame(Parameter = c("Age", "  Mean", "  SD"),
                  Value     = c("", "75.1", "8.2"), stringsAsFactors = FALSE)
df2 <- data.frame(Parameter = c("Sex", "  F", "  M"),
                  Value     = c("", "53%", "47%"), stringsAsFactors = FALSE)
```

## A single table

The quickest way is to hand a data.frame straight to
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
with the formatting you want – a single item needs no
[`list()`](https://rdrr.io/r/base/list.html):

``` r

doc <- rtf_document() |>
  rtf_tables(df1, border = "tfl", row_height_twips = 280L, col_rel_width = c(2, 1))
length(doc$contents)        # one page
#> [1] 1
```

For full control, build the table object yourself with
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
(or
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
for gt/rtables sources) and pass that:

``` r

tbl <- rtftable(df1, border = "tfl", col_rel_width = c(2, 1),
                col_spec = list(list(col = 2, align = "right")))
doc <- rtf_document() |> rtf_tables(tbl)
```

The formatting arguments of
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
(`border`, `col_rel_width`, `row_height_twips`, `col_spec`, …) are
documented on
[`?rtftable`](https://ichirio.github.io/rtfreporter/reference/rtftable.md);
they are the same options either way.

### Column alignment and `row_title`

By default the **row-heading column is left-aligned and every data
column is centred** — the usual clinical look. The heading column is the
first one unless you say otherwise with `row_title` (an integer vector
of column indices or a vector of column names):

``` r

# Columns 1-2 are row headings (left); columns 3+ are data (centre).
tbl <- rtftable(df1, row_title = c(1, 2))
```

`row_title` only sets the *default* alignment; an explicit `col_spec`
`align`, an `rtf_table_style`, or alignment read from a gt/rtables
source still wins, and column headers follow their column’s data
alignment.
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
and
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
take `row_title` too, so the same default applies to a whole batch of
tables at once.

## The same settings for several tables

Pass a **list** of bare data.frames and the formatting arguments are
applied to **every** one – handy for a batch of tables that should look
identical. Each element still becomes its own page:

``` r

doc <- rtf_document() |>
  rtf_tables(
    list(df1, df2),
    border           = "tfl",
    row_height_twips = 280L,
    col_rel_width    = c(2, 1)
  )
length(doc$contents)        # two pages, same look
#> [1] 2
```

## Different settings per table

When each table needs its own look, build each one separately (with its
own settings) and pass the list. Pre-built
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
objects keep their own settings:

``` r

t1 <- rtftable(df1, border = "tfl", row_height_twips = 280L)
t2 <- rtftable(df2, border = NULL,  row_height_twips = 200L)   # no borders

doc <- rtf_document() |> rtf_tables(list(t1, t2))
doc$contents[[1]]$row_height_twips   # 280 -- t1 keeps its own
#> [1] 280
doc$contents[[2]]$row_height_twips   # 200 -- t2 keeps its own
#> [1] 200
```

### Override semantics

A formatting argument you pass **explicitly** to
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
overrides that field on every pre-built table; arguments you leave at
their default do not touch the tables’’ own values. This lets you set a
shared override without rebuilding the tables:

``` r

doc <- rtf_document() |>
  rtf_tables(list(t1, t2), row_height_twips = 240L)   # explicit -> overrides both
doc$contents[[1]]$row_height_twips   # 240
#> [1] 240
doc$contents[[2]]$row_height_twips   # 240
#> [1] 240
```

(Bare data.frames are always built from the supplied arguments;
[`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
figure objects are never modified.)

## Titles and footnotes

Each page can carry a title block and a footnote block. One block
element becomes one line. Title lines default to centred + bold;
footnote lines default to left, and the footnote’s first line carries a
top rule as the separator. Use `NULL` for a page with none.

By default the **title** renders as plain **text paragraphs** spanning
the writable page width (so it is centred over the page), while the
**footnote** renders as a content-width single-column **table** (which
carries the separator rule). You can switch either form with the
document defaults `title_format` / `footnote_format` (`"text"` or
`"table"`):

``` r

doc <- rtf_document(default_format = list(title_format = "table")) |>  # legacy
  rtf_tables(list(df1), titles = list("Table 14.1.1"))
```

Pass `titles` / `footnotes` as lists parallel to the content (one
element per page):

``` r

doc <- rtf_document() |>
  rtf_tables(
    list(df1, df2),
    border    = "tfl",
    titles    = list(c("Table 14.1.1", "Demographics"),
                     c("Table 14.1.2", "Sex")),
    footnotes = list(c("Source: ADSL"), NULL)
  )
```

### Common (one block for every page)

If you pass a **length-1** list, that single block is applied to every
page – handy for a footnote that repeats on all pages:

``` r

doc <- rtf_document() |>
  rtf_tables(list(df1, df2),
             footnotes = list(c("Confidential", "Source: ADSL")))   # on both pages
```

### Per-row styling

A block can be a plain character vector (default styling) **or** a list
of rows, where each row is a string or a styled list with any of
`align`, `bold`, `italic`, `underline`, `color` (a `"#RRGGBB"` hex), and
`border` (an \[rtf_border()\] – e.g. to recolour or remove the footnote
separator):

``` r

doc <- rtf_document() |>
  rtf_tables(
    list(df1),
    titles    = list(list(
      "Table 14.1.1",
      list(text = "Provisional", italic = TRUE, color = "#C00000")
    )),
    footnotes = list(list(
      list(text = "Source: ADSL", border = rtf_border(top = TRUE))
    ))
  )
```

You can also set blocks after the content with
[`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)
/
[`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md)
(both accept a per-page list or a single common block). When the content
comes from a `gt` table via
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
its title and source notes flow through automatically – see [Importing
tables](https://ichirio.github.io/rtfreporter/articles/importing-tables.md).

## Cell-text markup

Cell text (data cells, column / spanning headers, titles and footnotes)
is run through a small markup step controlled by the `markup` argument
of
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
(and
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
for bare data.frames; the document-wide default is
`rtf_document(default_format = list(markup = ))`). It takes a character
vector of:

- `"script"` – `^{...}` renders as superscript, `_{...}` as subscript
  (this is how adapter-generated footnote reference marks such as `^{1}`
  become real superscripts);
- `"relational"` – the literal `">="` is converted to the `>=` symbol
  (U+2265) and `"<="` to `<=` (U+2264).

The default is **`"script"`** – super/subscript on, but the `>=` / `<=`
symbol conversion **off**, so a literal `>=` stays as typed. Opt into
the symbol conversion with `"all"` (or `"relational"`), or turn
everything off with `"none"`:

``` r

df_rel <- data.frame(Criterion = c("Age >= 65", "CrCl < 30"), n = c("120", "8"))

rtftable(df_rel)                       # default: ">=" stays literal
#> ──────────────
#> Criterion   n 
#> ──────────────
#> Age >= 65  120
#> CrCl < 30   8 
#> 
#> <rtftable> 2 rows x 2 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
rtftable(df_rel, markup = "all")       # ">=" -> the >= symbol, ^{}/_{} too
#> ──────────────
#> Criterion   n 
#> ──────────────
#> Age >= 65  120
#> CrCl < 30   8 
#> 
#> <rtftable> 2 rows x 2 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
#>   Markup:     script, relational
rtftable(df_rel, markup = "none")      # no markup at all (everything literal)
#> ──────────────
#> Criterion   n 
#> ──────────────
#> Age >= 65  120
#> CrCl < 30   8 
#> 
#> <rtftable> 2 rows x 2 columns
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     auto / inherited
```

## Figures

Images are content too: one figure per page, added with
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md).
Pass a list of image paths (PNG/JPEG) or
[`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
objects; `width_twips` / `height_twips` / `align` control the display,
and `titles` / `footnotes` work exactly as for tables.

**By default a figure embeds at its native size (100%)** – the pixel
dimensions divided by the image’s own DPI (read from the PNG `pHYs`
chunk / JPEG JFIF density). A 2500 x 1438 px plot saved at 300 DPI
therefore appears at 2500/300 x 1438/300 in. If the file carries no DPI,
the `rtfreporter.figure.default_dpi` option (96) is assumed. Save your
graphics device at the resolution you want on the page:

``` r

# Save at 300 DPI so the native size is predictable.
img <- tempfile(fileext = ".png")
png(img, width = 2500, height = 1438, res = 300)
plot(1:10, main = "Mean over time"); dev.off()
#> agg_png 
#>       2

doc <- rtf_document() |>
  rtf_figures(list(img), align = "center",           # native size (~8.3 x 4.8 in)
              titles = list(c("Figure 14.2.1", "Mean over time")))
length(doc$contents)        # one figure page
#> [1] 1
```

Pass an explicit `width_twips` (the height then follows the aspect
ratio) to scale a figure – for example to shrink one that is wider than
the page, since the native size is **not** auto-capped to the writable
width:

``` r

rtf_figures(list(img), width_twips = 9000L)          # force 6.25 in wide
```

Tables and figures share the same page model, so a report is just a
sequence of content pages – some tables, some figures – each optionally
titled and footnoted. To control the running header/footer across those
pages, see the headers & footers article.

See
[`?rtf_tables`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
and
[`?rtf_figures`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
for the full argument reference.

## Where next

- [Borders](https://ichirio.github.io/rtfreporter/articles/borders.md) —
  how a table gets its rules
- [Sections](https://ichirio.github.io/rtfreporter/articles/section-splitting.md)
  — when one document needs more than one header

The four recipes (`?rtfreporter-recipes`) are the same ground covered as
runnable help-page examples: demographics, adverse events, PK and
laboratory, each data-in to RTF-out.
