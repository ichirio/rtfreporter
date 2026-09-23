# rtfreporter — AI user manual

**This manual documents rtfreporter 0.8.0.9044** (the development
version, after release 0.8.0).
Check it matches what you have — `packageVersion("rtfreporter")`. If they
differ, trust the package, not this file, and fetch the matching copy with
`rtfreporter_ai_manual()`.

**Docs:** <https://ichirio.github.io/rtfreporter/> ·
**Source:** <https://github.com/ichirio/rtfreporter>

> **How to use this file.** Attach it at the start of a chat session and say:
> *"Use this manual when writing rtfreporter code."* The assistant then has the
> whole public API, the house idioms and the common traps in context.
> 日本語で質問しても構いません（本文は英語ですが、回答は質問の言語で返ります）。

> **Scope: *using* rtfreporter** — writing R code that produces reports.
> Working *on* the package itself (its internals, S3 architecture, test and
> release conventions) is a separate document; attach that one instead, not
> both, when the task is contributing to rtfreporter.

---

## 0. Ground rules for the assistant

1. **Only call functions listed in §17.** rtfreporter is a young package and is
   almost certainly *not* in your training data. If a requested feature has no
   function in §17, say so plainly instead of inventing a plausible name or
   argument.
2. **It is not `r2rtf` and not `reporter`.** Do not mix their verbs
   (`rtf_body()`, `rtf_colheader()`, `create_table()`, …) into rtfreporter code.
   rtfreporter depends on no other RTF package.
3. **Base R only.** No tidyverse is required. Use the native pipe `|>`
   (R >= 4.1) or `magrittr::%>%` — both appear in the docs.
4. **Units.** Widths and heights are **twips**: `1 inch = 1440 twips`,
   `1 pt = 20 twips`. Font size is in **half-points**: `18L` = 9 pt (the
   default), `16L` = 8 pt, `20L` = 10 pt.
5. **Indices are 1-based** (ordinary R). For blank-row positions, `0` means
   *before the first row* and `-1` means *after the last row*.
6. **One content item = one page.** Each element of the `tables` / `figures`
   list becomes its own page.
7. `rtftable()` returns **one** table object; `as_rtftables()` returns a
   **list** of page objects. `rtf_tables()` accepts either.
8. `generate_rtfreport()` will not replace an existing file unless
   `overwrite = TRUE`.

---

## 1. The one workflow

Every report is the same four steps:

```r
rtf_document()            # 1. document: paper, margins, fonts, defaults
  |> rtf_section(...)     # 2. section: running header / footer (repeat as needed)
  |> rtf_tables(...)      # 3. content: tables (or rtf_figures() for figures)
  |> generate_rtfreport("out.rtf", overwrite = TRUE)   # 4. write the file
```

`rtf_section()` is optional — omit it and the pages carry no running
header/footer. Everything else about a page (titles, footnotes, borders,
widths) is set on the content call, not on the document.

---

## 2. Copy-paste starter (complete, runnable)

```r
library(rtfreporter)

df <- data.frame(
  USUBJID = c("001-001", "001-002", "001-003"),
  TRT     = c("Placebo", "Active", "Active"),
  AVAL    = c(12.3, 14.1, 11.7)
)

doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
  rtf_section(
    page    = 1,
    secinfo = list(
      header = rtf_header(rows = list(
        c(l = "Protocol XYZ-001", r = "Confidential"),
        c(l = "Table 14.1.1",     r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
      )),
      footer = rtf_footer(rows = list(c(c = "ACME Pharma, Inc.")))
    )
  ) |>
  rtf_tables(
    as_rtftables(df, border = "tfl", row_height_twips = 280L),
    titles    = list(c("Subject Summary", "Safety Population")),
    footnotes = list(c("Source: ADaM ADSL"))
  )

generate_rtfreport(doc, "T_14_1_1.rtf", overwrite = TRUE)
```

---

## 3. Program structure — one output, and fifty

A report program is six stages. Keeping them apart is what lets the same
program produce one table today and a fifty-output deliverable next month.

```r
# 1  SETUP    study-level constants, paths
# 2  DATA     read ADaM / ARD -- no formatting here
# 3  BODY     shape the numbers into a plain data.frame
# 4  PRESENT  titles, footnotes, column header, per-page denominators
# 5  PAGES    as_rtftables() |> set_col_header() |> paginate_cols()
# 6  RENDER   rtf_document() |> rtf_section() |> rtf_tables() |> generate_rtfreport()
```

Four rules make it generalize:

1. **Stage 3 returns a plain `data.frame`** and knows nothing about RTF. It is
   the part with the clinical logic in it, so it must stay printable,
   diffable and testable on its own.
2. **Stage 5 returns pages and writes nothing.** `generate_rtfreport()` is the
   only line with a side effect and it comes last, so you can build the pages
   in the console and look at `pages[[1]]` before anything reaches disk.
3. **No number is pasted into a label.** A denominator that differs per page
   belongs in `set_col_header(values = )`, not in a `sprintf()` that has to be
   rebuilt for every page.
4. **Widths follow the layout.** `rep(2, length(days) * length(arms))`, never
   `rep(2, 24)` — the day you add an arm, the hard-coded count is a silent
   mis-render.

### The skeleton

```r
# 1  SETUP ------------------------------------------------------------------
STUDY <- "XYZ-001"

# 4  PRESENT (shared by every output in the deliverable) --------------------
study_header <- function(title_lines) {
  rtf_header(rows = c(
    list(c(l = paste("Protocol", STUDY), r = "Confidential"),
         c(l = "Phase III Safety Study",
           r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")),
    lapply(title_lines, function(t) c(c = t))))       # title block, centred
}
study_footer <- function(notes = character()) {
  rtf_footer(rows = c(lapply(notes, function(t) c(l = t)),
                      list(c(l = "ACME Pharma", r = "CONFIDENTIAL"))))
}

# 3  BODY -------------------------------------------------------------------
body_ae <- function(arms, days) {
  # ... compute counts; return a data.frame whose data columns are named
  #     "<arm>____<day>" so col_key() and paginate_cols(by = "____") can
  #     find them.  No RTF vocabulary in here.
}

# 5  PAGES ------------------------------------------------------------------
pages_ae <- function(arms, days) {
  df   <- body_ae(arms, days)
  vals <- denominators(arms)          # one row per page key, one column per token

  hdr <- rtf_col_header(
    c(list(col_cell(1L, "")),
      lapply(arms, function(a)
        col_cell(col_key(a), sprintf("%s\n(N={%s})\nn(%%)", a, make.names(a))))),
    c("System Organ Class\n  Preferred Term", rep(days, length(arms))))

  as_rtftables(
    df,
    read_meta   = FALSE,
    split       = "by_value",
    group_col   = "period",
    drop_cols   = "period",
    stub        = stub_spec(c("SOC", "PT"), label = "row_label", indent = 2L),
    blank_rows  = "between_groups",
    blank_row_first = TRUE, blank_row_end = TRUE,
    cell_format = fmt_value_paren,
    col_rel_width = c(5.5, rep(2, length(days) * length(arms)))
  ) |>
    set_col_header(hdr, values = vals) |>
    paginate_cols(by = "____", carry = 1,
                  page_order = c("cols", "group", "rows"))
}

# 6  RENDER -----------------------------------------------------------------
render <- function(pages, titles, notes, file) {
  doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
    rtf_section(secinfo = list(header = study_header(titles),
                               footer = study_footer(notes))) |>
    rtf_tables(pages, auto_section = TRUE)
  generate_rtfreport(doc, file, overwrite = TRUE)
}

render(pages_ae(ARMS, DAYS),
       titles = c("Table 14.3.1", "Solicited Local Adverse Events",
                  "Safety Analysis Set"),
       notes  = c("Percentages use the number of treated subjects.",
                  "AE = Adverse Event."),
       file   = "t_14_3_1.rtf")
```

### Column headers: two tiers

Match the effort to the header.

```r
# Simple -- name each printed column, inline, no separate object:
pages |> set_col_header(c(Statistic = "Statistic", A = "Drug A", B = "Drug B"))

# Complex -- spanning cells, per-page denominators, selection by key:
hdr <- rtf_col_header(
  c(list(col_cell(1L, "")),
    lapply(arms, function(a)
      col_cell(col_key(a), sprintf("%s\n(N={%s})\nn(%%)", a, make.names(a))))),
  c("Severity", rep(days, length(arms))))
pages |> set_col_header(hdr, values = vals)
```

`col_key(a)` picks the columns whose first `____`-delimited segment is `a`, so
the header never names a position and cannot drift when a column moves.

Token names must be **syntactic** (`[A-Za-z._][A-Za-z0-9._]*`), so an arm
called `HOGE-001` needs `make.names()` — `{HOGE.001}` — on both sides: the
token in the label and the column name in `values`. Build both from the same
expression so they cannot disagree.

### Section composition

The study block is identical across the whole deliverable; only the title
block changes. Composing the header from a function (above) keeps that split,
so a fifty-output program states the protocol line once.

Give `rtf_section()` one section per header that must *change* mid-document —
one per analyte, per period, per subgroup. Within a section, `rtf_tables(...,
auto_section = TRUE)` labels each page from its own page name, which is what
a `split = "by_value"` or `page_by` pagination already carries.

### Fifty outputs

One `pages_*()` function and one `render()` call per output, driven by a
table of specs, then bound into one document:

```r
outputs <- list(
  list(f = "t_14_3_1.rtf", pages = pages_ae(ARMS, DAYS),
       titles = c("Table 14.3.1", "Solicited Local Adverse Events")),
  list(f = "t_14_3_2.rtf", pages = pages_lb(ARMS),
       titles = c("Table 14.3.2", "Laboratory Shift"))
)
for (o in outputs) render(o$pages, o$titles, character(), o$f)

assemble_rtf(vapply(outputs, `[[`, "", "f"), "book.rtf", overwrite = TRUE,
             toc = "auto",
             book_page = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
```

Because stage 5 has no side effect, each `pages_*()` is a unit you can test:
assert on `rtf_columns()`, on `header_map()`, on `nrow(pages[[1]]$data)` —
without writing a file.

---
## 4. Function map — what to call for what

| Need | Call |
|---|---|
| Start a document | `rtf_document(page =, default_format =, watermark =)` |
| Paper / orientation / margins | `rtf_page(paper_size = "letter"/"A4", orientation = "landscape", margin_*_in =)` |
| Document-wide font size etc. | `rtf_default_format(font_size_half_points = 18L, ...)` |
| Edit an already-built document | `rtf_config(doc, page =, default_format =, ...)` |
| Running header / footer | `rtf_section(doc, page =, secinfo = list(header =, footer =))` |
| Build those bands | `rtf_header(rows =)`, `rtf_footer(rows =)` |
| data.frame → paginated pages | `as_rtftables(x, ...)` **(the workhorse — §5)** |
| One table object by hand | `rtftable(data, col_header =, col_spec =, ...)` |
| Place tables on pages | `rtf_tables(doc, tables, titles =, footnotes =, ...)` |
| Place figures on pages | `rtf_figures(doc, figures, ...)` + `rtfplot(path)` |
| Set titles / footnotes later | `rtf_titles(doc, list)`, `rtf_footnotes(doc, list)` |
| Write the RTF | `generate_rtfreport(report, file_path, overwrite = FALSE)` |
| Concatenate finished RTFs | `assemble_rtf(input_files, output_file, toc =, book_page =)` |
| Listings | `listing_col()` → `listing_spec()` → `as_rtftables(data, listing = spec)` |
| Merge a SOC/PT hierarchy into one stub | `as_rtftables(stub = stub_spec(c("SOC", "PT")))`; `stub_cols()` to do it on the data |
| Split a too-wide table by column | `paginate_cols(pages, at =, carry =)` |
| Align decimal points | `set_decimal_split(pages, cols =)` |
| Style after the fact | `style_cols()`, `style_body()`, `style_header()`, `style_zone()` |
| Borders | `border = "tfl"`, `rtf_border()`, `rtf_border_side()` |
| Column-width help | `auto_col_widths(df, ...)`, `fit_listing_widths(data, spec, page =)` |

---

## 5. `as_rtftables()` — the workhorse

Converts a `data.frame` **or a gt / gtsummary / rtables / tern / tfrmt /
flextable / huxtable object** into a list of `rtftable` pages, reading the
source's metadata (headers, alignment, spanning, titles, footnotes) on the way.

The arguments you will actually use:

| Argument | Use it for |
|---|---|
| `max_rows` | rows per page (this is what triggers pagination) |
| `split` | `"none"`, `"rows"`, `"group_safe"` (fill the page, never split a group), `"group_force"`, `"by_value"` |
| `group_col` | which column defines a group |
| `group_by` | how groups are detected: `"auto"`, `"indent"`, `"value"`, `"filled"` |
| `stub` | `stub_spec(c("SOC", "PT"), label =, indent =, layout =, label_span =)` — the full row-stub settings (**preferred**) |
| `stub_vars` | shorthand: fold a hierarchy into one indented stub column. Together with `stub_label` / `stub_indent` / `stub_group_summary` it is **superseded** by `stub =` — still supported and not deprecated, but it cannot reach `layout` or `label_span` |
| `drop_cols` | columns that drive grouping or sorting but are never printed |
| `blank_rows` | `"between_groups"`, integer positions, or `blank_rows_by_*()` specs |
| `sort_by` / `sort_desc` | sort before paginating |
| `page_by` | start a new page whenever this column changes |
| `cont_label` | continuation suffix, default `" (Cont.)"` |
| `collapse_repeats` | blank out repeated values in the named columns |
| `listing` | a `listing_spec()`; renders the data as a listing (§11) |
| `border` | `"tfl"` (default) or an `rtf_border()` object |
| `na` | text used for `NA`, default `""` |
| `column_widths_twips` / `auto_width` | absolute widths / font-aware estimation |
| `cell_format` | per-column formatting applied to the cells |

**The single most common mistake** is reaching for `stub_vars` on a flat table
that has no hierarchy, or omitting it on one that does.

---

## 6. The four table shapes

Their arguments barely overlap — choose by the shape of the table, not by habit.

| Setting | DM | AE | PK | LB shift |
|---|---|---|---|---|
| `stub_vars` (hierarchy) | – | yes | yes | – |
| `group_col` / `group_by` | yes | yes | yes | yes |
| `blank_rows` | – | yes | yes | yes |
| `split` / `max_rows` | yes | yes | yes | – |
| `drop_cols` (hidden carrier) | – | – | – | yes |
| `set_decimal_split()` | – | – | yes | – |
| `paginate_cols()` (too wide) | – | – | yes | – |

### 5a. DM — demographics (flat, grouped by characteristic)

```r
dm_doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
  rtf_tables(
    as_rtftables(dm,
      group_col = "Characteristic",   # keep a characteristic whole
      split     = "group_safe",
      max_rows  = 20,
      border    = "tfl"),
    titles = list(c("Table 14.1.1",
                    "Demographic and Baseline Characteristics",
                    "<Safety Analysis Set>"))
  )
generate_rtfreport(dm_doc, "dm.rtf", overwrite = TRUE)
```

### 5b. AE — SOC / PT hierarchy folded into one stub column

```r
ae_doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
  rtf_tables(
    as_rtftables(ae,
      stub_vars  = c("SOC", "PT"),   # the hierarchy DM does not have
      # equivalently, and with more settings available:
      #   stub = stub_spec(c("SOC", "PT"), label = "SOC / Preferred Term")
      group_by   = "indent",         # groups are found by indentation
      blank_rows = "between_groups",
      split      = "group_safe",
      max_rows   = 20,
      border     = "tfl"),
    titles    = list(c("Table 14.3.1",
                       "Adverse Events by System Organ Class and Preferred Term",
                       "<Safety Analysis Set>")),
    footnotes = list("Percentages use the number of treated subjects.")
  )
```

### 5c. PK — decimal alignment, and a table wider than the page

```r
pk_pages <- as_rtftables(pk,
  stub_vars  = c("Time", "Statistic"),
  group_by   = "indent",
  blank_rows = "between_groups",
  # ABSOLUTE widths: relative ones are normalised to the page, so the table
  # could never be "too wide" and paginate_cols() would have nothing to do.
  column_widths_twips = c(2000L, rep(1800L, 4)),
  border     = "tfl")

pk_pages <- pk_pages |>
  set_decimal_split(cols = 2:5) |>    # line the decimal points up
  paginate_cols(at = 4L, carry = 1L)  # split by COLUMN, repeating the stub

pk_doc <- rtf_document(page = rtf_page(orientation = "landscape"))
for (p in pk_pages) pk_doc <- rtf_tables(pk_doc, p)
```

### 5d. LB shift — grouped by a column that is never printed

```r
as_rtftables(lb,
  group_col  = "PARAMCD",   # group by it ...
  drop_cols  = "PARAMCD",   # ... but never print it
  blank_rows = "between_groups",
  border     = "tfl")
```

---

## 7. Headers, footers and page numbers

`rows` is a **list of named character vectors**; the names choose the columns:

| Keys used | Columns | Alignment |
|---|---|---|
| `l=` only | 1 | left |
| `c=` only | 1 | centre |
| `r=` only | 1 | right |
| `l=` + `r=` | 2 | left, right |
| `l=` + `c=` + `r=` | 3 | left, centre, right |
| unnamed value | 1 | centre |

```r
rtf_header(rows = list(
  c(l = "Protocol: RTF-101",      r = "ACME Pharma"),
  c(l = "Phase III Safety Study", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"),
  c(c = "Table 14.3.1  Shift Table (Safety Population)"),
  c(l = "ALT (Alanine Aminotransferase)")
))

rtf_footer(rows = list(c(l = "Source: ADSL.", r = "CONFIDENTIAL")))
```

### Page tokens

| Token | Behaviour |
|---|---|
| `{AUTO_PAGE}` | dynamic page number |
| `{AUTO_TOTAL_PAGES}` | dynamic total (a NUMPAGES field) — **recommended** |
| `{PAGE}` / `{TOTAL_PAGES}` | static, computed at render time; `assemble_rtf()` leaves them alone |
| `{BOOK_PAGE}` | an empty slot, filled later by `assemble_rtf(book_page =)` |

Convention: the **per-table** number goes in the header and should be static
(`"Page {PAGE} of {TOTAL_PAGES}"`), so that binding the table into a book does
not change it; the **document-wide** number goes in the footer and must be
dynamic (`"Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"`). For a standalone table,
`{AUTO_PAGE}` / `{AUTO_TOTAL_PAGES}` alone is enough.

### One section per analyte / subgroup

```r
doc <- rtf_document()
for (i in seq_along(lab_data)) {
  hdr <- rtf_header(rows = list(c(l = lab_data[[i]]$label)))
  doc <- doc |>
    rtf_section(page = i, secinfo = list(header = hdr, footer = common_footer)) |>
    rtf_tables(list(rtftable(lab_data[[i]]$df, border = "tfl")))
}
```

---

## 8. Column headers and spanning headers

```r
tbl <- rtftable(
  data       = df_shift,
  col_header = c("Baseline", "Low", "Normal", "High", "Low", "Normal", "High"),
  spanning_header = list(
    list(from = 2L, to = 4L, label = "Treatment A  (N=24)", underline = TRUE),
    list(from = 5L, to = 7L, label = "Treatment B  (N=24)", underline = TRUE)
  ),
  column_widths_twips = c(2160L, rep(900L, 6)),
  col_spec = lapply(seq_len(7L), function(j)
    list(col = j, align = if (j == 1L) "left" else "center")),
  border = "tfl", row_height_twips = 280L
)
```

### Editing the header of a finished table

`col_header =` on `rtftable()` / `as_rtftables()` speaks the **source**
columns (before `drop_cols` / `stub_vars`). `set_col_header()` speaks the
**final printed** columns — prefer it on an `as_rtftables()` result.

```r
rtf_columns(pages)     # the final printed column names — look before you write

pages <- pages |> set_col_header(
  # rows in render order, top first.
  list(col_cell("row_label", ""), col_cell(c("g1", "g2"), "Treatment")), # spanning row
  c(row_label = "Category", g1 = "Low", g2 = "High", Total = "Total")    # label row
)

header_map(pages[[1]])                        # inspect: which cell landed where
pages |> add_header_row(c("", "A", "B", ""))  # one extra row (.position = "top"/"bottom")
pages |> style_header(row = 2, cols = 2:3, bold = TRUE)
col_header_from_names(df)                     # a label row from the data names
```

* A **named** label row is a *patch*: each entry says which column it belongs
  to, so the row may be **shorter than the table** and the columns it does not
  mention keep their own name. Naming also survives a column reordering, so
  prefer it. `set_col_header(tbl, c(g1 = "Low", g2 = "High"))`
* An **unnamed** label row is positional and must carry **one entry per
  printed column** — a short one is an error, not a partial fill. (That check
  is what catches a header written for the whole table and applied after
  `paginate_cols()`.) A row that is only *partly* named is positional too.
* A **spanning row** is a `list()` of `col_cell(pos, label, align, bold,
  italic, underline, border)`; `pos` may be a name, a position, or a range
  (`c("g1", "g2")` / `c(2L, 3L)`). `col_key("TRT01A")` builds a name-based
  `pos` selector.
* Per-page numbers in the header (`"Placebo\n(N={n_pbo})"`): pass a
  `values = data.frame(...)` with one row per page key plus `by =`
  (`"group"` / `"rows"` / `"name"`) to `set_col_header()`.
* **Gotcha:** pages built from a bare `data.frame` have *no* header object
  (the data names are used at render time), so `style_header()` errors there.
  Give the table a header first — `col_header =` or `set_col_header()`.

---

## 9. Widths and sizing

| Goal | Argument |
|---|---|
| absolute column widths | `column_widths_twips = c(2880L, 1440L, 1440L)` |
| proportional widths | `col_rel_width = c(2, 1, 1)` |
| font-aware estimate | `w <- auto_col_widths(df, col_header =, table_width_twips = 14400L)` |
| table width, absolute | `table_width_twips = 9000L` (≈ 6.25 in) |
| table width, % of page | `table_width_pct = 70` |
| position on the page | `table_align = "left" / "center" / "right"` |
| data row height | `row_height_twips = 280L` (`NULL` = font-aware default) |

Do not pass both `column_widths_twips` and `col_rel_width` for the same table.
Relative widths are normalised to the page, so a table given only relative
widths can never be "too wide" — `paginate_cols()` needs **absolute** widths.

---

## 10. Titles and footnotes

`titles` / `footnotes` are lists **parallel to the pages**: one entry per page,
each a character vector whose elements become individual lines (`""` = a blank
line, `NULL` = the package default).

```r
rtf_tables(doc, list(df1, df2),
  titles = list(
    c("Table 14.1.1", "", "Demographics (Safety Population)"),
    c("Table 14.1.2", "", "Demographics by Region")),
  footnotes = list(c("Source: ADSL.", "", "N = 160 subjects."), NULL))

# or afterwards
doc |> rtf_titles(list("Page One", "Page Two")) |>
       rtf_footnotes(list(NULL, "Source: ADSL."))
```

Titles render centred and bold; footnotes render left-aligned, with a divider
rule above the first line.

---

## 11. Listings

```r
spec <- listing_spec(list(
  listing_col("USUBJID", width = 15, label = "Unique\nSubject ID",
              collapse_repeats = TRUE),
  listing_col(c("DISPTPD", "BRCA", "HIST"), width = 22,
              label = "Disposition/\nAny (BRCA) Mutations/\nHistology"),
  listing_col("STAGE", label = "Stage at\nInitial\nDiagnosis")
))

pages <- as_rtftables(adsl, listing = spec, max_rows = 8)  # never splits a subject
```

* `listing_col(vars, ...)` — several source variables in one printed column are
  stacked on separate lines. `width` is a **display width in characters** (a
  full-width CJK glyph counts as 2). `\n` in `label` starts a new header line.
* `fit_listing_widths(data, spec, page = rtf_page(...), size_half_points = 16L)`
  works the widths out from the paper, margins, font and data; a `width` you set
  yourself is honoured and the rest fit around it.
* `listing_code(fitted, name = "listing")` prints the fitted spec back as R
  source, to paste into the program and tune by hand.
* `build_listing(data, spec)` returns the reshaped `data.frame` if you want to
  inspect or patch it before rendering; otherwise pass `listing = spec` to
  `as_rtftables()` and skip the intermediate object.

Listings are wide: use `rtf_page(orientation = "landscape")` and a monospaced
font (`font = "courier_new"`).

---

## 12. Figures

```r
png_path <- tempfile(fileext = ".png")
ggplot2::ggsave(png_path, plot = p, width = 7, height = 4.5, dpi = 150)

doc |> rtf_tables(list(rtfplot(png_path, width_twips = 9000L)))
# or
doc |> rtf_figures(list(rtfplot(png_path)), titles = list("Figure 14.1.1"))
```

`rtfplot()` takes a PNG/JPEG path (or a plot object, rendered at
`render_width` / `render_height` / `render_dpi`). One figure per page; to show a
figure together with its summary table, put them on consecutive pages in the
same `rtf_tables()` list.

---

## 13. Borders and styling

```r
rtftable(df, border = "tfl")            # the clinical default: top + bottom rules
rtftable(df, border = rtf_border(top = TRUE, bottom = TRUE,
                                 left = TRUE, right = TRUE, inside_h = TRUE))
rtftable(df, border = "none") |>
  style_zone(header   = rtf_border(top = TRUE, bottom = TRUE),
             last_row = rtf_border(bottom = rtf_border_side("double", 10L)))
```

`rtf_border()` is the one constructor; `rtf_border_side(style, width, color)`
builds a single edge (`"single"`, `"double"`, …).

**Deprecated — do not generate these** (they still work but warn, and are
scheduled for removal before CRAN):

| Deprecated | Write instead |
|---|---|
| `rtf_border_top()` | `rtf_border(top = TRUE)` |
| `rtf_border_bottom()` | `rtf_border(bottom = TRUE)` |
| `rtf_border_box()` | `rtf_border(all = TRUE)` |
| `rtf_border_none()` | `rtf_border()` |
| `rtf_border_with()` | layer it at the attach point (`style_zone()` etc.) |
| `rtf_border_tfl()` | `border = "tfl"` or `rtf_table_style_tfl()` |
| `rtf_table_border()` | `rtftable(border = )` or `style_zone()` |

Post-hoc styling (each accepts one `rtftable` **or** a list of pages):

```r
pages |> style_cols(cols = 1, align = "left", indent_twips = 120)
pages |> style_header(row = 1, cols = 2:3, align = "center")
pages |> style_zone(header = ..., body = ..., first_row = ..., last_row = ...)

# style_body() row selection
pages      |> style_body(rows = ~ Statistic == "Mean", cols = 2, bold = TRUE)
pages[[1]] |> style_body(rows = 3:5, cols = 2, bold = TRUE)
```

`rows` takes `NULL` (all rows), integer positions, a logical vector, a
predicate `function(data)`, or a one-sided formula evaluated inside the body
data (columns as bare names). **On a page list, integer/logical selections are
rejected** — page-local row numbers are ambiguous across pages — so use a
formula/predicate there, or style one page at a time.

Reusable theme: `rtf_table_style_tfl()` / `rtf_table_style()` /
`rtf_table_style_with()`, passed as `style =` to `rtftable()`.

---

## 14. Bring your own table builder

`as_rtftables()` reads **gt**, **gtsummary** (`tbl_summary()`, …), **tfrmt**
(`print_to_gt()`), **rtables / tern** (`VTableTree`), **flextable** and
**huxtable** objects directly, carrying labels, alignment, spanning headers,
titles, footnotes and per-cell styling across.

```r
tbl   <- gtsummary::tbl_summary(trial[c("age", "grade")])
pages <- as_rtftables(tbl, max_rows = 25, border = "tfl")
```

Column *ids* differ by source: gt keeps the data names, gtsummary gives
`label` / `stat_1` / `stat_2`, tfrmt gives `rowname` plus your column levels,
while rtables / flextable / huxtable expose only positional `V1`, `V2`, ….
Check with `rtf_columns(pages)`; **integer indices are the most portable** way
to address columns in source-agnostic code.

Names are carried **verbatim**, non-syntactic ones included — a gt column
called `Drug A (N=60)` is selected with exactly that string, backticked where
R needs a name rather than a string:

```r
pages |> set_col_header(c(`Drug A (N=60)` = "Drug A"))
as_rtftables(g, drop_cols = "2024 total")
```

Anything else: convert it to a plain `data.frame` and re-specify `col_header`,
`col_spec` and friends yourself.

---

## 15. Many deliverables, one document

```r
assemble_rtf(c("t14_1_1.rtf", "t14_3_1.rtf"), "book.rtf",
             overwrite = TRUE,
             toc       = "auto",   # or a character vector / toc_heading() + toc_entry() list
             book_page = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
```

Also: `assemble_files()`, `assemble_folder()`, `assemble_spec()` /
`assemble_from_spec()`, `assemble_toc()`, `toc_heading()`, `toc_entry()`.
`rtf_replace_text()` patches text in a finished RTF.

---

## 16. Formatting helpers (run on the data, before building)

| Helper | What it does |
|---|---|
| `format_count_pct(count, pct, pct_unit = "fraction", pct_sign = FALSE)` | builds the cell from two numeric vectors → `" 31 (51.7)"` (padded to a common width) |
| `fmt_count_paren(x)` / `fmt_count_paren_bare(x)` | **takes a character column** of `"69 (80.2%)"` cells and right-justifies the count *and* the parenthetical part so both line up |
| `fmt_value_paren(x)` | same, when the part before the parenthesis is any text (`"54.2 (11.3)"`) |
| `realign_count_pct(x)` | re-parses `"n (xx.x)"` cells and reformats them to one width; non-matching cells are untouched |
| `fmt_right_align(x)` | right-justify every non-empty cell to the widest — the minimal `cell_format` template |
| `fmt_numeric(data, cols, digits =, rounding = "sas")` | numeric columns → formatted text (`"sas"` = SAS-style rounding) |
| `fmt_round(x, digits)` / `fmt_signif(x, digits)` | rounding / significant digits |
| `catx(sep, ...)` | SAS `CATX`: paste, dropping `NA` and `""` pieces |
| `collapse_repeats(x, cols)` | blank out repeated values in a column |
| `set_blank_rows()`, `blank_rows_by_change("Group")`, `blank_rows_by_rule("Group", "^Total", where = "before")` | blank separator rows |
| `add_cont_label(chunk, label)` | append `" (Cont.)"` |
| `text_width_in(text, font, size)` | estimated display width in inches (exact for Courier New) |

The `fmt_*_paren` / `realign_*` family operates on an **already-formatted
character column**, not on numbers — pad after formatting, not before.

The padding character is the **non-breaking space** (U+00A0), so Word keeps the
alignment; pass `nbsp = " "` if you are comparing the strings in plain text
(`trimws()` will not strip a U+00A0).

---

## 17. Complete public API (nothing outside this list exists)

**Document / render:** `rtf_document` `rtf_config` `rtf_page`
`rtf_default_format` `rtf_watermark` `generate_rtfreport`
`rtfreporter_options` `rtfreporter_reset_defaults` `rtfreporter_ai_manual`

**Sections / bands:** `rtf_section` `combine_sections` `rtf_header` `rtf_footer`
`update_header_row` `update_footer_row` `rtf_header_source` `rtf_titles`
`rtf_footnotes`

**Content:** `rtf_tables` `rtf_figures` `rtftable` `rtfplot` `as_rtftable`
`as_rtftables`

**Column headers:** `col_cell` `col_key` `rtf_col_header` `add_col_header_row`
`col_header_from_names` `set_col_header` `set_header_cell` `header_map`
`add_header_row` `rtf_columns`

**Stub / hierarchy:** `stub_cols` `stub_spec`

**Listings:** `listing_col` `listing_spec` `build_listing` `fit_listing_widths`
`listing_code` `listing_wrap` `listing_wrap_code` `listing_disp_width`
`listing_split_after` `listing_take`

**Pagination / layout:** `paginate` `paginate_cols` `set_decimal_split`
`set_blank_rows` `add_cont_label` `auto_col_widths` `text_width_in`

**Styling:** `style_header` `style_cols` `style_body` `style_zone`
`rtf_table_style` `rtf_table_style_with` `rtf_table_style_tfl`

**Borders:** `rtf_border` `rtf_border_side`
*(deprecated, still exported: `rtf_border_none` `rtf_border_top`
`rtf_border_bottom` `rtf_border_box` `rtf_table_border` `rtf_border_tfl`
`rtf_border_with` — see §13)*

**Formatting:** `fmt_count_paren` `fmt_count_paren_bare` `fmt_value_paren`
`fmt_right_align` `format_count_pct` `realign_count_pct` `fmt_signif`
`fmt_round` `fmt_numeric` `catx` `collapse_repeats` `blank_rows_by_change`
`blank_rows_by_rule`

**Assembly:** `assemble_rtf` `assemble_files` `assemble_toc` `assemble_spec`
`assemble_from_spec` `assemble_folder` `toc_heading` `toc_entry`
`rtf_replace_text`

**Experimental -- cards/cardx ARD to a table data.frame (may change or be
withdrawn; nothing else in the package depends on them, see
`?rtfreporter-ard`):** `ard_keys` `ard_normalize` `ard_spread`
`ard_template` `ard_cells` `ard_overall` `ard_pull` `ard_spec` `ard_spec_template`
`read_ard_spec`
`write_ard_spec` `ard_round`

**Spike -- the same ARD conversion as a deferred, LAST-WINS plan (#474; a
prototype beside the verbs above, may be withdrawn wholesale):**
`rtf_plan` `plan_data` `plan_cells` `plan_levels` `plan_labels` `plan_digits`
`plan_mutate` `plan_filter` `plan_derive` `plan_fmt` `plan_stub` `plan_group`
`plan_hide` `plan_sort` `plan_blanks` `plan_pages`
`plan_style`
`plan_cell_style` `plan_col_header` `plan_titles` `plan_footnotes` `plan_listing`
`plan_after` `apply_plan` `plan_template`

---

## 18. Troubleshooting

| Symptom | Cause / fix |
|---|---|
| `could not find function "rtf_body"` etc. | that is `r2rtf`, not rtfreporter — see §17 |
| The file is not written | add `overwrite = TRUE` to `generate_rtfreport()` |
| Everything lands on one page | pagination needs `max_rows` (and usually `split = "group_safe"`) |
| A group is split across pages | `split = "group_safe"` plus `group_col` / `group_by` |
| `paginate_cols()` does nothing | the table has only relative widths — give it `column_widths_twips` |
| The stub column is not indented | pass `stub_vars` (and `group_by = "indent"` downstream) |
| A grouping column shows up in the output | add it to `drop_cols` |
| Titles land on the wrong page | `titles` / `footnotes` must be one entry **per page**, in order |
| The total page count is wrong in a bound book | use `{AUTO_TOTAL_PAGES}`, or `{BOOK_PAGE}` + `assemble_rtf(book_page =)` |
| `style_header(): this rtftable has no column header` | a bare `data.frame` page has no header object — set one with `col_header =` or `set_col_header()` first |
| `the label row has N labels but the table has M printed columns` | an **unnamed** label row needs one entry per printed column; name the entries to patch a subset, check `rtf_columns()`, and set a whole-table header **before** `paginate_cols()` |
| Header edits hit the wrong column | address by name (`c(TRT01A = "…")`, `col_cell("TRT01A", …)`, `col_key()`), not by position |
| Decimal points are ragged | `set_decimal_split(cols =)` on the pages |

**When something is not covered here:** consult
<https://ichirio.github.io/rtfreporter/reference/> (every function),
`?rtfreporter-recipes` (four runnable table shapes), or
`vignette("rtfreporter-quickstart")`. Do not guess an API.
