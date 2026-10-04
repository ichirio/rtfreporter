# rtfreporter — AI user manual

**This manual documents rtfreporter 0.8.2.9019** (the development
version, after release 0.8.2).
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

1. **Only call functions listed in §18.** rtfreporter is a young package and is
   almost certainly *not* in your training data. If a requested feature has no
   function in §18, say so plainly instead of inventing a plausible name or
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
| Borders | `border = "tfl"`, `rtf_border()`, `rtf_border_line()` |
| Column-width help | `auto_col_widths(df, ...)`, `fit_listing_widths(data, spec, page =)` |
| Table from a cards / cardx ARD | `normalize_ard()` → `table_plan()` → `plan_*()` → `rtf_tables(doc, plan)` **(§17)** |

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
| `stub` | fold a hierarchy into one indented stub column: `stub = c("SOC", "PT")`, or `stub_spec(c("SOC", "PT"), label =, indent =, layout =, label_span =)` for the settings |
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

**The single most common mistake** is reaching for `stub` on a flat table
that has no hierarchy, or omitting it on one that does.

---

## 6. The four table shapes

Their arguments barely overlap — choose by the shape of the table, not by habit.

| Setting | DM | AE | PK | LB shift |
|---|---|---|---|---|
| `stub` (hierarchy) | – | yes | yes | – |
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
      stub       = c("SOC", "PT"),   # the hierarchy DM does not have
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
  stub       = c("Time", "Statistic"),
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
| `{PROGRAM}` / `{PROGRAM_NAME}` / `{PROGRAM_DIR}` | the program writing the file (`generate_rtfreport(program =)`, else `options(rtfreporter.program)`, else the `Rscript` script), its file name, its folder |
| `{DATETIME}` / `{DATETIME:<fmt>}` | when the file is written, `%d%b%Y  %H:%M` in the C locale (`options(rtfreporter.datetime_format)`); `options(rtfreporter.render_time)` fixes it |

`rtf_text_tokens()` returns this list as data (token, page or run, when it is
filled, what it becomes, an example) for a program that offers the tokens.

A run-information footer line is then one row, with no helper:
`c(l = "{PROGRAM}      Generated on: {DATETIME}")`.

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
  col_header = list(                     # rows top first
    list(list(from = 2L, to = 4L, label = "Treatment A  (N=24)", underline = TRUE),
         list(from = 5L, to = 7L, label = "Treatment B  (N=24)", underline = TRUE)),
    c("Baseline", "Low", "Normal", "High", "Low", "Normal", "High")
  ),
  column_widths_twips = c(2160L, rep(900L, 6)),
  col_spec = lapply(seq_len(7L), function(j)
    list(col = j, align = if (j == 1L) "left" else "center")),
  border = "tfl", row_height_twips = 280L
)
```

### Editing the header of a finished table

`col_header =` on `rtftable()` / `as_rtftables()` speaks the **source**
columns (before `drop_cols` / `stub`). `set_col_header()` speaks the
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
             last_row = rtf_border(bottom = rtf_border_line("double", 10L)))
```

`rtf_border()` is the one constructor; `rtf_border_line(style, width, color)`
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
| `rtf_border_side()` | `rtf_border_line()` (same arguments) |
| `add_col_header_row()` | the row inside `rtf_col_header()` (rows top first) |
| `col_header_from_names()` | nothing: `as_rtftables()` splits the names (`header_sep =`) |
| `set_header_cell()` | the cell in `rtf_col_header()` / `set_col_header()`; `style_header()` |
| `update_header_row()` / `update_footer_row()` | `rtf_header(rows = )` / `rtf_footer(rows = )` made again |
| `paginate()` | `as_rtftables()` |
| `rtftable(spanning_header = )`, `as_rtftable(gt_obj = )` | a first row of `col_header`; `as_rtftable(x)` |
| `as_rtftables(stub_vars = , stub_label = , stub_indent = , stub_group_summary = )` | `stub = stub_spec(vars, label = , indent = , group_summary = )` |
| `set_blank_rows(df = )`, `add_cont_label(chunk = )` | `data =` (or by position) |
| `assemble_files()`, `assemble_spec()`, `assemble_toc()`, `assemble_from_spec()`, `toc_heading()`, `toc_entry()` | `assemble_folder()` (the table of contents) and `assemble_rtf(toc = <that table or its path>)` |

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
             toc       = "auto",   # or labels, or a table (file, label, heading, level)
             book_page = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
```

A whole folder: `assemble_folder(dir, "book.rtf")`; without an output file it
returns the table of contents (a data.frame) to edit and pass to
`assemble_rtf(toc = )` (or save with `spec_file =` and pass the path).
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
| `round_num(x, digits)` | numeric rounding with the package rule; `options(rtfreporter.rounding = "sas")` makes **every** formatter round a half away from zero, as SAS does (default `"r"` = half to even) |
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

## 17. Tables from a cards / cardx ARD — the plan

Use this when the statistics are already in an **ARD** (the long frame
`cards` / `cardx` return: one statistic per row). The same route works for
any long frame of statistics, meaning keys plus a statistic name and a value,
built with dplyr. If the table already exists as a data frame, `gt`,
`rtables` and so on, stay with `as_rtftables()` (§5). The two routes produce
the same `rtftable` pages.

```
ARD ─ normalize_ard() ─▶ flat frame ─ table_plan() ─▶ plan ─ plan_*() ─▶ plan
                                                                  │
                          rtf_tables(doc, plan)  /  plan_apply(plan)
```

**Rules of the plan (tell the user these when they are confused):**

1. `table_plan()` takes the **roles** only: `cols` (across), `rows` (down),
   `label` (the row identity) and `stat`. No other argument exists on it.
2. Each `plan_*()` verb has **one job**. Every other setting belongs to the
   verb for that job (the table below).
3. **A later layer wins.** `plan_digits(2) |> plan_digits(AGE = 0)` means 2
   everywhere and 0 for AGE. Adjust a table by *adding* a line.
4. Nothing runs until `rtf_tables(doc, plan)` or `plan_apply(plan)`.
   `rtf_tables()` takes the plan directly, so there is no need to call
   `plan_apply()` first.
5. A raw cards ARD is refused by `table_plan()`. Flatten it with
   `normalize_ard()` first, and look at the result's column names: they are
   what `cols` / `rows` name.

### A demographics table, end to end

```r
library(rtfreporter); library(cards)
adsl <- pharmaverseadam::adsl
adsl <- adsl[adsl$SAFFL == "Y", ]
adsl$TRT01A <- factor(adsl$TRT01A,
                      levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))

ard <- ard_stack(adsl, .by = TRT01A,              # .by also counts the arms: the header's N
  ard_summary(variables = AGE, statistic = ~ continuous_summary_fns(
    c("N", "mean", "sd", "median", "p25", "p75", "min", "max"))),
  ard_tabulate(variables = c(AGEGR1, SEX)))

p <- ard |>
  normalize_ard() |>
  table_plan(cols = "TRT01A", rows = c(group = "variable")) |>
  plan_cells(
    continuous  = c("n"               = "{N:.0f}",           # a NAMED vector = one row per name
                    "Mean (SD)"       = "{mean:.1f} ({sd:.2f})",
                    "Median (Q1, Q3)" = "{median:.1f} ({p25:.1f}, {p75:.1f})",
                    "Min, Max"        = "{min:.0f}, {max:.0f}"),
    categorical = c(n == 0 ~ "0", "{n:.0f} ({p:.1f%})"),     # guarded chain: first that applies
    notes = FALSE) |>
  plan_labels(c(AGE = "Age (years)", AGEGR1 = "Age group, n (%)", SEX = "Sex, n (%)")) |>
  plan_levels(SEX = c("F", "M")) |>
  plan_stub(name = "row_label") |>
  plan_blanks(where = "between_groups", first = TRUE, last = TRUE) |>
  plan_columns(widths = c(4, 2, 2, 2)) |>
  plan_style(border = "tfl", align_count_pct = TRUE) |>
  plan_col_header(values = list(n = TRUE), rtf_col_header(   # {n} read from the ARD
    c("",               "{col}"),
    c("Characteristic", "(N={n})")))

doc <- rtf_document() |>
  rtf_section(secinfo = list(header = rtf_header(list(
    c(l = "Protocol: CDISCPILOT01", r = "Page {PAGE} of {TOTAL_PAGES}"),
    c(c = "Table 14.1.1  Demographics"))))) |>
  rtf_tables(p)
generate_rtfreport(doc, "t_dm.rtf", overwrite = TRUE)
```

### AE by SOC / PT, most frequent first

```r
adae <- pharmaverseadam::adae
adae <- adae[adae$TRTEMFL %in% "Y" & adae$SAFFL %in% "Y", ]
adae$TRT01A <- factor(adae$TRT01A, levels = levels(adsl$TRT01A))
ard_ae <- ard_stack_hierarchical(adae, variables = c(AEBODSYS, AEDECOD),
  by = TRT01A, denominator = adsl, id = USUBJID, over_variables = TRUE)

p_ae <- ard_ae |>
  normalize_ard(hierarchy = c("AEBODSYS", "AEDECOD"), overall = "Any TEAE") |>
  table_plan(cols = "TRT01A", rows = c(SOC = "AEBODSYS", PT = "AEDECOD"), label = NA) |>
  plan_sort(".overall", "SOC", ".depth", "-n", "PT") |>   # Any first; SOC, its own row, PTs by n desc
  plan_cells("{n:.0f} ({p:.1f%})", notes = FALSE) |>
  plan_stub(name = "System Organ Class\n  Preferred Term", indent = 2) |>
  plan_paginate_rows(max_rows = 30, split = "group_safe") |>
  plan_style(border = "tfl") |>
  plan_col_header(values = list(n = TRUE),
                  rtf_col_header(c("", "{col}"), c("", "(N={n})")))
```

### Cell templates

| Token | Means |
|---|---|
| `{mean}` | the value cards formatted (`stat_fmt`), else `stat` |
| `{mean:.1f}` | 1 decimal place (`plan_digits(rounding = "sas")` for SAS-style halves) |
| `{p:.1f%}` | ×100, then 1 decimal |
| `{mean:.3s}` | 3 significant digits |
| `{n:d}` | integer |

* `"{n} ({p})"`: one row. `c("A" = "...", "B" = "...")` (named): one row per
  name, and the name is the row label. `c(n == 0 ~ "0", "{n} ({p})")`: a
  chain, where the first element whose guard holds and whose statistics
  exist wins.
* Keys of `plan_cells()` / `plan_digits()`: an analysis variable (`AGE =`),
  then a `context`, then a kind (`continuous =` / `categorical =`), then
  `default` (one unnamed entry). The narrower key wins.
* `plan_digits()` fills only *open* tokens (`{mean}`). `{mean:.2f}` keeps its
  own digits.

### Which verb says what

| Job | Verb (arguments) |
|---|---|
| Start; roles | `table_plan(x, cols, rows, label, stat)`: `stat = c(variable =, name =, value =)` renames the three parts for a non-cards frame |
| Cell text | `plan_cells(..., stats = "cells"/"rows", value = "stat"/"stat_fmt", na, notes)`; on a finished table `na` alone (`as_rtftables(na =)`) |
| Digits / rounding | `plan_digits(..., rounding)`: `plan_digits(continuous = c(mean = 1, sd = 2))`; on a finished table `plan_digits(<column> = 2)`, `plan_digits(.rows = c(Mean = 1))` |
| Order of values | `plan_levels(VAR = c(...))` |
| Leave out the levels no record has (0 in every column, e.g. a code list's unused values) | `plan_levels(.drop_empty = c("RACE"))` |
| Printed text of values / variables | `plan_labels(c(AGE = "Age (years)"))` |
| Printed text of one analysis variable's levels (and its own name) | `plan_labels(SEX = c(SEX = "Sex", F = "Female", M = "Male"))`; their order: `plan_levels(SEX = c("M", "F"))` |
| Row order | `plan_sort(..., stat, keep)`: keys like `".overall"`, `".depth"`, a column, a statistic; `-name` = descending. Keep a hierarchy nested: `plan_sort(".overall", "SOC", ".depth", "-n", "PT")`, never `-n` alone |
| Stub (indented row headings) | `plan_stub(vars, name, indent, group_summary, before)` |
| Groups down the body | `plan_row_group(mode = "value"/"indent"/"filled"/"auto", collapse, group_col)`; `group_col` only on a finished table (an ARD plan's is its outermost row key) |
| Blank rows | `plan_blanks(where, first, last, counted)`: `where = "between_groups"`; listings `"records"` |
| Columns not printed | `plan_hide("COL")` |
| Widths, decimal alignment | `plan_columns(widths, decimal, row_title, auto_width, sep, cell_format, column_widths_twips)`: `widths = c(row_label = 5, .values = 2)` |
| Column header | `plan_col_header(header, values, header_sep, col_header_align)`: `values = list(n = TRUE)` reads N from the ARD; `list(n = "page", N = "table")` for per-page splits; `header_sep` splits a finished table's names into spanning rows |
| Whole-table look | `plan_style(border, align_count_pct, font, font_size_half_points, row_height_twips, ..., border_header, border_spanning, border_body, border_first_row, border_last_row, header_align, header_bold, header_italic, align, bold, italic, underline, table_width_twips, table_width_pct, table_width_pct_of_writable)`: the `border_*` and look fields make one `rtf_table_style()` |
| Look of some cells | `plan_cell_style(cols, header, where, bold, italic, align, color, background, border, underline, indent_twips)`: a value, or a formula `bold = ~ is.na(label)` |
| A page per value | `plan_paginate_group(col, keep)` |
| Row budget per page | `plan_paginate_rows(max_rows, split, break_before, min_group_rows, cont_label, page_by)`; `page_by =`: BY pages first (a period, a cohort), the row budget inside each |
| Too wide: column blocks | `plan_paginate_cols(at, cut_by, every, keep, col_header, fit, allow_span_break, order)` |
| A listing | `plan_listing(listing_col(...), ..., type, sep, spacer, spacer_rel_width, layout, wrap)` |
| Titles / footnotes | `plan_titles(...)`, `plan_footnotes(...)` (`pages =` for one block per page) |
| Anything else | `plan_after(function(pages) ...)`: last resort; prefer a verb |

Every verb maps onto an existing call: the ARD half onto `widen_ard()`, the
display half onto `as_rtftables()`, `stub_cols()`, `set_col_header()`,
`paginate_cols()` and the `style_*()` verbs. For example, `as_rtftables(max_rows
=)` becomes `plan_paginate_rows(max_rows =)`, `drop_cols` becomes
`plan_hide()`, `group_by` becomes `plan_row_group(mode =)`, `blank_rows`
becomes `plan_blanks(where =)`, and `split = "by_value", group_col` becomes
`plan_paginate_group(col =)`; `page_by` becomes `plan_paginate_rows(page_by =)`;
`group_col` alone on a finished table becomes `plan_row_group(group_col =)`;
`cell_format` becomes `plan_columns(cell_format =)`, `header_sep`
`plan_col_header(header_sep =)`, `style =` `plan_style(<its fields>)`, and
`na` `plan_cells(na =)`. Not carried over, by design: `read_meta` /
`read_attributes` (a plan's input has no adapter metadata; the plan sets
the attributes itself), and the deprecated `stub_vars` / `stub_label` /
`stub_indent` / `stub_group_summary` (`plan_stub()`) and `spanning_header`
(a row of `plan_col_header()`).

### Looking inside

* `print(p)`: the layers, the column names the roles may name, and the
  values every header token (`{n}`, `{n:sum}`, `{n:<column>}`) will take.
* `plan_apply(p, stage = "input")`: the frame going in.
  `stage = "table"`: the table data frame. `stage = "args"`: the resolved
  calls (`$widen`, `$rtf`), without running them. `stage = "pages"`: the
  `rtftable` pages.
* `plan_layers(p)`: the plan read back, layer by layer.
* `plan_header_tokens(p)`: the tokens a `plan_col_header()` cell may carry
  (`{col}`, `{n}`, `{n:sum}` ...), with their values, as data.
* `plan_template(ard, cols = "TRT01A")`: writes a starting program for this
  ARD (`file =` to save it). `form = "widen"` writes the one-call form
  instead.

### Without a plan

`widen_ard(nd, cols, rows, label, cells, ...)` does the ARD half at once and
returns the table data frame, to finish with `as_rtftables()`. Its `cells` is
a *list* keyed like `plan_cells()`: `list(continuous = c(...), categorical =
"...")`. `pull_ard(ard, ...)` takes single values out, for a footnote or a
header. `list_ard_keys(ard)` lists the keys, variables and statistics.
`cell_rows()` makes one named row whose value is a chain, and `overall_row()`
adds an "Any" row.

### Names that do NOT exist (do not write them)

| Wrong | Right |
|---|---|
| `spread_ard()` | `widen_ard()` |
| `ard_normalize()`, `ard_spread()`, `ard_table()`, `ard_template()` | `normalize_ard()`, `widen_ard()`, `plan_template()` |
| `rtf_plan()` | `table_plan()` |
| `plan_fmt()` | `plan_digits()` |
| `plan_header_style()`, `plan_col_style()`, `plan_zone_style()` | `plan_cell_style(header = TRUE / cols =)`, `plan_style(border_header = ...)` |
| `table_plan(cells =, stats =, sort_stat =, sep =, na =)` | `plan_cells()`, `plan_sort(stat =)`, `plan_columns(sep =)` |
| `plan_paginate_rows(by =)` | `plan_paginate_rows(page_by =)` (BY pages, a row budget inside), or `plan_paginate_group()` (a page per value) |
| `plan_row_group(col =)`, `plan_sort(desc =)` | the outermost row key is used (a finished table: `plan_row_group(group_col =)`); `plan_sort("-n")` |
| `show =`, `plan_stub(into =)` | `keep =`, `plan_stub(name =)` |
| `plan_apply(stage = "long")`, `$spread` | `stage = "input"`, `$widen` |
| `tfl_table_plan()`, `tfl_*()` | **tflspec**, not rtfreporter: it reads an Excel spec into a plan |

---

## 18. Complete public API (nothing outside this list exists)

**Document / render:** `rtf_document` `rtf_config` `rtf_page`
`rtf_default_format` `rtf_watermark` `generate_rtfreport` `rtf_text_tokens`
`rtfreporter_options` `rtfreporter_reset_defaults` `rtfreporter_ai_manual`

**Sections / bands:** `rtf_section` `combine_sections` `rtf_header` `rtf_footer`
`rtf_header_source` `rtf_titles` `rtf_footnotes`

**Content:** `rtf_tables` `rtf_figures` `rtftable` `rtfplot` `as_rtftable`
`as_rtftables`

**Column headers:** `col_cell` `col_key` `rtf_col_header` `set_col_header`
`header_map` `add_header_row` `rtf_columns`

**Stub / hierarchy:** `stub_cols` `stub_spec`

**Listings:** `listing_col` `listing_spec` `build_listing` `fit_listing_widths`
`listing_code` `listing_wrap` `listing_wrap_code` `listing_disp_width`
`listing_split_after` `listing_take`

**Pagination / layout:** `paginate_cols` `set_decimal_split`
`set_blank_rows` `add_cont_label` `auto_col_widths` `text_width_in`

**Styling:** `style_header` `style_cols` `style_body` `style_zone`
`rtf_table_style` `rtf_table_style_with` `rtf_table_style_tfl`

**Borders:** `rtf_border` `rtf_border_line`
*(deprecated, still exported: `rtf_border_none` `rtf_border_top`
`rtf_border_bottom` `rtf_border_box` `rtf_table_border` `rtf_border_tfl`
`rtf_border_with` `rtf_border_side` `add_col_header_row`
`col_header_from_names` `set_header_cell` `update_header_row`
`update_footer_row` `paginate` `assemble_files` `assemble_spec`
`assemble_toc` `assemble_from_spec` `toc_heading` `toc_entry` — see §13)*

**Formatting:** `fmt_count_paren` `fmt_count_paren_bare` `fmt_value_paren`
`fmt_right_align` `format_count_pct` `realign_count_pct` `fmt_signif`
`fmt_round` `fmt_numeric` `round_num` `catx` `collapse_repeats`
`blank_rows_by_change` `blank_rows_by_rule`

**Tables from a cards / cardx ARD:** `normalize_ard` `widen_ard` `pull_ard` `list_ard_keys`
`cell_rows` `overall_row` `table_plan` `plan_apply` `plan_layers` `plan_header_tokens`
`plan_template` `plan_levels` `plan_labels` `plan_cells` `plan_digits`
`plan_stub` `plan_cell_style` `plan_paginate_group` `plan_row_group`
`plan_hide` `plan_sort` `plan_blanks` `plan_paginate_rows`
`plan_paginate_cols` `plan_style` `plan_columns` `plan_col_header`
`plan_listing` `plan_titles` `plan_footnotes` `plan_after`

**Assembly:** `assemble_rtf` `assemble_folder`
`rtf_replace_text`

---

## 19. Troubleshooting

| Symptom | Cause / fix |
|---|---|
| `could not find function "rtf_body"` etc. | that is `r2rtf`, not rtfreporter — see §18 |
| The file is not written | add `overwrite = TRUE` to `generate_rtfreport()` |
| Everything lands on one page | pagination needs `max_rows` (and usually `split = "group_safe"`) |
| A group is split across pages | `split = "group_safe"` plus `group_col` / `group_by` |
| `paginate_cols()` does nothing | the table has only relative widths — give it `column_widths_twips` |
| The stub column is not indented | pass `stub` (and `group_by = "indent"` downstream) |
| A grouping column shows up in the output | add it to `drop_cols` |
| Titles land on the wrong page | `titles` / `footnotes` must be one entry **per page**, in order |
| The total page count is wrong in a bound book | use `{AUTO_TOTAL_PAGES}`, or `{BOOK_PAGE}` + `assemble_rtf(book_page =)` |
| `style_header(): this rtftable has no column header` | a bare `data.frame` page has no header object — set one with `col_header =` or `set_col_header()` first |
| `the label row has N labels but the table has M printed columns` | an **unnamed** label row needs one entry per printed column; name the entries to patch a subset, check `rtf_columns()`, and set a whole-table header **before** `paginate_cols()` |
| Header edits hit the wrong column | address by name (`c(TRT01A = "…")`, `col_cell("TRT01A", …)`, `col_key()`), not by position |
| Decimal points are ragged | `set_decimal_split(cols =)` on the pages |
| `table_plan()` refuses a cards ARD | flatten it first: `ard |> normalize_ard() |> table_plan(...)` |
| A header prints `(N=NA)` with a warning | the ARD does not state that population: build it with `ard_stack(.by = TRT)`, or give it: `plan_col_header(values = list(n = c(Placebo = 86, ...)))` |
| "the same key is given twice in one call" | a later *layer* wins, not a later argument: make it a second `plan_*()` call |
| "widen_ard(): N ARD rows were not used" | a note, not an error: no template names those statistics; `plan_cells(notes = FALSE)` silences it |
| A plan verb seems to be ignored | a later layer said the same key: `plan_apply(p, "args")` shows what was resolved |

**When something is not covered here:** consult
<https://ichirio.github.io/rtfreporter/reference/> (every function),
`?rtfreporter-recipes` (four runnable table shapes), or
`vignette("rtfreporter-quickstart")`. Do not guess an API.
