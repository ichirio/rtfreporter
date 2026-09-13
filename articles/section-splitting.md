# Splitting a report into sections, by table object

``` r

library(rtfreporter)
```

A common clinical layout is **one section per group** – one section per
lab test, per cohort, per visit – each starting on its own page with the
group name in the running header. rtfreporter builds this with
`rtf_tables(..., auto_section = TRUE)`. This article shows, for each
kind of table object, how to get the section names where `auto_section`
can see them.

## How `auto_section` decides where sections start

`rtf_tables(auto_section = TRUE)` reads the **names of the list
elements** it is given:

- a **non-empty** name starts a **new section** and is appended to the
  running header (the base header defined once by
  [`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)
  without a `page`);
- an **empty / unnamed** element **falls through** into the current
  section.

So the whole question is: *how do the per-page `rtftable`s in the list
get their names?*
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
returns **one element per page**, so there are two natural answers
depending on where the grouping variable lives.

| Where the group lives | Object types | How to name the sections |
|----|----|----|
| A **column** of the rendered body | a `data.frame` / `Tplyr` body you assemble; `cards` + `tfrmt` with `label_loc = "column"` | `as_rtftables(split = "by_value", group_col = ...)` – names each page by the group value |
| **Row groups** (label rows in the stub) | `gt`, `gtsummary`, `rtables` / `tern`, `tfrmt` (indented) | build one table per group, then \[[`combine_sections()`](https://ichirio.github.io/rtfreporter/reference/combine_sections.md)\] |

Both routes end the same way – a flat, named list handed to
`rtf_tables(..., auto_section = TRUE)`.

## The data

A small lab dataset: three lab tests, two visits, three treatment arms.

``` r

set.seed(1)
n      <- 24
arms   <- c("Placebo", "Low Dose", "High Dose")
tests  <- c("ALT (U/L)", "AST (U/L)", "Bilirubin (umol/L)")
visits <- c("Baseline", "Week 24")
centre <- c("ALT (U/L)" = 25, "AST (U/L)" = 22, "Bilirubin (umol/L)" = 10)

adlb <- do.call(rbind, lapply(tests, function(p) do.call(rbind, lapply(visits,
  function(v) data.frame(
    SUBJID  = seq_len(n),
    ARM     = factor(sample(arms, n, TRUE), levels = arms),
    PARAMCD = factor(p, levels = tests),
    AVISIT  = factor(v, levels = visits),
    AVAL    = round(rnorm(n, centre[p], 5), 1),
    stringsAsFactors = FALSE)))))

str(adlb, give.attr = FALSE)
#> 'data.frame':    144 obs. of  5 variables:
#>  $ SUBJID : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ ARM    : Factor w/ 3 levels "Placebo","Low Dose",..: 1 3 1 2 1 3 3 2 2 3 ...
#>  $ PARAMCD: Factor w/ 3 levels "ALT (U/L)","AST (U/L)",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ AVISIT : Factor w/ 2 levels "Baseline","Week 24": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ AVAL   : num  23.6 23.5 22.9 26.3 20.5 27.2 18.8 23.9 26.9 25.7 ...
```

We want **one section per lab test**.

## Route A – the group is a column (`split = "by_value"`)

When you assemble the body yourself (a plain `data.frame`, or a `Tplyr`
result), keep the grouping variable as a **column**. Here we summarise
`adlb` to a display body whose first column, `Test`, is the section key:

``` r

cell <- function(p, v, a) {
  x <- adlb$AVAL[adlb$PARAMCD == p & adlb$AVISIT == v & adlb$ARM == a]
  sprintf("%.1f (%.1f)", mean(x), sd(x))
}
lab_body <- do.call(rbind, lapply(tests, function(p) {
  d <- data.frame(Test = p, Visit = visits, check.names = FALSE,
                  stringsAsFactors = FALSE)
  for (a in arms) d[[a]] <- vapply(visits, function(v) cell(p, v, a), character(1))
  d
}))
lab_body
#>                 Test    Visit    Placebo   Low Dose  High Dose
#> 1          ALT (U/L) Baseline 26.2 (3.7) 23.7 (2.9) 24.0 (3.5)
#> 2          ALT (U/L)  Week 24 25.8 (4.9) 25.3 (4.2) 26.8 (7.0)
#> 3          AST (U/L) Baseline 23.7 (5.1) 21.6 (4.6) 20.8 (4.9)
#> 4          AST (U/L)  Week 24 19.9 (4.3) 17.5 (3.2) 21.2 (7.8)
#> 5 Bilirubin (umol/L) Baseline  9.1 (4.0) 11.6 (5.0)  9.1 (4.0)
#> 6 Bilirubin (umol/L)  Week 24 10.7 (5.5)  5.0 (5.7)  9.0 (4.1)
```

`as_rtftables(split = "by_value", group_col = "Test")` makes **one page
per distinct `Test`** and names each page with that value.
`drop_cols = "Test"` hides the key column from the printed table while
still using it to split and name – so the page names survive even though
the column does not:

``` r

pages <- as_rtftables(lab_body,
                      split      = "by_value",
                      group_col  = "Test",
                      drop_cols  = "Test",
                      col_header = c("Visit", arms))
names(pages)              # one section name per lab test
#> [1] "ALT (U/L)"          "AST (U/L)"          "Bilirubin (umol/L)"
```

That named list is already `auto_section`-ready:

``` r

hdr <- rtf_header(rows = list(
  c(l = "Acme Biopharma, Inc.", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"),
  c(c = "Table 14.3  Laboratory Results -- Mean (SD) by Visit"),
  c(c = "Safety Population"),
  c(c = "")))

doc <- rtf_document(page = list(paper_size = "letter", orientation = "landscape")) |>
  rtf_section(secinfo = list(header = hdr)) |>     # base header (no `page`)
  rtf_tables(pages, auto_section = TRUE)           # one section per lab test
```

``` r

generate_rtfreport(doc, "lab_by_test.rtf", overwrite = TRUE)
```

Each lab test now opens its own section with the test name appended to
the shared header. `Tplyr` fits the same route: `Tplyr::build()` returns
a data frame, so add the `Test` column and call
`as_rtftables(split = "by_value")` exactly as above.

## Route B – row-grouped frameworks (`combine_sections()`)

`gt`, `gtsummary` and `rtables` / `tern` render a grouping variable as
**indented label rows in the stub**, not as a column. After rendering
there is no column for `split = "by_value"` to key on. The natural unit
there is **one table per section**: build a small table for each lab
test, then stitch them together with
\[[`combine_sections()`](https://ichirio.github.io/rtfreporter/reference/combine_sections.md)\],
which names each table’s first page and lets the rest fall through.

[`combine_sections()`](https://ichirio.github.io/rtfreporter/reference/combine_sections.md)
is format-agnostic – it only arranges the names of a list of
`rtftable`s, so the same call works whatever built them:

``` r

# (stand-in page lists; in practice each comes from as_rtftables() below)
alt <- as_rtftables(data.frame(Visit = visits, Placebo = c("26.2", "25.8")))
ast <- as_rtftables(data.frame(Visit = visits, Placebo = c("23.7", "19.9")))

sections <- combine_sections(`ALT (U/L)` = alt, `AST (U/L)` = ast)
names(sections)          # first page named, the rest blank (fall-through)
#> [1] "ALT (U/L)" "AST (U/L)"
```

### gtsummary

Build one `tbl_*` per test, convert each with
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
and combine. [`setNames()`](https://rdrr.io/r/stats/setNames.html) +
[`do.call()`](https://rdrr.io/r/base/do.call.html) keep it to a single
loop:

``` r

library(gtsummary)

one_test <- function(p) {
  adlb[adlb$PARAMCD == p, c("ARM", "AVISIT", "AVAL")] |>
    tbl_continuous(variable = AVAL, by = ARM, include = AVISIT,
                   statistic = ~ "{mean} ({sd})") |>
    as_rtftables(read_meta = TRUE)
}

sections <- do.call(combine_sections, setNames(lapply(tests, one_test), tests))

doc <- rtf_document(page = list(paper_size = "letter", orientation = "landscape")) |>
  rtf_section(secinfo = list(header = hdr)) |>
  rtf_tables(sections, auto_section = TRUE)
generate_rtfreport(doc, "lab_gtsummary.rtf", overwrite = TRUE)
```

### rtables / tern

Same shape – one
[`build_table()`](https://rdrr.io/pkg/rtables/man/build_table.html) per
test, then combine:

``` r

library(rtables)

one_test <- function(p) {
  basic_table(show_colcounts = TRUE) |>
    split_cols_by("ARM") |>
    split_rows_by("AVISIT") |>
    analyze("AVAL", afun = function(x)
      rcell(c(mean(x), sd(x)), format = "xx.x (xx.x)")) |>
    build_table(df = adlb[adlb$PARAMCD == p, ]) |>
    as_rtftables(read_meta = TRUE)
}

sections <- do.call(combine_sections, setNames(lapply(tests, one_test), tests))
```

> **One table, or one per section?** If instead you
> `split_rows_by("PARAMCD")` in a *single* `rtables` table, all three
> tests render as row groups inside **one** table – i.e. **one**
> section, with the tests as indented headings. That is a perfectly good
> layout; use it when you want the tests together. Build *per-test*
> tables and
> [`combine_sections()`](https://ichirio.github.io/rtfreporter/reference/combine_sections.md)
> only when each test should be its **own** section.

## Choosing a route

| Object                               | Route                  |
|:-------------------------------------|:-----------------------|
| data.frame / Tplyr                   | A – split = “by_value” |
| cards + tfrmt (label_loc = “column”) | A – split = “by_value” |
| gt / gtsummary                       | B – combine_sections() |
| rtables / tern                       | B – combine_sections() |
| tfrmt (indented)                     | B – combine_sections() |

## A note on the “one section per page” default

You do **not** have to use either route. If you hand
`auto_section = TRUE` a list whose pages are each named (or each
unnamed), you simply get **one RTF section per page**. That is harmless
– it matches the one-page-one-section convention many SAS-based
pipelines use – and the page numbering and headers still come out
correct. `split = "by_value"` and
[`combine_sections()`](https://ichirio.github.io/rtfreporter/reference/combine_sections.md)
are for the tidier **one-section-per-group** layout, not a correctness
fix.

See also [Headers and
footers](https://ichirio.github.io/rtfreporter/articles/headers-footers.md)
for the section / header model, [Paginating with
`as_rtftables()`](https://ichirio.github.io/rtfreporter/articles/pagination.md)
for the `split` strategies, and [Importing
tables](https://ichirio.github.io/rtfreporter/articles/importing-tables.md)
for reading each framework’s object.

## Where next

- [Multi-stub section
  splits](https://ichirio.github.io/rtfreporter/articles/multistub-section-split.md)
  — one section per group, with an indented stub

The four recipes (`?rtfreporter-recipes`) are the same ground covered as
runnable help-page examples: demographics, adverse events, PK and
laboratory, each data-in to RTF-out.
