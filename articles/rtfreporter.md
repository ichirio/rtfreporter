# Get started with rtfreporter

rtfreporter writes clinical **tables, listings and figures** (TFLs) as
RTF files. There are two ways to get a table into a report:

1.  **From an analysis results dataset (ARD)** – the recommended path.
    The statistics come from
    [cards](https://pharmaverse.github.io/cards/) /
    [cardx](https://insightsengineering.github.io/cardx/); a **plan**
    says how they are laid out. This is what the rest of this page
    shows.
2.  **Bring your own table** – a table already built with gt, gtsummary,
    rtables / tern, tfrmt, flextable, huxtable or a plain data frame
    goes in through
    [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).
    See [the end of this page](#bring-your-own-table).

Either way the document around the table is the same:
[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md),
a running header and footer,
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md),
and
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
to write the file.

    ADSL ──cards──▶ ARD ──normalize_ard()──▶ flat frame ──table_plan() + plan_*()──▶ plan
                                                                                       │
                generate_rtfreport() ◀── rtf_document() |> rtf_section() |> rtf_tables(plan)

``` r

library(rtfreporter)
```

``` r

library(cards)
```

## 1. The statistics: an ARD

The example data is the CDISC pilot `ADSL` that cards ships. The ARD
summarises age, tabulates age group and sex, and does both by arm.
`ard_stack(.by = )` also counts the subjects in each arm – the plan
reads those counts for the `(N=xx)` in the column header, so nobody
types them.

``` r

adsl <- cards::ADSL
adsl$ARM <- factor(adsl$ARM,
                   levels = c("Placebo", "Xanomeline Low Dose",
                              "Xanomeline High Dose"))

ard <- ard_stack(
  adsl, .by = ARM,
  ard_summary(variables = AGE),
  ard_tabulate(variables = c(AGEGR1, SEX)))
```

## 2. Flatten it: `normalize_ard()`

[`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
turns the ARD’s group columns into ordinary key columns, named after
their variables, and adds what a table needs: the kind of each row
(`.kind`) and its label. Its column names are the ones the plan uses.

``` r

nd <- normalize_ard(ard)
nd[1:4, c("ARM", "variable", "variable_level", "stat_name", "stat", ".kind")]
#>       ARM variable variable_level stat_name      stat      .kind
#> 1 Placebo      AGE           <NA>         N 86.000000 continuous
#> 2 Placebo      AGE           <NA>      mean 75.209302 continuous
#> 3 Placebo      AGE           <NA>        sd  8.590167 continuous
#> 4 Placebo      AGE           <NA>    median 76.000000 continuous
```

## 3. The plan: `table_plan()` and the `plan_*()` verbs

[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
names the **roles** – the key that goes across (`cols`) and the keys
that go down (`rows`). Each `plan_*()` verb then adds one declaration;
nothing runs until the plan is used.

``` r

p <- nd |>
  table_plan(cols = "ARM", rows = c(group = "variable")) |>
  plan_cells(
    continuous  = c("n"         = "{N:.0f}",
                    "Mean (SD)" = "{mean:.1f} ({sd:.2f})",
                    "Median"    = "{median:.1f}",
                    "Min, Max"  = "{min:.0f}, {max:.0f}"),
    categorical = "{n:.0f} ({p:.1f%})",
    notes = FALSE) |>
  plan_labels(c(AGE    = "Age (years)",
                AGEGR1 = "Age group, n (%)",
                SEX    = "Sex, n (%)")) |>
  plan_levels(AGEGR1 = c("<65", "65-80", ">80"), SEX = c("F", "M")) |>
  plan_stub(name = "row_label") |>
  plan_blanks(where = "between_groups", first = TRUE) |>
  plan_columns(widths = c(4, 2, 2, 2)) |>
  plan_style(border = "tfl", align_count_pct = TRUE) |>
  plan_col_header(values = list(n = TRUE), rtf_col_header(
    c("",               "{col}"),
    c("Characteristic", "(N={n})")))
```

- [`plan_cells()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  turns statistics into text with templates: `{mean:.1f}` is the mean to
  one decimal, `{p:.1f%}` a proportion shown as a percentage. A
  continuous variable gets one printed row per name.
- [`plan_labels()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  /
  [`plan_levels()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  name the variables and order their levels.
- [`plan_stub()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  folds the variable and its rows into one indented column.
- [`plan_col_header()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  writes the column header; `{col}` is the arm and `{n}` its population,
  read from the ARD.

Before writing any RTF, look at the table the plan makes:

``` r

plan_apply(p, stage = "table")
#>              group     label     Placebo Xanomeline Low Dose Xanomeline High Dose
#> 1      Age (years)         n          86                  84                   84
#> 2      Age (years) Mean (SD) 75.2 (8.59)         75.7 (8.29)          74.4 (7.89)
#> 3      Age (years)    Median        76.0                77.5                 76.0
#> 4      Age (years)  Min, Max      52, 89              51, 88               56, 88
#> 5 Age group, n (%)       <65   14 (16.3)             8 (9.5)            11 (13.1)
#> 6 Age group, n (%)     65-80   42 (48.8)           47 (56.0)            55 (65.5)
#> 7 Age group, n (%)       >80   30 (34.9)           29 (34.5)            18 (21.4)
#> 8       Sex, n (%)         F   53 (61.6)           50 (59.5)            40 (47.6)
#> 9       Sex, n (%)         M   33 (38.4)           34 (40.5)            44 (52.4)
```

## 4. The document: `rtf_document()` to `generate_rtfreport()`

The running header and footer are small tables of rows, each with a
left, centre and right cell. `{PAGE}` / `{TOTAL_PAGES}`, `{PROGRAM}` and
`{DATETIME}` are filled in when the file is written; `{STUDY}` is a
token of our own, given once to `rtf_document(tokens = )`.

``` r

doc <- rtf_document(
    program = "programs/t_dm.R",
    tokens  = list(STUDY = "CDISCPILOT01")) |>
  rtf_section(secinfo = list(
    header = rtf_header(list(
      c(l = "Protocol: {STUDY}", r = "Page {PAGE} of {TOTAL_PAGES}"),
      c(c = "Table 14.1.1  Demographic Characteristics"),
      c(c = "Safety Analysis Set"))),
    footer = rtf_footer(list(
      c(l = "SD = standard deviation."),
      c(l = "Program: {PROGRAM}", r = "{DATETIME}"))))) |>
  rtf_tables(p)

generate_rtfreport(doc, "t_dm.rtf", overwrite = TRUE)
```

[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
takes the plan as it takes any table and runs it. Long tables are paged
by the plan
([`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)),
and the header repeats on every page.

## 5. Where next on this path

- [Tables from an
  ARD](https://ichirio.github.io/rtfreporter/articles/tables-from-ard.md)
  – the same steps in more depth, quartiles, and an adverse-events table
  sorted by frequency within a SOC / PT hierarchy.
- [The plan
  verbs](https://ichirio.github.io/rtfreporter/articles/plan-verbs.md) –
  every `plan_*()` verb, grouped by job, and the rule that a later layer
  wins.
- [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
  – the ARD half of a plan in one call, giving a data frame now instead
  of a plan to run later.
- [`plan_template()`](https://ichirio.github.io/rtfreporter/reference/plan_template.md)
  – reads an ARD and writes a starting plan to edit.
- [Headers, footers and
  tokens](https://ichirio.github.io/rtfreporter/articles/headers-footers.md)
  – `{PROGRAM_FULL}`, `program_fallback`, `drop_empty_rows` and tokens
  of your own.

## Bring your own table

When the table is already built – by gt, gtsummary, rtables / tern,
tfrmt, flextable, huxtable, or as a data frame –
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
reads it, including its labels, spanning headers and footnotes where the
source has them, and gives `rtftable` pages for the same
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md):

``` r

df <- data.frame(
  Parameter = c("Subjects", "Age, mean (SD)"),
  Placebo   = c("86", "75.2 (8.59)"),
  Active    = c("84", "75.7 (8.29)"))

doc2 <- rtf_document() |>
  rtf_tables(as_rtftables(df, border = "tfl"),
             titles = list(c("Table 1", "A table brought as a data frame")))
generate_rtfreport(doc2, "t_byo.rtf", overwrite = TRUE)
```

See [Importing tables with
as_rtftables()](https://ichirio.github.io/rtfreporter/articles/importing-tables.md),
[gt, gtsummary &
rtables](https://ichirio.github.io/rtfreporter/articles/gt-integration.md),
and the *same report, every framework* articles for
[demographics](https://ichirio.github.io/rtfreporter/articles/showcase-dm.md)
and [adverse
events](https://ichirio.github.io/rtfreporter/articles/showcase-ae.md).

## Listings and figures

- Listings: [Listings with a
  plan](https://ichirio.github.io/rtfreporter/articles/plan-listings.md)
  and [Listings end to
  end](https://ichirio.github.io/rtfreporter/articles/listings.md).
- Figures: [From a plot object to a
  page](https://ichirio.github.io/rtfreporter/articles/figures.md).
- Joining finished files into one deliverable with a table of contents:
  [Rendering, post-processing and
  assembly](https://ichirio.github.io/rtfreporter/articles/output.md).
