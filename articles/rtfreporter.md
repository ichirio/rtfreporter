# Get started with rtfreporter

rtfreporter writes clinical **tables, listings and figures** (TFLs) as
RTF files. Its two strengths are the two ways to get a table into a
report:

1.  **From an analysis results dataset (ARD), almost as-is** – the
    recommended path. The statistics come from
    [cards](https://pharmaverse.github.io/cards/) /
    [cardx](https://insightsengineering.github.io/cardx/); a few lines
    say how they are laid out. This is what the rest of this page shows.
2.  **Bring your own table** – it reads the table objects you already
    have: a table built with gt, gtsummary (a `tbl_split` too), rtables
    / tern, rlistings, tfrmt, flextable, huxtable or a plain data frame
    goes in through
    [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md).
    See [the end of this page](#bring-your-own-table).

From an ARD, rtfreporter takes three steps: the **table**, straight from
the ARD, the **report** around it (the document, its running header and
footer), and the **RTF file**.

[TABLE]

A table brought from another package skips step 1:
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
takes it as it is.

This page is the shortest way through the three steps. Each of them in
detail – the roles, the cells, the header, and more tables – is in
[Tables from an
ARD](https://ichirio.github.io/rtfreporter/articles/tables-from-ard.md).

The table this page writes:

![The first page of t_dm.rtf: the study’s running header, Table 14.1.1
Demographic Characteristics for the Safety Analysis Set, age as n, mean
(SD), median and min, max and age group and sex as n (%) under three
arms with (N=86), (N=84) and (N=84), then the footnotes, the program’s
path and the run date.](../reference/figures/get-started-dm.png)

The first page of t_dm.rtf: the study’s running header, Table 14.1.1
Demographic Characteristics for the Safety Analysis Set, age as n, mean
(SD), median and min, max and age group and sex as n (%) under three
arms with (N=86), (N=84) and (N=84), then the footnotes, the program’s
path and the run date.

The code is folded under **Show code** below each step.

``` r

library(rtfreporter)
```

``` r

library(cards)
```

## The input: an ARD

> **The ARD is made before rtfreporter, with
> [cards](https://pharmaverse.github.io/cards/) and
> [cardx](https://insightsengineering.github.io/cardx/).** That is data
> preparation, the analysis itself, and outside rtfreporter: see their
> documentation for the statistics they compute. rtfreporter starts from
> the ARD as given and changes no number in it.

The example data is the CDISC pilot `ADSL` that cards ships. The ARD
summarises age, tabulates age group and sex, and does both by arm.
`ard_stack(.by = )` also counts the subjects in each arm. rtfreporter
reads those counts for the `(N=xx)` in the column header, so nobody
types them.

Show code

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

A few of its rows, one statistic a row:

| group1 | group1_level | variable | variable_level | stat_name | stat  |
|:-------|:-------------|:---------|:---------------|:----------|:------|
| ARM    | Placebo      | AGE      |                | N         | 86    |
| ARM    | Placebo      | AGE      |                | mean      | 75.2  |
| ARM    | Placebo      | AGE      |                | sd        | 8.59  |
| ARM    | Placebo      | SEX      | F              | n         | 53    |
| ARM    | Placebo      | SEX      | F              | p         | 0.616 |
| ARM    | Placebo      | SEX      | M              | n         | 33    |
| ARM    | Placebo      | SEX      | M              | p         | 0.384 |

## 1. From the ARD straight to a table

The plan starts from the ARD.
[`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
names the **roles**: the key that goes across (`cols`) and the keys that
go down (`rows`). Each `plan_*()` verb then adds one declaration;
nothing runs until the plan is used.

Show code

``` r

p <- ard |>
  normalize_ard() |>
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
  plan_col_header(values = list(n = TRUE), rtf_col_header(
    c("",               "{col}"),
    c("Characteristic", "(N={n})"))) |>
  plan_stub(name = "row_label") |>
  plan_blanks(where = "between_groups", first = TRUE) |>
  plan_columns(widths = c(4, 2, 2, 2)) |>
  plan_style(border = "tfl", align_count_pct = TRUE)
```

What each verb says, and the names a plan can use: step 1 of [Tables
from an
ARD](https://ichirio.github.io/rtfreporter/articles/tables-from-ard.html#step-1-from-the-ard-straight-to-a-table).

## 2. The report: `rtf_document()` to `rtf_tables()`

The report is the document around the table. The running header and
footer are small tables of rows, each with a left, centre and right
cell: the study and the page number, then the table’s number, title and
population at the top; its footnotes, then the program and the run date
and time at the bottom. `{PAGE}` / `{TOTAL_PAGES}`, `{PROGRAM_FULL}`
(the program’s full path) and `{DATETIME}` are filled in when the file
is written; `{STUDY}` is a token of our own, given once to
`rtf_document(tokens = )`.

Show code

``` r

doc <- rtf_document(
    program = "/studies/XYZ-001/programs/tfl/t_dm.R",
    tokens  = list(STUDY = "XYZ-001")) |>
  rtf_section(
    header = rtf_header(list(
      c(l = "Ichirio Pharma",                      r = "DRAFT"),
      c(l = "Protocol: {STUDY} (CDISC pilot data)", r = "Page {PAGE} of {TOTAL_PAGES}"),
      c(c = ""),
      c(c = "Table 14.1.1"),
      c(c = "Demographic Characteristics"),
      c(c = "<Safety Analysis Set>"))),
    footer = rtf_footer(list(
      c(l = "N = number of subjects in the analysis set; percentages are based on N."),
      c(l = "SD = standard deviation."),
      c(l = "{PROGRAM_FULL}    Generated on: {DATETIME}")))) |>
  rtf_tables(p)
```

[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
takes the plan as it takes any table and runs it. Long tables are paged
by the plan
([`plan_paginate_rows()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)),
and the header repeats on every page.

## 3. The RTF file: `generate_rtfreport()`

``` r

generate_rtfreport(doc, "t_dm.rtf", overwrite = TRUE)
```

## More tables from the same steps

Each of these is one ARD and one table’s code in [Tables from an
ARD](https://ichirio.github.io/rtfreporter/articles/tables-from-ard.md);
the verbs named are the lines that differ from the table above. They use
[pharmaverseadam](https://pharmaverse.github.io/pharmaverseadam/)‘s
safety population, so the arms’ N differ from those of cards’ `ADSL`.

- [**Adverse events by SOC and
  PT**](https://ichirio.github.io/rtfreporter/articles/tables-from-ard.html#adverse-events-a-hierarchy-sorted-by-frequency):
  [`ard_stack_hierarchical()`](https://pharmaverse.github.io/cards/latest-tag/reference/ard_stack_hierarchical.html),
  `normalize_ard(hierarchy = )`,
  [`plan_sort()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  by frequency within each SOC, pages broken between SOCs.

  ![The first page of an adverse-events table: the running header with
  the study and the page number, the title block, “Any TEAE”, then
  Cardiac disorders and its preferred terms, most frequent first, each
  as n (%) by arm with (N=xx) in the column
  header.](../reference/figures/get-started-ae.png)
  The first page of an adverse-events table: the running header with the
  study and the page number, the title block, “Any TEAE”, then Cardiac
  disorders and its preferred terms, most frequent first, each as n (%)
  by arm with (N=xx) in the column header.

- [**Only the common terms, SOCs by
  frequency**](https://ichirio.github.io/rtfreporter/articles/tables-from-ard.html#only-the-common-terms-socs-by-frequency):
  `plan_filter(p >= 0.05)` and `plan_sort("-n@SOC", ...)`.

  ![Adverse events in 5% or more of subjects: Any TEAE, then General
  disorders, Skin and subcutaneous tissue disorders and the other SOCs,
  most frequent first.](../reference/figures/get-started-ae5.png)
  Adverse events in 5% or more of subjects: Any TEAE, then General
  disorders, Skin and subcutaneous tissue disorders and the other SOCs,
  most frequent first.

- [**A Total column and a
  p-value**](https://ichirio.github.io/rtfreporter/articles/tables-from-ard.html#a-total-column-and-a-p-value-plan_total-and-plan_compare):
  [`plan_total()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  and
  [`plan_compare()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md).

  ![Demographics with Placebo, the two Xanomeline arms, a Total column
  and a p-value column; the p-value sits on the first row of each
  characteristic.](../reference/figures/get-started-dm-p.png)
  Demographics with Placebo, the two Xanomeline arms, a Total column and
  a p-value column; the p-value sits on the first row of each
  characteristic.

- [**Subgroups, a section per
  arm**](https://ichirio.github.io/rtfreporter/articles/tables-from-ard.html#subgroups-a-section-per-arm-stack-and-plan_stat_groups):
  `normalize_ard(stack = )`,
  [`plan_stat_groups()`](https://ichirio.github.io/rtfreporter/reference/plan_verbs.md)
  and `plan_paginate_rows(page_by = , heading = )`.

  ![Objective response rate by subgroup for Placebo (N=86): All
  Subjects, Sex and Age Group with N and Responders (%) with the 90% CI
  below.](../reference/figures/get-started-orr.png)
  Objective response rate by subgroup for Placebo (N=86): All Subjects,
  Sex and Age Group with N and Responders (%) with the 90% CI below.

## Where next on this path

- [Tables from an
  ARD](https://ichirio.github.io/rtfreporter/articles/tables-from-ard.md)
  – the same steps in more depth: quartiles, an adverse-events table
  sorted by frequency within a SOC / PT hierarchy and filtered, a Total
  and a p-value column, and subgroups with a section per arm.
- [The plan
  verbs](https://ichirio.github.io/rtfreporter/articles/plan-verbs.md) –
  every `plan_*()` verb, grouped by job, and the rule that a later layer
  wins.
- [`widen_ard()`](https://ichirio.github.io/rtfreporter/reference/widen_ard.md)
  – the ARD half of a plan in one call, giving a data frame now instead
  of a plan to run later.
- [`plan_template()`](https://ichirio.github.io/rtfreporter/reference/plan_template.md)
  – reads an ARD and writes a starting plan to edit.
- `rtfreporter_ai_manual(dir, ard = )` – a folder to give an AI
  assistant: the manual, three complete programs and your ARD’s shape,
  so it writes a plan for the version you have.
- [Rendering, post-processing and
  assembly](https://ichirio.github.io/rtfreporter/articles/output.md) –
  the outputs joined into one deliverable: a cover page, a linked table
  of contents and a bookmark per output, which Word and LibreOffice turn
  into PDF bookmarks when they export it.
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

A data frame carries no labels of its own, so they are given on
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md).
Here the numbers – and the arms’ N in the header – are computed from the
data, not typed in:

Show code

``` r

adsl <- cards::ADSL
n   <- table(adsl$ARM)
age <- tapply(adsl$AGE, adsl$ARM,
              function(x) sprintf("%.1f (%.2f)", mean(x), sd(x)))
df  <- data.frame(Parameter = "Age, mean (SD)", t(as.vector(age)))

doc2 <- rtf_document() |>
  rtf_tables(rtftable(df, border = "tfl", col_header = rtf_col_header(
               c("", sprintf("%s\n(N=%d)", names(n), as.vector(n))))),
             titles = c("Table 1", "A table brought as a data frame"))
generate_rtfreport(doc2, "t_byo.rtf")
```

See [Importing tables with
as_rtftables()](https://ichirio.github.io/rtfreporter/articles/importing-tables.md),
[From gt and
gtsummary](https://ichirio.github.io/rtfreporter/articles/from-gt.md),
[From rtables and
tern](https://ichirio.github.io/rtfreporter/articles/from-rtables.md),
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
