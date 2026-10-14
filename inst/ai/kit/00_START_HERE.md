# rtfreporter — write the program for one table

You are writing an **R program** that makes one clinical table as an RTF file
with the **rtfreporter** package. Read this file, then the manual
(`01_rtfreporter-ai-user-manual.md`), then the examples. Answer with **one R
script** and nothing else.

## What you have

| File | What it is |
|---|---|
| `00_START_HERE.md` | this request |
| `01_rtfreporter-ai-user-manual.md` | the only functions that exist (§18), and how they fit together (§17 for a table from an ARD) |
| `02_example_orr_subgroups.R` | the canonical program: an ORR table by subgroup, a section per arm, N and ORR (CI) as two columns |
| `03_example_ae_soc_pt.R` | adverse events by system organ class and preferred term |
| `04_example_demographics.R` | demographics with a Total column and a p-value column |
| `05_ard.csv` | the statistics the table prints, one row each (the **normalized** ARD: keys as columns) — when the user supplied the ARD |
| `06_ard_shape.md` | what `05_ard.csv` holds: its keys and their values, its variables, the statistics each one has, and a suggested `cols` / `rows` — when supplied |

The user attaches, beside these: the **shell** of the table (a mock-up as
xlsx, docx or pdf: the rows, the columns, the cell format, the titles and
footnotes), and sometimes the data's code lists.

## The program's layout — exactly this, three steps

Every program you write has the same three steps, in this order, each
under its own comment line. The examples show the layout; copy it.

```r
library(rtfreporter)

# ---- 1. the table: the plan over the ARD -----------------------------------
tbl_plan <- ard |>
  normalize_ard(...) |>
  table_plan(cols = ..., rows = ..., label = ...) |>
  plan_...() |>
  plan_col_header(...)

# ---- 2. the document: section, titles, footnotes ---------------------------
rep <- rtf_document() |>
  rtf_section(header = rtf_header(...), footer = rtf_footer(...)) |>
  rtf_tables(tbl_plan, ...)

# ---- 3. the file ------------------------------------------------------------
generate_rtfreport(rep, file.path(output_path, paste0(report_name, ".rtf")), overwrite = TRUE)
```

`ard`, `output_path` and `report_name` exist before the program runs (the
user provides them); do not read files and do not set paths. If the ARD
must be made in the program, one short block before step 1 builds it with
`cards` / `cardx` and nothing else (see `02_example_orr_subgroups.R`).

## Rules

1. **Only the functions in the manual's §18 exist.** Nothing else — not
   `r2rtf`, not `reporter`, not a plausible `plan_something()` the manual
   does not list. If the shell asks for something no verb can do, say so in
   a comment at the top of the script, and do the rest.
2. **The ARD is the only source of numbers.** The plan places and formats
   what the ARD holds; it never computes. No `mutate()` of a statistic, no
   p-value computed in R and pasted in, no `variable` relabelled. A number
   that is not in `05_ard.csv` cannot be in the table: say so.
3. **Keep the ARD as it is; choose the layout in the plan.** Statistics in
   two columns: `plan_stat_groups()`. A test or a difference as a column:
   `plan_compare()`. Subgroups one under another: `normalize_ard(stack = )`.
   A section per arm with its N: `plan_paginate_group(heading = )`. Rows
   dropped by a rule: `plan_filter()`. A subgroup's own counts beside its
   analysis: `table_plan(show_keys = )`.
4. **Read `06_ard_shape.md` first** when it is there. Its keys are what
   `cols` and `rows` name; its statistics are what the templates may print
   (`{n}`, `{p:.1f%}`, `{estimate:.2f}` …). Write only names that appear
   there. Without it, take the names from the examples and say in a comment
   which ones you assumed.
5. **Match the shell**: its rows in its order (`plan_sort()`,
   `plan_levels()`), its labels (`plan_labels()`), its cell text
   (`plan_cells()` templates, the decimals it shows), its column header
   (`plan_col_header()` with `{n}` for the N in the header), its titles and
   footnotes (step 2), the `{PROGRAM_FULL}` / `{DATETIME}` line when the
   shell has one.
6. **Base R and the native pipe.** No tidyverse in the program unless the
   ARD block needs `dplyr` for the data.
7. **Comments in English, short, one per block.** No prose outside the
   script.

## When you are unsure

Write the script that does what you are sure of, and put the open
questions as `# QUESTION:` comments at the top: which column holds the
subgroup, which statistic the shell's "n (%)" reads, whether the Total
column is in the ARD. The user answers them and runs the script.

## For the person attaching this folder: what to give the AI

| | Input | How | What it improves |
|---|---|---|---|
| **Required** | The ARD for the table | through the function: `rtfreporter_ai_manual(dir, ard = ard)` writes `05_ard.csv` and `06_ard_shape.md` | the names the plan must use; every number comes from it |
| **Required** | The table's shell | attach it | the rows, columns, header and cell formats |
| Nice to have | The exact titles and footnotes, when the shell does not show them, or the TOC row of this table | attach or paste them | titles and footnotes copied, not composed |
| Nice to have | The company's layout conventions: widths, header style, decimals, blank rows, page size | attach them | the house look without edits afterwards |
| Nice to have | One existing program in the house style | attach it | the same document layout (header, footer, tokens) as your other programs |

**One table a chat**: the ARD of one table and its shell. Several tables in
one chat mix their keys.

**Do not give** patient-level data: the ADaM datasets. The plan works on the
ARD, which is already the counts and statistics. An ARD is aggregate, but it
is still the study's results: follow your company's rules on what may be sent
to an AI service. This kit sends nothing by itself. Other tables' ARDs, or
programs that are not the house-style example, do not help.

**When something is missing**: without an ARD the assistant writes the plan
against the names the shell suggests and marks each guessed name in a
comment; run it against the real ARD, or better, write the kit again with
`ard =`. Without titles it writes placeholders and says so. It never computes
a number the ARD does not hold (a p-value, a total); it says the number is
missing, so that you can add it to the ARD.

