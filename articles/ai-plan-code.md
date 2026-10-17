# Asking an AI chat for a table's plan code

[In
Japanese](https://ichirio.github.io/rtfreporter/articles/ai-plan-code-ja.md)

## 1. What it does

A chat AI writes the R program of **one table**, from the table’s
**shell** and its **ARD**:
`normalize_ard() |> table_plan() |> plan_*()`, the document and the
file. rtfreporter gives it a **kit** of facts first, so it writes only
functions that exist. Nothing is sent by rtfreporter.

## 2. Make the kit

``` r

library(rtfreporter)
ard <- readRDS("output/ard/ard.rds")          # the study ARD
ard <- ard[ard$output_id == "T-14-1-1", ]     # this table's rows
rtfreporter_ai_manual("kit/T-14-1-1", ard = ard, overwrite = TRUE)
```

| File | What it is |
|----|----|
| `00_START_HERE.md` | the request: the program’s three steps and the rules |
| `01_rtfreporter-ai-user-manual.md` | the manual: the functions that exist (§18), a table from an ARD (§17, §17a) |
| `02_` … `04_example_*.R` | programs in that layout: ORR by subgroup, AEs by SOC/PT, demographics |
| `05_ard.csv` | the table’s ARD, normalized (with `ard =`) |
| `06_ard_shape.md` | the ARD’s keys, variables and statistics — no numbers (with `ard =`) |

## 3. What to give the AI

| Give | Never |
|----|----|
| the kit’s files, `00` to `06` | the **ADaM** datasets, or any data with records |
| the **shell** of this table only (its part of the SHELL workbook: rows, columns, cell format, titles, footnotes) | other tables’ shells or ARDs |

**When the ARD may not be sent**: give `06_ard_shape.md` and leave out
`05_ard.csv` — the shape has the names the program needs, not the
numbers.

**One table a chat.**

**When the chat takes no attachment**, join the files into one text and
paste it:

``` r

kit <- "kit/T-14-1-1"
f   <- list.files(kit, full.names = TRUE)
f   <- f[basename(f) != "05_ard.csv"]           # leave out the numbers if you must
txt <- unlist(lapply(f, function(x) c(paste("#####", basename(x)), readLines(x, encoding = "UTF-8"), "")))
writeLines(txt, file.path(kit, "PASTE.md"), useBytes = TRUE)
nchar(paste(txt, collapse = "\n"))              # about 80,000 characters
```

Then paste the table’s shell (as text) under it. Too long for the chat:
keep `00_START_HERE.md`, `06_ard_shape.md`, the one example closest to
your table, and the manual’s §17, §17a and §18 only.

## 4. How to ask

> Write the R program for table T-14-1-1, following 00_START_HERE.md.
> The shell is below. The ARD is `ard` (05_ard.csv, its shape in
> 06_ard_shape.md); `output_path` and `report_name` exist.
>
> *(the shell, pasted)*

## 5. Try the code it returns

``` r

library(rtfreporter)
ard         <- readRDS("output/ard/ard.rds")
ard         <- ard[ard$output_id == "T-14-1-1", ]
output_path <- "output"
report_name <- "T-14-1-1"
source("T-14-1-1.R")                 # the AI's program, saved as a file
```

| What you see | What to do |
|----|----|
| an error naming a function or an argument | paste the error into the same chat: “this error; correct the program” |
| `# QUESTION:` lines at the top | answer them in the chat |
| a row or column missing | check `06_ard_shape.md`: the ARD may not hold it — add it to the ARD, not to the program |
| the numbers | they are the ARD’s; the plan never computes |

`plan_apply(tbl_plan, "table")` shows the table as a data frame before
the RTF.
