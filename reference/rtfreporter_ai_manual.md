# The AI assistant manuals that ship with this package

rtfreporter is too new to be in any chat model's training data: asked
for rtfreporter code, an assistant reaches for `r2rtf`'s verbs or
invents arguments, and flags neither as a guess. The fix is to give it
the facts first, and these two files are those facts, each sized to sit
in one chat session.

## Usage

``` r
rtfreporter_ai_manual(
  dir = NULL,
  ard = NULL,
  which = c("user", "dev"),
  overwrite = FALSE
)
```

## Arguments

- dir:

  A folder to write the manual into (created when it does not exist).
  `NULL` (default): nothing is written, and the installed manual's path
  comes back. For `which = "user"` the folder gets the kit (see *The
  kit*); for `"dev"`, the developer manual alone.

- ard:

  An ARD (a cards ARD, or one
  [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
  has flattened) for the kit's `05_ard.csv` and `06_ard_shape.md`.
  `NULL` (default): `00_START_HERE.md` says how to add them.

- which:

  `"user"` (default) or `"dev"` – see *Which manual*.

- overwrite:

  Replace files of the same names already in `dir`. Default `FALSE`.

## Value

The installed manual's path when `dir` is `NULL`; otherwise the paths
written, invisibly.

## Details

`rtfreporter_ai_manual()` returns the path to the copy **installed with
this package**, so the manual you attach always describes the version
you actually have. The same files are published on the documentation
site, but that copy tracks the development version – when the two
differ, this one is the one that matches your installation.

## Which manual

- `"user"` (default):

  For *using* rtfreporter: the workflow, every
  [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  argument, the four clinical table shapes, headers and page tokens,
  listings, figures, borders, tables from a cards / cardx ARD
  ([`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
  and the `plan_*()` verbs), and the complete export list.

- `"dev"`:

  For working *on* the package: the invariants, the S3 object model, the
  rendering pipeline, the adapter contract, the plan engine, and the
  test / docs / release conventions.

Attach **one** of them, never both: the user manual forbids touching
internals and the developer manual requires it, so an assistant given
both follows neither cleanly.

## The kit

`dir =`: With `dir`, the user manual comes as a **kit**: a flat folder
an assistant reads in order, to write one table's R program from a shell
(the table's mock-up) and an ARD.

- `00_START_HERE.md`:

  The request: what to attach, the three-step layout every program has
  (the plan over the ARD; the document; the file), the rules (only the
  functions in the manual; the numbers are the ARD's; the layout is the
  plan's).

- `01_rtfreporter-ai-user-manual.md`:

  The manual (its §17a is how to write a table's program from a shell
  and an ARD).

- `02_example_orr_subgroups.R`, `03_example_ae_soc_pt.R`,
  `04_example_demographics.R`:

  Programs in that layout: an ORR by subgroup with a section per arm,
  adverse events by SOC and PT, and demographics with a Total and a
  p-value column.

- `05_ard.csv`, `06_ard_shape.md`:

  With `ard`: the ARD as
  [`normalize_ard()`](https://ichirio.github.io/rtfreporter/reference/normalize_ard.md)
  flattens it (every row when there are 2,000 or fewer, else each
  analysis' first rows), and its shape – the keys and their values,
  which keys come alone or together, each variable's statistics, a key's
  own counts, and a starting
  [`table_plan()`](https://ichirio.github.io/rtfreporter/reference/table_plan.md)
  – so the program names what the ARD really has.

Nothing is sent anywhere: the folder is for the user to attach.

## Examples

``` r
# where the manual lives
rtfreporter_ai_manual()
#> [1] "/home/runner/work/_temp/Library/rtfreporter/ai/rtfreporter-ai-user-manual.md"
writeLines(head(readLines(rtfreporter_ai_manual()), 3))
#> # rtfreporter — AI user manual
#> 
#> **This manual documents rtfreporter 0.8.2.9071** (the development

# the kit, with an ARD's shape
if (requireNamespace("cards", quietly = TRUE)) {
  ard <- cards::ard_stack(cards::ADSL, .by = ARM,
                          cards::ard_tabulate(variables = c(SEX, AGEGR1)))
  kit <- rtfreporter_ai_manual(file.path(tempdir(), "kit"), ard = ard, overwrite = TRUE)
  basename(kit)
}
#> [1] "00_START_HERE.md"                 "01_rtfreporter-ai-user-manual.md"
#> [3] "02_example_orr_subgroups.R"       "03_example_ae_soc_pt.R"          
#> [5] "04_example_demographics.R"        "05_ard.csv"                      
#> [7] "06_ard_shape.md"                 
```
