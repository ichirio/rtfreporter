# Assemble every RTF in a folder into one TOC deliverable

Scans `dir` for `.rtf` files (natural-sorted, so `t2` comes before
`t10`) and reads each file's table number and title from its running
header into a **table of contents**: a `data.frame` with one row per
file. With an `output_file` it assembles the deliverable with that table
of contents
([`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md));
without one it only returns the table, to edit (rename labels, fill
`heading` to group entries, change `level`, reorder or drop rows) and
hand to `assemble_rtf(toc = )`.

## Usage

``` r
assemble_folder(
  dir,
  output_file = NULL,
  spec_file = NULL,
  recursive = FALSE,
  toc_title = "Table of Contents",
  toc_leader = "dot",
  toc_page_numbering = "decimal",
  overwrite = FALSE,
  ...
)
```

## Arguments

- dir:

  Directory of `.rtf` files to assemble.

- output_file:

  Path of the assembled `.rtf` to write, or `NULL` (default): return the
  table of contents only.

- spec_file:

  Optional path (`.xlsx` or `.csv`). When given, the generated spec is
  **saved** there (so you can inspect / edit it); when `NULL` (default)
  the spec is kept in memory only.

- recursive:

  Recurse into sub-directories when scanning? Default `FALSE`.

- toc_title, toc_leader, toc_page_numbering, overwrite, ...:

  Passed through to
  [`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md).

## Value

Without `output_file`, the table of contents. With one, invisibly, a
list with `output` (the assembled file) and `spec` (the table of
contents used).

## Details

The table has the columns `order` (the assembly order), `file`, `table`
(the table number read from the header, or `NA`), `heading` (a heading
printed above the entry whenever it changes; `NA` = none), `label` (the
entry text, `"Table N <title>"`), `level` (the entry's indent, default
2) and `pages` (informational).

## See also

[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md).

## Examples

``` r
# two TFL files in a folder, as a study's output
dir <- file.path(tempdir(), "tfl")
dir.create(dir, showWarnings = FALSE)
for (t in c("14.1.1", "14.2.1")) {
  doc <- rtf_document() |>
    rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
    rtf_titles(list(c(paste("Table", t), "Safety Population")))
  generate_rtfreport(doc, file.path(dir, paste0("t", gsub(".", "_", t,
    fixed = TRUE), ".rtf")), overwrite = TRUE)
}
# One call: scan a folder of TFL .rtf files and assemble them, in catalog
# order, into a single deliverable with an auto table of contents.
assemble_folder(dir, tempfile(fileext = ".rtf"), toc_title = "Contents")

# Or look at the table of contents first, edit it, then assemble
spec <- assemble_folder(dir)
spec$heading <- "Demographics and safety"
assemble_rtf(toc = spec, output_file = tempfile(fileext = ".rtf"))
```
