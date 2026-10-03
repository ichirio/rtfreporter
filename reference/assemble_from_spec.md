# Assemble RTF files from an assembly spec

Runs
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
with a Table of Contents built from an assembly spec (a `data.frame`
from
[`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md),
or the path to a saved `.xlsx` / `.csv` spec). Rows are ordered by the
spec's `order` column when present.

## Usage

``` r
assemble_from_spec(
  spec,
  output_file,
  toc_title = "Table of Contents",
  toc_leader = "dot",
  toc_page_numbering = "decimal",
  overwrite = FALSE,
  ...
)
```

## Arguments

- spec:

  An assembly-spec `data.frame`, or a path to a `.xlsx` / `.csv` spec
  file.

- output_file:

  Path of the assembled `.rtf` to write.

- toc_title, toc_leader, toc_page_numbering, overwrite, ...:

  Passed to
  [`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md).

## Value

Invisibly, `output_file`.

## See also

[`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md),
[`assemble_folder()`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md),
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
spec <- assemble_spec(dir)          # review / edit the order
assemble_from_spec(spec, tempfile(fileext = ".rtf"),
                   toc_title = "Table of Contents")
```
