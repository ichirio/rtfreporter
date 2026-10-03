# Collect the RTF files in a folder

Lists the `.rtf` files in `dir`, in natural-sorted order (so `t2` comes
before `t10`), ready to hand to
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
or the other assembly helpers.

## Usage

``` r
assemble_files(dir, pattern = "[.]rtf$", recursive = FALSE, sort = TRUE)
```

## Arguments

- dir:

  Directory to scan.

- pattern:

  File-name pattern (default `"[.]rtf$"`, case-insensitive).

- recursive:

  Recurse into sub-directories? Default `FALSE`.

- sort:

  Natural-sort the result? Default `TRUE`.

## Value

A character vector of file paths.

## See also

[`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md),
[`assemble_toc()`](https://ichirio.github.io/rtfreporter/reference/assemble_toc.md),
[`assemble_folder()`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md).

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
files <- assemble_files(dir)        # every .rtf, in catalog order
```
