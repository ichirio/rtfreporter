# Build a TOC definition from a set of RTF files (deprecated)

**Deprecated** in 0.8.x (warns once a session, still works); removed in
0.9.0. Give
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
the table of contents itself (`toc = ` the table from
[`assemble_folder()`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md)).

## Usage

``` r
assemble_toc(files = NULL, spec = NULL, ...)
```

## Arguments

- files:

  Vector of `.rtf` paths.

- spec:

  Optional assembly spec (from
  [`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md));
  when supplied, `files` is ignored and the spec is converted directly.

- ...:

  Passed to
  [`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md)
  when building from `files`.

## Value

A list suitable for `assemble_rtf(toc = )`.

## Details

Convenience wrapper that reads the files (via
[`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md))
and returns the `toc =` list of
[`toc_heading()`](https://ichirio.github.io/rtfreporter/reference/toc_heading.md)
/
[`toc_entry()`](https://ichirio.github.io/rtfreporter/reference/toc_entry.md)
objects ready for
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md).
Pass a ready-made `spec` to convert that instead.

## See also

[`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md),
[`assemble_from_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_from_spec.md).

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
# instead: the table of contents as the `toc`
assemble_rtf(toc = assemble_folder(dir), output_file = tempfile(fileext = ".rtf"))
```
