# Assemble every RTF in a folder into one TOC deliverable

One-call wrapper: scans `dir` for `.rtf` files
([`assemble_files()`](https://ichirio.github.io/rtfreporter/reference/assemble_files.md)),
builds the assembly spec by reading each file's header
([`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md)),
optionally writes the spec to disk, and assembles the deliverable with a
Table of Contents
([`assemble_from_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_from_spec.md)).

## Usage

``` r
assemble_folder(
  dir,
  output_file,
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

  Path of the assembled `.rtf` to write.

- spec_file:

  Optional path (`.xlsx` or `.csv`). When given, the generated spec is
  **saved** there (so you can inspect / edit it); when `NULL` (default)
  the spec is kept in memory only.

- recursive:

  Recurse into sub-directories when scanning? Default `FALSE`.

- toc_title, toc_leader, toc_page_numbering, overwrite, ...:

  Passed through to
  [`assemble_from_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_from_spec.md)
  /
  [`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md).

## Value

Invisibly, a list with `output` (the assembled file) and `spec` (the
assembly spec used).

## See also

[`assemble_files()`](https://ichirio.github.io/rtfreporter/reference/assemble_files.md),
[`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md),
[`assemble_from_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_from_spec.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# One call: scan a folder of TFL .rtf files and assemble them, in catalog
# order, into a single deliverable with an auto table of contents.
assemble_folder("output/tfl", "deliverable.rtf", toc_title = "Contents")
} # }
```
