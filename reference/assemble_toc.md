# Build a TOC definition from a set of RTF files

Convenience wrapper that reads the files (via
[`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md))
and returns the `toc =` list of
[`toc_heading()`](https://ichirio.github.io/rtfreporter/reference/toc_heading.md)
/
[`toc_entry()`](https://ichirio.github.io/rtfreporter/reference/toc_entry.md)
objects ready for
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md).
Pass a ready-made `spec` to convert that instead.

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

## See also

[`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md),
[`assemble_from_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_from_spec.md).

## Examples

``` r
if (FALSE) { # \dontrun{
toc <- assemble_toc(files = assemble_files("output/tfl"))
assemble_rtf(assemble_files("output/tfl"), "deliverable.rtf", toc = toc)
} # }
```
