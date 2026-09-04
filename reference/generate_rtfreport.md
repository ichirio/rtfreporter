# Generate an RTF file from a report object

Renders an `rtf_document` (from the pipe API) or internal `rtfreport`
object to an RTF file.

## Usage

``` r
generate_rtfreport(report, file_path, overwrite = FALSE)
```

## Arguments

- report:

  An `rtf_document` object (from
  [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md))
  or an internal `rtfreport` object.

- file_path:

  Output RTF file path.

- overwrite:

  Logical; whether to overwrite an existing file. Default `FALSE`.

## Value

Invisibly returns `file_path`.

## See also

[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
/
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
to compose the report, and
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
to concatenate several rendered files into one deliverable.

## Examples

``` r
df  <- data.frame(Parameter = "Age, Mean (SD)", Value = "75.1 (8.2)")
doc <- rtf_document() |>
  rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
  rtf_tables(as_rtftables(df), titles = list("Table 14.1.1"))

out <- tempfile(fileext = ".rtf")     # write to a temporary file
generate_rtfreport(doc, out, overwrite = TRUE)
file.exists(out)
#> [1] TRUE
```
