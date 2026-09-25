# Generate an RTF file from a report object

Renders an `rtf_document` (from the pipe API) or internal `rtfreport`
object to an RTF file.

## Usage

``` r
generate_rtfreport(report, file_path, overwrite = FALSE, program = NULL)
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

- program:

  The path of the program writing the file, for the `{PROGRAM}` tokens
  (see *Run tokens*). `NULL` (default) reads the document's own
  (`rtf_document(program = )`), then `getOption("rtfreporter.program")`,
  then the script `Rscript` is running.

## Value

Invisibly returns `file_path`.

## Run tokens

Beside the page tokens (`{PAGE}`, `{TOTAL_PAGES}`, ...), any header,
footer, title or footnote cell may say which program wrote the file and
when, filled **as the file is written**:

- `{PROGRAM}`:

  the program path, as given;

- `{PROGRAM_NAME}`, `{PROGRAM_DIR}`:

  its file name and its folder;

- `{DATETIME}`:

  the time the file is written, as
  `getOption("rtfreporter.datetime_format", "\%d\%b\%Y \%H:\%M")` in the
  C locale (`25Sep2026 10:05`);

- `{DATETIME:<format>}`:

  the same in another
  [`strftime()`](https://rdrr.io/r/base/strptime.html) format, e.g.
  `{DATETIME:\%Y-\%m-\%dT\%H:\%M}`.

The time is taken once per file, so every page shows the same one;
`options(rtfreporter.render_time = )` fixes it, for output that has to
be reproducible. A `{PROGRAM}` token with no program known is an error.

    footer <- rtf_footer(list(
      c(l = "SD = Standard Deviation."),
      c(l = "{PROGRAM}      Generated on: {DATETIME}")))
    generate_rtfreport(doc, "t_dm.rtf", program = file.path(work_dir, "t_dm.R"))

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
