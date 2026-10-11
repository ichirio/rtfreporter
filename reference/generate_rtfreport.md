# Generate an RTF file from a report object

Renders an `rtf_document` (from the pipe API) or internal `rtfreport`
object to an RTF file.

## Usage

``` r
generate_rtfreport(
  report,
  file_path,
  overwrite = FALSE,
  program = NULL,
  program_fallback = NULL
)
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

  Overrides, for this one file, the program the document names
  (`rtf_document(program = )`, where a program says it once), for the
  `{PROGRAM}` tokens (see *Run tokens*). `NULL` (default) uses the
  document's own, then `getOption("rtfreporter.program")`, then finds it
  (see *Run tokens*).

- program_fallback:

  Overrides, for this one file, the document's
  `rtf_document(program_fallback = )`: the program to name when none is
  said and none is found.

## Value

Invisibly returns `file_path`.

## Run tokens

Beside the page tokens (`{PAGE}`, `{TOTAL_PAGES}`, ...), any header,
footer, title or footnote cell may say which program wrote the file and
when, filled **as the file is written**:

- `{PROGRAM}`:

  the program path, as given;

- `{PROGRAM_FULL}`:

  the same path made absolute
  ([`normalizePath()`](https://rdrr.io/r/base/normalizePath.html)), from
  the working folder when the file is written, with the system's
  separator: `\` on Windows, `/` elsewhere;

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

The program is said once, where the document is made –
`rtf_document(program = )` – since one program writes one file;
`generate_rtfreport(program = )` overrides it for a single call;
`options(rtfreporter.program = )` sets one for the session. When none is
said and a `{PROGRAM...}` token is used, it is found, in this order: the
file [`source()`](https://rdrr.io/r/base/source.html) is running (the
innermost), the script `Rscript` runs, the document knitr is knitting,
the file open in RStudio's editor (an interactive session, with
rstudioapi; not an Untitled one).
[`source()`](https://rdrr.io/r/base/source.html) comes before `Rscript`,
so a batch (`Rscript run_all.R` that sources each table's program) names
each table's own program. A program found is said in a message; one said
is not. When nothing is found, `rtf_document(program_fallback = )` is
used – last, after `Rscript`, so a program's own file name always wins –
and said in a message too. For a production run, say it with
`rtf_document(program = )`: a relative path found this way is joined to
the working folder, which [`setwd()`](https://rdrr.io/r/base/getwd.html)
may have moved.

Tokens of one's own – `{STUDY}`, `{CUTOFF}` – come from
`rtf_document(tokens = list(STUDY = "ABC-123"))` and
`options(rtfreporter.tokens = )` (the document's value wins), and are
filled the same way, in headers, footers, titles and footnotes. A column
header takes its values from
[`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)'s
`values` instead.

The file name is completed to the one on disk, for every `{PROGRAM...}`
token: a file that is there gets its real case (`t_dm.R` that is
`T_DM.r` on Windows); a name with no extension becomes the program of
that name in its folder (`.R`, `.r`, `.Rmd`, `.qmd`), else gets `.R`; a
name with an extension that is not there is kept as it is.

    footer <- rtf_footer(list(
      c(l = "SD = Standard Deviation."),
      c(l = "{PROGRAM_FULL}      Generated on: {DATETIME}")))
    doc <- rtf_document(program = "programs/t_dm.R") |>
      rtf_section(page = 1, secinfo = list(footer = footer)) |>
      rtf_tables(tbl)
    generate_rtfreport(doc, "t_dm.rtf")

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
