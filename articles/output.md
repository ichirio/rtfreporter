# Rendering, post-processing and assembly

``` r

library(rtfreporter)
```

Once a document is built, three steps turn it into a deliverable:
**render** it to an `.rtf`, optionally **post-process** the file, and –
for a multi-table package – **assemble** several RTFs into one with a
table of contents.

``` r

df <- data.frame(
  Parameter = c("Creatinine clearance", "  Mean", "  SD"),
  Result    = c("", "88 ug/mL", "12 ug/mL"),
  stringsAsFactors = FALSE
)
doc <- rtf_document() |>
  rtf_tables(as_rtftables(df), titles = list(c("Table 14.3.1 Renal Function")))
```

## Rendering – `generate_rtfreport()`

[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
writes the document to a file you can open in Word or LibreOffice. Use
`overwrite = TRUE` to replace an existing file.

``` r

out <- tempfile(fileext = ".rtf")
generate_rtfreport(doc, out, overwrite = TRUE)
file.exists(out)
#> [1] TRUE
```

## Post-processing – `rtf_replace_text()`

Sometimes the rendered file needs a small tweak that you do not want to
push back into the (validated) source data.
[`rtf_replace_text()`](https://ichirio.github.io/rtfreporter/reference/rtf_replace_text.md)
does a find/replace directly on the generated `.rtf`.

A typical case is **units**: the analysis dataset carries an ASCII-safe
token like `ug/mL`, but the report should display `µg/mL`. Rather than
putting a non-ASCII character in the data, render with the ASCII token
and swap it in the finished file. The replacement is the RTF unicode
escape for `µ` (U+00B5 = 181), i.e. `\u181?`:

``` r

final <- tempfile(fileext = ".rtf")
rtf_replace_text(out, "ug/mL", "\\u181?g/mL", output_file = final)

rtf <- paste(readLines(final, warn = FALSE), collapse = "\n")
grepl("\\u181?g/mL", rtf, fixed = TRUE)   # Word/LibreOffice show "µg/mL"
#> [1] TRUE
```

The same approach swaps any ASCII placeholder for a symbol you cannot
(or prefer not to) carry in the source data.
[`rtf_replace_text()`](https://ichirio.github.io/rtfreporter/reference/rtf_replace_text.md)
also supports vectorised targets, fixed or regex matching
(`use_regex = TRUE`), case-insensitive matching, in-place editing (with
an automatic `.bak`) or writing to a separate `output_file`. See
[`?rtf_replace_text`](https://ichirio.github.io/rtfreporter/reference/rtf_replace_text.md).

> Note: rtfreporter already escapes non-ASCII characters you put in the
> data (so `"µg/mL"` in a cell renders correctly too). Post-processing
> is the tool for the “last mile” – when the source must stay ASCII, or
> for a one-off fix to a finished file.

## Assembling several RTFs – `assemble_rtf()`

A clinical deliverable is usually many tables, listings and figures.
Render each to its own `.rtf`, then combine them into a single document
with a table of contents using
[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md).

``` r

make <- function(title, path) {
  d <- data.frame(A = c("1", "2"), B = c("x", "y"), stringsAsFactors = FALSE)
  doc <- rtf_document() |> rtf_tables(as_rtftables(d), titles = list(c(title)))
  generate_rtfreport(doc, path, overwrite = TRUE); path
}
f1 <- make("Table 14.1.1 Demographics",   tempfile(fileext = ".rtf"))
f2 <- make("Table 14.2.1 Adverse Events",  tempfile(fileext = ".rtf"))
```

### Automatic TOC

`toc = "auto"` builds one TOC entry per file, taking each label from the
file’s first title:

``` r

pkg <- tempfile(fileext = ".rtf")
assemble_rtf(c(f1, f2), pkg, toc = "auto", overwrite = TRUE)
file.exists(pkg)
#> [1] TRUE
```

### Manual, multi-level TOC

For sections and explicit labels, pass a list of
[`toc_heading()`](https://ichirio.github.io/rtfreporter/reference/toc_heading.md)
(a section heading) and
[`toc_entry()`](https://ichirio.github.io/rtfreporter/reference/toc_entry.md)
(a linked entry). Each entry creates an RTF **bookmark**, so the TOC is
navigable in the word processor and the entries jump to the right table:

``` r

pkg2 <- tempfile(fileext = ".rtf")
assemble_rtf(
  c(f1, f2), pkg2,
  cover = list(title = "Study XYZ-001", subtitle = "Final Tables", version = "v1.0"),
  toc = list(
    toc_heading("EFFICACY ANALYSES"),
    toc_entry("Table 14.1.1 Demographics",  file = f1),
    toc_heading("SAFETY ANALYSES"),
    toc_entry("Table 14.2.1 Adverse Events", file = f2)
  ),
  toc_page_numbering = "roman",
  overwrite = TRUE
)
file.exists(pkg2)
#> [1] TRUE
```

[`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
also takes a `cover` page and `toc_leader` / `toc_page_numbering`
options; see
[`?assemble_rtf`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md).

### Assembling a folder

To combine every RTF in a directory,
[`assemble_files()`](https://ichirio.github.io/rtfreporter/reference/assemble_files.md)
lists them (by pattern) and
[`assemble_folder()`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md)
renders a spec-driven package; a spec file
([`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md)
/
[`assemble_from_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_from_spec.md))
lets you script the order, headings and labels. See
[`?assemble_files`](https://ichirio.github.io/rtfreporter/reference/assemble_files.md)
and
[`?assemble_folder`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md).

> Note: the assembled file is RTF. The TOC entries are RTF bookmarks for
> word-processor navigation; converting to PDF (and confirming the PDF
> bookmarks) is outside the scope of this article.

That completes the basic workflow: build a document, render it,
post-process if needed, and assemble a package. See
[`?generate_rtfreport`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md),
[`?rtf_replace_text`](https://ichirio.github.io/rtfreporter/reference/rtf_replace_text.md),
and
[`?assemble_rtf`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
for full references.

## Where next

- [The pharmaverse
  catalog](https://ichirio.github.io/rtfreporter/articles/tlg-catalog.md)
  — complete worked deliverables

The four recipes (`?rtfreporter-recipes`) are the same ground covered as
runnable help-page examples: demographics, adverse events, PK and
laboratory, each data-in to RTF-out.
