# Create an RTF document for pipe composition

Initialize a new RTF document object for building reports with pipes.
Provides sensible defaults for clinical trial reports.

## Usage

``` r
rtf_document(
  font_table = NULL,
  color_table = NULL,
  page = NULL,
  default_format = NULL,
  watermark = NULL,
  program = NULL,
  tokens = NULL,
  program_fallback = NULL
)
```

## Arguments

- font_table:

  Optional font table: a list of font specifications, each a named list
  with a `name` (e.g. `list(list(name = "Arial"))`). The first entry is
  the document's default font. Default: `list(list(name = "Courier"))`
  (a fixed-width font, which keeps clinical columns aligned).

- color_table:

  Optional character vector of `"#RRGGBB"` colours to pre-declare in the
  document's colour table (so they are available by index). Default
  `c("#000000")`. Colours actually used by borders and by `col_spec` /
  `cell_styles` `color` are added automatically, so you only need this
  to declare colours you reference elsewhere. Black and white are
  reserved and added implicitly.

- page:

  The page geometry. Pass an
  [`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)
  object (recommended – its help lists every key with its default), a
  named list with the same keys, or `NULL` (default) to use the option /
  factory defaults (landscape Letter, 0.75" top/bottom and 0.75"
  left/right margins). An omitted key falls back to the corresponding
  `rtfreporter.*` option (see
  [`rtfreporter_options()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_options.md)).

- default_format:

  Document-wide default formatting. Pass an
  [`rtf_default_format()`](https://ichirio.github.io/rtfreporter/reference/rtf_default_format.md)
  object (recommended – its help lists every key with its default), a
  named list with the same keys, or `NULL` (default). Each value is a
  *default* that a per-module setting
  ([`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  /
  [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  /
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  /
  [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md))
  overrides.

- watermark:

  A diagonal word drawn behind the page body on every page: an
  [`rtf_watermark()`](https://ichirio.github.io/rtfreporter/reference/rtf_watermark.md)
  object, a bare string (`watermark = "DRAFT"` uses the defaults), or
  `NULL` (default) for none. A section can override it by passing
  `watermark` in its `rtf_section(secinfo = )` – including
  `watermark = NA` to switch it off for that section alone.

- program:

  The path of the program that writes this document, for the `{PROGRAM}`
  / `{PROGRAM_FULL}` run tokens (see
  [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)):
  the place to say it, once per program.
  `generate_rtfreport(program = )` overrides it for one call. `NULL`
  (default) leaves it to that argument, then
  `getOption("rtfreporter.program")`, then finds it (the file
  [`source()`](https://rdrr.io/r/base/source.html) or `Rscript` runs,
  knitr's document, RStudio's editor; see
  [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)).

- tokens:

  Tokens of one's own for this document's headers, footers, titles and
  footnotes: a named list,
  `list(STUDY = "ABC-123", CUTOFF = "01JUN2026")`, written `{STUDY}`. A
  name is upper case – a letter, then letters, digits or `_` – and not
  one of rtfreporter's own
  ([`rtf_text_tokens()`](https://ichirio.github.io/rtfreporter/reference/rtf_text_tokens.md));
  a value is one string or number. `options(rtfreporter.tokens = )` sets
  them for a session; the document's value wins. Not in column headers:
  those take their values from
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)'s
  `values`. `NULL` (default): none.

- program_fallback:

  The program to name when none is said (`program`, the option) and none
  is found (the file [`source()`](https://rdrr.io/r/base/source.html) or
  `Rscript` runs, knitr's document, RStudio's editor): the last resort,
  e.g. the program a report is to be written to. Said in a message when
  used. `NULL` (default): none.

## Value

An `rtf_document` S3 object: a list with `document` (`font_table` /
`color_table` / `page` / `default_format` / `watermark`), `contents`
(filled by
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
/
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)),
`titles`, `footnotes`, and `sections` (filled by
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)).

## Details

`rtf_document()` is the **constructor**: it starts a new, empty document
and supplies a default for anything you do not specify. To **change
settings on a document you have already composed** – one that already
holds content and sections – use
[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
instead, which alters only the keys you pass and leaves the content
untouched. In short: `rtf_document()` *builds a new* document from
defaults;
[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
*edits an existing* one in place. The two are complementary, not
interchangeable – you cannot change the page of an already-composed
report with `rtf_document()` without discarding its content.

## See also

[`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)
/
[`rtf_default_format()`](https://ichirio.github.io/rtfreporter/reference/rtf_default_format.md)
for the page / formatting settings,
[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
to edit an already-composed document,
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
/
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
to add content,
[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)
for headers / footers, and
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
to render.

## Examples

``` r
# 1. Simplest: every default (landscape Letter, Courier 9 pt).
doc <- rtf_document()

# 2. A fully specified document, built from the rtf_page() /
#    rtf_default_format() constructors (whose own help shows every default):
doc <- rtf_document(
  font_table     = list(list(name = "Arial")),
  color_table    = c("#000000", "#1F4E79"),
  page           = rtf_page(paper_size = "A4", orientation = "portrait",
                            margin_left_in = 0.75, margin_right_in = 0.75),
  default_format = rtf_default_format(font_size_half_points = 20L,  # 10 pt
                                      row_height_twips = 240L)
)

# ... then add content and render:
df <- data.frame(Parameter = c("Age, Mean (SD)", "Sex, n (%)"),
                 Value = c("75.1 (8.2)", "120 (53%)"))
doc <- rtf_tables(doc, as_rtftables(df), titles = list("Table 14.1.1"))
f <- tempfile(fileext = ".rtf")
generate_rtfreport(doc, f, overwrite = TRUE)
unlink(f)
```
