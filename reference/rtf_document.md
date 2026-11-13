# Create an RTF document for pipe composition

Initialize a new RTF document object for building reports with pipes.
Provides sensible defaults for clinical trial reports.

## Usage

``` r
rtf_document(
  font_table = NULL,
  color_table = NULL,
  page = NULL,
  watermark = NULL,
  program = NULL,
  tokens = NULL,
  program_fallback = NULL,
  font_lang = NULL,
  style = NULL,
  file = NULL
)
```

## Arguments

- font_table:

  Optional font table: a list of font specifications, each a named list
  with a `name` (e.g. `list(list(name = "Arial"))`). The first entry is
  the document's default font. Default:
  `list(list(name = "Courier New"))` (a fixed-width font, which keeps
  clinical columns aligned). A `name` may be a named vector of families
  per script slot (see *Fonts per script* below). An empty list, or an
  entry whose `name` is not a family name, is an error.

- color_table:

  Optional character vector of `"#RRGGBB"` colours to pre-declare in the
  document's colour table (so they are available by index). Default
  `c("#000000")`. Colours actually used by borders and by `col_spec` /
  `cell_styles` `color` are added automatically, so you only need this
  to declare colours you reference elsewhere. Black and white are
  reserved and added implicitly. Another value (`"red"`, `NA`) is an
  error.

- page:

  The page geometry. Pass an
  [`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)
  object (recommended – its help lists every key with its default), a
  named list with the same keys, or `NULL` (default) to use the option /
  factory defaults (landscape Letter, 0.75" top/bottom and 0.75"
  left/right margins). An omitted key falls back to the corresponding
  `rtfreporter.*` option (see
  [`rtfreporter_options()`](https://ichirio.github.io/rtfreporter/reference/rtfreporter_options.md)).

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
  them for a session; the document's value wins. A value is written as
  it is: a `{PAGE}` or `{STUDY}` in it is not filled. `{{` and `}}`
  print a literal `{` and `}` in a header, footer, title or footnote, as
  in a column header. Not in column headers: those take their values
  from
  [`set_col_header()`](https://ichirio.github.io/rtfreporter/reference/set_col_header.md)'s
  `values`; nor in a table's cells, which print a token as written.
  `NULL` (default): none.

- program_fallback:

  The program to name when none is said (`program`, the option) and none
  is found (the file [`source()`](https://rdrr.io/r/base/source.html) or
  `Rscript` runs, knitr's document, RStudio's editor): the last resort,
  e.g. the program a report is to be written to. Said in a message when
  used. `NULL` (default): none.

- font_lang:

  Optional language per font slot, a named character vector such as
  `c(east_asian = "ja")`. `NULL` (default) is
  `getOption("rtfreporter.font_lang")`, itself `NULL`: nothing is
  written.

  Names

  :   `latin`, `east_asian` and `complex`.

  Languages

  :   `"ja"`, `"zh-Hans"`, `"zh-Hant"`, `"ko"`, `"ar"`, `"he"` and
      `"en"`.

  What it sets

  :   The font-table charset of that slot's fonts (`ja` = 128, `zh-Hans`
      = 134, `zh-Hant` = 136, `ko` = 129, `ar` = 178, `he` = 177) and
      the RTF language words (`\langfe`, `\lang`, `\alang`, and the
      document's `\deflangfe` / `\deflang` / `\adeflang`).

- style:

  The document's look, one
  [`rtf_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_style.md)
  object: a shared level (font, size, row height, padding, markup, the
  title and footnote form) and one section per module (`table`,
  `titles`, `footnotes`, `header`, `footer`). A module's own argument
  wins over it. `NULL` (default): none.

- file:

  The RTF file the document is written to: one string, a path.
  [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
  writes there when it is given no `file_path` (one given wins), so a
  program can say its output where it says its program. `NULL`
  (default): none, and
  [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
  needs `file_path`.

## Value

An `rtf_document` S3 object: a list with `document` (the settings given
here: `font_table`, `font_lang`, `color_table`, `page`, `style`,
`watermark`, `program`, `program_fallback`, `tokens` and `file`),
`contents` (filled by
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
/
[`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md):
one block per content, each with its own title and footnote;
[`rtf_layout()`](https://ichirio.github.io/rtfreporter/reference/rtf_layout.md)
shows how they fall on the pages), and `sections` (filled by
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

## Fonts per script

Every `font =` (a table, a header/footer band, titles, footnotes, a
watermark,
[`plan_style()`](https://ichirio.github.io/rtfreporter/reference/plan_style.md),
the `font_table` entries and `options(rtfreporter.font =)`) takes either
one family name – which keeps its old meaning and writes exactly the
same RTF – or a named character vector with a family per script slot:

- `latin`: ASCII and the high-ANSI range (Latin, Greek, Cyrillic); RTF
  `\loch\f` / `\hich\af`. Required.

- `east_asian`: CJK text; RTF `\dbch\af`.

- `complex`: right-to-left and complex scripts; RTF `\rtlch\af`.

- `hansi` (optional): overrides `latin` for the high-ANSI range only.

A slot an element leaves out takes the document default's family for
that slot, else the element's `latin`. A slotted document default also
writes the document-level defaults Word uses for new text (`\adeff`,
`\stshfdbch`, `\stshfloch`, `\stshfhich`, `\stshfbi`). Use `font_lang`
to tag a slot with its language, e.g.
`rtf_document(font_table = list(list(name = c(latin = "Times New Roman", east_asian = "MS Mincho"))), font_lang = c(east_asian = "ja"))`.

## Font names that are not ASCII

A family name with characters outside ASCII (Yu Mincho written in
Japanese, say) is written in the font table as Word writes it, so that
Word reads the same family for the Latin and the East Asian text: in the
bytes of its East Asian code page, each as `\'xx`, with the matching
`\fcharset` – Japanese 128 (code page 932), Simplified Chinese 134
(936), Traditional Chinese 136 (950), Korean 129 (949). The character
set is:

- a common family's own: Yu Mincho, Yu Gothic, MS Mincho, MS Gothic, MS
  PMincho, MS PGothic and Meiryo are Japanese; SimSun and SimHei
  Simplified Chinese; PMingLiU and Microsoft JhengHei Traditional
  Chinese; Malgun Gothic and Batang Korean. Their Latin name follows in
  `{\*\falt Yu Mincho}`.

- else Japanese for a name with kana, Korean for one with Hangul;

- else (Han characters only) the `east_asian` language of `font_lang`
  when it is one of these and writes the name, then the first of
  Japanese, Simplified Chinese, Traditional Chinese and Korean that
  writes it.

A slot tagged by `font_lang` keeps its language's character set when
that code page writes the name. A name none of these code pages can
write keeps the `\uN?` form, and an ASCII name is written as it always
was.

## See also

[`rtf_page()`](https://ichirio.github.io/rtfreporter/reference/rtf_page.md)
/
[`rtf_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_style.md)
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
# 1. Simplest: every default (landscape Letter, Courier New 9 pt).
doc <- rtf_document()

# 2. A fully specified document, built from the rtf_page() /
#    rtf_style() constructors:
doc <- rtf_document(
  font_table  = list(list(name = "Arial")),
  color_table = c("#000000", "#1F4E79"),
  page        = rtf_page(paper_size = "A4", orientation = "portrait",
                         margin_left_in = 0.75, margin_right_in = 0.75),
  style       = rtf_style(font_size_half_points = 20L,  # 10 pt
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
