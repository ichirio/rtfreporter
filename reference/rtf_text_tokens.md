# The tokens a page's text may carry

The `{TOKEN}`s that
[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)
fills in headers, footers, titles and footnotes, with what each becomes
and when. A program that offers them – an "insert" menu, a preview –
reads them here.

## Usage

``` r
rtf_text_tokens(doc = NULL)
```

## Arguments

- doc:

  An
  [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
  whose tokens of one's own are listed too; `NULL` (default): the
  session's only.

## Value

A data frame: `token` (as written, with its braces), `kind` (`"page"`,
`"run"` or `"own"`), `when`, `description`, `example` (what it might
print, for a preview).

## Details

- `when = "render"`: filled when the file is written
  ([`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)).

- `when = "viewer"`: an RTF field the word processor computes when the
  file is opened, so it stays right after
  [`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
  joins files.

- `when = "assemble"`: a slot left empty until
  [`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)
  fills it.

`{DATETIME}` also takes a format, `{DATETIME:%Y-%m-%d}`
([`base::strptime()`](https://rdrr.io/r/base/strptime.html) codes);
`example` shows one.

Tokens of one's own (`rtf_document(tokens = )`,
`options(rtfreporter.tokens = )`) follow, `kind = "own"`, their value as
the example: the session's, and a document's when `doc` is given.

## Examples

``` r
rtf_text_tokens()[, c("token", "when", "description")]
#>                 token     when
#> 1              {PAGE}   render
#> 2       {TOTAL_PAGES}   render
#> 3         {AUTO_PAGE}   viewer
#> 4  {AUTO_TOTAL_PAGES}   viewer
#> 5         {BOOK_PAGE} assemble
#> 6           {PROGRAM}   render
#> 7      {PROGRAM_FULL}   render
#> 8      {PROGRAM_NAME}   render
#> 9       {PROGRAM_DIR}   render
#> 10         {DATETIME}   render
#>                                                                   description
#> 1          Page number, written into the file (the first page of the section)
#> 2                             Total pages of this file, written into the file
#> 3           Page number the word processor shows (right after assemble_rtf())
#> 4                  Total pages the word processor counts (the whole document)
#> 5    Page number in the assembled book; empty until assemble_rtf(book_page =)
#> 6                           Path of the program that wrote the file, as given
#> 7                            The same path, absolute (the system's separator)
#> 8                                                   File name of that program
#> 9                                                      Folder of that program
#> 10 Date and time the file was written; {DATETIME:<format>} for another format
```
