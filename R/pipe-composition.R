# Pipe Composition API for rtfreporter
# S3-based immutable composition interface for building RTF reports with pipes (%>%)
#
# This is the primary public API. Build a document by piping rtf_document()
# through rtf_tables(), rtf_section(), format functions, then generate_rtfreport().

# ============================================================================
# Utility: NULL-coalescing operator
# ============================================================================

# NULL-coalescing operator: returns a if not NULL, otherwise b
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================================
# S3 Constructor: rtf_document()
# ============================================================================

#' Create an RTF document for pipe composition
#'
#' Initialize a new RTF document object for building reports with pipes.
#' Provides sensible defaults for clinical trial reports.
#'
#' `rtf_document()` is the **constructor**: it starts a new, empty document and
#' supplies a default for anything you do not specify. To **change settings on a
#' document you have already composed** -- one that already holds content and
#' sections -- use [rtf_config()] instead, which alters only the keys you pass
#' and leaves the content untouched. In short: `rtf_document()` *builds a new*
#' document from defaults; `rtf_config()` *edits an existing* one in place. The
#' two are complementary, not interchangeable -- you cannot change the page of
#' an already-composed report with `rtf_document()` without discarding its
#' content.
#'
#' @param font_table Optional font table: a list of font specifications, each a
#'   named list with a `name` (e.g. `list(list(name = "Arial"))`). The first
#'   entry is the document's default font. Default: `list(list(name =
#'   "Courier"))` (a fixed-width font, which keeps clinical columns aligned).
#'   A `name` may be a named vector of families per script slot (see
#'   *Fonts per script* below).
#' @param font_lang Optional language per font slot, a named character vector
#'   such as `c(east_asian = "ja")`.  Names are `latin`, `east_asian` and
#'   `complex`; languages are `"ja"`, `"zh-Hans"`, `"zh-Hant"`, `"ko"`,
#'   `"ar"`, `"he"` and `"en"`.  It sets the font-table charset of that slot's
#'   fonts (`ja` = 128, `zh-Hans` = 134, `zh-Hant` = 136, `ko` = 129,
#'   `ar` = 178, `he` = 177) and the RTF language words (`\langfe`,
#'   `\lang`, `\alang`, and the document's `\deflangfe` / `\deflang` /
#'   `\adeflang`).  `NULL` (default) is `getOption("rtfreporter.font_lang")`,
#'   itself `NULL`: nothing is written.
#' @param color_table Optional character vector of `"#RRGGBB"` colours to
#'   pre-declare in the document's colour table (so they are available by
#'   index). Default `c("#000000")`. Colours actually used by borders and by
#'   `col_spec` / `cell_styles` `color` are added automatically, so you only
#'   need this to declare colours you reference elsewhere. Black and white are
#'   reserved and added implicitly.
#' @param page The page geometry. Pass an [rtf_page()] object (recommended --
#'   its help lists every key with its default), a named list with the same
#'   keys, or `NULL` (default) to use the option / factory defaults (landscape
#'   Letter, 0.75" top/bottom and 0.75" left/right margins). An omitted key falls
#'   back to the corresponding `rtfreporter.*` option (see [rtfreporter_options()]).
#' @param default_format Document-wide default formatting. Pass an
#'   [rtf_default_format()] object (recommended -- its help lists every key with
#'   its default), a named list with the same keys, or `NULL` (default). Each
#'   value is a *default* that a per-module setting ([rtftable()] /
#'   [rtf_header()] / [rtf_footer()] / [rtf_table_style()]) overrides.
#' @param watermark A diagonal word drawn behind the page body on every page:
#'   an [rtf_watermark()] object, a bare string (`watermark = "DRAFT"` uses the
#'   defaults), or `NULL` (default) for none. A section can override it by
#'   passing `watermark` in its `rtf_section(secinfo = )` -- including
#'   `watermark = NA` to switch it off for that section alone.
#' @param program The path of the program that writes this document, for
#'   the `{PROGRAM}` / `{PROGRAM_FULL}` run tokens (see
#'   [generate_rtfreport()]): the place to say it, once per program.
#'   `generate_rtfreport(program = )` overrides it for one call.  `NULL`
#'   (default) leaves it to that argument, then
#'   `getOption("rtfreporter.program")`, then finds it (the file `source()`
#'   or `Rscript` runs, knitr's document, RStudio's editor; see
#'   [generate_rtfreport()]).
#' @param program_fallback The program to name when none is said
#'   (`program`, the option) and none is found (the file `source()` or
#'   `Rscript` runs, knitr's document, RStudio's editor): the last resort,
#'   e.g. the program a report is to be written to.  Said in a message when
#'   used.  `NULL` (default): none.
#' @param tokens Tokens of one's own for this document's headers, footers,
#'   titles and footnotes: a named list,
#'   `list(STUDY = "ABC-123", CUTOFF = "01JUN2026")`, written `{STUDY}`.
#'   A name is upper case -- a letter, then letters, digits or `_` -- and
#'   not one of rtfreporter's own ([rtf_text_tokens()]); a value is one
#'   string or number.  `options(rtfreporter.tokens = )` sets them for a
#'   session; the document's value wins.  Not in column headers: those take
#'   their values from [set_col_header()]'s `values`.  `NULL` (default):
#'   none.
#' @param file The RTF file the document is written to: one string, a path.
#'   [generate_rtfreport()] writes there when it is given no `file_path`
#'   (one given wins), so a program can say its output where it says its
#'   program.  `NULL` (default): none, and `generate_rtfreport()` needs
#'   `file_path`.
#'
#' @section Fonts per script:
#' Every `font =` (a table, a header/footer band, titles, footnotes, a
#' watermark, `plan_style()`, the `font_table` entries and
#' `options(rtfreporter.font =)`) takes either one family name -- which keeps
#' its old meaning and writes exactly the same RTF -- or a named character
#' vector with a family per script slot:
#'
#' * `latin`: ASCII and the high-ANSI range (Latin, Greek, Cyrillic);
#'   RTF `\loch\f` / `\hich\af`.  Required.
#' * `east_asian`: CJK text; RTF `\dbch\af`.
#' * `complex`: right-to-left and complex scripts; RTF `\rtlch\af`.
#' * `hansi` (optional): overrides `latin` for the high-ANSI range only.
#'
#' A slot an element leaves out takes the document default's family for that
#' slot, else the element's `latin`.  A slotted document default also writes
#' the document-level defaults Word uses for new text (`\adeff`,
#' `\stshfdbch`, `\stshfloch`, `\stshfhich`, `\stshfbi`).  Use
#' `font_lang` to tag a slot with its language, e.g.
#' `rtf_document(font_table = list(list(name = c(latin = "Times New Roman",
#' east_asian = "MS Mincho"))), font_lang = c(east_asian = "ja"))`.
#'
#' @return An `rtf_document` S3 object: a list with `document`
#'   (`font_table` / `color_table` / `page` / `default_format` / `watermark`),
#'   `contents`
#'   (filled by [rtf_tables()] / [rtf_figures()]), `titles`, `footnotes`, and
#'   `sections` (filled by [rtf_section()]).
#'
#' @seealso [rtf_page()] / [rtf_default_format()] for the page / formatting
#'   settings, [rtf_config()] to edit an already-composed document, [rtf_tables()]
#'   / [rtf_figures()] to add content, [rtf_section()] for headers / footers, and
#'   [generate_rtfreport()] to render.
#'
#' @examples
#' # 1. Simplest: every default (landscape Letter, Courier 9 pt).
#' doc <- rtf_document()
#'
#' # 2. A fully specified document, built from the rtf_page() /
#' #    rtf_default_format() constructors (whose own help shows every default):
#' doc <- rtf_document(
#'   font_table     = list(list(name = "Arial")),
#'   color_table    = c("#000000", "#1F4E79"),
#'   page           = rtf_page(paper_size = "A4", orientation = "portrait",
#'                             margin_left_in = 0.75, margin_right_in = 0.75),
#'   default_format = rtf_default_format(font_size_half_points = 20L,  # 10 pt
#'                                       row_height_twips = 240L)
#' )
#'
#' # ... then add content and render:
#' df <- data.frame(Parameter = c("Age, Mean (SD)", "Sex, n (%)"),
#'                  Value = c("75.1 (8.2)", "120 (53%)"))
#' doc <- rtf_tables(doc, as_rtftables(df), titles = list("Table 14.1.1"))
#' f <- tempfile(fileext = ".rtf")
#' generate_rtfreport(doc, f, overwrite = TRUE)
#' unlink(f)
#'
#' @export
rtf_document <- function(font_table = NULL, color_table = NULL, page = NULL,
                         default_format = NULL, watermark = NULL,
                         program = NULL, tokens = NULL,
                         program_fallback = NULL, font_lang = NULL,
                         file = NULL) {
  if (!is.null(program) &&
      (!is.character(program) || length(program) != 1L || is.na(program))) {
    stop("`program` must be a single string: the program's path.",
         call. = FALSE)
  }
  if (!is.null(file) &&
      (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file))) {
    stop("`file` must be a single string: the RTF file's path.",
         call. = FALSE)
  }
  .check_program(program_fallback, "program_fallback")
  .check_user_tokens(tokens)
  .check_page_arg(page)
  # Default clinical trial page used when none is supplied.  A *partial* `page`
  # is kept as given; any key left out (orientation, dimensions, margins) is
  # resolved to its default at render time -- including inferring the
  # orientation from the dimensions when it is omitted (see .orient_page()).
  if (is.null(page)) {
    page <- list(
      paper_size       = .opt("rtfreporter.page.paper_size"),
      orientation      = .opt("rtfreporter.page.orientation"),
      margin_top_in    = .opt("rtfreporter.page.margin_top_in"),
      margin_bottom_in = .opt("rtfreporter.page.margin_bottom_in"),
      margin_left_in   = .opt("rtfreporter.page.margin_left_in"),
      margin_right_in  = .opt("rtfreporter.page.margin_right_in")
    )
  }

  if (is.null(font_table)) {
    font_table <- list(list(name = .check_font(.opt("rtfreporter.font"),
                                               "rtfreporter.font")))
  }
  font_table <- .check_font_table(font_table)
  font_lang  <- .check_font_lang(font_lang %||% .opt("rtfreporter.font_lang"))

  if (is.null(color_table)) {
    color_table <- c("#000000")
  }

  # Create immutable S3 object
  doc <- structure(
    list(
      document = list(
        font_table = font_table,
        font_lang = font_lang,
        color_table = color_table,
        page = page,
        default_format = default_format,
        watermark = .normalize_watermark(watermark),
        program = program,
        program_fallback = program_fallback,
        tokens = tokens,
        file = file
      ),
      contents  = list(),
      titles    = list(),
      footnotes = list(),
      sections  = list()
    ),
    class = "rtf_document"
  )

  doc
}

# `page =` is an rtf_page() or a named list of its keys.  A bare string
# ("a4") used to be kept and failed only later, in print() or the renderer,
# with "$ operator is invalid for atomic vectors" (#594 E1).
.check_page_arg <- function(page) {
  if (is.null(page)) return(invisible(NULL))
  nm <- names(page)
  if (!is.list(page) ||
      (length(page) && (is.null(nm) || any(is.na(nm) | !nzchar(nm))))) {
    hint <- if (is.character(page) && length(page) == 1L) {
      sprintf("  For a paper size write page = rtf_page(paper_size = \"%s\").",
              page)
    } else ""
    stop(paste0("`page` must be an rtf_page() object or a named list of its ",
                "settings (e.g. list(paper_size = \"A4\")), not ",
                .describe_page_arg(page), ".", hint), call. = FALSE)
  }
  invisible(page)
}

.describe_page_arg <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    return(sprintf("the string \"%s\"", x))
  }
  if (is.list(x)) return("an unnamed list")
  sprintf("a %s", class(x)[1L])
}

# ============================================================================
# rtf_config() - Document-level settings
# ============================================================================

#' Configure document-level settings
#'
#' Update the **document-level** settings (font, colour, page, default format)
#' of an existing `rtf_document`, leaving its content, titles and sections
#' untouched. Only the arguments you pass are changed (`NULL` = no change).
#'
#' The point of `rtf_config()` is *deriving variants from an already-composed
#' document*. Build the report once -- add tables/figures and sections -- then
#' produce alternative outputs (a different paper size, font, or default text
#' size) by changing only the relevant setting and rendering each copy. Because
#' an `rtf_document` is immutable, each call returns a fresh copy and the
#' original is left intact, so the variants are independent.
#'
#' `page` and `default_format` are **merged per key**: passing
#' `page = list(width_in = 8.27, height_in = 11.69)` changes only those two keys
#' and keeps the document's existing orientation and margins. `font_table` and
#' `color_table` are replaced as a whole.
#'
#' @param doc An rtf_document object.
#' @param font_table Optional font table; replaces the current one.
#' @param font_lang Optional language per font slot (see [rtf_document()]);
#'   replaces the current one.  `NA` removes it.
#' @param color_table Optional colour table; replaces the current one.
#' @param page New page geometry, **merged per key** onto the current page: an
#'   [rtf_page()] object, or a named list with the same keys (see [rtf_page()]).
#' @param default_format New document-wide default formatting, **merged per key**
#'   onto the current defaults: an [rtf_default_format()] object, or a named list
#'   with the same keys (see [rtf_default_format()]). Each is a document-wide
#'   *default* that any per-module setting overrides.
#' @param watermark New page watermark: an [rtf_watermark()] object, a bare
#'   string, or `NA` to remove the one the document has. `NULL` (default)
#'   leaves it unchanged -- so `NA`, not `NULL`, is how one is cleared.
#'
#' @return Modified rtf_document object (new copy, original unchanged).
#'
#' @seealso [rtf_document()] (the constructor), [rtf_page()] /
#'   [rtf_default_format()] for the settings.
#'
#' @examples
#' # Compose once, then render two paper-size variants from the SAME content.
#' base <- rtf_document() |>
#'   rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
#'   rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL))
#'
#' letter <- base                                   # landscape Letter (default)
#' a4     <- base |> rtf_config(page = rtf_page(paper_size = "A4"))
#'
#' # Only the page changed; the content/section are shared.
#' identical(letter$contents, a4$contents)
#'
#' @export
rtf_config <- function(doc, font_table = NULL, color_table = NULL, page = NULL,
                       default_format = NULL, watermark = NULL,
                       font_lang = NULL) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }

  # Create a copy (immutable pattern)
  doc_copy <- doc

  # Whole-object replacements.
  if (!is.null(font_table)) {
    doc_copy$document$font_table <- .check_font_table(font_table)
  }
  if (!is.null(font_lang)) {
    doc_copy$document["font_lang"] <- list(
      if (identical(font_lang, NA)) NULL else .check_font_lang(font_lang))
  }
  if (!is.null(color_table)) {
    doc_copy$document$color_table <- color_table
  }
  # Per-key merges, so "change only the paper size" keeps the other page keys.
  .check_page_arg(page)
  if (!is.null(page)) {
    doc_copy$document$page <- .merge_list(doc_copy$document$page, page)
  }
  if (!is.null(default_format)) {
    doc_copy$document$default_format <-
      .merge_list(doc_copy$document$default_format, default_format)
  }
  # Whole-object replacement, like font_table.  `NA` clears an existing one --
  # `NULL` cannot, since it is this function's "leave alone" value.
  if (!is.null(watermark)) {
    doc_copy$document$watermark <- .normalize_watermark(watermark)
  }

  doc_copy
}

# ============================================================================
# Content Addition: rtf_tables() and rtf_figures()
# ============================================================================

# The table-format arguments of rtf_tables() that copy rtftable()'s, and
# read_gt: deprecated in 0.8.x (#511 B-1, #594), removed in 0.9.0.
.rtf_tables_fmt_args <- c(
  "col_header", "col_header_align", "spanning_header", "col_spec", "row_title",
  "border", "blank_rows", "read_attributes", "style", "col_rel_width",
  "column_widths_twips", "table_width_twips", "table_width_pct_of_writable",
  "table_width_pct", "table_align", "row_height_twips", "row_height_exact",
  "header_row_height_twips", "blank_row_height_twips", "cell_padding_left_twips",
  "cell_padding_right_twips", "cell_valign", "blank_row_normalize", "markup",
  "font_size_half_points", "font", "read_gt")

# Each one the caller passed says once a session that it is deprecated
.warn_rtf_tables_fmt_args <- function(passed) {
  for (a in intersect(passed, .rtf_tables_fmt_args)) {
    .deprecate_once(
      paste0("rtf_tables_", a),
      if (identical(a, "read_gt")) {
        paste0("`rtf_tables(read_gt = )` is deprecated: pass a gt or gtsummary ",
               "table through as_rtftables(), which reads its titles, labels and ",
               "notes.\n  Removed in 0.9.0.")
      } else {
        paste0("`rtf_tables(", a, " = )` is deprecated: write a table's ",
               "appearance in rtftable(), as_rtftables() or plan_style() (a bare ",
               "data.frame takes their defaults).\n  Removed in 0.9.0.")
      })
  }
  invisible()
}

#' Add content pages to document
#'
#' Append one or more content items as pages. **Each element of `tables`
#' becomes exactly one page**, holding a single table or figure.
#'
#' A table's appearance is written where the table is made: in [rtftable()],
#' [as_rtftables()] or [plan_style()].  A bare `data.frame` placed here gets
#' their defaults.
#'
#' @section Deprecated arguments:
#' The 26 table-format arguments that copy [rtftable()]'s (`col_header`,
#' `border`, `col_rel_width`, `row_height_twips`, `font_size_half_points`,
#' `font`, ... -- see the usage) and `read_gt` are **deprecated** in 0.8.x:
#' each one passed says so once a session and still works as before, and they
#' are removed in 0.9.0, the first CRAN release.  Write the same settings on
#' the table: `rtftable(df, border = "tfl", col_rel_width = c(2, 1))`, or
#' `as_rtftables(x, ...)` for a paged or converted table; pass a gt or
#' gtsummary table through [as_rtftables()] instead of `read_gt`.  Until then
#' they behave as they always did: used to build a bare `data.frame` element,
#' and, when passed explicitly, overriding the matching field of a pre-built
#' `rtftable()` page (`rtfplot()` pages are never modified).
#'
#' @param doc An rtf_document object.
#' @param tables The pages to add. The usual forms are:
#'   \describe{
#'     \item{a `table_plan`}{(from [table_plan()]) the plan of an ARD or a
#'       data.frame; it becomes its pages, as [plan_apply()] would make them.}
#'     \item{the output of [as_rtftables()]}{the pages of a gt, gtsummary,
#'       rtables/tern, rlistings, flextable or huxtable table, or of a
#'       `data.frame` paged and styled there.}
#'     \item{a bare `data.frame`}{one simple page; the table-format arguments
#'       below apply to it.}
#'   }
#'   More generally, `tables` is one page or a list of pages, **each element
#'   one page** with a single content: a `data.frame`, an `rtftable` (from
#'   [rtftable()] or [as_rtftables()]), an `rtfplot` (from [rtfplot()]), or a
#'   `gt_tbl` / gtsummary table. A `gt_tbl` or gtsummary table is best passed
#'   through [as_rtftables()], which reads its labels, alignment, titles and
#'   source notes; placed here directly it is treated like a `data.frame`
#'   unless `read_gt` is set (see `read_gt`).
#'
#'   `tables` may also be a single object of a class another package has
#'   registered an [as_rtftables()] method for; it is converted to its pages
#'   first (see "Supplying a source from another package" in [as_rtftables()]).
#' @param col_header,col_header_align,spanning_header,col_spec,row_title,border,blank_rows,read_attributes,style,col_rel_width,column_widths_twips,table_width_twips,table_width_pct_of_writable,table_width_pct,table_align,row_height_twips,row_height_exact,header_row_height_twips,blank_row_height_twips,cell_padding_left_twips,cell_padding_right_twips,cell_valign,blank_row_normalize,markup,font_size_half_points,font
#'   **Deprecated** (removed in 0.9.0): the same arguments of [rtftable()],
#'   written there (or in [as_rtftables()] / [plan_style()]) instead.  See
#'   "Deprecated arguments" below.
#' @param titles `NULL` (default) or a list of length `length(tables)` **or
#'   length 1** (a single block applied to every page). Each element is a
#'   title **block**: either a character vector (one entry per row, default
#'   styling) or a list of rows, where a row is a string or a styled
#'   `list(text=, align=, bold=, italic=, underline=, color=, border=)`. An
#'   empty string (`""`) is a blank row. The block renders as a single-column
#'   table the same width as the content (so it lines up); title rows default
#'   to centred + bold.
#'
#'   Title and footnote text may carry the same **page-number tokens** as a
#'   header or footer band -- `{AUTO_PAGE}`, `{AUTO_TOTAL_PAGES}`, `{PAGE}`,
#'   `{TOTAL_PAGES}` -- which is how a per-table
#'   "Page 1 of 3" sits next to the table instead of at the page margin. A
#'   static `{PAGE}` costs nothing here: these bands are re-emitted for every
#'   page, so each page bakes its own number without the extra RTF section the
#'   same token forces in a header.
#' @param footnotes `NULL` (default) or a list of length `length(tables)` or
#'   length 1 (common to all). Same block structure as `titles`, and the same
#'   page-number tokens; footnote rows default to left-aligned, and the first
#'   row carries a top rule (the separator) unless that row sets its own
#'   `border`.
#' @param auto_section Logical. When `TRUE` and `tables` is a **named** list,
#'   each name is used as a per-section heading appended to the common header
#'   defined by `rtf_section(secinfo = ...)` (called without a `page` argument).
#'   A section opens where the name **changes**, so a run of pages carrying the
#'   same name -- the pages of one split table, which all share its heading --
#'   is **one** section of several pages. A `"...n"` tail (the serial
#'   [as_rtftables()] adds so a page list with repeated headings stays
#'   addressable by name) is **not** part of the name here: it is stripped
#'   before both the comparison and the printed heading. An item with an empty
#'   name falls through into the open section, which is how pages whose
#'   headings *differ* are put in one section (see [combine_sections()]).
#'   `NULL` (default): what the pages say --- a plan's
#'   `plan_paginate_group(heading_at = "section")` --- else `FALSE`.
#' @param section_label_align Alignment for the auto-appended section label row.
#'   One of `"left"` (default), `"center"`, or `"right"`.
#' @param auto_title Logical. When `TRUE` and `tables` is a **named** list,
#'   each name is appended as the **last row of that table's title block**, so
#'   it prints immediately above the table rather than in the page header.
#'   Titles already present -- from `titles =`, or carried on the object as the
#'   `rtf_titles` attribute by [as_rtftables()] -- are kept and the name goes
#'   after them; a table with no titles gets the name as its whole block.
#'   Unnamed items are untouched.  Behaves identically under either
#'   `title_format`.  Composes with `auto_section`, which puts the same name in
#'   the section header instead.  `NULL` (default): what the pages say --- a
#'   plan's `plan_paginate_group(heading_at = "title")` --- else `FALSE`.
#' @param title_label_align Alignment for the auto-appended title label row.
#'   One of `"left"` (default), `"center"`, or `"right"`.
#' @param read_gt **Deprecated** (removed in 0.9.0): pass a gt or gtsummary
#'   table through [as_rtftables()], which reads its labels, alignment, titles
#'   and source notes, and place the pages it returns here.
#' @param page_break `TRUE` (the default): each table begins a new page.
#'   `FALSE`: a table goes on under the content before it, on the page that
#'   one ends on -- a listing under its summary table.  Nothing is measured:
#'   what does not fit, the reader (Word, LibreOffice) carries to the next
#'   page by itself -- with no title, no column header and no footnote
#'   there; fitting the page is the program's to arrange.  To go on with
#'   the title, column header and footnote on pages of its own, give
#'   `first_rows`.  One value for every table, or one each.  A table's own
#'   later pages (a plan's, or [as_rtftables()]'s) always begin a page; so
#'   does the document's first content.  Unlike reporter's
#'   `add_content(page_break = )` this is the break *before* the table.  A
#'   table that goes on the page before it opens no section of its own
#'   (`auto_section`): `auto_title` names it.  The page header and footer
#'   are the section's; a static `{PAGE}` / `{TOTAL_PAGES}` counts the pages
#'   as written, so where the reader carries a table on, use `{AUTO_PAGE}` /
#'   `{AUTO_TOTAL_PAGES}`.  [rtf_section()]`(page = n)` counts the pages as
#'   written.
#' @param first_rows With `page_break = FALSE`, the rows of the table that
#'   go on the page before it; the rest is paginated as the table says --
#'   a plan's `plan_paginate_rows(max_rows = , split = )` -- each page with
#'   its title, column header and footnote.  The program counts what fits:
#'   rtfreporter does not measure the page.  `tables` is then one table: a
#'   [table_plan()], a data frame (its rest one page; for `max_rows` give
#'   [as_rtftables()]`(first_rows = )` the pages) or an [rtftable()] (cut
#'   there, its rest one page).  `NULL` (the default): the whole table goes
#'   on.
#' @param space_before With `page_break = FALSE`, empty lines between the
#'   content before and this table (0: the title's own blank line alone).
#' @param cont_title With `first_rows`, a line under the title of each page
#'   after the shared one (`"(continued)"`); `NULL`: none.  Not
#'   [as_rtftables()]'s `cont_label`, which marks a group cut across pages.
#'
#' @return Modified rtf_document with appended contents.
#'
#' @examples
#' # Two clinical tables in one document, each on its own page with its own
#' # title; each table carries its own appearance (rtftable()), and a footnote
#' # is attached to the first page only.
#' t1 <- data.frame(Parameter = c("Age (years)", "Sex, n (%)"),
#'                  Value = c("75.1 (8.2)", "120 (53%)"))
#' t2 <- data.frame(Parameter = c("Weight (kg)", "Height (cm)"),
#'                  Value = c("78.0 (12.1)", "170 (9.5)"))
#'
#' doc <- rtf_document() |>
#'   rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
#'   rtf_tables(
#'     list(rtftable(t1, border = "tfl", col_rel_width = c(2, 1)),
#'          rtftable(t2, border = "tfl", col_rel_width = c(2, 1))),
#'     titles    = list("Table 14.1.1", "Table 14.1.2"),
#'     footnotes = list("Source: ADSL", NULL)
#'   )
#' f <- tempfile(fileext = ".rtf")
#' generate_rtfreport(doc, f)
#' unlink(f)
#'
#' @export
rtf_tables <- function(doc, tables,
                        col_header = NULL,
                        col_header_align = NULL,
                        spanning_header = NULL,
                        col_spec = NULL,
                        row_title = NULL,
                        border = "tfl",
                        blank_rows = NULL,
                        read_attributes = TRUE,
                        style = NULL,
                        col_rel_width = NULL,
                        column_widths_twips = NULL,
                        table_width_twips = NULL,
                        table_width_pct_of_writable = NULL,
                        table_width_pct = NULL,
                        table_align = "left",
                        row_height_twips = NULL,
                        row_height_exact = FALSE,
                        header_row_height_twips = NULL,
                        blank_row_height_twips = NULL,
                        cell_padding_left_twips = NULL,
                        cell_padding_right_twips = NULL,
                        cell_valign = "bottom",
                        blank_row_normalize = c("detect", "collapse"),
                        markup = NULL,
                        font_size_half_points = NULL,
                        font = NULL,
                        titles = NULL,
                        footnotes = NULL,
                        auto_section = NULL,
                        section_label_align = "left",
                        auto_title = NULL,
                        title_label_align = "left",
                        read_gt = FALSE,
                        page_break = TRUE,
                        first_rows = NULL,
                        space_before = 0L,
                        cont_title = NULL) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  # The table-format copies (#511 B-1, #594): deprecated, each saying so once
  # a session; removed in 0.9.0.  A table's appearance is written in one
  # place -- rtftable(), as_rtftables() or plan_style() -- and a bare
  # data.frame takes their defaults.
  .warn_rtf_tables_fmt_args(names(match.call())[-1L])

  # A source whose package registered an as_rtftables() method becomes its
  # pages here, so rtf_tables(doc, x) reads like every other source.
  expanded <- .has_rtftables_method(tables)
  # first_rows: the table as pages, the shared page's rows first (R/flow.R)
  if (!is.null(first_rows)) {
    if (all(page_break)) {
      stop("rtf_tables(): `first_rows` is the rows a table puts on the page before it: page_break = FALSE.",
           call. = FALSE)
    }
    tables <- .flow_first_rows(tables, first_rows)
    expanded <- TRUE
  } else if (expanded) {
    tables <- as_rtftables(tables)
  }

  # Auto-wrap a single content item so callers can write rtf_tables(tbl)
  # instead of rtf_tables(list(tbl)).  data.frame is IS a list in R, so it
  # needs an explicit guard; everything else that is not already a plain list
  # (rtftable, rtfplot, gt_tbl, gtsummary) is wrapped too.
  if (is.data.frame(tables) || !is.list(tables) ||
      inherits(tables, "rtftable") || inherits(tables, "rtfplot") ||
      .is_gt_tbl(tables) || .is_gtsummary_tbl(tables)) {
    tables <- list(tables)
  }

  # Where the page names print, when the call does not say: what the pages
  # say (a plan's plan_paginate_group(heading_at = ), carried on each page
  # as `rtf_heading_at`), else nowhere.  An argument given wins.
  if (is.null(auto_section) || is.null(auto_title)) {
    at <- unique(unlist(lapply(tables, attr, "rtf_heading_at", exact = TRUE)))
    if (is.null(auto_section)) auto_section <- identical(at, "section")
    if (is.null(auto_title)) auto_title <- identical(at, "title")
  }

  # Which table-formatting arguments did the caller pass EXPLICITLY?  These
  # override the corresponding fields of any pre-built rtftable item (e.g.
  # the output of as_rtftables()); arguments left at their default do not
  # touch the rtftable's own / gt-derived values.  Detection via match.call()
  # so we can tell "passed border = 'tfl'" from "defaulted to 'tfl'".
  .fmt_args <- c("col_header", "col_header_align", "spanning_header",
                 "col_spec", "row_title", "border", "blank_rows", "style",
                 "col_rel_width", "column_widths_twips", "table_width_twips",
                 "table_width_pct_of_writable", "table_width_pct",
                 "table_align", "row_height_twips", "row_height_exact",
                 "header_row_height_twips", "blank_row_height_twips",
                 "cell_padding_left_twips", "cell_padding_right_twips",
                 "cell_valign", "font_size_half_points", "font")
  .explicit <- intersect(names(match.call())[-1L], .fmt_args)
  .overrides <- if (length(.explicit))
                  mget(.explicit, envir = environment()) else list()

  .is_content_item <- function(x) {
    is.data.frame(x) ||
      inherits(x, "rtftable") ||
      inherits(x, "rtfplot") ||
      .is_gt_tbl(x) ||
      .is_gtsummary_tbl(x)
  }

  # Validate each page-level item: exactly one content per page.
  for (i in seq_along(tables)) {
    item <- tables[[i]]
    if (!.is_content_item(item)) {
      stop("Item ", i, " is a '", paste(class(item), collapse = "/"), "'. ",
           "Each list element is one page: a data.frame, rtftable(), rtfplot(), ",
           "gt_tbl or gtsummary table.\n  A table_plan, or a class whose ",
           "package registered an as_rtftables() method, can be passed as ",
           "`tables` itself.\n  Other tables (rtables/tern, rlistings, ",
           "flextable, huxtable, gt_group) become pages with ",
           "as_rtftables(): rtf_tables(doc, as_rtftables(x)).",
           call. = FALSE)
    }
  }

  # -- gtsummary pre-conversion: gtsummary tables -> gt_tbl ---------------
  # Done before gt extraction so the existing gt pipeline handles them
  # uniformly.  Conversion uses gtsummary::as_gt() (the package's own
  # rendering layer); no gtsummary-internal slots are read directly.
  for (i in seq_along(tables)) {
    if (.is_gtsummary_tbl(tables[[i]])) {
      tables[[i]] <- .gtsummary_to_gt(tables[[i]])
    }
  }

  # -- gt_tbl handling: extract requested attributes ----------------------
  # `read_gt` is normalised once; the resolved token vector is used both
  # for per-table extraction and for the page-level title / source-note
  # pull-through done after table promotion.
  gt_tokens   <- .resolve_gt_tokens(read_gt)
  gt_extracts <- vector("list", length(tables))
  for (i in seq_along(tables)) {
    if (.is_gt_tbl(tables[[i]])) {
      gt_extracts[[i]] <- .gt_to_rtftable_kwargs(tables[[i]], tokens = gt_tokens)
      # Replace the gt_tbl in `tables` with its rendered data.frame so
      # the downstream loop treats it like a bare data.frame.
      tables[[i]] <- gt_extracts[[i]]$data
    }
  }

  # Promote bare data.frames to rtftable using the supplied formatting args.
  # NB: iterate over indices (not items) so we can look up the matching
  # gt_extracts slot per page.  We restore the original names() afterwards.
  tables_names <- names(tables)
  tables <- lapply(seq_along(tables), function(i) {
    item <- tables[[i]]
    if (is.data.frame(item)) {
      # If this slot originated from a gt_tbl, merge the extracted
      # col_header / col_spec / column_widths_twips / col_rel_width
      # into the user-supplied arguments (user always wins).
      gtx <- gt_extracts[[i]]
      eff_col_header <- if (!is.null(col_header))    col_header
                       else if (!is.null(gtx) && !is.null(gtx$col_header))
                         gtx$col_header
                       else NULL
      eff_col_spec   <- if (!is.null(gtx) && !is.null(gtx$col_spec))
                         .merge_col_spec(col_spec, gtx$col_spec)
                       else col_spec
      eff_col_widths <- if (!is.null(column_widths_twips))
                         column_widths_twips
                       else if (!is.null(gtx) && !is.null(gtx$column_widths_twips))
                         gtx$column_widths_twips
                       else NULL
      eff_col_rel    <- if (!is.null(col_rel_width))
                         col_rel_width
                       else if (!is.null(gtx) && !is.null(gtx$col_rel_width))
                         gtx$col_rel_width
                       else NULL
      # Phase D: cell_styles from gt extraction (user has no rtf_tables()-level
      # override; cell_styles can only be set on individual rtftable() objects).
      eff_cell_styles <- if (!is.null(gtx)) gtx$cell_styles else NULL
      rtftable(
        data                        = item,
        col_header                  = eff_col_header,
        col_header_align            = col_header_align,
        spanning_header             = spanning_header,
        col_spec                    = eff_col_spec,
        row_title                   = row_title,
        border                      = border,
        blank_rows                  = blank_rows,
        read_attributes             = read_attributes,
        style                       = style,
        col_rel_width               = eff_col_rel,
        column_widths_twips         = eff_col_widths,
        table_width_twips           = table_width_twips,
        table_width_pct_of_writable = table_width_pct_of_writable,
        table_width_pct             = table_width_pct,
        table_align                 = table_align,
        row_height_twips            = row_height_twips,
        row_height_exact            = row_height_exact,
        header_row_height_twips     = header_row_height_twips,
        blank_row_height_twips      = blank_row_height_twips,
        cell_padding_left_twips     = cell_padding_left_twips,
        cell_padding_right_twips    = cell_padding_right_twips,
        cell_valign                 = cell_valign,
        cell_styles                 = eff_cell_styles,
        blank_row_normalize         = blank_row_normalize,
        markup                      = markup
      )
    } else if (inherits(item, "rtftable")) {
      # Pre-built rtftable (e.g. from as_rtftables()): apply only the
      # explicitly-passed rtf_tables() formatting arguments as overrides;
      # everything else keeps the table's own / gt-derived settings.
      .override_rtftable_fields(item, .overrides)
    } else {
      item   # rtfplot or anything else passes through untouched
    }
  })
  names(tables) <- tables_names

  # Captured before the auto_section block below, which rebuilds `tables`
  # with lapply(seq_along(.)) and so drops its names.  auto_title needs
  # them afterwards, and the two options must compose.
  auto_labels <- names(tables)

  # When auto_section = TRUE, wrap each named item in an rtf_auto_section_item
  # sentinel so that .pipe_doc_to_r6_report() can build per-section headers.
  if (isTRUE(auto_section)) {
    tbl_names <- names(tables)
    if (!is.null(tbl_names) && any(nzchar(tbl_names))) {
      # A section opens where the HEADING CHANGES, not at every named element:
      # a page name is a heading, so consecutive pages that carry the same one
      # are one section of several pages.  The uniquifying "...n" tail a page
      # list carries so its names stay addressable is not part of the heading
      # -- it is stripped here, for the comparison and for what is printed.
      # An empty name falls through into the open section, as it always has --
      # which is still how a caller groups pages whose headings differ
      # (combine_sections() blanks them).
      open <- NULL
      tables <- lapply(seq_along(tables), function(i) {
        nm <- .page_name_base(tbl_names[[i]])
        if (!is.null(nm) && !is.na(nm) && nzchar(nm) && !identical(nm, open)) {
          open <<- nm
          structure(
            list(content = tables[[i]], label = nm,
                 label_align = section_label_align),
            class = "rtf_auto_section_item"
          )
        } else {
          tables[[i]]
        }
      })
    }
  }

  # Validate titles / footnotes lengths
  .validate_parallel <- function(x, n, name) {
    if (is.null(x)) return(rep(list(NULL), n))
    # a character vector is one block (its lines), for every page (#594)
    if (is.character(x)) x <- list(x)
    if (!is.list(x)) {
      stop(sprintf("`%s` must be a list (or NULL).", name), call. = FALSE)
    }
    if (length(x) == 1L && n > 1L) return(rep(x, n))  # one block = common to all
    if (length(x) != n) {
      stop(sprintf(paste0("`%s` has %d blocks but there %s %d page%s: give ",
                          "one block per page, or one block (a list of ",
                          "length 1) for every page."),
                   name, length(x), if (n == 1L) "is" else "are", n,
                   if (n == 1L) "" else "s"), call. = FALSE)
    }
    x
  }
  titles    <- .validate_parallel(titles,    length(tables), "titles")
  footnotes <- .validate_parallel(footnotes, length(tables), "footnotes")

  # Pull through page-level title / source-note blocks.  Two sources, in
  # priority order (user-supplied `titles` / `footnotes` always win over
  # both):
  #   1. `rtf_titles` / `rtf_footnotes` attributes on a pre-built rtftable
  #      item -- this is how as_rtftables() carries gt's title / source
  #      notes onto each page.
  #   2. gt_extracts blocks, for a raw gt_tbl handed directly to rtf_tables()
  #      with read_gt = (legacy path).
  for (i in seq_along(tables)) {
    item <- tables[[i]]
    a_titles    <- attr(item, "rtf_titles",    exact = TRUE)
    a_footnotes <- attr(item, "rtf_footnotes", exact = TRUE)
    gtx <- gt_extracts[[i]]
    if (is.null(titles[[i]])) {
      if (!is.null(a_titles))                          titles[[i]] <- a_titles
      else if (!is.null(gtx) && !is.null(gtx$titles_block))
                                                       titles[[i]] <- gtx$titles_block
    }
    if (is.null(footnotes[[i]])) {
      if (!is.null(a_footnotes))                       footnotes[[i]] <- a_footnotes
      else if (!is.null(gtx) && !is.null(gtx$footnotes_block))
                                                       footnotes[[i]] <- gtx$footnotes_block
    }
  }

  # auto_title: the page name as the LAST row of that table's title block,
  # after whatever `titles =` or the `rtf_titles` attribute already put there.
  # Both title formats resolve their rows through .normalize_text_block(),
  # which honours a per-row `align`, so this needs no renderer change.
  if (isTRUE(auto_title) && !is.null(auto_labels) && any(nzchar(auto_labels))) {
    if (!is.character(title_label_align) || length(title_label_align) != 1L ||
        !title_label_align %in% c("left", "center", "right")) {
      stop("`title_label_align` must be \"left\", \"center\", or \"right\".",
           call. = FALSE)
    }
    for (i in seq_along(tables)) {
      nm <- .page_name_base(auto_labels[[i]])   # the heading, without "...n"
      if (is.na(nm) || !nzchar(nm)) next
      existing <- titles[[i]]
      rows <- if (is.null(existing)) list() else if (is.list(existing)) existing
              else as.list(existing)
      titles[[i]] <- c(rows, list(list(text = nm, align = title_label_align)))
    }
  }

  # cont_title: a line under the title of each page after the shared one
  # (first_rows), as auto_title adds the page name
  if (!is.null(cont_title)) {
    if (!is.character(cont_title) || length(cont_title) != 1L || is.na(cont_title)) {
      stop("rtf_tables(): `cont_title` is one line of text, or NULL.", call. = FALSE)
    }
    if (is.null(first_rows)) {
      stop("rtf_tables(): `cont_title` titles the pages after the shared one: give `first_rows`.",
           call. = FALSE)
    }
    for (i in seq_along(tables)[-1L]) {
      existing <- titles[[i]]
      rows <- if (is.null(existing)) list() else if (is.list(existing)) existing
              else as.list(existing)
      titles[[i]] <- c(rows, list(list(text = cont_title, align = "left")))
    }
  }

  # Create copy and append
  doc_copy <- doc
  doc_copy$contents  <- c(doc_copy$contents,  tables)
  doc_copy$titles    <- c(doc_copy$titles,    titles)
  doc_copy$footnotes <- c(doc_copy$footnotes, footnotes)
  # (page_break = FALSE: the pages' placement, beside them -- see R/flow.R)
  .flow_placement_append(doc_copy, length(tables), .flow_item_start(tables, expanded),
                         page_break = page_break, space_before = space_before)
}

#' Add figure content to document
#'
#' Append one or more figures as content pages -- image files (PNG/JPEG) or
#' plot objects. Each figure creates one new page. Display dimensions and
#' alignment apply to every path and plot object in `figures`; elements
#' already constructed via [rtfplot()] keep their own settings.
#'
#' @param doc An rtf_document object.
#' @param figures A figure, or a list of them: a character file path to an
#'   image file (PNG/JPEG), a plot object (a ggplot2 plot, a patchwork, a
#'   grob, a recorded base plot, a function that draws -- see [rtfplot()]),
#'   or a pre-built `rtfplot`.  One figure is given as it is --
#'   `rtf_figures(doc, plot)` -- and several as a list.  A plot object is
#'   drawn with `png_device` (default: the `rtfreporter.png_device` option,
#'   which follows `ggplot2::ggsave()`: ragg when installed,
#'   `grDevices::png()` otherwise); build an [rtfplot()] to choose per figure.
#' @param width_twips Display width in twips of a figure that is not an
#'   `rtfplot` already (a file or a plot object), as [rtfplot()] takes it.
#'   `NULL` (default): the image's native size -- a file at its embedded DPI,
#'   a plot object at its `render_width` -- with no fit to the page; give a
#'   width to shrink a figure wider than the page.
#' @param height_twips Display height in twips, as [rtfplot()] takes it.
#'   `NULL`: the native height, or -- with `width_twips` -- the height the
#'   image's aspect ratio gives.
#' @param align Horizontal alignment for bare paths: `"center"` (default),
#'   `"left"`, or `"right"`.
#' @param titles,footnotes Optional lists of length `length(figures)` or
#'   length 1 (common to all figures). See [rtf_tables()] for the block
#'   structure (character vectors or per-row styled lists).
#' @param page_break `TRUE` (the default): each figure begins a new page.
#'   `FALSE`: a figure goes on under the content before it; nothing is
#'   measured -- one that does not fit what is left of the page, the reader
#'   moves to the next, its title left behind.  One value for every figure,
#'   or one each; the document's first content always begins a page.  As
#'   [rtf_tables()]'s `page_break`.
#' @param space_before With `page_break = FALSE`, empty lines between the
#'   content before and this figure.
#' @param render_width,render_height,render_dpi,png_device How to draw the
#'   **plot objects** in `figures`, as [rtfplot()] takes them: size in inches,
#'   resolution, and PNG device.  `NULL` (default) is [rtfplot()]'s own
#'   default (6.5 x 4.5 in, 300 dpi, the `rtfreporter.png_device` option).  A
#'   file path ignores them (a file has its size already), and a figure given
#'   as an [rtfplot()] keeps the settings it was built with.
#'
#' @return Modified rtf_document with appended figure contents.
#'
#' @examples
#' png_path <- tempfile(fileext = ".png")
#' grDevices::png(png_path, width = 600, height = 400)
#' graphics::plot(1:10, main = "A figure")
#' grDevices::dev.off()
#' doc <- rtf_document() |>
#'   rtf_figures(list(png_path), width_twips = 6000L, align = "center")
#'
#' # one figure, as it is: a path, or a plot object
#' doc <- rtf_document() |> rtf_figures(png_path)
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#'   doc <- rtf_document() |> rtf_figures(p)
#' }
#' }
#'
#' @export
rtf_figures <- function(doc, figures,
                         width_twips = NULL, height_twips = NULL,
                         align = "center",
                         titles = NULL, footnotes = NULL,
                         page_break = TRUE, space_before = 0L,
                         render_width = NULL, render_height = NULL,
                         render_dpi = NULL, png_device = NULL) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  # How to draw a plot object: only what was given, so rtfplot() keeps its
  # own defaults for the rest (and a file path is never handed any).
  draw_args <- Filter(Negate(is.null),
                      list(render_width = render_width,
                           render_height = render_height,
                           render_dpi = render_dpi, png_device = png_device))

  # one figure, given as it is (a ggplot is a list too: it is not a list of
  # figures)
  if (.rtf_one_figure(figures)) figures <- list(figures)
  if (!is.list(figures)) {
    stop("`figures` must be a figure or a list of file paths, plot objects or ",
         "rtfplot() objects",
         call. = FALSE)
  }

  # Validate and promote each element to rtfplot.
  fig_objs <- lapply(seq_along(figures), function(i) {
    fig <- figures[[i]]
    if (inherits(fig, "rtfplot")) {
      return(fig)
    }
    # rtfplot() knows what can be drawn; say which item it was.
    tryCatch(
      do.call(rtfplot, c(list(fig, width_twips = width_twips,
                              height_twips = height_twips, align = align),
                         if (!is.character(fig)) draw_args)),
      error = function(e) {
        stop("Item ", i, ": ", conditionMessage(e), call. = FALSE)
      })
  })

  .validate_parallel <- function(x, n, name) {
    if (is.null(x)) return(rep(list(NULL), n))
    # a character vector is one block (its lines), for every page (#594)
    if (is.character(x)) x <- list(x)
    if (!is.list(x))
      stop(sprintf("`%s` must be a list (or NULL).", name), call. = FALSE)
    if (length(x) == 1L && n > 1L) return(rep(x, n))  # one block = common to all
    if (length(x) != n)
      stop(sprintf(paste0("`%s` has %d blocks but there %s %d figure%s: give ",
                          "one block per figure, or one block for every figure."),
                   name, length(x), if (n == 1L) "is" else "are", n,
                   if (n == 1L) "" else "s"), call. = FALSE)
    x
  }
  titles    <- .validate_parallel(titles,    length(figures), "titles")
  footnotes <- .validate_parallel(footnotes, length(figures), "footnotes")

  doc_copy <- doc
  doc_copy$contents  <- c(doc_copy$contents,  fig_objs)
  doc_copy$titles    <- c(doc_copy$titles,    titles)
  doc_copy$footnotes <- c(doc_copy$footnotes, footnotes)
  # (page_break = FALSE: the pages' placement, beside them -- see R/flow.R;
  # every figure a content of its own)
  .flow_placement_append(doc_copy, length(fig_objs), rep(TRUE, length(fig_objs)),
                         page_break = page_break, space_before = space_before,
                         fn = "rtf_figures")
}

# Is `x` one figure rather than a list of them?  An rtfplot, one path, a
# function that draws, an object that is not a list (ggplot2 4's plots), or
# a plot that is one (ggplot2 3's, a patchwork, a grob, a recorded plot, a
# lattice trellis).
.rtf_one_figure <- function(x) {
  inherits(x, "rtfplot") ||
    (is.character(x) && length(x) == 1L) ||
    is.function(x) ||
    (!is.list(x) && !is.atomic(x) && !is.null(x)) ||
    inherits(x, c("gg", "ggplot", "patchwork", "grob", "gTree", "gtable",
                  "recordedplot", "trellis"))
}

# ============================================================================
# rtf_titles() / rtf_footnotes() -- assign titles / footnotes to pages
# ============================================================================

#' Assign content titles to pages
#'
#' Replace the per-page title list with the supplied values.  `titles` must
#' have length equal to the number of pages already added via [rtf_tables()] /
#' [rtf_figures()], **or length 1** -- a single block is then applied to every
#' page (common title).
#'
#' Each element is a title **block**: either a character vector (one entry per
#' row, default styling) or a list of rows, where a row is a string or a styled
#' `list(text=, align=, bold=, italic=, underline=, color=, border=)`. An empty
#' string (`""`) is a blank row. The block renders as a single-column table the
#' same width as the content; title rows default to centred + bold.
#'
#' @param doc An rtf_document object.
#' @param titles A list of length = number of pages, or length 1 (common).
#' @param font_size_half_points,row_height_twips,markup,align,font Style for
#'   this block, overriding the document default from [rtf_default_format()].
#'   `font` names a family (e.g. `"Arial"`); one not already in the document's
#'   `font_table` is added to it automatically, the way a colour is.
#'   A named vector gives one family per script slot (`latin`,
#'   `east_asian`, `complex`, optional `hansi`), e.g. `c(latin =
#'   "Times New Roman", east_asian = "MS Mincho")`; see [rtf_document()].
#'   Anything left `NULL` is inherited. Font size and row height resolve
#'   **together**: a size given without a height recomputes the height from
#'   that size rather than inheriting one chosen for a different size, and an
#'   explicit height always wins. `align` sets the block's default row
#'   alignment; a per-row `align` still beats it.
#'
#' @return Modified rtf_document.
#'
#' @examples
#' df1 <- data.frame(Parameter = "Age", Value = "75.1")
#' df2 <- data.frame(Parameter = "Sex", Value = "53%")
#' doc <- rtf_document() |>
#'   rtf_tables(list(df1, df2)) |>
#'   rtf_titles(list(
#'     c("Table 14.1.1", "{HALF_BLANK_ROW}", "Safety Population"),
#'     "Table 14.1.2"
#'   ))
#'
#' @export
rtf_titles <- function(doc, titles, font_size_half_points = NULL,
                       row_height_twips = NULL, markup = NULL, font = NULL,
                       align = NULL) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  n <- length(doc$contents)
  if (n == 0L) {
    stop(paste0("Cannot set titles before any content has been added.  ",
                "Call rtf_titles() after rtf_tables() / rtf_figures(): it ",
                "sets the titles of the pages already added (one block per ",
                "page, or one for all).  Or pass `titles =` to rtf_tables() / ",
                "rtf_figures()."), call. = FALSE)
  }
  # a character vector is one block (its lines), for every page (#594)
  if (is.character(titles)) titles <- list(titles)
  if (!is.list(titles)) {
    stop(paste0("`titles` must be a list (one block per page) or a character ",
                "vector (one block for every page)."), call. = FALSE)
  }
  if (length(titles) == 1L && n > 1L) titles <- rep(titles, n)  # common to all
  if (length(titles) != n) {
    stop(sprintf(paste0("`titles` has %d blocks but the document has %d ",
                        "page%s: give one block per page, or one block (a ",
                        "list of length 1) for every page."),
                 length(titles), n, if (n == 1L) "" else "s"), call. = FALSE)
  }
  doc_copy <- doc
  doc_copy$titles <- titles
  st <- .element_style(font_size_half_points, row_height_twips, markup,
                       align, verb = "rtf_titles")
  if (!is.null(font)) st$font <- .check_font(font, "rtf_titles(font)")
  if (length(st)) doc_copy$title_style <- st
  doc_copy
}

#' Assign content footnotes to pages
#'
#' Same shape as [rtf_titles()]: a list with one block per page (or length 1,
#' common to all). Each block is a character vector or a list of rows (a string
#' or `list(text=, align=, bold=, italic=, underline=, color=, border=)`).
#' Footnote rows default to left-aligned, and the first row carries a top rule
#' (the separator) unless that row sets its own `border`. `NULL` per element
#' suppresses the footnote for that page.
#'
#' @param doc An rtf_document object.
#' @param footnotes A list of length = number of pages, or length 1 (common).
#'   Each page's element is a list of rows. A row is a single string, or a
#'   named vector `c(l = , c = , r = )` giving up to three cells positioned
#'   left / centre / right -- the same row model [rtf_footer()] uses. A bare
#'   string lands in whichever slot `align` selects.
#' @param border `NULL` (default) for **no rule** -- unlike the page footer,
#'   the footnote draws none unless asked. An [rtf_border()] puts one on the
#'   first row, e.g. `border = rtf_border(top = TRUE)`.
#' @param font_size_half_points,row_height_twips,markup,align,font Style for
#'   this block, overriding the document default from [rtf_default_format()].
#'   `font` names a family (e.g. `"Arial"`); one not already in the document's
#'   `font_table` is added to it automatically, the way a colour is.
#'   A named vector gives one family per script slot (`latin`,
#'   `east_asian`, `complex`, optional `hansi`), e.g. `c(latin =
#'   "Times New Roman", east_asian = "MS Mincho")`; see [rtf_document()].
#'   Anything left `NULL` is inherited. Font size and row height resolve
#'   **together**: a size given without a height recomputes the height from
#'   that size rather than inheriting one chosen for a different size, and an
#'   explicit height always wins. `align` sets the block's default row
#'   alignment; a per-row `align` still beats it.
#'
#' @return Modified rtf_document.
#'
#' @examples
#' df <- data.frame(A = 1:2, B = c("x", "y"))
#' doc <- rtf_document() |>
#'   rtf_tables(df) |>
#'   rtf_footnotes(list(c("Source: ADaM ADSL")))
#'
#' @export
rtf_footnotes <- function(doc, footnotes, font_size_half_points = NULL,
                          row_height_twips = NULL, markup = NULL, font = NULL,
                          align = NULL, border = NULL) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  n <- length(doc$contents)
  if (n == 0L) {
    stop(paste0("Cannot set footnotes before any content has been added.  ",
                "Call rtf_footnotes() after rtf_tables() / rtf_figures(): it ",
                "sets the footnotes of the pages already added (one block per ",
                "page, or one for all).  Or pass `footnotes =` to rtf_tables() / ",
                "rtf_figures()."), call. = FALSE)
  }
  # a character vector is one block (its lines), for every page (#594)
  if (is.character(footnotes)) footnotes <- list(footnotes)
  if (!is.list(footnotes)) {
    stop(paste0("`footnotes` must be a list (one block per page) or a ",
                "character vector (one block for every page)."), call. = FALSE)
  }
  if (length(footnotes) == 1L && n > 1L) footnotes <- rep(footnotes, n)  # common to all
  if (length(footnotes) != n) {
    stop(sprintf(paste0("`footnotes` has %d blocks but the document has %d ",
                        "page%s: give one block per page, or one block (a ",
                        "list of length 1) for every page."),
                 length(footnotes), n, if (n == 1L) "" else "s"), call. = FALSE)
  }
  doc_copy <- doc
  doc_copy$footnotes <- footnotes
  st <- .element_style(font_size_half_points, row_height_twips, markup,
                       align, verb = "rtf_footnotes")
  if (!is.null(font)) st$font <- .check_font(font, "rtf_footnotes(font)")
  if (!is.null(border)) {
    if (!inherits(border, "rtf_border")) {
      stop("`rtf_footnotes(border = )` must be NULL or an rtf_border() object.",
           call. = FALSE)
    }
    st$border <- border
  }
  if (length(st)) doc_copy$footnote_style <- st
  doc_copy
}

# ============================================================================
# Section Definition: rtf_section()
# ============================================================================

#' Define sections for pages
#'
#' Map page numbers to sections with headers/footers.
#' Pages are automatically numbered based on content order (starting at 1).
#'
#' @param doc An `rtf_document` object.
#' @param page Where this section starts. Pages are auto-numbered from the
#'   content order (starting at 1), so this is a **page number**. A single
#'   integer starts one section at that page; a vector starts several sections
#'   at once (its length must match the number of sections in `secinfo`).
#' @param header,footer The section's running header and footer: an
#'   [rtf_header()] / [rtf_footer()], or `NULL`.  `NULL` means **inherit**: a
#'   section with no header (footer) of its own keeps the previous section's,
#'   so only the first section's `NULL` leaves the pages without one.  For a
#'   blank band after a section that has one, pass an empty one:
#'   `rtf_header("")`.
#' @param secinfo *Superseded* by `header` / `footer` (documented, not
#'   warned): the same as a named list, `list(header = , footer = )`, and the
#'   way to give several sections at once -- a `list` of such lists, one per
#'   entry of `page`.  A name other than `header` / `footer` / `watermark` is
#'   an error (it used to be ignored, the band it meant left out), and so is
#'   a header / footer that is neither an [rtf_header()] / [rtf_footer()],
#'   its rows nor its text.
#'
#' @return The `rtf_document` with the section definition(s) added.
#'
#' @examples
#' df  <- data.frame(Parameter = "Age, Mean (SD)", Value = "75.1 (8.2)")
#' h1  <- rtf_header(c(l = "Table 14.1.1", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"))
#' h2  <- rtf_header(c(l = "Table 14.2.1", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"))
#' ftr <- rtf_footer(c(l = "Confidential"))
#'
#' # One header / footer applied to the whole document:
#' doc <- rtf_document() |>
#'   rtf_tables(list(df, df)) |>
#'   rtf_section(header = h1, footer = ftr)
#'
#' # A second section, with a different header, starting at page 2:
#' doc <- rtf_document() |>
#'   rtf_tables(list(df, df)) |>
#'   rtf_section(page = 1, header = h1, footer = ftr) |>
#'   rtf_section(page = 2, header = h2)        # the footer is inherited
#'
#' @seealso [rtf_header()] / [rtf_footer()] to build the header / footer, and
#'   [rtf_document()] for the document and its `page` geometry.
#' @export
rtf_section <- function(doc, page = NULL, secinfo = NULL, header = NULL,
                        footer = NULL) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  # header = / footer = are the section; secinfo the superseded list form
  if (!is.null(header) || !is.null(footer)) {
    if (!is.null(secinfo)) {
      stop("Give the section as `header =` / `footer =` or as `secinfo =`, not both.",
           call. = FALSE)
    }
    secinfo <- list(header = header, footer = footer)
  } else if (is.null(secinfo)) {
    secinfo <- list(header = NULL, footer = NULL)
  }
  .check_secinfo(secinfo, length(page))

  # Handle page = NULL: store as "_default" template used by auto_section.
  if (is.null(page)) {
    doc_copy <- doc
    doc_copy$sections[["_default"]] <- secinfo
    return(doc_copy)
  }

  # Normalize page to character for list indexing
  page <- as.character(page)

  # Create copy
  doc_copy <- doc

  # Handle single vs. multiple sections
  if (length(page) == 1) {
    # Single section
    if (is.list(secinfo) && !is.null(secinfo$header) || !is.null(secinfo$footer)) {
      # secinfo is a single section
      doc_copy$sections[[page]] <- secinfo
    } else if (is.list(secinfo) && length(secinfo) > 0 &&
               (is.list(secinfo[[1]]) || is.null(secinfo[[1]]))) {
      # secinfo might be a wrapped section or empty list
      doc_copy$sections[[page]] <- secinfo
    } else {
      stop("secinfo must be a list with header/footer fields", call. = FALSE)
    }
  } else {
    # Multiple pages, possibly multiple sections
    if (!is.list(secinfo)) {
      stop("When page is a vector, secinfo must be a list of section objects",
           call. = FALSE)
    }

    # Check if secinfo is a list of sections or a single section
    # If first element is a list (looks like a section), assume list of sections
    if (length(page) > 1 && length(secinfo) > 0) {
      if (is.list(secinfo[[1]]) || is.null(secinfo[[1]])) {
        # Likely list of section objects
        if (length(page) != length(secinfo)) {
          stop("Length of page (", length(page), ") must match length of secinfo (",
               length(secinfo), ")", call. = FALSE)
        }

        for (i in seq_along(page)) {
          doc_copy$sections[[page[i]]] <- secinfo[[i]]
        }
      } else {
        # Single section dict applied to all pages
        for (p in page) {
          doc_copy$sections[[p]] <- secinfo
        }
      }
    }
  }

  doc_copy
}

# A section's list: only `header`, `footer` and `watermark`; a header /
# footer is an rtf_header() / rtf_footer(), its rows (a list, or the text),
# or NULL.  A misspelled name used to be ignored, so the band it meant was
# left out without a word (#594 E3).  Several sections: a list of such
# lists.  A secinfo that is not a list is left to rtf_section()'s own
# messages.
.check_secinfo <- function(secinfo, n_page) {
  one <- function(s, where) {
    if (is.null(s) || !is.list(s) || inherits(s, c("rtf_header", "rtf_footer"))) {
      return(invisible())
    }
    bad <- setdiff(names(s), c("header", "footer", "watermark"))
    if (length(bad) || (length(s) && is.null(names(s)))) {
      stop(sprintf(paste0("%s has %s: a section has `header`, `footer` and ",
                          "`watermark` only (rtf_section(header = , footer = ))."),
                   where, if (length(bad)) paste0("`", bad, "`", collapse = ", ")
                          else "unnamed elements"), call. = FALSE)
    }
    for (k in c("header", "footer")) {
      v <- s[[k]]
      cls <- paste0("rtf_", k)
      if (!is.null(v) && !inherits(v, cls) && !is.list(v) && !is.character(v)) {
        stop(sprintf("%s's `%s` must be an %s() or NULL, not %s.", where, k, cls,
                     paste0("a ", class(v)[1L])), call. = FALSE)
      }
    }
    invisible()
  }
  # a list of sections (unnamed, each a list), or one wrapped section
  several <- length(secinfo) > 0L && is.null(names(secinfo)) &&
    all(vapply(secinfo, function(s) is.null(s) || (is.list(s) &&
      !inherits(s, c("rtf_header", "rtf_footer"))), logical(1L)))
  if (several) {
    for (i in seq_along(secinfo)) one(secinfo[[i]], sprintf("`secinfo[[%d]]`", i))
  } else {
    one(secinfo, "`secinfo`")
  }
  invisible()
}

# ============================================================================
# S3 Methods for printing
# ============================================================================

#' Print an rtf_document object
#'
#' @param x An rtf_document object.
#' @param ... Additional arguments (unused).
#'
#' @return `x`, invisibly. Called for the side effect of printing a one-line
#'   summary (page count, sections defined, page size).
#'
#' @examples
#' print(rtf_document())
#'
#' @export
print.rtf_document <- function(x, ...) {
  cat("rtf_document object\n")
  cat("  Pages:", length(x$contents), "\n")
  cat("  Sections defined:", length(x$sections), "\n")
  # a preset page (the default) has no width / height of its own (#547)
  pg <- x$document$page
  size <- if (!is.null(pg$width_in) || !is.null(pg$height_in)) {
    sprintf("%s x %s inches", pg$width_in %||% "?", pg$height_in %||% "?")
  } else {
    sprintf("%s (%s)", pg$paper_size %||% "letter", pg$orientation %||% "landscape")
  }
  cat("  Document page size:", size, "\n")
  invisible(x)
}
