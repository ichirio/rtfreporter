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
#'   "Courier New"))` (a fixed-width font, which keeps clinical columns aligned).
#'   A `name` may be a named vector of families per script slot (see
#'   *Fonts per script* below).  An empty list, or an entry whose `name` is
#'   not a family name, is an error.
#' @param font_lang Optional language per font slot, a named character vector
#'   such as `c(east_asian = "ja")`.  `NULL` (default) is
#'   `getOption("rtfreporter.font_lang")`, itself `NULL`: nothing is
#'   written.
#'   \describe{
#'     \item{Names}{`latin`, `east_asian` and `complex`.}
#'     \item{Languages}{`"ja"`, `"zh-Hans"`, `"zh-Hant"`, `"ko"`, `"ar"`,
#'       `"he"` and `"en"`.}
#'     \item{What it sets}{The font-table charset of that slot's fonts (`ja`
#'       = 128, `zh-Hans` = 134, `zh-Hant` = 136, `ko` = 129, `ar` = 178,
#'       `he` = 177) and the RTF language words (`\langfe`, `\lang`,
#'       `\alang`, and the document's `\deflangfe` / `\deflang` /
#'       `\adeflang`).}
#'   }
#' @param color_table Optional character vector of `"#RRGGBB"` colours to
#'   pre-declare in the document's colour table (so they are available by
#'   index). Default `c("#000000")`. Colours actually used by borders and by
#'   `col_spec` / `cell_styles` `color` are added automatically, so you only
#'   need this to declare colours you reference elsewhere. Black and white are
#'   reserved and added implicitly.  Another value (`"red"`, `NA`) is an
#'   error.
#' @param page The page geometry. Pass an [rtf_page()] object (recommended --
#'   its help lists every key with its default), a named list with the same
#'   keys, or `NULL` (default) to use the option / factory defaults (landscape
#'   Letter, 0.75" top/bottom and 0.75" left/right margins). An omitted key falls
#'   back to the corresponding `rtfreporter.*` option (see [rtfreporter_options()]).
#' @param default_format **Deprecated**: use `style = rtf_style()`.  An
#'   [rtf_default_format()] object or a named list of its keys still works:
#'   it maps onto the shared level of [rtf_style()], beneath any `style`
#'   given, and warns once a session.
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
#' @param style The document's look, one [rtf_style()] object: a shared
#'   level (font, size, row height, padding, markup, the title and footnote
#'   form) and one section per module (`table`, `titles`, `footnotes`,
#'   `header`, `footer`).  A module's own argument wins over it.  `NULL`
#'   (default): none.
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
#' @return An `rtf_document` S3 object: a list with `document` (the
#'   settings given here: `font_table`, `font_lang`, `color_table`, `page`,
#'   `default_format`, `style`, `watermark`, `program`, `program_fallback`,
#'   `tokens` and `file`),
#'   `contents` (filled by [rtf_tables()] / [rtf_figures()]: one block per
#'   content, each with its own title and footnote; [rtf_layout()] shows how
#'   they fall on the pages), and `sections` (filled by [rtf_section()]).
#'
#' @seealso [rtf_page()] / [rtf_style()] for the page / formatting
#'   settings, [rtf_config()] to edit an already-composed document, [rtf_tables()]
#'   / [rtf_figures()] to add content, [rtf_section()] for headers / footers, and
#'   [generate_rtfreport()] to render.
#'
#' @examples
#' # 1. Simplest: every default (landscape Letter, Courier New 9 pt).
#' doc <- rtf_document()
#'
#' # 2. A fully specified document, built from the rtf_page() /
#' #    rtf_style() constructors:
#' doc <- rtf_document(
#'   font_table  = list(list(name = "Arial")),
#'   color_table = c("#000000", "#1F4E79"),
#'   page        = rtf_page(paper_size = "A4", orientation = "portrait",
#'                          margin_left_in = 0.75, margin_right_in = 0.75),
#'   style       = rtf_style(font_size_half_points = 20L,  # 10 pt
#'                           row_height_twips = 240L)
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
                         style = NULL, file = NULL) {
  if (!is.null(program) &&
      (!is.character(program) || length(program) != 1L || is.na(program) ||
       !nzchar(program))) {
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
  .check_style_arg(style, "rtf_document")
  .check_color_table(color_table)
  .check_font_table_arg(font_table)
  .check_default_format_arg(default_format)
  if (!is.null(default_format)) .warn_default_format()
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
        style = style,
        watermark = .normalize_watermark(watermark),
        program = program,
        program_fallback = program_fallback,
        tokens = tokens,
        file = file
      ),
      contents  = .rtf_contents(),
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
  # A named list's values are checked as rtf_page() checks them: a negative
  # margin used to be written as a negative \margt, a string to stop with
  # "non-numeric argument to binary operator".
  for (k in intersect(names(page), c("width_in", "height_in", "margin_top_in",
                                     "margin_bottom_in", "margin_left_in",
                                     "margin_right_in", "header_dist_in",
                                     "footer_dist_in"))) {
    .chk_pos_in(page[[k]], k)
  }
  ps <- page[["paper_size"]]
  if (!is.null(ps) && (!is.character(ps) || length(ps) != 1L)) {
    stop("`paper_size` must be a single preset name (e.g. \"A4\") or NULL.",
         call. = FALSE)
  }
  o <- page[["orientation"]]
  if (!is.null(o) && !(is.character(o) && length(o) == 1L &&
                       o %in% c("landscape", "portrait"))) {
    stop("`orientation` must be \"landscape\" or \"portrait\".", call. = FALSE)
  }
  invisible(page)
}

# `default_format` (deprecated): an rtf_default_format() or a named list of
# its keys.  A string used to stop with "second argument must be a list".
.check_default_format_arg <- function(x) {
  if (!is.null(x) && !is.list(x)) {
    stop("`default_format` must be an rtf_default_format() object or a ",
         "named list of its settings.", call. = FALSE)
  }
  invisible(x)
}

# `color_table`: "#RRGGBB" colours.  "red" or NA used to be kept and written
# into the colour table as \redNA.
.check_color_table <- function(x) {
  if (is.null(x)) return(invisible(NULL))
  if (!is.character(x) || anyNA(x) || !all(grepl("^#[0-9A-Fa-f]{6}$", x))) {
    stop("`color_table` must be a character vector of 6-digit hex colours ",
         "such as \"#1F4E79\".", call. = FALSE)
  }
  invisible(x)
}

# `font_table`: a list of fonts, each a list with a `name` (or the name
# itself), the first the document's default.  A missing, empty or
# non-string name used to be dropped for Courier New without a word, or
# written as an empty font name.
.check_font_table_arg <- function(x) {
  if (is.null(x)) return(invisible(NULL))
  nms <- if (is.list(x)) lapply(x, function(f) if (is.list(f)) f$name else f)
  if (!length(nms) || !all(vapply(nms, is.character, logical(1L)))) {
    stop("`font_table` must be a non-empty list of fonts, each a list with ",
         "a `name`: a family name, or a named vector of them per script ",
         "slot, e.g. list(list(name = \"Arial\")).", call. = FALSE)
  }
  for (nm in nms) .check_font(nm, "font_table")
  invisible(x)
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
#' `page` and `style` are **merged per key**: passing
#' `page = list(width_in = 8.27, height_in = 11.69)` changes only those two keys
#' and keeps the document's existing orientation and margins, and an
#' [rtf_style()] sets only the keys it gives, over the document's (the later
#' wins). `font_table` and `color_table` are replaced as a whole.
#'
#' @param doc An rtf_document object.
#' @param font_table Optional font table; replaces the current one.
#' @param font_lang Optional language per font slot (see [rtf_document()]);
#'   replaces the current one.  `NA` removes it.
#' @param color_table Optional colour table; replaces the current one.
#' @param page New page geometry, **merged per key** onto the current page: an
#'   [rtf_page()] object, or a named list with the same keys (see [rtf_page()]).
#' @param default_format **Deprecated**: use `style = rtf_style()`.  Still
#'   merged per key onto the current one, and mapped onto the shared level of
#'   [rtf_style()] beneath any `style`; warns once a session.
#' @param style An [rtf_style()], **merged key by key** over the document's
#'   style: a key it sets wins, the others stay.
#' @param watermark New page watermark: an [rtf_watermark()] object, a bare
#'   string, or `NA` to remove the one the document has. `NULL` (default)
#'   leaves it unchanged -- so `NA`, not `NULL`, is how one is cleared.
#'
#' @return Modified rtf_document object (new copy, original unchanged).
#'
#' @seealso [rtf_document()] (the constructor), [rtf_page()] /
#'   [rtf_style()] for the settings.
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
                       font_lang = NULL, style = NULL) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }

  # Create a copy (immutable pattern)
  doc_copy <- doc

  # Whole-object replacements.
  .check_font_table_arg(font_table)
  .check_color_table(color_table)
  .check_default_format_arg(default_format)
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
    .warn_default_format()
    doc_copy$document$default_format <-
      .merge_list(doc_copy$document$default_format, default_format)
  }
  .check_style_arg(style, "rtf_config")
  if (!is.null(style)) {
    doc_copy$document$style <- .merge_style(doc_copy$document$style, style)
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

#' Add content pages to document
#'
#' Append one or more content items as pages. **Each element of `tables`
#' becomes exactly one page**, holding a single table or figure.
#'
#' A table's appearance is written where the table is made: in [rtftable()],
#' [as_rtftables()] or [plan_style()].  A bare `data.frame` placed here gets
#' their defaults.
#'
#' @section Page values:
#' A table cut into pages on a column -- [as_rtftables()]`(page_by = )`, a
#' value split on a named group column (`split = "by_value", group_col = `),
#' a plan's [plan_paginate_group()] -- knows each page's value of that column.
#' A title, footnote, `auto_section` heading, header or footer
#' ([rtf_header()] / [rtf_footer()]) then writes `{<column name>}`
#' (`{PARAM}`, `{SEX}`) and gets that page's value when the file is written:
#' \itemize{
#'   \item a title or footnote takes the value of its own table, so on a page
#'     of several tables (`page_break = FALSE`) each names its own;
#'   \item a header or footer belongs to the page: each column takes the first
#'     table on the page with a value for it (the rule of the `auto_section`
#'     heading); a later table with another value gets a warning.  A band
#'     that uses one is written for each page, as one with a static `{PAGE}`
#'     is;
#'   \item a `{NAME}` the page has no value for is printed as written, as any
#'     unknown token is (a title's `{N}` stays `{N}`); rtfreporter's own tokens
#'     (`{PAGE}`, `{PROGRAM}`, ...) and a token of one's own
#'     (`rtf_document(tokens = )`) of the same name win over a column's.
#' }
#' The values are on each page's `rtf_paginate_meta` as `page_values`
#' (`c(PARAM = "ALT")`), and [rtf_layout()] shows them.  An rtables page from
#' `rtables::paginate_table()` carries its `split_rows_by(page_by = TRUE)`
#' values by their page-title prefix; a gt_group member of tfrmt's
#' `print_to_gt()` the `page_structure()` columns of a `page_plan(note_loc = )`
#' that writes the page note.
#'
#' @param doc An rtf_document object.
#' @param tables The pages to add. The usual forms are:
#'   \describe{
#'     \item{a `table_plan`}{(from [table_plan()]) the plan of an ARD or a
#'       data.frame; it becomes its pages, as [plan_apply()] would make them.}
#'     \item{the output of [as_rtftables()]}{the pages of a gt, gtsummary,
#'       rtables/tern, rlistings, flextable or huxtable table, or of a
#'       `data.frame` paged and styled there.}
#'     \item{a bare `data.frame`}{one simple page with [rtftable()]'s
#'       defaults; write its appearance on [rtftable()] or [as_rtftables()].}
#'   }
#'   More generally, `tables` is one page or a list of pages, **each element
#'   one page** with a single content: a `data.frame`, an `rtftable` (from
#'   [rtftable()] or [as_rtftables()]), an `rtfplot` (from [rtfplot()]), or a
#'   `gt_tbl` / gtsummary table. A `gt_tbl` or gtsummary table goes through
#'   [as_rtftables()], which reads its labels, alignment, titles and source
#'   notes.
#'
#'   `tables` may also be a single object of a class another package has
#'   registered an [as_rtftables()] method for; it is converted to its pages
#'   first (see "Supplying a source from another package" in [as_rtftables()]).
#' @param titles `NULL` (default) or a list of length `length(tables)` **or
#'   length 1** (a single block applied to every page).  Each element is a
#'   title **block**.
#'   \describe{
#'     \item{A block}{Either a character vector (one entry per row, default
#'       styling) or a list of rows, where a row is a string or a styled
#'       `list(text=, align=, bold=, italic=, underline=, color=,
#'       border=)`.  An empty string (`""`) is a blank row.}
#'     \item{Looks}{The block renders as centred paragraphs, or, with
#'       `rtf_style(title_format = "table")`, as a single-column table the
#'       same width as the content (so it lines up); title rows default to
#'       centred + bold.}
#'     \item{Page numbers}{Title and footnote text may carry the same
#'       **page-number tokens** as a header or footer band --
#'       `{AUTO_PAGE}`, `{AUTO_TOTAL_PAGES}`, `{PAGE}`, `{TOTAL_PAGES}` --
#'       which is how a per-table "Page 1 of 3" sits next to the table
#'       instead of at the page margin.  A static `{PAGE}` costs nothing
#'       here: these bands are re-emitted for every page, so each page
#'       bakes its own number without the extra RTF section the same token
#'       forces in a header.}
#'     \item{Page values}{A table paginated on a column fills `{<column
#'       name>}` with its page's value (see *Page values*).}
#'   }
#' @param footnotes `NULL` (default) or a list of length `length(tables)` or
#'   length 1 (common to all), each a block of rows, a row being a string or
#'   a named vector `c(l = , c = , r = )` as in [rtf_footnotes()] (not the
#'   styled row lists of `titles`), with the same page-number tokens; footnote
#'   rows default to left-aligned and draw no rule:
#'   [rtf_footnotes()]`(border = )` puts one on the first row.
#' @param auto_section Logical.  When `TRUE` and `tables` is a **named**
#'   list, each name is used as a per-section heading appended to the common
#'   header defined by `rtf_section(secinfo = ...)` (called without a `page`
#'   argument).  `NULL` (default): what the pages say --- a plan's
#'   `plan_paginate_group(heading_at = "section")` --- else `FALSE`.
#'   \describe{
#'     \item{Where a section opens}{Where the name **changes**, so a run of
#'       pages carrying the same name -- the pages of one split table, which
#'       all share its heading -- is **one** section of several pages.}
#'     \item{The `"...n"` tail}{The serial [as_rtftables()] adds so a page
#'       list with repeated headings stays addressable by name is **not**
#'       part of the name here: it is stripped before both the comparison
#'       and the printed heading.}
#'     \item{An empty name}{An item with an empty name falls through into
#'       the open section, which is how pages whose headings *differ* are
#'       put in one section (see [combine_sections()]).}
#'   }
#' @param section_label_align Alignment for the auto-appended section label row.
#'   One of `"left"` (default), `"center"`, or `"right"`.
#' @param auto_title Logical.  When `TRUE` and `tables` is a **named** list,
#'   each name is appended as the **last row of that table's title block**,
#'   so it prints immediately above the table rather than in the page
#'   header.  `NULL` (default): what the pages say --- a plan's
#'   `plan_paginate_group(heading_at = "title")` --- else `FALSE`.
#'   \describe{
#'     \item{Titles already there}{From `titles =`, or carried on the object
#'       as the `rtf_titles` attribute by [as_rtftables()] -- are kept and
#'       the name goes after them; a table with no titles gets the name as
#'       its whole block.}
#'     \item{Unnamed items}{are untouched.}
#'     \item{With other arguments}{Behaves identically under either
#'       `title_format`.  Composes with `auto_section`, which puts the same
#'       name in the section header instead.}
#'   }
#' @param title_label_align Alignment for the auto-appended title label row.
#'   One of `"left"` (default), `"center"`, or `"right"`.
#' @param page_break `TRUE` (the default): each table begins a new page.
#'   `FALSE`: a table goes on under the content before it, on the page that
#'   one ends on -- a listing under its summary table.  One value for every
#'   table, or one each.
#'   \describe{
#'     \item{Fitting the page}{Nothing is measured: what does not fit, the
#'       reader (Word, LibreOffice) carries to the next page by itself --
#'       with no title, no column header and no footnote there; fitting the
#'       page is the program's to arrange.  To go on with the title, column
#'       header and footnote on pages of its own, give `first_rows`.}
#'     \item{Pages that always start a page}{A table's own later pages (a
#'       plan's, or [as_rtftables()]'s) always begin a page; so does the
#'       document's first content.  Unlike reporter's
#'       `add_content(page_break = )` this is the break *before* the
#'       table.}
#'     \item{Section headings}{A page has one `auto_section` heading: that
#'       of the first table on it that has one; a later table on the page
#'       with another heading gets a warning (`auto_title` names it on its
#'       title), and no page break is added for it.}
#'     \item{Header, footer, page numbers}{The page header and footer are
#'       the section's; a static `{PAGE}` / `{TOTAL_PAGES}` counts the pages
#'       as written, which are right when each page holds what fits it: the
#'       program counts the rows.  [rtf_section()]`(page = n)` counts the
#'       pages as written; [rtf_layout()] shows them.}
#'   }
#' @param first_rows With `page_break = FALSE`, the rows of the table that
#'   go on the page before it.  `NULL` (the default): the whole table goes
#'   on.
#'   \describe{
#'     \item{The rest}{is paginated as the table says -- a plan's
#'       `plan_paginate_rows(max_rows = , split = )` -- each page with its
#'       title, column header and footnote.  The program counts what fits:
#'       rtfreporter does not measure the page.}
#'     \item{`tables`}{is then one table: a [table_plan()], a data frame
#'       (its rest one page; for `max_rows` give
#'       [as_rtftables()]`(first_rows = )` the pages) or an [rtftable()]
#'       (cut there, its rest one page).}
#'   }
#' @param space_before With `page_break = FALSE`, empty lines between the
#'   content before and this table (0: the title's own blank line alone).
#' @param cont_title A line under the title of each later page of a table
#'   (`"(continued)"`): the pages after the shared one of `first_rows`, and
#'   a table's own later pages (a plan's, or [as_rtftables()]'s `max_rows`
#'   pages).  `NULL`: none.  [rtf_titles()] replaces the titles whole,
#'   this line too.  Not [as_rtftables()]'s `cont_label`, which marks a
#'   group cut across pages.
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
                        titles = NULL,
                        footnotes = NULL,
                        auto_section = NULL,
                        section_label_align = "left",
                        auto_title = NULL,
                        title_label_align = "left",
                        page_break = TRUE,
                        first_rows = NULL,
                        space_before = 0L,
                        cont_title = NULL) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  if (!is.null(first_rows) && all(page_break)) {
    stop("rtf_tables(): `first_rows` is the rows a table puts on the page before it: page_break = FALSE.",
         call. = FALSE)
  }

  # The pages, each knowing which table it is a page of (.rtf_content(),
  # R/content.R): a plan, a source with an as_rtftables() method, a bare data
  # frame, the rows first_rows puts on the page before and the rest, or a list
  # of pages.
  tables <- .rtf_content(tables, first_rows = first_rows)

  # Where the page names print, when the call does not say: what the pages
  # say (a plan's plan_paginate_group(heading_at = ), carried on each page
  # as `rtf_heading_at`), else nowhere.  An argument given wins.
  if (is.null(auto_section) || is.null(auto_title)) {
    at <- unique(unlist(lapply(tables, attr, "rtf_heading_at", exact = TRUE)))
    if (is.null(auto_section)) auto_section <- identical(at, "section")
    if (is.null(auto_title)) auto_title <- identical(at, "title")
  }
  page_names <- names(tables)

  # auto_section: the heading a page opens a section with, where the page
  # name CHANGES -- a page name is a heading, so consecutive pages that
  # carry the same one are one section of several pages.  The uniquifying
  # "...n" tail a page list carries so its names stay addressable is not
  # part of the heading: it is stripped, for the comparison and for what is
  # printed.  An empty name falls through into the open section, which is
  # how a caller groups pages whose headings differ (combine_sections()
  # blanks them).  rtf_layout() opens the sections (R/layout.R).
  sections <- rep(list(NULL), length(tables))
  if (isTRUE(auto_section) && !is.null(page_names) && any(nzchar(page_names))) {
    open <- NULL
    for (i in seq_along(tables)) {
      nm <- .page_name_base(page_names[[i]])
      if (!is.null(nm) && !is.na(nm) && nzchar(nm) && !identical(nm, open)) {
        open <- nm
        sections[[i]] <- list(label = nm, align = section_label_align)
      }
    }
  }

  # Validate titles / footnotes lengths
  titles    <- .rtf_block_texts(titles,    length(tables), "titles")
  footnotes <- .rtf_block_texts(footnotes, length(tables), "footnotes")

  # Titles and footnotes the pages carry, where `titles` / `footnotes` give
  # none: the `rtf_titles` / `rtf_footnotes` attributes of a page (how
  # as_rtftables() carries a source's title and notes onto each page).
  # A page that opens an auto section reads them as any other page does.
  for (i in seq_along(tables)) {
    item <- tables[[i]]
    if (is.null(titles[[i]])) {
      titles[i] <- list(attr(item, "rtf_titles", exact = TRUE))
    }
    if (is.null(footnotes[[i]])) {
      footnotes[i] <- list(attr(item, "rtf_footnotes", exact = TRUE))
    }
  }

  # auto_title: the page name as the LAST row of that table's title block,
  # after whatever `titles =` or the `rtf_titles` attribute already put there.
  # Both title formats resolve their rows through .normalize_text_block(),
  # which honours a per-row `align`, so this needs no renderer change.
  if (isTRUE(auto_title) && !is.null(page_names) && any(nzchar(page_names))) {
    if (!is.character(title_label_align) || length(title_label_align) != 1L ||
        !title_label_align %in% c("left", "center", "right")) {
      stop("`title_label_align` must be \"left\", \"center\", or \"right\".",
           call. = FALSE)
    }
    for (i in seq_along(tables)) {
      nm <- .page_name_base(page_names[[i]])   # the heading, without "...n"
      if (is.na(nm) || !nzchar(nm)) next
      # (marked as the page's heading: rtf_titles() keeps it under the
      # titles it sets)
      titles[[i]] <- .title_add_row(titles[[i]],
                                    list(text = nm, align = title_label_align, heading = TRUE))
    }
  }

  # cont_title: a line under the title of a table's later pages, which
  # rtf_layout() adds (R/layout.R) -- those of first_rows and of the
  # table's own pagination alike
  if (!is.null(cont_title) &&
      (!is.character(cont_title) || length(cont_title) != 1L || is.na(cont_title))) {
    stop("rtf_tables(): `cont_title` is one line of text, or NULL.", call. = FALSE)
  }

  # Each page a block, with its title, footnote and placement (R/blocks.R)
  .rtf_add_blocks(doc, tables, titles, footnotes, page_break = page_break,
                  space_before = space_before, cont_title = cont_title,
                  sections = sections)
}

# A title block with one row more at its end: the rows it has (a character
# vector's lines, or its list of rows) and then `row`
.title_add_row <- function(block, row) {
  rows <- if (is.null(block)) list() else if (is.list(block)) block else as.list(block)
  c(rows, list(row))
}

#' Add figure content to document
#'
#' Append one or more figures as content pages -- image files (PNG/JPEG/EMF) or
#' plot objects. Each figure creates one new page. Display dimensions and
#' alignment apply to every path and plot object in `figures`; elements
#' already constructed via [rtfplot()] keep their own settings.
#'
#' @param doc An rtf_document object.
#' @param figures A figure, or a list of them.  One figure is given as it is
#'   -- `rtf_figures(doc, plot)` -- and several as a list.
#'   \describe{
#'     \item{A file}{a character file path to an image file (PNG/JPEG/EMF).}
#'     \item{A plot object}{a ggplot2 plot, a patchwork, a grob, a recorded
#'       base plot, a function that draws -- see [rtfplot()].  It is drawn
#'       with `png_device` (default: the `rtfreporter.png_device` option,
#'       which follows `ggplot2::ggsave()`: ragg when installed,
#'       `grDevices::png()` otherwise).}
#'     \item{A pre-built `rtfplot`}{Build an [rtfplot()] to choose the device
#'       per figure.}
#'   }
#' @param width_twips Display width in twips of a figure that is not an
#'   `rtfplot` already (a file or a plot object), as [rtfplot()] takes it.
#'   `NULL` (default): the image's native size -- a file at its embedded DPI,
#'   a plot object at its `render_width` -- with no fit to the page; give a
#'   width to shrink a figure wider than the page.
#' @param height_twips Display height in twips, as [rtfplot()] takes it.
#'   `NULL`: the native height, or -- with `width_twips` -- the height the
#'   image's aspect ratio gives.
#' @param align Horizontal alignment of each figure made here, from a path or
#'   a plot object: `"center"` (default), `"left"`, or `"right"`. An
#'   [rtfplot()] keeps its own.
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

  # A table (a data frame, an rtftable, a plan, a gt table ...) is not a
  # figure, though .rtf_content() would make it a page: it goes in
  # rtf_tables(), where its titles and sections are read.
  for (i in seq_along(figures)) {
    fig <- figures[[i]]
    if (is.list(fig) && !.rtf_one_figure(fig)) {
      stop("Item ", i, ": a '", paste(class(fig), collapse = "/"), "' is not a ",
           "figure; a table goes in rtf_tables().", call. = FALSE)
    }
  }

  # Each element one figure, made an rtfplot where it is not one
  # (.rtf_content(), R/content.R); an element's error names it.  The list
  # method is called as such: `figures` is a list of figures whatever its
  # class, and its names have never named the pages.
  fig_objs <- .rtf_content.list(unname(figures), width_twips = width_twips,
                                height_twips = height_twips, align = align,
                                draw_args = draw_args)

  titles    <- .rtf_block_texts(titles,    length(figures), "titles", "figure")
  footnotes <- .rtf_block_texts(footnotes, length(figures), "footnotes", "figure")

  # Each figure a block of its own (R/blocks.R)
  .rtf_add_blocks(doc, fig_objs, titles, footnotes,
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
#' Replace the title of every content already added with the supplied
#' values.  `titles` must have length equal to the number of contents (table
#' or figure pages) already added via [rtf_tables()] / [rtf_figures()], **or
#' length 1** -- a single block is then applied to every content (common
#' title).  Each content is a page of its own unless it was added with
#' `page_break = FALSE`; [rtf_layout()] shows them.
#'
#' Each element is a title **block**: either a character vector (one entry per
#' row, default styling) or a list of rows, where a row is a string or a styled
#' `list(text=, align=, bold=, italic=, underline=, color=, border=,
#' row_height_twips=)`. An empty
#' string (`""`), or `NA`, is a blank row. The block renders as centred paragraphs, or,
#' with `rtf_style(title_format = "table")`, as a single-column table the
#' same width as the content; title rows default to centred + bold.  A row's
#' `row_height_twips` is that row's height, over the block's
#' `row_height_twips`; it applies to the table form
#' (`rtf_style(title_format = "table")`), as the block's does.
#'
#' @param doc An rtf_document object.
#' @param titles A list of length = number of contents, or length 1 (common).
#'   A page's heading (`rtf_tables(auto_title = TRUE)`, a plan's
#'   `plan_paginate_group(heading_at = "title")`) is not replaced: it
#'   stays as the block's last row, under the titles given.
#' @param font_size_half_points,row_height_twips,markup,align,font Style for
#'   this block, over the document's [rtf_style()] (its section, then its
#'   shared level).
#'   Anything left `NULL` is inherited.
#'   \describe{
#'     \item{`font`}{names a family (e.g. `"Arial"`); one not already in the
#'       document's `font_table` is added to it automatically, the way a
#'       colour is.  A named vector gives one family per script slot
#'       (`latin`, `east_asian`, `complex`, optional `hansi`), e.g.
#'       `c(latin = "Times New Roman", east_asian = "MS Mincho")`; see
#'       [rtf_document()].}
#'     \item{`font_size_half_points`, `row_height_twips`}{resolve
#'       **together**: a size given without a height recomputes the height
#'       from that size rather than inheriting one chosen for a different
#'       size, and an explicit height always wins.}
#'     \item{`align`}{sets the block's default row alignment; a per-row
#'       `align` still beats it.}
#'   }
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
  # a page's heading (plan_paginate_group(heading_at = "title"), the last
  # row of its title block) stays, under the titles given: the titles are
  # the report's, the heading the page's
  for (i in seq_len(n)) {
    old <- doc$contents[[i]]$title
    if (!is.list(old)) next
    kept <- Filter(function(r) is.list(r) && isTRUE(r$heading), old)
    for (r in kept) titles[[i]] <- .title_add_row(titles[[i]], r)
  }
  doc_copy <- doc
  doc_copy$contents <- .rtf_blocks_set(doc$contents, "title", titles)
  # The titles given are the whole titles: rtf_tables(cont_title = )'s
  # line, which the layout adds, goes with the ones they replace (as it
  # always has, when it was written into them).
  doc_copy$contents <- .rtf_blocks_set(doc_copy$contents, "cont_title",
                                       rep(list(NULL), n))
  st <- .element_style(font_size_half_points, row_height_twips, markup,
                       align, verb = "rtf_titles")
  if (!is.null(font)) st$font <- .check_font(font, "rtf_titles(font)")
  if (length(st)) doc_copy$title_style <- st
  doc_copy
}

#' Assign content footnotes to pages
#'
#' Same shape as [rtf_titles()]: a list with one block per content (table or
#' figure page; or length 1, common to all). Each block is a character vector or a list of rows (a string,
#' a named vector `c(l = , c = , r = )`, or the page footer's row list
#' `list(columns = , row_height_twips = )`, whose `row_height_twips` is that
#' row's height over the block's).
#' Footnote rows default to left-aligned and draw no rule unless `border`
#' (or a row's own `border`) asks for one. `NULL` per element
#' suppresses the footnote for that content.
#'
#' @param doc An rtf_document object.
#' @param footnotes A list of length = number of contents, or length 1
#'   (common).  Each content's element is a list of rows. A row is a single string, or a
#'   named vector `c(l = , c = , r = )` giving up to three cells positioned
#'   left / centre / right -- the same row model [rtf_footer()] uses. A bare
#'   string lands in whichever slot `align` selects.
#' @param border `NULL` (default) for **no rule** -- unlike the page footer,
#'   the footnote draws none unless asked. An [rtf_border()] puts one on the
#'   first row, e.g. `border = rtf_border(top = TRUE)`.
#' @param font_size_half_points,row_height_twips,markup,align,font Style for
#'   this block, over the document's [rtf_style()] (its section, then its
#'   shared level).
#'   Anything left `NULL` is inherited.
#'   \describe{
#'     \item{`font`}{names a family (e.g. `"Arial"`); one not already in the
#'       document's `font_table` is added to it automatically, the way a
#'       colour is.  A named vector gives one family per script slot
#'       (`latin`, `east_asian`, `complex`, optional `hansi`), e.g.
#'       `c(latin = "Times New Roman", east_asian = "MS Mincho")`; see
#'       [rtf_document()].}
#'     \item{`font_size_half_points`, `row_height_twips`}{resolve
#'       **together**: a size given without a height recomputes the height
#'       from that size rather than inheriting one chosen for a different
#'       size, and an explicit height always wins.}
#'     \item{`align`}{sets the block's default row alignment; a per-row
#'       `align` still beats it.}
#'   }
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
  doc_copy$contents <- .rtf_blocks_set(doc$contents, "footnote", footnotes)
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
    if (is.list(secinfo) && (!is.null(secinfo$header) || !is.null(secinfo$footer) ||
                             "watermark" %in% names(secinfo))) {
      # secinfo is a single section (a watermark alone is one too:
      # list(watermark = NA) switches the document's off for the section)
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
#' @return `x`, invisibly. Called for the side effect of printing the
#'   document's layout, as [rtf_layout()] makes it: the number of contents
#'   (blocks) and printed pages, the page and font size, and one line per
#'   content -- its page, the section a page begins, its name, the rows of a
#'   table or the size of a figure, and the rows of its title and footnote.
#'
#' @examples
#' print(rtf_document())
#' rtf_document() |>
#'   rtf_tables(list(Demog = data.frame(A = 1:3)), titles = "Table 1")
#'
#' @export
print.rtf_document <- function(x, ...) {
  .rtf_pages_print(.rtf_layout(x, warn = FALSE), "rtf_document")
  invisible(x)
}
