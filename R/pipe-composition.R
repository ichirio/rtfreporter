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
#' @param font_table Optional font table. Default: list(list(name = "Courier"))
#' @param color_table Optional color table. Default: c("#000000")
#' @param page Optional page settings (orientation, dimensions, margins).
#'             Default: landscape letter 11x8.5", margins 0.9 inch
#'             (top/bottom) and 0.6 inch (left/right).
#' @param default_format Optional document-wide default formatting.
#'
#' @return An rtf_document object (S3 class) with structure:
#'   - document: list(font_table, color_table, page, default_format)
#'   - contents: list (initially empty, populated by rtf_tables/rtf_figures)
#'   - sections: list (initially empty, populated by rtf_section)
#'
#' @examples
#' \dontrun{
#' doc <- rtf_document()
#' }
#'
#' @export
rtf_document <- function(font_table = NULL, color_table = NULL, page = NULL,
                         default_format = NULL) {
  # Default clinical trial settings
  if (is.null(page)) {
    page <- list(
      orientation = "landscape",
      width_in = 11,
      height_in = 8.5,
      margin_top_in = 0.9,
      margin_bottom_in = 0.9,
      margin_left_in = 0.6,
      margin_right_in = 0.6
    )
  }

  if (is.null(font_table)) {
    font_table <- list(list(name = "Courier"))
  }

  if (is.null(color_table)) {
    color_table <- c("#000000")
  }

  # Create immutable S3 object
  doc <- structure(
    list(
      document = list(
        font_table = font_table,
        color_table = color_table,
        page = page,
        default_format = default_format
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

# ============================================================================
# rtf_config() - Document-level settings
# ============================================================================

#' Configure document-level settings
#'
#' Update font, color, page, or default formatting for the document.
#' This function is typically called once after creating a document.
#' Only non-NULL parameters are updated (NULL = no change).
#'
#' @param doc An rtf_document object.
#' @param font_table Optional font table to replace default.
#' @param color_table Optional color table to replace default.
#' @param page Optional page settings list.
#' @param default_format Optional document-wide default formatting.
#'
#' @return Modified rtf_document object (new copy, original unchanged).
#'
#' @export
rtf_config <- function(doc, font_table = NULL, color_table = NULL, page = NULL,
                       default_format = NULL) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }

  # Create a copy (immutable pattern)
  doc_copy <- doc

  # Update only non-NULL parameters
  if (!is.null(font_table)) {
    doc_copy$document$font_table <- font_table
  }
  if (!is.null(color_table)) {
    doc_copy$document$color_table <- color_table
  }
  if (!is.null(page)) {
    doc_copy$document$page <- page
  }
  if (!is.null(default_format)) {
    doc_copy$document$default_format <- default_format
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
#' Table-formatting arguments (`col_rel_width`, `border`, `row_height_twips`,
#' ...) accepted by this function are used to build any bare `data.frame`
#' element of `tables`, and -- when passed **explicitly** -- also override the
#' matching field of any pre-built `rtftable()` element (for example the
#' output of [as_rtftables()]).  Arguments left at their default are not
#' applied to pre-built tables, so those keep their own / gt-derived settings.
#' `rtfplot()` elements are never modified.  (The `style` argument seeds
#' construction-time defaults only; it is not applied as an override to a
#' pre-built table.)
#'
#' @param doc An rtf_document object.
#' @param tables A list where each element is one page's content. Each
#'   element must be one of:
#'   - `data.frame`: simple table; the table-format arguments below apply.
#'   - `rtftable` object (from `rtftable()`): table with full formatting.
#'   - `rtfplot` object (from `rtfplot()`): embedded figure.
#'   - `gt_tbl` object (from the gt package): converted via
#'     `as.data.frame()` and treated like a `data.frame`. Pass
#'     `read_gt = TRUE` (or a vector of tokens) to also pull through
#'     gt's column labels, alignment, title/subtitle, and source notes.
#'     See `read_gt` below.
#'   - **gtsummary table** (`tbl_summary`, `tbl_regression`, `tbl_merge`,
#'     `tbl_stack`, etc.): automatically converted to a `gt_tbl` via
#'     `gtsummary::as_gt()` before any further processing. Pass
#'     `read_gt = TRUE` to pull through column labels, titles, source
#'     notes, footnotes, spanning headers, and hidden-column removal.
#'     Note: cell-level formatting (row indentation, bold group-header
#'     rows, footnote marks in cells) is **not** transferred to RTF.
#'     See [as_rtftable()] for details on gtsummary limitations.
#' @param col_rel_width,column_widths_twips,table_width_twips,table_width_pct_of_writable,table_width_pct,table_align Column-width and table-width settings applied to bare `data.frame` elements. See [rtftable()] for details.
#' @param col_header,col_header_align,spanning_header,col_spec,border,blank_rows,read_attributes,style Per-table content settings applied to bare `data.frame` elements. See [rtftable()] for details.
#' @param row_height_twips,row_height_exact,header_row_height_twips,blank_row_height_twips Row-height settings applied to bare `data.frame` elements. See [rtftable()] for details.
#' @param cell_padding_left_twips,cell_padding_right_twips,cell_valign Cell layout settings applied to bare `data.frame` elements. See [rtftable()] for details.
#' @param titles `NULL` (default) or a list of length `length(tables)`. Each
#'   element is a character vector -- one element per row of that page's
#'   title.  Magic tokens \code{"\{HALF_BLANK_ROW\}"} and
#'   \code{"\{BLANK_ROW\}"} are honoured.  Use `NULL` per element to
#'   fall back to the default (\code{\{HALF_BLANK_ROW\}} -- one
#'   half-height blank row).
#' @param footnotes `NULL` (default) or a list of length `length(tables)`.
#'   Same structure as `titles`; each element becomes one row in the
#'   footnote block.  Magic tokens supported.
#' @param auto_section Logical. When `TRUE` and `tables` is a **named** list,
#'   each name is used as a per-section heading appended to the common header
#'   defined by `rtf_section(secinfo = ...)` (called without a `page` argument).
#'   The document is then automatically split into one RTF section per named
#'   element. Unnamed items fall through to the previous section.
#'   Default `FALSE`.
#' @param section_label_align Alignment for the auto-appended section label row.
#'   One of `"left"` (default), `"center"`, or `"right"`.
#' @param read_gt **Legacy.**  Controls metadata extraction when a raw
#'   `gt_tbl` is handed *directly* to `rtf_tables()` (no pagination).  For
#'   new code, prefer converting up front with [as_rtftables()], which
#'   paginates *and* reads metadata, then pass the resulting list here --
#'   the page-level titles / footnotes flow through automatically (via the
#'   `rtf_titles` / `rtf_footnotes` attributes) with `read_gt` left at its
#'   default.  Allowed values:
#'   * `FALSE` (default) -- treat `gt_tbl` items as a rendered body only;
#'     ignore titles / labels / source notes.
#'   * `TRUE` -- read the render-relevant metadata: column labels,
#'     per-column alignment, spanning headers, widths, plus the page-level
#'     title / subtitle and footnote / source notes.  See [as_rtftables()]
#'     for the full *What is carried, by source* table.
#'   * A character vector of tokens -- selective opt-in.  See
#'     [as_rtftables()] for the token list.
#'   Explicit `rtf_tables()` / `rtf_titles()` / `rtf_footnotes()`
#'   values always override gt-extracted ones.
#'
#' @return Modified rtf_document with appended contents.
#'
#' @examples
#' \dontrun{
#' df1 <- data.frame(A = 1:3, B = c("x", "y", "z"))
#' df2 <- data.frame(A = 4:6, B = c("p", "q", "r"))
#'
#' # Three pages, shared formatting applied to both bare data.frames
#' doc <- rtf_document() %>%
#'   rtf_tables(
#'     list(df1, df2, rtfplot("fig.png")),
#'     col_rel_width    = c(1, 2),
#'     border           = "tfl",
#'     row_height_twips = 280L
#'   )
#' }
#'
#' @export
rtf_tables <- function(doc, tables,
                        col_header = NULL,
                        col_header_align = NULL,
                        spanning_header = NULL,
                        col_spec = NULL,
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
                        cell_padding_left_twips = 0L,
                        cell_padding_right_twips = 0L,
                        cell_valign = "bottom",
                        titles = NULL,
                        footnotes = NULL,
                        auto_section = FALSE,
                        section_label_align = "left",
                        read_gt = FALSE) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }

  if (!is.list(tables)) {
    stop("`tables` must be a list", call. = FALSE)
  }

  # Which table-formatting arguments did the caller pass EXPLICITLY?  These
  # override the corresponding fields of any pre-built rtftable item (e.g.
  # the output of as_rtftables()); arguments left at their default do not
  # touch the rtftable's own / gt-derived values.  Detection via match.call()
  # so we can tell "passed border = 'tfl'" from "defaulted to 'tfl'".
  .fmt_args <- c("col_header", "col_header_align", "spanning_header",
                 "col_spec", "border", "blank_rows", "style",
                 "col_rel_width", "column_widths_twips", "table_width_twips",
                 "table_width_pct_of_writable", "table_width_pct",
                 "table_align", "row_height_twips", "row_height_exact",
                 "header_row_height_twips", "blank_row_height_twips",
                 "cell_padding_left_twips", "cell_padding_right_twips",
                 "cell_valign")
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
      stop("Item ", i,
           " must be a data.frame, rtftable(), rtfplot(), gt_tbl, or",
           " gtsummary table object. ",
           "Each list element corresponds to exactly one page (one content).",
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
      .new_rtftable(
        data                        = item,
        col_header                  = eff_col_header,
        col_header_align            = col_header_align,
        spanning_header             = spanning_header,
        col_spec                    = eff_col_spec,
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
        cell_styles                 = eff_cell_styles
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

  # When auto_section = TRUE, wrap each named item in an rtf_auto_section_item
  # sentinel so that .pipe_doc_to_r6_report() can build per-section headers.
  if (isTRUE(auto_section)) {
    tbl_names <- names(tables)
    if (!is.null(tbl_names) && any(nzchar(tbl_names))) {
      tables <- lapply(seq_along(tables), function(i) {
        nm <- tbl_names[[i]]
        if (!is.null(nm) && nzchar(nm)) {
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
    if (!is.list(x)) {
      stop(sprintf("`%s` must be a list (or NULL).", name), call. = FALSE)
    }
    if (length(x) != n) {
      stop(sprintf("`%s` must have length %d (= length(tables)).",
                   name, n), call. = FALSE)
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

  # Create copy and append
  doc_copy <- doc
  doc_copy$contents  <- c(doc_copy$contents,  tables)
  doc_copy$titles    <- c(doc_copy$titles,    titles)
  doc_copy$footnotes <- c(doc_copy$footnotes, footnotes)
  doc_copy
}

#' Add figure content to document
#'
#' Append one or more image files (PNG/JPEG) as content pages. Each figure
#' creates one new page. Display dimensions and alignment apply to every
#' bare path in `figures`; elements already constructed via [rtfplot()] keep
#' their own settings.
#'
#' @param doc An rtf_document object.
#' @param figures A list whose elements are either character file paths to
#'   image files (PNG/JPEG) or pre-built `rtfplot` objects from [rtfplot()].
#' @param width_twips Display width in twips for bare paths.  `NULL` = full
#'   writable width.
#' @param height_twips Display height in twips for bare paths.  `NULL` =
#'   derived from the image's aspect ratio.
#' @param align Horizontal alignment for bare paths: `"center"` (default),
#'   `"left"`, or `"right"`.
#' @param titles,footnotes Optional lists of length `length(figures)`. See
#'   [rtf_tables()] for the same semantics -- character vectors per page,
#'   magic tokens supported.
#'
#' @return Modified rtf_document with appended figure contents.
#'
#' @export
rtf_figures <- function(doc, figures,
                         width_twips = NULL, height_twips = NULL,
                         align = "center",
                         titles = NULL, footnotes = NULL) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }

  if (!is.list(figures)) {
    stop("`figures` must be a list of file paths or rtfplot() objects",
         call. = FALSE)
  }

  # Validate and promote each element to rtfplot.
  fig_objs <- lapply(seq_along(figures), function(i) {
    fig <- figures[[i]]
    if (inherits(fig, "rtfplot")) {
      return(fig)
    }
    if (!is.character(fig) || length(fig) != 1L) {
      stop("Item ", i,
           " must be a single character file path or an rtfplot() object",
           call. = FALSE)
    }
    .new_rtfplot(path = fig, width_twips = width_twips,
                 height_twips = height_twips, align = align)
  })

  .validate_parallel <- function(x, n, name) {
    if (is.null(x)) return(rep(list(NULL), n))
    if (!is.list(x))
      stop(sprintf("`%s` must be a list (or NULL).", name), call. = FALSE)
    if (length(x) != n)
      stop(sprintf("`%s` must have length %d (= length(figures)).",
                   name, n), call. = FALSE)
    x
  }
  titles    <- .validate_parallel(titles,    length(figures), "titles")
  footnotes <- .validate_parallel(footnotes, length(figures), "footnotes")

  doc_copy <- doc
  doc_copy$contents  <- c(doc_copy$contents,  fig_objs)
  doc_copy$titles    <- c(doc_copy$titles,    titles)
  doc_copy$footnotes <- c(doc_copy$footnotes, footnotes)
  doc_copy
}

# ============================================================================
# rtf_titles() / rtf_footnotes() -- assign titles / footnotes to pages
# ============================================================================

#' Assign content titles to pages
#'
#' Replace the per-page title list with the supplied values.  The length of
#' `titles` must equal the number of pages already added via [rtf_tables()]
#' / [rtf_figures()].
#'
#' Each element is a character vector -- one element per row of the title
#' block.  Magic tokens \code{"\{HALF_BLANK_ROW\}"} (half-height blank
#' row) and \code{"\{BLANK_ROW\}"} (full-height blank row, equivalent
#' to `""`) are honoured.
#' Pass `NULL` for a single element to fall back to the default of one
#' \code{\{HALF_BLANK_ROW\}} row above the content.
#'
#' @param doc An rtf_document object.
#' @param titles A list of length equal to the number of pages.
#'
#' @return Modified rtf_document.
#'
#' @examples
#' \dontrun{
#' doc <- rtf_document() %>%
#'   rtf_tables(list(df1, df2)) %>%
#'   rtf_titles(list(
#'     c("Table 14.1.1", "{HALF_BLANK_ROW}", "Safety Population"),
#'     "Table 14.1.2"
#'   ))
#' }
#'
#' @export
rtf_titles <- function(doc, titles) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  n <- length(doc$contents)
  if (n == 0L) {
    stop("Cannot set titles before any content has been added.", call. = FALSE)
  }
  if (!is.list(titles)) {
    stop("`titles` must be a list (one element per page).", call. = FALSE)
  }
  if (length(titles) != n) {
    stop(sprintf("`titles` must have length %d (= number of pages).", n),
         call. = FALSE)
  }
  doc_copy <- doc
  doc_copy$titles <- titles
  doc_copy
}

#' Assign content footnotes to pages
#'
#' Same shape as [rtf_titles()]: a list with one element per page, each a
#' character vector whose entries become rows of the footnote block.  Magic
#' tokens are honoured.  `NULL` per element suppresses the footnote for
#' that page.
#'
#' @param doc An rtf_document object.
#' @param footnotes A list of length equal to the number of pages.
#'
#' @return Modified rtf_document.
#'
#' @export
rtf_footnotes <- function(doc, footnotes) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  n <- length(doc$contents)
  if (n == 0L) {
    stop("Cannot set footnotes before any content has been added.", call. = FALSE)
  }
  if (!is.list(footnotes)) {
    stop("`footnotes` must be a list (one element per page).", call. = FALSE)
  }
  if (length(footnotes) != n) {
    stop(sprintf("`footnotes` must have length %d (= number of pages).", n),
         call. = FALSE)
  }
  doc_copy <- doc
  doc_copy$footnotes <- footnotes
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
#' @param doc An rtf_document object.
#' @param page Integer or vector of page numbers to assign this section.
#'   - Single integer: one section starts at this page
#'   - Vector: assign multiple pages to sections (length must match secinfo)
#' @param secinfo Section information (one or more section definitions):
#'   - Single section: list(header = ..., footer = ...)
#'   - Multiple sections: list(sec1, sec2, ...) where each is a section list
#'
#' @return Modified rtf_document with section definitions added.
#'
#' @details
#' The `page` parameter identifies where each section starts. Pages are
#' auto-numbered from your content list (rtf_tables and rtf_figures).
#'
#' @examples
#' \dontrun{
#' doc <- rtf_document() %>%
#'   rtf_tables(list(df1, df2, df3)) %>%
#'   rtf_section(page = 1, secinfo = list(header = h1, footer = f1)) %>%
#'   rtf_section(page = 3, secinfo = list(header = h2, footer = f2))
#' }
#'
#' @export
rtf_section <- function(doc, page = NULL, secinfo) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }

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

# ============================================================================
# Format Functions (DEPRECATED)
# ============================================================================
#
# These four functions never actually applied formatting to the rendered RTF
# output -- they only stored values in unused slots of the rtf_document object.
# They are retained as no-ops with a deprecation warning so that existing
# scripts continue to run, and will be removed in a future release.
#
# Replacement: pass the formatting arguments (col_rel_width, border,
# row_height_twips, ...) directly to `rtf_tables()`, or build each item with
# `rtftable()` / `rtfplot()`.

#' Deprecated formatting functions
#'
#' `rtf_table_format()`, `rtf_header_format()`, `rtf_footer_format()`, and
#' `rtf_figure_format()` are **deprecated** and have no effect on the rendered
#' output. They will be removed in a future release.
#'
#' Use the formatting arguments of [rtf_tables()] (for tables and bare
#' data.frames) and [rtf_figures()] (for figures) instead, or build each item
#' explicitly with [rtftable()] / [rtfplot()] and pass it to `rtf_tables()`.
#'
#' @param doc An rtf_document object.
#' @param ... Ignored.
#'
#' @return `doc`, unchanged.
#' @name rtf_format-deprecated
#' @keywords internal
NULL

#' @rdname rtf_format-deprecated
#' @export
rtf_table_format <- function(doc, ...) {
  .Deprecated(
    new = "rtf_tables",
    msg = paste(
      "rtf_table_format() is deprecated and has no effect.",
      "Pass formatting arguments directly to rtf_tables() instead."
    )
  )
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  doc
}

#' @rdname rtf_format-deprecated
#' @export
rtf_header_format <- function(doc, ...) {
  .Deprecated(
    new = "rtf_header",
    msg = paste(
      "rtf_header_format() is deprecated and has no effect.",
      "Configure header rows via rtf_header() / rtf_section() instead."
    )
  )
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  doc
}

#' @rdname rtf_format-deprecated
#' @export
rtf_footer_format <- function(doc, ...) {
  .Deprecated(
    new = "rtf_footer",
    msg = paste(
      "rtf_footer_format() is deprecated and has no effect.",
      "Configure footer rows via rtf_footer() / rtf_section() instead."
    )
  )
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  doc
}

#' @rdname rtf_format-deprecated
#' @export
rtf_figure_format <- function(doc, ...) {
  .Deprecated(
    new = "rtf_figures",
    msg = paste(
      "rtf_figure_format() is deprecated and has no effect.",
      "Pass width_twips / height_twips / align to rtf_figures() or rtfplot() instead."
    )
  )
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  doc
}

# ============================================================================
# S3 Methods for printing
# ============================================================================

# -- Inspection helpers (shared by print.rtf_document / print.rtftable) -------

# Truncate a string for compact console display (ASCII ellipsis "..").
.insp_trunc <- function(s, n = 48L) {
  s <- gsub("[\r\n]+", " ", paste(as.character(s), collapse = " / "))
  if (nchar(s) > n) paste0(substr(s, 1L, n - 2L), "..") else s
}

# One text line per header/footer row, marking left / centre / right cells.
.insp_hf_lines <- function(hf) {
  rows <- hf$rows
  if (is.null(rows) || length(rows) == 0L) return(character(0))
  vapply(rows, function(r) {
    nm <- names(r) %||% rep("", length(r))
    cells <- vapply(seq_along(r), function(i) {
      tag <- switch(nm[[i]], l = "L:", c = "C:", r = "R:", "?:")
      paste0(tag, " ", .insp_trunc(r[[i]], 34L))
    }, character(1L))
    paste(cells, collapse = "   ")
  }, character(1L))
}

# One text line per column-header row ("a | b | c").
.insp_colheader_lines <- function(col_header, fallback = NULL) {
  if (is.null(col_header)) {
    return(if (is.null(fallback)) character(0)
           else paste(fallback, collapse = " | "))
  }
  rows <- if (is.character(col_header)) list(col_header) else col_header
  vapply(rows, function(row) {
    labs <- if (is.character(row)) row else vapply(row, function(cell) {
      if (is.list(cell)) as.character(cell$label %||% "") else as.character(cell)
    }, character(1L))
    paste(labs, collapse = " | ")
  }, character(1L))
}

# Short human description of a resolved table border.
.insp_border_desc <- function(b) {
  if (is.null(b)) return("none")
  sides <- c(
    if (!is.null(b$header$top))      "header-top",
    if (!is.null(b$header$bottom))   "header-bottom",
    if (!is.null(b$last_row$bottom)) "last-row-bottom")
  if (length(sides) == 0L) "custom" else paste(sides, collapse = " + ")
}

# Compact column-width description.
.insp_width_desc <- function(x) {
  if (!is.null(x$column_widths_twips))
    paste0("fixed twips: ", paste(x$column_widths_twips, collapse = " / "))
  else if (!is.null(x$col_rel_width))
    paste0("relative: ", paste(x$col_rel_width, collapse = " : "))
  else "equal (auto)"
}

#' Inspect an `rtftable`'s layout (text)
#'
#' `print.rtftable()` is a **build-time layout inspector**: it shows, as plain
#' text, how a single table will be laid out -- its column headers (each row,
#' with spanning noted), per-column body alignment, column widths, border
#' style, row count, and any title / footnote that travelled with the object
#' (e.g. from [as_rtftables()]).  It is meant for checking the structure while
#' you build a report, without rendering to RTF and opening Word.  For the full
#' document outline see [print.rtf_document()]; for a quick picture see
#' [plot.rtftable()].
#'
#' @param x An [rtftable()] object.
#' @param ... Unused.
#' @return Invisibly, `x`.
#' @export
print.rtftable <- function(x, ...) {
  ref   <- if (!is.null(x$data_list)) x$data_list[[1L]] else x$data
  ncols <- if (is.null(ref)) 0L else ncol(ref)
  ndata <- if (!is.null(x$data_list)) sum(vapply(x$data_list, nrow, integer(1L)))
           else nrow(x$data %||% data.frame())
  cat(sprintf("<rtftable>  %d column%s x %d data row%s%s\n",
              ncols, if (ncols == 1L) "" else "s",
              ndata, if (ndata == 1L) "" else "s",
              if (!is.null(x$data_list))
                sprintf("  (%d data.frames)", length(x$data_list)) else ""))

  hdr <- .insp_colheader_lines(x$col_header, names(ref))
  if (length(hdr)) {
    cat("  Column headers:\n")
    for (h in hdr) cat("    ", h, "\n", sep = "")
  }
  if (!is.null(x$spanning_header) && length(x$spanning_header))
    cat("    (+ spanning header)\n")

  aligns <- vapply(x$col_spec %||% list(),
                   function(s) substr(s$align %||% "?", 1L, 1L), character(1L))
  if (length(aligns))
    cat("  Body alignment: ", paste(aligns, collapse = " | "), "\n", sep = "")
  cat("  Column widths:  ", .insp_width_desc(x), "\n", sep = "")
  cat("  Border:         ", .insp_border_desc(x$border), "\n", sep = "")

  tt <- attr(x, "rtf_titles",    exact = TRUE)
  fn <- attr(x, "rtf_footnotes", exact = TRUE)
  if (!is.null(tt)) cat("  Title:    ", .insp_trunc(unlist(tt), 56L), "\n", sep = "")
  if (!is.null(fn)) cat("  Footnote: ", .insp_trunc(unlist(fn), 56L), "\n", sep = "")
  invisible(x)
}

#' Inspect an `rtf_document`'s structure (text)
#'
#' `print.rtf_document()` is a **build-time structural inspector**.  As you
#' assemble a document with the pipe (`rtf_document()` -> `rtf_section()` ->
#' `rtf_tables()`/`rtf_figures()`), printing it shows -- as plain text -- the
#' page setup, every section's running **header** and **footer** (left / centre
#' / right cells, including page-number tokens), and every content block (type,
#' size, column headers, title, footnote).  The goal is to confirm the layout
#' and attributes are what you intended *before* rendering to RTF and opening
#' the file in Word / LibreOffice.
#'
#' For one table's detail see [print.rtftable()]; for a quick visual sketch see
#' [plot.rtf_document()]; to write the file use [generate_rtfreport()].
#'
#' @param x An [rtf_document()] object.
#' @param ... Unused.
#' @return Invisibly, `x`.
#' @export
print.rtf_document <- function(x, ...) {
  pg  <- x$document$page %||% list()
  fmt <- x$document$default_format %||% list()
  orient <- pg$orientation %||% "landscape"
  w   <- pg$width_in  %||% 11
  h   <- pg$height_in %||% 8.5
  fs  <- (fmt$font_size_half_points %||% 18L) / 2

  cat("<rtf_document>\n")
  cat(sprintf("  Page:  %g x %g in, %s; font %gpt\n", w, h, orient, fs))

  secs <- x$sections
  cat(sprintf("  Sections (%d):\n",
              length(secs %||% list())))
  for (nm in names(secs)) {
    s <- secs[[nm]]
    label <- if (identical(nm, "_default")) "default (auto)" else paste("from page", nm)
    cat("    - ", label, ":\n", sep = "")
    hl <- if (!is.null(s$header)) .insp_hf_lines(s$header) else character(0)
    fl <- if (!is.null(s$footer)) .insp_hf_lines(s$footer) else character(0)
    if (length(hl)) { cat("        header:\n"); for (l in hl) cat("          ", l, "\n", sep = "") }
    if (length(fl)) { cat("        footer:\n"); for (l in fl) cat("          ", l, "\n", sep = "") }
    if (!length(hl) && !length(fl)) cat("        (no header / footer)\n")
  }

  ct <- x$contents
  cat(sprintf("  Content blocks (%d):\n", length(ct %||% list())))
  for (i in seq_along(ct)) {
    item  <- ct[[i]]
    if (inherits(item, "rtftable")) {
      ref   <- if (!is.null(item$data_list)) item$data_list[[1L]] else item$data
      ncols <- if (is.null(ref)) 0L else ncol(ref)
      ndata <- if (!is.null(item$data_list))
        sum(vapply(item$data_list, nrow, integer(1L))) else nrow(item$data %||% data.frame())
      hdr <- .insp_colheader_lines(item$col_header, names(ref))
      cat(sprintf("    [%d] table   %d x %d   cols: %s\n", i, ncols, ndata,
                  if (length(hdr)) .insp_trunc(hdr[[1L]], 46L) else "-"))
    } else if (inherits(item, "rtfplot") || inherits(item, "rtf_figure")) {
      cat(sprintf("    [%d] figure\n", i))
    } else {
      cat(sprintf("    [%d] %s\n", i, class(item)[[1L]]))
    }
    tt <- x$titles[[i]]
    fn <- x$footnotes[[i]]
    if (!is.null(tt) && length(tt) && any(nzchar(unlist(tt))))
      cat("         title:    ", .insp_trunc(unlist(tt), 50L), "\n", sep = "")
    if (!is.null(fn) && length(fn) && any(nzchar(unlist(fn))))
      cat("         footnote: ", .insp_trunc(unlist(fn), 50L), "\n", sep = "")
  }
  cat("  -> generate_rtfreport() writes the .rtf; plot() draws a sketch.\n")
  invisible(x)
}
