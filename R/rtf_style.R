# ============================================================================
#  rtf_style() -- one style object for every module (#804)
# ============================================================================
#
#  An ADDITION: every per-function style argument keeps
#  working unchanged, and rtf_style() is a second way to set the same things
#  in one place.  A shared level reaches every module that has the argument;
#  a section (`table`, `titles`, `footnotes`, `header`, `footer`) reaches its
#  module only, under the module's own argument names.
#
#  Precedence, highest first:
#
#    the module's own argument (rtftable(), rtf_titles(), rtf_header(), ...)
#      > the rtf_style() section
#      > the rtf_style() shared level
#      > the rtfreporter.* option
#      > the resource baseline
#
#  Several rtf_style() objects given through rtf_config(style = ) merge key by
#  key, the later winning.  The deprecated rtf_default_format() maps onto the
#  shared level beneath any rtf_style(): an explicit style wins over it.
#
#  How it is applied: the shared level becomes the document's defaults (the
#  ones generate_rtfreport() resolves into `doc_row_height`, `doc_pad_*`,
#  `doc_markup`, ...); a section's keys, and the shared `font` / `color`, are
#  written into each module's element when the document is turned into a
#  report, only where the element left the key unset.  So the renderers see
#  the module values they always did.
# ============================================================================

# The shared keys, in signature order.
.STYLE_SHARED_KEYS <- c(
  "font", "font_size_half_points", "row_height_twips",
  "cell_padding_left_twips", "cell_padding_right_twips",
  "cell_padding_top_twips", "cell_padding_bottom_twips",
  "markup", "color",
  "title_format", "footnote_format", "title_width", "footnote_width")

# The keys each section takes: the module's own argument names.
.STYLE_PAD_KEYS <- c("cell_padding_left_twips", "cell_padding_right_twips",
                     "cell_padding_top_twips", "cell_padding_bottom_twips")
.STYLE_SECTION_KEYS <- list(
  table     = c("font", "font_size_half_points", "row_height_twips",
                "row_height_exact", "header_row_height_twips",
                "blank_row_height_twips", .STYLE_PAD_KEYS, "cell_valign",
                "table_align", "markup"),
  titles    = c("font", "font_size_half_points", "row_height_twips", "markup",
                "align"),
  footnotes = c("font", "font_size_half_points", "row_height_twips", "markup",
                "align", "border"),
  header    = c("font", "font_size_half_points", "row_height_twips", "markup",
                .STYLE_PAD_KEYS, "border", "color"),
  footer    = c("font", "font_size_half_points", "row_height_twips", "markup",
                .STYLE_PAD_KEYS, "border", "color")
)
.STYLE_SECTIONS <- names(.STYLE_SECTION_KEYS)

# The shared keys that are document defaults (the rest -- font, color -- are
# written into the modules).
.STYLE_DOC_KEYS <- c("font_size_half_points", "row_height_twips",
                     .STYLE_PAD_KEYS, "markup", "title_format",
                     "footnote_format", "title_width", "footnote_width")

#' One style object for every module of a document
#'
#' `rtf_style()` gathers the look of a document -- font, size, row height,
#' cell padding, markup, the header / footer colour -- in one object, given
#' to [rtf_document()] or [rtf_config()] as `style = `.  It is an
#' **addition**: every style argument of [rtftable()], [rtf_titles()],
#' [rtf_footnotes()], [rtf_header()] and [rtf_footer()] keeps working, and
#' wins over the style.  A study standard is written once, here; a report
#' changes what it needs with the module's own argument.
#'
#' The **shared** arguments reach every module that has the argument.  A
#' **section** (`table`, `titles`, `footnotes`, `header`, `footer`) is a named
#' list that reaches its module only, under that module's own argument names;
#' a section key beats the shared one.  The order, highest first:
#'
#' **the module's own argument > the `rtf_style()` section > the
#' `rtf_style()` shared level > the `rtfreporter.*` option > the resource
#' baseline.**
#'
#' A section key reaches an element only where the element left the argument
#' unset: `NULL`, or, for [rtftable()]'s `table_align`, `cell_valign` and
#' `row_height_exact` and for a band's `border`, not passed.  As for the
#' module arguments, a font size and a row height resolve together: an
#' element that sets its own size and no height takes no height from the
#' style, its height following its size.  A table is cut into pages
#' ([as_rtftables()]) before the style reaches it, so a section's row height
#' does not change where the pages break.
#'
#' Row height, cell padding, vertical alignment and font size apply to the
#' whole table (the body, and the header for its own settings); they are not
#' set per row or per cell.
#'
#' Several `rtf_style()` objects given through [rtf_config()] merge key by
#' key, the later winning.  `rtf_style()` replaces [rtf_default_format()],
#' which is deprecated: a `default_format` still given maps onto the shared
#' level, beneath any `rtf_style()`.
#'
#' @param font Font family for the text of every module, e.g. `"Arial"`; a
#'   named vector gives one family per script slot (see [rtf_document()]).  A
#'   family not in the document's `font_table` is added to it.
#' @param font_size_half_points The document's font size, in half-points
#'   (`18` = 9 pt).
#' @param row_height_twips The row height of every table-shaped element
#'   (content table, title / footnote, header / footer), in twips.
#' @param cell_padding_left_twips,cell_padding_right_twips,cell_padding_top_twips,cell_padding_bottom_twips
#'   The cell padding of every table-shaped element, in twips.
#' @param markup Cell-text markup: `"script"`, `"relational"`, `"all"` or
#'   `"none"` (see [rtftable()]).
#' @param color Text colour, a 6-digit hex string such as `"#7F7F7F"`.  For
#'   now the shared colour reaches the page header and footer only.
#' @param title_format,footnote_format How the title / footnote block is
#'   written: `"text"` (plain paragraphs) or `"table"` (a single-column
#'   table).  The defaults are `"text"` for the title and `"table"` for the
#'   footnote.  A `border` on a title or footnote needs the `"table"` form;
#'   the `"text"` form ignores it and warns.
#' @param title_width,footnote_width The width of the title / footnote
#'   block: `"content"` (the table's), `"page"` (the writable width), a
#'   fraction in `(0, 1]` of the writable width, or twips (a whole number
#'   above 1).
#' @param table A named list of [rtftable()]'s arguments for every content
#'   table: `font`, `font_size_half_points`, `row_height_twips`,
#'   `row_height_exact`, `header_row_height_twips`, `blank_row_height_twips`,
#'   the four `cell_padding_*_twips`, `cell_valign`, `table_align`, `markup`.
#' @param titles A named list of [rtf_titles()]'s style arguments: `font`,
#'   `font_size_half_points`, `row_height_twips`, `markup`, `align`.
#' @param footnotes A named list of [rtf_footnotes()]'s style arguments:
#'   `font`, `font_size_half_points`, `row_height_twips`, `markup`, `align`,
#'   `border`.
#' @param header,footer A named list of [rtf_header()]'s / [rtf_footer()]'s
#'   style arguments: `font`, `font_size_half_points`, `row_height_twips`,
#'   `markup`, the four `cell_padding_*_twips`, `border`, `color`.
#' @param ... Not used: an argument not named above is an error.
#'
#' @return An `rtf_style` object for `rtf_document(style = )` /
#'   `rtf_config(style = )`.  Every argument left `NULL` is unset.
#'
#' @seealso [rtf_document()], [rtf_config()], [rtftable()], [rtf_header()].
#'
#' @examples
#' study <- rtf_style(
#'   font = "Arial", font_size_half_points = 18L,
#'   cell_padding_left_twips = 40L, cell_padding_right_twips = 40L,
#'   table     = list(header_row_height_twips = 260L, cell_valign = "center"),
#'   header    = list(color = "#7F7F7F"),
#'   footnotes = list(font_size_half_points = 16L)
#' )
#' study
#'
#' adsl_tbl <- data.frame(Parameter = c("Age, Mean (SD)", "Sex: Female, n (%)"),
#'                        Placebo = c("75.2 (8.6)", "53 (61.6)"),
#'                        Active  = c("74.4 (7.9)", "50 (59.5)"))
#' doc <- rtf_document(style = study) |>
#'   rtf_tables(as_rtftables(adsl_tbl), titles = "Table 14.1.1") |>
#'   rtf_footnotes("Source: ADSL", font_size_half_points = 14L) |> # this report: 7 pt wins
#'   rtf_section(header = rtf_header(c(l = "Study XYZ", r = "Page {AUTO_PAGE}")))
#' f <- tempfile(fileext = ".rtf")
#' generate_rtfreport(doc, f)
#' unlink(f)
#'
#' # A later style merges key by key over the document's:
#' doc <- rtf_config(doc, style = rtf_style(table = list(cell_valign = "top")))
#'
#' @export
rtf_style <- function(font = NULL, font_size_half_points = NULL,
                      row_height_twips = NULL,
                      cell_padding_left_twips = NULL,
                      cell_padding_right_twips = NULL,
                      cell_padding_top_twips = NULL,
                      cell_padding_bottom_twips = NULL,
                      markup = NULL, color = NULL,
                      title_format = NULL, footnote_format = NULL,
                      title_width = NULL, footnote_width = NULL,
                      table = NULL, titles = NULL, footnotes = NULL,
                      header = NULL, footer = NULL, ...) {
  dots <- list(...)
  if (length(dots)) {
    nm <- names(dots) %||% rep("", length(dots))
    nm[is.na(nm) | !nzchar(nm)] <- "<unnamed>"
    stop(sprintf(paste0("`rtf_style()`: unknown argument %s.  The sections are ",
                        "%s; the shared keys are %s."),
                 paste0("`", nm, "`", collapse = ", "),
                 paste(.STYLE_SECTIONS, collapse = ", "),
                 paste(.STYLE_SHARED_KEYS, collapse = ", ")), call. = FALSE)
  }
  shared <- list(font = font, font_size_half_points = font_size_half_points,
                 row_height_twips = row_height_twips,
                 cell_padding_left_twips = cell_padding_left_twips,
                 cell_padding_right_twips = cell_padding_right_twips,
                 cell_padding_top_twips = cell_padding_top_twips,
                 cell_padding_bottom_twips = cell_padding_bottom_twips,
                 markup = markup, color = color,
                 title_format = title_format, footnote_format = footnote_format,
                 title_width = title_width, footnote_width = footnote_width)
  out <- list()
  for (k in .STYLE_SHARED_KEYS) {
    out[k] <- list(.check_style_value(k, shared[[k]],
                                      sprintf("rtf_style(%s)", k)))
  }
  sections <- list(table = table, titles = titles, footnotes = footnotes,
                   header = header, footer = footer)
  for (s in .STYLE_SECTIONS) {
    out[[s]] <- .check_style_section(s, sections[[s]])
  }
  structure(out, class = "rtf_style")
}

# One section: NULL or a named list of the module's argument names.  Returns
# the set keys only (a NULL value is unset), each value checked.
.check_style_section <- function(s, x) {
  if (is.null(x)) return(list())
  if (!is.list(x) || inherits(x, c("rtf_style", "rtf_table_style", "rtf_border"))) {
    stop(sprintf(paste0("`rtf_style(%s = )` must be a named list of %s's ",
                        "arguments, e.g. list(%s = ...)."),
                 s, .style_module_name(s), .STYLE_SECTION_KEYS[[s]][2L]),
         call. = FALSE)
  }
  if (!length(x)) return(list())
  nm <- names(x)
  if (is.null(nm) || anyNA(nm) || !all(nzchar(nm))) {
    stop(sprintf("`rtf_style(%s = )`: every entry must be named.", s),
         call. = FALSE)
  }
  if (anyDuplicated(nm)) {
    stop(sprintf("`rtf_style(%s = )`: `%s` is given twice.", s,
                 nm[anyDuplicated(nm)]), call. = FALSE)
  }
  allowed <- .STYLE_SECTION_KEYS[[s]]
  bad <- setdiff(nm, allowed)
  if (length(bad)) {
    stop(sprintf("`rtf_style(%s = )`: unknown key %s.  The keys of `%s` are %s.",
                 s, paste0("`", bad, "`", collapse = ", "), s,
                 paste(allowed, collapse = ", ")), call. = FALSE)
  }
  out <- list()
  for (k in nm) {
    v <- .check_style_value(k, x[[k]], sprintf("rtf_style(%s)$%s", s, k))
    if (!is.null(v)) out[[k]] <- v
  }
  out
}

.style_module_name <- function(s) {
  switch(s, table = "rtftable()", titles = "rtf_titles()",
         footnotes = "rtf_footnotes()", header = "rtf_header()",
         footer = "rtf_footer()")
}

# Check one value the way its module checks it; NULL is unset.
.check_style_value <- function(k, v, arg) {
  if (is.null(v)) return(NULL)
  switch(k,
    font = .check_font(v, arg),
    font_size_half_points = .check_font_size(v, arg),
    row_height_twips = ,
    header_row_height_twips = ,
    blank_row_height_twips = ,
    cell_padding_left_twips = ,
    cell_padding_right_twips = ,
    cell_padding_top_twips = ,
    cell_padding_bottom_twips = .check_row_height(v, arg),
    markup = .resolve_markup(v),
    color = {
      if (!is.character(v) || length(v) != 1L || is.na(v) ||
          !grepl("^#[0-9A-Fa-f]{6}$", v)) {
        stop(sprintf(paste0("`%s` must be NULL or a 6-digit hex colour such ",
                            "as \"#7F7F7F\"."), arg), call. = FALSE)
      }
      toupper(v)
    },
    align = .check_align(v, arg),
    table_align = .check_align(v, arg),
    cell_valign = {
      if (!is.character(v) || length(v) != 1L ||
          !v %in% c("top", "center", "bottom")) {
        stop(sprintf("`%s` must be \"top\", \"center\" or \"bottom\".", arg),
             call. = FALSE)
      }
      v
    },
    row_height_exact = {
      if (!is.logical(v) || length(v) != 1L || is.na(v)) {
        stop(sprintf("`%s` must be TRUE or FALSE.", arg), call. = FALSE)
      }
      v
    },
    border = {
      if (!inherits(v, "rtf_border")) {
        stop(sprintf("`%s` must be NULL or an rtf_border() object.", arg),
             call. = FALSE)
      }
      v
    },
    title_format = ,
    footnote_format = {
      if (!is.character(v) || length(v) != 1L || !v %in% c("text", "table")) {
        stop(sprintf("`%s` must be \"text\" or \"table\".", arg), call. = FALSE)
      }
      v
    },
    title_width = ,
    footnote_width = .check_block_width(v, arg),
    stop(sprintf("`%s`: not a style key.", arg), call. = FALSE)
  )
}

#' @export
print.rtf_style <- function(x, ...) {
  fmt <- function(v) {
    if (inherits(v, "rtf_border")) return("<rtf_border>")
    if (is.character(v) && !length(v)) return("\"none\"")
    if (is.character(v)) return(paste0("\"", v, "\"", collapse = ", "))
    paste(format(v), collapse = ", ")
  }
  line <- function(keys, vals) {
    set <- keys[!vapply(vals[keys], is.null, logical(1L))]
    if (!length(set)) return(NULL)
    paste(sprintf("%s = %s", set, vapply(vals[set], fmt, character(1L))),
          collapse = "; ")
  }
  cat("<rtf_style>\n")
  sh <- line(.STYLE_SHARED_KEYS, unclass(x))
  if (is.null(sh) && !any(lengths(unclass(x)[.STYLE_SECTIONS]))) {
    cat("  (nothing set)\n")
    return(invisible(x))
  }
  if (!is.null(sh)) cat(strwrap(sh, indent = 2L, exdent = 4L, prefix = ""), sep = "\n")
  for (s in .STYLE_SECTIONS) {
    sec <- x[[s]]
    if (!length(sec)) next
    cat(strwrap(paste0(s, ": ", line(names(sec), sec)), indent = 2L,
                exdent = 4L), sep = "\n")
  }
  invisible(x)
}

# -- merging ------------------------------------------------------------------

# `b` over `a`, key by key: a shared key `b` sets wins; a section merges per key.
.merge_style <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  for (k in .STYLE_SHARED_KEYS) {
    if (!is.null(b[[k]])) a[k] <- list(b[[k]])
  }
  for (s in .STYLE_SECTIONS) {
    a[[s]] <- .merge_list(a[[s]] %||% list(), b[[s]])
  }
  a
}

.check_style_arg <- function(style, fn) {
  if (!is.null(style) && !inherits(style, "rtf_style")) {
    stop(sprintf("`%s(style = )` must be NULL or an rtf_style() object.", fn),
         call. = FALSE)
  }
  invisible(style)
}

# -- the deprecated default_format --------------------------------------------

.warn_default_format <- function() {
  .deprecate_once("rtf_default_format", paste0(
    "`rtf_default_format()` is deprecated: use rtf_style(); it will be ",
    "removed in 0.9."))
}

# A default_format (an rtf_default_format() or a named list of its keys) as the
# rtf_style() it maps onto: its keys are the shared level's.  Other names in a
# list were never read, and still are not.
.style_from_default_format <- function(df) {
  if (is.null(df)) return(NULL)
  keys <- intersect(names(df), .STYLE_DOC_KEYS)
  args <- unclass(df)[keys]
  args <- args[!vapply(args, is.null, logical(1L))]
  do.call(rtf_style, args)
}

# The document's style: its default_format beneath its rtf_style().
.doc_effective_style <- function(document) {
  .merge_style(.style_from_default_format(document$default_format),
               document$style)
}

# -- applying it to a report --------------------------------------------------

# Write `s` into an element's style list where the element left a key unset.
# `unset`: keys the element holds a value for that is only its default.  A
# size the element set without a height keeps the height its size implies.
.style_fill <- function(el, s, unset = character(0)) {
  if (!length(s)) return(el)
  if (is.null(el)) el <- list()
  own_fs <- !is.null(el[["font_size_half_points"]])
  own_rh <- !is.null(el[["row_height_twips"]])
  for (k in names(s)) {
    if (k == "row_height_twips" && own_fs && !own_rh) next
    if (is.null(el[[k]]) || k %in% unset) el[[k]] <- s[[k]]
  }
  el
}

# A section's keys with the shared font (and, for a band, the shared colour)
# beneath them.
.style_section <- function(st, s) {
  sec <- st[[s]] %||% list()
  low <- list(font = st$font)
  if (s %in% c("header", "footer")) low$color <- st$color
  low <- low[!vapply(low, is.null, logical(1L))]
  .merge_list(low, sec)
}

.style_fill_table <- function(tbl, s) {
  if (!inherits(tbl, "rtftable") || !length(s)) return(tbl)
  .style_fill(tbl, s, unset = tbl[["style_defaulted"]] %||% character(0))
}

.style_fill_band <- function(hf, s) {
  if (is.null(hf) || !length(s)) return(hf)
  hf <- .normalize_hf(hf)
  # a band's border left as the function's default (rtf_header(): none;
  # rtf_footer(): a top rule) is unset; one built by hand without one too
  # (a border passed, even NULL for none, is the band's own)
  if (isTRUE(hf[["border_default"]]) ||
      (is.null(hf[["border_default"]]) && is.null(hf[["border"]]))) {
    return(.style_fill(hf, s, unset = "border"))
  }
  .style_fill(hf, s[setdiff(names(s), "border")])
}

# The document's style (its default_format beneath its rtf_style()) applied to
# a report built from it: the shared level as the document defaults, each
# section (with the shared font / colour) into its modules.
.apply_style_to_report <- function(report, document) {
  st <- .doc_effective_style(document)
  if (is.null(st)) return(report)
  lvl <- unclass(st)[.STYLE_DOC_KEYS]
  lvl <- lvl[!vapply(lvl, is.null, logical(1L))]
  if (length(lvl)) report <- .rtfreport_set_default_format(report, lvl)

  tb <- .style_section(st, "table")
  for (i in seq_along(report$pages)) {
    p <- report$pages[[i]]
    p$content <- .style_fill_table(p$content, tb)
    for (j in seq_along(p$segments)) {
      p$segments[[j]]$content <- .style_fill_table(p$segments[[j]]$content, tb)
    }
    report$pages[[i]] <- p
  }
  ti <- .style_section(st, "titles")
  if (length(ti)) report$document$title_style <-
    .style_fill(report$document$title_style, ti)
  fn <- .style_section(st, "footnotes")
  if (length(fn)) report$document$footnote_style <-
    .style_fill(report$document$footnote_style, fn)
  hd <- .style_section(st, "header")
  ft <- .style_section(st, "footer")
  for (i in seq_along(report$sections)) {
    sec <- report$sections[[i]]
    if (!is.list(sec)) next
    sec$header <- .style_fill_band(sec$header, hd)
    sec$footer <- .style_fill_band(sec$footer, ft)
    report$sections[[i]] <- sec
  }
  report
}
