# ---------------------------------------------------------------------------
#  rtf_layout(): a document's blocks as printed pages
# ---------------------------------------------------------------------------
#  Nothing is measured.  A block begins a page when it says so
#  (`page_break = TRUE`, the default), when it is the document's first, or
#  when it is a later page of its table (rtf_tables() made those break); a
#  block with `page_break = FALSE` goes on the page before it, under what
#  is there.  What does not fit, the reader (Word, LibreOffice) carries on
#  to the next page: fitting a page is the program's to arrange, and
#  print(rtf_layout(doc)) shows the rows and figure sizes it has to go by.

#' Lay a document's contents out as printed pages
#'
#' Shows how the contents added by [rtf_tables()] and [rtf_figures()] fall
#' on the pages [generate_rtfreport()] writes.  Each content is a block; a
#' block begins a page unless it was added with `page_break = FALSE`, in
#' which case it goes on the page before it, under what is there.  A
#' table's own later pages (a plan's, or [as_rtftables()]'s) always begin
#' a page, and so does the document's first content.
#'
#' A page has one `auto_section` heading (see [rtf_tables()]): that of the
#' first block on it that has one.  A later block on the same page with a
#' different heading gets a warning, and its heading is not used; no page
#' break is added for it.  A page whose blocks have none stays in the
#' section before it.  An [rtf_section()]`(page = n)` on the page wins over
#' its heading.
#'
#' Nothing is measured: the pages are decided by `page_break` and by the
#' tables' own pagination (`max_rows`, `split`, `first_rows`) alone.  A page
#' holding more than fits is carried on by the reader (Word, LibreOffice),
#' and the page numbers written into the file are then off by those pages.
#' Fitting a page is the program's to arrange; `print()` of the result
#' shows the rows of each table and the size of each figure to arrange it
#' by.
#'
#' @param doc An [rtf_document()].
#'
#' @return An `rtf_pages` object: a list with
#'   \describe{
#'     \item{`pages`}{one entry per printed page, each a list with `number`
#'       (the page number, as [rtf_section()]`(page = )` counts it),
#'       `section` (the section the page is in, counted from 1),
#'       `section_start` (`TRUE` where a section begins: page 1, a page
#'       [rtf_section()] starts one on, or a page an `auto_section` heading
#'       opens), `section_label` (that heading, or `NULL`) and `segments`:
#'       the blocks on the page, top to bottom, each a list with `block` (its
#'       position in the document), `name`, `content` (an `rtftable` or
#'       `rtfplot`), `title` (with `cont_title`'s line on a table's later
#'       pages), `footnote`, `space_before`, `group` and `page_in_group`
#'       (which table, and which page of it), and `section` (the
#'       `auto_section` heading the block opens, or `NULL`).}
#'     \item{`n_blocks`}{the number of blocks.}
#'     \item{`paper`}{the page size and font size, for printing.}
#'   }
#'   `print()` shows one line per block.
#'
#' @seealso [rtf_tables()] / [rtf_figures()] (`page_break`, `space_before`,
#'   `first_rows`), [generate_rtfreport()].
#'
#' @examples
#' df <- function(n) data.frame(Item = sprintf("Row %02d", seq_len(n)), N = seq_len(n))
#' doc <- rtf_document() |>
#'   rtf_tables(list(Summary = df(8)), titles = "Table 1") |>
#'   rtf_tables(list(Listing = df(12)), titles = "Listing 1", page_break = FALSE) |>
#'   rtf_tables(list(Other = df(5)), titles = "Table 2")
#' lay <- rtf_layout(doc)
#' length(lay$pages)      # 2: the listing goes on the first page
#' lay
#'
#' @export
rtf_layout <- function(doc) {
  if (!inherits(doc, "rtf_document")) {
    stop("`doc` must be an rtf_document object", call. = FALSE)
  }
  .rtf_layout(doc)
}

# `warn = FALSE`: print() shows the layout without saying again what the
# rendering will say.
.rtf_layout <- function(doc, warn = TRUE) {
  blocks <- doc[["contents"]]
  nms <- names(blocks)
  pages <- list()
  for (i in seq_along(blocks)) {
    b <- blocks[[i]]
    # a table's later page begins a page (rtf_tables() made it break, too)
    flow <- !isTRUE(b$page_break) && b$page_in_group <= 1L && length(pages) > 0L
    # cont_title: under the title of a table's later pages, wherever its
    # pages come from (first_rows, or the table's own pagination)
    title <- b[["title"]]
    if (!is.null(b[["cont_title"]]) && b$page_in_group > 1L) {
      title <- .title_add_row(title, list(text = b[["cont_title"]], align = "left"))
    }
    seg <- list(block = i,
                name = if (is.null(nms) || is.na(nms[i]) || !nzchar(nms[i])) NULL
                       else .page_name_base(nms[i]),
                content = b[["content"]], title = title, footnote = b[["footnote"]],
                space_before = b$space_before %||% 0L,
                group = b$group, page_in_group = b$page_in_group,
                section = b[["section"]])
    if (flow) {
      k <- length(pages)
      pages[[k]]$segments[[length(pages[[k]]$segments) + 1L]] <- seg
    } else {
      pages[[length(pages) + 1L]] <- list(number = length(pages) + 1L, section = NA_integer_,
                                          section_start = FALSE, section_label = NULL,
                                          auto_section = NULL, segments = list(seg))
    }
  }
  # A page's auto_section heading: that of the first block on it that has
  # one (rtf_tables(auto_section = TRUE)).  RTF begins a section only at a
  # page, so a page has one heading: a later block's different heading is
  # said and not used, and no page break is added for it (the pages are
  # page_break's and the tables' own alone).  A page whose blocks have
  # none stays in the section open before it.
  for (k in seq_along(pages)) {
    secs <- Filter(Negate(is.null), lapply(pages[[k]]$segments, `[[`, "section"))
    if (!length(secs)) next
    first <- secs[[1L]]
    pages[[k]]$auto_section <- list(label = first[["label"]], label_align = first[["align"]])
    other <- setdiff(unique(vapply(secs, `[[`, "", "label")), first[["label"]])
    if (warn && length(other)) {
      warning(sprintf(paste0("rtf_layout(): page %d holds the auto_section headings ",
                             "'%s' and '%s'; a page has one: '%s' is its heading ",
                             "(auto_title names a table on its title)."),
                      k, first[["label"]], paste(other, collapse = "', '"),
                      first[["label"]]), call. = FALSE)
    }
  }
  # The sections the pages are in: one begins on page 1, on a page
  # rtf_section(page = n) names, and on a page an auto_section heading
  # opens -- an explicit section on that page wins over it (#548).
  keys <- names(doc$sections)
  explicit <- suppressWarnings(as.integer(keys[keys != "_default"]))
  sec <- 0L
  for (k in seq_along(pages)) {
    auto <- pages[[k]]$auto_section
    starts <- k == 1L || k %in% explicit || !is.null(auto)
    if (starts) sec <- sec + 1L
    pages[[k]]$section <- sec
    pages[[k]]$section_start <- starts
    pages[[k]]["section_label"] <- list(if (!is.null(auto) && !k %in% explicit) auto$label)
  }
  structure(list(pages = pages, n_blocks = length(blocks), paper = .rtf_paper_desc(doc)),
            class = "rtf_pages")
}

# "letter landscape, 9 pt": the page and the document's font size
.rtf_paper_desc <- function(doc) {
  # a preset page (the default) has no width / height of its own (#547)
  pg <- doc$document$page
  size <- if (!is.null(pg$width_in) || !is.null(pg$height_in)) {
    sprintf("%s x %s in", pg$width_in %||% "?", pg$height_in %||% "?")
  } else {
    sprintf("%s %s", pg$paper_size %||% "letter", pg$orientation %||% "landscape")
  }
  hp <- doc$document$default_format$font_size_half_points %||%
    .opt("rtfreporter.font_size_half_points")
  sprintf("%s, %s pt", size, format(hp / 2))
}

# The lines print() writes: one per block, its page's number on the page's
# first; columns padded to line up
.rtf_pages_lines <- function(x) {
  rows <- list()
  for (p in x$pages) {
    for (si in seq_along(p$segments)) {
      s <- p$segments[[si]]
      first <- si == 1L
      sec <- if (first && isTRUE(p$section_start)) {
        if (is.null(p$section_label)) sprintf("[sec %d]", p$section)
        else sprintf("[sec %d \"%s\"]", p$section, p$section_label)
      } else ""
      grp <- x$group_pages[[as.character(s$group)]]
      what <- .rtf_content_desc(s$content)
      if (!is.null(grp) && grp > 1L) what <- sprintf("%s (%d/%d)", what, s$page_in_group, grp)
      extra <- c(if (.rtf_text_rows(s$title)) sprintf("title %d", .rtf_text_rows(s$title)),
                 if (.rtf_text_rows(s$footnote)) sprintf("foot %d", .rtf_text_rows(s$footnote)),
                 if (!first && s$space_before > 0L) sprintf("space %d", s$space_before))
      rows[[length(rows) + 1L]] <- c(if (first) sprintf("page %d", p$number) else "+",
                                     sec, s$name %||% "", what,
                                     paste(extra, collapse = "  "))
    }
  }
  if (!length(rows)) return(character(0))
  m <- do.call(rbind, rows)
  # a block on the page before it: "+" under the end of "page n"
  w <- max(nchar(m[, 1L]))
  m[, 1L] <- ifelse(m[, 1L] == "+", formatC("+", width = w),
                    formatC(m[, 1L], width = w, flag = "-"))
  for (j in 2:4) m[, j] <- formatC(m[, j], width = max(nchar(m[, j])), flag = "-")
  # (an empty column takes no room: two spaces apart, never four)
  m <- m[, colSums(nchar(m) > 0L) > 0L, drop = FALSE]
  sub(" +$", "", paste0(" ", apply(m, 1L, paste, collapse = "  ")))
}

# How many pages each group (table) has, for the "(1/2)" of print()
.rtf_group_pages <- function(x) {
  g <- unlist(lapply(x$pages, function(p) vapply(p$segments, function(s) s$group, 1L)))
  if (!length(g)) return(list())
  as.list(table(g))
}

.rtf_pages_print <- function(x, what) {
  n_pages <- length(x$pages)
  if (!n_pages) {
    cat(sprintf("%s: no contents yet (%s)\n", what, x$paper))
    return(invisible())
  }
  x$group_pages <- .rtf_group_pages(x)
  cat(sprintf("%s: %d block%s on %d page%s (%s)\n", what, x$n_blocks,
              if (x$n_blocks == 1L) "" else "s", n_pages, if (n_pages == 1L) "" else "s",
              x$paper))
  cat(.rtf_pages_lines(x), sep = "\n")
  invisible()
}

#' @rdname rtf_layout
#' @param x An `rtf_pages` object.
#' @param ... Unused.
#' @export
print.rtf_pages <- function(x, ...) {
  .rtf_pages_print(x, "rtf_pages")
  invisible(x)
}
