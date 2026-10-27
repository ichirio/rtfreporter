# ---------------------------------------------------------------------------
#  Blocks: a document's contents, one content and its placement each
# ---------------------------------------------------------------------------
#  rtf_tables() and rtf_figures() turn what they are given into content
#  pages and add each as an `rtf_block` to `doc$contents`, an
#  `rtf_contents` (a list of blocks).  A block carries everything about its
#  one content: the title above it and the footnote below it, whether it
#  begins a page (`page_break`) and how many empty lines sit before it
#  (`space_before`), and which table it is a page of (`group`,
#  `page_in_group`).  Nothing beside the blocks keeps a list of its own, so
#  the titles cannot fall out of step with the contents.  rtf_layout() turns
#  the blocks into printed pages (R/layout.R).

# One block.  `title` / `footnote` are text blocks (NULL: none).
# `cont_title`: the line rtf_layout() adds under the title where the block
# is a later page of its table.  `section`: list(label =, align =) where
# rtf_tables(auto_section = TRUE) opens a section with the block's heading,
# else NULL; rtf_layout() decides the page's section from it (§12.3 of the
# layout design: the first block on a page that has one).
.rtf_block <- function(content, group = 1L, page_in_group = 1L,
                       title = NULL, footnote = NULL, page_break = TRUE,
                       space_before = 0L, cont_title = NULL, section = NULL) {
  structure(list(content       = content,
                 group         = as.integer(group),
                 page_in_group = as.integer(page_in_group),
                 title         = title,
                 footnote      = footnote,
                 page_break    = isTRUE(page_break),
                 space_before  = as.integer(space_before),
                 cont_title    = cont_title,
                 section       = section),
            class = "rtf_block")
}

# The blocks of a document, first to last (names: the page names
# rtf_tables() was given, as they always rode on `contents`)
.rtf_contents <- function(blocks = list()) {
  structure(blocks, class = "rtf_contents")
}

#' @export
`[.rtf_contents` <- function(x, i) {
  .rtf_contents(unclass(x)[i])
}

#' @export
length.rtf_contents <- function(x) {
  length(unclass(x))
}

# ---- adding blocks ----------------------------------------------------------------

# rtf_tables() / rtf_figures(): the titles or footnotes given for n
# contents -- NULL, one block for every content, or one block each.  A
# character vector is one block (its lines), for every content (#594).
# `unit` names the contents in the message ("page", "figure").
.rtf_block_texts <- function(x, n, name, unit = "page") {
  if (is.null(x)) return(rep(list(NULL), n))
  if (is.character(x)) x <- list(x)
  if (!is.list(x)) {
    stop(sprintf("`%s` must be a list (or NULL).", name), call. = FALSE)
  }
  if (length(x) == 1L && n > 1L) return(rep(x, n))  # one block = common to all
  if (length(x) != n) {
    one <- if (identical(unit, "page")) "one block (a list of length 1)" else "one block"
    stop(sprintf(paste0("`%s` has %d blocks but there %s %d %s%s: give ",
                        "one block per %s, or %s for every %s."),
                 name, length(x), if (n == 1L) "is" else "are", n, unit,
                 if (n == 1L) "" else "s", unit, one, unit), call. = FALSE)
  }
  x
}

# The one place rtf_tables() and rtf_figures() add to a document: each
# content page (from .rtf_content(), R/content.R) becomes a block with its
# title, footnote and placement.  The pages say which table each is a page
# of (`group`, `page_in_group`); a table's later pages always begin a page.
# `cont_title`: one line for every block; `sections`: one entry per page.
.rtf_add_blocks <- function(doc, contents, titles, footnotes,
                            page_break = TRUE, space_before = 0L,
                            fn = "rtf_tables", cont_title = NULL,
                            sections = NULL) {
  n_new <- length(contents)
  if (!is.logical(page_break) || anyNA(page_break) ||
      !length(page_break) %in% c(1L, n_new)) {
    stop(sprintf(paste0("%s(): `page_break` is TRUE or FALSE, one for every ",
                        "page or one each (%d)."), fn, n_new), call. = FALSE)
  }
  if (!is.numeric(space_before) || length(space_before) != 1L ||
      is.na(space_before) || space_before < 0 || space_before != round(space_before)) {
    stop(sprintf("%s(): `space_before` is a whole number of empty lines, 0 or more.", fn),
         call. = FALSE)
  }
  in_group <- attr(contents, "page_in_group") %||% rep(1L, n_new)
  pb <- rep_len(page_break, n_new)
  pb[in_group > 1L] <- TRUE
  old <- doc[["contents"]]
  # the groups go on from the document's: a call's first content begins one
  first <- if (length(old)) max(vapply(old, function(b) b$group, 1L)) else 0L
  group <- first + (attr(contents, "group") %||% seq_len(n_new))
  blocks <- vector("list", n_new)
  for (i in seq_len(n_new)) {
    blocks[[i]] <- .rtf_block(contents[[i]], group = group[i], page_in_group = in_group[i],
                              title = titles[[i]], footnote = footnotes[[i]],
                              page_break = pb[i], space_before = space_before,
                              cont_title = cont_title, section = sections[[i]])
  }
  names(blocks) <- names(contents)
  doc$contents <- .rtf_contents(c(unclass(old), blocks))
  doc
}

# rtf_titles() / rtf_footnotes(): the one field of every block.  `[<-`
# with a list keeps a NULL as the field's value (`$<-` would drop it).
.rtf_blocks_set <- function(blocks, field, values) {
  for (i in seq_along(blocks)) blocks[[i]][field] <- list(values[[i]])
  blocks
}

# ---- printing -----------------------------------------------------------------

# What a content is, in a few words: its kind and its rows or size
.rtf_content_desc <- function(ct) {
  if (inherits(ct, "rtftable")) {
    rows <- if (!is.null(ct$data)) nrow(ct$data)
            else sum(vapply(ct$data_list, nrow, integer(1L)))
    return(sprintf("rtftable %d row%s", rows, if (rows == 1L) "" else "s"))
  }
  if (inherits(ct, "rtfplot")) {
    d <- .rtfplot_display_twips(ct)
    return(sprintf("rtfplot %.1f x %.1f in", d$w / 1440, d$h / 1440))
  }
  class(ct)[1L]
}

# How many rows a title or footnote block has
.rtf_text_rows <- function(x) {
  if (is.null(x)) 0L else length(x)
}

#' @export
print.rtf_block <- function(x, ...) {
  cat(sprintf("<rtf_block> %s (group %d, page %d)\n", .rtf_content_desc(x[["content"]]),
              x$group, x$page_in_group))
  cat(sprintf("  page_break %s, space_before %d, title %d row(s), footnote %d row(s)\n",
              x$page_break, x$space_before, .rtf_text_rows(x[["title"]]),
              .rtf_text_rows(x[["footnote"]])))
  if (!is.null(x[["section"]])) {
    cat(sprintf("  opens the auto section \"%s\"\n", x[["section"]][["label"]]))
  }
  invisible(x)
}

#' @export
print.rtf_contents <- function(x, ...) {
  n <- length(x)
  cat(sprintf("<rtf_contents> %d block%s\n", n, if (n == 1L) "" else "s"))
  nms <- names(x)
  for (i in seq_len(n)) {
    b <- x[[i]]
    nm <- if (is.null(nms) || is.na(nms[i]) || !nzchar(nms[i])) "" else
            paste0(" ", .page_name_base(nms[i]))
    cat(sprintf(" %d%s: %s%s\n", i, nm, .rtf_content_desc(b[["content"]]),
                if (isTRUE(b$page_break)) "" else "  (goes on the page before)"))
  }
  invisible(x)
}
