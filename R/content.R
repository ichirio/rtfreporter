# ---------------------------------------------------------------------------
#  .rtf_content(): anything that can be content, as content pages
# ---------------------------------------------------------------------------
#  The one way in for rtf_tables() and rtf_figures(): a source becomes a list
#  of content pages (each an rtftable or an rtfplot), with two attributes
#  set here, where the pages are made, so that nothing further on has to
#  guess them from page names or pagination metadata:
#
#    group          1, 2, ... within the list: the pages of one table share it
#    page_in_group  which page of its table each page is
#
#  A new kind of table registers an as_rtftables() method (the hub, R/
#  as_rtftables.R); a new kind of figure is rtfplot()'s to draw.  The
#  document side is not touched for either.
#
#  `first_rows` (rtf_tables(page_break = FALSE, first_rows = )) cuts the
#  table where it is added: its first rows the page it shares, the rest
#  paginated as the table says (.content_split()).

.rtf_content <- function(x, ..., first_rows = NULL) {
  if (!is.null(first_rows)) {
    return(.rtf_one_table(.content_split(x, .check_first_rows(first_rows))))
  }
  UseMethod(".rtf_content")
}

# The pages, as .rtf_content() returns them
.rtf_content_pages <- function(pages, group, page_in_group) {
  attr(pages, "group") <- as.integer(group)
  attr(pages, "page_in_group") <- as.integer(page_in_group)
  pages
}

# The pages of one table: one group, its pages 1, 2, ...
.rtf_one_table <- function(pages) {
  .rtf_content_pages(pages, rep(1L, length(pages)), seq_along(pages))
}

# A plan: its pages, as plan_apply(x, "pages") makes them
.rtf_content.table_plan <- function(x, ...) {
  .rtf_one_table(as_rtftables(x))
}

# A built page is one page.  One that as_rtftables() marked a later page
# of its table (`rtf_paginate_meta$page_index`) says so, so that a list of
# built pages keeps its tables together.
.rtf_content.rtftable <- function(x, ...) {
  d <- x$data %||% x$data_list[[1L]]
  m <- attr(d, "rtf_paginate_meta", exact = TRUE) %||%
    attr(x, "rtf_paginate_meta", exact = TRUE)
  pi <- suppressWarnings(as.integer(m$page_index)[1L])
  .rtf_content_pages(list(x), 1L, if (length(pi) && !is.na(pi) && pi > 1L) pi else 1L)
}

.rtf_content.rtfplot <- function(x, ...) {
  .rtf_content_pages(list(x), 1L, 1L)
}

# A bare data frame: one simple page with rtftable()'s defaults
.rtf_content.data.frame <- function(x, ...) {
  .rtf_content_pages(list(rtftable(x)), 1L, 1L)
}

# A list: each element's pages, one after another.  An element that is a
# later page of a table (page_in_group > 1) goes on the table before it;
# any other begins a table of its own.  An element's error names it.
.rtf_content.list <- function(x, ...) {
  pages <- list()
  group <- integer(0)
  in_group <- integer(0)
  nms <- character(0)
  x_names <- names(x)
  for (i in seq_along(x)) {
    p <- tryCatch(.rtf_content(x[[i]], ...), error = function(e) {
      stop("Item ", i, ": ", conditionMessage(e), call. = FALSE)
    })
    g <- attr(p, "group")
    pig <- attr(p, "page_in_group")
    n <- length(pages)
    last <- if (n) group[n] else 0L
    if (n && length(p) == 1L && pig[1L] > 1L) {
      g <- last
      pig <- in_group[n] + 1L
    } else {
      g <- g + last
    }
    pages <- c(pages, unname(p))
    group <- c(group, g)
    in_group <- c(in_group, pig)
    # an element of one page is named by the list; one of several keeps
    # its pages' own names
    nms <- c(nms, if (length(p) == 1L) x_names[i] %||% "" else names(p) %||% rep("", length(p)))
  }
  if (!is.null(x_names) || any(nzchar(nms))) names(pages) <- nms
  .rtf_content_pages(pages, group, in_group)
}

# Anything else: a source a package registered an as_rtftables() method
# for (gt, gtsummary, rtables, rlistings, flextable, huxtable, ...) becomes
# its pages, one table; the rest is a figure -- rtfplot() knows what can be
# drawn, and says what cannot.  `width_twips` / `height_twips` / `align`
# and `draw_args` (render_width, render_height, render_dpi, png_device) are
# how to show and draw it; a file path is never handed `draw_args`.
.rtf_content.default <- function(x, ..., width_twips = NULL, height_twips = NULL,
                                 align = "center", draw_args = list()) {
  if (.has_rtftables_method(x)) return(.rtf_one_table(as_rtftables(x)))
  fig <- do.call(rtfplot, c(list(x, width_twips = width_twips,
                                 height_twips = height_twips, align = align),
                            if (!is.character(x)) draw_args))
  .rtf_content_pages(list(fig), 1L, 1L)
}

# ---- first_rows: the rows the shared page takes -------------------------------

.check_first_rows <- function(first_rows) {
  if (!is.numeric(first_rows) || length(first_rows) != 1L || is.na(first_rows) ||
      first_rows < 1 || first_rows != round(first_rows)) {
    stop("rtf_tables(): `first_rows` is a whole number of rows, 1 or more.", call. = FALSE)
  }
  as.integer(first_rows)
}

# One table as pages, its first `k` rows the first page and the rest
# paginated as the table says.  A plan takes it as a pages setting (its own
# max_rows / split stay); a data frame goes through as_rtftables(); a built
# rtftable is cut there, its rest one page; a list of one is its one.  Pages
# already built (as_rtftables()'s list) are past it.
.content_split <- function(x, k) {
  UseMethod(".content_split")
}

.content_split.table_plan <- function(x, k) {
  as_rtftables(.plan_layer(x, "pages", list(first_rows = k)))
}

.content_split.data.frame <- function(x, k) {
  as_rtftables(x, first_rows = k)
}

.content_split.rtftable <- function(x, k) {
  if (is.null(x$data)) .content_split_refused()
  if (k >= nrow(x$data)) return(list(x))
  .rtftable_cut(x, k)
}

.content_split.list <- function(x, k) {
  if (length(x) != 1L) .content_split_refused()
  .content_split(x[[1L]], k)
}

.content_split.default <- function(x, k) {
  .content_split_refused()
}

.content_split_refused <- function() {
  stop(paste0("rtf_tables(): `first_rows` takes one table -- a table_plan, a data frame ",
              "or an rtftable;\n  pages already built take it where they are built: ",
              "as_rtftables(first_rows = )."), call. = FALSE)
}
