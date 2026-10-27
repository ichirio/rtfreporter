# ---------------------------------------------------------------------------
#  Flow placement: a content on the page the one before it ends (#711)
# ---------------------------------------------------------------------------
#  rtf_tables(page_break = FALSE) / rtf_figures(page_break = FALSE) put a
#  content under the one before it, on the page that one ends on.  Nothing
#  is measured: what does not fit, the reader (Word, LibreOffice) carries to
#  the next page -- fitting a page is the program's to arrange.  A table
#  that should go on with its title, header and footnote on pages of its
#  own says how many of its rows the shared page takes, `first_rows`: the
#  rest is paginated by its own `max_rows` (as_rtftables(first_rows = ),
#  .paginate_df()).
#
#  Each content page is a block of the document's `contents`, with its
#  page_break / space_before (R/blocks.R); `first_rows` cuts the table where
#  it is added (.content_split(), R/content.R).  rtf_layout() turns the
#  blocks into printed pages, each the segments on it (R/layout.R), and a
#  page is drawn segment after segment (R/render_page.R).

# ---- cutting a built table ------------------------------------------------------

# Rows 1..k and k+1..n of a one-data-frame rtftable: the same columns,
# headers, widths and styles; the body rows, their cell styles and the blank
# rows divided (a blank row at the cut is dropped)
.rtftable_cut <- function(tbl, k) {
  if (!inherits(tbl, "rtftable") || is.null(tbl$data)) {
    stop("only a table of one data frame is cut.", call. = FALSE)
  }
  n <- nrow(tbl$data)
  k <- as.integer(k)
  if (k < 1L || k >= n) stop("the cut is within the rows: 1 to n - 1.", call. = FALSE)
  part <- function(rows, first) {
    x <- tbl
    d <- tbl$data[rows, , drop = FALSE]
    rownames(d) <- NULL
    for (a in c("rtf_titles", "rtf_footnotes", "rtf_paginate_meta")) {
      attr(d, a) <- attr(tbl$data, a, exact = TRUE)
    }
    x$data <- d
    if (!is.null(tbl$cell_styles) && length(tbl$cell_styles) == n) {
      x$cell_styles <- tbl$cell_styles[rows]
    }
    if (!is.null(tbl$blank_rows)) {
      b <- tbl$blank_rows
      x$blank_rows <- if (first) b[b < k] else b[b > k] - k
    }
    x
  }
  list(part(seq_len(k), TRUE), part(seq.int(k + 1L, n), FALSE))
}
