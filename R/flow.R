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
#  blocks into printed pages, each the segments on it (R/layout.R).  A page
#  of one segment is drawn as it always was; a page of several is drawn
#  segment after segment here.

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

# ---- drawing a page of several contents ---------------------------------------

# Each block as a page of one draws its title, content and footnote (the
# same renderers, the same arguments), with a separating paragraph between
# blocks: RTF merges table rows no paragraph separates.  `ctx`: the
# document's resolved settings, from generate_rtfreport().
.render_flow_blocks <- function(blocks, p_idx, ctx) {
  doc <- ctx$doc
  writable_w <- ctx$writable_w
  out <- character(0)
  sep <- .load_rtf_commands()$paragraph$flow_gap
  for (bi in seq_along(blocks)) {
    b <- blocks[[bi]]
    ct <- b$content
    if (bi > 1L) {
      out <- c(out, sep, rep(.load_rtf_commands()$paragraph$flow_blank_line,
                             b$space_before %||% 0L))
    }
    content_w     <- .content_width_twips(ct, writable_w)
    content_align <- .content_align(ct)
    title_w    <- .resolve_block_width(ctx$doc_title_width,    content_w, writable_w)
    footnote_w <- .resolve_block_width(ctx$doc_footnote_width, content_w, writable_w)
    out <- c(out, if (identical(ctx$doc_title_format, "text")) {
      .render_text_block_text(b$title, is_footer = FALSE, style = doc$title_style,
                              font_half_points = ctx$font_half_points,
                              font_index_map = ctx$font_index_map,
                              ctx$color_index_map, markup = ctx$doc_markup,
                              pad_l = ctx$doc_pad_l, pad_r = ctx$doc_pad_r,
                              current_page = p_idx, total_pages = ctx$total_pages)
    } else {
      .render_text_block_table(
        b$title, title_w, is_footer = FALSE, ctx$font_half_points,
        ctx$doc_pad_l, ctx$doc_pad_r, "\\clvertalt", content_align, ctx$color_index_map,
        doc_row_height = ctx$doc_row_height, markup = ctx$doc_markup,
        style = doc$title_style, font_index_map = ctx$font_index_map,
        current_page = p_idx, total_pages = ctx$total_pages)
    })
    if (inherits(ct, "rtftable")) {
      rows <- .render_rtftable(ct, writable_w, ctx$font_half_points,
                               font_index_map = ctx$font_index_map,
                               ctx$color_index_map,
                               doc_row_height = ctx$doc_row_height,
                               doc_pad_l = ctx$doc_pad_l, doc_pad_r = ctx$doc_pad_r,
                               doc_markup = ctx$doc_markup)
      out <- c(out, rows)
    } else if (inherits(ct, "rtfplot")) {
      out <- c(out, .render_rtfplot(ct, writable_w))
    }
    footnote_st <- doc$footnote_style
    out <- c(out, if (identical(ctx$doc_footnote_format, "text")) {
      .render_text_block_text(
        .footnote_rows(b$footnote, footnote_st$align %||% "left", "text"),
        is_footer = TRUE, style = footnote_st,
        font_half_points = ctx$font_half_points,
        ctx$color_index_map, markup = ctx$doc_markup,
        pad_l = ctx$doc_pad_l, pad_r = ctx$doc_pad_r,
        current_page = p_idx, total_pages = ctx$total_pages)
    } else {
      fn_rtf <- .render_header_footer(
        .footnote_hf(b$footnote, footnote_st, footnote_w),
        footnote_w, is_footer = TRUE,
        current_page = p_idx, total_pages = ctx$total_pages,
        color_index_map = ctx$color_index_map,
        font_half_points = ctx$font_half_points,
        doc_row_height = ctx$doc_row_height,
        doc_pad_l = ctx$doc_pad_l, doc_pad_r = ctx$doc_pad_r,
        doc_markup = ctx$doc_markup,
        font_index_map = ctx$font_index_map)
      if (length(fn_rtf)) c(sep, fn_rtf) else fn_rtf
    })
  }
  out
}
