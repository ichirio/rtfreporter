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
#  page_break / space_before (R/blocks.R); rtf_layout() turns the blocks
#  into printed pages (R/layout.R).  A page of one block is drawn as it
#  always was; a page of several reaches the renderer as an
#  `rtf_flow_page` (its blocks, first to last), drawn block after block
#  here.

# Which pages of a call begin a content: every page, but a table's own
# later pages (as_rtftables() marks them, `rtf_paginate_meta$page_index`),
# and -- a plan or another source expanded here -- only its first.
.flow_item_start <- function(tables, expanded) {
  n <- length(tables)
  if (isTRUE(expanded)) return(seq_len(n) == 1L)
  vapply(tables, function(x) {
    d <- if (inherits(x, "rtftable")) x$data %||% x$data_list[[1L]] else x
    m <- attr(d, "rtf_paginate_meta", exact = TRUE) %||%
      attr(x, "rtf_paginate_meta", exact = TRUE)
    pi <- m$page_index
    is.null(pi) || isTRUE(as.integer(pi)[1L] <= 1L)
  }, logical(1L))
}

# ---- first_rows: the rows the shared page takes -------------------------------

# rtf_tables(first_rows = ): the table as pages, its first `first_rows`
# rows the first page and the rest paginated as the table says.  A plan
# takes it as a pages setting (its own max_rows / split stay); a data frame
# goes through as_rtftables(); a built rtftable is cut there, its rest one
# page.  Pages already built (as_rtftables()'s list) are past it.
.flow_first_rows <- function(tables, first_rows) {
  if (!is.numeric(first_rows) || length(first_rows) != 1L || is.na(first_rows) ||
      first_rows < 1 || first_rows != round(first_rows)) {
    stop("rtf_tables(): `first_rows` is a whole number of rows, 1 or more.", call. = FALSE)
  }
  first_rows <- as.integer(first_rows)
  if (inherits(tables, "table_plan")) {
    return(as_rtftables(.plan_layer(tables, "pages", list(first_rows = first_rows))))
  }
  one <- if (is.data.frame(tables) || inherits(tables, "rtftable")) tables
         else if (is.list(tables) && length(tables) == 1L) tables[[1L]]
  if (is.data.frame(one)) return(as_rtftables(one, first_rows = first_rows))
  if (inherits(one, "rtftable") && !is.null(one$data)) {
    if (first_rows >= nrow(one$data)) return(list(one))
    return(.rtftable_cut(one, first_rows))
  }
  stop(paste0("rtf_tables(): `first_rows` takes one table -- a table_plan, a data frame ",
              "or an rtftable;\n  pages already built take it where they are built: ",
              "as_rtftables(first_rows = )."), call. = FALSE)
}

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

# ---- a block of a page of several -----------------------------------------------

.flow_block <- function(content, title, footnote, space_before = 0L) {
  list(content = content, title = title, footnote = footnote,
       space_before = as.integer(space_before %||% 0L))
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
