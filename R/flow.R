# ---------------------------------------------------------------------------
#  Flow placement: a content on the page the one before it ends (#flow)
# ---------------------------------------------------------------------------
#  rtf_tables(page_break = FALSE) puts a table on what is left of the page
#  the previous content ends on; what does not fit goes on the next pages,
#  their column headers drawn again.  The document keeps one entry a content
#  page in `contents` (as rtf_tables() expands them), and beside it
#  `placement`: each page's page_break / min_rows / space_before /
#  cont_title.  A document with no `placement` -- every one written before
#  this, and every one whose contents all break -- never reaches this file:
#  .pipe_doc_to_rtfreport() and the renderer take their old paths, byte for
#  byte.
#
#  .flow_layout() turns the content pages into physical pages: a page of one
#  content stays as it was; a page of several is an `rtf_flow_page` (its
#  blocks, first to last), which the renderer draws block after block.
#  Where a content goes is decided by its height, estimated from the page's
#  geometry and the table's rows (an estimate on the safe side: Word, not
#  this, lays the page out).

.flow_defaults <- function() {
  list(page_break = TRUE, min_rows = 3L, space_before = 0L, cont_title = NULL)
}

# rtf_tables() / rtf_figures(): the placement of the pages just added.
# `item_start`: which of them begin a content -- a table's own later pages
# always begin a page.  Nothing is written when every page breaks and the
# document has no placement yet.
.flow_placement_append <- function(doc, n_new, item_start, page_break = TRUE,
                                   min_rows = 3L, space_before = 0L,
                                   cont_title = NULL, fn = "rtf_tables") {
  if (!is.logical(page_break) || anyNA(page_break) ||
      !length(page_break) %in% c(1L, n_new)) {
    stop(sprintf(paste0("%s(): `page_break` is TRUE or FALSE, one for every ",
                        "page or one each (%d)."), fn, n_new), call. = FALSE)
  }
  if (!is.numeric(min_rows) || length(min_rows) != 1L || is.na(min_rows) ||
      min_rows < 1 || min_rows != round(min_rows)) {
    stop(sprintf("%s(): `min_rows` is a whole number, 1 or more.", fn), call. = FALSE)
  }
  if (!is.numeric(space_before) || length(space_before) != 1L ||
      is.na(space_before) || space_before < 0 || space_before != round(space_before)) {
    stop(sprintf("%s(): `space_before` is a whole number of empty lines, 0 or more.", fn),
         call. = FALSE)
  }
  if (!is.null(cont_title) &&
      (!is.character(cont_title) || length(cont_title) != 1L || is.na(cont_title))) {
    stop(sprintf("%s(): `cont_title` is one line of text, or NULL.", fn), call. = FALSE)
  }
  pb <- rep_len(page_break, n_new)
  pb[!item_start] <- TRUE
  if (all(pb) && is.null(doc$placement)) return(doc)
  n_old <- length(doc$contents) - n_new
  if (is.null(doc$placement)) doc$placement <- rep(list(.flow_defaults()), n_old)
  new <- lapply(seq_len(n_new), function(i) {
    list(page_break = pb[i], min_rows = as.integer(min_rows),
         space_before = as.integer(space_before), cont_title = cont_title)
  })
  doc$placement <- c(doc$placement, new)
  doc
}

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

.flow_active <- function(doc) {
  pl <- doc$placement
  !is.null(pl) && any(!vapply(pl, function(p) isTRUE(p$page_break), logical(1L)))
}

# ---- heights ------------------------------------------------------------------

# What the estimates read from the document: the page's body height, the
# writable width, the font, the default row height, the title / footnote
# forms -- resolved as the renderer resolves them.
.flow_metrics <- function(pipe_doc, report) {
  pg <- report$document$default_page %||% .new_rtfreport()$document$default_page
  df <- pipe_doc$document$default_format %||% list()
  fhp <- as.integer(df$font_size_half_points %||% .opt("rtfreporter.font_size_half_points"))
  doc_rh <- df$row_height_twips %||% .opt("rtfreporter.row_height_twips")
  rh <- as.integer(doc_rh %||% .default_row_height_twips(fhp))
  hf_rows <- function(which) {
    n <- vapply(pipe_doc$sections, function(s) {
      h <- s[[which]]
      if (is.null(h)) 0L else length(h$rows %||% h)
    }, integer(1L))
    if (length(n)) max(n) else 0L
  }
  safety_rows <- as.numeric(getOption("rtfreporter.flow.safety_rows", 1))
  body <- as.numeric(pg$height_twips) - as.numeric(pg$margin_top_twips) -
    as.numeric(pg$margin_bottom_twips) - (hf_rows("header") + hf_rows("footer")) * rh
  body <- body - safety_rows * rh - 0.05 * body
  pad_res <- .load_rtfreporter_defaults()
  list(body_h = body, rh = rh, fhp = fhp, doc_rh = doc_rh,
       writable_w = as.numeric(pg$width_twips) - as.numeric(pg$margin_left_twips) -
         as.numeric(pg$margin_right_twips),
       pad = as.integer(df$cell_padding_left_twips %||% .opt("rtfreporter.cell_padding_left_twips") %||%
                          pad_res$default_cell_padding_left_twips %||% 0L) +
         as.integer(df$cell_padding_right_twips %||% .opt("rtfreporter.cell_padding_right_twips") %||%
                      pad_res$default_cell_padding_right_twips %||% 0L),
       title_format = .resolve_text_block_format(
         df$title_format %||% .opt("rtfreporter.title_format"), "text"),
       footnote_format = .resolve_text_block_format(
         df$footnote_format %||% .opt("rtfreporter.footnote_format"), "table"),
       wrap = as.numeric(getOption("rtfreporter.flow.wrap_factor", 1)))
}

# A title / footnote block's lines (NULL: the title's one blank line)
.flow_text_lines <- function(block, is_footer) {
  if (is.null(block)) return(if (is_footer) 0L else 1L)
  rows <- .normalize_text_block(block, is_footer)
  txt <- vapply(rows, function(r) paste(r$text %||% r, collapse = "\n"), "")
  sum(lengths(strsplit(txt, "\n", fixed = TRUE)) + (!nzchar(txt)))
}

# A table's rows: the header's height and each body row's (twips), a row as
# many lines as its widest-wrapping cell needs
.flow_table_heights <- function(tbl, m) {
  df <- tbl$data
  metrics <- .resolve_element_metrics(tbl$font_size_half_points, tbl$row_height_twips,
                                      m$fhp, m$doc_rh)
  rh <- as.numeric(metrics$rh %||% m$rh)
  if (!rh) rh <- m$rh
  fs <- metrics$fs %||% m$fhp
  hdr_h <- as.numeric(tbl$header_row_height_twips %||% rh)
  blank_h <- as.numeric(tbl$blank_row_height_twips %||% rh)
  cellx <- .compute_cellx(ncol(df), m$writable_w, tbl)
  w_in <- pmax(diff(c(0, cellx)) - (tbl$cell_padding_left_twips %||% 0) -
                 (tbl$cell_padding_right_twips %||% 0) - m$pad, 144) / 1440
  lines_of <- function(cells) {
    cells <- ifelse(is.na(cells), "", as.character(cells))
    parts <- strsplit(cells, "\n", fixed = TRUE)
    max(1, vapply(seq_along(parts), function(j) {
      p <- parts[[j]]
      if (!length(p)) return(1)
      sum(pmax(1, ceiling(m$wrap * text_width_in(p, tbl$font, fs) / w_in[j])))
    }, 1))
  }
  exact <- isTRUE(tbl$row_height_exact)
  body <- if (!nrow(df)) numeric(0) else vapply(seq_len(nrow(df)), function(i) {
    (if (exact) 1 else lines_of(unlist(df[i, ], use.names = FALSE))) * rh
  }, 1)
  heads <- tbl$col_header %||% list(names(df))
  head_h <- sum(vapply(heads, function(h) {
    tx <- if (is.list(h)) vapply(h, function(z) paste(z$label %||% z$text %||% z, collapse = " "), "")
          else as.character(h)
    if (length(tx) != ncol(df)) return(hdr_h)
    (if (exact) 1 else lines_of(tx)) * hdr_h
  }, 1)) + if (!is.null(tbl$spanning_header)) hdr_h else 0
  list(head = head_h, body = body, blank = length(tbl$blank_rows %||% integer(0)) * blank_h)
}

# One block's height (twips): its title, its content, its footnote
.flow_block_height <- function(content, title, footnote, m) {
  h <- .flow_text_lines(title, FALSE) * m$rh
  fl <- .flow_text_lines(footnote, TRUE)
  if (fl) h <- h + fl * m$rh + (if (identical(m$footnote_format, "table")) 40 else 0)
  if (inherits(content, "rtfplot")) {
    return(h + as.numeric(.rtfplot_display_twips(content)$h) + m$rh)
  }
  if (inherits(content, "rtftable") && !is.null(content$data)) {
    th <- .flow_table_heights(content, m)
    return(h + th$head + sum(th$body) + th$blank)
  }
  if (inherits(content, "rtftable")) {
    # several data frames: each its header and rows
    return(h + sum(vapply(content$data_list, function(d) {
      t1 <- content
      t1$data <- d
      t1$data_list <- NULL
      t1$col_header <- NULL
      th <- .flow_table_heights(t1, m)
      th$head + sum(th$body)
    }, 1)))
  }
  h
}

# ---- cutting a table ------------------------------------------------------------

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

# The rows of a table that fit in `room` twips under its own title, header
# and footnote; a group's heading is not left last on the page
.flow_rows_fit <- function(tbl, title, footnote, room, m) {
  th <- .flow_table_heights(tbl, m)
  fixed <- .flow_block_height(NULL, title, footnote, m) + th$head
  k <- sum(cumsum(th$body) <= room - fixed)
  if (k > 0L && k < nrow(tbl$data)) {
    gi <- .compute_group_info(tbl$data, 1L, "auto")
    if (!identical(gi$mode, "value")) {
      while (k > 0L && isTRUE(gi$headers[k])) k <- k - 1L
    }
  }
  k
}

# ---- the layout -----------------------------------------------------------------

.flow_block <- function(content, title, footnote, space_before = 0L) {
  list(content = content, title = title, footnote = footnote,
       space_before = as.integer(space_before %||% 0L))
}

# The content pages -> physical pages.  Returns the document with
# `contents` / `titles` / `footnotes` one entry a physical page and no
# `placement`; a page of several blocks is an `rtf_flow_page` (inside the
# auto-section wrapper of its first content when that had one).
.flow_layout <- function(pipe_doc, report) {
  m <- .flow_metrics(pipe_doc, report)
  pages <- list()
  used <- 0
  open_page <- function(block, wrap) {
    pages[[length(pages) + 1L]] <<- list(blocks = list(block), wrap = wrap)
    used <<- .flow_block_height(block$content, block$title, block$footnote, m)
  }
  add_block <- function(block, h) {
    k <- length(pages)
    pages[[k]]$blocks[[length(pages[[k]]$blocks) + 1L]] <<- block
    used <<- used + h
  }
  for (i in seq_along(pipe_doc$contents)) {
    item <- pipe_doc$contents[[i]]
    pl <- pipe_doc$placement[[i]] %||% .flow_defaults()
    title <- if (i <= length(pipe_doc$titles)) pipe_doc$titles[[i]] else NULL
    footnote <- if (i <= length(pipe_doc$footnotes)) pipe_doc$footnotes[[i]] else NULL
    flow <- !isTRUE(pl$page_break) && length(pages) > 0L
    wrap <- NULL
    if (inherits(item, "rtf_auto_section_item")) {
      if (flow) {
        warning(sprintf(paste0("rtf_tables(page_break = FALSE): '%s' goes on the page ",
                               "before it, so it opens no section of its own; ",
                               "auto_title names it."), item$label), call. = FALSE)
      } else {
        wrap <- item
      }
      item <- .unwrap_auto_section_item(item)
    }
    block <- .flow_block(item, title, footnote, pl$space_before)
    if (!flow) {
      open_page(block, wrap)
      next
    }
    gap <- 40 + block$space_before * m$rh
    room <- m$body_h - used - gap
    h <- .flow_block_height(item, title, footnote, m)
    if (h <= room) {
      add_block(block, gap + h)
      next
    }
    k <- if (inherits(item, "rtftable") && !is.null(item$data) && nrow(item$data) > 1L) {
      .flow_rows_fit(item, title, footnote, room, m)
    } else 0L
    if (k >= pl$min_rows && k < nrow(item$data)) {
      cut <- .rtftable_cut(item, k)
      add_block(.flow_block(cut[[1L]], title, footnote, pl$space_before), gap + h)
      rest_title <- title
      if (!is.null(pl$cont_title)) {
        rows <- if (is.null(title)) list() else if (is.list(title)) title else as.list(title)
        rest_title <- c(rows, list(list(text = pl$cont_title, align = "left")))
      }
      open_page(.flow_block(cut[[2L]], rest_title, footnote, 0L), NULL)
    } else {
      open_page(.flow_block(item, title, footnote, 0L), NULL)
    }
  }
  out <- pipe_doc
  out$placement <- NULL
  out$contents <- lapply(pages, function(p) {
    ct <- if (length(p$blocks) == 1L) p$blocks[[1L]]$content
          else structure(list(blocks = p$blocks), class = "rtf_flow_page")
    if (is.null(p$wrap)) return(ct)
    w <- p$wrap
    w$content <- ct
    w
  })
  out$titles <- lapply(pages, function(p) p$blocks[[1L]]$title)
  out$footnotes <- lapply(pages, function(p) p$blocks[[1L]]$footnote)
  out
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
      out <- c(out, .render_rtftable(ct, writable_w, ctx$font_half_points,
                                     font_index_map = ctx$font_index_map,
                                     ctx$color_index_map,
                                     doc_row_height = ctx$doc_row_height,
                                     doc_pad_l = ctx$doc_pad_l, doc_pad_r = ctx$doc_pad_r,
                                     doc_markup = ctx$doc_markup))
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
