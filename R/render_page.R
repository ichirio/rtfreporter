# ---------------------------------------------------------------------------
#  Drawing a page: its segments, top to bottom
# ---------------------------------------------------------------------------
#  A report page is the segments on it (R/layout.R, .rtfreport_add_page()):
#  each a content with its title, footnote and the empty lines before it.
#  A page of one segment and a page of several are drawn by the same code,
#  so the title / footnote widths and the title_format / footnote_format
#  choice are made in one place.  A page of one draws exactly as before the
#  two were joined: nothing goes before its first segment, and the
#  paragraph that keeps the footnote table apart from the content above is
#  the same `flow_gap` that separates segments.
#
#  What is drawn for a content is .render_content()'s, an internal generic:
#  a new kind of content adds one method there (it comes in through
#  .rtf_content(), R/content.R); the page drawing is not touched.

# The segments a page is drawn from.  A page of one is drawn from the page's
# own title / content / footnote (its first segment's, as
# .rtfreport_add_page() copies them), so that one changed on the page itself
# -- a test or a hand-built rtfreport does -- is still what is drawn; a page
# with no content is drawn from them too (its title and footnote alone).
.page_segments <- function(page) {
  segs <- page$segments
  if (length(segs) > 1L) return(segs)
  list(list(content = page[["content"]], title = page[["title"]],
            footnote = page[["footnote"]], space_before = 0L,
            values = if (length(segs)) segs[[1L]][["values"]]))
}

# Each segment draws its title, content and footnote, with a separating
# paragraph between segments: RTF merges table rows no paragraph separates.
# `ctx`: the document's resolved settings, from generate_rtfreport().
.render_page_segments <- function(segments, p_idx, ctx) {
  doc <- ctx$doc
  writable_w <- ctx$writable_w
  out <- character(0)
  cmds <- .load_rtf_commands()$paragraph
  sep <- cmds$flow_gap
  # A segment's title and footnote fill the {<column>} tokens from its own
  # content's values (R/page_values.R), not another segment's on the page.
  on.exit(.run_ctx$page_values <- NULL, add = TRUE)
  for (bi in seq_along(segments)) {
    b <- segments[[bi]]
    ct <- b[["content"]]
    .run_ctx$page_values <- .segment_values(b)
    if (bi > 1L) {
      out <- c(out, sep, rep(cmds$flow_blank_line, b$space_before %||% 0L))
    }
    # Title / footnote follow the content's width and placement by default,
    # so they sit directly above / below it; "page", a fraction or twips
    # (title_width / footnote_width, #291) override the width.
    content_w     <- .content_width_twips(ct, writable_w)
    content_align <- .content_align(ct)
    title_w    <- .resolve_block_width(ctx$doc_title_width,    content_w, writable_w)
    footnote_w <- .resolve_block_width(ctx$doc_footnote_width, content_w, writable_w)
    # -- Title (default centre + bold; NULL -> one blank gap row).  Plain
    # paragraphs by default; the content-width table form is opt-in via
    # title_format = "table".
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
    out <- c(out, .render_content(ct, ctx))
    # -- Footnote (default left; the table form's first row carries the
    # separator rule).  The table form is the default so the rule is drawn;
    # footnote_format = "text" switches to plain paragraphs.
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
      # An INDEPENDENT table: RTF merges consecutive \trowd runs that no
      # paragraph separates, so without the gap the footnote would still be
      # the body table wearing different \cellx values -- and could not carry
      # a width of its own.  \fs2 keeps the paragraph from adding visible space.
      if (length(fn_rtf)) c(sep, fn_rtf) else fn_rtf
    })
  }
  out
}

# ---- the content itself -----------------------------------------------------

# The RTF of one content, with the document's resolved settings `ctx`.  A
# page with no content draws none.
.render_content <- function(x, ctx) {
  UseMethod(".render_content")
}

.render_content.default <- function(x, ctx) {
  character(0)
}

.render_content.rtftable <- function(x, ctx) {
  .render_rtftable(x, ctx$writable_w, ctx$font_half_points,
                   font_index_map = ctx$font_index_map,
                   ctx$color_index_map,
                   doc_row_height = ctx$doc_row_height,
                   doc_pad_l = ctx$doc_pad_l, doc_pad_r = ctx$doc_pad_r,
                   doc_markup = ctx$doc_markup)
}

.render_content.rtfplot <- function(x, ctx) {
  .render_rtfplot(x, ctx$writable_w)
}
