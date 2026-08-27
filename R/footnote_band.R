# ============================================================================
#  Footnote block -- the page footer's mechanism, as a table of its own
# ============================================================================
#
#  The footnote used to have a row model all its own: one cell per row, with
#  per-row bold / italic / colour / border. It now uses exactly what
#  `rtf_footer()` uses -- `c(l = , c = , r = )`, up to three cells positioned
#  left / centre / right -- differing only in its defaults:
#
#      page footer     a top rule by default
#      footnote        no rule at all by default
#
#  A bare string is not ambiguous: the block's `align` decides which of l / c /
#  r it becomes, so `"text"` is a single left-aligned cell spanning the block.
#
#  It is also emitted as an INDEPENDENT table. RTF treats consecutive
#  `\trowd ... \row` runs with no paragraph between them as one table, and the
#  footnote used to follow the body's rows directly -- so it was really the
#  body table wearing different `\cellx` values. A `\pard\par` in between makes
#  it a table of its own, which is what having a width of its own requires.

# Turn a footnote block into the `rows` shape .render_header_footer() consumes.
# `align` picks the slot a bare string lands in.
.footnote_rows <- function(block, align = "left", format = "table") {
  if (is.null(block)) return(list())
  rows <- if (is.list(block)) block else as.list(block)
  if (length(rows) == 0L) return(list())
  slot <- switch(align, left = "l", center = "c", right = "r", "l")

  lapply(rows, function(r) {
    if (is.list(r)) {
      stop(paste0("A footnote row is a string or a named vector such as ",
                  "c(l = \"...\", r = \"...\"). Per-row styling lists are no ",
                  "longer supported -- set the style on the block instead, ",
                  "via rtf_footnotes(font_size_half_points = , align = , ...)."),
           call. = FALSE)
    }
    if (is.null(r)) r <- ""
    nm <- names(r)                 # as.character() would drop them
    r  <- as.character(r)
    names(r) <- nm
    if (is.null(nm) || !any(nzchar(nm))) {
      # a bare string (or several) -- one cell in the block's own slot
      if (length(r) != 1L) {
        stop("A footnote row must be one string, or a named vector like ",
             "c(l = \"...\", r = \"...\").", call. = FALSE)
      }
      if (identical(format, "text")) return(r)   # the paragraph form wants text
      out <- r
      names(out) <- slot
      return(out)
    }
    bad <- setdiff(nm[nzchar(nm)], c("l", "c", "r"))
    if (length(bad)) {
      stop(sprintf("A footnote row may only be named l / c / r; got %s.",
                   paste0("'", bad, "'", collapse = ", ")), call. = FALSE)
    }
    if (identical(format, "text")) {
      stop(paste0("A c(l = , c = , r = ) footnote row needs the table form. ",
                  "Set footnote_format = \"table\" (the default), or give the ",
                  "row as a single string."), call. = FALSE)
    }
    r
  })
}

# Build the `hf`-shaped object the footer renderer takes, from the footnote
# block plus the block-level style the author set.
.footnote_hf <- function(block, style, width_twips, format = "table") {
  list(
    rows        = .footnote_rows(block, style$align %||% "left", format),
    border      = style$border,          # NULL -> no rule, unlike the footer
    width_twips = as.integer(width_twips),
    row_height_twips      = style$row_height_twips,
    font_size_half_points = style$font_size_half_points,
    markup                = style$markup,
    cell_padding_left_twips  = NULL,
    cell_padding_right_twips = NULL
  )
}
