# ============================================================================
#  Per-element style -- font size, row height, markup, block alignment
# ============================================================================
#
#  `rtf_document(default_format = )` sets the document baseline; each element
#  -- the table body, the header / footer band, the title and footnote blocks
#  -- may override it. Anything an element does not set falls back to the
#  document default, the same cascade `markup` and the widths already follow.
#
#  Font size and row height are resolved TOGETHER, because a row height that
#  does not follow its font size is worse than useless:
#
#    * an explicit row height on the element always wins;
#    * otherwise, if the element sets its OWN font size, the height is
#      recomputed from that size -- it does not inherit the document's height,
#      which was chosen for a different size;
#    * otherwise the document's height, and failing that the height the
#      document's font size implies.
#
#  `row_height_twips = 0L` keeps its established meaning of "automatic" (emit
#  no \trrh at all) and is passed through untouched.

.check_font_size <- function(x, arg) {
  if (is.null(x)) return(NULL)
  x <- suppressWarnings(as.integer(x))
  if (length(x) != 1L || is.na(x) || x <= 0L) {
    stop(sprintf("`%s` must be a single positive integer (half-points).", arg),
         call. = FALSE)
  }
  x
}

.check_row_height <- function(x, arg) {
  if (is.null(x)) return(NULL)
  x <- suppressWarnings(as.integer(x))
  if (length(x) != 1L || is.na(x) || x < 0L) {
    stop(sprintf("`%s` must be a single non-negative integer (twips) or NULL.",
                 arg), call. = FALSE)
  }
  x
}

.check_align <- function(x, arg) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L ||
      !x %in% c("left", "center", "right")) {
    stop(sprintf("`%s` must be \"left\", \"center\" or \"right\".", arg),
         call. = FALSE)
  }
  x
}

# Resolve one element's font size and row height together (see the note above).
.resolve_element_metrics <- function(own_fs, own_rh, doc_fs, doc_rh) {
  fs <- .check_font_size(own_fs, "font_size_half_points") %||%
          as.integer(doc_fs %||% 18L)
  rh <- if (!is.null(own_rh)) {
    as.integer(own_rh)
  } else if (!is.null(own_fs)) {
    .default_row_height_twips(fs)          # the size the element itself chose
  } else if (!is.null(doc_rh)) {
    as.integer(doc_rh)
  } else {
    .default_row_height_twips(fs)
  }
  list(fs = as.integer(fs), rh = as.integer(rh))
}

# The \fs run command for an element, or "" when it matches the document (so
# an element that changes nothing emits nothing).
.fs_cmd_for <- function(fs, doc_fs) {
  if (is.null(fs) || identical(as.integer(fs), as.integer(doc_fs))) return("")
  paste0("\\fs", as.integer(fs))
}

# Collect the style an element was given into a plain list, dropping the
# entries left unset so the renderer can tell "unset" from "set to the default".
.element_style <- function(font_size_half_points = NULL, row_height_twips = NULL,
                           markup = NULL, align = NULL, verb = "style") {
  out <- list(
    font_size_half_points =
      .check_font_size(font_size_half_points,
                       paste0(verb, "(font_size_half_points)")),
    row_height_twips =
      .check_row_height(row_height_twips, paste0(verb, "(row_height_twips)")),
    markup = if (is.null(markup)) NULL else .resolve_markup(markup),
    align  = .check_align(align, paste0(verb, "(align)"))
  )
  out[!vapply(out, is.null, logical(1L))]
}
