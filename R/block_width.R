# ============================================================================
#  Element widths -- one vocabulary for header / footer / table / footnote
# ============================================================================
#
#  Every band of a page can be given a width, and they all speak the same
#  four-value language:
#
#      "content"      follow the table body's rendered width
#      "page"         the writable width -- the page minus its margins
#      0 < x <= 1     that fraction of the writable width
#      integer > 1    twips, absolute
#
#  Nothing here changes a default: an element that is not given a width keeps
#  the behaviour it had (`"content"` for the title / footnote blocks, the
#  writable width for the header / footer band).

# Validate a width spec, returning it unchanged (or NULL when unset).
.check_block_width <- function(w, arg) {
  if (is.null(w)) return(NULL)
  if (is.character(w)) {
    if (length(w) != 1L || !w %in% c("content", "page")) {
      stop(sprintf("`%s` must be \"content\", \"page\", a fraction in (0, 1], or twips.",
                   arg), call. = FALSE)
    }
    return(w)
  }
  if (!is.numeric(w) || length(w) != 1L || is.na(w) || w <= 0) {
    stop(sprintf("`%s` must be \"content\", \"page\", a fraction in (0, 1], or twips.",
                 arg), call. = FALSE)
  }
  w
}

# Resolve a spec to twips.
#   spec           the value the author gave, or NULL
#   content_width  the table body's rendered width (the "content" answer)
#   writable       the page width minus margins (the "page" answer)
#   default        what NULL means for this element
.resolve_block_width <- function(spec, content_width, writable,
                                 default = "content") {
  if (is.null(spec)) spec <- default
  if (identical(spec, "content")) return(as.integer(content_width))
  if (identical(spec, "page"))    return(as.integer(writable))
  w <- as.numeric(spec)
  # <= 1 is a fraction of the writable width; anything larger is already twips
  if (w <= 1) return(as.integer(round(writable * w)))
  as.integer(round(w))
}
