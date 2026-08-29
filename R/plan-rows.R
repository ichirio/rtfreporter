# ============================================================================
#  SPIKE (design/plan-resolver) -- one row coordinate system
# ============================================================================
#
#  Step 3.  Nothing exported.
#
#  ---------------------------------------------------------------------------
#  The question this file exists to answer
#  ---------------------------------------------------------------------------
#
#  Two things insert rows: the stub (a label row per group) and blank rows.
#  Today they live in different coordinate systems and every later stage has to
#  translate.  `stub_cols()` hands back an `rtf_stub_src` attribute so
#  `cell_styles` can be remapped through the inserted rows; blank rows are
#  positions resolved separately; pagination counts something else again.
#
#  The claim under test: ONE row map -- output row -> source row, NA where the
#  row was synthesised -- computed once, is enough for every consumer.  Groups,
#  blanks, the page budget and per-row styles all read it, and none of them
#  needs a translation step of its own.
#
#  ---------------------------------------------------------------------------
#  Why the reshape happens here and not "at the very end"
#  ---------------------------------------------------------------------------
#
#  The design principle is that the USER's pipeline never rewrites the data --
#  declarations accumulate and nothing happens until resolve time.  Inside the
#  resolver the reshape is not only allowed, it is the point: this is the one
#  place that is permitted to know every declaration at once.
#
#  So the row stage calls the existing, tested `stub_cols()` rather than
#  reimplementing its walk.  That is deliberate: it keeps the spike honest for
#  the eventual comparison against `as_rtftables()`, which reaches the same
#  layout through the same function.  A production version could compute the
#  map without materialising, but proving the ARCHITECTURE does not require
#  rewriting a walk that already works.

# Resolve the printed row sequence.
#
# Returns:
#   body   the data.frame the remaining stages see (reshaped when a stub is
#          declared, otherwise the source unchanged)
#   src    per output row: the source row it came from, NA when synthesised
#   n      number of output rows
.plan_resolve_rows <- function(columns, d) {
  stub <- columns$stub
  if (length(stub) < 2L) {
    return(list(body = d, src = seq_len(nrow(d)), n = nrow(d)))
  }
  body <- stub_cols(d, vars = stub,
                    layout = columns$layout %||% "merged")
  src <- attr(body, "rtf_stub_src", exact = TRUE)
  attr(body, "rtf_stub_src")  <- NULL
  attr(body, "rtf_label_rows") <- NULL
  list(body = body, src = as.integer(src), n = nrow(body))
}

# Carry a per-source-row list through the row map.  A synthesised row has no
# source and therefore no styles.  This is the ONLY place the translation
# happens -- `.stub_remap_styles()` and friends exist because today it happens
# in several.
#' @keywords internal
plan_row_map <- function(res, x) {
  src <- res$rows$src
  if (is.null(x)) return(NULL)
  if (length(x) != max(c(0L, src), na.rm = TRUE)) {
    stop("`x` must have one element per SOURCE row (",
         max(c(0L, src), na.rm = TRUE), "); got ", length(x), ".",
         call. = FALSE)
  }
  lapply(src, function(s) if (is.na(s)) NULL else x[[s]])
}

# Which output rows were synthesised by the stub -- the label rows.  What
# `stub_cols(label_span = TRUE)` needs, derived rather than declared.
#' @keywords internal
plan_label_rows <- function(res) which(is.na(res$rows$src))

# The source rows a page prints, in order.  Everything a caller needs to slice
# its own per-source-row data onto a page.
#' @keywords internal
plan_page_source_rows <- function(res, page) {
  idx <- res$pages[[page]]
  s <- res$rows$src[idx]
  s[!is.na(s)]
}
