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
.plan_resolve_rows <- function(columns, d, style = NULL) {
  src <- seq_len(nrow(d))

  # 1. SORT first, so the stub's hierarchy runs, the group detection and every
  #    blank position all see the same order -- the reason as_rtftables() sorts
  #    before pagination too.  The row map records it, so per-source-row data
  #    follows without anyone reordering it separately.
  if (length(columns$sort)) {
    ord <- .resolve_sort_order(columns$sort, columns$sort_desc, d)
    if (!is.null(ord)) {
      d <- d[ord, , drop = FALSE]
      rownames(d) <- NULL
      src <- src[ord]
    }
  }

  # 2. STUB, which inserts label rows and so rewrites the map.
  if (length(columns$stub) >= 2L) {
    o <- columns$stub_opts %||% list()
    body <- do.call(stub_cols, c(
      list(d, vars = columns$stub, layout = columns$layout %||% "merged"),
      o[intersect(names(o), c("label", "indent", "group_summary",
                              "label_span"))]))
    ssrc <- as.integer(attr(body, "rtf_stub_src", exact = TRUE))
    attr(body, "rtf_stub_src")   <- NULL
    attr(body, "rtf_label_rows") <- NULL
    # compose the two maps: stub row -> sorted row -> SOURCE row
    src <- ifelse(is.na(ssrc), NA_integer_, src[ssrc])
    d <- body
  }

  # 3. CELL FORMATTING, on the printed body, exactly where .paginate_df()
  #    applies it: after the reshape, before the split.
  if (!is.null(style$cell_format)) {
    fl <- .resolve_cell_format(style$cell_format, ncol(d))
    if (!is.null(fl)) d <- .apply_cell_format(d, fl)
  } else if (isTRUE(style$align_count_pct)) {
    d <- .realign_count_pct_df(d)
  }

  # 4. COLLAPSE repeated values, addressed BY NAME through the column map.
  if (length(columns$collapse)) {
    idx <- unname(columns$body_map[columns$collapse])
    idx <- unique(idx[!is.na(idx)])
    if (length(idx)) d <- .collapse_repeats_chunk(d, idx)
  }

  list(body = d, src = src, n = nrow(d))
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
