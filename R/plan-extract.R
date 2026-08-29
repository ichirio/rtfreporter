# ============================================================================
#  SPIKE (design/plan-resolver) -- the extraction layer
# ============================================================================
#
#  Step 6.  Nothing exported.
#
#  `rtf_plan()` accepted a data.frame only, which left the plan unable to start
#  from the sources the package exists to serve.  This file lets a plan begin
#  from a gt_tbl, a gtsummary table, an rtables/tern table, a flextable or a
#  huxtable, exactly as `as_rtftables()` does.
#
#  ---------------------------------------------------------------------------
#  Why the dispatch is duplicated here and the adapters are not
#  ---------------------------------------------------------------------------
#
#  The ADAPTERS -- .gt_to_rtftable_kwargs() and friends -- are called, never
#  copied: they are the hard part and they are already tested.  Only the ten
#  lines of "which adapter is this?" appear twice.
#
#  That duplication is deliberate and temporary.  Factoring the chain out of
#  as_rtftables() would edit a shared file, and this branch is long-lived and
#  expects to be rebased onto main repeatedly; every shared file it touches is
#  a conflict waiting to happen. When the design lands for real the chain moves
#  out of as_rtftables() once and both callers use it.  A test below asserts
#  the two dispatches agree, so the duplicate cannot drift unnoticed.
#
#  ---------------------------------------------------------------------------
#  What extraction does NOT do
#  ---------------------------------------------------------------------------
#
#  It reads the source and stops.  Grouping, stubs, blanks, pagination and
#  styling stay declarations, resolved later against whatever the source turned
#  out to contain.  That is the whole reason a plan can name a column before it
#  knows the column exists.

# Read a source object into the parts a plan needs.
#
#   body        the extracted data.frame
#   kw          adapter metadata in SOURCE coordinates (col_header, col_spec,
#               widths); resolved against the column map later
#   cell_styles per SOURCE row, or NULL
#   titles / footnotes  the source's own blocks, carried through untouched
.plan_extract <- function(x, read_meta = TRUE, header_sep = NULL) {
  if (.is_gtsummary_tbl(x)) x <- .gtsummary_to_gt(x)

  pick <- function(kw) {
    list(body = kw$data, kw = kw, cell_styles = kw$cell_styles,
         titles = kw$titles_block, footnotes = kw$footnotes_block)
  }

  if (.is_gt_tbl(x)) {
    return(pick(.gt_to_rtftable_kwargs(x, tokens = .resolve_gt_tokens(read_meta))))
  }
  if (.is_rtables_tbl(x)) {
    return(pick(.rtables_to_rtftable_kwargs(
      x, tokens = .resolve_rtables_tokens(read_meta))))
  }
  if (.is_flextable_tbl(x)) {
    return(pick(.flextable_to_rtftable_kwargs(
      x, tokens = .resolve_flextable_tokens(read_meta))))
  }
  if (.is_huxtable_tbl(x)) {
    # a huxtable IS a data.frame subclass, so this must precede the plain case
    return(pick(.huxtable_to_rtftable_kwargs(
      x, tokens = .resolve_huxtable_tokens(read_meta))))
  }
  if (is.data.frame(x)) {
    kw <- list()
    # Column display names: a `label` attribute wins over the name, and a
    # delimited name becomes a spanning header -- the same reconstruction
    # as_rtftables() performs for a plain data.frame.
    disp <- .df_display_names(x, read_meta)
    auto <- .split_names_to_col_header(disp, header_sep %||% .default_header_seps())
    if (!is.null(auto)) {
      kw$col_header <- auto
    } else if (!identical(disp, names(x))) {
      kw$col_header <- disp
    }
    return(list(body = x, kw = kw, cell_styles = NULL,
                titles = NULL, footnotes = NULL))
  }
  stop("A plan can start from a gt_tbl, gtsummary, rtables/tern, flextable, ",
       "huxtable or data.frame; got '", paste(class(x), collapse = "/"), "'.",
       call. = FALSE)
}

# The plan constructor, widened to any supported source.  Extraction happens
# HERE, once, because a plan cannot name columns it has not seen; everything
# after it stays a declaration.
#' @keywords internal
rtf_plan_from <- function(x, read_meta = TRUE, header_sep = NULL) {
  ex <- .plan_extract(x, read_meta = read_meta, header_sep = header_sep)
  p <- rtf_plan(ex$body)
  p$source <- ex[c("kw", "cell_styles", "titles", "footnotes")]
  p
}

# Project a source-coordinate column header onto the final columns.
#
# A character header is one label per SOURCE column, so the map places it: for
# each final position, take the label of a source column that maps there.  A
# merged stub gets the stub column's own name, since its constituents no longer
# have separate headers.
#
# SPIKE LIMIT: a list-form (spanning) header is NOT projected yet -- that is
# the hardest of the eight reindexers and gets its own step.  It is dropped
# with the plan recording that it was, rather than being placed wrongly.
.plan_resolve_header <- function(kw, columns) {
  ch <- kw$col_header
  if (is.null(ch)) return(list(col_header = NULL, dropped = FALSE))
  if (!is.character(ch)) return(list(col_header = NULL, dropped = TRUE))

  orig <- names(columns$map)
  if (length(ch) != length(orig)) {
    return(list(col_header = NULL, dropped = TRUE))
  }
  out <- vapply(seq_along(columns$names), function(j) {
    src <- which(!is.na(columns$map) & columns$map == j)
    if (length(src) == 0L) return(columns$names[[j]])
    # several source columns can share a final position (a merged stub); the
    # stub carries its own joined name rather than one constituent's label
    if (length(src) > 1L) columns$names[[j]] else ch[[src[[1L]]]]
  }, character(1L))
  list(col_header = out, dropped = FALSE)
}
