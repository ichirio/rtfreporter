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
# This one function replaces FOUR of the package's eight reindexers --
# .reindex_col_header(), .reindex_header_row(), .reindex_header_cell() and
# .prepend_stub_header() -- because the column map already encodes everything
# they each had to be told separately: which columns were dropped, which were
# merged into a stub, and where the stub sits.
#
# A header is one of:
#   * a character vector, one label per SOURCE column;
#   * a list of ROWS, each either such a vector or a list of spanning CELLS
#     carrying $pos (single or c(min, max)) or legacy $from / $to.
#
# For a cell the projection is: take the SOURCE columns it spans, send them
# through the map, drop the ones that went away, and keep the extent of what
# is left.  A cell whose columns all vanished vanishes with them.
.plan_project_header <- function(ch, columns) {
  m      <- columns$map
  n0     <- length(m)
  nfinal <- length(columns$names)

  # One value per FINAL column.  Several source columns can share a final
  # position (a merged stub); `fallback` says what that position gets, since
  # its constituents no longer have separate entries.
  project_vec <- function(v, fallback = columns$names) {
    vapply(seq_len(nfinal), function(j) {
      src <- which(!is.na(m) & m == j)
      if (length(src) == 1L) as.character(v[[src[[1L]]]])
      else as.character(fallback[[j]])
    }, character(1L))
  }

  project_cell <- function(cell) {
    if (!is.list(cell)) return(cell)
    p <- if (!is.null(cell$pos)) cell$pos
         else if (!is.null(cell$from)) c(cell$from, cell$to)
         else return(cell)
    span <- if (length(p) <= 1L) as.integer(p)
            else seq.int(min(as.integer(p)), max(as.integer(p)))
    span <- span[span >= 1L & span <= n0]
    np   <- unique(m[span])
    np   <- np[!is.na(np)]
    if (length(np) == 0L) return(NULL)      # every column it covered is gone
    if (!is.null(cell$pos)) {
      cell$pos <- if (length(np) == 1L) np else c(min(np), max(np))
    } else {
      cell$from <- min(np)
      cell$to   <- max(np)
    }
    cell
  }

  project_row <- function(row) {
    if (is.character(row)) {
      if (length(row) == n0) return(project_vec(row))
      return(row)
    }
    if (is.list(row)) {
      cells <- Filter(Negate(is.null), lapply(row, project_cell))
      if (length(cells) == 0L) return(NULL)
      return(cells)
    }
    row
  }

  if (is.character(ch)) {
    if (length(ch) == n0) return(project_vec(ch))
    return(ch)
  }
  if (is.list(ch)) {
    rows <- Filter(Negate(is.null), lapply(ch, project_row))
    if (length(rows) == 0L) return(NULL)
    return(rows)
  }
  ch
}

# Per-column header ALIGNMENT, projected the same way.  A spanning cell takes
# its alignment from the columns it covers, so dropping this silently rendered
# a spanner centred where the source had it right-aligned -- found by diffing
# the RTF against as_rtftables(), not by reading either implementation.
#
# A merged stub column falls back to "left", matching as_rtftables().
.plan_project_header_align <- function(cha, columns) {
  if (is.null(cha) || !is.character(cha)) return(NULL)
  if (length(cha) != length(columns$map)) return(NULL)
  .plan_project_header_vec(cha, columns, fallback = "left")
}

# Shared by both projections: one value per final column, by name.
.plan_project_header_vec <- function(v, columns, fallback) {
  m <- columns$map
  vapply(seq_along(columns$names), function(j) {
    src <- which(!is.na(m) & m == j)
    if (length(src) == 1L) as.character(v[[src[[1L]]]]) else fallback
  }, character(1L))
}

.plan_resolve_header <- function(kw, columns) {
  ch  <- kw$col_header
  cha <- .plan_project_header_align(kw$col_header_align, columns)
  if (is.null(ch)) {
    return(list(col_header = NULL, col_header_align = cha, dropped = FALSE))
  }
  out <- .plan_project_header(ch, columns)
  list(col_header = out, col_header_align = cha, dropped = is.null(out))
}
