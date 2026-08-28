# ============================================================================
#  Internal: as_rtftables(stub_vars = ) -- build a clinical indented stub via
#  stub_cols() and remap the source metadata onto the reshaped body.
# ============================================================================

# Build a clinical indented stub from the extracted body via stub_cols(), then
# reindex the source metadata onto the new `[stub, <non-vars cols>]` layout.
# `body` is the extracted data.frame (original columns); `kw` the adapter
# kwargs (col_header / col_spec / widths, in original coordinates); `cell_styles`
# the per-original-row style list (or NULL).  Returns the updated trio.
#
# The stub column is synthesised at position 1; the hierarchy (`stub_vars`)
# columns are consumed, the remaining columns keep their order.  Metadata is
# mapped onto the kept columns (reusing the drop-col reindexers) and shifted one
# place right to make room for the stub.  Source column widths are not carried
# through the merge (the stub's width is unknown -- use `auto_width` or explicit
# widths).  Per-cell `cell_styles` are remapped through the inserted label rows
# using the `rtf_stub_src` attribute stub_cols() records.
.apply_stub_vars <- function(body, kw, cell_styles, spec) {
  n0         <- ncol(body)
  orig_names <- names(body)
  vars_idx   <- .resolve_col_indices(spec$vars, body, "stub(vars)")
  keep       <- setdiff(seq_len(n0), vars_idx)
  drop_names <- orig_names[vars_idx]

  # stub_cols() validates vars (>= 2, distinct) and does the row/label work.
  body2 <- stub_cols(body, vars = spec$vars, label = spec$label,
                     indent = spec$indent, group_summary = spec$group_summary,
                     layout = spec$layout, label_span = spec$label_span)
  src <- attr(body2, "rtf_stub_src", exact = TRUE)
  attr(body2, "rtf_stub_src") <- NULL
  span_rows <- attr(body2, "rtf_label_rows", exact = TRUE)
  attr(body2, "rtf_label_rows") <- NULL

  # layout = "columns" keeps every column exactly where it was, so none of the
  # column-indexed metadata moves and the source widths stay valid.  Only rows
  # were inserted: remap the per-cell styles through them and stop.
  if (identical(spec$layout, "columns")) {
    cell_styles <- .stub_remap_styles(cell_styles, src, NULL, NULL)
    cell_styles <- .stub_mark_spans(cell_styles, span_rows, length(src))
    return(list(body = body2, kw = kw, cell_styles = cell_styles))
  }

  stub_header <- names(body2)[1L]          # merged stub-column name

  # ---- reindex column-indexed metadata: keep the non-vars cols, prepend stub
  if (!is.null(kw$col_header)) {
    ch <- .reindex_col_header(kw$col_header, keep, n0)
    kw$col_header <- .prepend_stub_header(ch, stub_header, length(keep))
  }
  if (!is.null(kw$col_spec)) {
    cs <- .reindex_col_spec(kw$col_spec, keep, drop_names)
    # stat columns shift one place right (stub occupies new position 1).
    kw$col_spec <- lapply(cs, function(e) {
      if (!is.null(e$col) && is.numeric(e$col)) e$col <- as.integer(e$col) + 1L
      e
    })
  }
  cha <- kw$col_header_align
  if (!is.null(cha) && length(cha) == n0) {
    kw$col_header_align <- c("left", cha[keep])
  }
  # Widths refer to the pre-merge columns; the stub's width is unknown, so drop
  # them (auto_width / explicit widths take over on the reshaped body).
  kw$column_widths_twips <- NULL
  kw$col_rel_width       <- NULL

  # ---- remap per-cell styles through the inserted label rows -------------
  cell_styles <- .stub_remap_styles(cell_styles, src, n0, keep)
  cell_styles <- .stub_mark_spans(cell_styles, span_rows, length(src))

  list(body = body2, kw = kw, cell_styles = cell_styles)
}

# Remap a per-original-row style list onto the reshaped body.  An inserted
# label row has no source and carries no styles.  When `n0` / `keep` are given
# (the merged layout) each per-column vector also drops the hierarchy columns
# and gains a leading NA for the synthesised stub column; with `n0 = NULL`
# (layout = "columns") the columns are untouched.
.stub_remap_styles <- function(cell_styles, src, n0, keep) {
  if (is.null(cell_styles) || is.null(src)) return(cell_styles)
  lapply(src, function(s) {
    if (is.na(s)) return(NULL)
    r <- cell_styles[[s]]
    if (is.null(r) || !is.list(r) || is.null(n0)) return(r)
    lapply(r, function(v) if (length(v) == n0) c(v[NA_integer_], v[keep]) else v)
  })
}

# Carry stub_cols(label_span = TRUE) as a per-row style, the same form
# rtftable() folds the `rtf_label_rows` attribute into.  Doing it here means
# the flag rides the machinery that already slices styles per page, so it needs
# no separate re-indexing through the split (#314).
.stub_mark_spans <- function(cell_styles, span_rows, n_rows) {
  if (is.null(span_rows) || length(span_rows) == 0L) return(cell_styles)
  if (is.null(cell_styles)) cell_styles <- vector("list", n_rows)
  for (r in span_rows) {
    if (r >= 1L && r <= length(cell_styles)) {
      row_cs <- cell_styles[[r]]
      if (is.null(row_cs)) row_cs <- list()
      row_cs$span_row <- TRUE
      cell_styles[[r]] <- row_cs
    }
  }
  cell_styles
}

# Prepend a stub column (at position 1) to a reindexed col_header covering
# `nkeep` stat columns.  Char rows get the label prepended; on a multi-row
# header only the bottom (leaf) row carries `stub_header`, spanning rows get an
# empty leading cell and their cells shifted one column right.
.prepend_stub_header <- function(ch, stub_header, nkeep) {
  if (is.null(ch)) return(NULL)
  if (is.character(ch)) return(c(stub_header, ch))
  if (is.list(ch)) {
    n <- length(ch)
    return(lapply(seq_along(ch), function(i) {
      row <- ch[[i]]
      lab <- if (i == n) stub_header else ""     # leaf row carries the label
      if (is.character(row)) return(c(lab, row))
      if (is.list(row)) {
        shifted <- lapply(row, function(cell) {
          if (!is.null(cell$pos)) cell$pos <- as.integer(cell$pos) + 1L
          if (!is.null(cell$from)) {
            cell$from <- as.integer(cell$from) + 1L
            cell$to   <- as.integer(cell$to) + 1L
          }
          cell
        })
        return(c(list(list(pos = 1L, label = lab)), shifted))
      }
      row
    }))
  }
  ch
}
