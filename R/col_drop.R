# ============================================================================
#  Internal: as_rtftables(drop_cols = ) -- hide columns after pagination and
#  reindex every position-indexed metadata argument onto the kept columns.
# ============================================================================

# Resolve a `drop_cols` spec (character names and/or integer indices, or NULL)
# to a sorted vector of unique integer column indices into `df`.  Errors on
# unknown names / out-of-range indices, and refuses to drop every column.
.resolve_drop_cols <- function(cols, df) {
  if (is.null(cols) || length(cols) == 0L) return(integer(0))
  idx <- sort(unique(.resolve_col_indices(cols, df, "drop_cols")))
  if (length(idx) >= ncol(df)) {
    stop("`drop_cols` must leave at least one column to display.", call. = FALSE)
  }
  idx
}


# Remove the columns `drop_idx` (positions into the resolved body) from a
# `rtftable()` call-args list, reindexing every position-indexed argument to the
# kept columns.  Mirrors the internal "carry a helper column through pagination,
# strip it before rendering" pattern so a grouping / sort-key column can be used
# by the split and then hidden.  Handled args: data, col_header (flat leaf rows
# and spanning / pos cells), col_spec (integer + name `col`), col_rel_width,
# column_widths_twips, col_header_align, row_title, and per-row cell_styles
# (each per-column vector).  The `rtf_blank_rows` attribute survives the subset.
.apply_col_drop <- function(call_args, drop_idx) {
  data <- call_args$data
  n0   <- ncol(data)
  drop_idx <- sort(unique(as.integer(drop_idx)))
  drop_idx <- drop_idx[drop_idx >= 1L & drop_idx <= n0]
  if (length(drop_idx) == 0L) return(call_args)
  keep <- setdiff(seq_len(n0), drop_idx)
  if (length(keep) == 0L) {
    stop("`drop_cols` must leave at least one column to display.", call. = FALSE)
  }
  old_names  <- names(data)
  drop_names <- old_names[drop_idx]

  # data (preserve the blank-row attribute across the column subset)
  blank_attr <- attr(data, "rtf_blank_rows", exact = TRUE)
  data <- data[keep]
  if (!is.null(blank_attr)) attr(data, "rtf_blank_rows") <- blank_attr
  call_args$data <- data

  # length-n0 positional numeric vectors
  for (k in c("col_rel_width", "column_widths_twips")) {
    v <- call_args[[k]]
    if (!is.null(v) && length(v) == n0) call_args[[k]] <- v[keep]
  }

  # col_header_align: length-n0 vector reindexes; length-1 (scalar) untouched
  cha <- call_args$col_header_align
  if (!is.null(cha) && length(cha) == n0) call_args$col_header_align <- cha[keep]

  # col_header (flat leaf row(s) and spanning / pos cells)
  if (!is.null(call_args$col_header)) {
    call_args$col_header <- .reindex_col_header(call_args$col_header, keep, n0)
  }

  # col_spec: drop entries on a removed column, remap integer `col` to the new
  # position (name `col` survives unchanged -- names are not reused)
  if (!is.null(call_args$col_spec)) {
    call_args$col_spec <- .reindex_col_spec(call_args$col_spec, keep, drop_names)
  }

  # row_title: integer positions remap; column names survive unless dropped
  if (!is.null(call_args$row_title)) {
    call_args$row_title <- .reindex_row_title(call_args$row_title, keep,
                                              drop_names)
  }

  # cell_styles: per-row list; subset each length-n0 per-column vector
  if (!is.null(call_args$cell_styles)) {
    call_args$cell_styles <- lapply(call_args$cell_styles, function(r) {
      if (is.null(r) || !is.list(r)) return(r)
      lapply(r, function(v) if (length(v) == n0) v[keep] else v)
    })
  }

  call_args
}


# Reindex a col_header argument (raw rtftable form) onto the kept columns.
.reindex_col_header <- function(ch, keep, n0) {
  if (is.character(ch)) {
    if (length(ch) == n0) return(ch[keep])
    # pipe-delimited single string: split, reindex if it matches the width
    if (length(ch) == 1L && grepl("|", ch, fixed = TRUE)) {
      parts <- trimws(strsplit(ch, "|", fixed = TRUE)[[1L]])
      if (length(parts) == n0) return(parts[keep])
    }
    return(ch)
  }
  if (is.list(ch)) {
    rows <- lapply(ch, function(row) .reindex_header_row(row, keep, n0))
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0L) return(NULL)
    return(rows)
  }
  ch
}

# One header row: a character leaf row (length n0) or a list of spanning / pos
# cells.  Returns the reindexed row, or NULL if the row becomes empty.
.reindex_header_row <- function(row, keep, n0) {
  if (is.character(row)) {
    if (length(row) == n0) return(row[keep])
    return(row)
  }
  if (is.list(row)) {
    cells <- lapply(row, function(cell) .reindex_header_cell(cell, keep))
    cells <- Filter(Negate(is.null), cells)
    if (length(cells) == 0L) return(NULL)
    return(cells)
  }
  row
}

# One header cell with a `$pos` (single or c(min,max)) or legacy `$from`/`$to`.
# Returns the cell with positions remapped to kept-column coordinates, or NULL
# if every column it covered was dropped.
.reindex_header_cell <- function(cell, keep) {
  if (!is.list(cell)) return(cell)
  remap <- function(p) {
    span   <- if (length(p) <= 1L) p else seq.int(min(p), max(p))
    inside <- intersect(span, keep)
    if (length(inside) == 0L) return(NULL)
    np <- match(inside, keep)
    if (length(np) == 1L) np else c(min(np), max(np))
  }
  if (!is.null(cell$pos)) {
    np <- remap(cell$pos)
    if (is.null(np)) return(NULL)
    cell$pos <- np
    return(cell)
  }
  if (!is.null(cell$from) && !is.null(cell$to)) {
    np <- remap(c(cell$from, cell$to))
    if (is.null(np)) return(NULL)
    cell$from <- min(np)
    cell$to   <- max(np)
    return(cell)
  }
  cell
}

# Reindex a col_spec list onto kept columns.  Integer `col` is remapped to its
# new position (entry dropped if its column was removed); a character `col`
# survives unless it names a dropped column.
.reindex_col_spec <- function(col_spec, keep, drop_names) {
  out <- list()
  for (spec in col_spec) {
    col <- spec$col
    if (is.null(col)) { out[[length(out) + 1L]] <- spec; next }
    if (is.character(col)) {
      if (col %in% drop_names) next
    } else {
      nw <- match(as.integer(col), keep)
      if (is.na(nw)) next
      spec$col <- nw
    }
    out[[length(out) + 1L]] <- spec
  }
  if (length(out) == 0L) return(NULL)
  out
}

# Reindex a row_title argument onto kept columns.  Integer positions are
# remapped; column names survive unless dropped.  Returns NULL (-> rtftable's
# default of column 1) if nothing remains.
.reindex_row_title <- function(rt, keep, drop_names) {
  if (is.null(rt)) return(NULL)
  if (is.character(rt)) {
    rt2 <- rt[!(rt %in% drop_names)]
    if (length(rt2) == 0L) return(NULL)
    return(rt2)
  }
  m <- match(as.integer(rt), keep)
  m <- m[!is.na(m)]
  if (length(m) == 0L) return(NULL)
  m
}
