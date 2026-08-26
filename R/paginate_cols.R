# ============================================================================
#  paginate_cols() -- horizontal (column-wise) pagination
# ============================================================================
#
#  `paginate()` / `as_rtftables(split = )` cut ROWS.  A table wider than the
#  page needs the other axis: cut COLUMNS and continue on the next page,
#  repeating the row-heading column(s) so each page stays readable.
#
#  Page order
#  ----------
#  Row splitting happens first (as_rtftables), then this verb splits the
#  resulting pages by column.  The ROW page is the outer level: a row band
#  sweeps every column block before the next band starts, so the reader goes
#  across the table first and then down it.  Two row pages x three column
#  blocks come out as
#
#      row1/col1  row1/col2  row1/col3  row2/col1  row2/col2  row2/col3
#
#  Why a post-hoc verb on BUILT tables
#  -----------------------------------
#  Applied after `drop_cols` / `stub_vars` / a user `col_header`, so the
#  column positions the author writes mean the FINAL printed columns -- the
#  same convention `set_col_header()` follows.  It also works on a table built
#  by hand with `rtftable()`, not just on an `as_rtftables()` result.
#
#  Column widths
#  -------------
#  The same column keeps the same width on every page.  That is automatic for
#  `column_widths_twips` (absolute), but NOT for `col_rel_width` or for the
#  default equal distribution: `.compute_cellx()` re-normalises whatever it is
#  given across the page's table width, so a bare subset would stretch the kept
#  columns.  The fix is to scale the page's table width by the kept columns'
#  share -- `sum(rel[keep]) / sum(rel)`, or `length(keep) / ncol` when no
#  relative widths were given.  The scale is expressed through
#  `table_width_pct_of_writable` (or `table_width_twips` when the table pins an
#  absolute width) so the renderer still resolves the real writable width and
#  the page geometry stays authoritative.  Rounding drift is bounded by a few
#  twips on a page's last column, where `.compute_cellx()` absorbs the
#  remainder.

# Resolve the carry (row-heading) columns repeated on every column page.
.resolve_carry_cols <- function(carry, tbl, ref) {
  if (is.null(carry)) {
    rt <- tbl$row_title
    return(if (is.null(rt)) 1L else sort(unique(as.integer(rt))))
  }
  if (length(carry) == 0L) return(integer(0))
  sort(unique(.resolve_col_indices(carry, ref, "paginate_cols(carry)")))
}

# Resolve `at` (cut BEFORE these columns) / `cols` (explicit blocks) into a
# list of integer column vectors, each excluding the carry columns.
.resolve_col_blocks <- function(at, cols, ref, carry) {
  n0 <- ncol(ref)
  if (!is.null(at) && !is.null(cols)) {
    stop("Give either `at` or `cols`, not both.", call. = FALSE)
  }

  if (!is.null(cols)) {
    if (!is.list(cols)) {
      stop("`cols` must be a list of column blocks (names or positions).",
           call. = FALSE)
    }
    blocks <- lapply(cols, function(b)
      sort(unique(.resolve_col_indices(b, ref, "paginate_cols(cols)"))))
  } else {
    if (is.null(at) || length(at) == 0L) {
      stop("`at` (the columns to cut before) or `cols` is required.",
           call. = FALSE)
    }
    idx <- sort(unique(.resolve_col_indices(at, ref, "paginate_cols(at)")))
    if (any(idx < 2L)) {
      stop("`at` must name columns 2..ncol -- there is nothing before column 1.",
           call. = FALSE)
    }
    bounds <- c(1L, idx, n0 + 1L)
    blocks <- lapply(seq_len(length(bounds) - 1L), function(i)
      seq.int(bounds[i], bounds[i + 1L] - 1L))
  }

  blocks <- lapply(blocks, function(b) setdiff(b, carry))
  blocks <- Filter(function(b) length(b) > 0L, blocks)
  if (length(blocks) == 0L) {
    stop("`paginate_cols()` produced no column blocks: every column is a ",
         "carry (row-heading) column.", call. = FALSE)
  }
  blocks
}

# The internal cut positions implied by a block list: a boundary `b` means the
# page break falls immediately before column `b`.
.col_block_boundaries <- function(blocks) {
  if (length(blocks) < 2L) return(integer(0))
  vapply(blocks[-1L], function(b) as.integer(min(b)), integer(1L))
}

# Error when a page break falls strictly inside a spanning header cell
# (allow_span_break = FALSE).
.check_span_breaks <- function(tbl, boundaries) {
  if (length(boundaries) == 0L) return(invisible(NULL))
  rows <- c(list(tbl$spanning_header), tbl$col_header %||% list())
  for (row in rows) {
    if (!is.list(row) || length(row) == 0L) next
    for (cell in row) {
      if (!is.list(cell)) next
      p <- if (!is.null(cell$pos)) cell$pos else c(cell$from, cell$to)
      p <- suppressWarnings(as.integer(p))
      p <- p[!is.na(p)]
      if (length(p) < 2L) next
      f <- min(p); t <- max(p)
      if (t <= f) next
      hit <- boundaries[boundaries > f & boundaries <= t]
      if (length(hit)) {
        stop(sprintf(
          paste0("`paginate_cols()` would break the spanning header cell ",
                 "'%s' (columns %d..%d) at column %d. Move the cut to a group ",
                 "boundary, or pass allow_span_break = TRUE."),
          cell$label %||% "", f, t, hit[1L]), call. = FALSE)
      }
    }
  }
  invisible(NULL)
}

# Scale factor for one column page's table width, so that a kept column ends
# up exactly as wide as it is in the full table.  Returns NULL when the table
# pins absolute per-column widths (nothing to scale).
.col_page_width_scale <- function(tbl, keep, n0) {
  if (!is.null(tbl$column_widths_twips)) return(NULL)
  rel <- tbl$col_rel_width
  if (!is.null(rel) && length(rel) == n0) {
    tot <- sum(as.numeric(rel))
    if (tot > 0) return(sum(as.numeric(rel[keep])) / tot)
  }
  length(keep) / n0
}

# Subset a BUILT rtftable to `keep` columns, re-indexing every position-indexed
# field and rescaling the table width (see the note at the top of the file).
.rtftable_keep_cols <- function(tbl, keep) {
  ref <- if (!is.null(tbl$data)) tbl$data else tbl$data_list[[1L]]
  n0  <- ncol(ref)
  keep <- sort(unique(as.integer(keep)))
  if (length(keep) == 0L) {
    stop("A column page must keep at least one column.", call. = FALSE)
  }

  scale <- .col_page_width_scale(tbl, keep, n0)

  # -- body (the blank-row attribute does not survive a column subset) -----
  sub_df <- function(d) {
    ba <- attr(d, "rtf_blank_rows", exact = TRUE)
    d2 <- d[keep]
    if (!is.null(ba)) attr(d2, "rtf_blank_rows") <- ba
    d2
  }
  if (!is.null(tbl$data))      tbl$data      <- sub_df(tbl$data)
  if (!is.null(tbl$data_list)) tbl$data_list <- lapply(tbl$data_list, sub_df)

  # -- header block (shares the drop_cols re-indexers; spans are clipped) --
  if (!is.null(tbl$col_header)) {
    tbl$col_header <- .reindex_col_header(tbl$col_header, keep, n0)
  }
  if (!is.null(tbl$col_header_list)) {
    tbl$col_header_list <- lapply(tbl$col_header_list, function(ch) {
      if (is.null(ch)) NULL else .reindex_col_header(ch, keep, n0)
    })
  }
  if (!is.null(tbl$spanning_header)) {
    cells <- lapply(tbl$spanning_header, .reindex_header_cell, keep = keep)
    cells <- Filter(Negate(is.null), cells)
    tbl$spanning_header <- if (length(cells)) cells else NULL
  }

  # -- per-column formatting ----------------------------------------------
  # On a BUILT table col_spec is already the normalised length-ncol list
  # (positional, no `col` keys), so a plain subset is the re-indexing.
  if (!is.null(tbl$col_spec)) tbl$col_spec <- tbl$col_spec[keep]

  for (k in c("col_rel_width", "column_widths_twips")) {
    v <- tbl[[k]]
    if (!is.null(v) && length(v) == n0) tbl[[k]] <- v[keep]
  }

  # -- resolved column-index fields ---------------------------------------
  rt <- match(as.integer(tbl$row_title), keep)
  rt <- rt[!is.na(rt)]
  tbl$row_title <- if (length(rt)) rt else 1L

  # set_decimal_split() metadata, when the table carries it.
  if (!is.null(tbl$decimal_split)) {
    dc <- match(as.integer(tbl$decimal_split$cols), keep)
    dc <- dc[!is.na(dc)]
    if (length(dc)) tbl$decimal_split$cols <- dc else tbl$decimal_split <- NULL
  }

  if (!is.null(tbl$cell_styles)) {
    tbl$cell_styles <- lapply(tbl$cell_styles, function(r) {
      if (is.null(r) || !is.list(r)) return(r)
      lapply(r, function(v) if (length(v) == n0) v[keep] else v)
    })
  }

  # -- width: keep each column exactly as wide as in the full table --------
  # A block that keeps every column needs no rescaling; leave the table's own
  # width settings untouched so such a page is identical to its input.
  if (!is.null(scale) && !isTRUE(all.equal(scale, 1))) {
    if (!is.null(tbl$table_width_twips)) {
      tbl$table_width_twips <-
        as.integer(round(as.numeric(tbl$table_width_twips) * scale))
    } else {
      pct <- tbl$table_width_pct_of_writable %||% 1
      tbl$table_width_pct_of_writable <- as.numeric(pct) * scale
    }
  }

  tbl
}


#' Paginate a table horizontally, by columns
#'
#' @description
#' Splits a table across pages **by column** -- the horizontal counterpart of
#' the row pagination [as_rtftables()] performs -- repeating the row-heading
#' column(s) on every page so each one can be read on its own.
#'
#' Row splitting happens first; this verb then splits the resulting pages. The
#' **row page is the outer level**: a row band sweeps every column block before
#' the next band starts, so the reader goes across the table first and then
#' down it. Two row pages by three column blocks come out as
#' `row1/col1`, `row1/col2`, `row1/col3`, `row2/col1`, `row2/col2`,
#' `row2/col3`.
#'
#' ```r
#' as_rtftables(x, max_rows = 20) |> paginate_cols(at = c(4, 6))
#' ```
#'
#' Because it runs on **built** tables, the positions refer to the final
#' printed columns -- after `drop_cols`, `stub_vars` and any user
#' `col_header` -- the same convention [set_col_header()] uses.
#'
#' @section Column widths:
#' The same column is the same width on every page. With
#' `column_widths_twips` that is automatic. With `col_rel_width`, or with no
#' width setting at all, each page's table width is scaled to the kept
#' columns' share so the ratios do not stretch to refill the page. A column
#' block narrower than the whole table therefore yields a **narrower page**,
#' rather than one stretched to the margins.
#'
#' @section Spanning headers:
#' Spanning cells are clipped to each page's columns. By default a cut may
#' fall inside a spanning group, and the group's label is repeated over its
#' remaining columns on each page; `allow_span_break = FALSE` rejects such a
#' cut instead.
#'
#' @param x An [rtftable()], or a list of them (pages from [as_rtftables()]).
#'   Every page in a list must have the same columns.
#' @param at Columns to cut **before**, as names or positions (symmetric with
#'   `split_rows`). `at = c(4, 6)` yields blocks `1:3`, `4:5`, `6:ncol`.
#' @param cols Explicit column blocks as a list, e.g. `list(2:3, 4:5)`. Give
#'   either `at` or `cols`.
#' @param carry Row-heading columns repeated on every page. Defaults to the
#'   table's `row_title` (column 1 unless set). `carry = integer(0)` repeats
#'   nothing. Carry columns are removed from the blocks automatically, so they
#'   are never printed twice on a page.
#' @param allow_span_break Allow a cut inside a spanning header cell. Default
#'   `TRUE`.
#' @param ... Unused.
#'
#' @return A list of [rtftable()] pages. Names are carried through unchanged,
#'   so `rtf_tables(auto_section = TRUE)` keeps a table's column pages in one
#'   section.
#'
#' @seealso [as_rtftables()] for row pagination; [set_col_header()] for the
#'   same final-column addressing.
#'
#' @examples
#' df <- data.frame(Parameter = c("Mean", "SD"),
#'                  A_n = c("86", "86"), A_mean = c("45.2", "12.3"),
#'                  B_n = c("84", "84"), B_mean = c("44.8", "11.9"),
#'                  stringsAsFactors = FALSE)
#' pages <- rtftable(df) |> paginate_cols(at = 4)
#' length(pages)                 # 2 column pages
#' names(pages[[1]]$data)        # Parameter repeated on both
#' @export
paginate_cols <- function(x, ...) UseMethod("paginate_cols")

#' @rdname paginate_cols
#' @export
paginate_cols.rtftable <- function(x, at = NULL, cols = NULL, carry = NULL,
                                   allow_span_break = TRUE, ...) {
  paginate_cols(list(x), at = at, cols = cols, carry = carry,
                allow_span_break = allow_span_break, ...)
}

#' @rdname paginate_cols
#' @export
paginate_cols.list <- function(x, at = NULL, cols = NULL, carry = NULL,
                               allow_span_break = TRUE, ...) {
  if (length(x) == 0L) return(list())
  ok <- vapply(x, inherits, logical(1L), "rtftable")
  if (!all(ok)) {
    stop(sprintf(
      "`paginate_cols()` on a list expects every element to be an rtftable (as returned by as_rtftables()); element %d is '%s'.",
      which(!ok)[1L], paste(class(x[[which(!ok)[1L]]]), collapse = "/")),
      call. = FALSE)
  }

  ref_of <- function(t) if (!is.null(t$data)) t$data else t$data_list[[1L]]
  ref    <- ref_of(x[[1L]])
  ncols  <- vapply(x, function(t) ncol(ref_of(t)), integer(1L))
  if (any(ncols != ncols[1L])) {
    stop(sprintf(
      "`paginate_cols()` needs every page to have the same columns; page 1 has %d and page %d has %d.",
      ncols[1L], which(ncols != ncols[1L])[1L],
      ncols[which(ncols != ncols[1L])[1L]]), call. = FALSE)
  }

  carry_idx <- .resolve_carry_cols(carry, x[[1L]], ref)
  blocks    <- .resolve_col_blocks(at, cols, ref, carry_idx)

  if (!isTRUE(allow_span_break)) {
    bounds <- .col_block_boundaries(blocks)
    for (tbl in x) .check_span_breaks(tbl, bounds)
  }

  keeps <- lapply(blocks, function(b) sort(unique(c(carry_idx, b))))

  in_names <- names(x)
  out   <- vector("list", length(keeps) * length(x))
  onames <- character(length(out))
  k <- 0L
  # Row pages are the OUTER loop: a row band sweeps every column block before
  # the next band starts, so the reader goes across first and then down.
  for (i in seq_along(x)) {
    for (keep in keeps) {
      k <- k + 1L
      out[[k]]  <- .rtftable_keep_cols(x[[i]], keep)
      onames[k] <- if (!is.null(in_names)) in_names[i] else ""
    }
  }
  if (!is.null(in_names)) names(out) <- onames
  out
}
