# ============================================================================
#  Internal: body row ordering for as_rtftables(sort_by = / sort_desc = )
# ============================================================================

# Resolve a `sort_by` / `sort_desc` request to an integer row permutation of
# `df` (or NULL for "keep input order").  Columns are given as names and/or
# integer indices (a vector, or a list to mix the two), in body coordinates.
# The sort is stable and NA keys are placed last; descending keys are handled
# via `-xtfrm()` so mixed column types (character, numeric, factor) all work.
.resolve_sort_order <- function(sort_by, sort_desc, df) {
  if (is.null(sort_by) || length(sort_by) == 0L) return(NULL)
  idx  <- .resolve_sort_cols(sort_by, df)
  desc <- .normalize_sort_desc(sort_desc, length(idx))
  keys <- lapply(seq_along(idx), function(k) {
    r <- xtfrm(df[[idx[k]]])           # rank-transform: comparable as numeric
    if (isTRUE(desc[k])) -r else r
  })
  do.call(order, c(keys, list(na.last = TRUE)))
}

# Resolve `sort_by` columns (names / integers / a mixing list) to integer
# indices into `df`, preserving the supplied order (sort priority).
.resolve_sort_cols <- function(cols, df) {
  .resolve_col_indices(cols, df, "sort_by")
}

# Normalise `sort_desc` to a logical vector of length `n` (one per sort key).
.normalize_sort_desc <- function(sort_desc, n) {
  if (is.null(sort_desc)) return(rep(FALSE, n))
  d <- as.logical(sort_desc)
  if (anyNA(d)) stop("`sort_desc` must be TRUE / FALSE.", call. = FALSE)
  if (length(d) == 1L) return(rep(d, n))
  if (length(d) != n) {
    stop(sprintf("`sort_desc` must have length 1 or %d (one per `sort_by`).", n),
         call. = FALSE)
  }
  d
}
