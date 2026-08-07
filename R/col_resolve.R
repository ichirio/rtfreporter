# ============================================================================
#  Internal: column-spec resolution shared by as_rtftables() transforms
# ============================================================================

# Resolve a column spec (character names and/or integer indices, or a list
# mixing the two) to integer column indices into `df`, preserving the supplied
# order.  Shared by `sort_by`, `drop_cols` and stub_cols()'s `vars`; `arg`
# names the argument in the error messages.
.resolve_col_indices <- function(cols, df, arg) {
  vapply(cols, function(c1) {
    if (is.character(c1)) {
      m <- match(c1, names(df))
      if (is.na(m)) {
        stop(sprintf("`%s` column '%s' not found in the table.", arg, c1),
             call. = FALSE)
      }
      as.integer(m)
    } else {
      i <- as.integer(c1)
      if (is.na(i) || i < 1L || i > ncol(df)) {
        stop(sprintf("`%s` index %s out of range (1..%d).", arg, c1, ncol(df)),
             call. = FALSE)
      }
      i
    }
  }, integer(1L), USE.NAMES = FALSE)
}
