# ============================================================================
#  Internal: column-spec resolution shared by as_rtftables() transforms
# ============================================================================

# Resolve a column spec (character names and/or integer indices, or a list
# mixing the two) to integer column indices into `df`, preserving the supplied
# order.  Shared by `sort_by`, `drop_cols` and stub_cols()'s `vars`; `arg`
# names the argument in the error messages.
#
# A position is a whole number in 1..ncol(df) (integer or double).  TRUE /
# FALSE, a fraction, a factor, NA or anything else that is not a name stops
# (#850): as.integer() used to read TRUE and 1.5 as column 1.
.resolve_col_indices <- function(cols, df, arg) {
  if (is.null(cols)) return(integer(0))
  if (is.factor(cols) || (!is.atomic(cols) && !is.list(cols))) {
    stop(sprintf("`%s` must be column names or whole positions (1..%d).",
                 arg, ncol(df)), call. = FALSE)
  }
  vapply(cols, function(c1) {
    if (is.character(c1) && length(c1) == 1L && !is.na(c1)) {
      m <- match(c1, names(df))
      if (is.na(m)) {
        stop(sprintf("`%s` column '%s' not found in the table.", arg, c1),
             call. = FALSE)
      }
      as.integer(m)
    } else if (length(c1) == 1L && !is.factor(c1) && is.atomic(c1) &&
               is.na(c1)) {
      stop(sprintf("`%s` index NA out of range (1..%d).", arg, ncol(df)),
           call. = FALSE)
    } else if (is.numeric(c1) && !is.factor(c1) && length(c1) == 1L &&
               is.finite(c1) && c1 == trunc(c1)) {
      if (c1 < 1 || c1 > ncol(df)) {
        stop(sprintf("`%s` index %s out of range (1..%d).", arg,
                     format(c1), ncol(df)), call. = FALSE)
      }
      as.integer(c1)
    } else {
      shown <- if (length(c1) == 1L) format(c1) else
        paste0("<", class(c1)[1L], " of length ", length(c1), ">")
      stop(sprintf(paste0("`%s` must be a column name or a whole position ",
                          "(1..%d), not %s."), arg, ncol(df), shown),
           call. = FALSE)
    }
  }, integer(1L), USE.NAMES = FALSE)
}
