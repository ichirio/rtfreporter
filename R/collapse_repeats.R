# ============================================================================
#  collapse_repeats() -- public data.frame repeat-suppression helper
# ============================================================================
#
#  The internals (`.resolve_collapse_cols()`, `.collapse_repeats_chunk()`) live
#  in paginate.R, where `as_rtftables()` applies the same suppression PER PAGE.
#  This is the standalone, whole-frame version for pre-processing.

#' Blank out consecutive repeated values in a data.frame
#'
#' Suppresses consecutive repeated values in the chosen columns, keeping only
#' the first row of each run; suppressed cells become `NA`.  No rows are
#' removed -- only the display value is blanked (the RTF renderer draws `NA` as
#' an empty cell).  This is the classic "don't repeat the group label on every
#' row" layout for clinical listings.
#'
#' Suppression is **hierarchical**: columns are processed in the order given, so
#' a change in any earlier-listed column resets the run of every later one.
#' Keys are built from the original (pre-blanking) values.
#'
#' @param df A `data.frame`.
#' @param cols Columns to collapse: character names and/or integer positions.
#'   Mix names and positions with a `list()` (a bare `c()` would coerce the
#'   numbers to strings) -- the same convention as `drop_cols` / `sort_by`.
#'   Processed in the supplied order (outermost group first).
#'
#' @return `df` with suppressed cells set to `NA` in the selected columns.
#'
#' @details
#' [as_rtftables()] has a `collapse_repeats` argument that does the same thing
#' but **per page** -- a run is reset at every page break, so the first row of
#' each page shows its group value again.  Prefer that argument for paginated
#' tables; use `collapse_repeats()` here to pre-process a single frame (e.g.
#' before `as_rtftables()`), or when you want whole-frame suppression.
#'
#' @seealso [as_rtftables()] (the per-page `collapse_repeats` argument).
#'
#' @examples
#' df <- data.frame(
#'   grp = c("A", "A", "A", "B", "B"),
#'   sub = c("x", "x", "y", "x", "x"),
#'   n   = 1:5,
#'   stringsAsFactors = FALSE
#' )
#' collapse_repeats(df, cols = c("grp", "sub"))
#' @export
collapse_repeats <- function(df, cols) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame.", call. = FALSE)
  }
  idx <- .resolve_collapse_cols(cols, df)
  .collapse_repeats_chunk(df, idx)
}
