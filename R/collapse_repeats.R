# ============================================================================
#  collapse_repeats() -- post-hoc repeat-suppression verb
# ============================================================================
#
#  Operates on a finished rtftable (or a list of them, an as_rtftables() page
#  list), reusing the internals (`.resolve_collapse_cols()`,
#  `.collapse_repeats_chunk()`) that paginate.R applies per page.  On a page
#  list it collapses each page independently, so the result matches the
#  per-page `as_rtftables(collapse_repeats = )` argument exactly.

#' Blank out consecutive repeated values in a finished table
#'
#' Suppresses consecutive repeated values in the chosen columns of a built
#' [rtftable()], keeping only the first row of each run; suppressed cells become
#' `NA`.  No rows are removed -- only the display value is blanked (the renderer
#' draws `NA` as an empty cell).  This is the classic "don't repeat the group
#' label on every row" layout for clinical listings.
#'
#' Like the other post-hoc verbs it is an S3 generic with an `rtftable` method
#' and a **list** method.  Given a **list of pages** (an [as_rtftables()]
#' result) it collapses **each page independently**, so a run restarts at every
#' page break -- the first row of each page shows its value again.  This makes
#' `collapse_repeats(as_rtftables(x, ...), cols)` equivalent to
#' `as_rtftables(x, collapse_repeats = cols, ...)`.
#'
#' Suppression is **hierarchical**: columns are processed in the order given, so
#' a change in any earlier-listed column resets the run of every later one.
#'
#' @param x An [rtftable()], or a list of them (pages from [as_rtftables()]).
#' @param cols Columns to collapse: character names and/or integer positions in
#'   the table's **final** body columns.  Mix names and positions with a
#'   `list()` (a bare `c()` would coerce the numbers to strings).  Processed in
#'   the supplied order (outermost group first).
#'
#' @return An object of the same shape as `x` (rtftable, or list of pages), with
#'   suppressed cells set to `NA` in the selected columns.
#'
#' @seealso [as_rtftables()] (the equivalent per-page `collapse_repeats`
#'   argument); [style_body()] and the other post-hoc verbs.
#'
#' @examples
#' tbl <- rtftable(data.frame(
#'   grp = c("A", "A", "A", "B", "B"),
#'   sub = c("x", "x", "y", "x", "x"),
#'   n   = 1:5,
#'   stringsAsFactors = FALSE
#' ))
#' out <- collapse_repeats(tbl, cols = c("grp", "sub"))
#' out$data
#' @export
collapse_repeats <- function(x, cols) UseMethod("collapse_repeats")

#' @rdname collapse_repeats
#' @export
collapse_repeats.rtftable <- function(x, cols) {
  apply_one <- function(df) {
    idx <- .resolve_collapse_cols(cols, df)
    .collapse_repeats_chunk(df, idx)
  }
  if (!is.null(x$data_list)) {
    x$data_list <- lapply(x$data_list, apply_one)
    if (!is.null(x$data)) x$data <- apply_one(x$data)
  } else {
    x$data <- apply_one(x$data)
  }
  x
}

#' @rdname collapse_repeats
#' @export
collapse_repeats.list <- function(x, cols) {
  .style_map_pages(x, collapse_repeats, cols = cols, verb = "collapse_repeats")
}

#' @rdname collapse_repeats
#' @export
collapse_repeats.default <- function(x, cols) {
  stop("`collapse_repeats()` expects an rtftable or a list of rtftable pages ",
       "(as returned by as_rtftables() / as_rtftable()); build the table first.",
       call. = FALSE)
}
