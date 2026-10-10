# ============================================================================
#  set_blank_rows() — attach the rtf_blank_rows attribute
# ============================================================================
#
#  Standalone helper for assigning blank-row positions to a single
#  data.frame.  This is the function `paginate()` calls on every
#  per-page chunk; we expose it so callers who do their own paging
#  (or who only need blank-row insertion, no splitting) can use the
#  same blank-spec API.
#
#  Position semantics match `rtftable(blank_rows = ...)`:
#      0  -> blank row BEFORE the first data row
#      k  -> blank row AFTER data row k    (1 <= k <= nrow(df))
#  The resolved positions land on `attr(df, "rtf_blank_rows")` so that
#  `rtftable(read_attributes = TRUE)` picks them up automatically.
# ============================================================================

#' Attach blank-row positions to a data.frame
#'
#' Resolves a `blank_rows` specification (the same one [as_rtftables()]
#' accepts) into integer positions and stores them on
#' `attr(data, "rtf_blank_rows")`.  Use this when you already have a
#' page-sized data.frame and only need to add blank rows -- no
#' pagination required.
#'
#' [as_rtftables()] calls this function on every page it produces, so
#' the behaviour here defines what its `blank_rows`, `blank_row_first`
#' and `blank_row_end` arguments do.
#'
#' @param data A data.frame (or tibble).
#' @param df The old name of `data`: **deprecated** in 0.8.x (warns once a
#'   session, still works), removed in 0.9.0.
#' @param blank_rows Blank-row specification. One of -- or a `list()` combining
#'   any of (positions are unioned):
#'   \describe{
#'     \item{`NULL`}{(default) no positions from this argument.}
#'     \item{an integer vector}{explicit positions: `0` = before the first row,
#'       `k` = after row `k`.}
#'     \item{`"between_groups"`}{insert a blank at every group transition,
#'       using `group_by` (the same detection as the pagination splits).}
#'     \item{a [blank_rows_by_change()] or [blank_rows_by_rule()] spec}{resolved
#'       per page (each carries its own rule / `group_by`).}
#'   }
#'
#' @param blank_row_first Logical, default `FALSE`.  When `TRUE`,
#'   also adds position `0` (blank row at the top of `data`).
#' @param blank_row_end Logical, default `FALSE`.  When `TRUE`, also
#'   adds position `nrow(data)` (blank row at the bottom of `data`).
#' @param group_col Column name or 1-based index identifying the
#'   group, used only when `blank_rows = "between_groups"`.  `NULL`
#'   (default) means detection on column 1 -- see [as_rtftables()].
#' @param group_by How groups are recognised when
#'   `blank_rows = "between_groups"`: `"auto"` (default), `"indent"`,
#'   `"value"`, or `"filled"` -- the same detection as the pagination splits
#'   (see [as_rtftables()]).
#'
#' @return `data` with `attr(., "rtf_blank_rows")` updated.  The
#'   attribute is left absent when the resolved position set is
#'   empty.
#'
#' @examples
#' df <- data.frame(
#'   label = c("Demographics", "  Age", "  Sex",
#'             "Vitals",       "  HR",  "  BP"),
#'   v = 1:6,
#'   stringsAsFactors = FALSE
#' )
#' out <- set_blank_rows(df,
#'                       blank_rows      = "between_groups",
#'                       blank_row_first = TRUE,
#'                       blank_row_end   = TRUE)
#' attr(out, "rtf_blank_rows")
#'
#' @seealso [as_rtftables()] for the per-page version; [rtftable()]
#'   (`read_attributes = TRUE`) which consumes the attribute.
#' @export
set_blank_rows <- function(data,
                            blank_rows      = NULL,
                            blank_row_first = FALSE,
                            blank_row_end   = FALSE,
                            group_col       = NULL,
                            group_by        = c("auto", "indent",
                                                "value", "filled"),
                            df) {
  if (!missing(df)) {
    .deprecate_once(
      "set_blank_rows_df",
      paste0("`set_blank_rows(df = )` is deprecated: the argument is `data` ",
             "now.\n  The old name is removed in 0.9.0."))
    data <- df
  }
  df <- data
  if (!is.data.frame(df)) {
    stop("`data` must be a data.frame (or tibble).", call. = FALSE)
  }
  group_by  <- match.arg(group_by)
  group_idx <- .resolve_group_col(group_col, df)
  pos <- .resolve_pagewise_blanks(blank_rows, df, group_idx, group_by = group_by)
  if (isTRUE(blank_row_first)) pos <- c(0L,        pos)
  if (isTRUE(blank_row_end))   pos <- c(pos, nrow(df))
  pos <- sort(unique(as.integer(pos)))
  pos <- pos[pos >= 0L & pos <= nrow(df)]
  if (length(pos) > 0L) {
    attr(df, "rtf_blank_rows") <- pos
  } else {
    attr(df, "rtf_blank_rows") <- NULL
  }
  df
}
