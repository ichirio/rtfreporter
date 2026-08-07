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

#' Add blank separator rows to a table or data.frame
#'
#' Resolves a `blank_rows` specification (the same one `paginate()` accepts)
#' into integer positions and records them.  It is an S3 generic:
#'
#' \itemize{
#'   \item on a **`rtftable`** (or a **list of pages** from [as_rtftables()])
#'     it sets the resolved positions on the table, so the natural
#'     `as_rtftables(x, ...) |> set_blank_rows(...)` pipeline works and a list
#'     is handled **per page**;
#'   \item on a **data.frame** it stores the positions on
#'     `attr(., "rtf_blank_rows")` (consumed by `rtftable(read_attributes =
#'     TRUE)`).  `paginate()` calls this form on every chunk it produces, so it
#'     defines what `paginate(blank_rows = ...)` etc. do.
#' }
#'
#' @param x An [rtftable()], a list of them (pages from [as_rtftables()]), or a
#'   data.frame (or tibble).
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
#'   also adds position `0` (blank row at the top of `df`).
#' @param blank_row_end Logical, default `FALSE`.  When `TRUE`, also
#'   adds position `nrow(df)` (blank row at the bottom of `df`).
#' @param group_col Column name or 1-based index identifying the
#'   group, used only when `blank_rows = "between_groups"`.  `NULL`
#'   (default) means detection on column 1 — see [paginate()].
#' @param group_by How groups are recognised when
#'   `blank_rows = "between_groups"`: `"auto"` (default), `"indent"`,
#'   `"value"`, or `"filled"` — the same detection as the pagination splits
#'   (see [paginate()]).
#'
#' @param ... Passed between methods.
#'
#' @return An object of the same shape as `x` (rtftable, list of pages, or
#'   data.frame) with the blank-row positions set / attached.
#'
#' @examples
#' df <- data.frame(
#'   label = c("Demographics", "  Age", "  Sex",
#'             "Vitals",       "  HR",  "  BP"),
#'   v = 1:6,
#'   stringsAsFactors = FALSE
#' )
#' # As a post-hoc verb on the as_rtftables() output:
#' as_rtftables(df) |>
#'   set_blank_rows(blank_rows = "between_groups", group_by = "indent")
#'
#' # On a bare data.frame (attaches the attribute):
#' out <- set_blank_rows(df, blank_rows = "between_groups",
#'                       blank_row_first = TRUE, blank_row_end = TRUE)
#' attr(out, "rtf_blank_rows")
#'
#' @seealso [paginate()] for the per-page version; [rtftable()]
#'   (`read_attributes = TRUE`) which consumes the attribute; [collapse_repeats()]
#'   and the other post-hoc verbs.
#' @export
set_blank_rows <- function(x, ...) UseMethod("set_blank_rows")

#' @rdname set_blank_rows
#' @export
set_blank_rows.data.frame <- function(x,
                                      blank_rows      = NULL,
                                      blank_row_first = FALSE,
                                      blank_row_end   = FALSE,
                                      group_col       = NULL,
                                      group_by        = c("auto", "indent",
                                                          "value", "filled"),
                                      ...) {
  df <- x
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

#' @rdname set_blank_rows
#' @export
set_blank_rows.rtftable <- function(x, ...) {
  ref      <- if (!is.null(x$data)) x$data else x$data_list[[1L]]
  resolved <- set_blank_rows(ref, ...)
  pos      <- attr(resolved, "rtf_blank_rows", exact = TRUE)
  x$blank_rows <- if (is.null(pos)) integer(0) else as.integer(pos)
  x
}

#' @rdname set_blank_rows
#' @export
set_blank_rows.list <- function(x, ...) {
  .style_map_pages(x, set_blank_rows, ..., verb = "set_blank_rows")
}
