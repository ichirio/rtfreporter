# ============================================================================
#  blank_rows: three combinable specification modes
# ============================================================================
#
#  rtftable(blank_rows = ...) accepts any of:
#
#    1. Integer vector of positions:
#         c(0, 5, -1)
#       0    = blank row BEFORE the first data row
#       k    = blank row AFTER data row k (1 <= k <= n)
#       -1   = blank row AFTER the last data row
#       out-of-range values produce a warning and are ignored.
#
#    2. Variable-change spec via blank_rows_by_change():
#         blank_rows_by_change(cols, include_before_first, include_after_last)
#       Inserts a blank row whenever the value of any listed column changes
#       from one row to the next.  Optionally adds rows at the start / end.
#
#    3. Rule-based spec via blank_rows_by_rule():
#         blank_rows_by_rule(col, pattern, where = c("before", "after"))
#       Inserts a blank row before / after every data row whose value in
#       `col` matches the regular expression `pattern`.
#
#  Modes can be combined by wrapping them in a list:
#
#    blank_rows = list(
#      c(0, -1),
#      blank_rows_by_change(cols = "Visit"),
#      blank_rows_by_rule(col = "Parameter", pattern = "^Total", where = "before")
#    )
#
#  Additionally, when `read_attributes = TRUE` (the rtftable() default) and
#  the data.frame carries attr(data, "rtf_blank_rows") as a numeric vector,
#  those positions are folded in.  Explicit `blank_rows` overrides the
#  attribute (the attribute is used only when the argument is NULL).

# -- Constructor: by-variable-change -----------------------------------------

#' Blank-row specification: insert when a variable's value changes
#'
#' Constructor for a blank-row spec that inserts a blank separator row each
#' time the *group* of any column in `cols` changes from the previous row.
#' Pass the result to `rtftable(blank_rows = ...)` or
#' `as_rtftables(blank_rows = ...)`, optionally combined with other specs via a
#' list.
#'
#' @param cols Character vector of column names in the data frame.
#' @param group_by How a "group" is recognised in each column -- the same
#'   detection the pagination splits and `blank_rows = "between_groups"` use:
#'   \describe{
#'     \item{`"value"`}{(default) a group is a run of equal values; a blank is
#'       inserted whenever a value differs from the previous row (the classic
#'       behaviour).}
#'     \item{`"indent"`}{a non-indented, non-empty cell starts a group; indented
#'       / empty cells are members.}
#'     \item{`"filled"`}{a non-empty cell starts a group; `NA` / `""` cells are
#'       members.}
#'     \item{`"auto"`}{pick one of the above from each column's content.}
#'   }
#'   With several `cols`, a blank is inserted where **any** column's group
#'   changes.
#' @param include_before_first Logical. When `TRUE` (default), also insert a
#'   blank row before the first data row.
#' @param include_after_last Logical. When `TRUE` (default), also insert a
#'   blank row after the last data row.
#'
#' @return An object of class `rtf_blank_rows_by_change`.
#'
#' @examples
#' \dontrun{
#' rtftable(df, blank_rows = blank_rows_by_change(c("Treatment", "Visit")))
#' # indent-based groups (a stub/label column):
#' rtftable(df, blank_rows = blank_rows_by_change("label", group_by = "indent"))
#' }
#'
#' @export
blank_rows_by_change <- function(cols,
                                  group_by = c("value", "indent",
                                               "filled", "auto"),
                                  include_before_first = TRUE,
                                  include_after_last   = TRUE) {
  if (!is.character(cols) || length(cols) < 1L) {
    stop("`cols` must be a non-empty character vector.", call. = FALSE)
  }
  group_by <- match.arg(group_by)
  structure(
    list(
      cols                 = cols,
      group_by             = group_by,
      include_before_first = isTRUE(include_before_first),
      include_after_last   = isTRUE(include_after_last)
    ),
    class = "rtf_blank_rows_by_change"
  )
}

# -- Constructor: by-rule (regex on one column) ------------------------------

#' Blank-row specification: insert before/after rows matching a pattern
#'
#' Constructor for a blank-row spec that inserts a blank separator row
#' before or after every data row whose value in `col` matches the
#' regular expression `pattern`.
#'
#' @param col Name of the column to test.
#' @param pattern A regular expression (POSIX extended via [grepl()]).
#' @param where Either `"before"` (default) or `"after"`.
#'
#' @return An object of class `rtf_blank_rows_by_rule`.
#'
#' @examples
#' \dontrun{
#' # Blank row before every row whose Parameter does NOT start with a space
#' rtftable(df, blank_rows = blank_rows_by_rule(
#'   col = "Parameter", pattern = "^[^ ]", where = "before"))
#' }
#'
#' @export
blank_rows_by_rule <- function(col, pattern,
                                where = c("before", "after")) {
  if (!is.character(col) || length(col) != 1L) {
    stop("`col` must be a single column name.", call. = FALSE)
  }
  if (!is.character(pattern) || length(pattern) != 1L) {
    stop("`pattern` must be a single regular expression.", call. = FALSE)
  }
  where <- match.arg(where)
  structure(
    list(col = col, pattern = pattern, where = where),
    class = "rtf_blank_rows_by_rule"
  )
}

# -- Resolver ----------------------------------------------------------------

# Resolve a blank_rows spec into a sorted/deduplicated integer vector of
# positions (0 = before first; k = after row k).  Out-of-range integers
# warn and are dropped.  Accepts:
#   * NULL                            -> integer(0)
#   * integer / numeric vector        -> mode 1 (positions)
#   * rtf_blank_rows_by_change object -> mode 2
#   * rtf_blank_rows_by_rule object   -> mode 3
#   * list of any of the above        -> union of all resolved positions
.resolve_blank_rows <- function(spec, df) {
  if (is.null(spec)) return(integer(0))
  if (is.numeric(spec) && !is.list(spec)) {
    return(.resolve_blank_positions(as.integer(spec), nrow(df)))
  }
  if (inherits(spec, "rtf_blank_rows_by_change")) {
    return(.resolve_by_change(spec, df))
  }
  if (inherits(spec, "rtf_blank_rows_by_rule")) {
    return(.resolve_by_rule(spec, df))
  }
  if (is.list(spec)) {
    out <- integer(0)
    for (item in spec) out <- c(out, .resolve_blank_rows(item, df))
    return(sort(unique(out)))
  }
  stop("Unrecognised `blank_rows` specification.", call. = FALSE)
}

# Mode 1: positions
.resolve_blank_positions <- function(positions, n) {
  out <- integer(0)
  for (p in positions) {
    if (is.na(p)) next
    if (p == -1L) {
      out <- c(out, n)
    } else if (p >= 0L && p <= n) {
      out <- c(out, p)
    } else {
      warning(sprintf(
        "blank_rows position %d is out of range (data has %d rows); ignored.",
        p, n), call. = FALSE)
    }
  }
  sort(unique(out))
}

# Mode 2: by-variable-change.  `group_by = "value"` (default) uses exact
# equality on each column (the historical behaviour); "indent" / "filled" /
# "auto" reuse the pagination group detection (`.compute_group_info()`), so a
# blank lands at each group transition -- the same recognition as
# `blank_rows = "between_groups"`.  Several columns union their transitions.
.resolve_by_change <- function(spec, df) {
  n <- nrow(df)
  if (n == 0L) return(integer(0))
  mode <- spec$group_by %||% "value"
  out <- integer(0)
  for (col in spec$cols) {
    if (!col %in% names(df)) {
      warning(sprintf(
        "blank_rows_by_change column '%s' not found; ignored.", col),
        call. = FALSE)
      next
    }
    if (n < 2L) next
    if (identical(mode, "value")) {
      vals <- df[[col]]
      for (i in 2:n) {
        if (!identical(vals[[i]], vals[[i - 1L]])) out <- c(out, i - 1L)
      }
    } else {
      info    <- .compute_group_info(df, match(col, names(df)), group_by = mode)
      id      <- info$id
      # A blank sits after the row before each id change; NA ids (rows above the
      # first group header) never form a transition (which() drops NA), matching
      # `blank_rows = "between_groups"`.
      changes <- which(c(FALSE, id[-1L] != id[-length(id)]))
      out     <- c(out, changes - 1L)
    }
  }
  if (isTRUE(spec$include_before_first)) out <- c(out, 0L)
  if (isTRUE(spec$include_after_last))   out <- c(out, n)
  sort(unique(out))
}

# Mode 3: by-rule (regex on one column)
.resolve_by_rule <- function(spec, df) {
  n <- nrow(df)
  if (n == 0L) return(integer(0))
  if (!spec$col %in% names(df)) {
    warning(sprintf(
      "blank_rows_by_rule column '%s' not found; ignored.", spec$col),
      call. = FALSE)
    return(integer(0))
  }
  vals    <- as.character(df[[spec$col]])
  matches <- grepl(spec$pattern, vals)
  hits    <- which(matches)
  if (length(hits) == 0L) return(integer(0))
  positions <- if (spec$where == "before") hits - 1L else hits
  sort(unique(positions[positions >= 0L & positions <= n]))
}

# -- data.frame attribute reader (extensible) --------------------------------

# Read recognised attributes off a data.frame and return them as a named
# list.  Currently supported:
#   attr(df, "rtf_blank_rows") -- numeric vector (mode-1 positions)
#
# Future extension: more attribute keys can be added here without changing
# call sites.  rtftable() consumes the returned list as fallback defaults.
.read_data_attributes <- function(df) {
  out <- list()
  rba <- attr(df, "rtf_blank_rows", exact = TRUE)
  if (is.numeric(rba)) out$blank_rows <- as.integer(rba)
  # Rows that stub_cols(label_span = TRUE) marked to render as one cell
  # spanning the table (#312).
  rlr <- attr(df, "rtf_label_rows", exact = TRUE)
  if (is.numeric(rlr)) out$label_rows <- as.integer(rlr)
  out
}
