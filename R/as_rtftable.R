#' Convert one table object to a single rtftable
#'
#' Single-page convenience wrapper around [as_rtftables()]: takes a `gt_tbl`,
#' a [gtsummary](https://www.danieldsjoberg.com/gtsummary/) table, an
#' rtables/tern `VTableTree`, a `flextable`, a `huxtable`, or a plain
#' `data.frame` / tibble, and returns one `rtftable` (rather than a list of
#' pages).  It is exactly
#' `as_rtftables(x, read_meta = read_meta, split = "none", ...)[[1]]`.
#'
#' The body is the table's *rendered* body (gt via `gt::extract_body()`,
#' rtables via `formatters::matrix_form()`); only render-relevant metadata is
#' read.  See [as_rtftables()] for the full *What is carried, by source*
#' table -- in short: column labels, alignment, spanning headers, widths,
#' titles, footnotes, in-cell footnote marks and (for gt/gtsummary) explicit
#' `tab_style()` borders and text styles are carried; cell fills, fonts, gt
#' theme borders and Markdown are not.
#'
#' @param gt_obj A `gt_tbl`, a gtsummary table, an rtables/tern `VTableTree`,
#'   a `flextable`, a `huxtable`, or a plain `data.frame` / tibble.  A
#'   `gt_group` ([gt::gt_group()] / [gt::gt_split()], or a tfrmt `page_plan`
#'   render) or a gtsummary `tbl_split` container is accepted when it holds
#'   exactly one table; multi-member containers error -- convert those with
#'   [as_rtftables()].
#' @param read_meta `TRUE` (default, read all render-relevant metadata),
#'   `FALSE` (rendered body only), or a character vector of tokens.  See
#'   [as_rtftables()].
#' @param ... Passed to [rtftable()] (and on to [as_rtftables()]).  Explicit
#'   values always win over the values extracted from the source table.
#'
#' @return An `rtftable` S3 object.
#'
#' @examples
#' \dontrun{
#' library(gt)
#' g <- gt(head(mtcars, 5)) |>
#'   cols_label(mpg = "MPG", cyl = "Cyl") |>
#'   cols_align("right", columns = c(mpg, cyl))
#' tbl <- as_rtftable(g)
#' }
#'
#' @seealso [as_rtftables()] for the paginating, list-returning version and
#'   the per-source metadata table.
#'
#' @export
as_rtftable <- function(gt_obj, read_meta = TRUE, ...) {
  # A gt_group (gt::gt_group() / gt::gt_split(); tfrmt page_plan output) is
  # accepted only when it holds exactly one table -- this is a single-table
  # wrapper.  Multi-member groups belong to as_rtftables().
  if (.is_gt_group(gt_obj)) {
    tbls <- .gt_group_tables(gt_obj)
    if (length(tbls) != 1L) {
      stop("`gt_obj` is a gt_group holding ", length(tbls), " tables; use ",
           "as_rtftables() to convert them all.", call. = FALSE)
    }
    gt_obj <- tbls[[1L]]
  }
  # Same rule for a gtsummary tbl_split container.
  if (.is_gtsummary_split(gt_obj)) {
    tbls <- .gtsummary_split_tables(gt_obj)
    if (length(tbls) != 1L) {
      stop("`gt_obj` is a tbl_split holding ", length(tbls), " tables; use ",
           "as_rtftables() to convert them all.", call. = FALSE)
    }
    gt_obj <- tbls[[1L]]
  }
  # Accept gtsummary tables: convert to gt first, then validate.
  if (.is_gtsummary_tbl(gt_obj)) {
    gt_obj <- .gtsummary_to_gt(gt_obj)
  }
  is_gt  <- .is_gt_tbl(gt_obj)
  is_rtb <- .is_rtables_tbl(gt_obj)
  is_ft  <- .is_flextable_tbl(gt_obj)
  is_hux <- .is_huxtable_tbl(gt_obj)
  # NB: a huxtable is a data.frame subclass; test it before `is.data.frame()`.
  is_df  <- is.data.frame(gt_obj) && !is_hux
  if (!is_gt && !is_rtb && !is_ft && !is_hux && !is_df) {
    stop("`gt_obj` must be a gt_tbl, a gtsummary table, an rtables/tern ",
         "table (VTableTree), a flextable, a huxtable, or a data.frame/tibble.",
         call. = FALSE)
  }
  if (is_gt && !requireNamespace("gt", quietly = TRUE)) {
    stop("`as_rtftable()` requires the `gt` package.  Install it with ",
         "install.packages(\"gt\").", call. = FALSE)
  }
  if (is_ft && !requireNamespace("flextable", quietly = TRUE)) {
    stop("`as_rtftable()` requires the `flextable` package.  Install it with ",
         "install.packages(\"flextable\").", call. = FALSE)
  }
  if (is_hux && !requireNamespace("huxtable", quietly = TRUE)) {
    stop("`as_rtftable()` requires the `huxtable` package.  Install it with ",
         "install.packages(\"huxtable\").", call. = FALSE)
  }

  # Single-page convenience: delegate to as_rtftables() (split = "none")
  # and unwrap the one-element list.  All metadata extraction, merging and
  # per-cell styling lives in one place.
  as_rtftables(gt_obj, read_meta = read_meta, split = "none", ...)[[1L]]
}


# Per-column merge of col_spec lists.  `user` wins for any field it
# specifies; missing fields fall back to the corresponding entry in
# `gt`.  Either argument may be NULL.
.merge_col_spec <- function(user, gt) {
  if (is.null(user)) return(gt)
  if (is.null(gt))   return(user)

  # Build a hash keyed by `col` (numeric index or character name) so we
  # can merge entries that target the same column.
  key <- function(e) {
    if (is.null(e$col)) "" else paste0("col:", as.character(e$col))
  }
  merged <- list()
  for (e in gt) merged[[key(e)]] <- e
  for (e in user) {
    k <- key(e)
    if (!is.null(merged[[k]])) {
      # Per-field merge: user overrides gt for matching keys.
      base <- merged[[k]]
      for (f in setdiff(names(e), "col")) base[[f]] <- e[[f]]
      merged[[k]] <- base
    } else {
      merged[[k]] <- e
    }
  }
  unname(merged)
}
