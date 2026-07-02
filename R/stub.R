# Finishing helpers for plain data.frame / tibble input: shape a tidy
# "hierarchy columns + statistic columns" data.frame into the clinical
# stub layout that the rest of the pipeline (indent-based group detection,
# pagination, blank rows) already understands.

# The non-breaking space used for baked-in indentation.  The same character
# the gt / rtables / tfrmt adapters emit, so `as_rtftables(group_by = "auto")`
# detects the indented rows as group members.  Built via intToUtf8() to keep
# the R source ASCII-only.
.stub_nbsp <- function() intToUtf8(160L)

#' Merge hierarchy columns into one indented stub column
#'
#' `stub_cols()` finishes a tidy data.frame for clinical-table display: the
#' given hierarchy columns (parent first, leaf last) are merged into a
#' **single stub column**.  Each parent value becomes its own full-width
#' **label row** (the other columns are `NA`, which renders as an empty
#' cell), and the leaf rows below it are **indented** with non-breaking
#' spaces -- the layout a gt / rtables / tfrmt render bakes into its row
#' labels, produced here from plain columns.
#'
#' The result is an ordinary data.frame, so everything downstream of
#' [as_rtftables()] works unchanged: `group_by = "auto"` detects the
#' indentation, the group-aware splits keep a label row with its children
#' (and append the `" (Cont.)"` marker on a continued group),
#' `blank_rows = "between_groups"` separates the groups, and so on.
#'
#' @details
#' A label row is emitted at every **run** of a parent value (consecutive
#' rows sharing that value), not once per distinct value -- so sort the
#' input first if the hierarchy is scattered (see the `sort_by` argument of
#' [as_rtftables()], or sort the data.frame directly).  Runs are
#' hierarchical: a change in a higher-level column starts a new run in every
#' column below it.
#'
#' An `NA` or empty (`""`) parent cell contributes **no label row and no
#' indentation** for its rows.  This is the idiomatic way to keep a summary
#' row -- e.g. *"Subjects with at least one adverse event"* -- flush left at
#' the top of the table: leave its parent columns empty.
#'
#' With more than two `vars`, each additional level indents one step
#' further: level-1 label rows are flush left, level-2 label rows are
#' indented once, and so on; a leaf row is indented once per non-empty
#' ancestor.
#'
#' Column `label` attributes (the haven / labelled / xportr convention) on
#' the remaining columns are preserved, so [as_rtftables()] can still pick
#' them up as header labels.
#'
#' @param data A data.frame (or tibble).
#' @param vars The hierarchy columns to merge, **parent first, leaf last** --
#'   at least two.  A character / integer vector (or a `list()` to mix names
#'   and indices).
#' @param label Column name for the merged stub column (this is what a
#'   default column header shows).  `NULL` (default) joins the display names
#'   of the merged columns with `" / "` -- e.g. `"SOC / PT"` -- using a
#'   column's `label` attribute when it has one.
#' @param indent Integer (default `4`).  Number of non-breaking spaces
#'   prepended per nesting level.
#'
#' @return A data.frame: the stub column first, then every column of `data`
#'   not named in `vars`, in their original order.  Label rows hold `NA` in
#'   the non-stub columns.  Row count grows by one per emitted label row.
#'
#' @seealso [as_rtftables()], whose `group_by = "indent"` detection,
#'   group-aware splits and `blank_rows = "between_groups"` consume this
#'   layout directly; `collapse_repeats` / `drop_cols` there for the
#'   related repeat-suppression and hidden-column finishing.
#'
#' @examples
#' ae <- data.frame(
#'   soc = c("Cardiac disorders", "Cardiac disorders",
#'           "Gastrointestinal disorders"),
#'   pt  = c("Atrial fibrillation", "Bradycardia", "Nausea"),
#'   n   = c("3 (2.1%)", "1 (0.7%)", "5 (3.5%)"),
#'   stringsAsFactors = FALSE
#' )
#' stub_cols(ae, vars = c("soc", "pt"),
#'           label = "System Organ Class / Preferred Term")
#'
#' # A summary row stays flush left: leave its parent column empty.
#' ae2 <- rbind(
#'   data.frame(soc = "", pt = "Any adverse event", n = "9 (6.3%)",
#'              stringsAsFactors = FALSE),
#'   ae
#' )
#' tbl <- stub_cols(ae2, vars = c("soc", "pt"))
#' tbl
#'
#' # The output feeds straight into the converting / paginating pipeline.
#' pages <- as_rtftables(tbl, split = "group_force", max_rows = 4)
#'
#' @export
stub_cols <- function(data, vars, label = NULL, indent = 4L) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  idx <- .resolve_col_indices(vars, data, "vars")
  if (anyDuplicated(idx)) {
    stop("`vars` must name distinct columns.", call. = FALSE)
  }
  if (length(idx) < 2L) {
    stop("`vars` needs at least two columns (parent, then leaf) to merge.",
         call. = FALSE)
  }
  if (!is.null(label) &&
      (!is.character(label) || length(label) != 1L || is.na(label))) {
    stop("`label` must be NULL or a single string.", call. = FALSE)
  }
  indent <- as.integer(indent)
  if (length(indent) != 1L || is.na(indent) || indent < 0L) {
    stop("`indent` must be a single non-negative integer.", call. = FALSE)
  }

  n_row  <- nrow(data)
  leaf_i <- idx[length(idx)]
  par_i  <- idx[-length(idx)]
  pad    <- strrep(.stub_nbsp(), indent)

  # Parent / leaf values as character, with NA treated as "" (no group).
  as_chr <- function(j) {
    v <- as.character(data[[j]])
    v[is.na(v)] <- ""
    v
  }
  parents <- lapply(par_i, as_chr)
  leafv   <- as_chr(leaf_i)

  # Walk the rows once, emitting a label row whenever a (non-empty) parent
  # value starts a new hierarchical run, then the indented leaf row.  `src`
  # tracks the originating row (NA for inserted label rows) so the non-stub
  # columns can be sliced out of `data` afterwards.
  stub <- character(0)
  src  <- integer(0)
  prev <- NULL
  for (i in seq_len(n_row)) {
    cur <- vapply(parents, `[[`, character(1L), i)
    restart <- if (is.null(prev)) 1L else {
      changed <- which(cur != prev)
      if (length(changed)) changed[1L] else 0L
    }
    if (restart > 0L) {
      for (l in restart:length(cur)) {
        if (!nzchar(cur[l])) next
        depth_l <- sum(nzchar(cur[seq_len(l - 1L)]))
        stub <- c(stub, paste0(strrep(pad, depth_l), cur[l]))
        src  <- c(src, NA_integer_)
      }
    }
    depth <- sum(nzchar(cur))
    stub  <- c(stub, paste0(strrep(pad, depth), leafv[i]))
    src   <- c(src, i)
    prev  <- cur
  }

  # Non-stub columns: slice by source row (an NA index yields an all-NA row,
  # i.e. empty cells on the label rows).  Row subsetting drops plain vector
  # attributes, so `label` attributes are re-attached for as_rtftables().
  keep <- setdiff(seq_len(ncol(data)), idx)
  rest <- data[src, keep, drop = FALSE]
  rownames(rest) <- NULL
  for (k in seq_along(keep)) {
    lb <- attr(data[[keep[k]]], "label", exact = TRUE)
    if (!is.null(lb)) attr(rest[[k]], "label") <- lb
  }

  if (is.null(label)) {
    disp <- vapply(idx, function(j) {
      lb <- attr(data[[j]], "label", exact = TRUE)
      if (!is.null(lb) && length(lb) == 1L && !is.na(lb) && nzchar(lb)) {
        as.character(lb)
      } else {
        names(data)[j]
      }
    }, character(1L))
    label <- paste(disp, collapse = " / ")
  }

  out <- cbind(data.frame(.stub. = stub, stringsAsFactors = FALSE), rest)
  names(out) <- c(label, names(data)[keep])
  rownames(out) <- NULL
  out
}
