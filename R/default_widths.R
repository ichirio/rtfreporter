# ---------------------------------------------------------------------------
#  The column widths of a table that gives none
# ---------------------------------------------------------------------------
#  A program gives its widths (`col_rel_width`, `column_widths_twips`, a
#  plan's `plan_columns(widths = )`).  One that does not -- a draft, a quick
#  look -- gets these, better than equal columns:
#
#    * the row-heading columns (`row_title`, column 1 by default) as wide as
#      their longest line with room (up to half again, or one and a half
#      data columns), within a share of the table: 40% at most, less
#      when there are many data columns (2 points less a column over four,
#      20% at the least), 5 points more when the rows are indented (a
#      hierarchy: SOC / PT, a statistic under its variable);
#    * the data columns share the rest equally;
#    * a data column whose longest cell does not fit its share is widened to
#      fit, taken from the others as far as they stay wide enough for theirs.
#
#  Widths are counted in characters, for a table 120 characters wide (a
#  landscape page in a 9-point monospace font): the proportions are the same
#  at any page size, so a table split into column pages keeps them.
# ---------------------------------------------------------------------------

#' The column widths of a table that gives none
#'
#' The proportions [rtftable()] uses when a table gives no widths (no
#' `col_rel_width`, `column_widths_twips`, or a plan's `plan_columns(widths = )`):
#' the row-heading columns as wide as their longest line, within a share of
#' the table (at most 40%; less with many data columns, down to 20%; 5
#' points more when the rows are indented), the data columns sharing the
#' rest equally, and a data column whose longest cell does not fit widened
#' from the others.  A program that wants these as its starting point writes
#' them out: `col_rel_width = default_col_widths(data)` gives the same table,
#' and is then changed by hand.
#'
#' @param data A data frame: the table's body, as it prints.
#' @param row_title The row-heading columns, by position or name; `NULL`
#'   (default) is column 1, as in [rtftable()].
#' @param col_header The column header's labels (one row, or a list of rows:
#'   only the last is counted), or `NULL` for `names(data)`.  A header wraps,
#'   so it counts by its longest word.
#' @param width_chars The table's width in characters of its font: 120 (the
#'   default) is a landscape letter or A4 page in a 9-point monospace font.
#'   It decides whether a long cell fits its share.
#' @param digits The proportions are rounded to this many decimals and
#'   scaled so the smallest data column is 1.
#' @return A named numeric vector, one per column of `data`: relative widths,
#'   as `col_rel_width` (and `plan_columns(widths = )`) take them.
#' @seealso [auto_col_widths()] (widths in twips from the content alone).
#' @examples
#' df <- data.frame(Characteristic = c("Age (years)", "  Mean (SD)", "  Median"),
#'                  Placebo = c("", "75.2 (8.59)", "76.0"),
#'                  Active  = c("", "74.4 (7.89)", "75.0"))
#' default_col_widths(df)
#' @export
default_col_widths <- function(data, row_title = NULL, col_header = NULL, width_chars = 120,
                               digits = 1L) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  rt <- .normalize_row_title(row_title, ncol(data), names(data))
  w <- .default_col_weights(data, rt, col_header, width = width_chars)
  data_cols <- setdiff(seq_len(ncol(data)), rt)
  unit <- if (length(data_cols)) min(w[data_cols]) else min(w)
  out <- round(w / unit, digits)
  stats::setNames(out, names(data))
}

# The widths, in characters, of each column of `data` by the rule above, for
# a table `width` characters wide.  `rt`: the row-heading columns' positions.
.default_col_weights <- function(data, rt, col_header = NULL, width = 120) {
  n <- ncol(data)
  if (n == 0L) return(numeric())
  rt <- intersect(as.integer(rt %||% 1L), seq_len(n))
  data_cols <- setdiff(seq_len(n), rt)
  if (!length(rt) || !length(data_cols)) return(rep(1, n))
  cells <- lapply(data, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  longest_line <- function(x) {
    if (!length(x)) return(0L)
    max(0L, vapply(strsplit(x, "\r\n|\r|\n"), function(l) {
      if (length(l)) max(nchar(l, type = "width")) else 0L
    }, integer(1L)))
  }
  # a header wraps: its longest word
  hdr <- col_header %||% names(data)
  if (is.list(hdr)) hdr <- hdr[[length(hdr)]]
  hdr <- as.character(rep_len(c(hdr, character(n)), n))
  hdr[is.na(hdr)] <- ""
  hdr_word <- vapply(hdr, function(h) {
    words <- unlist(strsplit(h, "[[:space:]]+"))
    if (length(words)) max(nchar(words, type = "width")) else 0L
  }, integer(1L), USE.NAMES = FALSE)
  # what each column needs, in characters (+2 for the cell's padding)
  need <- vapply(seq_len(n), function(j) {
    max(longest_line(cells[[j]]), if (j %in% data_cols) hdr_word[j] else 0L) + 2
  }, numeric(1L))
  # the row headings: their content, within their share of the width
  nd <- length(data_cols)
  cap <- max(0.20, 0.40 - 0.02 * max(0L, nd - 4L))
  indented <- any(vapply(rt, function(j) any(grepl("^[[:space:]]+[^[:space:]]", cells[[j]])),
                         logical(1L)))
  if (indented) cap <- cap + 0.05
  # (with room: up to one and a half data columns, or half again its need)
  floor_w <- min(1.5 * width / (nd + 1.5), 1.5 * sum(need[rt]))
  # (and at least half what an equal split would give it)
  floor_w <- max(floor_w, 0.5 * width * length(rt) / n)
  rt_w <- min(max(sum(need[rt]), floor_w), max(cap * width, 0.5 * width * length(rt) / n))
  rest <- width - rt_w
  # the data columns: equal; one whose longest cell does not fit is widened,
  # from the others as far as they keep what they need (half a share at least)
  equal <- rest / nd
  d <- rep(equal, nd)
  long <- need[data_cols] > equal
  if (any(long) && !all(long)) {
    keep <- pmin(equal, pmax(need[data_cols][!long], equal / 2))
    spare <- rest - sum(keep)
    want <- need[data_cols][long]
    d[long] <- if (sum(want) <= spare) want else want * spare / sum(want)
    d[!long] <- keep + (rest - sum(d[long]) - sum(keep)) * keep / sum(keep)
  }
  w <- numeric(n)
  w[rt] <- rt_w * need[rt] / sum(need[rt])
  w[data_cols] <- d
  w
}

# A built table's default weights (one per column of its first page), for
# a table 120 characters wide: the same at any page size, so a table split
# into column pages keeps them.  NULL when the table has no data frame.
.default_table_weights <- function(tbl) {
  ref <- if (!is.null(tbl$data_list)) tbl$data_list[[1L]] else tbl$data
  if (!is.data.frame(ref) || !ncol(ref)) return(NULL)
  hdr <- tbl$col_header
  if (is.list(hdr) && !is.null(hdr$labels)) hdr <- hdr$labels
  if (!is.character(hdr) && !is.list(hdr)) hdr <- NULL
  tryCatch(.default_col_weights(ref, tbl$row_title, hdr, width = 120),
           error = function(e) rep(1, ncol(ref)))
}

# The column widths in twips of a table that gives none: the weights above,
# over the table's width.  The last column absorbs the rounding.
.default_cellx_widths <- function(tbl, ncols, total_w) {
  w <- .default_table_weights(tbl)
  if (length(w) != ncols) w <- rep(1, ncols)
  widths <- as.integer(round(total_w * w / sum(w)))
  widths[ncols] <- total_w - sum(widths[-ncols])
  widths
}
