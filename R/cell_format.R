# ============================================================================
#  Pluggable cell-format functions
# ============================================================================
#
#  `as_rtftables(cell_format = )` lets you re-format the *body cells* of a
#  table column-by-column just before pagination -- typically to line numbers
#  up in a monospaced clinical layout.
#
#  ---------------------------------------------------------------------------
#  THE CONTRACT (how to write your own format function)
#  ---------------------------------------------------------------------------
#  A cell-format function takes ONE table column and returns the reformatted
#  column:
#
#      function(x, nbsp = "\u00a0", na = "") -> character
#
#    * `x`   : a character vector -- the cells of a single column.
#    * `na`  : OPTIONAL.  The text the table prints for a missing value
#              (the `na` argument of [as_rtftables()]).  Declare this
#              formal only if your function wants to line the token up;
#              one written to the older two-argument contract is called
#              exactly as it was before.
#    * value : a character vector of the SAME length as `x`.
#
#  Rules:
#    * Return the same length you were given (one element per row); never drop
#      or add rows.
#    * Cells you do not want to touch (e.g. empty group-label cells, or values
#      that do not match your pattern) must be returned unchanged.
#    * Pad with the non-breaking space `"\u00a0"` (the `nbsp` default), NOT a
#      regular space -- RTF / Word collapse leading and repeated normal spaces,
#      which would undo your alignment.
#    * The function is called once per column (see `cell_format` in
#      [as_rtftables()]); it does not know which column it is, so base any
#      width decisions on `x` alone.
#
#  rtfreporter ships a few ready-made format functions (below).  When none of
#  them fits your data's exact notation, write your own following the rules
#  above and pass it as `cell_format`.
# ============================================================================


# Internal: validate a missing-value display text.  A single string; `NA`
# itself is rejected, since the whole point of the argument is to say what a
# missing value should look like.
.check_na_text <- function(na, arg = "na") {
  if (is.null(na)) return("")
  if (!is.character(na) || length(na) != 1L || is.na(na)) {
    stop(sprintf(paste0("`%s` must be a single string -- the text printed for ",
                        "a missing value (e.g. \"-\" or \"NA\").  \"\" (the ",
                        "default) leaves the cell empty."), arg), call. = FALSE)
  }
  na
}


# Internal: print `na` wherever the body has a missing value.  Applied to EVERY
# column (the row-label column included) at the cell-format stage, so it does
# not depend on `align_count_pct` / `cell_format` being set.  Because that
# stage runs BEFORE the split it never touches the `NA`s that
# `collapse_repeats` writes per page to blank out repeated values.
#
# `is.na()` is the test, so `NaN` counts as missing too -- R says so, and in a
# TFL a NaN is a 0/0 percentage.  `Inf` / `-Inf` are NOT missing: they are left
# alone and print as "Inf" / "-Inf", because an infinity means a division by
# zero upstream and rendering it as "-" would hide the bug.
.replace_na_df <- function(df, na) {
  if (!nzchar(na) || ncol(df) == 0L) return(df)
  for (j in seq_len(ncol(df))) {
    col <- df[[j]]
    if (is.list(col)) next
    bad <- is.na(col)
    if (!any(bad)) next
    col      <- as.character(col)
    col[bad] <- na
    df[[j]]  <- col
  }
  df
}


#' Right-align the cells of a column to a common width
#'
#' A minimal cell-format function (see *The contract* in the
#' \code{vignette} / [as_rtftables()]): every non-empty cell is right-justified
#' to the width of the widest cell, padding on the left with non-breaking
#' spaces.  Empty cells are left empty.  This is the simplest useful formatter
#' and a good template for writing your own.
#'
#' @param x Character vector (one table column).
#' @param nbsp Padding character; defaults to the non-breaking space
#'   (U+00A0) so RTF / Word keep the alignment.  Pass `" "` for plain text.
#' @param na Text to print for a missing value (`NA`, and `NaN` -- R counts it
#'   as missing).  The default `""` leaves the cell empty, as before.  A
#'   non-empty token is right-justified with the other cells, so its right edge
#'   lines up with theirs.  `Inf` / `-Inf` are **not** missing and print as
#'   `"Inf"` / `"-Inf"`.  See the `na` argument of [as_rtftables()].
#'
#' @return Character vector the same length as `x`.
#'
#' @examples
#' fmt_right_align(c("5", "120", "7"))
#'
#' @seealso [fmt_count_paren()], [realign_count_pct()], and the `cell_format`
#'   argument of [as_rtftables()].
#' @export
fmt_right_align <- function(x, nbsp = "\u00a0", na = "") {
  if (length(x) == 0L) return(x)
  na <- .check_na_text(na)
  x <- as.character(x)
  x[is.na(x)] <- na
  nz <- nzchar(trimws(x))
  if (!any(nz)) return(x)
  w   <- max(nchar(x[nz]))
  out <- x
  out[nz] <- formatC(x[nz], width = w, flag = "")   # right-justify
  if (!identical(nbsp, " ")) out <- gsub(" ", nbsp, out, fixed = TRUE)
  out
}


# Internal core for the count/percent aligners.  Scans the column, then
# right-justifies the integer count and right-justifies the text inside the
# parentheses, so the count digit and the percentage line up.
#
# `bare` decides whether cells with NO parentheses are touched:
#   * bare = FALSE  -> only "count (...)" cells are reformatted; a lone count
#                      (e.g. "0", or a raw total) is left exactly as-is.
#   * bare = TRUE   -> lone integer counts are ALSO padded into the same count
#                      field so they line up under the parenthetical cells.
# Cells that are not reformatted are returned byte-for-byte unchanged (no
# non-breaking-space substitution).
#
# `na` is the text a missing value prints as.  A missing cell is aligned like a
# BARE count whatever `bare` says -- asking for the token is asking to see it
# under the counts -- and the count field widens to the token if the token is
# the wider of the two, so the right edges still meet.
.fmt_count_core <- function(x, nbsp, bare, na = "") {
  if (length(x) == 0L) return(x)
  na <- .check_na_text(na)
  x <- as.character(x)
  x[is.na(x)] <- na
  rx <- "^[[:space:]]*([0-9]+)[[:space:]]*(\\((.*)\\))?[[:space:]]*$"
  m  <- regmatches(x, regexec(rx, x))
  count  <- rep(NA_character_, length(x))
  inner  <- rep("", length(x))
  haspar <- rep(FALSE, length(x))
  for (i in seq_along(x)) {
    g <- m[[i]]
    if (length(g) == 4L && nzchar(g[2L])) {
      count[i] <- g[2L]
      if (nzchar(g[3L])) { haspar[i] <- TRUE; inner[i] <- g[4L] }
    }
  }
  # Cells holding the missing-value token.  With na = "" (the default) `nat` is
  # all FALSE and everything below behaves exactly as it did before.
  nat <- nzchar(na) & is.na(count) & !is.na(x) & trimws(x) == na
  do  <- (!is.na(count) & (haspar | bare)) | nat  # cells we actually reformat
  if (!any(do)) return(x)
  isc  <- do & !nat                                             # the real counts
  wc   <- max(c(nchar(count[isc]),
                if (any(nat)) nchar(na) else NULL))             # count width
  wi <- if (any(haspar & do)) max(nchar(inner[haspar & do])) else 0L  # inner width
  full <- wc + if (wi > 0L) (2L + wi + 1L) else 0L              # "<count> (<inner>)"
  out <- x
  for (i in which(do)) {
    if (nat[i]) {                                               # missing token
      val <- formatC(formatC(na, width = wc, flag = ""),
                     width = max(full, wc), flag = "-")
    } else {
      cc <- formatC(count[i], width = wc, flag = "")            # right-justify count
      if (haspar[i]) {
        ii  <- formatC(inner[i], width = wi, flag = "")         # right-justify inner
        val <- paste0(cc, " (", ii, ")")
      } else {
        val <- formatC(cc, width = max(full, wc), flag = "-")   # bare count padded
      }
    }
    if (!identical(nbsp, " ")) val <- gsub(" ", nbsp, val, fixed = TRUE)
    out[i] <- val
  }
  out
}

#' Align "count (parenthetical)" cells
#'
#' Aligns clinical cells made of an integer **count** followed by a
#' **parenthetical** part -- e.g. `"69 (80.2%)"`, `"3 (<1%)"`, `"70 (100%)"`.
#' It scans the whole column, then right-justifies the count to the widest
#' count and right-justifies the text *inside* the parentheses to the widest
#' one, so the count digit **and** the percentage line up across rows.
#'
#' Only cells that have parentheses are touched; cells **without** them -- a
#' lone count such as `"0"` or a raw total, a continuous statistic like
#' `"75.2 (8.6)"` whose "count" is not an integer, free text, or empty
#' group-label cells -- are returned **unchanged**.  Use
#' [fmt_count_paren_bare()] if you also want bare integer counts padded into
#' the same column.
#'
#' Unlike the fixed-width [realign_count_pct()] this adapts to the column's
#' actual digit counts and does not care what is *inside* the parentheses,
#' coping with mixed notations like `"(<1%)"`, `"(100%)"` and `"( 2.8%)"` in
#' one column (e.g. tables produced by `tfrmt`).
#'
#' With `na` set, a missing cell prints that token right-justified in the count
#' field -- its right edge under the ones digit -- so it stays in line instead
#' of falling out of the column.
#'
#' @inheritParams fmt_right_align
#'
#' @return Character vector the same length as `x`.
#'
#' @examples
#' # Only the parenthetical cells are aligned; the lone "0" is left as-is.
#' fmt_count_paren(c("1 (1.2%)", "0", "11 (3.6%)", "108 (35.3%)"))
#'
#' # A missing cell lines up under the counts.
#' fmt_count_paren(c("1 (1.2%)", NA, "108 (35.3%)"), na = "-", nbsp = " ")
#'
#' @seealso [fmt_count_paren_bare()], [fmt_right_align()],
#'   [realign_count_pct()], and the `cell_format` argument of [as_rtftables()].
#' @export
fmt_count_paren <- function(x, nbsp = "\u00a0", na = "") {
  .fmt_count_core(x, nbsp = nbsp, bare = FALSE, na = na)
}

#' Align "count (parenthetical)" cells, including bare counts
#'
#' Like [fmt_count_paren()], but a **bare integer count** with no parentheses
#' (a lone `"0"` for a zero count, or a raw event total) is also padded into
#' the same count field, so it lines up under the parenthetical cells instead
#' of drifting out of line.  Cells that do not start with an integer (text,
#' decimals, empty cells) are still returned unchanged.
#'
#' @inheritParams fmt_right_align
#'
#' @return Character vector the same length as `x`.
#'
#' @examples
#' # The lone "0" is padded to share the column width.
#' fmt_count_paren_bare(c("1 (1.2%)", "0", "11 (3.6%)", "108 (35.3%)"))
#'
#' @seealso [fmt_count_paren()] (parenthetical cells only).
#' @export
fmt_count_paren_bare <- function(x, nbsp = "\u00a0", na = "") {
  .fmt_count_core(x, nbsp = nbsp, bare = TRUE, na = na)
}


# Internal: resolve the `cell_format` argument into a per-column list of
# functions (length `ncol`; NULL entries = leave the column untouched).
#
#   * a single function -> applied to columns 2..ncol (column 1 is the row
#     label and is left alone, the usual clinical convention);
#   * a list            -> taken positionally, `cell_format[[j]]` for column j
#     (entries that are not functions are ignored).
.resolve_cell_format <- function(cell_format, ncol) {
  if (is.null(cell_format) || ncol < 1L) return(NULL)
  fl <- vector("list", ncol)
  if (is.function(cell_format)) {
    if (ncol >= 2L) for (j in 2:ncol) fl[[j]] <- cell_format
  } else if (is.list(cell_format)) {
    n <- min(length(cell_format), ncol)
    for (j in seq_len(n)) {
      if (is.function(cell_format[[j]])) fl[[j]] <- cell_format[[j]]
    }
  } else {
    stop("`cell_format` must be a function or a list of functions.",
         call. = FALSE)
  }
  fl
}

# Internal: call one cell-format function.  `na` reaches only the functions
# that declare it, so a user function written to the older two-argument
# contract keeps being called exactly as it always was.
.call_cell_format <- function(f, col, na) {
  fmls <- tryCatch(names(formals(args(f))), error = function(e) NULL)
  if ("na" %in% fmls) f(col, na = na) else f(col)
}

# Internal: apply a resolved per-column format list to a data.frame's
# character columns.
.apply_cell_format <- function(df, fl, na = "") {
  for (j in seq_along(fl)) {
    f <- fl[[j]]
    if (is.function(f) && j <= ncol(df) && is.character(df[[j]])) {
      formatted <- .call_cell_format(f, df[[j]], na)
      if (length(formatted) != nrow(df)) {
        stop(sprintf(paste0("A `cell_format` function must return a vector the ",
                            "same length as the column (got %d, expected %d)."),
                     length(formatted), nrow(df)), call. = FALSE)
      }
      df[[j]] <- as.character(formatted)
    }
  }
  df
}
