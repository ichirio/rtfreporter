# ============================================================================
#  fmt_count_pct() (was format_count_pct()) / .realign_count_pct()
# ============================================================================
#
#  Clinical TFL cells of the form "n (xx.x)" need consistent display widths
#  so that columns line up in a monospaced renderer.  These two functions
#  produce the same output, from two different starting points:
#
#    fmt_count_pct(count, pct)
#        - inputs: numeric vectors `count` and `pct`
#        - output: padded "n (xx.x)" strings, equal-width
#
#    .realign_count_pct(strings)   (align_count_pct = TRUE; the built-in
#                                   cell format "count_pct")
#        - inputs: character vector already containing "n (xx.x)" cells
#                  (e.g. extracted from a gt body)
#        - output: same cells re-padded for equal width.  Non-matching
#                  strings are passed through unchanged.
#
#  Width rules -- every branch produces a 10-character string, and the
#  closing parenthesis lands at column 10 so it lines up across rows:
#
#       count missing (NA/NaN)    ->  the `na` token, right-justified in
#                                     the count field ("" = empty cell)
#       count = 0, or pct missing ->  "%3d       "           (no paren)
#       pct  >= 100               ->  "%3d  (%3d)"           e.g. " 30  (100)"
#       0 < pct < 10              ->  "%3d  (%3.1f)"         e.g. "  5  (5.0)"
#       10 <= pct < 100           ->  "%3d (%4.1f)"          e.g. " 14 (50.0)"
#
#  Padding spaces are converted to non-breaking spaces (U+00A0) by default
#  so RTF / Word does not collapse them.
# ============================================================================


#' Format count + percent cells to a uniform display width
#'
#' Returns each pair `(count[i], pct[i])` as a padded `"n (xx.x)"`
#' string suitable for monospaced clinical TFL alignment.  The five
#' branches -- a count only, a percentage below 10, 10 to 99, 100 or more,
#' and a missing count (`na`) -- match the convention in the rtfreporter
#' Issue #2 reference helper.
#'
#' `fmt_count_pct()` was called `format_count_pct()`, the one cell formatter
#' not named `fmt_`: the old name is **deprecated** (it says so once a session
#' and still works) and is removed in 0.9.0, the first CRAN release.
#'
#' @param count Integer / numeric vector of whole counts (a fraction is an
#'   error).  A `0` count produces the
#'   count-only branch (no parentheses); a **missing** count produces the `na`
#'   token instead.
#' @param pct   Numeric vector of percentages.  By default expressed
#'   as a *fraction* in `[0, 1]`; pass `pct_unit = "percent"` if your
#'   values are already in `[0, 100]`.  Recycled against `count` if
#'   one argument is length 1.  A fraction above 1 says once a session that
#'   the values look like percentages (`5` prints as `500`).
#' @param pct_unit Either `"fraction"` (default, `0..1`) or
#'   `"percent"` (`0..100`).
#' @param nbsp Character used to replace the padding spaces.  Default
#'   is the non-breaking space (Unicode code point U+00A0) so that RTF
#'   and Word do not collapse leading whitespace.  Pass `" "` (regular
#'   space) for plain-text output.
#' @param pct_sign Logical (default `FALSE`).  When `TRUE`, a literal
#'   `%` is placed before the closing parenthesis (e.g. `" 14 (50.0%)"`)
#'   and every branch is one character wider so the `)` still aligns.
#' @param na Text to print when the **count** is missing (`NA`, or `NaN` --
#'   R counts it as missing).  Right-justified in the count field, so its right
#'   edge lands on the ones digit, then padded to the branch width.  The
#'   default `""` returns an empty cell.  A missing *percent* alongside a real
#'   count is not missing data: the count still prints, on its own.  `Inf` /
#'   `-Inf` counts print as `"Inf"` / `"-Inf"` -- an infinity means a division
#'   by zero upstream, and hiding it would hide the bug.
#' @param rounding How the percent is rounded to one decimal: `"r"` (half to
#'   even) or `"sas"` (half away from zero).  `NULL`, the default, reads
#'   `getOption("rtfreporter.rounding")`; see [round_num()].  `1/16` is
#'   `6.25%`, printed `"6.2"` under `"r"` and `"6.3"` under `"sas"`.
#'
#' @return Character vector the same length as `count` / `pct`.
#'
#' @examples
#' # Fractions (the default)
#' fmt_count_pct(c(5L, 14L, 30L), c(0.05, 0.50, 1.00))
#'
#' # Percent values
#' fmt_count_pct(c(5L, 14L, 30L), c(5, 50, 100), pct_unit = "percent")
#'
#' # Plain spaces if the output is going to plain text rather than RTF
#' fmt_count_pct(7L, 0.333, nbsp = " ")
#'
#' # A missing count, shown as "-" under the ones digit
#' fmt_count_pct(c(5L, NA), c(0.05, NA), na = "-", nbsp = " ")
#'
#' @seealso `as_rtftables(align_count_pct = TRUE)` (the built-in cell format
#'   `"count_pct"`) for the same widths starting from already-formatted
#'   strings.
#' @export
fmt_count_pct <- function(count, pct,
                              pct_unit = c("fraction", "percent"),
                              nbsp     = "\u00a0",
                              pct_sign = FALSE,
                              na       = "",
                              rounding = NULL) {
  pct_unit <- match.arg(pct_unit)
  rnd <- .rounder(rounding)
  na <- .check_na_text(na)
  if (!is.numeric(count) || !is.numeric(pct)) {
    stop("`count` and `pct` must both be numeric.", call. = FALSE)
  }
  # a count is whole: 5.7 would print as 5 (#765)
  frac <- is.finite(count) & count != round(count)
  if (any(frac)) {
    stop(sprintf("`count` must be whole numbers; got %s.",
                 paste(utils::head(count[frac], 3L), collapse = ", ")), call. = FALSE)
  }
  n_in <- max(length(count), length(pct))
  if (length(count) == 1L) count <- rep(count, n_in)
  if (length(pct)   == 1L) pct   <- rep(pct,   n_in)
  if (length(count) != length(pct)) {
    stop("`count` and `pct` must have the same length (or one of them ",
         "be length 1).", call. = FALSE)
  }
  if (pct_unit == "fraction") {
    # a percentage written as a fraction would print 100 times too large
    if (any(is.finite(pct) & pct > 1)) {
      .warn_once(
        "fmt_count_pct_fraction",
        paste0("`fmt_count_pct()`: `pct` has values above 1 but `pct_unit` is ",
               "\"fraction\" (0..1), so 5 prints as 500.  Pass ",
               "`pct_unit = \"percent\"` for values in 0..100."))
    }
    pct <- pct * 100
  }
  # Round ONCE, with the package's rule, before any branch reads the value:
  # sprintf() would otherwise round the binary number its own way (6.25 ->
  # "6.2" whatever the study wants), and a 9.96 would pick the "< 10" width
  # and then print as "10.0".
  pct <- rnd(pct, 1L)

  # When pct_sign = TRUE a "%" is added before the closing paren and every
  # branch is one character wider, so the ")" still aligns across cells.
  out <- vapply(seq_len(n_in), function(i) {
    c1 <- count[i]; p <- pct[i]
    w <- if (pct_sign) 11L else 10L                    # full width of a cell
    if (is.na(c1)) {
      # The count itself is missing -- print the token in the count field.
      if (!nzchar(na)) return("")
      raw <- formatC(formatC(na, width = 3L, flag = ""), width = w, flag = "-")
    } else if (!is.finite(c1)) {
      # Inf / -Inf is not missing; show it rather than hide a bad numerator.
      raw <- formatC(formatC(as.character(c1), width = 3L, flag = ""),
                     width = w, flag = "-")
    } else if (!is.finite(p) || c1 == 0) {
      # A real count with no usable percent still prints, on its own.
      raw <- if (pct_sign) sprintf("%3d        ", as.integer(c1))
             else          sprintf("%3d       ",  as.integer(c1))
    } else if (p >= 100) {
      # Two spaces before '(' so the ')' aligns with the other
      # paren-bearing branches.
      raw <- if (pct_sign) sprintf("%3d  (%3d%%)", as.integer(c1), rnd(p, 0L))
             else          sprintf("%3d  (%3d)",   as.integer(c1), rnd(p, 0L))
    } else if (p < 10) {
      raw <- if (pct_sign) sprintf("%3d  (%3.1f%%)", as.integer(c1), p)
             else          sprintf("%3d  (%3.1f)",   as.integer(c1), p)
    } else {
      raw <- if (pct_sign) sprintf("%3d (%4.1f%%)", as.integer(c1), p)
             else          sprintf("%3d (%4.1f)",   as.integer(c1), p)
    }
    raw
  }, character(1L))

  if (!identical(nbsp, " ")) out <- gsub(" ", nbsp, out, fixed = TRUE)
  out
}


#' Re-align existing "n (xx.x)" strings to a uniform display width
#'
#' **Deprecated** in 0.8.x (it says so once a session and still works),
#' removed in 0.9.0: the same alignment is `as_rtftables(align_count_pct =
#' TRUE)`, or the built-in cell format `"count_pct"`
#' (`as_rtftables(cell_format = "count_pct")`).
#'
#' Scans `x` for cells matching the clinical-TFL pattern `"n (xx.x)"`
#' (e.g. `"5 (33.3)"`), parses the count and percent, and reformats
#' them through [fmt_count_pct()] so every cell is the same width.
#' Cells that do not match are returned unchanged.
#'
#' @param x Character vector.  Cells that match the regex
#'   `^\\d+ \\(\\d+(\\.\\d+)?\\)$` are reformatted; all others are
#'   returned unchanged.
#' @param nbsp Padding character (see [fmt_count_pct()]).
#' @param na Text to print for a missing cell -- an `NA` in `x`, or a cell that
#'   already holds this token because [as_rtftables()] substituted it.  It is
#'   right-justified in the count field, so its right edge lands on the ones
#'   digit.  The default `""` leaves the cell empty, as before.  Text that is
#'   neither the token nor a count/percent cell (`"NE"`, `"n/a"`, free text) is
#'   still returned unchanged and unpadded.
#'
#' @return Character vector the same length as `x`.
#'
#' @examples
#' # deprecated: the same as as_rtftables(align_count_pct = TRUE) does
#' suppressWarnings(
#'   realign_count_pct(c("5 (33.3)", "12 (100.0)", "0 (0.0)",
#'                       "not a count", "1 (5.0)", "1 (50.0)")))
#'
#' @seealso [fmt_count_pct()] for the numeric -> string variant.
#' @keywords internal
#' @export
realign_count_pct <- function(x, nbsp = "\u00a0", na = "") {
  .deprecate_once(
    "realign_count_pct",
    paste0("`realign_count_pct()` is deprecated: align the column with ",
           "`as_rtftables(align_count_pct = TRUE)` (the built-in cell format ",
           "\"count_pct\").\n  Removed in 0.9.0."))
  .realign_count_pct(x, nbsp = nbsp, na = na)
}

# The alignment itself: what align_count_pct = TRUE and the built-in cell
# format "count_pct" apply
.realign_count_pct <- function(x, nbsp = "\u00a0", na = "") {
  if (is.null(x) || length(x) == 0L) return(x)
  na <- .check_na_text(na)
  if (!is.character(x)) x <- as.character(x)
  out <- x
  # Optional trailing "%" inside the parens is captured so that cells like
  # "8 (28.6%)" (e.g. from tern::count_occurrences) are realigned WITH the
  # "%" preserved.  Cells without "%" keep the original "n (xx.x)" form.
  rx  <- "^\\s*(\\d+)\\s*\\((\\d+(?:\\.\\d+)?)(%?)\\)\\s*$"
  m   <- regmatches(x, regexec(rx, x))
  hit <- vapply(m, function(g) length(g) == 4L && !is.na(g[1L]) &&
                               nzchar(g[1L]), logical(1L))
  # A missing cell has no "%" of its own to go by, so it follows whatever the
  # rest of the column does -- every branch is one character wider with a "%".
  # And with no count/percent cell anywhere in the column there is nothing to
  # line the token up WITH, so it is left alone rather than padded to a width
  # this column never uses.
  col_pct_sign <- any(vapply(m[hit], function(g) nzchar(g[4L]), logical(1L)))
  pad_na       <- nzchar(na) && any(hit)
  for (i in seq_along(x)) {
    if (hit[i]) {
      g        <- m[[i]]
      n        <- as.integer(g[2L])
      pct      <- as.numeric(g[3L])
      pct_sign <- nzchar(g[4L])
      out[i] <- fmt_count_pct(n, pct, pct_unit = "percent",
                              nbsp = nbsp, pct_sign = pct_sign)
    } else if (nzchar(na) &&
               (is.na(x[i]) || identical(trimws(x[i]), na))) {
      out[i] <- if (pad_na) {
        fmt_count_pct(NA_integer_, NA_real_, pct_unit = "percent",
                      nbsp = nbsp, pct_sign = col_pct_sign, na = na)
      } else {
        na                       # shown, but with nothing to line it up with
      }
    }
  }
  out
}

#' Format count + percent cells (old name)
#'
#' **Deprecated** in 0.8.x (it says so once a session and still works),
#' removed in 0.9.0: the function is [fmt_count_pct()] now, named like the
#' other cell formatters.
#'
#' @inheritParams fmt_count_pct
#' @return As [fmt_count_pct()].
#' @examples
#' suppressWarnings(format_count_pct(5L, 0.05))   # fmt_count_pct(5L, 0.05)
#' @keywords internal
#' @export
format_count_pct <- function(count, pct,
                             pct_unit = c("fraction", "percent"),
                             nbsp     = "\u00a0",
                             pct_sign = FALSE,
                             na       = "",
                             rounding = NULL) {
  .deprecate_once(
    "format_count_pct",
    paste0("`format_count_pct()` is deprecated: it is `fmt_count_pct()` now, ",
           "with the same arguments.\n  Removed in 0.9.0."))
  fmt_count_pct(count, pct, pct_unit = pct_unit, nbsp = nbsp, pct_sign = pct_sign,
                na = na, rounding = rounding)
}
