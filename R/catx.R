# ============================================================================
#  catx() -- join values with a separator, skipping the missing ones (#360)
# ============================================================================
#
#  `listing_col(vars = )` joins several source variables into one printed
#  column, and the rule it uses -- separator between the values, missing and
#  empty ones dropped rather than printed as a doubled separator -- is the SAS
#  `CATX` rule that clinical programmers already write by hand everywhere else
#  in a listing program.
#
#  Exporting it does two things.  It lets a listing be prepared without
#  reaching outside the package for a join, and it makes the joins INSIDE a
#  listing and the joins a user writes for a column of their own the same
#  function, so they cannot drift apart: `.listing_combine()` below is a call
#  to this.

#' Join values with a separator, skipping the missing ones
#'
#' Concatenates its arguments element by element, separated by `sep`, leaving
#' out every value that is missing (`NA`) or empty (`""`).  This is the SAS
#' `CATX` rule, and it is what [listing_col()] applies to its `vars`: a record
#' whose middle value is missing prints `"COMPLETED/ADENOCARCINOMA"`, never
#' `"COMPLETED//ADENOCARCINOMA"`.
#'
#' Arguments are vectorised together.  A length-1 argument is recycled to the
#' longest one; any other length mismatch is an error, because in a clinical
#' listing it means two columns that were meant to line up do not.
#'
#' Leading and trailing blanks are **stripped from each value** before it is
#' joined -- again as SAS `CATX` does.  Source data read from a SAS-shaped
#' export is routinely padded, and padding that survived into a joined cell
#' would push the text over the column width for no visible reason.  (This is
#' the one place the function differs from `ydisctools::catx()`, which keeps
#' the padding.)
#'
#' @param sep Separator, a single string.  It is inserted only *between*
#'   values that survive, so it never appears at the start or the end.
#' @param ... The values to join: character vectors, or anything
#'   `as.character()` accepts.  A factor joins by its **labels**, not its
#'   level codes.
#'
#' @return A character vector as long as the longest argument.  With no `...`
#'   arguments, `character(0)`.
#'
#' @seealso [listing_col()], which joins a listing column's `vars` this way;
#'   [build_listing()], which applies it.
#'
#' @examples
#' catx("/", "COMPLETED", "BRCA1", "ADENOCARCINOMA")
#'
#' # A missing value is skipped, not printed as a doubled separator.
#' catx("/", "COMPLETED", NA, "ADENOCARCINOMA")
#'
#' # Vectorised, with length-1 arguments recycled.
#' adsl <- data.frame(
#'   HIST = c("ADENOCARCINOMA", "SMALL CELL"),
#'   BRCA = c("BRCA1", NA),
#'   stringsAsFactors = FALSE
#' )
#' catx("/", adsl$HIST, adsl$BRCA)
#' catx(" ", "Cohort", c("A", "B"))
#'
#' # Any separator, including none at all.
#' catx(", ", "Grade 3", "Prior radiation")
#' catx("", "AE", "0001")
#'
#' @export
catx <- function(sep, ...) {
  if (missing(sep) || !is.character(sep) || length(sep) != 1L || is.na(sep)) {
    stop("`sep` must be a single string.", call. = FALSE)
  }
  vals <- list(...)
  if (length(vals) == 0L) return(character(0))

  vals <- lapply(vals, function(x) {
    if (is.factor(x)) x <- as.character(x)
    x <- as.character(x)
    x[is.na(x)] <- ""
    trimws(x)
  })

  lens <- vapply(vals, length, integer(1L))
  n    <- max(lens)
  if (n == 0L) return(character(0))
  if (any(lens != n & lens != 1L)) {
    stop(sprintf(paste0("All arguments must be the same length, or length 1: ",
                        "got %s."), paste(lens, collapse = ", ")),
         call. = FALSE)
  }
  vals[lens == 1L] <- lapply(vals[lens == 1L], rep, times = n)

  vapply(seq_len(n), function(i) {
    parts <- vapply(vals, function(v) v[[i]], character(1L))
    paste(parts[nzchar(parts)], collapse = sep)
  }, character(1L))
}
