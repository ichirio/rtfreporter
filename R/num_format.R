# ============================================================================
#  Numeric -> character formatters (significant digits / decimal places)
# ============================================================================
#
#  rtfreporter renders whatever text it is given.  A column that is still
#  NUMERIC when it reaches the renderer is converted with `as.character()`,
#  which prints 15 significant digits, switches to scientific notation outside
#  1e-4 .. 1e5, and -- worst for a deliverable -- depends on the session's
#  `options(scipen)`.  These functions turn numbers into the text the report is
#  meant to show, BEFORE the table is built.
#
#  They are a DATA step, deliberately separate from `set_decimal_split()`,
#  which is a render option applied to a finished table.  Formatting first and
#  splitting second is then structural rather than something to remember, and
#  the split measures the formatted text instead of a 15-digit string.
#
#  Character columns are out of scope throughout: they are taken as already
#  formatted and pass through untouched.

# Digits in the integer part of |x| (0 counts as one digit).
.int_digits <- function(x) {
  ax <- floor(abs(x))
  ifelse(ax < 1, 1L, as.integer(floor(log10(ax))) + 1L)
}

# Half-away-from-zero rounding, as SAS does it.  The epsilon term defeats the
# binary representation of values like 2.675, stored as 2.67499999999999982.
.round_half_up <- function(x, digits = 0L) {
  z <- abs(x) * 10^digits
  sign(x) * floor(z + 0.5 + sqrt(.Machine$double.eps) * abs(z)) / 10^digits
}

.rounder <- function(rounding) {
  if (identical(rounding, "sas")) .round_half_up else base::round
}

# Decimal places implied by a significant-digit request for ONE value.
#   x == 0            -> `digits - 1` (0 has no significant digits)
#   |x| < 1, "signif" -> true significant figures, counted from the first one
#   otherwise         -> digits - (digits in the integer part)
.signif_decimals <- function(x, digits, small) {
  if (x == 0) return(max(0L, digits - 1L))
  if (abs(x) < 1 && identical(small, "signif")) {
    return(max(0L, digits - 1L - as.integer(floor(log10(abs(x))))))
  }
  max(0L, digits - .int_digits(x))
}

# Shared scalar formatter.  `dec_fun(v)` returns the decimals to use for `v`.
.fmt_num_one <- function(v, dec_fun, rnd, na, recompute) {
  if (is.na(v)) return(na)
  if (!is.finite(v)) return(as.character(v))
  d <- dec_fun(v)
  r <- rnd(v, d)
  if (recompute) {
    # rounding can carry |x| into another decade (99.995 -> 100.0)
    d2 <- dec_fun(r)
    if (!identical(d2, d)) { d <- d2; r <- rnd(v, d) }
  }
  sprintf("%.*f", as.integer(d), r)
}

.check_num_input <- function(x, fn) {
  if (!is.numeric(x)) {
    stop(sprintf("`%s()` formats numeric input; got %s. Character columns are ",
                 fn, paste(class(x), collapse = "/")),
         "taken as already formatted -- leave them alone.", call. = FALSE)
  }
  invisible(x)
}

.check_digits <- function(d, arg, fn) {
  d <- suppressWarnings(as.integer(d))
  if (length(d) != 1L || is.na(d) || d < 0L) {
    stop(sprintf("`%s(%s = )` must be a single non-negative integer.", fn, arg),
         call. = FALSE)
  }
  d
}


#' Format numbers to a number of significant digits
#'
#' @description
#' Renders a numeric vector as the text a clinical table should show, counting
#' **total printed digits including the integer part** -- the convention a SAP
#' means by "report to 4 significant digits", which is not the same as
#' [base::signif()]:
#'
#' \preformatted{
#' fmt_signif(c(0, 10.2, 103.4, 20.333333, 23.4463), digits = 4)
#' #> "0.000" "10.20" "103.4" "20.33" "23.45"
#' }
#'
#' The decimal places are `digits` minus the number of digits in the integer
#' part, never below zero -- so an integer part longer than `digits` simply
#' prints whole (`12345.6` at 4 digits is `"12346"`; integer digits are never
#' dropped). When rounding carries the value into another decade the decimals
#' are recomputed, so the result really does carry `digits` digits
#' (`99.995` is `"100.0"`, not `"100.00"`).
#'
#' @section Values below 1:
#' `small` decides what a value with no integer part means. `"signif"` (the
#' default) counts from the first significant digit, so precision is kept;
#' `"fixed"` applies the integer-counting rule everywhere, which can erase a
#' small value entirely:
#'
#' \tabular{lll}{
#'   **x** \tab **`"signif"`** \tab **`"fixed"`** \cr
#'   `0.333333`  \tab `0.3333`    \tab `0.333` \cr
#'   `0.0004567` \tab `0.0004567` \tab `0.000` \cr
#'   `0.00998`   \tab `0.009980`  \tab `0.010`
#' }
#'
#' `0.000` for a concentration near the limit of quantitation is the reason
#' `"signif"` is the default. Zero itself prints with `digits - 1` decimals
#' under both (`"0.000"` at 4), having no significant digits to count.
#'
#' @param x A numeric vector. Character input is an error -- character columns
#'   are taken as already formatted.
#' @param digits Total significant digits. Default `3`.
#' @param rounding `"r"` (default) rounds with [base::round()], which is
#'   banker's rounding; `"sas"` rounds half away from zero as SAS does. They
#'   differ on exact halves -- `23.445` is `"23.44"` under `"r"` and `"23.45"`
#'   under `"sas"`.
#' @param small `"signif"` (default) or `"fixed"`; see *Values below 1*.
#' @param na Text for `NA` and `NaN`. Default `""` (an empty cell, which is how
#'   rtfreporter renders a missing value anyway).
#'
#' @return A character vector the same length as `x`. `Inf` / `-Inf` pass
#'   through as `"Inf"` / `"-Inf"`.
#'
#' @seealso [fmt_round()] for plain decimal places, [fmt_numeric()] to apply
#'   either across a data frame, and [set_decimal_split()] to line the printed
#'   decimal points up.
#'
#' @examples
#' fmt_signif(c(0, 10.2, 103.4, 20.333333, 23.4463), digits = 4)
#' fmt_signif(0.0004567, digits = 4)                      # "0.0004567"
#' fmt_signif(0.0004567, digits = 4, small = "fixed")     # "0.000"
#' fmt_signif(23.445, digits = 4, rounding = "sas")       # "23.45"
#' @export
fmt_signif <- function(x, digits = 3L, rounding = c("r", "sas"),
                       small = c("signif", "fixed"), na = "") {
  .check_num_input(x, "fmt_signif")
  digits   <- .check_digits(digits, "digits", "fmt_signif")
  rounding <- match.arg(rounding)
  small    <- match.arg(small)
  rnd      <- .rounder(rounding)
  dec_fun  <- function(v) .signif_decimals(v, digits, small)
  vapply(x, .fmt_num_one, character(1L),
         dec_fun = dec_fun, rnd = rnd, na = na, recompute = TRUE,
         USE.NAMES = FALSE)
}


#' Format numbers to a fixed number of decimal places
#'
#' @description
#' Rounds to `digits` decimal places and prints them all, trailing zeros
#' included -- `fmt_round(2.5, 2)` is `"2.50"`, not `"2.5"`.
#'
#' @inheritParams fmt_signif
#' @param digits Decimal places. Default `2`.
#'
#' @return A character vector the same length as `x`. `Inf` / `-Inf` pass
#'   through as `"Inf"` / `"-Inf"`.
#'
#' @seealso [fmt_signif()], [fmt_numeric()].
#'
#' @examples
#' fmt_round(c(2.5, 20.333, 100), digits = 2)
#' fmt_round(23.445, digits = 2)                   # "23.44" -- banker's
#' fmt_round(23.445, digits = 2, rounding = "sas") # "23.45"
#' @export
fmt_round <- function(x, digits = 2L, rounding = c("r", "sas"), na = "") {
  .check_num_input(x, "fmt_round")
  digits   <- .check_digits(digits, "digits", "fmt_round")
  rounding <- match.arg(rounding)
  rnd      <- .rounder(rounding)
  vapply(x, .fmt_num_one, character(1L),
         dec_fun = function(v) digits, rnd = rnd, na = na, recompute = FALSE,
         USE.NAMES = FALSE)
}


# Resolve one `formats` entry to a decimals function.
.format_rule_to_dec <- function(rule, key, small) {
  if (!is.list(rule)) {
    stop(sprintf("`fmt_numeric(formats = )` entry '%s' must be a list, e.g. list(signif = 4).",
                 key), call. = FALSE)
  }
  has_s <- !is.null(rule$signif)
  has_d <- !is.null(rule$digits)
  if (has_s == has_d) {
    stop(sprintf("`fmt_numeric(formats = )` entry '%s' must give exactly one of `signif` or `digits`.",
                 key), call. = FALSE)
  }
  sm <- rule$small %||% small
  if (has_s) {
    s <- .check_digits(rule$signif, "signif", "fmt_numeric")
    function(v) .signif_decimals(v, s, sm)
  } else {
    d <- .check_digits(rule$digits, "digits", "fmt_numeric")
    function(v) d
  }
}


#' Format the numeric columns of a table for display
#'
#' @description
#' Applies [fmt_signif()] or [fmt_round()] across a data frame, turning the
#' selected **numeric** columns into the text the report should show. Run it
#' before building the table; character columns are taken as already formatted
#' and are never touched, and neither are columns outside `cols`.
#'
#' @section One column, several formats -- the carrier column:
#' A clinical column holds several statistics, each with its own format, so a
#' per-column setting is not enough. `by` names a **carrier column** whose
#' values key into `formats`. It can be the **row-heading (stub) column
#' itself**: values are matched after trimming, so an indented `"  Mean"` keys
#' on `"Mean"` and no extra column is needed.
#'
#' \preformatted{
#' pk |> fmt_numeric(
#'   cols = visits, by = "Nominal Time (h)",
#'   formats = list(
#'     n        = list(digits = 0),
#'     Mean     = list(signif = 4),
#'     `CV\%`    = list(digits = 1),
#'     .default = list(signif = 4)))
#' }
#'
#' A row whose selected cells are all missing needs no key, so group-label and
#' blank rows fall out on their own. A **non-missing** cell whose key matches
#' nothing and has no `.default` is an error naming the unmatched keys --
#' quietly printing 15 digits into a submission table is the worse outcome.
#'
#' @param data A data frame.
#' @param cols Columns to format: names or positions. A selected column that is
#'   not numeric is left alone.
#' @param by Optional carrier column (name or position) keying into `formats`.
#'   May be the row-heading column. Values are matched after trimming.
#' @param formats Named list of rules, used with `by`. Each entry gives exactly
#'   one of `signif` or `digits`, and may add `small`. The reserved name
#'   `.default` covers unmatched keys.
#' @param signif,digits Used **without** `by` to format every selected cell the
#'   same way. Give exactly one of them.
#' @inheritParams fmt_signif
#'
#' @return `data` with the selected numeric columns replaced by character
#'   columns. Row count, column order and every other column are unchanged.
#'
#' @seealso [fmt_signif()], [fmt_round()]; [set_decimal_split()] to line the
#'   printed decimal points up afterwards.
#'
#' @examples
#' df <- data.frame(
#'   stat = c("n", "Mean", "SD"),
#'   trt  = c(24, 902.3312, 230.1234)
#' )
#' # one rule for the whole column
#' fmt_numeric(df, cols = "trt", signif = 4)
#'
#' # per-statistic, keyed on the row-heading column
#' fmt_numeric(df, cols = "trt", by = "stat",
#'             formats = list(n    = list(digits = 0),
#'                            Mean = list(signif = 4),
#'                            SD   = list(signif = 4)))
#' @export
fmt_numeric <- function(data, cols, by = NULL, formats = NULL,
                        signif = NULL, digits = NULL,
                        rounding = c("r", "sas"),
                        small = c("signif", "fixed"), na = "") {
  if (!is.data.frame(data)) {
    stop("`fmt_numeric()` needs a data frame.", call. = FALSE)
  }
  rounding <- match.arg(rounding)
  small    <- match.arg(small)
  rnd      <- .rounder(rounding)

  col_idx <- .resolve_col_indices(cols, data, "fmt_numeric(cols)")
  num_idx <- col_idx[vapply(data[col_idx], is.numeric, logical(1L))]
  if (length(num_idx) == 0L) return(data)

  use_by <- !is.null(by)
  if (use_by) {
    if (!is.null(signif) || !is.null(digits)) {
      stop("Give either `by` + `formats`, or `signif` / `digits` -- not both.",
           call. = FALSE)
    }
    if (!is.list(formats) || length(formats) == 0L) {
      stop("`fmt_numeric(by = )` needs a named `formats` list.", call. = FALSE)
    }
    by_idx <- .resolve_col_indices(by, data, "fmt_numeric(by)")
    if (length(by_idx) != 1L) {
      stop("`fmt_numeric(by = )` must name a single column.", call. = FALSE)
    }
    keys <- trimws(as.character(data[[by_idx]]))
    keys[is.na(keys)] <- ""
    dec_by_rule <- lapply(names(formats), function(k)
      .format_rule_to_dec(formats[[k]], k, small))
    names(dec_by_rule) <- names(formats)

    # every non-missing selected cell must have a rule
    if (is.null(dec_by_rule[[".default"]])) {
      needed <- Reduce(`|`, lapply(data[num_idx], function(v) !is.na(v)))
      missing_keys <- setdiff(unique(keys[needed]), names(dec_by_rule))
      if (length(missing_keys)) {
        stop(sprintf(
          "`fmt_numeric()`: no format for %s. Add %s to `formats`, or a `.default` entry.",
          paste0("'", missing_keys, "'", collapse = ", "),
          if (length(missing_keys) == 1L) "it" else "them"), call. = FALSE)
      }
    }
    for (j in num_idx) {
      v <- data[[j]]
      data[[j]] <- vapply(seq_along(v), function(i) {
        f <- dec_by_rule[[keys[i]]] %||% dec_by_rule[[".default"]]
        # an all-missing row needs no rule; `na` covers it
        if (is.null(f)) return(if (is.na(v[i])) na else NA_character_)
        .fmt_num_one(v[i], f, rnd, na, recompute = TRUE)
      }, character(1L), USE.NAMES = FALSE)
    }
    return(data)
  }

  if (is.null(signif) == is.null(digits)) {
    stop("Give exactly one of `signif` or `digits` (or use `by` + `formats`).",
         call. = FALSE)
  }
  if (!is.null(signif)) {
    s <- .check_digits(signif, "signif", "fmt_numeric")
    dec_fun <- function(v) .signif_decimals(v, s, small)
    recompute <- TRUE
  } else {
    d <- .check_digits(digits, "digits", "fmt_numeric")
    dec_fun <- function(v) d
    recompute <- FALSE
  }
  for (j in num_idx) {
    data[[j]] <- vapply(data[[j]], .fmt_num_one, character(1L),
                        dec_fun = dec_fun, rnd = rnd, na = na,
                        recompute = recompute, USE.NAMES = FALSE)
  }
  data
}
