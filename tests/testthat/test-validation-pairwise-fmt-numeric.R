# Validation tests: fmt_numeric() -- how its mode (one rule for every cell, or
# a carrier column and a formats list) acts with cols, small, rounding and na:
# a pairwise set, and a decision table of the calls it refuses.  Helpers
# prefixed .fn_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rule, as the help says it (fmt_numeric(), fmt_signif(), fmt_round()):
#  fmt_numeric() "applies fmt_signif() or fmt_round() across a data frame" to
#  the numeric columns of `cols`; a selected column that is not numeric is
#  left alone, as is every column outside `cols`; a missing cell is `na`.
#  So the expected text of a cell is what fmt_signif() / fmt_round() give for
#  that value with the rule the mode chooses, the call's `small`, `rounding`
#  and `na` (a rule in `formats` may carry its own `small`, which beats the
#  call's):
#    signif / digits   one rule for every selected cell;
#    by + formats      the rule of the cell's key: the carrier column's text
#                      trimmed ("  Mean" keys on "Mean"), else `.default`;
#                      a row whose selected cells are all missing needs no
#                      key (FR-FM159).
#  The literal anchors, from the help's own table: 0.0004567 at 4 digits is
#  "0.0004567" with small = "signif" and "0.000" with "fixed"; 23.445 at 2
#  decimals is "23.44" under rounding "r" and "23.45" under "sas".
#
#  Data (.fn_df): carrier `stat` (n, Mean, SD, Hdr, "  Mean"), numeric A and B,
#  character C.  Row Hdr has A and B missing; row "  Mean" has B missing; B of
#  Mean is 0.0004567 (so small matters) and B of SD is 23.445 (an exact half
#  under 2 decimals, so rounding matters).
#
#  (A) FT-FM108, pairwise.  Factors and levels:
#    mode      signif 3 | digits 1 | by with formats naming every key |
#              by with only .default
#    cols      "A" | c("A", "B") | c(2, 3) by position | c("A", "C") (a
#              character column among them)
#    small     signif | fixed (the call's)
#    rounding  NULL | "r" | "sas"
#    na        "" (default) | "-"
#    rule      for the by modes: no rule carries small | the SD rule carries
#              small = "fixed" (it beats the call's; for the other modes it
#              has no meaning and takes the same two levels as a repeat, so it
#              is held at "none" there)
#  The product is 192 rows with no rule, and 96 with one for the two by modes:
#  288; .allpairs() picks the rows covering every pair of levels.  The oracle is the rule above.
#
#  (B) FT-FM109, decision table, a call that is refused or returned whole.
#    by | signif | digits | formats          | result
#    -  | -      | -      | -                | error "Give exactly one of"
#    -  | 3      | 1      | -                | error "Give exactly one of"
#    stat | 3    | -      | list(...)        | error "not both"
#    stat | -    | 1      | list(...)        | error "not both"
#    stat | -    | -      | NULL             | error "needs a named `formats` list"
#    stat | -    | -      | list()           | error, same
#    stat | -    | -      | list(n = ...)    | error "no format for 'Mean', 'SD'"
#                                              (a key a non-missing cell has,
#                                              none of them .default)
#    stat | -    | -      | + .default       | ok
#    stat | -    | -      | rule with both   | error "exactly one of `signif` or `digits`"
#    stat | -    | -      | rule with none   | error, same
#    stat | -    | -      | rule not a list  | error "must be a list"
#    -  | -      | -      | cols = "C" only  | the data, unchanged (FR-FM152: no
#                                              numeric column, nothing to do)
#  The unmatched key on a row whose selected cells are all missing is no error
#  (the Hdr row; FR-FM159).
# ---------------------------------------------------------------------------

.fn_df <- function() {
  data.frame(stat = c("n", "Mean", "SD", "Hdr", "  Mean"),
             A = c(24, 902.3312, 230.1234, NA, 12.5),
             B = c(30, 0.0004567, 23.445, NA, NA),
             C = c("a", "b", "c", "d", "e"),
             stringsAsFactors = FALSE)
}

# the formatter a rule stands for
.fn_one <- function(v, rule, small, rounding, na) {
  if (!is.null(rule$small)) small <- rule$small
  if (!is.null(rule$signif)) {
    fmt_signif(v, rule$signif, rounding = rounding, small = small, na = na)
  } else {
    fmt_round(v, rule$digits, rounding = rounding, na = na)
  }
}

.fn_formats <- function(kind, ruled) {
  if (kind == "by_all") {
    list(n = list(digits = 0), Mean = list(signif = 4), SD = list(signif = 3),
         Hdr = list(digits = 1))
  } else {
    list(.default = list(digits = 2))
  }
}

test_that("FT-FM108 [FR-FM148 FR-FM149 FR-FM151 FR-FM152 FR-FM153 FR-FM154 FR-FM155 FR-FM156 FR-FM157 FR-FM158 FR-FM159 FR-FM160 FR-FM161 FR-FM162 FR-IX404]: fmt_numeric(): mode x cols x small x rounding x na x a rule's own small, pairwise", {
  mode <- c("signif", "digits", "by_all", "by_default")
  cand <- expand.grid(mode = mode, cols = c("A", "AB", "pos", "AC"),
                      small = c("signif", "fixed"), rounding = c("NULL", "r", "sas"),
                      na = c("empty", "dash"), rule = c("none", "ruled"),
                      stringsAsFactors = FALSE)
  cand <- cand[cand$rule == "none" | cand$mode %in% c("by_all", "by_default"), ]
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 40L)
  colsets <- list(A = "A", AB = c("A", "B"), pos = c(2, 3), AC = c("A", "C"))
  d <- .fn_df()
  for (i in seq_len(nrow(ap$rows))) {
    r <- ap$rows[i, ]
    na <- if (r$na == "dash") "-" else ""
    rounding <- if (r$rounding == "NULL") NULL else r$rounding
    cols <- colsets[[r$cols]]
    rules <- NULL
    args <- list(d, cols = cols, small = r$small, rounding = rounding, na = na)
    if (r$mode == "signif") {
      args$signif <- 3
      rule_of <- function(key) list(signif = 3)
    } else if (r$mode == "digits") {
      args$digits <- 1
      rule_of <- function(key) list(digits = 1)
    } else {
      f <- .fn_formats(r$mode, r$rule)
      if (r$rule == "ruled") f$SD <- list(signif = 3, small = "fixed")
      args$by <- "stat"
      args$formats <- f
      rule_of <- function(key) f[[key]] %||% f[[".default"]]
    }
    out <- do.call(fmt_numeric, args)
    label <- paste(r, collapse = "/")
    expect_identical(names(out), names(d), info = label)
    expect_identical(out$stat, d$stat, info = label)
    expect_identical(out$C, d$C, info = label)
    chosen <- if (is.numeric(cols)) names(d)[cols] else cols
    numcols <- chosen[vapply(d[chosen], is.numeric, NA)]
    for (nm in setdiff(c("A", "B"), numcols)) {
      expect_identical(out[[nm]], d[[nm]], info = paste(label, nm))
    }
    for (nm in numcols) {
      want <- vapply(seq_len(nrow(d)), function(k) {
        v <- d[[nm]][k]
        if (is.na(v)) return(na)
        .fn_one(v, rule_of(trimws(d$stat[k])), r$small, rounding, na)
      }, "")
      expect_identical(out[[nm]], want, info = paste(label, nm))
    }
  }
})

test_that("FT-FM108 anchors: the help's own values for small and rounding, per mode", {
  v <- data.frame(k = c("a", "b"), x = c(0.0004567, 23.445))
  expect_identical(fmt_numeric(v, "x", signif = 4)$x[1L], "0.0004567")
  expect_identical(fmt_numeric(v, "x", signif = 4, small = "fixed")$x[1L], "0.000")
  expect_identical(fmt_numeric(v, "x", by = "k",
                               formats = list(a = list(signif = 4, small = "fixed"),
                                              b = list(signif = 4)))$x[1L], "0.000")
  expect_identical(fmt_numeric(v, "x", digits = 2)$x[2L], "23.44")
  expect_identical(fmt_numeric(v, "x", digits = 2, rounding = "sas")$x[2L], "23.45")
})

test_that("FT-FM109 [FR-FM152 FR-FM159 FR-FM165 FR-FM166 FR-FM167 FR-FM169 FR-FM170 FR-FM171 FR-IX404]: fmt_numeric(): the calls it refuses, accepts or returns whole, by by x signif x digits x formats", {
  d <- .fn_df()
  full <- list(n = list(digits = 0), Mean = list(signif = 4), SD = list(signif = 3))
  err <- function(..., msg) expect_error(fmt_numeric(d, ...), msg, fixed = TRUE)
  err("A", msg = "Give exactly one of `signif` or `digits`")
  err("A", signif = 3, digits = 1, msg = "Give exactly one of `signif` or `digits`")
  err("A", by = "stat", signif = 3, formats = full,
      msg = "Give either `by` + `formats`, or `signif` / `digits` -- not both.")
  err("A", by = "stat", digits = 1, formats = full,
      msg = "Give either `by` + `formats`, or `signif` / `digits` -- not both.")
  err("A", by = "stat", msg = "`fmt_numeric(by = )` needs a named `formats` list.")
  err("A", by = "stat", formats = list(),
      msg = "`fmt_numeric(by = )` needs a named `formats` list.")
  # the key "Mean" and "SD" have non-missing cells and no rule
  err(c("A", "B"), by = "stat", formats = full["n"],
      msg = "no format for 'Mean', 'SD'")
  err("A", by = "stat", formats = c(full, list(Hdr = list(digits = 1, signif = 2))),
      msg = "must give exactly one of `signif` or `digits`")
  err("A", by = "stat", formats = c(full, list(Hdr = list(small = "fixed"))),
      msg = "must give exactly one of `signif` or `digits`")
  err("A", by = "stat", formats = c(full, list(Hdr = 1)), msg = "must be a list")
  # accepted: the unmatched key is on a row with nothing to format, or .default covers it
  ok <- fmt_numeric(d, c("A", "B"), by = "stat", formats = full)
  expect_identical(ok$A[4L], "")
  ok <- fmt_numeric(d, "A", by = "stat",
                    formats = c(full["n"], list(.default = list(digits = 3))))
  expect_identical(ok$A[2L], "902.331")
  # nothing numeric selected: the data comes back whole, whatever the mode arguments
  expect_identical(fmt_numeric(d, "C"), d)
  expect_identical(fmt_numeric(d, "C", by = "stat", formats = list()), d)
})
