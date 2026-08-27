# Argument validation for verbs whose `...` is a pure catch-all (#302).
#
# Reported in #301: `set_decimal_split(colS = 3:31)` silently cleared the
# setting instead of applying it, because `colS` fell into `...` and the call
# then ran with `cols = NULL`.

mk <- function() {
  df <- data.frame(P = c("a", "b"), Q = c("x", "y"),
                   V1 = c("12.5", "3.45"), V2 = c("100.25", "7.5"),
                   stringsAsFactors = FALSE)
  rtftable(df, border = "tfl", row_title = 1:2)
}

# ── the helper itself ──────────────────────────────────────────────────────

test_that(".check_dots returns the recognised arguments untouched", {
  expect_identical(
    .check_dots(list(cols = 1:2, ratio = 0.4), c("cols", "ratio"), "v"),
    list(cols = 1:2, ratio = 0.4)
  )
  expect_identical(.check_dots(list(), c("cols"), "v"), list())
})

test_that(".check_dots warns about unknown names and drops them", {
  expect_warning(
    out <- .check_dots(list(cols = 1L, nope = 2L), c("cols", "ratio"), "v"),
    "unknown argument `nope`"
  )
  expect_identical(out, list(cols = 1L))
})

test_that(".check_dots leaves unnamed arguments strictly alone", {
  # a `.list` dispatcher is function(x, ...), so an ordinary positional call
  # arrives unnamed and must still reach the leaf method
  expect_silent(out <- .check_dots(list(1L, cols = 2L), c("cols"), "v"))
  expect_identical(out, list(1L, cols = 2L))
})

test_that(".check_dots keeps unnamed arguments in order when dropping a typo", {
  expect_warning(
    out <- .check_dots(list("a", nope = 1L, "b"), c("cols"), "v"),
    "unknown argument `nope`"
  )
  # the surviving elements keep their order; empty names are positional
  expect_identical(unname(out), list("a", "b"))
})

test_that("a positional call through a page list still works", {
  pages <- paginate_cols(mk(), at = 4, carry = 1:2)
  expect_silent(out <- add_header_row(pages, c("A", "B", "C")))
  expect_length(out, 2L)
})

test_that("the message names the verb and lists the valid arguments", {
  expect_warning(.check_dots(list(zzz = 1L), c("cols", "ratio"), "myverb"),
                 "`myverb\\(\\)`")
  expect_warning(.check_dots(list(zzz = 1L), c("cols", "ratio"), "myverb"),
                 "Valid arguments: cols, ratio")
})

test_that("a case-only difference is suggested -- the #301 case", {
  expect_warning(.check_dots(list(colS = 1L), c("cols"), "v"),
                 "did you mean `cols`")
})

test_that("a near miss is suggested by edit distance", {
  expect_warning(.check_dots(list(colls = 1L), c("cols"), "v"),
                 "did you mean `cols`")
  expect_warning(.check_dots(list(bolt = TRUE), c("bold", "italic"), "v"),
                 "did you mean `bold`")
})

test_that("nothing is suggested when no valid name is close", {
  w <- tryCatch(.check_dots(list(zzzzzzzz = 1L), c("cols"), "v"),
                warning = function(w) conditionMessage(w))
  expect_match(w, "unknown argument `zzzzzzzz` ignored")
  expect_false(grepl("did you mean", w))
})

test_that(".valid_args drops the object and the catch-all", {
  expect_identical(.valid_args(set_decimal_split.rtftable),
                   c("cols", "ratio", "decimal_mark", "include_compound"))
})

# ── set_decimal_split(): the reported bug ──────────────────────────────────

test_that("the #301 typo now warns instead of silently clearing", {
  expect_warning(
    tryCatch(set_decimal_split(mk(), colS = 3:4), error = function(e) NULL),
    "unknown argument `colS` \\(did you mean `cols`\\?\\)"
  )
})

test_that("a missing `cols` is an error, not a silent clear", {
  expect_error(set_decimal_split(mk()), "`cols` is required")
  expect_error(suppressWarnings(set_decimal_split(mk(), colS = 3:4)),
               "`cols` is required")
})

test_that("an explicit cols = NULL still clears, as documented", {
  tbl <- set_decimal_split(mk(), cols = 3:4)
  expect_false(is.null(tbl$decimal_split))
  expect_null(set_decimal_split(tbl, cols = NULL)$decimal_split)
})

test_that("the correct spelling is unaffected", {
  expect_silent(tbl <- set_decimal_split(mk(), cols = 3:4))
  expect_identical(tbl$decimal_split$cols, 3:4)
})

test_that("partial matching still resolves a prefix", {
  expect_silent(tbl <- set_decimal_split(mk(), col = 3:4))
  expect_identical(tbl$decimal_split$cols, 3:4)
})

test_that("a page list warns once per call, not once per page", {
  pages <- paginate_cols(mk(), at = 4, carry = 1:2)
  expect_length(pages, 2L)
  n <- 0L
  withCallingHandlers(
    tryCatch(set_decimal_split(pages, colS = 3:4), error = function(e) NULL),
    warning = function(w) { n <<- n + 1L; invokeRestart("muffleWarning") }
  )
  expect_identical(n, 1L)
})

test_that("a page list with the correct spelling is unaffected", {
  # each page is the 2 carry columns plus 1 data column, so column 3 is the
  # only data column present
  pages <- paginate_cols(mk(), at = 4, carry = 1:2)
  expect_silent(out <- set_decimal_split(pages, cols = 3L))
  expect_identical(out[[1L]]$decimal_split$cols, 3L)
})

# ── the other verbs ────────────────────────────────────────────────────────

test_that("style_cols() warns about an unknown argument", {
  expect_warning(style_cols(mk(), cols = 1L, alignn = "right"),
                 "did you mean `align`")
})

test_that("style_body() warns about an unknown argument", {
  expect_warning(style_body(mk(), rows = 1L, bolt = TRUE),
                 "did you mean `bold`")
})

test_that("style_zone() warns about an unknown argument", {
  expect_warning(style_zone(mk(), headr = list(bold = TRUE)),
                 "did you mean `header`")
})

test_that("style_header() warns about an unknown argument", {
  tbl <- set_col_header(mk(), c("A", "B", "C", "D"))
  expect_warning(style_header(tbl, row = 1L, labl = "x"),
                 "did you mean `label`")
})

test_that("paginate_cols() warns about an unknown argument", {
  expect_warning(paginate_cols(mk(), at = 4L, carrry = 1:2),
                 "did you mean `carry`")
})

test_that("add_header_row() warns about an unknown argument", {
  expect_warning(
    add_header_row(mk(), row = c("A", "B", "C", "D"), positon = "top"),
    "did you mean `.position`"
  )
})

# ── verbs whose `...` carries the payload must be untouched ────────────────

test_that("set_col_header() still takes its rows through `...`", {
  expect_silent(tbl <- set_col_header(mk(), c("A", "B", "C", "D")))
  expect_length(tbl$col_header, 1L)
})

test_that("set_header_cell() still takes its cells through `...`", {
  tbl <- set_col_header(mk(), c("A", "B", "C", "D"))
  expect_silent(set_header_cell(tbl, col_cell(1L, "Z"), row = 1L))
})

test_that("existing pipelines produce no new warnings", {
  expect_silent({
    mk() |>
      set_decimal_split(cols = 3:4) |>
      style_cols(cols = 1L, align = "left") |>
      style_body(rows = 1L, bold = TRUE) |>
      paginate_cols(at = 4L, carry = 1:2)
  })
})
