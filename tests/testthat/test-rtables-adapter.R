## tests/testthat/test-rtables-adapter.R
##
## rtables / tern (VTableTree) -> rtftable via as_rtftables().
## All tests skip when rtables / formatters are not installed.

library(testthat)

.make_rtables <- function(nested = FALSE, footnote = FALSE) {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  lyt <- rtables::basic_table(
    title       = "Table 1",
    subtitles   = "Safety",
    main_footer = "Main footer",
    prov_footer = "Prov footer"
  )
  lyt <- rtables::split_cols_by(lyt, "ARM")
  if (nested) lyt <- rtables::split_cols_by(lyt, "SEX")
  lyt <- rtables::split_rows_by(lyt, "RACE")
  afun <- if (footnote) {
    function(x) rtables::in_rows("Mean" = rtables::rcell(
      mean(x), format = "xx.x", footnotes = list("a note")))
  } else {
    function(x) rtables::in_rows("Mean" = rtables::rcell(mean(x), format = "xx.x"))
  }
  lyt <- rtables::analyze(lyt, "AGE", afun = afun)
  set.seed(1)
  df <- data.frame(
    ARM  = rep(c("A", "B"), each = 12),
    SEX  = rep(c("F", "M"), 12),
    RACE = rep(c("WHITE", "BLACK"), 12),
    AGE  = rnorm(48, 40, 5)
  )
  rtables::build_table(lyt, df)
}


# ── detection ─────────────────────────────────────────────────────────────────

test_that(".is_rtables_tbl recognises VTableTree and rejects others", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  tbl <- .make_rtables()
  expect_true(rtfreporter:::.is_rtables_tbl(tbl))
  expect_false(rtfreporter:::.is_rtables_tbl(data.frame(a = 1)))
  expect_false(rtfreporter:::.is_rtables_tbl(list()))
})


# ── as_rtftables() on an rtables object ────────────────────────────────────────

test_that("as_rtftables(rtables) returns a list of rtftable objects", {
  tbl <- .make_rtables()
  res <- as_rtftables(tbl)
  expect_type(res, "list")
  expect_length(res, 1L)
  expect_s3_class(res[[1L]], "rtftable")
})

test_that("as_rtftables(rtables) reads column labels + row labels (stub)", {
  tbl <- .make_rtables()
  rt  <- as_rtftables(tbl)[[1L]]
  # Column 1 is the stub: group label rows + data rows interleaved already.
  expect_true("WHITE" %in% rt$data[[1L]])
  expect_true("Mean"  %in% trimws(rt$data[[1L]]))   # indented stub label
  # Bottom header row carries the leaf column labels (ARM levels A / B).
  expect_false(is.null(rt$col_header))
})

test_that("as_rtftables(rtables) renders row-label indentation into the stub", {
  tbl <- .make_rtables()                       # RACE group rows + nested data
  rt  <- as_rtftables(tbl)[[1L]]
  stub <- rt$data[[1L]]
  # A nested data row ("Mean") is indented (leading whitespace); the
  # group-label row ("WHITE") is not.
  mean_cell  <- stub[trimws(stub) == "Mean"][1L]
  white_cell <- stub[trimws(stub) == "WHITE"][1L]
  expect_match(mean_cell, "^[[:space:] ]+Mean")
  expect_identical(white_cell, "WHITE")
})

test_that("as_rtftables(rtables) builds spanning header for nested col splits", {
  tbl <- .make_rtables(nested = TRUE)
  rt  <- as_rtftables(tbl)[[1L]]
  # 2 header levels -> multi-row col_header (spanner row + leaf-label row).
  expect_true(is.list(rt$col_header))
  expect_gte(length(rt$col_header), 2L)
  # The top row is a spanning row (list of col_cell objects).
  expect_true(is.list(rt$col_header[[1L]]))
})

test_that("as_rtftables(rtables) carries titles + footers as attributes", {
  tbl <- .make_rtables()
  rt  <- as_rtftables(tbl)[[1L]]
  expect_identical(attr(rt, "rtf_titles"), c("Table 1", "Safety"))
  fn <- attr(rt, "rtf_footnotes")
  expect_true("Main footer" %in% fn)
  expect_true("Prov footer" %in% fn)
})

test_that("as_rtftables(rtables) converts in-cell {N} marks to ^{N}", {
  tbl <- .make_rtables(footnote = TRUE)
  rt  <- as_rtftables(tbl)[[1L]]
  data_cell <- rt$data[[2L]][trimws(rt$data[[1L]]) == "Mean"][1L]
  expect_match(data_cell, "\\^\\{1\\}")
  expect_false(grepl("[^^]\\{1\\}", data_cell))   # no bare {1}
  # The footnote legend text comes through the footnote block.
  expect_true(any(grepl("a note", attr(rt, "rtf_footnotes"))))
})

test_that("as_rtftables(rtables, read_meta = FALSE) gives body only, no metadata", {
  tbl <- .make_rtables(footnote = TRUE)
  rt  <- as_rtftables(tbl, read_meta = FALSE)[[1L]]
  expect_null(rt$col_header)
  expect_null(attr(rt, "rtf_titles"))
  # Marks left literal when footnote_marks token is off.
  data_cell <- rt$data[[2L]][trimws(rt$data[[1L]]) == "Mean"][1L]
  expect_match(data_cell, "\\{1\\}")
})

test_that("as_rtftables(rtables) end-to-end render succeeds", {
  tbl <- .make_rtables(nested = TRUE, footnote = TRUE)
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(as_rtftables(tbl))
  expect_identical(doc$contents[[1L]]$title, c("Table 1", "Safety"))
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  generate_rtfreport(doc, out, overwrite = TRUE)
  expect_true(file.exists(out))
})

test_that("as_rtftable() accepts an rtables object (single page)", {
  tbl <- .make_rtables()
  rt  <- as_rtftable(tbl)
  expect_s3_class(rt, "rtftable")
})

test_that(".resolve_rtables_tokens validates tokens", {
  expect_identical(rtfreporter:::.resolve_rtables_tokens(FALSE), character(0))
  expect_setequal(rtfreporter:::.resolve_rtables_tokens(TRUE),
                  rtfreporter:::.RTABLES_TOKENS_ALL)
  expect_error(rtfreporter:::.resolve_rtables_tokens(c("col_header", "junk")),
               "Unknown")
})
