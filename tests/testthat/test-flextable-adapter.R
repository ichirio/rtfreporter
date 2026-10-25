## tests/testthat/test-flextable-adapter.R
##
## flextable -> rtftable via as_rtftables() / as_rtftable().
## All tests skip when flextable is not installed.

library(testthat)

# A representative clinical-style flextable: a spanning header, a relabelled
# stub column, colformat-formatted numeric columns, a caption and footer lines.
.make_flextable <- function() {
  skip_if_not_installed("flextable")
  df <- data.frame(
    Param   = c("Mean", "SD"),
    Placebo = c(75.13, 8.234),
    Active  = c(74.29, 7.912),
    stringsAsFactors = FALSE
  )
  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(
    ft, Param = "Characteristic", Placebo = "Placebo", Active = "Active")
  ft <- flextable::add_header_row(
    ft, values = c("", "Treatment group"), colwidths = c(1, 2))
  ft <- flextable::colformat_double(ft, j = c("Placebo", "Active"), digits = 1)
  ft <- flextable::align(ft, j = c("Placebo", "Active"),
                         align = "right", part = "body")
  ft <- flextable::align(ft, j = "Param", align = "left", part = "all")
  ft <- flextable::set_caption(ft, "Table 14.1: Demographics")
  ft <- flextable::add_footer_lines(
    ft, c("Source: ADSL.", "Note: values are mean/SD."))
  ft
}


# -- detection ---------------------------------------------------------------

test_that(".is_flextable_tbl recognises a flextable and rejects others", {
  ft <- .make_flextable()
  expect_true(rtfreporter:::.is_flextable_tbl(ft))
  expect_false(rtfreporter:::.is_flextable_tbl(data.frame(a = 1)))
  expect_false(rtfreporter:::.is_flextable_tbl(list()))
})


# -- as_rtftables() on a flextable -------------------------------------------

test_that("as_rtftables(flextable) returns a length-1 list of rtftable", {
  ft  <- .make_flextable()
  res <- as_rtftables(ft)
  expect_type(res, "list")
  expect_length(res, 1L)                  # not mis-recursed as a list of parts
  expect_s3_class(res[[1L]], "rtftable")
})

test_that("as_rtftables(flextable) reads the DISPLAYED body text", {
  rt <- as_rtftables(.make_flextable())[[1L]]
  # Stub column carries the row labels.
  expect_identical(rt$data[[1L]], c("Mean", "SD"))
  # colformat_double() formatting is reflected (75.1, not the raw 75.13).
  expect_identical(rt$data[[2L]], c("75.1", "8.2"))
  expect_identical(rt$data[[3L]], c("74.3", "7.9"))
})

test_that("as_rtftables(flextable) reads relabelled leaf headers + spanning", {
  rt <- as_rtftables(.make_flextable())[[1L]]
  # 2 header levels -> multi-row col_header (spanner row + leaf-label row).
  expect_true(is.list(rt$col_header))
  expect_gte(length(rt$col_header), 2L)
  # Bottom row = relabelled leaf labels (set_header_labels applied).
  leaf <- rt$col_header[[length(rt$col_header)]]
  expect_identical(as.character(leaf), c("Characteristic", "Placebo", "Active"))
  # Top row is a spanning row (list of col_cell objects) carrying the spanner.
  spanner <- rt$col_header[[1L]]
  expect_true(is.list(spanner))
  labs <- vapply(spanner, function(c) as.character(c$label %||% ""), character(1))
  expect_true("Treatment group" %in% labs)
})

test_that("as_rtftables(flextable) reads per-column alignment", {
  rt <- as_rtftables(.make_flextable())[[1L]]
  aligns <- vapply(rt$col_spec, function(s) s$align, character(1))
  expect_identical(aligns[[1L]], "left")    # stub
  expect_identical(aligns[[2L]], "right")   # numeric column
  expect_identical(aligns[[3L]], "right")
})

test_that("as_rtftables(flextable) carries caption + footer as attributes", {
  rt <- as_rtftables(.make_flextable())[[1L]]
  expect_identical(attr(rt, "rtf_titles"), "Table 14.1: Demographics")
  fn <- attr(rt, "rtf_footnotes")
  expect_true("Source: ADSL." %in% fn)
  expect_true("Note: values are mean/SD." %in% fn)
})

test_that("as_rtftables(flextable, read_meta = FALSE) gives body only", {
  rt <- as_rtftables(.make_flextable(), read_meta = FALSE)[[1L]]
  # No source metadata is read (rtftable() still supplies a default col_spec).
  expect_null(rt$col_header)
  expect_null(attr(rt, "rtf_titles"))
  expect_null(attr(rt, "rtf_footnotes"))
  # Body text is still the displayed text.
  expect_identical(rt$data[[2L]], c("75.1", "8.2"))
})

test_that("as_rtftables(flextable) honours a token subset", {
  rt   <- as_rtftables(.make_flextable(), read_meta = c("col_header", "titles"))[[1L]]
  full <- as_rtftables(.make_flextable())[[1L]]
  expect_false(is.null(rt$col_header))
  expect_identical(attr(rt, "rtf_titles"), "Table 14.1: Demographics")
  expect_null(attr(rt, "rtf_footnotes"))    # footnotes token off
  # With the alignment token off, the table's right-align is NOT read: the
  # numeric column keeps the default alignment rather than "right".
  pull_align <- function(rt) vapply(
    rt$col_spec, function(s) as.character(s$align %||% NA), character(1))
  expect_identical(pull_align(full)[[2L]], "right")
  expect_false(identical(pull_align(rt)[[2L]], "right"))
})

test_that("as_rtftables(flextable) paginates the body", {
  skip_if_not_installed("flextable")
  df <- data.frame(Param = c("A", "B", "C", "D"), Val = c("1", "2", "3", "4"),
                   stringsAsFactors = FALSE)
  ft <- flextable::set_caption(flextable::flextable(df), "Paginated")
  pages <- as_rtftables(ft, split = "rows", split_rows = 2L)
  expect_length(pages, 2L)
  # Shared header metadata is replicated onto every page.
  expect_false(is.null(pages[[1L]]$col_header))
  expect_false(is.null(pages[[2L]]$col_header))
})

test_that("as_rtftables(flextable) end-to-end render succeeds", {
  ft  <- .make_flextable()
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(as_rtftables(ft))
  expect_identical(doc$contents[[1L]]$title, "Table 14.1: Demographics")
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  generate_rtfreport(doc, out, overwrite = TRUE)
  expect_true(file.exists(out))
})

test_that("as_rtftable() accepts a flextable (single page)", {
  rt <- as_rtftable(.make_flextable())
  expect_s3_class(rt, "rtftable")
  expect_identical(rt$data[[1L]], c("Mean", "SD"))
})


# -- token validation --------------------------------------------------------

test_that(".resolve_flextable_tokens validates tokens", {
  expect_identical(rtfreporter:::.resolve_flextable_tokens(FALSE), character(0))
  expect_setequal(rtfreporter:::.resolve_flextable_tokens(TRUE),
                  rtfreporter:::.FLEXTABLE_TOKENS_ALL)
  expect_error(rtfreporter:::.resolve_flextable_tokens(c("col_header", "junk")),
               "Unknown")
})
