## tests/testthat/test-huxtable-adapter.R
##
## huxtable -> rtftable via as_rtftables() / as_rtftable().
## All tests skip when huxtable is not installed.

library(testthat)

# A representative huxtable: two header rows (a spanner + leaf labels), a
# number-formatted numeric column, per-column alignment and a caption.
.make_huxtable <- function() {
  skip_if_not_installed("huxtable")
  hx <- huxtable::huxtable(
    Characteristic = c("Characteristic", "Age (years)", "Sex, n (%)",
                       "  Female", "  Male"),
    Placebo = c("Placebo (N=30)", 75.134, NA, "16 (53)", "14 (47)"),
    Active  = c("Active (N=30)",  74.29,  NA, "18 (60)", "12 (40)"),
    add_colnames = FALSE
  )
  hx <- huxtable::insert_row(hx, "", "Treatment Group", "", after = 0)
  huxtable::colspan(hx)[1, 2] <- 2
  huxtable::header_rows(hx)[1:2] <- TRUE
  huxtable::number_format(hx)[3, 2:3] <- "%.1f"     # Age row -> 1 decimal
  huxtable::align(hx)[, 1]   <- "left"
  huxtable::align(hx)[, 2:3] <- "center"
  huxtable::caption(hx) <- "Table 1: Demographics"
  hx
}


# -- detection ---------------------------------------------------------------

test_that(".is_huxtable_tbl recognises a huxtable and rejects others", {
  hx <- .make_huxtable()
  expect_true(rtfreporter:::.is_huxtable_tbl(hx))
  expect_false(rtfreporter:::.is_huxtable_tbl(data.frame(a = 1)))
  expect_false(rtfreporter:::.is_huxtable_tbl(list()))
})


# -- as_rtftables() on a huxtable --------------------------------------------

test_that("as_rtftables(huxtable) returns a length-1 list of rtftable", {
  res <- as_rtftables(.make_huxtable())
  expect_type(res, "list")
  expect_length(res, 1L)
  expect_s3_class(res[[1L]], "rtftable")
})

test_that("a huxtable is read as a huxtable, not as a plain data.frame", {
  # The dispatch must test huxtable BEFORE the data.frame branch: the metadata
  # (header labels, caption) is extracted rather than the raw frame being used.
  rt <- as_rtftables(.make_huxtable())[[1L]]
  expect_false(is.null(rt$col_header))                 # header rows -> col_header
  expect_identical(attr(rt, "rtf_titles"), "Table 1: Demographics")
  # The two header rows are NOT part of the body (6 rows - 2 header = 4).
  expect_identical(nrow(rt$data), 4L)
})

test_that("as_rtftables(huxtable) reads the DISPLAYED body text", {
  rt <- as_rtftables(.make_huxtable())[[1L]]
  # Header rows excluded; row-label indentation preserved.
  expect_identical(rt$data[[1L]],
                   c("Age (years)", "Sex, n (%)", "  Female", "  Male"))
  # number_format is applied (75.134 -> 75.1), NA -> "".
  expect_identical(rt$data[[2L]], c("75.1", "", "16 (53)", "14 (47)"))
})

test_that("as_rtftables(huxtable) reads leaf headers + spanning", {
  rt <- as_rtftables(.make_huxtable())[[1L]]
  expect_true(is.list(rt$col_header))
  expect_gte(length(rt$col_header), 2L)
  leaf <- rt$col_header[[length(rt$col_header)]]
  expect_identical(as.character(leaf),
                   c("Characteristic", "Placebo (N=30)", "Active (N=30)"))
  spanner <- rt$col_header[[1L]]
  expect_true(is.list(spanner))
  labs <- vapply(spanner, function(c) as.character(c$label %||% ""), character(1))
  expect_true("Treatment Group" %in% labs)
})

test_that("as_rtftables(huxtable) reads per-column alignment", {
  rt <- as_rtftables(.make_huxtable())[[1L]]
  aligns <- vapply(rt$col_spec, function(s) s$align, character(1))
  expect_identical(aligns, c("left", "center", "center"))
})

test_that("as_rtftables(huxtable) carries the caption as the title", {
  rt <- as_rtftables(.make_huxtable())[[1L]]
  expect_identical(attr(rt, "rtf_titles"), "Table 1: Demographics")
})

test_that("as_rtftables(huxtable, read_meta = FALSE) gives body only", {
  rt <- as_rtftables(.make_huxtable(), read_meta = FALSE)[[1L]]
  expect_null(rt$col_header)
  expect_null(attr(rt, "rtf_titles"))
  expect_identical(rt$data[[2L]], c("75.1", "", "16 (53)", "14 (47)"))
})

test_that("as_rtftables(huxtable) honours a token subset", {
  skip_if_not_installed("huxtable")
  # A right-aligned column distinguishes "alignment read" from rtftable's
  # default (non-row-title columns default to centre).
  hx <- huxtable::huxtable(A = c("H", "x"), B = c("H2", "1"),
                           add_colnames = FALSE)
  huxtable::header_rows(hx)[1] <- TRUE
  huxtable::align(hx)[, 2] <- "right"
  huxtable::caption(hx) <- "Cap"
  on  <- as_rtftables(hx)[[1L]]
  off <- as_rtftables(hx, read_meta = c("col_header", "titles"))[[1L]]
  expect_false(is.null(off$col_header))
  expect_identical(attr(off, "rtf_titles"), "Cap")
  pull_align <- function(rt) vapply(rt$col_spec, function(s) s$align, character(1))
  expect_identical(pull_align(on)[[2L]], "right")     # alignment token on
  expect_false(identical(pull_align(off)[[2L]], "right"))  # off -> default
})

test_that("as_rtftables(huxtable) paginates the body", {
  pages <- as_rtftables(.make_huxtable(), split = "rows", split_rows = 2L)
  expect_length(pages, 2L)
  expect_false(is.null(pages[[1L]]$col_header))
  expect_false(is.null(pages[[2L]]$col_header))
})

test_that("as_rtftables(huxtable) end-to-end render succeeds", {
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(as_rtftables(.make_huxtable()))
  expect_identical(doc$contents[[1L]]$title, "Table 1: Demographics")
  out <- tempfile(fileext = ".rtf"); on.exit(unlink(out), add = TRUE)
  generate_rtfreport(doc, out, overwrite = TRUE)
  expect_true(file.exists(out))
})

test_that("as_rtftable() accepts a huxtable (single page)", {
  rt <- as_rtftable(.make_huxtable())
  expect_s3_class(rt, "rtftable")
  expect_identical(rt$data[[1L]][[1L]], "Age (years)")
})


# -- token validation --------------------------------------------------------

test_that(".resolve_huxtable_tokens validates tokens", {
  expect_identical(rtfreporter:::.resolve_huxtable_tokens(FALSE), character(0))
  expect_setequal(rtfreporter:::.resolve_huxtable_tokens(TRUE),
                  rtfreporter:::.HUXTABLE_TOKENS_ALL)
  expect_error(rtfreporter:::.resolve_huxtable_tokens(c("col_header", "junk")),
               "Unknown")
})
