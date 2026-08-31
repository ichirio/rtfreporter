## tests/testthat/test-rlistings-adapter.R
##
## rlistings (listing_df) -> rtftable via as_rtftables().
## All tests skip when rlistings / formatters are not installed.

library(testthat)

.make_listing <- function(disp_cols = NULL, titles = TRUE) {
  skip_if_not_installed("rlistings")
  skip_if_not_installed("formatters")
  dat <- data.frame(
    USUBJID = c(rep("AB12345-001", 3), rep("AB12345-002", 2)),
    VISIT   = c("SCREENING", "CYCLE 1", "CYCLE 2", "SCREENING", "CYCLE 1"),
    SECRET  = rep("must not print", 5L),
    AGE     = c(rep(64L, 3), rep(58L, 2)),
    AETERM  = c("Nausea", "Fatigue", "Headache", "Vomiting", "Neutropenia"),
    stringsAsFactors = FALSE
  )
  args <- list(dat, key_cols = c("USUBJID", "VISIT"))
  if (!is.null(disp_cols)) args$disp_cols <- disp_cols
  if (titles) {
    args$main_title  <- "Listing of Adverse Events"
    args$subtitles   <- "Safety Analysis Set"
    args$main_footer <- "Source: ADAE"
  }
  do.call(rlistings::as_listing, args)
}


# -- detection ----------------------------------------------------------------

test_that(".is_rlistings_tbl recognises listing_df and rejects others", {
  lst <- .make_listing()
  expect_true(rtfreporter:::.is_rlistings_tbl(lst))
  expect_false(rtfreporter:::.is_rlistings_tbl(data.frame(a = 1)))
  expect_false(rtfreporter:::.is_rlistings_tbl(list()))
  # It is NOT an rtables table -- that is the whole reason this adapter exists.
  expect_false(rtfreporter:::.is_rtables_tbl(lst))
  # ... but it *is* a data.frame, which is why dispatch order matters.
  expect_true(is.data.frame(lst))
})

test_that(".is_rlistings_tbl needs no rlistings installed", {
  fake <- structure(data.frame(a = 1), class = c("listing_df", "data.frame"))
  expect_true(rtfreporter:::.is_rlistings_tbl(fake))
})


# -- the display-column leak (#322) -------------------------------------------

test_that("a column excluded by disp_cols is not printed", {
  lst <- .make_listing(disp_cols = c("AGE", "AETERM"))
  expect_false("SECRET" %in% rlistings::listing_dispcols(lst))
  expect_true("SECRET" %in% names(lst))          # still in the tibble

  tbl <- as_rtftable(lst)
  cells <- unlist(lapply(tbl$data, as.character), use.names = FALSE)
  expect_false(any(cells == "must not print"))
  expect_identical(ncol(tbl$data),
                   length(rlistings::listing_dispcols(lst)))
})


# -- metadata that the data.frame fallback used to drop ------------------------

test_that("key-column repeat suppression survives", {
  lst <- .make_listing(disp_cols = c("AGE", "AETERM"))
  tbl <- as_rtftable(lst)
  subj <- as.character(tbl$data[[1L]])
  expect_identical(subj[[1L]], "AB12345-001")
  # rlistings blanks the repeats; the fallback used to reprint them.
  expect_true(any(!nzchar(subj)))
  expect_false(all(subj == "AB12345-001"))
})

test_that("titles and footers come across", {
  lst <- .make_listing(disp_cols = c("AGE", "AETERM"))
  kw  <- rtfreporter:::.rlistings_to_rtftable_kwargs(lst)
  expect_identical(kw$titles_block,
                   c("Listing of Adverse Events", "Safety Analysis Set"))
  expect_identical(kw$footnotes_block, "Source: ADAE")
})

test_that("column headers and alignment come across", {
  lst <- .make_listing(disp_cols = c("AGE", "AETERM"))
  kw  <- rtfreporter:::.rlistings_to_rtftable_kwargs(lst)
  expect_true(all(c("USUBJID", "VISIT", "AGE", "AETERM") %in%
                    unlist(kw$col_header)))
  expect_length(kw$col_spec, ncol(kw$data))
  aligns <- vapply(kw$col_spec, function(s) s$align, character(1L))
  expect_true(all(aligns %in% c("left", "center", "right")))
})


# -- tokens -------------------------------------------------------------------

test_that("read_meta = FALSE keeps the body but drops the metadata", {
  lst <- .make_listing(disp_cols = c("AGE", "AETERM"))
  kw  <- rtfreporter:::.rlistings_to_rtftable_kwargs(lst, tokens = character(0))
  expect_s3_class(kw$data, "data.frame")
  expect_null(kw$col_header)
  expect_null(kw$titles_block)
  expect_null(kw$footnotes_block)
})

test_that("individual read_meta tokens are honoured", {
  lst <- .make_listing(disp_cols = c("AGE", "AETERM"))
  kw  <- rtfreporter:::.rlistings_to_rtftable_kwargs(lst, tokens = "titles")
  expect_null(kw$col_header)
  expect_identical(kw$titles_block,
                   c("Listing of Adverse Events", "Safety Analysis Set"))
})

test_that("the rlistings and rtables token sets stay identical", {
  # The two adapters share one reader, so they must accept the same tokens.
  # The rlistings copy is spelled out (load order), hence this guard.
  expect_identical(rtfreporter:::.RLISTINGS_TOKENS_ALL,
                   rtfreporter:::.RTABLES_TOKENS_ALL)
})

test_that("an unknown token is rejected, naming rlistings", {
  expect_error(rtfreporter:::.resolve_rlistings_tokens("nonsense"),
               "rlistings")
})

test_that("the typed wrapper rejects a non-listing", {
  expect_error(rtfreporter:::.rlistings_to_rtftable_kwargs(data.frame(a = 1)),
               "listing_df")
})


# -- end to end ---------------------------------------------------------------

test_that("as_rtftables() paginates a listing and renders RTF", {
  lst   <- .make_listing(disp_cols = c("AGE", "AETERM"))
  pages <- as_rtftables(lst, split = "group_force", max_rows = 3)
  expect_gt(length(pages), 1L)
  for (p in pages) expect_s3_class(p, "rtftable")

  f <- withr::local_tempfile(fileext = ".rtf")
  generate_rtfreport(rtf_tables(rtf_document(), pages), f, overwrite = TRUE)
  expect_true(file.exists(f))
  expect_gt(file.size(f), 0)

  rtf <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl("must not print", rtf, fixed = TRUE))
  expect_true(grepl("Listing of Adverse Events", rtf, fixed = TRUE))
})

test_that("as_rtftable() accepts a listing directly", {
  lst <- .make_listing(disp_cols = c("AGE", "AETERM"))
  expect_s3_class(as_rtftable(lst), "rtftable")
})

test_that("a listing inside a list input is handled", {
  lst <- .make_listing(disp_cols = c("AGE", "AETERM"))
  pages <- as_rtftables(list(lst, lst))
  expect_length(pages, 2L)
  for (p in pages) expect_s3_class(p, "rtftable")
})
