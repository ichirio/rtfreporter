# Validation scenarios: data to an RTF file, byte for byte as the reference
# (FR-IX132 .. FR-IX134).  The programs are helper-validation-scenarios.R;
# the references are fixtures/validation/<name>.rtf.

.val_reference <- function(nm) readLines(.val_fixture(paste0(nm, ".rtf")), warn = FALSE)

test_that("FT-IX022 [FR-IX132]: a listing from records is written byte for byte as its reference", {
  out <- .val_render(.val_listing_doc())
  expect_identical(out, .val_reference("listing"))
  # what the reference holds: the records paginated, the blocks on each page
  n <- length(suppressMessages(plan_apply(.val_listing_plan(), "pages")))
  expect_gt(n, 1L)
  expect_identical(sum(grepl("Subjects by arm", out, fixed = TRUE)), n)
  expect_identical(sum(grepl("Source: ADSL", out, fixed = TRUE)), n)
  expect_true(any(grepl("S-010", out, fixed = TRUE)))
})

test_that("FT-IX023 [FR-IX133]: a figure from a PNG file is written byte for byte as its reference", {
  out <- .val_render(.val_figure_doc())
  expect_identical(out, .val_reference("figure"))
  expect_true(any(grepl("\\pngblip", out, fixed = TRUE)))
  expect_true(any(grepl("\\picwgoal4320\\pichgoal2880", out, fixed = TRUE)))
  expect_true(any(grepl("Figure 14.2.1", out, fixed = TRUE)))
})

test_that("FT-IX024 [FR-IX134]: a summary and its listing on one page are written byte for byte as their reference", {
  doc <- .val_page_doc()
  expect_length(rtf_layout(doc)$pages, 1L)
  out <- .val_render(doc)
  expect_identical(out, .val_reference("page"))
  expect_true(any(grepl("Table 14.1.1 Summary", out, fixed = TRUE)))
  expect_true(any(grepl("Listing 16.2.1 Records", out, fixed = TRUE)))
  expect_identical(sum(grepl("\\page", out, fixed = TRUE)), 0L)
})
