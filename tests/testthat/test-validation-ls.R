# The listing area's leaf requirements that had no test (the deviations of
# the first validation version): each test names its FT id and its FRs.
# Fictional adverse events of a made-up study.

.ls_ae <- function() {
  data.frame(USUBJID = sprintf("XP-01-%03d", 1:6),
             AEDECOD = rep(c("HEADACHE", "NAUSEA", "DIZZINESS"), 2),
             stringsAsFactors = FALSE)
}

.ls_spec <- function(...) {
  listing_spec(list(listing_col("USUBJID"), listing_col("AEDECOD")), ...)
}

test_that("FT-LS146 [FR-LS111]: build_listing() refuses an rlistings listing_df and points to as_rtftables()", {
  x <- structure(.ls_ae(), class = c("listing_df", "data.frame"))
  expect_error(build_listing(x, .ls_spec()),
               paste0("`data` is an rlistings listing (`listing_df`), which rlistings has ",
                      "already laid out -- its display columns, key-column suppression and ",
                      "titles are baked in.  Pass it straight to `as_rtftables()` instead ",
                      "of building a listing from it."),
               fixed = TRUE)
})

test_that("FT-LS147 [FR-LS231 FR-LS232]: fit_listing_widths() stops on a page with no writable width and on a character width it cannot measure", {
  d <- .ls_ae()
  msg <- "The page's margins leave no writable width."
  # 6 + 6 inches of margin: more than the sheet is wide
  expect_error(fit_listing_widths(d, .ls_spec(),
                                  page = rtf_page(margin_left_in = 6, margin_right_in = 6)),
               msg, fixed = TRUE)
  expect_error(fit_listing_widths(d, .ls_spec(),
                                  page = list(margin_left_in = 6, margin_right_in = 6)),
               msg, fixed = TRUE)
  # a size of 0 or NA gives no width to one character, whatever the font
  expect_error(fit_listing_widths(d, .ls_spec(), font = "Arial", font_size_half_points = 0),
               "Could not measure the width of one character in font \"Arial\".",
               fixed = TRUE)
  expect_error(fit_listing_widths(d, .ls_spec(), font = "Courier New",
                                  font_size_half_points = NA),
               "Could not measure the width of one character in font \"Courier New\".",
               fixed = TRUE)
})

test_that("FT-LS148 [FR-LS126]: listing_spec(blank_row_first = ) is passed on as as_rtftables(blank_row_first = ): a blank row on every page by default, none with FALSE", {
  d <- .ls_ae()
  blanks <- function(spec) {
    pages <- as_rtftables(d, listing = spec, max_rows = 3)
    lapply(pages, function(p) attr(p$data, "rtf_blank_rows"))
  }
  # NULL takes the template's TRUE
  expect_true(.ls_spec()$blank_row_first)
  on <- blanks(.ls_spec())
  expect_gt(length(on), 1L)
  for (b in on) expect_identical(b, 0L)            # every page, not only the first
  expect_false(.ls_spec(blank_row_first = FALSE)$blank_row_first)
  off <- blanks(.ls_spec(blank_row_first = FALSE))
  expect_identical(length(off), length(on))
  for (b in off) expect_false(0L %in% b)
})

test_that("FT-LS149 [FR-LS194]: listing_code(indent = ) is the number of spaces before each listing_col() call; 2 by default", {
  spec <- .ls_spec()
  cols <- function(code) grep("listing_col(", code, value = TRUE, fixed = TRUE)
  expect_identical(cols(listing_code(spec)),
                   c("  listing_col(\"USUBJID\"),", "  listing_col(\"AEDECOD\")"))
  expect_identical(cols(listing_code(spec, indent = 4)),
                   c("    listing_col(\"USUBJID\"),", "    listing_col(\"AEDECOD\")"))
  expect_identical(cols(listing_code(spec, indent = 0)),
                   c("listing_col(\"USUBJID\"),", "listing_col(\"AEDECOD\")"))
  # the first and last lines are not indented
  code <- listing_code(spec, indent = 4)
  expect_identical(code[[1L]], "listing_spec(list(")
  expect_identical(code[[length(code)]], "))")
})

test_that("FT-LS150 [FR-LS213]: fit_listing_widths(font = ) turns the page into characters of that font; NULL is getOption(\"rtfreporter.font\")", {
  d <- .ls_ae()
  tw <- function(...) attr(fit_listing_widths(d, .ls_spec(), ...), "rtf_listing_fit")$total_width
  courier <- tw(font = "Courier New")
  arial   <- tw(font = "Arial")
  # Arial's digit is narrower than Courier's, so more of them fit
  expect_gt(arial, courier)
  old <- options(rtfreporter.font = "Courier New")
  on.exit(options(old), add = TRUE)
  expect_identical(tw(), courier)
  options(rtfreporter.font = "Arial")
  expect_identical(tw(), arial)
})
