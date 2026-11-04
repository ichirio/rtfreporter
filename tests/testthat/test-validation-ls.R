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

# -- #910: a separator inside one value is data -------------------------------
# The separator breaks BETWEEN the variables joined in a cell.  One variable is
# one value: it stays on a line if it fits, and its "/" is a place to break only
# when it does not.  Fictional values.

.ls910_body <- function(d, ...) {
  build_listing(d, listing_spec(list(...), spacer = FALSE, blank_row = FALSE,
                                record = FALSE))
}

test_that("FT-LS164 [FR-LS256 FR-LS155 FR-LS148]: a column of one variable keeps a value that fits on one line, and a value or label too wide still breaks after its separator", {
  x <- data.frame(AGESEX = c("64/M", "41/F"), stringsAsFactors = FALSE)

  # #910's repro, through the default type
  b <- build_listing(x, listing_spec(type = "multiline",
         cols = list(listing_col("AGESEX", label = "Age/Sex", width = 10))))
  expect_identical(b$AGESEX, c("64/M", "", "41/F", ""))
  expect_identical(attr(b, "rtf_listing")$cols[[1L]]$label, "Age/Sex")

  # the value fits exactly, and one character short
  expect_identical(.ls910_body(x, listing_col("AGESEX", width = 4))[[1L]],
                   c("64/M", "41/F"))
  expect_identical(.ls910_body(x, listing_col("AGESEX", width = 3))[[1L]],
                   c("64/", "M", "41/", "F"))

  # too wide: the separator is still a place to break, and the line fills
  w <- data.frame(T = "COMPLETED/UPPER/HYPERGLYCAEMIA", stringsAsFactors = FALSE)
  expect_identical(.ls910_body(w, listing_col("T", width = 22))[[1L]],
                   c("COMPLETED/UPPER/", "HYPERGLYCAEMIA"))
  expect_identical(.ls910_body(w, listing_col("T", width = 10))[[1L]],
                   c("COMPLETED/", "UPPER/", "HYPERGLYCA", "EMIA"))

  # full-width characters count as two columns: the value below is 7 wide
  fw <- data.frame(V = "６４/男", stringsAsFactors = FALSE)
  expect_identical(.ls910_body(fw, listing_col("V", width = 7))[[1L]],
                   "６４/男")
  expect_identical(.ls910_body(fw, listing_col("V", width = 6))[[1L]],
                   c("６４/", "男"))

  # the header of a one-variable column: one line when it fits, the same
  # rule when it does not; a vector is the author's layout and is left alone
  lab <- function(width, label)
    attr(.ls910_body(x, listing_col("AGESEX", width = width, label = label)),
         "rtf_listing")$cols[[1L]]$label
  expect_identical(lab(10, "Age/Sex"), "Age/Sex")
  expect_identical(lab(5, "Age/Sex"), "Age/\nSex")
  expect_identical(lab(10, c("Age/", "Sex")), "Age/
Sex")
  expect_identical(lab(10, "Age/\nSex"), "Age/\nSex")
  # derived from the variable's own label
  attr(x$AGESEX, "label") <- "Age/Sex"
  expect_identical(attr(.ls910_body(x, listing_col("AGESEX", width = 10)),
                        "rtf_listing")$cols[[1L]]$label, "Age/Sex")

  # layout = "flow" is what it was
  expect_identical(.ls910_body(x, listing_col("AGESEX", width = 10,
                                              layout = "flow"))[[1L]],
                   c("64/M", "41/F"))
  expect_identical(.ls910_body(w, listing_col("T", width = 22,
                                              layout = "flow"))[[1L]],
                   listing_wrap(w$T, 22, layout = "flow"))
})

test_that("FT-LS165 [FR-LS257 FR-LS155 FR-LS150]: a column of two or more variables starts each variable on a line, and a separator inside a value is not a join", {
  d <- data.frame(A = c("A/B", "A/B", NA, "", "COMPLETED/UPPER/HYPERGLYCAEMIA"),
                  B = c("C", NA, "C/D", "C", "X"),
                  stringsAsFactors = FALSE)
  b <- .ls910_body(d, listing_col(c("A", "B"), width = 20, name = "AB"))
  # per record: the lines of one cell (cells are padded with "")
  cells <- split(b$AB, rep(1:5, c(2, 1, 1, 1, 3)))
  expect_identical(unname(cells[[1L]]), c("A/B/", "C"))
  expect_identical(unname(cells[[2L]]), "A/B")        # B missing: no trailing "/"
  expect_identical(unname(cells[[3L]]), "C/D")        # A missing: a value, not a join
  expect_identical(unname(cells[[4L]]), "C")          # A empty
  expect_identical(unname(cells[[5L]]), c("COMPLETED/UPPER/", "HYPERGLYCAEMIA/", "X"))

  # the same variables under "flow": one joined text, as before
  bf <- .ls910_body(d[1:3, ], listing_col(c("A", "B"), width = 20,
                                          layout = "flow", name = "AB"))
  expect_identical(bf$AB, c("A/B/C", "A/B", "C/D"))

  # a derived header stacks the labels of the variables, as the cell does,
  # and a "/" inside one label is words
  attr(d$A, "label") <- "Age/Sex"
  attr(d$B, "label") <- "Term"
  hdr <- function(...) attr(.ls910_body(d, listing_col(c("A", "B"), ...)),
                            "rtf_listing")$cols[[1L]]$label
  expect_identical(hdr(width = 20), "Age/Sex/\nTerm")
  expect_identical(hdr(width = 20, layout = "flow"), "Age/Sex/Term")
  expect_identical(hdr(), "Age/Sex/Term")              # no width: one line
})

test_that("FT-LS166 [FR-LS129 FR-LS171 FR-LS257]: listing_spec(wrap = listing_wrap) changes nothing, and a custom wrap is called one variable's value at a time, the separator kept on all but the last", {
  d <- data.frame(A = c("A/B", "64/M", NA), B = c("C", "HEADACHE", "D/E"),
                  stringsAsFactors = FALSE)
  cols <- list(listing_col(c("A", "B"), width = 12, name = "AB"),
               listing_col("A", width = 4, name = "A1"))
  default <- build_listing(d, listing_spec(cols))
  same    <- build_listing(d, listing_spec(cols, wrap = listing_wrap))
  expect_identical(same, default, ignore_attr = TRUE)
  expect_identical(attr(same, "rtf_listing")$cols[[1L]]$label,
                   attr(default, "rtf_listing")$cols[[1L]]$label)

  seen <- list()
  spy <- function(text, width, sep, layout) {
    seen[[length(seen) + 1L]] <<- list(text = text, layout = layout)
    listing_wrap(text, width, sep, layout)
  }
  build_listing(d[1L, ], listing_spec(list(listing_col(c("A", "B"), width = 12,
                                                       label = "")),
                                      wrap = spy))
  texts <- vapply(seen, function(s) s$text, character(1L))
  expect_identical(texts, c("", "A/B/", "C"))      # the header, then the two values
  expect_true(all(vapply(seen, function(s) identical(s$layout, "flow") ||
                           identical(s$text, ""), logical(1L))))
})
