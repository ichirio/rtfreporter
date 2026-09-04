# catx() -- the SAS CATX join, and the one listing_col() uses (#360).

test_that("values are joined by the separator", {
  expect_identical(catx("/", "COMPLETED", "BRCA1", "ADENOCARCINOMA"),
                   "COMPLETED/BRCA1/ADENOCARCINOMA")
  expect_identical(catx(" ", "Hello", "World"), "Hello World")
  expect_identical(catx("", "AE", "0001"), "AE0001")
  expect_identical(catx(", ", "a", "b", "c"), "a, b, c")
})

test_that("missing and empty values are skipped, never doubled", {
  expect_identical(catx("/", "COMPLETED", NA, "ADENOCARCINOMA"),
                   "COMPLETED/ADENOCARCINOMA")
  expect_identical(catx("/", "COMPLETED", "", "ADENOCARCINOMA"),
                   "COMPLETED/ADENOCARCINOMA")
  # the separator never appears at either end
  expect_identical(catx("/", NA, "STAGE IV", NA), "STAGE IV")
  # everything missing gives an empty string, not "NA"
  expect_identical(catx("/", NA, NA), "")
  expect_identical(catx("/", NA_character_), "")
})

test_that("blanks around a value are stripped", {
  # SAS CATX strips them, and so does listing_col(); ydisctools::catx() keeps
  # them, which is the one deliberate difference.
  expect_identical(catx("/", "  GRADE 3  ", " Y "), "GRADE 3/Y")
  # a value that is nothing but blanks counts as empty
  expect_identical(catx("/", "A", "   ", "B"), "A/B")
})

test_that("arguments are vectorised, and length-1 ones recycled", {
  expect_identical(catx("/", c("A", "B"), c("x", "y")), c("A/x", "B/y"))
  expect_identical(catx(" ", "Cohort", c("A", "B")), c("Cohort A", "Cohort B"))
  expect_identical(catx(",", c("A", NA), c("", "B"), "C"), c("A,C", "B,C"))
})

test_that("a length that cannot be recycled is an error", {
  expect_error(catx("/", c("A", "B"), c("x", "y", "z")),
               "same length, or length 1")
})

test_that("a factor joins by its labels, and numbers by their text", {
  expect_identical(catx("/", factor("high"), factor(c("a", "b"))[1L]),
                   "high/a")
  expect_identical(catx("/", 12.34, 9.87), "12.34/9.87")
  expect_identical(catx("/", 1L, TRUE), "1/TRUE")
})

test_that("the degenerate inputs behave", {
  expect_identical(catx("/"), character(0))
  expect_identical(catx("/", character(0)), character(0))
  expect_error(catx(), "single string")
  expect_error(catx(1), "single string")
  expect_error(catx(c("/", "-"), "a"), "single string")
})

test_that("listing_col() joins through exactly this function", {
  adsl <- data.frame(
    DISPTPD = c("COMPLETED", "ONGOING"),
    BRCA    = c("BRCA1", NA),
    HIST    = c("ADENOCARCINOMA", "SMALL CELL"),
    stringsAsFactors = FALSE
  )
  spec <- listing_spec(list(listing_col(c("DISPTPD", "BRCA", "HIST"),
                                        name = "COL01")),
                       spacer = FALSE, blank_row = FALSE, record = FALSE)
  expect_identical(build_listing(adsl, spec)$COL01,
                   catx("/", adsl$DISPTPD, adsl$BRCA, adsl$HIST))
})

test_that("a listing's own separator reaches catx()", {
  adsl <- data.frame(A = "x", B = "y", stringsAsFactors = FALSE)
  spec <- listing_spec(list(listing_col(c("A", "B"), name = "COL01")),
                       sep = " | ", spacer = FALSE, blank_row = FALSE,
                       record = FALSE)
  expect_identical(build_listing(adsl, spec)$COL01, catx(" | ", "x", "y"))
})
