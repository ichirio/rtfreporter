test_that("overall_row() refuses NA (#766)", {
  expect_error(overall_row(NA_character_), "`label` is required")
})

.ard766 <- function() {
  data.frame(TRT = c(NA, "A", "B"), variable = c("..ard_total_n..", "X", "X"),
             variable_level = NA_character_,
             context = c("total_n", "categorical", "categorical"),
             stat_name = c("N", "n", "n"), stat_label = c("N", "n", "n"),
             stat = c(254, 7, 9), stringsAsFactors = FALSE)
}

test_that("normalize_ard(keys =) keeps a key column the frame already has (#766)", {
  nz <- normalize_ard(.ard766(), keys = "TRT")
  expect_identical(nz$TRT, c("A", "B"))
})

test_that("widen_ard() checks `notes` (#766)", {
  nz <- normalize_ard(.ard766(), keys = "TRT")
  expect_error(widen_ard(nz, cols = "TRT", cells = "{n}", notes = "yes"),
               "`notes` must be")
  expect_no_error(widen_ard(nz, cols = "TRT", cells = "{n}", notes = FALSE))
})

test_that("the raw-ARD message reads 'a normalize_ard() result' (#766)", {
  expect_error(widen_ard(data.frame(a = 1), cols = "a"), "like a normalize_ard() result",
               fixed = TRUE)
})

test_that("normalize_ard(overall =) without a hierarchy stops (#766)", {
  d <- data.frame(TRT = "A", variable = "..ard_hierarchical_overall..",
                  variable_level = NA_character_, context = "hierarchical",
                  stat_name = "n", stat_label = "n", stat = 3, stringsAsFactors = FALSE)
  expect_error(normalize_ard(d, keys = "TRT", overall = "Any TEAE"), "give `hierarchy` too",
               fixed = TRUE)
})
