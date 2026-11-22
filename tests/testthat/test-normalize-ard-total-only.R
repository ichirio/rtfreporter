# normalize_ard() of an ARD that holds only the total, or only rows of the
# contexts it drops (#1092): a message that says what is missing, not
# base R's "replacement has 1 row, data has 0".  Fictional data only.

.nat_total <- function() {
  # the shape of cards::ard_total_n(): one row, no group, the N of the study
  data.frame(variable = "..ard_total_n..", context = "total_n",
             stat_name = "N", stat_label = "N", stat = 2,
             stringsAsFactors = FALSE)
}

test_that("FT-AR8006 [FR-AR264]: normalize_ard() of only the total, or only dropped contexts, says what to add", {
  expect_error(normalize_ard(.nat_total()),
               "The ARD has only the total (`..ard_total_n..`, from cards::ard_total_n()) and no statistics of a table.",
               fixed = TRUE)
  expect_error(normalize_ard(.nat_total()),
               "Bind it with the table's statistics: cards::bind_ard(",
               fixed = TRUE)
  att <- data.frame(variable = "AGE", context = "attributes",
                    stat_name = "label", stat_label = "Label", stat = "Age",
                    stringsAsFactors = FALSE)
  expect_error(normalize_ard(att),
               "The ARD has only rows of the contexts `drop_contexts` drops (attributes) and no statistics of a table.",
               fixed = TRUE)
  # with a statistic beside it, the total is kept as before
  body <- data.frame(variable = "AGE", context = "summary",
                     stat_name = "mean", stat_label = "Mean", stat = 55,
                     stringsAsFactors = FALSE)
  d <- normalize_ard(rbind(body, .nat_total()))
  expect_identical(attr(d, "ard_total_n"), 2)
  expect_identical(d$stat, 55)
})

test_that("FT-AR8007 [FR-AR264]: normalize_ard(cards::ard_total_n()) stops with the message", {
  skip_if_not_installed("cards")
  adsl <- data.frame(TRT01A = c("A", "B"), AGE = c(50, 60))
  expect_error(normalize_ard(cards::ard_total_n(adsl)),
               "The ARD has only the total (`..ard_total_n..`", fixed = TRUE)
})
