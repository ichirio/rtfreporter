## tests/testthat/test-split-spec-group-col.R
##
## #328 -- a page_split_*() spec carries its own group_col / group_by, and the
## group column is needed by more than pagination.  Naming it only inside the
## spec used to leave blank_rows / collapse_repeats reading NULL.

library(testthat)

## PT first, SOC second, so the "no group_col" fallback (column 1) is NOT the
## group column.  With SOC in column 1 the fallback lands on the right answer
## by accident and the defect is invisible.
.ae328 <- function() {
  data.frame(
    PT  = c("Atrial fibrillation", "Bradycardia", "Tachycardia", "Palpitations",
            "Headache", "Dizziness", "Somnolence", "Tremor",
            "Nasopharyngitis", "Pneumonia", "Sinusitis"),
    SOC = c(rep("Cardiac disorders", 4), rep("Nervous system disorders", 4),
            rep("Infections", 3)),
    N   = as.character(1:11),
    stringsAsFactors = FALSE
  )
}

test_that("the split factories tag their group configuration", {
  f <- page_split_group_safe(group_col = "SOC", max_rows = 10)
  expect_identical(attr(f, "rtf_split_group_col"), "SOC")
  expect_identical(attr(f, "rtf_split_group_by"), "auto")

  expect_identical(attr(page_split_group_force(group_col = "SOC", max_rows = 10),
                        "rtf_split_group_col"), "SOC")
  expect_identical(attr(page_split_by_value(group_col = "SOC"),
                        "rtf_split_group_col"), "SOC")

  # A spec with no group_col tags NULL, so nothing is adopted.
  expect_null(attr(page_split_group_safe(max_rows = 10), "rtf_split_group_col"))
})

test_that("group_col inside the split spec reaches blank_rows (#328)", {
  ae <- .ae328()
  top  <- as_rtftables(ae, group_col = "SOC", blank_rows = "between_groups")
  spec <- as_rtftables(ae, blank_rows = "between_groups",
                       split = page_split_group_safe(group_col = "SOC",
                                                     max_rows = 99))
  # SOC changes after rows 4 and 8.
  expect_identical(top[[1L]]$blank_rows, c(4L, 8L))
  expect_identical(spec[[1L]]$blank_rows, top[[1L]]$blank_rows)
})

test_that("naming group_col nowhere is unchanged", {
  none <- as_rtftables(.ae328(), blank_rows = "between_groups")
  # The column-1 fallback: PT changes on every row.
  expect_identical(none[[1L]]$blank_rows, 1:10)
})

test_that("an explicit top-level group_col still wins over the spec", {
  ae <- .ae328()
  out <- as_rtftables(ae, group_col = "PT", blank_rows = "between_groups",
                      split = page_split_group_safe(group_col = "SOC",
                                                    max_rows = 99))
  expect_identical(out[[1L]]$blank_rows, 1:10)   # PT, not SOC
})

test_that("collapse_repeats also sees the spec's group_col", {
  ae  <- .ae328()
  out <- as_rtftables(ae, collapse_repeats = "SOC",
                      split = page_split_group_safe(group_col = "SOC",
                                                    max_rows = 99))
  soc <- out[[1L]]$data$SOC
  expect_identical(soc[[1L]], "Cardiac disorders")
  expect_true(is.na(soc[[2L]]))                  # run suppressed
  expect_identical(soc[[5L]], "Nervous system disorders")
})

test_that("page_split_by_value names its pages from the spec's group_col", {
  pages <- as_rtftables(.ae328(),
                        split = page_split_by_value(group_col = "SOC"))
  expect_length(pages, 3L)
  expect_identical(names(pages),
                   c("Cardiac disorders", "Nervous system disorders",
                     "Infections"))
})

test_that("group_by from the spec is adopted, and an explicit one wins", {
  f <- page_split_group_safe(group_col = "SOC", max_rows = 99,
                             group_by = "value")
  expect_identical(attr(f, "rtf_split_group_by"), "value")
  # Explicit group_by at the top level is not overwritten.
  out <- as_rtftables(.ae328(), split = f, group_by = "value",
                      blank_rows = "between_groups")
  expect_identical(out[[1L]]$blank_rows, c(4L, 8L))
})
