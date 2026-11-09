# A header's {n} from a population stated by group: ..ard_total_n.. keyed by
# the column (cards::ard_strata(.by = , .f = ~ ard_total_n(.x))) is read
# first, before a stack's denominator; it is no cell of the body.

.tn_data <- function() {
  adsl <- data.frame(USUBJID = 1:10, TRT01A = rep(c("Drug X", "Placebo"), each = 5),
                     SAFFL = c(rep("Y", 4), "N", rep("Y", 5)),
                     AGE = c(30, NA, 40, 50, 60, 31, 41, 51, 61, 71),
                     stringsAsFactors = FALSE)
  adae <- data.frame(USUBJID = c(1, 1, 3, 6, 7),
                     TRT01A = c("Drug X", "Drug X", "Drug X", "Placebo", "Placebo"),
                     AEBODSYS = "SOC A", AEDECOD = c("PT1", "PT2", "PT1", "PT1", "PT2"),
                     stringsAsFactors = FALSE)
  list(adsl = adsl, adae = adae, saf = adsl[adsl$SAFFL == "Y", ])
}

.tn_ae <- function(x, overall = FALSE) {
  cards::ard_stack_hierarchical(x$adae, variables = c(AEBODSYS, AEDECOD), by = TRT01A,
                                denominator = x$saf, id = USUBJID,
                                over_variables = TRUE, overall = overall)
}

.tn_by_arm <- function(x) {
  cards::ard_strata(x$adsl, .by = TRT01A, .f = ~ cards::ard_total_n(.x))
}

.tn_header <- function(ard, total = FALSE) {
  p <- table_plan(normalize_ard(ard), cols = "TRT01A")
  if (total) p <- plan_total(p)
  p <- plan_col_header(p, "{col} (N={n})", values = list(n = TRUE))
  page <- suppressMessages(plan_apply(p))[[1L]]
  list(header = unlist(page$col_header), rows = nrow(page$data), cols = ncol(page$data))
}

test_that("a stack alone keeps its own denominator in the header", {
  skip_if_not_installed("cards")
  h <- .tn_header(.tn_ae(.tn_data()))
  expect_true(all(c("Drug X (N=4)", "Placebo (N=5)") %in% h$header))
})

test_that("a total stated by group wins over the stack's denominator", {
  skip_if_not_installed("cards")
  x <- .tn_data()
  base <- .tn_header(.tn_ae(x))
  h <- .tn_header(dplyr::bind_rows(.tn_ae(x), .tn_by_arm(x)))
  expect_true(all(c("Drug X (N=5)", "Placebo (N=5)") %in% h$header))
  # header-only: no extra row or column in the body
  expect_identical(h$rows, base$rows)
  expect_identical(h$cols, base$cols)
})

test_that("the overall total_n gives the Total column, the by-group one each arm", {
  skip_if_not_installed("cards")
  x <- .tn_data()
  h <- .tn_header(dplyr::bind_rows(.tn_ae(x, overall = TRUE), .tn_by_arm(x),
                                   cards::ard_total_n(x$adsl)), total = TRUE)
  expect_true(all(c("Drug X (N=5)", "Placebo (N=5)", "Total (N=10)") %in% h$header))
})

test_that("a summary with a missing value takes the stated total, not its non-missing count", {
  skip_if_not_installed("cards")
  x <- .tn_data()
  age <- cards::ard_summary(x$adsl, by = TRT01A, variables = AGE)
  h <- .tn_header(dplyr::bind_rows(age, .tn_by_arm(x)))
  expect_true(all(c("Drug X (N=5)", "Placebo (N=5)") %in% h$header))
})

test_that("normalize_ard() keeps a total by group, marked as no cell; the study total stays an attribute", {
  skip_if_not_installed("cards")
  x <- .tn_data()
  n <- normalize_ard(dplyr::bind_rows(.tn_by_arm(x), cards::ard_total_n(x$adsl)))
  kept <- n[n$variable %in% "..ard_total_n..", , drop = FALSE]
  expect_identical(sort(kept$TRT01A), c("Drug X", "Placebo"))
  expect_true(all(kept$.key_own))
  expect_identical(attr(n, "ard_total_n"), 10)
})
