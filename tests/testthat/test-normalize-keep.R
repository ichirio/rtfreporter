# normalize_ard(keep = ): a hierarchy's levels whose own rows stay

keep_ard <- function() {
  skip_if_not_installed("cards")
  adae <- cards::ADAE[1:300, ]
  cards::ard_stack_hierarchical(adae, variables = c(AEBODSYS, AEDECOD), by = TRTA,
                                id = USUBJID, denominator = cards::ADSL,
                                over_variables = TRUE)
}

test_that("keep = the outer level: its rows and the overall row, the inner level's out", {
  a <- keep_ard()
  h <- c("AEBODSYS", "AEDECOD")
  all <- normalize_ard(a, hierarchy = h, overall = "Any TEAE")
  soc <- normalize_ard(a, hierarchy = h, overall = "Any TEAE", keep = "AEBODSYS")
  expect_identical(sort(unique(soc$.depth)), c(0L, 1L))
  expect_identical(nrow(soc), sum(all$.depth != 2L))
  expect_true(all(is.na(soc$AEDECOD[soc$.depth == 1L & !soc$.overall])))
  ig <- attr(soc, "ard_ignored")
  expect_true(any(grepl("`keep` leaves out", ig$reason, fixed = TRUE)))
  expect_identical(sum(ig$rows[grepl("keep", ig$reason)]), sum(all$.depth == 2L))
})

test_that("keep = the inner level: its rows at depth 1, the overall row on it", {
  a <- keep_ard()
  h <- c("AEBODSYS", "AEDECOD")
  all <- normalize_ard(a, hierarchy = h, overall = "Any TEAE")
  pt <- normalize_ard(a, hierarchy = h, overall = "Any TEAE", keep = "AEDECOD")
  expect_identical(sum(pt$.depth == 1L & !pt$.overall), sum(all$.depth == 2L))
  expect_identical(unique(as.character(pt$AEDECOD[pt$.overall])), "Any TEAE")
  expect_identical(pt$.label[!pt$.overall & pt$.depth == 1L],
                   as.character(pt$AEDECOD[!pt$.overall & pt$.depth == 1L]))
  # a table of the preferred terms alone
  p <- table_plan(pt, cols = "TRTA", rows = c(label = "AEDECOD"))
  expect_s3_class(p, "table_plan")
})

test_that("keep = every level is the hierarchy as it is; a level not in it is an error", {
  a <- keep_ard()
  h <- c("AEBODSYS", "AEDECOD")
  all <- normalize_ard(a, hierarchy = h, overall = "Any TEAE")
  same <- normalize_ard(a, hierarchy = h, overall = "Any TEAE", keep = rev(h))
  expect_identical(same$.depth, all$.depth)
  expect_identical(same$.label, all$.label)
  expect_error(normalize_ard(a, hierarchy = h, keep = "AEHLT"), "not a level of `hierarchy`")
  expect_error(normalize_ard(a, hierarchy = h, keep = character()), "names the levels")
})
