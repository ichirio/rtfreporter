# plan_sort(): a group's own row (its lower row key empty) comes before the
# group's members -- a SOC's rows before its PTs', the overall rows first --
# when a row key is a sort key (#695)

test_that("three levels: the overall rows, then each SOC's own rows, then its PTs'", {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  adsl <- cards::ADSL
  ae <- cards::ADAE
  ae$AESEV <- factor(ae$AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
  top <- function(...) dplyr::slice_max(ae, as.integer(AESEV), by = c(USUBJID, ...), n = 1,
                                        with_ties = FALSE)
  h <- function(d, v) cards::ard_stack_hierarchical(d, variables = tidyselect::all_of(v),
                                                    by = TRTA, denominator = adsl, id = USUBJID)
  ard <- cards::bind_ard(
    cards::ard_stack_hierarchical(top(), variables = AESEV, by = TRTA, denominator = adsl,
                                  id = USUBJID, over_variables = TRUE),
    h(top(AEBODSYS), c("AEBODSYS", "AESEV")),
    h(top(AEBODSYS, AEDECOD), c("AEBODSYS", "AEDECOD", "AESEV")),
    .distinct = TRUE, .quiet = TRUE)
  p <- table_plan(normalize_ard(ard, hierarchy = c("AEBODSYS", "AEDECOD", "AESEV"),
                                overall = "Any TEAE"),
                  cols = "TRTA", rows = c(soc = "AEBODSYS", pt = "AEDECOD"),
                  label = list(label = "AESEV")) |>
    plan_cells(categorical = "{n} ({p:.1f%})") |>
    plan_sort(keys = c(".overall", "soc", "pt", ".depth"))
  t <- as.data.frame(suppressMessages(plan_apply(p, "table")))
  expect_identical(t$soc[1L], "Any TEAE")
  # the overall rows' grades next, before any SOC
  expect_identical(as.character(t$label[2:4]), c("MILD", "MODERATE", "SEVERE"))
  expect_true(all(is.na(t$soc[2:4])))
  # in each SOC: its own row, its grades, then its PTs
  first_soc <- t$soc[5L]
  blk <- t[t$soc %in% first_soc, , drop = FALSE]
  expect_true(is.na(blk$pt[1L]) && is.na(blk$label[1L]))
  own <- which(is.na(blk$pt))
  expect_identical(own, seq_along(own))
  # descending too: the group's own row stays first
  pd <- p |> plan_sort(keys = c(".overall", "-soc", "pt", ".depth"))
  td <- as.data.frame(suppressMessages(plan_apply(pd, "table")))
  b2 <- td[td$soc %in% td$soc[5L], , drop = FALSE]
  expect_true(is.na(b2$pt[1L]))
})
