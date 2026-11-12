# plan_labels() on a hierarchical AE table by maximum grade: a SOC, the overall
# row, a grade and a PT each relabel by the variable that holds them; an entry
# aimed at the wrong key says which key holds the value (it was an NA stopping
# `if` when a key had rows with no value, as a SOC row has no PT)

mgl_plan <- function() {
  ae <- cards::ADAE
  ae$AESEV <- factor(ae$AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
  top <- function(...) dplyr::slice_max(ae, as.integer(AESEV), by = c(USUBJID, ...), n = 1,
                                        with_ties = FALSE)
  h <- function(d, v) cards::ard_stack_hierarchical(d, variables = cards::all_of(v),
                                                    by = TRTA, denominator = cards::ADSL,
                                                    id = USUBJID)
  ard <- cards::bind_ard(
    cards::ard_stack_hierarchical(top(), variables = AESEV, by = TRTA, denominator = cards::ADSL,
                                  id = USUBJID, over_variables = TRUE),
    h(top(AEBODSYS), c("AEBODSYS", "AESEV")),
    h(top(AEBODSYS, AEDECOD), c("AEBODSYS", "AEDECOD", "AESEV")),
    .distinct = TRUE, .quiet = TRUE)
  table_plan(normalize_ard(ard, hierarchy = c("AEBODSYS", "AEDECOD", "AESEV"),
                           overall = "Any TEAE"),
             cols = "TRTA", rows = c(soc = "AEBODSYS", pt = "AEDECOD"),
             label = list(label = "AESEV")) |>
    plan_cells(categorical = "{n} ({p:.1f%})") |>
    plan_sort(keys = c(".overall", "soc", "pt", ".depth"))
}
mgl_tab <- function(p) {
  t <- as.data.frame(suppressMessages(plan_apply(p, "table")))
  t[] <- lapply(t, function(v) if (is.factor(v)) as.character(v) else v)
  t
}

test_that("a SOC, the overall row, a grade and a PT each relabel by their variable", {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  p <- mgl_plan()
  b <- mgl_tab(p)
  soc <- unique(b$soc[!is.na(b$soc) & !b$soc %in% "Any TEAE"])[1L]
  pt <- unique(b$pt[!is.na(b$pt)])[1L]

  # a SOC
  s <- mgl_tab(plan_labels(p, AEBODSYS = stats::setNames("Renamed SOC", soc)))
  expect_false(soc %in% s$soc)
  expect_identical(sum(s$soc %in% "Renamed SOC"), sum(b$soc %in% soc))
  expect_identical(s$pt, b$pt)
  expect_identical(s$label, b$label)
  # the others are unchanged
  expect_identical(s$soc[!b$soc %in% soc], b$soc[!b$soc %in% soc])

  # the overall row
  o <- mgl_tab(plan_labels(p, AEBODSYS = c("Any TEAE" = "All TEAEs")))
  expect_identical(o$soc[1L], "All TEAEs")
  expect_false("Any TEAE" %in% o$soc)
  expect_identical(o$soc[-seq_len(sum(b$soc %in% "Any TEAE"))],
                   b$soc[-seq_len(sum(b$soc %in% "Any TEAE"))])

  # a grade
  g <- mgl_tab(plan_labels(p, AESEV = c(MILD = "Mild (1)")))
  expect_identical(g$label, ifelse(b$label %in% "MILD", "Mild (1)", b$label))
  expect_identical(g$soc, b$soc)

  # a PT still works
  q <- mgl_tab(plan_labels(p, AEDECOD = stats::setNames("Renamed PT", pt)))
  expect_false(pt %in% q$pt)
  expect_identical(sum(q$pt %in% "Renamed PT"), sum(b$pt %in% pt))
  expect_identical(q$soc, b$soc)

  # all four in one card
  all4 <- mgl_tab(plan_labels(p, AEBODSYS = c("Any TEAE" = "All TEAEs",
                                              stats::setNames("Renamed SOC", soc)),
                              AESEV = c(MILD = "Mild (1)"),
                              AEDECOD = stats::setNames("Renamed PT", pt)))
  expect_true(all(c("All TEAEs", "Renamed SOC") %in% all4$soc))
  expect_true("Renamed PT" %in% all4$pt)
  expect_true("Mild (1)" %in% all4$label)
  expect_identical(nrow(all4), nrow(b))
})

test_that("an entry that matches no value of any key changes nothing and does not stop", {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  p <- mgl_plan()
  b <- mgl_tab(p)
  expect_identical(mgl_tab(plan_labels(p, AEDECOD = c("No such PT" = "X"))), b)
  expect_identical(mgl_tab(plan_labels(p, AEBODSYS = c("No such SOC" = "X"))), b)
})

test_that("an entry aimed at the wrong key names the key that holds the value", {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  p <- mgl_plan()
  b <- mgl_tab(p)
  soc <- unique(b$soc[!is.na(b$soc) & !b$soc %in% "Any TEAE"])[1L]
  # a SOC under the PT variable
  expect_error(mgl_tab(plan_labels(p, AEDECOD = stats::setNames("X", soc))),
               "AEDECOD.*occurs in 'soc'.*name it `labels\\$soc`")
  # the overall row under the PT variable
  expect_error(mgl_tab(plan_labels(p, AEDECOD = c("Any TEAE" = "X"))), "occurs in 'soc'")
  # a grade under a hierarchy key
  expect_error(mgl_tab(plan_labels(p, AEBODSYS = c(MILD = "X"))), "occurs in 'label'")
  expect_error(mgl_tab(plan_labels(p, AEDECOD = c(MILD = "X"))), "occurs in 'label'")
})
