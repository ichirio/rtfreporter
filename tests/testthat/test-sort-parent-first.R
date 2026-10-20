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
  h <- function(d, v) cards::ard_stack_hierarchical(d, variables = cards::all_of(v),
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
  # by a group's own count (`-n@pt`): the SOC's own rows, with no PT, first
  pn <- p |> plan_sort(keys = c(".overall", "-n@soc", "soc", "-n@pt", "pt", ".depth"))
  tn <- as.data.frame(suppressMessages(plan_apply(pn, "table")))
  b3 <- tn[tn$soc %in% tn$soc[5L], , drop = FALSE]
  own3 <- which(is.na(b3$pt))
  expect_identical(own3, seq_along(own3))
})

test_that("plan_total() with two column keys folds the inner one: each arm's 'Any grade' column (#695)", {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  adsl <- cards::ADSL
  ae <- cards::ADAE
  ae$AESEV <- factor(ae$AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
  top <- dplyr::slice_max(ae, as.integer(AESEV), by = c(USUBJID, AEBODSYS, AEDECOD), n = 1,
                          with_ties = FALSE)
  ard <- cards::ard_stack_hierarchical(top, variables = c(AEBODSYS, AEDECOD, AESEV), by = TRTA,
                                       denominator = adsl, id = USUBJID, over_variables = TRUE)
  p <- table_plan(normalize_ard(ard, hierarchy = c("AEBODSYS", "AEDECOD", "AESEV"),
                                overall = "Any TEAE"),
                  cols = c("TRTA", "AESEV"), rows = c(soc = "AEBODSYS"),
                  label = list(label = "AEDECOD")) |>
    plan_cells(categorical = "{n} ({p:.1f%})")
  t <- as.data.frame(suppressMessages(plan_apply(p |> plan_total(label = "Any grade",
                                                                  position = "first"), "table")))
  arms <- levels(factor(adsl$TRTA))
  grades <- c("Any grade", "MILD", "MODERATE", "SEVERE")
  want <- unlist(lapply(arms, function(a) paste0(a, "____", grades)))
  expect_identical(setdiff(names(t), c("soc", "label")), want)
  # the any-grade count is the PT's subjects; the overall row (no TRTA's
  # grade) is that arm's any-TEAE count, and no column of all arms
  pt <- t[!is.na(t$label), , drop = FALSE][1L, ]
  n_any <- as.integer(sub(" .*", "", pt[[want[1L]]]))
  n_g <- suppressWarnings(as.integer(sub(" .*", "", unlist(pt[want[2:4]]))))
  expect_identical(n_any, sum(n_g, na.rm = TRUE))
  expect_false(any(grepl("^NA____|^Any grade$", names(t))))
  # last: after each arm's grades
  t2 <- as.data.frame(suppressMessages(plan_apply(p |> plan_total(label = "Any grade"), "table")))
  expect_identical(names(t2)[3:6], paste0(arms[1L], "____", c("MILD", "MODERATE", "SEVERE", "Any grade")))
})

test_that("levels(<key>): plan_levels' values first, the others by the next keys (#695)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  ae <- cards::ADAE
  ard <- cards::ard_stack_hierarchical(ae, variables = c(AEBODSYS, AEDECOD), by = TRTA,
                                       denominator = adsl, id = USUBJID)
  base <- table_plan(normalize_ard(ard, hierarchy = c("AEBODSYS", "AEDECOD")),
                     cols = "TRTA", rows = c(soc = "AEBODSYS"), label = list(label = "AEDECOD")) |>
    plan_cells(categorical = "{n} ({p:.1f%})")
  socs <- function(p) unique(as.character(as.data.frame(suppressMessages(plan_apply(p, "table")))$soc))
  by_n <- socs(base |> plan_sort(keys = c("-n@soc", "soc", ".depth", "-n", "label")))
  pin <- "VASCULAR DISORDERS"
  pinned <- socs(base |> plan_levels(AEBODSYS = pin) |>
                   plan_sort(keys = c("levels(soc)", "-n@soc", "soc", ".depth", "-n", "label")))
  # pinned first, the rest in the count's order
  expect_identical(pinned, c(pin, setdiff(by_n, pin)))
  # by name: pinned first, the rest by name
  by_name <- socs(base |> plan_levels(AEBODSYS = pin) |>
                    plan_sort(keys = c("levels(soc)", "soc", ".depth", "-n", "label")))
  expect_identical(by_name, c(pin, sort(setdiff(by_name, pin))))
  # no plan_levels for the key: all tie, as if the key were not there
  expect_identical(socs(base |> plan_sort(keys = c("levels(soc)", "-n@soc", "soc", ".depth", "-n",
                                                   "label"))), by_n)
  expect_error(suppressMessages(plan_apply(base |> plan_sort(keys = "levels(nope)"), "table")),
               "no row key 'nope'")
})

test_that("levels(<key>): the pinned block first even when another has its count (a tie)", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  # every subject with an AE has one in this SOC: its count is the overall count
  ae <- cards::ADAE[cards::ADAE$AEBODSYS %in% c("GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
                                                 "CARDIAC DISORDERS"), ]
  gen <- "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS"
  ae <- ae[ae$USUBJID %in% ae$USUBJID[ae$AEBODSYS == gen], ]
  all <- ae[!duplicated(ae$USUBJID), ]
  all$AEBODSYS <- "ZZ ANY EVENT"          # a name that sorts last
  ard <- cards::ard_stack_hierarchical(rbind(ae, all), variables = c(AEBODSYS, AEDECOD), by = TRTA,
                                       denominator = adsl, id = USUBJID)
  p <- table_plan(normalize_ard(ard, hierarchy = c("AEBODSYS", "AEDECOD")),
                  cols = "TRTA", rows = c(soc = "AEBODSYS"), label = list(label = "AEDECOD")) |>
    plan_cells(categorical = "{n} ({p:.1f%})") |>
    plan_levels(AEBODSYS = "ZZ ANY EVENT") |>
    plan_sort(keys = c("levels(soc)", "-n@soc", "soc", ".depth", "-n", "label"))
  t <- as.data.frame(suppressMessages(plan_apply(p, "table")))
  expect_identical(as.character(t$soc[1L]), "ZZ ANY EVENT")
})
