# Coverage of the edge paths of R/ard_tables.R: pull_ard() on an ARD with no
# keys, normalize_ard()'s overall label on a factor hierarchy key, widen_ard()
# with per-variable labels and a `.label` order, notes = "applied", the
# comparison sort, plan_template(form = "widen") and records plans.

cov_ard_adsl <- function() {
  data.frame(USUBJID = sprintf("S%02d", 1:12), TRT = rep(c("A", "B"), 6),
             SEX = rep(c("F", "M", "F"), 4), stringsAsFactors = FALSE)
}

# a long frame of two parameters and a t-test p-value per parameter
cov_ard_cmp <- function() {
  d <- data.frame(TRT = rep(c("A", "B"), each = 6),
                  PARAM = rep(rep(c("ALT", "AST"), each = 3), 2),
                  stat_name = rep(c("n", "mean", "sd"), 4),
                  stat = c(20, 31.2, 4.1, 20, 28.7, 3.9, 18, 33.1, 5.3, 18, 30.2, 4.4),
                  stringsAsFactors = FALSE)
  d$variable <- d$PARAM
  d$context <- "summary"
  cmp <- data.frame(TRT = NA, PARAM = c("ALT", "AST"), stat_name = "p.value",
                    stat = c(0.0004, 0.123), variable = c("ALT", "AST"),
                    context = "stats_t_test", stringsAsFactors = FALSE)
  d <- rbind(d, cmp)
  d$.kind <- "continuous"
  d$CMP <- NA_character_
  d$CMP[d$context == "stats_t_test"] <- "A vs B"
  d
}

cov_ard_cmp_plan <- function() {
  table_plan(cov_ard_cmp(), cols = "TRT", rows = c(param = "variable"),
             label = c(row = "stat_name")) |>
    plan_cells(c("n" = "{n:.0f}", "Mean (SD)" = "{mean:.1f} ({sd:.1f})"), notes = FALSE) |>
    plan_compare(p = "{p.value:.3f}", by = "CMP")
}

test_that("pull_ard() names '(none)' when the ARD has no keys", {
  skip_on_cran()
  skip_if_not_installed("cards")
  ard <- cards::ard_tabulate(cov_ard_adsl(), variables = SEX)
  expect_error(pull_ard(ard[0L, ], cols = "TRT"),
               "`cols`: no key .TRT. in this ARD.  It has: \\(none\\)")
})

test_that("normalize_ard() puts the overall label first in a factor hierarchy key's levels", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adsl <- cov_ard_adsl()
  adae <- data.frame(USUBJID = c("S01", "S02", "S03", "S04", "S05"),
                     AESOC = factor(c("Soc A", "Soc B", "Soc A", "Soc B", "Soc A"),
                                    levels = c("Soc B", "Soc A")),
                     AETERM = c("Term 1", "Term 2", "Term 3", "Term 2", "Term 1"),
                     stringsAsFactors = FALSE)
  adae$TRT <- adsl$TRT[match(adae$USUBJID, adsl$USUBJID)]
  ard <- cards::ard_stack_hierarchical(adae, variables = c(AESOC, AETERM), by = TRT,
                                       denominator = adsl, id = USUBJID,
                                       over_variables = TRUE)
  d <- normalize_ard(ard, hierarchy = c("AESOC", "AETERM"), overall = "Any event")
  expect_true(is.factor(d$AESOC))
  expect_identical(levels(d$AESOC), c("Any event", "Soc B", "Soc A"))
  expect_true(all(as.character(d$AESOC[d$.overall]) == "Any event"))
})

test_that("widen_ard() recodes a `.label` order through a variable's own labels", {
  skip_on_cran()
  skip_if_not_installed("cards")
  ard <- cards::ard_stack(cov_ard_adsl(), .by = TRT,
                          cards::ard_tabulate(variables = SEX, statistic = ~ c("n", "p")))
  w <- widen_ard(normalize_ard(ard), cols = "TRT",
                 labels = list(SEX = c(F = "Female", M = "Male")),
                 levels = list(.label = c("M", "F")),
                 cells = list(categorical = "{n}"), notes = FALSE)
  expect_identical(as.character(w$label), c("Male", "Female"))
  expect_identical(w$A, c("2", "4"))
  expect_identical(w$B, c("2", "4"))
})

test_that("widen_ard(notes = \"applied\") lists twelve templates and counts the rest", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:14), TRT = rep(c("A", "B"), 7),
                     GRADE = sprintf("G%02d", 1:14), stringsAsFactors = FALSE)
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_tabulate(variables = GRADE, statistic = ~ "n"))
  msgs <- character()
  w <- withCallingHandlers(
    widen_ard(normalize_ard(ard), cols = "TRT", cells = list(categorical = "{n}"),
              notes = "applied"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  expect_identical(nrow(w), 14L)
  expect_true(any(grepl("14 templates produced 28 cells.", msgs, fixed = TRUE)))
  expect_true(any(grepl("... and 2 more", msgs, fixed = TRUE)))
})

test_that("widen_ard(notes = \"applied\") says nothing of templates when statistics are rows", {
  skip_on_cran()
  d <- data.frame(TRT = c("A", "B", "A", "B"), variable = "AGE", context = "summary",
                  stat_name = c("N", "N", "mean", "mean"), stat_label = c("N", "N", "Mean", "Mean"),
                  stat = c(6, 6, 41.5, 47.25), stringsAsFactors = FALSE)
  msgs <- character()
  w <- withCallingHandlers(
    widen_ard(normalize_ard(d), cols = "TRT", stats = "rows", notes = "applied"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  expect_identical(as.character(w$label), c("N", "Mean"))
  expect_false(any(grepl("template", msgs)))
})

test_that(".ard_applied_message() is silent for an empty long frame", {
  skip_on_cran()
  # internal: widen_ard() stops ("No cell was produced") before an empty long
  # frame could reach the message, so the guard is called directly
  expect_silent(res <- rtfreporter:::.ard_applied_message(NULL, "widen_ard()"))
  expect_false(res)
  long <- data.frame(.var = character(), .tpl = character(), stringsAsFactors = FALSE)
  expect_false(rtfreporter:::.ard_applied_message(long, "widen_ard()"))
})

test_that(".ard_chain_value() returns the value of the first template that fills", {
  skip_on_cran()
  # internal: no caller left in the package (widen_ard() uses .ard_chain_pick())
  s <- data.frame(stat_name = c("n", "p"), stat = c(3, 0.25), stat_fmt = c("3", "25.0"),
                  stringsAsFactors = FALSE)
  chain <- list(list(tpl = "{zz}"), list(tpl = "{n}"))
  expect_identical(rtfreporter:::.ard_chain_value(chain, s, "sas"), "3")
})

test_that("normalize_ard() keeps a character statistic that is not a number as text", {
  skip_on_cran()
  d <- data.frame(TRT = c("A", "B"), variable = "METHOD", context = "summary",
                  stat_name = "method", stat = c("Exact", "Asymptotic"),
                  stringsAsFactors = FALSE)
  n <- normalize_ard(d)
  expect_identical(n$stat, c("Exact", "Asymptotic"))
  d$stat <- c("1.5", "2.5")
  expect_identical(normalize_ard(d)$stat, c(1.5, 2.5))
})

test_that("a records plan stops on two list-column values in one cell", {
  skip_on_cran()
  rec <- data.frame(USUBJID = c("S1", "S1", "S2"), VIS = c("W1", "W1", "W1"),
                    stringsAsFactors = FALSE)
  rec$AVAL <- I(list(1, 2, 3))
  p <- table_plan(rec, cols = "VIS", rows = c(subject = "USUBJID"), value = "AVAL") |>
    plan_cells("{value}")
  err <- expect_error(plan_apply(p, "table"), "Two rows reach one cell")
  expect_match(conditionMessage(err), "values: '1', '2'|values: \u20181\u2019, \u20182\u2019")
})

test_that("plan_sort() orders the rows by a named comparison's statistic", {
  skip_on_cran()
  t <- as.data.frame(plan_apply(cov_ard_cmp_plan() |> plan_sort("-p.value[A vs B]"), "table"))
  expect_identical(t$param[1:2], c("AST", "ALT"))
  expect_identical(t[["A vs B"]][1:2], c("0.123", "0.000"))
})

test_that("plan_sort() stops on a comparison name plan_compare(by = ) has not", {
  skip_on_cran()
  expect_error(plan_apply(cov_ard_cmp_plan() |> plan_sort("-p.value[A vs Z]"), "table"),
               "no comparison 'A vs Z'.\n  Comparisons: .A vs B.")
})

test_that("plan_sort() carries a comparison to its row key's rows when there is no hierarchy", {
  skip_on_cran()
  t <- as.data.frame(plan_apply(cov_ard_cmp_plan() |>
                                  plan_sort("-p.value@param[A vs B]"), "table"))
  # every row of AST takes AST's p-value, so AST's block sorts first, whole
  expect_identical(t$param, c("AST", "AST", "ALT", "ALT"))
  expect_setequal(t$row[1:2], c("n", "Mean (SD)"))
})

test_that("plan_sort() carries a statistic to its row key's rows of a flat ARD (no depth)", {
  skip_on_cran()
  skip_if_not_installed("cards")
  adsl <- cov_ard_adsl()
  adsl$RACE <- rep(c("R1", "R2", "R2", "R2"), 3)
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_tabulate(variables = c(SEX, RACE), statistic = ~ "n"))
  p <- table_plan(normalize_ard(ard), cols = "TRT", rows = c(grp = "variable")) |>
    plan_cells("{n}", notes = FALSE)
  # the block's first row is its own: SEX's F (8 in all), RACE's R1 (3)
  t <- as.data.frame(plan_apply(p |> plan_sort("n@grp"), "table"))
  expect_identical(as.character(t$grp), c("RACE", "RACE", "SEX", "SEX"))
  expect_identical(as.character(t$label), c("R1", "R2", "F", "M"))
})

test_that("plan_template(form = \"widen\") notes a guessed `cols` and writes no magrittr line for |>", {
  skip_on_cran()
  skip_if_not_installed("cards")
  ard <- cards::ard_stack(cov_ard_adsl(), .by = TRT,
                          cards::ard_tabulate(variables = SEX, statistic = ~ "n"))
  code <- plan_template(ard, form = "widen", pipe = "|>")
  expect_true(any(code == "#  NOTE: `cols` was not given; the first key is used.  Check it."))
  expect_false(any(grepl("library(magrittr)", code, fixed = TRUE)))
  expect_true(any(code == "tbl_df <- ard |>"))
  expect_true(any(code == "    cols  = \"TRT\","))
})

test_that("plan_template(form = \"widen\") writes {n} for a tabulation with counts only", {
  skip_on_cran()
  skip_if_not_installed("cards")
  ard <- cards::ard_tabulate(cov_ard_adsl(), variables = SEX, statistic = ~ "n")
  code <- plan_template(ard, form = "widen", cols = "SEX", pipe = "%>%")
  expect_true(any(code == "      categorical = \"{n:.0f}\""))
  expect_true(any(grepl("library(magrittr)", code, fixed = TRUE)))
})

test_that("plan_template(form = \"widen\") writes the first statistic when there is no n", {
  skip_on_cran()
  skip_if_not_installed("cards")
  ard <- cards::ard_tabulate(cov_ard_adsl(), variables = SEX, statistic = ~ "N")
  code <- plan_template(ard, form = "widen", cols = "SEX", pipe = "%>%")
  expect_true(any(code == "      categorical = \"{N}\""))
})

test_that(".ard_pipe_rstudio() maps RStudio's native-pipe preference to an operator", {
  skip_on_cran()
  # internal: the preference is read only inside RStudio, so the mapping is
  # tested on its own (the split exists for this)
  expect_identical(rtfreporter:::.ard_pipe_rstudio(TRUE), "|>")
  expect_identical(rtfreporter:::.ard_pipe_rstudio(FALSE), "%>%")
  expect_null(rtfreporter:::.ard_pipe_rstudio(NA))
  expect_null(rtfreporter:::.ard_pipe_rstudio(NULL))
})
