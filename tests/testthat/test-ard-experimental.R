# Tests for the EXPERIMENTAL cards/cardx ARD helpers (issue #474).
# This whole file belongs to R/ard-experimental.R and is deleted with it.

skip_if_no_cards <- function() {
  testthat::skip_if_not_installed("cards")
}

make_ard <- function() {
  adsl <- cards::ADSL
  adsl$AGEGR <- as.character(cut(adsl$AGE, c(0, 64, 74, 200),
                                 labels = c("<65", "65-74", ">=75")))
  adsl$SEX <- as.character(adsl$SEX)
  adsl$TRT <- as.character(adsl$ARM)
  cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_continuous(
      variables = AGE,
      statistic = ~ cards::continuous_summary_fns(
        c("N", "mean", "sd", "median", "min", "max"))),
    cards::ard_categorical(variables = c(AGEGR, SEX),
                           statistic = ~ c("n", "p")),
    .total_n = TRUE)
}

# ard_table() was withdrawn (#474): the one entry point is
# ard_normalize() |> ard_spread().  These tests were written against the
# collapsed call, so this helper does the split, routing each argument to the
# step that owns it -- which also keeps them honest about where each belongs.
ard_pipe <- function(ard, ...) {
  args <- list(...)
  keep <- intersect(names(args), setdiff(names(formals(ard_normalize)), "ard"))
  x <- do.call(ard_normalize, c(list(ard = ard), args[keep]))
  do.call(ard_spread, c(list(x = x), args[setdiff(names(args), keep)]))
}

# ------------------------------------------------------------ ard_normalize

test_that("ard_normalize() keys the group pairs by name and flattens list-cols", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())

  expect_true(is.data.frame(d))
  expect_true("TRT" %in% names(d))
  expect_false(any(vapply(d, is.list, logical(1))))
  expect_setequal(stats::na.omit(unique(d$TRT)),
                  c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
  # attributes / total_n rows are gone; the by-variable's own counts stay,
  # marked, because a column header reads them
  expect_false(any(d$context %in% c("attributes", "total_n")))
  expect_true(all(d$.key_own[d$variable %in% "TRT"]))
  expect_false(any(d$.key_own[!d$variable %in% "TRT"]))
  # cards' own formatter came through
  expect_true("stat_fmt" %in% names(d))
  expect_true(any(!is.na(d$stat_fmt)))
})

test_that("ard_normalize() reads nothing from the object's attributes", {
  skip_if_no_cards()
  ard <- make_ard()
  stripped <- as.data.frame(ard)
  for (a in setdiff(names(attributes(stripped)), c("names", "row.names", "class")))
    attr(stripped, a) <- NULL
  class(stripped) <- "data.frame"
  expect_null(attr(stripped, "args", exact = TRUE))
  # identical output, including stat_fmt: with cards' class gone the package
  # applies the ARD's own `fmt_fun` column itself
  expect_equal(ard_normalize(stripped), ard_normalize(ard))
})

test_that("ard_normalize() folds a hierarchy and records the depth", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adae <- merge(cards::ADAE[, c("USUBJID", "AESOC", "AETERM")],
                adsl[, c("USUBJID", "TRT")], by = "USUBJID")
  ard <- cards::ard_stack_hierarchical(
    adae, variables = c(AESOC, AETERM), by = TRT,
    denominator = adsl, id = USUBJID)

  d <- ard_normalize(ard, hierarchy = c("AESOC", "AETERM"))
  expect_true(all(c("AESOC", "AETERM", ".depth", ".label") %in% names(d)))
  # a SOC summary row: AESOC filled from variable_level, AETERM still missing
  soc <- d[d$variable == "AESOC", , drop = FALSE]
  expect_true(all(!is.na(soc$AESOC)))
  expect_true(all(is.na(soc$AETERM)))
  expect_true(all(soc$.depth == 1L))
  pt <- d[d$variable == "AETERM", , drop = FALSE]
  expect_true(all(pt$.depth == 2L))
  expect_identical(pt$.label, pt$AETERM)
})

test_that("ard_normalize() labels the hierarchical overall rows on request", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adae <- merge(cards::ADAE[, c("USUBJID", "AESOC", "AETERM")],
                adsl[, c("USUBJID", "TRT")], by = "USUBJID")
  ard <- cards::ard_stack_hierarchical(
    adae, variables = c(AESOC, AETERM), by = TRT,
    denominator = adsl, id = USUBJID, over_variables = TRUE)

  d <- ard_normalize(ard, hierarchy = c("AESOC", "AETERM"),
                     overall = "Any TEAE")
  expect_true(any(d$AESOC == "Any TEAE", na.rm = TRUE))
  expect_true(all(is.na(d$AETERM[d$.overall])))
  # without `overall` the sentinel rows are dropped rather than left dangling
  d2 <- ard_normalize(ard, hierarchy = c("AESOC", "AETERM"))
  expect_false(any(d2$variable == "..ard_hierarchical_overall.."))
})

# ------------------------------------- ard_normalize() |> ard_spread()

test_that("the pipe builds the demographics shape", {
  skip_if_no_cards()
  tbl <- ard_pipe(
    make_ard(),
    cols   = "TRT",
    rows   = c(group = "variable"),
    labels = c(AGE = "Age (years)", AGEGR = "Age group", SEX = "Sex"),
    cells  = list(
      continuous  = c("n"         = "{N:.0f}",
                      "Mean (SD)" = "{mean:.1f} ({sd:.2f})",
                      "Min, Max"  = "{min:.1f}, {max:.1f}"),
      categorical = "{n:.0f} ({p:.1f%})"))

  expect_identical(names(tbl)[1:2], c("group", "label"))
  expect_setequal(names(tbl)[-(1:2)],
                  c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
  # `labels` recoded the values AND fixed their order
  expect_identical(as.character(unique(tbl$group)),
                   c("Age (years)", "Age group", "Sex"))
  age <- tbl[tbl$group == "Age (years)", ]
  expect_identical(age$label, c("n", "Mean (SD)", "Min, Max"))
  expect_match(age$Placebo[2], "^[0-9]+[.][0-9] [(][0-9]+[.][0-9]{2}[)]$")
  # a percentage token scales to 0-100
  sex <- tbl[tbl$group == "Sex", ]
  expect_match(sex$Placebo[1], "^[0-9]+ [(][0-9]+[.][0-9][)]$")
})

test_that("several column keys make one spanning-ready column name", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$SEX <- as.character(adsl$SEX)
  adsl$AGEGR <- as.character(cut(adsl$AGE, c(0, 74, 200),
                                 labels = c("<75", ">=75")))
  ard <- cards::ard_stack(adsl, .by = c(TRT, SEX),
                          cards::ard_categorical(variables = AGEGR,
                                                 statistic = ~ c("n", "p")))
  tbl <- ard_pipe(ard, cols = c("TRT", "SEX"), rows = c(group = "variable"),
                   cells = "{n:.0f} ({p:.1f%})")
  expect_true(all(grepl("____", names(tbl)[-(1:2)])))
  expect_true("Placebo____F" %in% names(tbl))
  # column order follows `levels` when given
  tbl2 <- ard_pipe(ard, cols = c("TRT", "SEX"), rows = c(group = "variable"),
                    cells = "{n:.0f} ({p:.1f%})",
                    levels = list(SEX = c("M", "F")))
  expect_identical(names(tbl2)[3:4], c("Placebo____M", "Placebo____F"))
})

test_that("levels may name the analysis variables, not the label column", {
  skip_if_no_cards()
  cells <- list(
    continuous  = c("n"         = "{N:.0f}",
                    "Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
    categorical = "{n:.0f} ({p:.1f%})")
  labels <- c(AGE = "Age", AGEGR = "Age group", SEX = "Sex")

  tbl <- ard_pipe(
    make_ard(), cols = "TRT", rows = c(group = "variable"),
    labels = labels, cells = cells,
    levels = list(TRT   = c("Xanomeline Low Dose", "Placebo",
                            "Xanomeline High Dose"),
                  AGEGR = c("<65", "65-74", ">=75"),
                  SEX   = c("M", "F")))

  # a column key entry orders the SPREAD columns ...
  expect_identical(names(tbl)[-(1:2)],
                   c("Xanomeline Low Dose", "Placebo", "Xanomeline High Dose"))
  # ... and a variable entry orders that variable's rows
  expect_identical(as.character(tbl$label[tbl$group == "Age group"]),
                   c("<65", "65-74", ">=75"))
  expect_identical(as.character(tbl$label[tbl$group == "Sex"]), c("M", "F"))
  # a variable with no entry keeps its templates' order
  expect_identical(as.character(tbl$label[tbl$group == "Age"]),
                   c("n", "Mean (SD)"))
  # and the variables themselves stay in `labels` order
  expect_identical(as.character(unique(tbl$group)),
                   c("Age", "Age group", "Sex"))
})

test_that("an unnamed labels / levels element is refused, not ignored", {
  skip_if_no_cards()
  ard <- make_ard()
  run <- function(...) {
    ard_pipe(ard, cols = "TRT", rows = c(group = "variable"),
              cells = "{n:.0f} ({p:.1f%})", ...)
  }
  # the two-parallel-vector idiom with vectors of different lengths: setNames()
  # gives the surplus label an NA name instead of complaining
  vars   <- c("AGE", "AGEGR")
  labs   <- c("Age", "Age group", "Sex")
  broken <- stats::setNames(labs, vars)
  expect_true(any(is.na(names(broken))))
  expect_error(run(labels = broken), "must name every element")

  expect_error(run(labels = c("Age", "Age group")), "must name every element")
  expect_error(run(levels = list(c("M", "F"))), "must name every element")
  expect_error(run(labels = c(AGE = "Age", AGE = "Age again")),
               "names must be unique")
  # the correct spelling still works
  expect_s3_class(run(labels = stats::setNames(labs[1:2], vars)), "data.frame")
})

test_that("a fallback chain picks the first template that resolves", {
  skip_if_no_cards()
  tbl <- ard_pipe(make_ard(), cols = "TRT", rows = c(group = "variable"),
                   cells = c("{n:.0f} ({p:.1f%})", "{mean:.1f}"))
  age <- tbl[tbl$group == "AGE", ]
  # AGE has no n/p, so the chain falls through to the mean
  expect_match(age$Placebo[1], "^[0-9]+[.][0-9]$")
  sex <- tbl[tbl$group == "SEX", ]
  expect_match(sex$Placebo[1], "[(]")
})

test_that("cells match whichever cards verb generation built the ARD", {
  skip_if_no_cards()
  skip_if_not(all(c("ard_summary", "ard_tabulate") %in%
                    getNamespaceExports("cards")))
  adsl <- cards::ADSL
  adsl$SEX <- as.character(adsl$SEX)
  adsl$TRT <- as.character(adsl$ARM)

  # cards 0.9 renamed the verbs, and the new ones stamp a different `context`
  new_ard <- cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_summary(variables = AGE),
    cards::ard_tabulate(variables = SEX, statistic = ~ c("n", "p")))
  old_ard <- cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_continuous(variables = AGE),
    cards::ard_categorical(variables = SEX, statistic = ~ c("n", "p")))
  expect_true(all(c("summary", "tabulate") %in% new_ard$context))
  expect_true(all(c("continuous", "categorical") %in% old_ard$context))

  cells_old <- list(continuous  = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
                    categorical = "{n:.0f} ({p:.1f%})")
  cells_new <- list(summary  = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
                    tabulate = "{n:.0f} ({p:.1f%})")
  run <- function(ard, cells) {
    ard_pipe(ard, cols = "TRT", rows = c(group = "variable"), cells = cells)
  }
  # either spelling of `cells` against either spelling of ARD
  expect_equal(run(new_ard, cells_old), run(old_ard, cells_old))
  expect_equal(run(new_ard, cells_new), run(old_ard, cells_old))
  expect_equal(run(old_ard, cells_new), run(old_ard, cells_old))
  expect_match(run(new_ard, cells_old)$Placebo[1],
               "^[0-9]+[.][0-9] [(][0-9]+[.][0-9]{2}[)]$")
})

test_that("cells match an ARD whose context cards has never used", {
  skip_if_no_cards()
  ard <- make_ard()
  # stand-in for a future cards rename: same rows, a context string that
  # matches neither the pre-0.9 nor the 0.9 spelling
  future <- as.data.frame(ard)
  future$context <- ifelse(future$context == "continuous", "univariate_stats",
                    ifelse(future$context == "categorical", "freq_table",
                           future$context))
  cells <- list(continuous  = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
                categorical = "{n:.0f} ({p:.1f%})")
  run <- function(a) {
    ard_pipe(a, cols = "TRT", rows = c(group = "variable"), cells = cells)
  }
  expect_equal(run(future), run(ard))
  expect_match(run(future)$Placebo[1],
               "^[0-9]+[.][0-9] [(][0-9]+[.][0-9]{2}[)]$")
})

test_that("the structural kind is read from the rows, not from the context", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  expect_true(".kind" %in% names(d))
  # a variable with levels to enumerate is categorical; one without is not
  expect_identical(unique(d$.kind[d$variable == "AGE"]), "continuous")
  expect_identical(unique(d$.kind[d$variable == "SEX"]), "categorical")
  expect_identical(unique(d$.kind[d$variable == "AGEGR"]), "categorical")
  # and it survives a context the package has never seen
  scrambled <- as.data.frame(make_ard())
  scrambled$context[!scrambled$context %in% c("attributes", "total_n")] <-
    "who knows"
  d2 <- ard_normalize(scrambled)
  expect_identical(d2$.kind, d$.kind)
})

test_that("ard_keys() reports the stable kind next to the context", {
  skip_if_no_cards()
  out <- utils::capture.output(k <- ard_keys(make_ard()))
  expect_true(any(grepl("Structural kinds", out)))
  expect_false(is.null(k$kinds))
  expect_identical(k$kinds$kind[k$kinds$variable == "AGE"], "continuous")
  expect_identical(k$kinds$kind[k$kinds$variable == "SEX"], "categorical")
})

test_that("a `cells` list that matches nothing says what the ARD holds", {
  skip_if_no_cards()
  err <- tryCatch(
    ard_pipe(make_ard(), cols = "TRT", rows = c(group = "variable"),
              cells = list(nonsense = "{n} ({p})")),
    error = function(e) conditionMessage(e))
  expect_match(err, "No cell was produced")
  expect_match(err, "cells` is keyed on")
  expect_match(err, "nonsense")
  expect_match(err, "ARD contexts")
  expect_match(err, "continuous")          # what the ARD actually carries
  expect_match(err, "structural kind")     # the stable key, named
})

# --------------------------------------------- positional vs named key ---

test_that("a positional key warns when that position holds several variables", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT    <- as.character(adsl$ARM)
  adsl$SEX    <- as.character(adsl$SEX)
  adsl$AGEGR  <- as.character(cut(adsl$AGE, c(0, 74, 200),
                                  labels = c("<75", ">=75")))
  # stacked: TRT is group1 in one block and group2 in the other
  stacked <- cards::bind_ard(
    cards::ard_tabulate(adsl, by = TRT, variables = AGEGR,
                        statistic = ~ c("n", "p")),
    cards::ard_tabulate(adsl, by = c(SEX, TRT), variables = AGEGR,
                        statistic = ~ c("n", "p")))

  expect_warning(
    bad <- ard_pipe(stacked, cols = "group1_level",
                     rows = c(group = "variable"),
                     cells = "{n:.0f} ({p:.1f%})"),
    "reads a POSITION")
  # the warning is earned: the position invented columns from the other block
  expect_true(all(c("F", "M") %in% names(bad)))

  # naming the variable is correct and silent
  expect_no_warning(
    good <- ard_pipe(stacked, cols = "TRT", rows = c(group = "variable"),
                      cells = "{n:.0f} ({p:.1f%})", notes = FALSE))
  expect_setequal(names(good)[-(1:2)],
                  c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))

  # a single-block ARD keeps quiet
  expect_no_warning(ard_pipe(make_ard(), cols = "group1_level",
                              rows = c(group = "variable"),
                              cells = "{n:.0f} ({p:.1f%})", notes = FALSE))
})

test_that("reading a position deliberately, with its name column, is quiet", {
  skip_if_no_cards()
  # A subgroup table's rows ARE "which variable" by "which level of it", so
  # group2 holding many variables is the table rather than a mistake.  Only
  # taking the level WITHOUT the name is ambiguous.
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$SEX <- as.character(adsl$SEX)
  adsl$AGEGR <- as.character(cut(adsl$AGE, c(0, 74, 200),
                                 labels = c("<75", ">=75")))
  stacked <- cards::bind_ard(
    cards::ard_tabulate(adsl, by = TRT, variables = AGEGR,
                        statistic = ~ c("n", "p")),
    cards::ard_tabulate(adsl, by = c(SEX, TRT), variables = AGEGR,
                        statistic = ~ c("n", "p")))

  # the name column alone, or with its level: deliberate, so quiet
  expect_no_warning(ard_pipe(stacked, cols = "TRT", rows = c(v = "group1"),
                              cells = "{n:.0f} ({p:.1f%})", notes = FALSE))
  expect_no_warning(
    both <- ard_pipe(stacked, cols = "TRT",
                      rows = c(v = "group1", lv = "group1_level"),
                      cells = "{n:.0f} ({p:.1f%})", notes = FALSE))
  # and the pair really does identify the row
  expect_true(all(c("TRT", "SEX") %in% as.character(both$v)))

  # the level alone still warns, and says how to make it unambiguous
  expect_warning(
    ard_pipe(stacked, cols = "group1_level", rows = c(g = "variable"),
              cells = "{n:.0f} ({p:.1f%})", notes = FALSE),
    "reads a POSITION")
  expect_warning(
    ard_pipe(stacked, cols = "group1_level", rows = c(g = "variable"),
              cells = "{n:.0f} ({p:.1f%})", notes = FALSE),
    "variable column too")
})

# ------------------------------------------------------------- ard_pull ---

test_that("ard_pull() reads the header denominators, keyed like the columns", {
  skip_if_no_cards()
  n <- ard_pull(make_ard(), cols = "TRT")
  expect_named(n)
  expect_setequal(names(n), c("Placebo", "Xanomeline High Dose",
                              "Xanomeline Low Dose"))
  expect_equal(unname(n[["Placebo"]]), 86)
  expect_equal(sum(n), 254)

  # the order can be made to match the table's columns
  lv <- c("Xanomeline Low Dose", "Placebo", "Xanomeline High Dose")
  expect_identical(names(ard_pull(make_ard(), cols = "TRT",
                                  levels = list(TRT = lv))), lv)
})

test_that("ard_pull() keys a crossed header exactly like the spread columns", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT   <- as.character(adsl$ARM)
  adsl$SEX   <- as.character(adsl$SEX)
  adsl$AGEGR <- as.character(cut(adsl$AGE, c(0, 74, 200),
                                 labels = c("<75", ">=75")))
  ard <- cards::ard_stack(adsl, .by = c(TRT, SEX),
                          cards::ard_tabulate(variables = AGEGR,
                                              statistic = ~ c("n", "N", "p")))
  n   <- ard_pull(ard, cols = c("TRT", "SEX"))
  tbl <- ard_pipe(ard, cols = c("TRT", "SEX"), rows = c(group = "variable"),
                   cells = "{n:.0f} ({p:.1f%})")
  # every spread column has a denominator, under the identical name
  expect_true(all(names(tbl)[-(1:2)] %in% names(n)))
  expect_equal(sum(n), 254)
})

test_that("ard_pull() says what to do when the statistic is not there", {
  skip_if_no_cards()
  expect_error(ard_pull(make_ard(), cols = "NOPE"), "no key 'NOPE'")
  expect_error(ard_pull(make_ard(), cols = "TRT", stat = "nonesuch"),
               "No 'nonesuch' found")
})

# ------------------------------------ statistics the table never prints ---

test_that("statistics no template names are simply not read", {
  skip_if_no_cards()
  skip_if_not_installed("cardx")
  adsl <- cards::ADSL
  adsl$TRT  <- as.character(adsl$ARM)
  adsl$RESP <- adsl$AGE > 75
  ard <- cardx::ard_categorical_ci(adsl, variables = RESP, by = TRT,
                                   method = "wilson")

  # the ARD carries plenty the table will never show
  expect_true(all(c("method", "alternative", "conf.level", "p.value") %in%
                    ard$stat_name))

  # a NAMED character vector is one recipe of two rows, not a context map
  tbl <- ard_pipe(ard, cols = "TRT", rows = c(group = "variable"),
                   cells = c("n (%)"  = "{n:.0f} ({estimate:.1f%})",
                             "95% CI" = "{conf.low:.1f%}, {conf.high:.1f%}"))
  expect_identical(as.character(unique(tbl$label)), c("n (%)", "95% CI"))
  expect_match(tbl$Placebo[1], "^[0-9]+ [(][0-9.]+[)]$")
  expect_match(tbl$Placebo[2], "^[0-9.]+, [0-9.]+$")
  # nothing leaked in from method / alternative
  expect_false(any(grepl("Wilson|two.sided", unlist(tbl))))
})

# ------------------------------------------------- the `rows` default ----

test_that("`rows` defaults to the analysis variable for a flat ARD", {
  skip_if_no_cards()
  cells <- list(continuous  = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
                categorical = "{n:.0f} ({p:.1f%})")
  explicit <- ard_pipe(make_ard(), cols = "TRT",
                        rows = c(group = "variable"), cells = cells)
  omitted  <- ard_pipe(make_ard(), cols = "TRT", cells = cells)
  expect_equal(omitted, explicit)
  expect_identical(names(omitted)[1:2], c("group", "label"))
})

test_that("a single-variable ARD gets no grouping column by default", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$SEX <- as.character(adsl$SEX)
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_categorical(variables = SEX,
                                                 statistic = ~ c("n", "p")))
  tbl <- ard_pipe(ard, cols = "TRT", cells = "{n:.0f} ({p:.1f%})")
  expect_identical(names(tbl)[1], "label")
  expect_false("group" %in% names(tbl))
})

test_that("an explicit `rows` always wins over the default", {
  skip_if_no_cards()
  tbl <- ard_pipe(make_ard(), cols = "TRT",
                   rows = c(characteristic = "variable"),
                   cells = "{n:.0f} ({p:.1f%})")
  expect_identical(names(tbl)[1], "characteristic")
})

# ------------------------------------------------- the declarative sort ---

test_that("`sort` can be declared instead of arranged afterwards", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adae <- merge(cards::ADAE[, c("USUBJID", "AESOC", "AETERM")],
                adsl[, c("USUBJID", "TRT")], by = "USUBJID")
  ard <- cards::ard_stack_hierarchical(
    adae, variables = c(AESOC, AETERM), by = TRT,
    denominator = adsl, id = USUBJID, over_variables = TRUE)

  args <- list(ard = ard, cols = "TRT",
               hierarchy = c("AESOC", "AETERM"), overall = "Any TEAE",
               rows = c(soc = "AESOC"), label = c(term = "AETERM"),
               cells = "{n:.0f} ({p:.1f%})")

  # sorted by hand, the way the snippet does it
  by_hand <- do.call(ard_pipe, c(args, list(sort = FALSE, sort_stat = "n")))
  by_hand <- by_hand[order(by_hand$soc != "Any TEAE", by_hand$soc,
                           !is.na(by_hand$term), -by_hand$.sort_stat,
                           by_hand$term), ]
  by_hand$.sort_stat <- NULL
  rownames(by_hand) <- NULL

  declared <- do.call(ard_pipe,
    c(args, list(sort = c(".overall", "soc", ".depth", "-n", "term"))))
  expect_equal(declared, by_hand)

  # a level's own summary row comes before the rows nested under it
  first_soc <- declared[declared$soc == declared$soc[2], ]
  expect_true(is.na(first_soc$term[1]))
  expect_false(any(is.na(first_soc$term[-1])))
  # ... and the overall block is at the top
  expect_identical(declared$soc[1], "Any TEAE")
})

test_that("`sort` names a column, a statistic, or the computed keys", {
  skip_if_no_cards()
  run <- function(sort) {
    ard_pipe(make_ard(), cols = "TRT", rows = c(group = "variable"),
              cells = "{n:.0f} ({p:.1f%})", sort = sort)
  }
  # descending by a statistic's total across the columns
  desc <- run(c("group", "-n"))
  sex  <- desc[desc$group == "SEX", ]
  expect_identical(as.character(sex$label), c("F", "M"))   # 143 then 111
  asc  <- run(c("group", "n"))
  sex2 <- asc[asc$group == "SEX", ]
  expect_identical(as.character(sex2$label), c("M", "F"))

  expect_error(run(c("group", "nonesuch")),
               "neither a column of the result nor a statistic")
})

# ------------------------------------------------------ factor variables ---

make_factor_ard <- function() {
  adsl <- cards::ADSL
  adsl$TRT    <- as.character(adsl$ARM)
  adsl$AGEGR  <- cut(adsl$AGE, c(0, 64, 74, Inf),
                     labels = c("<65", "65-74", ">=75"))
  adsl$SEX    <- factor(adsl$SEX, levels = c("F", "M"),
                        labels = c("Female", "Male"))
  cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_continuous(variables = AGE,
                          statistic = ~ cards::continuous_summary_fns("mean")),
    cards::ard_categorical(variables = c(AGEGR, SEX),
                           statistic = ~ c("n", "p")))
}

test_that("a factor level keeps its label, not its integer code", {
  skip_if_no_cards()
  d <- ard_normalize(make_factor_ard())
  expect_setequal(stats::na.omit(unique(d$variable_level[d$variable == "AGEGR"])),
                  c("<65", "65-74", ">=75"))
  expect_setequal(stats::na.omit(unique(d$variable_level[d$variable == "SEX"])),
                  c("Female", "Male"))
  # nothing became "1" / "2" / "3"
  expect_false(any(stats::na.omit(d$variable_level) %in% c("1", "2", "3")))
})

test_that("a factor's declared level order becomes the row order", {
  skip_if_no_cards()
  tbl <- ard_pipe(make_factor_ard(), cols = "TRT",
                   cells = list(continuous  = c("Mean" = "{mean:.1f}"),
                                categorical = "{n:.0f} ({p:.1f%})"))
  gr <- tbl[tbl$group == "AGEGR", ]
  expect_identical(as.character(gr$label), c("<65", "65-74", ">=75"))
  sx <- tbl[tbl$group == "SEX", ]
  expect_identical(as.character(sx$label), c("Female", "Male"))

  # an explicit `levels` still wins over what the factor declared
  tbl2 <- ard_pipe(make_factor_ard(), cols = "TRT",
                    cells = list(continuous  = c("Mean" = "{mean:.1f}"),
                                 categorical = "{n:.0f} ({p:.1f%})"),
                    levels = list(SEX = c("Male", "Female")))
  sx2 <- tbl2[tbl2$group == "SEX", ]
  expect_identical(as.character(sx2$label), c("Male", "Female"))
})

test_that("the keyed columns are plain factors, not ordered ones", {
  skip_if_no_cards()
  tbl <- ard_pipe(make_factor_ard(), cols = "TRT",
                   cells = list(continuous  = c("Mean" = "{mean:.1f}"),
                                categorical = "{n:.0f} ({p:.1f%})"),
                   levels = list(group = c("SEX", "AGEGR", "AGE"),
                                 SEX   = c("Male", "Female")))
  # `levels` fixes the DISPLAY order, which is all the caller said.  An
  # ordered factor would go on to claim `Male > Female` and `SEX > AGEGR`.
  expect_s3_class(tbl$group, "factor")
  expect_s3_class(tbl$label, "factor")
  expect_false(is.ordered(tbl$group))
  expect_false(is.ordered(tbl$label))
  # and the order is exactly what it was when they were ordered factors
  expect_identical(as.character(unique(tbl$group)), c("SEX", "AGEGR", "AGE"))
  expect_identical(as.character(tbl$label[tbl$group == "SEX"]),
                   c("Male", "Female"))
  # the argument that used to ask for the ordered class is gone
  expect_false("ordered" %in% names(formals(ard_spread)))
})

# ------------------------------------------------- stat versus stat_fmt ---

test_that("stats = 'rows' can carry either of the ARD's two values", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_continuous(variables = AGE))
  args <- list(ard = ard, cols = "TRT", rows = c(group = "variable"),
               label = c(Statistic = "stat_label"), stats = "rows")

  raw <- do.call(ard_pipe, args)
  expect_true(is.numeric(raw$Placebo))
  expect_equal(raw$Placebo[raw$Statistic == "Mean"],
               mean(adsl$AGE[adsl$TRT == "Placebo"]))

  fmt <- do.call(ard_pipe, c(args, list(value = "stat_fmt")))
  expect_true(is.character(fmt$Placebo))
  # cards' own formatting, not ours
  expect_match(fmt$Placebo[fmt$Statistic == "Mean"], "^[0-9]+[.][0-9]$")
  expect_identical(fmt$Statistic, raw$Statistic)
})

test_that("{x:stat_fmt} and {x:stat} name the ARD's own columns", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_continuous(variables = AGE))
  tbl <- ard_pipe(ard, cols = "TRT", rows = c(group = "variable"),
                   cells = c("bare" = "{mean}",
                             "fmt"  = "{mean:stat_fmt}",
                             "raw"  = "{mean:stat}",
                             "ours" = "{mean:.3f}"))
  v <- function(lab) tbl$Placebo[tbl$label == lab]
  expect_identical(v("fmt"), v("bare"))       # bare prefers stat_fmt
  expect_false(identical(v("raw"), v("fmt"))) # stat is unrounded
  expect_match(v("ours"), "^[0-9]+[.][0-9]{3}$")
  # the old spellings say what to write instead
  expect_error(ard_pipe(ard, cols = "TRT", rows = c(group = "variable"),
                         cells = "{mean:raw}"), "write 'stat'")
  expect_error(ard_pipe(ard, cols = "TRT", rows = c(group = "variable"),
                         cells = "{mean:fmt}"), "write 'stat_fmt'")
})

test_that("a bare token falls back to stat, but stat_fmt demanded is an error", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_continuous(variables = AGE))
  ard$fmt_fun <- NULL                       # an ARD carrying no formatting
  d <- ard_normalize(ard)
  expect_true(all(is.na(d$stat_fmt)))
  bare <- ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                     cells = "{mean}", notes = FALSE)
  expect_false(is.na(bare$Placebo[1]))       # fell back to `stat`
  expect_error(ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                          cells = "{mean:stat_fmt}", notes = FALSE),
               "no `stat_fmt` value")
})

# ------------------------------------------------------- ard_overall(from) ---

make_bound_ae <- function() {
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adae <- merge(cards::ADAE[, c("USUBJID", "AESOC")],
                adsl[, c("USUBJID", "TRT")], by = "USUBJID")
  adae <- adae[adae$AESOC %in% c("CARDIAC DISORDERS",
                                 "GASTROINTESTINAL DISORDERS"), ]
  # the treatment IS the analysed variable in the overall block
  overall <- cards::ard_tabulate(adsl[adsl$USUBJID %in% adae$USUBJID, ],
                                 variables = TRT, denominator = adsl,
                                 statistic = ~ c("n", "p"))
  soc <- cards::ard_tabulate(unique(adae[, c("USUBJID", "TRT", "AESOC")]),
                             by = TRT, variables = AESOC,
                             denominator = adsl,
                             statistic = ~ c("n", "N", "p"))
  cards::bind_ard(overall, soc)
}

test_that("an overall block built by binding is found when named", {
  skip_if_no_cards()
  ard <- make_bound_ae()
  # no cards sentinel in an ARD built this way
  expect_false(any(ard$variable == "..ard_hierarchical_overall.."))
  expect_true(any(ard$variable == "TRT"))

  # unnamed, the block is a key variable's own tabulation and is dropped
  without <- ard_pipe(ard, cols = "TRT", hierarchy = "AESOC",
                       label = c(soc = "AESOC"), cells = "{n:.0f} ({p:.1f%})",
                       notes = FALSE)
  expect_false("Any TEAE" %in% as.character(without$soc))

  # named, it becomes the overall row, with the key read from variable_level
  with <- ard_pipe(ard, cols = "TRT", hierarchy = "AESOC",
                    overall = ard_overall("Any TEAE", from = "TRT"),
                    label = c(soc = "AESOC"), cells = "{n:.0f} ({p:.1f%})",
                    sort = c(".overall", "soc"), notes = FALSE)
  expect_identical(as.character(with$soc)[1], "Any TEAE")
  expect_setequal(names(with)[-1],
                  c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
  expect_false(anyNA(with$Placebo[1]))
})

test_that("a bare string still means the cards sentinel", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adae <- merge(cards::ADAE[, c("USUBJID", "AESOC", "AETERM")],
                adsl[, c("USUBJID", "TRT")], by = "USUBJID")
  ard <- cards::ard_stack_hierarchical(
    adae, variables = c(AESOC, AETERM), by = TRT,
    denominator = adsl, id = USUBJID, over_variables = TRUE)
  a <- ard_pipe(ard, cols = "TRT", hierarchy = c("AESOC", "AETERM"),
                 overall = "Any TEAE", rows = c(soc = "AESOC"),
                 label = c(term = "AETERM"), cells = "{n:.0f} ({p:.1f%})",
                 notes = FALSE)
  b <- ard_pipe(ard, cols = "TRT", hierarchy = c("AESOC", "AETERM"),
                 overall = ard_overall("Any TEAE"), rows = c(soc = "AESOC"),
                 label = c(term = "AETERM"), cells = "{n:.0f} ({p:.1f%})",
                 notes = FALSE)
  expect_equal(a, b)
  expect_true("Any TEAE" %in% as.character(a$soc))
})

test_that("ard_overall() refuses a missing label", {
  expect_error(ard_overall(), "`label` is required")
  expect_error(ard_overall(c("a", "b")), "one string")
})

# ------------------------------------------------------------ the notes ----

test_that("what was not used is reported, and not attached by default", {
  skip_if_no_cards()
  args <- list(make_ard(), cols = "TRT",
               cells = list(continuous  = c("Mean" = "{mean:.1f}"),
                            categorical = "{n:.0f} ({p:.1f%})"))

  expect_message(do.call(ard_pipe, args), "ARD rows? (were|was) not used")
  expect_message(do.call(ard_pipe, args), "no template named it")
  expect_message(do.call(ard_pipe, args), "a key variable's own tabulation")

  # silenced
  expect_no_message(do.call(ard_pipe, c(args, list(notes = FALSE))))

  # the result stays a plain data frame, so a comparison against the table the
  # caller built before is not disturbed by an extra attribute
  quiet <- do.call(ard_pipe, c(args, list(notes = FALSE)))
  expect_null(attr(quiet, "ard_ignored", exact = TRUE))
  expect_identical(class(quiet), "data.frame")

  # ... unless asked for
  kept <- suppressMessages(do.call(ard_pipe, c(args, list(notes = "attr"))))
  ig <- attr(kept, "ard_ignored", exact = TRUE)
  expect_s3_class(ig, "data.frame")
  expect_setequal(names(ig),
                  c("variable", "context", "stat_name", "rows", "reason"))
  expect_true(all(ig$rows > 0))
  # the statistics no template named are in there
  expect_true(any(ig$stat_name == "median" &
                    ig$reason == "no template named it"))
  # and the body is identical either way
  bare <- kept
  attr(bare, "ard_ignored") <- NULL
  expect_equal(quiet, bare)
})

test_that("the middle stage survives being rebuilt", {
  skip_if_no_cards()
  ard <- make_ard()
  cells <- list(continuous  = c("Mean" = "{mean:.1f}"),
                categorical = "{n:.0f} ({p:.1f%})")
  ref <- ard_pipe(ard, cols = "TRT", cells = cells, notes = FALSE)

  d <- ard_normalize(ard)
  # the attributes are conveniences, not requirements
  bare <- d
  for (a in c("ard_factor_levels", "ard_ignored", "ard_hierarchy")) {
    attr(bare, a) <- NULL
  }
  got <- ard_spread(bare, cols = "TRT", cells = cells, notes = FALSE)
  expect_identical(lapply(got, as.character), lapply(ref, as.character))

  # a one-pipe middle stage works, `mutate()` and all
  piped <- ard |>
    ard_normalize() |>
    dplyr::filter(!is.na(.data$stat)) |>
    dplyr::mutate(.marker = 1L) |>
    ard_spread(cols = "TRT", cells = cells, notes = FALSE)
  expect_identical(lapply(piped, as.character), lapply(ref, as.character))

  # ... and the label column's declared order survives it too, because the
  # order rides in the `.label_order` column rather than an attribute.
  fd <- ard_normalize(make_factor_ard())
  fc <- list(continuous = c("Mean" = "{mean:.1f}"), categorical = "{n:.0f}")
  expect_true(".label_order" %in% names(fd))
  expect_true(any(!is.na(fd$.label_order)))
  ref_lab <- ard_spread(fd, cols = "TRT", cells = fc, notes = FALSE)$label
  expect_s3_class(ref_lab, "factor")
  for (rebuilt in list(dplyr::mutate(fd, .marker = 1L),
                       dplyr::filter(fd, !is.na(.data$stat)),
                       fd[!is.na(fd$stat), , drop = FALSE])) {
    got <- ard_spread(rebuilt, cols = "TRT", cells = fc, notes = FALSE)$label
    expect_s3_class(got, "factor")
    expect_identical(levels(got), levels(ref_lab))
  }

  # a caller who relabels a level keeps that level's position
  indented <- dplyr::mutate(fd, .label = ifelse(.data$.label == "Female",
                                                "  Female", .data$.label))
  lab <- ard_spread(indented, cols = "TRT", cells = fc, notes = FALSE)$label
  expect_true("  Female" %in% levels(lab))
  expect_lt(match("  Female", levels(lab)), match("Male", levels(lab)))

  # the one attribute left is a report, and nothing reads it to build a table
  expect_identical(
    intersect(c("ard_factor_levels", "ard_hierarchy"), names(attributes(d))),
    character())
  expect_false(is.null(attr(d, "ard_ignored", exact = TRUE)))

  # "is there a hierarchy?" is a column question now, so it survives too
  expect_true(all(is.na(ard_normalize(ard)$.depth)))
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adae <- merge(cards::ADAE[, c("USUBJID", "AESOC")],
                adsl[, c("USUBJID", "TRT")], by = "USUBJID")
  h <- ard_normalize(
    cards::ard_stack_hierarchical(adae, variables = AESOC, by = TRT,
                                  denominator = adsl, id = USUBJID),
    hierarchy = "AESOC")
  expect_true(all(h$.depth[!h$.key_own] == 1L))
})

test_that("passing the raw ARD says so", {
  skip_if_no_cards()
  err <- tryCatch(ard_spread(cards::ADSL, cols = "ARM"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "does not look like an ard_normalize")
  expect_match(err, "Pass the ARD through")
  # but a rebuilt frame without the class is still accepted
  d <- ard_normalize(make_ard())
  class(d) <- "data.frame"
  expect_s3_class(ard_spread(d, cols = "TRT", cells = "{n:.0f} ({p:.1f%})",
                             notes = FALSE), "data.frame")
})

test_that("a template naming a missing statistic yields NA, not an error", {
  skip_if_no_cards()
  tbl <- ard_pipe(make_ard(), cols = "TRT", rows = c(group = "variable"),
                   cells = "{nope}")
  expect_true(all(is.na(tbl$Placebo)))
})

test_that("stats = 'rows' gives one numeric row per statistic", {
  skip_if_no_cards()
  # the shape this mode is for: continuous variables, one row per statistic
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_continuous(variables = AGE))
  tbl <- ard_pipe(ard, cols = "TRT", rows = c(group = "variable"),
                   label = c(Statistic = "stat_label"), stats = "rows")
  expect_true(is.numeric(tbl$Placebo))
  expect_true(all(c("N", "Mean", "SD") %in% as.character(tbl$Statistic)))
})

test_that("stats = 'rows' on a variable with levels needs the level as a key", {
  skip_if_no_cards()
  # every level of AGEGR carries an `n`, so labelling rows by the statistic
  # alone cannot separate them -- previously the last level silently won
  expect_error(
    ard_pipe(make_ard(), cols = "TRT", rows = c(group = "variable"),
              label = c(Statistic = "stat_label"), stats = "rows"),
    "telling themselves apart")

  # naming the level as well is what the message asks for, and it works
  tbl <- ard_pipe(make_ard(), cols = "TRT",
                   rows = c(group = "variable", level = "variable_level"),
                   label = c(Statistic = "stat_label"), stats = "rows")
  gr <- tbl[tbl$group == "AGEGR", ]
  expect_setequal(stats::na.omit(unique(gr$level)), c("<65", "65-74", ">=75"))
  expect_true(is.numeric(tbl$Placebo))
})

test_that("sort_stat totals a statistic across the spread columns", {
  skip_if_no_cards()
  tbl <- ard_pipe(make_ard(), cols = "TRT", rows = c(group = "variable"),
                   cells = "{n:.0f} ({p:.1f%})", sort_stat = "n")
  expect_true(".sort_stat" %in% names(tbl))
  sex <- tbl[tbl$group == "SEX", ]
  expect_equal(sum(sex$.sort_stat), 254)
})

test_that("rounding = 'r' reaches the cells", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  d <- d[d$variable == "AGE" & d$stat_name == "mean", ]
  d$stat <- 0.5
  sas <- ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                    cells = "{mean:.0f}", rounding = "sas")
  r   <- ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                    cells = "{mean:.0f}", rounding = "r")
  expect_identical(sas$Placebo, "1")
  expect_identical(r$Placebo, "0")
})

test_that("the pipe refuses an input that is not an ARD", {
  expect_error(ard_normalize(data.frame(a = 1)), "cards ARD")
  skip_if_no_cards()
  expect_error(ard_pipe(make_ard(), cols = "NOPE"), "no column 'NOPE'")
})

test_that("naming the analysed variable says where its levels went", {
  skip_if_no_cards()
  # `levels` keys on the analysis variable, `label` cannot -- so the error has
  # to explain the asymmetry rather than just listing the columns.
  expect_error(ard_pipe(make_ard(), cols = "TRT", label = c(row = "AGEGR")),
               "analysis variable")
  expect_error(ard_pipe(make_ard(), cols = "TRT", label = c(row = "AGEGR")),
               "[.]label")
  # a name that is neither a column nor an analysed variable keeps the
  # original message
  expect_error(ard_pipe(make_ard(), cols = "TRT", label = c(row = "NOPE")),
               "no column 'NOPE'")
})

# ----------------------------------------------------------------- the spec

dm_spec <- function(output_id = NA) {
  ard_spec(
    tables = data.frame(output_id = output_id, cols = "TRT",
                        rows = "group = variable", rounding = "sas",
                        stringsAsFactors = FALSE),
    variables = data.frame(
      output_id = output_id,
      variable = c("AGE", "AGEGR", "SEX"),
      label    = c("Age (years)", "Age group", "Sex"),
      order    = 1:3,
      levels   = c(NA, "<65 | 65-74 | >=75", NA),
      stringsAsFactors = FALSE),
    cells = data.frame(
      output_id = output_id,
      variable = c("AGE", "AGE", "categorical"),
      row      = c("n", "Mean (SD)", NA),
      template = c("{N}", "{mean} ({sd})", "{n} ({p})"),
      digits   = c("0", "1,2", NA),
      stringsAsFactors = FALSE))
}

test_that("a three-sheet spec supplies the roles as well as the cells", {
  skip_if_no_cards()
  sp <- dm_spec()
  expect_s3_class(sp, "ard_spec")
  expect_identical(names(sp), c("tables", "variables", "cells"))

  # no cols / rows in the call: the `tables` sheet says them
  tbl <- ard_spread(ard_normalize(make_ard()), spec = sp, notes = FALSE)
  expect_identical(as.character(unique(tbl$group)),
                   c("Age (years)", "Age group", "Sex"))
  age <- tbl[tbl$group == "Age (years)", ]
  expect_identical(as.character(age$label), c("n", "Mean (SD)"))
  # digits = "1,2" per token, with no inline spec in the template
  expect_match(age$Placebo[2], "^[0-9]+[.][0-9] [(][0-9]+[.][0-9]{2}[)]$")
  gr <- tbl[tbl$group == "Age group", ]
  expect_identical(as.character(gr$label), c("<65", "65-74", ">=75"))

  # the same table as the arguments written out
  ref <- ard_spread(ard_normalize(make_ard()), cols = "TRT",
                    rows = c(group = "variable"), rounding = "sas",
                    labels = c(AGE = "Age (years)", AGEGR = "Age group",
                               SEX = "Sex"),
                    levels = list(AGEGR = c("<65", "65-74", ">=75")),
                    cells = list(AGE = c("n" = "{N:.0f}",
                                         "Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
                                 categorical = "{n} ({p})"),
                    notes = FALSE)
  expect_equal(as.data.frame(tbl), as.data.frame(ref))
})

test_that("an argument given in the call wins over the spec", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  tbl <- ard_spread(d, spec = dm_spec(), rows = c(block = "variable"),
                    notes = FALSE)
  expect_true("block" %in% names(tbl))
  expect_false("group" %in% names(tbl))
})

test_that("the workbook round-trips, and nothing but a workbook is one", {
  skip_if_no_cards()
  sp <- dm_spec("DM")
  expect_error(write_ard_spec(sp, tempfile(fileext = ".csv")), ".xlsx workbook")
  expect_error(write_ard_spec(sp, tempfile()), ".xlsx workbook")
  expect_error(read_ard_spec("spec.csv"), ".xlsx workbook")

  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  f <- tempfile(fileext = ".xlsx")
  on.exit(unlink(f), add = TRUE)
  write_ard_spec(sp, f)
  expect_true("about" %in% readxl::excel_sheets(f))
  back <- read_ard_spec(f, output_id = "DM")
  expect_identical(attr(back, "output_id"), "DM")
  expect_equal(back$cells$template, sp$cells$template)
  expect_equal(back$variables$levels, sp$variables$levels)
  a <- ard_spread(ard_normalize(make_ard()), spec = f, notes = FALSE)
  b <- ard_spread(ard_normalize(make_ard()), spec = sp, notes = FALSE)
  expect_equal(a, b)
})

test_that("rows with the same key are one chain, and `when` guards one", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  d$stat[d$stat_name == "n" & d$variable == "SEX" & d$.label == "F"] <- 0
  sp <- ard_spec(
    tables = data.frame(cols = "TRT", rows = "group = variable"),
    cells = data.frame(variable = c("SEX", "SEX"),
                       when     = c("n == 0", NA),
                       template = c("none", "{n} ({p})")))
  tbl <- ard_spread(d, spec = sp, notes = FALSE)
  sex <- tbl[tbl$group == "SEX", ]
  expect_identical(unname(unlist(sex[sex$label == "F", "Placebo"])), "none")
  expect_false(any(sex$Placebo[sex$label == "M"] == "none"))
  expect_error(rtfreporter:::.ard_spec_cells(ard_spec(cells = data.frame(
    variable = "SEX", when = "n ==", template = "x"))), "not valid R")
})

test_that("quoted values in `rows` are constant headings; NA drops the label", {
  a <- rtfreporter:::.ard_spec_table_args(ard_spec(tables = data.frame(
    cols = "BASEGR", rows = 'LBTOX_LBL | group1 = "Worst Post-Baseline"',
    label = "NA", sort = ".overall | group1 | -n")))
  expect_identical(a$cols, "BASEGR")
  expect_true(is.list(a$rows))
  expect_identical(names(a$rows), c("", "group1"))
  expect_identical(a$rows[[1L]], "LBTOX_LBL")
  expect_s3_class(a$rows[[2L]], "formula")
  expect_true(is.na(a$label))
  expect_identical(a$sort, c(".overall", "group1", "-n"))
  b <- rtfreporter:::.ard_spec_table_args(ard_spec(tables = data.frame(
    cols = "TR01AG1 | SEROSTAT", label = "label = AEDECOD", sort = "false")))
  expect_identical(b$cols, c("TR01AG1", "SEROSTAT"))
  expect_identical(b$label, c(label = "AEDECOD"))
  expect_false(b$sort)
})

test_that("ard_spec() refuses what it would otherwise quietly ignore", {
  expect_error(ard_spec(tables = data.frame(cols = "TRT", colz = "x")),
               "does not read")
  # a column that belongs on another sheet says which
  expect_error(ard_spec(tables = data.frame(cols = "TRT", levels = "a | b")),
               "a `variables` column")
  expect_error(ard_spec(tables = data.frame(rounding = "banker")), "must be")
  expect_error(ard_spec(cells = data.frame(variable = "AGE", row = "n")),
               "no `template`")
  expect_error(ard_spec(tables = data.frame(output_id = c("T1", "T1"),
                                            cols = "TRT")), "two rows")
  expect_error(ard_spec(variables = data.frame(variable = c("AGE", "AGE"))),
               "two rows")
  # `note` is for people and always allowed
  expect_s3_class(ard_spec(tables = data.frame(cols = "TRT", note = "hi")),
                  "ard_spec")
  # the one-sheet layout names where its columns went
  expect_error(ard_spec(data.frame(variable = "AGE", template = "{mean}")),
               "one-sheet layout")
})

test_that("`cols` has to come from somewhere", {
  skip_if_no_cards()
  expect_error(ard_spread(ard_normalize(make_ard()),
                          spec = ard_spec(cells = data.frame(
                            variable = "AGE", template = "{mean}"))),
               "`cols` is required")
})

test_that("ard_spec_template() scaffolds the three sheets", {
  skip_if_no_cards()
  sp <- ard_spec_template(make_ard(), cols = "TRT", output_id = "DM")
  expect_s3_class(sp, "ard_spec")
  expect_identical(sp$tables$cols, "TRT")
  expect_identical(sp$tables$output_id, "DM")
  expect_true(all(c("AGE", "AGEGR", "SEX") %in% sp$variables$variable))
  expect_false("TRT" %in% sp$variables$variable)   # a key, not a variable
  age <- sp$cells[sp$cells$variable == "AGE", ]
  expect_true(all(c("n", "Mean (SD)", "Min, Max") %in% age$row))
  lv <- sp$variables$levels[sp$variables$variable == "AGEGR"]
  expect_setequal(strsplit(lv, " | ", fixed = TRUE)[[1]], c("<65", "65-74", ">=75"))
  # and it runs as written
  tbl <- ard_spread(ard_normalize(make_ard()), spec = sp, notes = FALSE)
  expect_true(all(c("Placebo") %in% names(tbl)))
})

# -------------------------------------------- which template made each cell

test_that("notes = 'applied' names the template and its guard", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  d$stat[d$stat_name == "n" & d$variable == "SEX"] <- 0
  expect_message(
    ard_spread(d, cols = "TRT", rows = c(group = "variable"),
               cells = c(n == 0 ~ "none", "{n:.0f}"), notes = "applied"),
    "templates produced")
  msgs <- capture_messages(
    ard_spread(d, cols = "TRT", rows = c(group = "variable"),
               cells = c(n == 0 ~ "none", "{n:.0f}"), notes = "applied"))
  joined <- paste(msgs, collapse = "")
  expect_match(joined, "when n == 0", fixed = TRUE)
  expect_match(joined, "none", fixed = TRUE)
  expect_match(joined, "{n:.0f}", fixed = TRUE)
})

test_that("an unguarded recipe reports its template with no guard", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  joined <- paste(capture_messages(
    ard_spread(d, cols = "TRT", rows = c(group = "variable"),
               cells = "{n:.0f}", notes = "applied")), collapse = "")
  expect_match(joined, "{n:.0f}", fixed = TRUE)
  expect_false(grepl("when", joined, fixed = TRUE))
})

test_that("notes = 'applied' stays quiet for stats = 'rows'", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  d <- d[d$variable == "AGE", , drop = FALSE]
  joined <- paste(capture_messages(
    ard_spread(d, cols = "TRT", rows = c(group = "variable"),
               stats = "rows", notes = "applied")), collapse = "")
  expect_false(grepl("templates produced", joined, fixed = TRUE))
})

test_that("the rounding family: argument > spec > option > R's own", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  d <- d[d$variable == "AGE" & d$stat_name == "mean", ]
  d$stat <- 0.25
  cell <- function(...) {
    ard_spread(d, cols = "TRT", rows = c(group = "variable"),
               cells = "{mean:.1f}", notes = FALSE, ...)$Placebo[1]
  }
  sp <- ard_spec(tables = data.frame(rounding = "sas"),
                 cells = data.frame(variable = "AGE", template = "{mean:.1f}"))

  expect_identical(cell(), "0.2")                         # R's own, the default
  expect_identical(cell(rounding = "sas"), "0.3")         # the argument
  expect_identical(cell(spec = sp), "0.3")                # the spec file
  expect_identical(cell(spec = sp, rounding = "r"), "0.2")# argument beats spec

  old <- options(rtfreporter.rounding = "sas")            # the package's one
  on.exit(options(old), add = TRUE)
  expect_identical(cell(), "0.3")                         # the option
  expect_identical(cell(rounding = "r"), "0.2")           # argument beats option
  options(old)
})

test_that("a named rounding is passed on and leaves the spec alone", {
  skip_if_no_cards()
  a <- make_ard()
  cell <- function(...) {
    ard_pipe(a, cols = "TRT", rows = c(group = "variable"),
              cells = "{mean:.0f}", notes = FALSE, ...)$Placebo[1]
  }
  # AGE's mean is not a tie here, so compare the two families on one that is
  d <- ard_normalize(a)
  d <- d[d$variable == "AGE" & d$stat_name == "mean", ]
  d$stat <- 0.5
  f <- function(...) ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                                cells = "{mean:.0f}", notes = FALSE, ...)$Placebo[1]
  expect_identical(f(rounding = "sas"), "1")
  expect_identical(f(rounding = "r"), "0")
  expect_type(cell(rounding = "sas"), "character")
})

test_that("a variable the hierarchy does not cover keeps its own level", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$SEX <- as.character(adsl$SEX)
  adsl$R1  <- rep(c("A", "B"), length.out = nrow(adsl))
  adsl$R2  <- ifelse(adsl$R1 == "A", rep(c("A1", "A2"), length.out = nrow(adsl)),
                     NA_character_)
  ard <- dplyr::bind_rows(
    cards::ard_categorical(adsl, by = TRT, variables = c(SEX, R1)),
    cards::ard_categorical(adsl[adsl$R1 == "A", ], by = c(TRT, R1),
                           variables = R2))
  d <- ard_normalize(ard, hierarchy = c("R1", "R2"))
  # the nested pair keeps its depths ...
  expect_setequal(unique(d$.depth[d$variable == "R1"]), 1L)
  expect_setequal(unique(d$.depth[d$variable == "R2"]), 2L)
  # ... and SEX, which the hierarchy says nothing about, is depth 0 but still
  # labelled: before, it came back NA and the caller had to normalize twice
  sex <- d[d$variable == "SEX", , drop = FALSE]
  expect_setequal(unique(sex$.depth), 0L)
  expect_false(any(is.na(sex$.label)))
  expect_setequal(unique(sex$.label), c("F", "M"))
})

test_that("ard_template() puts keys outside `cols` into `rows`", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$SEX <- as.character(adsl$SEX)
  adsl$GRP <- rep(c("X", "Y"), length.out = nrow(adsl))
  ard <- cards::ard_categorical(adsl, by = c(TRT, GRP), variables = SEX)
  code <- paste(capture.output(ard_template(ard, cols = "TRT")), collapse = "
")
  expect_match(code, 'rows  = c(GRP = "GRP")', fixed = TRUE)
  # and the generated call runs, keeping GRP as a column of the result
  txt <- ard_template(ard, cols = "TRT")
  e <- new.env(); assign("ard", ard, e)
  invisible(capture.output(
    eval(parse(text = paste(txt[!startsWith(txt, "#")], collapse = "
")), e)))
  expect_true("GRP" %in% names(get("tbl_df", e)))
})

test_that("ard_template() spells the hierarchical case so that it runs", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adae <- merge(cards::ADAE[, c("USUBJID", "AESOC", "AEDECOD")],
                adsl[, c("USUBJID", "TRT")], by = "USUBJID")
  ard <- cards::ard_stack_hierarchical(
    adae, variables = c(AESOC, AEDECOD), by = TRT, denominator = adsl,
    id = USUBJID, over_variables = TRUE)
  txt <- ard_template(ard, cols = "TRT", hierarchy = c("AESOC", "AEDECOD"))
  code <- paste(txt, collapse = "
")
  expect_match(code, 'hierarchy = c("AESOC", "AEDECOD")', fixed = TRUE)
  expect_match(code, 'label = c(label = "AEDECOD")', fixed = TRUE)
  expect_match(code, "overall", fixed = TRUE)     # the sentinel was noticed
  e <- new.env(); assign("ard", ard, e)
  invisible(capture.output(
    eval(parse(text = paste(txt[!startsWith(txt, "#")], collapse = "
")), e)))
  out <- get("tbl_df", e)
  expect_true(any(out[[1]] == "Any event"))       # the overall block is there
})

test_that("ard_template() takes its decimal places from the ARD", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  house <- cards::ard_continuous(
    adsl, by = TRT, variables = AGE,
    statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd")))
  study <- cards::ard_continuous(
    adsl, by = TRT, variables = AGE,
    statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd")),
    fmt_fun = AGE ~ list(mean = 2, sd = 3))
  h <- paste(capture.output(ard_template(house, cols = "TRT")), collapse = "")
  s <- paste(capture.output(ard_template(study, cols = "TRT")), collapse = "")
  expect_match(h, "{mean:.1f} ({sd:.1f})", fixed = TRUE)   # cards' own default
  expect_match(s, "{mean:.2f} ({sd:.3f})", fixed = TRUE)   # the study's
})

test_that("ard_template() offers the full row set and trims what is absent", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  full <- cards::ard_continuous(
    adsl, by = TRT, variables = AGE,
    statistic = ~ cards::continuous_summary_fns(
      c("N", "mean", "sd", "median", "p25", "p75", "min", "max")))
  thin <- cards::ard_continuous(
    adsl, by = TRT, variables = AGE,
    statistic = ~ cards::continuous_summary_fns(c("mean")))
  f <- paste(capture.output(ard_template(full, cols = "TRT")), collapse = "")
  t <- paste(capture.output(ard_template(thin, cols = "TRT")), collapse = "")
  expect_match(f, "Q1, Q3", fixed = TRUE)
  expect_match(f, "Min, Max", fixed = TRUE)
  expect_false(grepl("Q1, Q3", t, fixed = TRUE))   # no p25/p75 in this ARD
  expect_false(grepl("Min, Max", t, fixed = TRUE))
})

test_that("ard_template() writes a script that reaches rtftables", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$SEX <- as.character(adsl$SEX)
  ard <- cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_continuous(
      variables = AGE,
      statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
    cards::ard_categorical(variables = SEX), .total_n = TRUE)
  txt <- capture.output(ard_template(ard, cols = "TRT"))
  code <- paste(txt, collapse = "
")
  expect_match(code, "as_rtftables(", fixed = TRUE)
  expect_match(code, 'stub_vars  = c("group", "label")', fixed = TRUE)
  expect_match(code, "ard_pull(ard, cols =", fixed = TRUE)
  # the whole script runs, and ends in an rtftables object
  e <- new.env(); assign("ard", ard, e)
  suppressMessages(
    eval(parse(text = paste(txt[!startsWith(txt, "#")], collapse = "
")), e))
  expect_true(exists("pages", e))
  expect_gte(length(get("pages", e)), 1L)
})

test_that("with more than one column key the header is left to header_sep", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$SEX <- as.character(adsl$SEX)
  adsl$GRP <- rep(c("X", "Y"), length.out = nrow(adsl))
  ard <- cards::ard_categorical(adsl, by = c(TRT, GRP), variables = SEX)
  code <- paste(capture.output(
    ard_template(ard, cols = c("TRT", "GRP"))), collapse = "
")
  expect_match(code, "header_sep", fixed = TRUE)
  expect_false(grepl("col_header = col_header", code, fixed = TRUE))
})

test_that("label = NA separates the rows without printing them", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  d <- d[d$variable == "AGE", , drop = FALSE]
  cells <- c("1" = "{mean:.1f}", "2" = "{sd:.2f}")
  shown <- ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                      label = c(row = ".label"), cells = cells, notes = FALSE)
  hidden <- ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                       label = NA, cells = cells, notes = FALSE)
  expect_true("row" %in% names(shown))
  expect_false("row" %in% names(hidden))
  # the rows still tell themselves apart -- two of them, same values
  expect_identical(nrow(hidden), nrow(shown))
  expect_identical(hidden$Placebo, shown$Placebo)
  # and dropping the column outright still collides, which is why NA exists
  expect_error(ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                          label = NULL, cells = cells, notes = FALSE),
               "same cell")
})

test_that("a label template indents by rule instead of by paste0()", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  d <- d[d$variable == "AGEGR", , drop = FALSE]
  plain <- ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                      cells = "{n:.0f}", notes = FALSE)
  tpl <- ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                    label = c(.label == "65-74" ~ "  {.label}", ~ "{.label}"),
                    cells = "{n:.0f}", notes = FALSE)
  expect_true("  65-74" %in% as.character(tpl$label))
  expect_false("  65-74" %in% as.character(plain$label))
  # every other label is untouched, and the numbers do not move
  expect_setequal(sub("^ +", "", as.character(tpl$label)),
                  as.character(plain$label))
  expect_setequal(tpl$Placebo, plain$Placebo)
})

test_that("a rows template writes a constant heading without a mutate", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  z <- ard_spread(d, cols = "TRT",
                  rows = c(grp = ~ "Baseline Characteristics",
                           group = "variable"),
                  cells = "{n:.0f}", notes = FALSE)
  expect_true("grp" %in% names(z))
  expect_setequal(unique(as.character(z$grp)), "Baseline Characteristics")
  # a bare string still means a column, so nothing already written changes
  y <- ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                  cells = "{n:.0f}", notes = FALSE)
  expect_identical(z$Placebo, y$Placebo)
})

test_that("`labels` can be scoped to one column, like `levels`", {
  skip_if_no_cards()
  # the same value means two things on the two axes: a shift table's "0"
  # is "Grade 0" down the side and "Baseline 0" across the top
  set.seed(1)
  d <- data.frame(USUBJID = sprintf("S%03d", 1:60),
                  BASE = sample(c("0", "1", "2"), 60, TRUE),
                  POST = sample(c("0", "1", "2"), 60, TRUE),
                  stringsAsFactors = FALSE)
  ard <- cards::ard_stack(
    d, .by = BASE,
    cards::ard_categorical(variables = POST, statistic = ~ c("n")))

  one <- function(labels) {
    suppressMessages(
      ard |> ard_normalize() |>
        ard_spread(cols = "BASE", rows = c(WORST = "variable_level"),
                   label = NA, labels = labels, cells = "{n:d}"))
  }

  flat <- one(c("0" = "Grade 0", "1" = "Grade 1", "2" = "Grade 2"))
  # one dictionary recodes BOTH axes, which is what it has always done
  expect_identical(names(flat)[-1L],
                   c("Grade 0", "Grade 1", "Grade 2"))
  expect_identical(as.character(flat$WORST)[1L], "Grade 0")

  scoped <- one(list(
    BASE  = c("0" = "Baseline 0", "1" = "Baseline 1", "2" = "Baseline 2"),
    WORST = c("0" = "Grade 0", "1" = "Grade 1", "2" = "Grade 2")))
  expect_identical(names(scoped)[-1L],
                   c("Baseline 0", "Baseline 1", "Baseline 2"))
  expect_identical(as.character(scoped$WORST)[1L], "Grade 0")
  # the numbers are the same table either way
  expect_equal(unname(as.matrix(flat[-1L])),
               unname(as.matrix(scoped[-1L])))
})

test_that("a scope and a plain value can be mixed, and are told apart", {
  skip_if_no_cards()
  # AGE = "Age (years)" is a VALUE; BASE = c("0" = ...) is a COLUMN
  set.seed(2)
  d <- data.frame(USUBJID = sprintf("S%03d", 1:40),
                  BASE = sample(c("0", "1"), 40, TRUE),
                  SEX  = sample(c("F", "M"), 40, TRUE),
                  RACE = sample(c("A", "B"), 40, TRUE),
                  stringsAsFactors = FALSE)
  ard <- cards::ard_stack(
    d, .by = BASE,
    cards::ard_categorical(variables = c(SEX, RACE),
                           statistic = ~ c("n")))
  out <- suppressMessages(
    ard |> ard_normalize() |>
      ard_spread(cols = "BASE", cells = "{n:d}",
                 labels = list(SEX  = "Sex [n]",
                               BASE = c("0" = "Baseline 0",
                                        "1" = "Baseline 1"))))
  expect_identical(names(out)[-(1:2)], c("Baseline 0", "Baseline 1"))
  expect_identical(as.character(out$group)[1L], "Sex [n]")
})

test_that("an unnamed entry in a scope is refused, not ignored", {
  skip_if_no_cards()
  expect_error(
    .ard_check_named(c("a", b = "c"), "labels$BASE"),
    "labels\\$BASE")
})

test_that("the default moves no row a declared order did not move", {
  skip_if_no_cards()
  # `sort = TRUE` groups by the row keys; it does not decide what their
  # order is.  Here B comes first in the data, and B comes first out.
  d <- data.frame(
    PARAM = c("B", "B", "A", "A"),
    TRT = c("x", "y", "x", "y"),
    variable = "V", variable_level = c("L", "L", "L", "L"),
    context = "categorical",
    stat_name = "n", stat_label = "n", stat = c(1, 2, 3, 4),
    .label = "L", .kind = "categorical",
    stringsAsFactors = FALSE)
  out <- ard_spread(d, cols = "TRT", rows = c(PARAM = "PARAM"),
                    label = c(label = ".label"), cells = "{n:.0f}",
                    notes = FALSE)
  expect_identical(as.character(out$PARAM), c("B", "A"))
})

test_that("a declared order applies even though nothing asked to sort", {
  skip_if_no_cards()
  # `levels` IS somebody ordering the key, so it is sorted on
  d <- data.frame(
    PARAM = c("B", "B", "A", "A"),
    TRT = c("x", "y", "x", "y"),
    variable = "V", variable_level = "L",
    context = "categorical",
    stat_name = "n", stat_label = "n", stat = c(1, 2, 3, 4),
    .label = "L", .kind = "categorical",
    stringsAsFactors = FALSE)
  out <- ard_spread(d, cols = "TRT", rows = c(PARAM = "PARAM"),
                    label = c(label = ".label"), cells = "{n:.0f}",
                    levels = list(PARAM = c("A", "B")), notes = FALSE)
  expect_identical(as.character(out$PARAM), c("A", "B"))
})


test_that("a plain key keeps its blocks; TRUE clusters them", {
  skip_if_no_cards()
  # The cells are gathered by their key already, so a block is whole
  # either way.  What differs is whether SEPARATE blocks of one key are
  # brought together: the default leaves them where the data put them,
  # `TRUE` is the verb that groups.
  d <- data.frame(
    GRP = c("B", "A", "B"),
    SUB = c("x", "x", "y"),
    TRT = "t",
    variable = "V", variable_level = "L",
    context = "categorical",
    stat_name = "n", stat_label = "n", stat = 1:3,
    .label = "L", .kind = "categorical",
    stringsAsFactors = FALSE)
  run <- function(...) ard_spread(d, cols = "TRT",
                                  rows = c(GRP = "GRP", SUB = "SUB"),
                                  label = c(label = ".label"),
                                  cells = "{n:.0f}", notes = FALSE, ...)
  expect_identical(as.character(run()$GRP), c("B", "A", "B"))
  expect_identical(as.character(run(sort = TRUE)$GRP),
                   c("B", "B", "A"))
})

test_that("a declared order sorts WITHIN a plain key's block", {
  skip_if_no_cards()
  # the outer key stays put; the label, which `levels` ordered, does not
  d <- data.frame(
    PARAM = rep(c("B", "A"), each = 2L),
    TRT = "x",
    variable = "V", variable_level = c("hi", "lo", "hi", "lo"),
    context = "categorical",
    stat_name = "n", stat_label = "n", stat = 1:4,
    .label = c("hi", "lo", "hi", "lo"), .kind = "categorical",
    stringsAsFactors = FALSE)
  out <- ard_spread(d, cols = "TRT", rows = c(PARAM = "PARAM"),
                    label = c(label = ".label"), cells = "{n:.0f}",
                    levels = list(label = c("lo", "hi")), notes = FALSE)
  expect_identical(as.character(out$PARAM), c("B", "B", "A", "A"))
  expect_identical(as.character(out$label), c("lo", "hi", "lo", "hi"))
})


# ------------------------------------------- key order, key rows, .kind ----

make_fct_ard <- function() {
  adsl <- cards::ADSL
  adsl$TRT <- factor(as.character(adsl$ARM),
                     c("Xanomeline Low Dose", "Placebo",
                       "Xanomeline High Dose"))
  adsl$SEX <- as.character(adsl$SEX)
  cards::ard_stack(adsl, .by = TRT,
                   cards::ard_continuous(variables = AGE),
                   cards::ard_categorical(variables = SEX))
}
fct_cells <- list(continuous = "{mean:.1f}", categorical = "{n}")
fct_order <- c("Xanomeline Low Dose", "Placebo", "Xanomeline High Dose")

test_that("a factor key keeps the order it declared, however the rows move", {
  skip_if_no_cards()
  d <- ard_normalize(make_fct_ard())
  expect_true(is.factor(d$TRT))
  expect_identical(levels(d$TRT), fct_order)

  moved <- rbind(d[d$TRT %in% "Placebo", ], d[!d$TRT %in% "Placebo", ])
  expect_identical(
    names(ard_spread(moved, cols = "TRT", cells = fct_cells,
                     notes = FALSE))[-(1:2)], fct_order)
  bound <- dplyr::bind_rows(d[d$TRT %in% "Placebo", ],
                            d[!d$TRT %in% "Placebo", ])
  expect_identical(
    names(ard_spread(bound, cols = "TRT", cells = fct_cells,
                     notes = FALSE))[-(1:2)], fct_order)
  # the header agrees with the body
  expect_identical(names(ard_pull(moved, cols = "TRT")), fct_order)
  # an explicit `levels` still wins
  rev_order <- rev(fct_order)
  expect_identical(
    names(ard_spread(moved, cols = "TRT", cells = fct_cells,
                     levels = list(TRT = rev_order), notes = FALSE))[-(1:2)],
    rev_order)
})

test_that("the declared order is read off the ARD, unused levels included", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- factor(as.character(adsl$ARM),
                     c("Xanomeline Low Dose", "Placebo",
                       "Xanomeline High Dose", "Not Dosed"))
  ard <- cards::ard_categorical(adsl, by = TRT, variables = SEX)
  expect_identical(levels(ard_normalize(ard)$TRT),
                   c("Xanomeline Low Dose", "Placebo",
                     "Xanomeline High Dose", "Not Dosed"))
  # normalizing twice (what ard_pull() does to a plan's frame) keeps it
  expect_identical(levels(ard_normalize(ard_normalize(ard))$TRT),
                   levels(ard_normalize(ard)$TRT))
})

test_that("a character key stays character", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  expect_type(d$TRT, "character")
})

test_that("a key variable's own rows are kept, marked, and left out of the body", {
  skip_if_no_cards()
  d <- ard_normalize(make_fct_ard())
  expect_true(any(d$.key_own))
  expect_true(all(d$variable[d$.key_own] == "TRT"))
  tbl <- ard_spread(d, cols = "TRT", cells = fct_cells, notes = FALSE)
  expect_false("TRT" %in% tbl$group)

  # flipping the mark spreads them after all
  d2 <- d
  d2$.key_own <- FALSE
  tbl2 <- ard_spread(d2, cols = "TRT", cells = fct_cells, notes = FALSE)
  expect_true("TRT" %in% tbl2$group)

  # drop_key_variables = TRUE removes them outright, and still reports it
  gone <- ard_normalize(make_fct_ard(), drop_key_variables = TRUE)
  expect_false("TRT" %in% gone$variable)
  expect_false(any(gone$.key_own))
  expect_equal(ard_spread(gone, cols = "TRT", cells = fct_cells,
                          notes = FALSE), tbl)
})

test_that(".kind is decided per summary, not per variable", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  adsl$DEC <- round(adsl$AGE / 10)
  d <- ard_normalize(cards::ard_stack(
    adsl, .by = TRT,
    cards::ard_continuous(variables = DEC),
    cards::ard_categorical(variables = DEC)))
  body <- !d$.key_own
  expect_identical(unique(d$.kind[body & d$context == "continuous"]),
                   "continuous")
  expect_identical(unique(d$.kind[body & d$context == "categorical"]),
                   "categorical")
})

test_that("the example workbooks shipped with the package still read and run", {
  skip_if_not_installed("readxl")
  dir <- system.file("extdata", "ard-spec", package = "rtfreporter")
  skip_if(!nzchar(dir), "examples not installed")
  for (id in c("DM", "AE", "ORR", "LB", "PK")) {
    sp <- read_ard_spec(file.path(dir, paste0(id, ".xlsx")))
    expect_identical(attr(sp, "output_id"), id)
    expect_s3_class(read_ard_spec(file.path(dir, "study.xlsx"),
                                  output_id = id), "ard_spec")
  }
  expect_error(read_ard_spec(file.path(dir, "study.xlsx")), "defines 5 reports")
})
