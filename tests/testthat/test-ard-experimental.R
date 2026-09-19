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

# ---------------------------------------------------------------- ard_round

test_that("ard_round() follows SAS on a tie and base R on request", {
  expect_equal(ard_round(c(0.5, 1.5, 2.5, -0.5, -2.5), 0),
               c(1, 2, 3, -1, -3))
  expect_equal(ard_round(c(0.5, 1.5, 2.5, -0.5, -2.5), 0, type = "r"),
               c(0, 2, 2, 0, -2))
  expect_equal(ard_round(2.345, 2), 2.35)
  expect_equal(ard_round(123.456, 1), 123.5)
  expect_equal(ard_round(c(NA, 1.25), 1), c(NA, 1.3))
})

# ------------------------------------------------------------ ard_normalize

test_that("ard_normalize() keys the group pairs by name and flattens list-cols", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())

  expect_true(is.data.frame(d))
  expect_true("TRT" %in% names(d))
  expect_false(any(vapply(d, is.list, logical(1))))
  expect_setequal(stats::na.omit(unique(d$TRT)),
                  c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
  # attributes / total_n rows are gone, and so are the by-variable's own counts
  expect_false(any(d$context %in% c("attributes", "total_n")))
  expect_false("TRT" %in% d$variable)
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

# ---------------------------------------------------------------- ard_table

test_that("ard_table() builds the demographics shape", {
  skip_if_no_cards()
  tbl <- ard_table(
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
  tbl <- ard_table(ard, cols = c("TRT", "SEX"), rows = c(group = "variable"),
                   cells = "{n:.0f} ({p:.1f%})")
  expect_true(all(grepl("____", names(tbl)[-(1:2)])))
  expect_true("Placebo____F" %in% names(tbl))
  # column order follows `levels` when given
  tbl2 <- ard_table(ard, cols = c("TRT", "SEX"), rows = c(group = "variable"),
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

  tbl <- ard_table(
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
    ard_table(ard, cols = "TRT", rows = c(group = "variable"),
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
  tbl <- ard_table(make_ard(), cols = "TRT", rows = c(group = "variable"),
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
    ard_table(ard, cols = "TRT", rows = c(group = "variable"), cells = cells)
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
    ard_table(a, cols = "TRT", rows = c(group = "variable"), cells = cells)
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
    ard_table(make_ard(), cols = "TRT", rows = c(group = "variable"),
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
    bad <- ard_table(stacked, cols = "group1_level",
                     rows = c(group = "variable"),
                     cells = "{n:.0f} ({p:.1f%})"),
    "reads a POSITION")
  # the warning is earned: the position invented columns from the other block
  expect_true(all(c("F", "M") %in% names(bad)))

  # naming the variable is correct and silent
  expect_no_warning(
    good <- ard_table(stacked, cols = "TRT", rows = c(group = "variable"),
                      cells = "{n:.0f} ({p:.1f%})", notes = FALSE))
  expect_setequal(names(good)[-(1:2)],
                  c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))

  # a single-block ARD keeps quiet
  expect_no_warning(ard_table(make_ard(), cols = "group1_level",
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
  expect_no_warning(ard_table(stacked, cols = "TRT", rows = c(v = "group1"),
                              cells = "{n:.0f} ({p:.1f%})", notes = FALSE))
  expect_no_warning(
    both <- ard_table(stacked, cols = "TRT",
                      rows = c(v = "group1", lv = "group1_level"),
                      cells = "{n:.0f} ({p:.1f%})", notes = FALSE))
  # and the pair really does identify the row
  expect_true(all(c("TRT", "SEX") %in% as.character(both$v)))

  # the level alone still warns, and says how to make it unambiguous
  expect_warning(
    ard_table(stacked, cols = "group1_level", rows = c(g = "variable"),
              cells = "{n:.0f} ({p:.1f%})", notes = FALSE),
    "reads a POSITION")
  expect_warning(
    ard_table(stacked, cols = "group1_level", rows = c(g = "variable"),
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
  tbl <- ard_table(ard, cols = c("TRT", "SEX"), rows = c(group = "variable"),
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
  tbl <- ard_table(ard, cols = "TRT", rows = c(group = "variable"),
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
  explicit <- ard_table(make_ard(), cols = "TRT",
                        rows = c(group = "variable"), cells = cells)
  omitted  <- ard_table(make_ard(), cols = "TRT", cells = cells)
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
  tbl <- ard_table(ard, cols = "TRT", cells = "{n:.0f} ({p:.1f%})")
  expect_identical(names(tbl)[1], "label")
  expect_false("group" %in% names(tbl))
})

test_that("an explicit `rows` always wins over the default", {
  skip_if_no_cards()
  tbl <- ard_table(make_ard(), cols = "TRT",
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
  by_hand <- do.call(ard_table, c(args, list(sort = FALSE, sort_stat = "n")))
  by_hand <- by_hand[order(by_hand$soc != "Any TEAE", by_hand$soc,
                           !is.na(by_hand$term), -by_hand$.sort_stat,
                           by_hand$term), ]
  by_hand$.sort_stat <- NULL
  rownames(by_hand) <- NULL

  declared <- do.call(ard_table,
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
    ard_table(make_ard(), cols = "TRT", rows = c(group = "variable"),
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
  tbl <- ard_table(make_factor_ard(), cols = "TRT",
                   cells = list(continuous  = c("Mean" = "{mean:.1f}"),
                                categorical = "{n:.0f} ({p:.1f%})"))
  gr <- tbl[tbl$group == "AGEGR", ]
  expect_identical(as.character(gr$label), c("<65", "65-74", ">=75"))
  sx <- tbl[tbl$group == "SEX", ]
  expect_identical(as.character(sx$label), c("Female", "Male"))

  # an explicit `levels` still wins over what the factor declared
  tbl2 <- ard_table(make_factor_ard(), cols = "TRT",
                    cells = list(continuous  = c("Mean" = "{mean:.1f}"),
                                 categorical = "{n:.0f} ({p:.1f%})"),
                    levels = list(SEX = c("Male", "Female")))
  sx2 <- tbl2[tbl2$group == "SEX", ]
  expect_identical(as.character(sx2$label), c("Male", "Female"))
})

test_that("the keyed columns are plain factors, not ordered ones", {
  skip_if_no_cards()
  tbl <- ard_table(make_factor_ard(), cols = "TRT",
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
  expect_false("ordered" %in% names(formals(ard_table)))
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

  raw <- do.call(ard_table, args)
  expect_true(is.numeric(raw$Placebo))
  expect_equal(raw$Placebo[raw$Statistic == "Mean"],
               mean(adsl$AGE[adsl$TRT == "Placebo"]))

  fmt <- do.call(ard_table, c(args, list(value = "stat_fmt")))
  expect_true(is.character(fmt$Placebo))
  # cards' own formatting, not ours
  expect_match(fmt$Placebo[fmt$Statistic == "Mean"], "^[0-9]+[.][0-9]$")
  expect_identical(fmt$Statistic, raw$Statistic)
})

test_that("{x:fmt} names cards' formatted value inside a template", {
  skip_if_no_cards()
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- cards::ard_stack(adsl, .by = TRT,
                          cards::ard_continuous(variables = AGE))
  tbl <- ard_table(ard, cols = "TRT", rows = c(group = "variable"),
                   cells = c("bare" = "{mean}",
                             "fmt"  = "{mean:fmt}",
                             "raw"  = "{mean:raw}",
                             "ours" = "{mean:.3f}"))
  v <- function(lab) tbl$Placebo[tbl$label == lab]
  expect_identical(v("fmt"), v("bare"))          # "" and "fmt" are the same
  expect_false(identical(v("raw"), v("fmt")))    # raw is unrounded
  expect_match(v("ours"), "^[0-9]+[.][0-9]{3}$")
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
  without <- ard_table(ard, cols = "TRT", hierarchy = "AESOC",
                       label = c(soc = "AESOC"), cells = "{n:.0f} ({p:.1f%})",
                       notes = FALSE)
  expect_false("Any TEAE" %in% as.character(without$soc))

  # named, it becomes the overall row, with the key read from variable_level
  with <- ard_table(ard, cols = "TRT", hierarchy = "AESOC",
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
  a <- ard_table(ard, cols = "TRT", hierarchy = c("AESOC", "AETERM"),
                 overall = "Any TEAE", rows = c(soc = "AESOC"),
                 label = c(term = "AETERM"), cells = "{n:.0f} ({p:.1f%})",
                 notes = FALSE)
  b <- ard_table(ard, cols = "TRT", hierarchy = c("AESOC", "AETERM"),
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

  expect_message(do.call(ard_table, args), "ARD rows? (were|was) not used")
  expect_message(do.call(ard_table, args), "no template named it")
  expect_message(do.call(ard_table, args), "a key variable's own tabulation")

  # silenced
  expect_no_message(do.call(ard_table, c(args, list(notes = FALSE))))

  # the result stays a plain data frame, so a comparison against the table the
  # caller built before is not disturbed by an extra attribute
  quiet <- do.call(ard_table, c(args, list(notes = FALSE)))
  expect_null(attr(quiet, "ard_ignored", exact = TRUE))
  expect_identical(class(quiet), "data.frame")

  # ... unless asked for
  kept <- suppressMessages(do.call(ard_table, c(args, list(notes = "attr"))))
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
  ref <- ard_table(ard, cols = "TRT", cells = cells, notes = FALSE)

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
  expect_true(all(h$.depth == 1L))
})

test_that("passing the raw ARD says so", {
  skip_if_no_cards()
  err <- tryCatch(ard_spread(cards::ADSL, cols = "ARM"),
                  error = function(e) conditionMessage(e))
  expect_match(err, "does not look like an ard_normalize")
  expect_match(err, "ard_table")
  # but a rebuilt frame without the class is still accepted
  d <- ard_normalize(make_ard())
  class(d) <- "data.frame"
  expect_s3_class(ard_spread(d, cols = "TRT", cells = "{n:.0f} ({p:.1f%})",
                             notes = FALSE), "data.frame")
})

test_that("a template naming a missing statistic yields NA, not an error", {
  skip_if_no_cards()
  tbl <- ard_table(make_ard(), cols = "TRT", rows = c(group = "variable"),
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
  tbl <- ard_table(ard, cols = "TRT", rows = c(group = "variable"),
                   label = c(Statistic = "stat_label"), stats = "rows")
  expect_true(is.numeric(tbl$Placebo))
  expect_true(all(c("N", "Mean", "SD") %in% as.character(tbl$Statistic)))
})

test_that("stats = 'rows' on a variable with levels needs the level as a key", {
  skip_if_no_cards()
  # every level of AGEGR carries an `n`, so labelling rows by the statistic
  # alone cannot separate them -- previously the last level silently won
  expect_error(
    ard_table(make_ard(), cols = "TRT", rows = c(group = "variable"),
              label = c(Statistic = "stat_label"), stats = "rows"),
    "telling themselves apart")

  # naming the level as well is what the message asks for, and it works
  tbl <- ard_table(make_ard(), cols = "TRT",
                   rows = c(group = "variable", level = "variable_level"),
                   label = c(Statistic = "stat_label"), stats = "rows")
  gr <- tbl[tbl$group == "AGEGR", ]
  expect_setequal(stats::na.omit(unique(gr$level)), c("<65", "65-74", ">=75"))
  expect_true(is.numeric(tbl$Placebo))
})

test_that("sort_stat totals a statistic across the spread columns", {
  skip_if_no_cards()
  tbl <- ard_table(make_ard(), cols = "TRT", rows = c(group = "variable"),
                   cells = "{n:.0f} ({p:.1f%})", sort_stat = "n")
  expect_true(".sort_stat" %in% names(tbl))
  sex <- tbl[tbl$group == "SEX", ]
  expect_equal(sum(sex$.sort_stat), 254)
})

test_that("round = 'r' reaches the cells", {
  skip_if_no_cards()
  d <- ard_normalize(make_ard())
  d <- d[d$variable == "AGE" & d$stat_name == "mean", ]
  d$stat <- 0.5
  sas <- ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                    cells = "{mean:.0f}", round = "sas")
  r   <- ard_spread(d, cols = "TRT", rows = c(group = "variable"),
                    cells = "{mean:.0f}", round = "r")
  expect_identical(sas$Placebo, "1")
  expect_identical(r$Placebo, "0")
})

test_that("ard_table() refuses an input that is not an ARD", {
  expect_error(ard_normalize(data.frame(a = 1)), "cards ARD")
  skip_if_no_cards()
  expect_error(ard_table(make_ard(), cols = "NOPE"), "no column 'NOPE'")
})

test_that("naming the analysed variable says where its levels went", {
  skip_if_no_cards()
  # `levels` keys on the analysis variable, `label` cannot -- so the error has
  # to explain the asymmetry rather than just listing the columns.
  expect_error(ard_table(make_ard(), cols = "TRT", label = c(row = "AGEGR")),
               "analysis variable")
  expect_error(ard_table(make_ard(), cols = "TRT", label = c(row = "AGEGR")),
               "[.]label")
  # a name that is neither a column nor an analysed variable keeps the
  # original message
  expect_error(ard_table(make_ard(), cols = "TRT", label = c(row = "NOPE")),
               "no column 'NOPE'")
})

# ----------------------------------------------------------------- the spec

test_that("a spec round-trips through CSV and drives the conversion", {
  skip_if_no_cards()
  sp <- ard_spec(data.frame(
    variable = c("AGE", "AGE", "AGEGR", "SEX"),
    label    = c("Age (years)", NA, "Age group", "Sex"),
    order    = c(1, 1, 2, 3),
    row      = c("n", "Mean (SD)", NA, NA),
    template = c("{N}", "{mean} ({sd})", "{n} ({p})", "{n} ({p})"),
    levels   = c(NA, NA, "<65 | 65-74 | >=75", NA),
    round    = "sas",
    digits   = c("0", "1,2", NA, NA),
    stringsAsFactors = FALSE))
  expect_s3_class(sp, "ard_spec")

  path <- tempfile(fileext = ".csv")
  write_ard_spec(sp, path)
  back <- read_ard_spec(path)
  expect_equal(back$template, sp$template)
  expect_equal(back$variable, sp$variable)
  unlink(path)

  tbl <- ard_table(make_ard(), cols = "TRT", rows = c(group = "variable"),
                   spec = sp)
  expect_identical(as.character(unique(tbl$group)),
                   c("Age (years)", "Age group", "Sex"))
  age <- tbl[tbl$group == "Age (years)", ]
  # the spec's row order becomes the label column's level order
  expect_s3_class(tbl$label, "factor")
  expect_identical(as.character(age$label), c("n", "Mean (SD)"))
  # digits = "1,2" applied per token, with no inline spec in the template
  expect_match(age$Placebo[2], "^[0-9]+[.][0-9] [(][0-9]+[.][0-9]{2}[)]$")
  # levels from the spec ordered the categories
  gr <- tbl[tbl$group == "Age group", ]
  expect_identical(as.character(gr$label), c("<65", "65-74", ">=75"))
})

test_that("ard_spec() validates and fills in the optional columns", {
  sp <- ard_spec(data.frame(variable = "AGE", template = "{mean}"))
  expect_true(all(c("label", "order", "context", "row", "levels", "round",
                    "digits", "signif") %in% names(sp)))
  expect_error(ard_spec(data.frame(x = 1)), "`variable` column")
  expect_error(ard_spec(data.frame(variable = "AGE", round = "banker")),
               "must be")
})

test_that("ard_spec_template() scaffolds one row per output row", {
  skip_if_no_cards()
  sp <- ard_spec_template(make_ard())
  expect_s3_class(sp, "ard_spec")
  expect_true(all(c("AGE", "AGEGR", "SEX") %in% sp$variable))
  age <- sp[sp$variable == "AGE", ]
  expect_true(all(c("n", "Mean (SD)", "Min, Max") %in% age$row))
  expect_true(all(!is.na(sp$template)))
})

# ------------------------------------------------------- inspect / generate

test_that("ard_keys() reports what the ARD holds", {
  skip_if_no_cards()
  out <- utils::capture.output(k <- ard_keys(make_ard()))
  expect_true(any(grepl("TRT", out)))
  expect_true("TRT" %in% k$keys)
  expect_true(all(c("AGE", "SEX") %in% k$variables))
  expect_true("continuous" %in% k$contexts)
})

test_that("ard_template() emits code that actually runs", {
  skip_if_no_cards()
  ard <- make_ard()
  code <- utils::capture.output(gen <- ard_template(ard, cols = "TRT"))
  expect_true(any(grepl("ard_table", gen)))
  expect_true(any(grepl("cols  = \"TRT\"", gen)))
  # the generated call evaluates against the same ARD
  tbl <- eval(parse(text = paste(gen, collapse = "\n")))
  expect_true(is.data.frame(tbl))
  expect_true("Placebo" %in% names(tbl))

  spec_code <- utils::capture.output(
    gen2 <- ard_template(ard, cols = "TRT", spec = TRUE))
  expect_true(any(grepl("read_ard_spec", gen2)))
})
