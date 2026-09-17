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

test_that("a template naming a missing statistic yields NA, not an error", {
  skip_if_no_cards()
  tbl <- ard_table(make_ard(), cols = "TRT", rows = c(group = "variable"),
                   cells = "{nope}")
  expect_true(all(is.na(tbl$Placebo)))
})

test_that("stats = 'rows' gives one numeric row per statistic", {
  skip_if_no_cards()
  tbl <- ard_table(make_ard(), cols = "TRT", rows = c(group = "variable"),
                   label = c(Statistic = "stat_label"), stats = "rows")
  expect_true(is.numeric(tbl$Placebo))
  age <- tbl[tbl$group == "AGE", ]
  expect_true(all(c("N", "Mean", "SD") %in% as.character(age$Statistic)))
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
