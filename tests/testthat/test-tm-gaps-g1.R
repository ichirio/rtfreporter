# Tests added to close the gaps found by the Traceability Matrix check
# (2026-11-12): each asserts what its requirement states.
# Group g1: normalize_ard(), widen_ard(), pull_ard(), overall_row(),
# list_ard_keys() and the number-format helpers.  Fictional data only.

# ---------------------------------------------------------------- helpers

# A small ARD as a plain data frame (cards' columns, no cards class): the
# treatment TRT (A, B) in group1, AGE summarised and SEX = F tabulated.
.g1_base <- function() {
  d <- data.frame(
    group1 = "TRT",
    group1_level = c("A", "A", "B", "B", "A", "A", "B", "B"),
    variable = c("AGE", "AGE", "AGE", "AGE", "SEX", "SEX", "SEX", "SEX"),
    variable_level = c(NA, NA, NA, NA, "F", "F", "F", "F"),
    context = c(rep("continuous", 4L), rep("categorical", 4L)),
    stat_name = c("N", "mean", "N", "mean", "n", "p", "n", "p"),
    stat_label = c("N", "Mean", "N", "Mean", "n", "%", "n", "%"),
    stat = c(10, 12.34, 12, 15.67, 4, 0.4, 6, 0.5),
    stringsAsFactors = FALSE)
  d$fmt_fun <- list(0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L)
  d
}

# A plain hierarchical ARD: the overall sentinel, a SOC and a term, per arm.
.g1_hier <- function() {
  data.frame(
    group1 = "TRT",
    group1_level = c("A", "B", "A", "B", "A", "B"),
    group2 = c(NA, NA, NA, NA, "AESOC", "AESOC"),
    group2_level = c(NA, NA, NA, NA, "Cardiac", "Cardiac"),
    variable = c("..ard_hierarchical_overall..", "..ard_hierarchical_overall..",
                 "AESOC", "AESOC", "AETERM", "AETERM"),
    variable_level = c(NA, NA, "Cardiac", "Cardiac", "Palpitations",
                       "Palpitations"),
    context = "hierarchical",
    stat_name = "n",
    stat_label = "n",
    stat = c(5, 7, 3, 4, 2, 1),
    stringsAsFactors = FALSE)
}

.g1_hier_cols <- c("AESOC", "AETERM")

# ------------------------------------------------------------ normalize_ard

test_that("FT-AR3000 [FR-AR101]: normalize_ard() class, one row per statistic, column order", {
  ard <- .g1_base()
  d <- normalize_ard(ard)
  expect_identical(class(d), c("ard_long", "data.frame"))
  expect_identical(nrow(d), 8L)
  expect_identical(
    names(d)[1:14],
    c("TRT", "variable", "variable_level", "context", "stat_name", "stat_label",
      "stat", "stat_fmt", ".kind", ".depth", ".label", ".label_order",
      ".overall", ".key_own"))
  # the ARD's other columns follow the leading ones
  expect_true(all(c("group1", "group1_level", "fmt_fun") %in% names(d)[-(1:14)]))
  expect_identical(d$stat, ard$stat)
  expect_identical(d$stat_name, ard$stat_name)
})

test_that("FT-AR3001 [FR-AR102]: a grouping variable in any group slot is one column", {
  d <- data.frame(
    group1 = c("TRT", "TRT", "SEX", "SEX", "SEX"),
    group1_level = c("A", "B", "F", "F", "M"),
    group2 = c(NA, NA, "TRT", "TRT", NA),
    group2_level = c(NA, NA, "A", "B", NA),
    variable = "AGE", variable_level = NA_character_,
    context = "continuous", stat_name = "mean", stat_label = "Mean",
    stat = c(1, 2, 3, 4, 5), stringsAsFactors = FALSE)
  n <- normalize_ard(d)
  # TRT is group1 on rows 1-2 and group2 on rows 3-4; row 5 groups by SEX only
  expect_identical(n$TRT, c("A", "B", "A", "B", NA))
  expect_identical(n$SEX, c(NA, NA, "F", "F", "M"))
})

test_that("FT-AR3002 [FR-AR103]: list columns are flattened, NULL / long / function to NA", {
  d <- .g1_base()[1:4, ]
  d$variable_level <- list(NULL, c(1, 2), mean,
                           factor("Severe", levels = c("Mild", "Severe")))
  n <- normalize_ard(d)
  expect_false(any(vapply(n, is.list, logical(1))))
  expect_identical(n$variable_level, c(NA, NA, NA, "Severe"))
  # the factor gives its label, never its integer code (2)
  expect_false(identical(n$variable_level[4], "2"))
})

test_that("FT-AR3003 [FR-AR105]: stat_fmt from cards' formatter, fmt_fun, a given column, or NA", {
  # cards' own formatter, no stat_fmt column in the ARD
  skip_if_not_installed("cards")
  dat <- data.frame(AGE = c(10, 20, 33), TRT = c("A", "B", "A"))
  ard <- cards::ard_summary(
    dat, by = TRT, variables = AGE,
    statistic = ~ cards::continuous_summary_fns(c("N", "mean")))
  expect_false("stat_fmt" %in% names(ard))
  n <- normalize_ard(ard)
  expect_identical(n$stat_fmt[n$TRT == "A" & n$stat_name == "mean"], "21.5")
  expect_identical(n$stat_fmt[n$TRT == "A" & n$stat_name == "N"], "2")

  # a fmt_fun column on a plain data frame: a function, and a decimal count
  p <- .g1_base()[1:2, ]
  p$stat <- c(12.34, 12.34)
  p$fmt_fun <- list(1L, function(x) paste0("<", x, ">"))
  expect_identical(normalize_ard(p)$stat_fmt, c("12.3", "<12.34>"))

  # neither a stat_fmt nor a fmt_fun column: NA
  q <- p
  q$fmt_fun <- NULL
  expect_identical(normalize_ard(q)$stat_fmt, c(NA_character_, NA_character_))

  # a given stat_fmt column is kept
  r <- q
  r$stat_fmt <- c("twelve", "thirteen")
  expect_identical(normalize_ard(r)$stat_fmt, c("twelve", "thirteen"))
})

test_that("FT-AR3004 [FR-AR110]: ard_ignored reports the removed rows with a reason", {
  d <- .g1_base()
  extra <- d[1:2, ]
  extra$group1 <- NA_character_
  extra$group1_level <- NA_character_
  extra$variable <- c("TRT", "TRT")
  extra$context <- "attributes"
  extra$stat_name <- c("name", "label")
  extra$stat <- c(1, 2)
  n <- normalize_ard(rbind(d, extra))
  ig <- attr(n, "ard_ignored", exact = TRUE)
  expect_s3_class(ig, "data.frame")
  expect_identical(names(ig), c("variable", "context", "stat_name", "rows", "reason"))
  expect_identical(ig$variable, c("TRT", "TRT"))
  expect_identical(ig$context, c("attributes", "attributes"))
  expect_identical(ig$stat_name, c("label", "name"))
  expect_identical(ig$rows, c(1L, 1L))
  expect_identical(ig$reason, rep("not a table statistic", 2L))
})

test_that("FT-AR3005 [FR-AR111]: manipulation between the stages leaves the table unchanged", {
  skip_if_not_installed("dplyr")
  d <- normalize_ard(.g1_base())
  cells <- list(continuous = c("Mean" = "{mean:.1f}"), categorical = "{n:.0f}")
  ref <- widen_ard(d, cols = "TRT", rows = c(group = "variable"), cells = cells,
                   notes = FALSE)
  expect_identical(ref$A, c("12.3", "4"))
  expect_identical(ref$B, c("15.7", "6"))
  same <- function(x) {
    got <- widen_ard(x, cols = "TRT", rows = c(group = "variable"), cells = cells,
                     notes = FALSE)
    expect_identical(got, ref)
  }
  same(dplyr::arrange(d, .overall))
  same(dplyr::bind_rows(d[1:3, ], d[-(1:3), ]))
  same(dplyr::select(d, dplyr::all_of(names(d))))
  same(dplyr::filter(d, TRUE))
  same(dplyr::mutate(d, extra = 1))
  same(d[seq_len(nrow(d)), ])
})

test_that("FT-AR3006 [FR-AR113]: keys = NULL, a character vector, and a structural name", {
  base <- .g1_base()
  # the default: a column for every grouping-variable name in the group columns
  two <- rbind(base, transform(base[5:6, ], group1 = "SEX", group1_level = "F"))
  expect_identical(names(normalize_ard(two))[1:2], c("TRT", "SEX"))
  # keys = "SEX": SEX is the key column, TRT is not made
  n <- normalize_ard(two, keys = "SEX")
  expect_identical(names(n)[1:2], c("SEX", "variable"))
  expect_false("TRT" %in% names(n))
  # the hierarchy names follow as key columns
  h <- normalize_ard(.g1_hier(), keys = "TRT", hierarchy = .g1_hier_cols)
  expect_identical(names(h)[1:3], c("TRT", "AESOC", "AETERM"))
  h2 <- normalize_ard(.g1_hier(), keys = "SEX", hierarchy = .g1_hier_cols)
  expect_identical(names(h2)[1:3], c("SEX", "AESOC", "AETERM"))
  expect_false("TRT" %in% names(h2))
  # a structural column name is never made a key
  s <- normalize_ard(base, keys = c("variable", "TRT"))
  expect_identical(names(s)[1:2], c("TRT", "variable"))
  expect_identical(s$variable, base$variable)
})

test_that("FT-AR3007 [FR-AR115]: no hierarchy: .depth NA and .label is variable_level", {
  d <- normalize_ard(.g1_base())
  expect_true(all(is.na(d$.depth)))
  expect_identical(d$.label, d$variable_level)
  expect_identical(d$.label, c(NA, NA, NA, NA, "F", "F", "F", "F"))
})

test_that("FT-AR3008 [FR-AR116]: a bare overall string labels the sentinel rows, depth 1", {
  d <- normalize_ard(.g1_hier(), hierarchy = .g1_hier_cols, overall = "Any TEAE")
  sentinel <- d$variable == "..ard_hierarchical_overall.."
  expect_identical(sum(sentinel), 2L)
  expect_identical(d$AESOC[sentinel], c("Any TEAE", "Any TEAE"))
  expect_identical(d$.overall, sentinel)
  expect_identical(d$.depth[sentinel], c(1L, 1L))
  expect_identical(d$.label[sentinel], c("Any TEAE", "Any TEAE"))
  # the other rows are not overall rows: SOC depth 1, term depth 2
  expect_identical(d$.depth[!sentinel], c(1L, 1L, 2L, 2L))
})

test_that("FT-AR3009 [FR-AR117]: overall_row(from = ) takes that variable's rows as overall", {
  base <- .g1_hier()
  base <- base[base$variable != "..ard_hierarchical_overall..", ]
  # the arm is the analysed variable of a separately built block
  ov <- data.frame(
    group1 = NA_character_, group1_level = NA_character_,
    group2 = NA_character_, group2_level = NA_character_,
    variable = "TRT", variable_level = c("A", "B"), context = "categorical",
    stat_name = "n", stat_label = "n", stat = c(9, 11), stringsAsFactors = FALSE)
  d <- normalize_ard(rbind(ov, base), hierarchy = .g1_hier_cols,
                     overall = overall_row("Any TEAE", from = "TRT"))
  rows <- d$variable == "TRT"
  expect_identical(sum(rows), 2L)
  expect_identical(d$.overall, rows)
  # the key column of TRT is filled from variable_level
  expect_identical(d$TRT[rows], c("A", "B"))
  # the label goes into the first hierarchy column
  expect_identical(d$AESOC[rows], c("Any TEAE", "Any TEAE"))
  expect_identical(d$.depth[rows], c(1L, 1L))
})

test_that("FT-AR3010 [FR-AR118]: overall = NULL drops the sentinel rows and reports them", {
  d <- normalize_ard(.g1_hier(), hierarchy = .g1_hier_cols)
  expect_false(any(d$variable == "..ard_hierarchical_overall.."))
  expect_identical(nrow(d), 4L)
  ig <- attr(d, "ard_ignored", exact = TRUE)
  expect_identical(ig$variable, "..ard_hierarchical_overall..")
  expect_identical(ig$stat_name, "n")
  expect_identical(ig$rows, 2L)
  expect_identical(ig$reason, "an overall block, and no `overall =` was given")
})

test_that("FT-AR3011 [FR-AR119]: attributes / total_n rows are removed and reported", {
  d <- .g1_base()
  extra <- d[1:2, ]
  extra$group1 <- NA_character_
  extra$group1_level <- NA_character_
  extra$variable <- c("..ard_total_n..", "TRT")
  extra$context <- c("total_n", "attributes")
  extra$stat_name <- c("N", "label")
  extra$stat <- c(22, 1)
  n <- normalize_ard(rbind(d, extra))
  expect_false(any(n$context %in% c("attributes", "total_n")))
  expect_identical(nrow(n), 8L)
  ig <- attr(n, "ard_ignored", exact = TRUE)
  expect_identical(ig$context, c("attributes", "total_n"))
  expect_identical(ig$variable, c("TRT", "..ard_total_n.."))
  expect_identical(ig$stat_name, c("label", "N"))
  expect_identical(ig$reason, c("not a table statistic", "not a table statistic"))
  expect_identical(attr(n, "ard_total_n", exact = TRUE), 22)
})

test_that("FT-AR3012 [FR-AR123]: drop_key_variables removes the key rows and reports them", {
  d <- .g1_base()
  own <- data.frame(
    group1 = NA_character_, group1_level = NA_character_,
    variable = "TRT", variable_level = c("A", "B"), context = "categorical",
    stat_name = "n", stat_label = "n", stat = c(20, 22), stringsAsFactors = FALSE)
  own$fmt_fun <- list(0L, 0L)
  x <- rbind(d, own)
  n <- normalize_ard(x, drop_key_variables = TRUE)
  expect_false("TRT" %in% n$variable)
  expect_identical(nrow(n), 8L)
  ig <- attr(n, "ard_ignored", exact = TRUE)
  expect_identical(ig$variable, "TRT")
  expect_identical(ig$context, "categorical")
  expect_identical(ig$stat_name, "n")
  expect_identical(ig$rows, 2L)
  expect_identical(ig$reason, "a key variable's own tabulation")
  # widen_ard() gives the same table as with the rows marked
  marked <- normalize_ard(x)
  expect_true(all(marked$.key_own[marked$variable == "TRT"]))
  cells <- list(continuous = "{mean:.1f}", categorical = "{n:.0f}")
  expect_identical(
    widen_ard(n, cols = "TRT", cells = cells, notes = FALSE),
    widen_ard(marked, cols = "TRT", cells = cells, notes = FALSE))
})

test_that("FT-AR3013 [FR-AR124]: stack adds NAME / NAME_level, NA on a row of no layer", {
  mk <- function(g1, l1, g2, l2, stat) {
    data.frame(group1 = g1, group1_level = l1, group2 = g2, group2_level = l2,
               variable = "AGE", variable_level = NA_character_,
               context = "continuous", stat_name = "mean", stat_label = "Mean",
               stat = stat, stringsAsFactors = FALSE)
  }
  d <- rbind(
    mk("SEX", "F", NA, NA, 1),                       # a layer: SEX
    mk("AGEGR1", "<65", NA, NA, 2),                  # a layer: AGEGR1
    mk("AGEGR1", "<65", "SEX", "F", 3),              # crossed, SEX in slot 2
    mk("SEX", "M", "AGEGR1", ">=65", 4),             # crossed, SEX in slot 1
    mk("TRT", "A", NA, NA, 5))                       # no layer
  n <- normalize_ard(d, keys = c("TRT", "SEX", "AGEGR1"),
                     stack = list(subgroup = list("SEX", "AGEGR1",
                                                  c("AGEGR1", "SEX"))))
  expect_identical(as.character(n$subgroup),
                   c("SEX", "AGEGR1", "AGEGR1 / SEX", "AGEGR1 / SEX", NA))
  expect_identical(as.character(n$subgroup_level),
                   c("F", "<65", "<65 / F", ">=65 / M", NA))
  expect_true(is.factor(n$subgroup))
  expect_identical(levels(n$subgroup), c("SEX", "AGEGR1", "AGEGR1 / SEX"))
  # the variables' own columns stay
  expect_identical(n$SEX, c("F", NA, "F", "M", NA))
  expect_identical(n$AGEGR1, c(NA, "<65", "<65", ">=65", NA))
  # a character vector: one layer a variable
  m <- normalize_ard(d[1:2, ], keys = c("SEX", "AGEGR1"),
                     stack = list(subgroup = c("SEX", "AGEGR1")))
  expect_identical(as.character(m$subgroup), c("SEX", "AGEGR1"))
  expect_identical(as.character(m$subgroup_level), c("F", "<65"))
})

test_that("FT-AR3014 [FR-AR128]: a cards ARD built with a by variable is one row per statistic", {
  skip_if_not_installed("cards")
  dat <- data.frame(AGE = c(10, 20, 33, 41), TRT = c("A", "B", "A", "B"))
  ard <- cards::ard_summary(
    dat, by = TRT, variables = AGE,
    statistic = ~ cards::continuous_summary_fns(c("N", "mean")))
  expect_true("group1" %in% names(ard))
  expect_identical(nrow(ard), 4L)
  d <- normalize_ard(ard)
  expect_identical(nrow(d), 4L)
  expect_identical(d$TRT, c("A", "A", "B", "B"))
  expect_identical(d$stat_name, c("N", "mean", "N", "mean"))
  expect_identical(d$stat, c(2, 21.5, 2, 30.5))
  expect_identical(anyDuplicated(d[c("TRT", "variable", "stat_name")]), 0L)
})

test_that("FT-AR3015 [FR-AR129]: an ARD with no grouping variable makes no key, keeps the total", {
  d <- data.frame(
    variable = c("..ard_total_n..", "AGE", "AGE"),
    variable_level = NA_character_,
    context = c("total_n", "continuous", "continuous"),
    stat_name = c("N", "N", "mean"), stat_label = c("N", "N", "Mean"),
    stat = c(254, 80, 12.5), stringsAsFactors = FALSE)
  n <- normalize_ard(d)
  expect_identical(names(n)[1], "variable")
  expect_identical(attr(n, "ard_total_n", exact = TRUE), 254)
  expect_identical(nrow(n), 2L)
  # group columns present but all NA: still no key column
  d$group1 <- NA_character_
  d$group1_level <- NA_character_
  m <- normalize_ard(d)
  expect_identical(names(m)[1], "variable")
  expect_identical(attr(m, "ard_total_n", exact = TRUE), 254)
})

test_that("FT-AR3016 [FR-AR130]: a plain data frame is read like the cards ARD, cards or not", {
  skip_if_not_installed("cards")
  dat <- data.frame(AGE = c(10, 20, 33, 41), TRT = c("A", "B", "A", "B"))
  ard <- cards::ard_summary(
    dat, by = TRT, variables = AGE,
    statistic = ~ cards::continuous_summary_fns(c("N", "mean")))
  plain <- data.frame(
    group1 = "TRT", group1_level = c("A", "A", "B", "B"),
    variable = "AGE", variable_level = NA_character_, context = "summary",
    stat_name = c("N", "mean", "N", "mean"),
    stat_label = c("N", "Mean", "N", "Mean"),
    stat = c(2, 21.5, 2, 30.5), stat_fmt = c("2", "21.5", "2", "30.5"),
    stringsAsFactors = FALSE)
  expect_false(any(vapply(plain, is.list, logical(1))))
  expect_identical(class(plain), "data.frame")
  cols <- c("TRT", "variable", "variable_level", "context", "stat_name",
            "stat_label", "stat", "stat_fmt", ".kind", ".depth", ".label",
            ".overall", ".key_own")
  from_cards <- normalize_ard(ard)
  expect_identical(normalize_ard(plain)[cols], from_cards[cols])
})

test_that("FT-AR3017 [FR-AR131]: hierarchical ARD with a separate overall block: arm key filled", {
  skip_if_not_installed("cards")
  adsl <- data.frame(USUBJID = paste0("S", 1:6),
                     TRT = c("A", "A", "A", "B", "B", "B"))
  adae <- data.frame(USUBJID = c("S1", "S2", "S4", "S5", "S5"),
                     TRT = c("A", "A", "B", "B", "B"),
                     AESOC = c("Cardiac", "Cardiac", "Cardiac", "Eye", "Eye"),
                     AETERM = c("Palp", "Palp", "Palp", "Dry", "Itch"))
  ard <- cards::ard_stack_hierarchical(
    adae, variables = c(AESOC, AETERM), by = TRT, denominator = adsl,
    id = USUBJID, over_variables = TRUE)
  # the arm is the analysed variable of a separately built overall block
  ov <- cards::ard_tabulate(adsl[adsl$USUBJID %in% adae$USUBJID, ],
                            variables = TRT, denominator = adsl,
                            statistic = ~ c("n", "p"))
  both <- suppressMessages(cards::bind_ard(ard, ov, .update = TRUE))
  d <- normalize_ard(both, hierarchy = c("AESOC", "AETERM"),
                     overall = overall_row("Any TEAE", from = "TRT"))
  rows <- d$variable == "TRT"
  expect_true(any(rows))
  expect_identical(d$TRT[rows], d$variable_level[rows])
  expect_setequal(d$TRT[rows], c("A", "B"))
  expect_true(all(d$.overall[rows]))
  expect_true(all(d$AESOC[rows] == "Any TEAE"))
  # the cards sentinel rows are overall rows too
  expect_true(all(d$.overall[d$variable == "..ard_hierarchical_overall.."]))
})

test_that("FT-AR3018 [FR-AR136]: a stack that is not a named list of character layers stops", {
  d <- .g1_base()
  msg <- "`stack` is a named list: a new key's name and its layers, a variable each"
  expect_error(normalize_ard(d, stack = list(g = character())), msg, fixed = TRUE)
  expect_error(normalize_ard(d, stack = list("SEX")), msg, fixed = TRUE)
  expect_error(normalize_ard(d, stack = list(g = 1)), msg, fixed = TRUE)
})

# ------------------------------------------------------------- widen_ard

# SEX (F, M, U) counted in two arms: n and p per level.  Arm B has no U.
.g1_cat <- function(first = c("A", "B")) {
  mk <- function(arm, lv, n, p) {
    data.frame(group1 = "TRT", group1_level = arm, variable = "SEX",
               variable_level = lv, context = "categorical",
               stat_name = c("n", "p"), stat_label = c("n", "%"),
               stat = c(n, p), stringsAsFactors = FALSE)
  }
  d <- rbind(mk(first[1], "F", 4, 0.4), mk(first[1], "M", 5, 0.5),
             mk(first[1], "U", 1, 0.1), mk(first[2], "F", 3, 0.25),
             mk(first[2], "M", 6, 0.5), mk(first[2], "U", 0, 0))
  d
}

.g1_cells <- function() {
  list(continuous = c("Mean" = "{mean:.1f}"), categorical = "{n:.0f}")
}

test_that("FT-AR3019 [FR-AR143]: widen_ard() gives a plain data.frame of character columns", {
  d <- normalize_ard(.g1_cat())
  tbl <- widen_ard(d, cols = "TRT", rows = c(grp = "variable"), cells = "{n:.0f}",
                   notes = FALSE)
  expect_identical(class(tbl), "data.frame")
  expect_identical(names(tbl), c("grp", "label", "A", "B"))
  expect_true(all(vapply(tbl[c("A", "B")], is.character, logical(1))))
  # one ARD record (a level of SEX) is one table row
  expect_identical(nrow(tbl), 3L)
  expect_identical(tbl$label, c("F", "M", "U"))
  expect_identical(tbl$A, c("4", "5", "1"))
  expect_identical(tbl$B, c("3", "6", "0"))
})

test_that("FT-AR3020 [FR-AR146]: rows left out are reported in the notes", {
  d <- normalize_ard(.g1_cat())
  one <- d[d$stat_name == "n" & d$TRT == "A" & d$variable_level == "F", ]
  # a statistic no template names, a record with no value for the cols key,
  # and a key variable's own row
  extra <- rbind(
    transform(one, stat_name = "median"),
    transform(one, TRT = NA_character_),
    transform(one, .key_own = TRUE, stat_name = "N"))
  msgs <- paste(capture_messages(
    widen_ard(rbind(d, extra), cols = "TRT", cells = "{n:.0f}",
              notes = "applied")), collapse = "")
  # (the six p rows no template names are among the nine)
  expect_match(msgs, "widen_ard(): 9 ARD rows were not used", fixed = TRUE)
  expect_match(msgs, "median +1 +no template named it")
  expect_match(msgs, "p +6 +no template named it")
  expect_match(msgs, "N +1 +a key variable's own tabulation")
  expect_match(msgs, "n +1 +no value for a `cols` key")
  # a statistic in no stat group is reported too
  msgs2 <- paste(capture_messages(
    widen_ard(d, cols = "TRT", cells = "{n:.0f}", stat_groups = list(count = "n"),
              notes = "applied")), collapse = "")
  expect_match(msgs2, "in no `stat_groups` group", fixed = TRUE)
  expect_match(msgs2, "6 ARD rows were not used", fixed = TRUE)
})

test_that("FT-AR3021 [FR-AR150]: column order: first seen, `levels` or factor; a name is ignored", {
  d <- normalize_ard(.g1_cat(first = c("B", "A")))
  tbl <- widen_ard(d, cols = "TRT", cells = "{n:.0f}", notes = FALSE)
  expect_identical(names(tbl), c("label", "B", "A"))
  expect_identical(tbl$B, c("4", "5", "1"))
  # a name given to the element is not the column text
  named <- widen_ard(d, cols = c(Arm = "TRT"), cells = "{n:.0f}", notes = FALSE)
  expect_identical(names(named), c("label", "B", "A"))
  expect_false("Arm" %in% names(named))
  # `levels` fixes the order
  lv <- widen_ard(d, cols = "TRT", cells = "{n:.0f}", levels = list(TRT = c("A", "B")),
                  notes = FALSE)
  expect_identical(names(lv), c("label", "A", "B"))
  # a factor key goes in its levels
  d$TRT <- factor(d$TRT, levels = c("A", "B"))
  fct <- widen_ard(d, cols = "TRT", cells = "{n:.0f}", notes = FALSE)
  expect_identical(names(fct), c("label", "A", "B"))
})

test_that("FT-AR3022 [FR-AR153]: rows keys are named by name, column, a leading dot dropped", {
  d <- normalize_ard(.g1_hier(), hierarchy = .g1_hier_cols, overall = "Any TEAE")
  tbl <- widen_ard(d, cols = "TRT", rows = c("AESOC", a = "AETERM", ".depth"),
                   cells = "{n:.0f}", notes = FALSE)
  expect_identical(names(tbl), c("AESOC", "a", "depth", "label", "A", "B"))
  expect_identical(as.character(tbl$AESOC), c("Any TEAE", "Cardiac", "Cardiac"))
  expect_identical(as.character(tbl$a), c(NA, NA, "Palpitations"))
  expect_identical(as.character(tbl$depth), c("1", "1", "2"))
  expect_identical(tbl$label, c("Any TEAE", "Cardiac", "Palpitations"))
  expect_identical(tbl$A, c("5", "3", "2"))
  # a key written as the name of a column
  v <- widen_ard(normalize_ard(.g1_base()), cols = "TRT",
                 rows = c(group = "variable"), cells = .g1_cells(), notes = FALSE)
  expect_identical(names(v)[1:2], c("group", "label"))
  expect_identical(v$group, c("AGE", "SEX"))
})

test_that("FT-AR3023 [FR-AR154]: rows = NULL: `group` for several variables, none with hierarchy", {
  flat <- widen_ard(normalize_ard(.g1_base()), cols = "TRT", cells = .g1_cells(),
                    notes = FALSE)
  expect_identical(names(flat), c("group", "label", "A", "B"))
  expect_identical(flat$group, c("AGE", "SEX"))
  # one analysis variable: no row-key column
  one <- widen_ard(normalize_ard(.g1_cat()), cols = "TRT", cells = "{n:.0f}",
                   notes = FALSE)
  expect_identical(names(one), c("label", "A", "B"))
  # a hierarchy (.depth not all NA): none, though there are several variables
  h <- normalize_ard(.g1_hier(), hierarchy = .g1_hier_cols, overall = "Any TEAE")
  expect_true(length(unique(h$variable)) > 1L)
  tbl <- widen_ard(h, cols = "TRT", cells = "{n:.0f}", notes = FALSE)
  expect_identical(names(tbl), c("label", "A", "B"))
  expect_identical(tbl$label, c("Any TEAE", "Cardiac", "Palpitations"))
})

test_that("FT-AR3024 [FR-AR156]: the default label is the .label column, named by the element", {
  d <- normalize_ard(.g1_hier(), hierarchy = .g1_hier_cols, overall = "Any TEAE")
  tbl <- widen_ard(d, cols = "TRT", cells = "{n:.0f}", notes = FALSE)
  expect_identical(tbl$label, c("Any TEAE", "Cardiac", "Palpitations"))
  expect_identical(tbl$label, unique(d$.label[d$TRT == "A" | d$.overall]))
  named <- widen_ard(d, cols = "TRT", label = c(Term = ".label"), cells = "{n:.0f}",
                     notes = FALSE)
  expect_identical(names(named), c("Term", "A", "B"))
  expect_identical(named$Term, tbl$label)
  # a flat ARD: the label is variable_level
  f <- normalize_ard(.g1_cat())
  flat <- widen_ard(f, cols = "TRT", cells = "{n:.0f}", notes = FALSE)
  expect_identical(flat$label, c("F", "M", "U"))
  expect_identical(flat$label, unique(f$.label))
})

test_that("FT-AR3025 [FR-AR159]: a label chain interpolates columns; a string names the source", {
  x <- .g1_cat()
  x$SITE <- c("S1", "S2", "S3")[match(x$variable_level, c("F", "M", "U"))]
  d <- normalize_ard(x)
  # a plain string names the source column of the label
  src <- widen_ard(d, cols = "TRT", cells = "{n:.0f}", label = c(Site = "SITE"),
                   notes = FALSE)
  expect_identical(names(src), c("Site", "A", "B"))
  expect_identical(src$Site, c("S1", "S2", "S3"))
  # the first element whose guard holds wins; {column} is interpolated and
  # {.label} is the label the row would carry
  tbl <- widen_ard(
    d, cols = "TRT", cells = "{n:.0f}", notes = FALSE,
    label = c(.label %in% "F" ~ "{SITE}: {.label}", ~ "{.label}"))
  expect_identical(tbl$label, c("S1: F", "M", "U"))
  expect_identical(tbl$A, c("4", "5", "1"))
  # the template over the default label, with another column interpolated
  t2 <- widen_ard(d, cols = "TRT", cells = "{n:.0f}", notes = FALSE,
                  label = c(.label %in% c("M", "U") ~ "  {.label} ({SITE})", ~ "{.label}"))
  expect_identical(t2$label, c("F", "  M (S2)", "  U (S3)"))
  # a plain string inside the chain names the source column: the rows its
  # guard does not reach keep that column's value
  t3 <- widen_ard(d, cols = "TRT", cells = "{n:.0f}", notes = FALSE,
                  label = c(.label %in% "S1" ~ "{variable_level} at {.label}", "SITE"))
  expect_identical(t3$label, c("F at S1", "S2", "S3"))
  expect_identical(t3$A, c("4", "5", "1"))
})

test_that("FT-AR3026 [FR-AR160]: default cells is {n} ({p}); a chain tries the templates in turn", {
  d <- normalize_ard(.g1_cat())
  tbl <- widen_ard(d, cols = "TRT", notes = FALSE)
  expect_identical(tbl$label, c("F", "M", "U"))
  expect_identical(tbl$A, c("4 (0.4)", "5 (0.5)", "1 (0.1)"))
  expect_identical(tbl$B, c("3 (0.25)", "6 (0.5)", "0 (0)"))
  # a chain: the first template where every token resolves, else the next
  b <- normalize_ard(.g1_base())
  ch <- widen_ard(b, cols = "TRT", rows = c(group = "variable"),
                  cells = c("{n} ({p})", "{mean}"), notes = FALSE)
  expect_identical(ch$group, c("AGE", "SEX"))
  expect_identical(ch$A, c("12.3", "4 (0.4)"))
  expect_identical(ch$B, c("15.7", "6 (0.5)"))
})

test_that("FT-AR3027 [FR-AR161]: guards are judged over the record's statistics and keys", {
  d <- normalize_ard(.g1_cat())
  tbl <- widen_ard(d, cols = "TRT", cells = c(n == 0 ~ "0", "{n} ({p})"),
                   notes = FALSE)
  expect_identical(tbl$B, c("3 (0.25)", "6 (0.5)", "0"))
  expect_identical(tbl$A, c("4 (0.4)", "5 (0.5)", "1 (0.1)"))
  # a guard on the keys: .label and .kind
  g <- widen_ard(d, cols = "TRT", notes = FALSE,
                 cells = c(.label == "M" ~ "male {n}", .kind == "categorical" ~ "cat {n}"))
  expect_identical(g$A, c("cat 4", "male 5", "cat 1"))
  # a guard on .depth
  h <- normalize_ard(.g1_hier(), hierarchy = .g1_hier_cols, overall = "Any TEAE")
  dp <- widen_ard(h, cols = "TRT", notes = FALSE,
                  cells = c(.depth == 2 ~ "  {n}", "{n}"))
  expect_identical(dp$A, c("5", "3", "  2"))
  # a template with an unresolved token moves on to the next element
  un <- widen_ard(d, cols = "TRT", cells = c("{n} ({nope})", "{n}"), notes = FALSE)
  expect_identical(un$A, c("4", "5", "1"))
  # a false guard moves on too
  fg <- widen_ard(d, cols = "TRT", cells = c(n > 100 ~ "big", "{n}"), notes = FALSE)
  expect_identical(fg$A, c("4", "5", "1"))
})

test_that("FT-AR3028 [FR-AR163]: a cells map is looked up by variable, context, kind, default", {
  mk <- function(var, ctx, vl = NA_character_, sn = "mean", st = c(12.34, 15.67)) {
    data.frame(group1 = "TRT", group1_level = c("A", "B"), variable = var,
               variable_level = vl, context = ctx, stat_name = sn,
               stat_label = sn, stat = st, stringsAsFactors = FALSE)
  }
  x <- rbind(mk("AGE", "summary"), mk("WT", "summary"),
             mk("SEX", "tabulate", "F", "n", c(4, 3)))
  d <- normalize_ard(x)
  # the variable first, then its context, then "default"
  cells <- list(AGE = "var {mean:.0f}", summary = "ctx {mean:.1f}",
                default = "def {n}")
  tbl <- widen_ard(d, cols = "TRT", rows = c(group = "variable"), cells = cells,
                   notes = FALSE)
  expect_identical(tbl$group, c("AGE", "WT", "SEX"))
  expect_identical(tbl$A, c("var 12", "ctx 12.3", "def 4"))
  expect_identical(tbl$B, c("var 16", "ctx 15.7", "def 3"))
  # a context cards has never used still matches by kind (continuous: no levels)
  ht <- normalize_ard(mk("HT", "newctx"))
  k1 <- widen_ard(ht, cols = "TRT", notes = FALSE,
                  cells = list(continuous = "kind {mean:.2f}", default = "def"))
  expect_identical(k1$A, "kind 12.34")
  k2 <- widen_ard(ht, cols = "TRT", notes = FALSE,
                  cells = list(categorical = "kind {mean:.2f}", default = "def"))
  expect_identical(k2$A, "def")
  lv <- normalize_ard(mk("LV", "newctx", "L", "n", c(4, 3)))
  k3 <- widen_ard(lv, cols = "TRT", notes = FALSE,
                  cells = list(categorical = "kind {n}", default = "def"))
  expect_identical(k3$A, "kind 4")
  # the context before the kind: a summary of a variable that has levels
  sm <- normalize_ard(mk("LV", "summary", "L", "n", c(4, 3)))
  expect_identical(sm$.kind[1], "categorical")
  c1 <- widen_ard(sm, cols = "TRT", notes = FALSE,
                  cells = list(summary = "ctx {n}", categorical = "kind {n}"))
  expect_identical(c1$A, "ctx 4")
  # the cards spellings of a context are one: `continuous` is `summary` ...
  c2 <- widen_ard(sm, cols = "TRT", notes = FALSE,
                  cells = list(continuous = "ctx {n}", categorical = "kind {n}"))
  expect_identical(c2$A, "ctx 4")
  # ... and `tabulate` is `categorical`
  ct <- normalize_ard(mk("CT", "categorical"))
  expect_identical(ct$.kind[1], "continuous")
  c3 <- widen_ard(ct, cols = "TRT", notes = FALSE,
                  cells = list(tabulate = "t {mean:.0f}", continuous = "kind {mean:.0f}"))
  expect_identical(c3$A, "t 12")
})

test_that("FT-AR3029 [FR-AR164]: cell_rows() puts each named line on the row of its label", {
  d <- normalize_ard(.g1_base())
  sex <- d[d$variable == "SEX", ]
  tbl <- widen_ard(sex, cols = "TRT", notes = FALSE,
                   cells = list(categorical = cell_rows(
                     "1" = c(n == 0 ~ "-", "{n:.0f}"),
                     "2" = "{p:.1f%}")))
  expect_identical(tbl$label, c("1", "2"))
  expect_identical(tbl$A, c("4", "40.0"))
  expect_identical(tbl$B, c("6", "50.0"))
  # an unnamed line takes its label from `label`, as a bare template does
  un <- widen_ard(sex, cols = "TRT", notes = FALSE,
                  cells = list(categorical = cell_rows("{n:.0f}", "2" = "{p:.1f%}")))
  expect_identical(un$label, c("F", "2"))
  expect_identical(un$A, c("4", "40.0"))
  # a continuous variable: each line reads the whole record
  age <- widen_ard(d[d$variable == "AGE", ], cols = "TRT", notes = FALSE,
                   cells = list(continuous = cell_rows(
                     "1" = "{N:.0f}", "2" = c(mean > 100 ~ "big", "{mean:.1f}"))))
  expect_identical(age$label, c("1", "2"))
  expect_identical(age$A, c("10", "12.3"))
  expect_identical(age$B, c("12", "15.7"))
})

# A flat frame keyed by its own columns: TRT (P, Q) and a row key SOC (A, B, Z),
# one count n a cell (1..6, so every cell has its own value).
.g1_keyed <- function(stat_name = "n") {
  d <- data.frame(
    TRT = rep(c("P", "Q"), 3L), SOC = rep(c("A", "B", "Z"), each = 2L),
    variable = "AE", variable_level = NA_character_, context = "categorical",
    stat_name = stat_name, stat_label = stat_name, stat = 1:6,
    stringsAsFactors = FALSE)
  normalize_ard(d, keys = c("TRT", "SOC"))
}

# One record per statistic, to try the token forms on exact values.
.g1_tokens <- function() {
  d <- data.frame(
    group1 = "TRT", group1_level = "A", variable = "V",
    variable_level = NA_character_, context = "continuous",
    stat_name = c("x", "p"), stat_label = c("x", "p"),
    stat = c(12.345, 0.256), stat_fmt = c("12.3", "25.6"),
    stringsAsFactors = FALSE)
  normalize_ard(d)
}

test_that("FT-AR3030 [FR-AR165]: the token forms give their exact values", {
  d <- .g1_tokens()
  one <- function(cells) widen_ard(d, cols = "TRT", cells = cells, notes = FALSE)$A
  expect_identical(one("{x}"), "12.3")           # stat_fmt when there is one
  expect_identical(one("{x:.1f}"), "12.3")
  expect_identical(one("{x:.3s}"), "12.3")
  expect_identical(one("{x:d}"), "12")
  expect_identical(one("{p:.1f%}"), "25.6")      # a % suffix multiplies by 100
  expect_identical(one("{x:stat}"), "12.345")
  expect_identical(one("{x:stat_fmt}"), "12.3")
  expect_identical(one("{p:stat_fmt}"), "25.6")
  # without a stat_fmt, {x} is the stat
  d$stat_fmt <- NA_character_
  expect_identical(one("{x}"), "12.345")
  expect_identical(one("{x:.2f}"), "12.35")
})

test_that("FT-AR3031 [FR-AR166]: stats = 'rows' ignores cells and labels by stat_label", {
  d <- normalize_ard(.g1_base())
  age <- d[d$variable == "AGE", ]
  rows <- widen_ard(age, cols = "TRT", stats = "rows", notes = FALSE)
  expect_identical(names(rows), c("label", "A", "B"))
  expect_identical(rows$label, c("N", "Mean"))
  expect_identical(rows$A, c(10, 12.34))
  expect_identical(rows$B, c(12, 15.67))
  # cells is ignored
  expect_identical(
    widen_ard(age, cols = "TRT", stats = "rows", cells = "{N}", notes = FALSE), rows)
  # the default: character cells built from `cells`
  cell <- widen_ard(age, cols = "TRT", cells = "{mean:.1f}", notes = FALSE)
  expect_true(is.character(cell$A))
  expect_identical(cell$A, "12.3")
  # a label naming a column other than .label: the rows are labelled by it
  by_name <- widen_ard(age, cols = "TRT", stats = "rows", label = c(l = "stat_name"),
                       notes = FALSE)
  expect_identical(names(by_name), c("l", "A", "B"))
  expect_identical(by_name$l, c("N", "mean"))
})

test_that("FT-AR3032 [FR-AR168]: levels order a row key by output name, label rows, variables", {
  x <- data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L),
    group2 = "SEX", group2_level = rep(c("F", "M"), 2L),
    variable = "AGE", variable_level = NA_character_, context = "continuous",
    stat_name = "mean", stat_label = "Mean", stat = c(1, 2, 3, 4),
    stringsAsFactors = FALSE)
  d <- normalize_ard(x)
  plain <- widen_ard(d, cols = "TRT", rows = c(sx = "SEX"), cells = "{mean:.0f}",
                     notes = FALSE)
  expect_identical(plain$sx, c("F", "M"))
  # the row key's output name: orders the rows and makes a factor
  by_out <- widen_ard(d, cols = "TRT", rows = c(sx = "SEX"), cells = "{mean:.0f}",
                      levels = list(sx = c("M", "F")), notes = FALSE)
  expect_true(is.factor(by_out$sx))
  expect_identical(levels(by_out$sx), c("M", "F"))
  expect_identical(as.character(by_out$sx), c("M", "F"))
  expect_identical(by_out$A, c("2", "1"))
  # its source name does the same
  by_src <- widen_ard(d, cols = "TRT", rows = c(sx = "SEX"), cells = "{mean:.0f}",
                      levels = list(SEX = c("M", "F")), notes = FALSE)
  expect_identical(as.character(by_src$sx), c("M", "F"))
  # an analysis variable's levels order its rows in the label column
  cat <- normalize_ard(.g1_cat())
  lv <- widen_ard(cat, cols = "TRT", cells = "{n:.0f}",
                  levels = list(SEX = c("U", "M", "F")), notes = FALSE)
  expect_identical(as.character(lv$label), c("U", "M", "F"))
  expect_identical(lv$A, c("1", "5", "4"))
  # the analysis variables themselves, when the name is `variable`
  b <- normalize_ard(.g1_base())
  vr <- widen_ard(b, cols = "TRT", rows = c(group = "variable"), cells = .g1_cells(),
                  levels = list(variable = c("SEX", "AGE")), notes = FALSE)
  expect_identical(as.character(vr$group), c("SEX", "AGE"))
})

test_that("FT-AR3033 [FR-AR169]: a level the ARD does not have is said, whatever notes says", {
  d <- normalize_ard(.g1_cat())
  msg <- "widen_ard(): `levels` names values the ARD does not have; they make no row or column:"
  got <- capture_messages(
    tbl <- widen_ard(d, cols = "TRT", cells = "{n:.0f}", notes = FALSE,
                     levels = list(TRT = c("A", "nope"))))
  expect_true(startsWith(got[1L], msg))
  expect_match(got[1L], "TRT: \"nope\"", fixed = TRUE)
  expect_identical(names(tbl), c("label", "A", "B"))
  expect_false("nope" %in% names(tbl))
  # a row level, and a value the ARD has (even as `labels` prints it): said or not
  got2 <- capture_messages(
    widen_ard(d, cols = "TRT", cells = "{n:.0f}", notes = FALSE,
              levels = list(SEX = c("F", "X"))))
  expect_true(startsWith(got2[1L], msg))
  expect_match(got2[1L], "SEX: \"X\"", fixed = TRUE)
  expect_identical(
    capture_messages(widen_ard(d, cols = "TRT", cells = "{n:.0f}", notes = FALSE,
                               levels = list(TRT = c("B", "A")))),
    character())
})

test_that("FT-AR3034 [FR-AR171]: labels scope: output name, then source name; .default the rest", {
  x <- data.frame(
    TRT = rep(c("P", "Q"), 4L), GRADE = rep(c("0", "1"), each = 4L),
    VISIT = rep(rep(c("0", "1"), each = 2L), 2L),
    variable = "AE", variable_level = NA_character_, context = "categorical",
    stat_name = "n", stat_label = "n", stat = 1:8, stringsAsFactors = FALSE)
  d <- normalize_ard(x, keys = c("TRT", "GRADE", "VISIT"))
  w <- function(labels) {
    widen_ard(d, cols = "TRT", rows = c(g = "GRADE", v = "VISIT"), cells = "{n:.0f}",
              labels = labels, notes = FALSE)
  }
  # the source name, when the output name has no scope
  src <- w(list(GRADE = c("0" = "Zero")))
  expect_identical(as.character(src$g), c("Zero", "Zero", "1", "1"))
  expect_identical(as.character(src$v), c("0", "1", "0", "1"))
  # the output name first
  out <- w(list(g = c("0" = "OutZero"), GRADE = c("0" = "SrcZero")))
  expect_identical(as.character(out$g), c("OutZero", "OutZero", "1", "1"))
  # .default covers the columns with no scope
  def <- w(list(GRADE = c("0" = "Zero"), .default = c("0" = "Other")))
  expect_identical(as.character(def$g), c("Zero", "Zero", "1", "1"))
  expect_identical(as.character(def$v), c("Other", "1", "Other", "1"))
  # a plain value beside a scope, told apart by shape
  mix <- w(list(GRADE = c("0" = "Zero"), TRT = "Arm"))
  expect_identical(as.character(mix$g), c("Zero", "Zero", "1", "1"))
})

test_that("FT-AR3035 [FR-AR173]: sort = FALSE keeps first-seen order; a factor key moves", {
  x <- data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L),
    group2 = "SEX", group2_level = rep(c("M", "F"), 2L),
    variable = "AGE", variable_level = NA_character_, context = "continuous",
    stat_name = "mean", stat_label = "Mean", stat = c(1, 2, 3, 4),
    stringsAsFactors = FALSE)
  d <- normalize_ard(x)
  # a character key out of alphabetical order (M then F): first-seen kept
  chr <- widen_ard(d, cols = "TRT", rows = c(sx = "SEX"), cells = "{mean:.0f}",
                   notes = FALSE)
  expect_identical(chr$sx, c("M", "F"))
  expect_identical(chr$A, c("1", "2"))
  # a factor key moves to its levels order, even with sort = FALSE
  d$SEX <- factor(d$SEX, levels = c("F", "M"))
  fct <- widen_ard(d, cols = "TRT", rows = c(sx = "SEX"), cells = "{mean:.0f}",
                   sort = FALSE, notes = FALSE)
  expect_identical(as.character(fct$sx), c("F", "M"))
  expect_identical(fct$A, c("2", "1"))
})

test_that("FT-AR3036 [FR-AR176]: sort = levels(soc) puts the values plan_levels() names first", {
  d <- .g1_keyed()
  tbl <- widen_ard(d, cols = "TRT", rows = c(soc = "SOC"), cells = "{n:.0f}",
                   levels = list(soc = c("Z", "A")), sort = "levels(soc)", notes = FALSE)
  expect_identical(as.character(tbl$soc), c("Z", "A", "B"))
  expect_identical(tbl$P, c("5", "1", "3"))
  # without the sort: the levels still order the rows, the unnamed value last
  plain <- widen_ard(d, cols = "TRT", rows = c(soc = "SOC"), cells = "{n:.0f}",
                     notes = FALSE)
  expect_identical(as.character(plain$soc), c("A", "B", "Z"))
  # a value the levels do not name ties: the next key decides
  nxt <- widen_ard(d, cols = "TRT", rows = c(soc = "SOC"), cells = "{n:.0f}",
                   levels = list(soc = "Z"), sort = c("levels(soc)", "-n"),
                   notes = FALSE)
  expect_identical(as.character(nxt$soc), c("Z", "B", "A"))
})

test_that("FT-AR3037 [FR-AR178]: rounding = NULL reads the option; {x} does not round", {
  d <- data.frame(group1 = "TRT", group1_level = "A", variable = "V",
                  variable_level = NA_character_, context = "continuous",
                  stat_name = "mean", stat_label = "Mean", stat = 0.5,
                  stat_fmt = "0.5", stringsAsFactors = FALSE)
  d <- normalize_ard(d)
  one <- function(cells, ...) {
    widen_ard(d, cols = "TRT", cells = cells, rounding = NULL, notes = FALSE, ...)$A
  }
  old <- options(rtfreporter.rounding = "r")
  on.exit(options(old), add = TRUE)
  expect_identical(one("{mean:.0f}"), "0")            # half to even
  expect_identical(one("{mean}"), "0.5")
  expect_identical(one("{mean:stat_fmt}"), "0.5")
  options(rtfreporter.rounding = "sas")
  expect_identical(one("{mean:.0f}"), "1")            # half away from zero
  expect_identical(one("{mean}"), "0.5")
  expect_identical(one("{mean:stat_fmt}"), "0.5")
  # a named rounding wins over the option
  expect_identical(
    widen_ard(d, cols = "TRT", cells = "{mean:.0f}", rounding = "r", notes = FALSE)$A,
    "0")
  options(rtfreporter.rounding = "r")
  expect_identical(
    widen_ard(d, cols = "TRT", cells = "{mean:.0f}", rounding = "sas", notes = FALSE)$A,
    "1")
})

test_that("FT-AR3038 [FR-AR183]: stat_groups key the records; a statistic in none is unread", {
  d <- normalize_ard(.g1_base())
  d <- d[d$variable == "SEX", ]
  # SEX has n and p; one group holds n, p is in no group
  msgs <- paste(capture_messages(
    widen_ard(d, cols = "TRT", cells = "{n:.0f}", stat_groups = list(g = "n"),
              notes = "applied")), collapse = "")
  expect_match(msgs, "p +2 +in no `stat_groups` group")
  # cols / rows name .stat_group like any other key
  b <- normalize_ard(.g1_base())
  b <- b[b$variable == "SEX", ]
  tbl <- widen_ard(b, cols = "TRT", rows = ".stat_group",
                   stat_groups = list(count = "n", pct = "p"),
                   cells = list(count = "{n:.0f}", pct = "{p:.1f%}"), notes = FALSE)
  expect_identical(names(tbl), c("stat_group", "label", "A", "B"))
  expect_identical(tbl$stat_group, c("count", "pct"))
  expect_identical(tbl$A, c("4", "40.0"))
  expect_identical(tbl$B, c("6", "50.0"))
  # the group as a column key
  cc <- widen_ard(b, cols = c("TRT", ".stat_group"),
                  stat_groups = list(count = "n", pct = "p"),
                  cells = list(count = "{n:.0f}", pct = "{p:.1f%}"), notes = FALSE)
  expect_identical(names(cc), c("label", "A____count", "A____pct", "B____count", "B____pct"))
  expect_identical(cc$A____pct, "40.0")
})

test_that("FT-AR3039 [FR-AR184]: filter drops rows whose records fail, keeps columns and overall", {
  x <- data.frame(
    TRT = rep(c("A", "B", "C"), 3L), TERM = rep(c("Any", "X", "Y"), each = 3L),
    variable = "AE", variable_level = NA_character_, context = "categorical",
    stat_name = "p", stat_label = "%",
    stat = c(0.01, 0.01, 0.01,      # Any: fails everywhere, but the overall row
             0.02, 0.01, 0.00,      # X: fails in every column
             0.20, 0.01, 0.01),     # Y: passes in A
    stringsAsFactors = FALSE)
  d <- normalize_ard(x, keys = c("TRT", "TERM"))
  d$.overall <- d$TERM == "Any"
  filt <- list(conds = list(quote(p >= 0.05)), any = TRUE, parents = "keep")
  w <- function(...) {
    widen_ard(d, cols = "TRT", rows = c(term = "TERM"), cells = "{p}", notes = FALSE, ...)
  }
  tbl <- w(filter = filt)
  expect_identical(tbl$term, c("Any", "Y"))
  # every value column stays, and a kept row shows its other columns' values
  expect_identical(names(tbl), c("term", "label", "A", "B", "C"))
  expect_identical(tbl$A, c("0.01", "0.2"))
  expect_identical(tbl$B, c("0.01", "0.01"))
  # NULL drops none
  expect_identical(w(filter = NULL)$term, c("Any", "X", "Y"))
  expect_identical(w()$term, c("Any", "X", "Y"))
  # any = FALSE: every column must pass
  none <- w(filter = list(conds = list(quote(p >= 0.05)), any = FALSE, parents = "keep"))
  expect_identical(none$term, "Any")
})

test_that("FT-AR3040 [FR-AR192]: a text statistic prints as text; guards compare numbers", {
  d <- data.frame(
    group1 = "TRT", group1_level = c("A", "B"), variable = "V",
    variable_level = NA_character_, context = "continuous", stat_name = "N",
    stat_label = "N", stringsAsFactors = FALSE)
  d$stat <- list(86, "few")
  n <- normalize_ard(d)
  expect_identical(n$stat, c(86, NA))
  expect_identical(widen_ard(n, cols = "TRT", cells = "{N}", notes = FALSE)$A, "86")
  expect_identical(widen_ard(n, cols = "TRT", cells = "{N}", notes = FALSE)$B, "few")
  # a guard compares numbers: N < 9 is false for 86, and for the text
  g <- widen_ard(n, cols = "TRT", cells = c(N < 9 ~ "<9", "{N}"), notes = FALSE)
  expect_identical(g$A, "86")
  expect_identical(g$B, "few")
  small <- d
  small$stat <- list(5, "few")
  s <- widen_ard(normalize_ard(small), cols = "TRT", cells = c(N < 9 ~ "<9", "{N}"),
                 notes = FALSE)
  expect_identical(s$A, "<9")
  expect_identical(s$B, "few")
})

test_that("FT-AR3041 [FR-AR196]: cols missing or naming no key stops, rtfreporter_missing_role", {
  d <- normalize_ard(.g1_base())
  expect_error(
    widen_ard(d, cells = .g1_cells()),
    "`cols` is required: the ARD has .TRT. to spread; name the column keys \\(cols = \"TRT\"\\)\\.")
  expect_error(widen_ard(d, cells = .g1_cells()), class = "rtfreporter_missing_role")
  # cols naming no key at all
  expect_error(
    widen_ard(d, cols = character(0), cells = .g1_cells()),
    "`cols` is required: name the key that goes across.", fixed = TRUE)
  expect_error(
    widen_ard(d, cols = character(0), cells = .g1_cells()),
    class = "rtfreporter_missing_role")
  # an ARD with no key at all has one column, All
  flat <- data.frame(variable = "AGE", variable_level = NA_character_,
                     context = "continuous", stat_name = "mean", stat_label = "Mean",
                     stat = 12.34, stringsAsFactors = FALSE)
  one <- widen_ard(normalize_ard(flat), cells = "{mean:.1f}", notes = FALSE)
  expect_identical(names(one), c("label", "All"))
  expect_identical(one$All, "12.3")
})

test_that("FT-AR3042 [FR-AR197]: a cols / rows / label that is no column stops", {
  d <- normalize_ard(.g1_cat())
  expect_error(
    widen_ard(d, cols = "TRT", rows = "NOPE", cells = "{n}"),
    "^`rows`: no column 'NOPE' in the normalized ARD. Available:")
  expect_error(
    widen_ard(d, cols = "NOPE", cells = "{n}"),
    "^`cols`: no column 'NOPE' in the normalized ARD. Available:")
  expect_error(
    widen_ard(d, cols = "TRT", label = "NOPE", cells = "{n}"),
    "^`label`: no column 'NOPE' in the normalized ARD. Available:")
  # the message lists the frame's columns
  expect_error(
    widen_ard(d, cols = "TRT", rows = "NOPE", cells = "{n}"),
    "Available: TRT, variable, variable_level", fixed = TRUE)
})

test_that("FT-AR3043 [FR-AR208]: rounding, stats and value outside their values stop", {
  d <- normalize_ard(.g1_cat())
  expect_error(
    widen_ard(d, cols = "TRT", rounding = "x"),
    paste0("`rounding` must be \"r\" (half to even, as base::round() does) or ",
           "\"sas\" (half away from zero, as SAS ROUND() does)."),
    fixed = TRUE)
  expect_error(widen_ard(d, cols = "TRT", stats = "x"), "'arg' should be one of")
  expect_error(widen_ard(d, cols = "TRT", value = "x"), "'arg' should be one of")
  expect_error(widen_ard(d, cols = "TRT", stats = "x"), "cells", fixed = TRUE)
  expect_error(widen_ard(d, cols = "TRT", value = "x"), "stat_fmt", fixed = TRUE)
})

test_that("FT-AR3044 [FR-AR211]: comparison records with no variable or stat_name stop", {
  d <- normalize_ard(.g1_cat())
  d$PARAM <- "ALT"
  odd <- d[1:2, ]
  odd$TRT <- NA_character_
  odd$variable <- NA_character_
  odd$stat_name <- NA_character_
  odd$context <- "comparison"
  odd$PARAM <- c("AST", "ALP")
  x <- rbind(d, odd)
  err <- tryCatch(
    widen_ard(x, cols = "TRT", cells = "{n:.0f}", notes = FALSE,
              compare = list(cols = list(p = "{p.value:.3f}"))),
    error = function(e) conditionMessage(e))
  expect_true(startsWith(
    err, "plan_compare(): 2 of the comparison records have no `variable` or no `stat_name`."))
  expect_match(err, "Their filled", fixed = TRUE)
  # the filled columns are listed (the first eight), not the empty ones
  filled <- sub(".*Their filled\\s+columns: ", "", err)
  expect_match(filled, "^variable_level, context, stat_label, stat, ")
  expect_false(grepl("stat_name", filled, fixed = TRUE))
  expect_false(grepl("TRT", filled, fixed = TRUE))
})

# ---------------------------------------------------------------- pull_ard

# AGE and WT each offer N (10 and 12 by arm A and B); wt_a is WT's N in arm A.
.g1_pull <- function(wt_a = 10) {
  data.frame(
    group1 = "TRT", group1_level = c("A", "B", "A", "B"),
    variable = c("AGE", "AGE", "WT", "WT"), variable_level = NA_character_,
    context = "continuous", stat_name = "N", stat_label = "N",
    stat = c(10, 12, wt_a, 12), stringsAsFactors = FALSE)
}

test_that("FT-AR3045 [FR-AR215]: pull_ard() answers when variables agree, refuses if they differ", {
  expect_identical(pull_ard(.g1_pull(), cols = "TRT"), c(A = 10, B = 12))
  err <- expect_error(pull_ard(.g1_pull(wt_a = 9), cols = "TRT"),
                      "This ARD offers more than one .N. for these columns")
  # the candidates are listed with their values, and a variable names one
  msg <- conditionMessage(err)
  expect_match(msg, "variable = .AGE. +context = .continuous. +stat = .N. +-> 10 / 12")
  expect_match(msg, "variable = .WT. +context = .continuous. +stat = .N. +-> 9 / 12")
  expect_identical(pull_ard(.g1_pull(wt_a = 9), cols = "TRT", variable = "WT"),
                   c(A = 9, B = 12))
  expect_identical(pull_ard(.g1_pull(wt_a = 9), cols = "TRT", variable = "AGE"),
                   c(A = 10, B = 12))
})

test_that("FT-AR3046 [FR-AR216]: pull_ard() names keys by grouping variable, or an analysed key", {
  # TRT in group2: named by its own name, not its position
  g2 <- data.frame(
    group1 = "SEX", group1_level = "F", group2 = "TRT",
    group2_level = c("A", "B"), variable = "AGE", variable_level = NA_character_,
    context = "continuous", stat_name = "N", stat_label = "N", stat = c(5, 7),
    stringsAsFactors = FALSE)
  expect_identical(pull_ard(g2, cols = "TRT"), c(A = 5, B = 7))
  expect_error(pull_ard(g2, cols = "group2"), "no key .group2. in this ARD")
  # TRT as the analysed variable of a separately built overall block
  ov <- data.frame(
    group1 = NA_character_, group1_level = NA_character_,
    variable = "TRT", variable_level = c("A", "B"), context = "categorical",
    stat_name = "n", stat_label = "n", stat = c(9, 11), stringsAsFactors = FALSE)
  expect_identical(pull_ard(ov, cols = "TRT", stat = "n", variable = "TRT"),
                   c(A = 9, B = 11))
  # a key's own rows are excluded unless named
  expect_error(pull_ard(ov, cols = "TRT", stat = "n"), "No .n. found for these columns.")
  # bound with an AE block: the arm names come back with that block's N
  ae <- data.frame(
    group1 = "TRT", group1_level = c("A", "B"), variable = "AESOC",
    variable_level = "Cardiac", context = "hierarchical", stat_name = "N",
    stat_label = "N", stat = c(20, 22), stringsAsFactors = FALSE)
  expect_identical(pull_ard(rbind(ov, ae), cols = "TRT"), c(A = 20, B = 22))
})

test_that("FT-AR3047 [FR-AR220]: pull_ard(levels = ) orders by levels, factor, first seen", {
  skip_if_not_installed("cards")
  dat <- data.frame(AGE = c(10, 20, 33, 41, 5, 6),
                    TRT = factor(c("A", "B", "A", "B", "B", "A"), levels = c("B", "A")))
  ard <- cards::ard_summary(
    dat, by = TRT, variables = AGE,
    statistic = ~ cards::continuous_summary_fns("N"))
  # a factor key: its levels (B, A), not the alphabet
  expect_identical(names(pull_ard(ard, cols = "TRT")), c("B", "A"))
  # levels = list(TRT = ) wins
  expect_identical(names(pull_ard(ard, cols = "TRT", levels = list(TRT = c("A", "B")))),
                   c("A", "B"))
  # a character key: first seen
  chr <- data.frame(
    group1 = "TRT", group1_level = c("B", "A"), variable = "AGE",
    variable_level = NA_character_, context = "continuous", stat_name = "N",
    stat_label = "N", stat = c(4, 2), stringsAsFactors = FALSE)
  expect_identical(pull_ard(chr, cols = "TRT"), c(B = 4, A = 2))
  expect_identical(pull_ard(chr, cols = "TRT", levels = list(TRT = c("A", "B"))),
                   c(A = 2, B = 4))
  # a plain vector for levels is not read
  expect_identical(names(pull_ard(chr, cols = "TRT", levels = c("A", "B"))), c("B", "A"))
  # two column keys: levels are matched on the joined names (FR-AR240), so
  # the levels of the first key alone match none and the order is unchanged
  two <- data.frame(
    group1 = "TRT", group1_level = c("A", "A", "B", "B"),
    group2 = "SEX", group2_level = c("F", "M", "F", "M"), variable = "AGE",
    variable_level = NA_character_, context = "continuous", stat_name = "N",
    stat_label = "N", stat = c(1, 2, 3, 4), stringsAsFactors = FALSE)
  expect_identical(
    pull_ard(two, cols = c("TRT", "SEX"), levels = list(TRT = c("B", "A"))),
    c(A____F = 1, A____M = 2, B____F = 3, B____M = 4))
  # levels naming the joined names (a crossed header) still order by them
  expect_identical(
    names(pull_ard(two, cols = c("TRT", "SEX"),
                   levels = list(TRT = c("B____M", "A____F")))),
    c("B____M", "A____F", "A____M", "B____F"))
})

test_that("FT-AR3048 [FR-AR225]: pull_ard(stat = nonesuch) stops, naming what the ARD offers", {
  err <- expect_error(pull_ard(.g1_pull(), cols = "TRT", stat = "nonesuch"),
                      "No .nonesuch. found for these columns\\.")
  msg <- conditionMessage(err)
  expect_match(msg, "key variables' own rows", fixed = TRUE)
  expect_match(msg, "are excluded unless you", fixed = TRUE)
  expect_match(msg, "name one with `variable =`", fixed = TRUE)
  # the candidates: variable, context, statistic and values
  expect_match(msg, "candidates, keyed by the value columns:", fixed = TRUE)
  expect_match(msg, "variable = .AGE. +context = .continuous. +stat = .N. +-> 10 / 12")
  expect_match(msg, "variable = .WT. +context = .continuous. +stat = .N. +-> 10 / 12")
})

# ------------------------------------------------------ overall_row, list_ard_keys

test_that("FT-AR3049 [FR-AR227]: overall_row() is a classed list of the label and from variables", {
  x <- overall_row("Any TEAE", from = "TRT")
  expect_s3_class(x, "overall_row")
  expect_identical(class(x), "overall_row")
  expect_true(is.list(x))
  expect_identical(unclass(x)$label, "Any TEAE")
  expect_identical(unclass(x)$from, "TRT")
  expect_identical(names(unclass(x)), c("label", "from"))
  y <- overall_row("Any TEAE", from = c("TRT", "ARM"))
  expect_identical(y$from, c("TRT", "ARM"))
  # no from: the cards sentinel
  z <- overall_row("Any TEAE")
  expect_identical(z$label, "Any TEAE")
  expect_identical(z$from, character(0))
  # a bare string given to normalize_ard(overall = ) is overall_row(label)
  h <- .g1_hier()
  expect_identical(
    normalize_ard(h, hierarchy = .g1_hier_cols, overall = "Any TEAE"),
    normalize_ard(h, hierarchy = .g1_hier_cols, overall = overall_row("Any TEAE")))
})

.g1_keys_ard <- function() {
  d <- data.frame(
    group1 = c("TRT", "TRT", "TRT", "TRT"), group1_level = c("A", "B", "A", "B"),
    group2 = c(NA, NA, "SEX", "SEX"), group2_level = c(NA, NA, "F", "F"),
    variable = c("AGE", "AGE", "WT", "WT"),
    variable_level = NA_character_,
    context = c("continuous", "continuous", "categorical", "categorical"),
    stat_name = c("N", "mean", "n", "p"), stat_label = c("N", "Mean", "n", "%"),
    stat = c(1, 2, 3, 4), stringsAsFactors = FALSE)
  # attributes that say otherwise: nothing is read from them
  attr(d, "grouping") <- "NOPE_GROUP"
  attr(d, "variables") <- "NOPE_VAR"
  d
}

test_that("FT-AR3050 [FR-AR233]: list_ard_keys() prints keys, variables, contexts and statistics", {
  out <- capture.output(list_ard_keys(.g1_keys_ard()))
  expect_identical(out[1L], "ARD keys (group1..groupN values) : TRT, SEX ")
  expect_identical(out[2L], "Analysis variables               : AGE, WT ")
  expect_identical(out[3L], "Contexts (cards-version specific): continuous, categorical ")
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "Statistics per context:", fixed = TRUE)
  expect_match(txt, "categorical +n +n")
  expect_match(txt, "categorical +p +%")
  expect_match(txt, "continuous +N +N")
  expect_match(txt, "continuous +mean +Mean")
  # the object's attributes are not read
  expect_false(grepl("NOPE", txt, fixed = TRUE))
})

test_that("FT-AR3051 [FR-AR234]: list_ard_keys() returns lists invisibly, stats sorted", {
  invisible(capture.output(vis <- withVisible(list_ard_keys(.g1_keys_ard()))))
  expect_false(vis$visible)
  x <- vis$value
  expect_identical(names(x), c("keys", "variables", "contexts", "kinds", "stats"))
  expect_identical(x$keys, c("TRT", "SEX"))
  expect_identical(x$variables, c("AGE", "WT"))
  expect_identical(x$contexts, c("continuous", "categorical"))
  expect_s3_class(x$kinds, "data.frame")
  expect_identical(x$kinds$variable, c("AGE", "WT"))
  expect_identical(x$kinds$kind, c("continuous", "continuous"))
  expect_identical(
    x$stats,
    data.frame(context = c("categorical", "categorical", "continuous", "continuous"),
               stat_name = c("n", "p", "N", "mean"),
               stat_label = c("n", "%", "N", "Mean"), stringsAsFactors = FALSE))
})

# ----------------------------------------- cells reached by different values

test_that("FT-AR3052 [FR-AR253]: stats = 'rows' stops on cells of different values", {
  x <- data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), 2L),
    variable = "SEX", variable_level = "F", context = "categorical",
    stat_name = "n", stat_label = "n", stat = c(4, 3, 7, 3),
    SITE = c("S1", "S1", "S2", "S2"), stringsAsFactors = FALSE)
  d <- normalize_ard(x)
  w <- NULL
  got <- withCallingHandlers(
    widen_ard(d, cols = "TRT", cells = "{n:.0f}", notes = FALSE),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
  # A / F is 4 (S1) and 7 (S2): the first printed; B / F is 3 twice: no warning
  expect_length(w, 1L)
  expect_true(startsWith(w, paste0(
    "widen_ard(): 1 cell is reached by records of different values, and a cell ",
    "prints one, so the others are dropped:")))
  expect_match(w, "cell   : label = 'F', column 'A'", fixed = TRUE)
  expect_match(w, "values : '4', '7'  ('n'; the first is printed)", fixed = TRUE)
  expect_match(w, "'SITE'", fixed = TRUE)
  expect_match(w, "to `rows` (or `cols`)", fixed = TRUE)
  expect_identical(got$A, "4")
  expect_identical(got$B, "3")
  # the same value twice, and the key in `rows`: quiet
  expect_no_warning(widen_ard(d[d$stat_name == "n" & d$TRT == "B", ], cols = "TRT",
                              cells = "{n:.0f}", notes = FALSE))
  expect_no_warning(widen_ard(d, cols = "TRT", rows = c(site = "SITE"),
                              cells = "{n:.0f}", notes = FALSE))
  # stats = "rows" is not a warning: it stops
  expect_error(
    suppressWarnings(widen_ard(d, cols = "TRT", stats = "rows", notes = FALSE)),
    "Two different values landed in the same cell")
  expect_no_warning(try(widen_ard(d, cols = "TRT", stats = "rows", notes = FALSE),
                        silent = TRUE))
  # a records plan stops too
  rec <- d
  attr(rec, ".ard_records") <- TRUE
  expect_error(widen_ard(rec, cols = "TRT", cells = "{n:.0f}", notes = FALSE),
               "Two rows reach one cell with different values of .n.")
})

# ------------------------------------------------------ the number formatters

.g1_nb <- " "
.g1_unbsp <- function(x) gsub(.g1_nb, " ", x, fixed = TRUE)
.g1_na_msg <- paste0("`na` must be a single string -- the text printed for a missing ",
                     "value (e.g. \"-\" or \"NA\").  \"\" (the default) leaves the cell empty.")

test_that("FT-FM3000 [FR-FM105]: round_num() takes integer or double and keeps the length", {
  expect_equal(round_num(1L:3L, 1), c(1, 2, 3))
  expect_length(round_num(1L:3L, 1), 3L)
  expect_equal(round_num(c(1.5, NA, Inf), 1), c(1.5, NA, Inf))
  expect_length(round_num(c(1.5, NA, Inf), 1), 3L)
  expect_identical(round_num(c(NA, Inf), 1, rounding = "sas"), c(NA, Inf))
  expect_identical(round_num(c(NA, Inf), 1, rounding = "r"), c(NA, Inf))
  expect_identical(round_num(c(NA, -Inf), 0, rounding = "sas"), c(NA, -Inf))
  expect_length(round_num(numeric(0), 1), 0L)
})

test_that("FT-FM3001 [FR-FM111]: round_num(digits = ) not a single non-negative integer stops", {
  msg <- "`round_num(digits = )` must be a single non-negative integer."
  expect_error(round_num(1, digits = NA), msg, fixed = TRUE)
  expect_error(round_num(1, digits = c(1, 2)), msg, fixed = TRUE)
  expect_error(round_num(1, digits = "1"), msg, fixed = TRUE)
  expect_error(round_num(1, digits = -1), msg, fixed = TRUE)
  expect_error(round_num(1, digits = TRUE), msg, fixed = TRUE)
  expect_error(round_num(1, digits = NULL), msg, fixed = TRUE)
  expect_error(round_num(1, digits = numeric(0)), msg, fixed = TRUE)
})

test_that("FT-FM3002 [FR-FM112]: round_num(x = ) that is not numeric stops, naming the classes", {
  expect_error(round_num("1"), "`round_num()` rounds numeric input; got character.",
               fixed = TRUE)
  expect_error(round_num(TRUE), "`round_num()` rounds numeric input; got logical.",
               fixed = TRUE)
  # several classes are joined with "/"
  expect_error(round_num(factor("a", ordered = TRUE)),
               "`round_num()` rounds numeric input; got ordered/factor.", fixed = TRUE)
  expect_error(round_num(as.POSIXct("2026-01-01", tz = "UTC")),
               "`round_num()` rounds numeric input; got POSIXct/POSIXt.", fixed = TRUE)
})

test_that("FT-FM3003 [FR-FM180]: a length-1 count is recycled against pct", {
  out <- fmt_count_pct(2L, c(0.10, 0.50, 0.95))
  expect_length(out, 3L)
  expect_identical(.g1_unbsp(out), c("  2 (10.0)", "  2 (50.0)", "  2 (95.0)"))
  # the count is a numeric vector of counts
  expect_identical(.g1_unbsp(fmt_count_pct(c(1, 22, 333), 0.5)),
                   c("  1 (50.0)", " 22 (50.0)", "333 (50.0)"))
})

test_that("FT-FM3004 [FR-FM181]: pct is a fraction in [0, 1]; a length-1 pct is recycled", {
  expect_identical(.g1_unbsp(fmt_count_pct(c(1L, 2L, 3L), 0.5)),
                   c("  1 (50.0)", "  2 (50.0)", "  3 (50.0)"))
  expect_identical(.g1_unbsp(fmt_count_pct(c(5L, 14L), c(0.053, 0.5))),
                   c("  5  (5.3)", " 14 (50.0)"))
  expect_identical(.g1_unbsp(fmt_count_pct(c(30L, 1L), c(1, 0))),
                   c(" 30  (100)", "  1  (0.0)"))
})

test_that("FT-FM3005 [FR-FM182]: the default pct_unit = fraction multiplies by 100", {
  expect_identical(.g1_unbsp(fmt_count_pct(1L, 0.5)), "  1 (50.0)")
  expect_identical(.g1_unbsp(fmt_count_pct(1L, 0.5, pct_unit = "fraction")), "  1 (50.0)")
  expect_identical(.g1_unbsp(fmt_count_pct(1L, 50, pct_unit = "percent")), "  1 (50.0)")
})

test_that("FT-FM3006 [FR-FM185]: nbsp replaces the padding spaces; the default is U+00A0", {
  expect_identical(fmt_count_pct(5L, 0.5),
                   "  5 (50.0)")
  expect_false(grepl(" ", fmt_count_pct(5L, 0.5), fixed = TRUE))
  expect_identical(fmt_count_pct(5L, 0.5, nbsp = " "), "  5 (50.0)")
  expect_identical(fmt_count_pct(5L, 0.5, nbsp = "."), "..5.(50.0)")
})

test_that("FT-FM3007 [FR-FM189]: an Inf or -Inf count prints as such, not as the na text", {
  expect_identical(.g1_unbsp(fmt_count_pct(-Inf, 0.5, na = "-")), "-Inf      ")
  expect_identical(.g1_unbsp(fmt_count_pct(Inf, 0.5, na = "-")), "Inf       ")
  expect_identical(.g1_unbsp(fmt_count_pct(c(-Inf, NA), 0.5, na = "-")),
                   c("-Inf      ", "  -       "))
})

test_that("FT-FM3008 [FR-FM200]: the count_pct alignment gives every n (xx.x) cell one width", {
  df <- data.frame(lab = c("a", "b", "c"),
                   a = c("5 (5.0)", "14 (50.0)", "30 (100.0)"),
                   stringsAsFactors = FALSE)
  out <- as_rtftables(df, align_count_pct = TRUE)[[1L]]$data
  expect_identical(.g1_unbsp(out$a), c("  5  (5.0)", " 14 (50.0)", " 30  (100)"))
  expect_identical(nchar(out$a), rep(10L, 3L))
  # the row-label column is not touched
  expect_identical(out$lab, df$lab)
})

test_that("FT-FM3009 [FR-FM202]: cell forms follow fmt_count_pct; surrounding whitespace ignored", {
  df <- data.frame(lab = c("a", "b", "c", "d"),
                   a = c(" 12 (100.0) ", "12 (100.0)", "0 (0.0)", "  7 ( 5.5)  "),
                   stringsAsFactors = FALSE)
  out <- .g1_unbsp(as_rtftables(df, align_count_pct = TRUE)[[1L]]$data$a)
  expect_identical(out[1:3], c(" 12  (100)", " 12  (100)", "  0       "))
  expect_identical(out[1L], out[2L])
  # a cell with white space inside the parentheses is not of the form: left as is
  expect_identical(out[4L], "  7 ( 5.5)  ")
})

test_that("FT-FM3010 [FR-FM205]: the count_pct alignment pads with U+00A0 by default", {
  df <- data.frame(lab = c("a", "b"), a = c("5 (5.0)", "14 (50.0)"),
                   stringsAsFactors = FALSE)
  out <- as_rtftables(df, align_count_pct = TRUE)[[1L]]$data$a
  expect_identical(out, c("  5  (5.0)", " 14 (50.0)"))
  expect_false(any(grepl(" ", out, fixed = TRUE)))
  expect_true(grepl(.g1_nb, out[1L], fixed = TRUE))
})

test_that("FT-FM3011 [FR-FM207]: the count_pct alignment right-justifies the na token", {
  x <- c("5 (33.3)", "-", "12 (100.0)", NA)
  out <- .g1_unbsp(rtfreporter:::.realign_count_pct(x, na = "-"))
  expect_identical(out, c("  5 (33.3)", "  -       ", " 12  (100)", "  -       "))
  df <- data.frame(lab = c("a", "b", "c", "d"), a = x, stringsAsFactors = FALSE)
  got <- as_rtftables(df, align_count_pct = TRUE, na = "-")[[1L]]$data$a
  expect_identical(.g1_unbsp(got), out)
})

test_that("FT-FM3012 [FR-FM218]: fmt_count_paren(nbsp = ) replaces the padding spaces", {
  x <- c("1 (1.2%)", "11 (3.6%)")
  plain <- fmt_count_paren(x, nbsp = " ")
  expect_identical(plain, c(" 1 (1.2%)", "11 (3.6%)"))
  def <- fmt_count_paren(x)
  expect_true(grepl(.g1_nb, def[1L], fixed = TRUE))
  expect_false(grepl(" ", def[1L], fixed = TRUE))
  expect_identical(def, gsub(" ", .g1_nb, plain, fixed = TRUE))
  expect_identical(fmt_count_paren(x, nbsp = "."), c(".1.(1.2%)", "11.(3.6%)"))
})

test_that("FT-FM3013 [FR-FM241]: an empty or whitespace-only cell is left as it is", {
  out <- fmt_right_align(c("5", "      ", "12"), nbsp = " ")
  expect_identical(out, c(" 5", "      ", "12"))
  # with the default padding the cell is the same to the eye (spaces are nbsp)
  def <- fmt_right_align(c("5", "      ", "12"))
  expect_identical(.g1_unbsp(def), c(" 5", "      ", "12"))
  expect_identical(def[1L], " 5")
  expect_identical(fmt_right_align(c("5", "", "12"), nbsp = " "), c(" 5", "", "12"))
})

test_that("FT-FM3014 [FR-FM244]: fmt_right_align(nbsp = ) is the padding, U+00A0 by default", {
  expect_identical(fmt_right_align(c("5", "120")), c("  5", "120"))
  expect_identical(fmt_right_align(c("5", "120"), nbsp = " "), c("  5", "120"))
  expect_identical(fmt_right_align(c("5", "120"), nbsp = "_"), c("__5", "120"))
})

test_that("FT-FM3015 [FR-FM247]: fmt_right_align(na = ) that is not a single non-NA string stops", {
  msg <- paste0("`na` must be a single string -- the text printed for a missing value ",
                "(e.g. \"-\" or \"NA\").  \"\" (the default) leaves the cell empty.")
  expect_error(fmt_right_align("5", na = NA), msg, fixed = TRUE)
  expect_error(fmt_right_align("5", na = NA_character_), msg, fixed = TRUE)
  expect_error(fmt_right_align("5", na = c("-", "x")), msg, fixed = TRUE)
  expect_error(fmt_right_align("5", na = 1L), msg, fixed = TRUE)
  expect_error(fmt_right_align("5", na = character(0)), msg, fixed = TRUE)
  # NULL is ""
  expect_identical(fmt_right_align(c("5", NA), na = NULL, nbsp = " "), c("5", ""))
})

test_that("FT-FM3016 [FR-FM251]: rounding = sas absorbs only the representation error", {
  # round_num()
  expect_identical(round_num(2.675, 2, rounding = "sas"), 2.68)
  expect_identical(round_num(1.00499999, 2, rounding = "sas"), 1)
  expect_identical(round_num(123456789.125, 0, rounding = "sas"), 123456789)
  expect_identical(round_num(1e15, 0, rounding = "sas"), 1e15)
  expect_identical(sprintf("%.0f", round_num(1e15, 0, rounding = "sas")),
                   "1000000000000000")
  # fmt_round(): 2.675 is still 2.68, 1.00499999 is 1.00
  expect_identical(fmt_round(c(2.675, 1.00499999), 2, rounding = "sas"),
                   c("2.68", "1.00"))
  # fmt_signif(): the same, by significant digits
  expect_identical(fmt_signif(2.675, 3, rounding = "sas"), "2.68")
  expect_identical(fmt_signif(1.00499999, 3, rounding = "sas"), "1.00")
  expect_identical(fmt_signif(123456789.125, 9, rounding = "sas"), "123456789")
  expect_identical(fmt_signif(1e15, 3, rounding = "sas"), "1000000000000000")
  # fmt_numeric(): digits and signif
  df <- data.frame(v = c(2.675, 1.00499999))
  expect_identical(fmt_numeric(df, "v", digits = 2, rounding = "sas")$v, c("2.68", "1.00"))
  expect_identical(fmt_numeric(df, "v", signif = 3, rounding = "sas")$v, c("2.68", "1.00"))
  big <- data.frame(v = c(123456789.125, 1e15))
  expect_identical(fmt_numeric(big, "v", digits = 0, rounding = "sas")$v,
                   c("123456789", "1000000000000000"))
  # fmt_count_pct(): a percent just below a tie stays below it
  expect_identical(fmt_count_pct(1L, 0.0100499999, nbsp = " ", rounding = "sas"),
                   "  1  (1.0)")
  expect_identical(fmt_count_pct(1L, 6.249999, pct_unit = "percent", nbsp = " ",
                                 rounding = "sas"), "  1  (6.2)")
  expect_identical(fmt_count_pct(1L, 6.25, pct_unit = "percent", nbsp = " ",
                                 rounding = "sas"), "  1  (6.3)")
  expect_identical(fmt_count_pct(1L, 0.0625, nbsp = " ", rounding = "sas"), "  1  (6.3)")
  # fmt_value_paren(): a parenthetical of 100 or more, to a whole number
  expect_identical(fmt_value_paren("12 (100.4999999)", nbsp = " ", rounding = "sas"),
                   "12 (100)")
  expect_identical(fmt_value_paren("12 (100.5)", nbsp = " ", rounding = "sas"),
                   "12 (101)")
})

test_that("FT-FM3017 [FR-FM252]: a string or logical digits / signif stops, in each function", {
  dg <- "`round_num(digits = )` must be a single non-negative integer."
  expect_error(round_num(1, digits = "2"), dg, fixed = TRUE)
  expect_error(round_num(1, digits = TRUE), dg, fixed = TRUE)
  expect_error(fmt_round(1, digits = "2"),
               "`fmt_round(digits = )` must be a single non-negative integer.", fixed = TRUE)
  expect_error(fmt_round(1, digits = TRUE),
               "`fmt_round(digits = )` must be a single non-negative integer.", fixed = TRUE)
  sg <- "`fmt_signif(digits = )` must be a single positive integer."
  expect_error(fmt_signif(1, digits = "2"), sg, fixed = TRUE)
  expect_error(fmt_signif(1, digits = TRUE), sg, fixed = TRUE)
  df <- data.frame(v = c(1.25, 2.35))
  nd <- "`fmt_numeric(digits = )` must be a single non-negative integer."
  ns <- "`fmt_numeric(signif = )` must be a single positive integer."
  expect_error(fmt_numeric(df, "v", digits = "2"), nd, fixed = TRUE)
  expect_error(fmt_numeric(df, "v", digits = TRUE), nd, fixed = TRUE)
  expect_error(fmt_numeric(df, "v", signif = "2"), ns, fixed = TRUE)
  expect_error(fmt_numeric(df, "v", signif = TRUE), ns, fixed = TRUE)
  # a formats rule
  d2 <- data.frame(g = c("Mean", "SD"), v = c(1.25, 2.35), stringsAsFactors = FALSE)
  rule <- function(r) fmt_numeric(d2, "v", by = "g", formats = list(Mean = r, SD = r))
  expect_error(rule(list(digits = "2")), nd, fixed = TRUE)
  expect_error(rule(list(digits = TRUE)), nd, fixed = TRUE)
  expect_error(rule(list(signif = "2")), ns, fixed = TRUE)
  expect_error(rule(list(signif = TRUE)), ns, fixed = TRUE)
  # the numbers still work
  expect_identical(rule(list(digits = 1))$v, c("1.2", "2.4"))
})

test_that("FT-FM3018 [FR-FM253]: na of fmt_round / fmt_signif / fmt_numeric: one non-NA string", {
  x <- c(1, NA)
  for (bad in list(NA, NA_character_, 1, c("a", "b"), character(0))) {
    expect_error(fmt_round(x, 1, na = bad), .g1_na_msg, fixed = TRUE)
    expect_error(fmt_signif(x, 3, na = bad), .g1_na_msg, fixed = TRUE)
    expect_error(fmt_numeric(data.frame(v = x), "v", digits = 1, na = bad),
                 .g1_na_msg, fixed = TRUE)
  }
  # NULL is ""
  expect_identical(fmt_round(x, 1, na = NULL), c("1.0", ""))
  expect_identical(fmt_signif(x, 3, na = NULL), c("1.00", ""))
  expect_identical(fmt_numeric(data.frame(v = x), "v", digits = 1, na = NULL)$v,
                   c("1.0", ""))
  # a string is printed
  expect_identical(fmt_numeric(data.frame(v = x), "v", digits = 1, na = "-")$v,
                   c("1.0", "-"))
})
