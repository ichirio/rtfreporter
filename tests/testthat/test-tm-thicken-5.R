# Thickening of the functional tests, batch 5: the ARD tools (area AR).  Each
# block asserts what its FR states, the figures worked out in a comment.  FT ids
# from 5501 up.

# One continuous variable (AGE: N and mean) and one categorical (SEX, level F:
# n and p), by arm A and B.
.t5_base <- function() {
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

# SEX (F, M, U) counted in two arms: n and p per level.
.t5_cat <- function() {
  mk <- function(arm, lv, n, p) {
    data.frame(group1 = "TRT", group1_level = arm, variable = "SEX",
               variable_level = lv, context = "categorical",
               stat_name = c("n", "p"), stat_label = c("n", "%"),
               stat = c(n, p), stringsAsFactors = FALSE)
  }
  rbind(mk("A", "F", 4, 0.4), mk("A", "M", 5, 0.5), mk("A", "U", 1, 0.1),
        mk("B", "F", 3, 0.25), mk("B", "M", 6, 0.5), mk("B", "U", 0, 0))
}

# A hierarchical ARD: a SOC row and its terms, counts by arm A and B.
.t5_soc <- function() {
  mk <- function(trt, soc, pt, n) {
    data.frame(group1 = "TRT", group1_level = trt,
               group2 = if (is.na(pt)) NA_character_ else "AESOC",
               group2_level = if (is.na(pt)) NA_character_ else soc,
               variable = if (is.na(pt)) "AESOC" else "AETERM",
               variable_level = if (is.na(pt)) soc else pt,
               context = "hierarchical", stat_name = "n", stat_label = "n",
               stat = n, stringsAsFactors = FALSE)
  }
  do.call(rbind, list(
    mk("A", "Cardiac", NA, 3), mk("B", "Cardiac", NA, 4),
    mk("A", "Cardiac", "Palp", 2), mk("B", "Cardiac", "Palp", 1),
    mk("A", "Cardiac", "Tach", 1), mk("B", "Cardiac", "Tach", 3),
    mk("A", "Eye", NA, 8), mk("B", "Eye", NA, 1),
    mk("A", "Eye", "Dry", 5), mk("B", "Eye", "Dry", 1),
    mk("A", "Eye", "Itch", 1), mk("B", "Eye", "Itch", 0)))
}

test_that("FT-AR5501 [FR-AR108]: normalize_ard() returns a factor key as a factor with its declared levels (unused included), a character key as character", {
  skip_if_not_installed("cards")
  dat <- data.frame(AGE = c(10, 20, 33, 41, 5, 6),
                    TRT = c("A", "B", "A", "B", "B", "A"))
  stat <- ~ cards::continuous_summary_fns("N")
  # a factor declaring the levels B, A, Z; Z has no subject
  dat$TRT <- factor(dat$TRT, levels = c("B", "A", "Z"))
  fct <- normalize_ard(cards::ard_summary(dat, by = TRT, variables = AGE, statistic = stat))
  expect_true(is.factor(fct$TRT))
  expect_identical(levels(fct$TRT), c("B", "A", "Z"))
  expect_false(is.ordered(fct$TRT))
  # normalizing the result again keeps the declared order
  expect_identical(levels(normalize_ard(fct)$TRT), c("B", "A", "Z"))
  # the same data with a character key: character, not a factor
  dat$TRT <- as.character(dat$TRT)
  chr <- normalize_ard(cards::ard_summary(dat, by = TRT, variables = AGE, statistic = stat))
  expect_type(chr$TRT, "character")
  expect_false(is.factor(chr$TRT))
  expect_setequal(chr$TRT, c("A", "B"))
})

test_that("FT-AR5502 [FR-AR114]: normalize_ard(hierarchy = ) folds the rows of its variables into their key columns, with .label the deepest value and .depth its position; a variable outside it is depth 0 labelled by variable_level", {
  h <- rbind(
    .t5_soc()[1:4, ],
    data.frame(group1 = "TRT", group1_level = c("A", "B"), group2 = NA_character_,
               group2_level = NA_character_, variable = "SEX", variable_level = "F",
               context = "categorical", stat_name = "n", stat_label = "n",
               stat = c(8, 9), stringsAsFactors = FALSE))
  d <- normalize_ard(h, hierarchy = c("AESOC", "AETERM"))
  soc <- d[d$variable == "AESOC", ]
  pt <- d[d$variable == "AETERM", ]
  sx <- d[d$variable == "SEX", ]
  # the SOC rows: the level in AESOC, no term, depth 1 (outermost), labelled by the SOC
  expect_identical(soc$AESOC, c("Cardiac", "Cardiac"))
  expect_true(all(is.na(soc$AETERM)))
  expect_identical(soc$.depth, c(1L, 1L))
  expect_identical(soc$.label, c("Cardiac", "Cardiac"))
  # the term rows: the SOC above it, the term in AETERM, depth 2, labelled by the term
  expect_identical(pt$AESOC, c("Cardiac", "Cardiac"))
  expect_identical(pt$AETERM, c("Palp", "Palp"))
  expect_identical(pt$.depth, c(2L, 2L))
  expect_identical(pt$.label, c("Palp", "Palp"))
  # a variable the hierarchy does not cover: depth 0, its variable_level the label
  expect_identical(sx$.depth, c(0L, 0L))
  expect_identical(sx$.label, c("F", "F"))
  expect_true(all(is.na(sx$AESOC)))
  expect_true(all(is.na(sx$AETERM)))
})

test_that("FT-AR5503 [FR-AR120]: a dropped ..ard_total_n.. row is kept as ard_total_n and .total_n when the ARD states exactly one total; two different totals are not remembered", {
  with_totals <- function(vals) {
    d <- .t5_base()
    tot <- d[rep(1L, length(vals)), ]
    tot$group1 <- NA_character_
    tot$group1_level <- NA_character_
    tot$variable <- "..ard_total_n.."
    tot$context <- "total_n"
    tot$stat_name <- "N"
    tot$stat <- vals
    normalize_ard(rbind(d, tot))
  }
  # one total (22): the attribute, and the column on each of the 8 statistic rows
  one <- with_totals(22)
  expect_false(any(one$variable == "..ard_total_n.."))
  expect_identical(attr(one, "ard_total_n", exact = TRUE), 22)
  expect_identical(one$.total_n, rep(22, 8L))
  # the same total stated twice is still exactly one total
  twice <- with_totals(c(22, 22))
  expect_identical(attr(twice, "ard_total_n", exact = TRUE), 22)
  expect_identical(unique(twice$.total_n), 22)
  # two different totals: neither the attribute nor the column; the rows are dropped all the same
  two <- with_totals(c(22, 30))
  expect_false(any(two$variable == "..ard_total_n.."))
  expect_null(attr(two, "ard_total_n", exact = TRUE))
  expect_false(".total_n" %in% names(two))
  expect_identical(nrow(two), 8L)
})

test_that("FT-AR5504 [FR-AR138]: normalize_ard(stack = ) naming a column the frame has stops, and stack_nest with NAME_outer there stops, with the messages", {
  k <- data.frame(
    TRT = rep(c("P", "Q"), 3L), SOC = rep(c("A", "B", "Z"), each = 2L),
    variable = "AE", variable_level = NA_character_, context = "categorical",
    stat_name = "n", stat_label = "n", stat = 1:6, stringsAsFactors = FALSE)
  # SOC is a column already
  expect_error(
    normalize_ard(k, keys = c("TRT", "SOC"), stack = list(SOC = "TRT")),
    "`stack`: .SOC. is a column already; name the stacked key something else.")
  # so is NAME_level
  k$grp_level <- "x"
  expect_error(
    normalize_ard(k, keys = c("TRT", "SOC"), stack = list(grp = "TRT")),
    "`stack`: .grp_level. is a column already; name the stacked key something else.")
  # a free name is accepted
  k$grp_level <- NULL
  free <- normalize_ard(k, keys = c("TRT", "SOC"), stack = list(grp = "TRT"))
  expect_true(all(c("grp", "grp_level") %in% names(free)))
  # stack_nest = TRUE with grp_outer already there
  k$grp_outer <- 1
  expect_error(
    normalize_ard(k, keys = c("TRT", "SOC"), stack = list(grp = list(c("TRT", "SOC"))),
                  stack_nest = TRUE),
    "`stack_nest`: 'grp_outer' is a column already.", fixed = TRUE)
  # without stack_nest the same column is not in the way
  ok <- normalize_ard(k, keys = c("TRT", "SOC"), stack = list(grp = list(c("TRT", "SOC"))))
  expect_true("grp_level" %in% names(ok))
})

test_that("FT-AR5505 [FR-AR145]: widen_ard() fills a cell from the first template of its chain that resolves; a template naming a statistic the record lacks makes the cell NA, not an error", {
  d <- normalize_ard(.t5_cat())
  w <- function(cells) widen_ard(d, cols = "TRT", cells = cells, notes = FALSE)
  # the first template that resolves wins (n 4 5 1, p 0.4 0.5 0.1 in arm A)
  expect_identical(w(c("{n:.0f}", "{p:.1f}"))$A, c("4", "5", "1"))
  expect_identical(w(c("{p:.1f}", "{n:.0f}"))$A, c("0.4", "0.5", "0.1"))
  # a template naming a statistic the record lacks gives way to the next
  expect_identical(w(c("{n} ({nope})", "{n:.0f}"))$A, c("4", "5", "1"))
  # when none resolves the cell is NA, and nothing is raised
  expect_no_error(none <- w(c("{n} ({nope})", "{nope2}")))
  expect_identical(none$A, rep(NA_character_, 3L))
  expect_identical(none$B, rep(NA_character_, 3L))
  expect_identical(names(none), c("label", "A", "B"))
  expect_no_error(single <- w("{nope}"))
  expect_true(all(is.na(single$A)))
})

test_that("FT-AR5506 [FR-AR172]: widen_ard(labels = list(SEX = c(SEX = ..., F = ...))) recodes the analysis variable's levels in the label column, its own entry being the variable's label", {
  d <- normalize_ard(.t5_cat())
  lab <- list(SEX = c(SEX = "Sex", F = "Female"))
  got <- widen_ard(d, cols = "TRT", rows = c(group = "variable"), cells = "{n:.0f}",
                   labels = lab, notes = FALSE)
  # the variable's label in its row-key column, the level F recoded, M and U as they were
  expect_identical(as.character(got$group), c("Sex", "Sex", "Sex"))
  expect_identical(as.character(got$label), c("Female", "M", "U"))
  # the numbers are SEX's own (F 4 3, M 5 6, U 1 0)
  expect_identical(got$A, c("4", "5", "1"))
  expect_identical(got$B, c("3", "6", "0"))
  # another variable is not touched: AGE keeps its name
  b <- normalize_ard(.t5_base())
  both <- widen_ard(b, cols = "TRT", rows = c(group = "variable"),
                    cells = list(continuous = "{mean:.1f}", categorical = "{n:.0f}"),
                    labels = lab, notes = FALSE)
  expect_identical(as.character(both$group), c("AGE", "Sex"))
  expect_identical(as.character(both$label[2L]), "Female")
  # without the scope the levels are the ARD's
  plain <- widen_ard(d, cols = "TRT", rows = c(group = "variable"), cells = "{n:.0f}",
                     notes = FALSE)
  expect_identical(as.character(plain$group), c("SEX", "SEX", "SEX"))
  expect_identical(as.character(plain$label), c("F", "M", "U"))
})

test_that("FT-AR5507 [FR-AR176]: widen_ard(sort = ) takes the scoped forms: one spread column, a group's own row carried to its rows, the largest or smallest across the columns, levels()", {
  h <- normalize_ard(.t5_soc(), hierarchy = c("AESOC", "AETERM"))
  w <- function(...) {
    widen_ard(h, cols = "TRT", rows = c(soc = "AESOC", pt = "AETERM"), cells = "{n:.0f}",
              notes = FALSE, ...)
  }
  lab <- function(t) as.character(t$label)
  # the counts (A, B): Cardiac 3 4, Palp 2 1, Tach 1 3; Eye 8 1, Dry 5 1, Itch 1 0
  expect_identical(lab(w()), c("Cardiac", "Palp", "Tach", "Eye", "Dry", "Itch"))
  # one spread column: -n[B] sorts on B alone, 4 3 1 1 1 0 (ties keep their order);
  # the total would have put Eye (9) first
  expect_identical(lab(w(sort = "-n[B]")),
                   c("Cardiac", "Tach", "Palp", "Eye", "Dry", "Itch"))
  # a group's own row carried to its rows: the SOCs by their own A (Eye 8, Cardiac 3),
  # each with its terms right under it, by A (Dry 5 > Itch 1; Palp 2 > Tach 1)
  carried <- w(sort = c("-n@soc[A]", "-n[A]"))
  expect_identical(lab(carried), c("Eye", "Dry", "Itch", "Cardiac", "Palp", "Tach"))
  expect_identical(as.character(carried$soc), rep(c("Eye", "Cardiac"), each = 3L))
  #  ... without the carry the rows are sorted flat on A: 8 5 3 2 1 1
  expect_identical(lab(w(sort = "-n[A]")),
                   c("Eye", "Dry", "Cardiac", "Palp", "Tach", "Itch"))
  # the SOCs by their own total (Eye 9, Cardiac 7), the terms by theirs (Tach 4 > Palp 3)
  expect_identical(lab(w(sort = c("-n@soc", "-n"))),
                   c("Eye", "Dry", "Itch", "Cardiac", "Tach", "Palp"))
  # the largest across the columns: Eye 8, Dry 5, Cardiac 4, Tach 3, Palp 2, Itch 1
  # (the total would have put Cardiac, 7, before Dry, 6)
  expect_identical(lab(w(sort = "-max(n)")),
                   c("Eye", "Dry", "Cardiac", "Tach", "Palp", "Itch"))
  #  ... and the smallest, ascending: Itch 0; Palp, Tach, Eye, Dry 1 (their order); Cardiac 3
  expect_identical(lab(w(sort = "min(n)")),
                   c("Itch", "Palp", "Tach", "Eye", "Dry", "Cardiac"))
  # the values plan_levels() names first: Eye before Cardiac, each block whole
  lv <- w(levels = list(soc = c("Eye", "Cardiac")), sort = "levels(soc)")
  expect_identical(as.character(lv$soc), rep(c("Eye", "Cardiac"), each = 3L))
  expect_identical(lab(lv), c("Eye", "Dry", "Itch", "Cardiac", "Palp", "Tach"))
  # a form that names nothing is refused, saying what is allowed
  expect_error(w(sort = "-n@nope"), "is not a row key")
})

test_that("FT-AR5508 [FR-AR186]: widen_ard(show_keys = ) keeps the own tabulation of those row keys in the body as a variable's block; NULL leaves a key's own counts out", {
  x <- data.frame(
    group1 = "TRT", group1_level = c("A", "B", "A", "B"),
    group2 = NA_character_, group2_level = NA_character_,
    variable = c("SEX", "SEX", "AGE", "AGE"), variable_level = c("F", "F", NA, NA),
    context = c("categorical", "categorical", "continuous", "continuous"),
    stat_name = c("n", "n", "mean", "mean"), stat_label = c("n", "n", "Mean", "Mean"),
    stat = c(4, 3, 12.3, 15.6), stringsAsFactors = FALSE)
  # AGE of the women, by arm: SEX is a key here
  y <- data.frame(
    group1 = "SEX", group1_level = "F", group2 = "TRT", group2_level = c("A", "B"),
    variable = "AGE", variable_level = NA_character_, context = "continuous",
    stat_name = "mean", stat_label = "Mean", stat = c(1, 2), stringsAsFactors = FALSE)
  d <- normalize_ard(rbind(x, y), stack = list(subgroup = "SEX"))
  # SEX's own counts are marked: 2 rows
  expect_identical(sum(d$.key_own), 2L)
  expect_identical(unique(d$variable[d$.key_own]), "SEX")
  cells <- list(SEX = "{n:.0f}", continuous = "{mean:.1f}")
  w <- function(...) {
    widen_ard(d, cols = "TRT", rows = c(block = "variable", grp = "subgroup",
                                        lvl = "subgroup_level"),
              cells = cells, notes = FALSE, ...)
  }
  # the default leaves SEX's own counts out: AGE overall, and AGE of the women
  none <- w()
  expect_identical(none$block, c("AGE", "AGE"))
  expect_false("SEX" %in% none$block)
  expect_identical(w(show_keys = NULL), none)
  # show_keys = "SEX": SEX is a block of its own, its level F with the counts 4 and 3
  kept <- w(show_keys = "SEX")
  expect_identical(kept$block, c("SEX", "AGE", "AGE"))
  expect_identical(as.character(kept$grp[1L]), "SEX")
  expect_identical(as.character(kept$lvl[1L]), "F")
  expect_identical(kept$A[1L], "4")
  expect_identical(kept$B[1L], "3")
  # the other rows are as they were
  expect_identical(kept$A[-1L], none$A)
  expect_identical(kept$B[-1L], none$B)
  # a column key is not a row key to show
  expect_error(widen_ard(d, cols = "TRT", show_keys = "TRT", cells = cells), "a column key")
})
