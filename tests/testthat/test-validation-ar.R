# The ARD area's leaf requirements that had no test (the deviations of the
# first validation version): each test names its FT id and its FRs.  The
# ARDs are built by hand, in the shape cards writes, from fictional counts.

.ar_rows <- function(g1, g1l, var, lvl, ctx, sn, st) {
  data.frame(group1 = g1, group1_level = g1l, variable = var,
             variable_level = lvl, context = ctx, stat_name = sn,
             stat_label = sn, stat = st, stringsAsFactors = FALSE)
}

# Two arms, SEX tabulated (M has no Placebo record), AGE summarised, and the
# arm's own tabulation (whose N is the study total).
.ar_ard <- function() {
  rbind(
    .ar_rows("TRT", "Drug A", "SEX", "F", "tabulate", "n", 3),
    .ar_rows("TRT", "Drug A", "SEX", "F", "tabulate", "N", 5),
    .ar_rows("TRT", "Drug A", "SEX", "M", "tabulate", "n", 2),
    .ar_rows("TRT", "Drug A", "SEX", "M", "tabulate", "N", 5),
    .ar_rows("TRT", "Placebo", "SEX", "F", "tabulate", "n", 4),
    .ar_rows("TRT", "Placebo", "SEX", "F", "tabulate", "N", 6),
    .ar_rows("TRT", "Drug A", "AGE", NA, "summary", "N", 5),
    .ar_rows("TRT", "Placebo", "AGE", NA, "summary", "N", 7),
    .ar_rows(NA, NA, "TRT", "Drug A", "tabulate", "n", 5),
    .ar_rows(NA, NA, "TRT", "Placebo", "tabulate", "n", 6),
    .ar_rows(NA, NA, "TRT", "Drug A", "tabulate", "N", 11),
    .ar_rows(NA, NA, "TRT", "Placebo", "tabulate", "N", 11))
}

# The SEX counts alone, normalized, for the widen_ard() tests.
.ar_sex <- function() {
  a <- .ar_ard()
  normalize_ard(a[a$variable == "SEX" & a$stat_name == "n", , drop = FALSE])
}

# The SEX counts with two comparison records (no arm, contexts of their own).
.ar_with_tests <- function() {
  a <- .ar_ard()
  a <- a[a$variable == "SEX" & a$stat_name == "n", , drop = FALSE]
  normalize_ard(rbind(a,
                      .ar_rows(NA, NA, "SEX", NA, "chisq", "p.value", 0.25),
                      .ar_rows(NA, NA, "SEX", NA, "fisher", "p.value", 0.5)))
}

# A hierarchy: SOC in group2, PT the analysed variable, under the arm.
.ar_hier <- function() {
  a <- rbind(
    .ar_rows("TRT", "Drug A", "AEDECOD", "Headache", "hierarchical", "n", 2),
    .ar_rows("TRT", "Placebo", "AEDECOD", "Headache", "hierarchical", "n", 1))
  a$group2 <- "AESOC"
  a$group2_level <- "Nervous system disorders"
  a
}

test_that("FT-AR083 [FR-AR135 FR-AR137 FR-AR139]: normalize_ard() refuses an overall that is not one string or overall_row(), a stack_sep that is not one text, and a stack naming a hierarchy variable", {
  a <- .ar_hier()
  msg <- "`overall` must be a single string or overall_row(); see ?overall_row."
  for (bad in list(1, c("Any AE", "Any TEAE"), TRUE, list("Any AE"))) {
    expect_error(normalize_ard(a, hierarchy = c("AESOC", "AEDECOD"), overall = bad),
                 msg, fixed = TRUE)
  }
  expect_s3_class(normalize_ard(a, hierarchy = c("AESOC", "AEDECOD"),
                                overall = overall_row("Any AE")), "ard_long")

  b <- .ar_ard()
  b$group2 <- ifelse(b$variable == "SEX", "SEX", NA)
  b$group2_level <- ifelse(b$variable == "SEX", b$variable_level, NA)
  b <- b[b$variable == "AGE" | b$variable == "TRT", , drop = FALSE]
  msg <- "`stack_sep` is one text, what joins a crossed layer's names: \" / \"."
  for (bad in list(1, c(" / ", "-"), NA_character_, NULL)) {
    expect_error(normalize_ard(b, stack = list(subgroup = "SEX"), stack_sep = bad),
                 msg, fixed = TRUE)
  }

  expect_error(normalize_ard(a, hierarchy = c("AESOC", "AEDECOD"),
                             stack = list(subgroup = "AESOC")),
               "`stack`: 'AESOC' is in `hierarchy`.  A hierarchy's levels nest; only subgroups stack.",
               fixed = TRUE)
})

test_that("FT-AR084 [FR-AR226]: pull_ard() whose answers disagree between variables or contexts stops with the candidates and picks none", {
  a <- .ar_ard()
  err <- expect_error(pull_ard(a, cols = "TRT"),
                      "This ARD offers more than one 'N' for these columns, and they\ndisagree.  Say which with `variable =` and/or `context =`.",
                      fixed = TRUE)
  msg <- conditionMessage(err)
  expect_true(grepl("candidates, keyed by the value columns:", msg, fixed = TRUE))
  # each candidate, with its values per column
  expect_true(grepl("variable = 'AGE' .* context = 'summary' .* stat = 'N' .* -> 5 / 7", msg))
  expect_true(grepl("variable = 'SEX' .* context = 'tabulate' .* stat = 'N' .* -> 5 / 6", msg))
  # answers that agree are no ambiguity
  agree <- a[!(a$variable == "AGE"), , drop = FALSE]
  agree <- rbind(agree, .ar_rows("TRT", c("Drug A", "Placebo"), "AGE", NA, "summary", "N", c(5, 6)))
  expect_identical(pull_ard(agree, cols = "TRT"), c("Drug A" = 5, Placebo = 6))
})

test_that("FT-AR085 [FR-AR209]: widen_ard(stat_groups = ) called directly refuses a malformed group map", {
  x <- .ar_sex()
  w <- function(sg) widen_ard(x, cols = "TRT", cells = "{n}", notes = FALSE, stat_groups = sg)
  expect_error(w(list("n")),
               "`stat_groups`: every group has a name of its own", fixed = TRUE)
  expect_error(w(list(a = "n", a = "N")),
               "`stat_groups`: every group has a name of its own", fixed = TRUE)
  expect_error(w(list(a = character())),
               "`stat_groups`: 'a' names no statistic.", fixed = TRUE)
  expect_error(w(list(a = "n", b = c("n", "N"))),
               "`stat_groups`: 'n' is in two groups; a statistic is in one.", fixed = TRUE)
  expect_error(w(list(a = "n", .variables = 1)),
               "`stat_groups`: `.variables` names the variables split into the groups: \"RESP\".",
               fixed = TRUE)
  expect_error(w(list(a = "n", .variables = "SEX", .others = "z")),
               "`stat_groups`: `.others` is one of the groups ('a'), where the other variables' records go whole.",
               fixed = TRUE)
  expect_error(w(list(a = "n", .others = "a")),
               "`stat_groups`: `.others` needs `.variables` (which variables are split).",
               fixed = TRUE)
})

test_that("FT-AR086 [FR-AR210]: widen_ard(compare = ) called directly refuses a malformed comparison spec and two records in one cell", {
  x <- .ar_with_tests()
  w <- function(cmp) widen_ard(x, cols = "TRT", cells = "{n}", notes = FALSE, compare = cmp)
  expect_error(w("{p.value}"),
               "`compare` is a list: `list(cols = list(p = \"{p.value:.3f}\"))`.", fixed = TRUE)
  msg <- "plan_compare(): every column has a name of its own: `plan_compare(p = \"{p.value:.3f}\")`."
  expect_error(w(list(cols = list("{p.value:.3f}"))), msg, fixed = TRUE)
  expect_error(w(list(cols = list(p = "{p.value}", p = "{p.value:.2f}"))), msg, fixed = TRUE)
  expect_error(w(list(cols = list(p = "{p.value}"), position = "middle")),
               "plan_compare(position = ) is \"last\" or \"first\".", fixed = TRUE)
  expect_error(w(list(cols = list(p = "{p.value}"), by = c("A", "B"))),
               "plan_compare(by = ) is one column, the key that splits the comparisons: \"COMPARISON\".",
               fixed = TRUE)
  expect_error(w(list(cols = list(p = "{p.value}"), by = "CMP")),
               "plan_compare(by = \"CMP\"): no such column in the comparison records.", fixed = TRUE)
  expect_error(w(list(cols = list(label = "{p.value}"))),
               "plan_compare(): 'label' is a column of the table already.", fixed = TRUE)
  # the chisq and the fisher p-value both reach the column p on the SEX row
  expect_error(w(list(cols = list(p = "{p.value:.2f}"))),
               "plan_compare(): two comparison records of 'SEX' reach the column 'p' on one row",
               fixed = TRUE)
  # one test picked by its context: the column is made
  ok <- w(list(cols = list(p = list(chisq = "{p.value:.2f}"))))
  expect_identical(ok$p, c("0.25", NA))
})

test_that("FT-AR087 [FR-AR212]: widen_ard(filter = list(by = )) naming no row key stops with the row keys", {
  x <- .ar_sex()
  expect_error(widen_ard(x, cols = "TRT", cells = "{n}", notes = FALSE,
                         filter = list(by = "nope")),
               "plan_filter(by = \"nope\"): not a row key.  Row keys: ", fixed = TRUE)
})

test_that("FT-AR088 [FR-AR121]: normalize_ard(drop_contexts = ) naming only the attributes keeps the total_n row and states no ard_total_n; character() drops nothing", {
  a <- .ar_ard()
  a <- rbind(a,
             .ar_rows(NA, NA, "..ard_total_n..", NA, "total_n", "N", 11))
  a$stat <- as.list(a$stat)
  a <- rbind(a, .ar_rows(NA, NA, "SEX", NA, "attributes", "label", NA))
  a$stat[[nrow(a)]] <- "Sex"
  # the default drops both, keeping the number on the attribute
  d0 <- normalize_ard(a)
  expect_false(any(d0$context %in% c("total_n", "attributes")))
  expect_identical(attr(d0, "ard_total_n"), 11)
  # only the attributes: the total_n row stays, and no attribute states it
  d1 <- normalize_ard(a, drop_contexts = "attributes")
  expect_identical(d1$stat[d1$context == "total_n"], 11)
  expect_false(any(d1$context == "attributes"))
  expect_null(attr(d1, "ard_total_n"))
  expect_false(".total_n" %in% names(d1))
  # nothing: every row is there
  d2 <- normalize_ard(a, drop_contexts = character())
  expect_identical(nrow(d2), nrow(a))
  expect_true(all(c("total_n", "attributes") %in% d2$context))
  expect_null(attr(d2, "ard_ignored"))
})

test_that("FT-AR089 [FR-AR180]: widen_ard(na = ) is what a cell no record fills holds; NA by default", {
  x <- .ar_sex()
  w0 <- widen_ard(x, cols = "TRT", cells = "{n}", notes = FALSE)
  expect_identical(w0$Placebo, c("4", NA))
  w1 <- widen_ard(x, cols = "TRT", cells = "{n}", notes = FALSE, na = "-")
  expect_identical(w1$Placebo, c("4", "-"))
  # a filled cell is untouched
  expect_identical(w1[["Drug A"]], w0[["Drug A"]])
})

test_that("FT-AR090 [FR-AR219]: pull_ard(variable = ) and (context = ) restrict the pull; variable = a key asks for its own tabulation", {
  a <- .ar_ard()
  expect_identical(pull_ard(a, cols = "TRT", variable = "SEX"), c("Drug A" = 5, Placebo = 6))
  expect_identical(pull_ard(a, cols = "TRT", variable = "AGE"), c("Drug A" = 5, Placebo = 7))
  expect_identical(pull_ard(a, cols = "TRT", context = "summary"), c("Drug A" = 5, Placebo = 7))
  # the key's own rows, on purpose: the study total as N, the arm's count as n
  expect_identical(pull_ard(a, cols = "TRT", variable = "TRT"), c("Drug A" = 11, Placebo = 11))
  expect_identical(pull_ard(a, cols = "TRT", stat = "n", variable = "TRT"),
                   c("Drug A" = 5, Placebo = 6))
  # both together
  expect_identical(pull_ard(a, cols = "TRT", variable = "SEX", context = "tabulate"),
                   c("Drug A" = 5, Placebo = 6))
  expect_error(pull_ard(a, cols = "TRT", variable = "SEX", context = "summary"),
               "No 'N' found for these columns.", fixed = TRUE)
})

test_that("FT-AR091 [FR-AR133]: normalize_ard() takes an ARD with no variable_level and gives it as NA", {
  a <- .ar_ard()
  a <- a[a$variable == "AGE", setdiff(names(a), "variable_level"), drop = FALSE]
  d <- normalize_ard(a)
  expect_true("variable_level" %in% names(d))
  expect_identical(d$variable_level, c(NA_character_, NA_character_))
  expect_identical(d$stat, c(5, 7))
})

test_that("FT-AR092 [FR-AR193]: widen_ard() on a frame of no rows, or one whose rows all drop, says every row was dropped", {
  x <- .ar_sex()
  msg <- "Every row was dropped before the cells were built"
  err <- expect_error(widen_ard(x[0, , drop = FALSE], cols = "TRT", cells = "{n}", notes = FALSE),
                      "No cell was produced, so there is nothing to widen.", fixed = TRUE)
  expect_true(grepl(msg, conditionMessage(err), fixed = TRUE))
  # every row without a value for the column key
  y <- x
  y$TRT <- NA_character_
  err <- expect_error(widen_ard(y, cols = "TRT", cells = "{n}", notes = FALSE),
                      "No cell was produced, so there is nothing to widen.", fixed = TRUE)
  expect_true(grepl(msg, conditionMessage(err), fixed = TRUE))
})

test_that("FT-AR093 [FR-AR194]: widen_ard() keeps non-ASCII arms and levels unchanged in the column names and values", {
  drug <- "実薬"          # the arm
  pbo  <- "プラセボ"
  f    <- "女性"          # the levels
  m    <- "男性"
  a <- rbind(
    .ar_rows("TRT", drug, "SEX", f, "tabulate", "n", 3),
    .ar_rows("TRT", drug, "SEX", m, "tabulate", "n", 2),
    .ar_rows("TRT", pbo, "SEX", f, "tabulate", "n", 4),
    .ar_rows("TRT", pbo, "SEX", m, "tabulate", "n", 1))
  w <- widen_ard(normalize_ard(a), cols = "TRT", cells = "{n}", notes = FALSE)
  expect_identical(names(w), c("label", drug, pbo))
  expect_identical(w$label, c(f, m))
  expect_identical(w[[drug]], c("3", "2"))
  expect_identical(w[[pbo]], c("4", "1"))
  expect_identical(Encoding(names(w)[2:3]), c("UTF-8", "UTF-8"))
})

test_that("FT-AR094 [FR-AR236 FR-AR237]: list_ard_keys() lists a grouping variable once whatever its slot, and no grouping variable as no keys", {
  # TRT in group1 in one block, in group2 (under SITE) in the other
  a <- .ar_ard()
  a <- a[a$variable == "SEX", , drop = FALSE]
  b <- .ar_rows("SITE", "Site 01", "AGE", NA, "summary", "N", 4)
  b$group2 <- "TRT"
  b$group2_level <- "Drug A"
  a$group2 <- NA_character_
  a$group2_level <- NA_character_
  out <- NULL
  expect_output(out <- list_ard_keys(rbind(a, b)), "ARD keys (group1..groupN values) : TRT, SITE",
                fixed = TRUE)
  expect_identical(out$keys, c("TRT", "SITE"))
  # no grouping variable at all
  one <- .ar_ard()
  one <- one[one$variable == "AGE", setdiff(names(one), c("group1", "group1_level")),
             drop = FALSE]
  expect_output(out <- list_ard_keys(one), "ARD keys (group1..groupN values) :  \n",
                fixed = TRUE)
  expect_null(out$keys)
})
