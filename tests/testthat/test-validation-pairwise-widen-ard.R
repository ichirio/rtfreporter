# Validation tests: widen_ard() -- how the column keys, the cell recipe, the
# declared orders (levels), the recoding (labels), the sort, the separator,
# the empty-cell text and sort_stat act together on the table made from an
# ARD: a pairwise set checked against a table built straight from the counts,
# and a decision table of stats x value x na.  Helpers prefixed .wa_.
# Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  Each argument alone (na, label, stat_groups, compare, filter ...) is in
#  test-validation-ar.R, test-validation-boundaries-batch3.R (FT-AR108) and
#  -batch4.R; this file takes the arguments that decide the shape of the
#  table together.  The rules, as read from the help (widen_ard()) and
#  FR-AR150 FR-AR151 FR-AR160 FR-AR166 FR-AR167 FR-AR168 FR-AR170 FR-AR172
#  FR-AR173 FR-AR175 FR-AR177 FR-AR179 FR-AR180:
#
#    columns  one column per value of the key, or per combination of several
#             keys, named by the values joined with `sep`; `levels` names the
#             order of a column key (otherwise the data's); `labels` recodes
#             the key's values to the column names.
#    rows     one row per level of the analysis variable, labelled by its
#             level; `levels = list(SEX = )` declares the order, `labels =
#             list(SEX = )` recodes the label text; with `sort = FALSE` a row
#             moves only where an order was declared (the data's otherwise).
#    sort     a character vector names keys in priority order, "-" for
#             descending: "n" is the statistic totalled across the spread
#             columns, so the rows go by their total count; nothing is
#             alphabetised behind the caller's back.  An explicit sort key
#             decides the order over a declared level order.  The sort and
#             sort_stat read the rows by the level of the data, the same
#             whether `labels` recodes it for display or not.
#    cells    "{n}" and "{n} ({p:.1f%})" print the count, and the count with
#             the percentage of a fraction (0.6 is 60.0), one text a cell.
#    na       a cell no record fills is `na`: NA (the default) or one text.
#    sort_stat  a statistic named is totalled across the spread columns and
#             attached as a numeric `.sort_stat` column, last.
#
#  Data: SEX (F, M, U) counted in two arms (TRT: Drug A, Placebo), alone
#  (.wa_long1) or in two sites each (SITE: S1, S2; .wa_long2); Placebo's U
#  count is missing from both (an empty cell), and the totals of the rows
#  differ (no ties) so a sort has one answer.
#
#  (A) FT-AR110, pairwise.  Factors and levels:
#    cols     TRT | TRT and SITE
#    cells    "{n}" | "{n} ({p:.1f%})"
#    levels   none | SEX (U, M, F) | TRT (Placebo, Drug A) | both
#    labels   none | TRT recoded (Drug A is Active) | SEX recoded (Female ...)
#    sort     FALSE | "-n" | "n"
#    na       NA | "-"
#    sep      "____" (the default) | " / "
#    sort_stat  NULL | "n"
#  The product is 2 x 2 x 4 x 3 x 3 x 2 x 2 x 2 = 1,152 rows, the SEX
#  recoding with a sort key or sort_stat among them (it used to leave the rows
#  in the data's order and .sort_stat NA); .allpairs() picks the rows covering
#  every pair.  Oracle: the table is built straight from the counts by the
#  rules above.
#
#  (D) FT-AR113, a cell that records of different values reach (a SITE
#  beside cols = "TRT", which neither `cols` nor `rows` names): the first
#  record's value is printed and widen_ard() WARNS (FR-AR253) -- not an error,
#  since a plan reading a position over stacked blocks lands several records
#  in a cell on purpose (test-ard-tables.R) -- naming the key(s) they differ
#  in, the cell and the values.  stats = "rows" stops (FR-AR202).  Rules:
#    the same value twice          no warning
#    the key named in `rows`       no warning, the sites apart
#    the ARD filtered to one site  no warning
#    notes = FALSE                 still warns (lost numbers are not notes)
#    notes = TRUE                  warns once, not twice
#
#  (B) FT-AR111, decision table, stats x value x na (x a `cells` given):
#    stats | value     | na  | the table
#    cells | any       | NA  | text cells from `cells`, an empty cell NA
#    cells | any       | "-" | the same, an empty cell "-"
#    rows  | "stat"    | any | every statistic its own row labelled with its
#                              stat_label, numeric cells, an empty cell NA (na
#                              has no effect); `cells` is ignored
#    rows  | "stat_fmt"| NA  | the same rows, the ARD's formatted text, NA empty
#    rows  | "stat_fmt"| "-" | the same, an empty cell "-"
#  `value` has no effect with stats = "cells".
# ---------------------------------------------------------------------------

.wa_long1 <- function() {
  data.frame(TRT = c("Drug A", "Drug A", "Drug A", "Placebo", "Placebo"),
             SITE = NA_character_,
             SEX = c("F", "M", "U", "F", "M"),
             n = c(3, 2, 1, 4, 9), p = c(0.6, 0.4, 0.1, 0.4, 0.9),
             stringsAsFactors = FALSE)
}

.wa_long2 <- function() {
  trt <- rep(c("Drug A", "Placebo"), each = 6)
  site <- rep(rep(c("S1", "S2"), each = 3), 2)
  d <- data.frame(TRT = trt, SITE = site, SEX = rep(c("F", "M", "U"), 4),
                  n = c(3, 2, 1, 5, 4, 2, 4, 9, 6, 7, 8, 3),
                  p = c(0.6, 0.4, 0.1, 0.5, 0.4, 0.2, 0.4, 0.9, 0.6, 0.7, 0.8, 0.3),
                  stringsAsFactors = FALSE)
  d[-12L, ]
}

# the ARD, in the shape cards writes, of the counts
.wa_ard <- function(long, two) {
  one <- function(i) {
    r <- long[i, ]
    out <- data.frame(group1 = "TRT", group1_level = r$TRT, variable = "SEX",
                      variable_level = r$SEX, context = "tabulate",
                      stat_name = c("n", "p"), stat_label = c("n", "%"),
                      stat = c(r$n, r$p),
                      stat_fmt = c(as.character(r$n), sprintf("%.1f", 100 * r$p)),
                      stringsAsFactors = FALSE)
    if (two) {
      out$group2 <- "SITE"
      out$group2_level <- r$SITE
    }
    out
  }
  suppressMessages(normalize_ard(do.call(rbind, lapply(seq_len(nrow(long)), one))))
}

.wa_fmt <- function(cells, n, p) {
  if (cells == "n") as.character(n) else sprintf("%d (%.1f)", as.integer(n), 100 * p)
}

# the table the rules give, from the counts
.wa_expect <- function(long, r, two) {
  sex_levels <- unique(long$SEX)
  if (r$levels %in% c("sex", "both")) sex_levels <- c("U", "M", "F")
  trt_levels <- unique(long$TRT)
  if (r$levels %in% c("trt", "both")) trt_levels <- c("Placebo", "Drug A")
  combos <- unique(long[, c("TRT", "SITE")])
  combos <- combos[order(match(combos$TRT, trt_levels), seq_len(nrow(combos))), ]
  shown_trt <- ifelse(r$labels == "trt" & combos$TRT == "Drug A", "Active", combos$TRT)
  cname <- if (two) paste(shown_trt, combos$SITE, sep = r$sep) else shown_trt
  total <- vapply(sex_levels, function(s) sum(long$n[long$SEX == s]), 0)
  names(total) <- sex_levels
  ord <- sex_levels
  if (r$sort == "-n") ord <- sex_levels[order(-total)]
  if (r$sort == "n") ord <- sex_levels[order(total)]
  na <- if (r$na == "dash") "-" else NA_character_
  out <- data.frame(label = if (r$labels == "sex") c(F = "Female", M = "Male", U = "Unknown")[ord] else ord,
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  for (k in seq_len(nrow(combos))) {
    out[[cname[k]]] <- vapply(ord, function(s) {
      i <- which(long$SEX == s & long$TRT == combos$TRT[k] &
                   (!two | long$SITE == combos$SITE[k]))
      if (length(i)) .wa_fmt(r$cells, long$n[i], long$p[i]) else na
    }, "", USE.NAMES = FALSE)
  }
  if (r$sort_stat == "n") out$.sort_stat <- unname(total[ord])
  out
}

test_that("FT-AR110 [FR-AR150 FR-AR151 FR-AR160 FR-AR168 FR-AR170 FR-AR172 FR-AR173 FR-AR175 FR-AR177 FR-AR179 FR-AR180 FR-AR252 FR-IX409]: widen_ard(): columns x cells x levels x labels x sort x na x sep x sort_stat, pairwise", {
  cand <- expand.grid(cols = c("one", "two"), cells = c("n", "np"),
                      levels = c("none", "sex", "trt", "both"),
                      labels = c("none", "trt", "sex"), sort = c("no", "-n", "n"),
                      na = c("na", "dash"), sep = c("____", " / "),
                      sort_stat = c("none", "n"), stringsAsFactors = FALSE)
  expect_identical(nrow(cand), 1152L)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 60L)
  for (i in seq_len(nrow(ap$rows))) {
    r <- ap$rows[i, ]
    two <- r$cols == "two"
    long <- if (two) .wa_long2() else .wa_long1()
    x <- .wa_ard(long, two)
    lv <- list()
    if (r$levels %in% c("sex", "both")) lv$SEX <- c("U", "M", "F")
    if (r$levels %in% c("trt", "both")) lv$TRT <- c("Placebo", "Drug A")
    lb <- list()
    if (r$labels == "trt") lb$TRT <- c("Drug A" = "Active")
    if (r$labels == "sex") lb$SEX <- c(F = "Female", M = "Male", U = "Unknown")
    args <- list(x, cols = if (two) c("TRT", "SITE") else "TRT",
                 cells = if (r$cells == "n") "{n}" else "{n} ({p:.1f%})",
                 sort = if (r$sort == "no") FALSE else r$sort,
                 na = if (r$na == "dash") "-" else NA_character_, sep = r$sep,
                 notes = FALSE)
    if (length(lv)) args$levels <- lv
    if (length(lb)) args$labels <- lb
    if (r$sort_stat == "n") args$sort_stat <- "n"
    got <- do.call(widen_ard, args)
    want <- .wa_expect(long, r, two)
    got <- as.data.frame(got)
    # a label column whose order `levels` declared is a factor
    got$label <- as.character(got$label)
    expect_identical(got, want, info = paste(unlist(r), collapse = "/"))
  }
})

test_that("FT-AR111 [FR-AR166 FR-AR167 FR-AR180 FR-IX409]: widen_ard(): stats x value x na, the decision table", {
  long <- .wa_long1()
  x <- .wa_ard(long, FALSE)
  rows <- c(var = "variable", lv = ".label")
  w <- function(...) widen_ard(x, cols = "TRT", rows = rows, notes = FALSE, ...)
  # the table a rows layout gives: SEX x (F, M, U) x (n, %); Placebo's U is empty
  lv <- rep(c("F", "M", "U"), each = 2L)
  expect_stat <- function(g, fmt) {
    expect_identical(g$lv, lv)
    expect_identical(g$label, rep(c("n", "%"), 3L))
    if (fmt) {
      expect_identical(g$`Drug A`, c("3", "60.0", "2", "40.0", "1", "10.0"))
    } else {
      expect_equal(g$`Drug A`, c(3, 0.6, 2, 0.4, 1, 0.1))
    }
  }
  # stats = "rows", value = "stat": numeric, na has no effect, cells is ignored
  for (na in list(NA_character_, "-")) {
    g <- w(stats = "rows", value = "stat", na = na, cells = "{n}")
    expect_stat(g, FALSE)
    expect_type(g$Placebo, "double")
    expect_identical(g$Placebo[5:6], c(NA_real_, NA_real_))
  }
  # value = "stat_fmt": the ARD's text, an empty cell is `na`
  g <- w(stats = "rows", value = "stat_fmt")
  expect_stat(g, TRUE)
  expect_identical(g$Placebo, c("4", "40.0", "9", "90.0", NA, NA))
  g <- w(stats = "rows", value = "stat_fmt", na = "-")
  expect_stat(g, TRUE)
  expect_identical(g$Placebo, c("4", "40.0", "9", "90.0", "-", "-"))
  # stats = "cells": value has no effect, an empty cell is `na`
  wc <- function(...) widen_ard(x, cols = "TRT", cells = "{n}", notes = FALSE, ...)
  expect_identical(wc(value = "stat_fmt"), wc(value = "stat"))
  expect_identical(wc()$Placebo, c("4", "9", NA))
  expect_identical(wc(na = "-", value = "stat_fmt")$Placebo, c("4", "9", "-"))
})

test_that("FT-AR112 [FR-AR170 FR-AR175 FR-AR179 FR-AR252 FR-IX409]: widen_ard(): sort and sort_stat against labels, the decision table", {
  x <- .wa_ard(.wa_long1(), FALSE)
  # totals across the arms: F 3 + 4 = 7, M 2 + 9 = 11, U 1 (Placebo has none)
  tot <- c(F = 7, M = 11, U = 1)
  scoped <- list(SEX = c(F = "Female", M = "Male", U = "Unknown"))
  shown <- c(F = "Female", M = "Male", U = "Unknown")
  run <- function(labels, ...) {
    args <- list(x, cols = "TRT", cells = "{n}", notes = FALSE, ...)
    if (!is.null(labels)) args$labels <- labels
    as.data.frame(do.call(widen_ard, args))
  }
  # labels: none | scoped by the analysis variable; the rows' label text follows
  for (case in list(list(name = "no labels", labels = NULL, text = c(F = "F", M = "M", U = "U")),
                    list(name = "SEX scoped", labels = scoped, text = shown))) {
    nm <- case$name
    for (sort in list(FALSE, "-n", "n")) {
      want <- names(tot)
      if (identical(sort, "-n")) want <- names(sort(tot, decreasing = TRUE))
      if (identical(sort, "n")) want <- names(sort(tot))
      got <- run(case$labels, sort = sort)
      expect_identical(as.character(got$label), unname(case$text[want]), info = paste(nm, "sort", sort))
      # the counts stay with their rows
      expect_identical(got$`Drug A`, unname(c(F = "3", M = "2", U = "1")[want]), info = paste(nm, sort))
    }
    # sort_stat: each row's total, whatever the label shows
    got <- run(case$labels, sort_stat = "n")
    expect_identical(as.character(got$label), unname(case$text[names(tot)]), info = nm)
    expect_identical(got$.sort_stat, unname(tot), info = nm)
    # and a sort key with a declared order of the data's own values
    got <- run(case$labels, sort = "-n", levels = list(SEX = c("U", "M", "F")), sort_stat = "n")
    expect_identical(as.character(got$label), unname(case$text[c("M", "F", "U")]), info = nm)
    expect_identical(got$.sort_stat, c(11, 7, 1), info = nm)
  }
  # a recoded row key reads the same: the key's labels do not hide the totals
  got <- as.data.frame(widen_ard(x, cols = "TRT", rows = c(var = "variable"), cells = "{n}",
                                 notes = FALSE, labels = c(SEX = "Sex"), sort_stat = "n"))
  expect_identical(as.character(got$var), rep("Sex", 3L))
  expect_identical(got$.sort_stat, unname(tot))
})

test_that("FT-AR113 [FR-AR253 FR-IX409]: widen_ard(): records of different values in one cell warn, naming the key, the cell and the values; the first is kept", {
  x <- .wa_ard(.wa_long2(), TRUE)
  run <- function(x, ...) as.data.frame(widen_ard(x, cols = "TRT", cells = "{n}", ...))
  # SITE is left over: Drug A / F is 3 (S1) and 5 (S2); five cells in all
  # (Drug A F, M, U; Placebo F, M), the first of each printed
  w <- tryCatch(run(x, notes = FALSE), warning = function(w) conditionMessage(w))
  expect_match(w, "^widen_ard\\(\\): 5 cells are reached by records of different values")
  expect_match(w, "cell   : label = 'F', column 'Drug A'", fixed = TRUE)
  expect_match(w, "values : '3', '5'", fixed = TRUE)
  expect_match(w, "'SITE'", fixed = TRUE)
  expect_match(w, "the first is printed", fixed = TRUE)
  expect_match(w, "Add the key that tells the records apart to `rows`", fixed = TRUE)
  got <- suppressWarnings(run(x, notes = FALSE))
  expect_identical(got$`Drug A`[got$label == "F"], "3")
  expect_identical(got$Placebo[got$label == "F"], "4")
  # notes = TRUE: one warning, not two
  n <- 0L
  withCallingHandlers(suppressMessages(run(x)),
                      warning = function(w) { n <<- n + 1L; invokeRestart("muffleWarning") })
  expect_identical(n, 1L)
  # the key named in `rows` keeps the sites apart, quietly
  expect_no_warning(by_site <- run(x, rows = c(site = "SITE"), notes = FALSE))
  expect_identical(by_site$`Drug A`[by_site$site == "S2" & by_site$label == "F"], "5")
  # one record a cell: filtered to a site
  s1 <- x[!is.na(x$SITE) & x$SITE == "S1", ]
  expect_no_warning(one <- run(s1, notes = FALSE))
  expect_identical(one$`Drug A`[one$label == "F"], "3")
  # the same value twice is one value
  l2 <- .wa_long2()[1:6, ]
  l2$n[l2$SITE == "S2"] <- l2$n[l2$SITE == "S1"]
  expect_no_warning(same <- run(.wa_ard(l2, TRUE), notes = FALSE))
  expect_identical(same$`Drug A`[same$label == "F"], "3")
})
