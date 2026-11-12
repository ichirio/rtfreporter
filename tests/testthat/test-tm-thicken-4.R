# Thickening of the functional tests, batch 4: table_plan() and its verbs
# (area PL) and the ARD tools (area AR).  Each block asserts what its FR
# states, the figures worked out in a comment.  FT ids from 5301 up.

.t4_wide <- function() {
  data.frame(
    grp = c("A", "A", "B", "B", "C", "C"),
    label = c("n", "Mean", "n", "Mean", "n", "Mean"),
    X = c("1", "2.5", "3", "4.5", "5", "6.5"),
    Y = c("7", "8.5", "9", "10.5", "11", "12.5"),
    stringsAsFactors = FALSE)
}

test_that("FT-PL5301 [FR-PL446]: a row budget with split rows / none / by_value stops at plan_apply(), saying what to do", {
  p <- table_plan(.t4_wide())
  # split = "rows": the message names the budget and the split, and says to drop it
  e <- tryCatch(plan_apply(plan_paginate_rows(p, max_rows = 4, split = "rows", break_before = 3), "args"),
                error = function(e) conditionMessage(e))
  expect_match(e, "plan_paginate_rows(max_rows = 4) has no effect with split = 'rows'.", fixed = TRUE)
  expect_match(e, "Drop `max_rows`", fixed = TRUE)
  expect_match(e, "split = \"group_safe\" / \"group_force\"", fixed = TRUE)
  # split = "none"
  e <- tryCatch(plan_apply(plan_paginate_rows(p, max_rows = 4, split = "none"), "args"),
                error = function(e) conditionMessage(e))
  expect_match(e, "plan_paginate_rows(max_rows = 4) has no effect with split = 'none'.", fixed = TRUE)
  expect_match(e, "Drop `max_rows`", fixed = TRUE)
  # plan_paginate_group()'s by_value: another remedy is named
  e <- tryCatch(plan_apply(plan_paginate_rows(plan_paginate_group(p, col = "grp"), max_rows = 4), "args"),
                error = function(e) conditionMessage(e))
  expect_match(e, "plan_paginate_rows(max_rows = 4) has no effect with split = 'by_value'.", fixed = TRUE)
  expect_match(e, "it comes from plan_paginate_group()", fixed = TRUE)
  expect_match(e, "plan_paginate_rows(max_rows = 4, split = \"group_safe\")", fixed = TRUE)
  expect_match(e, "or drop the budget and keep the value split.", fixed = TRUE)
})

test_that("FT-PL5302 [FR-PL457]: plan_paginate_group(heading = , heading_at = ) that is not allowed stops, with the message", {
  p <- table_plan(.t4_wide())
  msg_h <- "plan_paginate_group(heading = ) is one text with `{value}` and `{n}`: \"Treatment: {value} (N={n})\"."
  for (bad in list(c("a", "b"), "", NA_character_, 1, character(0), TRUE)) {
    expect_error(plan_paginate_group(p, col = "grp", heading = bad), msg_h, fixed = TRUE)
  }
  msg_a <- "plan_paginate_group(heading_at = ) is one of \"section\", \"title\", \"none\"."
  for (bad in list("header", "Section", c("title", "none"), NA_character_, 1, character(0))) {
    expect_error(plan_paginate_group(p, col = "grp", heading_at = bad), msg_a, fixed = TRUE)
  }
  # the three allowed values and one text are not refused
  for (ok in c("section", "title", "none")) {
    expect_s3_class(plan_paginate_group(p, col = "grp", heading = "G: {value} (N={n})", heading_at = ok),
                    "table_plan")
  }
})

test_that("FT-PL5303 [FR-PL960]: plan_sort(keep = , stat = ) and plan_filter(by = ) refuse what is not allowed, with the message", {
  p <- table_plan(.t4_wide())
  mk <- "plan_sort(keep = ) is TRUE (the sort keys stay) or FALSE (hide them)."
  for (bad in list(NA, "yes", 1, c(TRUE, FALSE), c(TRUE, TRUE), logical(0))) {
    expect_error(plan_sort(p, "grp", keep = bad), mk, fixed = TRUE)
  }
  ms <- "plan_sort(stat = ) is one statistic's name, e.g. \"n\", or NULL."
  for (bad in list(NA_character_, c("n", "mean"), 1, "", character(0))) {
    expect_error(plan_sort(p, "-grp", stat = bad), ms, fixed = TRUE)
  }
  mb <- "plan_filter(by = ) is one row key's name, or NULL."
  for (bad in list(NA_character_, 1, c("grp", "label"), character(0))) {
    expect_error(plan_filter(p, X > 0, by = bad), mb, fixed = TRUE)
  }
  # the allowed values go through
  expect_s3_class(plan_sort(p, "grp", keep = FALSE), "table_plan")
  expect_s3_class(plan_sort(p, "-grp", stat = "n"), "table_plan")
  expect_s3_class(plan_sort(p, "grp", stat = NULL, keep = NULL), "table_plan")
  expect_s3_class(plan_filter(p, X > 0, by = "grp"), "table_plan")
  expect_s3_class(plan_filter(p, X > 0, by = NULL), "table_plan")
})

# -- helpers: small normalized ARDs ---------------------------------------

# AGE by arm (A, B): N, mean, sd.
.t4_ard <- function() {
  d <- data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 3L),
    variable = "AGE", variable_level = NA_character_, context = "continuous",
    stat_name = rep(c("N", "mean", "sd"), 2L),
    stat_label = rep(c("N", "Mean", "SD"), 2L),
    stat = c(10, 12.34, 1.2, 12, 15.67, 2.2), stringsAsFactors = FALSE)
  d$stat_fmt <- c("10", "12.3", "1.2", "12", "15.7", "2.2")
  normalize_ard(d)
}

# A hierarchical normalized ARD: the overall row, three SOCs with their terms.
# p per arm (A, B): S1 own .50/.50, P1 .10/.10, P2 .10/.05; S2 own .20/.20,
# P3 .35/.10; S3 own .60/.60, P4 .40/.40; n is p * 20.
.t4_hier <- function() {
  ents <- list(
    list("..ard_hierarchical_overall..", NA, NA, c(0.9, 0.9)),
    list("AESOC", "S1", "S1", c(0.5, 0.5)), list("AETERM", "S1", "P1", c(0.1, 0.1)),
    list("AETERM", "S1", "P2", c(0.1, 0.05)),
    list("AESOC", "S2", "S2", c(0.2, 0.2)), list("AETERM", "S2", "P3", c(0.35, 0.1)),
    list("AESOC", "S3", "S3", c(0.6, 0.6)), list("AETERM", "S3", "P4", c(0.4, 0.4)))
  out <- list()
  for (e in ents) for (k in 1:2) for (sn in c("n", "p")) {
    term <- identical(e[[1L]], "AETERM")
    out[[length(out) + 1L]] <- data.frame(
      group1 = "TRT", group1_level = c("A", "B")[k],
      group2 = if (term) "AESOC" else NA_character_,
      group2_level = if (term) e[[2L]] else NA_character_,
      variable = e[[1L]], variable_level = if (is.na(e[[3L]])) NA_character_ else e[[3L]],
      context = "hierarchical", stat_name = sn, stat_label = sn,
      stat = if (sn == "n") round(e[[4L]][k] * 20) else e[[4L]][k],
      stringsAsFactors = FALSE)
  }
  normalize_ard(do.call(rbind, out), hierarchy = c("AESOC", "AETERM"), overall = "Any TEAE")
}
.t4_hier_plan <- function() {
  table_plan(.t4_hier(), cols = "TRT", rows = c(soc = "AESOC", pt = "AETERM")) |>
    plan_cells("{n} ({p:.1f%})", notes = FALSE)
}
# Two column keys (TRT, SEX): one mean per cell.
.t4_ard2 <- function() {
  g <- expand.grid(sx = c("F", "M"), trt = c("Placebo", "Drug"), stringsAsFactors = FALSE)
  normalize_ard(data.frame(
    group1 = "TRT", group1_level = g$trt, group2 = "SEX", group2_level = g$sx,
    variable = "AGE", variable_level = NA_character_, context = "continuous",
    stat_name = "mean", stat_label = "Mean", stat = c(2.5, 4.5, 6.5, 8.5),
    stringsAsFactors = FALSE))
}
.t4_tab <- function(p) as.data.frame(plan_apply(p, "table"))

test_that("FT-PL5304 [FR-PL201]: plan_apply(stage = 'input') is the frame going in: roles copied, label coalesced, overall rows relabelled; a finished table or listing is the data", {
  d <- data.frame(TRT = rep(c("A", "B"), each = 4L), PC = rep(c("ALT", "AST"), 4L),
                  STAT = rep(c("n", "mean"), each = 2L, times = 2L),
                  VAL = c(20, 19, 31.2, 28.7, 18, 17, 33.1, 30.2),
                  LBL = c("a", NA, "c", NA, NA, "f", NA, "h"), stringsAsFactors = FALSE)
  p <- table_plan(d, cols = "TRT", rows = c(param = "PC"), label = c("LBL", "STAT"),
                  stat = c(variable = "PC", name = "STAT", value = "VAL"))
  x <- plan_apply(p, "input")
  # the stat = roles' columns copied to variable / stat_name / stat
  expect_identical(x$variable, d$PC)
  expect_identical(x$stat_name, d$STAT)
  expect_identical(x$stat, d$VAL)
  # a coalesced label column: LBL where it is set, STAT where it is not
  expect_identical(x$label, c("a", "n", "c", "mean", "n", "f", "mean", "h"))
  # the source columns are still there
  expect_identical(x[names(d)], d)
  # plan_total(): the overall rows (TRT missing) are relabelled with its label
  o <- d[d$TRT == "A", ]
  o$TRT <- NA
  d2 <- rbind(d, o)
  p2 <- table_plan(d2, cols = "TRT", rows = c(param = "PC"), label = c("LBL", "STAT"),
                   stat = c(variable = "PC", name = "STAT", value = "VAL")) |>
    plan_total(label = "All")
  x2 <- plan_apply(p2, "input")
  expect_identical(x2$TRT[1:8], d$TRT)
  expect_identical(x2$TRT[9:12], rep("All", 4L))
  # a finished table or a listing: the data itself
  wide <- data.frame(label = c("a", "b"), v = c("1.5", "2.5"), stringsAsFactors = FALSE)
  expect_identical(plan_apply(table_plan(wide) |> plan_columns(), "input"), wide)
  expect_identical(plan_apply(table_plan(wide) |> plan_listing(listing_col("label", width = 5),
                                                                listing_col("v", width = 5)),
                              "input"), wide)
})

test_that("FT-PL5305 [FR-PL211 FR-PL212 FR-PL213]: plan_layers() declared order, merged layers, columns and parsed cells / compare", {
  p <- table_plan(.t4_ard(), cols = "TRT") |>
    plan_cells("{mean}", notes = FALSE) |>
    plan_levels(TRT = c("B", "A")) |> plan_levels(SEX = c("M", "F")) |>
    plan_labels(TRT = c(A = "Active")) |> plan_labels(AGE = "Age") |>
    plan_after(function(x) x) |> plan_after(function(x) x) |>
    plan_cell_style(cols = "A", bold = TRUE) |> plan_cell_style(cols = "B", italic = TRUE) |>
    plan_levels(TRT = c("A", "B"))
  lay <- plan_layers(p)
  # declared: the kinds in declaration order, one entry a call
  expect_identical(lay$declared, c("cells", "cell_options", "levels", "levels", "labels", "labels",
                                   "after", "after", "restyle", "restyle", "levels"))
  # levels and labels merge one key at a time (the same key restated is replaced)
  expect_identical(lay$layers$levels$levels, list(TRT = c("A", "B"), SEX = c("M", "F")))
  expect_identical(names(lay$layers$labels$labels), c("TRT", "AGE"))
  expect_identical(lay$layers$labels$labels$AGE, "Age")
  expect_identical(unname(lay$layers$labels$labels$TRT), "Active")
  # after and restyle: one entry per call
  expect_length(lay$layers$after, 2L)
  expect_length(lay$layers$restyle, 2L)
  # columns: names, spread, page_names (TRT's A is printed as "Active" by plan_labels());
  # widths NULL when plan_columns(widths = ) is not said
  expect_identical(lay$columns$names, c("label", "Active", "B"))
  expect_identical(lay$columns$spread, c("Active", "B"))
  expect_identical(lay$columns$page_names, c("label", "Active", "B"))
  expect_null(lay$columns$widths)
  # ... and the col_rel_width it resolves to when it is: label 3, the two values 1 each
  w <- plan_layers(p |> plan_columns(widths = c(label = 3, .values = 1)))$columns
  expect_identical(w$names, c("label", "Active", "B"))
  expect_equal(w$widths, c(3, 1, 1))
  # cells: each template parsed (variable, context, entry labels and chains)
  lc <- plan_layers(table_plan(.t4_ard(), cols = "TRT") |>
                      plan_cells("AGE\rcontinuous" = c(Mean = "{mean:.1f}", n = "{N}"), notes = FALSE))
  expect_length(lc$cells, 1L)
  expect_identical(lc$cells[[1L]]$variable, "AGE")
  expect_identical(lc$cells[[1L]]$context, "continuous")
  expect_identical(lc$cells[[1L]]$entry$labels, c("Mean", "n"))
  expect_identical(lc$cells[[1L]]$entry$chains[[1L]][[1L]]$tpl, "{mean:.1f}")
  expect_identical(lc$cells[[1L]]$entry$chains[[2L]][[1L]]$tpl, "{N}")
  # NULL when the statistics are rows or no template is declared
  expect_null(plan_layers(table_plan(.t4_ard(), cols = "TRT") |>
                            plan_cells(stats = "rows", notes = FALSE))$cells)
  expect_null(suppressMessages(plan_layers(table_plan(.t4_ard(), cols = "TRT") |> plan_columns()))$cells)
  # compare: the columns parsed with by and position; NULL without one
  expect_null(lc$compare)
  cmp <- plan_layers(p |> plan_compare(p = c(stats_t_test = "{p.value:.3f}"), by = "variable",
                                       position = "first"))$compare
  expect_identical(cmp$by, "variable")
  expect_identical(cmp$position, "first")
  expect_identical(cmp$cols[[1L]]$column, "p")
  expect_identical(cmp$cols[[1L]]$context, "stats_t_test")
  expect_identical(cmp$cols[[1L]]$entry$chains[[1L]][[1L]]$tpl, "{p.value:.3f}")
  cmp2 <- plan_layers(p |> plan_compare(p = "{p.value:.3f}"))$compare
  expect_null(cmp2$by)
  expect_identical(cmp2$position, "last")
})

test_that("FT-PL5306 [FR-PL227]: one unnamed entry is the default; variable, context, kind, stat group and default entries are picked by specificity", {
  b <- function(v, x, ctx) data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L), variable = v,
    variable_level = NA_character_, context = ctx, stat_name = rep(c("N", "mean"), 2L),
    stat_label = rep(c("N", "Mean"), 2L), stat = x, stringsAsFactors = FALSE)
  m <- normalize_ard(rbind(
    b("AGE", c(10, 12.34, 12, 15.67), "mysummary"),
    b("HGT", c(10, 150.123, 12, 160.456), "mysummary"),
    b("WGT", c(10, 70.1, 12, 71.2), "other"),
    data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L), variable = "SEX",
               variable_level = "F", context = "categorical", stat_name = rep(c("n", "p"), 2L),
               stat_label = rep(c("n", "%"), 2L), stat = c(4, 0.4, 5, 0.5),
               stringsAsFactors = FALSE)))
  tab <- .t4_tab(table_plan(m, cols = "TRT") |>
                   plan_cells("{n}", continuous = "{mean:.1f}", mysummary = "{N}",
                              HGT = "{mean:.2f}", notes = FALSE))
  # a variable's entry (HGT) before its context's (mysummary: AGE) before its kind's
  # (continuous: WGT) before the one unnamed entry, the default (SEX: n)
  expect_identical(tab$group, c("AGE", "HGT", "WGT", "SEX"))
  expect_identical(tab$A, c("10", "150.12", "70.1", "4"))
  expect_identical(tab$B, c("12", "160.46", "71.2", "5"))
  # the one unnamed entry is the plan-wide default, as `default =` says it
  only <- .t4_tab(table_plan(m, cols = "TRT") |> plan_cells("{n}", notes = FALSE))
  named <- .t4_tab(table_plan(m, cols = "TRT") |> plan_cells(default = "{n}", notes = FALSE))
  expect_identical(only, named)
  # (the continuous records have no `n`, only `N`: their cells are empty)
  expect_identical(only$A, c(NA, NA, NA, "4"))
  # a stat group's entry: each group of the record takes its own template
  g <- table_plan(.t4_ard(), cols = c("TRT", ".stat_group"), rows = c(group = "variable")) |>
    plan_stat_groups(N = "N", msd = c("mean", "sd")) |>
    plan_cells(N = "{N}", msd = "{mean:.1f} ({sd:.1f})", notes = FALSE)
  gt <- .t4_tab(g)
  expect_identical(names(gt), c("group", "label", "A____N", "A____msd", "B____N", "B____msd"))
  expect_identical(gt$A____N, "10")
  expect_identical(gt$A____msd, "12.3 (1.2)")
  expect_identical(gt$B____N, "12")
  expect_identical(gt$B____msd, "15.7 (2.2)")
})

test_that("FT-PL5307 [FR-PL232]: plan_cells(notes = TRUE) says which statistics no template used, notes = FALSE says nothing, also under rtf_tables()", {
  d <- .t4_ard()
  said <- function(f) {
    out <- character()
    withCallingHandlers(f(), message = function(m) {
      out <<- c(out, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
    paste(out, collapse = "\n")
  }
  # the default is notes = TRUE: only the mean is used, N and sd are not (4 of the 6 ARD rows)
  loud <- table_plan(d, cols = "TRT") |> plan_cells("{mean}")
  m1 <- said(function() plan_apply(loud, "table"))
  expect_match(m1, "4 ARD rows were not used", fixed = TRUE)
  expect_match(m1, "no template named it", fixed = TRUE)
  expect_identical(said(function() plan_apply(loud |> plan_cells("{mean}", notes = TRUE), "table")), m1)
  # when rtf_tables() runs the plan
  expect_match(said(function() rtf_tables(rtf_document(), loud)), "no template named it", fixed = TRUE)
  # notes = FALSE: nothing, by plan_apply() or by rtf_tables()
  quiet <- table_plan(d, cols = "TRT") |> plan_cells("{mean}", notes = FALSE)
  expect_identical(said(function() plan_apply(quiet, "table")), "")
  expect_identical(said(function() rtf_tables(rtf_document(), quiet)), "")
})

test_that("FT-PL5308 [FR-PL268]: two column keys fold the innermost into <value>____Total; records with no outer key make no column; no overall rows stops", {
  rec <- function(g1, l1, g2, l2, x) data.frame(
    group1 = g1, group1_level = l1, group2 = g2, group2_level = l2, variable = "AGE",
    variable_level = NA_character_, context = "continuous", stat_name = "mean",
    stat_label = "Mean", stat = x, stringsAsFactors = FALSE)
  na <- NA_character_
  cells <- rec("TRT", rep(c("A", "B"), each = 2L), "SEX", rep(c("F", "M"), 2L), c(1, 2, 3, 4))
  # no overall rows at all: stops, naming the key the rows lack
  expect_error(
    .t4_tab(table_plan(normalize_ard(cells), cols = c("TRT", "SEX")) |>
              plan_cells("{mean}", notes = FALSE) |> plan_total()),
    "plan_total(): the ARD has no overall rows, rows without SEX.", fixed = TRUE)
  # with them: records without SEX (10, 20 per arm), the A and B Total columns
  t2 <- normalize_ard(rbind(
    cells,
    rec("TRT", c("A", "B"), na, na, c(10, 20)),
    rec("SEX", c("F", "M"), na, na, c(30, 40)),          # SEX set, no TRT
    rec(na, na, na, na, 99)))                             # neither key
  tab <- .t4_tab(table_plan(t2, cols = c("TRT", "SEX")) |> plan_cells("{mean}", notes = FALSE) |>
                   plan_total())
  expect_identical(names(tab), c("label", "A____F", "A____M", "A____Total",
                                 "B____F", "B____M", "B____Total"))
  expect_false("NA____Total" %in% names(tab))
  expect_false(any(grepl("^NA", names(tab))))
  expect_identical(tab[1L, c("A____F", "A____M", "A____Total", "B____F", "B____M", "B____Total")],
                   data.frame(A____F = "1", A____M = "2", A____Total = "10",
                              B____F = "3", B____M = "4", B____Total = "20",
                              stringsAsFactors = FALSE))
})

test_that("FT-PL5309 [FR-PL304 FR-PL305]: plan_sort() on an ARD plan is widen_ard(sort = ) only; on a finished table as_rtftables(sort_by = ) with sort_desc; keys by output name", {
  p <- .t4_hier_plan()
  a <- plan_apply(p |> plan_sort(".overall", "soc", "-n"), "args")
  expect_identical(a$widen$sort, c(".overall", "soc", "-n"))   # as widen_ard(sort = )
  expect_null(a$rtf$sort_by)                                    # and nothing reaches as_rtftables()
  expect_null(a$rtf$sort_desc)
  # a finished table: as_rtftables(sort_by = ), sort_desc from "-name"
  w <- data.frame(g = c("b", "a", "c"), v = c(2, 3, 1), stringsAsFactors = FALSE)
  d1 <- plan_apply(table_plan(w) |> plan_sort("-v"), "args")$rtf
  expect_identical(d1$sort_by, "v")
  expect_true(d1$sort_desc)
  d2 <- plan_apply(table_plan(w) |> plan_sort("v"), "args")$rtf
  expect_identical(d2$sort_by, "v")
  expect_null(d2$sort_desc)                                     # ascending: no sort_desc
  expect_null(plan_apply(table_plan(w) |> plan_sort("-v"), "args")$widen$sort)
  # a key is a row key or the label column by its output name; "-name" descending.
  # The label column: Any TEAE < P1 < P2 < P3 < P4 < S1 < S2 < S3 (A before P before S).
  asc <- .t4_tab(p |> plan_sort("label"))$label
  expect_identical(asc, c("Any TEAE", "P1", "P2", "P3", "P4", "S1", "S2", "S3"))
  expect_identical(.t4_tab(p |> plan_sort("-label"))$label, rev(asc))
  # a row key (soc), ascending and descending, the rows of a group together
  s_asc <- .t4_tab(p |> plan_sort("soc"))
  expect_identical(s_asc$soc, c("Any TEAE", "S1", "S1", "S1", "S2", "S2", "S3", "S3"))
  s_desc <- .t4_tab(p |> plan_sort("-soc"))
  expect_identical(s_desc$soc, c("S3", "S3", "S2", "S2", "S1", "S1", "S1", "Any TEAE"))
  # a row key's empty value (a group's own row) comes first in its group, either way
  for (key in c("pt", "-pt")) {
    tab <- .t4_tab(p |> plan_sort("soc", key))
    for (s in c("S1", "S2", "S3")) expect_true(is.na(tab$pt[tab$soc == s][1L]), info = paste(key, s))
  }
  expect_identical(.t4_tab(p |> plan_sort("soc", "-pt"))$label[2:4], c("S1", "P2", "P1"))
  expect_identical(.t4_tab(p |> plan_sort("soc", "pt"))$label[2:4], c("S1", "P1", "P2"))
})

test_that("FT-PL5310 [FR-PL307]: '<stat>@<key>' carries the group's own row's value to its rows; .overall first; .depth puts a level's own row before its nested rows", {
  p <- .t4_hier_plan()
  # -n@soc: SOCs by their own count (S3 12, S1 10, S2 4 in arm A), each with its PTs;
  # the overall row (18) is its own group and comes first
  a <- .t4_tab(p |> plan_sort("-n@soc"))
  expect_identical(a$label, c("Any TEAE", "S3", "P4", "S1", "P1", "P2", "S2", "P3"))
  # ascending: S2 (4), S1 (10), S3 (12), the overall row (18) last
  b <- .t4_tab(p |> plan_sort("n@soc"))
  expect_identical(b$label, c("S2", "P3", "S1", "P1", "P2", "S3", "P4", "Any TEAE"))
  # .overall puts the overall rows first, whichever way the rest is sorted
  o <- .t4_tab(p |> plan_sort(".overall", "-label"))
  expect_identical(o$label[1L], "Any TEAE")
  expect_identical(o$label[-1L], c("S3", "S2", "S1", "P4", "P3", "P2", "P1"))
  # .depth: a level's own row (the SOC, with no pt) before the rows nested under it
  # even though "label" alone would print the terms (P..) before their SOC (S..)
  d <- .t4_tab(p |> plan_sort("soc", ".depth", "label"))
  expect_identical(d$label, c("Any TEAE", "S1", "P1", "P2", "S2", "P3", "S3", "P4"))
  expect_identical(.t4_tab(p |> plan_sort("soc", "label"))$label[1:4], c("Any TEAE", "P1", "P2", "S1"))
})

test_that("FT-PL5311 [FR-PL313]: plan_sort() twice uses the last call's keys; keep = left out keeps the earlier; sorting alone gives no pages", {
  p <- .t4_hier_plan()
  expect_identical(plan_apply(p |> plan_sort("-label") |> plan_sort("label"), "args")$widen$sort, "label")
  expect_identical(.t4_tab(p |> plan_sort("-label") |> plan_sort("label")), .t4_tab(p |> plan_sort("label")))
  # keep = FALSE said once stays unless the later call says otherwise
  hid <- p |> plan_sort("soc", keep = FALSE) |> plan_sort("pt")
  expect_false("pt" %in% names(plan_apply(hid |> plan_columns(), "pages")[[1L]]$data))
  expect_true("soc" %in% names(plan_apply(hid |> plan_columns(), "pages")[[1L]]$data))
  keep <- p |> plan_sort("soc", keep = FALSE) |> plan_sort("pt", keep = TRUE)
  expect_true("pt" %in% names(plan_apply(keep |> plan_columns(), "pages")[[1L]]$data))
  # plan_sort() alone: the table, not pages
  expect_s3_class(plan_apply(p |> plan_sort("-label")), "data.frame")
  expect_type(plan_apply(p |> plan_sort("-label") |> plan_columns()), "list")
})

test_that("FT-PL5312 [FR-PL331]: plan_empty() twice: the later text, when, position and footnote win; a field left out keeps the earlier", {
  p <- table_plan(.t4_hier(), cols = "TRT", rows = c(soc = "AESOC")) |> plan_cells("{n}", notes = FALSE)
  em <- function(p) plan_layers(p)$layers$empty
  all1 <- p |> plan_empty(text = "none", when = "no_rows", position = "first", footnote = "f1")
  # the later call's values win, field by field
  all2 <- all1 |> plan_empty(text = "nil", when = "overall_zero", position = "replace", footnote = "f2")
  expect_identical(em(all1)[c("text", "when", "position", "footnote")],
                   list(text = "none", when = "no_rows", position = "first", footnote = "f1"))
  expect_identical(em(all2)[c("text", "when", "position", "footnote")],
                   list(text = "nil", when = "overall_zero", position = "replace", footnote = "f2"))
  # a field left out keeps the earlier call's
  for (f in c("text", "when", "position", "footnote")) {
    one <- do.call(plan_empty, c(list(all1), stats::setNames(list(em(all2)[[f]]), f)))
    kept <- setdiff(c("text", "when", "position", "footnote"), f)
    expect_identical(em(one)[kept], em(all1)[kept], info = f)
    expect_identical(em(one)[[f]], em(all2)[[f]], info = f)
  }
})

.t4_template <- function(...) {
  code <- NULL
  utils::capture.output(code <- plan_template(...))
  code
}

test_that("FT-PL5313 [FR-PL350 FR-PL355]: plan_template() writes the table_plan pipeline (rows, cells, display verbs) and, with form = 'widen', the immediate form with the keys outside cols as rows", {
  ard <- .t4_hier()
  out <- utils::capture.output(res <- withVisible(plan_template(ard, cols = "TRT")))
  expect_false(res$visible)
  expect_type(res$value, "character")
  expect_gt(length(out), 5L)                          # it prints the code as well
  txt <- paste(res$value, collapse = "\n")
  has <- function(x, t = txt) grepl(x, t, fixed = TRUE)
  # the ARD half: normalize_ard(), table_plan(cols, rows), plan_cells() with the ARD's statistics
  expect_true(has("rtfreporter::normalize_ard("))
  expect_true(has("cols = \"TRT\",\n    rows = c(AESOC = \"AESOC\")"))
  expect_true(has("categorical = \"{n} ({p:.1f%})\""))
  # the display half, in this order
  verbs <- c("plan_stub(\n    name = \"row_label\"", "plan_row_group(mode = \"indent\")",
             "plan_blanks(\"between_groups\")",
             "plan_paginate_rows(max_rows = 22, split = \"group_safe\")",
             "plan_style(border = \"tfl\")")
  at <- vapply(verbs, function(v) regexpr(v, txt, fixed = TRUE)[[1L]], 1L)
  expect_true(all(at > 0L))
  expect_false(is.unsorted(at))
  expect_true(regexpr("table_plan(", txt, fixed = TRUE)[[1L]] < at[[1L]])
  # the code evaluates to a table_plan that runs to RTF pages
  skip_if_not_installed("magrittr")
  # (a raw-looking ARD without a hierarchy, as normalize_ard() reads it)
  one <- .t4_template(.t4_ard(), cols = "TRT")
  p <- eval(parse(text = one), envir = list2env(list(ard = .t4_ard()), parent = globalenv()))
  expect_s3_class(p, "table_plan")
  pages <- suppressMessages(plan_apply(p, "pages"))
  expect_s3_class(pages[[1L]], "rtftable")
  # form = "widen": normalize_ard() into widen_ard(), then as_rtftables()
  w <- .t4_template(.t4_ard2(), cols = "TRT", form = "widen")
  wt <- paste(w, collapse = "\n")
  expect_true(has("generated by rtfreporter::plan_template(form = \"widen\")", wt))
  pos <- function(x) regexpr(x, wt, fixed = TRUE)[[1L]]
  expect_true(pos("normalize_ard(") > 0L)
  expect_true(pos("normalize_ard(") < pos("widen_ard("))
  expect_true(pos("widen_ard(") < pos("as_rtftables("))
  expect_false(has("table_plan(", wt))
  # the keys outside cols are the rows (SEX), and the note says so
  expect_true(has("cols  = \"TRT\",\n    rows  = c(SEX = \"SEX\")", wt))
  expect_true(has("NOTE: keys outside `cols` became row keys: SEX", wt))
  expect_no_error(parse(text = w))
})

test_that("FT-PL5314 [FR-PL352]: plan_template() writes plan_col_header(values = list(n = TRUE), header = function(n) ...) only for one cols key with one denominator per column; otherwise a comment, and the code still parses", {
  one <- .t4_template(.t4_ard(), cols = "TRT")
  txt <- paste(one, collapse = "\n")
  expect_true(grepl("rtfreporter::plan_col_header(\n    values = list(n = TRUE),\n    header = function(n)", txt, fixed = TRUE))
  # its "\n" is written as one backslash and an n (R source), not a line break
  hdr <- grep("header = function(n)", one, fixed = TRUE, value = TRUE)
  expect_length(hdr, 1L)
  expect_true(grepl("\\nN = ", hdr, fixed = TRUE))          # backslash, n, "N = "
  expect_false(grepl("\nN = ", hdr, fixed = TRUE))          # not a real line break
  expect_no_error(parse(text = one))
  # it follows the last display verb with a pipe, and the pipeline's last line has none
  expect_true(any(grepl("plan_style(border = \"tfl\") %>%", one, fixed = TRUE)))
  # no denominator in the ARD (N dropped): no header verb, a comment says so, it parses
  ard <- .t4_ard()
  none <- .t4_template(ard[!ard$stat_name %in% "N", ], cols = "TRT")
  expect_false(any(grepl("rtfreporter::plan_col_header(", none, fixed = TRUE)))
  expect_true(startsWith(none[grep("plan_style(", none, fixed = TRUE) + 1L], "# "))   # a comment follows
  expect_no_error(parse(text = none))
  last <- grep("plan_style(border = \"tfl\")", none, fixed = TRUE, value = TRUE)
  expect_false(grepl("%>%[[:space:]]*$", last))
  # two cols keys: no header verb either, and a comment on header_sep
  two <- .t4_template(.t4_ard2(), cols = c("TRT", "SEX"))
  expect_false(any(grepl("rtfreporter::plan_col_header(", two, fixed = TRUE)))
  expect_true(any(grepl("^# Several column keys", two)))
  expect_no_error(parse(text = two))
  last2 <- grep("plan_style(border = \"tfl\")", two, fixed = TRUE, value = TRUE)
  expect_false(grepl("%>%[[:space:]]*$", last2))
})

test_that("FT-PL5315 [FR-PL358]: plan_template(pipe = NULL) follows the option, then RStudio's preference, then %>%; another value stops", {
  ard <- .t4_ard()
  old <- options(rtfreporter.ard_pipe = NULL)
  on.exit(options(old), add = TRUE)
  pipe_in <- function(code) {
    line <- grep("^p <- ard ", code, value = TRUE)[1L]
    sub("^.* ([^ ]+)$", "\\1", line)
  }
  testthat::local_mocked_bindings(.ard_pipe_rstudio = function() "|>", .package = "rtfreporter")
  # the option before RStudio's preference
  expect_identical(pipe_in(.t4_template(ard, cols = "TRT", pipe = NULL)), "|>")   # RStudio's
  options(rtfreporter.ard_pipe = "%>%")
  expect_identical(pipe_in(.t4_template(ard, cols = "TRT", pipe = NULL)), "%>%")  # the option wins
  options(rtfreporter.ard_pipe = "|>")
  testthat::local_mocked_bindings(.ard_pipe_rstudio = function() "%>%", .package = "rtfreporter")
  expect_identical(pipe_in(.t4_template(ard, cols = "TRT", pipe = NULL)), "|>")   # the option wins again
  # neither: %>%
  options(rtfreporter.ard_pipe = NULL)
  testthat::local_mocked_bindings(.ard_pipe_rstudio = function() NULL, .package = "rtfreporter")
  expect_identical(pipe_in(.t4_template(ard, cols = "TRT", pipe = NULL)), "%>%")
  # another value stops
  expect_error(plan_template(ard, cols = "TRT", pipe = "&&"),
               "`pipe` must be \"%>%\" (magrittr), \"|>\"", fixed = TRUE)
})

test_that("FT-PL5316 [FR-PL361 FR-PL476]: the derived stub is the row keys (or group) and the label, less page keys and hidden columns; groups are the outermost printed row key, never a page key", {
  # rows left to the analysis variable: the key is `group` (AGE and HGT here)
  b <- function(v, x) data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L), variable = v,
    variable_level = NA_character_, context = "continuous", stat_name = rep(c("N", "mean"), 2L),
    stat_label = rep(c("N", "Mean"), 2L), stat = x, stringsAsFactors = FALSE)
  m <- normalize_ard(rbind(b("AGE", c(10, 12.34, 12, 15.67)), b("HGT", c(10, 150.1, 12, 160.2))))
  g <- table_plan(m, cols = "TRT") |> plan_cells(continuous = "{mean}", notes = FALSE)
  expect_identical(names(plan_apply(g |> plan_stub(), "pages")[[1L]]$data), c("group / label", "A", "B"))
  # three row keys: site (constant), soc, pt; the table itself
  d <- .t4_hier()
  d$site <- "Site 1"
  p <- table_plan(d, cols = "TRT", rows = c(site = "site", soc = "AESOC", pt = "AETERM")) |>
    plan_cells("{n}", notes = FALSE)
  expect_identical(names(plan_apply(p |> plan_stub(), "pages")[[1L]]$data)[1L], "site / soc / pt / label")
  # less the page key (plan_paginate_group()) and the hidden columns (plan_hide())
  q <- p |> plan_paginate_group(col = "site", keep = FALSE) |> plan_hide("pt") |> plan_stub()
  expect_identical(names(plan_apply(q, "pages")[[1L]]$data), c("soc / label", "A", "B"))
  # ... and the page_by key of plan_paginate_rows()
  r <- p |> plan_paginate_rows(page_by = "soc", max_rows = 20, split = "group_safe") |>
    plan_hide("soc") |> plan_stub()
  expect_identical(names(plan_apply(r, "pages")[[1L]]$data)[1L], "site / pt / label")
  # groups: the outermost PRINTED row key.  Site is the page key, so the blank rows
  # between groups fall between the SOCs (Any TEAE | S1 P1 P2 | S2 P3 | S3 P4: 3 gaps).
  # (blank_rows are the positions after which a blank row is inserted)
  gaps <- function(p) plan_apply(p, "pages")[[1L]]$blank_rows
  expect_identical(gaps(p |> plan_paginate_group(col = "site", keep = FALSE) |> plan_blanks("between_groups") |>
                          plan_stub()), c(1L, 4L, 6L))   # after Any TEAE (1), S1's P2 (4), S2's P3 (6)
  # with the site printed, the one site is the one group: no blank row between groups
  expect_length(gaps(p |> plan_blanks("between_groups") |> plan_stub()), 0L)
  # folded into a stub, the stub's headings are the outermost printed key's values (the SOCs)
  pg <- plan_apply(p |> plan_paginate_group(col = "site", keep = FALSE) |> plan_stub(), "pages")[[1L]]
  stub <- sub("^[^[:alnum:]]+", "", as.character(pg$data[[1L]]))
  expect_identical(stub, c("Any TEAE", "S1", "P1", "P2", "S2", "P3", "S3", "P4"))
  expect_identical(grepl("^[^[:alnum:]]", as.character(pg$data[[1L]])),
                   c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE))   # the terms are indented under the SOC
})

test_that("FT-PL5317 [FR-PL400]: a hidden column still does its work: a page_by key hidden cuts and names the pages; positions count the printed columns", {
  w <- data.frame(grp = c("A", "A", "B", "B"), label = c("n", "Mean", "n", "Mean"),
                  X = c("1", "2.5", "3", "4.5"), Y = c("7", "8.5", "9", "10.5"), stringsAsFactors = FALSE)
  p <- table_plan(w) |> plan_paginate_rows(page_by = "grp", max_rows = 10, split = "group_safe") |>
    plan_hide("grp")
  pg <- plan_apply(p, "pages")
  expect_identical(names(pg), c("A", "B"))                      # the hidden key names the pages
  expect_identical(names(pg[[1L]]$data), c("label", "X", "Y"))   # and is not printed
  expect_identical(pg[[1L]]$data$X, c("1", "2.5"))               # page A is grp == "A" (2 rows)
  expect_identical(pg[[2L]]$data$X, c("3", "4.5"))
  # positions count the printed columns: widths (3 printed: label, X, Y), row_title
  q <- p |> plan_columns(widths = c(4, 1, 2), row_title = 2L)
  pq <- plan_apply(q, "pages")[[1L]]
  expect_equal(pq$col_rel_width, c(4, 1, 2))
  expect_identical(pq$row_title, 2L)                             # printed column 2 is X
  expect_identical(names(pq$data)[pq$row_title], "X")
  # as_rtftables() reads row_title in the table's own positions (grp is column 1): X is 3
  expect_identical(plan_apply(q, "args")$rtf$row_title, 3L)
  # without the hiding the same number is the table's second column (label)
  expect_identical(plan_apply(table_plan(w) |> plan_columns(row_title = 2L), "args")$rtf$row_title, 2L)
  # the same for plan_row_group(collapse = ): printed column 1 is the table's label (2)
  expect_identical(plan_apply(p |> plan_row_group(collapse = 1L), "args")$rtf$collapse_repeats, 2L)
  # a position beyond the printed columns stops, saying what is printed
  expect_error(plan_apply(p |> plan_columns(row_title = 4L), "args"),
               "the table prints 3 columns (label, X, Y); grp hidden", fixed = TRUE)
})

test_that("FT-PL5318 [FR-PL126 FR-PL128]: table_plan() refuses a raw cards ARD or a non-data-frame, and a stat = naming parts wrongly", {
  # a raw cards ARD (context and the group pairs, no .label / .kind) and anything that is no data frame
  raw <- data.frame(group1 = "TRT", group1_level = c("A", "B"), variable = "AGE",
                    context = "continuous", stat_name = "mean", stat_label = "Mean",
                    stat = I(list(1.5, 2.5)))
  msg <- "table_plan() takes the NORMALIZED frame, not a raw ARD, so that `cols` / `rows` / `label`"
  expect_error(table_plan(raw, cols = "group1_level"), msg, fixed = TRUE)
  expect_error(table_plan(raw, cols = "group1_level"), "normalize_ard() |>", fixed = TRUE)   # the line to write
  for (x in list(1:3, "x", list(a = 1), NULL)) {
    expect_error(table_plan(x, cols = "TRT"), if (is.null(x)) "`x` (the data) is required." else msg, fixed = TRUE)
  }
  # the normalized frame is taken
  expect_s3_class(table_plan(.t4_ard(), cols = "TRT"), "table_plan")
  # stat = : unnamed, unknown or repeated part names stop with the message; the three names pass
  full <- "table_plan(stat = ) names the parts of a statistic your frame calls something else"
  d <- .t4_ard()
  expect_error(table_plan(d, cols = "TRT", stat = c("x", "y")), full, fixed = TRUE)                  # unnamed
  expect_error(table_plan(d, cols = "TRT", stat = c(bogus = "x")), full, fixed = TRUE)               # unknown
  expect_error(table_plan(d, cols = "TRT", stat = c(value = "x", value = "y")), full, fixed = TRUE)  # repeated
  expect_error(table_plan(d, cols = "TRT", stat = c(value = "stat", bogus = "x")), full, fixed = TRUE)
  expect_s3_class(table_plan(d, cols = "TRT", stat = c(variable = "variable", name = "stat_name", value = "stat")),
                  "table_plan")
})

test_that("FT-PL5319 [FR-PL377]: {n} in a spanner takes the single value given; {n:sum} totals the columns the cell covers (Total left out); {n:<column>} names one column", {
  rec <- function(g1, l1, x) data.frame(
    group1 = g1, group1_level = l1, variable = "AGE", variable_level = NA_character_,
    context = "continuous", stat_name = "mean", stat_label = "mean", stat = x,
    stringsAsFactors = FALSE)
  na <- NA_character_
  d <- normalize_ard(rbind(rec("TRT", c("A", "B"), c(1, 2)), rec(na, na, 1.5)))
  p <- table_plan(d, cols = "TRT") |> plan_cells("{mean}", notes = FALSE) |> plan_total()
  expect_identical(names(.t4_tab(p)), c("label", "A", "B", "Total"))
  lab <- function(cell) vapply(cell, function(z) z$label, "")
  top <- function(spanner, values) {
    hdr <- rtf_col_header(list(col_cell(1L, "S"), col_cell(c(2L, 4L), spanner)),
                          c("", "{col}", "{col}", "{col}"))
    lab(plan_apply(p |> plan_col_header(values = values, hdr), "pages")[[1L]]$col_header[[1L]])
  }
  by_col <- c(A = 10, B = 12, Total = 22)
  # one value given: the spanner over three columns takes it
  expect_identical(top("N={n}", 99), c("S", "N=99"))
  # {n:sum} over A, B and Total: the Total column is left out, so 10 + 12 (not 44)
  expect_identical(top("ALL {n:sum}", by_col), c("S", "ALL 22"))
  # {n:sum} over the first two columns only: 10 + 12
  hdr2 <- rtf_col_header(list(col_cell(1L, "S"), col_cell(c(2L, 3L), "{n:sum}")), c("", "{col}", "{col}", "{col}"))
  expect_identical(lab(plan_apply(p |> plan_col_header(values = by_col, hdr2), "pages")[[1L]]$col_header[[1L]])[2L], "22")
  # {n:<column>}: one column's value, in any order
  expect_identical(top("B={n:B} A={n:A} T={n:Total}", by_col), c("S", "B=12 A=10 T=22"))
})

test_that("FT-PL5320 [FR-PL381]: a header sheet's cols (name, position, range, .values, KEY = value) and span (each, NA, a key) resolve on the page and are read back as cells", {
  p <- table_plan(.t4_ard2(), cols = c("TRT", "SEX"), rows = c(group = "variable")) |>
    plan_cells("{mean}", notes = FALSE)
  expect_identical(names(.t4_tab(p)), c("group", "label", "Placebo____F", "Placebo____M", "Drug____F", "Drug____M"))
  hd <- data.frame(
    line = c(1, 1, 1, 2, 2, 2, 3, 4),
    cols = c("group", "TRT = Placebo", "TRT = Drug", "2", "TRT = Placebo", "5:last", ".values", ".values"),
    text = c("G", "PBO", "DRG", "L", "x", "y", "{col}", "{col1}"),
    span = c(NA, NA, NA, NA, "each", NA, "each", "TRT"),
    align = c("center", "right", NA, NA, NA, NA, NA, NA),
    bold = c(TRUE, NA, NA, NA, NA, NA, NA, NA),
    border_bottom = c("single", NA, NA, NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE)
  q <- plan_col_header(p, hd)
  # plan_layers() reads the sheet back as cells
  cells <- plan_layers(q)$header$cells
  expect_identical(plan_layers(q)$header$source, "cells")
  expect_identical(names(cells), c("line", "cols", "span", "text", "align", "bold", "border_top", "border_bottom"))
  expect_identical(cells$cols, hd$cols)
  expect_identical(cells$text, hd$text)
  expect_identical(cells$align[1:2], c("center", "right"))
  expect_true(isTRUE(as.logical(cells$bold[1L])))
  expect_identical(cells$border_bottom[1L], "single")
  pg <- plan_apply(q, "pages")[[1L]]
  r1 <- pg$col_header[[1L]]
  r2 <- pg$col_header[[2L]]
  # row 1: a column name (group = column 1), then KEY = value cells over that value's columns
  expect_identical(vapply(r1, function(z) z$label, "")[1:3], c("G", "", "PBO"))
  expect_identical(vapply(r1, function(z) c(z$from, z$to), c(0L, 0L))[, c(1L, 3L, 4L)],
                   matrix(c(1L, 1L, 3L, 4L, 5L, 6L), nrow = 2L))
  expect_identical(r1[[1L]]$align, "center")
  expect_true(r1[[1L]]$bold)
  expect_identical(r1[[1L]]$border$bottom$style, "single")
  expect_identical(r1[[3L]]$align, "right")
  # row 2: a position (2), KEY = value with span "each" (a cell a column), a range 5:last (one cell), .values
  lab2 <- vapply(r2, function(z) z$label, "")
  expect_identical(lab2[1:2], c("", "L"))                         # column 1 not named: empty, column 2 = "L"
  expect_identical(lab2[3:4], c("x", "x"))                        # span "each": one cell a column of Placebo
  expect_identical(c(r2[[5L]]$from, r2[[5L]]$to), c(5L, 6L))      # "5:last", NA span: one cell over both
  expect_identical(r2[[5L]]$label, "y")
  # row 3: .values with span "each": a cell a value column, {col} the leaf
  expect_identical(unname(pg$col_header[[3L]]), c("", "", "F", "M", "F", "M"))
  # row 4: .values with span "TRT" (a key's name): a cell per value of that key
  r4 <- pg$col_header[[4L]]
  expect_identical(vapply(r4, function(z) z$label, "")[-1L], c("Placebo", "Drug"))
  expect_identical(vapply(r4, function(z) c(z$from, z$to), c(0L, 0L))[, -1L], matrix(c(3L, 4L, 5L, 6L), nrow = 2L))
})

test_that("FT-PL5321 [FR-PL385]: values = list(n = 'page', N = 'table') chooses the population on pages split by a group value; a named list makes each name a token", {
  skip_if_not_installed("cards")
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:20), stringsAsFactors = FALSE)
  lb <- expand.grid(USUBJID = adsl$USUBJID, PARAM = c("ALT", "HGB"), stringsAsFactors = FALSE)
  lb$BGRADE <- rep(c("G0", "G1"), length.out = nrow(lb))
  lb$WGRADE <- rep(c("G0", "G1", "G2"), length.out = nrow(lb))
  lb <- lb[!(lb$PARAM == "HGB" & seq_len(nrow(lb)) %% 4 == 0), ]
  expect_identical(as.integer(table(lb$PARAM)), c(20L, 15L))      # ALT 20 subjects, HGB 15 (every 4th row dropped)
  d <- normalize_ard(cards::bind_ard(
    cards::ard_tabulate(lb, by = c(PARAM, BGRADE), variables = WGRADE),
    cards::ard_tabulate(lb, by = PARAM, variables = BGRADE),
    cards::ard_total_n(adsl)), drop_contexts = "attributes")
  mk <- function(values) {
    table_plan(d, cols = "BGRADE", rows = c(PARAM = "PARAM"), label = c(label = ".label")) |>
      plan_cells("{n}", notes = FALSE) |> plan_paginate_group(keep = FALSE) |>
      plan_col_header(values = values, rtf_col_header(
        list(col_cell(1, ""), col_cell(c(2, 3), "Page {n} of {N}")), c("", "{col}")))
  }
  pg <- plan_apply(mk(list(n = "page", N = "table")), "pages")
  expect_identical(names(pg), c("ALT", "HGB"))
  spanner <- vapply(pg, function(x) x$col_header[[1L]][[2L]]$label, "")
  expect_identical(unname(spanner), c("Page 20 of 20", "Page 15 of 20"))   # n: the page's (20, 15), N: the table's 20
  # "table" for n as well: the analysis set's population on every page
  tb <- plan_apply(mk(list(n = "table", N = "table")), "pages")
  expect_identical(unname(vapply(tb, function(x) x$col_header[[1L]][[2L]]$label, "")),
                   c("Page 20 of 20", "Page 20 of 20"))
  # a named list: each name is a token ({total} prints 254 in the spanner over each column's {n})
  p <- table_plan(.t4_ard(), cols = "TRT") |> plan_cells("{mean}", notes = FALSE)
  hdr <- rtf_col_header(list(col_cell(1L, ""), col_cell(c(2L, 3L), "All {total}")),
                        c("", "{col} (N={n})", "{col} (N={n})"))
  h <- plan_apply(p |> plan_col_header(values = list(n = c(A = 10, B = 12), total = 254), hdr), "pages")[[1L]]$col_header
  expect_identical(h[[1L]][[2L]]$label, "All 254")
  expect_identical(unname(h[[2L]]), c("", "A (N=10)", "B (N=12)"))
})

test_that("FT-PL5322 [FR-PL387]: plan_col_header(header_sep = ) is as_rtftables(header_sep = ), splits a plain table's names into spanning rows and wins over plan_columns(sep = )", {
  w <- data.frame(label = c("a", "b"), `Placebo--F` = c("1", "2"), `Placebo--M` = c("3", "4"),
                  `Drug--F` = c("5", "6"), check.names = FALSE, stringsAsFactors = FALSE)
  p <- table_plan(w) |> plan_col_header(header_sep = "--")
  expect_identical(plan_apply(p, "args")$rtf$header_sep, "--")        # passed on as it is
  pg <- plan_apply(p, "pages")[[1L]]
  # two rows: the spanners (Placebo over columns 2-3, Drug over 4), then the leaves
  expect_length(pg$col_header, 2L)
  top <- pg$col_header[[1L]]
  expect_identical(vapply(top, function(z) z$label, ""), c("", "Placebo", "Drug"))
  expect_identical(vapply(top, function(z) c(z$from, z$to), c(0L, 0L)), matrix(c(1L, 1L, 2L, 3L, 4L, 4L), nrow = 2L))
  expect_identical(unname(pg$col_header[[2L]]), c("label", "F", "M", "F"))
  # a separator the names do not carry splits nothing
  expect_null(plan_apply(table_plan(w) |> plan_col_header(header_sep = "__"), "pages")[[1L]]$col_header)
  # it wins over plan_columns(sep = ): sep "__" says nothing here, header_sep "--" splits
  both <- table_plan(w) |> plan_columns(sep = "__") |> plan_col_header(header_sep = "--")
  expect_identical(plan_apply(both, "args")$rtf$header_sep, "--")
  expect_length(plan_apply(both, "pages")[[1L]]$col_header, 2L)
})

test_that("FT-PL5323 [FR-PL944]: plan_paginate_rows(max_rows = ) with the split left out pages with 'group_safe' (a listing leaves it to the listing); a split with no row budget stops", {
  d <- data.frame(grp = rep(c("A", "B", "C"), each = 2L), v = as.character(1:6), stringsAsFactors = FALSE)
  p <- table_plan(d)
  # the split left out: group_safe (whole groups to a page), the budget kept
  a <- plan_apply(plan_paginate_rows(p, max_rows = 4), "args")$rtf
  expect_identical(a$split, "group_safe")
  expect_identical(a$max_rows, 4)
  # 3 groups of 2 rows, 4 rows to a page: A+B (4 rows), C (2 rows) = 2 pages
  pg <- suppressMessages(plan_apply(plan_paginate_rows(p, max_rows = 4), "pages"))
  expect_identical(vapply(pg, function(x) nrow(x$data), 1L), c(4L, 2L), ignore_attr = TRUE)
  # a split given stays as it is
  expect_identical(plan_apply(plan_paginate_rows(p, max_rows = 4, split = "group_force"), "args")$rtf$split,
                   "group_force")
  # a listing plan leaves the split to as_rtftables(listing = )
  lp <- table_plan(d) |> plan_listing(listing_col("grp", width = 10), listing_col("v", width = 10)) |>
    plan_paginate_rows(max_rows = 4)
  expect_null(plan_apply(lp, "args")$rtf$split)
  # a split without a row budget, given one: none, rows, a page per value stop (FR-PL446)
  for (sp in c("none", "rows")) {
    expect_error(plan_apply(plan_paginate_rows(p, max_rows = 4, split = sp), "args"),
                 sprintf("plan_paginate_rows(max_rows = 4) has no effect with split = '%s'.", sp), fixed = TRUE)
  }
  expect_error(plan_apply(plan_paginate_rows(plan_paginate_group(p, col = "grp"), max_rows = 4), "args"),
               "plan_paginate_rows(max_rows = 4) has no effect with split = 'by_value'.", fixed = TRUE)
})

test_that("FT-PL5324 [FR-PL906]: a part carrying a whole-table setting, another column key or a non-plan stops; a part may carry its own row verbs and runs on its own", {
  rec <- data.frame(ID = rep(c("A", "B"), each = 3), T = rep(c("t1", "t2", "t3"), 2),
                    V = c(1.234, 2.5, NA, 3, 4.25, 5), stringsAsFactors = FALSE)
  long <- data.frame(T = c("t1", "t2", "t3"), variable = "V", stat_name = "mean", stat = c(1.5, 2.5, 3.5),
                     .label = NA_character_, stringsAsFactors = FALSE)
  parent <- table_plan(rec, cols = "T", rows = "ID", value = "V") |> plan_cells("{value:.1f}", na = "BLQ")
  child <- table_plan(long, cols = "T", rows = c(block = ~ "Summary")) |> plan_cells(V = c(Mean = "{mean:.1f}"))
  intro <- "plan_append(\"part2\"): a part says how its own rows are made, and the whole table's\n  settings are the first plan's, written before or after plan_append().  Move to the first plan:\n    "
  bad <- function(c2, verb) {
    e <- tryCatch(plan_append(parent, c2), error = function(e) conditionMessage(e))
    expect_true(startsWith(e, intro), info = verb)                 # names the part and says where it goes
    expect_match(e, verb, fixed = TRUE)                            # names the verb
  }
  bad(plan_col_header(child, rtf_col_header(c("a", "b", "c", "d"))), "plan_col_header()")
  bad(plan_columns(child, widths = c(1, 1, 1, 1)), "plan_columns()")
  bad(plan_paginate_rows(child, max_rows = 5), "plan_paginate_rows() / plan_paginate_group()")
  bad(plan_paginate_group(child, col = "T"), "plan_paginate_group()")
  bad(plan_paginate_cols(child, at = "t2"), "plan_paginate_cols()")
  bad(plan_blanks(child, where = "between_groups"), "plan_blanks(where = )")
  bad(plan_style(child, border = "tfl"), "plan_style()")
  bad(plan_titles(child, "T"), "plan_titles()")
  bad(plan_footnotes(child, "F"), "plan_footnotes()")
  bad(plan_after(child, function(x) x), "plan_after()")
  bad(plan_levels(child, T = c("t3", "t2", "t1")), "plan_levels(T = )")
  bad(plan_append(child, child), "plan_append()")
  # a part with a different column key stops
  other <- table_plan(data.frame(GRP = "x", variable = "V", stat_name = "mean", stat = 1, .label = NA_character_,
                                 stringsAsFactors = FALSE), cols = "GRP", rows = c(block = ~ "S")) |>
    plan_cells(V = c(Mean = "{mean:.1f}"))
  expect_error(plan_append(parent, other), "the part's column key is 'GRP' and the table's is 'T'", fixed = TRUE)
  # something that is not a table_plan stops
  expect_error(plan_append(parent, long), "is a table_plan")
  # a part may carry its own row verbs (cells, digits, labels, stub, a cell style with a condition, a first blank row)
  ok <- child |> plan_digits(2) |> plan_labels(V = "Value") |> plan_stub(indent = 2L) |>
    plan_cell_style(where = ~ label == "Summary", bold = TRUE) |> plan_blanks(first = TRUE, last = TRUE)
  expect_s3_class(plan_append(parent, ok), "table_plan")
  # ... and runs on its own, with only what it says
  expect_identical(as.data.frame(plan_apply(child, "table"))$block[1L], "Summary")
})
