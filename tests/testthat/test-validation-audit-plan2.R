# Help-to-FR audit, batch 5: the plan verbs (part 2), plan_append(),
# plan_listing() and the verbs batch 4 (test-validation-audit-plan.R) left.
#
# The help is the functional requirement: every statement in a function's Rd
# is an active FR in inst/validation/requirements.csv, and every FR has a
# test.  For each function below, one row per claim of its roxygen: the
# claim (short), the FR that states it, and the test that checks it.  The
# tests below this table close the gaps the audit found.
#
# plan_append()
#   claim                                                   | FR          | test
#   the child's rows under the parent's, on the same columns| FR-PL900/903| FT-PL905
#   several calls stack in the order written; .part column  | FR-PL903    | FT-PL905
#   name: part2, part3 ...; one non-empty string, not taken;| FR-PL903/945| FT-PL905, FT-PL954
#     the part's entry in plan_layers()                     |             |
#   absent = "error" / "skip"                               | FR-PL904    | FT-PL906
#   a part carries only the row verbs; a whole-table one    | FR-PL906/949| FT-PL908, FT-PL958
#     stops (cols = without a condition included)           |             |
#   a part's row verbs (filter, sort, levels of a row key)  | FR-PL949    | FT-PL958
#   its cols key is the parent's, or left out               | FR-PL906/949| FT-PL908, FT-PL958
#   the whole table's settings are the parent's, before or  | FR-PL907/948| FT-PL909, FT-PL957
#     after plan_append(), the later winning                |             |
#   blank rows between parts; a part's first / last         | FR-PL905    | FT-PL907
#   the parent's first / last: the table's top and bottom   | FR-PL950    | FT-PL959
#   inherited key by key: digits, na                        | FR-PL903    | FT-PL905
#   inherited key by key: cells, labels, levels of row keys | FR-PL946    | FT-PL955
#   folded into the parent's plan_stub(name =, indent =)    | FR-PL947    | FT-PL956
#   a constant row key is the heading row; a column a part  | FR-PL903    | FT-PL905
#     has not prints its empty-cell text                    |             |
#   page groups matched by value; the page key in every part| FR-PL904    | FT-PL906
#   a part run on its own: its own rows, only what it says  | FR-PL906    | FT-PL908
#
# plan_listing()
#   a listing of records through listing_spec()             | FR-PL027/480| FT-PL265, FT-PL266
#   listing_col()s are the printed columns, in order        | FR-PL481    | FT-PL266
#   ... bare column names are listing_col(<name>)           | FR-PL951    | FT-PL960
#   a wrapped record is not cut by a page break, unless it  | FR-PL952    | FT-PL961
#     alone is longer than a page (help fixed)              |             |
#   type / sep / align / layout / spacer / wrap: listing_   | FR-PL482/483| FT-PL267, FT-PL291
#     spec()'s own; record TRUE / FALSE / a name            |             |
#   the blank rows are plan_blanks()'s                      | FR-PL935, FR-PL193 | FT-PL944
#
# plan_style()
#   the whole table's look, by rtftable()'s names: border,  | FR-PL021/422-428 | FT-PL032, FT-PL208, FT-PL233, FT-PL247, FT-PL248, FT-PL283
#     zones, look, widths, font, heights, padding, markup   |             |
#   border or border_* zones, not both                      | FR-PL429    | FT-PL247
#   per-column settings are other verbs'                    | FR-PL430    | FT-PL247
#
# plan_cell_style()
#   header / cols / where; value, formula, list of formulas | FR-PL404-417| FT-PL032, FT-PL236-FT-PL243, FT-PL282
#   columns by name: a reordered table keeps its styles     | FR-PL954    | FT-PL963
#   value styles in the order written; formulas last-wins   | FR-PL407    | FT-PL240
#   a formula sees the printed rows with before = TRUE only | FR-PL366/371| FT-PL211
#
# plan_hide()
#   as_rtftables(drop_cols = ); several names; calls add up | FR-PL399/401| FT-PL037, FT-PL234, FT-PL280
#   the column still does its work: a page key, positions   | FR-PL400, FR-IX255 | FT-PL037, FT-PL235
#   ... a sort carrier, a style condition's identifier      | FR-PL953    | FT-PL962
#
# plan_nest()
#   rows under one level; heading dropped; levels / labels; | FR-PL318-325| FT-PL127, FT-PL128, FT-PL168, FT-PL169
#     the rows key read; the stub's indent                  |             |
#
# plan_compare()
#   templates, guards, a map by context, cell_rows(); by;   | FR-PL289-302| FT-PL118-FT-PL122, FT-PL161-FT-PL163
#     position; no population; errors                       |             |
#   a record whose variable has no block has no row         | FR-PL956    | FT-PL965
#   a record with no variable at all stops                  | FR-AR211    | FT-AR071
#
# plan_stat_groups()
#   groups, .stat_group across or down; levels; .variables, | FR-PL278-287| FT-PL115-FT-PL117, FT-PL158-FT-PL160
#     .others; a group with fewer rows leaves the rest empty|             |
#
# plan_row_group()
#   mode / collapse / group_col; the outermost row key      | FR-PL473-478| FT-PL032, FT-PL037-FT-PL039, FT-PL208, FT-PL290
#
# plan_empty()
#   text / when / position / footnote; label column or first| FR-PL327-333| FT-PL040, FT-PL170-FT-PL172
#
# plan_after()
#   functions of the pages, in order, after the header and  | FR-PL493-495| FT-PL233, FT-PL268, FT-PL269, FT-PL293
#     styles; anything else stops                           |             |
#
# plan_layers()
#   data, roles, label, group_col, declared, layers, cells, | FR-PL210-214| FT-PL126, FT-PL136-FT-PL138
#     compare, columns, pages; header "resolved"            |             |
#   header source "cells" for a header given as cell rows   | FR-PL955    | FT-PL964
#
# plan_template()
#   both halves; cols / hierarchy / form / file / pipe      | FR-PL350-359| FT-PL200-FT-PL207, FT-PL270-FT-PL272
#
# plan_header_tokens()
#   {col} / {colN} / {n} / {n:sum} / {n:<column>}; reasons  | FR-PL216-221| FT-PL139-FT-PL142
#
# plan_n_candidates()
#   scope all / page / table; attributes differ, page_col   | FR-PL222-225| FT-PL103, FT-PL104, FT-PL141, FT-PL142
#   left unmade with two populations: the pages', a warning | FR-PL957    | FT-PL966
#
# plan_apply()
#   stage auto / input / args / table / pages; errors name  | FR-PL200-209| FT-PL100-FT-PL102, FT-PL131-FT-PL135
#     the verbs; rtf_tables(doc, plan)                      |             |

# ---- plan_append() -------------------------------------------------------------

# Invented records of two subjects at three time points, and the means
.a2_rec <- function() data.frame(
  ID = rep(c("A", "B"), each = 3), T = rep(c("t1", "t2", "t3"), 2),
  V = c(1.234, 2.5, NA, 3, 4.25, 5), stringsAsFactors = FALSE)
.a2_mean <- function() data.frame(
  T = c("t1", "t2", "t3"), variable = "V", stat_name = "mean",
  stat = c(2.1, 3.4, 5), .label = NA_character_, stringsAsFactors = FALSE)
.a2_parent <- function() {
  table_plan(.a2_rec(), cols = "T", rows = "ID", value = "V") |>
    plan_cells("{value:.1f}", na = "BLQ")
}
.a2_child <- function() {
  table_plan(.a2_mean(), cols = "T", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean:.2f}"))
}
# two analyses, two time points: a summary parent and a summary part
.a2_two <- function(v) data.frame(
  T = rep(c("t1", "t2"), 2), variable = rep(c("V", "W"), each = 2),
  stat_name = "mean", stat = v, .label = NA_character_, stringsAsFactors = FALSE)
.a2_tab <- function(p) as.data.frame(plan_apply(p, "table"))
.nb <- function(k) strrep(" ", k)

test_that("FT-PL954 [FR-PL945]: plan_append(name = ) is one non-empty string, the part's entry in plan_layers()", {
  parent <- .a2_parent()
  for (bad in list("", NA_character_, c("a", "b"), 1)) {
    expect_error(parent |> plan_append(.a2_child(), name = bad),
                 "plan_append(name = ) is one non-empty string.", fixed = TRUE)
  }
  p <- parent |> plan_append(.a2_child(), name = "stats")
  parts <- plan_layers(p)$layers$part
  expect_identical(vapply(parts, `[[`, "", "name"), "stats")
  expect_identical(unique(.a2_tab(p)$.part), c("part1", "stats"))
})

test_that("FT-PL955 [FR-PL946]: a part takes the parent's cells, labels and row-key levels it does not say, key by key", {
  parent <- table_plan(.a2_two(c(1.25, 2.5, 3.75, 4)), cols = "T", rows = c(group = "variable")) |>
    plan_cells(c(Mean = "{mean:.1f}")) |>
    plan_labels(variable = c(V = "Vee", W = "Dub")) |>
    plan_levels(variable = c("W", "V"))
  child <- table_plan(.a2_two(c(10.25, 20.5, 30.75, 40)), cols = "T", rows = c(group = "variable"))
  t <- .a2_tab(parent |> plan_append(child))
  expect_identical(t$label[t$.part == "part2"],
                   c("Dub", paste0(.nb(4), "Mean"), "Vee", paste0(.nb(4), "Mean")))
  expect_identical(t$t1[t$.part == "part2"], c("", "30.8", "", "10.2"))
  # what the part says wins: its own template, its own map for `variable`
  # (which replaces the parent's for that key whole, as plan_labels() does)
  own <- child |> plan_cells(c(Avg = "{mean:.3f}")) |> plan_labels(variable = c(V = "Own V"))
  t <- .a2_tab(parent |> plan_append(own))
  expect_identical(t$label[t$.part == "part2"],
                   c("W", paste0(.nb(4), "Avg"), "Own V", paste0(.nb(4), "Avg")))
  expect_identical(t$t1[t$.part == "part2"], c("", "30.750", "", "10.250"))
  # the parent's own rows are as they were
  expect_identical(t$label[t$.part == "part1"][c(1L, 3L)], c("Dub", "Vee"))
})

test_that("FT-PL956 [FR-PL947]: every part is folded into the parent's plan_stub(name = , indent = )", {
  parent <- .a2_parent() |> plan_stub(name = "Subject", indent = 2L)
  t <- .a2_tab(parent |> plan_append(.a2_child()))
  expect_identical(names(t), c("Subject", "t1", "t2", "t3", ".part"))
  expect_identical(t$Subject, c("A", "B", "Summary", paste0(.nb(2), "Mean")))
})

test_that("FT-PL957 [FR-PL948]: the whole table's settings may come before plan_append(); the later of two wins", {
  hdr <- function(p) plan_apply(p, "pages")[[1L]]$col_header[[1L]]
  before <- .a2_parent() |> plan_col_header(rtf_col_header(c("Subject", "{col}"))) |>
    plan_append(.a2_child())
  expect_identical(hdr(before), c("Subject", "t1", "t2", "t3"))
  both <- before |> plan_col_header(rtf_col_header(c("Who", "{col} h")))
  expect_identical(hdr(both), c("Who", "t1 h", "t2 h", "t3 h"))
  pg <- plan_apply(.a2_parent() |> plan_columns(widths = c(label = 3, .values = 1)) |>
                     plan_append(.a2_child()), "pages")[[1L]]
  expect_identical(pg$col_rel_width, c(3, 1, 1, 1))
})

test_that("FT-PL958 [FR-PL949]: a part may filter, sort and order its own rows and leave cols out; a column style stops", {
  m <- .a2_mean()
  two <- rbind(m, transform(m, variable = "Z", stat = c(9, 8, 7)))
  child <- table_plan(two, cols = "T", rows = c(group = "variable")) |>
    plan_cells(c(Mean = "{mean:.1f}"), notes = FALSE)
  part_labels <- function(ch) {
    t <- .a2_tab(.a2_parent() |> plan_append(ch))
    t$label[t$.part == "part2"]
  }
  mean_row <- paste0(.nb(4), "Mean")
  expect_identical(part_labels(child), c("V", mean_row, "Z", mean_row))
  expect_identical(suppressMessages(part_labels(child |> plan_filter(mean > 5))), c("Z", mean_row))
  expect_identical(part_labels(child |> plan_sort("-group")), c("Z", mean_row, "V", mean_row))
  expect_identical(part_labels(child |> plan_levels(variable = c("Z", "V"))),
                   c("Z", mean_row, "V", mean_row))
  # cols left out: the parent's
  nocols <- table_plan(two, rows = c(group = "variable")) |>
    plan_cells(c(Mean = "{mean:.1f}"), notes = FALSE)
  t <- .a2_tab(.a2_parent() |> plan_append(nocols))
  expect_identical(names(t), c("label", "t1", "t2", "t3", ".part"))
  expect_identical(t$t3[t$.part == "part2"], c("", "5.0", "", "7.0"))
  # a style on whole columns is the whole table's
  expect_error(.a2_parent() |> plan_append(child |> plan_cell_style(cols = "t1", bold = TRUE)),
               "plan_cell_style(cols = ) without a condition", fixed = TRUE)
})

test_that("FT-PL959 [FR-PL950]: the parent's plan_blanks(first = , last = ) are the table's top and bottom", {
  pg <- plan_apply(.a2_parent() |> plan_append(.a2_child()) |>
                     plan_blanks(first = TRUE, last = TRUE), "pages")[[1L]]
  expect_identical(nrow(pg$data), 4L)                 # no blank row between the parts
  expect_identical(pg$blank_rows, c(0L, 4L))
})

# ---- plan_listing() ------------------------------------------------------------

# Invented adverse-event records; the second one wraps over six lines
.a2_ae <- function() data.frame(
  USUBJID = c("01-001", "01-001", "01-002", "01-003"),
  AETERM = c("Headache", "Nausea and vomiting after the second dose", "Fatigue", "Rash"),
  stringsAsFactors = FALSE)

test_that("FT-PL960 [FR-PL951]: plan_listing() takes bare column names as listing_col(<name>)", {
  bare <- plan_apply(table_plan(.a2_ae()) |> plan_listing("USUBJID", "AETERM"))
  cols <- plan_apply(table_plan(.a2_ae()) |>
                       plan_listing(listing_col("USUBJID"), listing_col("AETERM")))
  expect_identical(bare, cols)
  expect_identical(names(bare[[1L]]$data), c("USUBJID", ".sp1", "AETERM"))
})

test_that("FT-PL961 [FR-PL952]: a wrapped record moves whole to the next page; one longer than a page is cut", {
  base <- table_plan(.a2_ae()) |>
    plan_listing(listing_col("USUBJID", width = 8), listing_col("AETERM", width = 10))
  terms <- function(mr) {
    pg <- plan_apply(base |> plan_paginate_rows(max_rows = mr))
    vapply(pg, function(p) paste(trimws(p$data$AETERM), collapse = "|"), "")
  }
  # the record of seven rows (six lines, a blank) does not fit under the first
  expect_identical(terms(8), c("Headache|", "Nausea|and|vomiting|after the|second|dose|",
                               "Fatigue||Rash|"))
  # longer than a page, it is cut where the page is full and goes on at the
  # next page's top, with no continuation row (#881), the row it
  # saved going to the next record
  expect_identical(terms(6)[2:3], c("Nausea|and|vomiting|after the|second", "dose||Fatigue||Rash|"))
})

# ---- plan_hide(), plan_cell_style() ----------------------------------------------

test_that("FT-PL962 [FR-PL953]: a hidden column still carries a sort and a style condition", {
  tbl <- data.frame(ID = c("b", "a", "c"), K = c(2, 1, 3),
                    Placebo = c("1", "2", "3"), Active = c("4", "5", "6"))
  pg <- plan_apply(table_plan(tbl) |> plan_hide("K") |> plan_sort("K") |>
                     plan_cell_style(where = ~ K == 3, bold = TRUE))[[1L]]
  expect_identical(names(pg$data), c("ID", "Placebo", "Active"))
  expect_identical(pg$data$ID, c("a", "b", "c"))
  expect_null(pg$cell_styles[[1L]])
  expect_identical(pg$cell_styles[[3L]]$bold, rep(TRUE, 3L))
})

test_that("FT-PL963 [FR-PL954]: plan_cell_style(cols = ) follows its column when plan_levels() reorders the columns", {
  st <- data.frame(ARM = rep(c("Drug A", "Placebo"), each = 2), variable = "AGE",
                   .label = NA, .kind = "continuous", stat_name = rep(c("mean", "sd"), 2),
                   stat = c(50, 10, 52, 9))
  p <- table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(M = "{mean:.1f}"), notes = FALSE)
  bold_cols <- function(p) {
    pg <- plan_apply(p |> plan_cell_style(cols = "Placebo", bold = TRUE))[[1L]]
    names(pg$data)[vapply(pg$col_spec, function(s) isTRUE(s$bold), NA)]
  }
  expect_identical(bold_cols(p), "Placebo")
  expect_identical(bold_cols(p |> plan_levels(ARM = c("Placebo", "Drug A"))), "Placebo")
})

# ---- plan_layers(), plan_compare(), plan_n_candidates() ------------------------------

test_that("FT-PL964 [FR-PL955]: plan_layers()$header$source is \"cells\" for a header given as cell rows", {
  st <- data.frame(ARM = c("A", "B"), variable = "AGE", .label = NA, .kind = "continuous",
                   stat_name = "mean", stat = c(50, 52))
  p <- table_plan(st, cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(M = "{mean:.1f}"), notes = FALSE)
  cells <- data.frame(line = 1, cols = c("1", ".values"), text = c("", "{col}"))
  h <- plan_layers(p |> plan_col_header(header = cells))$header
  expect_identical(h$source, "cells")
  expect_identical(h$cells$cols, c("1", ".values"))
  expect_identical(h$cells$text, c("", "{col}"))
  expect_identical(plan_layers(p |> plan_col_header(rtf_col_header(c("", "{col}"))))$header$source,
                   "resolved")
})

test_that("FT-PL965 [FR-PL956]: a comparison record whose variable has no block in the body has no row", {
  st <- data.frame(ARM = rep(c("A", "B"), each = 2), variable = "AGE", .label = NA,
                   .kind = "continuous", stat_name = rep(c("mean", "sd"), 2),
                   stat = c(50, 10, 52, 9), context = "summary")
  # an analysis of variance's record names its term, ARM, not AGE
  aov <- data.frame(ARM = NA, variable = "ARM", .label = NA, .kind = NA,
                    stat_name = "p.value", stat = 0.03, context = "stats_aov")
  p <- table_plan(rbind(st, aov), cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(M = "{mean:.1f}"), notes = FALSE) |>
    plan_compare(p = "{p.value:.3f}")
  t <- as.data.frame(plan_apply(p, "table"))
  expect_identical(names(t), c("group", "label", "A", "B", "p"))
  expect_identical(t$group, "AGE")
  expect_true(is.na(t$p))
})

test_that("FT-PL966 [FR-PL957]: {n} left unmade with two populations prints each page's, with a warning", {
  skip_if_not_installed("cards")
  lb <- data.frame(USUBJID = rep(sprintf("S%02d", 1:10), 2),
                   PARAM = rep(c("ALT", "HGB"), each = 10),
                   BGRADE = rep(c("G0", "G1"), 10), WGRADE = rep(c("G0", "G1", "G1", "G2"), 5),
                   stringsAsFactors = FALSE)
  lb <- lb[-(19:20), ]                                  # two subjects untested for HGB
  adsl <- data.frame(USUBJID = sprintf("S%02d", 1:12))  # the analysis set: 12
  d <- normalize_ard(cards::bind_ard(
    cards::ard_tabulate(lb, by = c(PARAM, BGRADE), variables = WGRADE),
    cards::ard_tabulate(lb, by = PARAM, variables = BGRADE),
    cards::ard_total_n(adsl)), drop_contexts = "attributes")
  plan <- function(n) {
    table_plan(d, cols = "BGRADE", rows = c(PARAM = "PARAM"), label = c(label = ".label")) |>
      plan_cells("{n}", notes = FALSE) |>
      plan_paginate_group(keep = FALSE) |>
      plan_col_header(values = list(n = n),
                      rtf_col_header(list(col_cell(1, ""), col_cell(c(2, 3), "T (N={n})")),
                                     c("", "{col}")))
  }
  cand <- plan_n_candidates(plan(TRUE))
  expect_true(attr(cand, "differ"))
  tot <- cand[is.na(cand$column) & cand$scope == "page", ]
  expect_identical(tot$value[match(c("ALT", "HGB"), tot$page)], c(10, 8))
  expect_warning(pg <- suppressMessages(plan_apply(plan(TRUE), "pages")), "two populations")
  span <- vapply(pg, function(p) p$col_header[[1L]][[2L]]$label, "")
  expect_identical(unname(span), c("T (N=10)", "T (N=8)"))
  expect_identical(unname(vapply(suppressMessages(plan_apply(plan("table"), "pages")),
                                 function(p) p$col_header[[1L]][[2L]]$label, "")),
                   c("T (N=12)", "T (N=12)"))
})
