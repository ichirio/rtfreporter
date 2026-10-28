# Validation tests: table_plan(), plan_filter(), plan_columns(), plan_blanks().

# A long summary built by hand: one statistic a row, the label its name.
val_long <- function() {
  data.frame(
    TRT       = rep(c("A", "B"), each = 6),
    PARAM     = rep(rep(c("ALT", "AST"), each = 3), 2),
    stat_name = rep(c("n", "mean", "sd"), 4),
    stat      = c(20, 31.245, 4.1, 20, 28.7, 3.92,
                  18, 33.108, 5.3, 18, 30.2, 4.44),
    stringsAsFactors = FALSE
  )
}

val_plan <- function(d = val_long()) {
  table_plan(d, cols = "TRT", rows = c(param = "PARAM"),
             label = c(row = "stat_name")) |>
    plan_cells(notes = FALSE) |>
    plan_cells(c("n" = "{n:.0f}", "Mean (SD)" = "{mean} ({sd})")) |>
    plan_digits(2)
}

# One count a row: PARAM is the row key, VISIT the row label.
val_counts <- function() {
  data.frame(
    TRT       = rep(c("A", "B"), each = 4),
    PARAM     = rep(c("ALT", "ALT", "AST", "AST"), 2),
    VISIT     = rep(c("W1", "W2"), 4),
    stat_name = "n",
    stat      = c(5, 12, 3, 4, 6, 2, 8, 1),
    stringsAsFactors = FALSE
  )
}

val_count_plan <- function(d = val_counts(), notes = FALSE) {
  table_plan(d, cols = "TRT", rows = c(param = "PARAM"),
             label = c(visit = "VISIT")) |>
    plan_cells("{n:.0f}", notes = notes)
}

val_table <- function(p) as.data.frame(suppressMessages(plan_apply(p)))

val_rows <- function(t) paste(t$param, t$visit)

# A finished table with three groups (a heading row and two indented rows).
val_wide <- function() {
  data.frame(
    label = c("Group A", "  a1", "  a2", "Group B", "  b1", "  b2",
              "Group C", "  c1", "  c2"),
    v = as.character(1:9), stringsAsFactors = FALSE
  )
}

val_page_shape <- function(pages) {
  lapply(pages, function(p) list(rows = nrow(p$data), blanks = p$blank_rows))
}


# ---- the plan itself -------------------------------------------------------

test_that("FT-PL049 [FR-PL122]: a factor in table_plan(cols = ) orders the columns by its levels", {
  d <- val_long()
  d$TRT <- factor(d$TRT, levels = c("B", "A"))
  expect_identical(names(val_table(val_plan(d))), c("param", "row", "B", "A"))
  d$TRT <- factor(d$TRT, levels = c("A", "B"))
  expect_identical(names(val_table(val_plan(d))), c("param", "row", "A", "B"))
})

test_that("FT-PL050 [FR-PL123]: table_plan() keeps Japanese key values as names and labels", {
  active <- "実薬"
  placebo <- "プラセボ"
  liver <- "肝機能"
  d <- val_long()
  d$TRT <- ifelse(d$TRT == "A", active, placebo)
  d$PARAM[d$PARAM == "ALT"] <- liver
  out <- val_table(val_plan(d))
  expect_identical(names(out), c("param", "row", active, placebo))
  expect_true(liver %in% out$param)
  expect_identical(out[[active]][out$param == liver & out$row == "Mean (SD)"],
                   "31.25 (4.10)")
})

test_that("FT-PL051 [FR-PL124]: table_plan() over a frame of zero rows stops: nothing to widen", {
  skip_if_not_installed("cards")
  ard <- normalize_ard(cards::ard_tabulate(cards::ADSL, by = ARM, variables = SEX))
  p <- table_plan(ard[0, ], cols = "ARM", rows = c(group = "variable")) |>
    plan_cells("{n}")
  expect_error(plan_apply(p), "No cell was produced, so there is nothing to widen.",
               fixed = TRUE)
})

test_that("FT-PL052 [FR-PL129]: table_plan(stat = c(value = NA)) stops: each part is one column", {
  expect_error(table_plan(val_long(), stat = c(value = NA)),
               "table_plan(stat = ): each part is one column name.", fixed = TRUE)
})

test_that("FT-PL053 [FR-PL132]: table_plan(show_keys = <a column key>) stops at plan_apply()", {
  p <- table_plan(val_long(), cols = "TRT", rows = c(param = "PARAM"),
                  label = c(row = "stat_name"), show_keys = "TRT") |>
    plan_cells("{stat}", notes = FALSE)
  expect_error(plan_apply(p),
               "`show_keys`: 'TRT' is a column key; its own counts are the columns' populations",
               fixed = TRUE)
})


# ---- filtering rows --------------------------------------------------------

test_that("FT-PL054 [FR-PL136]: plan_filter() keeps a value column whose rows are all dropped", {
  d <- rbind(val_counts(),
             data.frame(TRT = "C", PARAM = "XYZ", VISIT = "W1", stat_name = "n",
                        stat = 1))
  out <- val_table(val_count_plan(d) |> plan_filter(PARAM != "XYZ"))
  expect_identical(names(out), c("param", "visit", "A", "B", "C"))
  expect_false("XYZ" %in% out$param)
  expect_true(all(is.na(out$C)))
})

test_that("FT-PL055 [FR-PL138]: plan_filter() keeps a row no statistic answers, and notes it", {
  msgs <- character()
  out <- withCallingHandlers(
    as.data.frame(plan_apply(val_count_plan(notes = TRUE) |> plan_filter(zzz > 1))),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(nrow(out), 4L)
  expect_match(paste(msgs, collapse = "\n"),
               "kept by plan_filter(): no statistic answered its condition", fixed = TRUE)
  # a condition that is not one TRUE / FALSE is unknown too
  expect_identical(nrow(val_table(val_count_plan() |> plan_filter(n + 1))), 4L)
})

test_that("FT-PL056 [FR-PL139]: plan_filter() twice uses only the last call's conditions", {
  out <- val_table(val_count_plan() |> plan_filter(n >= 10) |> plan_filter(n <= 2))
  expect_identical(val_rows(out), c("ALT W2", "AST W2"))
  expect_identical(val_table(val_count_plan() |> plan_filter(n <= 2)), out)
})

test_that("FT-PL057 [FR-PL140]: plan_filter(cond1, cond2) keeps a record if every one holds", {
  out <- val_table(val_count_plan() |> plan_filter(n >= 4, n <= 6))
  # ALT W1: 5 and 6; AST W2: A's 4.  ALT W2 (12, 2) and AST W1 (3, 8) fail one
  expect_identical(val_rows(out), c("ALT W1", "AST W2"))
})

test_that("FT-PL058 [FR-PL141]: plan_filter() conditions read the record's keys and statistics", {
  out <- val_table(val_count_plan() |> plan_filter(n >= 4, TRT != "A"))
  # only B's records can pass: ALT W1 (6) and AST W1 (8)
  expect_identical(val_rows(out), c("ALT W1", "AST W1"))
})

test_that("FT-PL059 [FR-PL146]: plan_filter(by = KEY) judges the rows sharing KEY together", {
  # one statistic a row: alone, the `sd` and `n` rows cannot answer `mean`
  alone <- val_table(val_plan() |> plan_filter(mean > 32))
  expect_true("AST" %in% alone$param)
  # judged by PARAM, AST (mean 28.7 and 30.2) goes whole, ALT (B: 33.1) stays
  together <- val_table(val_plan() |> plan_filter(mean > 32, by = "PARAM"))
  expect_identical(together$param, c("ALT", "ALT"))
  expect_identical(together$B[together$row == "Mean (SD)"], "33.11 (5.30)")
})

test_that("FT-PL060 [FR-PL151]: plan_filter(parents = \"x\") stops", {
  expect_error(plan_filter(val_count_plan(), n > 1, parents = "x"),
               "'arg' should be one of \"keep\", \"drop\"", fixed = TRUE)
})

test_that("FT-PL061 [FR-PL152]: plan_filter(by = <not a row key>) stops at plan_apply()", {
  p <- val_count_plan() |> plan_filter(n > 1, by = "VISIT")
  expect_error(plan_apply(p), "plan_filter(by = \"VISIT\"): not a row key.  Row keys: ",
               fixed = TRUE)
})

test_that("FT-PL062 [FR-PL153]: plan_filter(<not a table_plan>) stops", {
  expect_error(plan_filter(val_counts(), n > 1),
               "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})

test_that("FT-PL063 [FR-PL154]: plan_filter() on a finished table stops at plan_apply()", {
  p <- table_plan(data.frame(group = "ALT", A = "31.2")) |> plan_filter(A > 1)
  expect_error(plan_apply(p), "This source has no statistics to read -- ", fixed = TRUE)
  expect_error(plan_apply(p), "but table_plan(cols = ) / plan_cells() need them.",
               fixed = TRUE)
})


# ---- the columns -----------------------------------------------------------

test_that("FT-PL064 [FR-PL155]: plan_columns(), even empty, makes plan_apply() give RTF pages", {
  expect_s3_class(suppressMessages(plan_apply(val_plan())), "data.frame")
  pages <- suppressMessages(plan_apply(val_plan() |> plan_columns()))
  expect_type(pages, "list")
  expect_s3_class(pages[[1L]], "rtftable")
})

test_that("FT-PL065 [FR-PL156]: plan_columns() twice merges field by field, the later winning", {
  page <- plan_apply(table_plan(val_wide()) |>
                       plan_columns(widths = c(label = 3, v = 1), row_title = "label") |>
                       plan_columns(widths = c(label = 5, v = 1)))[[1L]]
  expect_equal(page$col_rel_width, c(5, 1))
  expect_equal(page$row_title, 1L)
})

test_that("FT-PL066 [FR-PL163]: plan_columns(decimal = <name not on the page>) ignores it", {
  p <- table_plan(val_wide())
  expect_silent(pages <- plan_apply(p |> plan_columns(decimal = "nope")))
  expect_identical(pages, plan_apply(p |> plan_columns()))
  expect_null(pages[[1L]]$decimal_split)
})

test_that("FT-PL067 [FR-PL175]: plan_columns(widths, decimal, row_title) match Japanese names", {
  item <- "項目"
  active <- "実薬"
  d <- data.frame(a = c("x", "y"), b = c("1.5", "10.25"))
  names(d) <- c(item, active)
  w <- stats::setNames(c(3, 1), c(item, active))
  page <- plan_apply(table_plan(d) |>
                       plan_columns(widths = w, decimal = active, row_title = item))[[1L]]
  expect_equal(page$col_rel_width, c(3, 1))
  expect_equal(page$row_title, 1L)
  expect_identical(page$decimal_split$cols, 2L)
  rev_page <- plan_apply(table_plan(d) |> plan_columns(widths = rev(w)))[[1L]]
  expect_equal(rev_page$col_rel_width, c(3, 1))
})

test_that("FT-PL068 [FR-PL180]: a column-key value containing the separator stops", {
  d <- val_long()
  d$TRT[d$TRT == "A"] <- "A____x"
  expect_error(plan_apply(val_plan(d)),
               "'TRT' has a value containing the key separator \"____\" ('A____x').",
               fixed = TRUE)
})

test_that("FT-PL069 [FR-PL181]: plan_columns(<not a table_plan>) stops", {
  expect_error(plan_columns(val_wide()),
               "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})


# ---- blank rows ------------------------------------------------------------

test_that("FT-PL070 [FR-PL182]: plan_blanks(), even empty, makes plan_apply() give RTF pages", {
  expect_s3_class(suppressMessages(plan_apply(val_plan())), "data.frame")
  pages <- suppressMessages(plan_apply(val_plan() |> plan_blanks()))
  expect_type(pages, "list")
  expect_s3_class(pages[[1L]], "rtftable")
})

test_that("FT-PL071 [FR-PL186]: plan_blanks(counted = TRUE) counts blanks in max_rows", {
  p <- table_plan(val_wide()) |> plan_paginate_rows(max_rows = 6, split = "group_safe")
  counted <- plan_apply(p |> plan_blanks(where = "between_groups", counted = TRUE))
  by_hand <- as_rtftables(val_wide(), split = "group_safe", max_rows = 6,
                          blank_rows = "between_groups", count_blank_rows = TRUE)
  expect_identical(val_page_shape(counted), val_page_shape(by_hand))
  for (pg in counted) expect_lte(nrow(pg$data) + length(pg$blank_rows), 6L)
  # not counted, the first page holds two groups and the blank between them
  loose <- plan_apply(p |> plan_blanks(where = "between_groups"))
  expect_identical(nrow(loose[[1L]]$data), 6L)
  expect_identical(loose[[1L]]$blank_rows, 3L)
})

test_that("FT-PL072 [FR-PL187]: plan_blanks(where = <positions>) puts a blank after those rows", {
  page <- plan_apply(table_plan(val_wide()) |> plan_blanks(where = c(2, 5)))[[1L]]
  expect_equal(page$blank_rows, c(2, 5))
})

test_that("FT-PL073 [FR-PL188]: plan_blanks(where = <blank_rows_by_*()>) goes to as_rtftables()", {
  d <- data.frame(grp = c("x", "x", "y", "y", "z", "z"), label = letters[1:6],
                  v = as.character(1:6))
  by_change <- blank_rows_by_change("grp")
  by_rule <- blank_rows_by_rule("label", "^b$", where = "after")
  for (spec in list(by_change, by_rule, list(by_change, by_rule))) {
    got <- plan_apply(table_plan(d) |> plan_blanks(where = spec))[[1L]]$blank_rows
    expect_identical(got, as_rtftables(d, blank_rows = spec)[[1L]]$blank_rows)
  }
  expect_equal(plan_apply(table_plan(d) |> plan_blanks(where = by_change))[[1L]]$blank_rows,
               c(2, 4))
})

test_that("FT-PL074 [FR-PL190]: plan_blanks() twice merges field by field, the later winning", {
  p <- table_plan(val_wide()) |> plan_blanks(where = "between_groups")
  args <- plan_apply(p |> plan_blanks(first = TRUE), "args")$rtf
  expect_identical(args$blank_rows, "between_groups")
  expect_true(args$blank_row_first)
  args <- plan_apply(p |> plan_blanks(first = TRUE) |> plan_blanks(where = 1), "args")$rtf
  expect_equal(args$blank_rows, 1)
  expect_true(args$blank_row_first)
})

test_that("FT-PL075 [FR-PL191]: plan_blanks(where = \"between_groups\"), one group: no blank row", {
  one <- data.frame(label = c("Group A", "  a1", "  a2"), v = c("1", "2", "3"))
  page <- plan_apply(table_plan(one) |> plan_blanks(where = "between_groups"))[[1L]]
  expect_length(page$blank_rows, 0L)
})

test_that("FT-PL076 [FR-PL194]: plan_blanks(where = \"nope\") stops at plan_apply()", {
  p <- table_plan(val_wide()) |> plan_blanks(where = "nope")
  expect_error(plan_apply(p), "Unrecognised `blank_rows` entry: nope", fixed = TRUE)
  expect_error(plan_apply(p), "declared by:\n    plan_blanks(where = \"nope\")", fixed = TRUE)
})

test_that("FT-PL077 [FR-PL195]: plan_blanks(<not a table_plan>) stops", {
  expect_error(plan_blanks(val_wide()),
               "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})
