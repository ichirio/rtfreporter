# Validation tests: the cells-side verbs -- plan_apply(), plan_layers(),
# plan_header_tokens(), plan_n_candidates(), plan_cells(), plan_digits(),
# plan_levels(), plan_labels(), plan_total(), plan_stat_groups(),
# plan_compare(), plan_sort(), plan_nest(), plan_empty().
# Small hand-built long frames (no cards); helpers prefixed vc_.

vc_long <- function() {
  data.frame(
    TRT       = rep(c("A", "B"), each = 6),
    PARAM     = rep(rep(c("ALT", "AST"), each = 3), 2),
    stat_name = rep(c("n", "mean", "sd"), 4),
    stat      = c(20, 31.245, 4.1, 20, 28.7, 3.92,
                  18, 33.108, 5.3, 18, 30.2, 4.44),
    stringsAsFactors = FALSE
  )
}

vc_base <- function(d = vc_long()) {
  table_plan(d, cols = "TRT", rows = c(param = "PARAM"), label = c(row = "stat_name"))
}

vc_plan <- function(d = vc_long()) {
  vc_base(d) |>
    plan_cells(c("n" = "{n:.0f}", "Mean (SD)" = "{mean} ({sd})"), notes = FALSE) |>
    plan_digits(2)
}

# the same frame with the analysis variable in a column called `variable`
vc_vlong <- function() {
  d <- vc_long()
  names(d)[names(d) == "PARAM"] <- "variable"
  d
}

vc_vbase <- function(d = vc_vlong()) {
  table_plan(d, cols = "TRT", rows = c(group = "variable"), label = c(row = "stat_name"))
}

# one count a row: PARAM the row key, VISIT the row label
vc_counts <- function() {
  data.frame(
    TRT       = rep(c("A", "B"), each = 4),
    PARAM     = rep(c("ALT", "ALT", "AST", "AST"), 2),
    VISIT     = rep(c("W1", "W2"), 4),
    stat_name = "n",
    stat      = c(5, 12, 3, 4, 6, 2, 8, 1),
    stringsAsFactors = FALSE
  )
}

vc_cplan <- function(d = vc_counts(), notes = FALSE) {
  table_plan(d, cols = "TRT", rows = c(param = "PARAM"), label = c(visit = "VISIT")) |>
    plan_cells("{n:.0f}", notes = notes)
}

vc_tab <- function(p, stage = "table") as.data.frame(suppressMessages(plan_apply(p, stage)))

vc_rows <- function(t) paste(t$param, t$visit)

# a finished table
vc_wide <- function() {
  data.frame(term = c("a", "b"), val = c(1.23456, 2.5), other = c(3.14159, 2),
             stringsAsFactors = FALSE)
}

# the long frame with overall rows (no TRT): A's statistics times ten
vc_total <- function() {
  d <- vc_long()
  o <- d[d$TRT == "A", ]
  o$TRT <- NA
  o$stat <- o$stat * 10
  rbind(d, o)
}

# the counts with the key's own tabulation (TRT counted: n per arm, N the total)
vc_own <- function(total = FALSE) {
  d <- vc_counts()
  d$variable <- d$PARAM
  d$.key_own <- FALSE
  own <- data.frame(TRT = c("A", "A", "B", "B", NA), PARAM = NA, VISIT = NA, stat_name = c("n", "N", "n", "N", "N"),
                    stat = c(20, 38, 18, 38, 38), variable = "TRT", .key_own = TRUE,
                    stringsAsFactors = FALSE)
  out <- rbind(d, own[names(d)])
  if (total) {
    o <- d[d$TRT == "A", ]
    o$TRT <- NA
    o$stat <- o$stat * 10
    out <- rbind(out, o)
  }
  out
}

# the frame with comparison records: a t-test per PARAM, no TRT, its own context
vc_cmp <- function() {
  d <- vc_long()
  d$variable <- d$PARAM
  d$context <- "summary"
  c1 <- data.frame(TRT = NA, PARAM = c("ALT", "AST"), stat_name = "p.value", stat = c(0.0004, 0.123),
                   variable = c("ALT", "AST"), context = "stats_t_test", stringsAsFactors = FALSE)
  c2 <- data.frame(TRT = NA, PARAM = c("ALT", "AST"), stat_name = "estimate", stat = c(-1.86, -1.5),
                   variable = c("ALT", "AST"), context = "stats_t_test", stringsAsFactors = FALSE)
  d <- rbind(d, c1, c2)
  d$.kind <- "continuous"
  d
}

vc_cp <- function(d = vc_cmp()) {
  table_plan(d, cols = "TRT", rows = c(param = "variable"), label = c(row = "stat_name")) |>
    plan_cells(c("n" = "{n:.0f}", "Mean (SD)" = "{mean:.1f} ({sd:.1f})"), notes = FALSE)
}

# a label column of one blank value: one row a PARAM
vc_lbl <- function(d) {
  d$LBL <- ""
  d
}

# two variables, RSUB to be nested under RACE's "Black"
vc_nest <- function() {
  data.frame(TRT = rep(c("A", "B"), each = 4),
             variable = rep(c("RACE", "RACE", "RSUB", "RSUB"), 2),
             LBL = rep(c("White", "Black", "Sub A", "Sub B"), 2),
             stat_name = "n", stat = c(10, 5, 3, 2, 11, 6, 4, 2), stringsAsFactors = FALSE)
}

vc_np <- function(d = vc_nest()) {
  table_plan(d, cols = "TRT", rows = c(group = "variable"), label = c(label = "LBL")) |>
    plan_cells("{n:.0f}", notes = FALSE)
}

# an AE-like normalized frame: an overall row ("Any TEAE") and, optionally, two SOCs
vc_empty <- function(n_any = 0, body = TRUE) {
  any <- data.frame(TRT = c("A", "B"), soc = NA, .label = "Any TEAE", stat_name = "n", stat = n_any,
                    .overall = TRUE, .kind = "categorical", stringsAsFactors = FALSE)
  b <- data.frame(TRT = rep(c("A", "B"), 2), soc = rep(c("Cardiac", "Skin"), each = 2),
                  .label = rep(c("Cardiac", "Skin"), each = 2), stat_name = "n", stat = c(3, 4, 1, 0),
                  .overall = FALSE, .kind = "categorical", stringsAsFactors = FALSE)
  if (body) rbind(any, b) else any
}

vc_ep <- function(d, ...) {
  table_plan(d, cols = "TRT", rows = c(soc = "soc"), ...) |> plan_cells("{n:.0f}", notes = FALSE)
}

vc_page1 <- function(p) {
  r <- suppressMessages(plan_apply(p))
  if (inherits(r, "rtftable")) r else r[[1L]]
}

vc_nbsp <- function(n) strrep(intToUtf8(160L), n)


# ---- plan_apply() ----------------------------------------------------------

test_that("FT-PL131 [FR-PL200 FR-PL203 FR-PL313]: plan_apply() goes as far as the plan declares, or as told", {
  p <- vc_plan()
  expect_s3_class(plan_apply(p), "data.frame")
  pg <- plan_apply(p |> plan_columns())
  expect_type(pg, "list")
  expect_s3_class(pg[[1L]], "rtftable")
  # a named stage stops where it is told
  expect_s3_class(plan_apply(p |> plan_columns(), "table"), "data.frame")
  expect_s3_class(plan_apply(p, "pages")[[1L]], "rtftable")
  expect_identical(plan_apply(p, "table"), plan_apply(p |> plan_columns(), "table"))
  # sorting alone does not turn the table into pages
  expect_s3_class(plan_apply(p |> plan_sort("-n")), "data.frame")
})

test_that("FT-PL132 [FR-PL201]: plan_apply(stage = \"input\") is the frame going in, the roles' columns added", {
  d <- vc_long()
  names(d)[names(d) == "stat_name"] <- "STAT"
  names(d)[names(d) == "stat"] <- "VAL"
  p <- table_plan(d, cols = "TRT", rows = c(param = "PARAM"), label = c(row = c("PARAM", "STAT")),
                  stat = c(name = "STAT", value = "VAL"))
  x <- plan_apply(p, "input")
  expect_identical(x$stat_name, d$STAT)
  expect_identical(x$stat, d$VAL)
  expect_identical(x$row, d$PARAM)          # coalesced: PARAM is never missing
  # plan_total(): the overall rows carry its label
  x2 <- plan_apply(vc_plan(vc_total()) |> plan_total(label = "All"), "input")
  expect_identical(x2$TRT[13:18], rep("All", 6L))
  expect_identical(x2[1:12, ], vc_long())
})

test_that("FT-PL133 [FR-PL202]: plan_apply(stage = \"args\") is the two argument lists, nothing run", {
  a <- plan_apply(vc_plan() |> plan_levels(TRT = c("B", "A")) |> plan_sort("-n") |>
                    plan_digits(rounding = "sas") |> plan_columns(sep = "|"), "args")
  expect_named(a, c("widen", "rtf"))
  expect_identical(a$widen$cols, "TRT")
  expect_identical(a$widen$rows, c(param = "PARAM"))
  expect_identical(a$widen$label, c(row = "stat_name"))
  expect_false(a$widen$notes)
  expect_identical(unname(a$widen$cells$default[["Mean (SD)"]]), "{mean:.2f} ({sd:.2f})")
  expect_identical(a$widen$levels$TRT, c("B", "A"))
  expect_identical(a$widen$sort, "-n")
  expect_identical(a$widen$rounding, "sas")
  expect_identical(a$widen$sep, "|")
  expect_false(a$rtf$read_meta)
  expect_identical(a$rtf$header_sep, "|")
  # a finished table: nothing to widen
  w <- plan_apply(table_plan(vc_wide()) |> plan_columns(), "args")
  expect_identical(w$widen, list())
  expect_false(w$rtf$read_meta)
})

test_that("FT-PL134 [FR-PL204]: a listing and a finished table pass through the stages", {
  recs <- data.frame(ID = c("1", "2"), AE = c("x", "y"), stringsAsFactors = FALSE)
  lp <- table_plan(recs) |> plan_listing(listing_col("ID"), listing_col("AE"))
  expect_identical(plan_apply(lp, "table"), recs)
  expect_identical(plan_apply(lp, "input"), recs)
  expect_s3_class(plan_apply(lp, "pages")[[1L]], "rtftable")
  expect_s3_class(plan_apply(lp)[[1L]], "rtftable")
  wp <- table_plan(vc_wide()) |> plan_columns()
  expect_identical(plan_apply(wp, "table"), vc_wide())
  expect_identical(plan_apply(wp, "input"), vc_wide())
  expect_s3_class(plan_apply(wp, "pages")[[1L]], "rtftable")
  expect_s3_class(plan_apply(wp)[[1L]], "rtftable")
})

test_that("FT-PL135 [FR-PL208 FR-PL209]: plan_apply() refuses what is not a plan, and an unknown stage", {
  expect_error(plan_apply(vc_long()), "Expected a table_plan; start from table_plan(ard).", fixed = TRUE)
  expect_error(plan_apply(vc_plan(), "x"),
               "'arg' should be one of \"auto\", \"input\", \"args\", \"table\", \"pages\"", fixed = TRUE)
})


# ---- plan_layers() ---------------------------------------------------------

test_that("FT-PL136 [FR-PL210 FR-PL211 FR-PL212 FR-PL213 FR-PL214]: plan_layers() reads the plan from the outside", {
  p <- vc_plan() |>
    plan_levels(TRT = c("B", "A")) |> plan_levels(PARAM = c("AST", "ALT")) |>
    plan_columns(widths = c(param = 2, row = 2, .values = 1)) |>
    plan_col_header(values = c(A = 20, B = 18), header = rtf_col_header(c("", "", "{col}")))
  lay <- plan_layers(p)
  expect_named(lay, c("data", "roles", "label", "group_col", "declared", "layers", "cells", "compare",
                      "columns", "header", "pages"))
  expect_identical(lay$data, vc_long())
  expect_identical(lay$label, "row")
  expect_null(lay$group_col)
  expect_identical(lay$declared, c("cells", "cell_options", "digits", "levels", "levels", "columns", "header"))
  # merged by kind, levels one key at a time
  expect_identical(lay$layers$levels$levels, list(TRT = c("B", "A"), PARAM = c("AST", "ALT")))
  expect_identical(lay$layers$cell_options, list(notes = FALSE))
  expect_identical(lay$layers$digits, list(default = 2))
  expect_identical(lay$columns$names, c("param", "row", "B", "A"))
  expect_identical(lay$columns$spread, c("B", "A"))
  expect_identical(lay$columns$page_names, c("param", "row", "B", "A"))
  expect_equal(lay$columns$widths, c(2, 2, 1, 1))
  # the cells parsed
  expect_length(lay$cells, 1L)
  expect_identical(lay$cells[[1L]]$variable, NA_character_)
  expect_identical(lay$cells[[1L]]$entry$labels, c("n", "Mean (SD)"))
  expect_null(lay$compare)
  # the header the plan built, as cell rows; the numbers were written in
  expect_identical(lay$header$source, "resolved")
  expect_s3_class(lay$header$cells, "data.frame")
  expect_null(lay$header$n_text)
  expect_true(lay$header$literal_n)
  expect_s3_class(lay$pages[[1L]], "rtftable")
  expect_identical(lay$pages[[1L]]$col_header[[1L]], c("", "", "B", "A"))
})

test_that("FT-PL137 [FR-PL213]: plan_layers()$compare parses the comparison columns; $cells is NULL for rows", {
  lay <- plan_layers(vc_cp() |> plan_compare(p = c(stats_t_test = "{p.value:.3f}"), position = "first") |>
                       plan_columns())
  expect_identical(lay$compare$position, "first")
  expect_null(lay$compare$by)
  expect_identical(lay$compare$cols[[1L]]$column, "p")
  expect_identical(lay$compare$cols[[1L]]$context, "stats_t_test")
  expect_identical(lay$compare$cols[[1L]]$entry$chains[[1L]][[1L]]$tpl, "{p.value:.3f}")
  rows <- plan_layers(vc_base() |> plan_cells(stats = "rows", notes = FALSE) |> plan_columns())
  expect_null(rows$cells)
  expect_identical(rows$declared, c("cell_options", "columns"))
})

test_that("FT-PL138 [FR-PL212 FR-PL215]: plan_layers() on a finished table, and not a plan", {
  lay <- plan_layers(table_plan(vc_wide()) |> plan_columns())
  expect_identical(lay$columns$names, c("term", "val", "other"))
  # no row key, no label column: the first column is the row heading, the
  # rest the values (#755)
  expect_identical(lay$columns$spread, c("val", "other"))
  expect_null(lay$columns$widths)
  expect_null(lay$cells)
  expect_identical(lay$declared, "columns")
  expect_error(plan_layers(vc_long()), "Expected a table_plan.", fixed = TRUE)
})


# ---- plan_header_tokens() / plan_n_candidates() ----------------------------

test_that("FT-PL139 [FR-PL216 FR-PL219]: plan_header_tokens() lists {col} (and {col1}, {col2}) with their values", {
  tk <- plan_header_tokens(vc_plan())       # not run yet: it builds the table itself
  expect_named(tk, c("token", "kind", "values", "text", "resolved", "note"))
  expect_identical(tk$token, "{col}")
  expect_identical(tk$kind, "column")
  expect_identical(tk$values[[1L]], c("A", "B"))
  expect_identical(tk$text, "= c(A, B)")
  expect_true(tk$resolved)
  p2 <- table_plan(vc_long(), cols = c("TRT", "PARAM"), label = c(row = "stat_name")) |>
    plan_cells("{stat:.1f}", notes = FALSE)
  tk2 <- plan_header_tokens(p2)
  expect_identical(tk2$token, c("{col}", "{col1}", "{col2}"))
  expect_identical(tk2$values[[1L]], c("ALT", "AST"))
  expect_identical(tk2$values[[2L]], c("A", "B"))
  expect_identical(tk2$values[[3L]], c("ALT", "AST"))
  # print() shows the same text (once the table is known: the same object)
  p <- vc_plan()
  invisible(plan_header_tokens(p))
  out <- utils::capture.output(print(p))
  expect_true(any(grepl("header tokens", out, fixed = TRUE)))
  expect_true(any(grepl(tk$text, out, fixed = TRUE)))
})

test_that("FT-PL140 [FR-PL217 FR-PL218]: {n}, {n:sum}, {n:<column>} from plan_col_header(values = ); the unresolved say why", {
  tk <- plan_header_tokens(vc_plan() |> plan_col_header(values = c(A = 20, B = 18)))
  expect_identical(tk$token, c("{col}", "{n}", "{n:sum}", "{n:A}", "{n:B}"))
  expect_identical(tk$kind, c("column", "population", "sum", "one", "one"))
  expect_identical(tk$values[[2L]], c(A = 20, B = 18))
  expect_identical(tk$values[[3L]], 38)
  expect_identical(tk$values[[4L]], 20)
  expect_identical(tk$text[2:3], c("= c(A = 20, B = 18)",
                                   "= 38 over every column (less over a spanner: its own columns)"))
  expect_true(all(tk$resolved))
  # a list: every name a token
  tk2 <- plan_header_tokens(vc_plan() |> plan_col_header(values = list(n = c(A = 20, B = 18), N = 38)))
  expect_identical(tk2$token, c("{col}", "{n}", "{n:sum}", "{n:A}", "{n:B}", "{N}", "{N:sum}"))
  expect_identical(tk2$values[[6L]], 38)
  # a population the data cannot state
  tk3 <- plan_header_tokens(vc_plan() |> plan_col_header(values = list(n = TRUE)))
  expect_identical(tk3$token, c("{col}", "{n}"))
  expect_false(tk3$resolved[2L])
  expect_identical(tk3$note[2L], "the data has no `variable` / `stat_name` / `stat` for these columns")
  expect_identical(tk3$text[2L], paste0("-- NOT resolved: ", tk3$note[2L]))
  expect_null(tk3$values[[2L]])
})

test_that("FT-PL141 [FR-PL220 FR-PL222 FR-PL266]: a key's own tabulation states the populations; Total is the study total; a comparison column is no {col}", {
  p <- table_plan(vc_own(total = TRUE), cols = "TRT", rows = c(param = "PARAM"), label = c(visit = "VISIT")) |>
    plan_cells("{n:.0f}", notes = FALSE)
  cand <- plan_n_candidates(p)
  expect_identical(cand, data.frame(scope = "all", page = NA_character_, column = c("A", "B", NA),
                                    value = c(20, 18, 38), stringsAsFactors = FALSE), ignore_attr = TRUE)
  expect_false(attr(cand, "differ"))
  tk <- plan_header_tokens(p |> plan_total() |> plan_col_header(values = list(n = TRUE)))
  expect_identical(tk$values[[match("{col}", tk$token)]], c("A", "B", "Total"))
  expect_equal(tk$values[[match("{n}", tk$token)]], c(A = 20, B = 18, Total = 38), ignore_attr = TRUE)
  expect_identical(tk$values[[match("{n:sum}", tk$token)]], 38)      # Total left out of the sum
  expect_identical(tk$values[[match("{n:Total}", tk$token)]], 38)
  expect_identical(vc_tab(p |> plan_total())$Total, c("50", "120", "30", "40"))
  # a comparison column, and the row keys and label, are no {col}
  tkc <- plan_header_tokens(vc_cp() |> plan_compare(p = "{p.value:.3f}"))
  expect_identical(tkc$values[[1L]], c("A", "B"))
})

test_that("FT-PL142 [FR-PL221 FR-PL224 FR-PL225]: nothing to read is no row; not a plan is refused", {
  one <- vc_counts()[vc_counts()$TRT == "A", ]
  one$TRT <- NULL
  p1 <- table_plan(one, rows = c(param = "PARAM"), label = c(visit = "VISIT")) |> plan_cells("{n:.0f}", notes = FALSE)
  expect_identical(names(vc_tab(p1)), c("param", "visit", "All"))
  nc <- plan_n_candidates(p1)
  expect_identical(nrow(nc), 0L)
  expect_identical(names(nc), c("scope", "page", "column", "value"))
  expect_identical(nrow(plan_n_candidates(vc_plan())), 0L)   # cols, but no stated population
  expect_error(plan_header_tokens(vc_long()), "plan_header_tokens(): expected a table_plan.", fixed = TRUE)
  expect_error(plan_n_candidates(vc_long()), "plan_n_candidates(): expected a table_plan.", fixed = TRUE)
})


# ---- plan_cells() ----------------------------------------------------------

test_that("FT-PL143 [FR-PL226 FR-PL227 FR-PL228]: a template per cell, keyed by specificity; rows, chains and cell_rows()", {
  t <- vc_tab(vc_vbase() |> plan_cells("{mean:.1f}", notes = FALSE))
  expect_identical(t$A[t$row == "mean"], c("31.2", "28.7"))
  expect_true(all(is.na(t$A[t$row != "mean"])))
  # a variable's own entry before the default
  t2 <- vc_tab(vc_vbase() |> plan_cells("{mean:.1f}", ALT = "{n:.0f}", notes = FALSE))
  expect_identical(t2$A[t2$group == "ALT" & t2$row == "n"], "20")
  expect_identical(t2$A[t2$group == "AST" & t2$row == "mean"], "28.7")
  expect_identical(t2, vc_tab(vc_vbase() |> plan_cells(default = "{mean:.1f}", ALT = "{n:.0f}", notes = FALSE)))
  # one row a template name
  t3 <- vc_tab(vc_vbase() |> plan_cells(ALT = c(N = "{n:.0f}"), AST = c("Mean (SD)" = "{mean:.1f} ({sd:.1f})"),
                                        notes = FALSE))
  expect_identical(t3$row, c("N", "Mean (SD)"))
  expect_identical(t3$B, c("18", "30.2 (4.4)"))
  # a guard chain, and cell_rows()
  t4 <- vc_tab(vc_vbase() |> plan_cells(c(mean > 30 ~ "high", "{mean:.1f}"), notes = FALSE))
  expect_identical(t4$A[t4$row == "mean"], c("high", "28.7"))
  expect_identical(t4$B[t4$row == "mean"], c("high", "high"))
  t5 <- vc_tab(vc_vbase() |> plan_cells(cell_rows("1" = "{n:.0f}", "2" = "{mean:.1f}"), notes = FALSE))
  expect_identical(t5$row, c("1", "2", "1", "2"))
  expect_identical(t5$A, c("20", "31.2", "20", "28.7"))
})

test_that("FT-PL144 [FR-PL229 FR-PL233]: a later plan_cells() replaces its key and keeps the rest; options alone add no template", {
  p <- vc_vbase() |> plan_cells("{mean:.1f}", ALT = "{n:.0f}", notes = FALSE) |> plan_cells(ALT = "{sd:.1f}")
  cells <- plan_apply(p, "args")$widen$cells
  expect_identical(cells$default, "{mean:.1f}")
  expect_identical(cells$ALT, "{sd:.1f}")
  expect_identical(cells$AST, "{mean:.1f}")        # expanded per variable: the default's
  expect_identical(vc_tab(p)$A[1:3], c(NA, NA, "4.1"))
  a <- plan_apply(vc_base() |> plan_cells("{n}") |> plan_cells(notes = FALSE, na = "-"), "args")$widen
  expect_identical(a$cells, list(default = "{n}"))
  expect_identical(a$na, "-")
})

test_that("FT-PL145 [FR-PL230 FR-PL231 FR-IX201]: stats = \"rows\" and value = ; na = fills what no template could", {
  r <- vc_tab(vc_base() |> plan_cells(stats = "rows", notes = FALSE))
  expect_identical(r$row, rep(c("n", "mean", "sd"), 2))
  expect_identical(r$A, c(20, 31.245, 4.1, 20, 28.7, 3.92))
  expect_type(r$A, "double")
  fmt <- vc_tab(vc_base() |> plan_cells(stats = "rows", value = "stat_fmt", notes = FALSE))
  expect_true(all(is.na(fmt$A)))              # the frame has no stat_fmt
  # with templates value = changes nothing: {mean} reads stat
  expect_identical(vc_tab(vc_base() |> plan_cells("{mean}", value = "stat_fmt", notes = FALSE)),
                   vc_tab(vc_base() |> plan_cells("{mean}", notes = FALSE)))
  t <- vc_tab(vc_base() |> plan_cells("{mean}", notes = FALSE))
  expect_true(all(is.na(t$A[t$row != "mean"])))
  t2 <- vc_tab(vc_base() |> plan_cells("{mean}", na = "-", notes = FALSE))
  expect_identical(t2$A[t2$row != "mean"], rep("-", 4L))
  # a finished table: na alone, and it means pages
  wide <- data.frame(label = c("a", "b"), v = c("1.5", NA), stringsAsFactors = FALSE)
  pg <- plan_apply(table_plan(wide) |> plan_cells(na = "NA!"))
  expect_s3_class(pg[[1L]], "rtftable")
  expect_identical(pg[[1L]]$data$v, c("1.5", "NA!"))
})

test_that("FT-PL146 [FR-PL232]: plan_cells(notes = ) reports the statistics no template used, or not", {
  msgs <- character()
  withCallingHandlers(
    plan_apply(vc_base() |> plan_cells(c("Mean (SD)" = "{mean:.1f} ({sd:.2f})")), "table"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  expect_match(paste(msgs, collapse = "\n"), "4 ARD rows were not used", fixed = TRUE)
  expect_match(paste(msgs, collapse = "\n"), "no template named it", fixed = TRUE)
  p <- vc_base() |> plan_cells(c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"), notes = FALSE)
  expect_silent(plan_apply(p, "table"))
  expect_silent(as_rtftables(p))
})

test_that("FT-PL147 [FR-PL234 FR-PL235 FR-PL236 FR-PL237]: plan_cells() refuses what it cannot key, read or run", {
  expect_error(plan_cells(vc_base(), "{n}", "{mean}"),
               "Give at most one unnamed value (the plan-wide default); name the rest.", fixed = TRUE)
  expect_error(plan_cells(vc_base(), a = "{n}", a = "{mean}"),
               "the same key is given twice in one call: 'a'.", fixed = TRUE)
  expect_error(plan_cells(vc_base(), stats = "x"), "'arg' should be one of \"cells\", \"rows\"", fixed = TRUE)
  expect_error(plan_cells(vc_base(), value = "x"), "'arg' should be one of \"stat\", \"stat_fmt\"", fixed = TRUE)
  expect_error(plan_cells(vc_base(), notes = NA),
               "plan_cells(notes = ) is TRUE (report the statistics no template used) or FALSE.", fixed = TRUE)
  expect_error(plan_apply(vc_base() |> plan_cells("{mean:%}", notes = FALSE), "table"),
               "`{mean:%}` asks the plan for its digits, but nothing declared them for 'default'.", fixed = TRUE)
  expect_error(plan_cells(vc_long(), "{n}"), "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})


# ---- plan_digits() ---------------------------------------------------------

test_that("FT-PL148 [FR-PL240 FR-PL241 FR-PL242 FR-IX202]: digits fill the open tokens, by statistic, by key, last wins", {
  tpl <- function(p) plan_apply(p, "args")$widen$cells
  open <- vc_base() |> plan_cells(c("Mean (SD)" = "{mean} ({sd})"), notes = FALSE)
  expect_identical(unname(tpl(open |> plan_digits(2))$default[["Mean (SD)"]]), "{mean:.2f} ({sd:.2f})")
  expect_identical(unname(tpl(open |> plan_digits(c(mean = 1, sd = 3)))$default[["Mean (SD)"]]),
                   "{mean:.1f} ({sd:.3f})")
  expect_identical(unname(tpl(open |> plan_digits("3s"))$default[["Mean (SD)"]]), "{mean:.3s} ({sd:.3s})")
  expect_identical(vc_tab(open |> plan_digits("3s"))$A, c("31.2 (4.10)", "28.7 (3.92)"))
  # a token with its own digits keeps them
  mixed <- vc_base() |> plan_cells(c("Mean (SD)" = "{mean:.4f} ({sd})"), notes = FALSE) |> plan_digits(1)
  expect_identical(unname(tpl(mixed)$default[["Mean (SD)"]]), "{mean:.4f} ({sd:.1f})")
  expect_identical(vc_tab(open |> plan_digits(1) |> plan_digits(default = 0))$A, c("31 (4)", "29 (4)"))
  # by variable: the narrower key wins, a statistic it leaves falls through
  v <- vc_vbase() |> plan_cells(c("Mean (SD)" = "{mean} ({sd})"), notes = FALSE) |> plan_digits(1)
  expect_identical(vc_tab(v |> plan_digits(AST = 3))$A, c("31.2 (4.1)", "28.700 (3.920)"))
  expect_identical(vc_tab(v |> plan_digits(AST = c(mean = 3)))$A, c("31.2 (4.1)", "28.700 (3.9)"))
  expect_identical(vc_tab(v |> plan_digits(AST = 3) |> plan_digits(AST = 0))$A, c("31.2 (4.1)", "29 (4)"))
})

test_that("FT-PL149 [FR-PL243 FR-PL249]: plan_digits(rounding = ) is the run's family, last wins", {
  d <- vc_long()
  d$stat[d$stat_name == "mean"] <- 2.125
  p <- vc_base(d) |> plan_cells("{mean}", notes = FALSE)
  expect_identical(vc_tab(p |> plan_digits(2, rounding = "r"))$A[2L], "2.12")
  expect_identical(vc_tab(p |> plan_digits(2, rounding = "sas"))$A[2L], "2.13")
  q <- p |> plan_digits(2, rounding = "sas") |> plan_digits(rounding = "r")
  expect_identical(plan_apply(q, "args")$widen$rounding, "r")
  expect_identical(vc_tab(q)$A[2L], "2.12")
  # rounding alone adds a round layer and no digits layer
  kinds <- vapply((p |> plan_digits(rounding = "r"))$layers, `[[`, "", "kind")
  expect_identical(kinds, c("cells", "cell_options", "round"))
  expect_error(plan_apply(p |> plan_digits(2, rounding = "x"), "table"),
               "`rounding` must be \"r\" (half to even, as base::round() does) or \"sas\" (half away from zero, as SAS ROUND() does).",
               fixed = TRUE)
})

test_that("FT-PL150 [FR-PL244 FR-PL246 FR-PL247]: a finished table's columns by name, .rows by the label; a key that reaches nothing is refused", {
  pg <- plan_apply(table_plan(vc_wide()) |> plan_digits(val = 2, other = "3s") |> plan_columns())
  expect_identical(pg[[1L]]$data$val, c("1.23", "2.50"))
  expect_identical(pg[[1L]]$data$other, c("3.14", "2.00"))
  pg2 <- plan_apply(table_plan(vc_wide()) |> plan_digits(val = 2) |> plan_digits(val = 0) |> plan_columns())
  expect_identical(pg2[[1L]]$data$val, c("1", "2"))
  rows <- plan_apply(vc_base() |> plan_cells(stats = "rows", notes = FALSE) |>
                       plan_digits(.rows = c(n = 0, mean = 1, sd = "2s")) |> plan_columns())
  expect_identical(rows[[1L]]$data$A, c("20", "31.2", "4.1", "20", "28.7", "3.9"))
  expect_error(plan_apply(table_plan(vc_wide()) |> plan_digits(nope = 2) |> plan_columns()),
               "plan_digits() names 'nope', which matched nothing.\n  A key is a column of the table, `.rows` or \"default\".",
               fixed = TRUE)
  expect_error(plan_apply(table_plan(vc_wide()) |> plan_digits(2) |> plan_columns()),
               "plan_digits() names 'default', which matched nothing.", fixed = TRUE)
  expect_error(plan_apply(vc_plan() |> plan_digits(nope = 1), "table"),
               "plan_digits() names 'nope', which matched nothing.\n  A key is an analysis variable, a context, a kind (continuous / categorical), a column of the table, `.rows` or \"default\".",
               fixed = TRUE)
  expect_error(plan_apply(table_plan(vc_wide()) |> plan_digits(.rows = c(a = 1)) |> plan_columns()),
               "plan_digits(.rows = ) is keyed by the value of the label column, and this table has none.",
               fixed = TRUE)
})

test_that("FT-PL911 [FR-PL245]: statistics as rows (plan_cells(stats = 'rows')): a column key formats that column; a key that reaches none is refused", {
  p <- vc_base() |> plan_cells(stats = "rows", notes = FALSE)
  pg <- plan_apply(p |> plan_digits(A = 1) |> plan_columns())
  expect_identical(pg[[1L]]$data$A, c("20.0", "31.2", "4.1", "20.0", "28.7", "3.9"))
  expect_identical(pg[[1L]]$data$B, c(18, 33.108, 5.3, 18, 30.2, 4.44))
  pg2 <- plan_apply(p |> plan_digits(A = 1) |> plan_digits(A = 0) |> plan_columns())
  expect_identical(pg2[[1L]]$data$A, c("20", "31", "4", "20", "29", "4"))
  expect_error(plan_apply(p |> plan_digits(nope = 1) |> plan_columns()),
               "plan_digits() names 'nope', which matched nothing.\n  A key is a column of the table, `.rows` or \"default\".",
               fixed = TRUE)
})

test_that("FT-PL151 [FR-PL248 FR-PL250]: plan_digits() refuses two defaults, a key twice, and not a plan", {
  expect_error(plan_digits(vc_base(), 1, 2),
               "Give at most one unnamed value (the plan-wide default); name the rest.", fixed = TRUE)
  expect_error(plan_digits(vc_base(), a = 1, a = 2), "the same key is given twice in one call: 'a'.", fixed = TRUE)
  expect_error(plan_digits(vc_long(), 1), "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})


# ---- plan_levels() / plan_labels() -----------------------------------------

test_that("FT-PL152 [FR-PL251 FR-PL252 FR-PL253]: plan_levels() orders a column key, a row key, the label rows, the variables; names are text", {
  expect_identical(names(vc_tab(vc_plan() |> plan_levels(TRT = c("B", "A")))), c("param", "row", "B", "A"))
  ch <- function(x) as.character(x)
  expect_identical(ch(vc_tab(vc_plan() |> plan_levels(PARAM = c("AST", "ALT")))$param), c("AST", "AST", "ALT", "ALT"))
  expect_identical(ch(vc_tab(vc_plan() |> plan_levels(param = c("AST", "ALT")))$param), c("AST", "AST", "ALT", "ALT"))
  expect_identical(ch(vc_tab(vc_plan() |> plan_levels(row = c("Mean (SD)", "n")))$row), rep(c("Mean (SD)", "n"), 2))
  expect_identical(ch(vc_tab(vc_plan() |> plan_levels(stat_name = c("Mean (SD)", "n")))$row),
                   rep(c("Mean (SD)", "n"), 2))
  v <- vc_vbase() |> plan_cells("{mean:.1f}", notes = FALSE)
  expect_identical(unique(as.character(vc_tab(v |> plan_levels(variable = c("AST", "ALT")))$group)), c("AST", "ALT"))
  expect_identical(unique(as.character(vc_tab(v |> plan_levels(variable = "AST"))$group)), c("AST", "ALT"))
  expect_identical(unique(as.character(vc_tab(v |> plan_levels(variable = c(AST = "Aspartate", "ALT")))$group)),
                   c("Aspartate", "ALT"))
  t <- vc_tab(vc_plan() |> plan_levels(TRT = c(B = "Drug", A = "Placebo")))
  expect_identical(names(t), c("param", "row", "Drug", "Placebo"))
  expect_identical(t$Drug[1L], "18")
})

test_that("FT-PL153 [FR-PL254 FR-PL257 FR-PL258 FR-IX200]: levels and labels merge a key at a time; labels name without ordering and win over levels' text", {
  a <- plan_apply(vc_plan() |> plan_levels(TRT = c("B", "A")) |> plan_levels(PARAM = c("AST", "ALT")), "args")$widen$levels
  expect_identical(a, list(TRT = c("B", "A"), PARAM = c("AST", "ALT")))
  b <- plan_apply(vc_plan() |> plan_levels(TRT = c("B", "A")) |> plan_levels(TRT = c("A", "B")), "args")$widen$levels
  expect_identical(b, list(TRT = c("A", "B")))
  v <- vc_vbase() |> plan_cells("{mean:.1f}", notes = FALSE)
  whole <- c(AST = "Aspartate")
  expect_identical(unique(as.character(vc_tab(v |> plan_labels(whole))$group)), c("ALT", "Aspartate"))
  # a column key's and a row key's values as text, no reordering
  expect_identical(names(vc_tab(vc_plan() |> plan_labels(TRT = c(B = "Drug", A = "Placebo")))),
                   c("param", "row", "Placebo", "Drug"))
  expect_identical(as.character(vc_tab(vc_plan() |> plan_labels(PARAM = c(ALT = "Alanine")))$param),
                   c("Alanine", "Alanine", "AST", "AST"))
  # the same key restated is replaced, not merged into
  expect_identical(names(vc_tab(vc_plan() |> plan_labels(TRT = c(A = "Placebo")) |> plan_labels(TRT = c(B = "Drug")))),
                   c("param", "row", "A", "Drug"))
  expect_identical(names(vc_tab(vc_plan() |> plan_labels(TRT = c(A = "Placebo")) |> plan_labels(TRT = c(A = "Pbo")))),
                   c("param", "row", "Pbo", "B"))
  # a variable's heading, two ways
  expect_identical(vc_tab(v |> plan_labels(variable = c(AST = "Aspartate"))), vc_tab(v |> plan_labels(AST = "Aspartate")))
  expect_identical(unique(as.character(vc_tab(v |> plan_labels(AST = "Aspartate"))$group)), c("ALT", "Aspartate"))
  # plan_labels()' text wins value by value, before or after plan_levels()
  want <- c("param", "row", "Drug", "Pbo")
  expect_identical(names(vc_tab(vc_plan() |> plan_levels(TRT = c(B = "Drug", A = "Placebo")) |>
                                  plan_labels(TRT = c(A = "Pbo")))), want)
  expect_identical(names(vc_tab(vc_plan() |> plan_labels(TRT = c(A = "Pbo")) |>
                                  plan_levels(TRT = c(B = "Drug", A = "Placebo")))), want)
})

test_that("FT-PL154 [FR-PL256]: plan_levels(.drop_empty = ) leaves out a level that is 0 in every column", {
  d <- vc_counts()
  d$variable <- "VISIT"
  d$variable_level <- d$VISIT
  d$stat[d$VISIT == "W2"] <- 0
  expect_identical(vc_rows(vc_tab(vc_cplan(d))), c("ALT W1", "ALT W2", "AST W1", "AST W2"))
  expect_identical(vc_rows(vc_tab(vc_cplan(d) |> plan_levels(.drop_empty = "VISIT"))), c("ALT W1", "AST W1"))
  d$stat[d$VISIT == "W2" & d$TRT == "B"] <- 1       # counted in one column: stays
  expect_identical(vc_rows(vc_tab(vc_cplan(d) |> plan_levels(.drop_empty = "VISIT"))),
                   c("ALT W1", "ALT W2", "AST W1", "AST W2"))
})

test_that("FT-PL155 [FR-PL261 FR-PL262 FR-PL263]: plan_levels() / plan_labels() refuse an unnamed entry, a bad .drop_empty, not a plan", {
  expect_error(plan_levels(vc_base(), c("A", "B")),
               "plan_levels(): every entry is named by the column or variable it applies to,", fixed = TRUE)
  expect_error(plan_labels(vc_base(), "x"),
               "plan_labels(): every entry is named by the column or variable it applies to,", fixed = TRUE)
  expect_error(plan_levels(vc_base(), .drop_empty = 1), "plan_levels(.drop_empty = ) takes variable names.", fixed = TRUE)
  expect_error(plan_levels(vc_base(), .drop_empty = NA_character_), "takes variable names", fixed = TRUE)
  expect_error(plan_levels(vc_long(), TRT = "A"), "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
  expect_error(plan_labels(vc_long(), TRT = c(A = "x")), "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})


# ---- plan_total() ----------------------------------------------------------

test_that("FT-PL156 [FR-PL264 FR-PL265 FR-PL270 FR-PL271]: the overall rows become the Total column, where asked", {
  t <- vc_tab(vc_plan(vc_total()) |> plan_total())
  expect_identical(names(t), c("param", "row", "A", "B", "Total"))
  expect_identical(t$Total, c("200", "312.45 (41.00)", "200", "287.00 (39.20)"))
  expect_identical(names(vc_tab(vc_plan(vc_total()))), c("param", "row", "A", "B"))
  msgs <- character()
  withCallingHandlers(
    plan_apply(vc_base(vc_total()) |> plan_cells(c("n" = "{n:.0f}", "Mean (SD)" = "{mean:.1f} ({sd:.1f})")), "table"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  expect_match(paste(msgs, collapse = "\n"), "no value for a `cols` key", fixed = TRUE)
  expect_identical(names(vc_tab(vc_plan(vc_total()) |> plan_total(label = "All", position = "first"))),
                   c("param", "row", "All", "A", "B"))
  expect_identical(names(vc_tab(vc_plan(vc_total()) |> plan_levels(TRT = c("B", "A")) |> plan_total(position = "first"))),
                   c("param", "row", "Total", "B", "A"))
  # twice: the last call, its defaults included
  expect_identical(names(vc_tab(vc_plan(vc_total()) |> plan_total(label = "All") |> plan_total(position = "first"))),
                   c("param", "row", "Total", "A", "B"))
  # a factor key keeps its order
  d <- vc_total()
  d$TRT <- factor(d$TRT, levels = c("B", "A"))
  expect_identical(names(vc_tab(vc_plan(d) |> plan_total())), c("param", "row", "B", "A", "Total"))
  expect_identical(names(vc_tab(vc_plan(d) |> plan_total(position = "first"))), c("param", "row", "Total", "B", "A"))
})

test_that("FT-PL157 [FR-PL272 FR-PL273 FR-PL274 FR-PL275 FR-PL276 FR-PL277]: plan_total() says what is wrong", {
  one <- vc_long()[vc_long()$TRT == "A", ]
  expect_error(plan_apply(table_plan(one, rows = c(param = "PARAM"), label = c(row = "stat_name")) |>
                            plan_cells("{n:.0f}", notes = FALSE) |> plan_total(), "table"),
               "plan_total(): a Total column is read for a column key; this table has no cols.", fixed = TRUE)
  expect_error(plan_apply(vc_plan() |> plan_total(), "table"),
               "plan_total(): the ARD has no overall rows, rows without TRT.", fixed = TRUE)
  d <- vc_total()
  made <- d[d$TRT %in% "B", ]
  made$TRT <- "Total"
  d <- rbind(d, made)          # overall rows, and a Total made in the data
  expect_error(plan_apply(vc_plan(d) |> plan_total(), "table"),
               "plan_total(label = \"Total\"): TRT already has a value \"Total\" (a Total made in the data?).  Keep one of the two.",
               fixed = TRUE)
  expect_error(plan_total(vc_plan(), label = ""), "plan_total(label = ) is one text, the column's heading: \"Total\".", fixed = TRUE)
  expect_error(plan_total(vc_plan(), label = c("a", "b")), "is one text", fixed = TRUE)
  expect_error(plan_total(vc_plan(), label = NA), "is one text", fixed = TRUE)
  expect_error(plan_total(vc_plan(), position = "x"), "'arg' should be one of \"last\", \"first\"", fixed = TRUE)
  expect_error(plan_total(vc_plan(), direction = "x"), "'arg' should be one of \"cols\", \"rows\", \"both\"", fixed = TRUE)
  expect_error(plan_apply(vc_plan(vc_total()) |> plan_total(direction = "rows"), "table"),
               "plan_total(direction = \"rows\"): the ARD has no margins -- a key's tabulation without the body's variable.",
               fixed = TRUE)
  expect_error(plan_total(vc_long()), "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})


# ---- plan_stat_groups() ----------------------------------------------------

test_that("FT-PL158 [FR-PL278 FR-PL279 FR-PL280]: the groups across or down, templates by group, the last call's groups", {
  sg <- function(d = vc_lbl(vc_long())) {
    table_plan(d, cols = c("TRT", ".stat_group"), rows = c(param = "PARAM"), label = c(row = "LBL"))
  }
  across <- vc_tab(sg() |> plan_stat_groups(N = "n", msd = c("mean", "sd")) |>
                     plan_cells(N = "{n:.0f}", msd = "{mean:.1f} ({sd:.1f})", notes = FALSE))
  expect_identical(names(across), c("param", "row", "A____N", "A____msd", "B____N", "B____msd"))
  expect_identical(across$A____N, c("20", "20"))
  expect_identical(across$B____msd, c("33.1 (5.3)", "30.2 (4.4)"))
  down <- vc_tab(table_plan(vc_lbl(vc_long()), cols = "TRT", rows = c(param = "PARAM", stat = ".stat_group"),
                            label = c(row = "LBL")) |>
                   plan_stat_groups(msd = c("mean", "sd"), N = "n") |>
                   plan_cells(N = "{n:.0f}", msd = "{mean:.1f} ({sd:.1f})", notes = FALSE))
  # the groups' order down is the data's (n before mean), not the order said; plan_levels() orders them
  expect_identical(as.character(down$stat), rep(c("N", "msd"), 2))
  expect_identical(down$A, c("20", "31.2 (4.1)", "20", "28.7 (3.9)"))
  ordered <- vc_tab(table_plan(vc_lbl(vc_long()), cols = "TRT", rows = c(param = "PARAM", stat = ".stat_group"),
                               label = c(row = "LBL")) |>
                      plan_stat_groups(msd = c("mean", "sd"), N = "n") |>
                      plan_levels(.stat_group = c("msd", "N")) |>
                      plan_cells(N = "{n:.0f}", msd = "{mean:.1f} ({sd:.1f})", notes = FALSE))
  expect_identical(as.character(ordered$stat), rep(c("msd", "N"), 2))
  # a statistic in no group is not read, and noted
  msgs <- character()
  t <- withCallingHandlers(
    as.data.frame(plan_apply(sg() |> plan_stat_groups(N = "n") |> plan_cells(N = "{n:.0f}"), "table")),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  expect_identical(names(t), c("param", "row", "A____N", "B____N"))
  expect_match(paste(msgs, collapse = "\n"), "in no `stat_groups` group", fixed = TRUE)
  # twice: the last call only
  twice <- vc_tab(sg() |> plan_stat_groups(N = "n") |> plan_stat_groups(msd = c("mean", "sd")) |>
                    plan_cells(msd = "{mean:.1f} ({sd:.1f})", N = "{n}", notes = FALSE))
  expect_identical(names(twice), c("param", "row", "A____msd", "B____msd"))
})

test_that("FT-PL159 [FR-PL281]: plan_stat_groups(.variables = , .others = ) scopes the split", {
  vl <- vc_lbl(vc_long())
  vl$variable <- vl$PARAM
  pv <- table_plan(vl, cols = c("TRT", ".stat_group"), rows = c(group = "variable"), label = c(row = "LBL"))
  whole <- vc_tab(pv |> plan_stat_groups(N = "n", msd = c("mean", "sd"), .variables = "ALT", .others = "N") |>
                    plan_cells(N = "{n:.0f}", msd = "{mean:.1f} ({sd:.1f})", notes = FALSE))
  expect_identical(as.character(whole$group), c("ALT", "AST"))
  expect_identical(whole$A____N, c("20", "20"))
  expect_true(is.na(whole$A____msd[2L]))
  # the other variable keeps its own cell key first
  own <- vc_tab(pv |> plan_stat_groups(N = "n", msd = c("mean", "sd"), .variables = "ALT", .others = "N") |>
                  plan_cells(N = "{n:.0f}", msd = "{mean:.1f} ({sd:.1f})", AST = "{mean:.2f}", notes = FALSE))
  expect_identical(own$A____N, c("20", "28.70"))
  # without .others the other variables are left out
  alone <- vc_tab(pv |> plan_stat_groups(N = "n", msd = c("mean", "sd"), .variables = "ALT") |>
                    plan_cells(N = "{n:.0f}", msd = "{mean:.1f} ({sd:.1f})", notes = FALSE))
  expect_identical(as.character(alone$group), "ALT")
})

test_that("FT-PL160 [FR-PL285 FR-PL286 FR-PL287 FR-PL288]: plan_stat_groups() says what is wrong", {
  p <- vc_base()
  expect_error(plan_stat_groups(p, N = "n", "mean"),
               "`stat_groups`: every group has a name of its own (`n = \"N\", orr_ci = c(\"n\", \"estimate\")`).",
               fixed = TRUE)
  expect_error(plan_stat_groups(p), "every group has a name of its own", fixed = TRUE)
  expect_error(plan_stat_groups(p, a = "n", b = c("n", "sd")),
               "`stat_groups`: 'n' is in two groups; a statistic is in one.", fixed = TRUE)
  expect_error(plan_stat_groups(p, a = character()), "`stat_groups`: 'a' names no statistic.", fixed = TRUE)
  expect_error(plan_stat_groups(p, a = "n", .variables = 1),
               "`stat_groups`: `.variables` names the variables split into the groups: \"RESP\".", fixed = TRUE)
  expect_error(plan_stat_groups(p, a = "n", .variables = "ALT", .others = "zz"),
               "`stat_groups`: `.others` is one of the groups ('a'), where the other variables' records go whole.",
               fixed = TRUE)
  expect_error(plan_stat_groups(p, a = "n", .others = "a"),
               "`stat_groups`: `.others` needs `.variables` (which variables are split).", fixed = TRUE)
  expect_error(plan_stat_groups(vc_long(), a = "n"), "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})


# ---- plan_compare() --------------------------------------------------------

test_that("FT-PL161 [FR-PL289 FR-PL290 FR-PL291 FR-PL292 FR-PL295]: comparison columns, placed on the block, as the templates say", {
  t <- vc_tab(vc_cp() |> plan_compare(p = "{p.value:.3f}"))
  expect_identical(names(t), c("param", "row", "A", "B", "p"))
  expect_identical(t$p, c("0.000", NA, "0.123", NA))        # the block's first row
  expect_identical(t[1:4], vc_tab(vc_cp()))                 # the body untouched
  t2 <- vc_tab(vc_cp() |> plan_compare(p = c(p.value < 0.001 ~ "<0.001", "{p.value:.3f}"), diff = "{estimate:.2f}"))
  expect_identical(names(t2)[5:6], c("p", "diff"))
  expect_identical(t2$p[c(1L, 3L)], c("<0.001", "0.123"))
  expect_identical(t2$diff[c(1L, 3L)], c("-1.86", "-1.50"))
  # keyed by context: that test, or none
  expect_identical(vc_tab(vc_cp() |> plan_compare(p = c(stats_t_test = "{p.value:.3f}")))$p, t$p)
  expect_true(all(is.na(vc_tab(vc_cp() |> plan_compare(p = c(stats_wilcox_test = "{p.value:.3f}")))$p)))
  # cell_rows(): the row of that label
  t3 <- vc_tab(vc_cp() |> plan_compare(p = cell_rows("Mean (SD)" = "{p.value:.3f}")))
  expect_identical(t3$p, c(NA, "0.000", NA, "0.123"))
  # position = "first": after the row keys and label
  expect_identical(names(vc_tab(vc_cp() |> plan_compare(p = "{p.value:.3f}", position = "first"))),
                   c("param", "row", "p", "A", "B"))
  # twice: the last call only
  expect_identical(names(vc_tab(vc_cp() |> plan_compare(p = "{p.value:.3f}") |> plan_compare(diff = "{estimate:.2f}"))),
                   c("param", "row", "A", "B", "diff"))
})

test_that("FT-PL162 [FR-PL293 FR-PL294]: plan_compare(by = ) splits the comparisons; the header leaves the columns to the author", {
  d <- vc_cmp()
  d$CMP <- NA_character_
  d$CMP[d$context == "stats_t_test"] <- rep(c("A vs B", "A vs C"), 2)
  t <- vc_tab(vc_cp(d) |> plan_compare(p = "{p.value:.3f}", by = "CMP"))
  expect_identical(names(t), c("param", "row", "A", "B", "A vs B", "A vs C"))
  expect_identical(t[["A vs B"]], c("0.000", NA, NA, NA))
  expect_identical(t[["A vs C"]], c(NA, NA, "0.123", NA))
  t2 <- vc_tab(vc_cp(d) |> plan_compare(p = "{p.value:.3f}", e = "{estimate:.1f}", by = "CMP"))
  expect_identical(names(t2)[5:8], c("A vs B____p", "A vs B____e", "A vs C____p", "A vs C____e"))
  # the header: {col} over the value columns, p named by the author
  pg <- plan_apply(vc_cp() |> plan_compare(p = "{p.value:.3f}") |>
                     plan_col_header(header = rtf_col_header(c("", "", "{col}"))))
  expect_identical(pg[[1L]]$col_header[[1L]], c("", "", "A", "B", "p"))
  expect_identical(plan_header_tokens(vc_cp() |> plan_compare(p = "{p.value:.3f}"))$values[[1L]], c("A", "B"))
})

test_that("FT-PL163 [FR-PL298 FR-PL299 FR-PL300 FR-PL301 FR-PL303]: plan_compare() says what is wrong", {
  msg <- "plan_compare(): every column has a name of its own: `plan_compare(p = \"{p.value:.3f}\")`."
  expect_error(plan_compare(vc_cp(), "{p.value}"), msg, fixed = TRUE)
  expect_error(plan_compare(vc_cp()), msg, fixed = TRUE)
  expect_error(plan_compare(vc_cp(), p = "x", p = "y"), msg, fixed = TRUE)
  expect_error(plan_compare(vc_cp(), p = "x", position = "mid"), "'arg' should be one of \"last\", \"first\"", fixed = TRUE)
  expect_error(plan_compare(vc_cp(), p = "x", by = 1),
               "plan_compare(by = ) is one column, the key that splits the comparisons: \"COMPARISON\".", fixed = TRUE)
  expect_error(plan_apply(vc_cp() |> plan_compare(A = "{p.value}"), "table"),
               "plan_compare(): 'A' is a column of the table already.", fixed = TRUE)
  expect_error(plan_apply(vc_cp() |> plan_compare(p = "{p.value}", by = "NOPE"), "table"),
               "plan_compare(by = \"NOPE\"): no such column in the comparison records.", fixed = TRUE)
  expect_error(plan_compare(vc_long(), p = "x"), "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})


# ---- plan_sort() -----------------------------------------------------------

test_that("FT-PL164 [FR-PL304 FR-PL305 FR-PL306 FR-PL308 FR-PL309]: the sort keys on an ARD plan", {
  p <- vc_cplan()
  expect_identical(plan_apply(p |> plan_sort("param", "-n"), "args")$widen$sort, c("param", "-n"))
  expect_null(rtfreporter:::.plan_rtf_args(p |> plan_sort("param", "-n"))$sort_by)
  # a key: a row key or the label, ascending or descending
  expect_identical(vc_rows(vc_tab(p |> plan_sort("-param"))), c("AST W1", "AST W2", "ALT W1", "ALT W2"))
  expect_identical(vc_rows(vc_tab(p |> plan_sort("-visit"))), c("ALT W2", "AST W2", "ALT W1", "AST W1"))
  # a statistic: its total (14, 11, 11, 5), one column's, the largest, the smallest
  expect_identical(vc_rows(vc_tab(p |> plan_sort("-n"))), c("ALT W2", "ALT W1", "AST W1", "AST W2"))
  expect_identical(vc_rows(vc_tab(p |> plan_sort("n"))), c("AST W2", "ALT W1", "AST W1", "ALT W2"))
  expect_identical(vc_rows(vc_tab(p |> plan_sort("-sum(n)"))), vc_rows(vc_tab(p |> plan_sort("-n"))))
  expect_identical(vc_rows(vc_tab(p |> plan_sort("-n[B]"))), c("AST W1", "ALT W1", "ALT W2", "AST W2"))
  expect_identical(vc_rows(vc_tab(p |> plan_sort("-max(n)"))), c("ALT W2", "AST W1", "ALT W1", "AST W2"))
  expect_identical(vc_rows(vc_tab(p |> plan_sort("min(n)"))), c("AST W2", "ALT W2", "AST W1", "ALT W1"))
  # several keys, as arguments or one vector: ties to the next key
  expect_identical(vc_rows(vc_tab(p |> plan_sort("param", "-n"))), c("ALT W2", "ALT W1", "AST W1", "AST W2"))
  expect_identical(vc_tab(p |> plan_sort(c("param", "-n"))), vc_tab(p |> plan_sort("param", "-n")))
  # levels(<key>): the values plan_levels() names first
  expect_identical(vc_rows(vc_tab(p |> plan_levels(PARAM = "AST") |> plan_sort("levels(param)"))),
                   c("AST W1", "AST W2", "ALT W1", "ALT W2"))
})

test_that("FT-PL165 [FR-PL310 FR-PL311 FR-PL312 FR-PL313]: stat = adds .sort_stat; keep = FALSE hides a key; TRUE / FALSE; last wins", {
  p <- vc_cplan()
  a <- plan_apply(p |> plan_sort("-n", stat = "n"), "args")$widen
  expect_identical(a$sort_stat, "n")
  t <- vc_tab(p |> plan_sort("-n", stat = "n"))
  expect_identical(t$.sort_stat, c(14, 11, 11, 5))
  # keep = FALSE: a column goes, a statistic is ignored
  pg <- plan_apply(p |> plan_sort("param", keep = FALSE) |> plan_columns())
  expect_identical(names(pg[[1L]]$data), c("visit", "A", "B"))
  pg2 <- plan_apply(p |> plan_sort("-n", keep = FALSE) |> plan_columns())
  expect_identical(names(pg2[[1L]]$data), c("param", "visit", "A", "B"))
  # FALSE: the data's order; TRUE: a plain key's values clustered first seen first
  d <- vc_counts()[c(2, 1, 4, 3, 6, 5, 8, 7), ]
  expect_identical(vc_rows(vc_tab(vc_cplan(d) |> plan_sort(FALSE))), c("ALT W2", "ALT W1", "AST W2", "AST W1"))
  expect_identical(plan_apply(vc_cplan(d) |> plan_sort(TRUE), "args")$widen$sort, TRUE)
  expect_identical(vc_rows(vc_tab(vc_cplan(d) |> plan_sort(TRUE))), c("ALT W2", "ALT W1", "AST W2", "AST W1"))
  # twice: the last keys
  expect_identical(vc_tab(p |> plan_sort("-n") |> plan_sort("n")), vc_tab(p |> plan_sort("n")))
})

test_that("FT-PL166 [FR-PL304 FR-PL316]: plan_sort() on a finished table is sort_by / sort_desc", {
  w <- data.frame(g = c("b", "a", "c"), v = c(2, 3, 1), stringsAsFactors = FALSE)
  a <- plan_apply(table_plan(w) |> plan_sort("-v"), "args")$rtf
  expect_identical(a$sort_by, "v")
  expect_true(a$sort_desc)
  expect_identical(plan_apply(table_plan(w) |> plan_sort("g") |> plan_columns())[[1L]]$data$g, c("a", "b", "c"))
  expect_identical(plan_apply(table_plan(w) |> plan_sort("-v") |> plan_columns())[[1L]]$data$g, c("a", "b", "c"))
  expect_identical(plan_apply(table_plan(w) |> plan_sort("v") |> plan_columns())[[1L]]$data$g, c("c", "b", "a"))
  expect_error(plan_apply(table_plan(w) |> plan_sort("zz") |> plan_columns()),
               "`sort_by` column 'zz' not found in the table.", fixed = TRUE)
})

test_that("FT-PL167 [FR-PL314 FR-PL315 FR-PL317]: plan_sort() says what key it cannot read", {
  p <- vc_cplan()
  expect_error(plan_apply(p |> plan_sort("nope"), "table"),
               "`sort`: 'nope' is neither a column of the result nor a statistic of this ARD.\n  Columns: 'param', 'visit', 'A', 'B'",
               fixed = TRUE)
  expect_error(plan_apply(p |> plan_sort("-n[Z]"), "table"), "`sort`: '-n[Z]' -- no spread column 'Z'.", fixed = TRUE)
  expect_error(plan_apply(p |> plan_sort("-n@zz"), "table"), "`sort`: '-n@zz' -- 'zz' is not a row key.", fixed = TRUE)
  expect_error(plan_apply(p |> plan_sort("-zz@param"), "table"),
               "`sort`: '-zz@param' -- 'zz' is not a statistic of this ARD.", fixed = TRUE)
  expect_error(plan_apply(p |> plan_sort("levels(zz)"), "table"), "`sort`: 'levels(zz)' -- no row key 'zz'.", fixed = TRUE)
  expect_error(plan_sort(vc_counts(), "n"), "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})


# ---- plan_nest() -----------------------------------------------------------

test_that("FT-PL168 [FR-PL318 FR-PL319 FR-PL320 FR-PL321 FR-PL322]: a variable's rows under one level of another", {
  plain <- vc_tab(vc_np())
  t <- vc_tab(vc_np() |> plan_nest(RSUB = c(RACE = "Black")))
  expect_identical(as.character(t$group), rep("RACE", 4L))
  expect_identical(as.character(t$label), c("White", "Black", paste0(vc_nbsp(4L), c("Sub A", "Sub B"))))
  expect_identical(t[3:4, c("A", "B")], plain[3:4, c("A", "B")], ignore_attr = TRUE)
  expect_identical(as.character(vc_tab(vc_np() |> plan_stub(indent = 2) |> plan_nest(RSUB = c(RACE = "Black")))$label)[3L],
                   paste0(vc_nbsp(2L), "Sub A"))
  expect_identical(as.character(vc_tab(vc_np() |> plan_nest(RSUB = c(RACE = "White")))$label),
                   c("White", paste0(vc_nbsp(4L), c("Sub A", "Sub B")), "Black"))
  # the variable by its heading, as plan_labels() gives it
  lab <- vc_tab(vc_np() |> plan_labels(variable = c(RACE = "Race", RSUB = "Sub")) |> plan_nest(RSUB = c(RACE = "Black")))
  expect_identical(as.character(lab$group), rep("Race", 4L))
  expect_identical(as.character(lab$label)[3L], paste0(vc_nbsp(4L), "Sub A"))
  # the nested rows keep plan_levels()' order and plan_labels()' text
  ord <- vc_tab(vc_np() |> plan_labels(RSUB = c("Sub A" = "Alpha")) |> plan_levels(RSUB = c("Sub B", "Sub A")) |>
                  plan_nest(RSUB = c(RACE = "Black")))
  expect_identical(as.character(ord$label)[3:4], paste0(vc_nbsp(4L), c("Sub B", "Alpha")))
  # a later nesting for the variable wins
  expect_identical(vc_tab(vc_np() |> plan_nest(RSUB = c(RACE = "White")) |> plan_nest(RSUB = c(RACE = "Black"))), t)
  expect_identical(plan_layers(vc_np() |> plan_nest(RSUB = c(RACE = "White")) |> plan_nest(RSUB = c(RACE = "Black")))$layers$nest,
                   list(nest = list(RSUB = list(parent = "RACE", level = "Black"))))
  # a factor key stays one
  nf <- vc_nest()
  nf$variable <- factor(nf$variable)
  tf <- vc_tab(vc_np(nf) |> plan_nest(RSUB = c(RACE = "Black")))
  expect_s3_class(tf$group, "factor")
  expect_identical(levels(tf$group), "RACE")
})

test_that("FT-PL169 [FR-PL323 FR-PL324 FR-PL326]: plan_nest() says what is wrong", {
  p <- vc_np()
  expect_error(plan_nest(p), "plan_nest(): name the nested variable, e.g. plan_nest(RACESUB = c(RACE = \"Asian\")).", fixed = TRUE)
  expect_error(plan_nest(p, "x"), "name the nested variable", fixed = TRUE)
  expect_error(plan_nest(p, RSUB = "Black"),
               "plan_nest(RSUB = ): one level of one variable, named by it: c(RACE = \"Asian\").", fixed = TRUE)
  expect_error(plan_nest(p, RSUB = c(RACE = "Black", RACE = "White")), "one level of one variable", fixed = TRUE)
  expect_error(plan_nest(p, RSUB = c(RACE = 1)), "one level of one variable", fixed = TRUE)
  expect_error(plan_apply(p |> plan_nest(RSUB = c(RACE = "Blue")), "table"),
               "plan_nest(RSUB = c(RACE = \"Blue\")): RACE has no row \"Blue\".  Its rows: White, Black.", fixed = TRUE)
  expect_error(plan_apply(p |> plan_nest(NOPE = c(RACE = "Black")), "table"),
               "plan_nest(NOPE = ): the table has no rows of NOPE.  Its group: RACE, RSUB.", fixed = TRUE)
  expect_error(plan_nest(vc_nest(), RSUB = c(RACE = "Black")), "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})


# ---- plan_empty() ----------------------------------------------------------

test_that("FT-PL170 [FR-PL327 FR-PL328 FR-PL329 FR-PL331]: an empty table says so, when and where asked", {
  none <- vc_ep(vc_empty(0, body = FALSE))
  t <- vc_tab(none |> plan_empty())
  expect_identical(t$label, c("Any TEAE", "No subjects met the criteria."))
  expect_true(is.na(t$soc[2L]))
  expect_identical(unlist(t[2L, c("A", "B")], use.names = FALSE), c("", ""))
  expect_identical(unlist(t[1L, c("A", "B")], use.names = FALSE), c("0", "0"))
  expect_identical(vc_tab(none |> plan_empty(position = "first"))$label[1L], "No subjects met the criteria.")
  expect_identical(vc_tab(none |> plan_empty(text = "None.", position = "replace"))$label, "None.")
  # when: no rows (whatever the count), the overall row 0 (rows or not), either
  some <- vc_ep(vc_empty(5, body = FALSE))
  expect_identical(nrow(vc_tab(some |> plan_empty())), 2L)
  expect_identical(nrow(vc_tab(some |> plan_empty(when = "no_rows"))), 2L)
  expect_identical(nrow(vc_tab(some |> plan_empty(when = "overall_zero"))), 1L)
  zero <- vc_ep(vc_empty(0, body = TRUE))
  expect_identical(nrow(vc_tab(zero |> plan_empty())), 4L)
  expect_identical(nrow(vc_tab(zero |> plan_empty(when = "overall_zero"))), 4L)
  expect_identical(nrow(vc_tab(zero |> plan_empty(when = "no_rows"))), 3L)
  # a table with events is as it was
  events <- vc_ep(vc_empty(5, body = TRUE))
  expect_identical(vc_tab(events |> plan_empty()), vc_tab(events))
  # twice: the last call, its defaults included
  expect_identical(vc_tab(none |> plan_empty(text = "a") |> plan_empty(position = "first"))$label[1L],
                   "No subjects met the criteria.")
})

test_that("FT-PL171 [FR-PL330 FR-PL332]: text = NULL, the footnote, a filtered-out table, label = NA", {
  none <- vc_ep(vc_empty(0, body = FALSE))
  e <- vc_page1(none |> plan_empty(footnote = "No events."))
  expect_identical(unlist(attr(e, "rtf_footnotes")), "No events.")
  expect_identical(nrow(e$data), 2L)
  e2 <- vc_page1(none |> plan_empty(text = NULL, footnote = "No events."))
  expect_identical(unlist(attr(e2, "rtf_footnotes")), "No events.")
  expect_identical(nrow(e2$data), 1L)
  expect_identical(unlist(attr(vc_page1(none |> plan_footnotes("F1") |> plan_empty(footnote = "No events.")), "rtf_footnotes")),
                   c("F1", "No events."))
  expect_s3_class(plan_apply(none |> plan_empty()), "data.frame")
  expect_s3_class(plan_apply(none |> plan_empty(footnote = "x"))[[1L]], "rtftable")
  events <- vc_ep(vc_empty(5, body = TRUE))
  expect_null(unlist(attr(vc_page1(events |> plan_empty(footnote = "No events.")), "rtf_footnotes")))
  # every body row filtered out: the overall row, then the sentence
  t <- vc_tab(events |> plan_filter(n > 100) |> plan_empty())
  expect_identical(t$label, c("Any TEAE", "No subjects met the criteria."))
  # no label column: the sentence in the first column
  t2 <- vc_tab(vc_ep(vc_empty(0, body = FALSE), label = NA) |> plan_empty())
  expect_identical(names(t2), c("soc", "A", "B"))
  expect_identical(t2$soc[2L], "No subjects met the criteria.")
})

test_that("FT-PL172 [FR-PL333 FR-PL334]: plan_empty() says what is wrong", {
  p <- vc_ep(vc_empty())
  expect_error(plan_empty(p, text = 1), "plan_empty(text = ) is one string, or NULL for no row.", fixed = TRUE)
  expect_error(plan_empty(p, text = c("a", "b")), "is one string, or NULL for no row.", fixed = TRUE)
  expect_error(plan_empty(p, footnote = 1), "plan_empty(footnote = ) is the footnote's text, or NULL.", fixed = TRUE)
  expect_error(plan_empty(p, when = "x"), "'arg' should be one of \"auto\", \"no_rows\", \"overall_zero\"", fixed = TRUE)
  expect_error(plan_empty(p, position = "x"), "'arg' should be one of \"last\", \"first\", \"replace\"", fixed = TRUE)
  expect_error(plan_empty(vc_empty()), "Expected a table_plan; pipe from table_plan(ard).", fixed = TRUE)
})

test_that("plan_empty() says a table its filter emptied, with no overall row in the ARD (#750)", {
  ae <- data.frame(ARM = c("Placebo", "Active"), .label = "Headache",
                   .kind = "categorical", stat_name = "n", stat = c(1, 2))
  p <- table_plan(ae, cols = "ARM") |> plan_cells(categorical = "{n}") |>
    plan_filter(n >= 5)
  # without plan_empty(): the error as before
  expect_error(plan_apply(p, "table"), "every row was filtered out", fixed = TRUE)
  # with it: the table's columns, the sentence its one row
  tbl <- plan_apply(plan_empty(p), "table")
  expect_identical(nrow(tbl), 1L)
  expect_true(all(c("Placebo", "Active") %in% names(tbl)))
  expect_identical(tbl[[1L]], "No subjects met the criteria.")
})

test_that(".values on a finished table leaves the row-heading column out (#755)", {
  tbl <- data.frame(Parameter = c("A", "B"), Placebo = c("1.5", "2"), Active = c("3", "4.25"))
  pg <- table_plan(tbl) |> plan_columns(decimal = ".values", row_title = "Parameter") |> plan_apply()
  expect_identical(as.integer(pg[[1L]]$decimal_split$cols), 2:3)
  # no row_title: the first column is the row heading
  pg <- table_plan(tbl) |> plan_cell_style(cols = ".values", bold = TRUE) |> plan_apply()
  expect_identical(vapply(pg[[1L]]$col_spec, function(s) isTRUE(s$bold), logical(1L)),
                   c(FALSE, TRUE, TRUE))
  # two row-heading columns, by position
  t2 <- data.frame(Grp = "G", Parameter = "A", Placebo = "1.5")
  pg <- table_plan(t2) |> plan_columns(decimal = ".values", row_title = 1:2) |> plan_apply()
  expect_identical(as.integer(pg[[1L]]$decimal_split$cols), 3L)
})

test_that("plan_compare() says when comparison records are not ARD rows (#757)", {
  d <- vc_cmp()
  # a row of a plain data frame bound into the ARD: no variable, no statistic
  odd <- d[1L, ]
  odd[1L, ] <- NA
  odd$PARAM <- "ALT"
  x <- rbind(d, odd)
  expect_error(vc_cp(x) |> plan_compare(p = "{p.value:.3f}") |> plan_apply("table"),
               "1 of the comparison records have no `variable` or no `stat_name`", fixed = TRUE)
  # the ARD's own comparison records: placed as before
  tbl <- vc_cp() |> plan_compare(p = "{p.value:.3f}") |> plan_apply("table")
  expect_true("p" %in% names(tbl))
})
