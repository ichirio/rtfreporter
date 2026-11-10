## tests/testthat/test-plan-cell-border.R
##
## Body borders from a plan (look-params GAPS.md G1, section 4 Q1 and Q2):
##   plan_cell_style(where = , border = ) and border = ~ <formula> build
##   cell_styles[[r]]$border, the path style_body(border = ) takes; on a
##   page's last row the table's border_last_row wins side by side.
##   plan_cell_style(cols = , border = ) keeps its meaning (the header cells)
##   and says so once a session.

library(testthat)

.cb_df <- function() {
  data.frame(Parameter = c("Subjects", "Age, mean (SD)", "Female, n (%)", "Male, n (%)"),
             Placebo = c("86", "75.2 (8.59)", "53 (61.6)", "33 (38.4)"),
             Active = c("84", "75.7 (8.29)", "50 (59.5)", "34 (40.5)"),
             stringsAsFactors = FALSE)
}

.cb_pages <- function(p) suppressMessages(plan_apply(p, "pages"))

.cb_direct <- function(...) {
  out <- suppressMessages(as_rtftables(.cb_df(), read_meta = FALSE, ...))
  if (inherits(out, "rtftable")) list(out) else out
}

.cb_rtf <- function(pages) {
  dir <- tempfile("cb-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  f <- file.path(dir, "t.rtf")
  generate_rtfreport(rtf_document() |> rtf_tables(pages), f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

.cb_rule <- rtf_border(bottom = TRUE)

test_that("plan_cell_style(where = , border = ) is style_body(rows = , border = ) on the pages", {
  got <- .cb_pages(table_plan(.cb_df()) |>
                     plan_cell_style(where = ~ Parameter == "Subjects", border = .cb_rule))
  want <- .cb_direct() |> style_body(rows = ~ Parameter == "Subjects", border = .cb_rule)
  expect_equal(got, want)
  expect_identical(.cb_rtf(got), .cb_rtf(want))
  # over `cols` only
  got2 <- .cb_pages(table_plan(.cb_df()) |>
                      plan_cell_style(cols = "Active", where = ~ Parameter == "Subjects",
                                      border = .cb_rule))
  want2 <- .cb_direct() |>
    style_body(rows = ~ Parameter == "Subjects", cols = "Active", border = .cb_rule)
  expect_equal(got2, want2)
  expect_null(got2[[1L]]$cell_styles[[1L]]$border[[2L]])
  # where = ~ TRUE: every body row of the column, not its header cell
  got3 <- .cb_pages(table_plan(.cb_df()) |>
                      plan_cell_style(cols = "Parameter", where = ~ TRUE,
                                      border = rtf_border(right = TRUE)))
  expect_equal(got3, .cb_direct() |>
                 style_body(cols = "Parameter", border = rtf_border(right = TRUE)))
  expect_null(got3[[1L]]$col_spec[[1L]]$border)
  # with other attributes in the same call
  got4 <- .cb_pages(table_plan(.cb_df()) |>
                      plan_cell_style(where = ~ Parameter == "Subjects", bold = TRUE,
                                      border = .cb_rule))
  expect_equal(got4, .cb_direct() |>
                 style_body(rows = ~ Parameter == "Subjects", bold = TRUE,
                            border = .cb_rule))
})

test_that("border = ~ <formula> gives one rtf_border (or NULL) a row", {
  f <- ~ ifelse(Parameter == "Age, mean (SD)", list(rtf_border(bottom = TRUE)), list(NULL))
  got <- .cb_pages(table_plan(.cb_df()) |> plan_cell_style(border = f))
  want <- .cb_direct() |> style_body(rows = ~ Parameter == "Age, mean (SD)", border = .cb_rule)
  expect_equal(got, want)
  # a single rtf_border is every row's
  one <- .cb_pages(table_plan(.cb_df()) |>
                     plan_cell_style(border = ~ rtf_border(left = TRUE)))
  expect_equal(one, .cb_direct() |> style_body(border = rtf_border(left = TRUE)))
  # named by column
  by_col <- .cb_pages(table_plan(.cb_df()) |>
                        plan_cell_style(border = list(Placebo = f)))
  expect_equal(by_col, .cb_direct() |>
                 style_body(rows = ~ Parameter == "Age, mean (SD)", cols = "Placebo",
                            border = .cb_rule))
  # NA is no border of its own, as NULL is
  na <- .cb_pages(table_plan(.cb_df()) |>
                    plan_cell_style(border = ~ ifelse(Parameter == "Subjects",
                                                      list(rtf_border(bottom = TRUE)), NA)))
  expect_equal(na, .cb_direct() |> style_body(rows = ~ Parameter == "Subjects", border = .cb_rule))
  # what is not a border stops, naming the row
  expect_error(.cb_pages(table_plan(.cb_df()) |>
                           plan_cell_style(border = ~ as.list(Parameter))),
               "plan_cell_style(border): row 1 gives character, not an rtf_border()",
               fixed = TRUE)
  expect_error(.cb_pages(table_plan(.cb_df()) |>
                           plan_cell_style(border = ~ list(.cb_rule, .cb_rule))),
               "plan_cell_style(border): the formula gives an rtf_border() or a list of them",
               fixed = TRUE)
})

test_that("on a page's last row, border_last_row wins side by side over a body border", {
  dbl <- rtf_border(bottom = "double")
  side <- rtf_border(bottom = TRUE, left = TRUE)
  got <- .cb_pages(table_plan(.cb_df()) |>
                     plan_style(border_last_row = dbl) |>
                     plan_cell_style(where = ~ TRUE, border = side))
  cs <- got[[1L]]$cell_styles
  last <- cs[[4L]]$border[[1L]]
  expect_identical(last$bottom$style, "double")
  expect_false(is.null(last$left))
  # the other rows keep the body border's own sides
  expect_identical(cs[[3L]]$border[[1L]]$bottom$style, "single")
  # the RTF: the last row's bottom rule is the double one, on every cell
  rows <- unlist(strsplit(.cb_rtf(got), "\\\\trowd"))[-1L]
  lr <- rows[length(rows)]
  expect_identical(lengths(regmatches(lr, gregexpr("\\\\clbrdrb\\\\brdrdb", lr))), 3L)
  # with the preset and a zone (plan_style(border = "tfl", border_last_row = ))
  got2 <- .cb_pages(table_plan(.cb_df()) |>
                      plan_style(border = "tfl", border_last_row = dbl) |>
                      plan_cell_style(where = ~ TRUE, border = side))
  expect_identical(got2[[1L]]$cell_styles[[4L]]$border[[2L]]$bottom$style, "double")
  # no last-row rule: the body border stays on the last row
  got3 <- .cb_pages(table_plan(.cb_df()) |> plan_style(border = "tfl") |>
                      plan_cell_style(where = ~ TRUE, border = side))
  expect_identical(got3[[1L]]$cell_styles[[4L]]$border[[1L]]$bottom$style, "single")
})

test_that("each page's last row takes border_last_row when the rows are paged", {
  dbl <- rtf_border(bottom = "double")
  got <- .cb_pages(table_plan(.cb_df()) |>
                     plan_style(border_last_row = dbl) |>
                     plan_paginate_rows(max_rows = 2) |>
                     plan_cell_style(where = ~ TRUE, border = .cb_rule))
  expect_gt(length(got), 1L)
  for (pg in got) {
    n <- length(pg$cell_styles)
    expect_identical(pg$cell_styles[[n]]$border[[1L]]$bottom$style, "double")
    if (n > 1L) {
      expect_identical(pg$cell_styles[[1L]]$border[[1L]]$bottom$style, "single")
    }
  }
})

test_that("plan_cell_style(cols = , border = ) is still the header cells' border, and says so once", {
  st <- rtfreporter:::.deprecation_state
  key <- "warn:plan_cell_style_cols_border"
  old <- if (exists(key, envir = st, inherits = FALSE)) get(key, envir = st)
  on.exit(if (is.null(old)) {
    if (exists(key, envir = st, inherits = FALSE)) rm(list = key, envir = st)
  } else assign(key, old, envir = st), add = TRUE)
  if (exists(key, envir = st, inherits = FALSE)) rm(list = key, envir = st)
  expect_warning(
    p <- plan_cell_style(table_plan(.cb_df()), cols = "Active", border = .cb_rule),
    "draws the border on the columns' header cells only", fixed = TRUE)
  # once a session
  expect_no_warning(plan_cell_style(table_plan(.cb_df()), cols = "Active", border = .cb_rule))
  # the pages are the ones style_cols(border = ) gives, as before
  got <- .cb_pages(p)
  want <- .cb_direct() |> style_cols(cols = "Active", border = .cb_rule)
  expect_equal(got, want)
  expect_identical(.cb_rtf(got), .cb_rtf(want))
  expect_null(got[[1L]]$cell_styles)
  # the header and a condition do not warn
  if (exists(key, envir = st, inherits = FALSE)) rm(list = key, envir = st)
  expect_no_warning(plan_cell_style(table_plan(.cb_df()), header = TRUE, border = .cb_rule))
  expect_no_warning(plan_cell_style(table_plan(.cb_df()), where = ~ TRUE, border = .cb_rule))
})

test_that("a plan without a body border is written as before", {
  p <- table_plan(.cb_df()) |>
    plan_style(border_last_row = rtf_border(bottom = "double")) |>
    plan_cell_style(where = ~ Parameter == "Subjects", bold = TRUE)
  want <- .cb_direct(style = rtf_table_style(border_last_row = rtf_border(bottom = "double"))) |>
    style_body(rows = ~ Parameter == "Subjects", bold = TRUE)
  expect_identical(.cb_rtf(.cb_pages(p)), .cb_rtf(want))
})
