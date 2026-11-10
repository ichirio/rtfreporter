# A cell template that is not one text -- a list inside a guard chain, or R
# code that reached plan_cells() as a string (a deparsed guard list, an `!r`
# guard read as plain text) -- stops naming the argument, instead of printing
# "list(...)" or `n == 0 ~ "0"` in the cells.  Fictional data only.

.lc_ard <- function() {
  one <- function(trt, k) {
    rbind(
      data.frame(TRT = trt, variable = "AGE", context = "summary", .kind = "continuous",
                 .label = NA, stat_name = c("N", "mean", "sd"), stat = c(4, 61.25, 9.5) + k),
      data.frame(TRT = trt, variable = "SEX", context = "tabulate", .kind = "categorical",
                 .label = rep(c("F", "M"), each = 2), stat_name = c("n", "p", "n", "p"),
                 stat = c(0, 0, 4 + k, 1)))
  }
  rbind(one("A", 0), one("B", 1))
}

.lc_page <- function(p) {
  pg <- suppressMessages(plan_apply(p |> plan_stub(name = "rl"), "pages"))
  if (inherits(pg, "rtftable")) pg else pg[[1L]]
}

.lc_plan <- function(...) {
  table_plan(.lc_ard(), cols = "TRT", rows = c(group = "variable")) |> plan_cells(...)
}

test_that("guards in cells give the cells' values", {
  pg <- .lc_page(.lc_plan(
    continuous = list("Mean (SD)" = list(N < 3 ~ "{mean:.1f} (-)", "{mean:.1f} ({sd:.2f})")),
    categorical = list(n == 0 ~ "0", "{n} ({p:.1f%}%)")))
  txt <- unlist(lapply(pg$data[c("A", "B")], as.character))
  expect_true("61.2 (9.50)" %in% txt)
  expect_true("0" %in% txt)
  expect_false(any(grepl("list(", txt, fixed = TRUE)))
  expect_false(any(grepl("~", txt, fixed = TRUE)))
})

test_that("a guard list written as text stops, naming the argument", {
  dep <- paste0("list(structure(\"n == 0 ~ \\\"0\\\"\", class = \"tfl_r\"), ",
                "\"{n} ({p:.1f%}%)\")")
  expect_error(.lc_page(.lc_plan(categorical = dep)),
               "plan_cells\\(categorical = \\): the cell template .* is R code written as text")
  expect_error(.lc_page(.lc_plan(categorical = 'list(n == 0 ~ "0", "{n} ({p:.1f%}%)")')),
               "R code written as text")
})

test_that("a guard read as plain text stops, naming the argument", {
  expect_error(
    .lc_plan(continuous = list(
      "Mean (SD)" = list('N < 3 ~ "{mean:.1f} (-)"', "{mean:.1f} ({sd:.2f})"))),
    "plan_cells\\(continuous = \\): the cell template 'N < 3 ~ ")
})

test_that("a list inside a guard chain never reaches a cell as list(...)", {
  expect_error(
    .lc_plan(continuous = list(
      "Mean (SD)" = list(list(N < 3 ~ "{mean:.1f} (-)", "{mean:.1f} ({sd:.2f})")))),
    "plan_cells\\(continuous = \\) is a template")
  # (widen_ard(cells = ) does not pass plan_cells(): the chain says it)
  el <- rtfreporter:::.ard_chain_el
  expect_error(el(list(n == 0 ~ "0", "{n}")), "not a list inside the chain")
  expect_error(el(c("{n}", "{N}")), "not 2 values")
  expect_error(el('list(n == 0 ~ "0", "{n}")'), "cells: the cell template .* is R code")
  expect_identical(el("{n} ({p:.1f%}%)")$tpl, "{n} ({p:.1f%}%)")
})

test_that("templates with parentheses, tildes or quotes are still templates", {
  ok <- c("{n} ({p:.1f%}%)", "{mean:.1f} ({sd:.2f})", "c: {n}", "~ {n}",
          "{n} ~ \"x\"", "(n = {n})", "\"{n}\"")
  for (x in ok) expect_false(rtfreporter:::.ard_tpl_is_code(x), info = x)
  code <- c('list("{n}")', 'c(n == 0 ~ "0", "{n}")', 'structure("x", class = "tfl_r")',
            'n == 0 ~ "0"', 'p > 0 & p < 0.0005 ~ "{n} (<0.1%)"')
  for (x in code) expect_true(rtfreporter:::.ard_tpl_is_code(x), info = x)
})
