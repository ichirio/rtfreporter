# Coverage of the edge paths of R/plan_append.R: a part whose own run stops
# (the error names the part), a part with no row key or label to fold, and
# the page groups ordered by the parent's levels of the page key.

cov_pa_rec <- function() {
  data.frame(ID = rep(c("A", "B"), each = 3), GRP = rep(c("x", "y"), each = 3),
             T = rep(c("t1", "t2", "t3"), 2), V = c(1.234, 2.5, NA, 3, 4.25, 5),
             stringsAsFactors = FALSE)
}
cov_pa_long <- function() {
  data.frame(GRP = rep(c("x", "y"), each = 3), T = rep(c("t1", "t2", "t3"), 2),
             variable = "V", stat_name = "mean", stat = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5),
             .label = NA_character_, stringsAsFactors = FALSE)
}
cov_pa_parent <- function() {
  table_plan(cov_pa_rec(), cols = "T", rows = "ID", value = "V") |>
    plan_cells("{value:.1f}") |>
    plan_paginate_rows(page_by = "GRP")
}
cov_pa_child <- function() {
  table_plan(cov_pa_long(), cols = "T", rows = c(block = ~ "Summary")) |>
    plan_cells(V = c(Mean = "{mean:.1f}"))
}

test_that("plan_append() names the part whose own run stops", {
  skip_on_cran()
  p <- cov_pa_parent() |> plan_append(cov_pa_child() |> plan_sort("zzz"), name = "stats")
  expect_error(plan_apply(p, "table"),
               "plan_append(\"stats\"): `sort`: 'zzz' is neither a column", fixed = TRUE)
})

test_that("plan_append() stacks a part with no row key under its empty label, page by page", {
  skip_on_cran()
  child <- table_plan(cov_pa_long(), cols = "T") |> plan_cells("{mean:.1f}")
  t <- as.data.frame(plan_apply(cov_pa_parent() |> plan_append(child), "table"))
  expect_identical(t$GRP, c("x", "x", "y", "y"))
  expect_identical(t$label, c("A", "", "B", ""))
  expect_identical(t$.part, c("part1", "part2", "part1", "part2"))
  expect_identical(t$t1, c("1.2", "1.5", "3.0", "4.5"))
})

test_that("plan_append() gives a part with its label hidden an empty label column", {
  skip_on_cran()
  child <- table_plan(cov_pa_long()[cov_pa_long()$GRP == "x", ], cols = "T") |>
    plan_cells("{mean:.1f}") |>
    plan_hide("label")
  parent <- table_plan(cov_pa_rec(), cols = "T", rows = "ID", value = "V") |>
    plan_cells("{value:.1f}")
  t <- as.data.frame(plan_apply(parent |> plan_append(child), "table"))
  expect_identical(names(t), c("label", "t1", "t2", "t3", ".part"))
  expect_identical(t$label, c("A", "B", ""))
  expect_identical(unlist(t[3L, c("t1", "t2", "t3")], use.names = FALSE), c("1.5", "2.5", "3.5"))
})

test_that("plan_append() orders the pages by the parent's named levels of the page key", {
  skip_on_cran()
  p <- cov_pa_parent() |>
    plan_levels(GRP = c(y = "Group Y", x = "Group X")) |>
    plan_append(cov_pa_child())
  t <- as.data.frame(plan_apply(p, "table"))
  expect_identical(as.character(t$GRP), rep(c("Group Y", "Group X"), each = 3))
  expect_identical(t$label[c(1, 4)], c("B", "A"))
  expect_identical(t$.part, rep(c("part1", "part2", "part2"), 2))
})

test_that("plan_append() orders the pages by the parent's unnamed levels of the page key", {
  skip_on_cran()
  p <- cov_pa_parent() |> plan_levels(GRP = c("y", "x")) |> plan_append(cov_pa_child())
  t <- as.data.frame(plan_apply(p, "table"))
  expect_identical(as.character(t$GRP), rep(c("y", "x"), each = 3))
  pg <- plan_apply(p, "pages")
  expect_length(pg, 2L)
  expect_identical(pg[[1L]]$data$label[1L], "B")
})
