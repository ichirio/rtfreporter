# The type rule of a cell template chain (#1129): each element of a chain is
# one text or one guard.  A list, or a value whose length is not 1, stops
# with a message that names the element, where widen_ard(cells = ) builds the
# chain and where plan_cells() checks its rule -- not later as R's own text
# ("list(...)") in a cell.  Fictional data only.

.ctr_st <- function() {
  data.frame(ARM = rep(c("Placebo", "Active"), each = 2), variable = "SEX",
             .label = "F", .kind = "categorical", stat_name = c("n", "p"),
             stat = c(3, 0.3, 0, 0))
}

.ctr_hint <- paste0("A cell template is one text or a guard, not a list ",
                    "inside the chain. A chain of them is one list or c(): ",
                    "c(n == 0 ~ \"0\", \"{n} ({p:.1f%})\").")

test_that("FT-AR8005 [FR-AR263]: widen_ard(cells = ) stops on a list or a non-length-1 element in a chain", {
  st <- .ctr_st()
  w <- function(cells) widen_ard(st, cols = "ARM", cells = cells, notes = FALSE)
  # a list as an element of the chain, after a guard and alone
  expect_error(w(list(n == 0 ~ "0", list("{n}"))),
               "Element 2 of a cell template chain is a list.", fixed = TRUE)
  expect_error(w(list(n == 0 ~ "0", list("{n}"))), .ctr_hint, fixed = TRUE)
  expect_error(w(list(list("{n}", "{p}"))),
               "Element 1 of a cell template chain is a list.", fixed = TRUE)
  # a vector of two as one element
  expect_error(w(list("{n}", c("{n}", "{p}"))),
               "Element 2 of a cell template chain is a value of length 2.",
               fixed = TRUE)
  # the same inside a named recipe and a cell_rows()
  expect_error(w(list(categorical = list("{n}", list("{p}")))),
               "Element 2 of a cell template chain is a list.", fixed = TRUE)
  expect_error(w(cell_rows(n = list("{n}", c("a", "b")))),
               "Element 2 of a cell template chain is a value of length 2.",
               fixed = TRUE)
  # a text and a guard pass
  ok <- w(c(n == 0 ~ "0", "{n} ({p:.1f%})"))
  expect_identical(ok$Placebo, "3 (30.0)")
  expect_identical(ok$Active, "0")
  expect_identical(w("{n}")$Placebo, "3")
})

test_that("FT-PL6810 [FR-PL999]: plan_cells() names a list or a vector inside a chain and says what a chain is", {
  p <- table_plan(.ctr_st(), cols = "ARM")
  cr <- "plan_cells(categorical = ) is a template, a named vector of templates, guards or cell_rows(), not "
  expect_error(plan_cells(p, categorical = list(n == 0 ~ "0", list("{n}"))),
               paste0(cr, "a list with a list in it."), fixed = TRUE)
  expect_error(plan_cells(p, categorical = list(n == 0 ~ "0", list("{n}"))),
               .ctr_hint, fixed = TRUE)
  expect_error(plan_cells(p, categorical = list("{n}", c("{n}", "{p}"))),
               paste0(cr, "a list with a vector of templates in it."),
               fixed = TRUE)
  expect_error(plan_cells(p, categorical = list("{n}", c("{n}", "{p}"))),
               .ctr_hint, fixed = TRUE)
  # a number in a chain keeps its message, without the chain hint
  e <- tryCatch(plan_cells(p, categorical = list("{n}", 1)),
                error = conditionMessage)
  expect_match(e, "a list with a number in it.", fixed = TRUE)
  expect_false(grepl("not a list inside the chain", e, fixed = TRUE))
  # a text and a guard pass
  expect_s3_class(plan_cells(p, categorical = c(n == 0 ~ "0", "{n} ({p:.1f%})")),
                  "table_plan")
  expect_s3_class(plan_cells(p, categorical = "{n}"), "table_plan")
})
