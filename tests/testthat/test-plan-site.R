# The call site a plan layer records (#1147): found by the verb's FUNCTION,
# so a layer built through do.call() is blamed on its own verb, and the
# same statement is listed once.

site_wide <- function() {
  data.frame(
    label = c("Group A", "  a1", "  a2"),
    v = as.character(1:3), stringsAsFactors = FALSE
  )
}

site_msg <- function(expr) {
  tryCatch({ expr; "" }, error = function(e) conditionMessage(e))
}

# the lines under "declared by:"
site_lines <- function(msg) {
  x <- strsplit(msg, "declared by:\n", fixed = TRUE)[[1L]][2L]
  trimws(strsplit(x, "\n", fixed = TRUE)[[1L]])
}

test_that("FT-PL1147-1 [FR-PL1004]: (#1147) a do.call() layer is blamed on its own verb, not an outer plan_cells()", {
  p0 <- table_plan(site_wide())
  # The outer verb has its first argument as a promise, evaluated inside
  # it, so its frame is still on the stack when the do.call() layers are
  # built -- as tflplanner's preview does with plan_cells(notes = FALSE).
  # The old code took that call for the site of every layer.  (plan_titles()
  # here: plan_cells() needs an ARD, and the mechanism is the same.)
  p <- plan_titles(
    do.call(plan_style, list(
      do.call(plan_style, list(p0, border = "rtf_border(top = TRUE)")),
      border = "rtf_border(bottom = TRUE)")),
    "Table 1")
  msg <- site_msg(plan_apply(p))
  expect_match(msg, "declared by:", fixed = TRUE)
  ln <- site_lines(msg)
  expect_length(ln, 2L)
  expect_true(all(startsWith(ln, "plan_style(")))
  expect_false(grepl("plan_titles", msg, fixed = TRUE))
})

test_that("FT-PL1147-2 [FR-PL1004]: (#1147) the site of a layer made by name or as rtfreporter::verb() reads plan_style(...)", {
  p0 <- table_plan(site_wide())
  p1 <- p0 |> plan_style(border = "nope")
  expect_identical(site_lines(site_msg(plan_apply(p1))),
                   "plan_style(border = \"nope\")")
  # the plan, its first argument, is not shown whether piped or not
  p2 <- rtfreporter::plan_style(p0, border = "nope")
  expect_identical(site_lines(site_msg(plan_apply(p2))),
                   "plan_style(border = \"nope\")")
})

test_that("FT-PL1147-3 [FR-PL1004]: (#1147) the same statement from several layers is listed once with its count", {
  p0 <- table_plan(site_wide())
  p <- p0 |>
    plan_style(border = "nope") |>
    plan_style(border = "other") |>
    plan_style(border = "nope")
  ln <- site_lines(site_msg(plan_apply(p)))
  expect_identical(ln, c("plan_style(border = \"nope\") (2 times)",
                         "plan_style(border = \"other\")"))
  # a do.call() pair of identical layers: one line, "(2 times)"
  q <- do.call(plan_style, list(do.call(plan_style, list(p0, border = "nope")),
                                border = "nope"))
  ln2 <- site_lines(site_msg(plan_apply(q)))
  expect_length(ln2, 1L)
  expect_match(ln2, " (2 times)", fixed = TRUE)
})
