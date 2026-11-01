# .paginate_df() — split = "by_value" page names

# ──────── split = "by_value" ──────────────────────────────────────────────

test_that("by_value with group_col emits 1 chunk per consecutive value, named by it", {
  df <- data.frame(
    visit = c("Wk1","Wk1","Wk2","Wk2","Wk2","Wk4"),
    val   = c(10, 11, 20, 22, 21, 30)
  )
  res <- .paginate_df(df, split = "by_value", group_col = "visit")
  expect_identical(names(res), c("Wk1", "Wk2", "Wk4"))
  expect_equal(nrow(res[["Wk1"]]), 2L)
  expect_equal(nrow(res[["Wk2"]]), 3L)
  expect_equal(nrow(res[["Wk4"]]), 1L)
})

test_that("by_value with indent-based detection names chunks by the group label", {
  df <- data.frame(
    label = c("Demographics","  Age","  Sex",
              "Vital signs","  Systolic",
              "Lab values","  Hb","  Plt"),
    v = 1:8,
    stringsAsFactors = FALSE
  )
  res <- .paginate_df(df, split = "by_value")     # group_col = NULL (indent)
  expect_identical(names(res),
                   c("Demographics", "Vital signs", "Lab values"))
})

test_that("by_value force-splits groups that exceed max_rows, keeping the name", {
  df <- data.frame(
    visit = rep("Wk1", 8L),
    val   = 1:8
  )
  res <- .paginate_df(df, split = "by_value", group_col = "visit",
                   max_rows = 3L)
  expect_gte(length(res), 2L)
  # every page carries the one heading; the "...n" tail keeps them addressable
  expect_true(all(rtfreporter:::.page_name_base(names(res)) == "Wk1"))
  expect_false(anyDuplicated(names(res)) > 0L)
})

test_that("by_value page_name lands in rtf_paginate_meta as well", {
  df <- data.frame(visit = c("A","A","B"), v = 1:3)
  res <- .paginate_df(df, split = "by_value", group_col = "visit")
  meta_a <- attr(res[["A"]], "rtf_paginate_meta")
  meta_b <- attr(res[["B"]], "rtf_paginate_meta")
  expect_identical(meta_a$page_name, "A")
  expect_identical(meta_b$page_name, "B")
})
