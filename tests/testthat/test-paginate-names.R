# paginate() — list-input name propagation + split = "by_value"

# ──────── List input name propagation ─────────────────────────────────────

test_that("paginate(named list) preserves input names when each input → 1 page", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(a = 4:6)
  res <- paginate(list(safety = df1, efficacy = df2))   # split = "none"
  expect_length(res, 2L)
  expect_identical(names(res), c("safety", "efficacy"))
})

test_that("paginate(named list) repeats the name when an input → many pages", {
  df  <- data.frame(label = LETTERS[1:8], v = 1:8, stringsAsFactors = FALSE)
  res <- paginate(list(demog = df), max_rows = 3L, split = "group_force")
  expect_gte(length(res), 2L)
  # a page name is a HEADING, so the pages of one table share it
  expect_true(all(names(res) == "demog"))
})

test_that("paginate(named list) handles unnamed elements (NULL name passes through)", {
  df  <- data.frame(a = 1:2)
  res <- paginate(list(named = df, df))                 # 2nd element unnamed
  expect_length(res, 2L)
  # First chunk has its name, second has empty/NA name
  expect_identical(names(res)[1L], "named")
  expect_true(is.na(names(res)[2L]) || !nzchar(names(res)[2L]))
})

test_that("unnamed list input yields no names on output (default behaviour)", {
  res <- paginate(list(data.frame(a = 1:2), data.frame(a = 3:4)))
  expect_length(res, 2L)
  expect_null(names(res))
})

# ──────── split = "by_value" ──────────────────────────────────────────────

test_that("by_value with group_col emits 1 chunk per consecutive value, named by it", {
  df <- data.frame(
    visit = c("Wk1","Wk1","Wk2","Wk2","Wk2","Wk4"),
    val   = c(10, 11, 20, 22, 21, 30)
  )
  res <- paginate(df, split = "by_value", group_col = "visit")
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
  res <- paginate(df, split = "by_value")     # group_col = NULL (indent)
  expect_identical(names(res),
                   c("Demographics", "Vital signs", "Lab values"))
})

test_that("by_value force-splits groups that exceed max_rows, keeping the name", {
  df <- data.frame(
    visit = rep("Wk1", 8L),
    val   = 1:8
  )
  res <- paginate(df, split = "by_value", group_col = "visit",
                   max_rows = 3L)
  expect_gte(length(res), 2L)
  expect_true(all(names(res) == "Wk1"))              # every page, one heading
})

test_that("by_value page_name lands in rtf_paginate_meta as well", {
  df <- data.frame(visit = c("A","A","B"), v = 1:3)
  res <- paginate(df, split = "by_value", group_col = "visit")
  meta_a <- attr(res[["A"]], "rtf_paginate_meta")
  meta_b <- attr(res[["B"]], "rtf_paginate_meta")
  expect_identical(meta_a$page_name, "A")
  expect_identical(meta_b$page_name, "B")
})

# ──────── List of named tibbles, no split → names round-trip ──────────────

test_that("list of named tibbles with split='none' returns names intact", {
  skip_if_not_installed("tibble")
  pages_in <- list(
    "Table 14.1.1" = tibble::tibble(x = 1:2),
    "Table 14.2.1" = tibble::tibble(x = 3:4)
  )
  res <- paginate(pages_in)             # split = "none" default
  expect_identical(names(res), names(pages_in))
  for (p in res) expect_s3_class(p, "tbl_df")
})
