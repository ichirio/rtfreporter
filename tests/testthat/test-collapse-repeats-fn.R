## tests/testthat/test-collapse-repeats-fn.R
##
## collapse_repeats() -- post-hoc verb on rtftable / page list (per-page for a
## list). The per-page as_rtftables(collapse_repeats=) argument is tested in
## test-collapse-repeats.R.

library(testthat)

df3 <- function() {
  data.frame(
    grp = c("A", "A", "A", "B", "B"),
    sub = c("x", "x", "y", "x", "x"),
    n   = 1:5,
    stringsAsFactors = FALSE
  )
}

# ── single rtftable ────────────────────────────────────────────────────────

test_that("collapse_repeats() blanks repeats on a single rtftable body", {
  out <- collapse_repeats(rtftable(df3()), cols = "grp")
  expect_s3_class(out, "rtftable")
  expect_identical(as.character(out$data$grp), c("A", NA, NA, "B", NA))
  expect_identical(out$data$sub, df3()$sub)      # untouched
  expect_identical(out$data$n,   df3()$n)
})

test_that("suppression is hierarchical (an earlier column change resets)", {
  out <- collapse_repeats(rtftable(df3()), cols = c("grp", "sub"))
  expect_identical(as.character(out$data$grp), c("A", NA, NA, "B", NA))
  expect_identical(as.character(out$data$sub), c("x", NA, "y", "x", NA))
})

test_that("integer positions and a mixing list both work", {
  o1 <- collapse_repeats(rtftable(df3()), cols = 1L)
  expect_identical(as.character(o1$data$grp), c("A", NA, NA, "B", NA))
  o2 <- collapse_repeats(rtftable(df3()), cols = list("grp", 2L))
  expect_identical(as.character(o2$data$sub), c("x", NA, "y", "x", NA))
})

# ── page list: per-page application ────────────────────────────────────────

test_that("on a page list, collapse resets at each page break (per page)", {
  df <- data.frame(g = c("A", "A", "A", "A"), v = 1:4, stringsAsFactors = FALSE)
  pages <- as_rtftables(df, split = "rows", split_rows = 2)
  expect_gt(length(pages), 1L)
  out <- collapse_repeats(pages, cols = "g")
  # Per-page: the FIRST row of every page shows "A" again (run reset at the
  # page break); a whole-frame collapse would blank all but the very first.
  first_each <- vapply(out, function(p) as.character(p$data$g)[[1L]], character(1L))
  expect_true(all(first_each == "A"))
  # within a multi-row page, later repeats are blanked
  multi <- Filter(function(p) nrow(p$data) > 1L, out)[[1L]]
  expect_true(is.na(as.character(multi$data$g)[[2L]]))
})

test_that("the page-list verb equals the per-page as_rtftables() argument", {
  df <- data.frame(g = c("A", "A", "A", "A"), v = 1:4, stringsAsFactors = FALSE)
  via_arg <- as_rtftables(df, split = "rows", split_rows = 2, collapse_repeats = "g")
  via_fun <- collapse_repeats(
    as_rtftables(df, split = "rows", split_rows = 2), cols = "g")
  gcol <- function(p) lapply(p, function(pg) as.character(pg$data$g))
  expect_identical(gcol(via_fun), gcol(via_arg))
})

# ── errors ─────────────────────────────────────────────────────────────────

test_that("errors on a data.frame and on an unknown column", {
  expect_error(collapse_repeats(df3(), cols = "grp"),
               "expects an rtftable")
  expect_error(collapse_repeats(rtftable(df3()), cols = "nope"), "not found")
})
