## tests/testthat/test-gt-group.R
##
## gt_group support (#214): gt's multi-table container (gt::gt_group() /
## gt::gt_split(); also what tfrmt's print_to_gt() returns under a page_plan)
## expands to one page set per member table.  All tests skip when gt is
## absent.

library(testthat)

# Two small member tables shared by the tests below.
.gt_group_members <- function() {
  list(
    gt::gt(data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)),
    gt::gt(data.frame(a = 4:5, b = letters[4:5], stringsAsFactors = FALSE))
  )
}

# ── detection & expansion ─────────────────────────────────────────────────────

test_that(".is_gt_group() detects gt_group containers only", {
  skip_if_not_installed("gt")
  gg <- gt::gt_group(.list = .gt_group_members())
  expect_true(rtfreporter:::.is_gt_group(gg))
  expect_false(rtfreporter:::.is_gt_group(.gt_group_members()[[1]]))
  expect_false(rtfreporter:::.is_gt_group(list(a = 1)))
})

test_that(".gt_group_tables() pulls every member as a gt_tbl", {
  skip_if_not_installed("gt")
  gg   <- gt::gt_group(.list = .gt_group_members())
  tbls <- rtfreporter:::.gt_group_tables(gg)
  expect_length(tbls, 2L)
  expect_true(all(vapply(tbls, inherits, logical(1), "gt_tbl")))
})

test_that(".gt_group_tables() on an empty gt_group returns an empty list", {
  skip_if_not_installed("gt")
  expect_identical(rtfreporter:::.gt_group_tables(gt::gt_group()), list())
})

# ── as_rtftables() ────────────────────────────────────────────────────────────

test_that("as_rtftables() expands a gt_group into one page per member", {
  skip_if_not_installed("gt")
  members <- .gt_group_members()
  pages   <- as_rtftables(gt::gt_group(.list = members))
  expect_length(pages, 2L)
  expect_true(all(vapply(pages, inherits, logical(1), "rtftable")))
  # Same bodies as converting the member list directly.
  direct <- as_rtftables(members)
  expect_identical(lapply(pages,  `[[`, "data"),
                   lapply(direct, `[[`, "data"))
})

test_that("as_rtftables() accepts gt_split() output", {
  skip_if_not_installed("gt")
  g     <- gt::gt(data.frame(a = 1:10, b = letters[1:10],
                             stringsAsFactors = FALSE))
  pages <- as_rtftables(gt::gt_split(g, row_every_n = 4))
  expect_length(pages, 3L)                      # 4 + 4 + 2 rows
  expect_equal(vapply(pages, function(p) nrow(p$data), integer(1L)),
               c(4L, 4L, 2L))
})

test_that("as_rtftables() pagination args apply per gt_group member", {
  skip_if_not_installed("gt")
  members <- list(
    gt::gt(data.frame(a = 1:4, stringsAsFactors = FALSE)),
    gt::gt(data.frame(a = 5:8, stringsAsFactors = FALSE))
  )
  pages <- as_rtftables(gt::gt_group(.list = members),
                        split = "group_force", max_rows = 2)
  # Each 4-row member -> 2 pages of 2 rows.
  expect_length(pages, 4L)
  expect_equal(vapply(pages, function(p) nrow(p$data), integer(1L)),
               rep(2L, 4L))
})

test_that("as_rtftables() converts an empty gt_group to an empty list", {
  skip_if_not_installed("gt")
  expect_identical(as_rtftables(gt::gt_group()), list())
})

# ── as_rtftable() ─────────────────────────────────────────────────────────────

test_that("as_rtftable() unwraps a single-member gt_group", {
  skip_if_not_installed("gt")
  gg  <- gt::gt_group(.list = .gt_group_members()[1])
  tbl <- as_rtftable(gg)
  expect_s3_class(tbl, "rtftable")
  expect_equal(nrow(tbl$data), 3L)
})

test_that("as_rtftable() rejects a multi-member gt_group informatively", {
  skip_if_not_installed("gt")
  gg <- gt::gt_group(.list = .gt_group_members())
  expect_error(as_rtftable(gg), "holding 2 tables.*as_rtftables")
})

# ── tfrmt page_plan round-trip ────────────────────────────────────────────────

test_that("a tfrmt page_plan render (gt_group) converts page by page", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tfrmt")
  dat <- expand.grid(group  = c("g1", "g2"),
                     label  = paste0("lbl", 1:3),
                     column = c("A", "B"),
                     param  = "n",
                     stringsAsFactors = FALSE)
  dat$value <- seq_len(nrow(dat))
  spec <- tfrmt::tfrmt(
    group = group, label = label, column = column, param = param,
    value = value,
    body_plan = tfrmt::body_plan(
      tfrmt::frmt_structure(group_val = ".default", label_val = ".default",
                            tfrmt::frmt("x"))),
    page_plan = tfrmt::page_plan(
      tfrmt::page_structure(group_val = ".default")))
  pg <- tfrmt::print_to_gt(spec, dat)
  expect_s3_class(pg, "gt_group")              # the motivating case for #214
  pages <- as_rtftables(pg)
  expect_length(pages, 2L)                     # one page per group value
  expect_true(all(vapply(pages, inherits, logical(1), "rtftable")))
})
