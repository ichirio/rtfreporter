# SPIKE (design/plan-resolver): one row coordinate system.
#
# The claim under test: ONE row map -- output row -> source row, NA where the
# row was synthesised -- is enough for every consumer.  Groups, blanks, the
# page budget and per-row data all read it, and none needs a translation of
# its own.
#
# The last section is an early instalment of the eventual acceptance criterion:
# the resolver must agree with as_rtftables() wherever both can express the
# same intent.

.sd <- function() {
  data.frame(SOC = rep(c("Cardiac", "GI", "Nervous"), each = 3L),
             PT  = paste0("PT", 1:9),
             N   = as.character(11:19),
             stringsAsFactors = FALSE)
}

.sizes <- function(res) vapply(res$pages, length, integer(1L))
.asizes <- function(a) vapply(a, function(x) nrow(x$data), integer(1L))

.stub_res <- function(...) {
  resolve_plan(rtf_plan(.sd()) |> plan_stub(c("SOC", "PT")) |>
                 plan_group("SOC", mode = "indent") |> plan_pages(...))
}

# ── the row map ────────────────────────────────────────────────────────────

test_that("a stub inserts one label row per group, recorded in the map", {
  res <- .stub_res()
  expect_identical(res$nrow_source, 9L)
  expect_identical(res$nrow, 12L)
  expect_identical(res$rows$src,
                   c(NA, 1L, 2L, 3L, NA, 4L, 5L, 6L, NA, 7L, 8L, 9L))
})

test_that("label rows are derived from the map, not declared", {
  expect_identical(plan_label_rows(.stub_res()), c(1L, 5L, 9L))
})

test_that("with no stub the map is the identity", {
  res <- resolve_plan(rtf_plan(.sd()))
  expect_identical(res$rows$src, 1:9)
  expect_identical(plan_label_rows(res), integer(0))
  expect_identical(res$nrow, res$nrow_source)
})

test_that("the plan still never rewrites the caller's data", {
  d <- .sd()
  p <- rtf_plan(d) |> plan_stub(c("SOC", "PT"))
  invisible(resolve_plan(p))
  expect_identical(p$data, d)
  expect_identical(d, .sd())
})

# ── everything reads that one map ──────────────────────────────────────────

test_that("per-source-row data is carried through with one function", {
  res <- .stub_res()
  out <- plan_row_map(res, as.list(paste0("s", 1:9)))
  expect_length(out, 12L)
  expect_null(out[[1L]])              # synthesised label row
  expect_identical(out[[2L]], "s1")
  expect_identical(out[[12L]], "s9")
})

test_that("carrying NULL stays NULL", {
  expect_null(plan_row_map(.stub_res(), NULL))
})

test_that("a wrongly sized vector is refused, naming the expected length", {
  expect_error(plan_row_map(.stub_res(), as.list(1:5)), "one element per SOURCE")
})

test_that("the source rows of a page come back in order", {
  res <- .stub_res(max_rows = 5L)
  expect_identical(plan_page_source_rows(res, 1L), 1:3)
  expect_identical(plan_page_source_rows(res, 2L), 4:6)
})

test_that("every source row is printed exactly once across the pages", {
  res <- .stub_res(max_rows = 5L)
  all_src <- unlist(lapply(seq_along(res$pages), plan_page_source_rows,
                           res = res))
  expect_identical(sort(all_src), 1:9)
})

# ── the column map answers for a folded grouping column ────────────────────

test_that("grouping on a column the stub consumed still works", {
  # as_rtftables() needs the POST-stub name here; the plan does not, because
  # the column map already knows where SOC ended up
  res <- .stub_res()
  expect_length(unique(res$groups$id), 3L)
  expect_identical(res$groups$col, 1L)     # the stub column
})

test_that("grouping on a hidden column is refused", {
  expect_error(
    resolve_plan(rtf_plan(.sd()) |> plan_group("SOC") |> plan_hide("SOC")),
    "hidden and printed|is hidden"
  )
})

test_that("a label row travels with its group when the page is cut", {
  res <- .stub_res(max_rows = 5L)
  for (i in seq_along(res$pages)) {
    rows <- res$pages[[i]]
    expect_true(is.na(res$rows$src[rows[[1L]]]),
                info = "each page opens with its group's label row")
  }
})

# ── blanks live in output coordinates too ──────────────────────────────────

test_that("blank positions are output rows, not source rows", {
  res <- resolve_plan(rtf_plan(.sd()) |> plan_stub(c("SOC", "PT")) |>
                        plan_group("SOC", mode = "indent") |>
                        plan_blanks("between_groups"))
  expect_identical(res$blanks, c(4L, 8L))
})

# ── agreement with as_rtftables() ──────────────────────────────────────────

test_that("stub + group_safe pagination matches as_rtftables()", {
  for (mx in c(4L, 5L, 8L, 12L)) {
    a <- as_rtftables(.sd(), stub_vars = c("SOC", "PT"), split = "group_safe",
                      max_rows = mx, group_by = "indent", border = "tfl")
    p <- .stub_res(max_rows = mx)
    expect_identical(.asizes(a), .sizes(p), info = paste("max_rows =", mx))
  }
})

test_that("the printed body matches as_rtftables() page for page", {
  a <- as_rtftables(.sd(), stub_vars = c("SOC", "PT"), split = "group_safe",
                    max_rows = 5L, group_by = "indent", border = "tfl")
  p <- .stub_res(max_rows = 5L)
  expect_length(a, length(p$pages))
  for (i in seq_along(a)) {
    pa <- a[[i]]$data
    pp <- p$rows$body[p$pages[[i]], , drop = FALSE]
    expect_identical(names(pa), names(pp))
    # values only: as_rtftables() also attaches rtf_paginate_meta
    expect_identical(unname(as.matrix(pa)), unname(as.matrix(pp)),
                     info = paste("page", i))
  }
})

test_that("the same holds with blank rows declared", {
  for (mx in c(5L, 6L, 9L)) {
    a <- as_rtftables(.sd(), stub_vars = c("SOC", "PT"), split = "group_safe",
                      max_rows = mx, group_by = "indent",
                      blank_rows = "between_groups", border = "tfl")
    p <- resolve_plan(rtf_plan(.sd()) |> plan_stub(c("SOC", "PT")) |>
                        plan_group("SOC", mode = "indent") |>
                        plan_blanks("between_groups") |>
                        plan_pages(max_rows = mx))
    expect_identical(.asizes(a), .sizes(p), info = paste("max_rows =", mx))
  }
})

test_that("the projected columns match what as_rtftables() builds", {
  a <- as_rtftables(.sd(), stub_vars = c("SOC", "PT"), border = "tfl")[[1L]]
  p <- resolve_plan(rtf_plan(.sd()) |> plan_stub(c("SOC", "PT")))
  expect_identical(p$columns$names, names(a$data))
  expect_identical(unname(as.matrix(p$rows$body)),
                   unname(as.matrix(a$data)))
})
