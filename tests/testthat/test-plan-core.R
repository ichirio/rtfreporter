# SPIKE (design/plan-resolver): deferred layers, resolved once.
#
# These tests are the spike's evidence.  They pin the three properties the
# experiment exists to demonstrate:
#
#   1. grouping is resolved ONCE and both consumers read the same result,
#      so the `group_col` split brain measured in as_rtftables() cannot occur;
#   2. the row budget sees the blank rows by itself, so nothing has to describe
#      them to it from outside (what `count_blank_rows` does today);
#   3. re-declaring a layer is LAST WRITER WINS, per field.
#
# They also check the resolver agrees with as_rtftables(split = "group_safe")
# on the cases both can express, so the spike is not quietly reinventing
# different pagination.

.df <- function() {
  # Column 1 is unique per row, so a resolver that silently defaults to the
  # first column produces visibly wrong answers.  The real grouping is col 2.
  data.frame(PT  = paste0("PT", sprintf("%02d", 1:12)),
             SOC = rep(c("Cardiac", "GI", "Nervous"), each = 4L),
             N   = as.character(1:12),
             stringsAsFactors = FALSE)
}

.sizes <- function(res) vapply(res$pages, length, integer(1L))

# ── the plan holds declarations, and changes nothing ───────────────────────

test_that("a plan never rewrites the data it was given", {
  d <- .df()
  p <- rtf_plan(d) |> plan_group("SOC") |> plan_blanks("between_groups") |>
    plan_pages(max_rows = 6L)
  expect_identical(p$data, d)
})

test_that("a plan rejects a source no adapter can read", {
  expect_error(rtf_plan(1:3), "A plan can start from")
})

test_that("the constructor takes two arguments and nothing else", {
  # ggplot2 starts from ggplot(data, mapping); a plan starts from
  # rtf_plan(x, read_meta).  Every setting is a layer, so there is exactly one
  # way to declare each one.
  expect_identical(names(formals(rtf_plan)), c("x", "read_meta"))
  expect_false(exists("rtf_plan_from", envir = asNamespace("rtfreporter"),
                      inherits = FALSE))
})

test_that("a layer verb rejects anything that is not a plan", {
  expect_error(plan_group(list(), "SOC"), "Expected an rtf_plan")
})

# ── last writer wins, per field ────────────────────────────────────────────

test_that("grouping lives in the role table, not a layer of its own", {
  # moved deliberately: a grouping column with two homes is a grouping column
  # that can disagree with itself, which is the as_rtftables() defect
  p <- rtf_plan(.df()) |> plan_group("SOC")
  expect_null(p$layers$group)
  expect_identical(p$layers$roles$SOC$roles, "group")
})

test_that("re-grouping adds the role to the new column", {
  p <- rtf_plan(.df()) |> plan_group("PT") |> plan_group("SOC")
  expect_identical(p$layers$roles$SOC$roles, "group")
  # PT keeps its own entry; two groupings are refused when resolved
  expect_error(resolve_plan(p), "single column")
})

test_that("a field the second call did not supply survives", {
  p <- rtf_plan(.df()) |>
    plan_pages(max_rows = 6L, groups = "keep") |>
    plan_pages(groups = "split")
  expect_identical(p$layers$pages$max_rows, 6L)
  expect_identical(p$layers$pages$groups, "split")
})

test_that("merging works across all three layers", {
  p <- rtf_plan(.df()) |>
    plan_blanks("between_groups", first = TRUE) |>
    plan_blanks(last = TRUE)
  expect_identical(p$layers$blanks$where, "between_groups")
  expect_true(p$layers$blanks$first)
  expect_true(p$layers$blanks$last)
})

test_that("an unsupplied field never overwrites with a default", {
  p <- rtf_plan(.df()) |> plan_pages(max_rows = 6L) |> plan_pages()
  expect_identical(p$layers$pages$max_rows, 6L)
})

# ── one grouping, read by every consumer ───────────────────────────────────

test_that("blanks come from the declared grouping, not from column 1", {
  res <- resolve_plan(rtf_plan(.df()) |> plan_group("SOC") |>
                        plan_blanks("between_groups") |>
                        plan_pages(max_rows = 12L))
  expect_identical(res$blanks, c(4L, 8L))
})

test_that("that matches what as_rtftables() produces on its correct path", {
  a <- as_rtftables(.df(), split = "none", group_col = "SOC",
                    blank_rows = "between_groups", border = "tfl")
  res <- resolve_plan(rtf_plan(.df()) |> plan_group("SOC") |>
                        plan_blanks("between_groups"))
  expect_identical(as.integer(a[[1L]]$blank_rows), res$blanks)
})

test_that("blanks without a grouping error instead of guessing a column", {
  expect_error(
    resolve_plan(rtf_plan(.df()) |> plan_blanks("between_groups")),
    "needs a grouping"
  )
})

test_that("the resolved grouping is reported once, for inspection", {
  res <- resolve_plan(rtf_plan(.df()) |> plan_group("SOC"))
  expect_length(unique(res$groups$id), 3L)
  expect_identical(res$groups$col, 2L)
})

test_that("an explicit mode is honoured", {
  d <- data.frame(lbl = c("A", "  a1", "  a2", "B", "  b1"),
                  N = as.character(1:5), stringsAsFactors = FALSE)
  res <- resolve_plan(rtf_plan(d) |> plan_group("lbl", mode = "indent") |>
                        plan_blanks("between_groups"))
  expect_identical(res$blanks, 3L)
})

test_that("an unknown mode is rejected at declaration time", {
  expect_error(plan_group(rtf_plan(.df()), "SOC", mode = "nope"))
})

# ── the budget sees the blanks by itself ───────────────────────────────────

test_that("counting the blanks changes the page split with no extra argument", {
  mk <- function(cb) {
    resolve_plan(rtf_plan(.df()) |> plan_group("SOC") |>
                   plan_blanks("between_groups") |>
                   plan_pages(max_rows = 6L, groups = "split",
                              count_blanks = cb))
  }
  expect_identical(.sizes(mk(FALSE)), c(6L, 6L))
  expect_identical(.sizes(mk(TRUE)),  c(4L, 4L, 4L))
})

test_that("with no blanks declared, counting them is a no-op", {
  a <- resolve_plan(rtf_plan(.df()) |> plan_group("SOC") |>
                      plan_pages(max_rows = 6L, groups = "split",
                                 count_blanks = TRUE))
  b <- resolve_plan(rtf_plan(.df()) |> plan_group("SOC") |>
                      plan_pages(max_rows = 6L, groups = "split"))
  expect_identical(.sizes(a), .sizes(b))
})

# ── pagination ─────────────────────────────────────────────────────────────

test_that("no page budget means one page", {
  res <- resolve_plan(rtf_plan(.df()) |> plan_group("SOC"))
  expect_length(res$pages, 1L)
  expect_identical(res$pages[[1L]], 1:12)
})

test_that("groups are kept whole by default", {
  for (mx in c(4L, 5L, 6L, 7L)) {
    res <- resolve_plan(rtf_plan(.df()) |> plan_group("SOC") |>
                          plan_pages(max_rows = mx))
    expect_identical(.sizes(res), c(4L, 4L, 4L))
  }
})

test_that("the resolver agrees with as_rtftables(group_safe)", {
  for (mx in c(4L, 8L, 9L, 12L)) {
    a <- as_rtftables(.df(), split = "group_safe", max_rows = mx,
                      group_col = "SOC", border = "tfl")
    b <- resolve_plan(rtf_plan(.df()) |> plan_group("SOC") |>
                        plan_pages(max_rows = mx))
    expect_identical(vapply(a, function(x) nrow(x$data), integer(1L)),
                     .sizes(b),
                     info = paste("max_rows =", mx))
  }
})

test_that("a group larger than the budget still makes progress", {
  # the named strategies are delegated to the functions as_rtftables() uses,
  # so the assertion is agreement rather than a shape written out by hand
  big <- data.frame(SOC = rep("One", 10L), N = as.character(1:10),
                    stringsAsFactors = FALSE)
  res <- resolve_plan(rtf_plan(big) |> plan_group("SOC") |>
                        plan_pages(max_rows = 4L))
  a <- as_rtftables(big, split = "group_safe", max_rows = 4L,
                    group_col = "SOC", border = "tfl")
  expect_identical(.sizes(res),
                   vapply(a, function(x) nrow(x$data), integer(1L)))
  # NB the total EXCEEDS the 10 source rows: a group split across pages has
  # its header row repeated on each continuation, which is what cont_label is
  # for.  Delegating to .split_group_safe() inherits that, so the plan and
  # as_rtftables() agree on it too.
  expect_gt(sum(.sizes(res)), 10L)
})

test_that("every row lands on exactly one page", {
  for (mx in c(2L, 3L, 5L, 7L, 11L)) {
    for (kg in c("keep", "prefer", "split")) {
      res <- resolve_plan(rtf_plan(.df()) |> plan_group("SOC") |>
                            plan_pages(max_rows = mx, groups = kg))
      expect_identical(sort(unlist(res$pages)), 1:12,
                       info = paste("max_rows", mx, "groups", kg))
    }
  }
})

test_that("an empty body yields one empty page", {
  d <- .df()[0L, , drop = FALSE]
  res <- resolve_plan(rtf_plan(d) |> plan_pages(max_rows = 5L))
  expect_length(res$pages, 1L)
  expect_length(res$pages[[1L]], 0L)
})

# ── blanks at the edges ────────────────────────────────────────────────────

test_that("first and last add the edge positions", {
  res <- resolve_plan(rtf_plan(.df()) |> plan_group("SOC") |>
                        plan_blanks("between_groups", first = TRUE,
                                    last = TRUE))
  expect_identical(res$blanks, c(0L, 4L, 8L, 12L))
})

test_that("explicit positions are accepted", {
  res <- resolve_plan(rtf_plan(.df()) |> plan_blanks(c(3L, 7L)))
  expect_identical(res$blanks, c(3L, 7L))
})

test_that("an unusable `where` is rejected", {
  # the message comes from the shared blank-row resolver the plan delegates to
  expect_error(resolve_plan(rtf_plan(.df()) |> plan_blanks("nonsense")),
               "Unrecognised")
})
