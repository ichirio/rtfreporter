# SPIKE (design/plan-resolver): the extraction layer.
#
# Claims:
#
#   1. a plan can begin from any source as_rtftables() supports, and extracts
#      the same body;
#   2. the dispatch duplicated here agrees with the one inside as_rtftables(),
#      so the temporary copy cannot drift unnoticed;
#   3. metadata the adapter read in SOURCE coordinates reaches the built table
#      through the same column map -- the adapter never learns what the stub
#      merged or the hidden columns removed.

.xd <- function() {
  data.frame(SOC = rep(c("Cardiac", "GI"), each = 3L),
             PT  = paste0("PT", 1:6),
             N   = as.character(11:16),
             stringsAsFactors = FALSE)
}

# ── every supported source ─────────────────────────────────────────────────

test_that("a plan starts from a data.frame", {
  p <- rtf_plan(.xd())
  expect_s3_class(p, "rtf_plan")
  expect_identical(names(p$data), c("SOC", "PT", "N"))
})

test_that("a plan starts from a gt_tbl and extracts the same body", {
  skip_if_not_installed("gt")
  g <- gt::gt(.xd())
  p <- rtf_plan(g)
  a <- as_rtftables(g, border = "tfl")[[1L]]
  expect_identical(unname(as.matrix(p$data)), unname(as.matrix(a$data)))
})

test_that("a plan starts from a gtsummary table", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  d <- data.frame(g = rep(c("A", "B"), each = 5L), x = c(1:5, 6:10))
  t <- gtsummary::tbl_summary(d, by = "g")
  expect_s3_class(rtf_plan(t), "rtf_plan")
})

test_that("a plan starts from an rtables/tern table", {
  skip_if_not_installed("rtables")
  lyt <- rtables::basic_table() |> rtables::split_cols_by("ARM") |>
    rtables::analyze("AGE",
                     afun = function(x) rtables::in_rows(Mean = mean(x)))
  dat <- data.frame(ARM = rep(c("A", "B"), each = 5L), AGE = c(60:64, 70:74))
  tt  <- rtables::build_table(lyt, dat)
  p <- rtf_plan(tt)
  a <- as_rtftables(tt, border = "tfl")[[1L]]
  expect_identical(unname(as.matrix(p$data)), unname(as.matrix(a$data)))
})

test_that("an unsupported input is refused, listing what is supported", {
  expect_error(rtf_plan(1:3), "gt_tbl, gtsummary, rtables")
})

# ── the duplicated dispatch cannot drift ───────────────────────────────────

test_that("the plan's dispatch agrees with as_rtftables() on every source", {
  srcs <- list(df = .xd())
  if (requireNamespace("gt", quietly = TRUE)) srcs$gt <- gt::gt(.xd())
  for (nm in names(srcs)) {
    p <- rtf_plan(srcs[[nm]])
    a <- as_rtftables(srcs[[nm]], border = "tfl")[[1L]]
    expect_identical(unname(as.matrix(p$data)), unname(as.matrix(a$data)),
                     info = nm)
  }
})

# ── source metadata placed through the column map ──────────────────────────

test_that("a label attribute becomes the header, at the final position", {
  d <- .xd()
  attr(d$N, "label") <- "Count"
  t <- rtf_plan(d) |> plan_hide("PT") |> plan_style(border = "tfl") |>
    plan_tables()
  # N is source column 3; hiding PT makes it final column 2
  expect_identical(t[[1L]]$col_header[[1L]], c("SOC", "Count"))
})

test_that("a merged stub takes its own joined name in the header", {
  d <- .xd()
  attr(d$SOC, "label") <- "System Organ Class"
  attr(d$N, "label") <- "Count"
  t <- rtf_plan(d) |> plan_stub(c("SOC", "PT")) |>
    plan_style(border = "tfl") |> plan_tables()
  expect_identical(t[[1L]]$col_header[[1L]], c("SOC / PT", "Count"))
})

test_that("a spanning header is projected, not dropped", {
  skip_if_not_installed("gt")
  # this was the spike's recorded limit until the header projection landed;
  # test-plan-header.R covers the projection itself in detail
  g <- gt::gt(.xd()) |> gt::tab_spanner("Group", c(SOC, PT))
  res <- resolve_plan(rtf_plan(g))
  expect_false(res$header$dropped)
  expect_type(res$header$col_header, "list")
})

test_that("no header in the source leaves none on the table", {
  res <- resolve_plan(rtf_plan(.xd()))
  expect_false(res$header$dropped)
  expect_null(res$header$col_header)
})

# ── a plan on an extracted source resolves and builds ──────────────────────

test_that("a gt source flows through the whole pipeline", {
  skip_if_not_installed("gt")
  t <- rtf_plan(gt::gt(.xd())) |>
    plan_stub(c("SOC", "PT")) |>
    plan_group("SOC", mode = "indent") |>
    plan_pages(max_rows = 5L) |>
    plan_style(border = "tfl") |>
    plan_tables()
  expect_true(length(t) >= 1L)
  expect_s3_class(t[[1L]], "rtftable")
  expect_identical(names(t[[1L]]$data), c("SOC / PT", "N"))
})

test_that("the source's cell styles ride the row map", {
  skip_if_not_installed("gt")
  p <- rtf_plan(gt::gt(.xd())) |> plan_stub(c("SOC", "PT"))
  res <- resolve_plan(p)
  cs <- res$source$cell_styles
  if (is.null(cs)) {
    expect_null(plan_row_map(res, NULL))
  } else {
    expect_length(plan_row_map(res, cs), res$nrow)
  }
})
