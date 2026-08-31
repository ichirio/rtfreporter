# SPIKE (design/plan-resolver): projecting a spanning column header.
#
# The claim under test, and the last piece of "eight reindexers become one
# map": .plan_project_header() replaces .reindex_col_header(),
# .reindex_header_row(), .reindex_header_cell() AND .prepend_stub_header(),
# because the map already encodes what each of them had to be told separately
# -- which columns went away, which were merged into a stub, and where the
# stub sits.

.hd <- function() {
  data.frame(SOC = rep(c("Cardiac", "GI"), each = 3L),
             PT  = paste0("PT", 1:6),
             A   = as.character(1:6),
             B   = as.character(11:16),
             stringsAsFactors = FALSE)
}

.spanner <- function() {
  gt::gt(.hd()) |> gt::tab_spanner("Treatment", c(A, B))
}

.render <- function(pages) {
  doc <- rtf_document(page = rtf_page(orientation = "landscape"))
  for (p in pages) doc <- rtf_tables(doc, p)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

.cells <- function(res) res$header$col_header[[1L]]

# ── the projection ─────────────────────────────────────────────────────────

test_that("a spanner survives with its own positions when nothing moves", {
  skip_if_not_installed("gt")
  res <- resolve_plan(rtf_plan(.spanner()))
  expect_false(res$header$dropped)
  expect_identical(.cells(res)[[1L]]$pos, c(3L, 4L))
  expect_identical(.cells(res)[[1L]]$label, "Treatment")
})

test_that("a stub merge shifts the spanner without anyone telling it to", {
  skip_if_not_installed("gt")
  # SOC + PT collapse to one column, so the spanner over A and B moves left by
  # one.  This is what .prepend_stub_header() did; here it falls out of the map
  res <- resolve_plan(rtf_plan(.spanner()) |> plan_stub(c("SOC", "PT")))
  expect_identical(.cells(res)[[1L]]$pos, c(2L, 3L))
  expect_identical(res$header$col_header[[2L]], c("SOC / PT", "A", "B"))
})

test_that("hiding one covered column narrows the span to a single position", {
  skip_if_not_installed("gt")
  res <- resolve_plan(rtf_plan(.spanner()) |> plan_hide("B"))
  expect_identical(.cells(res)[[1L]]$pos, 3L)
  expect_identical(res$header$col_header[[2L]], c("SOC", "PT", "A"))
})

test_that("hiding every covered column removes the spanning row entirely", {
  skip_if_not_installed("gt")
  res <- resolve_plan(rtf_plan(.spanner()) |> plan_hide(c("A", "B")))
  expect_length(res$header$col_header, 1L)          # only the leaf row left
  expect_identical(res$header$col_header[[1L]], c("SOC", "PT"))
})

test_that("a stub merge and a hide compose", {
  skip_if_not_installed("gt")
  res <- resolve_plan(rtf_plan(.spanner()) |>
                        plan_stub(c("SOC", "PT")) |> plan_hide("A"))
  expect_identical(.cells(res)[[1L]]$pos, 2L)
  expect_identical(res$header$col_header[[2L]], c("SOC / PT", "B"))
})

test_that("legacy from/to cells are projected like pos cells", {
  cols <- list(names = c("x", "z"),
               map = c(x = 1L, y = NA_integer_, z = 2L))
  ch <- list(list(list(from = 1L, to = 3L, label = "All")),
             c("x", "y", "z"))
  out <- rtfreporter:::.plan_project_header(ch, cols)
  expect_identical(out[[1L]][[1L]]$from, 1L)
  expect_identical(out[[1L]][[1L]]$to, 2L)
  expect_identical(out[[2L]], c("x", "z"))
})

test_that("a header that is not per-column is passed through untouched", {
  cols <- list(names = c("x", "z"), map = c(x = 1L, y = NA_integer_, z = 2L))
  expect_identical(rtfreporter:::.plan_project_header("one string", cols),
                   "one string")
})

# ── the adapter's own column spec ──────────────────────────────────────────

test_that("the adapter's col_spec is projected, not dropped", {
  skip_if_not_installed("gt")
  # gt aligns its columns, and a spanning cell inherits that alignment.
  # Dropping it rendered a right-aligned spanner centred.
  res <- resolve_plan(rtf_plan(.spanner()) |> plan_stub(c("SOC", "PT")))
  expect_true(length(res$style$col_spec) > 0L)
})

test_that("a plan-declared style beats the adapter's", {
  skip_if_not_installed("gt")
  res <- resolve_plan(rtf_plan(.spanner()) |>
                        plan_roles(A = role("display", align = "left")))
  pos <- unname(res$columns$map[["A"]])
  entry <- Filter(function(e) identical(e$col, pos), res$style$col_spec)
  expect_length(entry, 1L)
  expect_identical(entry[[1L]]$align, "left")
})

test_that("the adapter's spec for a hidden column is dropped", {
  skip_if_not_installed("gt")
  res <- resolve_plan(rtf_plan(.spanner()) |> plan_hide("B"))
  cols <- vapply(res$style$col_spec, function(e) e$col, integer(1L))
  expect_true(all(cols <= length(res$columns$names)))
})

# ── the acceptance criterion ───────────────────────────────────────────────

test_that("a spanner plus a stub renders identically to as_rtftables()", {
  skip_if_not_installed("gt")
  g <- .spanner()
  a <- as_rtftables(g, read_meta = TRUE, stub_vars = c("SOC", "PT"),
                    border = "tfl")
  p <- rtf_plan(g) |> plan_stub(c("SOC", "PT")) |>
    plan_style(border = "tfl") |> plan_tables()
  expect_identical(.render(a), .render(p))
})

test_that("a spanner plus a hidden column renders identically", {
  skip_if_not_installed("gt")
  g <- .spanner()
  a <- as_rtftables(g, read_meta = TRUE, drop_cols = "B", border = "tfl")
  p <- rtf_plan(g) |> plan_hide("B") |> plan_style(border = "tfl") |>
    plan_tables()
  expect_identical(.render(a), .render(p))
})

test_that("a plain spanner with no reshaping renders identically", {
  skip_if_not_installed("gt")
  g <- .spanner()
  a <- as_rtftables(g, read_meta = TRUE, border = "tfl")
  p <- rtf_plan(g) |> plan_style(border = "tfl") |> plan_tables()
  expect_identical(.render(a), .render(p))
})
