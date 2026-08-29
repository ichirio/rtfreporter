# SPIKE (design/plan-resolver): materialise into the EXISTING rtftable.
#
# Two claims:
#
#   1. no second table class is needed -- the resolved plan IS an rtftable, so
#      every existing consumer keeps working and the type surface does not
#      double;
#   2. the rendered RTF is identical to what as_rtftables() produces for the
#      same intent.  This is the acceptance criterion, exercised here on the
#      configurations the spike can already express.

.bd <- function() {
  data.frame(SOC = rep(c("Cardiac", "GI", "Nervous"), each = 3L),
             PT  = paste0("PT", 1:9),
             N   = as.character(11:19),
             stringsAsFactors = FALSE)
}

# Render a list of rtftable pages and return the RTF lines.
.render_pages <- function(pages) {
  doc <- rtf_document(page = rtf_page(orientation = "landscape"))
  for (p in pages) doc <- rtf_tables(doc, p)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

.plan_pages <- function(..., .border = "tfl") {
  rtf_plan(.bd()) |>
    plan_stub(c("SOC", "PT")) |>
    plan_group("SOC", mode = "indent") |>
    plan_pages(...) |>
    plan_tables(border = .border)
}

# ── no new table class ─────────────────────────────────────────────────────

test_that("a materialised page is an ordinary rtftable", {
  pages <- .plan_pages(max_rows = 5L)
  expect_s3_class(pages[[1L]], "rtftable")
  expect_identical(class(pages[[1L]]), "rtftable")
})

test_that("the existing style verbs accept it unchanged", {
  p <- .plan_pages(max_rows = 5L)[[1L]]
  expect_s3_class(set_decimal_split(p, cols = 2L), "rtftable")
  expect_s3_class(style_cols(p, cols = 1L, align = "left"), "rtftable")
  expect_s3_class(style_body(p, rows = 1L, bold = TRUE), "rtftable")
})

test_that("it goes into a document through the existing verb", {
  pages <- .plan_pages(max_rows = 5L)
  doc <- rtf_document()
  for (p in pages) doc <- rtf_tables(doc, p)
  expect_length(doc$contents, 3L)
})

test_that("paginate_cols() accepts it", {
  p <- .plan_pages(max_rows = 12L)[[1L]]
  w <- rtftable(p$data, border = "tfl",
                column_widths_twips = c(4000L, 3000L))
  expect_s3_class(paginate_cols(w, at = 2L, carry = 1L)[[1L]], "rtftable")
})

# ── the acceptance criterion: identical RTF ────────────────────────────────

test_that("the rendered RTF is identical to as_rtftables()", {
  for (mx in c(4L, 5L, 8L, 12L)) {
    a <- as_rtftables(.bd(), stub_vars = c("SOC", "PT"), split = "group_safe",
                      max_rows = mx, group_by = "indent", border = "tfl")
    expect_identical(.render_pages(a), .render_pages(.plan_pages(max_rows = mx)),
                     info = paste("max_rows =", mx))
  }
})

test_that("identical with no pagination at all", {
  a <- as_rtftables(.bd(), stub_vars = c("SOC", "PT"), border = "tfl")
  p <- rtf_plan(.bd()) |> plan_stub(c("SOC", "PT")) |> plan_tables(border = "tfl")
  expect_identical(.render_pages(a), .render_pages(p))
})

test_that("identical with blank rows between groups", {
  for (mx in c(5L, 9L, 12L)) {
    a <- as_rtftables(.bd(), stub_vars = c("SOC", "PT"), split = "group_safe",
                      max_rows = mx, group_by = "indent",
                      blank_rows = "between_groups", border = "tfl")
    p <- rtf_plan(.bd()) |> plan_stub(c("SOC", "PT")) |>
      plan_group("SOC", mode = "indent") |>
      plan_blanks("between_groups") |>
      plan_pages(max_rows = mx) |>
      plan_tables(border = "tfl")
    expect_identical(.render_pages(a), .render_pages(p),
                     info = paste("max_rows =", mx))
  }
})

test_that("identical without a stub", {
  d <- .bd()
  a <- as_rtftables(d, split = "group_safe", max_rows = 5L, group_col = "SOC",
                    border = "tfl")
  p <- rtf_plan(d) |> plan_group("SOC") |> plan_pages(max_rows = 5L) |>
    plan_tables(border = "tfl")
  expect_identical(.render_pages(a), .render_pages(p))
})

# ── blank positions are translated per page ────────────────────────────────

test_that("an interior blank is translated to page-local coordinates", {
  # one page, so the blanks after output rows 4 and 8 are both interior
  p <- rtf_plan(.bd()) |> plan_stub(c("SOC", "PT")) |>
    plan_group("SOC", mode = "indent") |>
    plan_blanks("between_groups") |>
    plan_pages(max_rows = 12L) |>
    plan_tables(border = "tfl")
  expect_length(p, 1L)
  expect_identical(as.integer(p[[1L]]$blank_rows), c(4L, 8L))
})

test_that("a blank at the foot of a page is dropped", {
  # pages are 1-4, 5-8, 9-12 and the blanks fall after rows 4 and 8 -- the
  # last row of pages 1 and 2 -- so neither separates anything and both go.
  # as_rtftables() does the same; the RTF-identity tests above depend on it.
  p <- rtf_plan(.bd()) |> plan_stub(c("SOC", "PT")) |>
    plan_group("SOC", mode = "indent") |>
    plan_blanks("between_groups") |>
    plan_pages(max_rows = 5L) |>
    plan_tables(border = "tfl")
  expect_length(p, 3L)
  for (i in 1:3) expect_null(p[[i]]$blank_rows, info = paste("page", i))
})

test_that("a page keeps the blanks that fall inside it", {
  # 12 source rows, no stub: blanks after rows 3 and 9, pages of 6
  d <- data.frame(G = rep(c("A", "B", "C", "D"), each = 3L),
                  N = as.character(1:12), stringsAsFactors = FALSE)
  p <- rtf_plan(d) |> plan_group("G") |>
    plan_blanks(c(3L, 9L)) |>
    plan_pages(max_rows = 6L, groups = "split") |>
    plan_tables(border = "tfl")
  expect_length(p, 2L)
  expect_identical(as.integer(p[[1L]]$blank_rows), 3L)   # interior
  expect_identical(as.integer(p[[2L]]$blank_rows), 3L)   # row 9 -> local 3
})

# ── carry columns arrive as final positions ────────────────────────────────

test_that("a carry column becomes row_title at its FINAL position", {
  d <- data.frame(GRP = rep(c("A", "B"), each = 2L),
                  HIDE = as.character(1:4),
                  LBL = paste0("L", 1:4),
                  N = as.character(11:14), stringsAsFactors = FALSE)
  p <- rtf_plan(d) |> plan_hide("HIDE") |>
    plan_roles(LBL = role("carry")) |>
    plan_tables(border = "tfl")
  # LBL is source column 3; after hiding HIDE it is final column 2
  expect_identical(p[[1L]]$row_title, 2L)
})

# ── per-source-row styles onto pages ───────────────────────────────────────

test_that("styles are sliced onto pages through the one row map", {
  res <- resolve_plan(rtf_plan(.bd()) |> plan_stub(c("SOC", "PT")) |>
                        plan_group("SOC", mode = "indent") |>
                        plan_pages(max_rows = 5L))
  cs <- lapply(1:9, function(i) list(bold = c(TRUE, FALSE)))
  sl <- plan_cell_styles(res, cs)
  expect_length(sl, 3L)
  expect_identical(vapply(sl, length, integer(1L)), c(4L, 4L, 4L))
  expect_null(sl[[1L]][[1L]])            # the label row has no source
  expect_identical(sl[[1L]][[2L]], cs[[1L]])
})

test_that("no styles gives one empty slot per page", {
  res <- resolve_plan(rtf_plan(.bd()) |> plan_pages(max_rows = 4L))
  expect_length(plan_cell_styles(res, NULL), length(res$pages))
})
