# stub_cols(layout = ) and stub_cols(label_span = ) (#312).

.df <- function() {
  data.frame(group = c("grp1", "grp1", "grp2", "grp2"),
             label = c("label1", "label2", "label3", "label4"),
             HOGE  = c("1 (1.11)", "2 (2.22)", "3 (3.33)", "4 (4.44)"),
             stringsAsFactors = FALSE)
}

.cells <- function(o) lengths(regmatches(o, gregexpr("cellx", o, fixed = TRUE)))

.render <- function(d, widths) {
  tbl <- rtftable(d, border = "tfl", column_widths_twips = widths)
  rtfreporter:::.render_rtftable(tbl, 9360L)
}

# ── layout = "columns" ─────────────────────────────────────────────────────

test_that("the hierarchy columns are kept, in their original order", {
  out <- stub_cols(.df(), vars = c("group", "label"), layout = "columns")
  expect_identical(names(out), c("group", "label", "HOGE"))
})

test_that("a group value gets its own row and is blank on member rows", {
  out <- stub_cols(.df(), vars = c("group", "label"), layout = "columns",
                   indent = 0L)
  expect_identical(out$group, c("grp1", NA, NA, "grp2", NA, NA))
  expect_identical(out$label, c(NA, "label1", "label2", NA, "label3", "label4"))
  expect_identical(out$HOGE,
                   c(NA, "1 (1.11)", "2 (2.22)", NA, "3 (3.33)", "4 (4.44)"))
})

test_that("the leaf is indented in its own column", {
  out <- stub_cols(.df(), vars = c("group", "label"), layout = "columns")
  nb  <- rtfreporter:::.stub_nbsp()
  expect_identical(out$label[[2L]], paste0(strrep(nb, 4L), "label1"))
  expect_identical(out$group[[1L]], "grp1")   # the group row is flush left
})

test_that("indent = 0 leaves the leaf unpadded", {
  out <- stub_cols(.df(), vars = c("group", "label"), layout = "columns",
                   indent = 0L)
  expect_identical(out$label[[2L]], "label1")
})

test_that("the row count grows by one per group", {
  expect_identical(
    nrow(stub_cols(.df(), vars = c("group", "label"), layout = "columns")),
    6L)
})

test_that("a column label attribute survives", {
  d <- .df()
  attr(d$group, "label") <- "Group"
  out <- stub_cols(d, vars = c("group", "label"), layout = "columns")
  expect_identical(attr(out$group, "label"), "Group")
})

test_that("nothing is merged: every row keeps the full cell count", {
  out <- stub_cols(.df(), vars = c("group", "label"), layout = "columns")
  n <- .cells(.render(out, c(2000L, 3000L, 4000L)))
  expect_true(all(n[n > 0] == 3L))
})

# ── contradictions ─────────────────────────────────────────────────────────

test_that("label_span with layout = columns is an error", {
  expect_error(
    stub_cols(.df(), vars = c("group", "label"), layout = "columns",
              label_span = TRUE),
    "applies to layout = \"merged\" only"
  )
})

test_that("label with layout = columns is an error", {
  expect_error(
    stub_cols(.df(), vars = c("group", "label"), layout = "columns",
              label = "Group / Label"),
    "does not create"
  )
})

test_that("label_span is validated", {
  expect_error(stub_cols(.df(), vars = c("group", "label"),
                         label_span = "yes"),
               "must be TRUE or FALSE")
  expect_error(stub_cols(.df(), vars = c("group", "label"),
                         label_span = NA),
               "must be TRUE or FALSE")
})

test_that("an unknown layout is rejected", {
  expect_error(stub_cols(.df(), vars = c("group", "label"), layout = "nope"))
})

# ── label_span ─────────────────────────────────────────────────────────────

test_that("the spanned rows are recorded on the result", {
  out <- stub_cols(.df(), vars = c("group", "label"), label_span = TRUE)
  expect_identical(attr(out, "rtf_label_rows", exact = TRUE), c(1L, 4L))
})

test_that("the default records nothing", {
  out <- stub_cols(.df(), vars = c("group", "label"))
  expect_null(attr(out, "rtf_label_rows", exact = TRUE))
})

test_that("a group row renders as one cell and the rest do not", {
  out <- stub_cols(.df(), vars = c("group", "label"), indent = 0L,
                   label_span = TRUE)
  n <- .cells(.render(out, c(4000L, 5000L)))
  n <- n[n > 0]
  expect_identical(n[[1L]], 2L)   # column header
  expect_identical(n[[2L]], 1L)   # grp1
  expect_identical(n[[3L]], 2L)   # label1
  expect_identical(n[[5L]], 1L)   # grp2
})

test_that("without label_span every row keeps the full cell count", {
  out <- stub_cols(.df(), vars = c("group", "label"), indent = 0L)
  n <- .cells(.render(out, c(4000L, 5000L)))
  expect_true(all(n[n > 0] == 2L))
})

test_that("the merged cell runs to the table's right edge", {
  out <- stub_cols(.df(), vars = c("group", "label"), indent = 0L,
                   label_span = TRUE)
  o   <- .render(out, c(4000L, 5000L))
  grp <- grep("grp1", o, value = TRUE)
  expect_length(grp, 1L)
  expect_true(grepl("cellx9000", grp[[1L]], fixed = TRUE))
})

test_that("a folded group-summary row is NOT spanned", {
  # the leaf is NA, so its statistics land on the label row -- that row
  # carries data in the other columns and must keep them
  d <- data.frame(group = c("grp1", "grp1", "grp1"),
                  label = c(NA, "label1", "label2"),
                  HOGE  = c("9 (9.99)", "1 (1.11)", "2 (2.22)"),
                  stringsAsFactors = FALSE)
  out <- stub_cols(d, vars = c("group", "label"), label_span = TRUE)
  expect_identical(out$HOGE[[1L]], "9 (9.99)")
  expect_null(attr(out, "rtf_label_rows", exact = TRUE))
})

# ── the span through the rest of the pipeline ──────────────────────────────

test_that("the span survives paginate_cols() with no re-indexing", {
  out <- cbind(stub_cols(.df(), vars = c("group", "label"), indent = 0L,
                         label_span = TRUE),
               EXTRA = c("a", "b", "c", "d", "e", "f"))
  attr(out, "rtf_label_rows") <- c(1L, 4L)
  tbl <- rtftable(out, border = "tfl",
                  column_widths_twips = c(3000L, 3000L, 3000L))
  pg  <- paginate_cols(tbl, at = 3L, carry = 1L)
  expect_length(pg, 2L)
  for (p in pg) {
    o <- rtfreporter:::.render_rtftable(p, 9360L)
    n <- .cells(o)
    expect_identical(n[n > 0][[2L]], 1L)   # the group row, merged per page
  }
})

test_that("a spanned row is never decimal-split", {
  d <- data.frame(group = c("grp1", "grp1"), label = c("label1", "label2"),
                  V = c("1.5", "2.25"), stringsAsFactors = FALSE)
  out <- stub_cols(d, vars = c("group", "label"), indent = 0L,
                   label_span = TRUE)
  tbl <- rtftable(out, border = "none",
                  column_widths_twips = c(3000L, 3000L)) |>
    set_decimal_split(cols = 2L)
  o <- rtfreporter:::.render_rtftable(tbl, 9360L)
  n <- .cells(o)
  n <- n[n > 0]
  expect_identical(n[[2L]], 1L)   # grp1: merged, not split
  expect_identical(n[[3L]], 3L)   # label1: split into two cells
})

# ── the default is untouched ───────────────────────────────────────────────

test_that("the merged layout is unchanged when neither option is given", {
  a <- stub_cols(.df(), vars = c("group", "label"))
  expect_identical(names(a), c("group / label", "HOGE"))
  expect_identical(nrow(a), 6L)
  expect_true(is.na(a$HOGE[[1L]]))
})
