## tests/testthat/test-gt-adapter.R
##
## gt adapter (v0.0.46+): the body comes from gt::extract_body(output="html")
## and only render-relevant metadata is read.  All tests skip when gt is
## absent.

library(testthat)

# ── detection & token resolution ──────────────────────────────────────────────

test_that(".is_gt_tbl() detects gt tables", {
  skip_if_not_installed("gt")
  expect_true(rtfreporter:::.is_gt_tbl(gt::gt(head(mtcars, 1))))
  expect_false(rtfreporter:::.is_gt_tbl(data.frame(a = 1)))
})

test_that(".resolve_gt_tokens() handles TRUE / FALSE / vector / bad token", {
  all <- rtfreporter:::.GT_META_TOKENS
  expect_identical(rtfreporter:::.resolve_gt_tokens(TRUE),  all)
  expect_identical(rtfreporter:::.resolve_gt_tokens(FALSE), character(0))
  expect_identical(rtfreporter:::.resolve_gt_tokens(c("titles")), "titles")
  expect_error(rtfreporter:::.resolve_gt_tokens("nope"), "Unknown")
})

# ── clean body ────────────────────────────────────────────────────────────────

test_that(".gt_to_rtftable_kwargs() body keeps only visible columns", {
  skip_if_not_installed("gt")
  g  <- gt::gt(data.frame(a = c("x", "y"), b = 1:2, stringsAsFactors = FALSE)) |>
    gt::cols_hide(a)
  kw <- rtfreporter:::.gt_to_rtftable_kwargs(g)
  expect_equal(ncol(kw$data), 1L)           # hidden column dropped
})

test_that(".gt_to_rtftable_kwargs() body has no stray newlines", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tfrmt")
  dat <- data.frame(group = c("Age", "Age"), label = c("Mean", "SD"),
                    column = "Trt A", value = c(45.1, 5.2),
                    param = c("mean", "sd"), stringsAsFactors = FALSE)
  tf <- tfrmt::tfrmt(
    group = group, label = label, column = column, param = param, value = value,
    body_plan = tfrmt::body_plan(
      tfrmt::frmt_structure(".default", ".default", tfrmt::frmt("xx.x"))))
  g  <- tfrmt::print_to_gt(tf, dat)
  kw <- rtfreporter:::.gt_to_rtftable_kwargs(g)
  # tfrmt's helper column (..tfrmt_row_grp_lbl) is gone, and the group-label
  # rows are genuinely empty (no "<br />" -> "\n").
  expect_false(any(grepl("tfrmt", names(kw$data))))
  expect_false(any(grepl("\n", as.matrix(kw$data), fixed = TRUE)))
})

# ── metadata: labels / alignment / spanning / widths / titles / footnotes ─────

test_that("col_header + alignment are read from a gt_tbl", {
  skip_if_not_installed("gt")
  g  <- gt::gt(head(mtcars, 1)[, c("mpg", "cyl")]) |>
    gt::cols_label(mpg = "MPG", cyl = "Cyl") |>
    gt::cols_align("right", columns = c(mpg, cyl))
  kw <- rtfreporter:::.gt_to_rtftable_kwargs(g)
  expect_identical(kw$col_header, c("MPG", "Cyl"))
  expect_identical(vapply(kw$col_spec, function(s) s$align, ""), c("right", "right"))
})

test_that("spanning headers become a stacked multi-row col_header", {
  skip_if_not_installed("gt")
  g  <- gt::gt(head(mtcars, 1)[, c("mpg", "cyl", "disp")]) |>
    gt::tab_spanner(label = "Engine", columns = c(cyl, disp))
  kw <- rtfreporter:::.gt_to_rtftable_kwargs(g)
  expect_true(is.list(kw$col_header))
  # last row is the leaf labels; an upper row carries the spanner cell.
  bottom <- kw$col_header[[length(kw$col_header)]]
  expect_true(is.character(bottom))
})

test_that("px widths -> column_widths_twips; pct -> col_rel_width", {
  skip_if_not_installed("gt")
  gpx <- gt::gt(head(mtcars, 1)[, c("mpg", "cyl")]) |>
    gt::cols_width(mpg ~ gt::px(100), cyl ~ gt::px(50))
  kpx <- rtfreporter:::.gt_to_rtftable_kwargs(gpx)
  expect_identical(kpx$column_widths_twips, c(1500L, 750L))   # 1px = 15 twips
})

test_that("titles + source notes become page-level blocks", {
  skip_if_not_installed("gt")
  g  <- gt::gt(head(mtcars, 1)[, "mpg", drop = FALSE]) |>
    gt::tab_header(title = "T", subtitle = "Sub") |>
    gt::tab_source_note("Source: x")
  kw <- rtfreporter:::.gt_to_rtftable_kwargs(g)
  expect_identical(kw$titles_block, c("T", "Sub"))
  expect_true("Source: x" %in% kw$footnotes_block)
})

# ── footnote marks ────────────────────────────────────────────────────────────

test_that(".convert_footnote_marks() rewrites gt's <sup> marks to ^{N}", {
  df <- data.frame(
    a = c('<span class="gt_footnote_marks"><sup>1</sup></span> 21', "x"),
    stringsAsFactors = FALSE)
  out <- rtfreporter:::.convert_footnote_marks(df)
  expect_match(out$a[1L], "\\^\\{1\\}")
  expect_false(grepl("gt_footnote_marks|<sup>", out$a[1L]))
  expect_identical(out$a[2L], "x")
})

test_that("gt in-cell footnote marks survive as ^{N} through the adapter", {
  skip_if_not_installed("gt")
  g  <- gt::gt(head(mtcars, 1)[, c("mpg", "cyl")]) |>
    gt::tab_footnote("note", gt::cells_body(rows = 1, columns = mpg))
  kw <- rtfreporter:::.gt_to_rtftable_kwargs(g)
  expect_match(kw$data[[1L]][1L], "\\^\\{1\\}")
})

# ── read_meta semantics ───────────────────────────────────────────────────────

test_that("read_meta = FALSE yields a clean body but no metadata", {
  skip_if_not_installed("gt")
  g  <- gt::gt(head(mtcars, 1)[, c("mpg", "cyl")]) |>
    gt::cols_label(mpg = "MPG") |>
    gt::tab_header(title = "T")
  kw <- rtfreporter:::.gt_to_rtftable_kwargs(g, tokens = character(0))
  expect_false(is.null(kw$data))
  expect_null(kw$col_header)
  expect_null(kw$col_spec)
  expect_null(kw$titles_block)
})

# ── end-to-end ────────────────────────────────────────────────────────────────

test_that("as_rtftable(gt) returns an rtftable with gt labels + align", {
  skip_if_not_installed("gt")
  g  <- gt::gt(head(mtcars, 2)[, c("mpg", "cyl")]) |>
    gt::cols_label(mpg = "MPG", cyl = "Cyl") |>
    gt::cols_align("right")
  tbl <- as_rtftable(g)
  expect_s3_class(tbl, "rtftable")
  expect_identical(unlist(tbl$col_header[[1L]]), c("MPG", "Cyl"))
  expect_identical(tbl$col_spec[[1L]]$align, "right")
})

test_that("as_rtftable(gt, read_meta = FALSE) ignores gt metadata", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 2)[, c("mpg", "cyl")]) |>
    gt::cols_label(mpg = "MPG") |>
    gt::tab_header(title = "T")
  tbl <- as_rtftable(g, read_meta = FALSE)
  expect_null(tbl$col_header)
  expect_null(attr(tbl, "rtf_titles"))
})

test_that("explicit args win over gt-extracted values", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 1)[, "mpg", drop = FALSE]) |>
    gt::cols_label(mpg = "MPG")
  tbl <- as_rtftable(g, col_header = "Override")
  expect_identical(tbl$col_header[[1L]], "Override")
})

# ── multi-stub fallback (gt::extract_body() can't read >1 stub column) ─────────
#
# tfrmt's row_grp_plan(label_loc = element_row_grp_loc(location = "column"))
# marks BOTH the group and label columns as stub.  gt::extract_body() then errors
# "the condition has length > 1" (its `if (is.na(rowname_col))` sees a length-2
# stub var).  The adapter detects that and reads the body from `_data` +
# `_boxhead` instead, naming the first stub "::rowname::".

# Build a two-stub tfrmt gt (label_loc = "column").
.mk_two_stub_gt <- function() {
  dat <- data.frame(group = c("Age", "Age", "Sex", "Sex"),
                    label = c("n", "Mean", "Male", "Female"),
                    column = "Trt A", param = "v", value = 1:4,
                    stringsAsFactors = FALSE)
  tf <- tfrmt::tfrmt(
    group = group, label = label, column = column, param = param, value = value,
    row_grp_plan = tfrmt::row_grp_plan(
      label_loc = tfrmt::element_row_grp_loc(location = "column")),
    body_plan = tfrmt::body_plan(
      tfrmt::frmt_structure(".default", ".default", tfrmt::frmt("xx"))))
  tfrmt::print_to_gt(tf, dat)
}

test_that("a two-stub gt actually breaks gt::extract_body() (the bug we handle)", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tfrmt")
  g <- .mk_two_stub_gt()
  expect_gt(sum(as.character(g[["_boxhead"]]$type) == "stub"), 1L)
  expect_error(gt::extract_body(g, output = "html"), "length > 1")
})

test_that(".gt_extract_body_safe() reads a two-stub body via the _data fallback", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tfrmt")
  g  <- .mk_two_stub_gt()
  eb <- rtfreporter:::.gt_extract_body_safe(g)
  # first stub renamed to the extract_body() sentinel; both stub cols kept,
  # hidden helper column dropped.
  expect_identical(names(eb)[1L], "::rowname::")
  expect_false(any(grepl("tfrmt", names(eb))))
  expect_equal(nrow(eb), 4L)
  expect_equal(as.character(eb[[1L]]), c("Age", "Age", "Sex", "Sex"))
  expect_equal(as.character(eb[[2L]]), c("n", "Mean", "Male", "Female"))
})

test_that("as_rtftable() reads a label_loc='column' tfrmt gt (was an error)", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tfrmt")
  g  <- .mk_two_stub_gt()
  rt <- expect_no_error(as_rtftable(g, read_meta = TRUE))
  expect_s3_class(rt, "rtftable")
  expect_equal(ncol(rt$data), 3L)            # group + label + data column
  expect_equal(nrow(rt$data), 4L)
})

test_that("the multi-stub fallback does not disturb single-stub tables", {
  skip_if_not_installed("gt")
  # a normal single-stub gt still goes through gt::extract_body()
  g  <- gt::gt(head(mtcars, 3)[, c("mpg", "cyl")], rownames_to_stub = TRUE) |>
    gt::fmt_number(mpg, decimals = 1)
  eb <- rtfreporter:::.gt_extract_body_safe(g)
  expect_identical(eb, gt::extract_body(g, output = "html"))
})

# ──────── #458: source column names survive the adapter verbatim ────────────

.gt_odd_names <- function() {
  data.frame(
    Characteristic  = c("Age, Mean (SD)", "Sex, n (%)"),
    `Drug A (N=60)` = c("54.2 (11.3)", "31 (51.7)"),
    `Drug B (N=58)` = c("56.8 (10.1)", "27 (46.6)"),
    `2024 total`    = c("112", "58"),
    check.names = FALSE, stringsAsFactors = FALSE)
}

test_that("non-syntactic gt column names reach the pages unchanged", {
  skip_if_not_installed("gt")
  src   <- .gt_odd_names()
  pages <- as_rtftables(gt::gt(src))
  # spaces, parentheses, '=' and a leading digit all survive
  expect_identical(names(pages[[1]]$data), names(src))
  expect_identical(rtf_columns(pages), names(src))
})

test_that("a non-syntactic name can be addressed by name", {
  skip_if_not_installed("gt")
  pages <- as_rtftables(gt::gt(.gt_odd_names()))

  out <- set_col_header(pages, c(`Drug A (N=60)` = "Drug A"))
  m   <- header_map(out[[1]])
  expect_true("Drug A" %in% m$text)

  expect_no_error(
    set_col_header(pages, list(col_cell(col_key("Drug A (N=60)"), "A"))))

  dropped <- as_rtftables(gt::gt(.gt_odd_names()), drop_cols = "2024 total")
  expect_false("2024 total" %in% rtf_columns(dropped))
})

test_that("the rendered header is unaffected either way", {
  skip_if_not_installed("gt")
  pages <- as_rtftables(gt::gt(.gt_odd_names()))
  # display labels come from col_header, not from the data names
  expect_true(all(names(.gt_odd_names()) %in% header_map(pages[[1]])$text))
})

test_that("make.unique() still separates a stub from a real `rowname` column", {
  skip_if_not_installed("gt")
  src <- data.frame(grp = c("A", "B"), rowname = c("x", "y"),
                    v = c("1", "2"), stringsAsFactors = FALSE)
  pages <- as_rtftables(gt::gt(src, rowname_col = "grp"))
  nm <- names(pages[[1]]$data)
  expect_identical(anyDuplicated(nm), 0L)
  expect_true(all(c("rowname", "rowname.1") %in% nm))
})
