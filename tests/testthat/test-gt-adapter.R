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

test_that("gt row groups are heading rows, their rows indented under them (#650)", {
  skip_if_not_installed("gt")
  ae <- data.frame(SOC = c("Nervous", "Nervous", "GI"), PT = c("Headache", "Dizziness", "Nausea"),
                   Placebo = c("1", "2", "3"), Active = c("4", "5", "6"), stringsAsFactors = FALSE)
  g <- gt::gt(ae, groupname_col = "SOC", rowname_col = "PT") |>
    gt::tab_spanner(label = "Arm", columns = c(Placebo, Active))
  tbl <- as_rtftable(g)
  nb2 <- strrep(intToUtf8(160L), 2L)
  expect_false("::group_id::" %in% names(tbl$data))
  expect_identical(tbl$data[[1L]],
                   c("Nervous", paste0(nb2, c("Headache", "Dizziness")), "GI", paste0(nb2, "Nausea")))
  expect_identical(tbl$data$Placebo, c("", "1", "2", "", "3"))
  # the spanner is over the two arm columns, after the stub
  sp <- tbl$col_header[[1L]]
  arm <- Filter(function(cell) identical(cell$label, "Arm"), sp)[[1L]]
  expect_identical(c(arm$from, arm$to), c(2L, 3L))
  # a table without groups is as it was
  plain <- as_rtftable(gt::gt(ae[c("PT", "Placebo")]))
  expect_identical(plain$data$PT, ae$PT)
})

# ── footnote marks on the labels and the footnote lines (#1020) ──────────────

# The RTF of `x` (gt / gtsummary) through as_rtftables() + generate_rtfreport().
.t71_rtf <- function(x, ...) {
  d <- tempfile("t71_")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "t71.rtf")
  generate_rtfreport(rtf_tables(rtf_document(), as_rtftables(x, ...)), f,
                     overwrite = TRUE)
  readLines(f, warn = FALSE)
}

test_that("FT-TB7101 [FR-TB577]: gt's footnote marks on a column label, a spanner and a body cell, and the footnote lines starting with their marks in gt's order", {
  skip_if_not_installed("gt")
  df <- data.frame(subj = sprintf("S%02d", 1:4), age = c(34, 45, 40, 51),
                   sex = c("F", "M", "F", "M"), stringsAsFactors = FALSE)
  g <- gt::gt(df) |>
    gt::tab_spanner("Demo", columns = c(age, sex)) |>
    gt::tab_footnote("Imputed.", locations = gt::cells_body(columns = age, rows = 2)) |>
    gt::tab_footnote("Age at consent.", locations = gt::cells_column_labels(age)) |>
    gt::tab_footnote("Spanner note.", locations = gt::cells_column_spanners("Demo")) |>
    gt::tab_footnote("General note.")
  p <- as_rtftables(g)[[1L]]
  # gt numbers spanner (1), label (2), body (3); the note without a place
  # comes first, unmarked, as gt prints it
  expect_identical(p$col_header[[1L]][[2L]]$label, "Demo^{1}")
  expect_identical(p$col_header[[2L]], c("subj", "age^{2}", "sex"))
  expect_identical(p$data$age[2L], "^{3}\u00a045")
  expect_identical(attr(p, "rtf_footnotes"),
                   c("General note.", "^{1} Spanner note.",
                     "^{2} Age at consent.", "^{3} Imputed."))
  rtf <- .t71_rtf(g)
  expect_true(any(grepl("Demo{\\super 1}\\cell", rtf, fixed = TRUE)))
  expect_true(any(grepl("age{\\super 2}\\cell", rtf, fixed = TRUE)))
  expect_true(any(grepl("{\\super 3}\\u160?45\\cell", rtf, fixed = TRUE)))
  notes <- grep("note\\.|consent\\.|Imputed\\.", rtf, value = TRUE)
  expect_identical(sub("^.*\\\\ri0 (.*)\\\\cell\\\\row$", "\\1", notes),
                   c("General note.", "{\\super 1} Spanner note.",
                     "{\\super 2} Age at consent.", "{\\super 3} Imputed."))
  # two notes on one label: the marks joined, as gt prints "a,b"
  g2 <- gt::gt(df) |>
    gt::tab_options(footnotes.marks = "letters") |>
    gt::tab_footnote("A", locations = gt::cells_column_labels(c(age, sex))) |>
    gt::tab_footnote("B", locations = gt::cells_column_labels(age))
  p2 <- as_rtftables(g2)[[1L]]
  expect_identical(p2$col_header, list(c("subj", "age^{a,b}", "sex^{a}")))
  expect_identical(attr(p2, "rtf_footnotes"), c("^{a} A", "^{b} B"))
})

test_that("FT-TB7102 [FR-TB577]: a gtsummary tbl_summary(by = ) header carries its footnote mark after N = n, and the note line starts with it", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  ts <- gtsummary::tbl_summary(gtsummary::trial[c("trt", "age", "grade")],
                               by = "trt")
  p <- as_rtftables(ts)[[1L]]
  expect_identical(p$col_header,
                   list(c("Characteristic", "Drug A  \nN = 98^{1}", "Drug B  \nN = 102^{1}")))
  expect_identical(attr(p, "rtf_footnotes"), "^{1} Median (Q1, Q3); n (%)")
  rtf <- .t71_rtf(ts)
  expect_true(any(grepl("Drug A  \\line N = 98{\\super 1}\\cell", rtf, fixed = TRUE)))
  expect_true(any(grepl("{\\super 1} Median (Q1, Q3); n (%)\\cell", rtf, fixed = TRUE)))
})

test_that("FT-TB7104 [FR-TB577]: gt's footnote marks on the title, the subtitle and a row-group label match their footnote lines", {
  skip_if_not_installed("gt")
  df <- data.frame(grp = c("A", "A", "B", "B"), subj = sprintf("S%02d", 1:4),
                   age = c(34, 45, 40, 51), stringsAsFactors = FALSE)
  g <- gt::gt(df, groupname_col = "grp", rowname_col = "subj") |>
    gt::tab_header("Title T", "Sub S") |>
    gt::tab_footnote("Grp B.", locations = gt::cells_row_groups("B")) |>
    gt::tab_footnote("Ttl.", locations = gt::cells_title("title")) |>
    gt::tab_footnote("Sub.", locations = gt::cells_title("subtitle")) |>
    gt::tab_footnote("Cell 1.", locations = gt::cells_body(columns = age, rows = 1))
  p <- as_rtftables(g)[[1L]]
  # gt numbers title (1), subtitle (2), body row 1 (3), group B (4)
  expect_identical(attr(p, "rtf_titles"), c("Title T^{1}", "Sub S^{2}"))
  nb2 <- strrep(intToUtf8(160L), 2L)
  expect_identical(p$data[[1L]],
                   c("A", paste0(nb2, c("S01", "S02")), "B^{4}", paste0(nb2, c("S03", "S04"))))
  expect_identical(attr(p, "rtf_footnotes"),
                   c("^{1} Ttl.", "^{2} Sub.", "^{3} Cell 1.", "^{4} Grp B."))
  rtf <- .t71_rtf(g)
  expect_true(any(grepl("Title T{\\super 1}", rtf, fixed = TRUE)))
  expect_true(any(grepl("Sub S{\\super 2}", rtf, fixed = TRUE)))
  expect_true(any(grepl("B{\\super 4}\\cell", rtf, fixed = TRUE)))
  expect_true(any(grepl("{\\super 4} Grp B.", rtf, fixed = TRUE)))
  # the same as gt prints them
  h <- as.character(gt::as_raw_html(g, inline_css = FALSE))
  sup <- function(x) {
    regmatches(h, regexpr(paste0(x, "<span[^>]*><sup>[^<]*</sup>"), h))
  }
  expect_match(sup("Title T"), "<sup>1</sup>", fixed = TRUE)
  expect_match(sup("Sub S"), "<sup>2</sup>", fixed = TRUE)
  expect_match(sup("B"), "<sup>4</sup>", fixed = TRUE)
  # without "footnote_marks" the title and group rows stay bare
  q <- as_rtftables(g, read_meta = c("titles", "footnotes"))[[1L]]
  expect_identical(attr(q, "rtf_titles"), c("Title T", "Sub S"))
  expect_identical(q$data[[1L]][4L], "B")
})

test_that("FT-TB7103 [FR-TB577 FR-TB118]: read_meta without \"footnote_marks\" writes neither the label marks nor the line marks", {
  skip_if_not_installed("gt")
  df <- data.frame(subj = sprintf("S%02d", 1:4), age = c(34, 45, 40, 51),
                   stringsAsFactors = FALSE)
  g <- gt::gt(df) |>
    gt::tab_spanner("Demo", columns = age) |>
    gt::tab_footnote("Imputed.", locations = gt::cells_body(columns = age, rows = 2)) |>
    gt::tab_footnote("Age at consent.", locations = gt::cells_column_labels(age)) |>
    gt::tab_footnote("Spanner note.", locations = gt::cells_column_spanners("Demo"))
  p <- as_rtftables(g, read_meta = c("col_header", "spanning", "footnotes"))[[1L]]
  expect_identical(p$col_header[[1L]][[2L]]$label, "Demo")
  expect_identical(p$col_header[[2L]], c("subj", "age"))
  expect_identical(attr(p, "rtf_footnotes"),
                   c("Imputed.", "Age at consent.", "Spanner note."))
  rtf <- .t71_rtf(g, read_meta = c("col_header", "spanning", "footnotes"))
  expect_false(any(grepl("Demo{\\super", rtf, fixed = TRUE)))
  expect_false(any(grepl("age{\\super", rtf, fixed = TRUE)))
  expect_false(any(grepl("\\{\\\\super [0-9]\\} (Imputed|Age at|Spanner)", rtf)))
  # the token alone, with the labels and the notes read
  q <- as_rtftables(g, read_meta = c("col_header", "spanning", "footnotes",
                                     "footnote_marks"))[[1L]]
  expect_identical(q$col_header[[2L]], c("subj", "age^{2}"))
  expect_identical(attr(q, "rtf_footnotes"),
                   c("^{1} Spanner note.", "^{2} Age at consent.", "^{3} Imputed."))
})

# ── a tab_caption() / modify_caption() caption is the title (#1029) ──────────

# The RTF of a document, through generate_rtfreport().
.t73_rtf <- function(doc) {
  d <- tempfile("t73_")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "t73.rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

# The first line of `rtf` holding `txt` (NA: none).
.t73_at <- function(rtf, txt) {
  i <- which(grepl(txt, rtf, fixed = TRUE))
  if (length(i)) i[[1L]] else NA_integer_
}

test_that("FT-TB7301 [FR-TB577]: a gt tab_caption() caption is the page title", {
  skip_if_not_installed("gt")
  g <- gt::gt(data.frame(A = 1:2)) |> gt::tab_caption("Table 9 Summary")
  p <- as_rtftables(g)
  expect_identical(attr(p[[1L]], "rtf_titles"), "Table 9 Summary")
  rtf <- .t73_rtf(rtf_tables(rtf_document(), p))
  expect_false(is.na(.t73_at(rtf, "Table 9 Summary")))
  # above the table's header row
  expect_lt(.t73_at(rtf, "Table 9 Summary"), .t73_at(rtf, "A\\cell"))
  # without the "titles" token it is not read
  q <- as_rtftables(g, read_meta = c("col_header", "footnotes"))
  expect_null(attr(q[[1L]], "rtf_titles"))
})

test_that("FT-TB7302 [FR-TB577]: a gtsummary modify_caption() caption is the title, its markdown ** markers dropped", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  ts <- gtsummary::tbl_summary(gtsummary::trial[c("trt", "age")], by = "trt") |>
    gtsummary::modify_caption("**Table 9** Summary")
  p <- as_rtftables(ts)
  expect_identical(attr(p[[1L]], "rtf_titles"), "Table 9 Summary")
  rtf <- .t73_rtf(rtf_tables(rtf_document(), p))
  expect_false(is.na(.t73_at(rtf, "Table 9 Summary")))
  expect_false(any(grepl("**", rtf, fixed = TRUE)))
})

test_that("FT-TB7303 [FR-TB577]: a caption with a tab_header() title and subtitle: the caption first, then the title, then the subtitle, as gt prints them", {
  skip_if_not_installed("gt")
  g <- gt::gt(data.frame(A = 1:2)) |>
    gt::tab_header("Title T", "Sub S") |>
    gt::tab_caption("Cap C") |>
    gt::tab_footnote("Ttl.", locations = gt::cells_title("title"))
  p <- as_rtftables(g)[[1L]]
  # the title's footnote mark stays where #1020 writes it
  expect_identical(attr(p, "rtf_titles"), c("Cap C", "Title T^{1}", "Sub S"))
  rtf <- .t73_rtf(rtf_tables(rtf_document(), list(p)))
  at <- vapply(c("Cap C", "Title T{\\super 1}", "Sub S"),
               function(t) .t73_at(rtf, t), integer(1L))
  expect_false(anyNA(at))
  expect_true(all(diff(at) > 0L))
  # gt prints the caption above the header too
  h <- as.character(gt::as_raw_html(g, inline_css = FALSE))
  expect_lt(regexpr("Cap C", h, fixed = TRUE), regexpr("Title T", h, fixed = TRUE))
})

test_that("FT-TB7304 [FR-TB577 FR-TB174]: titles the user gives win, and the caption is not written, as for the gt title", {
  skip_if_not_installed("gt")
  g <- gt::gt(data.frame(A = 1:2)) |>
    gt::tab_header("Title T") |>
    gt::tab_caption("Cap C")
  # rtf_tables(titles = )
  rtf <- .t73_rtf(rtf_tables(rtf_document(), as_rtftables(g),
                             titles = list("Mine M")))
  expect_false(is.na(.t73_at(rtf, "Mine M")))
  expect_true(is.na(.t73_at(rtf, "Cap C")))
  expect_true(is.na(.t73_at(rtf, "Title T")))
  # rtf_titles()
  doc <- rtf_titles(rtf_tables(rtf_document(), as_rtftables(g)), "Ours O")
  rtf <- .t73_rtf(doc)
  expect_false(is.na(.t73_at(rtf, "Ours O")))
  expect_true(is.na(.t73_at(rtf, "Cap C")))
  expect_true(is.na(.t73_at(rtf, "Title T")))
  # none given: the caption and the title are the page's
  rtf <- .t73_rtf(rtf_tables(rtf_document(), as_rtftables(g)))
  expect_lt(.t73_at(rtf, "Cap C"), .t73_at(rtf, "Title T"))
})

test_that("FT-TB7305 [FR-TB577 FR-TB115]: each member of a gt_split carries the caption, read the same way", {
  skip_if_not_installed("gt")
  g <- gt::gt(data.frame(A = 1:4)) |>
    gt::tab_header("Title T") |>
    gt::tab_caption(gt::md("**Table 9** Summary"))
  pages <- as_rtftables(gt::gt_split(g, row_every_n = 2))
  expect_length(pages, 2L)
  for (p in pages) {
    expect_identical(attr(p, "rtf_titles"), c("Table 9 Summary", "Title T"))
  }
  rtf <- .t73_rtf(rtf_tables(rtf_document(), pages))
  expect_identical(sum(grepl("Table 9 Summary", rtf, fixed = TRUE)), 2L)
  expect_false(any(grepl("**", rtf, fixed = TRUE)))
})

# ──────── #1024: a stub column is headed by gt's stubhead, not its variable ───

.t75_dat <- function() {
  dat <- expand.grid(grp = c("Age", "Weight"), lbl = c("n", "Mean"),
                     col = c("Placebo", "Active"), stringsAsFactors = FALSE)
  dat$param <- dat$lbl
  dat$value <- seq_len(nrow(dat)) * 1.5
  dat[order(dat$grp, dat$lbl), ]
}

# A tfrmt table with label_loc = "column": two stub columns, grp and lbl.
.t75_gt <- function(...) {
  tf <- tfrmt::tfrmt(
    group = grp, label = lbl, column = col, param = param, value = value,
    body_plan = tfrmt::body_plan(tfrmt::frmt_structure(
      group_val = ".default", label_val = ".default", tfrmt::frmt("xx.x"))),
    row_grp_plan = tfrmt::row_grp_plan(
      label_loc = tfrmt::element_row_grp_loc(location = "column")),
    ...)
  tfrmt::print_to_gt(tf, .t75_dat())
}

# The text of gt's stubhead cells (<th id="a::stub...">), as gt prints them.
.t75_gt_stubhead <- function(g) {
  html <- as.character(gt::as_raw_html(g, inline_css = FALSE))
  th <- regmatches(html, gregexpr("<th[^>]*id=\"a::stub[^\"]*\"[^>]*>.*?</th>",
                                  html))[[1L]]
  span <- as.integer(sub(".*colspan=\"([0-9]+)\".*", "\\1", th))
  text <- gsub("<[^>]+>", "", sub("^<th[^>]*>", "", th))
  list(text = text, colspan = span)
}

# The RTF header row: the line holding the column labels.
.t75_header_row <- function(rtf, label) {
  grep(paste0(label, "\\cell"), rtf, fixed = TRUE, value = TRUE)[1L]
}

test_that("FT-TB7501 [FR-TB582]: a tfrmt label_loc = \"column\" table with no stubhead: both stub columns are headed empty, as gt prints them, not by the variable name", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tfrmt")
  g <- .t75_gt()
  # gt prints nothing over the stub columns
  expect_identical(.t75_gt_stubhead(g)$text, c("", ""))
  p <- as_rtftables(g)[[1L]]
  expect_identical(p$col_header, list(c("", "", "Placebo", "Active")))
  # the column itself keeps its name; only its header is not the name
  expect_identical(names(p$data), c("rowname", "lbl", "Placebo", "Active"))
  rtf <- .t71_rtf(g)
  hdr <- .t75_header_row(rtf, "Placebo")
  expect_false(grepl(" lbl\\cell", hdr, fixed = TRUE))
  expect_false(grepl(" grp\\cell", hdr, fixed = TRUE))
  expect_match(hdr, "\\ql\\li0\\ri0 \\cell\\ql\\li0\\ri0 \\cell\\qc\\li0\\ri0 Placebo",
               fixed = TRUE)
})

test_that("FT-TB7502 [FR-TB582]: a tfrmt label_loc = \"column\" table is read from _data / _boxhead, its stub columns as ordinary columns headed by the stubhead as gt prints it", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tfrmt")
  g <- .t75_gt()
  # more than one stub column, which gt::extract_body() cannot read
  expect_identical(sum(as.character(g[["_boxhead"]]$type) == "stub"), 2L)
  expect_error(gt::extract_body(g, output = "html"))
  p <- as_rtftables(g)[[1L]]
  # read from _data: the group and label columns as ordinary columns
  expect_identical(p$data[[1L]], c("Age", "Age", "Weight", "Weight"))
  expect_identical(p$data[[2L]], c("Mean", "n", "Mean", "n"))
  expect_identical(p$data[[3L]], c(" 4.5", " 1.5", " 6.0", " 3.0"))
  # no stubhead: both headed empty, never by the variable name
  expect_identical(p$col_header, list(c("", "", "Placebo", "Active")))
  # one stubhead label per stub column (tfrmt's col_plan() renaming them):
  # each column takes its own, as gt prints them
  g2 <- .t75_gt(col_plan = tfrmt::col_plan(Parameter = grp, Statistic = lbl,
                                           Placebo, Active))
  sh <- .t75_gt_stubhead(g2)
  expect_identical(sh$text, c("Parameter", "Statistic"))
  expect_identical(sh$colspan, c(1L, 1L))
  p2 <- as_rtftables(g2)[[1L]]
  expect_identical(p2$col_header,
                   list(c("Parameter", "Statistic", "Placebo", "Active")))
  expect_match(.t75_header_row(.t71_rtf(g2), "Placebo"),
               "\\ql\\li0\\ri0 Parameter\\cell\\ql\\li0\\ri0 Statistic\\cell",
               fixed = TRUE)
  # one stubhead label (tab_stubhead()): gt prints it once over both stub
  # columns; it heads the first and the second is empty
  g3 <- gt::tab_stubhead(g, "Parameter")
  sh <- .t75_gt_stubhead(g3)
  expect_identical(sh$text, "Parameter")
  expect_identical(sh$colspan, 2L)
  p3 <- as_rtftables(g3)[[1L]]
  expect_identical(p3$col_header, list(c("Parameter", "", "Placebo", "Active")))
  hdr <- .t75_header_row(.t71_rtf(g3), "Placebo")
  expect_match(hdr, "\\ql\\li0\\ri0 Parameter\\cell\\ql\\li0\\ri0 \\cell",
               fixed = TRUE)
  expect_false(grepl(" lbl\\cell", hdr, fixed = TRUE))
})

test_that("FT-TB7503 [FR-TB577]: a gt row_group_as_column table: the stub column is headed by tab_stubhead(), or empty, not by the variable name", {
  skip_if_not_installed("gt")
  df <- data.frame(arm = c("A", "A", "B", "B"), stat = c("n", "Mean", "n", "Mean"),
                   v = c("10", "2.5", "12", "3.1"), stringsAsFactors = FALSE)
  g <- gt::gt(df, rowname_col = "stat", groupname_col = "arm",
              row_group_as_column = TRUE)
  p <- as_rtftables(g)[[1L]]
  expect_identical(.t75_gt_stubhead(g)$text, "")
  expect_identical(p$col_header, list(c("", "v")))
  # the groups are read as group rows, the stub as the first column, indented
  # by two non-breaking spaces
  nb <- strrep(intToUtf8(160L), 2L)
  expect_identical(p$data[[1L]], c("A", paste0(nb, c("n", "Mean")), "B",
                                   paste0(nb, c("n", "Mean"))))
  g2 <- gt::tab_stubhead(g, "Statistic")
  expect_identical(.t75_gt_stubhead(g2)$text, "Statistic")
  p2 <- as_rtftables(g2)[[1L]]
  expect_identical(p2$col_header, list(c("Statistic", "v")))
  hdr <- .t75_header_row(.t71_rtf(g2), "v")
  expect_match(hdr, "\\ql\\li0\\ri0 Statistic\\cell", fixed = TRUE)
  expect_false(grepl(" stat\\cell", hdr, fixed = TRUE))
  expect_false(grepl(" arm\\cell", hdr, fixed = TRUE))
})
