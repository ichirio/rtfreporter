## tests/testthat/test-header-source.R
##
## rtf_header_source(): deparse a table's column header back to editable
## rtf_col_header() source (name-based, 3 verbosity levels, snippet wrapper,
## and the add_span_level scaffold).

library(testthat)

df5 <- function() {
  data.frame(Item = "x", A_N = 1L, A_M = 2.5, B_N = 3L, B_M = 4.5,
             stringsAsFactors = FALSE)
}
hdr5 <- function() {
  rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 5), "Treatment")),
    list(col_cell(1, ""), col_cell(c(2, 3), "Drug A"), col_cell(c(4, 5), "Drug B")),
    c("Item", "N", "Mean", "N", "Mean"))
}

# ── name-based output ──────────────────────────────────────────────────────

test_that("rtf_header_source() emits a name-based rtf_col_header()", {
  tbl <- rtftable(df5(), col_header = hdr5())
  out <- rtf_header_source(tbl, snippet = FALSE)
  expect_true(grepl("rtf_col_header(", out, fixed = TRUE))
  expect_true(grepl('col_cell(c("A_N", "B_M"), "Treatment")', out, fixed = TRUE))
  # empty gap cell kept
  expect_true(grepl('col_cell("Item", "")', out, fixed = TRUE))
  # label row keyed by column name
  expect_true(grepl('c(Item = "Item", A_N = "N"', out, fixed = TRUE))
})

test_that("non-syntactic column names are backtick-quoted in the label row", {
  df  <- data.frame(row_label = "x", `Grade 0` = 1L, Total = 2L, check.names = FALSE)
  tbl <- rtftable(df, col_header = c("Category", "G0", "Total"))
  out <- rtf_header_source(tbl, snippet = FALSE)
  expect_true(grepl('`Grade 0` = "G0"', out, fixed = TRUE))
})

# ── verbosity levels ───────────────────────────────────────────────────────

test_that("level controls how many defaults are shown", {
  tbl <- rtftable(df5(), col_header = hdr5())
  ex  <- rtf_header_source(tbl, level = "explicit", snippet = FALSE)
  de  <- rtf_header_source(tbl, level = "default",  snippet = FALSE)
  al  <- rtf_header_source(tbl, level = "all",      snippet = FALSE)

  expect_false(grepl("align =", ex, fixed = TRUE))      # explicit: nothing extra
  expect_true(grepl("align =", de, fixed = TRUE))       # default: effective align
  expect_false(grepl("bold = FALSE", de, fixed = TRUE)) # default: no FALSE flags
  expect_true(grepl("bold = FALSE", al, fixed = TRUE))  # all: FALSE flags shown
})

test_that("border default width shows only at level default/all", {
  tbl <- rtftable(df5(), col_header = rtf_col_header(
    list(col_cell(1, ""),
         col_cell(c(2, 5), "Trt",
                  border = rtf_border(bottom = rtf_border_side("single")))),
    c("Item", "N", "Mean", "N", "Mean")))
  ex <- rtf_header_source(tbl, level = "explicit", snippet = FALSE)
  al <- rtf_header_source(tbl, level = "all",      snippet = FALSE)
  expect_true(grepl('rtf_border_side("single")', ex, fixed = TRUE))
  expect_true(grepl('rtf_border_side("single", 15)', al, fixed = TRUE))
})

# ── snippet wrapper ────────────────────────────────────────────────────────

test_that("snippet wraps in set_col_header() with no leading `tbl |>`", {
  tbl <- rtftable(df5(), border = "tfl",
    col_header_align = c("left", "left", "center", "center", "center"),
    col_header = hdr5())
  out <- rtf_header_source(tbl, snippet = TRUE)
  expect_true(startsWith(out, "set_col_header("))
  expect_false(grepl("tbl |>", out, fixed = TRUE))
  expect_true(grepl("align = c(", out, fixed = TRUE))            # from col_spec
  expect_true(grepl("style_zone(header =", out, fixed = TRUE))   # header zone (tfl)
  expect_false(grepl("body =", out, fixed = TRUE))               # body zone excluded
})

# ── add_span_level scaffold ────────────────────────────────────────────────

test_that("add_span_level prepends a stub-kept, rest-spanned scaffold row", {
  tbl <- rtftable(
    data.frame(row_label = "x", g1 = 1, g2 = 2, g3 = 3, Total = 4),
    col_header = c("Category", "G1", "G2", "G3", "Total"))
  out <- rtf_header_source(tbl, snippet = FALSE, add_span_level = TRUE)
  # stub col 1 stays single/empty; the rest bundled under one empty span
  expect_true(grepl('list(col_cell("row_label", ""), col_cell(c("g1", "Total"), ""))',
                    out, fixed = TRUE))
  # original label row still present below
  expect_true(grepl('c(row_label = "Category", g1 = "G1"', out, fixed = TRUE))
})

test_that("add_span_level accepts a stub by name and errors on a bad stub", {
  tbl <- rtftable(data.frame(lbl = "x", a = 1, b = 2),
                  col_header = c("L", "A", "B"))
  out <- rtf_header_source(tbl, snippet = FALSE, add_span_level = TRUE, stub = "lbl")
  expect_true(grepl('col_cell("lbl", "")', out, fixed = TRUE))
  expect_true(grepl('col_cell(c("a", "b"), "")', out, fixed = TRUE))
  expect_error(rtf_header_source(tbl, add_span_level = TRUE, stub = "nope"),
               "must be valid column")
})

# ── round-trip fidelity ────────────────────────────────────────────────────

test_that("emitted source round-trips back to the same header via set_col_header()", {
  tbl <- rtftable(df5(), col_header = hdr5())
  src <- rtf_header_source(tbl, level = "explicit", snippet = FALSE)
  hdr <- eval(parse(text = src))
  rebuilt <- set_col_header(rtftable(df5()), hdr)
  expect_identical(rebuilt$col_header, tbl$col_header)
})

# ── list input + no-header fallback ────────────────────────────────────────

test_that("rtf_header_source() accepts a page list (first page)", {
  pages <- as_rtftables(data.frame(g = c("a", "b"), v = 1:2),
                        split = "by_value", group_col = "g")
  out <- rtf_header_source(pages, snippet = FALSE)
  expect_true(grepl("rtf_col_header(", out, fixed = TRUE))
})

test_that("a header-less table deparses names(data) as one label row", {
  tbl <- rtftable(data.frame(A = 1, B = 2))
  out <- rtf_header_source(tbl, snippet = FALSE)
  expect_true(grepl('c(A = "A", B = "B")', out, fixed = TRUE))
})
