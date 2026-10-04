# Spanning-row alignment inherits from the level below.

local_deprecated("spanning_header")  # deprecated in 0.8.x

.render_tbl <- function(tbl) {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(tbl))
  .render_to_string(doc)
}

test_that("standalone spanning_header inherits right alignment from col_spec", {
  df <- data.frame(Item = "Age",
                   A_N = 30L, A_Mean = 45.2,
                   B_N = 30L, B_Mean = 46.1,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    col_header = c("Item", "N", "Mean", "N", "Mean"),
    col_spec   = list(
      list(col = 1, align = "left"),
      list(col = 2, align = "right"),
      list(col = 3, align = "right"),
      list(col = 4, align = "right"),
      list(col = 5, align = "right")
    ),
    spanning_header = list(
      list(from = 2, to = 3, label = "Drug A (N=30)", underline = TRUE),
      list(from = 4, to = 5, label = "Drug B (N=30)", underline = TRUE)
    ))
  txt <- .render_tbl(tbl)
  expect_match(txt, "\\\\qr\\\\li0\\\\ri0 \\\\ul Drug A \\(N=30\\)")
  expect_match(txt, "\\\\qr\\\\li0\\\\ri0 \\\\ul Drug B \\(N=30\\)")
})

test_that("inline spanning row inside col_header inherits alignment", {
  df <- data.frame(Item = "Age",
                   A_N = 30L, A_Mean = 45.2,
                   B_N = 30L, B_Mean = 46.1,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    col_header = list(
      list(
        list(from = 2, to = 3, label = "Drug A", underline = TRUE),
        list(from = 4, to = 5, label = "Drug B", underline = TRUE)
      ),
      c("Item", "N", "Mean", "N", "Mean")
    ),
    col_spec = list(
      list(col = 1, align = "left"),
      list(col = 2, align = "right"),
      list(col = 3, align = "right"),
      list(col = 4, align = "right"),
      list(col = 5, align = "right")
    ))
  txt <- .render_tbl(tbl)
  expect_match(txt, "\\\\qr\\\\li0\\\\ri0 \\\\ul Drug A")
  expect_match(txt, "\\\\qr\\\\li0\\\\ri0 \\\\ul Drug B")
})

test_that("a spanning cell takes the leftmost covered column's alignment", {
  df <- data.frame(Item = "X", L = "a", C = "b", R = 1L,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    col_header = c("Item", "L", "C", "R"),
    col_spec = list(
      list(col = 2, align = "left"),
      list(col = 3, align = "center"),
      list(col = 4, align = "right")
    ),
    spanning_header = list(
      list(from = 2, to = 4, label = "Mixed", underline = TRUE)
    ))
  txt <- .render_tbl(tbl)
  expect_match(txt, "\\\\ql\\\\li0\\\\ri0 \\\\ul Mixed")
})

test_that("explicit sp$align overrides the inheritance", {
  df <- data.frame(Item = "Age",
                   A_N = 30L, A_Mean = 45.2,
                   B_N = 30L, B_Mean = 46.1,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    col_header = c("Item", "N", "Mean", "N", "Mean"),
    col_spec   = list(
      list(col = 1, align = "left"),
      list(col = 2, align = "right"),
      list(col = 3, align = "right"),
      list(col = 4, align = "right"),
      list(col = 5, align = "right")
    ),
    spanning_header = list(
      list(from = 2, to = 3, label = "Forced Center", underline = TRUE,
           align = "center")
    ))
  txt <- .render_tbl(tbl)
  expect_match(txt, "\\\\qc\\\\li0\\\\ri0 \\\\ul Forced Center")
})

test_that("spanning cell inherits alignment when col_spec is not user-supplied", {
  df <- data.frame(a = 1L, b = 2L, c = 3L)
  tbl <- rtftable(df,
    spanning_header = list(
      list(from = 1, to = 3, label = "All", underline = TRUE)
    ))
  txt <- .render_tbl(tbl)
  # Default col_spec align = "left" → spanning inherits "left".
  expect_match(txt, "\\\\ql\\\\li0\\\\ri0 \\\\ul All")
})
