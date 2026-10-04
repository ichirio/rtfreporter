# Column-header / spanning alignment and style — defaults, cascades, overrides.

local_deprecated("spanning_header")  # deprecated in 0.8.x

.render_tbl <- function(tbl) {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(tbl))
  .render_to_string(doc)
}

# Match the alignment marker (\q[lcr]) immediately before a header label.
.hdr_align_for <- function(txt, label) {
  pat <- sprintf("\\\\q([lcr])\\\\li\\d+\\\\ri\\d+ %s\\\\cell", label)
  m   <- regmatches(txt, regexpr(pat, txt))
  if (length(m) == 0L) return(NA_character_)
  sub(".*\\\\q([lcr]).*", "\\1", m)
}

# Match the alignment marker for a spanning cell (may carry decorations).
.span_align_for <- function(txt, label) {
  pat <- sprintf("\\\\q([lcr])\\\\li\\d+\\\\ri\\d+ (\\\\\\w+ )*%s", label)
  m <- regmatches(txt, regexpr(pat, txt))
  if (length(m) == 0L) return(NA_character_)
  sub("\\\\q([lcr]).*", "\\1", m)
}

# ──── Column-header alignment ────────────────────────────────────────────────

test_that("header alignment defaults to data column alignment", {
  df <- data.frame(L = "x", C = "y", R = 1L, stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    col_header = c("Left", "Center", "Right"),
    col_spec = list(
      list(col = 1, align = "left"),
      list(col = 2, align = "center"),
      list(col = 3, align = "right")
    ))
  txt <- .render_tbl(tbl)
  expect_identical(.hdr_align_for(txt, "Left"),   "l")
  expect_identical(.hdr_align_for(txt, "Center"), "c")
  expect_identical(.hdr_align_for(txt, "Right"),  "r")
})

test_that("col_header_align scalar applies to every column", {
  df <- data.frame(L = "x", C = "y", R = 1L, stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    col_header = c("A", "B", "C"),
    col_spec = list(
      list(col = 1, align = "left"),
      list(col = 3, align = "right")
    ),
    col_header_align = "center")
  txt <- .render_tbl(tbl)
  expect_identical(.hdr_align_for(txt, "A"), "c")
  expect_identical(.hdr_align_for(txt, "B"), "c")
  expect_identical(.hdr_align_for(txt, "C"), "c")
})

test_that("col_header_align vector applies per-column", {
  df <- data.frame(L = "x", C = "y", R = 1L, stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    col_header = c("A", "B", "C"),
    col_header_align = c("right", "left", "center"))
  txt <- .render_tbl(tbl)
  expect_identical(.hdr_align_for(txt, "A"), "r")
  expect_identical(.hdr_align_for(txt, "B"), "l")
  expect_identical(.hdr_align_for(txt, "C"), "c")
})

test_that("col_spec[[j]]$header_align beats col_header_align", {
  df <- data.frame(L = "x", C = "y", R = 1L, stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    col_header = c("A", "B", "C"),
    col_header_align = "center",
    col_spec = list(list(col = 2, header_align = "right")))
  txt <- .render_tbl(tbl)
  expect_identical(.hdr_align_for(txt, "A"), "c")
  expect_identical(.hdr_align_for(txt, "B"), "r")
  expect_identical(.hdr_align_for(txt, "C"), "c")
})

# ──── Column-header bold/italic ──────────────────────────────────────────────

test_that("column-header bold/italic default to FALSE", {
  df  <- data.frame(L = "x", C = "y", R = 1L, stringsAsFactors = FALSE)
  tbl <- rtftable(df, col_header = c("Plain", "Mid", "End"))
  txt <- .render_tbl(tbl)
  expect_false(grepl("\\\\b Plain\\\\b0", txt))
  expect_false(grepl("\\\\i Plain\\\\i0", txt))
})

test_that("header bold/italic can be enabled via col_spec", {
  df  <- data.frame(L = "x", C = "y", R = 1L, stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    col_header = c("X", "Y", "Z"),
    col_spec = list(list(col = 2, header_bold = TRUE, header_italic = TRUE)))
  txt <- .render_tbl(tbl)
  expect_true(grepl("\\\\b ", txt) && grepl("\\\\i ", txt))
})

# ──── Spanning alignment ─────────────────────────────────────────────────────

test_that("spanning alignment inherits from the level below (right-aligned data)", {
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
      list(from = 2, to = 3, label = "Drug A"),
      list(from = 4, to = 5, label = "Drug B")
    ))
  txt <- .render_tbl(tbl)
  expect_identical(.span_align_for(txt, "Drug A"), "r")
  expect_identical(.span_align_for(txt, "Drug B"), "r")
})

test_that("spanning alignment with mixed columns picks the leftmost", {
  df <- data.frame(L = "x", C = "y", R = 1L, stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    col_header = c("L", "C", "R"),
    col_spec = list(
      list(col = 1, align = "left"),
      list(col = 2, align = "center"),
      list(col = 3, align = "right")
    ),
    spanning_header = list(list(from = 1, to = 3, label = "All3")))
  txt <- .render_tbl(tbl)
  expect_identical(.span_align_for(txt, "All3"), "l")
})

test_that("explicit sp$align overrides the inherited spanning alignment", {
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
      list(from = 2, to = 3, label = "Override", align = "center")
    ))
  txt <- .render_tbl(tbl)
  expect_identical(.span_align_for(txt, "Override"), "c")
})

# ──── Spanning bold/italic/underline opt-ins ─────────────────────────────────

test_that("spanning bold/italic/underline default to FALSE", {
  df  <- data.frame(L = "x", C = "y", R = 1L, stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    spanning_header = list(list(from = 1, to = 3, label = "PureSpan")))
  txt <- .render_tbl(tbl)
  expect_false(grepl("\\\\b PureSpan",  txt))
  expect_false(grepl("\\\\i PureSpan",  txt))
  expect_false(grepl("\\\\ul PureSpan", txt))
})

test_that("spanning bold/italic/underline can be enabled per cell", {
  df  <- data.frame(L = "x", C = "y", R = 1L, stringsAsFactors = FALSE)

  tbl_b <- rtftable(df,
    spanning_header = list(list(from = 1, to = 3, label = "BoldSpan", bold = TRUE)))
  expect_match(.render_tbl(tbl_b), "\\\\b BoldSpan\\\\b0")

  tbl_i <- rtftable(df,
    spanning_header = list(list(from = 1, to = 3, label = "ItalSpan", italic = TRUE)))
  expect_match(.render_tbl(tbl_i), "\\\\i ItalSpan\\\\i0")

  tbl_u <- rtftable(df,
    spanning_header = list(list(from = 1, to = 3, label = "ULSpan", underline = TRUE)))
  expect_match(.render_tbl(tbl_u), "\\\\ul ULSpan\\\\ulnone")
})

# ──── Multi-row col_header ───────────────────────────────────────────────────

test_that("multi-row col_header: both spanning and label levels inherit alignment", {
  df <- data.frame(Item = "Age",
                   A_N = 30L, A_Mean = 45.2,
                   B_N = 30L, B_Mean = 46.1,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    col_header = list(
      list(
        list(from = 2, to = 3, label = "ArmA"),
        list(from = 4, to = 5, label = "ArmB")
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
  expect_identical(.span_align_for(txt, "ArmA"), "r")
  expect_identical(.span_align_for(txt, "ArmB"), "r")
  expect_identical(.hdr_align_for(txt, "Item"), "l")
  expect_identical(.hdr_align_for(txt, "N"),    "r")
  expect_identical(.hdr_align_for(txt, "Mean"), "r")
})
