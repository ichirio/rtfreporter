# Unified col_header API (v0.0.21+): col_cell(), rtf_col_header(),
# add_col_header_row(), and the pos = ... cell-spec format.

# ──────── col_cell() validation ───────────────────────────────────────────

test_that("col_cell() builds a tagged spec for a single column", {
  c1 <- col_cell(1, "Item")
  expect_s3_class(c1, "rtf_col_cell")
  expect_identical(c1$pos, 1L)
  expect_identical(c1$label, "Item")
})

test_that("col_cell() handles pos = c(start, end) spans", {
  c1 <- col_cell(c(2, 5), "Treatment", align = "center", underline = TRUE)
  expect_identical(c1$pos, c(2L, 5L))
  expect_identical(c1$label, "Treatment")
  expect_identical(c1$align, "center")
  expect_true(c1$underline)
})

test_that("col_cell() rejects malformed pos", {
  expect_error(col_cell(0),          "values must be >= 1")
  expect_error(col_cell(c(3, 2)),    "start must be <= end")
  expect_error(col_cell(c(1, 2, 3)), "length 1 or 2")
  expect_error(col_cell(c("a", "b", "c")), "length 1 or 2")
  expect_error(col_cell(""),         "non-empty")
})

test_that("col_cell() accepts column-name pos (resolved later)", {
  c1 <- col_cell("g1", "G1")
  expect_identical(c1$pos, "g1")
  c2 <- col_cell(c("g1", "g3"), "Drug A")
  expect_identical(c2$pos, c("g1", "g3"))
})

test_that("col_cell() rejects malformed align", {
  expect_error(col_cell(1, "X", align = "justify"),
               "align.*must be NULL")
})

# ──────── rtf_col_header() and add_col_header_row() ───────────────────────

test_that("rtf_col_header() collects rows top-to-bottom", {
  hdr <- rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 3), "Group")),
    c("Item", "A", "B")
  )
  expect_s3_class(hdr, "rtf_col_header")
  expect_length(hdr, 2L)
  expect_true(is.list(hdr[[1L]]))
  expect_true(is.character(hdr[[2L]]))
})

test_that("add_col_header_row() appends and prepends", {
  hdr <- rtf_col_header(c("Item", "N", "Mean", "N", "Mean"))
  hdr <- add_col_header_row(hdr,
                             list(col_cell(1, ""),
                                  col_cell(c(2, 3), "Drug A"),
                                  col_cell(c(4, 5), "Drug B")),
                             .position = "top")
  expect_length(hdr, 2L)
  expect_true(is.list(hdr[[1L]]))                   # spanning row on top
  expect_true(is.character(hdr[[2L]]))              # labels at bottom

  hdr <- add_col_header_row(hdr, c("x", "y", "z", "w", "v"))
  expect_length(hdr, 3L)
  expect_true(is.character(hdr[[3L]]))              # appended at bottom
})

test_that("add_col_header_row() promotes a non-rtf_col_header input", {
  hdr <- add_col_header_row(c("A", "B"), list(col_cell(1, ""),
                                                col_cell(2, "Group")))
  expect_s3_class(hdr, "rtf_col_header")
  expect_length(hdr, 2L)
})

# ──────── pos-spec normalisation ───────────────────────────────────────────

test_that(".pos_row_to_spans converts pos cells to spanning specs with gap-fill", {
  row <- list(col_cell(1, "Item"),
              col_cell(c(3, 4), "Group"))   # gap at col 2
  out <- rtfreporter:::.pos_row_to_spans(row, 5L)
  # Expected: cells at (1,1)="Item", (2,2)="", (3,4)="Group", (5,5)=""
  expect_length(out, 4L)
  expect_identical(out[[1L]]$from, 1L); expect_identical(out[[1L]]$to, 1L)
  expect_identical(out[[1L]]$label, "Item")
  expect_identical(out[[2L]]$from, 2L); expect_identical(out[[2L]]$to, 2L)
  expect_identical(out[[2L]]$label, "")
  expect_identical(out[[3L]]$from, 3L); expect_identical(out[[3L]]$to, 4L)
  expect_identical(out[[3L]]$label, "Group")
  expect_identical(out[[4L]]$from, 5L); expect_identical(out[[4L]]$to, 5L)
})

test_that(".pos_row_to_spans rejects overlapping cells and out-of-range pos", {
  expect_error(
    rtfreporter:::.pos_row_to_spans(
      list(col_cell(c(1, 3), "X"), col_cell(c(2, 4), "Y")), 5L),
    "overlaps")
  expect_error(
    rtfreporter:::.pos_row_to_spans(list(col_cell(6, "Z")), 5L),
    "outside data column range")
})

# ──────── End-to-end: pos format in rtftable() ────────────────────────────

test_that("rtftable() accepts a bare list of cell specs as a single row", {
  df <- data.frame(A = 1, B = "x", C = 2.5, D = 1L, E = 2L)
  tbl <- rtftable(df, col_header = list(
    col_cell(1, "Item"),
    col_cell(c(2, 5), "Other")
  ))
  expect_length(tbl$col_header, 1L)
  expect_true(is.list(tbl$col_header[[1L]]))
  # Verify gap-fill at col 1 is NOT needed (cell 1 starts at pos 1)
  cells <- tbl$col_header[[1L]]
  expect_identical(cells[[1L]]$from, 1L)
  expect_identical(cells[[1L]]$to,   1L)
  expect_identical(cells[[2L]]$from, 2L)
  expect_identical(cells[[2L]]$to,   5L)
})

test_that("rtftable() accepts a multi-row pos-style col_header", {
  df <- data.frame(Item = "x",
                   A_N = 1L, A_M = 2.5,
                   B_N = 3L, B_M = 4.5,
                   stringsAsFactors = FALSE)
  hdr <- rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 5), "Treatment")),
    list(col_cell(1, ""),
         col_cell(c(2, 3), "Drug A"),
         col_cell(c(4, 5), "Drug B")),
    c("Item", "N", "Mean", "N", "Mean")
  )
  tbl <- rtftable(df, col_header = hdr)
  expect_length(tbl$col_header, 3L)

  # End-to-end render should contain all labels.
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(tbl))
  txt <- .render_to_string(doc)
  expect_match(txt, "Treatment")
  expect_match(txt, "Drug A")
  expect_match(txt, "Drug B")
})

test_that("first-level spanning via pos format now works (previously needed spanning_header)", {
  df <- data.frame(Item = "x", A = 1, B = 2)
  tbl <- rtftable(df, col_header = rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 3), "M (SD)"))
  ))
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(tbl))
  txt <- .render_to_string(doc)
  expect_match(txt, "M \\(SD\\)")
})

# ──────── Backward compatibility ───────────────────────────────────────────

test_that("legacy character col_header still works", {
  tbl <- rtftable(data.frame(A = 1, B = 2, C = 3),
                   col_header = c("X", "Y", "Z"))
  expect_length(tbl$col_header, 1L)
  expect_identical(tbl$col_header[[1L]], c("X", "Y", "Z"))
})

test_that("legacy list(spanning_spec_row) col_header still works", {
  tbl <- rtftable(data.frame(A = 1, B = 2, C = 3, D = 4),
    col_header = list(
      list(list(from = 1, to = 2, label = "G1"),
           list(from = 3, to = 4, label = "G2")),
      c("a", "b", "c", "d")
    ))
  expect_length(tbl$col_header, 2L)
  expect_true(is.list(tbl$col_header[[1L]]))
  expect_identical(tbl$col_header[[2L]], c("a", "b", "c", "d"))
})

test_that("legacy spanning_header argument still works", {
  df <- data.frame(Item = "x", A = 1L, B = 2L)
  tbl <- rtftable(df,
    spanning_header = list(
      list(from = 2, to = 3, label = "Group", underline = TRUE)
    ))
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(tbl))
  txt <- .render_to_string(doc)
  expect_match(txt, "Group")
})

# ──────── Builder-style incremental API ───────────────────────────────────

test_that("incremental builder produces the same internal shape as one-shot constructor", {
  one_shot <- rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 5), "Trt")),
    list(col_cell(1, ""),
         col_cell(c(2, 3), "A"),
         col_cell(c(4, 5), "B")),
    c("Item", "N", "M", "N", "M")
  )
  built <- rtf_col_header() |>
    add_col_header_row(list(col_cell(1, ""), col_cell(c(2, 5), "Trt"))) |>
    add_col_header_row(list(col_cell(1, ""),
                             col_cell(c(2, 3), "A"),
                             col_cell(c(4, 5), "B"))) |>
    add_col_header_row(c("Item", "N", "M", "N", "M"))
  expect_identical(unclass(one_shot), unclass(built))
})

# ──────── col_cell text-decoration flags propagate ────────────────────────

test_that("col_cell() preserves bold / italic / underline / align", {
  c1 <- col_cell(1, "X", align = "right", bold = TRUE,
                  italic = TRUE, underline = TRUE)
  expect_true(c1$bold)
  expect_true(c1$italic)
  expect_true(c1$underline)
  expect_identical(c1$align, "right")
  # Default-FALSE decorations leave the field absent.
  c2 <- col_cell(1, "X")
  expect_null(c2$bold)
  expect_null(c2$italic)
  expect_null(c2$underline)
  expect_null(c2$align)
})

# ──────── print methods ───────────────────────────────────────────────────

test_that("print.rtf_col_cell prints single-position cells", {
  txt <- paste(capture.output(print(col_cell(2, "Label"))), collapse = "\n")
  expect_match(txt, "<col_cell pos=2")
  expect_match(txt, "label=")
  expect_match(txt, "Label")
})

test_that("print.rtf_col_cell prints range, decorations, and align", {
  cc <- col_cell(c(2, 4), "Group", align = "center",
                 bold = TRUE, italic = TRUE, underline = TRUE)
  txt <- paste(capture.output(print(cc)), collapse = "\n")
  expect_match(txt, "pos=2\\.\\.4")
  expect_match(txt, "align=center")
  expect_match(txt, "\\[biu\\]")
})

test_that("print.rtf_col_header summarises a multi-row header", {
  hd <- rtf_col_header(
    list(col_cell(c(1, 2), "Pair"), col_cell(3, "Solo")),
    c("A", "B", "C")
  )
  txt <- paste(capture.output(print(hd)), collapse = "\n")
  expect_match(txt, "<rtf_col_header.*2 rows>")
  expect_match(txt, "cells:")
  expect_match(txt, "Pair@1-2")
  expect_match(txt, "Solo@3")
  expect_match(txt, "labels:")
})

test_that("print.rtf_col_header reports single-row in singular form", {
  hd <- rtf_col_header(c("A", "B"))
  txt <- paste(capture.output(print(hd)), collapse = "\n")
  expect_match(txt, "1 row>")
})

# ──────── Named label rows (name-aware placement) ─────────────────────────

test_that("named col_header vector places labels by column name", {
  df <- data.frame(row_label = "x", g1 = 1L, g2 = 2L, g3 = 3L, t = 4L,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df, col_header = c(row_label = "  Category",
                                     g1 = "G1", g2 = "G2", g3 = "G3",
                                     t = "Total"))
  expect_identical(tbl$col_header[[1L]],
                   c("  Category", "G1", "G2", "G3", "Total"))
})

test_that("named col_header is order-independent (safety against reordering)", {
  df <- data.frame(t = 4L, g3 = 3L, g1 = 1L, row_label = "x", g2 = 2L,
                   stringsAsFactors = FALSE)
  # Same spec as above, but data columns are shuffled: labels must follow
  # the names, not the writing order.
  tbl <- rtftable(df, col_header = c(row_label = "  Category",
                                     g1 = "G1", g2 = "G2", g3 = "G3",
                                     t = "Total"))
  # Column order is t, g3, g1, row_label, g2
  expect_identical(tbl$col_header[[1L]],
                   c("Total", "G3", "G1", "  Category", "G2"))
})

test_that("named col_header errors on an unknown column name", {
  df <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  expect_error(
    rtftable(df, col_header = c(a = "A", zzz = "B")),
    "unknown column name \"zzz\""
  )
})

test_that("mixed named/unnamed col_header: unnamed value matched as a col name", {
  df <- data.frame(row_label = "x", g1 = 1L, g2 = 2L,
                   stringsAsFactors = FALSE)
  # "g2" (unnamed) is matched as a column name; "  Cat" (unnamed, pos 1) is
  # positional; g1 is by name.
  tbl <- rtftable(df, col_header = c("  Cat", g1 = "G1", "g2"))
  expect_identical(tbl$col_header[[1L]], c("  Cat", "G1", "g2"))
})

test_that("mixed named/unnamed col_header: unnamed non-name label is positional", {
  df <- data.frame(row_label = "x", g1 = 1L, t = 2L,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df, col_header = c("  Category", g1 = "G1", "Total"))
  expect_identical(tbl$col_header[[1L]], c("  Category", "G1", "Total"))
})

test_that("named col_header errors on a conflict (two elements, one column)", {
  df <- data.frame(g1 = 1L, row_label = "x", g2 = 2L,
                   stringsAsFactors = FALSE)
  # Element 1 (unnamed "  Category") is positional -> column 1 (g1);
  # element `g1 = "G1"` also targets column 1 -> conflict.
  expect_error(
    rtftable(df, col_header = c("  Category", g1 = "G1", g2 = "G2")),
    "targeted twice"
  )
})

test_that("unnamed columns default to their own name", {
  df <- data.frame(row_label = "x", g1 = 1L, g2 = 2L,
                   stringsAsFactors = FALSE)
  # Only g1 named; row_label and g2 fall back to their column names.
  tbl <- rtftable(df, col_header = c(g1 = "Treatment"))
  expect_identical(tbl$col_header[[1L]], c("row_label", "Treatment", "g2"))
})

test_that("fully-unnamed character col_header is unchanged (legacy positional)", {
  df <- data.frame(g1 = 1L, g2 = 2L, g3 = 3L, stringsAsFactors = FALSE)
  # A label that happens to equal a column name must NOT be re-placed when
  # no element is named.
  tbl <- rtftable(df, col_header = c("g3", "g1", "g2"))
  expect_identical(tbl$col_header[[1L]], c("g3", "g1", "g2"))
})

# ──────── col_cell() spanning by column name ──────────────────────────────

test_that("col_cell() spanning by name resolves against data columns", {
  df <- data.frame(item = "x", g1 = 1L, g2 = 2L, g3 = 3L, total = 4L,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df, col_header = rtf_col_header(
    list(col_cell("item", ""), col_cell(c("g1", "g3"), "Drug"),
         col_cell("total", "Total")),
    c(item = "Item", g1 = "N", g2 = "Mean", g3 = "SD", total = "Total")
  ))
  spans <- tbl$col_header[[1L]]
  # item(1,1), Drug(2,4), Total(5,5)
  expect_identical(spans[[1L]]$from, 1L); expect_identical(spans[[1L]]$to, 1L)
  expect_identical(spans[[2L]]$from, 2L); expect_identical(spans[[2L]]$to, 4L)
  expect_identical(spans[[2L]]$label, "Drug")
  expect_identical(spans[[3L]]$from, 5L); expect_identical(spans[[3L]]$to, 5L)
})

test_that("col_cell() spanning by name errors on an unknown name", {
  df <- data.frame(item = "x", g1 = 1L, g2 = 2L, stringsAsFactors = FALSE)
  expect_error(
    rtftable(df, col_header = rtf_col_header(
      list(col_cell("item", ""), col_cell(c("g1", "zzz"), "Drug")))),
    "not found in data columns"
  )
})

test_that(".pos_row_to_spans resolves character pos with col_names", {
  out <- rtfreporter:::.pos_row_to_spans(
    list(col_cell(c("b", "c"), "BC")), ncol_df = 4L,
    col_names = c("a", "b", "c", "d"))
  # gap a(1,1), BC(2,3), tail d(4,4)
  expect_identical(out[[2L]]$from, 2L)
  expect_identical(out[[2L]]$to,   3L)
  expect_identical(out[[2L]]$label, "BC")
})

# ──────── .pos_row_to_spans error branches ────────────────────────────────

test_that(".pos_row_to_spans returns empty for an empty row", {
  res <- rtfreporter:::.pos_row_to_spans(list(), ncol_df = 3L)
  expect_identical(res, list())
})

test_that(".pos_row_to_spans errors on missing pos", {
  expect_error(
    rtfreporter:::.pos_row_to_spans(list(list(label = "X")), ncol_df = 3L),
    "missing `pos`"
  )
})

test_that(".pos_row_to_spans errors on pos with > 2 elements", {
  expect_error(
    rtfreporter:::.pos_row_to_spans(
      list(list(pos = c(1L, 2L, 3L), label = "X")), ncol_df = 5L),
    "scalar or a length-2"
  )
})

test_that(".pos_row_to_spans errors on inverted range (from > to)", {
  expect_error(
    rtfreporter:::.pos_row_to_spans(
      list(list(pos = c(3L, 1L), label = "X")), ncol_df = 5L),
    "must be <="
  )
})

test_that(".pos_row_to_spans errors when range is outside the data column range", {
  expect_error(
    rtfreporter:::.pos_row_to_spans(
      list(list(pos = c(1L, 6L), label = "X")), ncol_df = 5L),
    "outside data column"
  )
  expect_error(
    rtfreporter:::.pos_row_to_spans(
      list(list(pos = 0L, label = "X")), ncol_df = 5L),
    "outside data column"
  )
})

test_that(".pos_row_to_spans errors on overlapping cells", {
  expect_error(
    rtfreporter:::.pos_row_to_spans(
      list(list(pos = c(1L, 3L), label = "A"),
           list(pos = c(2L, 4L), label = "B")),
      ncol_df = 5L),
    "overlaps"
  )
})

test_that(".pos_row_to_spans fills gaps with empty cells + preserves decoration", {
  res <- rtfreporter:::.pos_row_to_spans(
    list(list(pos = c(2L, 3L), label = "X", bold = TRUE)),
    ncol_df = 5L
  )
  # gap at column 1, the X span at 2..3, and the renderer treats trailing
  # gaps as not necessary (the row stops at the last covered column).
  expect_identical(res[[1L]]$from, 1L)
  expect_identical(res[[1L]]$to,   1L)
  expect_identical(res[[1L]]$label, "")
  expect_identical(res[[2L]]$from, 2L)
  expect_identical(res[[2L]]$to,   3L)
  expect_identical(res[[2L]]$label, "X")
  expect_true(isTRUE(res[[2L]]$bold))
})

test_that(".pos_row_to_spans fills cells whose label is NULL", {
  # When the caller passes a cell without `label`, the helper sets "" as
  # the default.
  res <- rtfreporter:::.pos_row_to_spans(
    list(list(pos = 2L)),
    ncol_df = 3L
  )
  expect_identical(res[[2L]]$label, "")
})
