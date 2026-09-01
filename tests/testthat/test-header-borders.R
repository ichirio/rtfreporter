# Column-header border defaults: top on the topmost header row, bottom on the
# bottommost, group-underline on multi-column spanning cells, no body borders.

.render_tbl <- function(tbl) {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(tbl))
  .render_to_string(doc)
}

.count_top    <- function(txt) length(gregexpr("\\\\clbrdrt\\\\brdrs", txt)[[1L]])
.count_bottom <- function(txt) length(gregexpr("\\\\clbrdrb\\\\brdrs", txt)[[1L]])

.df_5col <- function() data.frame(Item = "Age",
                                  A_N = 30L, A_Mean = 45.2,
                                  B_N = 30L, B_Mean = 46.1,
                                  stringsAsFactors = FALSE)

test_that("single-row header gets top + bottom on every cell; data has no borders", {
  tbl <- rtftable(.df_5col(), col_header = c("Item", "N", "Mean", "N", "Mean"))
  txt <- .render_tbl(tbl)
  expect_identical(.count_top(txt),    5L)
  expect_identical(.count_bottom(txt), 5L)
})

test_that("two header rows: top only on row 1, bottom only on row 2", {
  tbl <- rtftable(.df_5col(),
    col_header = list(
      list(
        list(from = 2, to = 3, label = "ArmA"),
        list(from = 4, to = 5, label = "ArmB")
      ),
      c("Item", "N", "Mean", "N", "Mean")
    ))
  txt <- .render_tbl(tbl)
  # Row 1 = 3 cells (Item single + ArmA + ArmB) → 3 tops.
  # Row 2 = 5 cells (last header) → 5 bottoms.  Plus 2 group-underlines on the
  # multi-col spanning cells of row 1.  Data section emits nothing.
  expect_identical(.count_top(txt),    3L)
  expect_identical(.count_bottom(txt), 7L)
})

test_that("multi-col spanning cells carry a group-separator bottom border", {
  tbl <- rtftable(.df_5col(),
    col_header = c("Item", "N", "Mean", "N", "Mean"),
    spanning_header = list(
      list(from = 2, to = 3, label = "ArmA"),
      list(from = 4, to = 5, label = "ArmB")
    ))
  txt <- .render_tbl(tbl)
  # 2 span underlines + 5 last-row label cells.
  expect_identical(.count_bottom(txt), 7L)
})

test_that("single-column spanning rows do NOT add an extra bottom border", {
  df  <- data.frame(A = 1L, B = 2L, C = 3L)
  tbl <- rtftable(df, col_header = list(
    list(
      list(from = 1, to = 1, label = "x"),
      list(from = 2, to = 2, label = "y"),
      list(from = 3, to = 3, label = "z")
    ),
    c("A", "B", "C")
  ))
  txt <- .render_tbl(tbl)
  # Only row 2's 3 cells get bottoms.
  expect_identical(.count_bottom(txt), 3L)
})

test_that("no vertical borders are emitted by default", {
  tbl <- rtftable(.df_5col(), col_header = c("Item", "N", "Mean", "N", "Mean"))
  txt <- .render_tbl(tbl)
  expect_false(grepl("\\\\clbrdrl\\\\brdrs", txt))
  expect_false(grepl("\\\\clbrdrr\\\\brdrs", txt))
})

test_that("border = NULL suppresses every border", {
  tbl <- rtftable(.df_5col(), col_header = c("Item", "N", "Mean", "N", "Mean"),
                   border = NULL)
  txt <- .render_tbl(tbl)
  expect_false(grepl("\\\\clbrdrt\\\\brdrs", txt))
  expect_false(grepl("\\\\clbrdrb\\\\brdrs", txt))
})

test_that("col_spec[[j]]$border still applies when default borders are off", {
  df  <- data.frame(A = 1L, B = 2L, C = 3L)
  tbl <- rtftable(df,
    col_header = c("A", "B", "C"),
    border     = NULL,
    col_spec   = list(
      list(col = 2, border = rtf_border(top    = rtf_border_side("double", 20L),
                                         bottom = rtf_border_side("double", 20L)))
    ))
  txt <- .render_tbl(tbl)
  expect_match(txt, "\\\\brdrdb")
})

test_that("data section emits no default borders even with trailing blank_rows", {
  tbl <- rtftable(
    data.frame(A = 1:3, B = letters[1:3], stringsAsFactors = FALSE),
    col_header = c("A", "B"),
    blank_rows = c(-1)
  )
  txt <- .render_tbl(tbl)
  expect_identical(.count_top(txt),    2L)
  expect_identical(.count_bottom(txt), 2L)
})

test_that("explicit border$last_row still renders under the last data row", {
  tbl <- rtftable(
    data.frame(A = 1:3, B = letters[1:3], stringsAsFactors = FALSE),
    col_header = c("A", "B"),
    border = "none"
  ) |>
    style_zone(
      header   = rtf_border(top = rtf_border_side(), bottom = rtf_border_side()),
      last_row = rtf_border(bottom = rtf_border_side())
    )
  txt <- .render_tbl(tbl)
  # 2 header bottoms + 2 last-row bottoms.
  expect_identical(.count_bottom(txt), 4L)
})

test_that("col_cell(border = none) removes the group underline under one cell (#81)", {
  hdr_default <- rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 3), "Drug A"),
         col_cell(c(4, 5), "Drug B")),
    c("Item", "N", "Mean", "N", "Mean")
  )
  hdr_suppressed <- rtf_col_header(
    list(col_cell(1, ""),
         col_cell(c(2, 3), "Drug A",
                  border = rtf_border(bottom = rtf_border_side("none"))),
         col_cell(c(4, 5), "Drug B")),
    c("Item", "N", "Mean", "N", "Mean")
  )
  n_default    <- .count_bottom(.render_tbl(
    rtftable(.df_5col(), col_header = hdr_default)))
  n_suppressed <- .count_bottom(.render_tbl(
    rtftable(.df_5col(), col_header = hdr_suppressed)))
  # Exactly one fewer bottom rule once the Drug A underline is removed.
  expect_identical(n_suppressed, n_default - 1L)
})

test_that("col_cell(border = ...) adds a per-cell rule and validates its type (#81)", {
  expect_error(col_cell(1, "x", border = "oops"),
               "must be NULL or an rtf_border")
  cc <- col_cell(1, "x", border = rtf_border(bottom = rtf_border_side("thick")))
  expect_true(inherits(cc$border, "rtf_border"))
})

test_that('rtf_border_side("none") builds no command but overrides on merge (#81)', {
  none <- rtf_border_side("none")
  expect_identical(none$style, "none")
  expect_identical(rtfreporter:::.build_border_commands(
    rtf_border(bottom = none)), "")
  merged <- rtfreporter:::.merge_rtf_border(
    rtf_border(bottom = rtf_border_side()), rtf_border(bottom = none))
  expect_identical(merged$bottom$style, "none")
})

test_that("group underline only where the column grouping changes below (#102)", {
  df  <- data.frame(a = "x", b = "1", c = "2", d = "3", stringsAsFactors = FALSE)
  hdr <- rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 4), "G")),   # row 1: 1 | (2-4)
    list(col_cell(1, ""), col_cell(c(2, 4), "G")),   # row 2: 1 | (2-4)  (same)
    c("a", "b", "c", "d")                            # row 3: 1 | 2 | 3 | 4
  )
  txt  <- .render_tbl(rtftable(df, col_header = hdr))
  rows <- regmatches(txt, gregexpr("\\\\trowd.*?\\\\row", txt))[[1L]]
  # Drop the title/footnote block rows (single-column, top-valigned) so the
  # first rows are the column-header rows.
  rows <- rows[!grepl("\\\\clvertalt", rows)]
  cnt  <- function(r, pat) {
    m <- gregexpr(pat, r)[[1L]]; if (m[1L] == -1L) 0L else length(m)
  }
  bottoms <- vapply(rows[seq_len(3L)], cnt, integer(1L), pat = "\\\\clbrdrb")
  # row 1: grouping unchanged below -> NO group underline.
  expect_identical(unname(bottoms[1L]), 0L)
  # row 2: span (2-4) splits into 2,3,4 below -> underline on the span cell.
  expect_identical(unname(bottoms[2L]), 1L)
  # row 3: last header row -> outer-frame bottom on every column.
  expect_identical(unname(bottoms[3L]), 4L)
})

test_that("rtf_table_style_tfl() leaves body / first_row / last_row at NULL", {
  sty <- rtf_table_style_tfl()
  expect_null(sty$border_body)
  expect_null(sty$border_first_row)
  expect_null(sty$border_last_row)
})
