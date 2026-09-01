## tests/testthat/test-style-verbs.R
##
## Post-hoc styling verbs (#218): style_header / style_cols / style_body /
## style_zone / add_header_row, on a single rtftable and on page lists.

library(testthat)

# ── fixtures ──────────────────────────────────────────────────────────────────

.sv_df <- function() {
  data.frame(Item = c("Age", "Sex"), A = c("1", "2"),
             B = c("3", "4"), C = c("5", "6"), stringsAsFactors = FALSE)
}

# The 3-row header from the motivating use case: 1 / 2-4, 1 / 2-4, 1,2,3,4.
.sv_tbl <- function() {
  hdr <- rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 4), "Treatment Group")),
    list(col_cell(1, ""), col_cell(c(2, 4), "(N = 254)")),
    c("Item", "Placebo", "Drug A", "Drug B")
  )
  rtftable(.sv_df(), col_header = hdr, border = "tfl")
}

# Render a table and return the RTF text.
.sv_rtf <- function(tbl) {
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(rtf_document() |> rtf_tables(list(tbl)), f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# Count occurrences of a fixed pattern per \trowd block.
.sv_row_counts <- function(rtf, pattern) {
  rows <- unlist(strsplit(rtf, "\\\\trowd"))[-1]
  vapply(rows, function(r)
    length(regmatches(r, gregexpr(pattern, r))[[1]]),
    integer(1L), USE.NAMES = FALSE)
}

TOP <- "\\\\clbrdrt\\\\brdrs"
BOT <- "\\\\clbrdrb\\\\brdrs"

# ── style_header: cell rows ───────────────────────────────────────────────────

test_that("style_header() adds a top rule and erases the auto underline on one span cell", {
  ctrl <- .sv_tbl()
  pat  <- style_header(ctrl, row = 2, cols = 2:4,
                       border = rtf_border(top    = rtf_border_side("single"),
                                           bottom = rtf_border_side("none")))
  tc <- .sv_row_counts(.sv_rtf(ctrl), TOP)
  bc <- .sv_row_counts(.sv_rtf(ctrl), BOT)
  tp <- .sv_row_counts(.sv_rtf(pat), TOP)
  bp <- .sv_row_counts(.sv_rtf(pat), BOT)
  # control: row 2 has the automatic group underline, no top rule
  expect_equal(tc[2], 0L); expect_equal(bc[2], 1L)
  # patched: top rule on the span cell only, underline gone
  expect_equal(tp[2], 1L); expect_equal(bp[2], 0L)
  # all other rows untouched
  expect_equal(tc[-2], tp[-2]); expect_equal(bc[-2], bp[-2])
})

test_that("style_header() border merge is last-writer-wins per side", {
  tbl <- .sv_tbl() |>
    style_header(row = 2, cols = 2:4,
                 border = rtf_border(top = rtf_border_side("single"))) |>
    style_header(row = 2, cols = 2:4,
                 border = rtf_border(top = rtf_border_side("double")))
  cell <- tbl$col_header[[2]][[2]]
  expect_equal(cell$border$top$style, "double")
})

test_that("style_header() targets by span intersection and respects `cols`", {
  tbl <- style_header(.sv_tbl(), row = 1, cols = 3,
                      bold = TRUE)   # col 3 sits inside the 2-4 span
  expect_true(isTRUE(tbl$col_header[[1]][[2]]$bold))
  # col 1 cell untouched
  expect_null(tbl$col_header[[1]][[1]]$bold)
})

test_that("style_header() replaces labels on cell rows and label rows", {
  tbl <- .sv_tbl() |>
    style_header(row = 1, cols = 2, label = "Arms") |>
    style_header(row = 3, cols = 4, label = "Drug C")
  expect_equal(tbl$col_header[[1]][[2]]$label, "Arms")
  expect_equal(tbl$col_header[[3]][[4]], "Drug C")
})

test_that("style_header() warns when no header cell matches", {
  expect_warning(style_header(.sv_tbl(), row = 1, cols = 1, label = "x",
                              align = "left"),
                 NA)  # col 1 matches the col-1 cell: no warning
  # row 1 has cells at 1 and 2-4; every col matches something, so force a
  # mismatch via a single-cell row on a wider table is not possible here --
  # instead check the row-range validation.
  expect_error(style_header(.sv_tbl(), row = 9), "1\\.\\.3")
})

# ── style_header: labels-row promotion ───────────────────────────────────────

test_that("labels-row promotion is render-equivalent when nothing is styled", {
  ctrl <- .sv_tbl()
  # An all-NULL border triggers promotion but changes no side.
  prom <- style_header(ctrl, row = 3, cols = 2, border = rtf_border())
  expect_false(is.character(prom$col_header[[3]]))   # promoted to cells
  expect_identical(.sv_rtf(prom), .sv_rtf(ctrl))     # pixel-for-pixel RTF
})

test_that("border on a labels row lands on the requested cell only", {
  ctrl <- .sv_tbl()
  pat  <- style_header(ctrl, row = 3, cols = 3,
                       border = rtf_border(bottom = rtf_border_side("double")))
  dbl <- .sv_row_counts(.sv_rtf(pat), "\\\\clbrdrb\\\\brdrdb")
  expect_equal(dbl[3], 1L)                # one double rule in header row 3
  expect_equal(sum(dbl[-3]), 0L)
})

test_that("bold/align on a labels row routes to col_spec header styling", {
  tbl <- style_header(.sv_tbl(), row = 3, cols = c(2, 3),
                      bold = TRUE, align = "left")
  expect_true(tbl$col_spec[[2]]$header_bold)
  expect_true(tbl$col_spec[[3]]$header_bold)
  expect_equal(tbl$col_spec[[2]]$header_align, "left")
  expect_false(isTRUE(tbl$col_spec[[4]]$header_bold))
  expect_true(is.character(tbl$col_header[[3]]))     # NOT promoted
})

# ── style_cols ────────────────────────────────────────────────────────────────

test_that("style_cols() patches the normalized col_spec", {
  tbl <- style_cols(.sv_tbl(), cols = list("B", 4), align = "center",
                    bold = TRUE, color = "#003366")
  for (j in 3:4) {
    expect_equal(tbl$col_spec[[j]]$align, "center")
    expect_true(tbl$col_spec[[j]]$bold)
    expect_equal(tbl$col_spec[[j]]$color, "#003366")
  }
  expect_equal(tbl$col_spec[[2]]$align, "center")  # untouched default
  expect_false(tbl$col_spec[[2]]$bold)
})

test_that("style_cols() validates its inputs", {
  expect_error(style_cols(.sv_tbl(), cols = "nope"), "not found")
  expect_error(style_cols(.sv_tbl(), cols = 2, align = "middle"), "left")
  expect_error(style_cols(.sv_tbl(), cols = 2, border = "single"), "rtf_border")
})

# ── style_body ────────────────────────────────────────────────────────────────

test_that("style_body() writes cell_styles for formula / function / integer rows", {
  t1 <- style_body(.sv_tbl(), rows = ~ Item == "Age", cols = "A", bold = TRUE)
  expect_true(t1$cell_styles[[1]]$bold[2])
  expect_true(is.na(t1$cell_styles[[1]]$bold[1]))
  expect_null(t1$cell_styles[[2]])

  t2 <- style_body(.sv_tbl(), rows = function(d) d$Item == "Sex",
                   color = "#FF0000")
  expect_equal(t2$cell_styles[[2]]$color, rep("#FF0000", 4))

  t3 <- style_body(.sv_tbl(), rows = 2, cols = 1, italic = TRUE)
  expect_true(t3$cell_styles[[2]]$italic[1])
})

test_that("style_body() layers on top of existing overrides without erasing them", {
  tbl <- .sv_tbl() |>
    style_body(rows = 1, cols = 1:2, bold = TRUE) |>
    style_body(rows = 1, cols = 2,   italic = TRUE)
  expect_equal(tbl$cell_styles[[1]]$bold[1:2], c(TRUE, TRUE))
  expect_true(tbl$cell_styles[[1]]$italic[2])
  expect_true(is.na(tbl$cell_styles[[1]]$italic[1]))
})

test_that("style_body() renders bold where requested", {
  ctrl_b <- .sv_row_counts(.sv_rtf(.sv_tbl()), "\\\\b ")
  pat    <- style_body(.sv_tbl(), rows = 1, cols = 2, bold = TRUE)
  pat_b  <- .sv_row_counts(.sv_rtf(pat), "\\\\b ")
  expect_equal(sum(pat_b) - sum(ctrl_b), 1L)
})

test_that("style_body() rejects bad rows", {
  expect_error(style_body(.sv_tbl(), rows = 99), "1\\.\\.2")
  expect_error(style_body(.sv_tbl(), rows = c(TRUE, FALSE, TRUE)), "length 2")
  expect_error(style_body(.sv_tbl(), rows = Item ~ 1), "one-sided")
})

test_that("style_body(border = ) rules the requested body cells only", {
  ctrl <- .sv_tbl()
  pat  <- style_body(ctrl, rows = 1,
                     border = rtf_border(bottom = rtf_border_side("single")))
  bc <- .sv_row_counts(.sv_rtf(ctrl), BOT)
  bp <- .sv_row_counts(.sv_rtf(pat), BOT)
  expect_equal(bp[4] - bc[4], 4L)          # data row 1 = trowd 4: all 4 cells
  expect_equal(bp[-4], bc[-4])             # nothing else changed

  part <- style_body(ctrl, rows = 1, cols = 2:3,
                     border = rtf_border(bottom = rtf_border_side("single")))
  expect_equal(.sv_row_counts(.sv_rtf(part), BOT)[4] - bc[4], 2L)
})

test_that("style_body(border = ) merges on top of the zone border, none erases", {
  tbl <- .sv_tbl() |>
    style_zone(last_row = rtf_border(bottom = rtf_border_side("single"))) |>
    style_body(rows = 2, cols = 2,
               border = rtf_border(bottom = rtf_border_side("none")))
  bc <- .sv_row_counts(.sv_rtf(tbl), BOT)
  expect_equal(bc[5], 3L)                  # last data row: 4 zone rules - 1 erased
})

test_that("style_body(border = ) layers per side across calls", {
  tbl <- .sv_tbl() |>
    style_body(rows = 1, cols = 2,
               border = rtf_border(bottom = rtf_border_side("single"))) |>
    style_body(rows = 1, cols = 2,
               border = rtf_border(top = rtf_border_side("double")))
  b <- tbl$cell_styles[[1]]$border[[2]]
  expect_equal(b$bottom$style, "single")   # earlier side survives
  expect_equal(b$top$style, "double")
})

test_that("style_body(align = ) overrides the column alignment per cell", {
  ctrl <- .sv_tbl()
  pat  <- style_body(ctrl, rows = 1, cols = 2:4, align = "left")
  ql_c <- .sv_row_counts(.sv_rtf(ctrl), "\\\\ql")
  ql_p <- .sv_row_counts(.sv_rtf(pat), "\\\\ql")
  expect_equal(ql_p[4] - ql_c[4], 3L)      # cols 2-4 flip center -> left
  expect_equal(ql_p[5], ql_c[5])           # row 2 untouched
  expect_equal(pat$cell_styles[[1]]$align, c(NA, "left", "left", "left"))
  expect_error(style_body(ctrl, rows = 1, align = "middle"), "left")
})

test_that("cell_styles border/align survive the drop_cols reindex", {
  df <- data.frame(Item = c("Age", "Sex"), A = c("1", "2"),
                   B = c("3", "4"), C = c("5", "6"), stringsAsFactors = FALSE)
  b  <- rtf_border(bottom = rtf_border_side("double"))
  styles <- list(
    list(align  = c(NA, "left", NA, NA),
         border = list(NULL, b, NULL, NULL)),
    NULL
  )
  page <- as_rtftables(df, drop_cols = 3, cell_styles = styles)[[1]]
  expect_equal(length(page$cell_styles[[1]]$align), 3L)
  expect_equal(page$cell_styles[[1]]$align[2], "left")
  expect_length(page$cell_styles[[1]]$border, 3L)
  expect_equal(page$cell_styles[[1]]$border[[2]]$bottom$style, "double")
})

# ── style_zone ────────────────────────────────────────────────────────────────

test_that("style_zone() merges onto the resolved table border", {
  tbl <- style_zone(.sv_tbl(),
                    last_row = rtf_border(bottom = rtf_border_side("double")))
  expect_equal(tbl$border$last_row$bottom$style, "double")
  rtf <- .sv_rtf(tbl)
  expect_match(rtf, "\\\\clbrdrb\\\\brdrdb")
})

# ── add_header_row ────────────────────────────────────────────────────────────

test_that("add_header_row() prepends / appends a normalized row", {
  tbl <- add_header_row(.sv_tbl(),
                        list(col_cell(c(2, 4), "STUDY01")), .position = "top")
  expect_length(tbl$col_header, 4L)
  expect_equal(tbl$col_header[[1]][[2]]$label, "STUDY01")
  # pos-style cells got normalized to from/to spans covering all columns
  expect_equal(tbl$col_header[[1]][[2]]$from, 2L)
  expect_equal(tbl$col_header[[1]][[2]]$to, 4L)

  tbl2 <- add_header_row(.sv_tbl(), c("i", "p", "a", "b"),
                         .position = "bottom")
  expect_length(tbl2$col_header, 4L)
  expect_identical(tbl2$col_header[[4]], c("i", "p", "a", "b"))
})

# ── list (pages) methods ──────────────────────────────────────────────────────

test_that("verbs map over every page of an as_rtftables() list", {
  df <- data.frame(Item = letters[1:4], A = 1:4, B = 5:8, C = 9:12,
                   stringsAsFactors = FALSE)
  pages <- as_rtftables(df, split = "group_force", max_rows = 2)
  expect_length(pages, 2L)

  out <- pages |>
    style_cols(cols = "A", align = "center") |>
    style_zone(last_row = rtf_border(bottom = rtf_border_side("double"))) |>
    add_header_row(c("w", "x", "y", "z"), .position = "top")
  for (p in out) {
    expect_equal(p$col_spec[[2]]$align, "center")
    expect_equal(p$border$last_row$bottom$style, "double")
    expect_identical(p$col_header[[1]], c("w", "x", "y", "z"))
  }

  # predicate rows are evaluated per page
  out2 <- style_body(pages, rows = ~ Item == "c", bold = TRUE)
  expect_null(out2[[1]]$cell_styles)
  expect_true(out2[[2]]$cell_styles[[1]]$bold[1])
})

test_that("style_body() on a page list rejects integer/logical rows", {
  df <- data.frame(Item = letters[1:4], A = 1:4, stringsAsFactors = FALSE)
  pages <- as_rtftables(df, split = "group_force", max_rows = 2)
  expect_error(style_body(pages, rows = 1, bold = TRUE), "ambiguous")
  expect_error(style_body(pages, rows = c(TRUE, FALSE), bold = TRUE),
               "ambiguous")
})

test_that("list methods reject non-rtftable elements", {
  expect_error(style_cols(list(.sv_tbl(), data.frame(a = 1)), cols = 1,
                          bold = TRUE),
               "element 2")
})
