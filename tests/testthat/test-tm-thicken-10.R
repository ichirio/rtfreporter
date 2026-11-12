# Thickening of the functional tests, batch 10: the table builders and verbs
# (area TB, FR-TB376 to FR-TB633).  Each block asserts what its FR states, the
# figures worked out in a comment.  FT ids from 6001 up.

# ---------------------------------------------------------------- helpers

# The cell widths (twips) of every row of a rendered table: one integer vector
# per row that has \cellx marks.
.t10_widths <- function(tbl, width = 9360L) {
  rows <- .render_rtftable(tbl, width)
  rows <- grep("\\cellx", rows, fixed = TRUE, value = TRUE)
  lapply(rows, function(r) {
    x <- as.integer(sub("cellx", "", regmatches(r, gregexpr("cellx[0-9]+", r))[[1L]]))
    diff(c(0L, x))
  })
}

# A data frame with a row-heading column and `nd` data columns of "1", "2".
.t10_short <- function(nd, label = c("a", "b"), pad = "") {
  d <- data.frame(Param = paste0(pad, label), stringsAsFactors = FALSE)
  for (i in seq_len(nd)) d[[paste0("V", i)]] <- c("1", "2")
  d
}

# The RTF text of one table or a list of pages as a document.
.t10_rtf <- function(x, env = parent.frame()) {
  d <- tempfile("t10-")
  dir.create(d)
  do.call(on.exit, list(substitute(unlink(d, recursive = TRUE)), add = TRUE), envir = env)
  f <- file.path(d, "t.rtf")
  pages <- if (inherits(x, "rtftable")) list(x) else x
  generate_rtfreport(rtf_tables(rtf_document(), pages), f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

# A table plan from a small ARD (AGE by arm), for plan_columns(widths = ).
.t10_plan <- function() {
  adsl <- cards::ADSL
  adsl$TRT <- as.character(adsl$ARM)
  ard <- suppressMessages(normalize_ard(cards::ard_stack(
    adsl, .by = all_of("TRT"),
    cards::ard_summary(
      variables = AGE,
      statistic = ~ cards::continuous_summary_fns(c("N", "mean", "sd"))),
    .total_n = TRUE)))
  table_plan(ard, cols = "TRT", rows = c(group = "variable")) |>
    plan_cells(notes = FALSE) |>
    plan_cells(continuous = c("n" = "{N:d}", "Mean (SD)" = "{mean} ({sd})"))
}

# ------------------------------------------------- default_col_widths, text_width_in

test_that("FT-TB6001 [FR-TB376]: default_col_widths() is a named vector, smallest data column 1, and what rtftable() and plan_columns(widths = ) use", {
  skip_if_not_installed("cards")
  d <- data.frame(Characteristic = c("Age (years)", "  Mean (SD)", "  Median"),
                  Placebo = c("", "75.2 (8.59)", "76.0"),
                  Active = c("", "74.4 (7.89)", "75.0"), stringsAsFactors = FALSE)
  w <- default_col_widths(d)
  expect_type(w, "double")
  expect_identical(names(w), names(d))
  expect_length(w, 3L)
  expect_identical(min(w[-1L]), 1)
  # the row heading: the same proportions at any digits, only rounded
  expect_equal(default_col_widths(d, digits = 6L)[[1L]] / default_col_widths(d, digits = 6L)[[2L]],
               w[[1L]] / w[[2L]], tolerance = 0.05)
  # a table with no widths: the same cells as with col_rel_width = w, 9360 twips in all
  # (the 2 data columns equal, the heading 4011 : 2674 : 2675 as w = 1.5 : 1 : 1 widened
  # by its content)
  none <- .t10_widths(rtftable(d))
  given <- .t10_widths(rtftable(d, col_rel_width = w))
  expect_identical(none, given)
  expect_identical(sum(none[[1L]]), 9360L)
  expect_identical(cumsum(none[[1L]]), c(4011L, 6685L, 9360L))
  # plan_columns(widths = ): the vector is taken as relative widths of the printed columns
  p <- .t10_plan()
  pg0 <- suppressMessages(plan_apply(p, "pages"))
  pg0 <- if (inherits(pg0, "rtftable")) pg0 else pg0[[1L]]
  w0 <- default_col_widths(pg0$data, digits = 6L)
  expect_identical(names(w0), names(pg0$data))
  pg1 <- suppressMessages(plan_apply(plan_columns(p, widths = w0), "pages"))
  pg1 <- if (inherits(pg1, "rtftable")) pg1 else pg1[[1L]]
  expect_equal(unname(pg1$col_rel_width), unname(w0))
  r0 <- .t10_widths(pg0)[[1L]]
  r1 <- .t10_widths(pg1)[[1L]]
  expect_true(all(abs(r0 - r1) <= 2L))
})

test_that("FT-TB6002 [FR-TB383]: the row-heading share is its longest line + 2, capped, floored, and the indent adds 5 points", {
  share <- function(d) {
    w <- default_col_widths(d, digits = 8L)
    w[[1L]] / sum(w)
  }
  # a 30-character heading with six data columns takes 30 + 2 = 32 of the 120 characters
  expect_equal(share(.t10_short(6L, c(strrep("x", 30L), "y"))), 32 / 120, tolerance = 1e-6)
  # only the longest LINE counts: 30 + 5 characters on two lines are the same 32
  expect_equal(share(.t10_short(6L, c(paste0(strrep("x", 30L), "\n", strrep("y", 5L)), "y"))),
               32 / 120, tolerance = 1e-6)
  # a 26-character heading (need 28) with six data columns is above the 1.5-column floor
  # 1.5 * 120 / 7.5 = 24, so 28 of the 120
  expect_equal(share(.t10_short(6L, c(strrep("x", 26L), "y"))), 28 / 120, tolerance = 1e-6)
  # at most 40% (4 data columns: 48 of 120)
  expect_equal(share(.t10_short(4L, c(strrep("x", 100L), "y"))), 0.40, tolerance = 1e-6)
  # less with many data columns: 0.40 - 0.02 * (10 - 4) = 0.28; 20% at the least
  expect_equal(share(.t10_short(10L, c(strrep("x", 100L), "y"))), 0.28, tolerance = 1e-6)
  expect_equal(share(.t10_short(20L, c(strrep("x", 100L), "y"))), 0.20, tolerance = 1e-6)
  # 5 points more when the headings are indented
  expect_equal(share(.t10_short(4L, c(strrep("x", 100L), "y"), pad = "  ")), 0.45,
               tolerance = 1e-6)
  # never less than 1.5 times a data column's equal share ...
  expect_equal(unname(default_col_widths(.t10_short(3L), digits = 8L)), c(1.5, 1, 1, 1))
  # ... above that share where it takes it: two data columns, a 100-character heading is
  # capped at 48 but the floor 1.5 * 120 / 3.5 = 51.43 holds: 3/7 of the width, 1.5 : 1 : 1
  expect_equal(share(.t10_short(2L, c(strrep("x", 100L), "y"))), 3 / 7, tolerance = 1e-6)
  expect_equal(unname(default_col_widths(.t10_short(2L, c(strrep("x", 100L), "y")), digits = 8L)),
               c(1.5, 1, 1))
  # one data column: 60:40
  expect_identical(unname(default_col_widths(.t10_short(1L))), c(1.5, 1))
})

test_that("FT-TB6003 [FR-TB387]: every block of paginate_cols() keeps each column's default width, in the relative widths and in the rendered cells", {
  d <- data.frame(Param = c("a", "b"), V1 = c("1", "2"), V2 = c(strrep("x", 30L), "y"),
                  V3 = c("1", "2"), V4 = c("1", "2"), V5 = c("1", "2"),
                  stringsAsFactors = FALSE)
  dw <- default_col_widths(d, digits = 8L)
  pages <- suppressWarnings(paginate_cols(rtftable(d), at = c(3, 5)))
  expect_length(pages, 3L)
  expect_identical(lapply(pages, function(p) names(p$data)),
                   list(c("Param", "V1"), c("Param", "V2", "V3"), c("Param", "V4", "V5")))
  for (p in pages) {
    cols <- names(p$data)
    # the relative widths, the first column's as the unit
    expect_equal(p$col_rel_width / p$col_rel_width[[1L]], dw[cols] / dw[[cols[1L]]],
                 ignore_attr = TRUE, tolerance = 1e-6)
    # and the rendered cells keep the same proportions to a twip or two
    r <- .t10_widths(p)[[1L]]
    expect_true(all(abs(r / r[[1L]] - unname(dw[cols] / dw[[cols[1L]]])) < 0.01))
  }
  # V2 (a 30-character cell) is the widest data column on its block and stays so
  expect_gt(pages[[2L]]$col_rel_width[[2L]], pages[[2L]]$col_rel_width[[3L]])
})

test_that("FT-TB6004 [FR-TB388]: text_width_in() is one number per text, proportional to the characters in a monospace font, the sum of the characters' widths in Arial and Times New Roman", {
  txt <- c("hello", "w", NA, "")
  out <- text_width_in(txt)
  expect_type(out, "double")
  expect_length(out, 4L)
  expect_identical(out[3:4], c(0, 0))
  # a monospace font (the default Courier New): n characters are n times one, any character
  one <- text_width_in("a")
  expect_equal(text_width_in("hello"), 5 * one, tolerance = 1e-10)
  expect_equal(text_width_in("WiW. "), 5 * one, tolerance = 1e-10)
  expect_equal(text_width_in("iiii"), text_width_in("WWWW"))
  # Arial: the characters' widths added, 12 pt = 12 / 72 inch per em;
  # Helvetica: i 222, W 944, n 556 (1/1000 em)
  ar <- function(x) text_width_in(x, font = "Arial", font_size_half_points = 24L)
  expect_equal(ar("iWn"), (222 + 944 + 556) / 1000 * 12 / 72)
  expect_equal(ar("iWn"), ar("i") + ar("W") + ar("n"))
  expect_equal(ar(c("i", "W", "iW")), c(222, 944, 1166) / 1000 * 12 / 72)
  # Times New Roman: Times-Roman: i 278, W 944, n 500
  tr <- function(x) text_width_in(x, font = "Times New Roman", font_size_half_points = 24L)
  expect_equal(tr("iWn"), (278 + 944 + 500) / 1000 * 12 / 72)
  expect_equal(tr("iWn"), tr("i") + tr("W") + tr("n"))
  expect_length(tr(c("a", "bb", "ccc")), 3L)
})

# ------------------------------------------------------- style_body, style_cols

# The \trowd line of the table row that holds `txt` as a whole cell.
.t10_row <- function(rtf, txt) {
  grep(paste0(" ", txt, "\\"), grep("trowd", rtf, fixed = TRUE, value = TRUE),
       fixed = TRUE, value = TRUE)[1L]
}

# The body rows of x that carry `flag` (bold, italic, ...) on any cell.
.t10_flag_rows <- function(x, flag) {
  which(vapply(x$cell_styles, function(r) any(r[[flag]] %in% TRUE), logical(1L)))
}

.t10_df <- function() {
  data.frame(Item = c("Age", "Sex", "Mean", "Max"), A = c("1", "2", "3", "4"),
             B = c("5", "6", "7", "8"), stringsAsFactors = FALSE)
}

test_that("FT-TB6005 [FR-TB395]: style_body(rows = ) takes NULL (every row), positions, a logical over all rows, a predicate and a one-sided formula", {
  t0 <- rtftable(.t10_df())
  # NULL: every body row
  expect_identical(.t10_flag_rows(style_body(t0, bold = TRUE), "bold"), 1:4)
  expect_identical(.t10_flag_rows(style_body(t0, rows = NULL, bold = TRUE), "bold"), 1:4)
  # integer positions
  expect_identical(.t10_flag_rows(style_body(t0, rows = c(4, 2), bold = TRUE), "bold"), c(2L, 4L))
  # a logical vector over all the body rows (4 of them)
  expect_identical(.t10_flag_rows(style_body(t0, rows = c(TRUE, FALSE, FALSE, TRUE), bold = TRUE),
                                  "bold"), c(1L, 4L))
  # a predicate function(data) giving one logical per row: A is "3" and "4" in rows 3, 4
  expect_identical(.t10_flag_rows(style_body(t0, rows = function(d) as.numeric(d$A) > 2,
                                             bold = TRUE), "bold"), c(3L, 4L))
  # a one-sided formula evaluated in the body data
  f <- style_body(t0, rows = ~ Item == "Mean", bold = TRUE)
  expect_identical(.t10_flag_rows(f, "bold"), 3L)
  # and the RTF: only the "Mean" row carries \b
  rtf <- .t10_rtf(f)
  expect_match(.t10_row(rtf, "Mean"), "\\b Mean\\b0 ", fixed = TRUE)
  expect_false(grepl("\\b ", .t10_row(rtf, "Age"), fixed = TRUE))
})

test_that("FT-TB6006 [FR-TB396]: style_body(rows = ) on several data frames counts rows across them, evaluates a predicate or formula on each in turn, and NA selects nothing", {
  d1 <- data.frame(g = c("a", "b"), b = c(1, 2), stringsAsFactors = FALSE)
  d2 <- data.frame(g = c("c", "d", "e"), b = c(0, 3, NA), stringsAsFactors = FALSE)
  m <- rtftable(list(d1, d2))
  # integer rows are counted across both frames: rows 3 and 5 are "c" and "e"
  expect_identical(.t10_flag_rows(style_body(m, rows = c(3, 5), italic = TRUE), "italic"),
                   c(3L, 5L))
  # the formula is evaluated on each frame in turn: b == max(b) holds for the maximum of
  # each frame, row 2 of the first (global 2) and row 2 of the second (3 -> global 4);
  # evaluated on the stacked data the maximum would be one row only
  expect_identical(.t10_flag_rows(style_body(m, rows = ~ b == max(b, na.rm = TRUE), bold = TRUE),
                                  "bold"), c(2L, 4L))
  expect_identical(.t10_flag_rows(style_body(m, rows = function(df) df$b == max(df$b, na.rm = TRUE),
                                             bold = TRUE), "bold"), c(2L, 4L))
  # the NA of global row 5 (b is NA) selects nothing: b > 0 holds in rows 1, 2, 4
  expect_identical(.t10_flag_rows(style_body(m, rows = ~ b > 0, bold = TRUE), "bold"),
                   c(1L, 2L, 4L))
  # the row count is the 5 rows of both frames
  expect_error(style_body(m, rows = 6, bold = TRUE), "positions must be in 1..5")
  expect_identical(.t10_flag_rows(style_body(m, rows = 5, bold = TRUE), "bold"), 5L)
})

test_that("FT-TB6007 [FR-TB399]: style_body(cols = ) is every column by default, names and positions mix in a list(), against the final printed columns", {
  d <- data.frame(Param = c("x", "x"), Visit = c("V1", "V2"), A = c("1", "2"),
                  B = c("3", "4"), stringsAsFactors = FALSE)
  tbl <- as_rtftables(d, stub = stub_spec(vars = c("Param", "Visit")))[[1L]]
  # the final printed columns: the stub, then A and B
  expect_identical(names(tbl$data), c("Param / Visit", "A", "B"))
  # NULL: every column
  expect_identical(style_body(tbl, bold = TRUE)$cell_styles[[1L]]$bold, rep(TRUE, 3L))
  expect_identical(style_body(tbl, cols = NULL, bold = TRUE)$cell_styles[[1L]]$bold, rep(TRUE, 3L))
  # a name and a position in one list(): the stub (1) and B (3)
  out <- style_body(tbl, rows = 1, cols = list("Param / Visit", 3), bold = TRUE)
  expect_identical(out$cell_styles[[1L]]$bold, c(TRUE, NA, TRUE))
  expect_null(out$cell_styles[[2L]])
  # the printed columns, not the source's: "Param" no longer exists, position 4 is out
  expect_error(style_body(tbl, cols = "Param", bold = TRUE), "column 'Param' not found")
  expect_error(style_body(tbl, cols = 4, bold = TRUE), "index 4 out of range \\(1..3\\)")
  # in the RTF: row 1 has \b in the stub and B only
  # (row 1 is the group row "x"; \b sits on the stub and on B)
  row <- .t10_row(.t10_rtf(out), "x")
  expect_false(is.na(row))
  expect_identical(lengths(regmatches(row, gregexpr("\\b ", row, fixed = TRUE))), 2L)
})

test_that("FT-TB6008 [FR-TB401]: style_body(bold = , italic = , underline = ) set the flag on the selected cells, NULL leaves it as it is", {
  t0 <- rtftable(.t10_df())
  # the cell of row 2, column 2 only
  t1 <- style_body(t0, rows = 2, cols = 2, bold = TRUE, italic = TRUE, underline = TRUE)
  cs <- t1$cell_styles[[2L]]
  expect_identical(cs$bold, c(NA, TRUE, NA))
  expect_identical(cs$italic, c(NA, TRUE, NA))
  expect_identical(cs$underline, c(NA, TRUE, NA))
  expect_null(t1$cell_styles[[1L]])
  row <- .t10_row(.t10_rtf(t1), "2")
  expect_match(row, "\\b \\i \\ul 2\\ulnone \\i0 \\b0 ", fixed = TRUE)
  # NULL (the default) leaves a flag as it is: italic is switched off, bold and underline stay
  t2 <- style_body(t1, rows = 2, cols = 2, italic = FALSE)
  expect_identical(t2$cell_styles[[2L]]$bold, c(NA, TRUE, NA))
  expect_identical(t2$cell_styles[[2L]]$underline, c(NA, TRUE, NA))
  expect_identical(t2$cell_styles[[2L]]$italic, c(NA, FALSE, NA))
  # a table styled with no flag at all has none
  expect_null(style_body(t0, rows = 2, align = "left")$cell_styles[[2L]]$bold)
  # and the other rows' cells carry no flag in the RTF
  expect_false(grepl("\\b ", .t10_row(.t10_rtf(t1), "Age"), fixed = TRUE))
})

test_that("FT-TB6009 [FR-TB405]: style_body(background = ) fills the selected cells over the column's own fill, NULL leaves them unfilled, an empty or unknown colour is not drawn, a colour used is in the colour table", {
  d <- data.frame(Item = c("Age", "Sex"), A = c("1", "2"), stringsAsFactors = FALSE)
  # NULL (the default): nothing is filled
  expect_false(any(grepl("clcbpat", .t10_rtf(rtftable(d)), fixed = TRUE)))
  # the selected cells (row 2, both columns) are filled, in the colour table as the 4th
  # entry after auto (0), black and white
  t1 <- style_body(rtftable(d), rows = 2, background = "#123456")
  rtf <- .t10_rtf(t1)
  expect_true(any(grepl("\\red18\\green52\\blue86;", rtf, fixed = TRUE)))
  expect_identical(lengths(regmatches(.t10_row(rtf, "Sex"), gregexpr("clcbpat3", .t10_row(rtf, "Sex")))), 2L)
  expect_false(grepl("clcbpat", .t10_row(rtf, "Age"), fixed = TRUE))
  # it overrides the column's own fill: column A is grey in both rows, row 2 is pink there
  t2 <- style_body(style_cols(rtftable(d), cols = "A", background = "#EFEFEF"),
                   rows = 2, cols = "A", background = "#F8D7DA")
  rtf2 <- .t10_rtf(t2)
  # colours: 3 = EFEFEF, 4 = F8D7DA
  expect_true(any(grepl("\\red239\\green239\\blue239;\\red248\\green215\\blue218;", rtf2,
                        fixed = TRUE)))
  expect_match(.t10_row(rtf2, "Age"), "clcbpat3", fixed = TRUE)
  expect_match(.t10_row(rtf2, "Sex"), "clcbpat4", fixed = TRUE)
  expect_false(grepl("clcbpat3", .t10_row(rtf2, "Sex"), fixed = TRUE))
  # an empty or NA colour is not drawn and adds nothing to the colour table
  for (bg in list("", NA_character_)) {
    rtf3 <- .t10_rtf(style_body(rtftable(d), rows = 2, background = bg))
    expect_false(any(grepl("clcbpat", rtf3, fixed = TRUE)))
    expect_true(any(grepl("{\\colortbl;\\red0\\green0\\blue0;\\red255\\green255\\blue255;}",
                          rtf3, fixed = TRUE)))
  }
  # a colour that is not in the document's colour table is not drawn
  expect_identical(.cell_shading_cmd("#FFFFFF", list("#000000" = 1L)), "")
  expect_identical(.cell_shading_cmd("#FFFFFF", list("#FFFFFF" = 7L)), "\\clcbpat7")
})

test_that("FT-TB6010 [FR-TB410]: style_body() keeps per-cell borders and alignments with their columns when drop_cols removes columns, and a selection that matches no row leaves the table unchanged", {
  df <- data.frame(Item = c("Age", "Sex"), A = c("1", "2"), B = c("3", "4"), C = c("5", "6"),
                   stringsAsFactors = FALSE)
  b <- rtf_border(bottom = rtf_border_line("double"))
  styles <- list(list(align = c("right", "left", NA, "center"),
                      border = list(NULL, b, NULL, NULL)),
                 NULL)
  # drop column 3 (B): the styles of Item, A and C stay with them
  p3 <- as_rtftables(df, drop_cols = 3, cell_styles = styles)[[1L]]
  expect_identical(names(p3$data), c("Item", "A", "C"))
  expect_identical(p3$cell_styles[[1L]]$align, c("right", "left", "center"))
  expect_length(p3$cell_styles[[1L]]$border, 3L)
  expect_identical(p3$cell_styles[[1L]]$border[[2L]]$bottom$style, "double")
  expect_null(p3$cell_styles[[1L]]$border[[1L]])
  # drop the first column: A's border becomes the first, its alignment the first
  p1 <- as_rtftables(df, drop_cols = 1, cell_styles = styles)[[1L]]
  expect_identical(names(p1$data), c("A", "B", "C"))
  expect_identical(p1$cell_styles[[1L]]$align, c("left", NA, "center"))
  expect_identical(p1$cell_styles[[1L]]$border[[1L]]$bottom$style, "double")
  # and the cell is rendered: the double bottom border (\brdrdb) on the first cell of row 1
  row <- .t10_row(.t10_rtf(p1), "1")
  expect_match(row, "\\clbrdrb\\brdrdb", fixed = TRUE)
  expect_match(row, "\\ql\\li0\\ri0 1\\cell", fixed = TRUE)
  # a selection that matches no row: the table unchanged
  tbl <- rtftable(.t10_df())
  expect_identical(style_body(tbl, rows = ~ Item == "nothing", bold = TRUE), tbl)
  expect_identical(style_body(tbl, rows = function(d) d$Item == "zzz", bold = TRUE), tbl)
  expect_identical(style_body(tbl, rows = integer(0), bold = TRUE), tbl)
  expect_identical(style_body(tbl, rows = rep(FALSE, 4L), bold = TRUE), tbl)
})

test_that("FT-TB6011 [FR-TB411]: style_body() on a list of pages styles every page, a predicate or formula per page, and refuses integer or logical rows as ambiguous", {
  df <- data.frame(Item = letters[1:4], A = 1:4, stringsAsFactors = FALSE)
  pages <- as_rtftables(df, split = "group_force", max_rows = 2)
  expect_length(pages, 2L)
  # a formula is evaluated per page: "c" is the first row of page 2 only
  f <- style_body(pages, rows = ~ Item == "c", bold = TRUE)
  expect_length(f, 2L)
  expect_null(f[[1L]]$cell_styles)
  expect_identical(.t10_flag_rows(f[[2L]], "bold"), 1L)
  # a predicate: A is above 1 in row 2 of page 1 and both rows of page 2
  p <- style_body(pages, rows = function(d) d$A > 1, italic = TRUE)
  expect_identical(.t10_flag_rows(p[[1L]], "italic"), 2L)
  expect_identical(.t10_flag_rows(p[[2L]], "italic"), 1:2)
  # every page is styled: the whole page when rows is NULL
  a <- style_body(pages, bold = TRUE)
  expect_identical(vapply(a, function(x) length(.t10_flag_rows(x, "bold")), integer(1L)), c(2L, 2L))
  # integer and logical rows are refused
  expect_error(style_body(pages, rows = 1, bold = TRUE), "ambiguous")
  expect_error(style_body(pages, rows = c(TRUE, FALSE), bold = TRUE), "ambiguous")
  expect_error(style_body(pages, rows = 1:2, bold = TRUE), "ambiguous")
})

# The \cf / \clcbpat index of a colour "#RRGGBB" in the document's colour table (entry 1
# is the first after the empty "auto" one).
.t10_cidx <- function(rtf, hex) {
  line <- grep("\\colortbl", rtf, fixed = TRUE, value = TRUE)[1L]
  ents <- strsplit(sub("\\}$", "", sub("^\\{\\\\colortbl;", "", line)), ";", fixed = TRUE)[[1L]]
  v <- strtoi(substring(hex, c(2L, 4L, 6L), c(3L, 5L, 7L)), 16L)
  match(sprintf("\\red%d\\green%d\\blue%d", v[1L], v[2L], v[3L]), ents)
}

.t10_tb2 <- function() {
  data.frame(Item = c("Age", "Sex"), A = c("1", "2"), B = c("4", "5"), stringsAsFactors = FALSE)
}

test_that("FT-TB6012 [FR-TB414]: style_cols() patches the selected columns' specification (all when cols is NULL), returns a copy, for a table or each page of a list, and the other columns keep their look", {
  t1 <- rtftable(.t10_tb2())
  bolds <- function(x) vapply(x$col_spec, function(s) s$bold, logical(1L))
  # the selected column only
  one <- style_cols(t1, cols = "A", bold = TRUE)
  expect_identical(bolds(one), c(FALSE, TRUE, FALSE))
  # all when cols is NULL
  expect_identical(bolds(style_cols(t1, bold = TRUE)), c(TRUE, TRUE, TRUE))
  expect_identical(bolds(style_cols(t1, cols = NULL, bold = TRUE)), c(TRUE, TRUE, TRUE))
  # a modified copy: the original keeps its specification
  expect_identical(bolds(t1), c(FALSE, FALSE, FALSE))
  expect_s3_class(one, "rtftable")
  # the other columns keep their look: the same spec as before, field by field
  expect_identical(one$col_spec[[1L]], t1$col_spec[[1L]])
  expect_identical(one$col_spec[[3L]], t1$col_spec[[3L]])
  # in the RTF only column A's body cells are bold
  rtf <- .t10_rtf(one)
  expect_match(.t10_row(rtf, "1"), "\\b 1\\b0 ", fixed = TRUE)
  expect_false(grepl("\\b Age", .t10_row(rtf, "Age"), fixed = TRUE))
  expect_false(grepl("\\b 4", .t10_row(rtf, "Age"), fixed = TRUE))
  # a list of pages: each page is patched, the list keeps its length and names
  pages <- list(p1 = t1, p2 = t1)
  out <- style_cols(pages, cols = 1, align = "right")
  expect_identical(names(out), c("p1", "p2"))
  for (p in out) {
    expect_identical(vapply(p$col_spec, function(s) s$align, ""), c("right", "center", "center"))
  }
  expect_identical(vapply(t1$col_spec, function(s) s$align, ""), c("left", "center", "center"))
})

test_that("FT-TB6013 [FR-TB415]: style_cols(cols = ) takes names, positions, both in a list(), NULL for all, and names the missing column or the position out of range", {
  t1 <- rtftable(.t10_tb2())
  bolds <- function(x) vapply(x$col_spec, function(s) s$bold, logical(1L))
  expect_identical(bolds(style_cols(t1, cols = "B", bold = TRUE)), c(FALSE, FALSE, TRUE))
  expect_identical(bolds(style_cols(t1, cols = 2, bold = TRUE)), c(FALSE, TRUE, FALSE))
  expect_identical(bolds(style_cols(t1, cols = list("Item", 3), bold = TRUE)), c(TRUE, FALSE, TRUE))
  expect_identical(bolds(style_cols(t1, cols = NULL, bold = TRUE)), c(TRUE, TRUE, TRUE))
  expect_error(style_cols(t1, cols = "nope", bold = TRUE),
               "`style_cols(cols)` column 'nope' not found in the table.", fixed = TRUE)
  expect_error(style_cols(t1, cols = 9, bold = TRUE),
               "`style_cols(cols)` index 9 out of range (1..3).", fixed = TRUE)
  expect_error(style_cols(t1, cols = list("A", "nope"), bold = TRUE),
               "`style_cols(cols)` column 'nope' not found in the table.", fixed = TRUE)
})

test_that("FT-TB6014 [FR-TB416]: style_cols(align = ) aligns the body cells of the columns, header_align the header label, left / center / right", {
  t1 <- rtftable(.t10_tb2())
  ctl <- c(left = "ql", center = "qc", right = "qr")
  for (al in names(ctl)) {
    a <- ctl[[al]]
    # body alignment of column A (in the body rows), the header left as it was
    rtf <- .t10_rtf(style_cols(t1, cols = "A", align = al))
    expect_match(.t10_row(rtf, "1"), paste0("\\", a, "\\li0\\ri0 1\\cell"), fixed = TRUE)
    expect_match(.t10_row(rtf, "Age"), "\\ql\\li0\\ri0 Age\\cell", fixed = TRUE)
    expect_match(.t10_row(rtf, "A"), "\\qc\\li0\\ri0 A\\cell", fixed = TRUE)
    # header_align: the header label of column A only; the body cell is as it was (\qc)
    rtf2 <- .t10_rtf(style_cols(t1, cols = "A", header_align = al))
    expect_match(.t10_row(rtf2, "A"), paste0("\\", a, "\\li0\\ri0 A\\cell"), fixed = TRUE)
    expect_match(.t10_row(rtf2, "1"), "\\qc\\li0\\ri0 1\\cell", fixed = TRUE)
  }
  expect_error(style_cols(t1, cols = 2, align = "middle"), "left")
  expect_error(style_cols(t1, cols = 2, header_align = "middle"), "left")
})

test_that("FT-TB6015 [FR-TB418]: style_cols(bold = , italic = , underline = ) set the body-cell flags, header_bold and header_italic the header label", {
  t1 <- rtftable(.t10_tb2())
  body <- style_cols(t1, cols = "A", bold = TRUE, italic = TRUE, underline = TRUE)
  sp <- body$col_spec[[2L]]
  expect_true(sp$bold && sp$italic && sp$underline)
  expect_false(isTRUE(sp$header_bold) || isTRUE(sp$header_italic))
  rtf <- .t10_rtf(body)
  expect_match(.t10_row(rtf, "1"), "\\b \\i \\ul 1\\ulnone \\i0 \\b0 ", fixed = TRUE)
  expect_match(.t10_row(rtf, "2"), "\\b \\i \\ul 2\\ulnone \\i0 \\b0 ", fixed = TRUE)
  # the header label stays plain; the other columns' cells too
  expect_match(.t10_row(rtf, "A"), "\\qc\\li0\\ri0 A\\cell", fixed = TRUE)
  expect_match(.t10_row(rtf, "Age"), "\\ql\\li0\\ri0 Age\\cell", fixed = TRUE)
  # header_bold and header_italic: the header label only, the body cell stays plain
  hdr <- style_cols(t1, cols = "A", header_bold = TRUE, header_italic = TRUE)
  expect_true(hdr$col_spec[[2L]]$header_bold && hdr$col_spec[[2L]]$header_italic)
  expect_false(isTRUE(hdr$col_spec[[2L]]$bold))
  rtf2 <- .t10_rtf(hdr)
  expect_match(.t10_row(rtf2, "A"), "\\b \\i A\\i0 \\b0 \\cell", fixed = TRUE)
  expect_match(.t10_row(rtf2, "1"), "\\qc\\li0\\ri0 1\\cell", fixed = TRUE)
})

test_that("FT-TB6016 [FR-TB420]: style_cols(indent_twips = ) sets the body cells' left indent in twips and color = their text colour", {
  t1 <- rtftable(.t10_tb2())
  out <- style_cols(t1, cols = "A", indent_twips = 360, color = "#FF0000")
  expect_identical(out$col_spec[[2L]]$indent_twips, 360L)
  expect_identical(out$col_spec[[2L]]$color, "#FF0000")
  rtf <- .t10_rtf(out)
  i <- .t10_cidx(rtf, "#FF0000")
  expect_false(is.na(i))
  # body cells of A: \li360 and the colour; the header label and the other columns: neither
  expect_match(.t10_row(rtf, "1"), sprintf("\\qc\\li360\\ri0 \\cf%d 1\\cf1 \\cell", i), fixed = TRUE)
  expect_match(.t10_row(rtf, "2"), sprintf("\\qc\\li360\\ri0 \\cf%d 2\\cf1 \\cell", i), fixed = TRUE)
  expect_match(.t10_row(rtf, "A"), "\\qc\\li0\\ri0 A\\cell", fixed = TRUE)
  expect_match(.t10_row(rtf, "Age"), "\\ql\\li0\\ri0 Age\\cell", fixed = TRUE)
  expect_identical(lengths(regmatches(.t10_row(rtf, "Age"), gregexpr("\\cf", .t10_row(rtf, "Age"), fixed = TRUE))), 2L)
  # no indent without the argument
  expect_false(any(grepl("\\li360", .t10_rtf(t1), fixed = TRUE)))
})

test_that("FT-TB6017 [FR-TB421]: style_cols(background = ) fills the body cells, header_background the header cells only, NULL leaves cells unfilled", {
  t1 <- rtftable(.t10_tb2())
  expect_false(any(grepl("clcbpat", .t10_rtf(t1), fixed = TRUE)))
  # background: the body cells of A, not the header cell
  rtf <- .t10_rtf(style_cols(t1, cols = "A", background = "#C0C0C0"))
  i <- .t10_cidx(rtf, "#C0C0C0")
  expect_false(is.na(i))
  expect_match(.t10_row(rtf, "1"), sprintf("\\clcbpat%d\\clvertalb", i), fixed = TRUE)
  expect_match(.t10_row(rtf, "2"), sprintf("\\clcbpat%d\\clvertalb", i), fixed = TRUE)
  expect_false(grepl("clcbpat", .t10_row(rtf, "A"), fixed = TRUE))
  expect_identical(lengths(regmatches(.t10_row(rtf, "Age"), gregexpr("clcbpat", .t10_row(rtf, "Age"), fixed = TRUE))), 1L)
  # header_background: the header cell of A only, the body is not shaded
  rtf2 <- .t10_rtf(style_cols(t1, cols = "A", header_background = "#D6E4F0"))
  j <- .t10_cidx(rtf2, "#D6E4F0")
  expect_false(is.na(j))
  expect_match(.t10_row(rtf2, "A"), sprintf("\\clcbpat%d\\clvertalb", j), fixed = TRUE)
  expect_false(grepl("clcbpat", .t10_row(rtf2, "1"), fixed = TRUE))
  expect_identical(sum(grepl("clcbpat", rtf2, fixed = TRUE)), 1L)
  # both together are kept apart: two colours, header and body each their own
  rtf3 <- .t10_rtf(style_cols(t1, cols = "A", background = "#C0C0C0", header_background = "#D6E4F0"))
  expect_match(.t10_row(rtf3, "A"),
               sprintf("\\clcbpat%d\\clvertalb", .t10_cidx(rtf3, "#D6E4F0")), fixed = TRUE)
  expect_match(.t10_row(rtf3, "1"),
               sprintf("\\clcbpat%d\\clvertalb", .t10_cidx(rtf3, "#C0C0C0")), fixed = TRUE)
  # header_color colours the header label only
  rtf4 <- .t10_rtf(style_cols(t1, cols = "A", header_color = "#336699"))
  expect_match(.t10_row(rtf4, "A"), sprintf("\\cf%d A\\cf1 ", .t10_cidx(rtf4, "#336699")), fixed = TRUE)
  expect_false(grepl("\\cf", .t10_row(rtf4, "1"), fixed = TRUE))
})

# ------------------------------------------------------- style_header, style_zone

# A table with a spanner "Group" over columns 3-4 above the leaf labels, and a row of
# "(N = 9)" between them when `n` is TRUE.
.t10_hdr_tbl <- function(n = TRUE, border = NULL) {
  d <- data.frame(Item = c("Age", "Sex", "Mean"), A = c("1", "2", "3"), B = c("4", "5", "6"),
                  C = c("7", "8", "9"), stringsAsFactors = FALSE)
  rows <- list(list(col_cell(1, ""), col_cell(2, ""), col_cell(c(3, 4), "Group")))
  if (n) rows <- c(rows, list(list(col_cell(1, ""), col_cell(2, ""), col_cell(c(3, 4), "(N = 9)"))))
  hdr <- do.call(rtf_col_header, c(rows, list(c("Item", "A", "B", "C"))))
  if (is.null(border)) rtftable(d, col_header = hdr) else rtftable(d, col_header = hdr, border = border)
}

# How many times `pat` occurs (fixed) in each rendered row of a table.
.t10_count <- function(tbl, pat) {
  vapply(.render_rtftable(tbl, 9360L), function(r) {
    m <- gregexpr(pat, r, fixed = TRUE)[[1L]]
    if (m[[1L]] == -1L) 0L else length(m)
  }, integer(1L), USE.NAMES = FALSE)
}

.t10_reset_edge_warning <- function() {
  if (exists("border_edge_reading", envir = .deprecation_state, inherits = FALSE)) {
    rm(list = "border_edge_reading", envir = .deprecation_state)
  }
}

test_that("FT-TB6018 [FR-TB426]: style_header(row = ) targets every header row by default, or the header rows given, 1 being the top", {
  t3 <- .t10_hdr_tbl()
  # rows: 1 the spanner "Group", 2 "(N = 9)", 3 the leaf labels
  bold_rows <- function(x) {
    r <- .render_rtftable(x, 9360L)[1:3]
    c(grepl("\\b Group\\b0", r[[1L]], fixed = TRUE), grepl("\\b (N = 9)\\b0", r[[2L]], fixed = TRUE),
      grepl("\\b Item\\b0", r[[3L]], fixed = TRUE))
  }
  expect_identical(bold_rows(t3), c(FALSE, FALSE, FALSE))
  # NULL (the default): every header row
  expect_identical(bold_rows(style_header(t3, bold = TRUE)), c(TRUE, TRUE, TRUE))
  expect_identical(bold_rows(style_header(t3, row = NULL, bold = TRUE)), c(TRUE, TRUE, TRUE))
  # integer indices, 1 the top row
  expect_identical(bold_rows(style_header(t3, row = 1, bold = TRUE)), c(TRUE, FALSE, FALSE))
  expect_identical(bold_rows(style_header(t3, row = 2, bold = TRUE)), c(FALSE, TRUE, FALSE))
  expect_identical(bold_rows(style_header(t3, row = 3, bold = TRUE)), c(FALSE, FALSE, TRUE))
  expect_identical(bold_rows(style_header(t3, row = c(1, 3), bold = TRUE)), c(TRUE, FALSE, TRUE))
  # the cells of the rows named carry the flag, the others' do not
  out <- style_header(t3, row = 2, bold = TRUE)
  expect_true(all(vapply(out$col_header[[2L]], function(z) isTRUE(z$bold), NA)))
  expect_false(any(vapply(out$col_header[[1L]], function(z) isTRUE(z$bold), NA)))
  # a row that is not there is refused
  expect_error(style_header(t3, row = 4, bold = TRUE), "`style_header\\(row = \\)` must be in 1..3")
})

test_that("FT-TB6019 [FR-TB430]: style_header(border = ) merges per side, a later call wins per side, and none erases the automatic group underline", {
  t2 <- .t10_hdr_tbl()
  # the automatic underline: row 2's last cell (the "(N = 9)" span) has \clbrdrb
  expect_identical(.t10_count(t2, "\\clbrdrb\\brdrs\\brdrw15")[2L], 1L)
  # a top rule is added, the underline survives where the new border leaves bottom NULL
  b1 <- style_header(t2, row = 2, cols = 3, border = rtf_border(top = TRUE))
  cell <- b1$col_header[[2L]][[3L]]
  expect_identical(cell$border$top, rtf_border_line("single", 15L))
  expect_null(cell$border$bottom)
  r2 <- .render_rtftable(b1, 9360L)[[2L]]
  expect_match(r2, "\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw15\\clvertalb\\cellx9360", fixed = TRUE)
  # a later call wins per side: top is now double; the bottom of the call before survives
  b2 <- style_header(style_header(b1, row = 2, cols = 3, border = rtf_border(bottom = "dot")),
                     row = 2, cols = 3, border = rtf_border(top = "double"))
  cell2 <- b2$col_header[[2L]][[3L]]
  expect_identical(cell2$border$top$style, "double")
  expect_identical(cell2$border$bottom$style, "dot")
  expect_match(.render_rtftable(b2, 9360L)[[2L]],
               "\\clbrdrt\\brdrdb\\brdrw15\\clbrdrb\\brdrdot\\brdrw15\\clvertalb\\cellx9360", fixed = TRUE)
  # a side the new border leaves NULL stays: top is still double after a bottom-only call
  expect_identical(style_header(b2, row = 2, cols = 3, border = rtf_border(bottom = "thick"))$col_header[[2L]][[3L]]$border$top$style,
                   "double")
  # "none" erases the automatic underline of the spanning cell: no \clbrdrb on that cell
  gone <- style_header(t2, row = 2, cols = 3, border = rtf_border(bottom = "none"))
  expect_identical(gone$col_header[[2L]][[3L]]$border$bottom$style, "none")
  expect_identical(.t10_count(gone, "\\clbrdrb\\brdrs\\brdrw15")[2L], 0L)
  expect_match(.render_rtftable(gone, 9360L)[[2L]], "\\clvertalb\\cellx5200\\clvertalb\\cellx9360", fixed = TRUE)
  # the other cells are untouched: the leaf row keeps its four underlines
  expect_identical(.t10_count(gone, "\\clbrdrb\\brdrs\\brdrw15")[3L], 4L)
  # a label row turns into cells when it takes a border
  lab <- style_header(rtftable(.t10_tb2()), cols = 2, border = rtf_border(bottom = "double"))
  expect_true(is.list(lab$col_header[[1L]]))
  expect_identical(lab$col_header[[1L]][[2L]]$border$bottom$style, "double")
})

test_that("FT-TB6020 [FR-TB436]: style_header() on a table with no header of its own makes the printed column names its header and styles that; on a table with no columns it stops", {
  d <- data.frame(Param = c("x", "x"), Visit = c("V1", "V2"), A = c("1", "2"), stringsAsFactors = FALSE)
  # no header of its own: the names of the data
  plain <- rtftable(d)
  expect_null(plain$col_header)
  out <- style_header(plain, bold = TRUE)
  expect_identical(out$col_header, list(c("Param", "Visit", "A")))
  expect_identical(vapply(out$col_spec, function(s) s$header_bold, NA), c(TRUE, TRUE, TRUE))
  r1 <- .render_rtftable(out, 9360L)[[1L]]
  expect_match(r1, "\\b Param\\b0 \\cell", fixed = TRUE)
  expect_match(r1, "\\b A\\b0 \\cell", fixed = TRUE)
  # what is printed: a stub built from two columns is "Param / Visit"
  stub <- as_rtftables(d, stub = stub_spec(vars = c("Param", "Visit")))[[1L]]
  expect_identical(style_header(stub, italic = TRUE)$col_header, list(c("Param / Visit", "A")))
  # a table with no columns stops
  expect_error(style_header(rtftable(data.frame()), bold = TRUE),
               "`style_header()`: this rtftable has no column header.", fixed = TRUE)
})

test_that("FT-TB6021 [FR-TB439 FR-TB440]: style_zone() merges each rtf_border() onto its zone side by side, from an empty border, returning a copy; a NULL zone is untouched, no zone returns the table", {
  t0 <- .t10_hdr_tbl(n = FALSE, border = "none")
  expect_null(t0$border)
  .t10_reset_edge_warning()
  on.exit(.t10_reset_edge_warning(), add = TRUE)
  z <- function(...) suppressWarnings(style_zone(t0, ...))
  # rows: 1 spanner, 2 leaf labels, 3-5 the data rows (Age, Sex, Mean)
  # header: the leaf row's bottom and the spanner's underline
  h <- z(header = rtf_border(bottom = "double"))
  expect_identical(.t10_count(h, "\\clbrdrb\\brdrdb"), c(1L, 4L, 0L, 0L, 0L))
  # spanning: the spanning row only
  s <- z(spanning = rtf_border(bottom = "dot"))
  expect_identical(.t10_count(s, "\\clbrdrb\\brdrdot"), c(1L, 0L, 0L, 0L, 0L))
  # body: the outer edge of the data rows, the bottom of the last
  b <- z(body = rtf_border(bottom = "dash"))
  expect_identical(.t10_count(b, "\\clbrdrb\\brdrdash"), c(0L, 0L, 0L, 0L, 4L))
  # first_row and last_row: the first and the last data row
  fl <- z(first_row = rtf_border(top = "thick"), last_row = rtf_border(bottom = "double"))
  expect_identical(.t10_count(fl, "\\clbrdrt\\brdrth"), c(0L, 0L, 4L, 0L, 0L))
  expect_identical(.t10_count(fl, "\\clbrdrb\\brdrdb"), c(0L, 0L, 0L, 0L, 4L))
  # each lands in its own zone of the border, the others stay unset
  zones <- c("header", "spanning", "body", "first_row", "last_row")
  for (zn in zones) {
    o <- do.call(style_zone, c(list(t0), stats::setNames(list(rtf_border(bottom = "dot")), zn)))
    expect_identical(o$border[[zn]]$bottom$style, "dot")
    for (k in setdiff(zones, zn)) expect_null(o$border[[k]])
  }
  # returns a modified copy: the original has no border
  expect_null(t0$border)
  expect_s3_class(h, "rtftable")
  # merged side by side: a later top keeps the bottom, FALSE and "none" erase a side
  m <- style_zone(h, header = rtf_border(top = "dash"))
  expect_identical(m$border$header$top$style, "dash")
  expect_identical(m$border$header$bottom$style, "double")
  expect_identical(style_zone(m, header = rtf_border(bottom = FALSE))$border$header$bottom$style, "none")
  expect_identical(style_zone(m, header = rtf_border(top = "none"))$border$header$top$style, "none")
  expect_identical(style_zone(m, header = rtf_border(top = "none"))$border$header$bottom$style, "double")
  # a zone left NULL is untouched; a call with none returns the table unchanged
  expect_identical(style_zone(m), m)
  expect_identical(style_zone(m, header = NULL), m)
  o2 <- style_zone(m, body = rtf_border(bottom = "dot", inside_h = "none"))
  for (k in c("header", "spanning", "first_row", "last_row")) expect_identical(o2$border[[k]], m$border[[k]])
})

test_that("FT-TB6022 [FR-TB443]: style_zone() warns once a session, in full, when a zone border sets left / right or a multi-row zone's top / bottom without naming inside_v / inside_h", {
  .t10_reset_edge_warning()
  on.exit(.t10_reset_edge_warning(), add = TRUE)
  d <- data.frame(Item = c("a", "b", "c"), A = c("1", "2", "3"), stringsAsFactors = FALSE)
  tbl <- rtftable(d, border = "none")
  s <- rtf_border_line()
  msg <- paste0(
    "The edges of `rtf_border()` now mean the *outer* edges of whatever you attach it to.\n",
    "  Before 0.5.0 `left`/`right` were drawn on every cell of a row, and a multi-row zone's\n",
    "  `top`/`bottom` on every one of its rows.  To keep that look, name the interior rule:\n",
    "    rtf_border(left = s, right = s, inside_v = s)   # was: left/right alone\n",
    "    rtf_border(bottom = s, inside_h = s)            # was: bottom alone on a body zone\n",
    "  Naming `inside_h` / `inside_v` (with `rtf_border_line(\"none\")` for \"no rule\") ",
    "silences this.\n",
    "  See ?rtf_border, section \"Writing borders before and after 0.5.0\", for the old ",
    "and new spelling of each case.")
  caught <- function(expr) {
    ws <- character()
    withCallingHandlers(expr, warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    ws
  }
  # a multi-row zone's top, no inside_h named: warns, in full
  expect_identical(caught(style_zone(tbl, body = rtf_border(top = s))), msg)
  # once a session: silent the second time, whatever the shape
  expect_identical(caught(style_zone(tbl, body = rtf_border(top = s))), character())
  expect_identical(caught(style_zone(tbl, body = rtf_border(left = s))), character())
  # left / right on their own warn as well (after a reset)
  .t10_reset_edge_warning()
  expect_identical(caught(style_zone(tbl, body = rtf_border(left = s, right = s))), msg)
  .t10_reset_edge_warning()
  expect_identical(caught(style_zone(tbl, last_row = rtf_border(bottom = s))), character())
  expect_identical(caught(style_zone(tbl, body = rtf_border(bottom = s))), msg)
  # naming inside_h / inside_v silences it, and so does a single row
  .t10_reset_edge_warning()
  expect_identical(caught(style_zone(tbl, body = rtf_border(top = s, inside_h = "none"))), character())
  expect_identical(caught(style_zone(tbl, body = rtf_border(left = s, inside_v = "none"))), character())
  expect_identical(caught(style_zone(rtftable(d[1L, ], border = "none"), body = rtf_border(top = s))),
                   character())
  # none of them used up the session's one warning
  expect_identical(caught(style_zone(tbl, body = rtf_border(top = s))), msg)
})

# ------------------------------------------------------- rtf_table_style

.t10_style_full <- function() {
  rtf_table_style(border_header = rtf_border(bottom = "double"),
                  border_spanning = rtf_border(bottom = "dot"),
                  border_body = rtf_border(bottom = "dash", inside_h = "none"),
                  border_first_row = rtf_border(top = "thick"),
                  border_last_row = rtf_border(bottom = "dash"),
                  header_bold = TRUE, header_italic = TRUE, align = "right", bold = TRUE,
                  cell_padding_left_twips = 60L, cell_padding_right_twips = 40L)
}

.t10_ab <- function() data.frame(Item = c("Age", "Sex"), A = c("1", "2"), stringsAsFactors = FALSE)

test_that("FT-TB6023 [FR-TB445]: rtf_table_style() is a list of class rtf_table_style, and rtftable() takes a snapshot of it that later changes or derived styles do not reach", {
  st <- .t10_style_full()
  expect_type(st, "list")
  expect_s3_class(st, "rtf_table_style")
  d <- .t10_ab()
  tbl <- rtftable(d, style = st)
  before <- .t10_rtf(tbl)
  # the fields the table needs were copied at construction
  expect_identical(tbl$cell_padding_left_twips, 60L)
  expect_identical(tbl$cell_padding_right_twips, 40L)
  expect_identical(tbl$border$header, st$border_header)
  expect_identical(tbl$border$last_row, st$border_last_row)
  # changing the style afterwards ...
  st$align <- "left"
  st$header_bold <- FALSE
  st$bold <- FALSE
  st$cell_padding_left_twips <- 500L
  st$border_header <- NULL
  # ... and deriving another from it do not change the table built before
  derived <- rtf_table_style_with(st, align = "center", header_italic = FALSE)
  expect_s3_class(derived, "rtf_table_style")
  expect_identical(.t10_rtf(tbl), before)
  expect_identical(tbl$cell_padding_left_twips, 60L)
  expect_identical(tbl$border$header$bottom$style, "double")
  # a table made now from the changed style is another one
  expect_false(identical(.t10_rtf(rtftable(d, style = st)), before))
  expect_false(identical(.t10_rtf(rtftable(d, style = derived)), before))
  # the base style itself is not changed by deriving
  expect_identical(.t10_style_full()$align, "right")
  expect_identical(rtf_table_style_with(.t10_style_full(), align = "center")$align, "center")
  expect_identical(.t10_style_full()$align, "right")
})

test_that("FT-TB6024 [FR-TB449]: rtf_table_style(header_bold = , header_italic = ) are the header labels' look; any value other than TRUE is stored as FALSE", {
  expect_false(rtf_table_style()$header_bold)
  expect_false(rtf_table_style()$header_italic)
  expect_true(rtf_table_style(header_bold = TRUE)$header_bold)
  expect_true(rtf_table_style(header_italic = TRUE)$header_italic)
  for (v in list(FALSE, NA, NULL, "TRUE", "yes", 1, 1L, 0, c(TRUE, TRUE), character())) {
    expect_false(rtf_table_style(header_bold = v)$header_bold)
    expect_false(rtf_table_style(header_italic = v)$header_italic)
  }
  # the header labels of a table made with the style
  d <- .t10_ab()
  r1 <- .render_rtftable(rtftable(d, style = rtf_table_style(header_bold = TRUE)), 9360L)
  expect_match(r1[[1L]], "\\b Item\\b0 \\cell", fixed = TRUE)
  expect_false(grepl("\\i ", r1[[1L]], fixed = TRUE))
  expect_false(grepl("\\b ", r1[[2L]], fixed = TRUE))
  r2 <- .render_rtftable(rtftable(d, style = rtf_table_style(header_italic = TRUE)), 9360L)
  expect_match(r2[[1L]], "\\i Item\\i0 \\cell", fixed = TRUE)
  expect_false(grepl("\\b ", r2[[1L]], fixed = TRUE))
  r3 <- .render_rtftable(rtftable(d, style = rtf_table_style(header_bold = "yes")), 9360L)
  expect_false(grepl("\\b ", r3[[1L]], fixed = TRUE))
})

test_that("FT-TB6025 [FR-TB452]: rtf_table_style(cell_padding_left_twips = , cell_padding_right_twips = ) pad header and data cells in twips; NULL leaves the document's padding", {
  d <- .t10_ab()
  on.exit(rtfreporter_reset_defaults(), add = TRUE)
  st <- rtf_table_style(cell_padding_left_twips = 60L, cell_padding_right_twips = 40L)
  r <- .render_rtftable(rtftable(d, style = st), 9360L)
  # header cells, then both body rows: \li60 \ri40 on every cell
  for (i in 1:3) {
    expect_identical(lengths(regmatches(r[[i]], gregexpr("\\li60\\ri40", r[[i]], fixed = TRUE))), 2L)
  }
  # one side only: the other is not touched
  l <- .render_rtftable(rtftable(d, style = rtf_table_style(cell_padding_left_twips = 90L)), 9360L)
  expect_identical(lengths(regmatches(l[[2L]], gregexpr("\\li90\\ri0", l[[2L]], fixed = TRUE))), 2L)
  rr <- .render_rtftable(rtftable(d, style = rtf_table_style(cell_padding_right_twips = 30L)), 9360L)
  expect_identical(lengths(regmatches(rr[[1L]], gregexpr("\\li0\\ri30", rr[[1L]], fixed = TRUE))), 2L)
  # NULL (the default): the table carries none, and the document's padding applies
  bare <- rtftable(d, style = rtf_table_style())
  expect_null(bare$cell_padding_left_twips)
  expect_null(bare$cell_padding_right_twips)
  doc <- rtf_config(rtf_tables(rtf_document(), list(bare)),
                    style = rtf_style(cell_padding_left_twips = 55L, cell_padding_right_twips = 35L))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  x <- grep("trowd", readLines(f, warn = FALSE), value = TRUE)
  expect_length(x, 3L)
  for (row in x) {
    expect_identical(lengths(regmatches(row, gregexpr("\\li55\\ri35", row, fixed = TRUE))), 2L)
  }
})

test_that("FT-TB6026 [FR-TB455]: a style given as rtftable(border = ) or as style = with border at its default gives the table the style's five border zones", {
  st <- .t10_style_full()
  d <- .t10_ab()
  zones <- c("header", "spanning", "body", "first_row", "last_row")
  as_border <- rtftable(d, border = st)
  as_style <- rtftable(d, style = st)
  for (z in zones) {
    expect_identical(as_border$border[[z]], st[[paste0("border_", z)]])
    expect_identical(as_style$border[[z]], st[[paste0("border_", z)]])
  }
  expect_identical(as_style$border, as_border$border)
  # the zones are drawn: header bottom double, the first data row's top thick, the last
  # data row's bottom dash
  r <- .render_rtftable(as_style, 9360L)
  expect_match(r[[1L]], "\\clbrdrb\\brdrdb\\brdrw15", fixed = TRUE)
  expect_match(r[[2L]], "\\clbrdrt\\brdrth\\brdrw15", fixed = TRUE)
  expect_match(r[[3L]], "\\clbrdrb\\brdrdash\\brdrw15", fixed = TRUE)
  # (border = <style> takes the zones only: the same rules, the look of the cells is not carried)
  cells_off <- function(x) sub("\\\\q[lcr]\\\\li.*$", "", x)
  expect_identical(cells_off(.render_rtftable(as_border, 9360L)[1:3]), cells_off(r[1:3]))
  # border left at its default, no style: the preset's rules; the style's zones differ
  expect_false(identical(rtftable(d)$border, as_style$border))
  # a border named explicitly with style = wins over the style's zones
  none <- rtftable(d, style = st, border = "none")
  expect_null(none$border)
})

test_that("FT-TB6027 [FR-TB456]: rtf_table_style_tfl() is a style with a single 15-twip black rule above and below the header and nothing else, a new object on each call", {
  s <- rtf_table_style_tfl()
  expect_s3_class(s, "rtf_table_style")
  line <- rtf_border_line("single", 15L, NULL)
  expect_identical(s$border_header$top, line)
  expect_identical(s$border_header$bottom, line)
  expect_null(s$border_header$left)
  expect_null(s$border_header$right)
  for (z in c("border_spanning", "border_body", "border_first_row", "border_last_row")) {
    expect_null(s[[z]])
  }
  expect_false(s$header_bold)
  expect_null(s$header_align)
  expect_identical(s$header_align, NULL)
  # each call builds a new object: changing one leaves the next as it was
  s$header_bold <- TRUE
  s$border_header <- NULL
  s2 <- rtf_table_style_tfl()
  expect_false(s2$header_bold)
  expect_identical(s2$border_header$top, line)
  expect_identical(s2, rtf_table_style_tfl())
  # the table with the preset: the rules are above and below the header only
  r <- .render_rtftable(rtftable(.t10_ab(), style = rtf_table_style_tfl()), 9360L)
  expect_match(r[[1L]], "\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw15", fixed = TRUE)
  expect_false(any(grepl("clbrdr", r[2:3], fixed = TRUE)))
})

# ------------------------------------------------------- rtf_border, rtf_border_line

.t10_slots <- c("top", "bottom", "left", "right", "inside_h", "inside_v")

test_that("FT-TB6028 [FR-TB473]: rtf_border() is a list of class rtf_border with the six slots, every side not named unset, and building one never changes another", {
  b0 <- rtf_border()
  expect_s3_class(b0, "rtf_border")
  expect_type(b0, "list")
  expect_identical(names(b0), .t10_slots)
  for (s in .t10_slots) expect_null(b0[[s]])
  # it says everything about the border: only the sides named are set
  b1 <- rtf_border(top = TRUE, inside_v = "dot")
  expect_identical(names(b1), .t10_slots)
  expect_identical(b1$top, rtf_border_line("single", 15L))
  expect_identical(b1$inside_v, rtf_border_line("dot"))
  for (s in c("bottom", "left", "right", "inside_h")) expect_null(b1[[s]])
  # building another never changes the first
  keep <- b1
  b2 <- rtf_border(all = "double", inside_h = TRUE)
  b3 <- rtf_border(bottom = "thick")
  expect_identical(b1, keep)
  expect_identical(b1, rtf_border(top = TRUE, inside_v = "dot"))
  expect_identical(b0, rtf_border())
  expect_identical(b3$bottom$style, "thick")
  expect_null(b3$top)
  expect_identical(b2$left$style, "double")
})

test_that("FT-TB6029 [FR-TB474]: rtf_border(top, bottom, left, right) take TRUE, FALSE or none, a style name, an rtf_border_line() or NULL", {
  for (side in c("top", "bottom", "left", "right")) {
    mk <- function(v) do.call(rtf_border, stats::setNames(list(v), side))
    # TRUE: single, 15 twips, black (colour NULL)
    expect_identical(mk(TRUE)[[side]], rtf_border_line("single", 15L, NULL))
    expect_identical(mk(TRUE)[[side]]$style, "single")
    expect_identical(mk(TRUE)[[side]]$width, 15L)
    expect_null(mk(TRUE)[[side]]$color)
    # FALSE and "none": an explicit no-line
    expect_identical(mk(FALSE)[[side]]$style, "none")
    expect_identical(mk("none")[[side]]$style, "none")
    # a style name, at the default weight and colour
    for (st in c("single", "double", "thick", "dash", "dot")) {
      expect_identical(mk(st)[[side]], rtf_border_line(st, 15L, NULL))
    }
    # an rtf_border_line() with a weight and colour of its own
    ln <- rtf_border_line("double", 30L, "#003366")
    expect_identical(mk(ln)[[side]], ln)
    # NULL: unset
    expect_null(mk(NULL)[[side]])
  }
  # the other sides stay unset
  expect_null(rtf_border(top = TRUE)$bottom)
  # a value of no family is refused
  expect_error(rtf_border(top = "wavy"), "`top` must be one of")
  expect_error(rtf_border(left = 3), "`left` must be TRUE, FALSE, a border style name")
})

test_that("FT-TB6030 [FR-TB475]: rtf_border(all = ) sets top, bottom, left and right at once, a side named explicitly wins, all = FALSE erases while rtf_border() says nothing", {
  a <- rtf_border(all = "dot")
  for (s in c("top", "bottom", "left", "right")) expect_identical(a[[s]], rtf_border_line("dot"))
  expect_null(a$inside_h)
  expect_null(a$inside_v)
  # a side named explicitly wins over all
  w <- rtf_border(all = TRUE, bottom = "double", left = FALSE)
  expect_identical(w$top, rtf_border_line("single"))
  expect_identical(w$right, rtf_border_line("single"))
  expect_identical(w$bottom$style, "double")
  expect_identical(w$left$style, "none")
  # all = FALSE: an explicit "none" on every side; rtf_border() leaves them unset
  e <- rtf_border(all = FALSE)
  for (s in c("top", "bottom", "left", "right")) expect_identical(e[[s]]$style, "none")
  for (s in .t10_slots) expect_null(rtf_border()[[s]])
  # erasing means something when merged on top of a rule; saying nothing does not
  d <- .t10_ab()
  base <- style_zone(rtftable(d, border = "none"), last_row = rtf_border(bottom = TRUE))
  erased <- suppressWarnings(style_zone(base, last_row = rtf_border(all = FALSE)))
  kept <- style_zone(base, last_row = rtf_border())
  expect_identical(erased$border$last_row$bottom$style, "none")
  expect_identical(kept$border$last_row$bottom$style, "single")
  expect_false(any(grepl("clbrdrb", .render_rtftable(erased, 9360L), fixed = TRUE)))
  expect_true(any(grepl("clbrdrb", .render_rtftable(kept, 9360L), fixed = TRUE)))
})

test_that("FT-TB6031 [FR-TB476]: rtf_border(inside_h = , inside_v = ) are the rules between rows and between cells, values as the edges, NULL no rule, not printed when unset", {
  b <- rtf_border(inside_h = TRUE, inside_v = "dot")
  expect_identical(b$inside_h, rtf_border_line("single", 15L))
  expect_identical(b$inside_v, rtf_border_line("dot"))
  expect_identical(rtf_border(inside_h = FALSE)$inside_h$style, "none")
  expect_identical(rtf_border(inside_v = "none")$inside_v$style, "none")
  ln <- rtf_border_line("thick", 30L, "#FF0000")
  expect_identical(rtf_border(inside_h = ln, inside_v = ln)$inside_v, ln)
  expect_null(rtf_border(inside_h = NULL)$inside_h)
  # NULL by default
  expect_null(rtf_border()$inside_h)
  expect_null(rtf_border(top = TRUE)$inside_v)
  # not printed when unset, printed when set
  expect_false(any(grepl("inside", capture.output(print(rtf_border(top = TRUE))), fixed = TRUE)))
  expect_true(any(grepl("inside_h: single, 15 twips", capture.output(print(b)), fixed = TRUE)))
  expect_true(any(grepl("inside_v: dot, 15 twips", capture.output(print(b)), fixed = TRUE)))
  # on a body of three rows and two columns: inside_h is under rows 1 and 2, inside_v
  # is the right edge of the first cell and the left edge of the second, in every row
  t0 <- rtftable(data.frame(Item = c("Age", "Sex", "Mean"), A = c("1", "2", "3"),
                            stringsAsFactors = FALSE), border = "none")
  r <- .render_rtftable(style_zone(t0, body = rtf_border(inside_h = TRUE, inside_v = "dot")), 9360L)
  expect_identical(grepl("\\clbrdrb\\brdrs", r, fixed = TRUE), c(FALSE, TRUE, TRUE, FALSE))
  expect_identical(grepl("\\clbrdrr\\brdrdot", r, fixed = TRUE), c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(grepl("\\clbrdrl\\brdrdot", r, fixed = TRUE), c(FALSE, TRUE, TRUE, TRUE))
  # no rule where none is given
  r0 <- .render_rtftable(style_zone(t0, body = rtf_border(top = FALSE, inside_h = "none")), 9360L)
  expect_false(any(grepl("clbrdr", r0, fixed = TRUE)))
})

test_that("FT-TB6032 [FR-TB480]: an rtf_border records which of inside_h and inside_v the caller named, as an attribute that is not part of its value", {
  nm <- function(b) attr(b, "inside_named")
  expect_identical(nm(rtf_border(top = TRUE)), c(h = FALSE, v = FALSE))
  expect_identical(nm(rtf_border(top = TRUE, inside_h = TRUE)), c(h = TRUE, v = FALSE))
  expect_identical(nm(rtf_border(left = TRUE, inside_v = "none")), c(h = FALSE, v = TRUE))
  expect_identical(nm(rtf_border(inside_h = "none", inside_v = "none")), c(h = TRUE, v = TRUE))
  # naming a side as NULL still counts as naming it
  expect_identical(nm(rtf_border(inside_h = NULL)), c(h = TRUE, v = FALSE))
  # not part of the value: the two compare equal apart from the attribute
  a <- rtf_border(top = TRUE)
  b <- rtf_border(top = TRUE, inside_h = NULL)
  expect_equal(a, b, ignore_attr = TRUE)
  expect_false(identical(a, b))
  # the use of it: a left / right border without inside_v named warns (once), one that
  # names it does not
  .t10_reset_edge_warning()
  on.exit(.t10_reset_edge_warning(), add = TRUE)
  tbl <- rtftable(data.frame(Item = c("a", "b"), A = c("1", "2"), stringsAsFactors = FALSE), border = "none")
  expect_silent(style_zone(tbl, body = rtf_border(left = TRUE, inside_v = "none")))
  expect_warning(style_zone(tbl, body = rtf_border(left = TRUE)), "outer")
})

test_that("FT-TB6033 [FR-TB481]: a border line saved under the old class name rtf_border_side is still accepted as a side", {
  old <- structure(list(style = "double", width = 30L, color = "#003366"), class = "rtf_border_side")
  b <- rtf_border(top = old, bottom = old, inside_h = old)
  expect_s3_class(b, "rtf_border")
  expect_identical(b$top, old)
  expect_identical(b$bottom$style, "double")
  expect_identical(b$inside_h$color, "#003366")
  expect_identical(rtf_border(all = old)$left, old)
  # and it draws: the double rule of 30 twips in the colour
  d <- .t10_ab()
  r <- .t10_rtf(style_zone(rtftable(d, border = "none"), last_row = rtf_border(bottom = old)))
  row <- grep("trowd", r, value = TRUE)[3L]
  expect_match(row, "\\clbrdrb\\brdrdb\\brdrw30\\brdrcf3", fixed = TRUE)
  # a thing of neither class is refused
  expect_error(rtf_border(top = structure(list(style = "single"), class = "other")),
               "`top` must be TRUE, FALSE, a border style name")
})

test_that("FT-TB6034 [FR-TB482]: print() of an rtf_border lists top, bottom, left and right (none when unset) with style, width and colour, and inside_h and inside_v only when set", {
  p <- function(b) capture.output(print(b))
  expect_identical(p(rtf_border()), c("<rtf_border>", "  top     : none", "  bottom  : none",
                                      "  left    : none", "  right   : none"))
  expect_identical(p(rtf_border(top = TRUE, bottom = rtf_border_line("double", 30L, "#003366"))),
                   c("<rtf_border>", "  top     : single, 15 twips",
                     "  bottom  : double, 30 twips, color=#003366",
                     "  left    : none", "  right   : none"))
  expect_identical(p(rtf_border(left = "dot", inside_h = "thick", inside_v = FALSE)),
                   c("<rtf_border>", "  top     : none", "  bottom  : none",
                     "  left    : dot, 15 twips", "  right   : none",
                     "  inside_h: thick, 15 twips", "  inside_v: none, 15 twips"))
  # only the one set
  expect_identical(p(rtf_border(inside_v = "dash"))[6L], "  inside_v: dash, 15 twips")
  expect_length(p(rtf_border(inside_v = "dash")), 6L)
  # print returns its argument invisibly
  b <- rtf_border(top = TRUE)
  out <- capture.output(res <- withVisible(print(b)))
  expect_false(res$visible)
  expect_identical(res$value, b)
})

test_that("FT-TB6035 [FR-TB489]: rtf_border_line() is a list of class rtf_border_line holding style, width and color, and the three can differ from side to side in one rtf_border()", {
  l <- rtf_border_line("double", 30L, "#003366")
  expect_s3_class(l, "rtf_border_line")
  expect_type(l, "list")
  expect_identical(names(l), c("style", "width", "color"))
  expect_identical(l$style, "double")
  expect_identical(l$width, 30L)
  expect_identical(l$color, "#003366")
  d <- rtf_border_line()
  expect_identical(unclass(d), list(style = "single", width = 15L, color = NULL))
  b <- rtf_border(top = rtf_border_line("double", 30L, "#003366"),
                  bottom = rtf_border_line("dot", 10L),
                  left = rtf_border_line("thick", 45L, "#C9372C"),
                  right = "dash")
  expect_identical(b$top$style, "double")
  expect_identical(b$bottom$style, "dot")
  expect_identical(b$left$style, "thick")
  expect_identical(c(b$top$width, b$bottom$width, b$left$width, b$right$width), c(30L, 10L, 45L, 15L))
  expect_identical(c(b$top$color, b$left$color), c("#003366", "#C9372C"))
  expect_null(b$bottom$color)
  # all kept in the RTF: three rules with their own weights
  r <- .t10_rtf(style_zone(rtftable(.t10_ab()[1L, ], border = "none"),
                           first_row = rtf_border(top = b$top, bottom = b$bottom)))
  row <- grep("trowd", r, value = TRUE)[2L]
  expect_match(row, "\\clbrdrt\\brdrdb\\brdrw30\\brdrcf3", fixed = TRUE)
  expect_match(row, "\\clbrdrb\\brdrdot\\brdrw10", fixed = TRUE)
})

test_that("FT-TB6036 [FR-TB490]: rtf_border_line(style = ) is single by default, one of six, and none is an explicit no-line that overrides an inherited border where NULL does not", {
  expect_identical(rtf_border_line()$style, "single")
  for (st in c("single", "double", "thick", "dash", "dot", "none")) {
    expect_identical(rtf_border_line(st)$style, st)
  }
  expect_error(rtf_border_line("wavy"), "should be one of")
  # none merged on top of an inherited rule erases it; NULL leaves it
  d <- .t10_ab()
  base <- style_zone(rtftable(d, border = "none"), last_row = rtf_border(bottom = rtf_border_line("double")))
  none <- style_zone(base, last_row = rtf_border(bottom = rtf_border_line("none")))
  unset <- style_zone(base, last_row = rtf_border(bottom = NULL))
  expect_identical(none$border$last_row$bottom$style, "none")
  expect_identical(unset$border$last_row$bottom$style, "double")
  expect_true(any(grepl("\\clbrdrb\\brdrdb", .render_rtftable(unset, 9360L), fixed = TRUE)))
  expect_false(any(grepl("clbrdrb", .render_rtftable(none, 9360L), fixed = TRUE)))
})

test_that("FT-TB6037 [FR-TB492]: rtf_border_line(width = ) is the line weight in twips, 15 by default, converted to integer, and ignored when style is none", {
  expect_identical(rtf_border_line()$width, 15L)
  expect_identical(rtf_border_line(width = 30)$width, 30L)
  expect_type(rtf_border_line(width = 30)$width, "integer")
  # converted to integer (truncated)
  expect_identical(rtf_border_line(width = 12.7)$width, 12L)
  expect_identical(rtf_border_line("dot", "20")$width, 20L)
  # in the RTF as \brdrw<width>
  r <- .render_rtftable(style_zone(rtftable(.t10_ab(), border = "none"),
                                   last_row = rtf_border(bottom = rtf_border_line("single", 45L))), 9360L)
  expect_match(r[[3L]], "\\clbrdrb\\brdrs\\brdrw45", fixed = TRUE)
  # ignored for none: any width is accepted, even one that would be refused otherwise
  expect_identical(rtf_border_line("none", width = 0L)$style, "none")
  expect_no_error(rtf_border_line("none", width = 0L))
  expect_error(rtf_border_line("single", width = 0L), "`width` must be a positive integer \\(twips\\)")
  r0 <- .render_rtftable(style_zone(rtftable(.t10_ab(), border = "none"),
                                    last_row = rtf_border(bottom = rtf_border_line("none", 99L))), 9360L)
  expect_false(any(grepl("clbrdr", r0, fixed = TRUE)))
})

test_that("FT-TB6038 [FR-TB494]: rtf_border_line(color = ) is black by default, otherwise a 6-digit hex string", {
  expect_null(rtf_border_line()$color)
  expect_identical(rtf_border_line(color = "#003366")$color, "#003366")
  expect_identical(rtf_border_line(color = "#abcdef")$color, "#abcdef")
  for (bad in c("003366", "#00336", "#0033666", "red", "#GG0000", "")) {
    expect_error(rtf_border_line(color = bad),
                 "`color` must be NULL or a 6-digit hex color string (e.g. \"#FF0000\").", fixed = TRUE)
  }
  expect_error(rtf_border_line(color = c("#000000", "#FFFFFF")), "6-digit hex color string")
  # in the RTF: no colour for the default (black), a colour-table entry otherwise
  t0 <- rtftable(.t10_ab(), border = "none")
  plain <- .t10_rtf(style_zone(t0, last_row = rtf_border(bottom = rtf_border_line())))
  expect_false(any(grepl("brdrcf", plain, fixed = TRUE)))
  blue <- .t10_rtf(style_zone(t0, last_row = rtf_border(bottom = rtf_border_line(color = "#003366"))))
  i <- .t10_cidx(blue, "#003366")
  expect_false(is.na(i))
  expect_true(any(grepl(sprintf("\\clbrdrb\\brdrs\\brdrw15\\brdrcf%d", i), blue, fixed = TRUE)))
})

# ------------------------------------------------------- col_cell, col_key, widths

.t10_g <- function() {
  data.frame(Item = c("a", "b"), g1 = c("1", "2"), g2 = c("3", "4"), g3 = c("5", "6"),
             stringsAsFactors = FALSE)
}
.t10_leaf <- c("Item", "g1", "g2", "g3")

test_that("FT-TB6039 [FR-TB520]: col_cell(pos, label, ...) is a list of class rtf_col_cell holding pos and label, the rest only when given, positions those of the data columns", {
  c0 <- col_cell(2, "x")
  expect_s3_class(c0, "rtf_col_cell")
  expect_type(c0, "list")
  expect_identical(names(c0), c("pos", "label"))
  expect_identical(c0$pos, 2L)
  expect_identical(c0$label, "x")
  # given: only those that are
  b <- rtf_border(bottom = TRUE)
  expect_identical(names(col_cell(2, "x", align = "right")), c("pos", "label", "align"))
  expect_identical(names(col_cell(2, "x", bold = TRUE)), c("pos", "label", "bold"))
  expect_identical(names(col_cell(2, "x", italic = TRUE)), c("pos", "label", "italic"))
  expect_identical(names(col_cell(2, "x", underline = TRUE)), c("pos", "label", "underline"))
  expect_identical(names(col_cell(2, "x", border = b)), c("pos", "label", "border"))
  all_set <- col_cell(2, "x", align = "right", bold = TRUE, italic = TRUE, underline = TRUE, border = b)
  expect_identical(names(all_set), c("pos", "label", "align", "bold", "italic", "underline", "border"))
  expect_identical(all_set$align, "right")
  expect_identical(all_set$border, b)
  expect_true(all_set$bold && all_set$italic && all_set$underline)
  # not given as FALSE: absent
  expect_identical(names(col_cell(2, "x", bold = FALSE, italic = FALSE, underline = FALSE)), c("pos", "label"))
  # positions are the data columns', not the header row above: a span g1..g2 over (2, 3),
  # then in the second row column 3 alone is g2 and the cells around it are filled
  hdr <- rtf_col_header(list(col_cell(c(2, 3), "G12"), col_cell(4, "G3")),
                        list(col_cell(3, "c3")), .t10_leaf)
  row2 <- rtftable(.t10_g(), col_header = hdr)$col_header[[2L]]
  expect_identical(vapply(row2, function(z) z$from, integer(1L)), c(1L, 3L, 4L))
  expect_identical(vapply(row2, function(z) z$to, integer(1L)), c(2L, 3L, 4L))
  expect_identical(vapply(row2, function(z) z$label, ""), c("", "c3", ""))
})

test_that("FT-TB6040 [FR-TB521]: col_cell(pos = ) is one column or c(start, end), an inclusive range, start <= end, values at least 1", {
  expect_identical(col_cell(3, "x")$pos, 3L)
  expect_identical(col_cell(c(2, 5), "x")$pos, c(2L, 5L))
  expect_identical(col_cell(c(3, 3), "x")$pos, c(3L, 3L))
  # inclusive: c(2, 3) covers both g1 and g2 -- the cell reaches the end of column 3
  d <- .t10_g()
  one <- rtftable(d, col_header = rtf_col_header(list(col_cell(1, ""), col_cell(c(2, 3), "Span"),
                                                      col_cell(4, "Last")), .t10_leaf))
  cell <- Filter(function(z) identical(z$label, "Span"), one$col_header[[1L]])[[1L]]
  expect_identical(c(cell$from, cell$to), c(2L, 3L))
  # the rendered spanner ends where column 3 ends: three cells in the row, the second
  # (the span) as wide as columns 2 and 3 together
  w <- .t10_widths(one)
  expect_length(w[[1L]], 3L)
  expect_length(w[[2L]], 4L)
  expect_identical(w[[1L]][[2L]], w[[2L]][[2L]] + w[[2L]][[3L]])
  # a single column: one cell as wide as the column
  expect_identical(w[[1L]][[3L]], w[[2L]][[4L]])
  expect_error(col_cell(c(5, 2), "x"), "`pos` start must be <= end.", fixed = TRUE)
  expect_error(col_cell(0, "x"), "`pos` values must be >= 1.", fixed = TRUE)
  expect_error(col_cell(c(0, 3), "x"), "`pos` values must be >= 1.", fixed = TRUE)
  expect_error(col_cell(-1, "x"), "`pos` values must be >= 1.", fixed = TRUE)
})

test_that("FT-TB6041 [FR-TB525]: col_cell(pos = <function>) is a selector of the data column names returning a logical, integer or character vector, the matched columns adjacent, resolved when the header is attached", {
  d <- .t10_g()
  seen <- 0L
  mk <- function(f) rtf_col_header(list(col_cell(1, ""), col_cell(f, "S")), .t10_leaf)
  span_of <- function(f) {
    cells <- rtftable(d, col_header = mk(f))$col_header[[1L]]
    z <- Filter(function(z) identical(z$label, "S"), cells)[[1L]]
    c(z$from, z$to)
  }
  # a logical vector over the data columns, an integer vector of positions, a character
  # vector of names: all the span g1..g2 = columns 2 to 3
  expect_identical(span_of(function(nm) nm %in% c("g1", "g2")), c(2L, 3L))
  expect_identical(span_of(function(nm) c(2L, 3L)), c(2L, 3L))
  expect_identical(span_of(function(nm) c("g1", "g2")), c(2L, 3L))
  expect_identical(span_of(function(nm) which(nm == "g3")), c(4L, 4L))
  # not called when the cell or the header is built, called with the data column names
  # when the header is attached
  sel <- function(nm) {
    seen <<- seen + 1L
    attr_names <<- nm
    c(2L, 3L)
  }
  attr_names <- NULL
  cell <- col_cell(sel, "S")
  hdr <- mk(sel)
  expect_identical(seen, 0L)
  expect_true(is.function(cell$pos))
  tbl <- rtftable(d, col_header = hdr)
  expect_gte(seen, 1L)
  expect_identical(attr_names, names(d))
  # the matched columns must be adjacent
  expect_error(rtftable(d, col_header = mk(function(nm) nm %in% c("g1", "g3"))),
               "matched non-adjacent data columns (2, 4); a header cell can only span a contiguous range.",
               fixed = TRUE)
  expect_error(rtftable(d, col_header = mk(function(nm) list(1))),
               "must return a logical, integer or character vector")
  # a col_key() is such a selector
  expect_identical(span_of(col_key("g1")), c(2L, 2L))
})

test_that("FT-TB6042 [FR-TB530]: col_cell(border = ) inherits the zone border when NULL; an rtf_border() applies to this header cell only and overrides the zone border and the automatic group underline", {
  d <- .t10_g()
  hdr <- function(...) rtf_col_header(list(col_cell(1, ""), col_cell(c(2, 3), "Span", ...), col_cell(4, "Y")),
                                      .t10_leaf)
  defs <- function(x, row) strsplit(.render_rtftable(x, 9360L)[[row]], "\\\\cellx[0-9]+")[[1L]]
  # NULL: the zone's border (the header zone's top rule) and the automatic underline under the span
  base <- rtftable(d, col_header = hdr())
  r1 <- defs(base, 1L)
  expect_match(r1[[2L]], "^\\\\clbrdrt\\\\brdrs\\\\brdrw15\\\\clbrdrb\\\\brdrs\\\\brdrw15\\\\clvertalb")
  expect_match(r1[[1L]], "\\clbrdrt\\brdrs\\brdrw15\\clvertalb", fixed = TRUE)
  # an rtf_border() overrides the zone border: a double top on this cell only
  top <- rtftable(d, col_header = hdr(border = rtf_border(top = "double")))
  r2 <- defs(top, 1L)
  expect_match(r2[[2L]], "^\\\\clbrdrt\\\\brdrdb\\\\brdrw15")
  expect_match(r2[[1L]], "\\clbrdrt\\brdrs\\brdrw15\\clvertalb", fixed = TRUE)
  expect_match(r2[[3L]], "^\\\\clbrdrt\\\\brdrs\\\\brdrw15\\\\clvertalb")
  expect_identical(lengths(regmatches(.render_rtftable(top, 9360L)[[1L]],
                                      gregexpr("brdrdb", .render_rtftable(top, 9360L)[[1L]]))), 1L)
  # and the underline: bottom = "none" removes it under this spanning cell only
  gone <- rtftable(d, col_header = hdr(border = rtf_border(bottom = "none")))
  expect_identical(.t10_count(base, "\\clbrdrb\\brdrs\\brdrw15")[1L], 1L)
  expect_identical(.t10_count(gone, "\\clbrdrb\\brdrs\\brdrw15")[1L], 0L)
  # the leaf row keeps its rules in both
  expect_identical(.t10_count(gone, "\\clbrdrb\\brdrs\\brdrw15")[2L], 4L)
  # the zone border changed elsewhere does not touch the cell that has its own
  z <- style_zone(top, header = rtf_border(top = "dot"))
  r3 <- defs(z, 1L)
  expect_match(r3[[2L]], "^\\\\clbrdrt\\\\brdrdb")
  expect_match(r3[[1L]], "\\clbrdrt\\brdrdot", fixed = TRUE)
})

test_that("FT-TB6043 [FR-TB540 FR-TB541]: a col_key() that matches nothing or non-adjacent columns stops when the header is attached, naming the selector; a selector without the names, or returning the wrong thing, stops", {
  d <- data.frame(Item = c("Age", "Sex"), P__1 = c("1", "2"), D__1 = c("3", "4"), P__2 = c("5", "6"),
                  D__2 = c("7", "8"), P__3 = c("1", "1"), stringsAsFactors = FALSE)
  hdr <- function(sel) rtf_col_header(list(col_cell(sel, "G")), names(d))
  # no match: the selector's label and the keys it could have used
  expect_error(rtftable(d, col_header = hdr(col_key("Q", sep = "__"))),
               "col_key(\"Q\") matched no data columns. Available keys: \"Item\", \"P\", \"D\".",
               fixed = TRUE)
  expect_error(rtftable(d, col_header = hdr(col_key("Plcebo"))),
               "col_key(\"Plcebo\") matched no data columns. Available keys: \"Item\", \"P__1\", \"D__1\", \"P__2\", \"D__2\", \"P__3\".",
               fixed = TRUE)
  # non-adjacent: the positions found
  expect_error(rtftable(d, col_header = hdr(col_key("P", sep = "__"))),
               "col_key(\"P\") matched non-adjacent data columns (2, 4, 6); a header cell can only span a contiguous range.",
               fixed = TRUE)
  # a selector attached without the data column names
  expect_error(.pos_row_to_spans(list(col_cell(col_key("P"), "G")), 6L, NULL),
               "A selector `pos` in col_cell() requires the header to be attached to a table (data column names are unknown here).",
               fixed = TRUE)
  # a selector returning the wrong thing
  bad <- function(f) rtftable(d, col_header = hdr(f))
  expect_error(bad(function(nm) c(TRUE, FALSE)), "The `pos` selector returned 2 logical values for 6 data columns.",
               fixed = TRUE)
  expect_error(bad(function(nm) 9L), "The `pos` selector matched positions outside the data column range 1..6.",
               fixed = TRUE)
  expect_error(bad(function(nm) "zz"), "The `pos` selector returned unknown column name(s): \"zz\".",
               fixed = TRUE)
  expect_error(bad(function(nm) list(1)), "The `pos` selector must return a logical, integer or character vector.",
               fixed = TRUE)
  expect_error(bad(function(nm) integer()), "matched no data columns")
})

test_that("FT-TB6044 [FR-TB542]: text_width_in(font = Arial / Times New Roman) measures each character with the AFM metrics at the size given: i narrower than W, a character outside printable ASCII is an n, no AFM file the average width", {
  ar <- function(x, size = 24L) text_width_in(x, font = "Arial", font_size_half_points = size)
  tr <- function(x, size = 24L) text_width_in(x, font = "Times New Roman", font_size_half_points = size)
  # Helvetica (Arial): i 222, W 944, n 556; Times-Roman: i 278, W 944, n 500 (1/1000 em);
  # at 12 pt an em is 12/72 inch
  em <- 12 / 72
  expect_equal(ar("i"), 0.222 * em)
  expect_equal(ar("W"), 0.944 * em)
  expect_lt(ar("i"), ar("W"))
  expect_equal(tr("i"), 0.278 * em)
  expect_equal(tr("W"), 0.944 * em)
  expect_lt(tr("i"), tr("W"))
  expect_equal(tr("n"), 0.500 * em)
  expect_equal(ar("n"), 0.556 * em)
  # at the size given: 18 half-points = 9 pt is three quarters of 12 pt
  expect_equal(ar("n", 18L), 0.75 * ar("n", 24L))
  # a character outside printable ASCII takes the width of an n
  expect_equal(ar("é"), ar("n"))
  expect_equal(tr("é"), tr("n"))
  expect_equal(ar("éi"), ar("n") + ar("i"))
  # Helvetica is Arial's, and the names do not depend on case, spaces or hyphens
  expect_identical(text_width_in("Total", font = "Helvetica", font_size_half_points = 24L), ar("Total"))
  expect_identical(text_width_in("Total", font = "times-new-roman", font_size_half_points = 24L), tr("Total"))
  # with no AFM file (an empty cache entry): the average width of the font, as before; Arial's
  # is 6 pt of 12, so a character is 1/12 inch whatever it is
  on.exit(suppressWarnings(rm(list = c("arial", "times"), envir = .afm_cache)), add = TRUE)
  assign("arial", NULL, envir = .afm_cache)
  expect_equal(ar("iiii"), ar("WWWW"))
  expect_equal(ar("iiii"), 4 * 6 / 72)
})

test_that("FT-TB6045 [FR-TB568]: rtftable() sizes the columns by col_rel_width, column_widths_twips (which wins), table_width_twips, table_width_pct and table_width_pct_of_writable, the twips over the percentages and table_width_pct over the other", {
  d <- data.frame(Item = "x", A = "y", B = "z", stringsAsFactors = FALSE)
  w <- function(...) .t10_widths(rtftable(d, ...))[[2L]]
  # relative widths apportion the table's total: 8000 twips as 2 : 1 : 1
  expect_identical(w(col_rel_width = c(2, 1, 1), table_width_twips = 8000), c(4000L, 2000L, 2000L))
  # the same proportions at the width given to the renderer (9360 twips)
  expect_identical(sum(w(col_rel_width = c(2, 1, 1))), 9360L)
  expect_identical(w(col_rel_width = c(2, 1, 1)), c(4680L, 2340L, 2340L))
  # column_widths_twips gives each column's width and wins over col_rel_width
  expect_identical(w(col_rel_width = c(2, 1, 1), column_widths_twips = c(3000, 2000, 1000)),
                   c(3000L, 2000L, 1000L))
  expect_identical(w(column_widths_twips = c(3000, 2000, 1000), table_width_twips = 9000),
                   c(3000L, 2000L, 1000L))
  # table_width_twips is the table's total width
  expect_identical(sum(w(table_width_twips = 6000)), 6000L)
  # table_width_pct is table_width_pct_of_writable * 100: the same table
  expect_identical(w(table_width_pct = 50), w(table_width_pct_of_writable = 0.5))
  expect_identical(sum(w(table_width_pct = 50)), 4680L)
  # given together: table_width_twips wins over both percentages ...
  expect_identical(sum(w(table_width_twips = 6000, table_width_pct = 50, table_width_pct_of_writable = 0.9)),
                   6000L)
  expect_identical(sum(w(table_width_twips = 6000, table_width_pct_of_writable = 0.9)), 6000L)
  # ... and table_width_pct over table_width_pct_of_writable
  expect_identical(sum(w(table_width_pct = 50, table_width_pct_of_writable = 0.9)), 4680L)
  # a wrong length is refused (when the table is built into a document)
  expect_error(.t10_rtf(rtftable(d, col_rel_width = c(1, 1))),
               "`col_rel_width` length \\(2\\) must match ncol \\(3\\)")
  expect_error(.t10_rtf(rtftable(d, column_widths_twips = c(1000, 1000))),
               "`column_widths_twips` length \\(2\\) must match ncol \\(3\\)")
})

test_that("FT-TB6046 [FR-TB570]: rtftable(cell_padding_left_twips = , cell_padding_right_twips = ) pad every cell as \\li and \\ri; NULL takes the style's, else the document's rtf_style(), else the option, else 0", {
  on.exit(rtfreporter_reset_defaults(), add = TRUE)
  d <- data.frame(Item = "x", A = "y", stringsAsFactors = FALSE)
  rows_of <- function(doc) {
    f <- tempfile(fileext = ".rtf")
    on.exit(unlink(f), add = TRUE)
    generate_rtfreport(doc, f, overwrite = TRUE)
    grep("trowd", readLines(f, warn = FALSE), value = TRUE)
  }
  pads <- function(rows, l, r) {
    all(vapply(rows, function(x) {
      lengths(regmatches(x, gregexpr(sprintf("\\li%d\\ri%d", l, r), x, fixed = TRUE))) == 2L
    }, NA))
  }
  doc <- function(tbl, style = NULL) {
    d0 <- rtf_tables(rtf_document(), list(tbl))
    if (is.null(style)) d0 else rtf_config(d0, style = style)
  }
  # the argument: every cell, header and data
  expect_true(pads(rows_of(doc(rtftable(d, cell_padding_left_twips = 70L, cell_padding_right_twips = 30L))), 70L, 30L))
  expect_true(pads(rows_of(doc(rtftable(d, cell_padding_left_twips = 70L))), 70L, 0L))
  expect_true(pads(rows_of(doc(rtftable(d, cell_padding_right_twips = 30L))), 0L, 30L))
  # nothing at all: 0
  expect_true(pads(rows_of(doc(rtftable(d))), 0L, 0L))
  # the option, when nothing else says
  options(rtfreporter.cell_padding_left_twips = 120L, rtfreporter.cell_padding_right_twips = 110L)
  expect_true(pads(rows_of(doc(rtftable(d))), 120L, 110L))
  # the document's rtf_style() before the option
  expect_true(pads(rows_of(doc(rtftable(d), rtf_style(cell_padding_left_twips = 90L,
                                                      cell_padding_right_twips = 80L))), 90L, 80L))
  # the style's before the document's
  st <- rtf_table_style(cell_padding_left_twips = 60L, cell_padding_right_twips = 50L)
  expect_true(pads(rows_of(doc(rtftable(d, style = st), rtf_style(cell_padding_left_twips = 90L,
                                                                  cell_padding_right_twips = 80L))), 60L, 50L))
  # the argument before the style
  expect_true(pads(rows_of(doc(rtftable(d, style = st, cell_padding_left_twips = 10L,
                                        cell_padding_right_twips = 5L))), 10L, 5L))
  # 0 is a value, not "unset"
  expect_true(pads(rows_of(doc(rtftable(d, cell_padding_left_twips = 0L, cell_padding_right_twips = 0L))), 0L, 0L))
})

test_that("FT-TB6047 [FR-TB573]: rtftable() returns an rtftable that rtf_tables() places as one page, and the help's examples render", {
  df <- data.frame(Subject = c("001", "002"), Age = c(34L, 45L))
  tbl <- rtftable(df)
  expect_s3_class(tbl, "rtftable")
  dm <- data.frame(Parameter = c("Age (years)", "  Mean (SD)", "  Median"),
                   Placebo = c("", "75.1 (8.2)", "76.0"), Active = c("", "74.3 (7.9)", "75.0"),
                   stringsAsFactors = FALSE)
  tbl2 <- rtftable(dm,
                   col_header = list(list(list(from = 2, to = 3, label = "Treatment", underline = TRUE)),
                                     c("Parameter", "Placebo", "Active")),
                   col_spec = list(list(col = 1, align = "left")), col_rel_width = c(2, 1, 1),
                   border = "tfl")
  expect_s3_class(tbl2, "rtftable")
  # one page: one table of two tables in a list is two pages, one table is one
  page_breaks <- function(x) sum(grepl("\\page", x, fixed = TRUE))
  one <- .t10_rtf(tbl2)
  two <- .t10_rtf(list(tbl2, tbl2))
  expect_identical(sum(grepl("Age (years)", one, fixed = TRUE)), 1L)
  expect_identical(sum(grepl("Age (years)", two, fixed = TRUE)), 2L)
  expect_gt(page_breaks(two), page_breaks(one))
  # the example: the "Treatment" spanner over the two arms, the label column twice as wide
  rows <- grep("trowd", one, value = TRUE)
  span <- grep("Treatment", rows, value = TRUE)
  expect_length(span, 1L)
  w <- .t10_widths(tbl2)
  expect_length(w[[1L]], 2L)                   # the spanner row: the label cell and the span
  expect_identical(w[[1L]][[2L]], w[[2L]][[2L]] + w[[2L]][[3L]])
  expect_equal(w[[2L]][[1L]] / w[[2L]][[2L]], 2, tolerance = 0.01)
  expect_match(span, "\\ul Treatment\\ulnone", fixed = TRUE)
  # the first example: the column names are the header
  expect_match(grep("Subject", .t10_rtf(tbl), value = TRUE)[1L],
               "Subject\\cell", fixed = TRUE)
})

test_that("FT-TB6048 [FR-TB516]: cell_rows(<name> = ) holds a template, a chain of templates tried in order, a chain with guards, and a single bare guard is a row too", {
  d <- data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 3L),
                  variable = "AGE", variable_level = NA, context = "continuous",
                  stat_name = rep(c("n", "p", "estimate"), 2L),
                  stat_label = rep(c("n", "p", "Est"), 2L),
                  stat = c(0, 0.5, 1.25, 7, 0.25, 2.5), stringsAsFactors = FALSE)
  d$fmt_fun <- list(0L, 1L, 1L, 0L, 1L, 1L)
  n <- normalize_ard(d)
  wide <- function(cells) widen_ard(n, cols = "TRT", rows = c(group = "variable"), cells = cells, notes = FALSE)
  # a template: A has n = 0, B n = 7
  one <- wide(cell_rows(est = "{n:.0f}"))
  expect_identical(one$label, "est")
  expect_identical(c(one$A, one$B), c("0", "7"))
  # a chain: the first template that resolves (zz is no statistic, so the second)
  expect_identical(unlist(wide(cell_rows(est = c("{n:.0f} ({zz:.1f})", "{n:.0f}")))[c("A", "B")], use.names = FALSE),
                   c("0", "7"))
  expect_identical(unlist(wide(cell_rows(est = c("{n:.0f} ({p:.1f})", "{n:.0f}")))[c("A", "B")], use.names = FALSE),
                   c("0 (0.5)", "7 (0.2)"))
  # a chain with guards: the zero count is "0", the rest the estimate line
  g <- wide(cell_rows(est = c(n == 0 ~ "0", "{n:.0f} ({estimate:.1f})")))
  expect_identical(c(g$A, g$B), c("0", "7 (2.5)"))
  # a guard in the middle of a chain is tried in order too
  g2 <- wide(cell_rows(est = c(n > 5 ~ "big", "{n:.0f}")))
  expect_identical(c(g2$A, g2$B), c("0", "big"))
  # a single bare guard is a row: it holds where n > 0 (B), A has no cell
  bare <- wide(cell_rows(est = n > 0 ~ "has"))
  expect_identical(bare$label, "est")
  expect_true(is.na(bare$A))
  expect_identical(bare$B, "has")
  expect_identical(bare, wide(cell_rows(est = c(n > 0 ~ "has"))))
  # several rows, a row a name
  two <- wide(cell_rows("1" = "{n:.0f}", "2" = c(n == 0 ~ "-", "{p:.1f}")))
  expect_identical(two$label, c("1", "2"))
  expect_identical(two$A, c("0", "-"))
  expect_identical(two$B, c("7", "0.2"))
  # print shows a guard as written
  expect_match(capture.output(print(cell_rows(est = n == 0 ~ "0")))[2L], "n == 0 ~ \"0\"", fixed = TRUE)
})

# ------------------------------------------------------- stub_spec(vars = one column)

test_that("FT-TB6049 [FR-TB591]: stub_spec(vars = ) may name one column, kept as the stub as it is and renamed to label when one is given; the flat stub_* arguments of as_rtftables() are an error", {
  h <- data.frame(SOC = c("Gastro", "Gastro", "Nerv"), PT = c("Nausea", "Vomit", "Headache"),
                  N = c("1", "2", "3"), stringsAsFactors = FALSE)
  # one column, no label: the table as it is, the column keeps its name and its values
  p0 <- as_rtftables(h, stub = stub_spec(vars = "SOC"))[[1L]]
  expect_identical(names(p0$data), c("SOC", "PT", "N"))
  expect_identical(p0$data$SOC, h$SOC)
  expect_identical(nrow(p0$data), 3L)
  # a label renames it, the values stay
  p1 <- as_rtftables(h, stub = stub_spec(vars = "SOC", label = "Term"))[[1L]]
  expect_identical(names(p1$data), c("Term", "PT", "N"))
  expect_identical(p1$data$Term, h$SOC)
  expect_identical(p1$data$PT, h$PT)
  # the column named by position is the same; the stub is the first column of the page
  p2 <- as_rtftables(h, stub = stub_spec(vars = 2, label = "Event"))[[1L]]
  expect_identical(names(p2$data), c("Event", "SOC", "N"))
  expect_identical(p2$data$Event, h$PT)
  # the stub is what the table prints first, with the label
  expect_identical(names(p1$data)[[1L]], "Term")
  # the flat arguments were removed in 0.9.0 and are an error
  expect_error(as_rtftables(h, stub_vars = "SOC"), "unused argument (stub_vars = \"SOC\")", fixed = TRUE)
  expect_error(as_rtftables(h, stub_label = "Term"), "unused argument (stub_label = \"Term\")", fixed = TRUE)
  expect_error(as_rtftables(h, stub_indent = 2), "unused argument (stub_indent = 2)", fixed = TRUE)
  expect_error(as_rtftables(h, stub_group_summary = "empty"),
               "unused argument (stub_group_summary = \"empty\")", fixed = TRUE)
})

# ------------------------------------------------------- huxtable header rows

test_that("FT-TB6050 [FR-TB631]: in a huxtable only the header_rows() from the first row on are the column header, the last of them the leaf labels; a row flagged further down is a body row in its place; none at the top gives a blank header", {
  skip_if_not_installed("huxtable")
  d <- data.frame(Param = c("Age", "Weight", "Height"), Mean = c("1", "2", "3"),
                  SD = c("4", "5", "6"), stringsAsFactors = FALSE)
  hx <- huxtable::as_hux(d, add_colnames = TRUE)
  base <- as_rtftables(hx)[[1L]]
  expect_identical(unlist(base$col_header), c("Param", "Mean", "SD"))
  expect_identical(base$data$V1, c("Age", "Weight", "Height"))
  # a row flagged further down (the "Weight" row, row 3 of the huxtable) stays in the body
  h <- hx
  huxtable::header_rows(h)[3] <- TRUE
  p <- as_rtftables(h)[[1L]]
  expect_identical(unlist(p$col_header), c("Param", "Mean", "SD"))
  expect_identical(p$data$V1, c("Age", "Weight", "Height"))
  expect_identical(p$data$V2, c("1", "2", "3"))
  # no row flagged at the top: the header is blank, the label row is a body row
  h0 <- hx
  huxtable::header_rows(h0) <- FALSE
  p0 <- as_rtftables(h0)[[1L]]
  expect_identical(unlist(p0$col_header), rep("", 3L))
  expect_identical(p0$data$V1, c("Param", "Age", "Weight", "Height"))
  # a flag on the second row only is not "from the first row on": blank header again
  h1 <- hx
  huxtable::header_rows(h1) <- FALSE
  huxtable::header_rows(h1)[2] <- TRUE
  p1 <- as_rtftables(h1)[[1L]]
  expect_identical(unlist(p1$col_header), rep("", 3L))
  expect_identical(p1$data$V1, c("Param", "Age", "Weight", "Height"))
  # two rows flagged from the first on: the spanner above the leaf labels
  sp <- huxtable::insert_row(hx, "", "Stats", "", after = 0L)
  huxtable::colspan(sp)[1L, 2L] <- 2L
  huxtable::header_rows(sp)[1:2] <- TRUE
  p2 <- as_rtftables(sp)[[1L]]
  expect_length(p2$col_header, 2L)
  expect_identical(p2$col_header[[1L]], list(list(from = 1L, to = 1L, label = ""),
                                             list(from = 2L, to = 3L, label = "Stats")))
  expect_identical(p2$col_header[[2L]], c("Param", "Mean", "SD"))
  expect_identical(p2$data$V1, c("Age", "Weight", "Height"))
})
