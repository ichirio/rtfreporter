# Tests added to close the gaps found by the Traceability Matrix check
# (2026-11-12): each asserts what its requirement states.
# Group g5: set_decimal_split(), auto_col_widths(), default_col_widths(),
# text_width_in(), the style verbs, rtf_table_style(), rtf_border(),
# col_cell() / col_key() and the adapters.  Fictional data only.

# ---------------------------------------------------------------- helpers

# Every \cellx position of one rendered row, as integers.
.g5_cellx <- function(row) {
  as.integer(sub("\\cellx", "",
                 regmatches(row, gregexpr("\\cellx[0-9]+", row))[[1L]]))
}

# A scratch directory removed when the calling test ends.
.g5_dir <- function(env = parent.frame()) {
  d <- tempfile("g5-")
  dir.create(d)
  do.call(on.exit, list(substitute(unlink(d, recursive = TRUE)), add = TRUE),
          envir = env)
  d
}

# The RTF text of one table or a list of pages as a document.
.g5_rtf <- function(x, env = parent.frame()) {
  d <- .g5_dir(env)
  f <- file.path(d, "t.rtf")
  pages <- if (inherits(x, "rtftable")) list(x) else x
  generate_rtfreport(rtf_tables(rtf_document(), pages), f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

# ------------------------------------------------------- set_decimal_split

test_that("FT-TB3200 [FR-TB348]: a misspelt argument warns once with the valid list", {
  tbl <- rtftable(data.frame(a = "x", v = "1.5", stringsAsFactors = FALSE))
  ws <- character()
  withCallingHandlers(
    set_decimal_split(tbl, cols = 2L, colS = 3L),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_length(ws, 1L)
  expect_match(ws, "`set_decimal_split()`: unknown argument `colS` (did you mean `cols`?) ignored.",
               fixed = TRUE)
  expect_match(ws, paste0("Valid arguments: cols, ratio, decimal_mark, pad_chars, ",
                          "min_chars, max_chars, include_compound."), fixed = TRUE)
})

test_that("FT-TB3201 [FR-TB349]: one numeric ratio gives the same split point on every page", {
  p1 <- data.frame(a = c("x", "y"), v = c("1.5", "2.25"), stringsAsFactors = FALSE)
  p2 <- data.frame(a = c("z", "w"), v = c("1000.125", "20.5"), stringsAsFactors = FALSE)
  pages <- list(rtftable(p1, col_rel_width = c(1, 1)),
                rtftable(p2, col_rel_width = c(1, 1)))
  split_x <- function(tbls) {
    vapply(tbls, function(t) .g5_cellx(.render_rtftable(t, 9360L)[2L])[2L], integer(1L))
  }
  # no ratio: each page sizes the halves from its own widest parts
  auto <- split_x(set_decimal_split(pages, cols = 2L))
  expect_false(auto[1L] == auto[2L])
  # a ratio of 0.3: 4680 + round(4680 * 0.3) = 6084 on both pages
  expect_identical(split_x(set_decimal_split(pages, cols = 2L, ratio = 0.3)),
                   c(6084L, 6084L))
})

test_that("FT-TB3202 [FR-TB351]: the first separator in a cell is the split point", {
  p <- .decimal_split_cells(c("1.2.3", "1.5,2"))
  expect_identical(p$left, c("1", "1"))
  expect_identical(p$right, c(".2.3", ".5,2"))
  q <- .decimal_split_cells(c("1,2,3", "1.5,2"), decimal_mark = ",")
  expect_identical(q$left, c("1", "1.5"))
  expect_identical(q$right, c(",2,3", ",2"))
})

test_that("FT-TB3203 [FR-TB357]: NULL pad_chars, min_chars and max_chars are the defaults", {
  # max_chars NULL = 10: 3 + 8 = 11 is over the cap, so the raw 3 : 8
  expect_equal(.decimal_split_ratio(3L, 8L, max_chars = NULL), 3 / 11)
  # ... while 3 + 7 = 10 is not over it: (3 + 1) : (7 + 1)
  expect_equal(.decimal_split_ratio(3L, 7L, max_chars = NULL), 4 / 12)
  # pad_chars NULL = c(1, 1): (4 + 1) : max(2 + 1, 6)
  expect_equal(.decimal_split_ratio(4L, 2L, pad_chars = NULL), 5 / 11)
  expect_equal(.decimal_split_ratio(4L, 2L, pad_chars = c(0, 0)), 4 / 10)
  # min_chars NULL = c(4, 6): max(1 + 1, 4) : max(1 + 1, 6)
  expect_equal(.decimal_split_ratio(1L, 1L, min_chars = NULL), 4 / 10)
  expect_equal(.decimal_split_ratio(1L, 1L, min_chars = c(0, 0)), 2 / 4)
})

test_that("FT-TB3204 [FR-TB359]: a prefix goes with the left half, a suffix with the right", {
  p <- .decimal_split_cells(c("~5.2", "<=3.1", ">4.7", "5.2%", "100"))
  expect_identical(p$left, c("~5", "<=3", ">4", "5", "100"))
  expect_identical(p$right, c(".2", ".1", ".7", ".2%", ""))
  expect_true(all(p$split))
})

test_that("FT-TB3205 [FR-TB360]: free text is one cell across the pair, as without the option", {
  d <- data.frame(Statistic = c("Mean", "n", "Group A"),
                  Value = c("3.45", "n (%)", "Placebo"), stringsAsFactors = FALSE)
  plain <- .render_rtftable(rtftable(d, col_rel_width = c(1, 1)), 9360L)
  split <- .render_rtftable(set_decimal_split(rtftable(d, col_rel_width = c(1, 1)),
                                              cols = 2L), 9360L)
  # the decimal row has three cells, the free-text rows two as before
  expect_length(.g5_cellx(split[2L]), 3L)
  expect_length(.g5_cellx(split[3L]), 2L)
  expect_length(.g5_cellx(split[4L]), 2L)
  expect_identical(.g5_cellx(split[3L]), .g5_cellx(plain[3L]))
  expect_identical(.g5_cellx(split[4L]), .g5_cellx(plain[4L]))
  expect_identical(split[3L], plain[3L])
  expect_identical(split[4L], plain[4L])
})

test_that("FT-TB3206 [FR-TB362]: a column with no separator on a page is left untouched there", {
  p1 <- data.frame(a = c("x", "y"), v = c("1.5", "2.25"), stringsAsFactors = FALSE)
  p2 <- data.frame(a = c("z", "w"), v = c("10", "20"), stringsAsFactors = FALSE)
  pages <- set_decimal_split(list(rtftable(p1), rtftable(p2)), cols = 2L)
  r1 <- .render_rtftable(pages[[1L]], 9360L)
  r2 <- .render_rtftable(pages[[2L]], 9360L)
  expect_length(.g5_cellx(r1[2L]), 3L)   # page 1: split
  expect_length(.g5_cellx(r2[2L]), 2L)   # page 2: untouched
  expect_identical(r2, .render_rtftable(rtftable(p2), 9360L))
})

# ------------------------------------------------- widths and text widths

# A statistics table, every cell short, with `nd` data columns.
.g5_short <- function(nd, label = c("a", "b"), pad = "") {
  d <- data.frame(Param = paste0(pad, label), stringsAsFactors = FALSE)
  for (i in seq_len(nd)) d[[paste0("V", i)]] <- c("1", "2")
  d
}

test_that("FT-TB3207 [FR-TB370]: Arial is one flat average width and the size scales it", {
  d <- data.frame(a = strrep("i", 20L), b = strrep("W", 20L), stringsAsFactors = FALSE)
  # 20 characters at 9 pt: Courier 7.22 * 9 / 12 = 5.415 pt each, Arial 6.0 * 9 / 12 =
  # 4.5 pt each; in twips 2166 and 1800, plus the 288 of padding
  expect_identical(auto_col_widths(d, font = "Arial"), c(2088L, 2088L))
  expect_identical(auto_col_widths(d, font = "Courier New"), c(2454L, 2454L))
  expect_identical(auto_col_widths(d, font = "Courier"), c(2454L, 2454L))
  expect_identical(auto_col_widths(d, font = "No Such Font"), c(2454L, 2454L))
  expect_identical(auto_col_widths(d), c(2454L, 2454L))
  # 18 pt (36 half-points) doubles the character width
  expect_identical(auto_col_widths(d, font = "Arial", font_size_half_points = 36L),
                   c(3888L, 3888L))
  expect_identical(auto_col_widths(d, font_size_half_points = 36L), c(4620L, 4620L))
})

test_that("FT-TB3208 [FR-TB376]: default_col_widths is a named vector, smallest data column 1", {
  d <- data.frame(Characteristic = c("Age (years)", "  Mean (SD)", "  Median"),
                  Placebo = c("", "75.2 (8.59)", "76.0"),
                  Active = c("", "74.4 (7.89)", "75.0"), stringsAsFactors = FALSE)
  w <- default_col_widths(d)
  expect_type(w, "double")
  expect_named(w, names(d))
  expect_length(w, ncol(d))
  expect_identical(min(w[-1L]), 1)
  # the same proportions when the table gives no widths
  none <- .render_rtftable(rtftable(d), 9360L)[2L]
  given <- .render_rtftable(rtftable(d, col_rel_width = w), 9360L)[2L]
  expect_identical(.g5_cellx(given), .g5_cellx(none))
  expect_identical(.g5_cellx(none), c(4011L, 6685L, 9360L))
})

test_that("FT-TB3209 [FR-TB378]: row_title names the row-heading column by position or name", {
  d <- data.frame(Item = c("x", "y"), Param = c("p", "q"), V = c("1", "2"),
                  stringsAsFactors = FALSE)
  expect_identical(default_col_widths(d), c(Item = 1.5, Param = 1, V = 1))
  expect_identical(default_col_widths(d, row_title = NULL), c(Item = 1.5, Param = 1, V = 1))
  expect_identical(default_col_widths(d, row_title = 2L), c(Item = 1, Param = 1.5, V = 1))
  expect_identical(default_col_widths(d, row_title = "Param"),
                   c(Item = 1, Param = 1.5, V = 1))
})

test_that("FT-TB3210 [FR-TB381]: width_chars decides whether a long cell fits its share", {
  d <- data.frame(Param = c("a", "b"), V1 = c(strrep("x", 28L), "y"), V2 = c("1", "2"),
                  stringsAsFactors = FALSE)
  # 120 characters: the 30 a cell needs fits its 34.3-character share
  expect_identical(default_col_widths(d), c(Param = 1.5, V1 = 1, V2 = 1))
  # 60 characters: the share is 17.1 and V1 is widened (25.7 : 25.7 : 8.6)
  expect_identical(default_col_widths(d, width_chars = 60), c(Param = 3, V1 = 3, V2 = 1))
})

test_that("FT-TB3211 [FR-TB383]: the row-heading width is its longest line + 2, capped by share", {
  share <- function(d) {
    w <- default_col_widths(d, digits = 8L)
    w[[1L]] / sum(w)
  }
  # six data columns: a 30-character heading takes 32 of the 120 characters
  expect_equal(share(.g5_short(6L, c(strrep("x", 30L), "y"))), 32 / 120, tolerance = 1e-6)
  # at most 40% ... 4 data columns: 48 of 120
  expect_equal(share(.g5_short(4L, c(strrep("x", 100L), "y"))), 0.40, tolerance = 1e-6)
  # less with many data columns: 0.40 - 0.02 * (10 - 4), and 20% at the least
  expect_equal(share(.g5_short(10L, c(strrep("x", 100L), "y"))), 0.28, tolerance = 1e-6)
  expect_equal(share(.g5_short(20L, c(strrep("x", 100L), "y"))), 0.20, tolerance = 1e-6)
  # 5 points more when the headings are indented
  expect_equal(share(.g5_short(4L, c(strrep("x", 100L), "y"), pad = "  ")), 0.45,
               tolerance = 1e-6)
  # never less than 1.5 times a data column's share, and 60:40 with one data column
  w3 <- default_col_widths(.g5_short(3L), digits = 8L)
  expect_equal(unname(w3), c(1.5, 1, 1, 1))
  expect_identical(unname(default_col_widths(.g5_short(1L))), c(1.5, 1))
})

test_that("FT-TB3212 [FR-TB387]: every block of paginate_cols keeps each column's default width", {
  d <- data.frame(Param = c("a", "b"), V1 = c("1", "2"), V2 = c(strrep("x", 30L), "y"),
                  V3 = c("1", "2"), V4 = c("1", "2"), V5 = c("1", "2"),
                  stringsAsFactors = FALSE)
  dw <- default_col_widths(d, digits = 8L)
  pages <- suppressWarnings(paginate_cols(rtftable(d), at = c(3, 5)))
  expect_length(pages, 3L)
  for (p in pages) {
    cols <- names(p$data)
    expect_equal(p$col_rel_width / p$col_rel_width[[1L]], dw[cols] / dw[[cols[1L]]],
                 ignore_attr = TRUE, tolerance = 1e-6)
  }
  expect_identical(unlist(lapply(pages, function(p) names(p$data))),
                   c("Param", "V1", "Param", "V2", "V3", "Param", "V4", "V5"))
})

test_that("FT-TB3213 [FR-TB389]: text_width_in coerces to character and counts NA as empty", {
  one <- 5.415 / 72   # one Courier New character at 9 pt, in inches
  expect_equal(text_width_in(c(1, 12.5)), c(1, 4) * one)
  expect_equal(text_width_in(factor("ab")), 2 * one)
  expect_equal(text_width_in(c(NA, "ab")), c(0, 2 * one))
  expect_equal(text_width_in(NA), 0)
})

test_that("FT-TB3214 [FR-TB390]: font names match whatever the case, spaces and hyphens", {
  arial <- text_width_in("abc", font = "Arial")
  times <- text_width_in("abc", font = "Times New Roman")
  courier <- text_width_in("abc", font = "Courier New")
  expect_false(isTRUE(all.equal(arial, times)))
  expect_false(isTRUE(all.equal(arial, courier)))
  for (f in c("Times-Roman", "TIMES", "times_new_roman", "Times")) {
    expect_identical(text_width_in("abc", font = f), times)
  }
  for (f in c("helvetica", "ARIAL", "Helvetica")) {
    expect_identical(text_width_in("abc", font = f), arial)
  }
  for (f in c("Courier", "courier_new", "COURIER-NEW")) {
    expect_identical(text_width_in("abc", font = f), courier)
  }
  # NULL is the package's font
  old <- options(rtfreporter.font = "Arial")
  on.exit(options(old), add = TRUE)
  expect_identical(text_width_in("abc"), arial)
  expect_identical(text_width_in("abc", font = NULL), arial)
  options(rtfreporter.font = "Times New Roman")
  expect_identical(text_width_in("abc"), times)
})

test_that("FT-TB3215 [FR-TB391]: an unknown font falls back to Courier New without a warning", {
  expect_no_warning(w <- text_width_in("abc", font = "Comic Sans"))
  expect_equal(w, 3 * 5.415 / 72)
})

test_that("FT-TB3216 [FR-TB392]: the default size is 18 half-points (9 pt) and is in half-points", {
  expect_identical(text_width_in("abc"), text_width_in("abc", font_size_half_points = 18L))
  expect_equal(text_width_in("abc"), 3 * 5.415 / 72)
  # 24 half-points = 12 pt: 7.22 pt a character
  expect_equal(text_width_in("abc", font_size_half_points = 24L), 3 * 7.22 / 72)
})

test_that("FT-TB3217 [FR-TB542]: with no AFM file Arial is measured at the flat average", {
  local_mocked_bindings(.afm_widths = function(family) NULL)
  # 3 characters of 6.0 * 9 / 12 = 4.5 pt each
  expect_equal(text_width_in("abc", font = "Arial"), 3 * 4.5 / 72)
  expect_equal(text_width_in(c("iii", "WWW"), font = "Arial"), c(3, 3) * 4.5 / 72)
})

# ------------------------------------------------------------ style verbs

.g5_df <- function() {
  data.frame(Item = c("Age", "Sex", "Mean"), A = c("1", "2", "3"), B = c("4", "5", "6"),
             C = c("7", "8", "9"), stringsAsFactors = FALSE)
}

# A three-row header: a spanner over columns 3-4, a second row, the leaf labels.
.g5_hdr_tbl <- function() {
  hdr <- rtf_col_header(
    list(col_cell(1, ""), col_cell(2, ""), col_cell(c(3, 4), "Group")),
    list(col_cell(1, ""), col_cell(2, ""), col_cell(c(3, 4), "(N = 9)")),
    c("Item", "A", "B", "C"))
  rtftable(.g5_df(), col_header = hdr)
}

# Which body rows carry a TRUE flag in any column.
.g5_flag_rows <- function(x, flag) {
  which(vapply(x$cell_styles, function(r) any(r[[flag]] %in% TRUE), logical(1L)))
}

test_that("FT-TB3218 [FR-TB395]: style_body with rows = NULL styles every body row", {
  out <- style_body(rtftable(.g5_df()), bold = TRUE)
  expect_identical(.g5_flag_rows(out, "bold"), 1:3)
  for (r in 1:3) expect_identical(out$cell_styles[[r]]$bold, rep(TRUE, 4L))
})

test_that("FT-TB3219 [FR-TB396]: rows of a multi-data-frame table are counted across all frames", {
  d1 <- data.frame(g = c("a", "b"), b = c(1, 2), stringsAsFactors = FALSE)
  d2 <- data.frame(g = c("c", "d", "e"), b = c(0, 3, NA), stringsAsFactors = FALSE)
  m <- rtftable(list(d1, d2))
  # the formula is evaluated on each frame in turn; b > 1 holds for row 2 of the first
  # frame and row 2 of the second (global row 4); the NA of row 5 selects nothing
  out <- style_body(m, rows = ~ b > 1, bold = TRUE)
  expect_identical(.g5_flag_rows(out, "bold"), c(2L, 4L))
  expect_identical(.g5_flag_rows(style_body(m, rows = function(df) df$b > 1, bold = TRUE),
                                 "bold"), c(2L, 4L))
  expect_identical(.g5_flag_rows(style_body(m, rows = c(3, 5), italic = TRUE), "italic"),
                   c(3L, 5L))
})

test_that("FT-TB3220 [FR-TB399]: cols are resolved against the final printed columns", {
  d <- data.frame(Param = c("x", "x"), Visit = c("V1", "V2"), A = c("1", "2"),
                  B = c("3", "4"), stringsAsFactors = FALSE)
  pages <- as_rtftables(d, stub = stub_spec(vars = c("Param", "Visit")))
  tbl <- pages[[1L]]
  expect_identical(names(tbl$data), c("Param / Visit", "A", "B"))
  out <- style_body(tbl, cols = list("Param / Visit", 3), bold = TRUE)
  expect_identical(out$cell_styles[[1L]]$bold, c(TRUE, NA, TRUE))
  expect_error(style_body(tbl, cols = "Param", bold = TRUE), "column 'Param' not found")
  # NULL is every column
  expect_identical(style_body(tbl, bold = TRUE)$cell_styles[[1L]]$bold, rep(TRUE, 3L))
})

test_that("FT-TB3221 [FR-TB401]: italic is set and a NULL flag leaves an existing one", {
  t0 <- rtftable(.g5_df())
  t1 <- style_body(t0, rows = 2, bold = TRUE)
  t2 <- style_body(t1, rows = 2, italic = TRUE, bold = NULL)
  expect_identical(t2$cell_styles[[2L]]$italic, rep(TRUE, 4L))
  expect_identical(t2$cell_styles[[2L]]$bold, rep(TRUE, 4L))
  expect_null(t2$cell_styles[[1L]])
  t3 <- style_body(t2, rows = 2, underline = TRUE)
  expect_identical(t3$cell_styles[[2L]]$italic, rep(TRUE, 4L))
  expect_identical(t3$cell_styles[[2L]]$underline, rep(TRUE, 4L))
  row <- .render_rtftable(t2, 9360L)[3L]
  expect_match(row, "\\b \\i Sex\\i0 \\b0 \\cell", fixed = TRUE)
})

test_that("FT-TB3222 [FR-TB410]: a selection that matches no row returns the table unchanged", {
  tbl <- rtftable(.g5_df())
  expect_identical(style_body(tbl, rows = ~ Item == "nothing", bold = TRUE), tbl)
  expect_identical(style_body(tbl, rows = function(d) d$Item == "zzz", bold = TRUE), tbl)
  expect_identical(style_body(tbl, rows = integer(0), bold = TRUE), tbl)
})

test_that("FT-TB3223 [FR-TB412]: style_body on a page list refuses integer and logical rows", {
  t1 <- rtftable(.g5_df())
  msg <- paste0("`style_body()` on a page list cannot take integer/logical `rows` -- ",
                "page-local row numbers are ambiguous across pages. Use a predicate ",
                "function or one-sided formula (evaluated per page), or style a single ",
                "page directly.")
  expect_error(style_body(list(t1, t1), rows = 1, bold = TRUE), msg, fixed = TRUE)
  expect_error(style_body(list(t1, t1), rows = c(TRUE, FALSE, TRUE), bold = TRUE), msg,
               fixed = TRUE)
  expect_error(
    style_body(list(t1, .g5_df()), rows = ~ TRUE, bold = TRUE),
    paste0("`style_body()` on a list expects every element to be an rtftable (as ",
           "returned by as_rtftables()); element 2 is 'data.frame'."), fixed = TRUE)
})

test_that("FT-TB3224 [FR-TB414]: style_cols patches every column, or each page of a list", {
  t1 <- rtftable(.g5_df())
  all_bold <- style_cols(t1, bold = TRUE)
  expect_identical(vapply(all_bold$col_spec, function(s) s$bold, logical(1L)), rep(TRUE, 4L))
  expect_identical(vapply(t1$col_spec, function(s) s$bold, logical(1L)), rep(FALSE, 4L))
  pages <- style_cols(list(t1, t1), cols = 1, align = "right")
  expect_length(pages, 2L)
  for (p in pages) {
    expect_identical(vapply(p$col_spec, function(s) s$align, ""),
                     c("right", "center", "center", "center"))
  }
})

test_that("FT-TB3225 [FR-TB415]: style_cols names a missing column and an out-of-range position", {
  t1 <- rtftable(.g5_df())
  expect_error(style_cols(t1, cols = "nope", bold = TRUE),
               "`style_cols(cols)` column 'nope' not found in the table.", fixed = TRUE)
  expect_error(style_cols(t1, cols = 9, bold = TRUE),
               "`style_cols(cols)` index 9 out of range (1..4).", fixed = TRUE)
  mixed <- style_cols(t1, cols = list("A", 4), bold = TRUE)
  expect_identical(vapply(mixed$col_spec, function(s) s$bold, logical(1L)),
                   c(FALSE, TRUE, FALSE, TRUE))
})

test_that("FT-TB3226 [FR-TB417]: style_cols names the align argument it rejects", {
  t1 <- rtftable(.g5_df())
  expect_error(style_cols(t1, align = "middle"),
               "`style_cols(align = )` must be \"left\", \"center\", or \"right\".",
               fixed = TRUE)
  expect_error(style_cols(t1, header_align = "middle"),
               "`style_cols(header_align = )` must be \"left\", \"center\", or \"right\".",
               fixed = TRUE)
})

test_that("FT-TB3227 [FR-TB418]: style_cols sets body italic and underline and the header look", {
  out <- style_cols(rtftable(.g5_df()), cols = 2, italic = TRUE, underline = TRUE,
                    header_bold = TRUE, header_italic = TRUE)
  s <- out$col_spec[[2L]]
  expect_true(s$italic)
  expect_true(s$underline)
  expect_true(s$header_bold)
  expect_true(s$header_italic)
  expect_false(out$col_spec[[3L]]$italic)
  expect_false(out$col_spec[[3L]]$underline)
  expect_false(out$col_spec[[3L]]$header_bold)
  expect_match(.render_rtftable(out, 9360L)[3L], "\\i \\ul 2\\ulnone \\i0 \\cell", fixed = TRUE)
})

test_that("FT-TB3228 [FR-TB420]: style_cols sets the body indent in twips and the colour", {
  out <- style_cols(rtftable(.g5_df()), cols = 2, indent_twips = 120, color = "#FF0000")
  expect_identical(out$col_spec[[2L]]$indent_twips, 120L)
  expect_identical(out$col_spec[[2L]]$color, "#FF0000")
  expect_identical(out$col_spec[[3L]]$indent_twips, 0L)
  rows <- .render_rtftable(out, 9360L)
  expect_match(rows[3L], "\\qc\\li120\\ri0", fixed = TRUE)
  expect_match(rows[3L], "\\qc\\li0\\ri0 5\\cell", fixed = TRUE)
})

test_that("FT-TB3229 [FR-TB426]: style_header with row = NULL styles every header row", {
  out <- style_header(.g5_hdr_tbl(), bold = TRUE)
  for (r in 1:2) {
    expect_true(all(vapply(out$col_header[[r]], function(z) isTRUE(z$bold), logical(1L))))
  }
  # the leaf label row is styled through the columns
  expect_identical(vapply(out$col_spec, function(s) s$header_bold, logical(1L)), rep(TRUE, 4L))
  # row = 2 touches only the second row
  one <- style_header(.g5_hdr_tbl(), row = 2, bold = TRUE)
  expect_false(any(vapply(one$col_header[[1L]], function(z) isTRUE(z$bold), logical(1L))))
  expect_true(all(vapply(one$col_header[[2L]], function(z) isTRUE(z$bold), logical(1L))))
})

test_that("FT-TB3230 [FR-TB428]: style_header cols mix names and positions; spans intersect", {
  t1 <- .g5_hdr_tbl()
  bolds <- function(x, r) vapply(x$col_header[[r]], function(z) isTRUE(z$bold), logical(1L))
  # "A" is column 2, position 4 lies in the 3-4 span: the Item cell is untouched
  out <- style_header(t1, row = 1, cols = list("A", 4), bold = TRUE)
  expect_identical(bolds(out, 1L), c(FALSE, TRUE, TRUE))
  # a position inside a span targets the spanning cell
  expect_identical(bolds(style_header(t1, row = 1, cols = 4, bold = TRUE), 1L),
                   c(FALSE, FALSE, TRUE))
  # NULL is every column
  expect_identical(bolds(style_header(t1, row = 1, bold = TRUE), 1L), c(TRUE, TRUE, TRUE))
  expect_error(style_header(t1, cols = "x", bold = TRUE),
               "`style_header(cols)` column 'x' not found in the table.", fixed = TRUE)
  expect_error(style_header(t1, cols = 9, bold = TRUE),
               "`style_header(cols)` index 9 out of range (1..4).", fixed = TRUE)
})

test_that("FT-TB3231 [FR-TB429]: several labels are recycled over the targeted cells in order", {
  t1 <- rtftable(.g5_df(), col_header = c("A1", "B1", "C1", "D1"))
  out <- style_header(t1, row = 1, cols = 1:3, label = c("X", "Y"))
  expect_identical(out$col_header[[1L]], c("X", "Y", "X", "D1"))
  cells <- .g5_hdr_tbl()
  out2 <- style_header(cells, row = 1, label = c("X", "Y"))
  expect_identical(vapply(out2$col_header[[1L]], function(z) z$label, ""), c("X", "Y", "X"))
})

test_that("FT-TB3232 [FR-TB430]: border sides merge, a later call wins per side, none erases", {
  t1 <- .g5_hdr_tbl()
  b1 <- style_header(t1, row = 2, cols = 4, border = rtf_border(top = TRUE))
  b2 <- style_header(b1, row = 2, cols = 4, border = rtf_border(bottom = TRUE))
  cell <- b2$col_header[[2L]][[3L]]
  expect_identical(cell$border$top$style, "single")
  expect_identical(cell$border$bottom$style, "single")
  # the other side of a later call: top again overrides, bottom survives
  b3 <- style_header(b2, row = 2, cols = 4, border = rtf_border(top = "double"))
  cell3 <- b3$col_header[[2L]][[3L]]
  expect_identical(cell3$border$top$style, "double")
  expect_identical(cell3$border$bottom$style, "single")
  # "none" is an explicit no-line, not an unset side
  b4 <- style_header(b3, row = 2, cols = 4, border = rtf_border(top = "none"))
  expect_identical(b4$col_header[[2L]][[3L]]$border$top$style, "none")
  # the cell next to it is untouched
  expect_null(b2$col_header[[2L]][[2L]]$border)
})

test_that("FT-TB3233 [FR-TB432]: style_header pins the message of each bad argument", {
  t1 <- rtftable(.g5_df())
  expect_error(style_header(t1, align = "middle"),
               "`style_header(align = )` must be \"left\", \"center\", or \"right\".",
               fixed = TRUE)
  expect_error(style_header(t1, border = list(top = 1)),
               "`style_header(border = )` must be an rtf_border() object.", fixed = TRUE)
  expect_error(style_header(t1, bold = "yes"),
               "`style_header(bold = )` must be TRUE or FALSE.", fixed = TRUE)
  expect_error(style_header(t1, italic = NA),
               "`style_header(italic = )` must be TRUE or FALSE.", fixed = TRUE)
})

test_that("FT-TB3234 [FR-TB436]: style_header on a table with no columns stops", {
  empty <- rtftable(data.frame())
  expect_error(style_header(empty, bold = TRUE),
               "`style_header()`: this rtftable has no column header.", fixed = TRUE)
})

test_that("FT-TB3235 [FR-TB438]: style_header warns with the valid arguments, a bad list stops", {
  t1 <- rtftable(.g5_df())
  w <- tryCatch(style_header(t1, labl = "x"), warning = function(w) conditionMessage(w))
  expect_match(w, "`style_header()`: unknown argument `labl` (did you mean `label`?) ignored.",
               fixed = TRUE)
  expect_match(w, paste0("Valid arguments: row, cols, label, border, align, bold, italic, ",
                         "underline."), fixed = TRUE)
  expect_error(style_header(list(t1, .g5_df()), bold = TRUE),
               paste0("`style_header()` on a list expects every element to be an rtftable (as ",
                      "returned by as_rtftables()); element 2 is 'data.frame'."), fixed = TRUE)
})

# ------------------------------------------------- style_zone and borders

# One body row: no multi-row zone, so a zone border raises no old-reading warning.
.g5_one <- function() data.frame(Item = "a", A = "1", B = "2", stringsAsFactors = FALSE)

.g5_edge <- function(style) rtf_border(bottom = rtf_border_line(style))

# Forget that the once-a-session warning was given.
.g5_reset_edge_warning <- function() {
  if (exists("border_edge_reading", envir = .deprecation_state, inherits = FALSE)) {
    rm(list = "border_edge_reading", envir = .deprecation_state)
  }
}

test_that("FT-TB3236 [FR-TB439]: style_zone merges onto an empty border and returns a copy", {
  tbl <- rtftable(.g5_one(), border = "none")
  expect_null(tbl$border)
  out <- style_zone(tbl, body = rtf_border(top = TRUE))
  expect_s3_class(out, "rtftable")
  expect_identical(out$border$body$top, rtf_border_line("single", 15L))
  expect_null(tbl$border)                      # the original is unchanged
  # each zone lands on its own
  for (z in c("header", "spanning", "body", "first_row", "last_row")) {
    arg <- stats::setNames(list(.g5_edge("dot")), z)
    o <- do.call(style_zone, c(list(tbl), arg))
    expect_identical(o$border[[z]]$bottom$style, "dot")
    others <- setdiff(c("header", "spanning", "body", "first_row", "last_row"), z)
    for (k in others) expect_null(o$border[[k]])
  }
})

test_that("FT-TB3237 [FR-TB439]: a NULL side keeps what was there, FALSE or none erases it", {
  base <- style_zone(rtftable(.g5_one(), border = "none"),
                     last_row = rtf_border(top = TRUE, bottom = TRUE))
  kept <- style_zone(base, last_row = rtf_border(top = "double"))
  expect_identical(kept$border$last_row$top$style, "double")
  expect_identical(kept$border$last_row$bottom$style, "single")
  off <- style_zone(base, last_row = rtf_border(bottom = FALSE))
  expect_identical(off$border$last_row$bottom$style, "none")
  expect_identical(off$border$last_row$top$style, "single")
  off2 <- style_zone(base, last_row = rtf_border(bottom = "none"))
  expect_identical(off2$border$last_row$bottom$style, "none")
  # the table it was made from still has both rules
  expect_identical(base$border$last_row$bottom$style, "single")
})

test_that("FT-TB3238 [FR-TB440]: a zone left NULL is untouched and no zone returns the table", {
  tbl <- rtftable(.g5_one(), border = "none")
  tbl <- style_zone(tbl, spanning = .g5_edge("dash"), body = .g5_edge("dot"),
                    first_row = .g5_edge("thick"), last_row = .g5_edge("double"))
  expect_identical(style_zone(tbl), tbl)
  out <- style_zone(tbl, header = .g5_edge("single"))
  expect_identical(out$border$header$bottom$style, "single")
  for (z in c("spanning", "body", "first_row", "last_row")) {
    expect_identical(out$border[[z]], tbl$border[[z]])
  }
  expect_identical(out$border$spanning$bottom$style, "dash")
  expect_identical(out$border$body$bottom$style, "dot")
  expect_identical(out$border$first_row$bottom$style, "thick")
  expect_identical(out$border$last_row$bottom$style, "double")
})

test_that("FT-TB3239 [FR-TB442]: style_zone merges on every page of a list, a bad element stops", {
  t1 <- rtftable(.g5_one(), border = "none")
  pages <- style_zone(list(t1, t1), last_row = .g5_edge("double"))
  expect_length(pages, 2L)
  for (p in pages) expect_identical(p$border$last_row$bottom$style, "double")
  expect_error(
    style_zone(list(t1, data.frame()), body = .g5_edge("single")),
    paste0("`style_zone()` on a list expects every element to be an rtftable (as ",
           "returned by as_rtftables()); element 2 is 'data.frame'."), fixed = TRUE)
})

test_that("FT-TB3240 [FR-TB443]: the changed edge reading warns once a session, in full", {
  .g5_reset_edge_warning()
  on.exit(.g5_reset_edge_warning(), add = TRUE)
  d <- data.frame(Item = c("a", "b", "c"), A = c("1", "2", "3"), stringsAsFactors = FALSE)
  tbl <- rtftable(d, border = "none")
  s <- rtf_border_line()
  ws <- character()
  withCallingHandlers(
    style_zone(tbl, body = rtf_border(top = s)),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_identical(ws, paste0(
    "The edges of `rtf_border()` now mean the *outer* edges of whatever you attach it to.\n",
    "  Before 0.5.0 `left`/`right` were drawn on every cell of a row, and a multi-row zone's\n",
    "  `top`/`bottom` on every one of its rows.  To keep that look, name the interior rule:\n",
    "    rtf_border(left = s, right = s, inside_v = s)   # was: left/right alone\n",
    "    rtf_border(bottom = s, inside_h = s)            # was: bottom alone on a body zone\n",
    "  Naming `inside_h` / `inside_v` (with `rtf_border_line(\"none\")` for \"no rule\") ",
    "silences this.\n",
    "  See ?rtf_border, section \"Writing borders before and after 0.5.0\", for the old ",
    "and new spelling of each case."))
  # the second call, and a left/right border, are silent this session
  expect_silent(style_zone(tbl, body = rtf_border(top = s)))
  expect_silent(style_zone(tbl, body = rtf_border(left = s)))
})

test_that("FT-TB3241 [FR-TB443]: naming inside_h or inside_v, or a single row, does not warn", {
  .g5_reset_edge_warning()
  on.exit(.g5_reset_edge_warning(), add = TRUE)
  d <- data.frame(Item = c("a", "b", "c"), A = c("1", "2", "3"), stringsAsFactors = FALSE)
  tbl <- rtftable(d, border = "none")
  s <- rtf_border_line()
  expect_silent(style_zone(tbl, body = rtf_border(top = s, inside_h = "none")))
  expect_silent(style_zone(tbl, body = rtf_border(left = s, inside_v = "none")))
  expect_silent(style_zone(rtftable(d[1L, ], border = "none"), body = rtf_border(top = s)))
  # left/right alone does warn (and only once)
  expect_warning(style_zone(tbl, body = rtf_border(left = s)),
                 "The edges of `rtf_border()` now mean the *outer* edges", fixed = TRUE)
})

# ----------------------------------------------------- rtf_table_style

test_that("FT-TB3242 [FR-TB449]: header_bold and header_italic default FALSE, non-TRUE is FALSE", {
  def <- rtf_table_style()
  expect_identical(def$header_bold, FALSE)
  expect_identical(def$header_italic, FALSE)
  odd <- rtf_table_style(header_bold = "yes", header_italic = 1)
  expect_identical(odd$header_bold, FALSE)
  expect_identical(odd$header_italic, FALSE)
  on <- rtf_table_style(header_bold = TRUE, header_italic = TRUE)
  expect_identical(on$header_bold, TRUE)
  expect_identical(on$header_italic, TRUE)
})

test_that("FT-TB3243 [FR-TB451]: bold, italic and underline style the data cells, default FALSE", {
  d <- data.frame(Item = "a", A = "1", stringsAsFactors = FALSE)
  def <- rtf_table_style()
  expect_identical(c(def$bold, def$italic, def$underline), c(FALSE, FALSE, FALSE))
  odd <- rtf_table_style(bold = "yes", italic = 1, underline = "TRUE")
  expect_identical(c(odd$bold, odd$italic, odd$underline), c(FALSE, FALSE, FALSE))
  tbl <- rtftable(d, style = rtf_table_style(bold = TRUE, italic = TRUE, underline = TRUE))
  for (s in tbl$col_spec) {
    expect_identical(c(s$bold, s$italic, s$underline), c(TRUE, TRUE, TRUE))
  }
  expect_match(.render_rtftable(tbl, 9360L)[2L], "\\b \\i \\ul a\\ulnone \\i0 \\b0 \\cell",
               fixed = TRUE)
  plain <- rtftable(d, style = def)
  for (s in plain$col_spec) {
    expect_identical(c(s$bold, s$italic, s$underline), c(FALSE, FALSE, FALSE))
  }
})

# The \ri of the cells of the row that holds `text`, in a document's RTF.
.g5_ri <- function(doc, text, env = parent.frame()) {
  d <- .g5_dir(env)
  f <- file.path(d, "t.rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  l <- readLines(f, warn = FALSE)
  row <- grep(text, l, fixed = TRUE, value = TRUE)[1L]
  unique(regmatches(row, gregexpr("\\\\ri[0-9]+", row))[[1L]])
}

test_that("FT-TB3244 [FR-TB452]: the style's right padding is \\ri, NULL leaves the document's", {
  on.exit(rtfreporter_reset_defaults(), add = TRUE)
  d <- data.frame(Item = "Zed", A = "1", stringsAsFactors = FALSE)
  styled <- rtftable(d, style = rtf_table_style(cell_padding_right_twips = 77L))
  expect_identical(styled$cell_padding_right_twips, 77L)
  expect_identical(.g5_ri(rtf_tables(rtf_document(), list(styled)), "Zed"), "\\ri77")
  # NULL padding: the table carries none and the document's applies
  bare <- rtftable(d, style = rtf_table_style())
  expect_null(bare$cell_padding_right_twips)
  expect_null(bare$cell_padding_left_twips)
  doc <- rtf_config(rtf_tables(rtf_document(), list(bare)),
                    style = rtf_style(cell_padding_right_twips = 55L))
  expect_identical(.g5_ri(doc, "Zed"), "\\ri55")
})

test_that("FT-TB3245 [FR-TB570]: padding cascades style, then rtf_style, then option, then 0", {
  on.exit(rtfreporter_reset_defaults(), add = TRUE)
  d <- data.frame(Item = "Zed", A = "1", stringsAsFactors = FALSE)
  doc_of <- function(tbl, ...) rtf_config(rtf_tables(rtf_document(), list(tbl)), ...)
  # none set: 0
  expect_identical(.g5_ri(doc_of(rtftable(d)), "Zed"), "\\ri0")
  # the option alone
  options(rtfreporter.cell_padding_right_twips = 9L)
  expect_identical(.g5_ri(doc_of(rtftable(d)), "Zed"), "\\ri9")
  # rtf_style beats the option
  expect_identical(.g5_ri(doc_of(rtftable(d), style = rtf_style(cell_padding_right_twips = 33L)),
                          "Zed"), "\\ri33")
  # the table's style beats rtf_style, and the table's own argument beats its style
  tbl <- rtftable(d, style = rtf_table_style(cell_padding_right_twips = 44L))
  expect_identical(.g5_ri(doc_of(tbl, style = rtf_style(cell_padding_right_twips = 33L)),
                          "Zed"), "\\ri44")
  own <- rtftable(d, style = rtf_table_style(cell_padding_right_twips = 44L),
                  cell_padding_right_twips = 11L)
  expect_identical(.g5_ri(doc_of(own, style = rtf_style(cell_padding_right_twips = 33L)),
                          "Zed"), "\\ri11")
  # left padding too
  options(rtfreporter.cell_padding_left_twips = 8L)
  d2 <- file.path(.g5_dir(), "l.rtf")
  generate_rtfreport(doc_of(rtftable(d)), d2, overwrite = TRUE)
  row <- grep("Zed", readLines(d2, warn = FALSE), fixed = TRUE, value = TRUE)[1L]
  expect_match(row, "\\li8\\ri9", fixed = TRUE)
})

test_that("FT-TB3246 [FR-TB454]: printing a style lists zones, alignments and padding", {
  b <- rtf_border(top = TRUE)
  sty <- rtf_table_style(border_header = b, border_first_row = b, border_last_row = b,
                         header_bold = TRUE, align = "right", bold = TRUE)
  expect_identical(capture.output(print(sty)), c(
    "<rtf_table_style>",
    "  borders:",
    "    header    : <rtf_border>",
    "    spanning  : none",
    "    body      : none",
    "    first_row : <rtf_border>",
    "    last_row  : <rtf_border>",
    "  header_align : (inherit align)",
    "  header_bold  : TRUE",
    "  align        : right",
    "  bold         : TRUE",
    "  cell_padding : L=(default) R=(default)"))
  sty2 <- rtf_table_style(border_spanning = b, border_body = b, header_align = "left",
                          cell_padding_left_twips = 10L, cell_padding_right_twips = 20L)
  out <- capture.output(print(sty2))
  expect_identical(out[c(3:7, 8:12)], c(
    "    header    : none",
    "    spanning  : <rtf_border>",
    "    body      : <rtf_border>",
    "    first_row : none",
    "    last_row  : none",
    "  header_align : left",
    "  header_bold  : FALSE",
    "  align        : (per column)",
    "  bold         : FALSE",
    "  cell_padding : L=10 R=20"))
})

test_that("FT-TB3247 [FR-TB455]: a style as border or style gives its five zones", {
  d <- data.frame(Item = "a", A = "1", stringsAsFactors = FALSE)
  dbl <- rtf_border(top = "double", bottom = "double")
  sty <- rtf_table_style(border_header = dbl, border_last_row = dbl)
  for (tbl in list(rtftable(d, border = sty), rtftable(d, style = sty))) {
    expect_identical(tbl$border$header, dbl)
    expect_identical(tbl$border$last_row, dbl)
    expect_null(tbl$border$spanning)
    expect_null(tbl$border$body)
    expect_null(tbl$border$first_row)
  }
  # all five zones
  b <- function(s) rtf_border(bottom = s)
  full <- rtf_table_style(border_header = b("single"), border_spanning = b("double"),
                          border_body = b("dot"), border_first_row = b("dash"),
                          border_last_row = b("thick"))
  t5 <- rtftable(d, style = full)
  expect_identical(vapply(c("header", "spanning", "body", "first_row", "last_row"),
                          function(z) t5$border[[z]]$bottom$style, ""),
                   c(header = "single", spanning = "double", body = "dot",
                     first_row = "dash", last_row = "thick"))
})

test_that("FT-TB3248 [FR-TB456]: rtf_table_style_tfl is a header rule pair, new each call", {
  s <- rtf_table_style_tfl()
  expect_s3_class(s, "rtf_table_style")
  line <- rtf_border_line("single", 15L)
  expect_identical(s$border_header$top, line)
  expect_identical(s$border_header$bottom, line)
  expect_null(s$border_header$top$color)
  expect_identical(s$border_header$top$width, 15L)
  for (z in c("border_spanning", "border_body", "border_first_row", "border_last_row")) {
    expect_null(s[[z]])
  }
  expect_identical(s$header_bold, FALSE)
  expect_null(s$header_align)
  # a new object each time: changing one leaves the next call's as it was
  s$header_bold <- TRUE
  s$border_header$top$width <- 99L
  fresh <- rtf_table_style_tfl()
  expect_identical(fresh$header_bold, FALSE)
  expect_identical(fresh$border_header$top$width, 15L)
})

test_that("FT-TB3249 [FR-TB459]: rtf_table_style_tfl takes no argument", {
  expect_error(rtf_table_style_tfl(header_bold = TRUE), "unused argument (header_bold = TRUE)",
               fixed = TRUE)
  expect_error(rtf_table_style_tfl(1), "unused argument (1)", fixed = TRUE)
  derived <- rtf_table_style_with(rtf_table_style_tfl(), header_bold = TRUE)
  expect_true(derived$header_bold)
})

# ------------------------------------------------------------- rtf_border

test_that("FT-TB3250 [FR-TB473]: rtf_border has the six slots, unnamed ones unset", {
  b <- rtf_border(top = TRUE)
  expect_s3_class(b, "rtf_border")
  expect_identical(names(b), c("top", "bottom", "left", "right", "inside_h", "inside_v"))
  for (s in c("bottom", "left", "right", "inside_h", "inside_v")) expect_null(b[[s]])
  expect_identical(b$top, rtf_border_line("single", 15L))
  all_unset <- rtf_border()
  expect_identical(names(all_unset), names(b))
  for (s in names(all_unset)) expect_null(all_unset[[s]])
  # building one never changes another
  b2 <- rtf_border(bottom = TRUE)
  expect_null(b$bottom)
  expect_null(b2$top)
})

test_that("FT-TB3251 [FR-TB474]: each edge takes TRUE, FALSE, none, a style name, a line or NULL", {
  b <- rtf_border(top = TRUE, bottom = FALSE, left = "thick", right = "dot")
  expect_identical(b$top, rtf_border_line("single", 15L, NULL))
  expect_identical(c(b$top$style, b$top$width), c("single", "15"))
  expect_null(b$top$color)
  expect_identical(b$bottom$style, "none")
  expect_identical(rtf_border(bottom = "none")$bottom$style, "none")
  expect_identical(b$left$style, "thick")
  expect_identical(b$right$style, "dot")
  expect_identical(rtf_border(top = "dash")$top$style, "dash")
  expect_identical(rtf_border(top = "double")$top$style, "double")
  expect_identical(rtf_border(top = "single")$top$width, 15L)
  line <- rtf_border_line("double", 30L, "#003366")
  expect_identical(rtf_border(left = line)$left, line)
  expect_null(rtf_border(top = NULL)$top)
})

test_that("FT-TB3252 [FR-TB475]: all is the four edges at once and a named side wins", {
  all_on <- rtf_border(all = TRUE)
  for (s in c("top", "bottom", "left", "right")) {
    expect_identical(all_on[[s]], rtf_border_line("single", 15L))
  }
  expect_null(all_on$inside_h)
  expect_null(all_on$inside_v)
  mix <- rtf_border(all = "double", top = "dot")
  expect_identical(mix$top$style, "dot")
  for (s in c("bottom", "left", "right")) expect_identical(mix[[s]]$style, "double")
  # all = FALSE erases, rtf_border() says nothing
  off <- rtf_border(all = FALSE)
  for (s in c("top", "bottom", "left", "right")) expect_identical(off[[s]]$style, "none")
  for (s in names(rtf_border())) expect_null(rtf_border()[[s]])
})

test_that("FT-TB3253 [FR-TB476]: inside_h and inside_v are printed only when set", {
  expect_identical(capture.output(print(rtf_border(top = TRUE))), c(
    "<rtf_border>",
    "  top     : single, 15 twips",
    "  bottom  : none",
    "  left    : none",
    "  right   : none"))
  h <- capture.output(print(rtf_border(inside_h = TRUE)))
  expect_identical(h[-1L], c(
    "  top     : none",
    "  bottom  : none",
    "  left    : none",
    "  right   : none",
    "  inside_h: single, 15 twips"))
  v <- capture.output(print(rtf_border(inside_v = "dot")))
  expect_identical(v[length(v)], "  inside_v: dot, 15 twips")
  expect_false(any(grepl("inside_h", v)))
})

test_that("FT-TB3254 [FR-TB477]: a bad side stops naming the slot and what is accepted", {
  styles <- "\"single\", \"double\", \"thick\", \"dash\", \"dot\", \"none\""
  expect_error(rtf_border(top = "not-a-side"),
               paste0("`top` must be one of ", styles, ", not \"not-a-side\"."), fixed = TRUE)
  expect_error(rtf_border(inside_h = "wavy"),
               paste0("`inside_h` must be one of ", styles, ", not \"wavy\"."), fixed = TRUE)
  expect_error(rtf_border(inside_v = "wavy"),
               paste0("`inside_v` must be one of ", styles, ", not \"wavy\"."), fixed = TRUE)
  kind <- "must be TRUE, FALSE, a border style name, an rtf_border_line(), or NULL."
  expect_error(rtf_border(top = 5), paste("`top`", kind), fixed = TRUE)
  expect_error(rtf_border(left = list(1)), paste("`left`", kind), fixed = TRUE)
  expect_error(rtf_border(inside_h = 5), paste("`inside_h`", kind), fixed = TRUE)
  expect_error(rtf_border(inside_v = list()), paste("`inside_v`", kind), fixed = TRUE)
})

test_that("FT-TB3255 [FR-TB480]: an rtf_border records which of inside_h and inside_v were named", {
  s <- rtf_border_line()
  expect_identical(attr(rtf_border(inside_h = s), "inside_named"), c(h = TRUE, v = FALSE))
  expect_identical(attr(rtf_border(inside_v = s), "inside_named"), c(h = FALSE, v = TRUE))
  expect_identical(attr(rtf_border(inside_h = s, inside_v = s), "inside_named"),
                   c(h = TRUE, v = TRUE))
  expect_identical(attr(rtf_border(left = s, right = s), "inside_named"),
                   c(h = FALSE, v = FALSE))
  # the attribute is not part of the value: the six slots are the same
  expect_identical(names(rtf_border(inside_h = s)), names(rtf_border()))
})

test_that("FT-TB3256 [FR-TB480]: a left/right border with neither inside rule named warns once", {
  .g5_reset_edge_warning()
  on.exit(.g5_reset_edge_warning(), add = TRUE)
  tbl <- rtftable(data.frame(Item = c("a", "b"), A = c("1", "2"), stringsAsFactors = FALSE),
                  border = "none")
  s <- rtf_border_line()
  expect_warning(style_zone(tbl, last_row = rtf_border(left = s, right = s)),
                 "The edges of `rtf_border()` now mean the *outer* edges", fixed = TRUE)
  expect_silent(style_zone(tbl, last_row = rtf_border(left = s, right = s)))
  .g5_reset_edge_warning()
  expect_silent(style_zone(tbl, last_row = rtf_border(left = s, right = s, inside_v = "none")))
})

test_that("FT-TB3257 [FR-TB481]: a line saved under the old class name is read as a side", {
  line <- rtf_border_line("dot", 20L, "#003366")
  old <- line
  class(old) <- "rtf_border_side"
  b <- rtf_border(top = old)
  expect_identical(b$top$style, "dot")
  expect_identical(b$top$width, 20L)
  expect_identical(b$top$color, "#003366")
  expect_identical(unclass(b$top), unclass(line))
})

test_that("FT-TB3258 [FR-TB482]: printing an rtf_border gives style, width and colour per side", {
  b <- rtf_border(top = rtf_border_line("single", 30L, "#00FF00"), bottom = "double")
  expect_identical(capture.output(print(b)), c(
    "<rtf_border>",
    "  top     : single, 30 twips, color=#00FF00",
    "  bottom  : double, 15 twips",
    "  left    : none",
    "  right   : none"))
  both <- capture.output(print(rtf_border(inside_h = TRUE, inside_v = TRUE)))
  expect_identical(both[6:7], c("  inside_h: single, 15 twips", "  inside_v: single, 15 twips"))
})

# ------------------------------------------------------- rtf_border_line

test_that("FT-TB3259 [FR-TB489]: rtf_border_line is a classed list of style, width and color", {
  l <- rtf_border_line("dot", 20L, "#003366")
  expect_s3_class(l, "rtf_border_line")
  expect_identical(names(l), c("style", "width", "color"))
  expect_identical(l$style, "dot")
  expect_identical(l$width, 20L)
  expect_identical(l$color, "#003366")
  expect_true(is.list(l))
})

test_that("FT-TB3260 [FR-TB490]: every style is accepted, none erases, NULL keeps", {
  for (s in c("single", "double", "thick", "dash", "dot", "none")) {
    expect_identical(rtf_border_line(s)$style, s)
  }
  expect_error(rtf_border_line("wavy"), "should be one of")
  base <- rtftable(.g5_one())       # the default header has a top and a bottom rule
  expect_identical(base$border$header$top$style, "single")
  erased <- style_zone(base, header = rtf_border(top = rtf_border_line("none")))
  expect_identical(erased$border$header$top$style, "none")
  expect_identical(erased$border$header$bottom$style, "single")
  kept <- style_zone(base, header = rtf_border(top = NULL, bottom = "double"))
  expect_identical(kept$border$header$top$style, "single")
  expect_identical(kept$border$header$bottom$style, "double")
  # and in the RTF: no top rule on the header cells after "none"
  head <- .render_rtftable(erased, 9360L)[1L]
  expect_false(grepl("clbrdrt", head, fixed = TRUE))
  expect_true(grepl("clbrdrb", head, fixed = TRUE))
  expect_true(grepl("clbrdrt", .render_rtftable(base, 9360L)[1L], fixed = TRUE))
})

test_that("FT-TB3261 [FR-TB492]: width is 15 twips by default, an integer, and ignored for none", {
  expect_identical(rtf_border_line()$width, 15L)
  expect_identical(rtf_border_line(width = 20.7)$width, 20L)
  expect_true(is.integer(rtf_border_line(width = 20.7)$width))
  expect_identical(rtf_border_line("single", 40)$width, 40L)
  # a none line with any width draws nothing, and may be given a width below 1
  expect_s3_class(rtf_border_line("none", width = 0), "rtf_border_line")
  tbl <- style_zone(rtftable(.g5_one(), border = "none"),
                    header = rtf_border(top = rtf_border_line("none", width = 40)))
  expect_false(grepl("brdrw40", paste(.render_rtftable(tbl, 9360L), collapse = ""),
                     fixed = TRUE))
  thick <- style_zone(rtftable(.g5_one(), border = "none"),
                      header = rtf_border(top = rtf_border_line("single", width = 40)))
  expect_true(grepl("\\clbrdrt\\brdrs\\brdrw40", .render_rtftable(thick, 9360L)[1L],
                    fixed = TRUE))
  expect_error(rtf_border_line("single", 0), "`width` must be a positive integer (twips).",
               fixed = TRUE)
})

test_that("FT-TB3262 [FR-TB494]: the colour is black unless a 6-digit hex string is given", {
  expect_null(rtf_border_line()$color)
  expect_identical(rtf_border_line(color = "#003366")$color, "#003366")
  plain <- style_zone(rtftable(.g5_one(), border = "none"),
                      header = rtf_border(top = rtf_border_line()))
  coloured <- style_zone(rtftable(.g5_one(), border = "none"),
                         header = rtf_border(top = rtf_border_line(color = "#003366")))
  # colours are indexed in the document's colour table: render the whole document
  head_of <- function(tbl, env = parent.frame()) {
    grep("Item\\cell", .g5_rtf(tbl, env), value = TRUE, fixed = TRUE)[1L]
  }
  expect_false(grepl("brdrcf", head_of(plain), fixed = TRUE))
  expect_true(grepl("\\clbrdrt\\brdrs\\brdrw15\\brdrcf3", head_of(coloured), fixed = TRUE))
  expect_error(rtf_border_line(color = "blue"),
               "`color` must be NULL or a 6-digit hex color string", fixed = TRUE)
})

# ------------------------------------------------ cell_rows, col_cell, col_key

test_that("FT-TB3263 [FR-TB516]: cell_rows holds a template, a chain, or a chain with guards", {
  d <- data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 3L),
                  variable = "AGE", variable_level = NA, context = "continuous",
                  stat_name = rep(c("n", "p", "estimate"), 2L),
                  stat_label = rep(c("n", "p", "Est"), 2L),
                  stat = c(0, 0.5, 1.25, 7, 0.25, 2.5), stringsAsFactors = FALSE)
  d$fmt_fun <- list(0L, 1L, 1L, 0L, 1L, 1L)
  n <- normalize_ard(d)
  wide <- function(cells) {
    widen_ard(n, cols = "TRT", rows = c(group = "variable"), cells = cells, notes = FALSE)
  }
  # a plain template
  one <- wide(cell_rows(est = "{n:.0f}"))
  expect_identical(one$label, "est")
  expect_identical(c(one$A, one$B), c("0", "7"))
  # a chain: the first template that resolves is used (zz is not a statistic)
  chain <- wide(cell_rows(est = c("{n:.0f} ({zz:.1f})", "{n:.0f}")))
  expect_identical(c(chain$A, chain$B), c("0", "7"))
  full <- wide(cell_rows(est = c("{n:.0f} ({p:.1f})", "{n:.0f}")))
  expect_identical(c(full$A, full$B), c("0 (0.5)", "7 (0.2)"))
  # a chain with a guard: the zero count is "0", the rest the estimate line
  guard <- wide(cell_rows(est = c(n == 0 ~ "0", "{n:.0f} ({estimate:.1f})")))
  expect_identical(c(guard$A, guard$B), c("0", "7 (2.5)"))
})

test_that("FT-TB3264 [FR-TB520]: col_cell holds pos and label, and the rest only when given", {
  expect_identical(names(col_cell(2, "x")), c("pos", "label"))
  expect_s3_class(col_cell(2, "x"), "rtf_col_cell")
  b <- rtf_border(bottom = TRUE)
  c1 <- col_cell(2, "x", border = b)
  expect_identical(names(c1), c("pos", "label", "border"))
  expect_identical(c1$border, b)
  all_set <- col_cell(2, "x", align = "right", bold = TRUE, italic = TRUE, underline = TRUE)
  expect_identical(names(all_set),
                   c("pos", "label", "align", "bold", "italic", "underline"))
  expect_identical(names(col_cell(2, "x", bold = FALSE, italic = FALSE)), c("pos", "label"))
})

test_that("FT-TB3265 [FR-TB520]: positions are those of the data columns, not of the row above", {
  d <- data.frame(Item = "a", g1 = "1", g2 = "2", g3 = "3", stringsAsFactors = FALSE)
  hdr <- rtf_col_header(
    list(col_cell(c(1, 2), "G1"), col_cell(c(3, 4), "G2")),
    list(col_cell(3, "c3")),
    c("Item", "g1", "g2", "g3"))
  tbl <- rtftable(d, col_header = hdr)
  row2 <- tbl$col_header[[2L]]
  expect_identical(vapply(row2, function(z) z$from, integer(1L)), c(1L, 3L, 4L))
  expect_identical(vapply(row2, function(z) z$to, integer(1L)), c(2L, 3L, 4L))
  expect_identical(vapply(row2, function(z) z$label, ""), c("", "c3", ""))
})

test_that("FT-TB3266 [FR-TB521]: a range is inclusive, start <= end, values at least 1", {
  expect_identical(col_cell(3)$pos, 3L)
  expect_identical(col_cell(c(2, 5))$pos, c(2L, 5L))
  expect_identical(col_cell(c(3, 3))$pos, c(3L, 3L))
  expect_error(col_cell(c(5, 2)), "`pos` start must be <= end.", fixed = TRUE)
  expect_error(col_cell(0), "`pos` values must be >= 1.", fixed = TRUE)
  expect_error(col_cell(c(0, 3)), "`pos` values must be >= 1.", fixed = TRUE)
})

test_that("FT-TB3267 [FR-TB522]: a pos of the wrong kind or length stops with the full message", {
  msg <- paste0("`pos` must be a number or a column name of length 1 or 2, ",
                "or a selector function such as col_key().")
  expect_error(col_cell(NA), msg, fixed = TRUE)
  expect_error(col_cell(c(1, NA)), msg, fixed = TRUE)
  expect_error(col_cell(list(1)), msg, fixed = TRUE)
  expect_error(col_cell(TRUE), msg, fixed = TRUE)
  expect_error(col_cell(c(1, 2, 3)), msg, fixed = TRUE)
  expect_error(col_cell(numeric(0)), msg, fixed = TRUE)
  expect_error(col_cell(c(-1, 3)), "`pos` values must be >= 1.", fixed = TRUE)
  expect_error(col_cell(c(4, 2)), "`pos` start must be <= end.", fixed = TRUE)
})

test_that("FT-TB3268 [FR-TB523]: a named span follows the named columns when data is reordered", {
  d <- data.frame(Item = c("a", "b"), g1 = c("1", "2"), g2 = c("3", "4"), g3 = c("5", "6"),
                  stringsAsFactors = FALSE)
  hdr <- rtf_col_header(list(col_cell(c("g1", "g3"), "Drug")))
  span_of <- function(df) {
    cell <- rtftable(df, col_header = hdr)$col_header[[1L]]
    cell <- Filter(function(z) identical(z$label, "Drug"), cell)[[1L]]
    c(cell$from, cell$to)
  }
  expect_identical(span_of(d), c(2L, 4L))
  expect_identical(span_of(d[, c("g1", "g2", "g3", "Item")]), c(1L, 3L))
  expect_identical(span_of(d[, c("g1", "Item", "g2", "g3")]), c(1L, 4L))
  # one name is one column, wherever it lies
  one <- rtf_col_header(list(col_cell("g2", "Two")))
  cell <- Filter(function(z) identical(z$label, "Two"),
                 rtftable(d[, c("g3", "g2", "g1", "Item")], col_header = one)$col_header[[1L]])
  expect_identical(c(cell[[1L]]$from, cell[[1L]]$to), c(2L, 2L))
})

test_that("FT-TB3269 [FR-TB525]: a selector matches adjacent columns and is resolved on attach", {
  d <- data.frame(Item = "a", g1 = "1", g2 = "2", g3 = "3", stringsAsFactors = FALSE)
  seen <- list()
  sel <- function(nm) {
    seen[[length(seen) + 1L]] <<- nm
    c(2L, 4L)
  }
  cell <- col_cell(sel, "x")
  expect_length(seen, 0L)                   # not called when the cell is built
  hdr <- rtf_col_header(list(cell), c("Item", "g1", "g2", "g3"))
  expect_length(seen, 0L)                   # nor when the header is
  expect_error(
    rtftable(d, col_header = hdr),
    paste0("The `pos` selector matched non-adjacent data columns (2, 4); a header cell can ",
           "only span a contiguous range."),
    fixed = TRUE)
  expect_identical(seen[[1L]], names(d))    # resolved against the data column names
  # an adjacent match resolves to its range
  adj <- rtf_col_header(list(col_cell(function(nm) nm %in% c("g1", "g2"), "G")))
  cells <- rtftable(d, col_header = adj)$col_header[[1L]]
  g <- Filter(function(z) identical(z$label, "G"), cells)[[1L]]
  expect_identical(c(g$from, g$to), c(2L, 3L))
})

test_that("FT-TB3270 [FR-TB526]: the label is a scalar, NULL is empty, longer uses first", {
  expect_identical(col_cell(2, "")$label, "")
  expect_identical(col_cell(2, NULL)$label, "")
  expect_identical(col_cell(2, c("a", "b"))$label, "a")
  expect_identical(col_cell(2)$label, "")
})

test_that("FT-TB3271 [FR-TB527]: align NULL inherits the leftmost covered column's header_align", {
  d <- data.frame(Item = "a", g1 = "1", g2 = "2", g3 = "3", stringsAsFactors = FALSE)
  mk <- function(...) {
    rtftable(d, col_header = rtf_col_header(list(col_cell(1, ""), col_cell(c(2, 4), "X", ...)),
                                            c("Item", "g1", "g2", "g3")))
  }
  first <- function(tbl) .render_rtftable(tbl, 9360L)[1L]
  # the span's own cell is the second \cell of the row
  align_of <- function(row) sub(".*\\\\cell\\\\(q[lrc])\\\\li0\\\\ri0 X\\\\cell.*", "\\1", row)
  expect_identical(align_of(first(mk())), "qc")
  expect_identical(align_of(first(style_cols(mk(), cols = 2, header_align = "right"))), "qr")
  # a column that is not the leftmost covered one does not decide
  expect_identical(align_of(first(style_cols(mk(), cols = 3, header_align = "right"))), "qc")
  # an explicit align sets the cell, whatever the column says
  expect_identical(align_of(first(style_cols(mk(align = "left"), cols = 2,
                                             header_align = "right"))), "ql")
  expect_identical(align_of(first(mk(align = "center"))), "qc")
  expect_identical(align_of(first(mk(align = "right"))), "qr")
})

test_that("FT-TB3272 [FR-TB530]: a NULL border inherits the zone border, an rtf_border overrides", {
  d <- data.frame(Item = "a", g1 = "1", g2 = "2", g3 = "3", stringsAsFactors = FALSE)
  hdr <- rtf_col_header(
    list(col_cell(1, "I"), col_cell(2, "W"), col_cell(3, "X"),
         col_cell(4, "Y", border = rtf_border(top = "double"))),
    c("Item", "g1", "g2", "g3"))
  row <- .render_rtftable(rtftable(d, col_header = hdr), 9360L)[1L]
  cell_defs <- strsplit(row, "\\\\cellx[0-9]+")[[1L]]
  expect_match(cell_defs[1L], "\\trowd\\trrh230\\clbrdrt\\brdrs\\brdrw15\\clvertalb", fixed = TRUE)
  expect_match(cell_defs[2L], "^\\\\clbrdrt\\\\brdrs\\\\brdrw15\\\\clvertalb")
  expect_match(cell_defs[3L], "^\\\\clbrdrt\\\\brdrs\\\\brdrw15\\\\clvertalb")
  expect_match(cell_defs[4L], "^\\\\clbrdrt\\\\brdrdb\\\\brdrw15\\\\clvertalb")
  expect_equal(lengths(regmatches(row, gregexpr("brdrdb", row))), 1L)
})

test_that("FT-TB3273 [FR-TB532]: printing a col_cell shows position, label, alignment and flags", {
  expect_output(print(col_cell(c(2, 5), "Treatment", align = "center", bold = TRUE)),
                "<col_cell pos=2..5 label=\"Treatment\", align=center [b]>", fixed = TRUE)
  expect_output(print(col_cell(c(2, 5), "Treatment", align = "center", bold = TRUE,
                               italic = TRUE, underline = TRUE)),
                "<col_cell pos=2..5 label=\"Treatment\", align=center [biu]>", fixed = TRUE)
  expect_output(print(col_cell(3, "One")), "<col_cell pos=3 label=\"One\">", fixed = TRUE)
  expect_output(print(col_cell(c("g1", "g3"), "Drug")),
                "<col_cell pos=g1..g3 label=\"Drug\">", fixed = TRUE)
  expect_output(print(col_cell(col_key("Placebo"), "T")),
                "<col_cell pos=col_key(\"Placebo\") label=\"T\">", fixed = TRUE)
})

test_that("FT-TB3274 [FR-TB536]: col_key picks the separator that occurs, or uses the given one", {
  # "____" first
  sel <- col_key("Placebo")
  expect_identical(sel(c("Placebo____Day 1", "Active____Day 1", "Placebo")),
                   c(TRUE, FALSE, TRUE))
  # then tfrmt's delimiter
  expect_identical(sel(c("Placebo___tlang_delim___Day 1", "Active___tlang_delim___Day 1")),
                   c(TRUE, FALSE))
  expect_identical(col_key("Day 1", part = 2)(c("Placebo___tlang_delim___Day 1",
                                                "Active___tlang_delim___Day 2")),
                   c(TRUE, FALSE))
  # a single string is the separator: "____" is then not one
  pipe <- col_key("Placebo", sep = "|")
  expect_identical(pipe(c("Placebo|Day 1", "Active|Day 1", "Placebo____Day 1")),
                   c(TRUE, FALSE, FALSE))
  expect_identical(col_key("Day 1", sep = "|", part = 2)(c("Placebo|Day 1", "Placebo|Day 2")),
                   c(TRUE, FALSE))
})

test_that("FT-TB3275 [FR-TB537]: a sep that is not NULL or one non-empty string stops", {
  msg <- "`sep` must be NULL or a single non-empty string."
  expect_error(col_key("a", sep = ""), msg, fixed = TRUE)
  expect_error(col_key("a", sep = 1), msg, fixed = TRUE)
  expect_error(col_key("a", sep = NA_character_), msg, fixed = TRUE)
  expect_error(col_key("a", sep = c("|", ";")), msg, fixed = TRUE)
  expect_error(col_key("a", sep = character(0)), msg, fixed = TRUE)
})

test_that("FT-TB3276 [FR-TB539]: a name with too few segments gives NA and does not match", {
  expect_error(col_key("x", part = 0), "`part` must be a non-zero integer.", fixed = TRUE)
  expect_identical(.name_segment(c("x", "a____x"), NULL, 2L), c(NA_character_, "x"))
  sel <- col_key("x", part = 2)
  expect_identical(sel(c("x", "a____x", "x____a")), c(FALSE, TRUE, FALSE))
  # counting from the end
  expect_identical(.name_segment(c("x", "a____b____x"), NULL, -1L), c("x", "x"))
  expect_identical(.name_segment("x", NULL, -2L), NA_character_)
  expect_identical(col_key("x", part = -2)(c("x", "x____a")), c(FALSE, TRUE))
})

test_that("FT-TB3277 [FR-TB541]: a bad selector result is reported under the selector's name", {
  d <- data.frame(Item = "a", g1 = "1", g2 = "2", g3 = "3", stringsAsFactors = FALSE)
  attach_with <- function(fun) {
    sel <- structure(fun, rtf_sel_label = "my_sel")
    rtftable(d, col_header = rtf_col_header(list(col_cell(sel, "x")),
                                            c("Item", "g1", "g2", "g3")))
  }
  expect_error(attach_with(function(nm) TRUE),
               "my_sel returned 1 logical values for 4 data columns.", fixed = TRUE)
  expect_error(attach_with(function(nm) "zz"),
               "my_sel returned unknown column name(s): \"zz\".", fixed = TRUE)
  expect_error(attach_with(function(nm) 9L),
               "my_sel matched positions outside the data column range 1..4.", fixed = TRUE)
  expect_error(attach_with(function(nm) list(1)),
               "my_sel must return a logical, integer or character vector.", fixed = TRUE)
  expect_error(attach_with(function(nm) rep(FALSE, 4L)),
               "my_sel matched no data columns.", fixed = TRUE)
  # a col_key selector names itself
  expect_error(
    rtftable(d, col_header = rtf_col_header(list(col_cell(col_key("Placebo"), "x")),
                                            c("Item", "g1", "g2", "g3"))),
    "col_key(\"Placebo\") matched no data columns.", fixed = TRUE)
  # without the column names
  expect_error(
    .resolve_col_sel(col_key("Placebo"), NULL),
    paste0("A selector `pos` in col_cell() requires the header to be attached to a table ",
           "(data column names are unknown here)."), fixed = TRUE)
})

# --------------------------------------------- rtftable, as_rtftables, adapters

test_that("FT-TB3278 [FR-TB573]: the help example is one page and Treatment spans the two arms", {
  dm <- data.frame(Parameter = c("Age (years)", "  Mean (SD)", "  Median"),
                   Placebo = c("", "75.1 (8.2)", "76.0"), Active = c("", "74.3 (7.9)", "75.0"),
                   stringsAsFactors = FALSE)
  tbl <- rtftable(
    dm,
    col_header = list(
      list(list(from = 2, to = 3, label = "Treatment", underline = TRUE)),
      c("Parameter", "Placebo", "Active")),
    col_spec = list(list(col = 1, align = "left")),
    col_rel_width = c(2, 1, 1),
    border = "tfl")
  expect_s3_class(tbl, "rtftable")
  span <- tbl$col_header[[1L]][[1L]]
  expect_identical(c(span$from, span$to, span$label), c(2L, 3L, "Treatment"))
  doc <- rtf_section(rtf_tables(rtf_document(), list(tbl)), page = 1,
                     secinfo = list(header = NULL, footer = NULL))
  d <- .g5_dir()
  f <- file.path(d, "t.rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  l <- readLines(f, warn = FALSE)
  expect_length(grep("\\sbkpage", l, fixed = TRUE), 1L)      # one page
  rows <- grep("\\trowd", l, fixed = TRUE, value = TRUE)
  expect_length(rows, 5L)                                    # spanner, labels, three rows
  label_x <- .g5_cellx(rows[2L])
  # the row-label column is twice as wide as an arm
  expect_identical(diff(c(0L, label_x))[1L], 2L * diff(c(0L, label_x))[2L])
  # the spanner is one cell over the two arm columns
  expect_identical(.g5_cellx(rows[1L]), label_x[c(1L, 3L)])
  expect_match(rows[1L], "\\ul Treatment\\ulnone \\cell\\row", fixed = TRUE)
})

test_that("FT-TB3279 [FR-TB576]: a dropped group_col hides the group_safe continuation marker", {
  gd <- data.frame(grp = c("G1", "G1", "G1", "G1", "G1", "G2"), lab = letters[1:6],
                   stringsAsFactors = FALSE)
  shown <- as_rtftables(gd, split = "group_safe", max_rows = 3, group_col = "grp",
                        min_group_rows = 0)
  expect_identical(shown[[2L]]$data$grp[1L], "G1 (Cont.)")      # on a printed group_col
  p <- as_rtftables(gd, split = "group_safe", max_rows = 3, group_col = "grp",
                    drop_cols = "grp", min_group_rows = 0)
  expect_length(p, 3L)
  for (pg in p) expect_identical(names(pg$data), "lab")
  expect_false(any(grepl("Cont.", unlist(lapply(p, function(pg) pg$data$lab)), fixed = TRUE)))
  expect_identical(p[[1L]]$data$lab, c("a", "b", "c"))
  expect_identical(p[[2L]]$data$lab, c("", "d", "e"))
})

test_that("FT-TB3280 [FR-TB577]: a gt spanner and pct widths are read", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 2)[, c("mpg", "cyl", "disp")])
  g <- gt::tab_spanner(g, label = "Engine", columns = c(cyl, disp))
  g <- gt::cols_width(g, mpg ~ gt::pct(10), cyl ~ gt::pct(30), disp ~ gt::pct(60))
  tbl <- as_rtftables(g)[[1L]]
  expect_length(tbl$col_header, 2L)
  spans <- tbl$col_header[[1L]]
  expect_identical(vapply(spans, function(z) c(z$from, z$to), integer(2L)),
                   matrix(c(1L, 1L, 2L, 3L), nrow = 2L))
  expect_identical(vapply(spans, function(z) z$label, ""), c("", "Engine"))
  expect_identical(tbl$col_header[[2L]], c("mpg", "cyl", "disp"))
  expect_identical(tbl$col_rel_width, c(10, 30, 60))
})

test_that("FT-TB3281 [FR-TB578]: gt italic, underline, align on spanners and labels carried", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 2)[, c("mpg", "cyl", "disp")])
  g <- gt::tab_spanner(g, label = "Engine", columns = c(cyl, disp))
  g <- gt::tab_style(g, gt::cell_text(style = "italic", align = "left", decorate = "underline"),
                     gt::cells_column_spanners(spanners = "Engine"))
  g <- gt::tab_style(g, gt::cell_text(style = "italic", weight = "bold", align = "right",
                                       decorate = "underline"),
                     gt::cells_column_labels(columns = cyl))
  tbl <- as_rtftables(g)[[1L]]
  sp <- tbl$col_header[[1L]][[2L]]
  expect_identical(sp$label, "Engine")
  expect_true(sp$italic)
  expect_true(sp$underline)
  expect_identical(sp$align, "left")
  lab <- tbl$col_header[[2L]][[2L]]
  expect_identical(lab$label, "cyl")
  expect_true(lab$italic)
  expect_true(lab$underline)
  expect_true(lab$bold)
  expect_identical(tbl$col_spec[[2L]]$header_align, "right")
  expect_true(tbl$col_spec[[2L]]$header_italic)
  expect_false(isTRUE(tbl$col_header[[2L]][[1L]]$italic))
})

test_that("FT-TB3282 [FR-TB578]: the other verbs override what was read from gt", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 2)[, c("mpg", "cyl")])
  g <- gt::tab_style(g, gt::cell_text(weight = "bold"), gt::cells_body(rows = 1, columns = mpg))
  g <- gt::tab_style(g, gt::cell_text(style = "italic", align = "right"),
                     gt::cells_column_labels(columns = cyl))
  g <- gt::cols_align(g, "right", columns = c(mpg, cyl))
  tbl <- as_rtftables(g)[[1L]]
  expect_identical(tbl$cell_styles[[1L]]$bold, c(TRUE, NA))
  expect_identical(style_body(tbl, rows = 1, cols = 1, bold = FALSE)$cell_styles[[1L]]$bold,
                   c(FALSE, NA))
  expect_identical(tbl$col_spec[[1L]]$align, "right")
  expect_identical(style_cols(tbl, cols = 1, align = "left")$col_spec[[1L]]$align, "left")
  expect_true(tbl$col_spec[[2L]]$header_italic)
  over <- style_cols(tbl, cols = "cyl", header_italic = FALSE, header_align = "left")
  expect_false(over$col_spec[[2L]]$header_italic)
  expect_identical(over$col_spec[[2L]]$header_align, "left")
})

test_that("FT-TB3283 [FR-TB579]: an rtables column split gives exact leaf labels and spanners", {
  skip_if_not_installed("rtables")
  skip_if_not_installed("formatters")
  set.seed(1)
  df <- data.frame(ARM = rep(c("A", "B"), each = 12), SEX = rep(c("F", "M"), 12),
                   RACE = rep(c("WHITE", "BLACK"), 12), AGE = rnorm(48, 40, 5))
  afun <- function(x) rtables::in_rows("Mean" = rtables::rcell(mean(x), format = "xx.x"))
  one <- rtables::analyze(rtables::split_cols_by(rtables::basic_table(), "ARM"), "AGE",
                          afun = afun)
  p1 <- as_rtftables(rtables::build_table(one, df))[[1L]]
  expect_identical(p1$col_header, list(c("", "A", "B")))
  nested <- rtables::analyze(
    rtables::split_rows_by(
      rtables::split_cols_by(rtables::split_cols_by(rtables::basic_table(), "ARM"), "SEX"),
      "RACE"), "AGE", afun = afun)
  p2 <- as_rtftables(rtables::build_table(nested, df))[[1L]]
  expect_length(p2$col_header, 2L)
  expect_identical(p2$col_header[[2L]], c("", "F", "M", "F", "M"))
  spans <- p2$col_header[[1L]]
  expect_identical(vapply(spans, function(z) z$label, ""), c("", "A", "B"))
  expect_identical(vapply(spans, function(z) z$from, integer(1L)), c(1L, 2L, 4L))
  expect_identical(vapply(spans, function(z) z$to, integer(1L)), c(1L, 3L, 5L))
})

test_that("FT-TB3284 [FR-TB584]: a gt label reads as text, <br> is a line break, fills dropped", {
  skip_if_not_installed("gt")
  d <- data.frame(a = 1, b = 2, c = 3)
  g <- gt::cols_label(gt::gt(d), a = gt::html("W<sub>t</sub>"), b = gt::html("A<br>B"),
                      c = gt::md("**Age**"))
  g <- gt::tab_style(g, list(gt::cell_fill(color = "#FF0000"),
                             gt::cell_text(font = "Arial", size = gt::px(20))),
                     gt::cells_body())
  tbl <- as_rtftables(g)[[1L]]
  expect_identical(unlist(tbl$col_header), c("Wt", "A\nB", "Age"))
  txt <- .g5_rtf(tbl)
  head_row <- grep("Wt\\cell", txt, fixed = TRUE, value = TRUE)
  expect_length(head_row, 1L)
  expect_match(head_row,
               "\\qr\\li0\\ri0 Wt\\cell\\qr\\li0\\ri0 A\\line B\\cell\\qr\\li0\\ri0 Age\\cell",
               fixed = TRUE)
  all_txt <- paste(txt, collapse = "\n")
  expect_false(grepl("\\red255\\green0\\blue0", all_txt, fixed = TRUE))   # the fill
  expect_false(grepl("Arial", all_txt, fixed = TRUE))                     # the font
  expect_false(grepl("\\fs30", all_txt, fixed = TRUE))                    # 20 px
  expect_false(grepl("cbpat", all_txt, fixed = TRUE))
})

test_that("FT-TB3285 [FR-TB585]: several group columns give one group row each, in gt's order", {
  skip_if_not_installed("gt")
  d <- data.frame(a = c("x", "y", "x", "y"), b = c("p", "p", "q", "p"), v = c(1.234, 2, 3, 4))
  g <- gt::fmt_number(gt::gt(d, groupname_col = c("a", "b")), v, decimals = 1)
  tbl <- as_rtftables(g)[[1L]]
  cells <- trimws(unlist(tbl$data, use.names = FALSE), whitespace = "[\\h\\v]")
  expect_identical(cells[grepl(" - ", cells, fixed = TRUE)], c("x - p", "y - p", "x - q"))
  expect_identical(cells, c("x - p", "1.2", "y - p", "2.0", "4.0", "x - q", "3.0"))
})

test_that("FT-TB3286 [FR-TB591]: the flat stub arguments were removed and are an error", {
  h <- data.frame(SOC = c("Gastro", "Gastro", "Nerv"), PT = c("Nausea", "Vomit", "Headache"),
                  N = c("1", "2", "3"), stringsAsFactors = FALSE)
  expect_error(as_rtftables(h, stub_indent = 2), "unused argument (stub_indent = 2)",
               fixed = TRUE)
  expect_error(as_rtftables(h, stub_group_summary = "empty"),
               "unused argument (stub_group_summary = \"empty\")", fixed = TRUE)
  expect_error(as_rtftables(h, stub_vars = "SOC"), "unused argument (stub_vars = \"SOC\")",
               fixed = TRUE)
  expect_error(as_rtftables(h, stub_label = "Term"), "unused argument (stub_label = \"Term\")",
               fixed = TRUE)
  # one column: kept as it is, renamed to the label
  p <- as_rtftables(h, stub = stub_spec(vars = "SOC", label = "Term"))[[1L]]
  expect_identical(names(p$data), c("Term", "PT", "N"))
  expect_identical(p$data$Term, h$SOC)
})

test_that("FT-TB3287 [FR-TB592]: table_align and cell_valign of length 0, 2, NULL, case stop", {
  d <- data.frame(Item = c("a", "b"), V = c("1", "2"), stringsAsFactors = FALSE)
  va <- "`cell_valign` must be 'top', 'center', or 'bottom'."
  ta <- "`table_align` must be 'left', 'center', or 'right'."
  for (bad in list(NULL, character(0), c("top", "bottom"), "Center", "cent", "TOP", "")) {
    expect_error(rtftable(d, cell_valign = bad), va, fixed = TRUE)
  }
  for (bad in list(NULL, character(0), c("left", "right"), "Center", "cent", "LEFT", "")) {
    expect_error(rtftable(d, table_align = bad), ta, fixed = TRUE)
  }
  for (ok in c("top", "center", "bottom")) {
    expect_s3_class(rtftable(d, cell_valign = ok), "rtftable")
  }
  for (ok in c("left", "center", "right")) {
    expect_s3_class(rtftable(d, table_align = ok), "rtftable")
  }
})

test_that("FT-TB3288 [FR-TB603]: rtf_tables(first_rows = ) not a whole number of rows stops", {
  d <- data.frame(Item = c("a", "b"), V = c("1", "2"), stringsAsFactors = FALSE)
  msg <- "rtf_tables(): `first_rows` is a whole number of rows, 1 or more."
  for (bad in list(Inf, 0, -1, 1.5, NA, "2", TRUE, c(1, 2), numeric(0))) {
    expect_error(rtf_tables(rtf_document(), d, page_break = FALSE, first_rows = bad), msg,
                 fixed = TRUE)
  }
  expect_error(rtf_tables(rtf_document(), list(rtftable(d)), page_break = FALSE,
                          first_rows = Inf), msg, fixed = TRUE)
  expect_error(as_rtftables(d, first_rows = Inf),
               "as_rtftables(): `first_rows` is a whole number of rows, 1 or more.", fixed = TRUE)
})

test_that("FT-TB3289 [FR-TB605]: a folding stub's indent must be a single non-negative integer", {
  h <- data.frame(SOC = c("Gastro", "Gastro", "Nerv"), PT = c("Nausea", "Vomit", "Headache"),
                  N = c("1", "2", "3"), stringsAsFactors = FALSE)
  msg <- "`indent` must be a single non-negative integer."
  for (bad in list(TRUE, 1.5, "2", NA, -1, c(1, 2))) {
    expect_error(stub_cols(h, vars = c("SOC", "PT"), indent = bad), msg, fixed = TRUE)
    expect_error(as_rtftables(h, stub = stub_spec(c("SOC", "PT"), indent = bad)), msg,
                 fixed = TRUE)
    # one column, nothing to fold: the argument is checked all the same
    expect_error(stub_cols(h, vars = "SOC", indent = bad), msg, fixed = TRUE)
  }
  expect_error(stub_cols(h, vars = "SOC", group_summary = 1),
               "`group_summary` must be a subset of", fixed = TRUE)
})

test_that("FT-TB3290 [FR-TB605]: a group_summary that is not a character vector stops", {
  h <- data.frame(SOC = c("Gastro", "Gastro", "Nerv"), PT = c("Nausea", "Vomit", "Headache"),
                  N = c("1", "2", "3"), stringsAsFactors = FALSE)
  head <- paste0("`group_summary` must be a subset of c(\"empty\", \"parent\") ",
                 "(or \"all\" / \"none\" / NULL); got: ")
  expect_error(stub_cols(h, vars = c("SOC", "PT"), group_summary = list("empty")),
               paste0(head, "a list"), fixed = TRUE)
  expect_error(stub_cols(h, vars = c("SOC", "PT"), group_summary = 1),
               paste0(head, "a numeric"), fixed = TRUE)
  expect_error(as_rtftables(h, stub = stub_spec(c("SOC", "PT"), group_summary = list("empty"))),
               paste0(head, "a list"), fixed = TRUE)
  expect_error(as_rtftables(h, stub = stub_spec(c("SOC", "PT"), group_summary = 1)),
               paste0(head, "a numeric"), fixed = TRUE)
  expect_error(stub_cols(h, vars = c("SOC", "PT"), group_summary = NA),
               "`group_summary` must be a subset of", fixed = TRUE)
  expect_error(stub_cols(h, vars = c("SOC", "PT"), group_summary = ""),
               "`group_summary` must be a subset of", fixed = TRUE)
})

test_that("FT-TB3291 [FR-TB606]: a column position is a whole number wherever a column is taken", {
  h <- data.frame(SOC = c("Gastro", "Gastro", "Nerv"), PT = c("Nausea", "Vomit", "Headache"),
                  N = c("1", "2", "3"), stringsAsFactors = FALSE)
  wide <- data.frame(P = "ALT", D1 = "1", D8 = "2", D15 = "3", stringsAsFactors = FALSE)
  num <- data.frame(g = c("a", "b"), v = c(1.234, 2.5), stringsAsFactors = FALSE)
  t1 <- rtftable(h)
  whole <- function(arg, shown, n) {
    paste0("`", arg, "` must be a column name or a whole position (1..", n, "), not ", shown, ".")
  }
  expect_error(as_rtftables(h, split = "by_value", page_by = 1.5),
               whole("page_by", "1.5", 3L), fixed = TRUE)
  expect_error(fmt_numeric(num, cols = 1.5), whole("fmt_numeric(cols)", "1.5", 2L), fixed = TRUE)
  expect_error(fmt_numeric(num, cols = 2, by = TRUE, formats = list(.default = 1)),
               whole("fmt_numeric(by)", "TRUE", 2L), fixed = TRUE)
  expect_error(fmt_numeric(num, cols = 2, by = 1.5, formats = list(.default = 1)),
               whole("fmt_numeric(by)", "1.5", 2L), fixed = TRUE)
  pages <- as_rtftables(wide)
  expect_error(paginate_cols(pages, cut_by = list(1.5)), whole("paginate_cols(cut_by)", "1.5", 4L),
               fixed = TRUE)
  expect_error(paginate_cols(pages, cut_by = list(TRUE)),
               whole("paginate_cols(cut_by)", "TRUE", 4L), fixed = TRUE)
  expect_error(paginate_cols(pages, at = 3, keep = 1.5),
               whole("paginate_cols(carry)", "1.5", 4L), fixed = TRUE)
  expect_error(paginate_cols(pages, at = 3, keep = TRUE),
               whole("paginate_cols(carry)", "TRUE", 4L), fixed = TRUE)
  expect_error(style_cols(t1, cols = TRUE, bold = TRUE), whole("style_cols(cols)", "TRUE", 3L),
               fixed = TRUE)
  expect_error(style_body(t1, cols = 1.5, bold = TRUE), whole("style_body(cols)", "1.5", 3L),
               fixed = TRUE)
  expect_error(style_header(t1, cols = TRUE, bold = TRUE),
               whole("style_header(cols)", "TRUE", 3L), fixed = TRUE)
  expect_error(set_decimal_split(t1, cols = 1.5), whole("set_decimal_split(cols)", "1.5", 3L),
               fixed = TRUE)
  # NA is out of range
  expect_error(style_cols(t1, cols = NA, bold = TRUE),
               "`style_cols(cols)` index NA out of range (1..3).", fixed = TRUE)
})

test_that("FT-TB3292 [FR-TB608]: by_value gathers a returning value and applies min_group_rows", {
  df <- data.frame(grp = c("A", "A", "A", "B", "B", "A", "A", "A", "B"),
                   lab = paste0("lab", 1:9), stringsAsFactors = FALSE)
  cut <- function(m) {
    as_rtftables(df, split = "by_value", max_rows = 4, min_group_rows = m, group_col = "grp")
  }
  # min_group_rows = 1: page 1 is full (four of the group's six rows)
  p1 <- cut(1)
  expect_identical(p1[[1L]]$data$lab, c("lab1", "lab2", "lab3", "lab6"))
  expect_identical(p1[[2L]]$data$lab, c("", "lab7", "lab8"))
  # min_group_rows = 3: the cut is pulled back so three rows go on to the next page
  p3 <- cut(3)
  expect_length(p3, 3L)
  expect_identical(p3[[1L]]$data$lab, c("lab1", "lab2", "lab3"))
  expect_identical(p3[[2L]]$data$grp[1L], "A (Cont.)")
  expect_identical(p3[[2L]]$data$lab, c("", "lab6", "lab7", "lab8"))
  expect_identical(p3[[3L]]$data$lab, c("lab4", "lab5", "lab9"))
  expect_identical(sub("[.]{3}[0-9]+$", "", names(p3)), c("A", "A", "B"))
})

test_that("FT-TB3293 [FR-TB625]: a huxtable markdown cell is read as exactly its text", {
  skip_if_not_installed("huxtable")
  d <- data.frame(a = c("text", "plain"), b = c("1", "2"), stringsAsFactors = FALSE)
  hx <- huxtable::set_markdown(huxtable::as_hux(d), 2L, 1L)
  p <- as_rtftables(hx)
  expect_identical(p[[1L]]$data$V1, c("text", "plain"))
  expect_identical(p[[1L]]$data$V2, c("1", "2"))
})

test_that("FT-TB3294 [FR-TB631]: two huxtable header rows are the spanner row and leaf labels", {
  skip_if_not_installed("huxtable")
  d <- data.frame(Param = c("Age", "Wt"), Mean = c("1", "2"), SD = c("3", "4"),
                  stringsAsFactors = FALSE)
  hx <- huxtable::as_hux(d, add_colnames = TRUE)
  hx <- huxtable::insert_row(hx, "", "Group", "", after = 0L)
  huxtable::colspan(hx)[1L, 2L] <- 2L
  huxtable::header_rows(hx)[1:2] <- TRUE
  p <- as_rtftables(hx)[[1L]]
  expect_length(p$col_header, 2L)
  expect_identical(p$col_header[[1L]],
                   list(list(from = 1L, to = 1L, label = ""),
                        list(from = 2L, to = 3L, label = "Group", align = "left")))
  expect_identical(p$col_header[[2L]], c("Param", "Mean", "SD"))
  expect_identical(p$data$V1, c("Age", "Wt"))
})
