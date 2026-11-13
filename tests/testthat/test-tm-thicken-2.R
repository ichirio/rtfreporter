# Thickening of the functional tests, batch 2: pagination and page layout
# (areas TB and DC), titles and footnotes.  Each block asserts what its FR
# states, the figures worked out in a comment.  FT ids from 5101 up.

# Pages as the rows of their bodies, without the paginate meta.
.t2_bodies <- function(pages) {
  lapply(pages, function(p) {
    d <- p$data
    attr(d, "rtf_paginate_meta") <- NULL
    d
  })
}

.t2_meta <- function(page) attr(page$data, "rtf_paginate_meta", exact = TRUE)

# The file a document makes, as lines.
.t2_lines <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

.t2_pc_df <- function() {
  data.frame(P = c("a", "b"), A_n = c("1", "2"), A_m = c("3", "4"), B_n = c("5", "6"),
             B_m = c("7", "8"), stringsAsFactors = FALSE)
}

test_that("FT-TB5101 [FR-TB121]: each split gives the pages the statement says", {
  d <- data.frame(grp = c(rep("A", 3), rep("B", 3)), v = as.character(1:6),
                  stringsAsFactors = FALSE)
  nr <- function(p) unname(vapply(p, function(x) nrow(x$data), integer(1)))
  # "none" is the default: one page, and max_rows is not checked
  expect_length(as_rtftables(d), 1L)
  expect_identical(nr(as_rtftables(d, max_rows = 2)), 6L)
  expect_identical(nr(as_rtftables(d, split = "none")), 6L)
  # "rows" cuts before each position of split_rows: before 3 and 5 -> 1:2, 3:4, 5:6
  r <- as_rtftables(d, split = "rows", split_rows = c(3, 5))
  expect_identical(nr(r), c(2L, 2L, 2L))
  expect_identical(r[[2L]]$data$v, c("3", "4"))
  # "group_safe" packs whole groups: with max_rows = 4 the groups of 3 do not share a page
  gs <- as_rtftables(d, split = "group_safe", max_rows = 4, group_col = "grp")
  expect_identical(nr(gs), c(3L, 3L))
  expect_identical(gs[[1L]]$data$grp, rep("A", 3L))
  # a group that alone exceeds max_rows is split, the next page opening with the label
  # (A has 6 rows, max_rows 4: 4 rows, then "A (Cont.)" and the last 2)
  big <- data.frame(grp = rep("A", 6), v = as.character(1:6), stringsAsFactors = FALSE)
  gb <- as_rtftables(big, split = "group_safe", max_rows = 4, group_col = "grp",
                     group_by = "value")
  expect_identical(nr(gb), c(4L, 3L))
  expect_identical(gb[[2L]]$data$grp, c("A (Cont.)", "A", "A"))
  expect_identical(gb[[2L]]$data$v, c("", "5", "6"))
  # "group_force" cuts every max_rows rows (the continuation row counted) and repeats the
  # group label: A A A B | B (Cont.) B B -> 4 rows, then the label row and 2 more
  # (min_group_rows = 0: a group's first row may be left alone at the foot of a page)
  gf <- as_rtftables(d, split = "group_force", max_rows = 4, group_col = "grp",
                     group_by = "value", min_group_rows = 0)
  expect_identical(nr(gf), c(4L, 3L))
  expect_identical(gf[[1L]]$data$grp, c("A", "A", "A", "B"))
  expect_identical(gf[[2L]]$data$grp, c("B (Cont.)", "B", "B"))
  # "by_value" makes one page per distinct value of group_col, named by it
  bv <- as_rtftables(d, split = "by_value", group_col = "grp")
  expect_identical(names(bv), c("A", "B"))
  expect_identical(nr(bv), c(3L, 3L))
  expect_identical(bv$B$data$v, c("4", "5", "6"))
})

test_that("FT-TB5102 [FR-TB131 FR-TB132]: page_by pages, their names, tails and meta", {
  s <- data.frame(period = c("P1", "P1", "P2", "P1"), param = c("ALT", "AST", "ALT", "ALT"),
                  v = as.character(1:4), stringsAsFactors = FALSE)
  # one page per value, named by it; P1's rows are scattered (1, 2, 4) and gathered in order
  p <- as_rtftables(s, page_by = "period")
  expect_identical(names(p), c("P1", "P2"))
  expect_identical(p$P1$data$v, c("1", "2", "4"))
  expect_identical(p$P2$data$v, "3")
  # NULL (the default): one page, no names
  expect_null(names(as_rtftables(s)))
  # two columns: the page is named by the pair
  two <- as_rtftables(s, page_by = c("period", "param"))
  expect_identical(names(two), c("P1, ALT", "P1, AST", "P2, ALT"))
  expect_identical(two[["P1, ALT"]]$data$v, c("1", "4"))
  # every other pagination setting applies within one partition: P1 has 3 rows, max_rows 2
  # -> pages "P1...1" (2 rows) and "P1...2" (1 row); P2 is used once and carries no tail
  cut <- as_rtftables(s, page_by = "period", split = "group_force", max_rows = 2,
                      group_col = "param", drop_cols = "period")
  expect_identical(names(cut), c("P1...1", "P1...2", "P2"))
  expect_identical(unname(vapply(cut, function(x) nrow(x$data), integer(1))), c(2L, 1L, 1L))
  # the meta of a page: page_by and page_values (the value, named by its column)
  m <- .t2_meta(cut[[2L]])
  expect_identical(m$page_by, "P1")
  expect_identical(m$page_values, c(period = "P1"))
  m2 <- .t2_meta(two[[3L]])
  expect_identical(m2$page_values, c(period = "P2", param = "ALT"))
  # with a value-based split inside, the page is named by its outermost value alone and the
  # page_by value stays in the meta (page_group / page_by)
  inner <- as_rtftables(s, page_by = "period", split = "by_value", group_col = "param")
  expect_identical(names(inner), c("ALT...1", "ALT...2", "AST"))
  mi <- .t2_meta(inner[[2L]])
  expect_identical(mi$page_group, "ALT")
  expect_identical(mi$page_by, "P2")
  # the tail is not part of the heading auto_title prints
  doc <- rtf_document() |>
    rtf_section(secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(cut, auto_title = TRUE)
  heads <- unname(vapply(doc$contents, function(ct) {
    t <- ct$title
    t[[length(t)]]$text
  }, character(1L)))
  expect_identical(heads, c("P1", "P1", "P2"))
})

test_that("FT-TB5103 [FR-TB142]: count_blank_rows makes max_rows the rows the page prints", {
  # three groups of three rows; between_groups puts a blank after row 3 and row 6
  df9 <- data.frame(label = c("Group A", "  a1", "  a2", "Group B", "  b1", "  b2",
                              "Group C", "  c1", "  c2"),
                    v = as.character(1:9), stringsAsFactors = FALSE)
  nr <- function(p) unname(vapply(p, function(x) nrow(x$data), integer(1)))
  # FALSE (the default) counts the input rows only: A and B (6 rows) fit max_rows = 6 and
  # print 7 with the blank between them
  off <- as_rtftables(df9, split = "group_safe", max_rows = 6, blank_rows = "between_groups")
  expect_identical(nr(off), c(6L, 3L))
  expect_identical(off[[1L]]$blank_rows, 3L)           # 6 rows + 1 blank = 7 > max_rows
  # TRUE: a page prints 3 + 1 + 3 = 7 rows, so max_rows = 6 holds one group per page ...
  on6 <- as_rtftables(df9, split = "group_safe", max_rows = 6, blank_rows = "between_groups",
                      count_blank_rows = TRUE)
  expect_identical(nr(on6), c(3L, 3L, 3L))
  # ... and max_rows = 7 holds two with their blank between
  on7 <- as_rtftables(df9, split = "group_safe", max_rows = 7, blank_rows = "between_groups",
                      count_blank_rows = TRUE)
  expect_identical(nr(on7), c(6L, 3L))
  expect_identical(on7[[1L]]$blank_rows, 3L)
  # blank_row_first / blank_row_end print a row each: two groups of 3 and both edges print 8
  edge <- data.frame(A = letters[1:6], .rec = rep(1:2, each = 3), stringsAsFactors = FALSE)
  pages <- function(mr, cnt) {
    as_rtftables(edge, split = "group_safe", max_rows = mr, group_col = ".rec",
                 group_by = "value", drop_cols = ".rec", blank_row_first = TRUE,
                 blank_row_end = TRUE, count_blank_rows = cnt)
  }
  expect_identical(nr(pages(7, FALSE)), 6L)             # the edges are not counted
  expect_identical(nr(pages(7, TRUE)), c(3L, 3L))       # 6 + 2 = 8 > 7
  expect_identical(nr(pages(8, TRUE)), 6L)              # 8 fit
  # an edge merges with a blank row it sits against: each group ends in a blank data row, so
  # blank_row_end costs nothing and only blank_row_first does: 6 + 1 = 7 fit max_rows = 7
  merged <- data.frame(A = c("a", "b", "", "c", "d", ""), .rec = rep(1:2, each = 3),
                       stringsAsFactors = FALSE)
  mp <- function(mr) {
    as_rtftables(merged, split = "group_safe", max_rows = mr, group_col = ".rec",
                 group_by = "value", drop_cols = ".rec", blank_row_first = TRUE,
                 blank_row_end = TRUE, count_blank_rows = TRUE)
  }
  expect_identical(nr(mp(7)), 6L)
  expect_identical(nr(mp(6)), c(3L, 3L))
})

test_that("FT-TB5104 [FR-TB151]: first_rows with page_by, a custom split or by_value stops with its message", {
  d <- data.frame(g = c("x", "x", "y", "y", "z"), k = 1:5, stringsAsFactors = FALSE)
  msg <- paste0("`first_rows` paginates one table by rows or groups: ",
                "not with `page_by` or a custom `split`.")
  expect_error(as_rtftables(d, page_by = "g", first_rows = 2), msg, fixed = TRUE)
  expect_error(as_rtftables(d, split = function(df, ...) list(df), first_rows = 2),
               msg, fixed = TRUE)
  expect_error(as_rtftables(d, split = "by_value", group_col = "g", first_rows = 2),
               "`first_rows` paginates by rows or groups: not split = \"by_value\".",
               fixed = TRUE)
})

test_that("FT-TB5105 [FR-TB284 FR-TB287 FR-TB288]: paginate_cols() stops with the message of a bad at, cut_by, fit, order or col_header", {
  tb <- rtftable(.t2_pc_df())
  # FR-TB284: at, cut_by
  expect_error(paginate_cols(tb, at = 1),
               "`at` must name columns 2..ncol -- there is nothing before column 1.",
               fixed = TRUE)
  expect_error(paginate_cols(tb, at = 9),
               "`paginate_cols(at)` index 9 out of range (1..5).", fixed = TRUE)
  bad <- paste0("`cut_by` must be a list of column blocks, a single separator found in the ",
                "column names, or one key per column (5).")
  expect_error(paginate_cols(tb, cut_by = c("a", "b")), bad, fixed = TRUE)
  expect_error(paginate_cols(tb, cut_by = 2:3), bad, fixed = TRUE)
  # FR-TB287: fit and order
  fit <- paste0("`paginate_cols(fit = )` is TRUE (page 1's scale on every block) or FALSE ",
                "(each column keeps its width).")
  expect_error(paginate_cols(tb, at = 2, fit = "nope"), fit, fixed = TRUE)
  expect_error(paginate_cols(tb, at = 2, fit = NA), fit, fixed = TRUE)
  expect_error(paginate_cols(tb, at = 2, order = "x"),
               paste0("`order`: \"x\" is not an axis.  Give the axes outermost first, from ",
                      "\"group\", \"rows\", \"cols\" -- or \"across\" / \"down\" for the two ",
                      "usual orders."), fixed = TRUE)
  expect_error(paginate_cols(tb, at = 2, order = c("cols", "cols")),
               "`order`: each axis may appear once.", fixed = TRUE)
  # FR-TB288: col_header
  expect_error(paginate_cols(tb, at = 3, col_header = "names"),
               paste0("`col_header = \"names\"` needs `cut_by` to be the separator in the ",
                      "column names (e.g. cut_by = \"____\")."), fixed = TRUE)
  expect_error(paginate_cols(tb, at = 3, col_header = c("a", "b")),
               "`paginate_cols(col_header)`: the label row has 2 labels but the table has 5 printed columns.",
               fixed = TRUE)
})

test_that("FT-TB5106 [FR-TB596]: max_rows = 1 gives pages of one row and no continuation row; max_rows at or above the rows gives one page", {
  d <- data.frame(g = c("A", "A", "B"), v = as.character(1:3), stringsAsFactors = FALSE)
  for (s in c("group_force", "group_safe")) {
    p <- as_rtftables(d, split = s, max_rows = 1, group_col = "g", group_by = "value")
    expect_length(p, 3L)
    expect_identical(unname(vapply(p, function(x) nrow(x$data), integer(1))), c(1L, 1L, 1L),
                     info = s)
    expect_identical(unlist(lapply(p, function(x) x$data$v)), c("1", "2", "3"), info = s)
    expect_identical(unlist(lapply(p, function(x) x$data$g)), c("A", "A", "B"), info = s)
  }
  bv <- as_rtftables(d, split = "by_value", max_rows = 1, group_col = "g")
  expect_identical(names(bv), c("A...1", "A...2", "B"))
  expect_identical(unname(unlist(lapply(bv, function(x) x$data$v))), c("1", "2", "3"))
  # max_rows equal to or above the number of rows: one page
  for (mr in c(3, 4, 100)) {
    expect_length(as_rtftables(d, split = "group_force", max_rows = mr, group_col = "g"), 1L)
    expect_length(as_rtftables(d, split = "group_safe", max_rows = mr, group_col = "g"), 1L)
  }
})

# ---- titles and footnotes ---------------------------------------------------

.t2_tbl <- function() rtftable(data.frame(a = c("x", "y"), stringsAsFactors = FALSE))

# The footnote rows of a document: its table rows holding `pat`.
.t2_fn <- function(..., pat = "FN") {
  doc <- rtf_document() |> rtf_tables(.t2_tbl()) |> rtf_footnotes(...)
  l <- .t2_lines(doc)
  grep(pat, l, value = TRUE)
}
.t2_cells <- function(r) lengths(regmatches(r, gregexpr("\\cellx", r, fixed = TRUE)))
.t2_align <- function(r) unique(unlist(regmatches(r, gregexpr("\\\\q[lrc]", r))))
# the cell sides a row draws a border on: clbrdr<side>
.t2_sides <- function(r) {
  vapply(regmatches(r, gregexpr("\\\\clbrdr[tblr]", r)), function(m) {
    paste(sub("\\clbrdr", "", m, fixed = TRUE), collapse = "")
  }, character(1L))
}

test_that("FT-DC5101 [FR-DC206]: a footnote row is a string or c(l, c, r) cells; align picks the slot of a bare string; the row's own slot wins; a blank string is a row", {
  # a bare string: one cell, left by default
  r <- .t2_fn(list(c("FN one", "FN two")))
  expect_identical(.t2_cells(r), c(1L, 1L))
  expect_identical(.t2_align(r), "\\ql")
  # c(l = , r = ): two cells, left and right; c(l, c, r): three, left, centre and right
  r2 <- .t2_fn(list(list(c(l = "FN a", r = "FN b"))))
  expect_identical(.t2_cells(r2), 2L)
  expect_identical(regmatches(r2, gregexpr("\\\\q[lrc]", r2))[[1L]], c("\\ql", "\\qr"))
  r3 <- .t2_fn(list(list(c(l = "FN a", c = "FN b", r = "FN c"))))
  expect_identical(.t2_cells(r3), 3L)
  expect_identical(regmatches(r3, gregexpr("\\\\q[lrc]", r3))[[1L]], c("\\ql", "\\qc", "\\qr"))
  # a bare string lands in the slot that align selects
  want <- c(left = "\\ql", center = "\\qc", right = "\\qr")
  for (al in names(want)) {
    expect_identical(.t2_align(.t2_fn(list(c("FN one")), align = al)), unname(want[al]))
  }
  # a row's own slot beats the block align: c(r = ) stays right under align = "center"
  expect_identical(.t2_align(.t2_fn(list(list(c(r = "FN one"))), align = "center")), "\\qr")
  # a blank string is a row of its own: the row after "FN one" is an empty left cell
  doc <- rtf_document() |> rtf_tables(.t2_tbl()) |> rtf_footnotes(list(c("FN one", "", "FN two")))
  l <- .t2_lines(doc)
  i <- grep("FN one", l)
  expect_true(grepl("\\\\cellx[0-9]+\\\\ql\\\\li0\\\\ri0 \\\\cell", l[i + 1L]))
  expect_true(grepl("FN two", l[i + 2L], fixed = TRUE))
})

test_that("FT-DC5102 [FR-DC207]: rtf_footnotes(border = ) NULL is no rule; top is the rule on the first row, bottom below the last", {
  rows <- list(c("FN one", "FN two", "FN three"))
  # NULL: no border on any row
  none <- .t2_fn(rows)
  expect_length(none, 3L)
  expect_false(any(grepl("clbrdr", none, fixed = TRUE)))
  # top = TRUE: the separator, on the first row only
  top <- .t2_fn(rows, border = rtf_border(top = TRUE))
  expect_identical(.t2_sides(top), c("t", "", ""))
  # bottom = TRUE: below the last row only
  bot <- .t2_fn(rows, border = rtf_border(bottom = TRUE))
  expect_identical(.t2_sides(bot), c("", "", "b"))
  # inside_h: between rows, so not on the last
  ih <- .t2_fn(rows, border = rtf_border(inside_h = TRUE))
  expect_identical(.t2_sides(ih), c("b", "b", ""))
  # left / right on every row's outer edges
  lr <- .t2_fn(rows, border = rtf_border(left = TRUE, right = TRUE))
  expect_identical(.t2_sides(lr), rep("lr", 3L))
  # a bad border stops
  d <- rtf_document() |> rtf_tables(.t2_tbl())
  expect_error(rtf_footnotes(d, list("x"), border = "top"), "rtf_border")
})

test_that("FT-DC5103 [FR-DC218]: a footnote row named other than l / c / r, or a per-row style list, stops at render with the message", {
  doc <- function(rows) rtf_document() |> rtf_tables(.t2_tbl()) |> rtf_footnotes(list(rows))
  expect_error(.t2_lines(doc(list(c(x = "FN one")))),
               "A footnote row may only be named l / c / r; got 'x'.", fixed = TRUE)
  expect_error(.t2_lines(doc(list(c(left = "FN one")))),
               "A footnote row may only be named l / c / r; got 'left'.", fixed = TRUE)
  expect_error(
    .t2_lines(doc(list(list(text = "x", bold = TRUE)))),
    paste0("A footnote row is a string or a named vector such as c(l = \"...\", r = \"...\"). ",
           "Per-row styling lists are no longer supported -- set the style on the block ",
           "instead, via rtf_footnotes(font_size_half_points = , align = , ...)."),
    fixed = TRUE)
})

test_that("FT-DC5104 [FR-DC189 FR-TB172]: a title block is a character vector or rows with a style; a blank string is a blank row; rows default to centred and bold; rtf_tables(titles = ) takes one block or one each", {
  tb <- .t2_tbl()
  # a character vector, one entry per row: centred and bold, the blank string a blank row
  l <- .t2_lines(rtf_document() |> rtf_tables(tb) |> rtf_titles(list(c("T1", "", "T2"))))
  i <- grep("T1", l)
  expect_identical(l[i], "\\pard\\qc\\li0\\ri0 \\b T1\\b0 \\par")
  expect_identical(l[i + 1L], "\\pard\\qc\\li0\\ri0\\par")
  expect_identical(l[i + 2L], "\\pard\\qc\\li0\\ri0 \\b T2\\b0 \\par")
  # a list of rows: a string, and a styled row (right, not bold, italic, underlined, coloured)
  l2 <- .t2_lines(rtf_document() |> rtf_tables(tb) |> rtf_titles(list(list(
    "S1", list(text = "S2", align = "right", bold = FALSE, italic = TRUE, underline = TRUE,
               color = "#C00000")))))
  expect_true("\\pard\\qc\\li0\\ri0 \\b S1\\b0 \\par" %in% l2)
  expect_true(any(grepl("^\\\\pard\\\\qr\\\\li0\\\\ri0 \\\\cf[3-9] \\\\i \\\\ul S2\\\\ulnone \\\\i0 \\\\cf1 \\\\par$", l2)))
  expect_false(any(grepl("\\b S2", l2, fixed = TRUE)))
  # a row's border (the table form): on that row only
  lt <- .t2_lines(rtf_document(style = rtf_style(title_format = "table")) |>
                    rtf_tables(tb) |>
                    rtf_titles(list(list(list(text = "TB", border = rtf_border(bottom = TRUE)),
                                         "TP"))))
  expect_identical(.t2_sides(grep("TB", lt, value = TRUE)), "b")
  expect_identical(.t2_sides(grep("TP", lt, value = TRUE)), "")
  # rtf_tables(titles = ): one block for every page, or one each
  two <- rtf_tables(rtf_document(), list(tb, tb), titles = list(list("A"), list("B")))
  expect_identical(two$contents[[1L]]$title, list("A"))
  expect_identical(two$contents[[2L]]$title, list("B"))
  one <- rtf_tables(rtf_document(), list(tb, tb), titles = list(c("Same", "Too")))
  expect_identical(one$contents[[1L]]$title, c("Same", "Too"))
  expect_identical(one$contents[[2L]]$title, c("Same", "Too"))
  expect_null(rtf_tables(rtf_document(), list(tb, tb))$contents[[1L]]$title)
  # the styled row and the blank row reach the file through rtf_tables(titles = ) too
  lr <- .t2_lines(rtf_tables(rtf_document(), list(tb), titles = list(list(
    "", list(text = "x", align = "left", bold = FALSE)))))
  expect_true("\\pard\\qc\\li0\\ri0\\par" %in% lr)
  expect_true("\\pard\\ql\\li0\\ri0 x\\par" %in% lr)
})

test_that("FT-DC5105 [FR-DC192]: rtf_titles(font_size_half_points = ) overrides the document's titles style, then its shared size; NULL inherits", {
  tb <- .t2_tbl()
  line <- function(doc, pat = "TX") grep(pat, .t2_lines(doc), value = TRUE)
  # the shared level only (rtf_style(font_size_half_points = 30)): the block's own size wins
  shared <- rtf_document(style = rtf_style(font_size_half_points = 30L)) |> rtf_tables(tb)
  expect_identical(line(rtf_titles(shared, list("TX"), font_size_half_points = 20L)),
                   "\\pard\\qc\\li0\\ri0\\fs20 \\b TX\\b0 \\par")
  # the document's titles section (26) is in force over the shared size (30) ...
  both <- rtf_document(style = rtf_style(titles = list(font_size_half_points = 26L),
                                         font_size_half_points = 30L)) |>
    rtf_tables(tb)
  expect_identical(line(rtf_titles(both, list("TX"))),
                   "\\pard\\qc\\li0\\ri0\\fs26 \\b TX\\b0 \\par")
  # ... and the block's own size (20) over both
  expect_identical(line(rtf_titles(both, list("TX"), font_size_half_points = 20L)),
                   "\\pard\\qc\\li0\\ri0\\fs20 \\b TX\\b0 \\par")
  # NULL inherits the shared size, which is then the document's: no \fs on the paragraph
  expect_identical(line(rtf_titles(shared, list("TX"))), "\\pard\\qc\\li0\\ri0 \\b TX\\b0 \\par")
  expect_true(grepl("\\widowctrl\\fs30", paste(.t2_lines(rtf_titles(shared, list("TX"))),
                                              collapse = "\n"), fixed = TRUE))
})

test_that("FT-DC5106 [FR-DC364]: a header, footer or footnote row's border that is not NULL or an rtf_border() stops, naming the row", {
  expect_error(
    rtf_header(list(c(l = "A"), list(columns = c(l = "B"), border = "bottom"))),
    "`rtf_header()` row 2's `border` must be NULL or an rtf_border() object.", fixed = TRUE)
  expect_error(
    rtf_footer(list(c(l = "A"), c(l = "B"), list(columns = c(l = "C"), border = 1))),
    "`rtf_footer()` row 3's `border` must be NULL or an rtf_border() object.", fixed = TRUE)
  # a footnote row's border is checked when the file is written
  doc <- rtf_document() |>
    rtf_tables(.t2_tbl()) |>
    rtf_footnotes(list(list(list(columns = "x", border = 1))))
  expect_error(.t2_lines(doc),
               "`rtf_footnotes()` row 1's `border` must be NULL or an rtf_border() object.",
               fixed = TRUE)
  # NULL and an rtf_border() are accepted
  expect_no_error(rtf_header(list(list(columns = c(l = "A"), border = NULL),
                                  list(columns = c(l = "B"), border = rtf_border(bottom = TRUE)))))
})

test_that("FT-TB5107 [FR-TB261]: add_cont_label() appends \" (Cont.)\" as the built-in strategies do, in the column col", {
  x <- data.frame(g = rep("A", 6), v = as.character(1:6), stringsAsFactors = FALSE)
  # the default suffix is what group_force writes on the next page
  built <- as_rtftables(x, split = "group_force", max_rows = 4, group_col = "g",
                        group_by = "value")[[2L]]$data
  expect_identical(built$g[1L], "A (Cont.)")
  mine <- add_cont_label(x[5:6, ], "A")
  expect_identical(mine$g[1L], "A (Cont.)")
  expect_identical(mine$v[1L], "")
  expect_identical(mine$g, built$g)
  expect_identical(mine$v, built$v)
  # cont_label is the suffix; col the column, by name or position (the first by default)
  expect_identical(add_cont_label(x[5:6, ], "A", cont_label = " cont")$g[1L], "A cont")
  by_pos <- add_cont_label(x[5:6, ], "A", col = 2L)
  by_name <- add_cont_label(x[5:6, ], "A", col = "v")
  expect_identical(by_pos, by_name)
  expect_identical(by_pos$v[1L], "A (Cont.)")
  expect_identical(by_pos$g[1L], "")
})

test_that("FT-TB5108 [FR-TB280]: paginate_cols() keeps the page names, numbering repeats with a \"...n\" tail; an unnamed list stays unnamed", {
  d <- data.frame(P = letters[1:6], A_n = "1", A_m = "2", B_n = "3", B_m = "4",
                  stringsAsFactors = FALSE)
  pg <- list(g1 = rtftable(d[1:3, ]), g2 = rtftable(d[4:6, ]))
  # each column page keeps its row page's name, numbered by column block
  down <- paginate_cols(pg, at = 4, fit = FALSE, order = "down")
  expect_identical(names(down), c("g1...1", "g1...2", "g2...1", "g2...2"))
  across <- paginate_cols(pg, at = 4, fit = FALSE)
  expect_identical(names(across), c("g1...1", "g2...1", "g1...2", "g2...2"))
  # so the list stays addressable by name: g1's second block holds the B columns
  expect_identical(names(down[["g1...2"]]$data), c("P", "B_n", "B_m"))
  expect_identical(names(down[["g2...1"]]$data), c("P", "A_n", "A_m"))
  # one page cut in two: its name numbered
  expect_identical(names(paginate_cols(pg["g1"], at = 4, fit = FALSE)), c("g1...1", "g1...2"))
  # an unnamed list stays unnamed
  expect_null(names(paginate_cols(unname(pg), at = 4, fit = FALSE)))
})
