# Thickening of the functional tests, batch 13: the last part-only FRs.  Each
# block asserts every clause of its FR through the public functions, the
# figures worked out in a comment.  FT ids from 6801 up.

.t13_bs <- intToUtf8(92L)

.t13_render <- function(doc, ...) {
  d <- tempfile("t13-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "out.rtf")
  generate_rtfreport(doc, f, overwrite = TRUE, ...)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}
.t13_count <- function(txt, what) {
  lengths(regmatches(txt, gregexpr(what, txt, fixed = TRUE)))
}

# ------------------------------------------------------------------- IX

.t13_wdf <- function() {
  data.frame(A = c("aaaa", "bb"), B = c("cccccccc", "d"),
             C = c("ee", "ffff"), stringsAsFactors = FALSE)
}

# The per-column widths of the widest row of the table as rendered on a
# landscape Letter page with the default 0.75in margins: (11 - 1.5) * 1440 =
# 13680 twips writable.
.t13_widths <- function(tbl) {
  doc <- rtf_document(page = list(orientation = "landscape")) |>
    rtf_tables(list(tbl))
  txt <- .t13_render(doc)
  rows <- strsplit(txt, paste0(.t13_bs, "trowd"), fixed = TRUE)[[1L]]
  cand <- lapply(rows, function(r) {
    as.integer(sub(".*cellx", "",
                   regmatches(r, gregexpr("\\\\cellx[0-9]+", r))[[1L]]))
  })
  diff(c(0L, cand[[which.max(vapply(cand, length, integer(1L)))]]))
}

test_that("FT-IX6801 [FR-IX319]: column_widths_twips wins outright and its sum is the width; col_rel_width apportions the total; table_width_twips beats table_width_pct; no width gives the writable page; an explicit col_rel_width beats auto_width", {
  writable <- 13680L
  # the same spellings through rtftable() and through as_rtftables()
  built <- list(
    rtftable = function(...) rtftable(.t13_wdf(), ...),
    as_rtftables = function(...) as_rtftables(.t13_wdf(), ...)[[1L]])
  for (nm in names(built)) {
    w <- function(...) .t13_widths(built[[nm]](...))
    lab <- function(s) paste(nm, s)
    # none of them: the writable page width
    expect_identical(sum(w()), writable, label = lab("none"))
    # column_widths_twips wins outright: the widths are the given ones, 1000 +
    # 2000 + 3000 = 6000 is the table's width whatever table_width_twips,
    # table_width_pct and col_rel_width say
    cw <- c(1000L, 2000L, 3000L)
    expect_identical(w(column_widths_twips = cw), cw, label = lab("column_widths"))
    expect_identical(w(column_widths_twips = cw, table_width_twips = 9000L,
                       table_width_pct = 50, col_rel_width = c(1, 1, 1)),
                     cw, label = lab("column_widths beats the rest"))
    # col_rel_width apportions whatever the total is: 3 : 2 : 1 of
    # 13680 (6840, 4560, 2280), of table_width_twips 6000 (3000, 2000, 1000),
    # of table_width_pct 50 = 6840 (3420, 2280, 1140)
    rel <- c(3, 2, 1)
    expect_identical(w(col_rel_width = rel), c(6840L, 4560L, 2280L), label = lab("rel"))
    expect_identical(w(col_rel_width = rel, table_width_twips = 6000L),
                     c(3000L, 2000L, 1000L), label = lab("rel in twips"))
    expect_identical(w(col_rel_width = rel, table_width_pct = 50),
                     c(3420L, 2280L, 1140L), label = lab("rel in pct"))
    # the totals alone
    expect_identical(sum(w(table_width_twips = 6000L)), 6000L, label = lab("twips"))
    expect_identical(sum(w(table_width_pct = 50)), 6840L, label = lab("pct"))
    # table_width_twips wins over table_width_pct: 6000, not 50 % of 13680
    expect_identical(w(table_width_twips = 6000L, table_width_pct = 50,
                       col_rel_width = rel),
                     c(3000L, 2000L, 1000L), label = lab("twips beats pct"))
    expect_identical(sum(w(table_width_twips = 6000L, table_width_pct = 50)), 6000L,
                     label = lab("twips beats pct, no rel"))
  }
  # an explicit col_rel_width wins over auto_width: the widths are the 3 : 2 : 1
  # shares of the page, not the content's; auto_width alone sizes to the content
  auto <- function(...) .t13_widths(as_rtftables(.t13_wdf(), auto_width = TRUE, ...)[[1L]])
  expect_identical(auto(col_rel_width = c(3, 2, 1)), c(6840L, 4560L, 2280L))
  expect_lt(sum(auto()), writable)
  # column_widths_twips beats auto_width as well
  expect_identical(auto(column_widths_twips = c(1000L, 2000L, 3000L)),
                   c(1000L, 2000L, 3000L))
})

.t13_lab <- function(periods = c("Period 1", "Period 2"),
                     params = c("ALT", "Bilirubin", "Haemoglobin")) {
  do.call(rbind, lapply(periods, function(per) {
    do.call(rbind, lapply(params, function(p) {
      d <- data.frame(period = per,
                      Parameter = c(p, "  n", "  Mean (SD)", "  Median"),
                      stringsAsFactors = FALSE)
      for (v in paste0("V", 1:4)) d[[v]] <- c("", "80", "50.0 (2.10)", "49.5")
      d
    }))
  }))
}

test_that("FT-IX6802 [FR-IX321]: page_by moves group_col to the first column outside it, and with split = \"by_value\" the group is the outer axis and page_by the inner one, a group's pages keeping its name alone", {
  lab <- .t13_lab()
  meta <- function(p) attr(p$data, "rtf_paginate_meta", exact = TRUE)
  first <- function(pg) unname(vapply(pg, function(p) p$data[[1L]][1L], ""))
  heading <- function(pg) sub("[.]{3}[0-9]+$", "", names(pg))
  # group_col left NULL: with page_by = "period" it is the first column not
  # named there, Parameter (column 2).  Each period is 3 blocks of 4 rows; with
  # max_rows = 8 two whole blocks fit a page and the third starts the next:
  # 8 + 4 rows a period, every page opening on a block label.
  pg <- as_rtftables(lab, page_by = "period", split = "group_safe",
                     group_by = "indent", max_rows = 8, drop_cols = "period")
  expect_identical(meta(pg[[1L]])$group_col, 2L)
  expect_identical(unname(vapply(pg, function(p) nrow(p$data), 1L)), c(8L, 4L, 8L, 4L))
  expect_identical(first(pg), c("ALT", "Haemoglobin", "ALT", "Haemoglobin"))
  expect_identical(heading(pg), c("Period 1", "Period 1", "Period 2", "Period 2"))
  # the first column outside page_by, not the first column: with the BY
  # column second, the group column is column 1 (Parameter); with the BY column
  # first, page_by = "Parameter" leaves column 1 (period) named, so column 2
  swapped <- lab[c("Parameter", "period", paste0("V", 1:4))]
  pg2 <- as_rtftables(swapped, page_by = "period", split = "group_safe",
                      group_by = "indent", max_rows = 8, drop_cols = "period")
  expect_identical(meta(pg2[[1L]])$group_col, 1L)
  expect_identical(first(pg2), c("ALT", "Haemoglobin", "ALT", "Haemoglobin"))
  pg3 <- as_rtftables(lab, page_by = "Parameter", split = "group_safe",
                      group_by = "indent", max_rows = 8)
  expect_identical(meta(pg3[[1L]])$group_col, 1L)
  # with split = "by_value" the group (Parameter, the default group_col) is the
  # outer axis and page_by (period) the inner one: ALT of Period 1, ALT of
  # Period 2, then Bilirubin ... ; 3 groups x 2 periods = 6 pages of 4 rows,
  # and a group's pages keep its name alone (the period is not joined to it)
  bv <- as_rtftables(lab, page_by = "period", split = "by_value",
                     group_by = "indent", drop_cols = "period")
  expect_length(bv, 6L)
  expect_identical(names(bv), c("ALT...1", "ALT...2", "Bilirubin...1", "Bilirubin...2",
                                "Haemoglobin...1", "Haemoglobin...2"))
  expect_identical(heading(bv), rep(c("ALT", "Bilirubin", "Haemoglobin"), each = 2L))
  expect_identical(unname(vapply(bv, function(p) nrow(p$data), 1L)), rep(4L, 6L))
  expect_identical(vapply(bv, function(p) meta(p)$page_group, ""),
                   setNames(rep(c("ALT", "Bilirubin", "Haemoglobin"), each = 2L), names(bv)))
  expect_identical(unname(vapply(bv, function(p) meta(p)$page_by, "")),
                   rep(c("Period 1", "Period 2"), 3L))
  expect_identical(meta(bv[[1L]])$group_col, 2L)
  # page_by = NULL changes nothing: by_value names the pages from group_col
  expect_identical(names(as_rtftables(lab, split = "by_value", group_col = "period",
                                      drop_cols = "period")),
                   c("Period 1", "Period 2"))
})

# The body of nine rows in three groups of three
.t13_nine <- function() {
  data.frame(G = rep(c("A", "B", "C"), each = 3L), N = as.character(1:9),
             stringsAsFactors = FALSE)
}

test_that("FT-IX6803 [FR-IX324]: with count_blank_rows a separator blank never lands on a page's first or last row, survives where a page holds two groups, and an explicit position is honoured wherever it lands", {
  d <- .t13_nine()
  pages <- function(blank, mr) {
    as_rtftables(d, split = "group_safe", max_rows = mr, group_col = "G",
                 blank_rows = blank, count_blank_rows = TRUE)
  }
  blanks <- function(p) lapply(p, function(x) x$blank_rows)
  rows <- function(p) vapply(p, function(x) nrow(x$data), 1L)
  total <- function(p) vapply(p, function(x) nrow(x$data) + length(x$blank_rows), 1L)
  for (sep in list("between_groups", blank_rows_by_change("G"))) {
    # a budget of 6: A + a separator + B is 7 rows, so a page holds one group
    # and there is no separator to place, at the top or the bottom
    one <- pages(sep, 6L)
    expect_identical(unname(rows(one)), c(3L, 3L, 3L))
    expect_identical(blanks(one), list(NULL, NULL, NULL))
    # a budget of 7: A, a separator, B (3 + 1 + 3) fit one page; the separator
    # after row 3 survives and none lands after row 6, its last row
    two <- pages(sep, 7L)
    expect_identical(unname(rows(two)), c(6L, 3L))
    expect_identical(blanks(two), list(3L, NULL))
    expect_true(all(total(two) <= 7L))
    # all three groups on one page (budget 12): separators after rows 3 and 6,
    # none after row 9, the last, and none before row 1
    all3 <- pages(sep, 12L)
    expect_identical(blanks(all3), list(c(3L, 6L)))
  }
  # an explicit position is honoured wherever it lands: after row 3 is the last
  # row of the first page (A + the blank = 4 rows), kept; the pages of B and C
  # have none; after rows 3, 6 and 9 every page's last row carries its blank
  expect_identical(blanks(pages(3, 4L)), list(3L, NULL, NULL))
  expect_identical(blanks(pages(c(3, 6, 9), 4L)), list(3L, 3L, 3L))
  expect_true(all(total(pages(c(3, 6, 9), 4L)) <= 4L))
  # a mixed spec: the separator half is trimmed at the edge, the explicit
  # position is kept (budget 7: B and C on one page, the separator after row 3)
  mixed <- pages(list("between_groups", 6), 7L)
  expect_identical(blanks(mixed), list(3L, NULL))
})

.t13_ds_rows <- function(txt) {
  ln <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  ln[startsWith(ln, paste0(.t13_bs, "trowd"))]
}
# The commands of each cell definition of a table row, in order: what sits
# before each \cellx, without the \trowd\trrh opening
.t13_defs <- function(row) {
  head <- sub("^(.*\\\\cellx[0-9]+)\\\\q[lcr].*$", "\\1", row)
  head <- sub("^\\\\trowd\\\\trrh[0-9]+", "", head)
  strsplit(head, "\\\\cellx[0-9]+")[[1L]]
}

test_that("FT-IX6804 [FR-IX330]: set_decimal_split() on a column with style_cols() / style_body() settings: per-cell alignment, border, fill and font follow onto both halves, the halves stay right and left, no interior rule or padding", {
  df <- data.frame(Statistic = c("n", "Mean", "Category"),
                   Value = c("12", "3.45", "n (%)"), stringsAsFactors = FALSE)
  vert <- rtf_border(left = rtf_border_line("single"), right = rtf_border_line("single"),
                     inside_v = rtf_border_line("single"))
  tbl <- rtftable(df, col_rel_width = c(3, 2), cell_padding_left_twips = 72L,
                  cell_padding_right_twips = 72L, font_size_half_points = 16L,
                  font = "Arial") |>
    style_zone(body = vert) |>
    style_cols(cols = "Value", align = "center", italic = TRUE,
               background = "#EFEFEF") |>
    style_body(rows = 2L, cols = "Value", bold = TRUE, color = "#FF0000",
               background = "#F8D7DA", align = "right",
               border = rtf_border(bottom = rtf_border_line("double"))) |>
    set_decimal_split(cols = "Value")
  rows <- .t13_ds_rows(.t13_render(rtf_document() |> rtf_tables(list(tbl))))
  expect_length(rows, 4L)                         # header + 3 body rows
  n_row <- rows[2L]
  mean_row <- rows[3L]
  cat_row <- rows[4L]
  count <- function(r, w) .t13_count(r, w)
  # n and Mean are split: three cells (label, left half, right half); Category
  # is free text, merged back into one cell, so two
  expect_identical(count(n_row, "cellx"), 3L)
  expect_identical(count(mean_row, "cellx"), 3L)
  expect_identical(count(cat_row, "cellx"), 2L)
  # the halves are right and left whatever the column's alignment (centre) and
  # the cell's own (right): the pair is never broken; the interior has no
  # padding (left half \ri0, right half \li0) and the outer edges keep 72
  for (r in list(n_row, mean_row)) {
    expect_true(grepl("\\\\qr\\\\li72\\\\ri0\\\\f", r))
    expect_true(grepl("\\\\ql\\\\li0\\\\ri72\\\\f", r))
  }
  # the merged cell keeps the column's alignment and its padding
  expect_true(grepl("\\\\qc\\\\li72\\\\ri72\\\\f", cat_row))
  # border: the rule between the two halves is suppressed, the outer ones kept
  # (left half: its left edge, no right edge; right half: the reverse)
  for (r in list(n_row, mean_row)) {
    defs <- .t13_defs(r)
    expect_length(defs, 3L)
    expect_true(grepl("clbrdrl", defs[2L], fixed = TRUE))
    expect_false(grepl("clbrdrr", defs[2L], fixed = TRUE))
    expect_false(grepl("clbrdrl", defs[3L], fixed = TRUE))
    expect_true(grepl("clbrdrr", defs[3L], fixed = TRUE))
  }
  # the cell's own border, a double rule under it, is on both halves; the
  # other rows have none
  bottom <- paste0(.t13_bs, "clbrdrb", .t13_bs, "brdrdb")
  expect_identical(count(mean_row, bottom), 2L)
  expect_identical(count(n_row, bottom), 0L)
  # fill: the column's on both halves of n, the cell's own on both halves of
  # Mean, a different one; the merged Category cell has the column's once
  shade <- function(r) regmatches(r, gregexpr("clcbpat[0-9]+", r))[[1L]]
  expect_length(shade(n_row), 2L)
  expect_length(unique(shade(n_row)), 1L)
  expect_length(shade(mean_row), 2L)
  expect_length(unique(shade(mean_row)), 1L)
  expect_false(identical(unique(shade(n_row)), unique(shade(mean_row))))
  expect_identical(shade(cat_row), shade(n_row)[1L])
  # font: the cell's bold and colour and the column's italic are on both
  # halves of Mean, the column's italic alone on both halves of n
  halves <- function(r) {
    strsplit(sub("^.*\\\\cellx[0-9]+", "", r), paste0(.t13_bs, "cell"), fixed = TRUE)[[1L]][2:3]
  }
  for (h in halves(mean_row)) {
    expect_true(grepl(paste0(.t13_bs, "b "), h, fixed = TRUE))
    expect_true(grepl(paste0(.t13_bs, "i "), h, fixed = TRUE))
    expect_true(grepl(paste0(.t13_bs, "cf"), h, fixed = TRUE))
  }
  for (h in halves(n_row)) {
    expect_true(grepl(paste0(.t13_bs, "i "), h, fixed = TRUE))
    expect_false(grepl(paste0(.t13_bs, "b "), h, fixed = TRUE))
  }
  # the table's own font and size are on every cell of a split row (3 cells)
  fsw <- regmatches(rows[1L], regexpr("\\\\f[0-9]+\\\\fs16", rows[1L]))
  expect_length(fsw, 1L)
  expect_identical(count(n_row, paste0(fsw, " ")), 3L)
  expect_identical(count(mean_row, paste0(fsw, " ")), 3L)
})

# One cell definition as written for the sides given: a side is c(style, width)
.t13_def <- function(top = NULL, bottom = NULL, left = NULL, right = NULL) {
  one <- function(side, s) {
    if (is.null(s)) "" else sprintf("\\clbrdr%s\\brdr%s\\brdrw%s", side, s[1L], s[2L])
  }
  paste0(one("t", top), one("b", bottom), one("l", left), one("r", right), "\\clvertalb")
}

test_that("FT-IX6805 [FR-IX338]: one rtf_border() is the whole table at rtftable(border = ), one kind of row at style_zone(), one cell at col_cell(border = ): the edges are the selection's outer frame, inside_h and inside_v the rules between its rows and cells", {
  df <- data.frame(a = c("r1", "r2"), b = c("x", "y"), c = c("p", "q"),
                   stringsAsFactors = FALSE)
  # top single, bottom double, left dot, right dash, a thick rule between the
  # rows (inside_h) and a single rule of width 30 between the cells (inside_v)
  b <- rtf_border(top = "single", bottom = "double", left = "dot", right = "dash",
                  inside_h = "thick", inside_v = rtf_border_line("single", 30L))
  top <- c("s", 15)
  dbl <- c("db", 15)
  thick <- c("th", 15)
  left <- c("dot", 15)
  right <- c("dash", 15)
  v30 <- c("s", 30)
  defs_of <- function(tbl) {
    rows <- .t13_ds_rows(.t13_render(rtf_document() |> rtf_tables(list(tbl))))
    lapply(rows, .t13_defs)
  }
  # the whole table: header, r1 and r2 are its three rows.  The top edge is
  # the header's top, the bottom edge the last row's bottom, left and right the
  # outer cells' sides; the thick rule is under the header and under r1 (the
  # rows' bottoms); the width-30 rule is the right of cell 1 and the left of
  # cells 2 and 3, and the rule between cells 1/2 is not at the outer edges
  tab <- defs_of(rtftable(df, border = b))
  expect_identical(tab, list(
    c(.t13_def(top, thick, left, v30), .t13_def(top, thick, v30, v30),
      .t13_def(top, thick, v30, right)),
    c(.t13_def(NULL, thick, left, v30), .t13_def(NULL, thick, v30, v30),
      .t13_def(NULL, thick, v30, right)),
    c(.t13_def(NULL, dbl, left, v30), .t13_def(NULL, dbl, v30, v30),
      .t13_def(NULL, dbl, v30, right))))
  # one kind of row: style_zone(body = b) on a table with no border is the body's
  # two rows alone; the same edges and rules, the header untouched
  zone <- defs_of(rtftable(df, border = "none") |> style_zone(body = b))
  expect_identical(zone, list(
    rep(.t13_def(), 3L),
    c(.t13_def(top, thick, left, v30), .t13_def(top, thick, v30, v30),
      .t13_def(top, thick, v30, right)),
    c(.t13_def(NULL, dbl, left, v30), .t13_def(NULL, dbl, v30, v30),
      .t13_def(NULL, dbl, v30, right))))
  # one cell: col_cell(border = b) over columns 2 and 3 of the header row is the
  # four outer edges of that one cell; a cell has no inside, so the thick and
  # the width-30 rules are not drawn; nothing else has a border
  cell <- defs_of(rtftable(df, border = "none", col_header = list(
    list(col_cell(1L, ""), col_cell(2:3, "SPAN", border = b)), c("a", "b", "c"))))
  expect_identical(cell[[1L]], c(.t13_def(), .t13_def(top, dbl, left, right)))
  expect_identical(cell[-1L], rep(list(rep(.t13_def(), 3L)), 3L))
})

# ------------------------------------------------------------------- TB

.t13_items <- function(n) {
  data.frame(Item = sprintf("Row %02d", seq_len(n)), Value = as.character(seq_len(n)),
             stringsAsFactors = FALSE)
}

test_that("FT-TB6801 [FR-TB184]: rtf_tables(first_rows = ) is NULL by default; with page_break = FALSE it puts that many rows of the one table on the shared page and paginates the rest as the table says, each page with its title, header and footnote, for a table_plan, a data frame and an rtftable", {
  expect_null(formals(rtf_tables)$first_rows)
  base <- rtf_tables(rtf_document(), list(.t13_items(3L)), titles = list("Table 1"))
  hdr <- paste0("Item", .t13_bs, "cell")
  page <- paste0(.t13_bs, "page")
  # the text of each page: the document cut at \page
  pages_of <- function(doc) strsplit(.t13_render(doc), page, fixed = TRUE)[[1L]]
  has <- function(p, what) .t13_count(p, what) > 0L
  rows_on <- function(p) {
    sprintf("Row %02d", 1:10)[vapply(sprintf("Row %02d", 1:10), function(r) has(p, r), NA)]
  }
  make <- function(tables, ...) {
    rtf_tables(base, tables, titles = list("Table 2"), footnotes = list("Source note"),
               page_break = FALSE, ...)
  }
  # the table of ten rows; first_rows = 4: Table 1 and the first 4 rows share
  # page 1; a table_plan's rest is cut by its own max_rows = 3 (6 rows: two
  # pages of 3); a data frame's and an rtftable's rest is one page of 6
  plan <- table_plan(.t13_items(10L)) |> plan_paginate_rows(max_rows = 3L, split = "group_force")
  pl <- pages_of(make(plan, first_rows = 4L))
  expect_length(pl, 3L)
  expect_identical(rows_on(pl[[1L]]), sprintf("Row %02d", 1:4))
  expect_identical(rows_on(pl[[2L]]), sprintf("Row %02d", 5:7))
  expect_identical(rows_on(pl[[3L]]), sprintf("Row %02d", 8:10))
  expect_true(has(pl[[1L]], "Table 1") && has(pl[[1L]], "Table 2"))
  for (p in pl[-1L]) expect_false(has(p, "Table 1"))
  for (p in pl) {
    expect_true(has(p, "Table 2"))           # the title, on the shared page and each later one
    expect_true(has(p, "Source note"))       # the footnote
  }
  # the column header: Table 1's, the shared page's and each later page's
  expect_identical(.t13_count(paste(pl, collapse = page), hdr), 4L)
  for (p in pl[-1L]) expect_identical(.t13_count(p, hdr), 1L)
  for (src in list(.t13_items(10L), rtftable(.t13_items(10L)))) {
    pg <- pages_of(make(src, first_rows = 4L))
    expect_length(pg, 2L)
    expect_identical(rows_on(pg[[1L]]), sprintf("Row %02d", 1:4))
    expect_identical(rows_on(pg[[2L]]), sprintf("Row %02d", 5:10))
    for (p in pg) {
      expect_true(has(p, "Table 2"))
      expect_true(has(p, "Source note"))
    }
    expect_identical(.t13_count(paste(pg, collapse = page), hdr), 3L)
    expect_identical(.t13_count(pg[[2L]], hdr), 1L)
  }
  # NULL: the whole table goes on the shared page, one page
  whole <- pages_of(make(.t13_items(10L)))
  expect_length(whole, 1L)
  expect_identical(rows_on(whole[[1L]]), sprintf("Row %02d", 1:10))
  # a table that is already pages is refused, and first_rows needs page_break = FALSE
  expect_error(rtf_tables(base, .t13_items(10L), first_rows = 4L), "page_break = FALSE")
  expect_error(make(as_rtftables(.t13_items(10L), split = "rows", split_rows = 5L),
                    first_rows = 4L), "as_rtftables(first_rows = )", fixed = TRUE)
})

# ------------------------------------------------------------------- DC

.t13_mark <- c(table = "TBL", titles = "TTL", footnotes = "FNT", header = "HDR",
               footer = "FTR")
.t13_text <- function(m) paste(.t13_mark[[m]], ">= 1^{2}")
# the five keys with an option, the modules each reaches here, and the value
# each source gives (the resource baseline's is read from the output: the
# size 18 half-points, the height 230 twips, no padding, markup "script")
.t13_keys <- list(
  font_size_half_points = list(
    mods = names(.t13_mark),
    val = c(arg = 30L, section = 28L, shared = 26L, option = 22L, baseline = 18L)),
  row_height_twips = list(
    mods = c("table", "header", "footer"),
    val = c(arg = 410L, section = 420L, shared = 430L, option = 450L, baseline = 230L)),
  cell_padding_left_twips = list(
    mods = c("table", "header", "footer"),
    val = c(arg = 55L, section = 66L, shared = 44L, option = 77L, baseline = 0L)),
  cell_padding_right_twips = list(
    mods = c("table", "header", "footer"),
    val = c(arg = 35L, section = 36L, shared = 34L, option = 37L, baseline = 0L)),
  markup = list(mods = names(.t13_mark), val = NULL))

# the value source `s` gives for `key` when `winner` is the highest source set:
# for markup the winner "relational", the others "none", the baseline "script"
.t13_value <- function(key, s, winner) {
  if (!is.null(.t13_keys[[key]]$val)) return(.t13_keys[[key]]$val[[s]])
  if (identical(s, "baseline")) "script" else if (identical(s, winner)) "relational" else "none"
}

# The table of the document: its own argument by the routes the statement
# names -- rtftable(), its rtf_table_style(), as_rtftables()
.t13_table <- function(args, route) {
  d <- data.frame(A = c(.t13_text("table"), "a2"), B = c("b1", "b2"))
  switch(route,
         rtftable = do.call(rtftable, c(list(d), args)),
         style = rtftable(d, style = do.call(rtf_table_style, args)),
         as_rtftables = do.call(as_rtftables, c(list(d), args))[[1L]])
}

.t13_doc <- function(own, style, route) {
  o <- function(m) if (is.null(own[[m]])) list() else own[[m]]
  doc <- rtf_document(style = style) |>
    rtf_section(page = 1, secinfo = list(
      header = do.call(rtf_header, c(list(c(l = .t13_text("header"))), o("header"))),
      footer = do.call(rtf_footer, c(list(c(l = .t13_text("footer"))), o("footer"))))) |>
    rtf_tables(list(.t13_table(o("table"), route)))
  doc <- do.call(rtf_titles, c(list(doc, .t13_text("titles")), o("titles")))
  do.call(rtf_footnotes, c(list(doc, .t13_text("footnotes")), o("footnotes")))
}

# What a module shows for a key: the last \fs (else the document's), \trrh,
# \li or \ri before its text, or how its ">= 1^{2}" is written after it
.t13_observe <- function(txt, key, m) {
  ln <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  l <- ln[grepl(paste0(.t13_mark[[m]], " "), ln, fixed = TRUE)][1L]
  at <- regexpr(paste0(.t13_mark[[m]], " "), l, fixed = TRUE)
  before <- substr(l, 1L, at - 1L)
  after <- substr(l, at, nchar(l))
  last <- function(x, word) {
    hit <- regmatches(x, gregexpr(paste0("\\\\", word, "[0-9]+"), x))[[1L]]
    if (!length(hit)) return(NA_integer_)
    as.integer(sub(paste0("\\\\", word), "", hit[length(hit)]))
  }
  switch(key,
         font_size_half_points = {
           fs <- last(before, "fs")
           if (is.na(fs)) last(regmatches(txt, regexpr("\\\\widowctrl\\\\fs[0-9]+", txt)), "fs") else fs
         },
         row_height_twips = last(before, "trrh"),
         cell_padding_left_twips = last(before, "li"),
         cell_padding_right_twips = last(before, "ri"),
         markup = {
           rel <- grepl("\\u8805?", after, fixed = TRUE)
           scr <- grepl("{\\super 2}", after, fixed = TRUE)
           c("none", "script", "relational", "all")[1L + scr + 2L * rel]
         })
}

# One case: the sources in `set` given, the highest of them the winner
.t13_dc_run <- function(key, mod, set, route = "rtftable") {
  winner <- if (length(set)) set[1L] else "baseline"
  v <- function(s) .t13_value(key, s, winner)
  old <- options(if ("option" %in% set) stats::setNames(list(v("option")), paste0("rtfreporter.", key)) else list())
  on.exit(options(old), add = TRUE)
  own <- list()
  sty <- list()
  if ("arg" %in% set) own[[mod]] <- stats::setNames(list(v("arg")), key)
  if ("section" %in% set) sty[[mod]] <- stats::setNames(list(v("section")), key)
  if ("shared" %in% set) sty[[key]] <- v("shared")
  style <- if (length(sty)) do.call(rtf_style, sty) else NULL
  list(got = .t13_observe(.render_to_string(.t13_doc(own, style, route)), key, mod),
       want = .t13_value(key, winner, winner))
}

test_that("FT-DC6801 [FR-DC315]: the order is the module's own argument > the rtf_style() section > the shared level > the rtfreporter.* option > the baseline, for the five option keys in every module they reach, and the argument by rtftable(), rtf_table_style() and as_rtftables()", {
  # the ladder: each rung sets one more source above the ones before it
  ladder <- list(none = character(0), option = "option",
                 shared = c("shared", "option"),
                 section = c("section", "shared", "option"),
                 arg = c("arg", "section", "shared", "option"))
  n <- 0L
  for (key in names(.t13_keys)) {
    for (mod in .t13_keys[[key]]$mods) {
      for (rung in names(ladder)) {
        r <- .t13_dc_run(key, mod, ladder[[rung]])
        expect_identical(r$got, r$want,
                         label = sprintf("%s / %s / %s", key, mod, rung))
        n <- n + 1L
      }
      # a lower source alone, and a higher one over a lower one that is not next to it
      for (set in list("section", "shared", c("arg", "option"), c("section", "option"))) {
        r <- .t13_dc_run(key, mod, set)
        expect_identical(r$got, r$want,
                         label = sprintf("%s / %s / {%s}", key, mod, paste(set, collapse = " ")))
        n <- n + 1L
      }
    }
    # the table's own argument by each of its routes beats the section, the
    # shared level and the option
    # (rtf_table_style() has the height and the paddings, not the size or markup)
    for (route in c(if (!key %in% c("font_size_half_points", "markup")) "style", "as_rtftables")) {
      r <- .t13_dc_run(key, "table", c("arg", "section", "shared", "option"), route)
      expect_identical(r$got, r$want, label = sprintf("%s / table / %s", key, route))
      n <- n + 1L
    }
  }
  # 19 key-module pairs x 9 sets + 3 keys by rtf_table_style() + 5 keys by as_rtftables()
  expect_identical(n, 19L * 9L + 8L)
  # each option is read when nothing above it is set, and is not the baseline
  for (key in names(.t13_keys)) {
    got <- .t13_dc_run(key, "table", "option")$got
    expect_false(identical(got, .t13_dc_run(key, "table", character(0))$got), label = key)
  }
})

# ------------------------------------------------------------------- PL

.t13_fx <- function(...) test_path("fixtures", "plan-append", ...)

# The document the tables are written in: no run date, the program as given
.t13_pl_doc <- function(p, titles, program) {
  rtf_document(tokens = list(STUDY = "DEMO-02"), program = program) |>
    rtf_section(
      header = rtf_header(list(c(l = "Protocol: {STUDY} (synthetic data)",
                                 r = "Page {PAGE} of {TOTAL_PAGES}"))),
      footer = rtf_footer(list(c(l = "Synthetic data made for this sample.",
                                 r = "Program: {PROGRAM}")))) |>
    rtf_tables(p, titles = titles)
}
.t13_bytes <- function(doc) {
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(out), add = TRUE)
  # the second column block is the wider one: paginate_cols() says so, the table is as meant
  suppressWarnings(generate_rtfreport(doc, out, overwrite = TRUE))
  readLines(out, warn = FALSE)
}

# The tables BY HAND: the listing and the summary each made on their own,
# joined row by row, paged with as_rtftables() -- the way they were written
# before plan_append().  `d` holds the page variables `by`, the subject's
# `label`, the column `col`, `BLQ` and `AVAL`.
.t13_sig <- function(x, d) fmt_signif(signif(x, d), digits = d)
.t13_hand <- function(d, by, col, cols, col_label, stub_header, col_cut = NULL) {
  d$AVALC <- ifelse(d$BLQ, "BLQ", .t13_sig(d$AVAL, 3))
  wide <- stats::reshape(d[, c(by, "label", col, "AVALC")],
                         idvar = c(by, "label"), timevar = col, direction = "wide")
  names(wide) <- sub("^AVALC[.]", "", names(wide))
  wide$part <- 1L
  wide$ord <- 0L
  ard <- cards::ard_summary(
    d[!is.na(d$AVAL), ], by = tidyselect::all_of(c(by, col)), variables = AVAL,
    statistic = ~ list(
      N = length,
      mean = function(x) if (length(x)) mean(x) else NA_real_,
      sd = stats::sd,
      se = function(x) stats::sd(x) / sqrt(length(x)),
      cv = function(x) if (!length(x) || mean(x) == 0) NA_real_
                       else 100 * stats::sd(x) / mean(x),
      median = stats::median,
      min = function(x) if (length(x)) min(x) else NA_real_,
      max = function(x) if (length(x)) max(x) else NA_real_))
  p <- ard |>
    normalize_ard() |>
    table_plan(cols = col, rows = c(stats::setNames(by, by), group = "variable")) |>
    plan_cells(continuous = c(
      "N" = "{N:.0f}", "Mean" = "{mean:.4s}", "SD" = "{sd:.5s}", "SE" = "{se:.5s}",
      "CV(%)" = "{cv:.1f}", "Median" = "{median:.4s}", "Min" = "{min:.3s}",
      "Max" = "{max:.3s}"), notes = FALSE)
  p <- do.call(plan_levels, c(list(p), stats::setNames(list(cols), col)))
  stats_tab <- as.data.frame(plan_apply(p, "table"))
  stats_tab$label <- paste0(strrep(intToUtf8(160L), 4L), stats_tab$label)   # indented with no-break spaces, as a stub is
  stats_tab$part <- 3L
  stats_tab$ord <- seq_len(nrow(stats_tab))
  heads <- unique(stats_tab[by])
  heads$label <- "Summary Statistics"
  heads[cols] <- ""
  heads$part <- 2L
  heads$ord <- 0L
  keep <- c(by, "part", "ord", "label", cols)
  body <- rbind(wide[keep], heads[keep], stats_tab[keep])
  body[cols] <- lapply(body[cols], function(v) ifelse(is.na(v), "", v))
  key <- lapply(by, function(b) match(body[[b]], levels(factor(d[[b]]))))
  body <- body[do.call(order, c(key, list(body$part, body$ord, body$label))), ]
  pages <- as_rtftables(
    body, page_by = by, drop_cols = c(by, "part", "ord"),
    blank_row_first = TRUE, blank_row_end = TRUE,
    blank_rows = blank_rows_by_rule("label", "^Summary Statistics$", where = "before"),
    col_header = rtf_col_header(
      list(col_cell(1, ""), col_cell(c(2, length(cols) + 1L), col_label)),
      c(stub_header, cols)),
    col_rel_width = c(2.4, rep(1.2, length(cols))),
    border = "tfl")
  if (!is.null(col_cut)) {
    pages <- suppressWarnings(paginate_cols(pages, at = match(col_cut, cols) + 1L,
                           order = c("group", "rows", "cols")))
  }
  lapply(pages, function(pg) {
    pg |>
      set_decimal_split(cols = seq_len(ncol(pg$data))[-1]) |>
      style_zone(last_row = rtf_border(bottom = TRUE))
  })
}
.t13_records <- function(d) {
  d$label <- sprintf("%s/%d/%s", d$USUBJID, d$AGE, d$SEX)
  d$BLQ <- d$BLQFL %in% "Y"
  d
}
.t13_stats <- c(N = "{N:.0f}", Mean = "{mean:.4s}", SD = "{sd:.5s}", SE = "{se:.5s}",
                "CV(%)" = "{cv:.1f}", Median = "{median:.4s}", Min = "{min:.3s}",
                Max = "{max:.3s}")
.t13_fns <- ~ list(N = length, mean = function(x) if (length(x)) mean(x) else NA_real_,
                   sd = stats::sd, median = stats::median,
                   min = function(x) if (length(x)) min(x) else NA_real_,
                   max = function(x) if (length(x)) max(x) else NA_real_,
                   se = function(x) stats::sd(x) / sqrt(length(x)),
                   cv = function(x) if (!length(x) || mean(x) == 0) NA_real_
                                    else 100 * stats::sd(x) / mean(x))

test_that("FT-PL6801 [FR-PL901]: a listing above its summary, as two plans joined by plan_append(), gives byte for byte the RTF of the same table built by hand: the time points across in two column blocks, the PK parameters across, the analytes across; the table's verbs before or after plan_append() give the same bytes, and the later of two wins", {
  skip_if_not_installed("cards")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyselect")
  stub <- "Subject ID/Age (years)/Gender"
  by_sub <- ~ "{USUBJID}/{AGE}/{SEX}"

  # ---- the time points across, in two column blocks (a break after 4 hr)
  adpc <- utils::read.csv(.t13_fx("adpc.csv"))
  adpc <- adpc[adpc$RENALGR %in% c("Normal", "Moderate"), ]
  times <- c("Pre-Dose", paste(c(0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 24, 36, 48), "hr"))
  hand_in <- .t13_records(adpc)
  hand_in$RENALGR <- factor(hand_in$RENALGR, levels = c("Normal", "Moderate"))
  hand_in$ANALYTE <- factor(hand_in$ANALYTE, levels = c("Parent", "Metabolite M1"))
  hand_in$ATPT <- factor(hand_in$ATPT, levels = times)
  hand <- .t13_hand(hand_in, by = c("ANALYTE", "RENALGR"), col = "ATPT", cols = times,
                    col_label = "Time (hr) Relative to Dose", stub_header = stub,
                    col_cut = "4 hr")
  titles <- list(list("Table 14.4.3", "Drug X Plasma Concentrations (ng/mL) by Subject",
                      "PK Population",
                      list(text = "Analyte: {ANALYTE}", align = "left"),
                      list(text = "Renal Function Group: {RENALGR}", align = "left")))
  program <- "plan-append/conc_cols_time.R"
  want <- .t13_bytes(.t13_pl_doc(hand, titles, program))
  expect_gt(length(want), 100L)
  ard <- cards::ard_summary(adpc[!is.na(adpc$AVAL), ], by = c(ANALYTE, RENALGR, ATPT),
                            variables = AVAL, statistic = .t13_fns)
  rec <- adpc
  rec$AVAL[rec$BLQFL == "Y"] <- NA                      # a BLQ sample prints "BLQ"
  listing <- rec |>
    table_plan(cols = "ATPT", rows = c(subject = by_sub), value = "AVAL") |>
    plan_cells("{value:.3s}", na = "BLQ")
  summary <- normalize_ard(ard) |>
    table_plan(cols = "ATPT", rows = c(block = ~ "Summary Statistics")) |>
    plan_cells(continuous = .t13_stats)
  col_header <- rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, length(times) + 1L), "Time (hr) Relative to Dose")),
    c(stub, "{col}"))
  whole_table <- function(p, cut = "4 hr") {
    p |>
      plan_levels(ATPT = times) |>
      plan_blanks(first = TRUE, last = TRUE, where = "between_parts") |>
      plan_col_header(col_header) |>
      plan_columns(widths = c(label = 2.4, .values = 1.2), decimal = ".values") |>
      plan_paginate_rows(page_by = c("ANALYTE", "RENALGR")) |>
      plan_paginate_cols(at = cut, order = "down") |>
      plan_style(border = "tfl") |>
      plan_after(function(x) lapply(x, style_zone, last_row = rtf_border(bottom = TRUE)))
  }
  # the verbs written after plan_append()
  after <- whole_table(listing |> plan_append(summary))
  expect_identical(.t13_bytes(.t13_pl_doc(after, titles, program)), want)
  # ... and before it: the same bytes
  before <- whole_table(listing) |> plan_append(summary)
  expect_identical(.t13_bytes(.t13_pl_doc(before, titles, program)), want)
  # ... the later of two wins: a first cut at 2 hr is beaten by the one at 4 hr
  later <- whole_table(listing, cut = "2 hr") |> plan_append(summary) |>
    plan_paginate_cols(at = "4 hr", order = "down")
  expect_identical(.t13_bytes(.t13_pl_doc(later, titles, program)), want)
  # ... and the earlier one does not: the cut at 2 hr last gives other bytes
  last_cut <- whole_table(listing) |> plan_append(summary) |>
    plan_paginate_cols(at = "2 hr", order = "down")
  expect_false(identical(.t13_bytes(.t13_pl_doc(last_cut, titles, program)), want))

  # ---- the PK parameters across, a page an analyte and group
  adpp <- utils::read.csv(.t13_fx("adpp.csv"))
  params <- c("C_{max} (ng/mL)", "T_{max} (hr)", "AUC_{last} (ng·hr/mL)",
              "AUC_{inf} (ng·hr/mL)", "t1/2 (hr)")
  hand_pp <- adpp
  hand_pp$label <- sprintf("%s/%d/%s", hand_pp$USUBJID, hand_pp$AGE, hand_pp$SEX)
  hand_pp$BLQ <- FALSE
  hand_pp$RENALGR <- factor(hand_pp$RENALGR, levels = c("Normal", "Mild", "Moderate"))
  hand_pp$ANALYTE <- factor(hand_pp$ANALYTE, levels = c("Parent", "Metabolite M1"))
  hand_pp$PARAM <- factor(hand_pp$PARAM, levels = params)
  hand2 <- .t13_hand(hand_pp, by = c("ANALYTE", "RENALGR"), col = "PARAM", cols = params,
                     col_label = "Parameter", stub_header = stub)
  titles2 <- list(list("Table 14.4.1", "Drug X Pharmacokinetic Parameters by Subject",
                       "PK Population",
                       list(text = "Analyte: {ANALYTE}", align = "left"),
                       list(text = "Renal Function Group: {RENALGR}", align = "left")))
  program2 <- "plan-append/param_by_group.R"
  ard2 <- cards::ard_summary(adpp, by = c(ANALYTE, RENALGR, PARAM), variables = AVAL,
                             statistic = .t13_fns)
  listing2 <- adpp |>
    table_plan(cols = "PARAM", rows = c(subject = by_sub), value = "AVAL") |>
    plan_cells("{value:.3s}")
  summary2 <- normalize_ard(ard2) |>
    table_plan(cols = "PARAM", rows = c(block = ~ "Summary Statistics")) |>
    plan_cells(continuous = .t13_stats)
  whole2 <- function(p) {
    p |>
      plan_levels(PARAM = params, ANALYTE = c("Parent", "Metabolite M1"),
                  RENALGR = c("Normal", "Mild", "Moderate")) |>
      plan_blanks(first = TRUE, last = TRUE, where = "between_parts") |>
      plan_col_header(rtf_col_header(
        list(col_cell(1, ""), col_cell(c(2, length(params) + 1L), "Parameter")),
        c(stub, "{col}"))) |>
      plan_columns(widths = c(label = 2.4, .values = 1.2), decimal = ".values") |>
      plan_paginate_rows(page_by = c("ANALYTE", "RENALGR")) |>
      plan_style(border = "tfl") |>
      plan_after(function(x) lapply(x, style_zone, last_row = rtf_border(bottom = TRUE)))
  }
  want2 <- .t13_bytes(.t13_pl_doc(hand2, titles2, program2))
  expect_identical(.t13_bytes(.t13_pl_doc(whole2(listing2 |> plan_append(summary2)),
                                          titles2, program2)), want2)
  expect_identical(.t13_bytes(.t13_pl_doc(whole2(listing2) |> plan_append(summary2),
                                          titles2, program2)), want2)

  # ---- the analytes across, a page a group and time point
  adpc2 <- utils::read.csv(.t13_fx("adpc.csv"))
  adpc2 <- adpc2[adpc2$ATPT %in% c("1 hr", "12 hr"), ]
  hand_an <- .t13_records(adpc2)
  hand_an$RENALGR <- factor(hand_an$RENALGR, levels = c("Normal", "Mild", "Moderate"))
  hand_an$ATPT <- factor(hand_an$ATPT, levels = c("1 hr", "12 hr"))
  hand_an$ANALYTE <- factor(hand_an$ANALYTE, levels = c("Parent", "Metabolite M1"))
  hand3 <- .t13_hand(hand_an, by = c("RENALGR", "ATPT"), col = "ANALYTE",
                     cols = c("Parent", "Metabolite M1"), col_label = "Analyte",
                     stub_header = stub)
  titles3 <- list(list("Table 14.4.2", "Drug X Plasma Concentrations (ng/mL) by Subject",
                       "PK Population",
                       list(text = "Time: {ATPT}", align = "left"),
                       list(text = "Renal Function Group: {RENALGR}", align = "left")))
  program3 <- "plan-append/conc_cols_analyte.R"
  ard3 <- cards::ard_summary(adpc2[!is.na(adpc2$AVAL), ], by = c(RENALGR, ATPT, ANALYTE),
                             variables = AVAL, statistic = .t13_fns)
  rec3 <- adpc2
  rec3$AVAL[rec3$BLQFL == "Y"] <- NA
  listing3 <- rec3 |>
    table_plan(cols = "ANALYTE", rows = c(subject = by_sub), value = "AVAL") |>
    plan_cells("{value:.3s}", na = "BLQ")
  summary3 <- normalize_ard(ard3) |>
    table_plan(cols = "ANALYTE", rows = c(block = ~ "Summary Statistics")) |>
    plan_cells(continuous = .t13_stats)
  whole3 <- function(p) {
    p |>
      plan_levels(ANALYTE = c("Parent", "Metabolite M1"), ATPT = c("1 hr", "12 hr"),
                  RENALGR = c("Normal", "Mild", "Moderate")) |>
      plan_blanks(first = TRUE, last = TRUE, where = "between_parts") |>
      plan_col_header(rtf_col_header(
        list(col_cell(1, ""), col_cell(c(2, 3), "Analyte")),
        c(stub, "{col}"))) |>
      plan_columns(widths = c(label = 2.4, .values = 1.2), decimal = ".values") |>
      plan_paginate_rows(page_by = c("RENALGR", "ATPT")) |>
      plan_style(border = "tfl") |>
      plan_after(function(x) lapply(x, style_zone, last_row = rtf_border(bottom = TRUE)))
  }
  want3 <- .t13_bytes(.t13_pl_doc(hand3, titles3, program3))
  expect_identical(.t13_bytes(.t13_pl_doc(whole3(listing3 |> plan_append(summary3)),
                                          titles3, program3)), want3)
  expect_identical(.t13_bytes(.t13_pl_doc(whole3(listing3) |> plan_append(summary3),
                                          titles3, program3)), want3)
})
