# Validation tests: rtftable() -- how its styling arguments act together:
# border x header rows x stub x column widths x cell padding x row height x
# alignment x blank rows -- a pairwise set checked against the RTF control
# words written for every row, a decision table of the frame against the
# header, and a decision table of where a style value comes from; then blank
# rows in a frame, "none" and the group underline, and the style verbs
# (order, a later call on the same target, overlapping zones and levels, the
# decimal split against widths and alignment).  Helpers prefixed .ts_.
# Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (rtftable(), rtf_border(),
#  rtf_table_style(), rtf_style()) and the code (R/rtftable.R rtftable();
#  R/rtf_border.R .expand_table_border(); R/generate_rtfreport.R
#  .render_rtftable(), .compute_cellx(), .header_outer_border(),
#  .cell_vpad_cmd(), the data-row loop and .blank_row_rtf()):
#
#    border   "tfl": a rule above the topmost header row and below the
#             bottommost one, nothing on the body (FR-TB566 FR-TB457); a
#             spanning cell the next row subdivides gets the group underline
#             with "tfl" and with a frame (FR-TB458); "none": no \clbrdr at
#             all; an rtf_border(): its edges
#             frame the table (top on the topmost row, the column names
#             included, bottom on the last data row, left on the first cell
#             and right on the last cell of every row), inside_h the rule under
#             every row but the last, inside_v every cell edge -- on a spanning
#             row the merged cell's (FR-IX338 FR-IX339 FR-TB614).
#    widths   every row's \cellx: default -> ends at the writable width;
#             col_rel_width -> that share of it; column_widths_twips -> their
#             sums; table_width_pct -> ends at that share of the writable
#             width (FR-TB568 FR-TB548); a spanning cell
#             ends on the \cellx of its last column; a blank row is one cell
#             as wide as the table.
#    padding  left / right as \li / \ri on every header and data cell
#             (FR-TB570); top / bottom as \clpadl<N>\clpadfl3 /
#             \clpadb<N>\clpadfb3 on every header, spanning and data cell,
#             blank rows bare (FR-TB545).
#    height   row_height_twips with row_height_exact = TRUE -> \trrh-N, FALSE
#             -> \trrhN, on the header and blank rows too (FR-TB552 FR-TB553
#             FR-TB554); none -> the size's height (230), a minimum.
#    align    a row-title column \ql, the others \qc (FR-TB547; row_title =
#             integer(0): none); col_spec align on its column; the header
#             label follows the data unless col_header_align is given
#             (FR-TB564 FR-TB565); cell_valign as \clvertal[btc] on every
#             header and data cell (FR-TB555).
#    blank    positions 0 / k / -1 put one full-width bare row there
#             (FR-TB567); inside an rtf_border() frame it is a body row: the
#             body's left / right edges, inside_v (then one empty cell per
#             column) and inside_h, a trailing one the frame's bottom edge
#             (FR-TB615).  border = "none" draws no group underline
#             (FR-TB616).
#
#  Data: three records (Age, Sex, Wt) in three columns Param, A, B.
#
#  (A) FT-TB785, pairwise.  Factors and levels:
#    border   tfl, none, box (rtf_border(all = TRUE)), grid (rtf_border(all =
#             TRUE, inside_h = TRUE, inside_v = TRUE))
#    header   names (col_header NULL), two (two label rows), span (a spanning
#             row over A and B, then a label row)
#    stub     col1 (row_title NULL), none (row_title = integer(0))
#    widths   default, rel c(2, 1, 1), twips c(3000, 1500, 1500), pct 50
#    pad      none, lr (left 40, right 30), tb (top 20, bottom 10), all four
#    height   default, exact 300, min 300
#    halign   default, data (col_spec col 2 right), header (col_header_align
#             "left")
#    valign   bottom, top, center
#    blank    none, inside (after row 1), ends (0 and -1)
#  No constraints (#869 and #870, which this set left out at first, are
#  fixed: a blank row is inside a box / grid frame, and "none" draws no
#  group underline).  The product is 31,104 rows; the candidates are every
#  89th of them (expand.grid order, 350 rows), checked to hold every pair of
#  levels, and .allpairs() picks the rows covering every pair.  Oracle: each
#  row's \trrh, \cellx, \clbrdr[tblr], \clvertal, \clpadl / \clpadb, \q, \li
#  / \ri and text, from the rules above; a blank row inside box is one cell
#  with the left / right edges, inside grid one empty cell per column with
#  every edge, and a trailing one carries the bottom edge (FR-TB615).
#  FR-TB545 FR-TB547 FR-TB548 FR-TB552 FR-TB553 FR-TB554 FR-TB555 FR-TB564
#  FR-TB565 FR-TB566 FR-TB567 FR-TB568 FR-TB570 FR-TB457 FR-TB458 FR-IX338
#  FR-IX339 FR-TB614 FR-TB615 FR-TB616.
#
#  (B) FT-TB786, decision table: the frame's top edge and the inside_h seam
#  against the header (the rows' \clbrdrt / \clbrdrb, header rows first).
#    border  col_header      rows      top           bottom
#    box     NULL            N D D D   N             last D
#    box     list() (none)   D D D     first D       last D
#    grid    NULL            N D D D   N             every row
#    grid    list() (none)   D D D     first D       every row
#    box     two rows        H H D D D first H       last D
#    tfl     NULL            N D D D   N             N
#    tfl     list() (none)   D D D     none          none
#  (NULL used to be read as "no header": the top rule fell between the
#  column names and the first data row, and grid lost the seam under them.)
#  FR-IX338 FR-TB566 FR-TB614.
#
#  (C) FT-TB787, decision table: where a table's value comes from, the
#  table's style (rtftable(style = rtf_table_style())) among the sources
#  FT-DC248 (#818) ranks without it.  Sources: A the argument, T the table
#  style, S rtf_style(table = list()), H rtf_style() shared, O the option.
#  Values: height A 410, T 420, S 430, H 440, O 450 (\trrh of a data row);
#  padding A 55, T 66, S 77, H 88, O 99 (\ri, \clpadl, \clpadb).
#    key \ set      T    A T   T S   T H   T O   S H O   A T S H O
#    row height     T    A     T     T     T     S       A
#    pad right      T    A     T     T     T     S       A
#    pad top        T    A     T     T     T     S       A
#    pad bottom     T    A     T     T     T     S       A
#  The header label's alignment (col 2): T header_align right -> r; A
#  col_header_align left + T header_align right -> l; T align right -> r
#  (data and header); T align right + header_align left -> data r, header l.
#  The table style's borders: style = T (last row ruled), border left at
#  "tfl" -> T's; border = "none" -> none; border = rtf_border(all = TRUE) ->
#  the frame.
#  FR-TB569 FR-TB570 FR-IX395 FR-IX335 FR-TB448 FR-TB450 FR-TB455 FR-DC315.
# ---------------------------------------------------------------------------

.ts_data <- function() {
  data.frame(Param = c("Age", "Sex", "Wt"), A = c("1.0", "2.0", "3.0"),
             B = c("4", "5", "6"), stringsAsFactors = FALSE)
}

# The table's rows read back from the RTF: per row its \trrh and, per cell,
# \cellx, the four borders, \clvertal, top / bottom padding, \q, \li, \ri and
# the text.
.ts_rows <- function(txt) {
  body <- sub("^.*?\\\\trowd", "\\\\trowd", txt)
  rows <- strsplit(body, "\\\\row(?![a-z])", perl = TRUE)[[1L]]
  rows <- gsub("\n", "", rows[grepl("\\\\trowd", rows)])
  num <- function(x, re) {
    ifelse(grepl(re, x), as.integer(sub(paste0("^.*?", re, "([0-9]+).*$"), "\\1", x)), 0L)
  }
  lapply(rows, function(r) {
    def <- sub("^(.*\\\\cellx[0-9]+).*$", "\\1", r)
    cells <- substring(r, nchar(def) + 1L)
    cd <- regmatches(def, gregexpr("(\\\\(cl|brdr)[a-z]+[0-9]*)*\\\\cellx[0-9]+", def))[[1L]]
    ct <- strsplit(cells, "\\\\cell(?![a-z])", perl = TRUE)[[1L]][seq_along(cd)]
    side <- function(s) grepl(paste0("\\\\clbrdr", s, "\\\\brdr(?!nil|none)"), cd, perl = TRUE)
    list(trrh = as.integer(sub(".*\\\\trrh(-?[0-9]+).*", "\\1", def)),
         cellx = as.integer(sub(".*\\\\cellx", "", cd)),
         t = side("t"), b = side("b"), l = side("l"), r = side("r"),
         va = sub(".*\\\\clvertal([tcb]).*", "\\1", cd),
         padt = num(cd, "\\\\clpadl"), padb = num(cd, "\\\\clpadb"),
         q = ifelse(grepl("\\\\q[lrcj]", ct), sub("^.*?\\\\q([lrcj]).*$", "\\1", ct), ""),
         li = num(ct, "\\\\li"), ri = num(ct, "\\\\ri"),
         text = trimws(gsub("\\\\[a-z]+-?[0-9]* ?|[{}]", "", ct)))
  })
}

.ts_render <- function(tbl, style = NULL) {
  doc <- rtf_document(style = style)
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  .ts_rows(.render_to_string(rtf_tables(doc, list(tbl))))
}

.TS_BORDER <- list(tfl = "tfl", none = "none", box = rtf_border(all = TRUE),
                   grid = rtf_border(all = TRUE, inside_h = TRUE, inside_v = TRUE))
.TS_HEADER <- list(names = NULL, two = list(c("a", "b", "c"), c("P", "A", "B")),
                   span = list(list(list(from = 2, to = 3, label = "Trt")),
                               c("P", "A", "B")))

.ts_args <- function(cs) {
  a <- list(.ts_data(), border = .TS_BORDER[[cs$border]],
            col_header = .TS_HEADER[[cs$header]])
  if (cs$stub == "none") a$row_title <- integer(0)
  switch(cs$widths,
         rel = a$col_rel_width <- c(2, 1, 1),
         twips = a$column_widths_twips <- c(3000, 1500, 1500),
         pct = a$table_width_pct <- 50)
  if (cs$pad %in% c("lr", "all")) {
    a$cell_padding_left_twips <- 40L
    a$cell_padding_right_twips <- 30L
  }
  if (cs$pad %in% c("tb", "all")) {
    a$cell_padding_top_twips <- 20L
    a$cell_padding_bottom_twips <- 10L
  }
  if (cs$height != "default") {
    a$row_height_twips <- 300L
    a$row_height_exact <- cs$height == "exact"
  }
  if (cs$halign == "data") a$col_spec <- list(list(col = 2, align = "right"))
  if (cs$halign == "header") a$col_header_align <- "left"
  a$cell_valign <- cs$valign
  a$blank_rows <- switch(cs$blank, none = NULL, inside = 1, ends = c(0, -1))
  a
}

# Every pair of levels of two factors that the constraint allows.
.ts_pairs <- function(levels, ok_pair) {
  pr <- utils::combn(names(levels), 2L, simplify = FALSE)
  unlist(lapply(pr, function(p) {
    g <- expand.grid(levels[[p[1L]]], levels[[p[2L]]], stringsAsFactors = FALSE)
    g <- g[ok_pair(p, g[[1L]], g[[2L]]), , drop = FALSE]
    paste(p[1L], g[[1L]], p[2L], g[[2L]], sep = "|")
  }))
}

.ts_check <- function(cs, W) {
  rows <- .ts_render(do.call(rtftable, .ts_args(cs)))
  bad <- character(0)
  data_txt <- list(c("Age", "1.0", "4"), c("Sex", "2.0", "5"), c("Wt", "3.0", "6"))
  hdr_txt <- switch(cs$header, names = list(c("Param", "A", "B")),
                    two = list(c("a", "b", "c"), c("P", "A", "B")),
                    span = list(c("", "Trt"), c("P", "A", "B")))
  blank <- list(if (cs$border == "grid") c("", "", "") else "")
  seq_txt <- c(hdr_txt,
               if (cs$blank == "ends") blank, data_txt[1L],
               if (cs$blank == "inside") blank, data_txt[2:3],
               if (cs$blank == "ends") blank)
  got_txt <- lapply(rows, `[[`, "text")
  if (!identical(got_txt, seq_txt)) return("rows")
  nh <- length(hdr_txt)
  is_blank <- vapply(seq_txt, function(x) all(!nzchar(x)), NA)
  kind <- ifelse(is_blank, "blank", ifelse(seq_along(rows) <= nh, "header", "data"))
  kind[cs$header == "span" & seq_along(rows) == 1L] <- "span"
  # every case has a header row, so the frame's top edge is on row 1; its
  # bottom edge is on the last row, a trailing blank one included (FR-TB615)
  first <- 1L
  last <- length(rows)
  framed <- cs$border %in% c("box", "grid")
  # widths
  cx <- switch(cs$widths,
               default = NULL,
               rel = { w <- as.integer(round(W * c(2, 1, 1) / 4)); w[3L] <- W - sum(w[1:2]); cumsum(w) },
               twips = cumsum(c(3000L, 1500L, 1500L)),
               pct = NULL)
  # default and pct: the columns share the width by their content
  # (default_col_widths()); the table ends at the writable width, or half of it
  full <- rows[[which(kind %in% c("header", "data"))[1L]]]$cellx
  if (is.null(cx)) {
    end <- if (cs$widths == "pct") as.integer(round(W * 0.5)) else W
    if (full[3L] != end || any(diff(c(0L, full)) <= 0L)) bad <- c(bad, "widths")
    cx <- full
  }
  height <- switch(cs$height, default = 230L, exact = -300L, min = 300L)
  pl <- if (cs$pad %in% c("lr", "all")) 40L else 0L
  pr <- if (cs$pad %in% c("lr", "all")) 30L else 0L
  pt <- if (cs$pad %in% c("tb", "all")) 20L else 0L
  pb <- if (cs$pad %in% c("tb", "all")) 10L else 0L
  dq <- c(if (cs$stub == "col1") "l" else "c", if (cs$halign == "data") "r" else "c", "c")
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    k <- kind[i]
    tag <- function(x) sprintf("%s row %d", x, i)
    if (r$trrh != height) bad <- c(bad, tag("trrh"))
    # a blank row is one full-width cell, but one cell per column inside a
    # grid, so its vertical rules run on (FR-TB615)
    want_cx <- switch(k, blank = if (cs$border == "grid") cx else cx[3L],
                      span = cx[c(1L, 3L)], cx)
    if (!identical(r$cellx, as.integer(want_cx))) bad <- c(bad, tag("cellx"))
    n <- length(r$cellx)
    # borders
    z <- rep(FALSE, n)
    wt <- wb <- wl <- wr <- z
    if (cs$border == "tfl") {
      if (i == first) wt[] <- TRUE
      if (i == nh) wb[] <- TRUE
    } else if (framed) {
      if (i == first) wt[] <- TRUE
      if (i == last || cs$border == "grid") wb[] <- TRUE
      wl[1L] <- TRUE
      wr[n] <- TRUE
      if (cs$border == "grid") wl[] <- wr[] <- TRUE
    }
    # the group underline under the spanning cell the label row subdivides;
    # none under "none" (FR-TB616)
    if (k == "span" && cs$border != "none") wb[2L] <- TRUE
    if (!identical(list(r$t, r$b, r$l, r$r), list(wt, wb, wl, wr))) bad <- c(bad, tag("borders"))
    if (k == "blank") {
      # no padding on a spacer row
      if (any(r$padt > 0L) || any(r$padb > 0L)) bad <- c(bad, tag("blank pad"))
      next
    }
    if (any(r$va != substr(cs$valign, 1L, 1L))) bad <- c(bad, tag("valign"))
    if (any(r$padt != pt) || any(r$padb != pb)) bad <- c(bad, tag("pad tb"))
    if (any(r$li != pl) || any(r$ri != pr)) bad <- c(bad, tag("pad lr"))
    wq <- if (k == "data") dq else if (cs$halign == "header") rep("l", 3L) else dq
    if (k == "span") {
      # the spanning label: the header alignment of the leftmost column it
      # covers (col 2)
      if (r$q[2L] != wq[2L]) bad <- c(bad, tag("align span"))
    } else if (!identical(r$q, wq)) {
      bad <- c(bad, tag("align"))
    }
  }
  bad
}

test_that("FT-TB785 [FR-TB545 FR-TB547 FR-TB548 FR-TB552 FR-TB553 FR-TB554 FR-TB555 FR-TB564 FR-TB565 FR-TB566 FR-TB567 FR-TB568 FR-TB570 FR-TB457 FR-TB458 FR-IX338 FR-IX339 FR-TB614 FR-TB615 FR-TB616]: pairwise -- border x header x stub x widths x padding x height x alignment x valign x blank rows", {
  levels <- list(border = c("tfl", "none", "box", "grid"),
                 header = c("names", "two", "span"), stub = c("col1", "none"),
                 widths = c("default", "rel", "twips", "pct"),
                 pad = c("none", "lr", "tb", "all"),
                 height = c("default", "exact", "min"),
                 halign = c("default", "data", "header"),
                 valign = c("bottom", "top", "center"),
                 blank = c("none", "inside", "ends"))
  cand <- do.call(expand.grid, c(levels, stringsAsFactors = FALSE))
  expect_identical(nrow(cand), 31104L)
  cand <- cand[seq(1L, nrow(cand), by = 89L), ]
  rownames(cand) <- NULL
  all_pairs <- .ts_pairs(levels, function(p, a, b) rep(TRUE, length(a)))
  ap <- .allpairs(cand)
  # the candidates hold every valid pair, and the rows picked cover them all
  expect_setequal(ap$need, all_pairs)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 30L)
  W <- utils::tail(.ts_render(rtftable(.ts_data()))[[1L]]$cellx, 1L)
  set <- ap$rows
  for (i in seq_len(nrow(set))) {
    cs <- as.list(set[i, ])
    expect_identical(.ts_check(cs, W), character(0), label = paste(unlist(cs), collapse = " / "))
  }
})

test_that("FT-TB786 [FR-IX338 FR-TB566 FR-TB614]: decision table -- the frame's top edge and the inside_h seam against the header", {
  df <- .ts_data()
  two <- list(c("a", "b", "c"), c("P", "A", "B"))
  bits <- function(border, col_header) {
    rows <- .ts_render(rtftable(df, border = border, col_header = col_header))
    vapply(rows, function(r) paste0(if (all(r$t)) "T" else if (any(r$t)) "t" else "-",
                                    if (all(r$b)) "B" else if (any(r$b)) "b" else "-"), "")
  }
  box <- rtf_border(all = TRUE)
  grid <- rtf_border(all = TRUE, inside_h = TRUE, inside_v = TRUE)
  dt <- list(
    list(box,   NULL,   c("T-", "--", "--", "-B")),
    list(box,   list(), c("T-", "--", "-B")),
    list(grid,  NULL,   c("TB", "-B", "-B", "-B")),
    list(grid,  list(), c("TB", "-B", "-B")),
    list(box,   two,    c("T-", "--", "--", "--", "-B")),
    list("tfl", NULL,   c("TB", "--", "--", "--")),
    list("tfl", list(), c("--", "--", "--")))
  for (k in seq_along(dt)) {
    d <- dt[[k]]
    expect_identical(bits(d[[1L]], d[[2L]]), d[[3L]], label = sprintf("row %d", k))
  }
})

test_that("FT-TB787 [FR-TB569 FR-TB570 FR-IX395 FR-IX335 FR-TB448 FR-TB450 FR-TB455 FR-DC315]: decision table -- the argument > the table style > rtf_style(table = ) > rtf_style() > the option", {
  df <- .ts_data()
  val <- list(row_height_twips = c(A = 410L, T = 420L, S = 430L, H = 440L, O = 450L),
              pad = c(A = 55L, T = 66L, S = 77L, H = 88L, O = 99L))
  read <- list(row_height_twips = function(r) r$trrh,
               cell_padding_right_twips = function(r) r$ri[2L],
               cell_padding_top_twips = function(r) r$padt[2L],
               cell_padding_bottom_twips = function(r) r$padb[2L])
  sets <- list(c("T", "T"), c("A T", "A"), c("T S", "T"), c("T H", "T"),
               c("T O", "T"), c("S H O", "S"), c("A T S H O", "A"))
  for (key in names(read)) {
    v <- if (key == "row_height_twips") val$row_height_twips else val$pad
    for (s in sets) {
      src <- strsplit(s[1L], " ")[[1L]]
      one <- function(x) stats::setNames(list(v[[x]]), key)
      ts <- if ("T" %in% src) do.call(rtf_table_style, one("T"))
      args <- c(list(df, style = ts), if ("A" %in% src) one("A"))
      st_args <- c(if ("H" %in% src) one("H"), if ("S" %in% src) list(table = one("S")))
      st <- if (length(st_args)) do.call(rtf_style, st_args)
      old <- options(stats::setNames(list(if ("O" %in% src) v[["O"]]),
                                     paste0("rtfreporter.", key)))
      rows <- .ts_render(do.call(rtftable, args), style = st)
      options(old)
      expect_identical(read[[key]](rows[[2L]]), v[[s[2L]]],
                       label = sprintf("%s, set %s", key, s[1L]))
    }
  }
  # the header label's alignment (col 2) and the data's
  q <- function(...) {
    rows <- .ts_render(rtftable(df, ...))
    c(header = rows[[1L]]$q[2L], data = rows[[2L]]$q[2L])
  }
  expect_identical(q(style = rtf_table_style(header_align = "right")), c(header = "r", data = "c"))
  expect_identical(q(style = rtf_table_style(header_align = "right"), col_header_align = "left"),
                   c(header = "l", data = "c"))
  expect_identical(q(style = rtf_table_style(align = "right")), c(header = "r", data = "r"))
  expect_identical(q(style = rtf_table_style(align = "right", header_align = "left")),
                   c(header = "l", data = "r"))
  # the table style's borders, while border is left at its default
  last_b <- function(...) {
    rows <- .ts_render(rtftable(df, ...))
    vapply(rows, function(r) any(r$b), NA)
  }
  ts <- rtf_table_style(border_last_row = rtf_border(bottom = TRUE))
  expect_identical(last_b(style = ts), c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(last_b(style = ts, border = "none"), c(FALSE, FALSE, FALSE, FALSE))
  expect_identical(last_b(style = ts, border = rtf_border(all = TRUE)), c(FALSE, FALSE, FALSE, TRUE))
})

# ---------------------------------------------------------------------------
#  Blank rows in a frame (#869), "none" and the group underline (#870), and
#  the style verbs: order, a later call on the same target, overlapping
#  zones and levels, and the decimal split against widths and alignment.
# ---------------------------------------------------------------------------
#
#  (D) FT-TB973, decision table: a blank row against the frame (FR-TB615).
#  Each row of the table is coded "<top><bottom><left><right><inside v>
#  <cells>": T / B all cells, t / b some, L / R the first cell's left / last
#  cell's right edge, v a rule between cells, - none.
#    border (zone)            header  blank   rows
#    box                      names   1, -1   H D blank D D blank
#    grid                     names   1, -1   every row ruled, blank 3 cells
#    box                      none    0, -1   lead blank has the top edge,
#                                              trailing one the bottom edge
#    grid                     none    0       lead blank has the top edge
#    box + inside_h           names   2       blank ruled below
#    box, last_row none       names   -1      the erased edge stays erased
#    box, last_row double     names   -1      the zone's edge stays on D3
#    tfl                      names   0 1 -1  blank rows bare
#    none                     names   1       bare
#    tfl + body left / right  names   1       blank row carries them
#
#  (E) FT-TB974, decision table: the group underline under "Trt" (spanning
#  A..B over a label row) against the border (FR-TB616 FR-TB458 FR-IX341).
#    border                               underline   any \clbrdr
#    "none"                               no          no
#    "tfl"                                yes         yes
#    rtf_border(all = TRUE)               yes         yes
#    rtf_table_style_tfl()                yes         yes
#    "none" + style_zone(header = top)    yes         yes
#    "none" + style_header(bottom = TRUE) yes         yes
#    "tfl" + style_header(bottom = none)  no          yes
#
#  (F) FT-TB975, pairwise: the style verbs in three orders, all on column A.
#    zone    none, hdr (style_zone(header = bottom double)), ih (body =
#            inside_h single), last (last_row = bottom thick)
#    header  none, align (style_header(cols = 2, align = left)), label
#            ("Arm"), border (bottom dot; promotes the label row), bold
#    cols    none, align (right), border (bottom dash), bold, bg (fill),
#            hbold (header_bold)
#    body    none (rows = 1 throughout), bold, align (left), border (bottom
#            dot), color
#    split   none, on (set_decimal_split(cols = 2, ratio = 0.5))
#  Constraints: header border x cols hbold is left out (#872: a label row
#  promoted by style_header(border = ) keeps the header styling it had then,
#  so a later style_cols(header_bold = ) is lost, and its header_background
#  is dropped); header bold x cols hbold write the same field (FR-IX398, (G)).
#  Every row of .allpairs() is applied in the orders zone header cols body
#  split, the reverse, and cols split zone body header; the three RTFs are
#  identical, and the first is checked: header cell A's bottom dot (cell) >
#  dash (column) > double (zone) > single (tfl), its label, alignment and
#  bold; body cell A of row 1 split r / l or aligned by the cell > the
#  column, bold, colour, fill on every half, bottom dot (cell) > single
#  (inside_h); row 2 ("NE", never split) one cell aligned by the column; row
#  3's bottom thick only with last.
#  FR-IX397 FR-IX330 FR-IX331 FR-IX332 FR-TB408 FR-TB422 FR-TB430 FR-TB363.
#
#  (G) FT-TB976, decision table: a later call on the same target, as the RTF
#  of the two calls against the RTF of the one call it amounts to.
#    two calls                                     same as
#    zone header top double, then bottom dot       one zone with both
#    zone header bottom dot, then bottom double    bottom double
#    zone header bottom dot, then bottom none      bottom none
#    zone header bottom dot, then rtf_border()     the first alone
#    body border bottom dot, then top double       one border with both
#    body border bottom dot, then bottom none      bottom none
#    header border bottom dot, then bottom double  bottom double
#    cols border bottom dot, then top double       one border with both
#    cols align left, then right                   right
#    body align left, then right                   right
#    header label X, then Y                        Y
#    header align left, then cols header_align r   header_align right
#    cols header_align right, then header align l  align left
#    cols bold TRUE, then FALSE                    no style_cols()
#    body color red, then blue                     blue
#    decimal split ratio 0.3, then 0.6             0.6
#    decimal split, then cols = NULL               no split
#  FR-IX398 FR-IX332 FR-TB439 FR-TB430 FR-TB408 FR-TB345.
#
#  (H) FT-TB977, decision table: zones and levels on the same cell.
#    body inside_h + last_row bottom thick      rows s, s, thick
#    body inside_h + first_row bottom double    rows double, s, none
#    one data row: first_row double + last_row  thick (last_row is applied
#      thick                                     after first_row)
#    box, no header, first_row top double       double (a zone wins over the
#                                                frame's edge)
#    last_row thick + style_body(row 3, none)   none on the cells it names
#    style_cols(border) only                    the header cell, not the body
#    header zone double + cols dash + cell dot  dot (and in either order)
#  FR-IX397 FR-IX331 FR-TB439 FR-TB422 FR-IX338.
#
#  (I) FT-TB978, pairwise: set_decimal_split() against the widths, the
#  alignment, the ratio, the padding, the frame and the header.
#    widths  default, rel c(2, 1, 1), twips c(3000, 1500, 1500), pct 50
#    align   default, colspec (col_spec right), cols (style_cols left),
#            body (style_cols left, then style_body right)
#    ratio   0.3, 0.5, auto
#    pad     none, lr (left 40, right 30)
#    border  tfl, grid
#    header  names, span (A..B under "Trt")
#  Data: A = 1.25, NE, 3.0.  Oracle against the same table unsplit: the
#  header rows identical; row 2 identical but for A's alignment (colspec r,
#  cols l, body r, else c); rows 1 and 3 gain one \cellx between A's edges,
#  at x0 + round(w r) (auto: strictly inside), the halves \qr \li<left>\ri0
#  "1" / "3" and \ql \li0\ri<right> ".25" / ".0"; with grid the left half
#  has no right edge and the right half no left edge.
#  FR-IX399 FR-IX330 FR-TB363 FR-TB364 FR-TB349 FR-TB360 FR-TB548 FR-TB568.
# ---------------------------------------------------------------------------

# The table's rows as written: the RTF from the first \trowd on.
.ts_body <- function(tbl) {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  txt <- .render_to_string(rtf_tables(doc, list(tbl)))
  sub("^.*?\\\\trowd", "\\\\trowd", txt)
}

# Per row: each cell's definition (borders, fill, \cellx) and content.
.ts_raw <- function(tbl) {
  rows <- strsplit(.ts_body(tbl), "\\\\row(?![a-z])", perl = TRUE)[[1L]]
  rows <- gsub("\n", "", rows[grepl("\\\\trowd", rows)])
  lapply(rows, function(r) {
    def <- sub("^(.*\\\\cellx[0-9]+).*$", "\\1", r)
    cd <- regmatches(def, gregexpr("(\\\\(cl|brdr)[a-z]+[0-9]*)*\\\\cellx[0-9]+", def))[[1L]]
    ct <- strsplit(substring(r, nchar(def) + 1L), "\\\\cell(?![a-z])", perl = TRUE)[[1L]][seq_along(cd)]
    list(def = cd, ct = ct, cellx = as.integer(sub(".*\\\\cellx", "", cd)))
  })
}

# The style of a cell's side ("s", "db", "dot", "dash", "th"), "" for none.
.ts_side <- function(def, s) {
  m <- regmatches(def, regexec(paste0("\\\\clbrdr", s, "\\\\brdr([a-z]+)"), def))
  vapply(m, function(x) if (length(x) > 1L) x[2L] else "", "")
}

.ts_q <- function(ct) sub("^\\\\q([lrcj]).*$", "\\1", ct)
.ts_text <- function(ct) trimws(gsub("\\\\[a-z]+-?[0-9]* ?|[{}]", "", ct))

# A row coded "<top><bottom><left><right><inside v><cells>" (see (D)).
.ts_code <- function(r) {
  n <- length(r$def)
  hv <- function(s) nzchar(.ts_side(r$def, s))
  tb <- function(x, up) if (all(x)) up else if (any(x)) tolower(up) else "-"
  l <- hv("l")
  rr <- hv("r")
  inner <- n > 1L && (any(l[-1L]) || any(rr[-n]))
  paste0(tb(hv("t"), "T"), tb(hv("b"), "B"), if (l[1L]) "L" else "-",
         if (rr[n]) "R" else "-", if (inner) "v" else "-", n)
}

test_that("FT-TB973 [FR-TB615 FR-TB567 FR-IX338 FR-TB566]: decision table -- a blank row against the frame: inside a box or grid it carries the edges, a leading / trailing one the top / bottom edge", {
  df <- .ts_data()
  box <- rtf_border(all = TRUE)
  grid <- rtf_border(all = TRUE, inside_h = TRUE, inside_v = TRUE)
  code <- function(tbl) vapply(.ts_raw(tbl), .ts_code, "")
  t1 <- function(border, blank, col_header = NULL) {
    rtftable(df, border = border, blank_rows = blank, col_header = col_header)
  }
  dt <- list(
    list(t1(box, c(1, -1)),
         c("T-LR-3", "--LR-3", "--LR-1", "--LR-3", "--LR-3", "-BLR-1")),
    list(t1(grid, c(1, -1)),
         c("TBLRv3", "-BLRv3", "-BLRv3", "-BLRv3", "-BLRv3", "-BLRv3")),
    list(t1(box, c(0, -1), list()),
         c("T-LR-1", "--LR-3", "--LR-3", "--LR-3", "-BLR-1")),
    list(t1(grid, 0, list()),
         c("TBLRv3", "-BLRv3", "-BLRv3", "-BLRv3")),
    list(t1(rtf_border(all = TRUE, inside_h = TRUE), 2),
         c("TBLR-3", "-BLR-3", "-BLR-3", "-BLR-1", "-BLR-3")),
    list(t1(box, -1) |> style_zone(last_row = rtf_border(bottom = "none")),
         c("T-LR-3", "--LR-3", "--LR-3", "--LR-3", "--LR-1")),
    list(t1(box, -1) |> style_zone(last_row = rtf_border(bottom = "double")),
         c("T-LR-3", "--LR-3", "--LR-3", "-BLR-3", "--LR-1")),
    list(t1("tfl", c(0, 1, -1)),
         c("TB---3", "-----1", "-----3", "-----1", "-----3", "-----3", "-----1")),
    list(t1("none", 1),
         c("-----3", "-----3", "-----1", "-----3", "-----3")),
    list(t1("tfl", 1) |> style_zone(body = rtf_border(left = TRUE, right = TRUE, inside_v = "none")),
         c("TB---3", "--LR-3", "--LR-1", "--LR-3", "--LR-3")))
  for (k in seq_along(dt)) {
    expect_identical(code(dt[[k]][[1L]]), dt[[k]][[2L]], label = sprintf("row %d", k))
  }
  # the moved edges keep their line: a double frame's bottom is double
  dbl <- .ts_raw(t1(rtf_border(all = "double"), -1))
  expect_identical(.ts_side(utils::tail(dbl, 1L)[[1L]]$def, "b"), "db")
  # a blank row is still the table's width and height, and bare inside
  r <- .ts_raw(t1(box, 1, list()))[[2L]]
  expect_identical(r$cellx, .ts_raw(t1(box, 1, list()))[[1L]]$cellx[3L])
  expect_false(grepl("clpad", r$def))
})

test_that("FT-TB974 [FR-TB616 FR-TB458 FR-IX341 FR-TB566]: decision table -- the group underline under a spanning cell against the border: none under \"none\"", {
  df <- .ts_data()
  hdr <- list(list(list(from = 2, to = 3, label = "Trt")), c("P", "A", "B"))
  t1 <- function(border) rtftable(df, border = border, col_header = hdr)
  under <- function(tbl) {
    r <- .ts_raw(tbl)[[1L]]
    c(underline = nzchar(.ts_side(r$def[2L], "b")),
      any = grepl("\\\\clbrdr", .ts_body(tbl)))
  }
  dt <- list(
    list(t1("none"), c(underline = FALSE, any = FALSE)),
    list(t1("tfl"), c(underline = TRUE, any = TRUE)),
    list(t1(rtf_border(all = TRUE)), c(underline = TRUE, any = TRUE)),
    list(t1(rtf_table_style_tfl()), c(underline = TRUE, any = TRUE)),
    list(t1("none") |> style_zone(header = rtf_border(top = TRUE)),
         c(underline = TRUE, any = TRUE)),
    list(t1("none") |> style_header(row = 1, cols = 2, border = rtf_border(bottom = TRUE)),
         c(underline = TRUE, any = TRUE)),
    list(t1("tfl") |> style_header(row = 1, cols = 2, border = rtf_border(bottom = "none")),
         c(underline = FALSE, any = TRUE)))
  for (k in seq_along(dt)) {
    expect_identical(under(dt[[k]][[1L]]), dt[[k]][[2L]], label = sprintf("row %d", k))
  }
})

.ts_split_data <- function() {
  data.frame(Param = c("Age", "Sex", "Wt"), A = c("1.25", "NE", "3.0"),
             B = c("4", "5", "6"), stringsAsFactors = FALSE)
}

# One verb of (F) for a level, as a function of the table.
.ts_verb <- function(factor, level) {
  b <- function(...) rtf_border(...)
  v <- switch(paste(factor, level),
    "zone hdr" = function(x) style_zone(x, header = b(bottom = "double")),
    "zone ih" = function(x) style_zone(x, body = b(inside_h = TRUE)),
    "zone last" = function(x) style_zone(x, last_row = b(bottom = "thick")),
    "header align" = function(x) style_header(x, cols = 2, align = "left"),
    "header label" = function(x) style_header(x, cols = 2, label = "Arm"),
    "header border" = function(x) style_header(x, row = 1, cols = 2, border = b(bottom = "dot")),
    "header bold" = function(x) style_header(x, cols = 2, bold = TRUE),
    "cols align" = function(x) style_cols(x, cols = 2, align = "right"),
    "cols border" = function(x) style_cols(x, cols = 2, border = b(bottom = "dash")),
    "cols bold" = function(x) style_cols(x, cols = 2, bold = TRUE),
    "cols bg" = function(x) style_cols(x, cols = 2, background = "#FF0000"),
    "cols hbold" = function(x) style_cols(x, cols = 2, header_bold = TRUE),
    "body bold" = function(x) style_body(x, rows = 1, cols = 2, bold = TRUE),
    "body align" = function(x) style_body(x, rows = 1, cols = 2, align = "left"),
    "body border" = function(x) style_body(x, rows = 1, cols = 2, border = b(bottom = "dot")),
    "body color" = function(x) style_body(x, rows = 1, cols = 2, color = "#0000FF"),
    "split on" = function(x) set_decimal_split(x, cols = 2, ratio = 0.5),
    NULL)
  v %||% identity
}

.ts_verbs_check <- function(cs) {
  apply_in <- function(ord) {
    x <- rtftable(.ts_split_data())
    for (f in ord) x <- .ts_verb(f, cs[[f]])(x)
    x
  }
  ords <- list(c("zone", "header", "cols", "body", "split"),
               c("split", "body", "cols", "header", "zone"),
               c("cols", "split", "zone", "body", "header"))
  txt <- vapply(ords, function(o) .ts_body(apply_in(o)), "")
  bad <- character(0)
  if (!identical(txt[2L], txt[1L]) || !identical(txt[3L], txt[1L])) bad <- "order"
  rows <- .ts_raw(apply_in(ords[[1L]]))
  # header cell A
  h <- rows[[1L]]
  want_hb <- if (cs$header == "border") "dot" else if (cs$cols == "border") "dash" else
    if (cs$zone == "hdr") "db" else "s"
  if (.ts_side(h$def[2L], "b") != want_hb) bad <- c(bad, "header bottom")
  if (.ts_text(h$ct[2L]) != if (cs$header == "label") "Arm" else "A") bad <- c(bad, "header label")
  if (.ts_q(h$ct[2L]) != if (cs$header == "align") "l" else "c") bad <- c(bad, "header align")
  if (grepl("\\\\b ", h$ct[2L]) != (cs$header == "bold" || cs$cols == "hbold")) bad <- c(bad, "header bold")
  # body row 1, cell A: one cell, or two halves
  r1 <- rows[[2L]]
  ia <- if (cs$split == "on") 2:3 else 2L
  if (length(r1$def) != if (cs$split == "on") 4L else 3L) return(c(bad, "split cells"))
  want_q <- if (cs$split == "on") c("r", "l") else
    if (cs$body == "align") "l" else if (cs$cols == "align") "r" else "c"
  if (!identical(.ts_q(r1$ct[ia]), want_q)) bad <- c(bad, "body align")
  want_txt <- if (cs$split == "on") c("1", ".25") else "1.25"
  if (!identical(.ts_text(r1$ct[ia]), want_txt)) bad <- c(bad, "body text")
  if (any(grepl("\\\\b ", r1$ct[ia]) != (cs$body == "bold" || cs$cols == "bold"))) bad <- c(bad, "body bold")
  if (any(grepl("\\\\cf([02-9]|[1-9][0-9])", r1$ct[ia]) != (cs$body == "color"))) bad <- c(bad, "body color")
  if (any(grepl("\\\\clcbpat", r1$def[ia]) != (cs$cols == "bg"))) bad <- c(bad, "body fill")
  want_b1 <- if (cs$body == "border") "dot" else if (cs$zone == "ih") "s" else ""
  if (any(.ts_side(r1$def[ia], "b") != want_b1)) bad <- c(bad, "body bottom")
  # row 2 ("NE"): one cell over A, aligned by the column
  r2 <- rows[[3L]]
  if (length(r2$def) != 3L || .ts_q(r2$ct[2L]) != if (cs$cols == "align") "r" else "c") {
    bad <- c(bad, "unsplit row")
  }
  # row 3: the last row's bottom
  r3 <- rows[[4L]]
  if (any(.ts_side(r3$def, "b") != if (cs$zone == "last") "th" else "")) bad <- c(bad, "last bottom")
  bad
}

test_that("FT-TB975 [FR-IX397 FR-IX330 FR-IX331 FR-IX332 FR-TB408 FR-TB422 FR-TB430 FR-TB363]: pairwise -- the style verbs in any order, on overlapping levels of one column", {
  levels <- list(zone = c("none", "hdr", "ih", "last"),
                 header = c("none", "align", "label", "border", "bold"),
                 cols = c("none", "align", "border", "bold", "bg", "hbold"),
                 body = c("none", "bold", "align", "border", "color"),
                 split = c("none", "on"))
  # #872 (promotion freezes the header styling) and one shared field
  out <- function(header, cols) header %in% c("border", "bold") & cols == "hbold"
  cand <- do.call(expand.grid, c(levels, stringsAsFactors = FALSE))
  cand <- cand[!out(cand$header, cand$cols), ]
  rownames(cand) <- NULL
  all_pairs <- .ts_pairs(levels, function(p, a, b) {
    if (identical(p, c("header", "cols"))) return(!out(a, b))
    rep(TRUE, length(a))
  })
  ap <- .allpairs(cand)
  expect_setequal(ap$need, all_pairs)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 40L)
  for (i in seq_len(nrow(ap$rows))) {
    cs <- as.list(ap$rows[i, ])
    expect_identical(.ts_verbs_check(cs), character(0), label = paste(unlist(cs), collapse = " / "))
  }
})

test_that("FT-TB976 [FR-IX398 FR-IX332 FR-TB439 FR-TB430 FR-TB408 FR-TB345]: decision table -- a later call of a verb on the same target against an earlier one", {
  t0 <- rtftable(.ts_split_data())
  b <- function(...) rtf_border(...)
  z <- function(x, ...) style_zone(x, ...)
  bo <- function(x, ...) style_body(x, rows = 1, cols = 2, ...)
  he <- function(x, ...) style_header(x, row = 1, cols = 2, ...)
  co <- function(x, ...) style_cols(x, cols = 2, ...)
  dt <- list(
    list(t0 |> z(header = b(top = "double")) |> z(header = b(bottom = "dot")),
         t0 |> z(header = b(top = "double", bottom = "dot"))),
    list(t0 |> z(header = b(bottom = "dot")) |> z(header = b(bottom = "double")),
         t0 |> z(header = b(bottom = "double"))),
    list(t0 |> z(header = b(bottom = "dot")) |> z(header = b(bottom = "none")),
         t0 |> z(header = b(bottom = "none"))),
    list(t0 |> z(header = b(bottom = "dot")) |> z(header = b()),
         t0 |> z(header = b(bottom = "dot"))),
    list(t0 |> bo(border = b(bottom = "dot")) |> bo(border = b(top = "double")),
         t0 |> bo(border = b(top = "double", bottom = "dot"))),
    list(t0 |> bo(border = b(bottom = "dot")) |> bo(border = b(bottom = "none")),
         t0 |> bo(border = b(bottom = "none"))),
    list(t0 |> he(border = b(bottom = "dot")) |> he(border = b(bottom = "double")),
         t0 |> he(border = b(bottom = "double"))),
    list(t0 |> co(border = b(bottom = "dot")) |> co(border = b(top = "double")),
         t0 |> co(border = b(top = "double", bottom = "dot"))),
    list(t0 |> co(align = "left") |> co(align = "right"), t0 |> co(align = "right")),
    list(t0 |> bo(align = "left") |> bo(align = "right"), t0 |> bo(align = "right")),
    list(t0 |> he(label = "X") |> he(label = "Y"), t0 |> he(label = "Y")),
    list(t0 |> he(align = "left") |> co(header_align = "right"), t0 |> co(header_align = "right")),
    list(t0 |> co(header_align = "right") |> he(align = "left"), t0 |> he(align = "left")),
    list(t0 |> co(bold = TRUE) |> co(bold = FALSE), t0),
    list(t0 |> bo(color = "#FF0000") |> bo(color = "#0000FF"), t0 |> bo(color = "#0000FF")),
    list(t0 |> set_decimal_split(cols = 2, ratio = 0.3) |> set_decimal_split(cols = 2, ratio = 0.6),
         t0 |> set_decimal_split(cols = 2, ratio = 0.6)),
    list(t0 |> set_decimal_split(cols = 2) |> set_decimal_split(cols = NULL), t0))
  for (k in seq_along(dt)) {
    expect_identical(.ts_body(dt[[k]][[1L]]), .ts_body(dt[[k]][[2L]]), label = sprintf("row %d", k))
  }
  # and the rows say something: each pair differs from the plain table but
  # the two that undo themselves (rows 14 and 17) and row 6, which erases a
  # rule the plain table does not have
  plain <- .ts_body(t0)
  differs <- vapply(dt, function(d) !identical(.ts_body(d[[1L]]), plain), NA)
  expect_identical(which(!differs), c(6L, 14L, 17L))
})

test_that("FT-TB977 [FR-IX397 FR-IX331 FR-TB439 FR-TB422 FR-IX338]: decision table -- zones and levels that meet on the same cell", {
  df <- .ts_data()
  b <- function(...) rtf_border(...)
  bots <- function(tbl) vapply(.ts_raw(tbl)[-1L], function(r) .ts_side(r$def[1L], "b"), "")
  t0 <- rtftable(df)
  # body inside_h against last_row / first_row
  expect_identical(bots(t0 |> style_zone(body = b(inside_h = TRUE), last_row = b(bottom = "thick"))),
                   c("s", "s", "th"))
  expect_identical(bots(t0 |> style_zone(body = b(inside_h = TRUE), first_row = b(bottom = "double"))),
                   c("db", "s", ""))
  # one data row: it is the first and the last row; last_row is applied last
  one <- rtftable(df[1L, ]) |> style_zone(first_row = b(bottom = "double"), last_row = b(bottom = "thick"))
  expect_identical(bots(one), "th")
  # a zone named on its own wins over the frame's edge
  fr <- rtftable(df, border = b(all = TRUE), col_header = list()) |>
    style_zone(first_row = b(top = "double"))
  expect_identical(.ts_side(.ts_raw(fr)[[1L]]$def, "t"), rep("db", 3L))
  # a cell's border erases the zone's on the cells it names only
  er <- t0 |> style_zone(last_row = b(bottom = "thick")) |>
    style_body(rows = 3, cols = 2, border = b(bottom = "none"))
  expect_identical(.ts_side(.ts_raw(er)[[4L]]$def, "b"), c("th", "", "th"))
  # style_cols(border = ) is the column's header cell, never its body
  cb <- .ts_raw(t0 |> style_cols(cols = 2, border = b(bottom = "dot")))
  expect_identical(.ts_side(cb[[1L]]$def, "b"), c("s", "dot", "s"))
  expect_identical(unlist(lapply(cb[-1L], function(r) .ts_side(r$def, "b"))), rep("", 9L))
  # header cell: the cell over the column over the zone, in either order
  hz <- function(x) style_zone(x, header = b(bottom = "double"))
  hc <- function(x) style_cols(x, cols = 2, border = b(bottom = "dash"))
  hh <- function(x) style_header(x, row = 1, cols = 2, border = b(bottom = "dot"))
  a1 <- .ts_raw(hh(hc(hz(t0))))[[1L]]
  a2 <- .ts_raw(hz(hc(hh(t0))))[[1L]]
  expect_identical(.ts_side(a1$def, "b"), c("db", "dot", "db"))
  expect_identical(a2$def, a1$def)
})

.ts_split_check <- function(cs, W) {
  a <- list(.ts_split_data(),
            border = if (cs$border == "grid") rtf_border(all = TRUE, inside_h = TRUE, inside_v = TRUE) else "tfl")
  if (cs$header == "span") {
    a$col_header <- list(list(list(from = 2, to = 3, label = "Trt")), c("P", "A", "B"))
  }
  switch(cs$widths,
         rel = a$col_rel_width <- c(2, 1, 1),
         twips = a$column_widths_twips <- c(3000, 1500, 1500),
         pct = a$table_width_pct <- 50)
  if (cs$pad == "lr") {
    a$cell_padding_left_twips <- 40L
    a$cell_padding_right_twips <- 30L
  }
  if (cs$align == "colspec") a$col_spec <- list(list(col = 2, align = "right"))
  x <- do.call(rtftable, a)
  if (cs$align %in% c("cols", "body")) x <- style_cols(x, cols = 2, align = "left")
  if (cs$align == "body") x <- style_body(x, cols = 2, align = "right")
  ratio <- switch(cs$ratio, auto = NULL, as.numeric(cs$ratio))
  base <- .ts_raw(x)
  got <- .ts_raw(set_decimal_split(x, cols = 2, ratio = ratio))
  bad <- character(0)
  nh <- if (cs$header == "span") 2L else 1L
  if (!identical(got[seq_len(nh)], base[seq_len(nh)])) bad <- c(bad, "header")
  cx <- base[[nh + 1L]]$cellx
  # row 2 ("NE"): as unsplit, aligned by the column (or the cell)
  ne <- got[[nh + 2L]]
  want_ne <- switch(cs$align, colspec = "r", cols = "l", body = "r", "c")
  if (!identical(ne$cellx, cx) || .ts_q(ne$ct[2L]) != want_ne) bad <- c(bad, "unsplit row")
  pl <- if (cs$pad == "lr") 40L else 0L
  pr <- if (cs$pad == "lr") 30L else 0L
  for (k in c(1L, 3L)) {
    r <- got[[nh + k]]
    tag <- function(x) sprintf("%s row %d", x, k)
    if (length(r$cellx) != 4L || !identical(r$cellx[-2L], cx)) {
      bad <- c(bad, tag("cellx"))
      next
    }
    xs <- r$cellx[2L]
    if (cs$ratio == "auto") {
      if (xs <= cx[1L] || xs >= cx[2L]) bad <- c(bad, tag("split point"))
    } else if (xs != cx[1L] + as.integer(round((cx[2L] - cx[1L]) * as.numeric(cs$ratio)))) {
      bad <- c(bad, tag("split point"))
    }
    want <- c(sprintf("\\qr\\li%d\\ri0", pl), sprintf("\\ql\\li0\\ri%d", pr))
    if (!all(startsWith(r$ct[2:3], want))) bad <- c(bad, tag("halves"))
    want_txt <- if (k == 1L) c("1", ".25") else c("3", ".0")
    if (!identical(.ts_text(r$ct[2:3]), want_txt)) bad <- c(bad, tag("text"))
    if (cs$border == "grid") {
      lr <- c(nzchar(.ts_side(r$def, "l")), nzchar(.ts_side(r$def, "r")))
      if (!identical(lr, c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE))) bad <- c(bad, tag("edges"))
    }
  }
  if (utils::tail(cx, 1L) != switch(cs$widths, twips = 6000L, pct = as.integer(round(W * 0.5)), W)) {
    bad <- c(bad, "width")
  }
  bad
}

test_that("FT-TB978 [FR-IX399 FR-IX330 FR-TB363 FR-TB364 FR-TB349 FR-TB360 FR-TB548 FR-TB568]: pairwise -- set_decimal_split() x widths x alignment x ratio x padding x frame x header", {
  levels <- list(widths = c("default", "rel", "twips", "pct"),
                 align = c("default", "colspec", "cols", "body"),
                 ratio = c("0.3", "0.5", "auto"), pad = c("none", "lr"),
                 border = c("tfl", "grid"), header = c("names", "span"))
  cand <- do.call(expand.grid, c(levels, stringsAsFactors = FALSE))
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, .ts_pairs(levels, function(p, a, b) rep(TRUE, length(a))))
  expect_lt(nrow(ap$rows), 25L)
  W <- utils::tail(.ts_render(rtftable(.ts_data()))[[1L]]$cellx, 1L)
  for (i in seq_len(nrow(ap$rows))) {
    cs <- as.list(ap$rows[i, ])
    expect_identical(.ts_split_check(cs, W), character(0), label = paste(unlist(cs), collapse = " / "))
  }
})
