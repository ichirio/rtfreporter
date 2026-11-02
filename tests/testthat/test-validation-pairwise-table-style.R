# Validation tests: rtftable() -- how its styling arguments act together:
# border x header rows x stub x column widths x cell padding x row height x
# alignment x blank rows -- a pairwise set checked against the RTF control
# words written for every row, a decision table of the frame against the
# header, and a decision table of where a style value comes from.  Helpers
# prefixed .ts_.  Fictional data only.
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
#             (FR-TB567).
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
#  Constraints: no blank row with box or grid (#869: a blank row is
#  written bare, so it breaks the frame's left and right edges, and a
#  trailing one falls outside the bottom edge); no spanning row with "none"
#  (#870: the group underline is drawn under "none" too).  The valid
#  product is 18,144 rows; the candidates are every 89th of them
#  (expand.grid order, 204 rows), checked to hold every valid pair of levels,
#  and .allpairs() picks the rows covering every pair (25).  Oracle: each row's \trrh, \cellx, \clbrdr[tblr], \clvertal,
#  \clpadl / \clpadb, \q, \li / \ri and text, from the rules above.
#  FR-TB545 FR-TB547 FR-TB548 FR-TB552 FR-TB553 FR-TB554 FR-TB555 FR-TB564
#  FR-TB565 FR-TB566 FR-TB567 FR-TB568 FR-TB570 FR-TB457 FR-TB458 FR-IX338
#  FR-IX339 FR-TB614.
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
  blank <- list("")
  seq_txt <- c(hdr_txt,
               if (cs$blank == "ends") blank, data_txt[1L],
               if (cs$blank == "inside") blank, data_txt[2:3],
               if (cs$blank == "ends") blank)
  got_txt <- lapply(rows, `[[`, "text")
  if (!identical(got_txt, seq_txt)) return("rows")
  nh <- length(hdr_txt)
  is_blank <- vapply(seq_txt, function(x) identical(x, ""), NA)
  kind <- ifelse(is_blank, "blank", ifelse(seq_along(rows) <= nh, "header", "data"))
  kind[cs$header == "span" & seq_along(rows) == 1L] <- "span"
  ruled <- which(kind != "blank")
  first <- ruled[1L]
  last <- utils::tail(ruled, 1L)
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
    want_cx <- switch(k, blank = cx[3L], span = cx[c(1L, 3L)], cx)
    if (!identical(r$cellx, as.integer(want_cx))) bad <- c(bad, tag("cellx"))
    n <- length(r$cellx)
    if (k == "blank") {
      if (any(r$t | r$b | r$l | r$r) || any(r$padt > 0L) || any(r$padb > 0L)) {
        bad <- c(bad, tag("blank bare"))
      }
      next
    }
    # borders
    z <- rep(FALSE, n)
    wt <- wb <- wl <- wr <- z
    if (cs$border == "tfl") {
      if (i == first) wt[] <- TRUE
      if (i == nh) wb[] <- TRUE
    } else if (cs$border %in% c("box", "grid")) {
      if (i == first) wt[] <- TRUE
      if (i == last || cs$border == "grid") wb[] <- TRUE
      wl[1L] <- TRUE
      wr[n] <- TRUE
      if (cs$border == "grid") wl[] <- wr[] <- TRUE
    }
    # the group underline under the spanning cell the label row subdivides
    if (k == "span") wb[2L] <- TRUE
    if (!identical(list(r$t, r$b, r$l, r$r), list(wt, wb, wl, wr))) bad <- c(bad, tag("borders"))
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

test_that("FT-TB785 [FR-TB545 FR-TB547 FR-TB548 FR-TB552 FR-TB553 FR-TB554 FR-TB555 FR-TB564 FR-TB565 FR-TB566 FR-TB567 FR-TB568 FR-TB570 FR-TB457 FR-TB458 FR-IX338 FR-IX339 FR-TB614]: pairwise -- border x header x stub x widths x padding x height x alignment x valign x blank rows", {
  levels <- list(border = c("tfl", "none", "box", "grid"),
                 header = c("names", "two", "span"), stub = c("col1", "none"),
                 widths = c("default", "rel", "twips", "pct"),
                 pad = c("none", "lr", "tb", "all"),
                 height = c("default", "exact", "min"),
                 halign = c("default", "data", "header"),
                 valign = c("bottom", "top", "center"),
                 blank = c("none", "inside", "ends"))
  framed <- function(border, blank) border %in% c("box", "grid") & blank != "none"
  bare_span <- function(border, header) border == "none" & header == "span"
  cand <- do.call(expand.grid, c(levels, stringsAsFactors = FALSE))
  cand <- cand[!framed(cand$border, cand$blank) & !bare_span(cand$border, cand$header), ]
  expect_identical(nrow(cand), 18144L)
  cand <- cand[seq(1L, nrow(cand), by = 89L), ]
  rownames(cand) <- NULL
  all_pairs <- .ts_pairs(levels, function(p, a, b) {
    if (identical(p, c("border", "blank"))) return(!framed(a, b))
    if (identical(p, c("border", "header"))) return(!bare_span(a, b))
    rep(TRUE, length(a))
  })
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
