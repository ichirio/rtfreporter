## tests/testthat/test-promoted-header.R
##
## A labels row promoted to cells -- a gt column label with a tab_style()
## border becomes a row of {from, to, label, promoted = TRUE} cells -- and
## the verbs that address the labels row by column: as_rtftables(stub =
## stub_spec(label = )), style_header() and a named set_col_header() (#1041).

library(testthat)

# Fictional data: a stub (rowname), a label column and two arms; the
# "Placebo" column label carries a bottom border, which promotes the row.
.t76_df <- data.frame(stat = c("n", "Mean"), lbl = c("Age", "Age"),
                      Placebo = c("10", "2.5"), Active = c("12", "3.1"))
.t76_gt <- function(border = TRUE) {
  g <- gt::gt(.t76_df, rowname_col = "stat")
  if (border) {
    g <- gt::tab_style(g, gt::cell_borders(sides = "bottom"),
                       gt::cells_column_labels("Placebo"))
  }
  g
}

# The RTF lines of the pages, written with border = "none" by the caller so
# the only rule in the header is the one carried from gt.
.t76_rtf <- function(pages) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(rtf_tables(rtf_document(), pages), f)
  readLines(f, warn = FALSE)
}

# Of the first table row that holds `label`: does each cell draw a bottom
# rule?  The cell definitions are the pieces before each \cellx.
.t76_bottom <- function(rtf, label) {
  line <- rtf[grepl(paste0(label, "\\cell"), rtf, fixed = TRUE)][1L]
  n <- lengths(regmatches(line, gregexpr("\\cellx", line, fixed = TRUE)))
  defs <- strsplit(line, "\\cellx", fixed = TRUE)[[1L]][seq_len(n)]
  grepl("\\clbrdrb", defs, fixed = TRUE)
}

.t76_labels <- function(row) vapply(row, function(cell) cell$label, "")
.t76_spans <- function(row) {
  vapply(row, function(cell) paste(cell$from, cell$to, sep = "-"), "")
}

test_that("FT-TB7601 [FR-TB197]: stub_cols(label = ) names the stub column; through stub_spec(label = ) it heads the stub on a gt labels row promoted to cells", {
  skip_if_not_installed("gt")
  d <- data.frame(soc = c("Cardiac", "Cardiac"), pt = c("AF", "MI"),
                  n = c("1", "2"))
  # label = NULL joins the display names, a label attribute when there is one
  expect_identical(names(stub_cols(d, vars = c("soc", "pt")))[1L], "soc / pt")
  dl <- d
  attr(dl$soc, "label") <- "System Organ Class"
  expect_identical(names(stub_cols(dl, vars = c("soc", "pt")))[1L],
                   "System Organ Class / pt")
  # label = names the merged stub column
  expect_identical(names(stub_cols(d, vars = c("soc", "pt"),
                                   label = "Term"))[1L], "Term")

  # a gt labels row promoted to cells: the stub's cell takes the label, the
  # other cells keep their labels and the carried border
  st <- stub_spec(c("rowname", "lbl"), label = "Parameter")
  plain <- as_rtftables(.t76_gt(FALSE), stub = st)[[1L]]
  pages <- as_rtftables(.t76_gt(), stub = st, border = "none")
  p <- pages[[1L]]
  expect_identical(plain$col_header, list(c("Parameter", "Placebo", "Active")))
  expect_length(p$col_header, 1L)
  row <- p$col_header[[1L]]
  expect_identical(.t76_labels(row), c("Parameter", "Placebo", "Active"))
  expect_identical(.t76_spans(row), c("1-1", "2-2", "3-3"))
  expect_true(all(vapply(row, function(cell) isTRUE(cell$promoted), NA)))
  expect_identical(row[[2L]]$border$bottom$style, "single")
  rtf <- .t76_rtf(pages)
  expect_true(any(grepl("Parameter\\cell", rtf, fixed = TRUE)))
  expect_identical(.t76_bottom(rtf, "Parameter"), c(FALSE, TRUE, FALSE))
})

test_that("FT-TB7602 [FR-TB425]: style_header() patches the targeted cells of the targeted rows, a gt labels row promoted to cells and a spanning promoted cell included", {
  skip_if_not_installed("gt")
  pages <- as_rtftables(.t76_gt(), border = "none")
  before <- pages
  out <- style_header(pages, bold = TRUE)
  # a modified copy, of every page of the list
  expect_identical(pages, before)
  expect_type(out, "list")
  expect_s3_class(out[[1L]], "rtftable")
  expect_s3_class(style_header(pages[[1L]], bold = TRUE), "rtftable")
  # every cell bold; each keeps its label, span and border
  row <- out[[1L]]$col_header[[1L]]
  expect_true(all(vapply(row, function(cell) isTRUE(cell$bold), NA)))
  expect_identical(.t76_labels(row), c("", "lbl", "Placebo", "Active"))
  expect_identical(.t76_spans(row), c("1-1", "2-2", "3-3", "4-4"))
  expect_identical(row[[3L]]$border$bottom$style, "single")
  rtf <- .t76_rtf(out)
  expect_true(any(grepl("\\b Placebo\\b0", rtf, fixed = TRUE)))
  expect_identical(.t76_bottom(rtf, "\\b Placebo\\b0 "),
                   c(FALSE, FALSE, TRUE, FALSE))
  # cols = targets the cells it names
  one <- style_header(pages, cols = "Placebo", italic = TRUE)[[1L]]$col_header[[1L]]
  expect_identical(vapply(one, function(cell) isTRUE(cell$italic), NA),
                   c(FALSE, FALSE, TRUE, FALSE))

  # a spanning promoted cell under a label row: row = picks the row, a cell
  # is targeted when its span intersects cols, and the label row's styling
  # reaches the spanning cell through the columns it spans
  pc <- function(f, t, l) list(from = f, to = t, label = l, promoted = TRUE)
  tb <- rtftable(.t76_df, border = "none", col_header = list(
    c("", "", "Arm A", "Arm B"),
    list(pc(1L, 2L, "Parameter"), pc(3L, 3L, "Placebo"), pc(4L, 4L, "Active"))))
  s2 <- style_header(tb, row = 2L, cols = "lbl", underline = TRUE)
  expect_identical(s2$col_header[[1L]], tb$col_header[[1L]])
  expect_identical(vapply(s2$col_header[[2L]], function(cell) isTRUE(cell$underline), NA),
                   c(TRUE, FALSE, FALSE))
  expect_identical(.t76_spans(s2$col_header[[2L]]), c("1-2", "3-3", "4-4"))
  s1 <- style_header(tb, row = 1L, cols = "lbl", bold = TRUE)
  expect_true(isTRUE(s1$col_spec[[2L]]$header_bold))
  expect_identical(vapply(s1$col_header[[2L]], function(cell) isTRUE(cell$bold), NA),
                   c(TRUE, FALSE, FALSE))
  expect_identical(.t76_spans(s1$col_header[[2L]]), c("1-2", "3-3", "4-4"))
  expect_true(any(grepl("\\b Parameter\\b0", .t76_rtf(list(s1)), fixed = TRUE)))
})

test_that("FT-TB7603 [FR-TB215]: a named set_col_header() row is a patch; on a gt labels row promoted to cells it renames only the named columns' cells", {
  skip_if_not_installed("gt")
  tbl <- rtftable(data.frame(row_label = "A", g1 = 1, g2 = 2, Total = 3))
  # all named: each to its column, shorter than the table, the rest their names
  expect_identical(set_col_header(tbl, c(g1 = "Low"))$col_header,
                   list(c("row_label", "Low", "g2", "Total")))
  # unnamed: one label per printed column
  expect_identical(set_col_header(tbl, c("Item", "L", "H", "T"))$col_header,
                   list(c("Item", "L", "H", "T")))
  # an unnamed value that is a column name goes to that column
  expect_identical(set_col_header(tbl, c("Item", "Total", "Mid", g1 = "Low"))$col_header,
                   list(c("Item", "Low", "Mid", "Total")))

  # a gt labels row promoted to cells: the named cell renamed, its border
  # kept; the other cells keep their labels
  pages <- as_rtftables(.t76_gt(), border = "none")
  out <- set_col_header(pages, c(Placebo = "PBO"))
  expect_length(out[[1L]]$col_header, 1L)
  row <- out[[1L]]$col_header[[1L]]
  expect_identical(.t76_labels(row), c("", "lbl", "PBO", "Active"))
  expect_identical(.t76_spans(row), c("1-1", "2-2", "3-3", "4-4"))
  expect_identical(row[[3L]]$border$bottom$style, "single")
  expect_null(row[[4L]]$border)
  rtf <- .t76_rtf(out)
  expect_false(any(grepl("rowname\\cell", rtf, fixed = TRUE)))
  expect_identical(.t76_bottom(rtf, "PBO"), c(FALSE, FALSE, TRUE, FALSE))
  # an unknown name stops as on a plain row
  expect_error(set_col_header(pages, c(Plaecbo = "PBO")),
               "unknown column name \"Plaecbo\"")

  # a spanning promoted cell keeps its span when another column is renamed
  pc <- function(f, t, l) list(from = f, to = t, label = l, promoted = TRUE)
  tb <- rtftable(.t76_df, col_header = list(
    list(pc(1L, 2L, "Parameter"), pc(3L, 3L, "Placebo"), pc(4L, 4L, "Active"))))
  sp <- set_col_header(tb, c(Active = "ACT"))$col_header[[1L]]
  expect_identical(.t76_labels(sp), c("Parameter", "Placebo", "ACT"))
  expect_identical(.t76_spans(sp), c("1-2", "3-3", "4-4"))
  # a named column inside the span renames the whole span
  sp <- set_col_header(tb, c(lbl = "Term"))$col_header[[1L]]
  expect_identical(.t76_labels(sp), c("Term", "Placebo", "Active"))
  expect_identical(.t76_spans(sp), c("1-2", "3-3", "4-4"))
  # two names on one span stop
  expect_error(set_col_header(tb, c(stat = "S", lbl = "Term")),
               "the header cell over columns 1-2 is targeted twice -- name \"stat\" and name \"lbl\"",
               fixed = TRUE)

  # the header is replaced by the one row: a spanner row above is dropped
  gs <- gt::tab_spanner(.t76_gt(), "Treatment", columns = c("Placebo", "Active"))
  ps <- as_rtftables(gs)
  expect_length(ps[[1L]]$col_header, 2L)
  hs <- set_col_header(ps, c(Placebo = "PBO"))[[1L]]$col_header
  expect_length(hs, 1L)
  expect_identical(.t76_labels(hs[[1L]]), c("", "lbl", "PBO", "Active"))
  expect_identical(hs[[1L]][[3L]]$border$bottom$style, "single")
})
