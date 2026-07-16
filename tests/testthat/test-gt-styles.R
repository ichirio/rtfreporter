## tests/testthat/test-gt-styles.R
##
## read_meta "styles" token (#223): explicit gt tab_style() borders and text
## styles are carried into the rtftable -- spanner cells, column labels, body
## and stub cells -- and the style_*() verbs still override what was read.
## All tests skip when gt is absent.

library(testthat)

.gs_df <- function() {
  data.frame(Item = c("Age", "Sex"), P = c("1", "2"),
             A = c("3", "4"), B = c("5", "6"), stringsAsFactors = FALSE)
}

# A gt table with one spanner and a batch of explicit tab_style() calls.
.gs_gt <- function() {
  gt::gt(.gs_df(), rowname_col = "Item") |>
    gt::tab_spanner(label = "(N = 254)", columns = c(P, A, B), id = "n254") |>
    gt::tab_style(
      style = list(
        gt::cell_borders(sides = "top", style = "solid", weight = gt::px(1)),
        gt::cell_borders(sides = "bottom", style = "hidden"),
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_column_spanners(spanners = "n254")
    ) |>
    gt::tab_style(style = gt::cell_text(weight = "bold", align = "center"),
                  locations = gt::cells_column_labels(columns = P)) |>
    gt::tab_style(style = gt::cell_borders(sides = "bottom", style = "dashed",
                                           weight = gt::px(2)),
                  locations = gt::cells_body(columns = A, rows = 1)) |>
    gt::tab_style(style = gt::cell_text(color = "#FF0000", style = "italic"),
                  locations = gt::cells_body(columns = B, rows = 2)) |>
    gt::tab_style(style = gt::cell_text(weight = "bold"),
                  locations = gt::cells_stub(rows = 1))
}

.gs_rtf <- function(tbl) {
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(rtf_document() |> rtf_tables(list(tbl)), f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# ── spanner cells ─────────────────────────────────────────────────────────────

test_that("spanner borders and bold are carried onto the header cell", {
  skip_if_not_installed("gt")
  tbl <- as_rtftable(.gs_gt(), read_meta = TRUE)
  # header rows: 1 = spanner row, 2 = labels row
  span <- tbl$col_header[[1]]
  cell <- span[[which(vapply(span, function(c) c$label == "(N = 254)",
                             logical(1)))]]
  expect_equal(cell$border$top$style, "single")
  expect_equal(cell$border$top$width, 15L)
  expect_equal(cell$border$bottom$style, "none")
  expect_true(isTRUE(cell$bold))
})

test_that("the carried 'bottom hidden' erases the automatic group underline", {
  skip_if_not_installed("gt")
  count_bot <- function(tbl) {
    rows <- unlist(strsplit(.gs_rtf(tbl), "\\\\trowd"))[-1]
    vapply(rows, function(r)
      length(regmatches(r, gregexpr("\\\\clbrdrb\\\\brdrs", r))[[1]]),
      integer(1L), USE.NAMES = FALSE)
  }
  ctrl <- as_rtftable(.gs_gt(), read_meta = c("col_header", "spanning"))
  pat  <- as_rtftable(.gs_gt(), read_meta = TRUE)
  expect_equal(count_bot(ctrl)[1], 1L)   # tfl auto underline under the span
  expect_equal(count_bot(pat)[1],  0L)   # carried "hidden" erased it
})

# ── column labels ─────────────────────────────────────────────────────────────

test_that("column-label text styles land in col_spec header_*", {
  skip_if_not_installed("gt")
  tbl <- as_rtftable(.gs_gt(), read_meta = TRUE)
  j <- match("P", names(tbl$data))
  expect_true(tbl$col_spec[[j]]$header_bold)
  expect_equal(tbl$col_spec[[j]]$header_align, "center")
  expect_false(isTRUE(tbl$col_spec[[j + 1L]]$header_bold))
  # no border/underline on labels -> the labels row is NOT promoted
  expect_true(is.character(tbl$col_header[[2]]))
})

test_that("a border on a column label promotes the labels row", {
  skip_if_not_installed("gt")
  g <- gt::gt(.gs_df()) |>
    gt::tab_style(style = gt::cell_borders(sides = "bottom", style = "double"),
                  locations = gt::cells_column_labels(columns = A))
  tbl <- as_rtftable(g, read_meta = TRUE)
  row <- tbl$col_header[[1]]
  expect_false(is.character(row))                       # promoted
  j <- match("A", names(tbl$data))
  expect_equal(row[[j]]$border$bottom$style, "double")
  expect_null(row[[j - 1L]]$border)
  # labels survived the promotion
  expect_equal(vapply(row, `[[`, character(1), "label"),
               c("Item", "P", "A", "B"))
})

# ── body and stub cells ───────────────────────────────────────────────────────

test_that("body and stub styles land in cell_styles at the right cells", {
  skip_if_not_installed("gt")
  tbl <- as_rtftable(.gs_gt(), read_meta = TRUE)
  nms <- names(tbl$data)
  jA <- match("A", nms); jB <- match("B", nms)
  # body border: col A, data row 1 (dashed 2px -> dash, 30 twips)
  b <- tbl$cell_styles[[1]]$border[[jA]]
  expect_equal(b$bottom$style, "dash")
  expect_equal(b$bottom$width, 30L)
  # body text: col B, row 2 -> italic + red
  expect_true(tbl$cell_styles[[2]]$italic[jB])
  expect_equal(tbl$cell_styles[[2]]$color[jB], "#FF0000")
  # stub: row 1, first (rowname) column -> bold
  expect_true(tbl$cell_styles[[1]]$bold[1])
  expect_true(is.na(tbl$cell_styles[[1]]$bold[jB]))
})

test_that("grouped tables map _data rownum to the rendered row", {
  skip_if_not_installed("gt")
  df <- data.frame(grp = c("G1", "G2", "G1"), Item = c("a", "b", "c"),
                   P = 1:3, stringsAsFactors = FALSE)
  g <- gt::gt(df, groupname_col = "grp") |>
    gt::tab_style(style = gt::cell_text(weight = "bold"),
                  locations = gt::cells_body(columns = Item, rows = 3))
  tbl <- as_rtftable(g, read_meta = TRUE)
  # data row 3 ("c", group G1) renders as body row 2 (G1 rows first)
  expect_equal(as.character(tbl$data$Item), c("a", "c", "b"))
  j <- match("Item", names(tbl$data))
  expect_true(tbl$cell_styles[[2]]$bold[j])
  expect_null(tbl$cell_styles[[3]])
})

# ── precedence and tokens ─────────────────────────────────────────────────────

test_that("style_header() overrides what the adapter read (last writer wins)", {
  skip_if_not_installed("gt")
  tbl <- as_rtftable(.gs_gt(), read_meta = TRUE) |>
    style_header(row = 1, cols = 2:4,
                 border = rtf_border(top = rtf_border_side("double")))
  span <- tbl$col_header[[1]]
  cell <- span[[which(vapply(span, function(c) c$label == "(N = 254)",
                             logical(1)))]]
  expect_equal(cell$border$top$style, "double")   # verb replaced the side
  expect_equal(cell$border$bottom$style, "none")  # untouched side survives
})

test_that("the styles token can be switched off", {
  skip_if_not_installed("gt")
  t1 <- as_rtftable(.gs_gt(), read_meta = c("col_header", "spanning"))
  span <- t1$col_header[[1]]
  expect_true(all(vapply(span, function(c) is.null(c$border), logical(1))))
  expect_null(t1$cell_styles)
  t2 <- as_rtftable(.gs_gt(), read_meta = FALSE)
  expect_null(t2$cell_styles)
})

# ── transparent borders (#226) ────────────────────────────────────────────────

test_that("transparent gt borders are invisible and therefore not carried", {
  skip_if_not_installed("gt")
  g <- gt::gt(.gs_df()) |>
    gt::tab_style(style = gt::cell_borders(sides = "bottom",
                                           color = "transparent"),
                  locations = gt::cells_body(columns = A, rows = 1)) |>
    gt::tab_style(style = gt::cell_borders(sides = "top", color = "#FFFFFF00"),
                  locations = gt::cells_column_labels(columns = B))
  tbl <- as_rtftable(g, read_meta = TRUE)
  expect_null(tbl$cell_styles)
  expect_true(is.character(tbl$col_header[[1]]))   # labels row NOT promoted
})

test_that("a plain tfrmt render carries no borders (its transparent overlay)", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tfrmt")
  dat <- expand.grid(grp = "Age", lbl = c("n", "Mean"),
                     trt = c("Placebo", "Drug"),
                     param = "value", stringsAsFactors = FALSE)
  dat$val <- seq_len(nrow(dat))
  spec <- tfrmt::tfrmt(
    group = grp, label = lbl, column = trt,
    param = param, value = val,
    body_plan = tfrmt::body_plan(
      tfrmt::frmt_structure(group_val = ".default", label_val = ".default",
                            tfrmt::frmt("xx"))))
  tbl <- as_rtftable(tfrmt::print_to_gt(spec, dat), read_meta = TRUE)
  # tfrmt's transparent overlay must not promote the labels row nor attach
  # any header-cell border.
  borders <- unlist(lapply(tbl$col_header, function(row) {
    if (is.character(row)) return(logical(0))
    vapply(row, function(cell) !is.null(cell$border), logical(1))
  }))
  expect_equal(sum(borders), 0L)
  cs_list <- if (is.null(tbl$cell_styles)) list() else tbl$cell_styles
  no_cell_border <- vapply(cs_list, function(cs) {
    is.null(cs) || is.null(cs$border)
  }, logical(1))
  expect_true(all(no_cell_border))
})

# ── gtsummary round-trip ──────────────────────────────────────────────────────

test_that("gtsummary bold_labels() is carried through as_gt()", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")
  tb <- gtsummary::tbl_summary(gtsummary::trial, include = grade) |>
    gtsummary::bold_labels()
  tbl <- as_rtftable(tb, read_meta = TRUE)
  expect_false(is.null(tbl$cell_styles))
  lab <- match("label", names(tbl$data))
  expect_true(any(vapply(tbl$cell_styles, function(cs) {
    is.list(cs) && !is.null(cs$bold) && isTRUE(cs$bold[lab])
  }, logical(1))))
})
