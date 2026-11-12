# Interaction tests: plan_style() (FR-IX407) and plan_cell_style() (FR-IX408),
# one block each that asserts every clause of the requirement's statement.
# Helpers prefixed .ix66_.  Fictional data only.

.ix66_df <- function() {
  data.frame(lab = c("n", "Mean", "SD"), A = c("5 (10.0)", "2.1", "3.3"),
             B = c("7 (12.1)", "4.5", "6.2"), stringsAsFactors = FALSE)
}

.ix66_pages <- function(p) suppressWarnings(suppressMessages(plan_apply(p, "pages")))

.ix66_zone <- function() rtf_border(bottom = TRUE, inside_h = FALSE, inside_v = FALSE)

# the direct call a plan_style() argument list stands for: the border zones
# and look fields make one rtf_table_style() passed as `style`
.ix66_direct <- function(d, args) {
  zones <- c("border_header", "border_spanning", "border_body", "border_first_row",
             "border_last_row")
  looks <- c("header_align", "header_bold", "header_italic", "align", "bold",
             "italic", "underline")
  sty <- args[intersect(names(args), c(zones, looks))]
  rest <- args[setdiff(names(args), names(sty))]
  if (length(sty)) rest$style <- do.call(rtf_table_style, sty)
  # a look-only style keeps the preset's rules, which the direct call says
  # with the preset as an object
  if (length(sty) && !any(names(sty) %in% zones)) {
    rest$border <- rtfreporter:::.normalize_table_border("tfl")
  }
  out <- suppressMessages(do.call(as_rtftables, c(list(d, read_meta = FALSE), rest)))
  if (inherits(out, "rtftable")) list(out) else out
}

# the direct call for the rules: the preset, then style_zone() with the zone
.ix66_merged <- function(d, border, zone) {
  do.call(style_zone, c(list(.ix66_direct(d, list(border = border))), zone))
}

# the value of a cell-style attribute at body row i, column j (NA: not set)
.ix66_cell <- function(pg, i, a, j) {
  v <- pg$cell_styles[[i]][[a]]
  if (is.null(v)) NA else v[[j]]
}

# the header's value of an attribute over column j
.ix66_head <- function(pg, j, a) {
  row1 <- unclass(pg$col_header)[[1L]]
  if (is.list(row1)) {
    cell <- Filter(function(z) z$from == j, row1)[[1L]]
    if (!is.null(cell[[a]])) return(cell[[a]])
  }
  pg$col_spec[[j]][[paste0("header_", a)]]
}

test_that("FT-IX6601 [FR-IX407]: plan_style(): the rules, look, widths, heights, paddings, font, placement and other settings reach as_rtftables() by name as the direct call; a value outside its limits stops when the plan is applied", {
  d <- .ix66_df()
  zone <- list(header = .ix66_zone())

  # rules: border and a border_* zone, the zone merged onto the preset's side
  # by side, in one call and across calls in either order, as the direct call
  want <- suppressWarnings(.ix66_merged(d, "tfl", zone))
  one <- plan_style(table_plan(d), border = "tfl", border_header = .ix66_zone())
  expect_equal(.ix66_pages(one), want)
  border_first <- plan_style(plan_style(table_plan(d), border = "tfl"),
                             border_header = .ix66_zone())
  expect_equal(.ix66_pages(border_first), want)
  zone_first <- plan_style(plan_style(table_plan(d), border_header = .ix66_zone()),
                           border = "tfl")
  expect_equal(.ix66_pages(zone_first), want)
  # the merge is side by side: the zone's bottom is the zone's, the preset's top stays
  tz <- .ix66_pages(plan_style(table_plan(d), border = "tfl",
                               border_header = rtf_border(bottom = "double")))[[1L]]$border
  expect_identical(tz$header$bottom$style, "double")
  expect_false(is.null(tz$header$top))

  # every other setting: the pages are as_rtftables()'s with the same
  # arguments, and each setting given is the page's value
  groups <- list(
    look = list(header_bold = TRUE, header_italic = TRUE, header_align = "right",
                align = "right", bold = TRUE, italic = TRUE, underline = TRUE),
    width_twips = list(table_width_twips = 6000),
    width_pct = list(table_width_pct = 60),
    width_frac = list(table_width_pct_of_writable = 0.5),
    heights = list(row_height_twips = 300, row_height_exact = TRUE,
                   header_row_height_twips = 400, blank_row_height_twips = 100),
    paddings = list(cell_padding_left_twips = 40, cell_padding_right_twips = 30,
                    cell_padding_top_twips = 20, cell_padding_bottom_twips = 10),
    font = list(font = "Arial", font_size_half_points = 20),
    placement = list(cell_valign = "center", table_align = "right"),
    other = list(align_count_pct = TRUE))
  page_fields <- c("row_height_twips", "row_height_exact", "header_row_height_twips",
                   "blank_row_height_twips", "cell_padding_left_twips",
                   "cell_padding_right_twips", "cell_padding_top_twips",
                   "cell_padding_bottom_twips", "font", "font_size_half_points",
                   "cell_valign", "table_align", "table_width_twips")
  for (g in names(groups)) {
    args <- groups[[g]]
    got <- .ix66_pages(do.call(plan_style, c(list(table_plan(d)), args)))
    expect_equal(got, .ix66_direct(d, args), info = g)
    for (nm in intersect(names(args), page_fields)) {
      expect_equal(got[[1L]][[nm]], args[[nm]], info = paste(g, nm))
    }
  }
  # the look fields reach the page: the header is bold, the body right-aligned
  look_pg <- .ix66_pages(plan_style(table_plan(d), header_bold = TRUE, align = "right"))[[1L]]
  expect_false(isTRUE(all.equal(look_pg, .ix66_pages(plan_style(table_plan(d)))[[1L]])))
  # the three widths: table_width_pct is a percentage, the fraction is table_width_pct_of_writable
  pct <- .ix66_pages(plan_style(table_plan(d), table_width_pct = 60))[[1L]]
  expect_equal(pct$table_width_pct_of_writable, 60 / 100)
  frac <- .ix66_pages(plan_style(table_plan(d), table_width_pct_of_writable = 0.5))[[1L]]
  expect_equal(frac$table_width_pct_of_writable, 0.5)
  # align_count_pct aligns the counts: the two cells have the same width
  cnt_df <- data.frame(p = c("Age", "Sex"), v = c("5 (10.0)", "12  (3.5)"))
  cnt <- .ix66_pages(plan_style(table_plan(cnt_df), align_count_pct = TRUE))[[1L]]
  expect_identical(nchar(cnt$data$v[1L]), nchar(cnt$data$v[2L]))

  # a value outside the limits: the plan is made, and stops when applied
  p_bad <- plan_style(table_plan(d), table_width_pct = 100.1)
  expect_s3_class(p_bad, "table_plan")
  expect_error(.ix66_pages(p_bad), "`table_width_pct` must be a number in \\(0, 100\\]")
  expect_error(.ix66_pages(plan_style(table_plan(d), row_height_twips = -1)),
               "`row_height_twips` must be a single non-negative integer")
})

test_that("FT-IX6602 [FR-IX408]: plan_cell_style(): columns, the header and a condition take their attributes, a formula computes them row by row, styles reach only the named cells, and a later call on the same place gives the final value of what it names", {
  d <- .ix66_df()
  vals <- list(bold = TRUE, italic = TRUE, underline = TRUE, align = "right",
               color = "#CC0000", background = "#EEEEEE", indent_twips = 200)
  cs <- function(...) {
    .ix66_pages(plan_cell_style(table_plan(d), ...))[[1L]]
  }
  base <- .ix66_pages(plan_style(table_plan(d)))[[1L]]
  hdr_msg <- "plan_cell_style(header = TRUE) takes bold, italic, underline, align, border, color and background."

  # whole columns take bold, italic, underline, align, color, background,
  # indent_twips and border (the border on the column, FR-PL409); only the named column
  for (a in names(vals)) {
    pg <- do.call(cs, stats::setNames(list("A", vals[[a]]), c("cols", a)))
    expect_equal(pg$col_spec[[2L]][[a]], vals[[a]], info = paste("cols", a))
    expect_identical(pg$col_spec[[3L]][[a]], base$col_spec[[3L]][[a]], info = paste("cols other", a))
  }
  pg <- cs(cols = "A", border = rtf_border(bottom = TRUE))
  expect_false(is.null(pg$col_spec[[2L]]$border))
  expect_null(pg$col_spec[[3L]]$border)

  # the header takes bold, italic, underline, align, border, color and background
  for (a in c("bold", "italic", "underline", "align")) {
    pg <- do.call(cs, stats::setNames(list("A", TRUE, vals[[a]]), c("cols", "header", a)))
    expect_equal(.ix66_head(pg, 2L, a), vals[[a]], info = paste("header", a))
    expect_identical(.ix66_head(pg, 3L, a), .ix66_head(base, 3L, a), info = paste("header other", a))
  }
  pg <- cs(cols = "A", header = TRUE, border = rtf_border(bottom = TRUE))
  expect_false(is.null(.ix66_head(pg, 2L, "border")))
  expect_null(.ix66_head(pg, 3L, "border"))
  # color and background on every label row, so not with row =; indent_twips refused
  two <- table_plan(d) |>
    plan_col_header(rtf_col_header(list(col_cell(1, ""), col_cell(c(2, 3), "Arm")),
                                   c("lab", "A", "B")))
  pg <- .ix66_pages(plan_cell_style(two, cols = "A", header = TRUE, color = "#CC0000",
                                    background = "#EEEEEE"))[[1L]]
  expect_identical(pg$col_spec[[2L]]$header_color, "#CC0000")
  expect_identical(pg$col_spec[[2L]]$header_background, "#EEEEEE")
  expect_null(pg$col_spec[[3L]]$header_color)
  expect_error(plan_cell_style(two, header = TRUE, row = 2, color = "#CC0000"),
               "color and background")
  expect_error(plan_cell_style(two, header = TRUE, row = 2, background = "#EEEEEE"),
               "color and background")
  expect_error(plan_cell_style(two, header = TRUE, indent_twips = 200), hdr_msg, fixed = TRUE)

  # a condition takes all eight, the border on the body cells of the chosen rows
  for (a in names(vals)) {
    pg <- do.call(cs, stats::setNames(list("A", ~ lab == "n", vals[[a]]),
                                      c("cols", "where", a)))
    expect_equal(.ix66_cell(pg, 1L, a, 2L), vals[[a]], info = paste("where", a))
    expect_true(is.na(.ix66_cell(pg, 2L, a, 2L)), info = paste("where row 2", a))
    expect_true(is.na(.ix66_cell(pg, 1L, a, 3L)), info = paste("where col B", a))
  }
  pg <- cs(cols = "A", where = ~ lab == "n", border = rtf_border(bottom = TRUE))
  expect_false(is.null(pg$cell_styles[[1L]]$border[[2L]]))
  expect_null(pg$cell_styles[[1L]]$border[[3L]])
  expect_null(pg$cell_styles[[2L]]$border)
  expect_null(pg$col_spec[[2L]]$border)

  # a formula attribute is computed row by row for every column
  pg <- cs(bold = ~ ifelse(lab == "n", TRUE, NA), align = ~ ifelse(A == "2.1", "right", NA))
  expect_identical(vapply(1:3, function(j) .ix66_cell(pg, 1L, "bold", j), TRUE), c(TRUE, TRUE, TRUE))
  expect_true(all(is.na(vapply(1:3, function(j) .ix66_cell(pg, 2L, "bold", j), NA))))
  expect_identical(.ix66_cell(pg, 2L, "align", 2L), "right")
  expect_true(is.na(.ix66_cell(pg, 1L, "align", 2L)))
  # a list of formulas named by column: the named columns only
  pg <- cs(color = list(A = ~ ifelse(lab == "n", "#CC0000", NA)))
  expect_identical(.ix66_cell(pg, 1L, "color", 2L), "#CC0000")
  expect_true(is.na(.ix66_cell(pg, 1L, "color", 3L)))
  expect_true(is.na(.ix66_cell(pg, 2L, "color", 2L)))
  # a formula with `where`, or with a value attribute, stops
  expect_error(plan_cell_style(table_plan(d), bold = ~ lab == "n", where = ~ TRUE),
               "neither `where` nor `header`", fixed = TRUE)

  # a later call on the same place: the final value of the attribute it names,
  # the attributes it does not name keep the earlier value
  later <- function(p, ...) plan_cell_style(p, ...)
  p1 <- plan_cell_style(table_plan(d), cols = "A", bold = TRUE, align = "right")
  pg <- .ix66_pages(later(p1, cols = "A", align = "center"))[[1L]]
  expect_identical(pg$col_spec[[2L]]$align, "center")
  expect_true(pg$col_spec[[2L]]$bold)
  p2 <- plan_cell_style(table_plan(d), cols = "A", header = TRUE, bold = TRUE, align = "right")
  pg <- .ix66_pages(later(p2, cols = "A", header = TRUE, align = "center"))[[1L]]
  expect_identical(.ix66_head(pg, 2L, "align"), "center")
  expect_true(.ix66_head(pg, 2L, "bold"))
  p3 <- plan_cell_style(table_plan(d), where = ~ lab == "n", bold = TRUE, align = "right")
  pg <- .ix66_pages(later(p3, where = ~ lab == "n", align = "center"))[[1L]]
  expect_identical(.ix66_cell(pg, 1L, "align", 2L), "center")
  expect_true(.ix66_cell(pg, 1L, "bold", 2L))
  p4 <- plan_cell_style(table_plan(d), bold = ~ ifelse(lab == "n", TRUE, NA),
                        align = ~ ifelse(lab == "n", "right", NA))
  pg <- .ix66_pages(later(p4, align = ~ ifelse(lab == "n", "center", NA)))[[1L]]
  expect_identical(.ix66_cell(pg, 1L, "align", 2L), "center")
  expect_true(.ix66_cell(pg, 1L, "bold", 2L))
})
