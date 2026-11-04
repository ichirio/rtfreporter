# Validation tests: plan_style() -- how the whole-table settings (rules, the
# default look of header and body, widths, heights, padding, font, placement)
# act together on the pages a plan gives: a pairwise set against the direct
# call, and decision tables of the rules against the zones and of the limits
# of each setting.  Helpers prefixed .ps_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (plan_style(), rtftable(),
#  rtf_table_style()) and FR-PL422 .. FR-PL431:
#
#    reach    plan_style() makes plan_apply() run to pages; every setting it
#             is given reaches as_rtftables() / rtftable() by name, so the
#             pages are those of the direct call (FR-PL423 FR-PL426 FR-PL427);
#             the border zones and the look fields (header_bold, ..., bold,
#             ..., underline) make one rtf_table_style() passed as `style`
#             (FR-PL424 FR-PL425).
#    value    a setting the call gives is the page's: row_height_twips,
#             font, table_align, the paddings ... are those values on the
#             rtftable; table_width_pct is that percentage of the writable
#             width as a fraction (60 is 0.6).
#    rules    `border` ("tfl", "none", an rtf_border()) and the border_*
#             zones are two ways to say the rules: both in one call is an
#             error (FR-PL429).
#    limits   each setting has the limits of rtftable()'s help: table_width_pct
#             a number in (0, 100], table_width_pct_of_writable in (0, 1],
#             table_width_twips / row_height_twips / the paddings one
#             non-negative integer, font_size_half_points positive,
#             cell_valign top / center / bottom, table_align left / center /
#             right, row_height_exact TRUE or FALSE; the error is raised
#             when the plan is applied.
#
#  Data (.ps_df): a label column and two arms; one cell is "n (xx.x)" so that
#  align_count_pct has something to align.
#
#  (A) FT-PL990, pairwise.  Factors and levels:
#    rules      none given | border "tfl" | border "none" | border =
#               rtf_border(all = TRUE) | zones (border_header, border_last_row)
#    look       none | header (header_bold, header_italic, header_align
#               "right") | body (align "right", bold, italic, underline)
#    width      none | table_width_twips 6000 | table_width_pct 60 |
#               table_width_pct_of_writable 0.5
#    heights    none | row_height_twips 300 | 300 with row_height_exact |
#               header 400, blank 100 and row 260
#    padding    none | left 40 right 30 | top 20 bottom 10 | all four
#    font       none | "Arial" | size 20 | "Arial" and 20
#    place      none | cell_valign "top" | cell_valign "center" with
#               table_align "center" | table_align "right"
#    other      none | align_count_pct TRUE | markup "none" | markup "all" with
#               blank_row_normalize "collapse"
#  The product is 5 x 3 x 4 x 4 x 4 x 4 x 4 x 4 = 61,440 rows; .allpairs()
#  picks the rows covering every pair of levels.  Oracle: the pages equal the
#  direct call's, and each given setting is the page's (the rule above).
#
#  (B) FT-PL991, decision table, the rules against the zones in one call:
#  border ("tfl", "none", an rtf_border()) x each of the five zones = 15 rows
#  all errors; border alone, one zone alone, and all five zones: ok.
#
#  (C) FT-PL992, decision table, the limits just inside and just outside.
#  Across several calls a border and a zone are not asserted: the help says
#  "not both" for one call only (see the report of the PR).
# ---------------------------------------------------------------------------

.ps_df <- function() {
  data.frame(Param = c("Age", "Sex"), Placebo = c("5 (10.0)", "12  (3.5)"),
             Active = c("7 (12.1)", "9  (2.0)"), stringsAsFactors = FALSE)
}

.ps_pages <- function(p) suppressMessages(plan_apply(p, "pages"))

.ps_zone <- function() rtf_border(bottom = TRUE, inside_h = FALSE, inside_v = FALSE)

# the levels of each factor: the plan_style() arguments they stand for
.ps_levels <- list(
  rules = list(none = list(), tfl = list(border = "tfl"), nobord = list(border = "none"),
               box = list(border = rtf_border(all = TRUE)),
               zones = list(border_header = .ps_zone(), border_last_row = .ps_zone())),
  look = list(none = list(),
              header = list(header_bold = TRUE, header_italic = TRUE, header_align = "right"),
              body = list(align = "right", bold = TRUE, italic = TRUE, underline = TRUE)),
  width = list(none = list(), twips = list(table_width_twips = 6000),
               pct = list(table_width_pct = 60),
               frac = list(table_width_pct_of_writable = 0.5)),
  heights = list(none = list(), row = list(row_height_twips = 300),
                 exact = list(row_height_twips = 300, row_height_exact = TRUE),
                 three = list(header_row_height_twips = 400, blank_row_height_twips = 100,
                              row_height_twips = 260)),
  padding = list(none = list(), lr = list(cell_padding_left_twips = 40, cell_padding_right_twips = 30),
                 tb = list(cell_padding_top_twips = 20, cell_padding_bottom_twips = 10),
                 four = list(cell_padding_left_twips = 40, cell_padding_right_twips = 30,
                             cell_padding_top_twips = 20, cell_padding_bottom_twips = 10)),
  font = list(none = list(), arial = list(font = "Arial"),
              size = list(font_size_half_points = 20),
              both = list(font = "Arial", font_size_half_points = 20)),
  place = list(none = list(), top = list(cell_valign = "top"),
               centre = list(cell_valign = "center", table_align = "center"),
               right = list(table_align = "right")),
  other = list(none = list(), pct = list(align_count_pct = TRUE),
               none_markup = list(markup = "none"),
               all_markup = list(markup = "all", blank_row_normalize = "collapse")))

# the direct call a plan_style() argument list stands for
.ps_direct <- function(d, args) {
  zone_names <- c("border_header", "border_spanning", "border_body",
                  "border_first_row", "border_last_row")
  look_names <- c("header_align", "header_bold", "header_italic", "align", "bold",
                  "italic", "underline")
  sty <- args[intersect(names(args), c(zone_names, look_names))]
  rest <- args[setdiff(names(args), names(sty))]
  if (length(sty)) rest$style <- do.call(rtf_table_style, sty)
  out <- suppressMessages(do.call(as_rtftables, c(list(d, read_meta = FALSE), rest)))
  if (inherits(out, "rtftable")) list(out) else out
}

test_that("FT-PL990 [FR-PL422 FR-PL423 FR-PL424 FR-PL425 FR-PL426 FR-PL427 FR-IX407]: plan_style(): rules x look x width x heights x padding x font x placement x other settings, pairwise", {
  cand <- expand.grid(lapply(.ps_levels, names), stringsAsFactors = FALSE)
  expect_identical(nrow(cand), 61440L)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 60L)
  d <- .ps_df()
  single <- c("row_height_twips", "row_height_exact", "header_row_height_twips",
              "blank_row_height_twips", "cell_padding_left_twips", "cell_padding_right_twips",
              "cell_padding_top_twips", "cell_padding_bottom_twips", "cell_valign",
              "table_align", "font", "font_size_half_points", "table_width_twips")
  for (i in seq_len(nrow(ap$rows))) {
    r <- ap$rows[i, ]
    args <- list()
    for (f in names(.ps_levels)) args <- c(args, .ps_levels[[f]][[r[[f]]]])
    label <- paste(unlist(r), collapse = "/")
    got <- .ps_pages(do.call(plan_style, c(list(table_plan(d)), args)))
    expect_equal(got, .ps_direct(d, args), info = label)
    pg <- got[[1L]]
    for (nm in intersect(names(args), single)) {
      expect_equal(pg[[nm]], args[[nm]], info = paste(label, nm))
    }
    if (!is.null(args[["table_width_pct"]])) {
      expect_equal(pg$table_width_pct_of_writable, args[["table_width_pct"]] / 100, info = label)
    }
    if (!is.null(args[["table_width_pct_of_writable"]])) {
      expect_equal(pg$table_width_pct_of_writable, args[["table_width_pct_of_writable"]], info = label)
    }
    if (isTRUE(args$align_count_pct)) {
      expect_identical(nchar(pg$data$Placebo[1L]), nchar(pg$data$Placebo[2L]), info = label)
    }
  }
})

test_that("FT-PL991 [FR-PL424 FR-PL429 FR-IX407]: plan_style(): border against the border_* zones in one call, the decision table", {
  zones <- c("border_header", "border_spanning", "border_body", "border_first_row",
             "border_last_row")
  msg <- "`border` is the whole table's rules and `border_*`"
  d <- .ps_df()
  borders <- list(tfl = "tfl", none = "none", box = rtf_border(all = TRUE))
  for (b in names(borders)) for (z in zones) {
    args <- list(table_plan(d), border = borders[[b]])
    args[[z]] <- .ps_zone()
    expect_error(do.call(plan_style, args), msg, fixed = TRUE, info = paste(b, z))
  }
  # border alone, one zone alone, all five zones: ok, and the zones are one style
  expect_s3_class(.ps_pages(plan_style(table_plan(d), border = "tfl"))[[1L]], "rtftable")
  for (z in zones) {
    args <- list(table_plan(d))
    args[[z]] <- .ps_zone()
    expect_s3_class(.ps_pages(do.call(plan_style, args))[[1L]], "rtftable")
  }
  all5 <- stats::setNames(rep(list(.ps_zone()), 5L), zones)
  got <- .ps_pages(do.call(plan_style, c(list(table_plan(d)), all5)))
  expect_equal(got, .ps_direct(d, all5))
})

test_that("FT-PL992 [FR-PL426 FR-PL427 FR-IX407]: plan_style(): each setting just inside and just outside its limit", {
  d <- .ps_df()
  ok <- function(...) expect_s3_class(.ps_pages(plan_style(table_plan(d), ...))[[1L]], "rtftable")
  bad <- function(pattern, ...) {
    expect_error(.ps_pages(plan_style(table_plan(d), ...)), pattern)
  }
  ok(table_width_pct = 100);              bad("`table_width_pct` must be a number in \\(0, 100\\]", table_width_pct = 100.1)
  bad("`table_width_pct` must be a number in \\(0, 100\\]", table_width_pct = 0)
  ok(table_width_pct_of_writable = 1);    bad("`table_width_pct_of_writable` must be a number in \\(0, 1\\]", table_width_pct_of_writable = 1.01)
  bad("`table_width_pct_of_writable` must be a number in \\(0, 1\\]", table_width_pct_of_writable = 0)
  ok(table_width_twips = 0);              bad("`table_width_twips` must be a single non-negative integer", table_width_twips = -1)
  ok(row_height_twips = 0);               bad("`row_height_twips` must be a single non-negative integer", row_height_twips = -1)
  ok(cell_padding_left_twips = 0);        bad("`cell_padding_left_twips` must be a single non-negative integer", cell_padding_left_twips = -1)
  ok(cell_padding_bottom_twips = 0);      bad("`cell_padding_bottom_twips` must be a single non-negative integer", cell_padding_bottom_twips = -1)
  ok(font_size_half_points = 1);          bad("`font_size_half_points` must be a single positive integer", font_size_half_points = 0)
  ok(cell_valign = "bottom");             bad("`cell_valign` must be", cell_valign = "middle")
  ok(table_align = "left");               bad("`table_align` must be", table_align = "Left")
  ok(row_height_exact = FALSE);           bad("`row_height_exact` must be TRUE or FALSE", row_height_exact = NA)
  ok(align = "center");                   bad("`align` must be \"left\", \"center\" or \"right\"", align = "middle")
  ok(header_align = "left");              bad("`header_align` must be \"left\", \"center\" or \"right\"", header_align = "x")
  bad("`border_header` must be NULL or an rtf_border object", border_header = "tfl")
  bad("`border` must be", border = "TFL")
})
