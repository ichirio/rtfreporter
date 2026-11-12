## tests/testthat/test-plan-header-look.R
##
## Three plan styling gaps (look-params GAPS.md G3, G4, G5):
##   G3  plan_cell_style(header = TRUE, row = ) -> style_header(row = )
##   G4  the header's text colour and fill from a plan:
##       plan_cell_style(header = TRUE, color = , background = ) ->
##       style_cols(header_color = , header_background = ); col_spec$header_color
##       drawn on the label rows and on promoted label cells
##   G5  plan_style(border = <preset>, border_* = ): the preset, with each zone
##       merged onto it side by side, as style_zone() does
## Only calls that stopped before now run; the RTF of everything else is the
## same.

library(testthat)

.hl_df <- function() {
  data.frame(Parameter = c("Subjects", "Age, mean (SD)", "Female, n (%)"),
             Placebo = c("86", "75.2 (8.59)", "53 (61.6)"),
             Active = c("84", "75.7 (8.29)", "50 (59.5)"),
             stringsAsFactors = FALSE)
}

# a spanning row over the arms, then the column labels
.hl_hdr <- function() {
  rtf_col_header(list(col_cell(1, ""), col_cell(c(2, 3), "Treatment")),
                 c("Parameter", "Placebo", "Active"))
}

.hl_pages <- function(p) suppressMessages(plan_apply(p, "pages"))

.hl_direct <- function(...) {
  out <- suppressMessages(as_rtftables(.hl_df(), read_meta = FALSE, ...))
  if (inherits(out, "rtftable")) list(out) else out
}

# The RTF text of a list of pages.
.hl_rtf <- function(pages) {
  dir <- tempfile("hl-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  f <- file.path(dir, "t.rtf")
  generate_rtfreport(rtf_document() |> rtf_tables(pages), f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# The \trowd blocks of an RTF text.
.hl_rows <- function(rtf) unlist(strsplit(rtf, "\\\\trowd"))[-1]

# ---- G3: plan_cell_style(header = TRUE, row = ) ---------------------------

test_that("plan_cell_style(header = TRUE, row = ) styles that header row only, as style_header(row = )", {
  base <- table_plan(.hl_df()) |> plan_col_header(.hl_hdr())
  # the spanning row
  got <- .hl_pages(base |> plan_cell_style(header = TRUE, row = 1, italic = TRUE))
  want <- .hl_direct(col_header = .hl_hdr()) |>
    style_header(row = 1, italic = TRUE)
  expect_equal(got, want)
  expect_true(isTRUE(got[[1L]]$col_header[[1L]][[2L]]$italic))
  expect_false(any(vapply(got[[1L]]$col_spec, function(s) isTRUE(s$header_italic), NA)))
  # the label row, a border on one column: that row is promoted, the
  # spanning row is left alone
  rule <- rtf_border(bottom = "double")
  got2 <- .hl_pages(base |> plan_cell_style(header = TRUE, row = 2,
                                            cols = "Active", border = rule))
  want2 <- .hl_direct(col_header = .hl_hdr()) |>
    style_header(row = 2, cols = 3, border = rule)
  expect_equal(got2, want2)
  expect_null(got2[[1L]]$col_header[[1L]][[2L]]$border)
  expect_identical(.hl_rtf(got2), .hl_rtf(want2))
  # left out, every header row: the plan is what it was
  all_rows <- .hl_pages(base |> plan_cell_style(header = TRUE, bold = TRUE))
  expect_equal(all_rows, .hl_direct(col_header = .hl_hdr()) |>
                 style_header(bold = TRUE))
})

test_that("plan_cell_style(row = ) needs header = TRUE, a header row, and no colour or fill", {
  p <- table_plan(.hl_df()) |> plan_col_header(.hl_hdr())
  expect_error(plan_cell_style(p, row = 1, bold = TRUE),
               "plan_cell_style(row = ) picks a column-header row, so it is given with header = TRUE.",
               fixed = TRUE)
  expect_error(plan_cell_style(p, header = TRUE, row = 1, color = "#CC0000"),
               "color and background\n  are the columns' header look", fixed = TRUE)
  expect_error(plan_cell_style(p, header = TRUE, row = 2, background = "#EEEEEE",
                               bold = TRUE),
               "Make them a call without `row`.", fixed = TRUE)
  expect_error(.hl_pages(plan_cell_style(p, header = TRUE, row = 3, bold = TRUE)),
               "`style_header(row = )` must be in 1..2", fixed = TRUE)
})

# ---- G4: the header's text colour and fill ---------------------------------

test_that("style_cols(header_color = ) colours the header labels of those columns only", {
  t0 <- rtftable(.hl_df())
  t1 <- style_cols(t0, cols = "Active", header_color = "#1F3864")
  expect_identical(t1$col_spec[[3L]]$header_color, "#1F3864")
  expect_null(t1$col_spec[[2L]]$header_color)
  expect_null(t1$col_spec[[3L]]$color)
  rtf <- .hl_rtf(list(t1))
  expect_match(rtf, "\\red31\\green56\\blue100;", fixed = TRUE)
  hdr <- .hl_rows(rtf)[1L]
  expect_match(hdr, "\\\\cf[0-9]+ Active\\\\cf1 ")
  expect_no_match(hdr, "\\\\cf[0-9]+ Placebo")
  # the body keeps its colour
  body <- paste(.hl_rows(rtf)[-1L], collapse = "")
  expect_no_match(body, "\\\\cf[2-9]")
  # none given: no colour command in the header
  expect_no_match(.hl_rows(.hl_rtf(list(t0)))[1L], "\\\\cf")
})

test_that("a promoted label cell keeps its column's header colour, in either order of the verbs", {
  t0 <- rtftable(.hl_df())
  rule <- rtf_border(bottom = "double")
  a <- t0 |> style_header(cols = 3, border = rule) |>
    style_cols(cols = 3, header_color = "#CC0000")
  b <- t0 |> style_cols(cols = 3, header_color = "#CC0000") |>
    style_header(cols = 3, border = rule)
  ra <- .hl_rtf(list(a))
  expect_identical(ra, .hl_rtf(list(b)))
  expect_match(.hl_rows(ra)[1L], "\\\\cf[0-9]+ Active\\\\cf1 ")
  expect_no_match(.hl_rows(ra)[1L], "\\\\cf[0-9]+ Placebo")
})

test_that("plan_cell_style(header = TRUE, color = , background = ) is style_cols(header_color = , header_background = )", {
  p <- table_plan(.hl_df())
  got <- .hl_pages(p |> plan_cell_style(header = TRUE, cols = "Active",
                                        color = "#1F3864", background = "#D9E2F3"))
  want <- .hl_direct() |>
    style_cols(cols = 3, header_color = "#1F3864", header_background = "#D9E2F3")
  expect_equal(got, want)
  expect_identical(.hl_rtf(got), .hl_rtf(want))
  # the body is not coloured or filled
  expect_null(got[[1L]]$col_spec[[3L]]$color)
  expect_null(got[[1L]]$col_spec[[3L]]$background)
  # with the text look in the same call: style_header() then style_cols()
  got2 <- .hl_pages(p |> plan_cell_style(header = TRUE, bold = TRUE,
                                         color = "#1F3864"))
  want2 <- .hl_direct() |> style_header(bold = TRUE) |>
    style_cols(header_color = "#1F3864")
  expect_equal(got2, want2)
  # indent is still the body's
  expect_error(plan_cell_style(p, header = TRUE, indent_twips = 100),
               "plan_cell_style(header = TRUE) takes bold, italic, underline, align, border, color and background.",
               fixed = TRUE)
})

# ---- G5: plan_style(border = , border_* = ) --------------------------------

test_that("plan_style(border = \"tfl\", border_last_row = ) is the preset with that zone merged on, as style_zone()", {
  z <- rtf_border(bottom = TRUE)
  got <- .hl_pages(table_plan(.hl_df()) |>
                     plan_style(border = "tfl", border_last_row = z))
  want <- .hl_direct(border = "tfl") |> style_zone(last_row = z)
  expect_equal(got, want)
  expect_identical(.hl_rtf(got), .hl_rtf(want))
  # the preset's header rules are kept
  expect_false(is.null(got[[1L]]$border$header$top))
  # a zone on the preset's own zone merges side by side
  dbl <- rtf_border(bottom = "double")
  got2 <- .hl_pages(table_plan(.hl_df()) |>
                      plan_style(border = "tfl", border_header = dbl))
  expect_identical(got2[[1L]]$border$header$bottom$style, "double")
  expect_false(is.null(got2[[1L]]$border$header$top))
  expect_equal(got2, .hl_direct(border = "tfl") |> style_zone(header = dbl))
  # border_group with the preset: the preset's rules stay, the group rule is
  # the style's
  got3 <- .hl_pages(table_plan(.hl_df()) |>
                      plan_style(border = "tfl", border_group = z))
  expect_false(is.null(got3[[1L]]$border$header$top))
  # with the look fields too: one style, the merged rules
  got4 <- .hl_pages(table_plan(.hl_df()) |>
                      plan_style(border = "tfl", border_last_row = z,
                                 header_bold = TRUE))
  expect_true(isTRUE(got4[[1L]]$col_spec[[1L]]$header_bold))
  expect_false(is.null(got4[[1L]]$border$header$top))
  expect_false(is.null(got4[[1L]]$border$last_row$bottom))
})

test_that("plan_style(border = <whole-table rtf_border()>, border_spanning = ) on a spanning header is style_zone() after the expansion", {
  box <- rtf_border(all = TRUE)
  sp <- rtf_border(bottom = "double", inside_h = FALSE, inside_v = FALSE)
  base <- table_plan(.hl_df()) |> plan_col_header(.hl_hdr())
  want <- suppressWarnings(.hl_direct(col_header = .hl_hdr(), border = box) |>
                             style_zone(spanning = sp))
  # in one call, and across calls in either order
  one <- suppressWarnings(.hl_pages(base |> plan_style(border = box, border_spanning = sp)))
  expect_equal(one, want)
  expect_identical(.hl_rtf(one), .hl_rtf(want))
  two <- suppressWarnings(.hl_pages(base |> plan_style(border_spanning = sp) |>
                                      plan_style(border = box)))
  expect_equal(two, want)
  # the spanning zone is the zone style_zone() gives: no frame sides of the
  # header expansion on it
  expect_null(one[[1L]]$border$spanning$left)
  expect_identical(one[[1L]]$border$spanning$bottom$style, "double")
  # with the other zones too, and the inside rules
  grid <- rtf_border(all = TRUE, inside_h = TRUE, inside_v = TRUE)
  lr <- rtf_border(bottom = "double")
  want2 <- suppressWarnings(.hl_direct(col_header = .hl_hdr(), border = grid) |>
                              style_zone(spanning = sp, last_row = lr))
  got2 <- suppressWarnings(.hl_pages(base |> plan_style(border = grid, border_spanning = sp,
                                                        border_last_row = lr)))
  expect_equal(got2, want2)
  expect_identical(.hl_rtf(got2), .hl_rtf(want2))
})

test_that("what these calls do not use is written as before", {
  # a plan without any of the new arguments: the same RTF as the direct call
  p <- table_plan(.hl_df()) |> plan_col_header(.hl_hdr()) |>
    plan_style(border = "tfl") |>
    plan_cell_style(header = TRUE, bold = TRUE) |>
    plan_cell_style(cols = "Active", color = "#CC0000")
  want <- .hl_direct(col_header = .hl_hdr(), border = "tfl") |>
    style_header(bold = TRUE) |> style_cols(cols = 3, color = "#CC0000")
  expect_identical(.hl_rtf(.hl_pages(p)), .hl_rtf(want))
})
