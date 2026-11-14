# Thickening of the functional tests, batch 7: the interactions (area IX) and
# the number formatters (area FM) whose every mapping only checked part of the
# statement.  Each block asserts what its FR states, the figures worked out in a
# comment.  FT ids from 5701 up.

.t7_nb <- "\u00a0"
.t7_plain <- function(x) gsub(.t7_nb, " ", x, fixed = TRUE)

# ----------------------------------------------------------------------- FM

test_that("FT-FM5701 [FR-FM178]: a count of 0, or a missing or infinite percent, prints the count alone, padded to the cell width", {
  # "%3d" + 7 spaces: 3 + 7 = 10 characters, the width of every branch
  alone <- "  5       "
  expect_identical(fmt_count_pct(5L, NA_real_, na = "-", nbsp = " "), alone)  # the FR's example
  expect_identical(fmt_count_pct(5L, Inf, nbsp = " "), alone)
  expect_identical(fmt_count_pct(5L, -Inf, nbsp = " "), alone)
  expect_identical(fmt_count_pct(0L, 0.5, nbsp = " "), "  0       ")
  expect_identical(fmt_count_pct(0L, NA_real_, nbsp = " "), "  0       ")
  out <- fmt_count_pct(c(0L, 5L, 5L, 5L, 12L), c(0.5, NA, Inf, -Inf, 0.25), nbsp = " ")
  expect_identical(out, c("  0       ", "  5       ", "  5       ", "  5       ", " 12 (25.0)"))
  # no parenthesis, and the same width as the branch with one
  expect_false(any(grepl("(", out[1:4], fixed = TRUE)))
  expect_identical(nchar(out), rep(10L, 5L))
  # the padding is the non-breaking space by default
  expect_identical(fmt_count_pct(5L, NA_real_), gsub(" ", .t7_nb, alone, fixed = TRUE))
})

test_that("FT-FM5702 [FR-FM184]: pct_sign = TRUE puts a % before the parenthesis and widens every branch to 11; FALSE is the default", {
  # branches (pct_unit = "fraction"): below 10, 10 to 99, 100 or more, count alone
  # "%3d  (%3.1f%%)" = 3+2+1+3+1+1 = 11; "%3d (%4.1f%%)" = 3+1+1+4+1+1 = 11;
  # "%3d  (%3d%%)" = 3+2+1+3+1+1 = 11; "%3d" + 8 spaces = 11
  on <- fmt_count_pct(c(5L, 14L, 30L, 7L), c(0.053, 0.5, 1, NA), pct_sign = TRUE, nbsp = " ")
  expect_identical(on, c("  5  (5.3%)", " 14 (50.0%)", " 30  (100%)", "  7        "))
  expect_identical(nchar(on), rep(11L, 4L))
  # the closing parenthesis stays in the same column (11) in the three branches that have one
  expect_identical(as.integer(regexpr(")", on[1:3], fixed = TRUE)), rep(11L, 3L))
  expect_true(all(endsWith(on[1:3], "%)")))
  # the missing token branch is one wider as well
  expect_identical(nchar(fmt_count_pct(NA_integer_, NA_real_, na = "-", pct_sign = TRUE, nbsp = " ")), 11L)
  # FALSE is the default: the same cells, no sign, width 10
  off <- fmt_count_pct(c(5L, 14L, 30L, 7L), c(0.053, 0.5, 1, NA), nbsp = " ")
  expect_identical(off, c("  5  (5.3)", " 14 (50.0)", " 30  (100)", "  7       "))
  expect_identical(off, fmt_count_pct(c(5L, 14L, 30L, 7L), c(0.053, 0.5, 1, NA),
                                      pct_sign = FALSE, nbsp = " "))
  expect_identical(nchar(off), rep(10L, 4L))
  expect_false(any(grepl("%", off, fixed = TRUE)))
})

test_that("FT-FM5703 [FR-FM200]: the count_pct alignment gives matching cells one width and keeps a percent's own digits", {
  count_pct <- function(x, ...) {
    df <- data.frame(lab = seq_along(x), a = x, stringsAsFactors = FALSE)
    .t7_plain(as_rtftables(df, align_count_pct = TRUE, ...)[[1L]]$data$a)
  }
  # fmt_count_pct(pct_unit = "percent"): "%3d  (%3.1f)" / "%3d (%4.1f)" / "%3d  (%3d)", 10 wide
  expect_identical(count_pct(c("5 (5.0)", "14 (50.0)", "30 (100.0)")),
                   c("  5  (5.0)", " 14 (50.0)", " 30  (100)"))
  # a percent with no decimal keeps its digits, padded to the width the format
  # gave it ("16.0" is 4 wide, "16" is padded to 4): never "16.0"
  expect_identical(count_pct(c("5 (5.0)", "14 (16)")), c("  5  (5.0)", " 14 (  16)"))
  # ... with the sign: " 14 (16.0%)" padded the same way
  signed <- count_pct(c("5 (5.0%)", "14 (16%)"))
  expect_identical(signed, c("  5  (5.0%)", " 14 (  16%)"))
  expect_false(any(grepl("16.0", signed, fixed = TRUE)))
  # two decimals: "3.25" is longer than the 3-wide "3.2" the format gave, so it stays
  expect_identical(count_pct(c("7 (3.25)", "14 (50.0)")), c("  7  (3.25)", " 14 (50.0)"))
  # every matching one-decimal cell shares one width
  expect_length(unique(nchar(count_pct(c("1 (0.5)", "22 (12.5)", "333 (99.9)", "4 (100.0)")))), 1L)
})

test_that("FT-FM5704 [FR-FM205]: the count_pct alignment pads with the non-breaking space, as fmt_count_pct() does", {
  x <- c("5 (5.0)", "14 (50.0)")
  def <- rtfreporter:::.realign_count_pct(x)
  expect_identical(def, c("\u00a0\u00a05\u00a0\u00a0(5.0)", "\u00a014\u00a0(50.0)"))
  expect_false(any(grepl(" ", def, fixed = TRUE)))
  expect_identical(def, fmt_count_pct(c(5L, 14L), c(5, 50), pct_unit = "percent"))
  # " " keeps plain spaces, another character is the padding
  expect_identical(rtfreporter:::.realign_count_pct(x, nbsp = " "), c("  5  (5.0)", " 14 (50.0)"))
  expect_identical(rtfreporter:::.realign_count_pct(x, nbsp = "."), c("..5..(5.0)", ".14.(50.0)"))
  # the built-in alignment of as_rtftables() applies the default
  df <- data.frame(lab = c("a", "b"), a = x, stringsAsFactors = FALSE)
  expect_identical(as_rtftables(df, align_count_pct = TRUE)[[1L]]$data$a, def)
})

test_that("FT-FM5705 [FR-FM222]: a cell already holding the na token is aligned like a missing cell; other text stays unchanged", {
  x <- c("1 (1.2%)", "-", NA, "NE", "n/a", "75.2 (8.6)")
  out <- fmt_count_paren(x, na = "-", nbsp = " ")
  # the count field is 1 wide; "1 (1.2%)" is 8 wide: the token + 7 spaces
  expect_identical(out[1:3], c("1 (1.2%)", "-       ", "-       "))
  expect_identical(out[2L], out[3L])                 # the token cell is the missing cell
  expect_identical(out[4:6], c("NE", "n/a", "75.2 (8.6)"))   # unchanged, unpadded
  # a wider count field moves the token's right edge with the counts
  wide <- fmt_count_paren(c("108 (35.3%)", "-", "NE"), na = "-", nbsp = " ")
  expect_identical(wide, c("108 (35.3%)", "  -        ", "NE"))
  # without na, "-" is just text
  expect_identical(fmt_count_paren(c("1 (1.2%)", "-"), nbsp = " "), c("1 (1.2%)", "-"))
})

test_that("FT-FM5706 [FR-FM254]: a nbsp that is not one string stops, and so does a pct_sign that is not TRUE or FALSE", {
  nb <- "`nbsp` must be a single string -- the padding character, \"\\u00a0\" by default."
  for (bad in list(NA, NA_character_, c(" ", "."), 1, character(0))) {
    expect_error(fmt_count_pct(1L, 0.5, nbsp = bad), nb, fixed = TRUE)
    expect_error(fmt_value_paren("1 (0.5)", nbsp = bad), nb, fixed = TRUE)
  }
  # "" pads with nothing: " 1" + " (50.0)" loses every pad
  expect_identical(fmt_count_pct(1L, 0.5, nbsp = ""), "1(50.0)")
  expect_identical(fmt_value_paren(c("12 (3.4)", "1 (0.5)"), nbsp = ""), c("12(3.4)", "1(0.5)"))
  ps <- "`pct_sign` must be TRUE or FALSE."
  for (bad in list(NA, "yes", 1, c(TRUE, FALSE))) {
    expect_error(fmt_count_pct(1L, 0.5, pct_sign = bad), ps, fixed = TRUE)
  }
  expect_identical(fmt_count_pct(1L, 0.5, pct_sign = TRUE, nbsp = " "), "  1 (50.0%)")
})

test_that("FT-FM5707 [FR-FM150]: fmt_numeric() text renders as written, without scientific notation, and is what set_decimal_split() splits on", {
  op <- options(scipen = -9)
  on.exit(options(op), add = TRUE)
  d <- data.frame(lab = c("a", "b", "c"), v = c(123456, 1e10, 1 / 3))
  # without the formatting the writer shows an exponent ...
  raw <- rtfreporter:::.render_rtftable(rtftable(d[1:2, ], border = "none"), 9360L)
  expect_true(any(grepl("e+", raw, fixed = TRUE)))
  # ... fmt_numeric() makes text that is written as it is
  f <- fmt_numeric(d, "v", digits = 2)
  expect_identical(f$v, c("123456.00", "10000000000.00", "0.33"))
  out <- rtfreporter:::.render_rtftable(rtftable(f, border = "none"), 9360L)
  expect_false(any(grepl("e+", out, fixed = TRUE)))
  expect_true(any(grepl("10000000000.00", out, fixed = TRUE)))
  # the same text is what set_decimal_split() cuts: the left part, the point and the right
  tbl <- set_decimal_split(rtftable(f, border = "none"), cols = "v")
  cx <- rtfreporter:::.compute_cellx(2L, 9360L, tbl)
  plan <- rtfreporter:::.decimal_split_plan(tbl, cx, tbl$col_spec, "script")
  expect_equal(plan$parts[[2L]]$left, c("123456", "10000000000", "0"))
  expect_equal(plan$parts[[2L]]$right, c(".00", ".00", ".33"))
})

# ----------------------------------------------------------------------- IX

.t7_render <- function(doc, ...) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE, ...)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# AGE summarised (N, mean, sd) in arms A and B, with a formatted statistic column
.t7_ard <- function(fmt = TRUE) {
  d <- data.frame(
    group1 = "TRT", group1_level = rep(c("A", "B"), each = 3L),
    variable = "AGE", variable_level = NA_character_, context = "continuous",
    stat_name = rep(c("N", "mean", "sd"), 2L),
    stat_label = rep(c("N", "Mean", "SD"), 2L),
    stat = c(10, 12.34, 1.2, 12, 15.67, 2.2), stringsAsFactors = FALSE)
  if (fmt) d$stat_fmt <- c("10", "12.3", "1.2", "12", "15.7", "2.2")
  normalize_ard(d)
}

.t7_two_vars <- function() {
  normalize_ard(rbind(
    data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 4L),
               variable = "SEX", variable_level = rep(c("F", "F", "M", "M"), 2L),
               context = "categorical", stat_name = rep(c("n", "p"), 4L),
               stat_label = rep(c("n", "%"), 4L),
               stat = c(4, 0.4, 6, 0.6, 5, 0.5, 5, 0.5), stringsAsFactors = FALSE),
    data.frame(group1 = "TRT", group1_level = rep(c("A", "B"), each = 2L),
               variable = "AGE", variable_level = NA_character_,
               context = "continuous", stat_name = rep(c("N", "mean"), 2L),
               stat_label = rep(c("N", "Mean"), 2L),
               stat = c(10, 12.34, 12, 15.67), stringsAsFactors = FALSE)))
}

test_that("FT-IX5701 [FR-IX123]: a header and a footer in their own font both start in the document's font", {
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(c(l = "HEADX"), font = "Arial"),
      footer = rtf_footer(c(l = "FOOTX"), font = "Arial"))) |>
    rtf_tables(data.frame(A = c("1", "2")))
  out <- .t7_render(doc)
  # the document's font is f0 (Courier New), their own is f1 (Arial)
  expect_match(out, "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}{\\f1\\fnil\\fcharset0 Arial;}}",
               fixed = TRUE)
  # each starts in f0 ...
  expect_match(out, "{\\header \\f0\\fs18", fixed = TRUE)
  expect_match(out, "{\\footer \\f0\\fs18", fixed = TRUE)
  # ... then switches to its own font for its text
  expect_match(out, "\\f1 HEADX\\cell", fixed = TRUE)
  expect_match(out, "\\f1 FOOTX\\cell", fixed = TRUE)
})

test_that("FT-IX5702 [FR-IX201]: plan_cells(value = ) changes a cell only with stats = \"rows\"; with templates {x} reads stat_fmt, else stat", {
  tab <- function(ard, ...) plan_apply(table_plan(ard, cols = "TRT") |> plan_cells(..., notes = FALSE), "table")
  # stats = "rows": value picks the column the cells hold
  by_stat <- tab(.t7_ard(), stats = "rows")                    # value = "stat" (the default)
  expect_identical(by_stat$A, c(10, 12.34, 1.2))
  expect_identical(by_stat$B, c(12, 15.67, 2.2))
  by_fmt <- tab(.t7_ard(), stats = "rows", value = "stat_fmt")
  expect_identical(by_fmt$A, c("10", "12.3", "1.2"))
  expect_identical(by_fmt$B, c("12", "15.7", "2.2"))
  # with templates value changes nothing: an open {mean} reads stat_fmt when the frame has it ...
  expect_identical(tab(.t7_ard(), "{mean}")$A, "12.3")
  expect_identical(tab(.t7_ard(), "{mean}", value = "stat")$A, "12.3")
  expect_identical(tab(.t7_ard(), "{mean}", value = "stat_fmt")$A, "12.3")
  # ... and stat otherwise, whatever value says
  expect_identical(tab(.t7_ard(FALSE), "{mean}")$A, "12.34")
  expect_identical(tab(.t7_ard(FALSE), "{mean}", value = "stat_fmt")$A, "12.34")
})

test_that("FT-IX5703 [FR-IX303]: font_table, page and font_lang default from the rtfreporter.* option; an explicit argument beats it", {
  old <- options(rtfreporter.font = "Arial", rtfreporter.page.paper_size = "A4",
                 rtfreporter.font_lang = c(east_asian = "ja"))
  on.exit(options(old), add = TRUE)
  d <- rtf_document()$document
  expect_identical(d$font_table[[1L]]$name, "Arial")           # rtfreporter.font
  expect_identical(d$page$paper_size, "A4")                   # rtfreporter.page.*
  expect_identical(d$font_lang, c(east_asian = "ja"))         # rtfreporter.font_lang
  e <- rtf_document(font_table = list(list(name = "Times New Roman")),
                    page = rtf_page(paper_size = "Letter"),
                    font_lang = c(latin = "en"))$document
  expect_identical(e$font_table[[1L]]$name, "Times New Roman")
  expect_identical(e$page$paper_size, "Letter")
  expect_identical(e$font_lang, c(latin = "en"))
  # with the options cleared the package defaults come back
  options(rtfreporter.font = NULL, rtfreporter.page.paper_size = NULL,
          rtfreporter.font_lang = NULL)
  f <- rtf_document()$document
  expect_identical(f$font_table[[1L]]$name, "Courier New")
  expect_identical(f$page$paper_size, "letter")
  expect_null(f$font_lang)
})

test_that("FT-IX5704 [FR-IX305]: rtf_config() merges page and style per key, and replaces font_table, color_table, watermark and font_lang whole", {
  d <- rtf_document(
    page = rtf_page(paper_size = "A4", orientation = "portrait"),
    style = rtf_style(font_size_half_points = 20L, row_height_twips = 300L),
    font_table = list(list(name = "Arial"), list(name = "Times New Roman")),
    color_table = c("#000000", "#FF0000"),
    watermark = rtf_watermark("A", color = "#112233"),
    font_lang = c(latin = "en", east_asian = "ja"))
  e <- rtf_config(d, page = list(orientation = "landscape"),
                  style = rtf_style(row_height_twips = 400L),
                  font_table = list(list(name = "Courier New")),
                  color_table = "#00FF00",
                  watermark = "B", font_lang = c(latin = "ko"))$document
  # per key: the paper size stays, the orientation changes; the font size stays, the height changes
  expect_identical(e$page$paper_size, "A4")
  expect_identical(e$page$orientation, "landscape")
  expect_identical(e$style$font_size_half_points, 20L)
  expect_identical(e$style$row_height_twips, 400L)
  # whole: nothing of the earlier table, colour, watermark or language survives
  expect_identical(e$font_table, list(list(name = "Courier New")))
  expect_identical(e$color_table, "#00FF00")
  expect_identical(e$watermark$text, "B")
  expect_identical(e$watermark$color, "#C8C8C8")               # not the first watermark's colour
  expect_identical(e$font_lang, c(latin = "ko"))
  # the document it was given is untouched
  expect_identical(d$document$page$orientation, "portrait")
  expect_length(d$document$font_table, 2L)
})

test_that("FT-IX5705 [FR-IX322]: drop_cols: col_header is written against the printed columns, col_spec against the input columns", {
  df <- data.frame(A = c("a1", "a2"), B = c("b1", "b2"), C = c("c1", "c2"),
                   D = c("d1", "d2"), E = c("e1", "e2"), stringsAsFactors = FALSE)
  # B is dropped: four printed columns A C D E, so a header row of four labels fits ...
  t1 <- as_rtftables(df, drop_cols = "B", col_header = list(c("H1", "H2", "H3", "H4")),
                     col_spec = list(list(col = 3L, align = "right")))[[1L]]
  expect_identical(names(t1$data), c("A", "C", "D", "E"))
  expect_identical(t1$col_header, list(c("H1", "H2", "H3", "H4")))
  # ... and one of five (the input's width) does not
  expect_error(as_rtftables(df, drop_cols = "B",
                            col_header = list(c("H1", "H2", "H3", "H4", "H5"))),
               "the label row has 5 labels but the table has 4 printed columns",
               fixed = TRUE)
  # col_spec col = 3 is the INPUT's third column (C), which is the printed second
  expect_identical(vapply(t1$col_spec, function(s) s$align, ""),
                   c("left", "right", "center", "center"))
  # a spanning cell's position is in the printed columns: 2:3 is C and D
  t2 <- as_rtftables(df, drop_cols = "B", col_header = list(
    list(col_cell(1L, ""), col_cell(2:3, "SPAN"), col_cell(4L, "")),
    c("A", "C", "D", "E")))[[1L]]
  span <- Filter(function(z) identical(z$label, "SPAN"), t2$col_header[[1L]])[[1L]]
  expect_identical(c(span$from, span$to), c(2L, 3L))
})

test_that("FT-IX5706 [FR-IX323]: sort_by is applied first: cell_styles follow their rows, and a sort key may be a column that is dropped", {
  df <- data.frame(grp = c("B", "A", "B", "A"), ord = c(2, 1, 1, 2),
                   Label = c("b2", "a1", "b1", "a2"), stringsAsFactors = FALSE)
  cs <- list(
    list(bold = c(TRUE,  FALSE, FALSE)),   # row 1 (Label b2)
    list(bold = c(FALSE, TRUE,  FALSE)),   # row 2 (a1)
    list(bold = c(FALSE, FALSE, TRUE)),    # row 3 (b1)
    list(bold = c(TRUE,  TRUE,  FALSE)))   # row 4 (a2)
  res <- as_rtftables(df, sort_by = "Label", cell_styles = cs)[[1L]]
  # sorted: a1, a2, b1, b2 = input rows 2, 4, 3, 1, and each style went with its row
  expect_identical(res$data$Label, c("a1", "a2", "b1", "b2"))
  expect_identical(res$cell_styles, cs[c(2L, 4L, 3L, 1L)])
  # a sort key listed in drop_cols orders the rows although it is not printed:
  # ord = 2, 1, 1, 2 ascending (stable) = input rows 2, 3, 1, 4 = a1, b1, b2, a2
  carrier <- as_rtftables(df, sort_by = "ord", drop_cols = "ord")[[1L]]
  expect_false("ord" %in% names(carrier$data))
  expect_identical(carrier$data$Label, c("a1", "b1", "b2", "a2"))
  # group detection sees the sorted order (grp: A A B B), the blank row sits between them
  g <- as_rtftables(df, sort_by = "grp", group_col = "grp",
                    blank_rows = "between_groups")[[1L]]
  expect_identical(g$data$Label, c("a1", "a2", "b2", "b1"))
  expect_identical(g$blank_rows, 2L)
})

test_that("FT-IX5707 [FR-IX342]: paginate_cols() refuses to cut inside a selector span, and cuts at its boundary", {
  x <- data.frame(lab = c("r1", "r2"), `Placebo____D1` = 1:2, `Placebo____D2` = 3:4,
                  `Drug____D1` = 5:6, `Drug____D2` = 7:8, check.names = FALSE)
  tb <- rtftable(x, col_header = rtf_col_header(list(
    col_cell(col_key("Placebo"), "Placebo"), col_cell(col_key("Drug"), "Drug"))))
  # the Placebo span is columns 2..3: a cut before column 3 is inside it
  expect_error(paginate_cols(tb, at = 3, allow_span_break = FALSE),
               "`paginate_cols()` would break the spanning header cell 'Placebo' (columns 2..3) at column 3.",
               fixed = TRUE)
  # a cut before column 4 is between the groups
  pages <- suppressWarnings(paginate_cols(tb, at = 4, allow_span_break = FALSE))
  expect_identical(lapply(pages, function(p) names(p$data)),
                   list(c("lab", "Placebo____D1", "Placebo____D2"),
                        c("lab", "Drug____D1", "Drug____D2")))
})

test_that("FT-IX5708 [FR-IX344]: layout = \"flow\" with a width makes the separator a break opportunity; with no width both layouts return the text as it stands", {
  d <- data.frame(AEDECOD = "40/F/SCREENING/COMPLETED", stringsAsFactors = FALSE)
  one <- function(layout, width = NULL) {
    build_listing(d, listing_spec(list(
      listing_col("AEDECOD", layout = layout, width = width, sep = "/"))))$AEDECOD
  }
  # width 12: "40/F/" shares a line (5 characters), "SCREENING/" is 10, "COMPLETED" is 9
  expect_identical(one("flow", 12), c("40/F/", "SCREENING/", "COMPLETED", ""))
  expect_identical(one("flow")[1L], "40/F/SCREENING/COMPLETED")
  expect_identical(one("stack")[1L], "40/F/SCREENING/COMPLETED")
  expect_identical(one("flow"), one("stack"))
})

test_that("FT-IX5709 [FR-IX348]: listing_spec(type = \"multiline\") supplies defaults and an explicit argument overrides each of them", {
  base <- listing_spec(list(listing_col("A")))
  expect_identical(base$type, "multiline")
  expect_identical(base$sep, "/")
  expect_true(base$spacer)
  expect_identical(base$spacer_rel_width, 1)
  expect_true(base$blank_row)
  expect_true(base$blank_row_first)
  expect_identical(base$align, "left")
  expect_identical(base$layout, "stack")
  expect_false(base$wrap_custom)
  over <- listing_spec(list(listing_col("A")), type = "multiline", sep = "|",
                       spacer = FALSE, spacer_rel_width = 3, blank_row = FALSE,
                       blank_row_first = FALSE, align = "right", layout = "flow",
                       wrap = function(text, width, sep, layout) text)
  expect_identical(over$sep, "|")
  expect_false(over$spacer)
  expect_identical(over$spacer_rel_width, 3)
  expect_false(over$blank_row)
  expect_false(over$blank_row_first)
  expect_identical(over$align, "right")
  expect_identical(over$layout, "flow")
  expect_true(over$wrap_custom)
  # record: TRUE (default) appends the hidden record column, FALSE none, a string names it
  d <- data.frame(S = c("s1", "s2"), A = c("a", "b"), stringsAsFactors = FALSE)
  expect_true(".rtf_record" %in% names(build_listing(d, base)))
  expect_false(".rtf_record" %in% names(build_listing(d, listing_spec(list(listing_col("A")), record = FALSE))))
  named <- build_listing(d, listing_spec(list(listing_col("A")), record = "REC"))
  expect_true("REC" %in% names(named))
  expect_false(".rtf_record" %in% names(named))
})

test_that("FT-IX5710 [FR-IX350]: listing_col(rel_width = ) wins over width; without it the width, then the longest line of the label", {
  d <- data.frame(A = "a", B = "b", C = "c", D = "d", E = "e", stringsAsFactors = FALSE)
  spec <- listing_spec(list(
    listing_col("A", width = 11, rel_width = 20),         # rel_width
    listing_col("B", width = 11),                         # the width
    listing_col("C", label = "Visit\nDay"),               # longest line: "Visit" = 5
    listing_col("D", label = "Study Site Number"),        # no break: 17
    listing_col("E")),                                    # the label is the column name: 1
    spacer = FALSE)
  expect_identical(as_rtftables(d, listing = spec)[[1L]]$col_rel_width, c(20, 11, 5, 17, 1))
})

test_that("FT-IX5711 [FR-IX356]: the gutters of listing_spec(spacer, spacer_rel_width) come out of the character budget before any column is fitted", {
  d <- data.frame(USUBJID = sprintf("PILOT-20%d-10%02d", rep(4:6, each = 4), 1:12),
                  AEDECOD = rep(c("HYPERGLYCAEMIA", "UPPER RESPIRATORY TRACT INFECTION"), 6),
                  AETOXGR = rep(c("3", "4", "3", "2"), 3), stringsAsFactors = FALSE)
  cols <- function() list(listing_col("USUBJID"), listing_col("AEDECOD"), listing_col("AETOXGR"))
  spent <- function(...) {
    fit <- fit_listing_widths(d, listing_spec(cols(), ...), total_width = 60)
    sum(vapply(fit$cols, function(cl) as.integer(cl$width), integer(1L)))
  }
  # three columns have two gutters: the columns get 60 - 2 * (width of a gutter)
  expect_identical(spent(spacer = FALSE), 60L)                       # no gutter prints
  expect_identical(spent(), 58L)                                     # spacer_rel_width 1 each
  expect_identical(spent(spacer_rel_width = 3), 54L)                 # 60 - 2 * 3
  expect_identical(spent(spacer_rel_width = 0.5), 59L)               # 60 - 2 * 0.5
})

test_that("FT-IX5712 [FR-IX357]: collapse_repeats = TRUE with blank rows prints the value once across records, and still does without them", {
  d <- data.frame(USUBJID = c("S1", "S1", "S2", "S2"),
                  AEDECOD = c("Cough", "Headache", "Nausea", "Rash"), stringsAsFactors = FALSE)
  printed <- function(...) {
    spec <- listing_spec(list(listing_col("USUBJID", label = "Subject", collapse_repeats = TRUE),
                              listing_col("AEDECOD", label = "Term")), ...)
    v <- as_rtftables(d, listing = spec)[[1L]]$data$USUBJID
    v[is.na(v)] <- ""
    v
  }
  # with blank rows (the default): 4 records and 4 blank rows between them; the
  # run goes on through the blank rows, so each subject prints once
  with_blank <- printed()
  expect_length(with_blank, 8L)
  expect_identical(with_blank[nzchar(with_blank)], c("S1", "S2"))
  expect_identical(with_blank[c(1L, 5L)], c("S1", "S2"))
  # without blank rows the key still collapses across the two records of S1 and of S2
  without <- printed(blank_row = FALSE)
  expect_identical(without, c("S1", "", "S2", ""))
})

test_that("FT-IX5713 [FR-IX318]: as_rtftables(listing = ) supplies defaults only: a collapse_repeats the caller passes beats the one the listing marks", {
  d <- data.frame(USUBJID = c("S1", "S1", "S2", "S2"),
                  AEDECOD = c("Cough", "Headache", "Nausea", "Rash"), stringsAsFactors = FALSE)
  spec <- listing_spec(list(listing_col("USUBJID", label = "Subject", collapse_repeats = TRUE),
                            listing_col("AEDECOD", label = "Term")))
  printed <- function(...) {
    v <- as_rtftables(d, listing = spec, ...)[[1L]]$data$USUBJID
    v[is.na(v)] <- ""
    v
  }
  # the listing's own default: the subject prints once per run (S1 twice, S2 twice -> once each)
  expect_identical(printed()[nzchar(printed())], c("S1", "S2"))
  # the caller names another column: the subject is no longer blanked, every one of the
  # 8 rows (4 records, 4 blank rows) holds it
  mine <- printed(collapse_repeats = "AEDECOD")
  expect_identical(mine, rep(c("S1", "S2"), each = 4L))
})

test_that("FT-IX5714 [FR-IX368]: widen_ard(value = ) changes a cell only with stats = \"rows\"; with stats = \"cells\" each token says which value it takes", {
  ard <- .t7_ard()
  rows <- function(...) widen_ard(ard, cols = "TRT", stats = "rows", notes = FALSE, ...)
  # rows: value is the column the cells hold
  expect_identical(rows()$A, c(10, 12.34, 1.2))                      # "stat"
  expect_identical(rows(value = "stat")$A, c(10, 12.34, 1.2))
  expect_identical(rows(value = "stat_fmt")$A, c("10", "12.3", "1.2"))
  expect_identical(rows(value = "stat_fmt")$B, c("12", "15.7", "2.2"))
  # cells: value is ignored, the token decides
  cells <- function(...) widen_ard(ard, cols = "TRT", notes = FALSE, ...)
  expect_identical(cells(cells = "{mean}", value = "stat"), cells(cells = "{mean}"))
  expect_identical(cells(cells = "{mean}", value = "stat_fmt"), cells(cells = "{mean}"))
  expect_identical(cells(cells = "{mean}")$A, "12.3")                # stat_fmt then stat
  expect_identical(cells(cells = "{mean:stat}")$A, "12.34")
  expect_identical(cells(cells = "{mean:stat_fmt}")$A, "12.3")
  expect_identical(widen_ard(.t7_ard(FALSE), cols = "TRT", cells = "{mean}", notes = FALSE)$A,
                   "12.34")                                           # no stat_fmt: stat
})

test_that("FT-IX5715 [FR-IX373]: widen_ard(sort = FALSE) still sorts on a factor key and on a label column that levels ordered; only a plain key stays", {
  # a factor key (levels A, B) over data written B first
  d <- data.frame(PARAM = factor(c("B", "B", "A", "A"), levels = c("A", "B")),
                  TRT = c("x", "y", "x", "y"), variable = "V",
                  variable_level = "L", context = "categorical",
                  stat_name = "n", stat_label = "n", stat = c(1, 2, 3, 4),
                  .label = "L", .kind = "categorical", stringsAsFactors = FALSE)
  run <- function(p) widen_ard(p, cols = "TRT", rows = c(PARAM = "PARAM"),
                               label = c(label = ".label"), cells = "{n:.0f}",
                               sort = FALSE, notes = FALSE)
  expect_identical(as.character(run(d)$PARAM), c("A", "B"))          # the factor's levels move rows
  expect_identical(run(d)$x, c("3", "1"))
  d$PARAM <- as.character(d$PARAM)
  expect_identical(as.character(run(d)$PARAM), c("B", "A"))          # a plain key stays
  # a label column that levels ordered: SEX's F, M written M first in levels
  tv <- .t7_two_vars()
  w <- function(...) widen_ard(tv, cols = "TRT", rows = c(group = "variable"),
                               cells = list(continuous = c(n = "{N:.0f}"), categorical = "{n:.0f}"),
                               sort = FALSE, notes = FALSE, ...)
  expect_identical(as.character(w()$label), c("F", "M", "n"))        # the data's order
  expect_identical(as.character(w(levels = list(SEX = c("M", "F")))$label), c("M", "F", "n"))
  expect_identical(w(levels = list(SEX = c("M", "F")))$A, c("6", "4", "10"))
  # and the blocks of a plain key (group) stay where the data put them
  expect_identical(as.character(w()$group), c("SEX", "SEX", "AGE"))
})

test_that("FT-IX5716 [FR-IX378]: normalize_ard(drop_contexts = ) decides whether the study total survives", {
  ard <- rbind(
    data.frame(group1 = NA_character_, group1_level = NA_character_,
               variable = "..ard_total_n..", variable_level = NA_character_,
               context = "total_n", stat_name = "N", stat_label = "N", stat = 20,
               stringsAsFactors = FALSE),
    data.frame(group1 = "SEX", group1_level = c("F", "M"), variable = "AESOC",
               variable_level = "Cardiac", context = "categorical",
               stat_name = "n", stat_label = "n", stat = c(3, 4),
               stringsAsFactors = FALSE))
  # the default drops the total_n row and keeps its number, as an attribute and a column
  dflt <- normalize_ard(ard)
  expect_false("..ard_total_n.." %in% dflt$variable)
  expect_identical(attr(dflt, "ard_total_n"), 20)
  expect_identical(dflt$.total_n, c(20, 20))                         # one per remaining row
  # naming only "attributes" keeps the row and states no attribute
  kept <- normalize_ard(ard, drop_contexts = "attributes")
  expect_true("..ard_total_n.." %in% kept$variable)
  expect_null(attr(kept, "ard_total_n"))
  expect_false(".total_n" %in% names(kept))
})

test_that("FT-IX5717 [FR-IX380]: assemble_rtf(toc = ) as a table or the path of one, with input_files left out, takes the table's file column in the table's order", {
  mk <- function(dir, file, no, title, mark) {
    doc <- rtf_document() |>
      rtf_section(page = 1, secinfo = list(
        header = rtf_header(rows = list(c(c = paste0("Table ", no)), c(c = title))),
        footer = NULL)) |>
      rtf_tables(data.frame(A = mark))
    generate_rtfreport(doc, file.path(dir, file), overwrite = TRUE)
    file.path(dir, file)
  }
  d <- tempfile("asm"); dir.create(d)
  od <- tempfile("asmout"); dir.create(od)
  on.exit(unlink(c(d, od), recursive = TRUE), add = TRUE)
  a <- mk(d, "a.rtf", "14.1.1", "Demog", "MARKAAA")
  b <- mk(d, "b.rtf", "14.3.1", "AEs", "MARKBBB")
  c3 <- mk(d, "c.rtf", "14.2.1", "Vitals", "MARKCCC")
  marks <- function(f) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    regmatches(txt, gregexpr("MARK[ABC]{3}", txt))[[1L]]
  }
  sp <- assemble_folder(d)
  edited <- sp[3:1, ]                                  # b, c, a
  edited$order <- 1:3
  # the table itself, input_files left out: its file column, in its order
  o1 <- file.path(od, "o1.rtf")
  assemble_rtf(toc = edited, output_file = o1)
  expect_identical(marks(o1), c("MARKBBB", "MARKCCC", "MARKAAA"))
  # the path of the same table
  csv <- file.path(od, "toc.csv")
  utils::write.csv(edited, csv, row.names = FALSE)
  o2 <- file.path(od, "o2.rtf")
  assemble_rtf(toc = csv, output_file = o2)
  expect_identical(marks(o2), c("MARKBBB", "MARKCCC", "MARKAAA"))
  # given both, a file of the table that input_files lacks stops
  expect_error(assemble_rtf(toc = edited, input_files = c(a, b),
                            output_file = file.path(od, "o3.rtf")),
               paste0("`toc`: file '", sp$file[2L], "' is not in `input_files`."), fixed = TRUE)
})

test_that("FT-IX5718 [FR-IX386]: rtf_figures(width_twips, height_twips) set the size on the page; render_width / render_height only the inches drawn", {
  skip_if_not(isTRUE(unname(capabilities("png"))), "no PNG device")
  old <- options(rtfreporter.png_device = "png")
  on.exit(options(old), add = TRUE)
  draw <- function() plot(1:10)
  goal <- function(...) {
    rtf <- .t7_render(rtf_document() |> rtf_figures(draw, ..., render_dpi = 100))
    regmatches(rtf, regexpr("\\\\picwgoal[0-9]+\\\\pichgoal[0-9]{1,5}", rtf))
  }
  # drawn at 3 x 2 inches (aspect 3:2): the width given, the height follows (4000 * 2 / 3)
  expect_identical(goal(width_twips = 4000L, render_width = 3, render_height = 2),
                   "\\picwgoal4000\\pichgoal2667")
  expect_identical(goal(width_twips = 4000L, render_width = 8, render_height = 2),
                   "\\picwgoal4000\\pichgoal1000")                # 8 x 2: the height follows 1000
  # the height given: the width follows (3000 * 3 / 2)
  expect_identical(goal(height_twips = 3000L, render_width = 3, render_height = 2),
                   "\\picwgoal4500\\pichgoal3000")
  # both given: both stand, whatever the drawing
  expect_identical(goal(width_twips = 4000L, height_twips = 1000L, render_width = 3, render_height = 2),
                   "\\picwgoal4000\\pichgoal1000")
  # neither: the inches drawn are the size (3 * 1440 = 4320, 2 * 1440 = 2880)
  expect_identical(goal(render_width = 3, render_height = 2), "\\picwgoal4320\\pichgoal2880")
})

test_that("FT-IX5719 [FR-IX359]: an explicit rounding = on a call wins over options(rtfreporter.rounding), in either direction", {
  old <- options(rtfreporter.rounding = "sas")
  on.exit(options(old), add = TRUE)
  df <- data.frame(v = 2.5)
  # the option "sas": half away from zero (3); the call's "r" is half to even (2)
  expect_identical(c(round_num(2.5, 0), round_num(2.5, 0, rounding = "r")), c(3, 2))
  expect_identical(c(fmt_round(2.5, 0), fmt_round(2.5, 0, rounding = "r")), c("3", "2"))
  expect_identical(c(fmt_signif(2.5, 1), fmt_signif(2.5, 1, rounding = "r")), c("3", "2"))
  expect_identical(c(fmt_numeric(df, "v", digits = 0)$v, fmt_numeric(df, "v", digits = 0, rounding = "r")$v),
                   c("3", "2"))
  # 1/16 = 6.25 percent: "sas" 6.3, "r" 6.2
  expect_identical(c(fmt_count_pct(1L, 1 / 16, nbsp = " "),
                     fmt_count_pct(1L, 1 / 16, nbsp = " ", rounding = "r")),
                   c("  1  (6.3)", "  1  (6.2)"))
  # the option "r": the call's "sas" wins the other way
  options(rtfreporter.rounding = "r")
  expect_identical(c(round_num(2.5, 0), round_num(2.5, 0, rounding = "sas")), c(2, 3))
  expect_identical(c(fmt_count_pct(1L, 1 / 16, nbsp = " "),
                     fmt_count_pct(1L, 1 / 16, nbsp = " ", rounding = "sas")),
                   c("  1  (6.2)", "  1  (6.3)"))
})

test_that("FT-IX5720 [FR-IX362]: small decides what digits means below 1: signif counts from the first significant digit, fixed counts the integer part", {
  # 0.0004567 at 4 digits: signif keeps 4 significant digits, fixed keeps 3 decimals
  expect_identical(fmt_signif(0.0004567, 4), "0.0004567")
  expect_identical(fmt_signif(0.0004567, 4, small = "fixed"), "0.000")
  expect_identical(fmt_signif(c(0.0004567, 0.012345), 2), c("0.00046", "0.012"))
  expect_identical(fmt_signif(c(0.0004567, 0.012345), 2, small = "fixed"), c("0.0", "0.0"))
  df <- data.frame(v = 0.0004567)
  expect_identical(fmt_numeric(df, "v", signif = 4)$v, "0.0004567")
  expect_identical(fmt_numeric(df, "v", signif = 4, small = "fixed")$v, "0.000")
})

test_that("FT-IX5721 [FR-IX367]: the na token follows the other cells of the column: its percent sign, and no padding when nothing matches", {
  cp <- function(x, ...) {
    df <- data.frame(lab = seq_along(x), a = x, stringsAsFactors = FALSE)
    .t7_plain(as_rtftables(df, align_count_pct = TRUE, ...)[[1L]]$data$a)
  }
  # a column with a percent sign: every branch 11 wide, the token's cell too ("  -" + 8)
  signed <- cp(c("5 (5.0%)", "14 (50.0%)", NA), na = "-")
  expect_identical(signed, c("  5  (5.0%)", " 14 (50.0%)", "  -        "))
  expect_identical(nchar(signed), rep(11L, 3L))
  # without the sign: 10 wide ("  -" + 7)
  plain <- cp(c("5 (5.0)", "14 (50.0)", NA), na = "-")
  expect_identical(plain, c("  5  (5.0)", " 14 (50.0)", "  -       "))
  # no cell of the column matches "n (xx.x)": the token is left as it is, unpadded
  expect_identical(cp(c("text", NA), na = "-"), c("text", "-"))
  expect_identical(cp(c("text", "-", NA), na = "-"), c("text", "-", "-"))
})

test_that("FT-FM5708 [FR-FM146]: fmt_signif(digits = ) that is not a single positive integer stops with the function's message", {
  msg <- "`fmt_signif(digits = )` must be a single positive integer."
  for (bad in list(0, 0L, -1, 1.5, NA, Inf, c(1, 2), "2", TRUE)) {
    expect_error(fmt_signif(1.234, digits = bad), msg, fixed = TRUE)
  }
  # zero significant digits would print nothing of a value below 1; 1 is the minimum
  expect_identical(fmt_signif(0.012345, digits = 1), "0.01")
  expect_identical(fmt_signif(123.456, digits = 1L), "123")
})
