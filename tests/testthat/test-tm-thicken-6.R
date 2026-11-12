# Thickening of the functional tests, batch 6: the document and its helpers
# (area DC) and the list of leaves of area LS.  Each block asserts what its FR
# states, the figures worked out in a comment.  FT ids DC5601 up, LS5601 up.

.t6_tb <- function() data.frame(A = c("1", "2"), B = c("x", "y"), stringsAsFactors = FALSE)

.t6_render <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# the lines of a rendered file that hold `what`
.t6_lines <- function(txt, what) {
  l <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  l[grepl(what, l, fixed = TRUE)]
}

.t6_count <- function(txt, what) lengths(regmatches(txt, gregexpr(what, txt, fixed = TRUE)))

test_that("FT-DC5601 [FR-DC110]: rtf_document(watermark = list(1, 2)) stops with the message; a vector of two strings, a number and a zero-length value stop the same way", {
  msg <- "`watermark` must be an rtf_watermark object, a single string, or NULL / NA for none."
  expect_error(rtf_document(watermark = list(1, 2)), msg, fixed = TRUE)
  expect_error(rtf_document(watermark = c("A", "B")), msg, fixed = TRUE)
  expect_error(rtf_document(watermark = 1), msg, fixed = TRUE)
  expect_error(rtf_document(watermark = character(0)), msg, fixed = TRUE)
  # what is allowed does not stop
  expect_no_error(rtf_document(watermark = "DRAFT"))
  expect_no_error(rtf_document(watermark = NULL))
  expect_no_error(rtf_document(watermark = NA))
  expect_no_error(rtf_document(watermark = rtf_watermark("DRAFT")))
})

test_that("FT-DC5602 [FR-DC114]: rtf_document(program_fallback = 1) stops with the message; so do an empty string, NA and two strings", {
  msg <- "`program_fallback` must be a single string: the path of the program that writes the file."
  expect_error(rtf_document(program_fallback = 1), msg, fixed = TRUE)
  expect_error(rtf_document(program_fallback = ""), msg, fixed = TRUE)
  expect_error(rtf_document(program_fallback = NA_character_), msg, fixed = TRUE)
  expect_error(rtf_document(program_fallback = c("a.R", "b.R")), msg, fixed = TRUE)
  expect_no_error(rtf_document(program_fallback = "work/t_dm.R"))
  expect_no_error(rtf_document(program_fallback = NULL))
})

test_that("FT-DC5603 [FR-DC230]: rtf_watermark(font = ) of two names stops with the message; a slot vector with an unknown slot stops naming it", {
  expect_error(rtf_watermark("X", font = c("A", "B")),
               "`font` must be NULL or one font-family name.", fixed = TRUE)
  expect_error(rtf_watermark("X", font = c(latin = "A", bad = "B")),
               "`font`: unknown font slot(s) \"bad\"; use latin, hansi, east_asian, complex.",
               fixed = TRUE)
  expect_error(rtf_watermark("X", font = c(latin = "A", bad = "B", worse = "C")),
               "`font`: unknown font slot(s) \"bad\", \"worse\"; use latin, hansi, east_asian, complex.",
               fixed = TRUE)
  # one name, NULL and a slot vector of known slots are accepted
  expect_no_error(rtf_watermark("X", font = "Arial"))
  expect_no_error(rtf_watermark("X", font = NULL))
  expect_no_error(rtf_watermark("X", font = c(latin = "Arial", east_asian = "MS Mincho")))
})

test_that("FT-DC5604 [FR-DC102]: font_table declares the fonts: the first is the default, every entry is in \\fonttbl; the default is the font option", {
  # the default is list(list(name = "Courier New")), from the option
  expect_identical(rtf_document()$document$font_table, list(list(name = "Courier New")))
  txt0 <- .t6_render(rtf_tables(rtf_document(), .t6_tb()))
  expect_identical(.t6_count(txt0, "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}}"), 1L)
  old <- options(rtfreporter.font = "Verdana")
  on.exit(options(old), add = TRUE)
  expect_identical(.t6_count(.t6_render(rtf_tables(rtf_document(), .t6_tb())),
                             "{\\fonttbl{\\f0\\fnil\\fcharset0 Verdana;}}"), 1L)
  options(old)
  # two fonts: both in the table, the first is f0 (\deff0) and the text uses it with no \f of its own
  doc <- rtf_document(font_table = list(list(name = "Arial"), list(name = "Times New Roman")))
  expect_identical(doc$document$font_table[[1L]]$name, "Arial")
  txt <- .t6_render(rtf_tables(doc, .t6_tb()))
  expect_identical(.t6_count(
    txt, "{\\fonttbl{\\f0\\fnil\\fcharset0 Arial;}{\\f1\\fnil\\fcharset0 Times New Roman;}}"), 1L)
  expect_true(grepl("\\deff0", txt, fixed = TRUE))
  cell <- .t6_lines(txt, "1\\cell")
  expect_length(cell, 1L)
  expect_false(grepl("\\f1", cell, fixed = TRUE))
  expect_false(grepl("Courier", txt, fixed = TRUE))
})

test_that("FT-DC5605 [FR-DC103]: a font_table name that is a vector per script slot writes the slots; a single string writes no slot controls", {
  slots <- c(latin = "Arial", east_asian = "MS Gothic", complex = "Arial", hansi = "Calibri")
  txt <- .t6_render(rtf_tables(rtf_document(font_table = list(list(name = slots))), .t6_tb()))
  # the families: latin f0, hansi f1, east_asian f2 (complex = latin shares f0)
  expect_identical(.t6_count(
    txt, "{\\fonttbl{\\f0\\fnil\\fcharset0 Arial;}{\\f1\\fnil\\fcharset0 Calibri;}{\\f2\\fnil\\fcharset0 MS Gothic;}}"),
    1L)
  # the document's default of each slot and the text's runs
  expect_true(grepl("\\adeff0\\stshfdbch2\\stshfloch0\\stshfhich1\\stshfbi0", txt, fixed = TRUE))
  cell <- .t6_lines(txt, "1\\cell")
  expect_true(grepl("\\rtlch\\af0\\ltrch\\hich\\af1\\dbch\\af2\\loch\\f0 1\\cell", cell, fixed = TRUE))
  # a single string keeps its meaning: no slot controls, the same table
  one <- .t6_render(rtf_tables(rtf_document(font_table = list(list(name = "Arial"))), .t6_tb()))
  expect_true(grepl("{\\fonttbl{\\f0\\fnil\\fcharset0 Arial;}}", one, fixed = TRUE))
  expect_false(grepl("\\adeff", one, fixed = TRUE))
  expect_false(grepl("\\dbch", one, fixed = TRUE))
  expect_true(grepl("\\ql\\li0\\ri0 1\\cell", one, fixed = TRUE))
  # latin alone is the same as the single string in the font table
  latin <- .t6_render(rtf_tables(rtf_document(font_table = list(list(name = c(latin = "Arial")))), .t6_tb()))
  expect_true(grepl("{\\fonttbl{\\f0\\fnil\\fcharset0 Arial;}}", latin, fixed = TRUE))
})

test_that("FT-DC5606 [FR-DC105]: color_table declares colours by index; the default is c(\"#000000\"); black and white are reserved, not repeated", {
  expect_identical(rtf_document()$document$color_table, c("#000000"))
  def <- .t6_render(rtf_tables(rtf_document(), .t6_tb()))
  expect_identical(.t6_count(def, "{\\colortbl;\\red0\\green0\\blue0;\\red255\\green255\\blue255;}"), 1L)
  # a declared colour is the third entry, after black and white, and is used by that index
  doc <- rtf_document(color_table = c("#000000", "#1F4E79")) |>
    rtf_tables(rtftable(.t6_tb(), col_spec = list(list(col = 1, color = "#1F4E79"))))
  txt <- .t6_render(doc)
  expect_identical(.t6_count(
    txt, "{\\colortbl;\\red0\\green0\\blue0;\\red255\\green255\\blue255;\\red31\\green78\\blue121;}"), 1L)
  expect_true(grepl("\\cf3 1\\cf1 \\cell", txt, fixed = TRUE))
  # white and black declared again are not repeated; the colour table has the one of each
  rep3 <- .t6_render(rtf_tables(
    rtf_document(color_table = c("#000000", "#FFFFFF", "#1F4E79")), .t6_tb()))
  expect_identical(.t6_count(rep3, "\\red0\\green0\\blue0;"), 1L)
  expect_identical(.t6_count(rep3, "\\red255\\green255\\blue255;"), 1L)
  expect_identical(.t6_count(rep3, "\\red31\\green78\\blue121;"), 1L)
  # colours of borders and of col_spec / cell_styles are added automatically
  hdr <- rtf_header(rows = list(c(l = "H")),
                    border = rtf_border(bottom = rtf_border_line("single", 15, color = "#00AA00")))
  auto <- .t6_render(
    rtf_tables(rtf_section(rtf_document(), page = 1, secinfo = list(header = hdr, footer = NULL)),
               rtftable(.t6_tb(), col_spec = list(list(col = 2, color = "#FF0000")))))
  expect_true(grepl("\\red0\\green170\\blue0;", auto, fixed = TRUE))
  expect_true(grepl("\\red255\\green0\\blue0;", auto, fixed = TRUE))
})

test_that("FT-DC5607 [FR-DC115]: tokens of one's own fill headers, footers, titles and footnotes when the file is written; NULL gives none", {
  mk <- function(...) {
    rtf_document(...) |>
      rtf_section(page = 1, secinfo = list(
        header = rtf_header(c(l = "H {STUDY}", r = "C {CUTOFF}")),
        footer = rtf_footer(c(l = "F {STUDY}")))) |>
      rtf_tables(.t6_tb(), titles = list("T {STUDY} / {CUTOFF}"),
                 footnotes = list(list("N {CUTOFF}")))
  }
  doc <- mk(tokens = list(STUDY = "ABC-123", CUTOFF = "01JUN2026"))
  expect_identical(doc$document$tokens, list(STUDY = "ABC-123", CUTOFF = "01JUN2026"))
  txt <- .t6_render(doc)
  expect_true(grepl("H ABC-123\\cell", txt, fixed = TRUE))       # header
  expect_true(grepl("C 01JUN2026\\cell", txt, fixed = TRUE))
  expect_true(grepl("F ABC-123\\cell", txt, fixed = TRUE))       # footer
  expect_true(grepl("T ABC-123 / 01JUN2026", txt, fixed = TRUE)) # title
  expect_true(grepl("N 01JUN2026\\cell", txt, fixed = TRUE))     # footnote
  expect_false(grepl("STUDY", txt, fixed = TRUE))
  expect_false(grepl("CUTOFF", txt, fixed = TRUE))
  # NULL (the default): none, the braces are printed as written
  expect_null(rtf_document()$document$tokens)
  none <- .t6_render(mk())
  expect_true(grepl("H \\{STUDY\\}\\cell", none, fixed = TRUE))
  expect_true(grepl("N \\{CUTOFF\\}\\cell", none, fixed = TRUE))
  expect_false(grepl("ABC-123", none, fixed = TRUE))
})

test_that("FT-DC5608 [FR-DC281]: {DATETIME} is the time the file is written, in the option's format (default %d%b%Y  %H:%M); {DATETIME:<format>} takes another; every page shows the same time and rtfreporter.render_time fixes it", {
  old <- options(rtfreporter.render_time = as.POSIXct("2026-09-25 10:05:07"),
                 rtfreporter.datetime_format = NULL)
  on.exit(options(old), add = TRUE)
  mk <- function() {
    rtf_document() |>
      rtf_tables(list(data.frame(A = "p1"), data.frame(A = "p2")),
                 titles = list("D1 {DATETIME} D2 {DATETIME:%Y-%m-%d} D3 {DATETIME:%H%M%S}"))
  }
  txt <- .t6_render(mk())
  # the default format is "%d%b%Y  %H:%M" (two spaces): 25Sep2026  10:05
  expect_identical(getOption("rtfreporter.datetime_format", "%d%b%Y  %H:%M"), "%d%b%Y  %H:%M")
  d <- .t6_lines(txt, "D1 ")
  expect_length(d, 2L)                      # one title row for each of the two pages
  expect_identical(d[1L], d[2L])            # the same time on every page
  expect_true(grepl("D1 25Sep2026  10:05 D2 2026-09-25 D3 100507", d[1L], fixed = TRUE))
  # the option's format
  options(rtfreporter.datetime_format = "%Y/%m/%d")
  txt2 <- .t6_render(mk())
  expect_true(grepl("D1 2026/09/25 D2 2026-09-25 D3 100507", txt2, fixed = TRUE))
  # without the fixed time the file takes the time it is written: once, so both pages agree
  options(rtfreporter.render_time = NULL, rtfreporter.datetime_format = "%Y%m%d%H%M%S")
  d3 <- .t6_lines(.t6_render(mk()), "D1 ")
  expect_length(d3, 2L)
  expect_identical(d3[1L], d3[2L])
  expect_true(grepl("D1 20[0-9]{12} ", d3[1L]))
})

test_that("FT-DC5609 [FR-DC311]: title_format / footnote_format are text (paragraphs) or table (a one-column table), title_width / footnote_width their width; NULL takes the option, then text for the title and table for the footnote", {
  mk <- function(style = NULL) {
    rtf_document(style = style) |>
      rtf_tables(.t6_tb(), titles = list("TTL"), footnotes = list(list("FNO")))
  }
  row_of <- function(txt, what) .t6_lines(txt, what)[1L]
  is_table <- function(l) grepl("\\trowd", l, fixed = TRUE) || grepl("\\row", l, fixed = TRUE)
  # the defaults: the title is text, the footnote is a table
  def <- .t6_render(mk())
  expect_false(is_table(row_of(def, "TTL")))
  expect_true(is_table(row_of(def, "FNO")))
  # the style says the other way round
  alt <- .t6_render(mk(rtf_style(title_format = "table", footnote_format = "text")))
  expect_true(is_table(row_of(alt, "TTL")))
  expect_false(is_table(row_of(alt, "FNO")))
  # left NULL, the option; the style wins over the option
  old <- options(rtfreporter.title_format = "table", rtfreporter.footnote_format = "text")
  on.exit(options(old), add = TRUE)
  opt <- .t6_render(mk())
  expect_true(is_table(row_of(opt, "TTL")))
  expect_false(is_table(row_of(opt, "FNO")))
  st <- .t6_render(mk(rtf_style(title_format = "text", footnote_format = "table")))
  expect_false(is_table(row_of(st, "TTL")))
  expect_true(is_table(row_of(st, "FNO")))
  options(old)
  # the widths of the table form: twips, a fraction of the writable width, "page", "content"
  # (landscape Letter, 0.75 in margins: 15840 - 2 * 1080 = 13680 twips)
  last_x <- function(l) as.integer(utils::tail(regmatches(l, gregexpr("(?<=\\\\cellx)[0-9]+", l, perl = TRUE))[[1L]], 1L))
  w <- function(...) .t6_render(mk(rtf_style(title_format = "table", footnote_format = "table", ...)))
  tw <- w(title_width = 5000, footnote_width = 4000)
  expect_identical(last_x(row_of(tw, "TTL")), 5000L)
  expect_identical(last_x(row_of(tw, "FNO")), 4000L)
  fr <- w(title_width = 0.5, footnote_width = 0.25)
  expect_identical(last_x(row_of(fr, "TTL")), 6840L)       # 13680 * 0.5
  expect_identical(last_x(row_of(fr, "FNO")), 3420L)       # 13680 * 0.25
  pg <- w(title_width = "page", footnote_width = "page")
  expect_identical(last_x(row_of(pg, "TTL")), 13680L)
  expect_identical(last_x(row_of(pg, "FNO")), 13680L)
  # left NULL: the option
  old <- options(rtfreporter.title_width = 5000, rtfreporter.footnote_width = 0.5)
  on.exit(options(old), add = TRUE)
  ow <- w()
  expect_identical(last_x(row_of(ow, "TTL")), 5000L)
  expect_identical(last_x(row_of(ow, "FNO")), 6840L)
})

test_that("FT-DC5610 [FR-DC252]: rtf_replace_text(backup = TRUE), the default, copies the original to <input>.bak before overwriting in place; FALSE makes no copy; it is ignored with output_file", {
  d <- tempfile("t6-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  p <- file.path(d, "a.rtf")
  # the default
  writeLines("one two", p)
  rtf_replace_text(p, "one", "1")
  expect_identical(readLines(p, warn = FALSE), "1 two")
  expect_true(file.exists(paste0(p, ".bak")))
  expect_identical(readLines(paste0(p, ".bak"), warn = FALSE), "one two")   # the original
  expect_identical(formals(rtf_replace_text)$backup, TRUE)
  # TRUE written out: the same
  unlink(paste0(p, ".bak"))
  writeLines("one two", p)
  rtf_replace_text(p, "one", "1", backup = TRUE)
  expect_identical(readLines(paste0(p, ".bak"), warn = FALSE), "one two")
  # FALSE: no copy
  unlink(paste0(p, ".bak"))
  writeLines("one two", p)
  rtf_replace_text(p, "one", "1", backup = FALSE)
  expect_identical(readLines(p, warn = FALSE), "1 two")
  expect_false(file.exists(paste0(p, ".bak")))
  # with output_file the input is not overwritten and no copy is made, backup or not
  for (b in c(TRUE, FALSE)) {
    q <- file.path(d, paste0("out-", b, ".rtf"))
    writeLines("one two", p)
    rtf_replace_text(p, "one", "1", output_file = q, backup = b)
    expect_identical(readLines(q, warn = FALSE), "1 two")
    expect_identical(readLines(p, warn = FALSE), "one two")
    expect_false(file.exists(paste0(p, ".bak")))
    expect_false(file.exists(paste0(q, ".bak")))
  }
})

test_that("FT-DC5611 [FR-DC253]: rtf_replace_text(use_regex = FALSE), the default, treats target as a fixed string; TRUE as a Perl-compatible regular expression", {
  p <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  expect_identical(formals(rtf_replace_text)$use_regex, FALSE)
  # fixed: the metacharacters are literal
  writeLines("a.b axb a+b", p)
  rtf_replace_text(p, "a.b", "Z")
  expect_identical(readLines(p, warn = FALSE), "Z axb a+b")
  writeLines("a.b axb a+b", p)
  rtf_replace_text(p, "a+b", "Z", use_regex = FALSE)
  expect_identical(readLines(p, warn = FALSE), "a.b axb Z")
  writeLines("a.b axb a+b", p)
  rtf_replace_text(p, "[a-z]", "Q")
  expect_identical(readLines(p, warn = FALSE), "a.b axb a+b")      # a literal "[a-z]": no match
  # regex: the metacharacters act
  writeLines("a.b axb a+b", p)
  rtf_replace_text(p, "a.b", "Z", use_regex = TRUE)
  expect_identical(readLines(p, warn = FALSE), "Z Z Z")   # "." is any character: axb and a+b too
  # Perl syntax: a look-ahead, which the default (TRE) syntax does not have
  writeLines("price 10 and 20 EUR", p)
  rtf_replace_text(p, "[0-9]+(?= EUR)", "N", use_regex = TRUE)
  expect_identical(readLines(p, warn = FALSE), "price 10 and N EUR")
})
