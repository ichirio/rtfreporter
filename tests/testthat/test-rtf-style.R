# rtf_style() (#804): one style object for every module.  The order, highest
# first: the module's own argument > the rtf_style() section > the rtf_style()
# shared level > the rtfreporter.* option > the resource baseline.

.sty_df <- function() data.frame(A = c("a1", "a2"), B = c("b1", "b2"))

# The rows of a rendered document that carry `text` in a cell.
.sty_rows <- function(txt, text) {
  rows <- regmatches(txt, gregexpr("\\\\trowd.*?\\\\row", txt))[[1L]]
  rows[grepl(text, rows, fixed = TRUE)]
}

.sty_doc <- function(style = NULL, tbl = rtftable(.sty_df()), header = NULL,
                     footer = NULL, titles = NULL, footnotes = NULL) {
  doc <- rtf_document(style = style) |>
    rtf_section(page = 1, secinfo = list(header = header, footer = footer)) |>
    rtf_tables(list(tbl))
  if (!is.null(titles)) doc <- rtf_titles(doc, titles)
  if (!is.null(footnotes)) doc <- rtf_footnotes(doc, footnotes)
  doc
}

.sty_report <- function(doc) rtfreporter:::.pipe_doc_to_rtfreport(doc)

test_that("FT-DC231 [FR-DC308 FR-DC317 FR-DC324]: rtf_style() is an object of its own; rtf_document(style = ) keeps it; it prints what is set", {
  st <- rtf_style(font_size_half_points = 20L, table = list(cell_valign = "top"))
  expect_s3_class(st, "rtf_style")
  expect_identical(st$font_size_half_points, 20L)
  expect_null(st$row_height_twips)
  expect_identical(st$table, list(cell_valign = "top"))
  expect_identical(st$header, list())
  # a NULL in a section is unset
  expect_identical(rtf_style(header = list(color = NULL))$header, list())
  doc <- rtf_document(style = st)
  expect_identical(doc$document$style, st)
  expect_null(rtf_document()$document$style)
  out <- capture.output(print(st))
  expect_identical(out[1L], "<rtf_style>")
  expect_true(any(grepl("font_size_half_points = 20", out, fixed = TRUE)))
  expect_true(any(grepl("table: cell_valign = \"top\"", out, fixed = TRUE)))
  expect_identical(capture.output(print(rtf_style())), c("<rtf_style>", "  (nothing set)"))
})

test_that("FT-DC232 [FR-DC309 FR-DC312]: the shared level and the table section reach the content table in the RTF", {
  txt <- .render_to_string(.sty_doc(rtf_style(
    font = "Arial", font_size_half_points = 20L, row_height_twips = 300L,
    cell_padding_left_twips = 40L, cell_padding_right_twips = 50L,
    cell_padding_top_twips = 20L, cell_padding_bottom_twips = 25L)))
  expect_true(grepl("\\fs20", txt, fixed = TRUE))
  expect_true(grepl("Arial", txt, fixed = TRUE))
  body <- .sty_rows(txt, "a1\\cell")
  expect_length(body, 1L)
  expect_true(grepl("\\trrh300", body, fixed = TRUE))
  expect_true(grepl("\\li40\\ri50", body, fixed = TRUE))
  expect_true(grepl("\\clpadl20\\clpadfl3\\clpadb25\\clpadfb3", body, fixed = TRUE))

  txt <- .render_to_string(.sty_doc(rtf_style(table = list(
    font_size_half_points = 16L, header_row_height_twips = 260L,
    cell_padding_left_twips = 60L, cell_valign = "center",
    table_align = "center", row_height_exact = TRUE, markup = "none"))))
  body <- .sty_rows(txt, "a1\\cell")
  expect_true(grepl("\\fs16", body, fixed = TRUE))
  expect_true(grepl("\\li60", body, fixed = TRUE))
  expect_true(grepl("\\clvertalc", body, fixed = TRUE))
  expect_true(grepl("\\trqc", body, fixed = TRUE))
  # the header row: its own height, exact
  hdr <- .sty_rows(txt, " A\\cell")
  expect_true(grepl("\\trrh-260", hdr, fixed = TRUE))
  tb <- .sty_report(.sty_doc(rtf_style(table = list(markup = "none"))))$pages[[1]]$content
  expect_identical(tb$markup, character(0))
})

test_that("FT-DC233 [FR-DC313]: the titles and footnotes sections reach their blocks", {
  doc <- .sty_doc(rtf_style(
    titles    = list(font_size_half_points = 24L, align = "left", font = "Arial"),
    footnotes = list(font_size_half_points = 14L, row_height_twips = 333L,
                     border = rtf_border(top = TRUE), markup = "all")),
    titles = "TTL", footnotes = "FNT")
  rep <- .sty_report(doc)
  expect_identical(rep$document$title_style$font_size_half_points, 24L)
  expect_identical(rep$document$title_style$align, "left")
  expect_identical(rep$document$footnote_style$markup, c("script", "relational"))
  expect_s3_class(rep$document$footnote_style$border, "rtf_border")
  txt <- .render_to_string(doc)
  expect_true(grepl("\\fs24", txt, fixed = TRUE))
  fn <- .sty_rows(txt, "FNT")
  expect_true(grepl("\\trrh333", fn, fixed = TRUE))
  expect_true(grepl("\\fs14", fn, fixed = TRUE))
})

test_that("FT-DC234 [FR-DC310 FR-DC314]: the header and footer sections reach their bands; the shared colour too", {
  doc <- .sty_doc(rtf_style(
    color  = "#7f7f7f",
    header = list(font_size_half_points = 16L, row_height_twips = 400L,
                  cell_padding_left_twips = 30L, cell_padding_top_twips = 15L),
    footer = list(border = rtf_border(bottom = TRUE), color = "#C00000")),
    header = rtf_header(c(l = "HDR")), footer = rtf_footer(c(l = "FTR")))
  rep <- .sty_report(doc)
  h <- rep$sections[[1]]$header
  f <- rep$sections[[1]]$footer
  expect_identical(h$color, "#7F7F7F")
  expect_identical(f$color, "#C00000")
  expect_identical(f$border, rtf_border(bottom = TRUE))
  # a band written as a bare vector takes the section too
  rep2 <- .sty_report(.sty_doc(rtf_style(header = list(font_size_half_points = 16L)),
                               header = c(l = "HDR")))
  expect_identical(rep2$sections[[1]]$header$font_size_half_points, 16L)
  txt <- .render_to_string(doc)
  hr <- .sty_rows(txt, "HDR")
  expect_true(grepl("\\trrh400", hr, fixed = TRUE))
  expect_true(grepl("\\fs16", hr, fixed = TRUE))
  expect_true(grepl("\\li30", hr, fixed = TRUE))
  expect_true(grepl("\\clpadl15\\clpadfl3", hr, fixed = TRUE))
  # the colour is written once rtf_header() / rtf_footer() take one (#800)
  skip_if_not("color" %in% names(formals(rtf_header)),
              "band colour not written yet (#800)")
  expect_true(grepl("\\cf[0-9]+ HDR", txt))
  expect_true(grepl("\\cf[0-9]+ FTR", txt))
})

test_that("FT-DC235 [FR-DC311]: title_format, footnote_format, title_width and footnote_width are shared keys", {
  doc <- .sty_doc(rtf_style(title_format = "table", footnote_format = "text",
                            title_width = "page", footnote_width = 0.5),
                  titles = "TTL", footnotes = "FNT")
  df <- .sty_report(doc)$document$default_format
  expect_identical(df$title_format, "table")
  expect_identical(df$footnote_width, 0.5)
  txt <- .render_to_string(doc)
  expect_length(.sty_rows(txt, "TTL"), 1L)         # the title is a table
  expect_length(.sty_rows(txt, "FNT"), 0L)         # the footnote is text
  expect_true(grepl("FNT", txt, fixed = TRUE))
})

test_that("FT-DC236 [FR-DC315 FR-DC316]: the module's argument > the section > the shared level > the option", {
  old <- options(rtfreporter.row_height_twips = 290L,
                 rtfreporter.cell_padding_left_twips = 10L)
  on.exit(options(old), add = TRUE)
  rh <- function(style, tbl = rtftable(.sty_df())) {
    body <- .sty_rows(.render_to_string(.sty_doc(style, tbl)), "a1\\cell")
    c(trrh = regmatches(body, regexpr("\\\\trrh[0-9]+", body)),
      li = regmatches(body, regexpr("\\\\li[0-9]+", body)))
  }
  expect_identical(rh(NULL), c(trrh = "\\trrh290", li = "\\li10"))       # the option
  shared <- rtf_style(row_height_twips = 300L, cell_padding_left_twips = 20L)
  expect_identical(rh(shared), c(trrh = "\\trrh300", li = "\\li20"))      # shared > option
  both <- rtf_style(row_height_twips = 300L, cell_padding_left_twips = 20L,
                    table = list(row_height_twips = 310L,
                                 cell_padding_left_twips = 30L))
  expect_identical(rh(both), c(trrh = "\\trrh310", li = "\\li30"))        # section > shared
  own <- rtftable(.sty_df(), row_height_twips = 320L, cell_padding_left_twips = 40L)
  expect_identical(rh(both, own), c(trrh = "\\trrh320", li = "\\li40"))   # argument > section
  # the same through as_rtftables() and an rtf_table_style()
  own2 <- as_rtftables(.sty_df(), row_height_twips = 320L)[[1]]
  expect_identical(rh(both, own2)[["trrh"]], "\\trrh320")
  own3 <- rtftable(.sty_df(), style = rtf_table_style(row_height_twips = 330L))
  expect_identical(rh(both, own3)[["trrh"]], "\\trrh330")

  # a setting whose default is a value: passed, it wins; left out, the section's
  sec <- rtf_style(table = list(cell_valign = "center", table_align = "right"))
  tb <- function(t) .sty_report(.sty_doc(sec, t))$pages[[1]]$content
  expect_identical(tb(rtftable(.sty_df()))$cell_valign, "center")
  expect_identical(tb(rtftable(.sty_df()))$table_align, "right")
  expect_identical(tb(rtftable(.sty_df(), cell_valign = "bottom"))$cell_valign, "bottom")
  expect_identical(tb(as_rtftables(.sty_df())[[1]])$cell_valign, "center")
  expect_identical(tb(as_rtftables(.sty_df(), table_align = "left")[[1]])$table_align, "left")

  # a size set without a height keeps the height its size implies
  t20 <- .sty_report(.sty_doc(both, rtftable(.sty_df(), font_size_half_points = 20L)))
  expect_null(t20$pages[[1]]$content$row_height_twips)

  # a band's border: passed, it wins (even the default value); left out, the section's
  b <- rtf_border(bottom = TRUE)
  fb <- function(f) .sty_report(.sty_doc(rtf_style(footer = list(border = b)),
                                         footer = f))$sections[[1]]$footer$border
  expect_identical(fb(rtf_footer(c(l = "F"))), b)
  expect_identical(fb(rtf_footer(c(l = "F"), border = rtf_border(top = TRUE))),
                   rtf_border(top = TRUE))
  hb <- .sty_report(.sty_doc(rtf_style(header = list(border = b)),
                             header = rtf_header(c(l = "H"), border = NULL)))
  expect_null(hb$sections[[1]]$header$border)

  # the titles' and the bands' own arguments win as well
  rep <- .sty_report(.sty_doc(rtf_style(footnotes = list(font_size_half_points = 16L),
                                        header = list(font_size_half_points = 16L)),
                              header = rtf_header(c(l = "H"), font_size_half_points = 22L),
                              titles = "T") |>
                       rtf_footnotes("F", font_size_half_points = 14L))
  expect_identical(rep$document$footnote_style$font_size_half_points, 14L)
  expect_identical(rep$sections[[1]]$header$font_size_half_points, 22L)
})

test_that("FT-DC237 [FR-DC318]: several rtf_style() objects through rtf_config() merge key by key, the later winning", {
  doc <- rtf_document(style = rtf_style(font_size_half_points = 20L,
                                        row_height_twips = 300L,
                                        table = list(cell_valign = "top",
                                                     table_align = "center"))) |>
    rtf_config(style = rtf_style(row_height_twips = 320L,
                                 table = list(cell_valign = "center"),
                                 header = list(color = "#7F7F7F"))) |>
    rtf_config(style = rtf_style(markup = "all"))
  st <- doc$document$style
  expect_identical(st$font_size_half_points, 20L)
  expect_identical(st$row_height_twips, 320L)
  expect_identical(st$markup, c("script", "relational"))
  expect_identical(st$table, list(cell_valign = "center", table_align = "center"))
  expect_identical(st$header, list(color = "#7F7F7F"))
  # a document without one takes the first as it is
  expect_identical(rtf_config(rtf_document(), style = rtf_style(font = "Arial"))$document$style,
                   rtf_style(font = "Arial"))
  # NULL changes nothing
  expect_identical(rtf_config(doc, style = NULL)$document$style, st)
})

test_that("FT-DC239 [FR-DC320 FR-DC321 FR-DC322 FR-DC323]: an unknown key or section, a bad value, a style that is not one", {
  expect_error(rtf_style(header = list(colour = "#000000")),
               "`rtf_style(header = )`: unknown key `colour`.  The keys of `header` are font,",
               fixed = TRUE)
  expect_error(rtf_style(table = list(bold = TRUE, align = "left")),
               "unknown key `bold`, `align`", fixed = TRUE)
  expect_error(rtf_style(titles = list(cell_padding_left_twips = 10L)),
               "The keys of `titles` are font, font_size_half_points, row_height_twips, markup, align.",
               fixed = TRUE)
  expect_error(rtf_style(tables = list(font = "Arial")),
               "`rtf_style()`: unknown argument `tables`.  The sections are table, titles, footnotes, header, footer",
               fixed = TRUE)
  expect_error(rtf_style(font = "Arial", NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 1),
               "unknown argument `<unnamed>`", fixed = TRUE)
  expect_error(rtf_style(bogus = 1, colour = 2), "unknown argument `bogus`, `colour`",
               fixed = TRUE)
  expect_error(rtf_style(table = "center"), "`rtf_style(table = )` must be a named list",
               fixed = TRUE)
  expect_error(rtf_style(table = rtf_table_style()), "must be a named list", fixed = TRUE)
  expect_error(rtf_style(header = list("#000000")), "every entry must be named", fixed = TRUE)
  expect_error(rtf_style(header = list(color = "#000000", color = "#FFFFFF")),
               "`color` is given twice", fixed = TRUE)
  # the values, checked as their modules check them
  expect_error(rtf_style(color = "grey"),
               "`rtf_style(color)` must be NULL or a 6-digit hex colour", fixed = TRUE)
  expect_error(rtf_style(header = list(color = "#12345")),
               "`rtf_style(header)$color` must be NULL or a 6-digit hex colour", fixed = TRUE)
  expect_error(rtf_style(font_size_half_points = 0), "rtf_style(font_size_half_points)",
               fixed = TRUE)
  expect_error(rtf_style(table = list(row_height_twips = -1)),
               "`rtf_style(table)$row_height_twips` must be a single non-negative", fixed = TRUE)
  expect_error(rtf_style(table = list(cell_valign = "middle")),
               "`rtf_style(table)$cell_valign` must be \"top\", \"center\" or \"bottom\".",
               fixed = TRUE)
  expect_error(rtf_style(table = list(table_align = "middle")), "table_align", fixed = TRUE)
  expect_error(rtf_style(table = list(row_height_exact = NA)), "must be TRUE or FALSE",
               fixed = TRUE)
  expect_error(rtf_style(footer = list(border = TRUE)), "must be NULL or an rtf_border()",
               fixed = TRUE)
  expect_error(rtf_style(markup = "bogus"), "`markup` must be a subset", fixed = TRUE)
  expect_error(rtf_style(title_format = "fancy"),
               "`rtf_style(title_format)` must be \"text\" or \"table\".", fixed = TRUE)
  expect_error(rtf_style(footnote_width = "wide"), "`rtf_style(footnote_width)` must be",
               fixed = TRUE)
  expect_error(rtf_style(font = 1), "`rtf_style(font)` must be a single font family",
               fixed = TRUE)
  expect_error(rtf_document(style = list(font = "Arial")),
               "`rtf_document(style = )` must be NULL or an rtf_style() object.", fixed = TRUE)
  expect_error(rtf_config(rtf_document(), style = rtf_page()),
               "`rtf_config(style = )` must be NULL or an rtf_style() object.", fixed = TRUE)
})

test_that("FT-DC240 [FR-DC308 FR-DC315]: the example of #804 runs, and each report setting wins over the study's", {
  study <- rtf_style(
    font = "Arial", font_size_half_points = 18L,
    cell_padding_left_twips = 40L, cell_padding_right_twips = 40L,
    table     = list(header_row_height_twips = 260L, cell_valign = "center"),
    header    = list(color = "#7F7F7F"),
    footnotes = list(font_size_half_points = 16L)
  )
  adsl_tbl <- data.frame(Parameter = c("Age, Mean (SD)", "Sex: Female, n (%)"),
                         Placebo = c("75.2 (8.6)", "53 (61.6)"))
  doc <- rtf_document(style = study) |>
    rtf_tables(as_rtftables(adsl_tbl), titles = "Table 14.1.1") |>
    rtf_footnotes("Source: ADSL", font_size_half_points = 14L) |>  # this report: 7 pt wins
    rtf_section(header = rtf_header(c(l = "Study XYZ")))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  expect_identical(generate_rtfreport(doc, f), f)
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_true(grepl("\\clvertalc", .sty_rows(txt, "75.2 (8.6)\\cell"), fixed = TRUE))
  expect_true(grepl("\\trrh260", .sty_rows(txt, "Placebo\\cell"), fixed = TRUE))
  expect_true(grepl("\\fs14", .sty_rows(txt, "Source: ADSL"), fixed = TRUE))
  expect_true(grepl("\\li40\\ri40", .sty_rows(txt, "Study XYZ"), fixed = TRUE))
  expect_true(grepl("Arial", txt, fixed = TRUE))
})
