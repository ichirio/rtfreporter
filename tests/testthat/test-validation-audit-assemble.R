# Help-to-FR audit, batch 7 (first file): the assembly of a deliverable
# (assemble_rtf(), assemble_folder()) and the figures (rtf_figures(),
# rtfplot(), print() of an rtfplot), all area AS.  The listings are in
# test-validation-audit-listing.R.  The table of contents has no exported
# function of its own: toc_heading() and toc_entry() became internal, and a
# table of contents is given to assemble_rtf(toc = ).
#
# The help is the functional requirement: every statement in a function's Rd
# is an active FR in inst/validation/requirements.csv, and every FR has a
# test.  For each function below, one row per claim of its roxygen: the
# claim (short), the FR that states it, and the test that checks it.  The
# tests below this table close the gaps the audit found.
#
# assemble_rtf()
#   claim                                                   | FR          | test
#   the files joined; the header of the first; invisible    | FR-AS101    | FT-AS015, FT-AS016, FT-AS029, FT-AS067
#   input_files: two or more, in order; left out with a     | FR-AS102/103| FT-AS016, FT-AS020, FT-AS053
#     table                                                 |             |
#   a missing input; a file with no \sectd                  | FR-AS104-106| FT-AS017, FT-AS018, FT-AS118
#   output_file; overwrite FALSE by default                 | FR-AS107/108| FT-AS019, FT-AS067
#   {AUTO_PAGE} / {AUTO_TOTAL_PAGES} across the files;      | FR-AS140/141| FT-AS036, FT-AS037, FT-AS072-FT-AS076, FT-AS126
#     static tokens keep the file's own numbers             |             |
#   cover: title 22 pt, subtitle 14, date / version 11,     | FR-AS109/110| FT-AS027-FT-AS029, FT-AS044
#     meta 10; an empty field skipped                       |             |
#   toc NULL: no contents, no bookmarks, byte for byte      | FR-AS111    | FT-AS023, FT-AS042, FT-AS043
#   toc "auto": the first centred bold cell, else the name  | FR-AS112    | FT-AS021, FT-AS040, FT-AS041, FT-AS129
#   toc as labels, one per file                             | FR-AS113/114| FT-AS024, FT-AS027
#   toc as a table: file, label, heading, level, order      | FR-AS115/116| FT-AS053, FT-AS067
#   ... level a whole number from 1 to 3 (help fixed)       | FR-AS238    | FT-AS133
#   toc as the path of a .xlsx / .csv table                 | FR-AS117    | FT-AS053
#   a table without file / label; missing files; other toc  | FR-AS118/119, FR-AS123/124 | FT-AS053, FT-AS067, FT-AS119
#   HYPERLINK to a bookmark, PAGEREF for the page           | FR-AS125/126| FT-AS011-FT-AS014, FT-AS022, FT-AS126
#   toc_title; toc_leader "dot" / "none"                    | FR-AS127-129| FT-AS048, FT-AS054, FT-AS119
#   one page sequence from the first page, a cover and the  | FR-AS130, FR-AS235, FR-AS270, FR-AS272 | FT-AS014, FT-AS049, FT-AS126, FT-AS7201-FT-AS7203
#     contents counted (#1049); toc_page_numbering removed  | FR-AS271    | FT-AS050, FT-AS7204
#   bookmark_prefix; unique bookmark names                  | FR-AS133/134| FT-AS026, FT-AS031, FT-AS032
#   the outline paragraph; no format leaks; escaping        | FR-AS135-137| FT-AS025, FT-AS030, FT-AS033-FT-AS035, FT-AS039, FT-AS044, FT-AS128
#   book_page: the {BOOK_PAGE} slots, NULL empty; static    | FR-AS138/139| FT-AS059-FT-AS061, FT-AS063
#     tokens refused                                        |             |
#
# assemble_folder()
#   the .rtf files of dir, read from the running header;    | FR-AS148/149| FT-AS053, FT-AS056, FT-AS130
#     the table returned without output_file                |             |
#   ... no "Table N" in the header: the rtf_titles() title  | FR-AS242    | FT-AS137
#     block, as the help's example writes it (#843)         |             |
#   the order: by table number, then by file name, C locale | FR-AS151    | FT-AS056, FT-AS130
#   ... the same table number keeps the file-name order     | FR-AS236    | FT-AS131
#   the title, the "Table N" line, a <...> subtitle         | FR-AS150    | FT-AS058, FT-AS129
#   the columns order, file, table, heading, label, level,  | FR-AS148/149| FT-AS053, FT-AS056
#     pages                                                 |             |
#   ... level a whole number from 1 to 3 (help fixed)       | FR-AS238    | FT-AS133
#   output_file: assembled, list(output, spec) invisibly    | FR-AS152    | FT-AS052
#   spec_file: .xlsx / .csv saved; NULL in memory only      | FR-AS153/154| FT-AS052, FT-AS065, FT-AS066
#   ... written, never read back                            | FR-AS237    | FT-AS132
#   recursive; a missing dir; no .rtf file                  | FR-AS155/156| FT-AS065, FT-AS120
#   toc_* and overwrite as assemble_rtf(); ...              | FR-AS157/158| FT-AS052, FT-AS120
#   an edited table handed to assemble_rtf(toc = )          | FR-AS159    | FT-AS053
#
# rtf_figures()
#   doc an rtf_document; figures appended, one page each    | FR-AS178/179| FT-AS078, FT-AS085, FT-AS099
#   one figure as it is, or a list                          | FR-AS180/182| FT-AS079, FT-AS081, FT-AS099
#   a PNG / JPEG path, a plot object, an rtfplot            | FR-AS181    | FT-AS081, FT-AS099
#   ... an EMF path                                         | FR-AS239    | FT-AS134
#   a table refused; an element that draws nothing          | FR-AS183/184| FT-AS077, FT-AS079, FT-AS121
#   width_twips / height_twips / align for paths and plots; | FR-AS185/186/196 | FT-AS100
#     an rtfplot keeps its own                              |             |
#   titles, footnotes: one block or one per figure          | FR-AS187/188| FT-AS080, FT-AS099
#   page_break; space_before                                | FR-AS189-192| FT-AS071, FT-AS122
#   render_* and png_device for plot objects; a file and an | FR-AS193-195, FR-IX390, FR-AS253 | FT-AS098, FT-AS101, FT-AS102, FT-AS151
#     rtfplot keep theirs                                   |             |
#
# rtfplot()
#   a PNG / JPEG / EMF path, read for size and resolution   | FR-AS197/233| FT-AS104-FT-AS107, FT-AS112, FT-AS124
#   a plot object: printed, grid.draw(), recordPlot(), a    | FR-AS198    | FT-AS068, FT-AS114
#     function                                              |             |
#   width_twips / height_twips; one derives the other       | FR-AS199    | FT-AS088, FT-AS113
#   ... not one positive number stops                       | FR-AS240    | FT-AS135
#   no resolution: the default_dpi option; no fit to page   | FR-AS200    | FT-AS090
#   align                                                   | FR-AS201/202| FT-AS068, FT-AS103, FT-AS104, FT-AS108
#   render_width / render_height / render_dpi; refused for  | FR-AS203-205, FR-AS209 | FT-AS068, FT-AS089, FT-AS094-FT-AS097
#     a file                                                |             |
#   png_device "auto" / "ragg" / "png"                      | FR-AS206-208| FT-AS064, FT-AS091-FT-AS093, FT-AS115
#   not one path; a missing file; another extension         | FR-AS210-212, FR-AS234 | FT-AS086, FT-AS109-FT-AS111, FT-AS125
#   render_format "png" / "emf": a plot object drawn as EMF | FR-AS251    | FT-AS149
#   ... a format not allowed; no devEMF; dpi / device with  | FR-AS252, FR-AS209 | FT-AS150, FT-AS152
#     "emf"; a file with render_format                      |             |
#   embedded as \pict with its blip and display size        | FR-AS214    | FT-AS085, FT-AS087, FT-AS088, FT-AS124
#
# print() of an rtfplot
#   type and file, native size and dpi, display size,       | FR-AS241    | FT-AS136
#     alignment; x invisibly                                |             |
#   an EMF's frame in mm                                    | FR-AS233    | FT-AS124

# A minimal deliverable whose running header carries the "Table N" line and
# the title, as assemble_folder() reads them (invented numbers and titles).
.a7_tbl <- function(dir, file, number, title) {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(c(c = paste0("Table ", number)), c(c = title)))))
  doc <- rtf_tables(doc, data.frame(Parameter = "Age", Value = "51.0"))
  f <- file.path(dir, file)
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}
.a7_dir <- function() {
  d <- tempfile("a7asm")
  dir.create(d)
  d
}

# A PNG of 120 x 80 pixels: the signature, its IHDR, a pHYs chunk when dpm
# (dots per metre) is given, and IEND.  Nothing is drawn, so no device is
# needed; the reader takes the size and the resolution from the chunks.
.a7_png <- function(dpm = NULL) {
  u32 <- function(n) as.raw(c(n %/% 16777216, (n %/% 65536) %% 256, (n %/% 256) %% 256, n %% 256))
  chunk <- function(type, data) c(u32(length(data)), charToRaw(type), data, as.raw(c(0, 0, 0, 0)))
  bytes <- c(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)),
             chunk("IHDR", c(u32(120), u32(80), as.raw(c(8, 2, 0, 0, 0)))),
             if (!is.null(dpm)) chunk("pHYs", c(u32(dpm), u32(dpm), as.raw(1))),
             chunk("IEND", raw(0)))
  f <- tempfile("a7fig", fileext = ".png")
  writeBin(bytes, f)
  f
}

# ---- assemble_folder(), assemble_rtf() ------------------------------------------

test_that("FT-AS131 [FR-AS236]: assemble_folder() keeps the files of one table number in file-name order", {
  d <- .a7_dir()
  .a7_tbl(d, "t_b.rtf", "14.2.1", "Vital signs, second part")
  .a7_tbl(d, "t_a.rtf", "14.2.1", "Vital signs, first part")
  .a7_tbl(d, "t_c10.rtf", "14.1.1", "Demographics")
  .a7_tbl(d, "t_c9.rtf", "14.1.1", "Demographics, continued")
  toc <- assemble_folder(d)
  expect_identical(basename(toc$file), c("t_c9.rtf", "t_c10.rtf", "t_a.rtf", "t_b.rtf"))
  expect_identical(toc$table, c("14.1.1", "14.1.1", "14.2.1", "14.2.1"))
  expect_identical(toc$order, 1:4)
})

test_that("FT-AS132 [FR-AS237]: assemble_folder(spec_file = ) is written, never read back", {
  d <- .a7_dir()
  .a7_tbl(d, "t1.rtf", "14.1.1", "Demographics")
  .a7_tbl(d, "t2.rtf", "14.2.1", "Adverse events")
  sf <- tempfile("a7spec", fileext = ".csv")
  edited <- assemble_folder(d)
  edited$label <- c("EDITED ONE", "EDITED TWO")
  utils::write.csv(edited, sf, row.names = FALSE)
  # without output_file: the edited file is replaced by the folder's table
  toc <- assemble_folder(d, spec_file = sf)
  expect_identical(utils::read.csv(sf, stringsAsFactors = FALSE)$label, toc$label)
  expect_false(any(grepl("EDITED", toc$label)))
  # with output_file: the same, and the assembled file uses the folder's labels
  utils::write.csv(edited, sf, row.names = FALSE)
  out <- tempfile("a7out", fileext = ".rtf")
  res <- assemble_folder(d, output_file = out, spec_file = sf)
  expect_identical(utils::read.csv(sf, stringsAsFactors = FALSE)$label, res$spec$label)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_false(grepl("EDITED", txt, fixed = TRUE))
  # an edited one is assembled through assemble_rtf(toc = )
  utils::write.csv(edited, sf, row.names = FALSE)
  assemble_rtf(toc = sf, output_file = out, overwrite = TRUE)
  expect_match(paste(readLines(out, warn = FALSE), collapse = "\n"), "EDITED TWO", fixed = TRUE)
})

test_that("FT-AS133 [FR-AS238]: the level of a table of contents row is a whole number from 1 to 3", {
  d <- .a7_dir()
  .a7_tbl(d, "t1.rtf", "14.1.1", "Demographics")
  .a7_tbl(d, "t2.rtf", "14.2.1", "Adverse events")
  toc <- assemble_folder(d)
  out <- tempfile("a7out", fileext = ".rtf")
  toc$level <- c(1, 3)
  # (the levels are indented 600 twips each with toc_style indent = 600)
  assemble_rtf(toc = toc, output_file = out, toc_style = list(indent = 600))
  txt <- readLines(out, warn = FALSE)
  expect_true(any(grepl("{\\pard\\ri720\\keep\\fs20\\tqr", txt, fixed = TRUE)))        # level 1: no indent
  expect_true(any(grepl("{\\pard\\li1200\\ri720\\keep\\fs20\\tqr", txt, fixed = TRUE))) # level 3
  # by default both levels start flush left: no \li on either entry
  assemble_rtf(toc = toc, output_file = out, overwrite = TRUE)
  txt <- readLines(out, warn = FALSE)
  expect_identical(sum(grepl("{\\pard\\ri720\\keep\\fs20\\tqr", txt, fixed = TRUE)), 2L)
  for (bad in list(4, 0, 2.5)) {
    toc$level <- c(2, bad)
    expect_error(assemble_rtf(toc = toc, output_file = out, overwrite = TRUE),
                 "A TOC entry's `level` must be a whole number from 1 to 3.", fixed = TRUE)
  }
})

# ---- rtf_figures(), rtfplot() ----------------------------------------------------

test_that("FT-AS134 [FR-AS239]: rtf_figures() takes an EMF path as rtfplot() does", {
  emf <- test_path("fixtures", "figure.emf")
  doc <- rtf_figures(rtf_document(), list(emf, emf), align = "left")
  expect_length(doc$contents, 2L)
  fig <- doc$contents[[1L]]$content
  expect_s3_class(fig, "rtfplot")
  expect_identical(fig$img_type, "emf")
  expect_identical(fig$align, "left")
  expect_identical(fig, rtfplot(emf, align = "left"))
})

test_that("FT-AS135 [FR-AS240]: rtfplot(width_twips = , height_twips = ) are NULL or one positive number", {
  png <- .a7_png()
  for (bad in list(0, -10, NA_real_, "100", c(100, 200))) {
    expect_error(rtfplot(png, width_twips = bad),
                 "`width_twips` must be NULL or a single positive number of twips.", fixed = TRUE)
    expect_error(rtfplot(png, height_twips = bad),
                 "`height_twips` must be NULL or a single positive number of twips.", fixed = TRUE)
  }
  expect_identical(rtfplot(png, width_twips = 1500.6)$width_twips, 1500L)
})

test_that("FT-AS136 [FR-AS241]: print() of an rtfplot shows the type, sizes, dpi and alignment", {
  # no resolution in the file: the default dpi is assumed and said
  png <- .a7_png()
  fig <- rtfplot(png, align = "right")
  out <- capture.output(res <- withVisible(print(fig)))
  expect_false(res$visible)
  expect_identical(res$value, fig)
  expect_identical(out[1L], paste0("<rtfplot> PNG: ", basename(png)))
  expect_match(out[2L], "Native size:  120 x 80 px  (dpi unknown, assuming 96)", fixed = TRUE)
  expect_match(out[3L], "Display:      1.25 in (1800 twips) wide x 0.83 in (1200 twips)", fixed = TRUE)
  expect_match(out[4L], "Align:        right", fixed = TRUE)
  # 11811 dots per metre is 300 dpi
  out <- capture.output(print(rtfplot(.a7_png(11811L))))
  expect_match(out[2L], "Native size:  120 x 80 px  (300 x 300 dpi)", fixed = TRUE)
  expect_match(out[3L], "0.40 in (576 twips) wide x 0.27 in (384 twips)", fixed = TRUE)
})

test_that("FT-AS137 [FR-AS242]: assemble_folder() reads the rtf_titles() title block when the header has no \"Table N\" line", {
  d <- .a7_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  titled <- function(file, titles, header = NULL) {
    doc <- rtf_document()
    if (!is.null(header)) {
      doc <- rtf_section(doc, page = 1, secinfo = list(
        header = rtf_header(rows = list(c(c = header)))))
    }
    doc <- rtf_tables(doc, data.frame(Parameter = "Age", Value = "51.0"))
    doc <- rtf_titles(doc, list(titles))
    f <- file.path(d, file)
    generate_rtfreport(doc, f, overwrite = TRUE)
    f
  }
  # the help's example: the number and the title written with rtf_titles()
  titled("t14_2_1.rtf", c("Table 14.2.1", "Safety Population"))
  titled("t14_1_1.rtf", c("Table 14.1.1", "Safety Population"))
  # a header-based file still sorts and reads as before
  .a7_tbl(d, "a_hdr.rtf", "14.1.5", "Vital Signs")
  # the header's "Table N" wins over the title block for the number and the
  # order; the label is the file's title (#1030)
  titled("b_both.rtf", c("Table 99.9", "Body Title"), header = "Table 14.1.3")
  # no "Table N" anywhere: ordered by file name, after the numbered files,
  # and labelled by its title (#1030)
  titled("l16_1.rtf", c("Listing 16.1", "Subject Listing"))
  spec <- assemble_folder(d)
  expect_identical(basename(spec$file),
                   c("t14_1_1.rtf", "b_both.rtf", "a_hdr.rtf", "t14_2_1.rtf", "l16_1.rtf"))
  expect_identical(spec$table, c("14.1.1", "14.1.3", "14.1.5", "14.2.1", NA))
  expect_identical(spec$label,
                   c("Table 14.1.1  Safety Population", "Table 99.9  Body Title",
                     "Table 14.1.5  Vital Signs", "Table 14.2.1  Safety Population",
                     "Listing 16.1  Subject Listing"))
})
