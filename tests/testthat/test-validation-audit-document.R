# Help-to-FR audit, batch 1: the document-level exports.
#
# The help is the functional requirement: every statement in a function's Rd
# is an active FR in inst/validation/requirements.csv, and every FR has a
# test.  For each function below, one row per claim of its roxygen: the
# claim (short), the FR that states it, and the test that checks it.  The
# tests below this table close the gaps the audit found.
# (write_rtf() does not exist; the writer is generate_rtfreport().)
#
# rtf_document()
#   claim                                                   | FR          | test
#   new, empty document; rtf_config() edits one             | FR-DC101    | FT-DC056
#   font_table: first entry the default, default Courier New| FR-DC102    | FT-DC053
#   font_table name per script slot                         | FR-DC103    | FT-DC074
#   font_table needs latin; unknown slot is an error        | FR-DC104    | FT-DC257
#   font_lang: slots, languages, charsets, NULL = option    | FR-DC121    | FT-DC080
#   font_lang malformed is an error                         | FR-DC122    | FT-DC078
#   color_table: declared, default #000000, black/white     | FR-DC105    | FT-DC015
#   page: rtf_page() / named list / NULL = landscape Letter | FR-DC106    | FT-DC165
#   page: a key left out falls back to its option           | FR-IX303    | FT-DC046
#   page that is not a named list is an error               | FR-DC107    | FT-DC258
#   default_format deprecated, beneath style, warns once    | FR-DC319    | FT-DC238
#   watermark: object, string or NULL                       | FR-DC109    | FT-DC214
#   watermark: a section overrides it, NA switches it off   | FR-IX300    | FT-DC259
#   program: the path for the {PROGRAM...} tokens          | FR-DC111    | FT-DC260
#   program: generate_rtfreport(program = ) wins; order     | FR-IX301    | FT-DC182
#   program_fallback: the last resort, said in a message    | FR-DC113    | FT-DC261
#   tokens: list(STUDY = ), written {STUDY}                 | FR-DC115    | FT-DC262
#   tokens: name upper case, not rtfreporter's own; values  | FR-DC117-120| FT-DC263
#   tokens: options(rtfreporter.tokens =), document wins    | FR-IX304    | FT-DC209
#   tokens: not in column headers                           | FR-DC284    | FT-DC203
#   file: written when generate_rtfreport() has no file_path| FR-DC123    | FT-DC264
#   file malformed is an error                              | FR-DC124    | FT-DC265
#   style: one rtf_style(); NULL none                       | FR-DC317    | FT-DC231
#   style: a module's own argument wins                     | FR-DC315    | FT-DC236
#   fonts per script: one family writes the same RTF        | FR-DC103    | FT-DC077
#   a slot left out: the default's family, else latin;      | FR-DC339    | FT-DC266
#     a slotted default writes \adeff / \stshf*             |             |
#   @return: document / contents / sections                 | FR-DC101    | FT-DC056
#   example: render after rtf_tables()                      | FR-DC268    | FT-DC160
#
# rtf_config()
#   changes document settings, leaves content and sections  | FR-DC126    | FT-DC140
#   NULL changes nothing; only what is passed               | FR-DC127    | FT-DC267
#   returns a new copy, the original intact                | FR-DC125    | FT-DC143
#   page merged per key                                     | FR-DC130    | FT-DC267
#   style merged key by key                                 | FR-DC318    | FT-DC237
#   font_table / color_table replaced whole                 | FR-DC128-129| FT-DC267
#   font_lang replaced; NA removes                          | FR-DC133    | FT-DC084
#   default_format deprecated, merged per key               | FR-DC319    | FT-DC238
#   watermark: object / string / NA removes; NULL unchanged | FR-DC132    | FT-DC215
#   merge versus replace, against rtf_document()            | FR-IX305    | FT-DC267
#   not an rtf_document / page malformed is an error        | FR-DC134/135| FT-DC146, FT-DC258
#   example: identical contents                             | FR-DC126    | FT-DC140
#
# rtf_page()
#   default landscape Letter, 0.75" margins; class rtf_page | FR-DC151    | FT-DC268
#   argument > rtfreporter.page.* option > factory default  | FR-IX311    | FT-DC161
#   paper_size presets, case-insensitive                    | FR-DC152    | FT-DC129
#   orientation landscape / portrait                        | FR-DC155    | FT-DC123
#   width_in / height_in win, orientation inferred          | FR-DC157    | FT-DC127
#   with dimensions, paper_size / orientation stay unset    | FR-DC340    | FT-DC268
#   only a value passed is warned about                     | FR-IX309/310| FT-DC130, FT-DC125
#   four margins in inches                                  | FR-DC159    | FT-DC055
#   header / footer distance; NULL = the full margin        | FR-DC161    | FT-DC045
#   (code) guards: paper_size, orientation, inches          | FR-DC153/156/158/160 | FT-DC040, FT-DC124, FT-DC269
#
# rtf_titles()
#   replaces the title of every content; returns the doc    | FR-DC185    | FT-DC270
#   one block per content, or length 1 for all              | FR-DC186-187| FT-DC270
#   (code) a character vector is one block for all          | FR-DC188    | FT-DC270
#   a content is a page unless page_break = FALSE           | FR-TB182    | FT-TB164
#   block: vector or list of styled rows; "" blank; bold    | FR-DC189    | FT-DC195
#   paragraphs, or a table with title_format = "table"      | FR-DC311    | FT-DC235
#   a row's row_height_twips (table form)                   | FR-DC303    | FT-DC229
#   a page's heading stays as the last row                  | FR-DC190    | FT-DC006
#   style over rtf_style() (section, then shared)           | FR-DC192    | FT-DC271
#   font: added to font_table; per script slot              | FR-DC196    | FT-DC064
#   size and height resolve together                        | FR-DC193    | FT-DC271
#   align: block default, a row's align beats it            | FR-DC195    | FT-DC066
#   markup of the block                                     | FR-DC194    | FT-DC068
#   (code) guards                                           | FR-DC197-201| FT-DC272
#
# rtf_footnotes()
#   same shape as rtf_titles(): one block each, or one      | FR-DC203-205| FT-DC273
#   returns the modified document                           | FR-DC202    | FT-DC273
#   row: string, c(l =, c =, r =), list(columns =, ...)     | FR-DC206/305| FT-DC088, FT-DC230
#   rows left-aligned, no rule unless border                | FR-DC206/207| FT-DC088, FT-DC097
#   NULL element: no footnote for that content              | FR-DC203    | FT-DC273
#   border: NULL none; rtf_border() on the first row        | FR-DC207    | FT-DC097
#   font size, the height following it                      | FR-DC208    | FT-DC103
#   row height: an explicit one wins                        | FR-DC209    | FT-DC273
#   markup / align / font                                   | FR-DC210-212| FT-DC095, FT-DC065, FT-DC073
#   (code) guards                                           | FR-DC213-217/219 | FT-DC274, FT-DC092
#
# rtf_header() / rtf_footer()
#   rows: named vector or list; l / c / r cells             | FR-DC325    | FT-DC241
#   a row list(columns =, row_height_twips =, color =)      | FR-DC301/296| FT-DC228, FT-DC224
#   page tokens {AUTO_PAGE}, {PAGE}, ...                    | FR-DC285    | FT-DC133
#   page values {<column>}; the band per page               | FR-IX104/315| FT-IX004, FT-DC139
#   border: header none, footer top rule                    | FR-DC326    | FT-DC241
#   width_twips / width                                     | FR-DC327    | FT-DC242
#   font size; the height following it                      | FR-DC328    | FT-DC242
#   font: added to font_table; per script slot              | FR-DC341    | FT-DC275
#   markup                                                  | FR-DC329    | FT-DC243
#   row_height_twips: NULL inherits the document's          | FR-DC342    | FT-DC276
#   left / right padding: NULL inherits, else 0             | FR-DC330    | FT-DC242
#   top / bottom padding                                    | FR-DC300    | FT-DC227
#   color                                                   | FR-DC295    | FT-DC224
#   drop_empty_rows                                         | FR-DC331    | FT-DC244
#   twips arguments not integers are an error (fixed here)  | FR-DC343    | FT-DC277
#   @return: a named list of the band's settings            | FR-DC344    | FT-DC278
#
# rtf_style()
#   one object for every module; an addition, modules win   | FR-DC307/315| FT-DC236
#   shared level reaches every module; section its own      | FR-DC309/335| FT-DC232, FT-DC253
#   the order of precedence                                 | FR-DC315    | FT-DC236
#   a section key reaches only what the element left unset  | FR-DC316    | FT-DC236
#   size and height resolve together                        | FR-DC316/336| FT-DC251
#   pages are cut before the style reaches the table        | FR-DC345    | FT-DC279
#   several styles merge through rtf_config()               | FR-DC318/337| FT-DC237, FT-DC250
#   replaces rtf_default_format(), beneath any style        | FR-DC319/334| FT-DC238, FT-DC248
#   color: the page header and footer only                  | FR-DC310    | FT-DC234
#   title / footnote format and width; defaults             | FR-DC311    | FT-DC235
#   table / titles / footnotes / header / footer sections   | FR-DC312-314| FT-DC232, FT-DC233, FT-DC234
#   ...: an unknown argument is an error                    | FR-DC321    | FT-DC239
#   values checked as the module checks them                | FR-DC320/322| FT-DC239
#   @return rtf_style; NULL unset                           | FR-DC308    | FT-DC231
#
# generate_rtfreport()
#   renders an rtf_document or rtfreport; report = superseded| FR-DC269   | FT-DC003
#   file_path: NULL = rtf_document(file = ); given wins     | FR-DC271/IX302 | FT-DC264
#   overwrite: TRUE writes over; FALSE stops                | FR-DC274/275| FT-DC002, FT-DC264
#   program overrides the document's                        | FR-DC276    | FT-DC178
#   program_fallback overrides the document's               | FR-DC277    | FT-DC261
#   {PROGRAM}, {PROGRAM_FULL}, {PROGRAM_NAME}, {PROGRAM_DIR}| FR-DC276    | FT-DC178, FT-DC180
#   {DATETIME}, {DATETIME:fmt}, once per file, render_time  | FR-DC281    | FT-DC177
#   {PROGRAM} with no program known is an error             | FR-DC280    | FT-DC179
#   the search for the program; said in a message           | FR-DC278/IX301 | FT-DC184, FT-DC182
#   tokens of one's own in headers ... footnotes            | FR-DC283    | FT-DC262
#   a column header takes set_col_header() values           | FR-DC284    | FT-DC203
#   the file name completed to the one on disk              | FR-DC279    | FT-DC183
#   page tokens                                             | FR-DC285    | FT-DC133
#   (code) no file / file_path malformed is an error        | FR-DC272/273| FT-DC264, FT-DC265
#   overwrite is logical: anything else an error (fixed)    | FR-DC346    | FT-DC280
#   @return file_path, invisibly                            | FR-DC268    | FT-DC160

# A one-table document, rendered to text.
.au_rs <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}
.au_one <- function(doc = rtf_document(), header = NULL, footer = NULL) {
  doc <- rtf_tables(doc, list(rtftable(data.frame(A = c("a1", "a2")))))
  if (is.null(header) && is.null(footer)) return(doc)
  rtf_section(doc, page = 1, header = header, footer = footer)
}
.au_band <- function(s, band = "header") {
  regmatches(s, regexpr(paste0("\\{\\\\", band, " [^\n]*"), s))
}
.au_n <- function(s, pat) lengths(regmatches(s, gregexpr(pat, s, fixed = TRUE)))

test_that("FT-DC257 [FR-DC104]: rtf_document(font_table = ) needs a latin font and known slots", {
  expect_error(rtf_document(font_table = list(list(name = c(hansi = "A")))),
               "`font_table` must name a `latin` font", fixed = TRUE)
  expect_error(rtf_document(font_table = list(list(name = c(latin = "A", bad = "B")))),
               "`font_table`: unknown font slot(s)", fixed = TRUE)
})

test_that("FT-DC258 [FR-DC107 FR-DC135]: a page that is not an rtf_page() or a named list is refused", {
  msg <- paste0("`page` must be an rtf_page() object or a named list of its settings ",
                "(e.g. list(paper_size = \"A4\")), not the string \"a4\".  For a ",
                "paper size write page = rtf_page(paper_size = \"a4\").")
  expect_error(rtf_document(page = "a4"), msg, fixed = TRUE)
  expect_error(rtf_document(page = list(1, 2)), "not an unnamed list", fixed = TRUE)
  expect_error(rtf_document(page = 3), "not a numeric", fixed = TRUE)
  expect_error(rtf_config(rtf_document(), page = "a4"), msg, fixed = TRUE)
})

test_that("FT-DC259 [FR-IX300]: a section's watermark overrides the document's; NA switches it off", {
  t2 <- list(rtftable(data.frame(A = 1)), rtftable(data.frame(B = 2)))
  doc <- rtf_tables(rtf_document(watermark = "DOCWM"), t2) |>
    rtf_section(page = 1, header = rtf_header(c(l = "H")))
  # the watermark is written once per section: two sections, both the document's
  s <- .au_rs(rtf_section(doc, page = 2, header = rtf_header(c(l = "H2"))))
  expect_identical(c(.au_n(s, "\\sectd"), .au_n(s, "DOCWM")), c(2L, 2L))
  # a section that gives only a watermark is a section (fixed in this audit):
  # NA switches the document's off for it, a string replaces it
  s <- .au_rs(rtf_section(doc, page = 2, secinfo = list(watermark = NA)))
  expect_identical(c(.au_n(s, "\\sectd"), .au_n(s, "DOCWM")), c(2L, 1L))
  s <- .au_rs(rtf_section(doc, page = 2, secinfo = list(watermark = "SECWM")))
  expect_identical(c(.au_n(s, "DOCWM"), .au_n(s, "SECWM")), c(1L, 1L))
})

test_that("FT-DC260 [FR-DC111 FR-DC112]: rtf_document(program = ) names the program for the run tokens", {
  doc <- .au_one(rtf_document(program = "programs/t_dm.R"),
                 footer = rtf_footer(c(l = "N={PROGRAM_NAME} D={PROGRAM_DIR}")))
  expect_no_message(s <- .au_rs(doc))
  expect_true(grepl("N=t_dm.R D=programs", s, fixed = TRUE))
  msg <- "`program` must be a single string: the program's path."
  expect_error(rtf_document(program = 1), msg, fixed = TRUE)
  expect_error(rtf_document(program = c("a.R", "b.R")), msg, fixed = TRUE)
  expect_error(rtf_document(program = NA_character_), msg, fixed = TRUE)
})

test_that("FT-DC261 [FR-DC113 FR-DC277]: program_fallback is the last resort, said in a message", {
  old <- options(rtfreporter.program = NULL)
  on.exit(options(old), add = TRUE)
  local_mocked_bindings(.find_program = function() NULL)
  doc <- .au_one(rtf_document(program_fallback = "work/t_fb.R"),
                 footer = rtf_footer(c(l = "P={PROGRAM}")))
  expect_message(s <- .au_rs(doc), "program_fallback", fixed = TRUE)
  expect_true(grepl("P=work/t_fb.R", s, fixed = TRUE))
  # generate_rtfreport(program_fallback = ) overrides the document's
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  suppressMessages(generate_rtfreport(doc, f, program_fallback = "call.R"))
  expect_true(any(grepl("P=call.R", readLines(f, warn = FALSE), fixed = TRUE)))
})

test_that("FT-DC262 [FR-DC115 FR-DC283]: tokens of one's own are filled in headers, footers, titles and footnotes", {
  doc <- rtf_document(tokens = list(STUDY = "XYZ-9", N = 12)) |>
    rtf_tables(list(rtftable(data.frame(A = 1)))) |>
    rtf_titles("T={STUDY}") |>
    rtf_footnotes("F={STUDY} n={N}") |>
    rtf_section(page = 1, header = rtf_header(c(l = "H={STUDY} U={UNSET}")),
                footer = rtf_footer(c(l = "B={STUDY}")))
  s <- .au_rs(doc)
  for (x in c("T=XYZ-9", "F=XYZ-9 n=12", "H=XYZ-9", "B=XYZ-9")) {
    expect_true(grepl(x, s, fixed = TRUE), info = x)
  }
  # a token not set is left as written
  expect_true(grepl("UNSET", s, fixed = TRUE))
  expect_false(grepl("{STUDY}", s, fixed = TRUE))
})

test_that("FT-DC263 [FR-DC117 FR-DC118 FR-DC119 FR-DC120]: a token's name and value are checked", {
  expect_error(rtf_document(tokens = list(study = "x")),
               "`tokens`: a token's name is upper case -- a letter, then letters, digits or _ (STUDY, DATA_CUTOFF): not `study`.",
               fixed = TRUE)
  expect_error(rtf_document(tokens = list(`1A` = "x")), "not `1A`", fixed = TRUE)
  expect_error(rtf_document(tokens = list(PAGE = "x")),
               "`tokens`: {PAGE} is rtfreporter's own token; give yours another name.",
               fixed = TRUE)
  expect_error(rtf_document(tokens = list(PROGRAM_FULL = "x")), "{PROGRAM_FULL}", fixed = TRUE)
  expect_error(rtf_document(tokens = list("x")),
               "`tokens`: every token has a name, e.g. list(STUDY = \"ABC-123\").",
               fixed = TRUE)
  expect_error(rtf_document(tokens = list(STUDY = "a", STUDY = "b")),
               "`tokens`: `STUDY` is named twice.", fixed = TRUE)
  expect_error(rtf_document(tokens = list(STUDY = c("a", "b"))),
               "`tokens`: `STUDY` is one value (a string or a number).", fixed = TRUE)
  expect_error(rtf_document(tokens = list(STUDY = NA)),
               "`tokens`: `STUDY` is one value (a string or a number).", fixed = TRUE)
})

test_that("FT-DC264 [FR-DC123 FR-DC271 FR-IX302 FR-DC272 FR-DC275]: the document's file, the file_path that wins, and overwrite = FALSE", {
  f <- tempfile(fileext = ".rtf")
  g <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f, g)), add = TRUE)
  doc <- .au_one(rtf_document(file = f))
  # no file_path: the document's own, returned invisibly
  v <- withVisible(generate_rtfreport(doc))
  expect_false(v$visible)
  expect_identical(v$value, f)
  expect_true(file.exists(f))
  # a file_path given wins
  unlink(f)
  generate_rtfreport(doc, g)
  expect_true(file.exists(g))
  expect_false(file.exists(f))
  # overwrite = FALSE stops on the document's file that is there
  generate_rtfreport(doc)
  expect_error(generate_rtfreport(doc, overwrite = FALSE),
               "`file_path` already exists and `overwrite = FALSE`.", fixed = TRUE)
  # no file anywhere
  expect_error(generate_rtfreport(.au_one()),
               "generate_rtfreport(): no file to write -- give `file_path`, or the document's with `rtf_document(file = )`.",
               fixed = TRUE)
})

test_that("FT-DC265 [FR-DC124 FR-DC273]: a file that is not one string is refused", {
  msg <- "`file` must be a single string: the RTF file's path."
  for (bad in list(c("a.rtf", "b.rtf"), "", NA_character_, 1)) {
    expect_error(rtf_document(file = bad), msg, fixed = TRUE)
  }
  expect_error(generate_rtfreport(.au_one(), file_path = 1),
               "`file_path` must be a single string: the RTF file's path.", fixed = TRUE)
})

test_that("FT-DC266 [FR-DC339]: a slot an element leaves out takes the document default's family; a slotted default writes the document defaults", {
  ft <- list(list(name = c(latin = "Times New Roman", east_asian = "MS Mincho")))
  s <- .au_rs(rtf_titles(.au_one(rtf_document(font_table = ft)), "TTL", font = "Arial"))
  # Times New Roman f0, MS Mincho f1, Arial f2: the title's east_asian slot is
  # the default's (f1), its complex slot its own latin (f2)
  expect_true(grepl("\\rtlch\\af2\\ltrch\\hich\\af2\\dbch\\af1\\loch\\f2 \\b TTL", s, fixed = TRUE))
  expect_true(grepl("\\adeff0\\stshfdbch1\\stshfloch0\\stshfhich0\\stshfbi0", s, fixed = TRUE))
  # a document font with no slots writes none of them
  expect_false(grepl("stshf", .au_rs(.au_one()), fixed = TRUE))
})

test_that("FT-DC267 [FR-DC127 FR-DC128 FR-DC129 FR-DC130 FR-IX305]: rtf_config() changes what is passed, merging page and style, replacing the rest", {
  base <- rtf_document(font_table = list(list(name = "Arial"), list(name = "Symbol")),
                       color_table = c("#000000", "#FF0000"),
                       page = rtf_page(paper_size = "A4", margin_left_in = 1),
                       style = rtf_style(font_size_half_points = 20L))
  # nothing passed: nothing changes
  expect_identical(rtf_config(base)$document, base$document)
  # font_table and color_table replaced whole
  d <- rtf_config(base, font_table = list(list(name = "Courier New")),
                  color_table = c("#00FF00"))
  expect_identical(vapply(d$document$font_table, function(f) f$name, ""), "Courier New")
  expect_identical(d$document$color_table, "#00FF00")
  # page merged per key: the paper and the margins stay
  p <- rtf_config(base, page = list(width_in = 8.27, height_in = 11.69))$document$page
  expect_identical(c(p$width_in, p$height_in, p$margin_left_in), c(8.27, 11.69, 1))
  expect_identical(p$paper_size, "A4")
  expect_identical(p$orientation, "landscape")
  # style merged key by key
  st <- rtf_config(base, style = rtf_style(markup = "none"))$document$style
  expect_identical(st$font_size_half_points, 20L)
  expect_identical(st$markup, rtf_style(markup = "none")$markup)
})

test_that("FT-DC268 [FR-DC151 FR-DC340]: rtf_page() defaults; given dimensions leave paper and orientation unset", {
  p <- rtf_page()
  expect_s3_class(p, "rtf_page")
  expect_identical(p$paper_size, "letter")
  expect_identical(p$orientation, "landscape")
  expect_identical(c(p$margin_top_in, p$margin_bottom_in, p$margin_left_in, p$margin_right_in),
                   rep(0.75, 4L))
  expect_null(p$width_in); expect_null(p$height_in)
  expect_null(p$header_dist_in); expect_null(p$footer_dist_in)
  q <- rtf_page(width_in = 8.27, height_in = 11.69)
  expect_null(q$paper_size)
  expect_null(q$orientation)
  # the dimensions decide, with nothing to warn about
  expect_no_warning(s <- .au_rs(.au_one(rtf_document(page = q))))
  expect_true(grepl("\\paperw11909\\paperh16834", s, fixed = TRUE))
  # a paper_size passed with them is warned about
  expect_warning(.au_rs(.au_one(rtf_document(page = rtf_page(width_in = 8.27, height_in = 11.69,
                                                             paper_size = "letter")))),
                 "Both `paper_size` and `width_in`/`height_in` were supplied", fixed = TRUE)
})

test_that("FT-DC269 [FR-DC158 FR-DC160]: page sizes and margins in inches are single positive numbers", {
  expect_error(rtf_page(width_in = "wide"),
               "`width_in` must be a single positive number of inches, or NULL.", fixed = TRUE)
  expect_error(rtf_page(height_in = 0), "`height_in` must be a single positive", fixed = TRUE)
  expect_error(rtf_page(margin_top_in = -1),
               "`margin_top_in` must be a single positive number of inches, or NULL.", fixed = TRUE)
  expect_error(rtf_page(margin_right_in = NA), "`margin_right_in` must be", fixed = TRUE)
  expect_error(rtf_page(header_dist_in = c(1, 2)), "`header_dist_in` must be", fixed = TRUE)
  expect_error(rtf_page(footer_dist_in = 0), "`footer_dist_in` must be", fixed = TRUE)
})

test_that("FT-DC270 [FR-DC185 FR-DC186 FR-DC187 FR-DC188]: rtf_titles() sets one block per content, or one for all", {
  d <- rtf_tables(rtf_document(), list(data.frame(A = 1), data.frame(B = 2)))
  d0 <- d
  titles_of <- function(x) lapply(x$contents, function(b) b$title)
  d2 <- rtf_titles(d, list("T1", NULL))
  expect_identical(titles_of(d2), list("T1", NULL))
  expect_identical(d, d0)  # the original is unchanged
  expect_identical(titles_of(rtf_titles(d, list("C"))), list("C", "C"))
  expect_identical(titles_of(rtf_titles(d, c("L1", "L2"))), list(c("L1", "L2"), c("L1", "L2")))
  s <- .au_rs(d2)
  expect_identical(.au_n(s, "T1"), 1L)
})

test_that("FT-DC271 [FR-DC192 FR-DC193]: a title's size and height resolve together, over the document's style", {
  doc <- function(...) rtf_document(style = rtf_style(title_format = "table", ...))
  trrh <- function(s) regmatches(s, regexpr("\\\\trrh[0-9]+(?=[^\n]*TTL)", s, perl = TRUE))
  # the size, over the style's section and shared level
  s <- .au_rs(rtf_titles(.au_one(doc(titles = list(font_size_half_points = 16L))),
                         "TTL", font_size_half_points = 24L))
  expect_true(grepl("\\fs24", s, fixed = TRUE))
  # a size alone takes the height of that size, not the style's height
  own <- trrh(.au_rs(rtf_titles(.au_one(doc()), "TTL", font_size_half_points = 24L)))
  styled <- trrh(.au_rs(rtf_titles(.au_one(doc(row_height_twips = 500L)), "TTL",
                                   font_size_half_points = 24L)))
  expect_identical(styled, own)
  expect_false(identical(styled, "\\trrh500"))
  # an explicit height always wins
  s <- .au_rs(rtf_titles(.au_one(doc(row_height_twips = 500L)), "TTL",
                         font_size_half_points = 24L, row_height_twips = 600L))
  expect_identical(trrh(s), "\\trrh600")
})

test_that("FT-DC272 [FR-DC197 FR-DC198 FR-DC199 FR-DC200 FR-DC201]: rtf_titles() checks its document, its blocks and its style", {
  expect_error(rtf_titles(list(), list("T")), "`doc` must be an rtf_document object", fixed = TRUE)
  expect_error(rtf_titles(rtf_document(), list("T")),
               "Cannot set titles before any content has been added.  Call rtf_titles() after rtf_tables() / rtf_figures()",
               fixed = TRUE)
  d <- rtf_tables(rtf_document(), list(data.frame(A = 1), data.frame(B = 2)))
  expect_error(rtf_titles(d, 1),
               "`titles` must be a list (one block per page) or a character vector (one block for every page).",
               fixed = TRUE)
  expect_error(rtf_titles(d, list("a", "b", "c")),
               "`titles` has 3 blocks but the document has 2 pages: give one block per page, or one block (a list of length 1) for every page.",
               fixed = TRUE)
  expect_error(rtf_titles(d, "T", font_size_half_points = -1),
               "`rtf_titles(font_size_half_points)` must be a single positive integer (half-points).",
               fixed = TRUE)
  expect_error(rtf_titles(d, "T", row_height_twips = -1),
               "`rtf_titles(row_height_twips)` must be a single non-negative integer (twips) or NULL.",
               fixed = TRUE)
  expect_error(rtf_titles(d, "T", align = "middle"),
               "`rtf_titles(align)` must be \"left\", \"center\" or \"right\".", fixed = TRUE)
  expect_error(rtf_titles(d, "T", font = 1), "`rtf_titles(font)` must be", fixed = TRUE)
})

test_that("FT-DC273 [FR-DC202 FR-DC203 FR-DC204 FR-DC205 FR-DC209]: rtf_footnotes() sets one block per content, or one for all; NULL suppresses one", {
  d <- rtf_tables(rtf_document(), list(data.frame(A = 1), data.frame(B = 2)))
  d0 <- d
  fn_of <- function(x) lapply(x$contents, function(b) b$footnote)
  d2 <- rtf_footnotes(d, list("FNONE", NULL))
  expect_identical(fn_of(d2), list("FNONE", NULL))
  expect_identical(d, d0)
  expect_identical(.au_n(.au_rs(d2), "FNONE"), 1L)
  expect_identical(fn_of(rtf_footnotes(d, list("C"))), list("C", "C"))
  expect_identical(fn_of(rtf_footnotes(d, "Source: ADSL")), list("Source: ADSL", "Source: ADSL"))
  # an explicit row height wins over the one the size implies
  s <- .au_rs(rtf_footnotes(.au_one(), "FNX", font_size_half_points = 24L,
                            row_height_twips = 600L))
  expect_true(grepl("\\\\trrh600[^\n]*FNX", s))
})

test_that("FT-DC274 [FR-DC213 FR-DC214 FR-DC215 FR-DC216 FR-DC219]: rtf_footnotes() checks its document, its blocks and its style", {
  expect_error(rtf_footnotes(list(), list("F")), "`doc` must be an rtf_document object", fixed = TRUE)
  expect_error(rtf_footnotes(rtf_document(), list("F")),
               "Cannot set footnotes before any content has been added.  Call rtf_footnotes() after rtf_tables() / rtf_figures()",
               fixed = TRUE)
  d <- rtf_tables(rtf_document(), list(data.frame(A = 1), data.frame(B = 2)))
  expect_error(rtf_footnotes(d, 1),
               "`footnotes` must be a list (one block per page) or a character vector (one block for every page).",
               fixed = TRUE)
  expect_error(rtf_footnotes(d, list("a", "b", "c")),
               "`footnotes` has 3 blocks but the document has 2 pages: give one block per page, or one block (a list of length 1) for every page.",
               fixed = TRUE)
  expect_error(rtf_footnotes(d, "F", align = "middle"),
               "`rtf_footnotes(align)` must be \"left\", \"center\" or \"right\".", fixed = TRUE)
  expect_error(rtf_footnotes(d, "F", font_size_half_points = 0),
               "`rtf_footnotes(font_size_half_points)` must be a single positive integer (half-points).",
               fixed = TRUE)
  expect_error(rtf_footnotes(d, "F", row_height_twips = "x"),
               "`rtf_footnotes(row_height_twips)` must be a single non-negative integer (twips) or NULL.",
               fixed = TRUE)
})

test_that("FT-DC275 [FR-DC341]: a band's font is added to the font table; a named vector gives one per slot", {
  s <- .au_rs(.au_one(header = rtf_header(c(l = "HDR"), font = "Arial")))
  expect_true(grepl("{\\f1\\fnil\\fcharset0 Arial;}", s, fixed = TRUE))
  expect_true(grepl("\\f1", .au_band(s), fixed = TRUE))
  s <- .au_rs(.au_one(footer = rtf_footer(c(l = "FTR"),
                                          font = c(latin = "Arial", east_asian = "MS Gothic"))))
  expect_true(grepl("Arial;}", s, fixed = TRUE))
  expect_true(grepl("MS Gothic;}", s, fixed = TRUE))
  # NULL: the document font, nothing added
  s <- .au_rs(.au_one(header = rtf_header(c(l = "HDR"))))
  expect_false(grepl("\\f1", s, fixed = TRUE))
})

test_that("FT-DC276 [FR-DC342]: a band's row height inherits the document's, else follows its font size", {
  hh <- function(doc) regmatches(.au_band(.au_rs(doc)), regexpr("\\\\trrh[0-9]+", .au_band(.au_rs(doc))))
  h <- rtf_header(c(l = "H"))
  base <- hh(.au_one(header = h))
  expect_identical(hh(.au_one(rtf_document(style = rtf_style(row_height_twips = 400L)), header = h)),
                   "\\trrh400")
  expect_identical(hh(.au_one(rtf_document(style = rtf_style(header = list(row_height_twips = 450L))),
                              header = h)), "\\trrh450")
  old <- options(rtfreporter.row_height_twips = 350L)
  on.exit(options(old), add = TRUE)
  expect_identical(hh(.au_one(header = h)), "\\trrh350")
  options(old)
  # the band's own wins; a size alone recomputes the height
  expect_identical(hh(.au_one(header = rtf_header(c(l = "H"), row_height_twips = 500L))), "\\trrh500")
  expect_false(identical(hh(.au_one(header = rtf_header(c(l = "H"), font_size_half_points = 28L))),
                         base))
})

test_that("FT-DC277 [FR-DC343]: a band's twips arguments are single non-negative integers", {
  # these used to be written into the file as NA (fixed in this audit)
  for (fn in list(rtf_header, rtf_footer)) {
    expect_error(fn(c(l = "H"), row_height_twips = "a"),
                 "`row_height_twips` must be a single non-negative integer (twips) or NULL.",
                 fixed = TRUE)
    expect_error(fn(c(l = "H"), width_twips = "x"),
                 "`width_twips` must be a single non-negative integer (twips) or NULL.",
                 fixed = TRUE)
    expect_error(fn(c(l = "H"), cell_padding_left_twips = -5),
                 "`cell_padding_left_twips` must be a single non-negative integer (twips) or NULL.",
                 fixed = TRUE)
    expect_error(fn(c(l = "H"), cell_padding_right_twips = c(1, 2)),
                 "`cell_padding_right_twips` must be", fixed = TRUE)
  }
  s <- .au_band(.au_rs(.au_one(header = rtf_header(c(l = "H"), width_twips = 9000,
                                                   cell_padding_left_twips = 50))))
  expect_true(grepl("\\cellx9000\\ql\\li50", s, fixed = TRUE))
})

test_that("FT-DC278 [FR-DC344]: rtf_header() / rtf_footer() return a named list of the band's settings for rtf_section()", {
  h <- rtf_header(c(l = "A", r = "B"), row_height_twips = 300L)
  expect_type(h, "list")
  expect_true(all(c("rows", "border", "width_twips", "width", "row_height_twips") %in% names(h)))
  expect_identical(h$rows, list(c(l = "A", r = "B")))
  expect_identical(h$row_height_twips, 300L)
  f <- rtf_footer(list(c(l = "F1"), c(l = "F2")))
  expect_length(f$rows, 2L)
  expect_s3_class(f$border, "rtf_border")
  expect_s3_class(rtf_section(.au_one(), page = 1, header = h, footer = f), "rtf_document")
})

test_that("FT-DC279 [FR-DC345]: a table is cut into pages before the style's row height reaches it", {
  big <- data.frame(A = sprintf("row %03d", 1:90))
  pages <- as_rtftables(big, split = "group_force", max_rows = 30)
  plain <- rtf_tables(rtf_document(), pages)
  styled <- rtf_tables(rtf_document(style = rtf_style(table = list(row_height_twips = 1500L))),
                       pages)
  expect_identical(length(rtf_layout(styled)$pages), length(rtf_layout(plain)$pages))
  expect_identical(length(rtf_layout(plain)$pages), 3L)
  # the style reaches the table rows all the same
  expect_true(grepl("\\trrh1500", .au_rs(styled), fixed = TRUE))
})

test_that("FT-DC280 [FR-DC346]: generate_rtfreport(overwrite = ) is TRUE or FALSE", {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  # these used to be read as FALSE without a word (fixed in this audit)
  for (bad in list("yes", NA, c(TRUE, FALSE), 1)) {
    expect_error(generate_rtfreport(.au_one(), f, overwrite = bad),
                 "`overwrite` must be TRUE or FALSE.", fixed = TRUE)
  }
  expect_false(file.exists(f))
})
