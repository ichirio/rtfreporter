# test-assemble-toc-title.R -- the table of contents of assemble_rtf(): its
# labels, headings, title (line breaks, tokens, alignment), text styles and
# layout (page numbers, wrapped labels, several pages, a cover).  The small
# inputs and the fixed cases are in helper-assemble-toc.R.

# -- labels --------------------------------------------------------------------

test_that("FT-AS4001 [FR-AS112]: toc = 'auto' makes one level-1 entry per file, labelled by its title", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 Demographics",
                       t_14_2_1 = "Table 14.2.1 Adverse Events"))
  x <- .tt_asm(f, toc = "auto")
  expect_identical(.tt_labels(x), c("Table 14.1.1 Demographics",
                                    "Table 14.2.1 Adverse Events"))
  e <- .tt_entries(x)
  expect_length(e, 2L)
  expect_false(any(grepl("\\\\li[0-9]", e)))
  expect_match(e[1L], "HYPERLINK \\\\\\\\l \"tfl_t_14_1_1\"")
  expect_match(e[2L], "PAGEREF tfl_t_14_2_1 ")
})

test_that("FT-AS4002 [FR-AS113]: toc as a character vector labels the files in order", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 Demographics",
                       t_14_2_1 = "Table 14.2.1 Adverse Events"))
  x <- .tt_asm(f, toc = c("First label", "Second label"))
  expect_identical(.tt_labels(x), c("First label", "Second label"))
})

test_that("FT-AS4003 [FR-AS115]: a table with a label for every row uses those labels, not the titles", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 Demographics",
                       t_14_2_1 = "Table 14.2.1 Adverse Events"))
  x <- .tt_asm(f, toc = data.frame(file = f, label = c("Demog", "AEs")))
  expect_identical(.tt_labels(x), c("Demog", "AEs"))
  expect_false(any(grepl("Table 14\\.", .tt_entries(x))))
})

test_that("FT-AS4004 [FR-AS258 FR-AS115]: a table with no label column takes every label from its file's title, as 'auto' does", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 Demographics",
                       t_14_2_1 = "Table 14.2.1 Adverse Events"))
  x <- .tt_asm(f, toc = data.frame(file = f))
  auto <- .tt_asm(f, toc = "auto")
  expect_identical(.tt_labels(x), c("Table 14.1.1 Demographics",
                                    "Table 14.2.1 Adverse Events"))
  expect_identical(.tt_labels(x), .tt_labels(auto))
  # (a table's entries are level 2: indented, as with a label column)
  expect_true(all(grepl("^\\{\\\\pard\\\\li600", .tt_entries(x))))
  expect_identical(.tt_labels(.tt_asm(f, toc = data.frame(file = f, label = c("a", "b")))),
                   c("a", "b"))
})

test_that("FT-AS4005 [FR-AS258]: a table with some NA labels keeps the explicit ones and reads the others from the titles", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 Demographics",
                       t_14_2_1 = "Table 14.2.1 Adverse Events",
                       t_14_3_1 = "Table 14.3.1 Laboratory"))
  x <- .tt_asm(f, toc = data.frame(file = f, label = c(NA, "Safety AEs", NA)))
  expect_identical(.tt_labels(x), c("Table 14.1.1 Demographics", "Safety AEs",
                                    "Table 14.3.1 Laboratory"))
  # the outline paragraphs (PDF bookmarks) carry the same labels
  expect_true(any(grepl("outlinelevel0 Safety AEs\\\\par", x)))
  expect_true(any(grepl("outlinelevel0 Table 14\\.1\\.1 Demographics\\\\par", x)))
})

test_that("FT-AS4006 [FR-AS258 FR-AS112]: a file whose title cannot be read is labelled by its file name, for 'auto' and for a missing label alike", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 Demographics"))
  f <- c(f, .tt_input(d, "untitled_listing.rtf", title = NULL))
  auto <- .tt_asm(f, toc = "auto")
  tab  <- .tt_asm(f, toc = data.frame(file = f, label = NA_character_))
  expect_identical(.tt_labels(auto)[2L], "untitled_listing")
  expect_identical(.tt_labels(tab), .tt_labels(auto))
})

test_that("FT-AS4007 [FR-AS258 FR-AS117]: a .csv table without a label column, or with an empty label cell (NA), reads the labels from the titles", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 Demographics",
                       t_14_2_1 = "Table 14.2.1 Adverse Events"))
  csv1 <- file.path(d, "toc1.csv")
  utils::write.csv(data.frame(file = f), csv1, row.names = FALSE)
  expect_identical(.tt_labels(.tt_asm(f, toc = csv1)),
                   c("Table 14.1.1 Demographics", "Table 14.2.1 Adverse Events"))
  csv2 <- file.path(d, "toc2.csv")
  utils::write.csv(data.frame(file = f, label = c("Own", NA)), csv2,
                   row.names = FALSE)
  expect_identical(.tt_labels(.tt_asm(f, toc = csv2)),
                   c("Own", "Table 14.2.1 Adverse Events"))
})

test_that("FT-AS4008 [FR-AS118]: a table without the column file stops, one without label does not", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  out <- file.path(d, "assembled.rtf")
  expect_error(assemble_rtf(f, out, toc = data.frame(label = c("a", "b"))),
               "^A table of contents needs the column `file`")
  expect_no_error(assemble_rtf(f, out, toc = data.frame(file = f)))
})

# -- headings ------------------------------------------------------------------

test_that("FT-AS4009 [FR-AS267 FR-AS116]: with no heading column the TOC has no heading and its entries are level 2", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 A", t_14_2_1 = "Table 14.2.1 B"))
  x <- .tt_asm(f, toc = data.frame(file = f))
  expect_length(.tt_headings(x), 0L)
  expect_length(.tt_paras(x), 2L + 2L)   # the title, the blank line, two entries
})

test_that("FT-AS4010 [FR-AS267]: one heading amid NAs is printed once, before its row, and the NA rows print nothing", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 A", t_14_2_1 = "Table 14.2.1 B",
                       t_14_3_1 = "Table 14.3.1 C"))
  n <- 100L
  toc <- data.frame(file = f[rep_len(1:3, n)],
                    label = sprintf("Entry %03d", seq_len(n)),
                    heading = c(rep(NA, 70), "Pharmacokinetics", rep(NA, 29)))
  x <- .tt_asm(f, toc = toc)
  expect_identical(.tt_headings(x), "Pharmacokinetics")
  expect_length(.tt_entries(x), n)
  p <- .tt_paras(x)
  h <- grep("Pharmacokinetics", p)
  # 70 entries come before it (after the title and the blank line)
  expect_identical(h, 2L + 70L + 1L)
  expect_match(p[h + 1L], "Entry 071")
  expect_match(p[h - 1L], "Entry 070")
})

test_that("FT-AS4011 [FR-AS267]: NA and empty headings between two headings print nothing", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C",
                       t_14_4_1 = "D", t_14_5_1 = "E"))
  toc <- data.frame(file = f, label = LETTERS[1:5],
                    heading = c("Part 1", NA, "", "Part 2", NA))
  x <- .tt_asm(f, toc = toc)
  expect_identical(.tt_headings(x), c("Part 1", "Part 2"))
  expect_identical(.tt_labels(x), LETTERS[1:5])
})

test_that("FT-AS4012 [FR-AS267]: the same heading on consecutive rows is printed once, and again when it comes back", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C",
                       t_14_4_1 = "D"))
  x <- .tt_asm(f, toc = data.frame(file = f, label = LETTERS[1:4],
                                   heading = c("Same", "Same", "Same", "Same")))
  expect_identical(.tt_headings(x), "Same")
  y <- .tt_asm(f, toc = data.frame(file = f, label = LETTERS[1:4],
                                   heading = c("X", "X", "Y", "X")))
  expect_identical(.tt_headings(y), c("X", "Y", "X"))
})

test_that("FT-AS4013 [FR-AS267 FR-AS116]: two headings split the entries; a heading is bold, flush left, with no page number", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C"))
  x <- .tt_asm(f, toc = data.frame(file = f, label = c("a", "b", "c"),
                                   heading = c("Efficacy", "Safety", "Safety")))
  p <- .tt_paras(x)
  expect_identical(p[3L], "{\\pard\\b\\fs22 Efficacy\\par}")
  expect_match(p[4L], "^\\{\\\\pard\\\\li600\\\\ri720\\\\keep\\\\fs20")
  expect_identical(p[5L], "{\\pard\\b\\fs22 Safety\\par}")
  expect_false(any(grepl("PAGEREF|tqr", p[c(3L, 5L)])))
})

# -- levels --------------------------------------------------------------------

test_that("FT-AS4014 [FR-AS116 FR-AS238]: entries at level 1, 2 and 3 are indented 0, 600 and 1200 twips; headings and the title are not", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C"))
  x <- .tt_asm(f, toc = data.frame(file = f, label = c("a", "b", "c"),
                                   heading = c("H", NA, NA), level = 1:3))
  e <- .tt_entries(x)
  expect_match(e[1L], "^\\{\\\\pard\\\\ri720\\\\keep\\\\fs20\\\\tqr")
  expect_match(e[2L], "^\\{\\\\pard\\\\li600\\\\ri720\\\\keep\\\\fs20\\\\tqr")
  expect_match(e[3L], "^\\{\\\\pard\\\\li1200\\\\ri720\\\\keep\\\\fs20\\\\tqr")
  p <- .tt_paras(x)
  expect_identical(p[3L], "{\\pard\\b\\fs22 H\\par}")
  expect_identical(p[1L], "{\\pard\\qc\\b\\fs28 Table of Contents\\par}")
})

# -- the title -----------------------------------------------------------------

test_that("FT-AS4015 [FR-AS127]: the default title is one centred bold 14 pt paragraph, 'Table of Contents', with no line break", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  p <- .tt_paras(.tt_asm(f, toc = "auto"))
  expect_identical(p[1L], "{\\pard\\qc\\b\\fs28 Table of Contents\\par}")
  expect_identical(p[2L], "{\\pard\\fs18\\par}")
})

test_that("FT-AS4016 [FR-AS254]: a newline in toc_title is a line break (\\line) inside the one title paragraph", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  p <- .tt_paras(.tt_asm(f, toc = "auto", toc_title = "Table of Contents\nStudy XYZ-101"))
  expect_identical(p[1L],
                   "{\\pard\\qc\\b\\fs28 Table of Contents\\line Study XYZ-101\\par}")
  expect_identical(p[2L], "{\\pard\\fs18\\par}")
})

test_that("FT-AS4017 [FR-AS254]: two newlines are two line breaks, a trailing newline one before the paragraph ends, a lone newline one", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  title <- function(t) .tt_paras(.tt_asm(f, toc = "auto", toc_title = t))[1L]
  expect_identical(title("Contents\n\nXYZ"),
                   "{\\pard\\qc\\b\\fs28 Contents\\line \\line XYZ\\par}")
  expect_identical(title("Contents\n"), "{\\pard\\qc\\b\\fs28 Contents\\line \\par}")
  expect_identical(title("\n"), "{\\pard\\qc\\b\\fs28 \\line \\par}")
  expect_identical(title(""), "{\\pard\\qc\\b\\fs28 \\par}")
  # (still ONE paragraph: the title's \par is the only one before the blank line)
  expect_identical(.tt_paras(.tt_asm(f, toc = "auto", toc_title = "a\nb\nc"))[2L],
                   "{\\pard\\fs18\\par}")
})

test_that("FT-AS4018 [FR-AS255]: tokens of one's own (the session option) are filled in toc_title, as in a title", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  old <- options(rtfreporter.tokens = list(STUDY_ID = "XYZ-101",
                                           ANALYSIS_TYPE = "Final Analysis",
                                           ODD = "a{b}c"))
  on.exit(options(old), add = TRUE)
  p <- .tt_paras(.tt_asm(f, toc = "auto",
                         toc_title = "Table of Contents\n\n{STUDY_ID} ({ANALYSIS_TYPE})"))
  expect_identical(p[1L], paste0("{\\pard\\qc\\b\\fs28 Table of Contents\\line \\line ",
                                 "XYZ-101 (Final Analysis)\\par}"))
  # a value is written as it is: its braces are escaped, not read as markup
  p2 <- .tt_paras(.tt_asm(f, toc = "auto", toc_title = "{ODD}"))
  expect_identical(p2[1L], "{\\pard\\qc\\b\\fs28 a\\{b\\}c\\par}")
})

test_that("FT-AS4019 [FR-AS255]: a document's own token is not known at assembly (only the session's are); a title read from a file has its own filled already", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- c(.tt_input(d, "t_14_1_1.rtf", "Table 14.1.1 {STUDY} Demographics",
                   tokens = list(STUDY = "DOC-7")),
         .tt_input(d, "t_14_2_1.rtf", "Table 14.2.1 B"))
  x <- .tt_asm(f, toc = "auto", toc_title = "Contents {STUDY}")
  expect_identical(.tt_labels(x)[1L], "Table 14.1.1 DOC-7 Demographics")
  expect_identical(.tt_paras(x)[1L], "{\\pard\\qc\\b\\fs28 Contents \\{STUDY\\}\\par}")
  old <- options(rtfreporter.tokens = list(STUDY = "SESSION"))
  on.exit(options(old), add = TRUE)
  expect_identical(.tt_paras(.tt_asm(f, toc = "auto", toc_title = "Contents {STUDY}"))[1L],
                   "{\\pard\\qc\\b\\fs28 Contents SESSION\\par}")
})

test_that("FT-AS4020 [FR-AS255]: an unknown token in toc_title stays as text, with no warning and no error, as in a title", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  x <- expect_no_warning(.tt_asm(f, toc = "auto", toc_title = "Contents {NO_SUCH_TOKEN}"))
  expect_identical(.tt_paras(x)[1L],
                   "{\\pard\\qc\\b\\fs28 Contents \\{NO_SUCH_TOKEN\\}\\par}")
})

test_that("FT-AS4021 [FR-AS255]: {DATETIME} and a viewer's page token are filled in toc_title as in a title", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  old <- options(rtfreporter.render_time = as.POSIXct("2026-11-12 09:30:00", tz = "UTC"))
  on.exit(options(old), add = TRUE)
  p <- .tt_paras(.tt_asm(f, toc = "auto",
                         toc_title = "Contents {DATETIME:%Y-%m-%d} {AUTO_PAGE}"))
  expect_match(p[1L], "Contents 2026-11-12 \\\\chpgn \\\\par\\}$")
})

test_that("FT-AS4022 [FR-AS256]: toc_title named l, r or c (or unnamed) is aligned left, right or centre", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  title <- function(t) .tt_paras(.tt_asm(f, toc = "auto", toc_title = t))[1L]
  expect_identical(title(c(l = "Contents")), "{\\pard\\ql\\b\\fs28 Contents\\par}")
  expect_identical(title(c(r = "Contents")), "{\\pard\\qr\\b\\fs28 Contents\\par}")
  expect_identical(title(c(c = "Contents")), "{\\pard\\qc\\b\\fs28 Contents\\par}")
  expect_identical(title("Contents"),         "{\\pard\\qc\\b\\fs28 Contents\\par}")
  expect_identical(title(c(l = "Contents\nXYZ")),
                   "{\\pard\\ql\\b\\fs28 Contents\\line XYZ\\par}")
})

test_that("FT-AS4023 [FR-AS256]: toc_title must be one string with at most one name, l, c or r", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  out <- file.path(d, "assembled.rtf")
  expect_error(assemble_rtf(f, out, toc = "auto", toc_title = c(x = "Contents")),
               "may only be named l, c or r")
  expect_error(assemble_rtf(f, out, toc = "auto", toc_title = c(l = "A", r = "B")),
               "^`toc_title` must be a single string")
  expect_error(assemble_rtf(f, out, toc = "auto", toc_title = c("A", "B")),
               "^`toc_title` must be a single string")
  expect_error(assemble_rtf(f, out, toc = "auto", toc_title = NA_character_),
               "^`toc_title` must be a single string")
  expect_error(assemble_rtf(f, out, toc = "auto", toc_title = 1),
               "^`toc_title` must be a single string")
})

test_that("FT-AS4024 [FR-AS257]: tokens are filled in a heading (the table's heading column) as in a title; an unknown one stays as text", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C"))
  old <- options(rtfreporter.tokens = list(STUDY_ID = "XYZ-101"))
  on.exit(options(old), add = TRUE)
  x <- .tt_asm(f, toc = data.frame(file = f,
                                   label = c("{STUDY_ID} entry", "b", "c"),
                                   heading = c("Efficacy {STUDY_ID}", NA, "Safety {NOPE}")))
  expect_identical(.tt_headings(x), c("Efficacy XYZ-101", "Safety \\{NOPE\\}"))
  # an entry's label is the text as given, not a title: its token is not filled
  expect_identical(.tt_labels(x)[1L], "\\{STUDY_ID\\} entry")
})

# -- the title's lines in the height estimate ---------------------------------

test_that("FT-AS4025 [FR-AS259]: the TOC height estimate counts the title's line breaks, so a taller title can push entries to a second TOC page", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  n <- 35L   # fills a one-line-title page exactly: 35 entries at 10 pt
  toc <- data.frame(file = f[rep_len(1:2, n)], label = sprintf("Entry %02d", seq_len(n)))
  e <- lapply(seq_len(n), function(i) list(type = "entry", label = "x", level = 2L))
  expect_identical(max(rtfreporter:::.toc_entry_pages(e, 1L)), 1L)
  expect_identical(max(rtfreporter:::.toc_entry_pages(e, 2L)), 2L)
  one <- .tt_asm(f, toc = toc)
  two <- .tt_asm(f, toc = toc, toc_title = "Contents\n")
  expect_length(grep("\\\\pagebb", one), 0L)
  expect_length(grep("\\\\pagebb", two), 1L)
  # the body starts one page later, and the cached numbers say so
  expect_identical(.tt_pages(two) - .tt_pages(one), rep(1L, n))
  # (the line count is that of the filled title: a token's own newline counts)
  old <- options(rtfreporter.tokens = list(TWO = "x\ny"))
  on.exit(options(old), add = TRUE)
  three <- .tt_asm(f, toc = toc, toc_title = "Contents {TWO}")
  expect_length(grep("\\\\pagebb", three), 1L)
  expect_identical(rtfreporter:::.toc_title_lines("a\\line b\\\\line c\\line "), 3L)
})

# -- toc_style -----------------------------------------------------------------

test_that("FT-AS4026 [FR-AS260]: toc_style sets the size and boldness of the title, a heading, an entry and the blank line", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  st <- list(title = list(size = 16, bold = FALSE),
             heading = list(size = 12, bold = FALSE),
             entry = list(size = 9, bold = TRUE),
             gap = list(size = 6))
  x <- .tt_asm(f, toc = data.frame(file = f, label = c("a", "b"),
                                   heading = c("H", NA)), toc_style = st)
  p <- .tt_paras(x)
  expect_identical(p[1L], "{\\pard\\qc\\fs32 Table of Contents\\par}")
  expect_identical(p[2L], "{\\pard\\fs12\\par}")
  expect_identical(p[3L], "{\\pard\\fs24 H\\par}")
  expect_match(p[4L], "^\\{\\\\pard\\\\li600\\\\ri648\\\\keep\\\\b\\\\fs18\\\\tqr\\\\tldot\\\\tx14112 ")
  # half points
  y <- .tt_asm(f, toc = "auto", toc_style = list(entry = list(size = 10.5)))
  expect_match(.tt_entries(y)[1L], "^\\{\\\\pard\\\\ri756\\\\keep\\\\fs21\\\\tqr")
})

test_that("FT-AS4027 [FR-AS260]: toc_style is read from options(rtfreporter.toc_style) when not given, and the argument wins", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  old <- options(rtfreporter.toc_style = list(title = list(size = 20)))
  on.exit(options(old), add = TRUE)
  expect_identical(.tt_paras(.tt_asm(f, toc = "auto"))[1L],
                   "{\\pard\\qc\\b\\fs40 Table of Contents\\par}")
  expect_identical(.tt_paras(.tt_asm(f, toc = "auto",
                                     toc_style = list(title = list(size = 8))))[1L],
                   "{\\pard\\qc\\b\\fs16 Table of Contents\\par}")
  # (and rtfreporter_options() lists it, unset)
  expect_true("rtfreporter.toc_style" %in% names(rtfreporter_options()))
})

test_that("FT-AS4028 [FR-AS260]: a partial toc_style is merged over the built-in values key by key", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  tab <- data.frame(file = f, label = c("a", "b"), heading = c("H", NA))
  p <- .tt_paras(.tt_asm(f, toc = tab, toc_style = list(entry = list(bold = TRUE))))
  expect_identical(p[1L], "{\\pard\\qc\\b\\fs28 Table of Contents\\par}")
  expect_identical(p[2L], "{\\pard\\fs18\\par}")
  expect_identical(p[3L], "{\\pard\\b\\fs22 H\\par}")
  expect_match(p[4L], "^\\{\\\\pard\\\\li600\\\\ri720\\\\keep\\\\b\\\\fs20\\\\tqr")   # bold added, size kept
  p2 <- .tt_paras(.tt_asm(f, toc = tab, toc_style = list(heading = list(size = 13))))
  expect_identical(p2[3L], "{\\pard\\b\\fs26 H\\par}")               # size changed, bold kept
  expect_match(p2[4L], "^\\{\\\\pard\\\\li600\\\\ri720\\\\keep\\\\fs20\\\\tqr")
  expect_identical(.tt_paras(.tt_asm(f, toc = tab, toc_style = list()))[1:3],
                   .tt_paras(.tt_asm(f, toc = tab, toc_style = NULL))[1:3])
})

test_that("FT-AS4029 [FR-AS260 FR-AS259]: the height estimate uses the sizes of toc_style", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  n <- 30L
  toc <- data.frame(file = f[rep_len(1:2, n)], label = sprintf("Entry %02d", seq_len(n)))
  small <- .tt_asm(f, toc = toc)
  big   <- .tt_asm(f, toc = toc,
                   toc_style = list(entry = list(size = 20)))
  expect_length(grep("\\\\pagebb", small), 0L)
  expect_gt(length(grep("\\\\pagebb", big)), 0L)
  expect_gt(.tt_pages(big)[1L], .tt_pages(small)[1L])
})

test_that("FT-AS4030 [FR-AS261]: a toc_style with an unknown key, a bad size or a bad bold stops with a message naming it", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  out <- file.path(d, "assembled.rtf")
  bad <- function(st) assemble_rtf(f, out, toc = "auto", toc_style = st)
  expect_error(bad(list(foot = list(size = 8))),
               "`toc_style` has unknown key\\(s\\) 'foot'; the keys are title, heading, entry, gap")
  expect_error(bad(list(title = list(colour = "red"))),
               "`toc_style\\$title` has unknown key\\(s\\) 'colour'")
  expect_error(bad(list(gap = list(bold = TRUE))),
               "`toc_style\\$gap` has unknown key\\(s\\) 'bold'; it takes size")
  expect_error(bad(list(title = list(size = 0))), "`toc_style\\$title\\$size` must be a positive number")
  expect_error(bad(list(title = list(size = -3))), "`toc_style\\$title\\$size` must be a positive number")
  expect_error(bad(list(entry = list(size = "big"))), "`toc_style\\$entry\\$size` must be a positive number")
  expect_error(bad(list(entry = list(size = NA_real_))), "`toc_style\\$entry\\$size` must be a positive number")
  expect_error(bad(list(entry = list(size = c(9, 10)))), "`toc_style\\$entry\\$size` must be a positive number")
  expect_error(bad(list(heading = list(size = 10.3))), "`toc_style\\$heading\\$size` must be a positive number")
  expect_error(bad(list(heading = list(size = Inf))), "`toc_style\\$heading\\$size` must be a positive number")
  expect_error(bad(list(title = list(bold = "yes"))), "`toc_style\\$title\\$bold` must be TRUE or FALSE")
  expect_error(bad(list(title = list(bold = NA))), "`toc_style\\$title\\$bold` must be TRUE or FALSE")
  expect_error(bad(list(title = list(bold = c(TRUE, FALSE)))), "`toc_style\\$title\\$bold` must be TRUE or FALSE")
  expect_error(bad("big"), "`toc_style` must be NULL or a list with the keys title, heading, entry, gap")
  expect_error(bad(list(8)), "`toc_style` must be NULL or a list")
  expect_error(bad(list(title = 12)), "`toc_style\\$title` must be a list with size and bold")
  # the same checks when it comes from the option
  old <- options(rtfreporter.toc_style = list(entry = list(size = 0)))
  on.exit(options(old), add = TRUE)
  expect_error(assemble_rtf(f, out, toc = "auto"), "`toc_style\\$entry\\$size`")
})

test_that("FT-AS4031 [FR-AS260]: the TOC writes no font of its own: it follows the document's font (that of the first input)", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"), font = "Arial")
  x <- .tt_asm(f, toc = data.frame(file = f, label = c("a", "b"), heading = c("H", NA)))
  expect_match(x[2L], "Arial")
  expect_false(any(grepl("\\\\f[0-9]", .tt_paras(x))))
})

test_that("FT-AS4032 [FR-AS262]: with the default toc_title and toc_style the TOC is written byte for byte as before they existed", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  out <- .tt_case_baseline(d)
  expect_identical(.tt_bytes(out),
                   .tt_bytes(test_path("fixtures", "toc-baseline-defaults.rtf")))
  # the explicit defaults give the same bytes
  f <- list.files(d, "^[tl]_.*[.]rtf$", full.names = TRUE)
  toc <- data.frame(file = f, heading = c("Efficacy", "Safety", "Safety"),
                    label = c("Table 14.1.1 Demographics", "Table 14.2.1 Adverse Events",
                              "Listing 16.1.1 Adverse events"), level = c(2L, 2L, 3L))
  f <- f[match(c("t_14_1_1", "t_14_2_1", "l_16_1_1"), sub("[.]rtf$", "", basename(f)))]
  toc$file <- f
  out2 <- file.path(d, "again.rtf")
  assemble_rtf(f, out2, cover = .tt_cover, toc = toc,
               toc_title = c(c = "Table of Contents"), toc_style = list())
  expect_identical(.tt_bytes(out2), .tt_bytes(out))
})

# -- where a file is named -----------------------------------------------------

test_that("FT-AS4033 [FR-AS268]: the file column may hold absolute paths, or paths relative to the working directory", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 A", t_14_2_1 = "Table 14.2.1 B"))
  abs <- .tt_asm(f, toc = data.frame(file = f))
  expect_identical(.tt_labels(abs), c("Table 14.1.1 A", "Table 14.2.1 B"))
  old <- setwd(d)
  on.exit(setwd(old), add = TRUE, after = FALSE)
  out <- file.path(d, "relative.rtf")
  assemble_rtf(toc = data.frame(file = c("t_14_1_1.rtf", "t_14_2_1.rtf")), output_file = out)
  rel <- readLines(out, warn = FALSE)
  expect_identical(.tt_labels(rel), c("Table 14.1.1 A", "Table 14.2.1 B"))
  expect_identical(.tt_entries(rel), .tt_entries(abs))
  # relative paths with input_files given the same way
  assemble_rtf(c("t_14_1_1.rtf", "t_14_2_1.rtf"), out, overwrite = TRUE,
               toc = data.frame(file = c("./t_14_1_1.rtf", "t_14_2_1.rtf"),
                                label = c("one", "two")))
  expect_identical(.tt_labels(readLines(out, warn = FALSE)), c("one", "two"))
  # a number (an index) is not accepted: it is looked for as a file name, and stops
  expect_error(assemble_rtf(f, out, overwrite = TRUE, toc = data.frame(file = 1:2, label = c("a", "b"))),
               "The table of contents names missing file(s): 1, 2", fixed = TRUE)
})

test_that("FT-AS4034 [FR-AS268]: a file given as a number (an index) is not accepted by a table: it is looked for as a file", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  out <- file.path(d, "assembled.rtf")
  expect_error(assemble_rtf(f, out, toc = data.frame(file = 1:2, label = c("a", "b"))),
               "names missing file\\(s\\): 1, 2")
})

# -- layout: digits, long labels, several pages, a cover ------------------------

test_that("FT-AS4035 [FR-AS264 FR-AS125]: page numbers crossing 9 -> 10 and 99 -> 100 keep the same tab stop, leader and fields on every entry", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  out <- .tt_case_digits(d)
  x <- readLines(out, warn = FALSE)
  expect_identical(.tt_pages(x), c(2L, 9L, 10L, 98L, 99L, 100L))
  e <- .tt_entries(x)
  expect_length(e, 6L)
  # the paragraph's tab stop and leader are the same on every entry
  expect_true(all(grepl("^\\{\\\\pard\\\\ri720\\\\keep\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 \\{\\\\field", e)))
  expect_true(all(grepl("\\\\tab\\{\\\\field\\{\\\\\\*\\\\fldinst PAGEREF tfl_t_14_[0-9]_[0-9] \\\\\\\\h\\}\\{\\\\fldrslt [0-9]+\\}\\}\\\\par\\}$", e)))
  # each PAGEREF names its own file's bookmark, and the bookmark is there
  bm <- sub(".*PAGEREF ([^ ]+) .*", "\\1", e)
  expect_identical(bm, paste0("tfl_", c("t_14_1_1", "t_14_1_2", "t_14_2_1",
                                        "t_14_2_2", "t_14_3_1", "t_14_3_2")))
  for (b in bm) expect_true(any(grepl(paste0("{\\*\\bkmkstart ", b, "}"), x, fixed = TRUE)))
})

test_that("FT-AS4036 [FR-AS264]: the same page numbers with the leader off: the tab stop stays, the leader goes, on every entry", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"), pages = c(9L, 90L))
  e <- .tt_entries(.tt_asm(f, toc = "auto", toc_leader = "none"))
  expect_true(all(grepl("^\\{\\\\pard\\\\ri720\\\\keep\\\\fs20\\\\tqr\\\\tx14112 ", e)))
  expect_false(any(grepl("tldot", e)))
})

test_that("FT-AS4037 [FR-AS265]: a label too long for a line stays in its one paragraph, with its indent, right tab and leader, and counts its extra lines in the estimate", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  out <- .tt_case_longlabel(d)
  x <- readLines(out, warn = FALSE)
  e <- .tt_entries(x)
  expect_length(e, 3L)
  lab <- .tt_long_label()
  expect_true(nchar(lab) > 2 * 114)   # more than two lines of Courier New at 10 pt
  expect_match(e[2L], paste0("^\\{\\\\pard\\\\li600\\\\ri720\\\\keep\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 .*",
                             "fldrslt ", substr(lab, 1, 20)), fixed = FALSE)
  expect_true(grepl(lab, e[2L], fixed = TRUE))
  expect_match(e[3L], "^\\{\\\\pard\\\\li1200\\\\ri720\\\\keep\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ")
  # one paragraph each: the hanging indent is the indent of the level (no \fi)
  expect_false(any(grepl("\\\\fi[0-9-]", e)))
  expect_identical(sum(grepl("\\\\par\\}$", e)), 3L)
  # the estimate: that label is 3 lines (level 2) / 3 lines (level 3), a short one 1
  one <- list(type = "entry", label = "x", level = 2L)
  long <- list(type = "entry", label = lab, level = 2L)
  expect_identical(max(rtfreporter:::.toc_entry_pages(rep(list(one), 35L))), 1L)
  expect_identical(max(rtfreporter:::.toc_entry_pages(c(rep(list(one), 34L), list(long)))), 2L)
})

test_that("FT-AS4038 [FR-AS263 FR-AS125]: a TOC of two pages is split where the estimate says, a heading never closes a page, and the cached page numbers start after the TOC", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  out <- .tt_case_multipage(d)
  x <- readLines(out, warn = FALSE)
  p <- .tt_paras(x)
  pb <- grep("\\\\pagebb", p)
  expect_length(pb, 1L)
  # the heading that would have been last on page 1 starts page 2
  expect_identical(p[pb], "{\\pard\\pagebb\\b\\fs22 Safety\\par}")
  expect_identical(.tt_headings(x), "Safety")
  # the estimate and the file agree: 2 pages, the break before the heading
  f <- list.files(d, "^t_.*[.]rtf$", full.names = TRUE)
  toc <- rtfreporter:::.normalize_toc(data.frame(
    file = f[rep_len(1:3, 40L)], label = sprintf("Table 14.%d.1 Summary %d", 1:40, 1:40),
    heading = c(rep(NA, 34L), "Safety", rep(NA, 5L)), level = 2L), f)
  pages <- rtfreporter:::.toc_entry_pages(toc)
  expect_identical(max(pages), 2L)
  expect_identical(which(diff(pages) == 1L) + 1L, 35L)   # the heading is the 35th item
  # the pages run on from the two TOC pages: the first file starts on page 3,
  # and no page-number command is written (#1049)
  expect_identical(.tt_pages(x)[1L], 3L)
  expect_false(any(grepl("pgnlcrm|pgnrestart|pgndec", x)))
})

test_that("FT-AS4039 [FR-AS263 FR-AS130]: the page numbering of a two-page TOC: no page-number command, and the cached numbers count the TOC pages", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"), pages = c(2L, 1L))
  n <- 40L
  toc <- data.frame(file = f[rep_len(1:2, n)], label = sprintf("Entry %02d", seq_len(n)))
  x <- .tt_asm(f, toc = toc)
  expect_length(grep("\\\\pagebb", x), 1L)           # two TOC pages
  expect_true(any(x == "\\sectd\\sbkpage\\lndscpsxn"))
  expect_false(any(grepl("pgnlcrm|pgnrestart|pgndec", x)))
  # cached numbers: the 2 TOC pages first; the first file is 2 pages
  expect_identical(.tt_pages(x)[c(1L, 2L)], c(3L, 5L))
})

test_that("FT-AS4040 [FR-AS266 FR-AS109]: with a cover the sections run cover, TOC, body, and the TOC's cached page numbers count the cover and the TOC pages", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "Table 14.1.1 A", t_14_2_1 = "Table 14.2.1 B",
                       t_14_3_1 = "Table 14.3.1 C"), pages = c(2L, 1L, 3L))
  x <- .tt_asm(f, cover = .tt_cover, toc = "auto")
  sect <- which(x == "\\sect")
  expect_length(sect, 4L)   # cover | TOC | file 1 | file 2 | file 3
  expect_true(any(grepl("\\\\fs44 Study XYZ-101", x[seq_len(sect[1L])])))
  toc <- x[(sect[1L] + 1L):sect[2L]]
  expect_true(any(grepl("\\\\fs28 Table of Contents", toc)))
  expect_false(any(grepl("\\\\fs28 Table of Contents|\\\\fs44", x[(sect[2L] + 1L):length(x)])))
  expect_true(any(grepl("Table 14\\.1\\.1 A", x[(sect[2L] + 1L):length(x)])))
  # one sequence from the cover: no numbering command anywhere
  expect_false(any(grepl("pgnlcrm|pgnrestart|pgndec", x)))
  # cover 1 + TOC 1 page; files of 2, 1 and 3 pages
  expect_identical(.tt_pages(x), 3L + c(0L, 2L, 3L))
})

# -- the pending-human fixtures ---------------------------------------------------

test_that("FT-AS4041 [FR-AS264]: the digit-boundary TOC is rebuilt byte for byte (fixtures/toc-digits.rtf)", {
  fx <- test_path("fixtures", "toc-digits.rtf")
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  expect_identical(.tt_bytes(.tt_case_digits(d)), .tt_bytes(fx))
})

test_that("FT-AS4042 [FR-AS265]: the long-label TOC is rebuilt byte for byte (fixtures/toc-longlabel.rtf)", {
  fx <- test_path("fixtures", "toc-longlabel.rtf")
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  expect_identical(.tt_bytes(.tt_case_longlabel(d)), .tt_bytes(fx))
})

test_that("FT-AS4043 [FR-AS263]: the two-page TOC is rebuilt byte for byte (fixtures/toc-multipage.rtf)", {
  fx <- test_path("fixtures", "toc-multipage.rtf")
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  expect_identical(.tt_bytes(.tt_case_multipage(d)), .tt_bytes(fx))
})

test_that("FT-AS4044 [FR-AS266]: the cover and TOC deliverable is rebuilt byte for byte (fixtures/toc-cover.rtf)", {
  fx <- test_path("fixtures", "toc-cover.rtf")
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  expect_identical(.tt_bytes(.tt_case_cover(d)), .tt_bytes(fx))
})

# -- the hanging indent of an entry (toc_style entry = list(hanging = )) ----------

# three levels, a heading, one entry per level
.tt_hang_toc <- function(f) {
  data.frame(file = f, heading = c("Efficacy", NA, NA), label = c("One", "Two", "Three"),
             level = c(1L, 2L, 3L))
}

test_that("FT-AS4045 [FR-AS269]: by default an entry paragraph carries no \\fi (no hanging indent) and the defaults are written as before", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C"))
  x <- .tt_asm(f, toc = .tt_hang_toc(f))
  e <- .tt_entries(x)
  expect_length(e, 3L)
  expect_false(any(grepl("\\\\fi[0-9-]", e)))
  expect_match(e[2L], "^\\{\\\\pard\\\\li600\\\\ri720\\\\keep\\\\fs20\\\\tqr")
  # the explicit default is the same bytes
  y <- .tt_asm(f, toc = .tt_hang_toc(f), toc_style = list(entry = list(hanging = FALSE)))
  expect_identical(y, x)
  d2 <- .tt_dir(); on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  expect_identical(.tt_bytes(.tt_case_baseline(d2)),
                   .tt_bytes(test_path("fixtures", "toc-baseline-defaults.rtf")))
})

test_that("FT-AS4046 [FR-AS269]: hanging = TRUE writes \\li<indent + 720>\\fi-720 on the entry paragraph of every level, and no heading changes", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C"))
  toc <- .tt_hang_toc(f)
  x0 <- .tt_asm(f, toc = toc)
  x  <- .tt_asm(f, toc = toc, toc_style = list(entry = list(hanging = TRUE)))
  e <- .tt_entries(x)
  expect_length(e, 3L)
  expect_match(e[1L], "^\\{\\\\pard\\\\li720\\\\fi-720\\\\ri720\\\\keep\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ")
  expect_match(e[2L], "^\\{\\\\pard\\\\li1320\\\\fi-720\\\\ri720\\\\keep\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ")
  expect_match(e[3L], "^\\{\\\\pard\\\\li1920\\\\fi-720\\\\ri720\\\\keep\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ")
  # headings (and the title) are as they were
  noent <- function(l) .tt_paras(l)[!grepl("PAGEREF", .tt_paras(l))]
  expect_identical(noent(x), noent(x0))
  expect_length(grep("\\\\b\\\\fs22 Efficacy", noent(x)), 1L)
  expect_false(any(grepl("\\\\fi[0-9-]", noent(x))))
})

test_that("FT-AS4047 [FR-AS269]: hanging = 360 writes \\li<indent + 360>\\fi-360", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C"))
  x <- .tt_asm(f, toc = .tt_hang_toc(f), toc_style = list(entry = list(hanging = 360)))
  e <- .tt_entries(x)
  expect_match(e[1L], "^\\{\\\\pard\\\\li360\\\\fi-360\\\\ri720\\\\keep\\\\fs20\\\\tqr")
  expect_match(e[2L], "^\\{\\\\pard\\\\li960\\\\fi-360\\\\ri720\\\\keep\\\\fs20\\\\tqr")
  expect_match(e[3L], "^\\{\\\\pard\\\\li1560\\\\fi-360\\\\ri720\\\\keep\\\\fs20\\\\tqr")
})

test_that("FT-AS4048 [FR-AS269 FR-AS260]: the option rtfreporter.toc_style sets the hanging indent, and the argument wins over it", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C"))
  old <- options(rtfreporter.toc_style = list(entry = list(hanging = 360)))
  on.exit(options(old), add = TRUE)
  e <- .tt_entries(.tt_asm(f, toc = .tt_hang_toc(f)))
  expect_match(e[2L], "^\\{\\\\pard\\\\li960\\\\fi-360\\\\ri720\\\\keep\\\\fs20\\\\tqr")
  e <- .tt_entries(.tt_asm(f, toc = .tt_hang_toc(f),
                           toc_style = list(entry = list(hanging = TRUE))))
  expect_match(e[2L], "^\\{\\\\pard\\\\li1320\\\\fi-720\\\\ri720\\\\keep\\\\fs20\\\\tqr")
  e <- .tt_entries(.tt_asm(f, toc = .tt_hang_toc(f),
                           toc_style = list(entry = list(hanging = FALSE))))
  expect_false(any(grepl("\\\\fi[0-9-]", e)))
})

test_that("FT-AS4049 [FR-AS269 FR-AS261]: a hanging that is not TRUE, FALSE or one positive whole number of twips, or is under another key, stops with a message naming it", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B"))
  out <- file.path(d, "assembled.rtf")
  bad <- function(h) assemble_rtf(f, out, toc = "auto", toc_style = list(entry = list(hanging = h)))
  msg <- "`toc_style\\$entry\\$hanging` must be TRUE, FALSE or a positive whole number of twips"
  expect_error(bad("yes"), msg)
  expect_error(bad(-1), msg)
  expect_error(bad(0), msg)
  expect_error(bad(1.5), msg)
  expect_error(bad(c(1, 2)), msg)
  expect_error(bad(NA), msg)
  expect_error(bad(NA_real_), msg)
  expect_error(bad(c(TRUE, FALSE)), msg)
  expect_error(assemble_rtf(f, out, toc = "auto", toc_style = list(heading = list(hanging = TRUE))),
               "`toc_style\\$heading` has unknown key\\(s\\) 'hanging'; it takes size, bold")
  expect_error(assemble_rtf(f, out, toc = "auto", toc_style = list(title = list(hanging = 360))),
               "`toc_style\\$title` has unknown key\\(s\\) 'hanging'")
  expect_error(assemble_rtf(f, out, toc = "auto", toc_style = list(gap = list(hanging = TRUE))),
               "`toc_style\\$gap` has unknown key\\(s\\) 'hanging'")
  old <- options(rtfreporter.toc_style = list(entry = list(hanging = "yes")))
  on.exit(options(old), add = TRUE)
  expect_error(assemble_rtf(f, out, toc = "auto"), msg)
})

test_that("FT-AS4050 [FR-AS269 FR-AS265]: with a hanging indent a wrapped label counts at least as many lines in the height estimate, and one more when the narrower lines wrap again", {
  lines <- rtfreporter:::.toc_label_lines
  # a label just over one line (level 2, 10 pt = 20 half-points) takes two,
  # and its second line, 720 twips narrower, still holds the rest
  expect_equal(lines(strrep("x", 150), 2L, 20L, 0L), 2)
  expect_equal(lines(strrep("x", 150), 2L, 20L, 720L), 2)
  # a label that fills two lines without the hanging indent wraps a third time
  full <- strrep("x", 210)
  expect_equal(lines(full, 2L, 20L, 0L), 2)
  expect_equal(lines(full, 2L, 20L, 720L), 3)
  # never fewer, whatever the length; a one-line label stays one line
  for (n in c(1, 50, 109, 110, 200, 215, 246, 400, 1000)) {
    for (lv in 1:3) {
      expect_gte(lines(strrep("x", n), lv, 20L, 720L), lines(strrep("x", n), lv, 20L, 0L))
    }
  }
  expect_equal(lines("x", 2L, 20L, 720L), 1)
  # the pages: a page holds 35 one-line entries; with 33 before it, two lines
  # fit and three do not
  one  <- list(type = "entry", label = "x", level = 2L)
  long <- list(type = "entry", label = full, level = 2L)
  on  <- rtfreporter:::.toc_style(list(entry = list(hanging = TRUE)))
  off <- rtfreporter:::.toc_style(NULL)
  pages <- function(st) {
    max(rtfreporter:::.toc_entry_pages(c(rep(list(one), 33L), list(long)), 1L, st))
  }
  expect_identical(pages(off), 1L)
  expect_identical(pages(on), 2L)
})

test_that("FT-AS4051 [FR-AS269]: the long-label TOC with a hanging indent is rebuilt byte for byte (fixtures/toc-hanging.rtf)", {
  fx <- test_path("fixtures", "toc-hanging.rtf")
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE))
  out <- .tt_case_hanging(d)
  expect_identical(.tt_bytes(out), .tt_bytes(fx))
  e <- .tt_entries(readLines(out, warn = FALSE))
  expect_match(e[2L], "^\\{\\\\pard\\\\li2480\\\\fi-1880\\\\ri720\\\\keep\\\\fs20")
  expect_match(e[3L], "^\\{\\\\pard\\\\li3080\\\\fi-1880\\\\ri720\\\\keep\\\\fs20")
})
