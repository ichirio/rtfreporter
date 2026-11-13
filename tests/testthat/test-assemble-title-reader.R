# test-assemble-title-reader.R -- the one reader of a file's title for a
# contents entry (#1021, #1030): assemble_rtf(toc = "auto"), a table toc row
# with no label and assemble_folder()'s label read the title rows as the file
# shows them and join them, and a line break inside a row, with two spaces.
# Helpers prefixed .tr_.  Fictional data only.

# A folder for a test's input files (the test removes it)
.tr_dir <- function() {
  d <- tempfile("tr")
  dir.create(d)
  d
}

# One report in `dir`: a table (or a figure, kind = "figure") with the
# title rows `titles` (NULL: none) and, optionally, a running header row.
.tr_doc <- function(dir, name, titles, kind = "table", header = NULL) {
  doc <- rtf_document()
  if (!is.null(header)) {
    doc <- rtf_section(doc, page = 1, header = rtf_header(header))
  }
  if (kind == "figure") {
    png_file <- file.path(tempdir(), paste0(name, "-tr.png"))
    png(png_file, width = 400, height = 300)
    plot.new()
    dev.off()
    doc <- rtf_figures(doc, png_file)
  } else {
    doc <- rtf_tables(doc, data.frame(Subject = c("S01", "S02"),
                                      Value = c("1.0", "2.0")))
  }
  if (!is.null(titles)) doc <- rtf_titles(doc, list(titles))
  f <- file.path(dir, paste0(name, ".rtf"))
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

# assemble_rtf() of `files` into a file outside their folder; its lines
.tr_asm <- function(files, toc) {
  out <- tempfile("tr-book", fileext = ".rtf")
  on.exit(unlink(out))
  assemble_rtf(files, out, toc = toc, overwrite = TRUE)
  readLines(out, warn = FALSE)
}

# The text of each contents entry (the HYPERLINK's result) and of each
# outline entry
.tr_entries <- function(lines) {
  e <- grep("HYPERLINK", lines, value = TRUE)
  sub(".*\\{\\\\fldrslt ((?:[^{}\\\\]|\\\\.)*)\\}\\}\\\\tab.*$", "\\1", e, perl = TRUE)
}
.tr_outline <- function(lines) {
  o <- grep("\\\\outlinelevel[0-9] ", lines, value = TRUE)
  sub("^.*\\\\outlinelevel[0-9] (.*)\\\\par\\}$", "\\1", o)
}

test_that("FT-AS7001 [FR-AS250]: a title with a line break gives one entry, the text before and after the break joined with two spaces, never the text \\line, for toc = \"auto\" and assemble_folder() alike", {
  d <- .tr_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  a <- .tr_doc(d, "t1", c("Table 1\nDemographics", "Safety Population"))
  b <- .tr_doc(d, "t2", c("Table 2", "Adverse Events"))
  # the file shows the break as a line break inside the title row
  expect_true(any(grepl("\\b Table 1\\line Demographics\\b0", readLines(a, warn = FALSE),
                        fixed = TRUE)))
  want <- c("Table 1  Demographics  Safety Population", "Table 2  Adverse Events")
  x <- .tr_asm(c(a, b), "auto")
  expect_identical(.tr_entries(x), want)
  expect_identical(.tr_outline(x), want)
  expect_false(any(grepl("\\line", .tr_entries(x), fixed = TRUE)))
  # assemble_folder(): the same label, nothing after the break dropped
  expect_identical(assemble_folder(d)$label, want)
})

test_that("FT-AS7002 [FR-AS250 FR-AS112]: toc = \"auto\" joins every title row of a file, a blank row between them passed over, not the first row only", {
  d <- .tr_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  a <- .tr_doc(d, "t_14_1_1", c("Table 14.1.1", "Demographics", "Safety Population"))
  b <- .tr_doc(d, "t_14_1_2", c("Table 14.1.2", "", "Vital Signs"))
  x <- .tr_asm(c(a, b), "auto")
  expect_identical(.tr_entries(x),
                   c("Table 14.1.1  Demographics  Safety Population",
                     "Table 14.1.2  Vital Signs"))
  expect_identical(.tr_outline(x), .tr_entries(x))
  # the entries' layout is that of any entry: level 1, a dot leader
  e <- grep("HYPERLINK", x, value = TRUE)
  expect_true(all(startsWith(e, "{\\pard\\fs20\\tqr\\tldot\\tx14112 {\\field")))
})

test_that("FT-AS7003 [FR-AS149 FR-AS250]: a figure and a listing are labelled by their titles, as a table is, by assemble_folder() and toc = \"auto\"", {
  d <- .tr_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  .tr_doc(d, "t_dm", c("Table 1", "Demographics"))
  .tr_doc(d, "f_figure", c("Figure 1", "Plot of Values"), kind = "figure")
  .tr_doc(d, "l_ae", c("Listing 1", "Adverse Events"))
  sp <- assemble_folder(d)
  # the order is unchanged: the numbered table, then the others by file name
  expect_identical(basename(sp$file), c("t_dm.rtf", "f_figure.rtf", "l_ae.rtf"))
  expect_identical(sp$table, c("1", NA, NA))
  want <- c("Table 1  Demographics", "Figure 1  Plot of Values", "Listing 1  Adverse Events")
  expect_identical(sp$label, want)
  expect_identical(.tr_entries(.tr_asm(sp$file, "auto")), want)
})

test_that("FT-AS7004 [FR-AS250 FR-AS112 FR-AS149 FR-AS258]: a file with no title is labelled by its file name, one with a \"Table N\" line in its running header only by that line, by every route", {
  d <- .tr_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  a <- .tr_doc(d, "t_14_1_1", c("Table 14.1.1", "Demographics"))
  b <- .tr_doc(d, "t_14_2_1", NULL,
               header = list(c(c = "Table 14.2.1"), c(c = "Adverse Events")))
  u <- .tr_doc(d, "untitled_listing", NULL)
  want <- c("Table 14.1.1  Demographics", "Table 14.2.1  Adverse Events",
            "untitled_listing")
  files <- c(a, b, u)
  expect_identical(.tr_entries(.tr_asm(files, "auto")), want)
  expect_identical(.tr_entries(.tr_asm(files, data.frame(file = files))), want)
  sp <- assemble_folder(d)
  expect_identical(sp$label[match(files, sp$file)], want)
})

test_that("FT-AS7005 [FR-AS250 FR-AS258]: toc = \"auto\", a table with no label and assemble_folder() give the same text for the same files: line breaks, markup, non-ASCII text", {
  d <- .tr_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  files <- c(
    .tr_doc(d, "t_14_1_1", c("Table 14.1.1\nH_{2}O at \u00b5g/mL", "Safety Population")),
    .tr_doc(d, "t_14_2_1", c("Table 14.2.1", "Caf\u00e9 {Visit}", "Week 4\nWeek 8")),
    .tr_doc(d, "f_14_3_1", c("Figure 14.3.1", "Mean \u2265 65"), kind = "figure"))
  auto <- .tr_asm(files, "auto")
  none <- .tr_asm(files, data.frame(file = files))
  sp   <- assemble_folder(d)
  lab  <- sp$label[match(files, sp$file)]
  expect_identical(lab, c("Table 14.1.1  H2O at \u00b5g/mL  Safety Population",
                          "Table 14.2.1  Caf\u00e9 {Visit}  Week 4  Week 8",
                          "Figure 14.3.1  Mean \u2265 65"))
  expect_identical(.tr_entries(none), .tr_entries(auto))
  expect_identical(.tr_outline(none), .tr_outline(auto))
  # the folder's table, handed to assemble_rtf(), writes the same entries
  folder <- .tr_asm(sp$file, sp)
  expect_identical(.tr_entries(folder)[match(files, sp$file)], .tr_entries(auto))
  # the entry is the label, RTF-escaped: \uN? for the non-ASCII, \{ \} for braces
  expect_identical(.tr_entries(auto)[2L],
                   "Table 14.2.1  Caf\\u233? \\{Visit\\}  Week 4  Week 8")
  expect_false(any(grepl("\\line", .tr_entries(auto), fixed = TRUE)))
  expect_false(any(grepl("\\sub", .tr_entries(auto), fixed = TRUE)))
})

test_that("FT-AS7006 [FR-AS250 FR-AS112]: one level-1 entry per file, read the same way by toc = \"auto\" and assemble_folder(): title rows joined, a blank row passed over, a line break joined, table / figure / listing by title, the header's \"Table N\" line, then the file name; tokens filled, markup without its formatting; the outline entry the same; one entry a file, titles that change by page read from the first page", {
  d <- .tr_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # a table: a line break in a row, a blank row, own / run / static / dynamic
  # page tokens, the book slot, markup and non-ASCII text
  t1 <- file.path(d, "t_14_1_1.rtf")
  doc <- rtf_document(tokens = list(STUDY = "XY-1"), program = "prog_x.R") |>
    rtf_tables(data.frame(A = 1:2, B = "x")) |>
    rtf_titles(list(c("Table 14.1.1\nDemographics", "",
                      paste("{STUDY} {PROGRAM} p{PAGE}/{TOTAL_PAGES}",
                            "[{AUTO_PAGE}/{AUTO_TOTAL_PAGES}] {BOOK_PAGE} H_{2}O \u00b5g"))))
  generate_rtfreport(doc, t1, overwrite = TRUE)
  f1 <- .tr_doc(d, "f_14_2_1", c("Figure 14.2.1", "Mean Values"), kind = "figure")
  l1 <- .tr_doc(d, "l_16_1", c("Listing 16.1", "Subject Listing"))
  h1 <- .tr_doc(d, "t_14_3_1", NULL,
                header = list(c(c = "Table 14.3.1"), c(c = "Vital Signs")))
  u1 <- .tr_doc(d, "untitled", NULL)
  files <- c(t1, f1, l1, h1, u1)
  # the file shows the break, the fields, the slot and the markup group
  src <- paste(readLines(t1, warn = FALSE), collapse = "\n")
  expect_true(grepl("\\b Table 14.1.1\\line Demographics\\b0", src, fixed = TRUE))
  expect_true(grepl("\\chpgn", src, fixed = TRUE))
  expect_true(grepl("NUMPAGES", src, fixed = TRUE))
  expect_true(grepl("{\\*\\rtfreporterbookpage}", src, fixed = TRUE))
  expect_true(grepl("H{\\sub 2}O", src, fixed = TRUE))

  want <- c("Table 14.1.1  Demographics  XY-1 prog_x.R p1/1 [ / ] H2O \u00b5g",
            "Figure 14.2.1  Mean Values", "Listing 16.1  Subject Listing",
            "Table 14.3.1  Vital Signs", "untitled")
  want_rtf <- want
  want_rtf[1L] <- "Table 14.1.1  Demographics  XY-1 prog_x.R p1/1 [ / ] H2O \\u181?g"

  x <- .tr_asm(files, "auto")
  e <- grep("HYPERLINK", x, value = TRUE)
  expect_length(e, 5L)
  # level 1: no indent
  expect_true(all(startsWith(e, "{\\pard\\fs20\\tqr\\tldot\\tx14112 {\\field")))
  expect_identical(.tr_entries(x), want_rtf)
  expect_identical(.tr_outline(x), want_rtf)
  expect_false(any(grepl("\\\\line|\\\\chpgn|NUMPAGES|rtfreporterbookpage|\\\\sub|\\{STUDY\\}", e)))
  # assemble_folder(): the same labels, as plain text
  sp <- assemble_folder(d)
  expect_identical(sp$label[match(files, sp$file)], want)

  # one entry a file: titles that change by page are read from the first page
  d2 <- .tr_dir()
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  pv <- data.frame(PARAM = rep(c("ALT", "AST"), each = 2), Value = c("1", "2", "3", "4"))
  p1 <- file.path(d2, "t_14_4_1.rtf")
  generate_rtfreport(rtf_document() |>
                       rtf_tables(as_rtftables(pv, page_by = "PARAM", drop_cols = "PARAM"),
                                  titles = list(c("Table 14.4.1", "Summary of {PARAM}"))),
                     p1, overwrite = TRUE)
  p2 <- .tr_doc(d2, "t_14_4_2", c("Table 14.4.2", "Other"))
  expect_true(grepl("Summary of AST", paste(readLines(p1, warn = FALSE), collapse = "\n"), fixed = TRUE))
  want_pv <- c("Table 14.4.1  Summary of ALT", "Table 14.4.2  Other")
  expect_identical(.tr_entries(.tr_asm(c(p1, p2), "auto")), want_pv)
  expect_identical(assemble_folder(d2)$label, want_pv)
})

# -- #1046: a running header's "Table N" line keeps the title written on it --

# A file whose running header has the centred lines `lines` and no title rows
.tr_hdr <- function(dir, name, lines) {
  .tr_doc(dir, name, NULL,
          header = c(list(c(l = "Study XYZ-001", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")),
                     lapply(lines, function(x) c(c = x))))
}

# A file of the shape assemble_folder() reads with a running header of the
# centred cells `cells`, written as RTF (\uN? escapes as given)
.tr_raw_hdr <- function(path, cells) {
  rows <- paste0("\\trowd\\pard\\intbl\\qc\\li0\\ri0 ", cells, "\\cell\\row")
  writeLines(c("{\\rtf1\\ansi", paste0("{\\header ", paste(rows, collapse = " "), "}"),
               "\\sectd", "BODY", "}"), path)
  path
}

test_that("FT-AS7101 [FR-AS150]: the title from a running header: the rest of a \"Table N\" line that carries it, over the centred lines below it (a population, \"Draft\"), for toc = \"auto\", a table with no label and assemble_folder(); else the longest centred cell that is not a <...> subtitle; \\uN? read back", {
  d <- .tr_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  a <- .tr_hdr(d, "t_14_1_1", c("Table 14.1.1 Demographic Characteristics",
                                "Safety Population"))
  b <- .tr_hdr(d, "t_14_1_2", c("Table 14.1.2 Vital Signs", "Full Analysis Set",
                                "Draft"))
  c3 <- .tr_doc(d, "t_14_1_3", c("Table 14.1.3", "Demo"))
  files <- c(a, b, c3)
  want <- c("Table 14.1.1  Demographic Characteristics", "Table 14.1.2  Vital Signs",
            "Table 14.1.3  Demo")
  expect_identical(.tr_entries(.tr_asm(files, "auto")), want)
  expect_identical(.tr_outline(.tr_asm(files, "auto")), want)
  expect_identical(.tr_entries(.tr_asm(files, data.frame(file = files))), want)
  sp <- assemble_folder(d)
  expect_identical(sp$label[match(files, sp$file)], want)
  expect_identical(sp$table[match(files, sp$file)], c("14.1.1", "14.1.2", "14.1.3"))

  # the number alone on its line: the longest other centred cell, not a <...>
  # subtitle; \uN? read back (signed / unsigned, a surrogate pair as one
  # character, a no-break space as a space), on the "Table N" line too
  d2 <- .tr_dir()
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  .tr_raw_hdr(file.path(d2, "a.rtf"), c(
    "Table 14.6.1", "Short title", "<A subtitle that is much longer than the others>",
    "Longest \\u181?g/mL \\u-10179?\\u-8704? \\u55357?\\u56832? a\\u160?b"))
  .tr_raw_hdr(file.path(d2, "b.rtf"), c(
    "Table 14.6.2 Inline \\u181?g/mL\\u160?title",
    "A population line much longer than the title on the number's line"))
  sp2 <- assemble_folder(d2)
  smile <- "\U0001F600"
  expect_identical(sp2$table, c("14.6.1", "14.6.2"))
  expect_identical(sp2$label, c(
    paste0("Table 14.6.1  Longest \u00b5g/mL ", smile, " ", smile, " a b"),
    "Table 14.6.2  Inline \u00b5g/mL title"))
})

test_that("FT-AS7102 [FR-AS150]: a \"Table N\" line with the number only takes the longest other centred line, as before", {
  d <- .tr_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  a <- .tr_hdr(d, "t_14_2_1", c("Table 14.2.1", "Vital Signs Summary by Visit",
                                "Safety Population"))
  b <- .tr_doc(d, "t_14_2_2", c("Table 14.2.2", "Other"))
  files <- c(a, b)
  want <- c("Table 14.2.1  Vital Signs Summary by Visit", "Table 14.2.2  Other")
  expect_identical(.tr_entries(.tr_asm(files, "auto")), want)
  expect_identical(.tr_entries(.tr_asm(files, data.frame(file = files))), want)
  expect_identical(assemble_folder(d)$label, want)
})

test_that("FT-AS7103 [FR-AS150]: a <...> subtitle is never the title, with the number alone on its line or the title on it", {
  d <- .tr_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  a <- .tr_hdr(d, "t_14_3_1", c("Table 14.3.1", "Lab Values",
                                "<Safety Population of all treated subjects>"))
  b <- .tr_hdr(d, "t_14_3_2", c("Table 14.3.2 Lab Shifts",
                                "<Safety Population of all treated subjects>"))
  files <- c(a, b)
  want <- c("Table 14.3.1  Lab Values", "Table 14.3.2  Lab Shifts")
  expect_identical(.tr_entries(.tr_asm(files, "auto")), want)
  expect_identical(.tr_entries(.tr_asm(files, data.frame(file = files))), want)
  expect_identical(assemble_folder(d)$label, want)
})

test_that("FT-AS7104 [FR-AS250]: a file whose titles change by page ({PARAM} with page_by) gets one entry, named from its first page's title, from its title rows or its running header", {
  d <- .tr_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  pv <- data.frame(PARAM = rep(c("ALT", "AST", "BILI"), each = 2),
                   Value = as.character(1:6))
  pages <- as_rtftables(pv, page_by = "PARAM", drop_cols = "PARAM")
  a <- file.path(d, "t_14_5_1.rtf")
  generate_rtfreport(rtf_document() |>
                       rtf_tables(pages, titles = list(c("Table 14.5.1", "Summary of {PARAM}"))),
                     a, overwrite = TRUE)
  b <- file.path(d, "t_14_5_2.rtf")
  generate_rtfreport(rtf_document() |>
                       rtf_section(header = rtf_header(list(c(c = "Table 14.5.2 Lab {PARAM}"),
                                                            c(c = "Safety Population")))) |>
                       rtf_tables(pages),
                     b, overwrite = TRUE)
  # the files do show a title per page
  src_a <- paste(readLines(a, warn = FALSE), collapse = "\n")
  src_b <- paste(readLines(b, warn = FALSE), collapse = "\n")
  for (v in c("ALT", "AST", "BILI")) {
    expect_true(grepl(paste("Summary of", v), src_a, fixed = TRUE))
    expect_true(grepl(paste("Lab", v), src_b, fixed = TRUE))
  }
  files <- c(a, b)
  want <- c("Table 14.5.1  Summary of ALT", "Table 14.5.2  Lab ALT")
  x <- .tr_asm(files, "auto")
  expect_length(grep("HYPERLINK", x), 2L)
  expect_identical(.tr_entries(x), want)
  expect_identical(.tr_entries(.tr_asm(files, data.frame(file = files))), want)
  sp <- assemble_folder(d)
  expect_identical(nrow(sp), 2L)
  expect_identical(sp$label, want)
  expect_identical(sp$pages, c(3L, 3L))
})
