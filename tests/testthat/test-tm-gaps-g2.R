# Tests added to close the gaps found by the Traceability Matrix check (2026-11-12):
# each asserts what its requirement states.

# ---- helpers (prefixed .g2 so that no other test file is touched) ------------

# A work folder (each test removes it with on.exit()).
.g2_dir <- function() {
  d <- tempfile("g2-")
  dir.create(d)
  d
}

# A small file that has the shape assemble_rtf() reads: a preamble, a \sectd line,
# a body line and the closing brace.
.g2_fake <- function(path, tag, pre = "{\\fonttbl}") {
  writeLines(c("{\\rtf1\\ansi", pre, "\\sectd", paste0("BODY-", tag), "}"), path)
  path
}

# A file written by generate_rtfreport(): a header with the page fields, a footer
# and a title block (title = NULL: none).
.g2_demo <- function(path, title = NULL, footer = "Confidential") {
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(
      c(l = "Protocol RTF-101", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")
    )),
    footer = rtf_footer(footer)
  ))
  doc <- rtf_tables(doc, list(data.frame(A = 1:2, B = c("x", "y"),
                                         stringsAsFactors = FALSE)),
                    titles = if (is.null(title)) NULL else list(c(title)))
  generate_rtfreport(doc, path, overwrite = TRUE)
  path
}

# a base plot with small margins, so that it draws on a small device too
.g2_pl <- function() {
  graphics::par(mar = rep(0.1, 4))
  graphics::plot(1)
}

.g2_lines <- function(f) readLines(f, warn = FALSE)
.g2_txt <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")

# ---- assemble_rtf -----------------------------------------------------------

test_that("FT-AS3000 [FR-AS101]: assemble_rtf() writes file 1 whole but its brace, file 2 from its first \\sectd, joined by \\sect, one closing brace, and returns output_file invisibly", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_fake(file.path(d, "a.rtf"), "A", pre = "PRE-A")
  f2 <- .g2_fake(file.path(d, "b.rtf"), "B", pre = "PRE-B")
  out <- file.path(d, "out.rtf")
  res <- withVisible(assemble_rtf(c(f1, f2), out))
  expect_false(res$visible)
  expect_identical(res$value, out)
  expect_identical(
    .g2_lines(out),
    c("{\\rtf1\\ansi", "PRE-A", "\\sectd", "BODY-A", "\\sect", "\\sectd", "BODY-B", "}")
  )
})

test_that("FT-AS3001 [FR-AS102]: input_files are joined in the order given, and a toc table (or the path of one) names the files when input_files is left out", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_fake(file.path(d, "a.rtf"), "A")
  f2 <- .g2_fake(file.path(d, "b.rtf"), "B")
  out <- file.path(d, "out.rtf")
  assemble_rtf(c(f2, f1), out)
  txt <- .g2_lines(out)
  expect_lt(which(txt == "BODY-B"), which(txt == "BODY-A"))

  csv <- file.path(d, "toc.csv")
  utils::write.csv(data.frame(file = c(f1, f2), label = c("First A", "Second B")),
                   csv, row.names = FALSE)
  out2 <- file.path(d, "out2.rtf")
  assemble_rtf(output_file = out2, toc = csv)
  l2 <- .g2_lines(out2)
  expect_lt(which(l2 == "BODY-A"), which(l2 == "BODY-B"))
  expect_true(any(grepl("{\\fldrslt First A}}", l2, fixed = TRUE)))
  expect_true(any(grepl("{\\fldrslt Second B}}", l2, fixed = TRUE)))

  # a table as a data.frame names the files too, in its row order
  out3 <- file.path(d, "out3.rtf")
  assemble_rtf(output_file = out3,
               toc = data.frame(file = c(f2, f1), label = c("Second B", "First A"),
                                stringsAsFactors = FALSE))
  l3 <- .g2_lines(out3)
  expect_lt(which(l3 == "BODY-B"), which(l3 == "BODY-A"))
  expect_true(any(grepl("{\\fldrslt Second B}}", l3, fixed = TRUE)))
})

test_that("FT-AS3002 [FR-AS106]: assemble_rtf() with a cover and a first file without \\sectd stops", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  bad <- file.path(d, "bad.rtf")
  writeLines(c("{\\rtf1\\ansi", "no section here", "}"), bad)
  g <- .g2_fake(file.path(d, "g.rtf"), "G")
  expect_error(
    assemble_rtf(c(bad, g), file.path(d, "out.rtf"), cover = list(title = "T")),
    "File 1 has no \\sectd; cannot insert cover / TOC.", fixed = TRUE)
  # a table of contents alone stops the same way
  expect_error(
    assemble_rtf(c(bad, g), file.path(d, "out.rtf"), toc = c("A", "B")),
    "File 1 has no \\sectd; cannot insert cover / TOC.", fixed = TRUE)
})

test_that("FT-AS3003 [FR-AS109]: the cover is 22 pt bold centred title, 14 pt subtitle, 11 pt date and version, 10 pt meta lines, escaped, before the table of contents; NULL adds none", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_fake(file.path(d, "a.rtf"), "A")
  f2 <- .g2_fake(file.path(d, "b.rtf"), "B")
  out <- file.path(d, "out.rtf")
  assemble_rtf(c(f1, f2), out, toc = c("L1", "L2"),
               cover = list(title = "A{B}", subtitle = "S\u00b5", date = "D",
                            version = "V", meta = c("M1", "M\\2")))
  l <- .g2_lines(out)
  expect_true("{\\pard\\qc\\b\\fs44 A\\{B\\}\\par}" %in% l)
  expect_true("{\\pard\\qc\\fs28 S\\u181?\\par}" %in% l)
  expect_true("{\\pard\\qc\\fs22 D\\par}" %in% l)
  expect_true("{\\pard\\qc\\fs22 V\\par}" %in% l)
  expect_true("{\\pard\\qc\\fs20 M1\\par}" %in% l)
  expect_true("{\\pard\\qc\\fs20 M\\\\2\\par}" %in% l)
  expect_lt(which(l == "{\\pard\\qc\\fs20 M\\\\2\\par}"),
            which(l == "{\\pard\\qc\\b\\fs28 Table of Contents\\par}"))

  out0 <- file.path(d, "out0.rtf")
  assemble_rtf(c(f1, f2), out0, toc = c("L1", "L2"), cover = NULL)
  l0 <- .g2_lines(out0)
  expect_false(any(grepl("\\fs44", l0, fixed = TRUE)))
  expect_false(any(grepl("\\qc\\fs20", l0, fixed = TRUE)))
  # the cover adds its section (preamble 4 lines, title, subtitle, date, version,
  # meta and the blank paragraphs) and one \sect
  expect_gt(length(l), length(l0))
  # (the cached page numbers of the entries move by the cover page)
  keep <- function(x) x[!grepl("PAGEREF", x, fixed = TRUE)]
  expect_identical(setdiff(keep(l0), keep(l)), character(0))
})

test_that("FT-AS3004 [FR-AS110]: NULL, NA and empty cover fields (and an NA line of meta) are skipped without a message", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_fake(file.path(d, "a.rtf"), "A")
  f2 <- .g2_fake(file.path(d, "b.rtf"), "B")
  out <- file.path(d, "out.rtf")
  expect_silent(assemble_rtf(
    c(f1, f2), out,
    cover = list(title = "T", subtitle = NULL, date = NA, version = "",
                 meta = c("M", NA))))
  l <- .g2_lines(out)
  expect_true("{\\pard\\qc\\b\\fs44 T\\par}" %in% l)
  expect_false(any(grepl("\\qc\\fs28", l, fixed = TRUE)))
  expect_false(any(grepl("\\qc\\fs22", l, fixed = TRUE)))
  expect_identical(sum(grepl("\\qc\\fs20", l, fixed = TRUE)), 1L)
  expect_true("{\\pard\\qc\\fs20 M\\par}" %in% l)
  expect_false(any(grepl("HYPERLINK", l, fixed = TRUE)))
})

test_that("FT-AS3005 [FR-AS111]: toc = NULL with toc_page_numbering = \"none\" and cover = NULL is byte for byte the plain concatenation", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_fake(file.path(d, "a.rtf"), "A", pre = "PRE-A")
  f2 <- .g2_fake(file.path(d, "b.rtf"), "B", pre = "PRE-B")
  out <- file.path(d, "out.rtf")
  assemble_rtf(c(f1, f2), out, toc = NULL, toc_page_numbering = "none", cover = NULL)
  ref <- file.path(d, "ref.rtf")
  writeLines(c("{\\rtf1\\ansi", "PRE-A", "\\sectd", "BODY-A", "\\sect",
               "\\sectd", "BODY-B", "}"), ref)
  expect_identical(readBin(out, "raw", file.size(out)),
                   readBin(ref, "raw", file.size(ref)))
  txt <- .g2_txt(out)
  expect_false(grepl("bkmkstart", txt, fixed = TRUE))
  expect_false(grepl("outlinelevel", txt, fixed = TRUE))
  expect_false(grepl("Table of Contents", txt, fixed = TRUE))
})

test_that("FT-AS3006 [FR-AS112]: toc = \"auto\" makes one level-1 entry per file from the decoded title, and the file name without .rtf for a file with no title", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_demo(file.path(d, "t_one.rtf"), "Lab \u00b5g/mL summary")
  f2 <- .g2_demo(file.path(d, "plain_two.rtf"), NULL)
  out <- file.path(d, "out.rtf")
  assemble_rtf(c(f1, f2), out, toc = "auto")
  l <- .g2_lines(out)
  ent <- l[grepl("HYPERLINK", l, fixed = TRUE)]
  expect_length(ent, 2L)
  # level 1: no indent
  expect_true(all(startsWith(ent, "{\\pard\\fs20\\tqr\\tldot\\tx14112 {\\field")))
  expect_true(grepl("{\\fldrslt Lab \\u181?g/mL summary}}", ent[1], fixed = TRUE))
  expect_true(grepl("{\\fldrslt plain_two}}", ent[2], fixed = TRUE))
})

test_that("FT-AS3007 [FR-AS115]: a toc table prints a heading above an entry when its value changes (NA or empty none), uses each label, and indents entries at level 2 when level is absent", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  fs <- vapply(1:4, function(i) .g2_fake(file.path(d, paste0("f", i, ".rtf")), i),
               character(1L))
  out <- file.path(d, "out.rtf")
  tb <- data.frame(file = fs, label = paste("Label", 1:4),
                   heading = c("HA", "HA", NA, "HB"), stringsAsFactors = FALSE)
  assemble_rtf(toc = tb, output_file = out)
  l <- .g2_lines(out)
  h <- grep("\\b\\fs22", l, fixed = TRUE)
  expect_identical(l[h], c("{\\pard\\b\\fs22 HA\\par}", "{\\pard\\b\\fs22 HB\\par}"))
  e <- grep("HYPERLINK", l, fixed = TRUE)
  expect_length(e, 4L)
  expect_true(all(startsWith(l[e], "{\\pard\\li600\\fs20\\tqr\\tldot\\tx14112 ")))
  for (k in 1:4) {
    expect_true(grepl(sprintf("{\\fldrslt Label %d}}", k), l[e[k]], fixed = TRUE))
  }
  # HA right above Label 1 only; HB right above Label 4
  expect_identical(h[1], e[1] - 1L)
  expect_identical(h[2], e[4] - 1L)

  # no label (NA) takes the file's title; level 1, 3 and NA (2) give no indent, 1200 and
  # 600 twips; order = 3, 1, 2 takes row 2, then row 3, then row 1
  g <- vapply(1:3, function(i) {
    .g2_demo(file.path(d, sprintf("g%d.rtf", i)), sprintf("Table %d T%d", i, i))
  }, character(1L))
  tb2 <- data.frame(file = g, label = c(NA, "Own two", NA), level = c(1L, 3L, NA),
                    order = c(3L, 1L, 2L), stringsAsFactors = FALSE)
  out2 <- file.path(d, "out2.rtf")
  assemble_rtf(toc = tb2, output_file = out2)
  l2 <- .g2_lines(out2)
  e2 <- grep("HYPERLINK", l2, fixed = TRUE)
  expect_length(e2, 3L)
  expect_true(startsWith(l2[e2[1]], "{\\pard\\li1200\\fs20\\tqr\\tldot\\tx14112 "))
  expect_true(startsWith(l2[e2[2]], "{\\pard\\li600\\fs20\\tqr\\tldot\\tx14112 "))
  expect_true(startsWith(l2[e2[3]], "{\\pard\\fs20\\tqr\\tldot\\tx14112 "))
  expect_true(grepl("{\\fldrslt Own two}}", l2[e2[1]], fixed = TRUE))
  expect_true(grepl("{\\fldrslt Table 3 T3}}", l2[e2[2]], fixed = TRUE))
  expect_true(grepl("{\\fldrslt Table 1 T1}}", l2[e2[3]], fixed = TRUE))
  # the files follow the same order
  bm <- sub("^.*bkmkstart ([A-Za-z0-9_]+)\\}.*$", "\\1", grep("bkmkstart", l2, value = TRUE))
  expect_identical(bm, c("tfl_g2", "tfl_g3", "tfl_g1"))
})

test_that("FT-AS3008 [FR-AS117]: an .xlsx toc without readxl stops with its message, and an existing .xlsx is read as the table", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_fake(file.path(d, "a.rtf"), "A")
  f2 <- .g2_fake(file.path(d, "b.rtf"), "B")
  x <- file.path(d, "spec.xlsx")
  writexl::write_xlsx(data.frame(file = c(f1, f2), label = c("Xlsx one", "Xlsx two")), x)
  out <- file.path(d, "out.rtf")
  assemble_rtf(output_file = out, toc = x)
  l <- .g2_lines(out)
  expect_true(any(grepl("{\\fldrslt Xlsx one}}", l, fixed = TRUE)))
  expect_true(any(grepl("{\\fldrslt Xlsx two}}", l, fixed = TRUE)))
  expect_lt(which(l == "BODY-A"), which(l == "BODY-B"))

  # the same table as a .csv path
  csv <- file.path(d, "spec.csv")
  utils::write.csv(data.frame(file = c(f2, f1), label = c("Csv one", "Csv two")),
                   csv, row.names = FALSE)
  outc <- file.path(d, "outc.rtf")
  assemble_rtf(output_file = outc, toc = csv)
  lc <- .g2_lines(outc)
  expect_true(any(grepl("{\\fldrslt Csv one}}", lc, fixed = TRUE)))
  expect_lt(which(lc == "BODY-B"), which(lc == "BODY-A"))

  local({
    local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
    expect_error(
      assemble_rtf(output_file = file.path(d, "out2.rtf"), toc = x),
      "Reading an .xlsx spec needs the 'readxl' package; install it or use a .csv path.",
      fixed = TRUE)
  })
})

test_that("FT-AS3009 [FR-AS125]: each HYPERLINK target is the bookmark written at the start of its file's section, entry for entry", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_demo(file.path(d, "g2_a.rtf"), "Table 1 A")
  f2 <- .g2_demo(file.path(d, "g2_b.rtf"), "Table 2 B")
  out <- file.path(d, "out.rtf")
  assemble_rtf(c(f1, f2), out, toc = "auto")
  txt <- .g2_txt(out)
  links <- regmatches(txt, gregexpr("HYPERLINK \\\\\\\\l \"[^\"]+\"", txt))[[1]]
  links <- sub("^.*\"([^\"]+)\"$", "\\1", links)
  starts <- regmatches(txt, gregexpr("bkmkstart [A-Za-z0-9_]+", txt))[[1]]
  starts <- sub("^bkmkstart ", "", starts)
  expect_identical(links, c("tfl_g2_a", "tfl_g2_b"))
  expect_identical(starts, links)
  # and the PAGEREF of each entry points at the same bookmark
  pr <- regmatches(txt, gregexpr("PAGEREF [A-Za-z0-9_]+", txt))[[1]]
  expect_identical(sub("^PAGEREF ", "", pr), links)
  # the cached page numbers: the contents on page 1, then one page for each file
  cached <- regmatches(txt, gregexpr("PAGEREF [^}]*\\}\\{\\\\fldrslt [0-9]+", txt))[[1]]
  expect_identical(sub("^.*fldrslt ", "", cached), c("2", "3"))
})

test_that("FT-AS3010 [FR-AS127]: toc_title is the centred bold title of the contents page", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_fake(file.path(d, "a.rtf"), "A")
  f2 <- .g2_fake(file.path(d, "b.rtf"), "B")
  out <- file.path(d, "out.rtf")
  assemble_rtf(c(f1, f2), out, toc = c("L1", "L2"), toc_title = "My List")
  expect_true("{\\pard\\qc\\b\\fs28 My List\\par}" %in% .g2_lines(out))
  assemble_rtf(c(f1, f2), out, toc = c("L1", "L2"), overwrite = TRUE)
  expect_true("{\\pard\\qc\\b\\fs28 Table of Contents\\par}" %in% .g2_lines(out))
  # named l or r (FR-AS256) it is aligned left or right
  assemble_rtf(c(f1, f2), out, toc = c("L1", "L2"), toc_title = c(l = "My List"),
               overwrite = TRUE)
  expect_true("{\\pard\\ql\\b\\fs28 My List\\par}" %in% .g2_lines(out))
  assemble_rtf(c(f1, f2), out, toc = c("L1", "L2"), toc_title = c(r = "My List"),
               overwrite = TRUE)
  expect_true("{\\pard\\qr\\b\\fs28 My List\\par}" %in% .g2_lines(out))
})

test_that("FT-AS3011 [FR-AS133]: bookmark names are prefix + file name without .rtf, other characters an underscore, cut to 32, bk_ before a name not starting with a letter", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  long <- strrep("abcdefghij", 6)
  fs <- c(.g2_fake(file.path(d, "my file-1.2.rtf"), "A"),
          .g2_fake(file.path(d, "1abc.rtf"), "B"),
          .g2_fake(file.path(d, paste0(long, ".rtf")), "C"))
  out <- file.path(d, "out.rtf")
  bk <- function(...) {
    assemble_rtf(fs, out, toc = c("a", "b", "c"), overwrite = TRUE, ...)
    txt <- .g2_txt(out)
    starts <- regmatches(txt, gregexpr("bkmkstart [A-Za-z0-9_]+", txt))[[1]]
    sub("^bkmkstart ", "", starts)
  }
  expect_identical(bk(), c("tfl_my_file_1_2", "tfl_bk_1abc",
                           paste0("tfl_", substr(long, 1, 32))))
  expect_identical(bk(bookmark_prefix = "xx_"),
                   c("xx_my_file_1_2", "xx_bk_1abc", paste0("xx_", substr(long, 1, 32))))
})

test_that("FT-AS3012 [FR-AS135]: each section starts, after its header / footer preamble, with an invisible outline paragraph carrying its entry label or its file name", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_demo(file.path(d, "one.rtf"), "Table 1 A")
  f2 <- .g2_demo(file.path(d, "two.rtf"), "Table 2 B")
  f3 <- .g2_demo(file.path(d, "three.rtf"), "Table 3 C")
  out <- file.path(d, "out.rtf")
  tb <- data.frame(file = c(f1, f2), label = c("Entry one", "Entry two"),
                   stringsAsFactors = FALSE)
  assemble_rtf(c(f1, f2, f3), out, toc = tb)
  l <- .g2_lines(out)
  sect <- which(trimws(l) == "\\sectd")
  expect_length(sect, 3L)
  want <- c("Entry one", "Entry two", "three")
  for (k in 1:3) {
    from <- sect[k]
    to <- if (k < 3) sect[k + 1] else length(l)
    seg <- l[from:to]
    bm <- grep("^\\{\\\\\\*\\\\bkmkstart ", seg)
    expect_length(bm, 1L)
    outline <- sprintf(paste0("{\\pard\\plain\\cf2\\fs2\\sl1\\slmult0",
                              "\\sa0\\sb0\\outlinelevel0 %s\\par}"), want[k])
    expect_identical(seg[bm + 1L], outline)
    # after the page set-up, header and footer of the section
    expect_true(any(grepl("^\\{\\\\header", seg[seq_len(bm)])))
    expect_true(any(grepl("^\\{\\\\footer", seg[seq_len(bm)])))
    expect_true(any(grepl("^\\\\sbkpage", seg[seq_len(bm)])))
  }
  # white (colour 2 of the table) and 1 twip high: the paragraph cannot be seen
  expect_true(any(startsWith(l, "{\\colortbl;\\red0\\green0\\blue0;\\red255\\green255\\blue255;")))
})

test_that("FT-AS3013 [FR-AS140]: inputs without page tokens are unchanged apart from the cover / TOC; with a cover alone every NUMPAGES cache is the pages plus one", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  g1 <- .g2_fake(file.path(d, "a.rtf"), "A")
  g2 <- .g2_fake(file.path(d, "b.rtf"), "B")
  out <- file.path(d, "out.rtf")
  assemble_rtf(c(g1, g2), out, toc = c("a", "b"), cover = list(title = "T"))
  l <- .g2_lines(out)
  expect_false(any(grepl("NUMPAGES", l, fixed = TRUE)))
  expect_true(all(c("{\\rtf1\\ansi", "{\\fonttbl}", "BODY-A", "BODY-B") %in% l))
  expect_identical(sum(l == "\\sectd"), 2L)

  f1 <- .g2_demo(file.path(d, "c.rtf"), "Table 1 A")
  f2 <- .g2_demo(file.path(d, "d.rtf"), "Table 2 B")
  out2 <- file.path(d, "out2.rtf")
  assemble_rtf(c(f1, f2), out2, cover = list(title = "T"))
  txt <- .g2_txt(out2)
  caches <- regmatches(txt, gregexpr("NUMPAGES\\}\\{\\\\fldrslt [0-9]+", txt))[[1]]
  expect_gte(length(caches), 2L)
  # two one-page inputs plus the cover page
  expect_true(all(grepl("fldrslt 3$", caches)))

  # a table of contents alone adds one page, a cover and a table of contents two, and
  # neither adds none; the field instruction is as the input wrote it
  cache_of <- function(...) {
    o <- file.path(d, "o.rtf")
    assemble_rtf(c(f1, f2), o, overwrite = TRUE, ...)
    t <- .g2_txt(o)
    list(n = length(gregexpr("NUMPAGES}{\\fldrslt ", t, fixed = TRUE)[[1]]),
         v = unique(sub("^.*fldrslt ", "", regmatches(
           t, gregexpr("NUMPAGES\\}\\{\\\\fldrslt [0-9]+", t))[[1]])))
  }
  expect_identical(cache_of()$v, "2")
  expect_identical(cache_of(toc = "auto")$v, "3")
  expect_identical(cache_of(toc = "auto", cover = list(title = "T"))$v, "4")
  expect_identical(cache_of(toc = "auto")$n, cache_of()$n)
})

# ---- assemble_folder --------------------------------------------------------

# A file whose running header has the centred cells given (one row each).
.g2_hdr_file <- function(path, cells) {
  rows <- vapply(cells, function(x) {
    paste0("\\trowd\\pard\\intbl\\qc\\li0\\ri0 ", x, "\\cell\\row")
  }, character(1L), USE.NAMES = FALSE)
  writeLines(c("{\\rtf1\\ansi", paste0("{\\header ", paste(rows, collapse = " "), "}"),
               "\\sectd", "BODY", "}"), path)
  path
}

test_that("FT-AS3014 [FR-AS149]: the table assemble_folder() returns has heading NA in every row", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  .g2_demo(file.path(d, "a.rtf"), "Table 1 A")
  .g2_demo(file.path(d, "b.rtf"), "Table 2 B")
  sp <- assemble_folder(d)
  expect_identical(sp$heading, c(NA_character_, NA_character_))
  expect_identical(sp$level, c(2L, 2L))

  # the other columns: the number of the "Table N" line (of the running header, else of
  # the title block) is table, NA when there is none; the label is "Table N  <title>",
  # or the title alone, or the file name without .rtf; pages is the page count
  .g2_hdr_file(file.path(d, "h.rtf"), c("Table 14.1.1", "Demographics"))
  .g2_demo(file.path(d, "c_plain.rtf"), NULL)
  sp <- assemble_folder(d)
  expect_identical(names(sp), c("order", "file", "table", "heading", "label", "level", "pages"))
  # by table number (1, 2, 14.1.1), then the file without one
  expect_identical(basename(sp$file), c("a.rtf", "b.rtf", "h.rtf", "c_plain.rtf"))
  expect_identical(sp$table, c("1", "2", "14.1.1", NA))
  expect_identical(sp$label, c("Table 1  A", "Table 2  B", "Table 14.1.1  Demographics",
                               "c_plain"))
  expect_identical(sp$heading, rep(NA_character_, 4L))
  expect_identical(sp$level, rep(2L, 4L))
  expect_identical(sp$pages[c(1, 2, 4)], rep(1L, 3L))
})

test_that("FT-AS3015 [FR-AS150]: the title is the longest centred header cell that is not the Table N line or a <...> subtitle, decoded (signed / unsigned, surrogate pairs, no-break space), or the rest of the Table N line", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  .g2_hdr_file(file.path(d, "a.rtf"), c(
    "Table 14.1.2", "Short title", "<A subtitle that is much longer than the others>",
    "Longest \\u181?g/mL \\u-10179?\\u-8704? \\u55357?\\u56832? a\\u160?b"))
  .g2_hdr_file(file.path(d, "b.rtf"), "Table 14.1.3 Inline title")
  sp <- assemble_folder(d)
  smile <- "\U0001F600"
  expect_identical(sp$table, c("14.1.2", "14.1.3"))
  expect_identical(sp$label[1],
                   paste0("Table 14.1.2  Longest \u00b5g/mL ", smile, " ", smile, " a b"))
  expect_identical(sp$label[2], "Table 14.1.3  Inline title")
})

test_that("FT-AS3016 [FR-AS153]: no spec_file writes no file; a spec_file is written before assembling and holds the returned table", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  .g2_demo(file.path(d, "a.rtf"), "Table 1 A")
  .g2_demo(file.path(d, "b.rtf"), "Table 2 B")
  before <- list.files(d)
  sp <- assemble_folder(d)
  expect_identical(list.files(d), before)

  w <- .g2_dir()
  out <- file.path(w, "book.rtf")
  writeLines("occupied", out)
  csv <- file.path(w, "spec.csv")
  expect_error(assemble_folder(d, out, spec_file = csv), "already exists")
  expect_true(file.exists(csv))
  got <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_identical(got$file, sp$file)
  expect_identical(got$label, sp$label)
  expect_identical(got$level, sp$level)
  expect_identical(got$pages, sp$pages)
  expect_identical(as.character(got$table), sp$table)

  # an .xlsx path is saved the same way (it needs writexl and readxl here)
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  xl <- file.path(w, "spec.xlsx")
  expect_error(assemble_folder(d, out, spec_file = xl), "already exists")
  expect_true(file.exists(xl))
  gx <- as.data.frame(readxl::read_xlsx(xl))
  expect_identical(gx$file, sp$file)
  expect_identical(gx$label, sp$label)
})

test_that("FT-AS3017 [FR-AS154]: an .xlsx spec_file without writexl stops with its message", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  .g2_demo(file.path(d, "a.rtf"), "Table 1 A")
  .g2_demo(file.path(d, "b.rtf"), "Table 2 B")
  local({
    local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
    expect_error(
      assemble_folder(d, spec_file = file.path(d, "x.xlsx")),
      "Writing an .xlsx spec needs the 'writexl' package; install it or use a .csv path.",
      fixed = TRUE)
  })
  expect_false(file.exists(file.path(d, "x.xlsx")))
  # any other extension stops, and writes nothing
  expect_error(assemble_folder(d, spec_file = file.path(d, "toc.txt")),
               "`spec_file` must end in .xlsx or .csv.", fixed = TRUE)
  expect_false(file.exists(file.path(d, "toc.txt")))
})

test_that("FT-AS3018 [FR-AS157]: assemble_folder() defaults to the title Table of Contents, a dot leader, decimal page numbering and overwrite = FALSE", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  .g2_demo(file.path(d, "a.rtf"), "Table 1 A")
  .g2_demo(file.path(d, "b.rtf"), "Table 2 B")
  w <- .g2_dir()
  out <- file.path(w, "out.rtf")
  assemble_folder(d, out)
  l <- .g2_lines(out)
  expect_true("{\\pard\\qc\\b\\fs28 Table of Contents\\par}" %in% l)
  expect_true(any(grepl("\\tqr\\tldot\\tx14112", l, fixed = TRUE)))
  expect_true("\\sectd\\sbkpage\\lndscpsxn\\pgnrestart\\pgndec" %in% l)
  expect_error(assemble_folder(d, out), "already exists")

  # the arguments are those of assemble_rtf(): a given value is used
  out2 <- file.path(w, "out2.rtf")
  assemble_folder(d, out2, toc_title = "Contents", toc_leader = "none",
                  toc_page_numbering = "roman")
  l2 <- .g2_lines(out2)
  expect_true("{\\pard\\qc\\b\\fs28 Contents\\par}" %in% l2)
  expect_false(any(grepl("\\tldot", l2, fixed = TRUE)))
  expect_true("\\sectd\\sbkpage\\lndscpsxn\\pgnrestart\\pgnlcrm" %in% l2)
  # overwrite = TRUE replaces the file
  expect_no_error(assemble_folder(d, out, overwrite = TRUE))
})

test_that("FT-AS3019 [FR-AS158]: assemble_folder() passes bookmark_prefix and book_page to assemble_rtf()", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  slot <- function(path, t) {
    doc <- rtf_document()
    doc <- rtf_section(doc, page = 1, secinfo = list(
      footer = rtf_footer(rows = list(c(c = "{BOOK_PAGE}")))))
    doc <- rtf_tables(doc, list(data.frame(A = 1L)), titles = list(c(t)))
    generate_rtfreport(doc, path, overwrite = TRUE)
  }
  slot(file.path(d, "a.rtf"), "Table 1 A")
  slot(file.path(d, "b.rtf"), "Table 2 B")
  w <- .g2_dir()
  out <- file.path(w, "out.rtf")
  assemble_folder(d, out, bookmark_prefix = "xx_", book_page = "Book {AUTO_PAGE}")
  txt <- .g2_txt(out)
  starts <- regmatches(txt, gregexpr("bkmkstart [A-Za-z0-9_]+", txt))[[1]]
  expect_identical(starts, c("bkmkstart xx_a", "bkmkstart xx_b"))
  expect_false(grepl(rtfreporter:::.load_rtf_commands()$fields$book_page_slot, txt,
                     fixed = TRUE))
  expect_identical(lengths(regmatches(txt, gregexpr("Book \\\\chpgn", txt))), 2L)
  # and cover
  out2 <- file.path(w, "out2.rtf")
  assemble_folder(d, out2, cover = list(title = "CVR"))
  expect_true("{\\pard\\qc\\b\\fs44 CVR\\par}" %in% .g2_lines(out2))
})

test_that("FT-AS3020 [FR-AS159]: a table from assemble_folder(), edited, gives the new label, the new indent, no dropped file and the new order", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  .g2_demo(file.path(d, "a.rtf"), "Table 1 Alpha")
  .g2_demo(file.path(d, "b.rtf"), "Table 2 Beta")
  .g2_demo(file.path(d, "c.rtf"), "Table 3 Gamma")
  sp <- assemble_folder(d)
  expect_identical(sp$label, c("Table 1  Alpha", "Table 2  Beta", "Table 3  Gamma"))
  ed <- sp[c(1, 3), ]                          # a row dropped
  ed$label[1] <- "Edited first"                # a label changed
  ed$level[1] <- 3L                            # a level changed
  ed$order <- c(2L, 1L)                        # reordered: Gamma first
  ed$heading <- c("Group A", "Group A")        # a heading set (printed once)
  w <- .g2_dir()
  out <- file.path(w, "out.rtf")
  assemble_rtf(toc = ed, output_file = out)
  l <- .g2_lines(out)
  ent <- l[grepl("HYPERLINK", l, fixed = TRUE)]
  expect_length(ent, 2L)
  expect_true(startsWith(ent[1], "{\\pard\\li600\\fs20"))
  expect_true(grepl("{\\fldrslt Table 3  Gamma}}", ent[1], fixed = TRUE))
  expect_true(startsWith(ent[2], "{\\pard\\li1200\\fs20"))
  expect_true(grepl("{\\fldrslt Edited first}}", ent[2], fixed = TRUE))
  txt <- paste(l, collapse = "\n")
  expect_identical(sum(l == "{\\pard\\b\\fs22 Group A\\par}"), 1L)
  expect_false(grepl("Beta", txt, fixed = TRUE))
  expect_lt(regexpr("Gamma", sub(".*Table of Contents", "", txt)) [1],
            regexpr("Alpha", sub(".*Table of Contents", "", txt))[1])
})

test_that("FT-AS3021 [FR-AS250]: a contents entry keeps the text of markup (H_{2}O) without its formatting and the decoded non-ASCII text, for toc = \"auto\" and assemble_folder()", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  .g2_demo(file.path(d, "a.rtf"), "Table 1 H_{2}O at \u00b5g/mL")
  .g2_demo(file.path(d, "b.rtf"), "Table 2 Plain")
  sp <- assemble_folder(d)
  expect_identical(sp$label[1], "Table 1  H2O at \u00b5g/mL")
  # an assemble_folder() label comes from the header or title block alike
  w <- .g2_dir()
  out <- file.path(w, "out.rtf")
  assemble_rtf(file.path(d, c("a.rtf", "b.rtf")), out, toc = "auto")
  l <- .g2_lines(out)
  ent <- l[grepl("HYPERLINK", l, fixed = TRUE)]
  expect_true(grepl("{\\fldrslt Table 1 H2O at \\u181?g/mL}}", ent[1], fixed = TRUE))
  expect_false(grepl("\\sub", ent[1], fixed = TRUE))
})

# ---- rtf_figures / rtfplot --------------------------------------------------

# A PNG of known pixel size and resolution (res in dpi), drawn by the png device.
.g2_png <- function(dir, name = "p.png", w = 400, h = 300, res = 100) {
  f <- file.path(dir, name)
  grDevices::png(f, width = w, height = h, res = res)
  .g2_pl()
  grDevices::dev.off()
  f
}

.g2_bytes <- function(f) readBin(f, "raw", file.size(f))

# contents of a document as a plain list of blocks
.g2_blocks <- function(doc) unclass(doc$contents)

test_that("FT-AS3022 [FR-AS179]: rtf_figures() appends one block per figure, in the order given, after the document's content, each with its own page flag", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_png(d, "f1.png", 100, 100)
  f2 <- .g2_png(d, "f2.png", 200, 100)
  f3 <- .g2_png(d, "f3.png", 300, 100)
  doc <- rtf_document() |> rtf_tables(list(data.frame(A = 1)))
  doc <- rtf_figures(doc, list(f1, f2, f3), page_break = c(TRUE, FALSE, TRUE))
  b <- .g2_blocks(doc)
  expect_length(b, 4L)
  expect_s3_class(b[[1]]$content, "rtftable")
  expect_identical(lapply(b[2:4], function(x) x$content$path), list(f1, f2, f3))
  expect_identical(vapply(b[2:4], function(x) x$content$img_width, 1L),
                   c(100L, 200L, 300L))
  expect_identical(vapply(b, function(x) x$page_break, NA), c(TRUE, TRUE, FALSE, TRUE))
})

test_that("FT-AS3023 [FR-AS180]: a single path, a single drawing function, a plot object each give one block", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .g2_png(d)
  b1 <- .g2_blocks(rtf_figures(rtf_document(), f))
  expect_length(b1, 1L)
  expect_s3_class(b1[[1]]$content, "rtfplot")
  expect_identical(b1[[1]]$content$path, f)
  b2 <- .g2_blocks(rtf_figures(rtf_document(), .g2_pl,
                               render_width = 2, render_height = 1, render_dpi = 50))
  expect_length(b2, 1L)
  expect_s3_class(b2[[1]]$content, "rtfplot")
  expect_identical(c(b2[[1]]$content$img_width, b2[[1]]$content$img_height), c(100L, 50L))
  # an rtfplot is one figure, kept as it is; a list is one figure for each element
  rp <- rtfplot(f)
  b3 <- .g2_blocks(rtf_figures(rtf_document(), rp))
  expect_length(b3, 1L)
  expect_identical(b3[[1]]$content, rp)
  b4 <- .g2_blocks(rtf_figures(rtf_document(), list(f, .g2_pl, rp),
                               render_width = 2, render_height = 1, render_dpi = 50))
  expect_length(b4, 3L)
  # a ggplot2 plot is one figure, not a list of its parts
  skip_if_not_installed("ggplot2")
  g <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  b5 <- .g2_blocks(rtf_figures(rtf_document(), g,
                               render_width = 2, render_height = 1, render_dpi = 50))
  expect_length(b5, 1L)
  expect_s3_class(b5[[1]]$content, "rtfplot")
})

test_that("FT-AS3024 [FR-AS181]: a PNG path, a JPEG path, a patchwork, a lattice object, a gtable and a recorded base plot are each one figure", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("lattice")
  skip_if_not_installed("gtable")
  skip_if_not(isTRUE(unname(capabilities("jpeg"))))
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  png_f <- .g2_png(d)
  jpg <- file.path(d, "p.jpg")
  grDevices::jpeg(jpg, width = 200, height = 100)
  .g2_pl()
  grDevices::dev.off()
  g <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  pw <- patchwork::wrap_plots(g, g)
  lat <- lattice::xyplot(y ~ x, data.frame(x = 1:3, y = 1:3))
  gt <- gtable::gtable_col("g", list(grid::rectGrob()), grid::unit(1, "null"), grid::unit(1, "null"))
  grDevices::png(tempfile(fileext = ".png"))
  .g2_pl()
  rec <- grDevices::recordPlot()
  grDevices::dev.off()
  figs <- list(png = png_f, jpeg = jpg, patchwork = pw, lattice = lat, gtable = gt,
               recorded = rec, ggplot = g, grob = grid::rectGrob(), `function` = .g2_pl,
               rtfplot = rtfplot(png_f))
  for (nm in names(figs)) {
    b <- .g2_blocks(rtf_figures(rtf_document(), figs[[nm]],
                                render_width = 2, render_height = 1, render_dpi = 50))
    expect_length(b, 1L)
    expect_s3_class(b[[1]]$content, "rtfplot")
    expect_identical(b[[1]]$content$img_type, if (nm == "jpeg") "jpeg" else "png")
  }
  # and all together in a list: one block each, in order
  b <- .g2_blocks(rtf_figures(rtf_document(), unname(figs),
                              render_width = 2, render_height = 1, render_dpi = 50))
  expect_length(b, 10L)
  expect_identical(b[[1]]$content$path, png_f)
  expect_identical(b[[2]]$content$path, jpg)
})

test_that("FT-AS3025 [FR-AS185]: width_twips and height_twips set the display size of a figure; NULL is the native size of a file and render_width x render_height of a plot object", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .g2_png(d, "a.png", 400, 300, res = 100)   # 4 x 3 inches
  rtf_of <- function(...) {
    .render_to_string(rtf_figures(rtf_document(), ...))
  }
  txt <- rtf_of(f, width_twips = 2880, height_twips = 1440)
  expect_true(grepl("\\picwgoal2880\\pichgoal1440", txt, fixed = TRUE))
  # native size: 4 x 3 in at its embedded 100 dpi
  txt <- rtf_of(f)
  expect_true(grepl("\\picwgoal5760\\pichgoal4320", txt, fixed = TRUE))
  # a plot object: its render_width x render_height (3 x 2 in)
  txt <- rtf_of(.g2_pl, render_width = 3, render_height = 2, render_dpi = 100)
  expect_true(grepl("\\picwgoal4320\\pichgoal2880", txt, fixed = TRUE))
  # given sizes do not reach an rtfplot() already made
  rp <- rtfplot(f, width_twips = 1440L, height_twips = 720L)
  txt <- rtf_of(rp, width_twips = 2880, height_twips = 1440)
  expect_true(grepl("\\picwgoal1440\\pichgoal720", txt, fixed = TRUE))
  # no fit to the page: a 10 x 1 in file (1000 x 100 px at 100 dpi) is 14400 x 1440 twips,
  # wider than the 9360 twips of the text area
  wide <- .g2_png(d, "wide.png", 1000, 100, res = 100)
  txt <- rtf_of(wide)
  expect_true(grepl("\\picwgoal14400\\pichgoal1440", txt, fixed = TRUE))
})

test_that("FT-AS3026 [FR-AS186]: rtf_figures(align) is center, left or right; another value stops as rtfplot() does", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .g2_png(d)
  msg <- "`align` must be 'left', 'center', or 'right'."
  expect_error(rtfplot(f, align = "middle"), msg, fixed = TRUE)
  expect_error(rtf_figures(rtf_document(), f, align = "middle"), msg, fixed = TRUE)
  expect_identical(.g2_blocks(rtf_figures(rtf_document(), f))[[1]]$content$align, "center")
  expect_identical(.g2_blocks(rtf_figures(rtf_document(), f, align = "left"))[[1]]$content$align,
                   "left")
  expect_identical(.g2_blocks(rtf_figures(rtf_document(), f, align = "right"))[[1]]$content$align,
                   "right")
})

test_that("FT-AS3027 [FR-AS187]: titles and footnotes are NULL, one block for every figure, or one block per figure", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .g2_png(d, "f1.png")
  f2 <- .g2_png(d, "f2.png")
  ttl <- function(...) lapply(.g2_blocks(rtf_figures(rtf_document(), list(f1, f2), ...)),
                              function(b) b$title)
  fnt <- function(...) lapply(.g2_blocks(rtf_figures(rtf_document(), list(f1, f2), ...)),
                              function(b) b$footnote)
  expect_identical(ttl(), list(NULL, NULL))
  expect_identical(ttl(titles = list("a", "b")), list("a", "b"))
  expect_identical(ttl(titles = list(c("a", "b"))), list(c("a", "b"), c("a", "b")))
  expect_identical(ttl(titles = c("a", "b")), list(c("a", "b"), c("a", "b")))
  expect_identical(fnt(footnotes = "x"), list("x", "x"))
  expect_identical(fnt(footnotes = list("x", "y")), list("x", "y"))
  expect_identical(fnt(footnotes = NULL), list(NULL, NULL))
})

test_that("FT-AS3028 [FR-AS189]: page_break TRUE or FALSE for every figure or one each; the document's first content always begins a page", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  a <- rtfplot(.g2_png(d, "a.png"), width_twips = 2000L)
  b <- rtfplot(.g2_png(d, "b.png"), width_twips = 2000L)
  flags <- function(pb) {
    doc <- rtf_figures(rtf_document(), list(a, b), page_break = pb)
    list(flags = vapply(.g2_blocks(doc), function(x) x$page_break, NA),
         pages = length(.pipe_doc_to_rtfreport(doc)$pages))
  }
  t1 <- flags(TRUE)
  expect_identical(t1$flags, c(TRUE, TRUE))
  expect_identical(t1$pages, 2L)
  f1 <- flags(FALSE)
  expect_identical(f1$flags, c(FALSE, FALSE))
  expect_identical(f1$pages, 1L)
  expect_identical(flags(c(FALSE, FALSE))$flags, f1$flags)
  e1 <- flags(c(TRUE, FALSE))
  expect_identical(e1$flags, c(TRUE, FALSE))
  expect_identical(e1$pages, 1L)
  e2 <- flags(c(FALSE, TRUE))
  expect_identical(e2$flags, c(FALSE, TRUE))
  # the first content begins a page whatever its flag: the second one's own flag
  # decides whether there is a second page
  expect_identical(e2$pages, 2L)
})

test_that("FT-AS3029 [FR-AS190]: a page_break that is not TRUE or FALSE, or not of length 1 or the number of figures, stops with the full message", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .g2_png(d)
  expect_error(
    rtf_figures(rtf_document(), f, page_break = "yes"),
    "rtf_figures(): `page_break` is TRUE or FALSE, one for every page or one each (1).",
    fixed = TRUE)
  expect_error(
    rtf_figures(rtf_document(), list(f, f), page_break = c(TRUE, FALSE, TRUE)),
    "rtf_figures(): `page_break` is TRUE or FALSE, one for every page or one each (2).",
    fixed = TRUE)
  expect_error(
    rtf_figures(rtf_document(), list(f, f), page_break = NA),
    "rtf_figures(): `page_break` is TRUE or FALSE, one for every page or one each (2).",
    fixed = TRUE)
})

test_that("FT-AS3030 [FR-AS194]: png_device = NULL takes the option rtfreporter.png_device; \"auto\" resolves", {
  skip_if_not_installed("ragg")
  old <- options(rtfreporter.png_device = "png")
  on.exit(options(old), add = TRUE)
  fn <- .g2_pl
  draw <- function(...) {
    b <- .g2_blocks(rtf_figures(rtf_document(), fn, render_width = 2, render_height = 1,
                                render_dpi = 50, ...))
    .g2_bytes(b[[1]]$content$path)
  }
  by_png <- draw()
  options(rtfreporter.png_device = "ragg")
  by_ragg <- draw()
  expect_false(identical(by_png, by_ragg))
  # the argument over the option
  expect_identical(draw(png_device = "png"), by_png)
  options(rtfreporter.png_device = "png")
  expect_identical(draw(png_device = "ragg"), by_ragg)
  # "auto" takes ragg when it is installed
  expect_identical(draw(png_device = "auto"), by_ragg)
})

test_that("FT-AS3031 [FR-AS195]: a render_* value rtfplot() refuses stops with the number of the figure", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .g2_png(d)
  fn <- .g2_pl
  expect_error(rtf_figures(rtf_document(), fn, render_width = -1), "Item 1: `render_width`")
  expect_error(rtf_figures(rtf_document(), fn, render_height = 0), "Item 1: `render_height`")
  expect_error(rtf_figures(rtf_document(), fn, render_dpi = NA_real_),
               "Item 1: `render_dpi` must be a single positive number.", fixed = TRUE)
  expect_error(rtf_figures(rtf_document(), fn, png_device = "svg"), "Item 1: `png_device`")
  # a file takes none of them, so the second figure is the one refused
  expect_error(rtf_figures(rtf_document(), list(f, fn), render_width = -1),
               "Item 2: `render_width`")
  expect_error(rtf_figures(rtf_document(), list(fn, fn), render_height = -1),
               "Item 1: `render_height`")
})

test_that("FT-AS3032 [FR-AS197]: rtfplot() reads an upper-case .PNG's pixel size and a JPEG's JFIF density in dots per centimetre as dots per inch", {
  skip_if_not(isTRUE(unname(capabilities("jpeg"))))
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .g2_png(d, "a.png", 400, 300, res = 100)
  up <- file.path(d, "A.PNG")
  file.copy(f, up)
  rp <- rtfplot(up)
  expect_identical(rp$img_type, "png")
  expect_identical(c(rp$img_width, rp$img_height), c(400L, 300L))
  expect_equal(c(rp$dpi_x, rp$dpi_y), c(100, 100), tolerance = 1e-3)

  jpg <- file.path(d, "p.jpg")
  grDevices::jpeg(jpg, width = 200, height = 100)
  .g2_pl()
  grDevices::dev.off()
  raw <- .g2_bytes(jpg)
  # SOI, then the JFIF APP0: marker (2), length (2), "JFIF\0", version (2),
  # units (1), x density (2), y density (2)
  expect_identical(raw[3:4], as.raw(c(0xFF, 0xE0)))
  expect_identical(rawToChar(raw[7:10]), "JFIF")
  raw[14] <- as.raw(2L)                        # dots per centimetre
  raw[15:16] <- as.raw(c(0x00, 0x76))          # 118
  raw[17:18] <- as.raw(c(0x00, 0x78))          # 120
  jpg2 <- file.path(d, "q.JPG")
  writeBin(raw, jpg2)
  rj <- rtfplot(jpg2)
  expect_identical(rj$img_type, "jpeg")
  expect_identical(c(rj$img_width, rj$img_height), c(200L, 100L))
  expect_equal(c(rj$dpi_x, rj$dpi_y), c(118 * 2.54, 120 * 2.54))

  # units 1: dots per inch as they are, in a file with the extension .jpeg
  raw[14] <- as.raw(1L)
  raw[15:16] <- as.raw(c(0x00, 0x60))            # 96
  raw[17:18] <- as.raw(c(0x00, 0x48))            # 72
  jpg3 <- file.path(d, "r.jpeg")
  writeBin(raw, jpg3)
  r3 <- rtfplot(jpg3)
  expect_identical(r3$img_type, "jpeg")
  expect_equal(c(r3$dpi_x, r3$dpi_y), c(96, 72))
})

test_that("FT-AS3033 [FR-AS198]: a ggplot, a lattice object, a patchwork and a gtable are each drawn into an image of render_width x render_height", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("lattice")
  skip_if_not_installed("gtable")
  df <- data.frame(x = 1:3, y = 1:3)
  g <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point()
  objs <- list(
    ggplot = g,
    lattice = lattice::xyplot(y ~ x, df),
    patchwork = patchwork::wrap_plots(g, g),
    gtable = gtable::gtable_col("g", list(grid::rectGrob()), grid::unit(1, "null"), grid::unit(1, "null"))
  )
  # a grob (grid.draw()), a recorded base plot (replayed) and a function (called)
  grDevices::png(tempfile(fileext = ".png"))
  .g2_pl()
  rec <- grDevices::recordPlot()
  grDevices::dev.off()
  objs <- c(objs, list(grob = grid::rectGrob(), recorded = rec, `function` = .g2_pl))
  for (nm in names(objs)) {
    rp <- rtfplot(objs[[nm]], render_width = 2, render_height = 1, render_dpi = 100)
    info <- paste("object:", nm)
    expect_true(file.exists(rp$path), info = info)
    expect_identical(rp$img_type, "png", info = info)
    expect_identical(c(rp$img_width, rp$img_height), c(200L, 100L), info = info)
    expect_identical(c(rp$dpi_x, rp$dpi_y), c(100, 100), info = info)
    txt <- .render_to_string(rtf_figures(rtf_document(), rp))
    expect_true(grepl("\\picwgoal2880\\pichgoal1440", txt, fixed = TRUE), info = info)
  }
})

test_that("FT-AS3034 [FR-AS203]: a plot object without sizes is drawn 6.5 x 4.5 in: 1950 x 1350 px at 300 dpi, shown 9360 x 6480 twips", {
  rp <- rtfplot(.g2_pl)
  expect_identical(c(rp$img_width, rp$img_height), c(1950L, 1350L))
  expect_identical(c(rp$dpi_x, rp$dpi_y), c(300, 300))
  txt <- .render_to_string(rtf_figures(rtf_document(), rp))
  expect_true(grepl("\\picwgoal9360\\pichgoal6480", txt, fixed = TRUE))
  # given width_twips / height_twips say otherwise
  rp2 <- rtfplot(.g2_pl, width_twips = 4680L, height_twips = 3240L)
  txt2 <- .render_to_string(rtf_figures(rtf_document(), rp2))
  expect_true(grepl("\\picwgoal4680\\pichgoal3240", txt2, fixed = TRUE))
  # render_width x render_height inches: 3 x 2 in at 300 dpi is 900 x 600 px, shown 4320 x 2880
  rp3 <- rtfplot(.g2_pl, render_width = 3, render_height = 2)
  expect_identical(c(rp3$img_width, rp3$img_height), c(900L, 600L))
  txt3 <- .render_to_string(rtf_figures(rtf_document(), rp3))
  expect_true(grepl("\\picwgoal4320\\pichgoal2880", txt3, fixed = TRUE))
})

test_that("FT-AS3035 [FR-AS207]: an option png_device that is not auto, ragg or png stops with the message naming the option", {
  old <- options(rtfreporter.png_device = "svg")
  on.exit(options(old), add = TRUE)
  expect_error(
    rtfplot(.g2_pl),
    "`png_device` (option `rtfreporter.png_device`) must be \"auto\", \"ragg\" or \"png\".",
    fixed = TRUE)
  # the same for the argument
  options(old)
  expect_error(
    rtfplot(.g2_pl, png_device = "svg"),
    "`png_device` (option `rtfreporter.png_device`) must be \"auto\", \"ragg\" or \"png\".",
    fixed = TRUE)
  # "ragg" without the ragg package
  local({
    local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
    expect_error(
      rtfplot(.g2_pl, png_device = "ragg"),
      "`png_device = \"ragg\"` needs the ragg package: install.packages(\"ragg\"), or use \"png\".",
      fixed = TRUE)
  })
})

test_that("FT-AS3036 [FR-AS210]: a figure that is a character vector of other than one element, NA, NULL or another atomic vector stops with the messages", {
  expect_error(rtfplot(NA_character_), "A figure is one file path, or one plot object.",
               fixed = TRUE)
  expect_error(rtfplot(c("a.png", "b.png")), "A figure is one file path, or one plot object.",
               fixed = TRUE)
  expect_error(rtfplot(character(0)), "A figure is one file path, or one plot object.",
               fixed = TRUE)
  expect_error(rtfplot(NULL), "A figure is one file path, or one plot object; got NULL.",
               fixed = TRUE)
  expect_error(rtfplot(1:3),
               "A figure is one file path, or one plot object; got an integer vector.",
               fixed = TRUE)
  expect_error(rtfplot(c(1.5, 2)),
               "A figure is one file path, or one plot object; got a numeric vector.",
               fixed = TRUE)
})

test_that("FT-AS3037 [FR-AS213]: a plot object that fails while drawn leaves no PNG file behind and the error is its own", {
  before <- list.files(tempdir(), pattern = "[.]png$")
  devs <- grDevices::dev.list()
  expect_error(rtfplot(function() stop("boom")), "boom", fixed = TRUE)
  expect_identical(list.files(tempdir(), pattern = "[.]png$"), before)
  expect_identical(grDevices::dev.list(), devs)
})

test_that("FT-AS3038 [FR-AS253]: render_format reaches each plot object, never a file, and an rtfplot() keeps the format it was made with", {
  skip_if_not_installed("devEMF")
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rp <- rtfplot(.g2_png(d, "r.png"))
  fn <- .g2_pl
  b <- .g2_blocks(rtf_figures(rtf_document(), list(rp, fn), render_format = NULL))
  expect_identical(vapply(b, function(x) x$content$img_type, ""), c("png", "png"))
  b <- .g2_blocks(rtf_figures(rtf_document(), list(rp, fn, .g2_png(d, "q.png")),
                              render_format = "emf"))
  expect_identical(vapply(b, function(x) x$content$img_type, ""), c("png", "emf", "png"))
})

# ---- rtfreporter_ai_manual / rtfreporter_validation_kit ---------------------

# An ARD of one analysis variable (AGE) with `n` groups and two statistics (mean, sd)
# each: 2 * n rows once flattened by normalize_ard().
.g2_ard <- function(n, variables = "AGE") {
  df <- data.frame(g = sprintf("G%04d", seq_len(n)), AGE = seq(30, 60, length.out = n),
                   BMI = seq(20, 30, length.out = n), stringsAsFactors = FALSE)
  eval(bquote(cards::ard_continuous(
    df, by = g, variables = .(variables),
    statistic = ~ cards::continuous_summary_fns(c("mean", "sd")))))
}

test_that("FT-AS3039 [FR-AS218]: which = \"dev\" writes the developer manual alone: no kit files and no PASTE.md", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rtfreporter_ai_manual(d, which = "dev")
  expect_identical(list.files(d, all.files = TRUE, no.. = TRUE),
                   "rtfreporter-ai-dev-manual.md")
})

test_that("FT-AS3040 [FR-AS219]: 05_ard.csv holds every normalized row up to 2,000, else each analysis' first 30; 06_ard_shape.md lists each variable's statistics", {
  skip_if_not_installed("cards")
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # 1000 groups x 2 statistics = 2000 rows: every row
  ard <- .g2_ard(1000)
  rtfreporter_ai_manual(d, ard = ard)
  csv <- utils::read.csv(file.path(d, "05_ard.csv"), stringsAsFactors = FALSE)
  expect_identical(nrow(csv), 2000L)
  expect_identical(csv$g[1:4], c("G0001", "G0001", "G0002", "G0002"))
  expect_identical(csv$stat_name[1:2], c("mean", "sd"))
  shape <- readLines(file.path(d, "06_ard_shape.md"), warn = FALSE)
  expect_true("| `AGE` | continuous | mean, sd | 2000 |" %in% shape)
  expect_true(any(grepl("2000 rows, one statistic each", shape, fixed = TRUE)))

  # 1001 groups x 2 statistics = 2002 rows of AGE, as many of BMI: each analysis' first 30
  d2 <- .g2_dir()
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  big <- .g2_ard(1001, c("AGE", "BMI"))
  rtfreporter_ai_manual(d2, ard = big)
  csv2 <- utils::read.csv(file.path(d2, "05_ard.csv"), stringsAsFactors = FALSE)
  expect_identical(nrow(csv2), 60L)
  expect_identical(table(csv2$variable)[c("AGE", "BMI")], table(rep(c("AGE", "BMI"), each = 30L))[c("AGE", "BMI")])
  expect_identical(csv2$g[csv2$variable == "AGE"], rep(sprintf("G%04d", 1:15), each = 2L))
  shape2 <- readLines(file.path(d2, "06_ard_shape.md"), warn = FALSE)
  expect_true("| `AGE` | continuous | mean, sd | 2002 |" %in% shape2)
  expect_true("| `BMI` | continuous | mean, sd | 2002 |" %in% shape2)
})

test_that("FT-AS3041 [FR-AS220]: shell = NULL writes no 07_shell.md and none in PASTE.md", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rtfreporter_ai_manual(d)
  expect_false(file.exists(file.path(d, "07_shell.md")))
  expect_false("07_shell.md" %in% list.files(d))
  heads <- sub("^##### ", "", grep("^##### ", readLines(file.path(d, "PASTE.md"),
                                                         warn = FALSE), value = TRUE))
  expect_false("07_shell.md" %in% heads)
  # a shell: CRLF and trailing tabs and spaces dropped, as 07_shell.md
  f <- file.path(d, "shell.txt")
  writeBin(charToRaw("Table 1  Demo\t\t\r\n  Mean (SD)\t x \t \r\n"), f)
  rtfreporter_ai_manual(d, shell = f, overwrite = TRUE)
  expect_identical(readLines(file.path(d, "07_shell.md"), warn = FALSE),
                   c("Table 1  Demo", "  Mean (SD)\t x"))
})

test_that("FT-AS3042 [FR-AS224]: overwrite = TRUE restores a changed file of the kit; the default keeps it and stops with the overwrite hint", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rtfreporter_ai_manual(d)
  f <- file.path(d, "01_rtfreporter-ai-user-manual.md")
  orig <- readLines(f, warn = FALSE)
  g <- file.path(d, "00_START_HERE.md")
  orig_start <- readLines(g, warn = FALSE)
  writeLines("changed by hand", f)
  writeLines("changed by hand", g)
  expect_error(rtfreporter_ai_manual(d), "pass `overwrite = TRUE` to replace it.", fixed = TRUE)
  expect_identical(readLines(f, warn = FALSE), "changed by hand")
  expect_identical(readLines(g, warn = FALSE), "changed by hand")
  rtfreporter_ai_manual(d, overwrite = TRUE)
  expect_identical(readLines(f, warn = FALSE), orig)
  expect_identical(readLines(g, warn = FALSE), orig_start)
})

test_that("FT-AS3043 [FR-AS226]: a dir that is a file stops with the full message, and a file that cannot be written with could not write", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  fl <- file.path(d, "afile")
  writeLines("x", fl)
  expect_error(rtfreporter_ai_manual(fl),
               sprintf("`rtfreporter_ai_manual()`: '%s' is a file, not a folder.", fl),
               fixed = TRUE)
  expect_error(rtfreporter_ai_manual(c("a", "b")),
               "`rtfreporter_ai_manual(dir = )` is one folder.", fixed = TRUE)
  kit <- file.path(d, "kit")
  local({
    local_mocked_bindings(file.copy = function(from, to, ...) FALSE, .package = "base")
    expect_error(
      rtfreporter_ai_manual(kit),
      sprintf("`rtfreporter_ai_manual()`: could not write '%s'.",
              file.path(kit, "01_rtfreporter-ai-user-manual.md")),
      fixed = TRUE)
  })
})

test_that("FT-AS3044 [FR-AS228]: building the kit calls no network function", {
  skip_if_not_installed("cards")
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  called <- character(0)
  trap <- function(what) function(...) {
    called <<- c(called, what)
    stop("network used: ", what)
  }
  local({
    local_mocked_bindings(url = trap("url"),
                          socketConnection = trap("socketConnection"),
                          curlGetHeaders = trap("curlGetHeaders"),
                          .package = "base")
    local_mocked_bindings(download.file = trap("utils::download.file"),
                          .package = "utils")
    rtfreporter_ai_manual(d, ard = .g2_ard(5), shell = c("Table 1", "性別  n (%)"))
  })
  expect_identical(called, character(0))
  expect_true(file.exists(file.path(d, "PASTE.md")))
})

test_that("FT-AS3045 [FR-AS232]: the validation kit not installed, or a file that cannot be written, stops with the messages", {
  d <- .g2_dir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  kit <- file.path(d, "kit")
  # a folder that is not empty
  writeLines("x", file.path(d, "occupied.txt"))
  expect_error(rtfreporter_validation_kit(d),
               sprintf(paste0("rtfreporter_validation_kit(): '%s' is not empty; give an empty ",
                              "or new folder, or overwrite = TRUE."), d),
               fixed = TRUE)
  local({
    # (the source-loaded package has its own system.file(); an installed one uses base's)
    env <- environment()
    mock <- function(pkg) {
      local_mocked_bindings(system.file = function(...) "", .package = pkg, .env = env)
    }
    tryCatch(mock("rtfreporter"), error = function(e) mock("base"))
    expect_error(rtfreporter_validation_kit(kit),
                 "The validation kit is not installed with this copy of rtfreporter.",
                 fixed = TRUE)
  })
  local({
    local_mocked_bindings(file.copy = function(from, to, ...) rep(FALSE, length(from)),
                          .package = "base")
    expect_error(
      rtfreporter_validation_kit(kit),
      paste0("rtfreporter_validation_kit(): could not write build_user_matrix.R, ",
             "uat_cases.csv, uat_cases.xlsx, urs.csv, urs.md, validation_plan.md, ",
             "validation_summary_report.md, quality-plan.md, requirements.csv to '", kit, "'."),
      fixed = TRUE)
  })
})

# ---- rtf_document / rtf_config / rtf_section / rtf_watermark ----------------

.g2_rows <- function(txt) {
  regmatches(txt, gregexpr("\\\\trowd.*?\\\\row(?![A-Za-z])", txt, perl = TRUE))[[1]]
}

# The border sides each cell of a row has, as letters (t, b, l, r) per cell.
.g2_sides <- function(row) {
  defs <- regmatches(row, gregexpr("(?:\\\\(?:cl|brdr)[A-Za-z0-9]*)*\\\\cellx[0-9]+", row,
                                   perl = TRUE))[[1]]
  vapply(defs, function(m) {
    paste0(c(if (grepl("\\clbrdrt", m, fixed = TRUE)) "t",
             if (grepl("\\clbrdrb", m, fixed = TRUE)) "b",
             if (grepl("\\clbrdrl", m, fixed = TRUE)) "l",
             if (grepl("\\clbrdrr", m, fixed = TRUE)) "r"), collapse = "")
  }, "", USE.NAMES = FALSE)
}

# The cell right edges (\cellxN) of a row
.g2_cellx <- function(row) {
  as.integer(sub("^\\\\cellx", "", regmatches(row, gregexpr("\\\\cellx[0-9]+", row))[[1]]))
}

# The rows of a rendered document that hold `text`
.g2_rows_with <- function(txt, text) {
  r <- .g2_rows(txt)
  r[grepl(text, r, fixed = TRUE)]
}

# The text of the {\header ...} group of each section, in order
.g2_headers <- function(txt) {
  secs <- strsplit(txt, "\n\\sectd\n", fixed = TRUE)[[1]][-1]
  vapply(secs, rtfreporter:::.rtf_header_block, "", USE.NAMES = FALSE)
}

.g2_tb <- function() data.frame(A = c("1", "2"), B = c("x", "y"), stringsAsFactors = FALSE)

test_that("FT-DC3000 [FR-DC101]: rtf_document() has no content and no section, the colour table c(\"#000000\") and writes Courier New on a landscape Letter page", {
  doc <- rtf_document()
  expect_s3_class(doc, "rtf_document")
  expect_identical(names(doc), c("document", "contents", "sections"))
  expect_length(doc$contents, 0L)
  expect_length(doc$sections, 0L)
  expect_identical(doc$document$color_table, c("#000000"))
  txt <- .render_to_string(rtf_tables(doc, .g2_tb()))
  expect_true(grepl("{\\f0\\fnil\\fcharset0 Courier New;}", txt, fixed = TRUE))
  expect_true(grepl("\\paperw15840\\paperh12240\\landscape", txt, fixed = TRUE))
  expect_true(grepl("{\\colortbl;\\red0\\green0\\blue0;\\red255\\green255\\blue255;}", txt,
                    fixed = TRUE))
})

test_that("FT-DC3001 [FR-DC105]: a declared colour is in \\colortbl after black and white; colours of borders and cells are added after it; black and white are not repeated", {
  tbl <- rtftable(.g2_tb(), col_spec = list(list(col = 2, color = "#FF0000"),
                                            list(col = 1, color = "#1F4E79")))
  hdr <- rtf_header(rows = list(c(l = "H")),
                    border = rtf_border(bottom = rtf_border_line("single", 15, color = "#00AA00")))
  doc <- rtf_document(color_table = c("#000000", "#1F4E79")) |>
    rtf_section(page = 1, secinfo = list(header = hdr, footer = NULL)) |>
    rtf_tables(tbl)
  txt <- .render_to_string(doc)
  pal <- regmatches(txt, regexpr("\\{\\\\colortbl[^}]*\\}", txt))
  expect_identical(
    pal,
    paste0("{\\colortbl;\\red0\\green0\\blue0;\\red255\\green255\\blue255;",
           "\\red31\\green78\\blue121;\\red0\\green170\\blue0;\\red255\\green0\\blue0;}"))
  # the declared colour is used by its index (the third entry), a colour of a cell later
  expect_true(grepl("\\cf3 1\\cf1 \\cell", txt, fixed = TRUE))
  expect_true(grepl("\\cf5 x\\cf1 \\cell", txt, fixed = TRUE))
  expect_true(grepl("\\brdrcf4", txt, fixed = TRUE))
})

test_that("FT-DC3002 [FR-DC106]: page = NULL is landscape Letter with 0.75 in margins from the options; rtf_page() and a partial list set the page, a key left out resolved at write", {
  txt <- .render_to_string(rtf_tables(rtf_document(), .g2_tb()))
  expect_true(grepl("\\paperw15840\\paperh12240\\landscape\\margl1080\\margr1080\\margt1080\\margb1080",
                    txt, fixed = TRUE))
  old <- options(rtfreporter.page.margin_left_in = 1, rtfreporter.page.paper_size = "a4")
  on.exit(options(old), add = TRUE)
  txt <- .render_to_string(rtf_tables(rtf_document(), .g2_tb()))
  expect_true(grepl("\\paperw16838\\paperh11905\\landscape\\margl1440\\margr1080", txt,
                    fixed = TRUE))
  options(old)
  txt <- .render_to_string(rtf_tables(rtf_document(page = rtf_page(paper_size = "A4")), .g2_tb()))
  expect_true(grepl("\\paperw16838\\paperh11905\\landscape\\margl1080", txt, fixed = TRUE))
  # a partial list is kept as given; the keys left out are those of the options
  part <- rtf_document(page = list(orientation = "portrait"))
  expect_identical(part$document$page, list(orientation = "portrait"))
  txt <- .render_to_string(rtf_tables(part, .g2_tb()))
  expect_true(grepl("\\paperw12240\\paperh15840", txt, fixed = TRUE))
  expect_false(grepl("\\landscape", txt, fixed = TRUE))
  expect_true(grepl("\\margl1080\\margr1080\\margt1080\\margb1080", txt, fixed = TRUE))
})

test_that("FT-DC3003 [FR-DC109]: watermark = an rtf_watermark() writes one shape with its text, a bare string is the same with the defaults, NULL, NA and an empty string write none", {
  count <- function(txt) lengths(regmatches(txt, gregexpr("shpinst", txt, fixed = TRUE)))
  render <- function(wm) .render_to_string(rtf_tables(rtf_document(watermark = wm), .g2_tb()))
  one <- render(rtf_watermark("DRAFT"))
  expect_identical(count(one), 1L)
  expect_true(grepl("{\\sn gtextUNICODE}{\\sv DRAFT}", one, fixed = TRUE))
  expect_identical(render("DRAFT"), one)
  expect_identical(count(render(NULL)), 0L)
  expect_identical(count(render(NA)), 0L)
  expect_identical(count(render("")), 0L)
  expect_identical(count(.render_to_string(rtf_tables(rtf_document(), .g2_tb()))), 0L)
})

test_that("FT-DC3004 [FR-DC111]: the program of rtf_document(program = ) fills {PROGRAM}, {PROGRAM_FULL}, {PROGRAM_NAME} and {PROGRAM_DIR}; NULL takes the option rtfreporter.program, else the script Rscript runs", {
  mk <- function(...) {
    rtf_document(...) |>
      rtf_section(page = 1, secinfo = list(header = NULL, footer = rtf_footer(list(
        c(l = "P=<{PROGRAM}> N=<{PROGRAM_NAME}> D=<{PROGRAM_DIR}> F=<{PROGRAM_FULL}>"))))) |>
      rtf_tables(.g2_tb())
  }
  old <- options(rtfreporter.program = NULL)
  on.exit(options(old), add = TRUE)
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(tempdir())
  sep <- if (.Platform$OS.type == "windows") "\\" else "/"
  here <- normalizePath(getwd(), winslash = "\\")
  full <- gsub("\\", "\\\\", paste(here, "programs", "t_dm.R", sep = sep), fixed = TRUE)
  txt <- .render_to_string(mk(program = "programs/t_dm.R"))
  expect_true(grepl("P=<programs/t_dm.R> N=<t_dm.R> D=<programs> F=<", txt, fixed = TRUE))
  expect_true(grepl(paste0("F=<", full, ">"), txt, fixed = TRUE))
  # program = NULL: the option
  options(rtfreporter.program = "opt.R")
  expect_true(grepl("P=<opt.R> N=<opt.R>", .render_to_string(mk()), fixed = TRUE))
  # no option: the script Rscript runs (said in a message)
  options(rtfreporter.program = NULL)
  local_mocked_bindings(commandArgs = function(...) c("Rterm", "--file=run/t_x.R"),
                        .package = "base")
  expect_message(txt <- .render_to_string(mk()), "(the script Rscript runs)", fixed = TRUE)
  expect_true(grepl("P=<run/t_x.R> N=<t_x.R> D=<run>", txt, fixed = TRUE))
})

test_that("FT-DC3005 [FR-DC132]: rtf_config(watermark = ) with an rtf_watermark() writes it; NULL leaves the document's watermark", {
  count <- function(txt) lengths(regmatches(txt, gregexpr("shpinst", txt, fixed = TRUE)))
  base <- rtf_tables(rtf_document(), .g2_tb())
  withx <- rtf_config(base, watermark = rtf_watermark("XMARK"))
  txt <- .render_to_string(withx)
  expect_identical(count(txt), 1L)
  expect_true(grepl("{\\sn gtextUNICODE}{\\sv XMARK}", txt, fixed = TRUE))
  kept <- rtf_config(withx, watermark = NULL)
  expect_identical(kept$document$watermark, withx$document$watermark)
  txt2 <- .render_to_string(kept)
  expect_identical(count(txt2), 1L)
  expect_true(grepl("{\\sn gtextUNICODE}{\\sv XMARK}", txt2, fixed = TRUE))
})

test_that("FT-DC3006 [FR-DC133]: rtf_config(font_lang = NULL) leaves the document's font language", {
  doc <- rtf_document(font_lang = c(east_asian = "ja"))
  kept <- rtf_config(doc, font_lang = NULL)
  expect_identical(kept$document$font_lang, doc$document$font_lang)
  expect_identical(kept$document$font_lang, c(east_asian = "ja"))
  txt <- .render_to_string(rtf_tables(kept, .g2_tb()))
  expect_true(grepl("\\deflangfe1041", txt, fixed = TRUE))
  # NA removes it
  none <- rtf_config(doc, font_lang = NA)
  expect_false(grepl("\\deflangfe", .render_to_string(rtf_tables(none, .g2_tb())), fixed = TRUE))
})

test_that("FT-DC3007 [FR-DC137]: rtf_config(font_lang = ) with an unknown language or slot stops with the messages of rtf_document()", {
  doc <- rtf_document()
  for (bad in list(c(east_asian = "zz"), c(bad = "ja"), c("ja"))) {
    msg <- tryCatch(rtf_document(font_lang = bad), error = function(e) conditionMessage(e))
    expect_match(msg, "font_lang")
    expect_error(rtf_config(doc, font_lang = bad), msg, fixed = TRUE)
  }
  expect_error(rtf_config(doc, font_lang = c(east_asian = "zz")),
               "`font_lang`: unknown language(s) \"zz\"; use ja, zh-Hans, zh-Hant, ko, ar, he, en.",
               fixed = TRUE)
  expect_error(rtf_config(doc, font_lang = c(bad = "ja")),
               "`font_lang`: names must be distinct slots among latin, east_asian, complex.",
               fixed = TRUE)
})

test_that("FT-DC3008 [FR-DC163]: rtf_section(page = 2) starts a section at the second table, pages counted from 1 in the order of the contents", {
  doc <- rtf_document() |> rtf_tables(list(
    data.frame(T1 = "one"), data.frame(T2 = "two"), data.frame(T3 = "three")))
  doc <- rtf_section(doc, page = 1, secinfo = list(header = rtf_header(rows = list(c(l = "HDRONE")))))
  doc <- rtf_section(doc, page = 2, secinfo = list(header = rtf_header(rows = list(c(l = "HDRTWO")))))
  expect_identical(names(doc$sections), c("1", "2"))
  txt <- .render_to_string(doc)
  pos <- function(x) regexpr(x, txt, fixed = TRUE)[1]
  expect_identical(lengths(regmatches(txt, gregexpr("HDRONE", txt, fixed = TRUE))), 1L)
  expect_identical(lengths(regmatches(txt, gregexpr("HDRTWO", txt, fixed = TRUE))), 1L)
  expect_lt(pos("HDRONE"), pos("one"))
  expect_lt(pos("one"), pos("HDRTWO"))
  expect_lt(pos("HDRTWO"), pos("two"))
  expect_lt(pos("two"), pos("three"))
  expect_identical(lengths(regmatches(txt, gregexpr("\n\\sectd\n", txt, fixed = TRUE))), 2L)
})

test_that("FT-DC3009 [FR-DC168]: secinfo = list(header, footer, watermark) is held in the section; a list of such lists gives one section per page", {
  h <- rtf_header(rows = list(c(l = "HH")))
  f <- rtf_footer(rows = list(c(l = "FF")))
  doc <- rtf_document() |> rtf_tables(list(data.frame(A = "a"), data.frame(A = "b")))
  d1 <- rtf_section(doc, page = 1, secinfo = list(header = h, footer = f, watermark = "WM1"))
  expect_length(d1$sections, 1L)
  s <- d1$sections[[1]]
  expect_identical(names(d1$sections), "1")
  expect_identical(s$header, h)
  expect_identical(s$footer, f)
  expect_identical(s$watermark, "WM1")
  txt <- .render_to_string(d1)
  expect_true(grepl("{\\sn gtextUNICODE}{\\sv WM1}", txt, fixed = TRUE))
  # two pages, one list each
  h2 <- rtf_header(rows = list(c(l = "H2")))
  d2 <- rtf_section(doc, page = c(1, 2), secinfo = list(
    list(header = h, footer = f), list(header = h2, footer = NULL)))
  expect_length(d2$sections, 2L)
  expect_identical(names(d2$sections), c("1", "2"))
  expect_identical(d2$sections[["1"]]$header, h)
  expect_identical(d2$sections[["2"]]$header, h2)
  expect_identical(d2$sections[["1"]]$footer, f)
  expect_null(d2$sections[["2"]]$footer)
})

test_that("FT-DC3010 [FR-DC169]: a string watermark in secinfo draws that text in that section only", {
  doc <- rtf_document() |> rtf_tables(list(data.frame(A = "a1"), data.frame(A = "b2")))
  doc <- rtf_section(doc, page = 1, secinfo = list(header = rtf_header(rows = list(c(l = "S1")))))
  doc <- rtf_section(doc, page = 2, secinfo = list(
    header = rtf_header(rows = list(c(l = "S2"))), watermark = "SECTION"))
  txt <- .render_to_string(doc)
  hdr <- .g2_headers(txt)
  expect_length(hdr, 2L)
  expect_false(grepl("shpinst", hdr[1], fixed = TRUE))
  expect_true(grepl("{\\sn gtextUNICODE}{\\sv SECTION}", hdr[2], fixed = TRUE))
  expect_identical(lengths(regmatches(txt, gregexpr("shpinst", txt, fixed = TRUE))), 1L)
})

test_that("FT-DC3011 [FR-DC170]: a string header with a vector page is applied to every page named; rtf_header() / rtf_footer() objects and a list of rows are stored as given", {
  doc <- rtf_document() |> rtf_tables(list(data.frame(A = "a"), data.frame(A = "b"),
                                           data.frame(A = "c")))
  d <- rtf_section(doc, page = c(1, 3), secinfo = list(header = "HSTR"))
  expect_length(d$sections, 2L)
  expect_identical(names(d$sections), c("1", "3"))
  expect_identical(unname(vapply(d$sections, function(x) x$header, "")), c("HSTR", "HSTR"))
  txt <- .render_to_string(d)
  expect_identical(lengths(regmatches(txt, gregexpr("HSTR", txt, fixed = TRUE))), 2L)
  h <- rtf_header(rows = list(c(l = "HO")))
  f <- rtf_footer(rows = list(c(l = "FO")))
  rows <- list(c(l = "R1"), c(r = "R2"))
  d2 <- rtf_section(doc, page = 1, secinfo = list(header = h, footer = f))
  expect_identical(d2$sections[[1]]$header, h)
  expect_identical(d2$sections[[1]]$footer, f)
  d3 <- rtf_section(doc, page = 1, secinfo = list(header = rows))
  expect_identical(d3$sections[[1]]$header, rows)
})

test_that("FT-DC3012 [FR-DC174]: secinfo with an element that is not header, footer or watermark, or an unnamed one, stops in the same place", {
  doc <- rtf_document() |> rtf_tables(.g2_tb())
  m1 <- tryCatch(rtf_section(doc, page = 1, secinfo = list(hdr = "h")),
                 error = function(e) conditionMessage(e))
  expect_identical(
    m1,
    paste0("`secinfo` has `hdr`: a section has `header`, `footer` and `watermark` only ",
           "(rtf_section(header = , footer = ))."))
  m2 <- tryCatch(rtf_section(doc, page = 1, secinfo = list("h")),
                 error = function(e) conditionMessage(e))
  expect_match(m2, "has unnamed elements", fixed = TRUE)
  expect_identical(sub(" has .*$", "", m2), sub(" has .*$", "", m1))
})

test_that("FT-DC3013 [FR-DC175]: a secinfo header that is not an rtf_header(), or a footer that is not an rtf_footer(), stops", {
  doc <- rtf_document() |> rtf_tables(.g2_tb())
  expect_error(rtf_section(doc, page = 1, secinfo = list(header = 1)),
               "`secinfo`'s `header` must be an rtf_header() or NULL, not a numeric.",
               fixed = TRUE)
  expect_error(rtf_section(doc, page = 1, secinfo = list(footer = 1)),
               "`secinfo`'s `footer` must be an rtf_footer() or NULL, not a numeric.",
               fixed = TRUE)
})

test_that("FT-DC3014 [FR-DC184]: combine_sections() with something that is not an rtftable names its argument, an unnamed one by position", {
  t1 <- rtftable(.g2_tb())
  expect_error(combine_sections(A = list(1, 2)),
               "`combine_sections()` argument 'A' must contain only rtftable objects.",
               fixed = TRUE)
  expect_error(combine_sections(t1, B = list(1)),
               "`combine_sections()` argument 'B' must contain only rtftable objects.",
               fixed = TRUE)
  expect_error(combine_sections(B = list(t1), list(1)),
               "`combine_sections()` argument #2 must contain only rtftable objects.",
               fixed = TRUE)
  expect_error(combine_sections(list(1, 2)),
               "`combine_sections()` argument #1 must contain only rtftable objects.",
               fixed = TRUE)
})

test_that("FT-DC3015 [FR-DC220]: a section's watermark is a shape inside that section's header group only; a section without its own holds the document's", {
  doc <- rtf_document(watermark = "DOCMARK") |>
    rtf_tables(list(data.frame(A = "a1"), data.frame(A = "b2")))
  doc <- rtf_section(doc, page = 1, secinfo = list(header = rtf_header(rows = list(c(l = "S1")))))
  doc <- rtf_section(doc, page = 2, secinfo = list(
    header = rtf_header(rows = list(c(l = "S2"))), watermark = "SECMARK"))
  expect_s3_class(rtf_watermark("DRAFT"), "rtf_watermark")
  hdr <- .g2_headers(.render_to_string(doc))
  expect_length(hdr, 2L)
  one <- function(x, what) lengths(regmatches(x, gregexpr(what, x, fixed = TRUE)))
  expect_identical(one(hdr[1], "shpinst"), 1L)
  expect_true(grepl("{\\sv DOCMARK}", hdr[1], fixed = TRUE))
  expect_false(grepl("SECMARK", hdr[1], fixed = TRUE))
  expect_identical(one(hdr[2], "shpinst"), 1L)
  expect_true(grepl("{\\sv SECMARK}", hdr[2], fixed = TRUE))
  expect_false(grepl("DOCMARK", hdr[2], fixed = TRUE))
})

test_that("FT-DC3016 [FR-DC221]: a watermark of an empty string writes no shape", {
  expect_identical(rtf_watermark("")$text, "")
  expect_identical(rtf_watermark(NA)$text, "")
  txt <- .render_to_string(rtf_tables(rtf_document(watermark = rtf_watermark("")), .g2_tb()))
  expect_false(grepl("shpinst", txt, fixed = TRUE))
  expect_false(grepl("shpinst", .render_to_string(
    rtf_tables(rtf_config(rtf_document(), watermark = rtf_watermark("")), .g2_tb())),
    fixed = TRUE))
})

test_that("FT-DC3017 [FR-DC223 FR-DC231 FR-DC229]: rtf_watermark() size, box and font: 144 half-points by default, a 6 x 2 in box, the document font unless font is given", {
  shape <- function(wm, doc_args = list()) {
    doc <- do.call(rtf_document, c(list(watermark = wm), doc_args))
    .render_to_string(rtf_tables(doc, .g2_tb()))
  }
  box <- function(txt) {
    m <- regmatches(txt, regexec("\\\\shpleft(-?[0-9]+)\\\\shptop(-?[0-9]+)\\\\shpright(-?[0-9]+)\\\\shpbottom(-?[0-9]+)",
                                 txt))[[1]]
    v <- as.integer(m[-1])
    c(w = v[3] - v[1], h = v[4] - v[2])
  }
  def <- shape(rtf_watermark("X"))
  expect_true(grepl("{\\sn gtextSize}{\\sv 1440}", def, fixed = TRUE))     # 144 half-points = 72 pt
  expect_identical(box(def), c(w = 8640L, h = 2880L))                      # 6 x 2 in
  small <- shape(rtf_watermark("X", font_size_half_points = 100L, width_in = 3, height_in = 1))
  expect_true(grepl("{\\sn gtextSize}{\\sv 1000}", small, fixed = TRUE))
  expect_identical(box(small), c(w = 4320L, h = 1440L))
  # the document's font, when font is NULL
  arial <- shape(rtf_watermark("X"), list(font_table = list(list(name = "Arial"))))
  expect_true(grepl("{\\sn gtextFont}{\\sv Arial}", arial, fixed = TRUE))
  own <- shape(rtf_watermark("X", font = "Verdana"),
               list(font_table = list(list(name = "Arial"))))
  expect_true(grepl("{\\sn gtextFont}{\\sv Verdana}", own, fixed = TRUE))
})

# ---- titles / footnotes / header / footer / style ---------------------------

test_that("FT-DC3018 [FR-DC189]: a styled title row's border is drawn in the table form", {
  doc <- rtf_document(style = rtf_style(title_format = "table")) |>
    rtf_tables(.g2_tb()) |>
    rtf_titles(list(list(list(text = "TBORDER", border = rtf_border(bottom = TRUE)), "TPLAIN")))
  txt <- .render_to_string(doc)
  with_b <- .g2_rows_with(txt, "TBORDER")
  plain <- .g2_rows_with(txt, "TPLAIN")
  expect_length(with_b, 1L)
  expect_identical(.g2_sides(with_b), "b")
  expect_true(grepl("\\clbrdrb\\brdrs\\brdrw15", with_b, fixed = TRUE))
  expect_identical(.g2_sides(plain), "")
})

test_that("FT-DC3019 [FR-DC192]: rtf_titles(font_size_half_points = 20L) is the block's size over the document's rtf_style(); NULL inherits", {
  base <- rtf_document(style = rtf_style(font_size_half_points = 30)) |> rtf_tables(.g2_tb())
  set <- .render_to_string(rtf_titles(base, "TitleX", font_size_half_points = 20L))
  line <- grep("TitleX", strsplit(set, "\n")[[1]], value = TRUE)
  expect_identical(line, "\\pard\\qc\\li0\\ri0\\fs20 \\b TitleX\\b0 \\par")
  inh <- .render_to_string(rtf_titles(base, "TitleX"))
  line <- grep("TitleX", strsplit(inh, "\n")[[1]], value = TRUE)
  expect_identical(line, "\\pard\\qc\\li0\\ri0 \\b TitleX\\b0 \\par")
  # the document's size is then the one in force
  expect_true(grepl("\\widowctrl\\fs30", inh, fixed = TRUE))
})

test_that("FT-DC3020 [FR-DC194]: rtf_titles(markup = ) is the block's own markup; NULL takes the document's", {
  base <- rtf_document() |> rtf_tables(.g2_tb())
  line <- function(doc) grep("MK", strsplit(.render_to_string(doc), "\n")[[1]], value = TRUE)
  expect_identical(line(rtf_titles(base, "MK x^{2} y>=1", markup = "script")),
                   "\\pard\\qc\\li0\\ri0 \\b MK x{\\super 2} y>=1\\b0 \\par")
  expect_identical(line(rtf_titles(base, "MK x^{2} y>=1", markup = "relational")),
                   "\\pard\\qc\\li0\\ri0 \\b MK x^\\{2\\} y\\u8805?1\\b0 \\par")
  expect_identical(line(rtf_titles(base, "MK x^{2} y>=1", markup = "all")),
                   "\\pard\\qc\\li0\\ri0 \\b MK x{\\super 2} y\\u8805?1\\b0 \\par")
  expect_identical(line(rtf_titles(base, "MK x^{2} y>=1", markup = "none")),
                   "\\pard\\qc\\li0\\ri0 \\b MK x^\\{2\\} y>=1\\b0 \\par")
  # NULL: the document's setting
  none_doc <- rtf_document(style = rtf_style(markup = "none")) |> rtf_tables(.g2_tb())
  expect_identical(line(rtf_titles(none_doc, "MK x^{2} y>=1")),
                   "\\pard\\qc\\li0\\ri0 \\b MK x^\\{2\\} y>=1\\b0 \\par")
  expect_identical(line(rtf_titles(base, "MK x^{2} y>=1")),
                   "\\pard\\qc\\li0\\ri0 \\b MK x{\\super 2} y>=1\\b0 \\par")
})

test_that("FT-DC3021 [FR-DC196]: rtf_titles(font = ) adds a font the document does not have and writes the block in it; a named vector gives one family per slot; NULL adds none", {
  base <- rtf_document() |> rtf_tables(.g2_tb())
  txt <- .render_to_string(rtf_titles(base, "TitleF", font = "Arial"))
  expect_true(grepl("{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}{\\f1\\fnil\\fcharset0 Arial;}}",
                    txt, fixed = TRUE))
  expect_true("\\pard\\qc\\li0\\ri0\\f1 \\b TitleF\\b0 \\par" %in% strsplit(txt, "\n")[[1]])
  slots <- .render_to_string(rtf_titles(base, "TitleF", font = c(latin = "Aaa", east_asian = "Bbb")))
  expect_true(grepl("{\\f1\\fnil\\fcharset0 Aaa;}{\\f2\\fnil\\fcharset0 Bbb;}}", slots, fixed = TRUE))
  expect_true(grepl("\\dbch\\af2\\loch\\f1 \\b TitleF", slots, fixed = TRUE))
  none <- .render_to_string(rtf_titles(base, "TitleF"))
  expect_true(grepl("{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}}", none, fixed = TRUE))
  expect_false(grepl("\\f1", none, fixed = TRUE))
})

test_that("FT-DC3022 [FR-DC207 FR-DC363]: a footnote block border draws left / right on every row's outer edges, inside_h between rows and inside_v between the cells; a row's own border merges over it", {
  rows <- list(c(l = "a1", c = "b1", r = "c1"), c(l = "a2", c = "b2", r = "c2"),
               c(l = "a3", c = "b3", r = "c3"))
  doc <- function(border, rws = rows) {
    rtf_document() |> rtf_tables(.g2_tb()) |> rtf_footnotes(list(rws), border = border)
  }
  sides <- function(txt, key) .g2_sides(.g2_rows_with(txt, key))
  # left / right / inside_h / inside_v together
  t1 <- .render_to_string(doc(rtf_border(left = TRUE, right = TRUE, inside_h = TRUE,
                                         inside_v = TRUE)))
  expect_identical(sides(t1, "a1"), c("blr", "blr", "blr"))
  expect_identical(sides(t1, "a2"), c("blr", "blr", "blr"))
  expect_identical(sides(t1, "a3"), c("lr", "lr", "lr"))
  # left and right alone: the outer edges only
  t2 <- .render_to_string(doc(rtf_border(left = TRUE, right = TRUE)))
  for (k in c("a1", "a2", "a3")) expect_identical(sides(t2, k), c("l", "", "r"))
  # inside_v alone: between the cells of every row
  t3 <- .render_to_string(doc(rtf_border(inside_v = TRUE)))
  for (k in c("a1", "a2", "a3")) expect_identical(sides(t3, k), c("r", "lr", "l"))
  # inside_h alone: between rows, not above the first or below the last
  t4 <- .render_to_string(doc(rtf_border(inside_h = TRUE)))
  expect_identical(sides(t4, "a1"), c("b", "b", "b"))
  expect_identical(sides(t4, "a2"), c("b", "b", "b"))
  expect_identical(sides(t4, "a3"), c("", "", ""))
  # top is the rule above the first row only
  t5 <- .render_to_string(doc(rtf_border(top = TRUE)))
  expect_identical(sides(t5, "a1"), c("t", "t", "t"))
  expect_identical(sides(t5, "a2"), c("", "", ""))
  # a row's own border is merged over the block's: the sides it sets win
  mixed <- rows
  mixed[[2]] <- list(columns = c(l = "a2", c = "b2", r = "c2"),
                     border = rtf_border(left = "none", inside_v = "dot"))
  t6 <- .render_to_string(doc(rtf_border(left = TRUE, right = TRUE, inside_h = TRUE), mixed))
  expect_identical(sides(t6, "a1"), c("bl", "b", "br"))
  # (left "none" erases the left rule of the first cell; inside_v dot is between its cells)
  expect_identical(sides(t6, "a2"), c("br", "blr", "blr"))
  r2 <- .g2_rows_with(t6, "a2")
  expect_true(grepl("\\clbrdrr\\brdrdot", r2, fixed = TRUE))
  expect_false(grepl("\\clbrdrl\\brdrs", sub("\\cellx.*", "", r2), fixed = TRUE))
})

test_that("FT-DC3023 [FR-DC208]: rtf_footnotes(row_height_twips = ) is the row height whatever the size; the size alone makes the height follow it; NULL inherits the document's", {
  base <- rtf_document(style = rtf_style(font_size_half_points = 20L)) |> rtf_tables(.g2_tb())
  row_of <- function(...) .g2_rows_with(.render_to_string(rtf_footnotes(base, list(list("FootX")), ...)), "FootX")
  both <- row_of(font_size_half_points = 14L, row_height_twips = 300L)
  expect_true(grepl("\\trrh300", both, fixed = TRUE))
  expect_true(grepl("\\fs14 FootX", both, fixed = TRUE))
  size_only <- row_of(font_size_half_points = 14L)
  expect_true(grepl("\\trrh180", size_only, fixed = TRUE))
  expect_true(grepl("\\fs14 FootX", size_only, fixed = TRUE))
  inherit <- row_of()
  expect_false(grepl("\\fs", inherit, fixed = TRUE))
  expect_true(grepl("\\trrh250", inherit, fixed = TRUE))
  expect_true(grepl("\\widowctrl\\fs20", .render_to_string(base), fixed = TRUE))
})

test_that("FT-DC3024 [FR-DC210]: rtf_footnotes(markup = \"none\") switches the markup off for the footnote alone", {
  doc <- rtf_document() |>
    rtf_tables(.g2_tb(), titles = list("Ttl x^{2}")) |>
    rtf_footnotes(list(list("Fn x^{2}")), markup = "none")
  txt <- .render_to_string(doc)
  expect_true(grepl("Fn x^\\{2\\}\\cell", txt, fixed = TRUE))
  expect_false(grepl("Fn x{\\super", txt, fixed = TRUE))
  expect_true(grepl("Ttl x{\\super 2}", txt, fixed = TRUE))
  # and switched on for the footnote in a document without markup
  doc2 <- rtf_document(style = rtf_style(markup = "none")) |>
    rtf_tables(.g2_tb(), titles = list("Ttl x^{2}")) |>
    rtf_footnotes(list(list("Fn x^{2}")), markup = "script")
  txt2 <- .render_to_string(doc2)
  expect_true(grepl("Fn x{\\super 2}\\cell", txt2, fixed = TRUE))
  expect_true(grepl("Ttl x^\\{2\\}", txt2, fixed = TRUE))
})

test_that("FT-DC3025 [FR-DC234]: rtf_header_source() of a list of pages takes the first page, the same as the rtftable", {
  tab1 <- rtftable(data.frame(A = "x", B = "y"), col_header = c("First A", "First B"))
  tab2 <- rtftable(data.frame(A = "x", B = "y"), col_header = c("Second A", "Second B"))
  from_list <- rtf_header_source(list(tab1, tab2))
  expect_identical(from_list, rtf_header_source(tab1))
  expect_true(grepl("First A", from_list, fixed = TRUE))
  expect_false(grepl("Second", from_list, fixed = TRUE))
  expect_identical(rtf_header_source(list(tab1, tab2), snippet = FALSE),
                   rtf_header_source(tab1, snippet = FALSE))
})

test_that("FT-DC3026 [FR-DC237 FR-DC239]: level \"default\" writes the effective align and default border widths but not FALSE flags; \"all\" writes them; snippet = FALSE is the bare rtf_col_header() and snippet = TRUE has the header and spanning borders in style_zone()", {
  df5 <- data.frame(Item = "x", A_N = 1L, A_M = 2.5, B_N = 3L, B_M = 4.5,
                    stringsAsFactors = FALSE)
  hdr <- rtf_col_header(
    list(col_cell(1, ""), col_cell(c(2, 5), "Treatment", border = rtf_border(bottom = TRUE))),
    c("Item", "N", "Mean", "N", "Mean"))
  tbl <- rtftable(df5, col_header = hdr, border = rtfreporter:::.rtf_table_border(
    header = rtf_border(bottom = TRUE), spanning = rtf_border(bottom = TRUE)))
  ex <- rtf_header_source(tbl, level = "explicit", snippet = FALSE)
  de <- rtf_header_source(tbl, level = "default", snippet = FALSE)
  al <- rtf_header_source(tbl, level = "all", snippet = FALSE)
  w <- 'rtf_border(bottom = rtf_border_line("single", 15))'
  expect_true(grepl("rtf_border(bottom = TRUE)", ex, fixed = TRUE))
  expect_false(grepl(w, ex, fixed = TRUE))
  expect_true(grepl(w, de, fixed = TRUE))
  expect_true(grepl("align =", de, fixed = TRUE))
  expect_false(grepl("bold = FALSE", de, fixed = TRUE))
  expect_true(grepl(w, al, fixed = TRUE))
  expect_true(grepl("bold = FALSE", al, fixed = TRUE))
  # snippet = FALSE: the bare value
  expect_true(startsWith(ex, "rtf_col_header("))
  expect_false(grepl("set_col_header(", ex, fixed = TRUE))
  expect_false(grepl("style_zone", ex, fixed = TRUE))
  # snippet = TRUE: set_col_header() with the zones' borders
  sn <- rtf_header_source(tbl)
  expect_true(startsWith(sn, "set_col_header("))
  expect_true(grepl("style_zone(header = rtf_border(bottom = TRUE), spanning = rtf_border(bottom = TRUE))",
                    sn, fixed = TRUE))
})

test_that("FT-DC3027 [FR-DC240 FR-DC241]: add_span_level = FALSE (the default) writes no scaffold row; stub = 2 keeps the second column single and empty, the default the first", {
  tbl <- rtftable(data.frame(Item = "x", A = 1, B = 2, C = 3),
                  col_header = c("Item", "A", "B", "C"))
  dflt <- rtf_header_source(tbl, snippet = FALSE)
  off <- rtf_header_source(tbl, snippet = FALSE, add_span_level = FALSE)
  expect_identical(dflt, off)
  expect_false(grepl('col_cell("Item", "")', dflt, fixed = TRUE))
  expect_false(grepl('""', dflt, fixed = TRUE))
  first <- rtf_header_source(tbl, snippet = FALSE, add_span_level = TRUE)
  expect_true(grepl('list(col_cell("Item", ""), col_cell(c("A", "C"), ""))', first, fixed = TRUE))
  second <- rtf_header_source(tbl, snippet = FALSE, add_span_level = TRUE, stub = 2)
  expect_true(grepl('col_cell("A", "")', second, fixed = TRUE))
  expect_false(grepl('list(col_cell("Item", ""), col_cell(c("A", "C"), ""))', second, fixed = TRUE))
  by_name <- rtf_header_source(tbl, snippet = FALSE, add_span_level = TRUE, stub = "A")
  expect_identical(by_name, second)
})

test_that("FT-DC3028 [FR-DC245]: rtf_text_tokens() is a data frame of the eleven tokens with the kinds and the times they are filled", {
  tk <- rtf_text_tokens()
  expect_s3_class(tk, "data.frame")
  expect_identical(names(tk), c("token", "kind", "when", "description", "example"))
  want <- data.frame(
    token = c("{PAGE}", "{TOTAL_PAGES}", "{AUTO_PAGE}", "{AUTO_TOTAL_PAGES}", "{BOOK_PAGE}",
              "{PROGRAM}", "{PROGRAM_FULL}", "{PROGRAM_NAME}", "{PROGRAM_DIR}", "{DATETIME}",
              "{PAGE_HEADING}"),
    kind = c("page", "page", "page", "page", "page", "run", "run", "run", "run", "run", "title"),
    when = c("render", "render", "viewer", "viewer", "assemble",
             "render", "render", "render", "render", "render", "compose"),
    stringsAsFactors = FALSE)
  expect_setequal(tk$token, want$token)
  got <- tk[match(want$token, tk$token), c("token", "kind", "when")]
  rownames(got) <- NULL
  expect_identical(got, want)
  expect_true(all(nzchar(tk$description)))
  expect_true(all(nzchar(tk$example[tk$token != "{BOOK_PAGE}"])))
})

test_that("FT-DC3029 [FR-DC250]: rtf_replace_text() applies the replacements one after the other, so an earlier one can affect a later one", {
  p <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  writeLines("a", p)
  rtf_replace_text(p, c("a", "b"), c("b", "c"))
  expect_identical(readLines(p, warn = FALSE), "c")
  writeLines("a b", p)
  rtf_replace_text(p, c("a", "b"), c("b", "c"))
  expect_identical(readLines(p, warn = FALSE), "c c")
  # in the other order a is not reached by the later replacement
  writeLines("a b", p)
  rtf_replace_text(p, c("b", "a"), c("c", "b"))
  expect_identical(readLines(p, warn = FALSE), "b c")
})

test_that("FT-DC3030 [FR-DC254]: rtf_replace_text() is case-sensitive by default", {
  p <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(p, paste0(p, ".bak"))), add = TRUE)
  writeLines("Draft DRAFT draft", p)
  rtf_replace_text(p, "draft", "X")
  expect_identical(readLines(p, warn = FALSE), "Draft DRAFT X")
  writeLines("Draft DRAFT draft", p)
  rtf_replace_text(p, "draft", "X", case_insensitive = FALSE)
  expect_identical(readLines(p, warn = FALSE), "Draft DRAFT X")
})

# ---- options / generate_rtfreport / rtf_style / bands -----------------------

test_that("FT-DC3031 [FR-DC263]: rtfreporter_options() has one element per rtfreporter.* default, each the option's value where set and the factory baseline otherwise", {
  nms <- c("rtfreporter.page.paper_size", "rtfreporter.page.orientation",
           "rtfreporter.page.margin_top_in", "rtfreporter.page.margin_bottom_in",
           "rtfreporter.page.margin_left_in", "rtfreporter.page.margin_right_in",
           "rtfreporter.font", "rtfreporter.font_size_half_points", "rtfreporter.font_lang",
           "rtfreporter.row_height_twips", "rtfreporter.cell_padding_left_twips",
           "rtfreporter.cell_padding_right_twips", "rtfreporter.cell_padding_top_twips",
           "rtfreporter.cell_padding_bottom_twips", "rtfreporter.markup",
           "rtfreporter.title_format", "rtfreporter.footnote_format",
           "rtfreporter.title_width", "rtfreporter.footnote_width",
           "rtfreporter.figure.default_dpi", "rtfreporter.png_device",
           "rtfreporter.rounding", "rtfreporter.ard_pipe",
           "rtfreporter.toc_style")
  saved <- stats::setNames(lapply(nms, getOption), nms)
  on.exit(options(saved), add = TRUE)
  rtfreporter_reset_defaults()
  base <- rtfreporter_options()
  expect_identical(names(base), nms)
  # one set to a value that is not the baseline: that value, the rest unchanged
  options(rtfreporter.font = "Arial", rtfreporter.rounding = "sas")
  got <- rtfreporter_options()
  expect_identical(names(got), nms)
  expect_identical(got$rtfreporter.font, "Arial")
  expect_identical(got$rtfreporter.rounding, "sas")
  for (n in setdiff(nms, c("rtfreporter.font", "rtfreporter.rounding"))) {
    expect_identical(got[[n]], base[[n]], info = n)
  }
  for (n in nms) expect_identical(got[n], stats::setNames(list(getOption(n)), n)[1] |>
                                    (\(x) if (is.null(x[[1]])) got[n] else x)(), info = n)
})

test_that("FT-DC3032 [FR-DC264]: the factory baseline is landscape Letter with 0.75 in margins, Courier New at 18 half-points, markup script, title text, footnote table, 96 dpi, png_device auto, rounding r, and NULL for the rest", {
  saved <- rtfreporter:::.rtfreporter_factory_defaults()
  saved <- stats::setNames(lapply(names(saved), getOption), names(saved))
  on.exit(options(saved), add = TRUE)
  rtfreporter_reset_defaults()
  expect_identical(
    rtfreporter_options(),
    list(rtfreporter.page.paper_size = "letter",
         rtfreporter.page.orientation = "landscape",
         rtfreporter.page.margin_top_in = 0.75,
         rtfreporter.page.margin_bottom_in = 0.75,
         rtfreporter.page.margin_left_in = 0.75,
         rtfreporter.page.margin_right_in = 0.75,
         rtfreporter.font = "Courier New",
         rtfreporter.font_size_half_points = 18L,
         rtfreporter.font_lang = NULL,
         rtfreporter.row_height_twips = NULL,
         rtfreporter.cell_padding_left_twips = NULL,
         rtfreporter.cell_padding_right_twips = NULL,
         rtfreporter.cell_padding_top_twips = NULL,
         rtfreporter.cell_padding_bottom_twips = NULL,
         rtfreporter.markup = "script",
         rtfreporter.title_format = "text",
         rtfreporter.footnote_format = "table",
         rtfreporter.title_width = NULL,
         rtfreporter.footnote_width = NULL,
         rtfreporter.figure.default_dpi = 96L,
         rtfreporter.png_device = "auto",
         rtfreporter.rounding = "r",
         rtfreporter.ard_pipe = NULL,
         rtfreporter.toc_style = NULL))
})

test_that("FT-DC3033 [FR-DC267]: rtfreporter_reset_defaults() returns invisibly the factory list it applied", {
  saved <- rtfreporter:::.rtfreporter_factory_defaults()
  saved <- stats::setNames(lapply(names(saved), getOption), names(saved))
  on.exit(options(saved), add = TRUE)
  options(rtfreporter.font = "Arial")
  v <- withVisible(rtfreporter_reset_defaults())
  expect_false(v$visible)
  expect_identical(v$value, rtfreporter_options())
  expect_identical(v$value$rtfreporter.font, "Courier New")
  expect_identical(getOption("rtfreporter.font"), "Courier New")
})

test_that("FT-DC3034 [FR-DC269]: generate_rtfreport(doc = ) takes an rtfreport object and writes the file", {
  doc <- rtf_tables(rtf_document(), data.frame(Cellvalue = "zzz"))
  rep <- rtfreporter:::.pipe_doc_to_rtfreport(doc)
  expect_s3_class(rep, "rtfreport")
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc = rep, file_path = f)
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_true(startsWith(txt, "{\\rtf1"))
  expect_true(grepl("Cellvalue\\cell", txt, fixed = TRUE))
  expect_true(grepl("zzz\\cell", txt, fixed = TRUE))
  expect_identical(txt, .render_to_string(doc))
})

test_that("FT-DC3035 [FR-DC274]: generate_rtfreport() writes over an existing file by default", {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  writeLines("OLD CONTENT", f)
  generate_rtfreport(rtf_tables(rtf_document(), data.frame(Newvalue = "n")), f)
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl("OLD CONTENT", txt, fixed = TRUE))
  expect_true(startsWith(txt, "{\\rtf1"))
  expect_true(grepl("Newvalue\\cell", txt, fixed = TRUE))
})

test_that("FT-DC3036 [FR-DC278]: with no program said, the script Rscript runs, the document knitr is knitting and the file in RStudio's editor are found and named in a message", {
  mk <- function() {
    rtf_document() |>
      rtf_section(page = 1, secinfo = list(header = NULL, footer = rtf_footer(list(
        c(l = "P=<{PROGRAM}> N=<{PROGRAM_NAME}>"))))) |>
      rtf_tables(.g2_tb())
  }
  old <- options(rtfreporter.program = NULL, knitr.in.progress = NULL)
  on.exit(options(old), add = TRUE)
  # Rscript: the --file= argument
  local({
    local_mocked_bindings(commandArgs = function(...) c("Rterm", "--file=work/t_run.R"),
                          .package = "base")
    expect_identical(rtfreporter:::.find_program(),
                     list(path = "work/t_run.R", how = "the script Rscript runs"))
    expect_message(txt <- .render_to_string(mk()),
                   "rtfreporter: {PROGRAM} is work/t_run.R (the script Rscript runs)",
                   fixed = TRUE)
    expect_true(grepl("P=<work/t_run.R> N=<t_run.R>", txt, fixed = TRUE))
  })
  # knitr: the document knit is working on
  skip_if_not_installed("knitr")
  local({
    options(knitr.in.progress = TRUE)
    local_mocked_bindings(commandArgs = function(...) "Rterm", .package = "base")
    local_mocked_bindings(current_input = function(...) "C:/docs/report.Rmd", .package = "knitr")
    expect_identical(rtfreporter:::.find_program(),
                     list(path = "C:/docs/report.Rmd", how = "the document knitr is knitting"))
    expect_message(txt <- .render_to_string(mk()),
                   "rtfreporter: {PROGRAM} is C:/docs/report.Rmd (the document knitr is knitting)",
                   fixed = TRUE)
    expect_true(grepl("N=<report.Rmd>", txt, fixed = TRUE))
    options(knitr.in.progress = NULL)
  })
  # RStudio: the file open in the editor (interactive), not an Untitled one
  skip_if_not_installed("rstudioapi")
  local({
    local_mocked_bindings(commandArgs = function(...) "Rterm", .package = "base")
    local_mocked_bindings(.is_interactive = function() TRUE)
    path <- "C:/work/t_edit.R"
    local_mocked_bindings(isAvailable = function(...) TRUE,
                          getSourceEditorContext = function(...) list(path = path),
                          .package = "rstudioapi")
    expect_identical(rtfreporter:::.find_program(),
                     list(path = "C:/work/t_edit.R", how = "the file open in RStudio's editor"))
    expect_message(txt <- .render_to_string(mk()),
                   "rtfreporter: {PROGRAM} is C:/work/t_edit.R (the file open in RStudio's editor)",
                   fixed = TRUE)
    expect_true(grepl("N=<t_edit.R>", txt, fixed = TRUE))
    path <- ""
    expect_null(rtfreporter:::.find_program())
  })
})

test_that("FT-DC3037 [FR-DC280]: a {PROGRAM...} token in a header, footer, title or footnote with no program known stops with the full message and how to say one", {
  old <- options(rtfreporter.program = NULL)
  on.exit(options(old), add = TRUE)
  local_mocked_bindings(.find_program = function() NULL)
  msg <- paste0("A header, footer, title or footnote uses {PROGRAM}, {PROGRAM_FULL}, ",
                "{PROGRAM_NAME} or {PROGRAM_DIR}, but no program is known.")
  how <- paste0("Say which with rtf_document(program = \"path/to/prog.R\"), or run ",
                "the program with source() or Rscript.")
  place <- function(where, text) {
    doc <- rtf_tables(rtf_document(), .g2_tb())
    switch(where,
      header = rtf_section(doc, page = 1, secinfo = list(header = rtf_header(c(l = text)))),
      footer = rtf_section(doc, page = 1, secinfo = list(footer = rtf_footer(c(l = text)))),
      title = rtf_titles(doc, list(text)),
      footnote = rtf_footnotes(doc, list(text)))
  }
  for (where in c("header", "footer", "title", "footnote")) {
    for (tok in c("{PROGRAM}", "{PROGRAM_FULL}", "{PROGRAM_NAME}", "{PROGRAM_DIR}")) {
      info <- paste(where, tok)
      err <- tryCatch(.render_to_string(place(where, paste("x", tok))),
                      error = function(e) conditionMessage(e))
      expect_identical(err, paste0(msg, "\n  ", how), info = info)
    }
  }
})

test_that("FT-DC3038 [FR-DC281]: {DATETIME} is taken once per file (every page shows the same time), in the datetime_format option or the given format, with an English month whatever the locale", {
  mk <- function() {
    rtf_document() |>
      rtf_tables(list(data.frame(A = "p1"), data.frame(A = "p2")),
                 titles = list("T {DATETIME} # {DATETIME:%H%M%S}"))
  }
  # (one time for the file: a clock that moves on every call would show two)
  calls <- 0L
  local_mocked_bindings(.resolve_render_time = function() {
    calls <<- calls + 1L
    as.POSIXct("2026-09-25 10:05:00") + calls
  })
  lines <- strsplit(.render_to_string(mk()), "\n")[[1]]
  t_lines <- grep("\\\\b T ", lines, value = TRUE)
  expect_length(t_lines, 2L)
  expect_identical(t_lines[1], t_lines[2])
  expect_true(grepl("T 25Sep2026  10:05 # 1005", t_lines[1]))
  # fixed by the option, a format of the option and a format in the token
  local_mocked_bindings(.resolve_render_time = function() as.POSIXct("2026-09-25 10:05:00"))
  old <- options(rtfreporter.datetime_format = "%Y")
  on.exit(options(old), add = TRUE)
  doc <- rtf_tables(rtf_document(), .g2_tb(), titles = list("A {DATETIME} B {DATETIME:%Y-%m}"))
  expect_true(grepl("A 2026 B 2026-09", .render_to_string(doc), fixed = TRUE))
  options(rtfreporter.datetime_format = NULL)
  # an English month in a locale of another language
  doc2 <- rtf_tables(rtf_document(), .g2_tb(), titles = list("M {DATETIME}"))
  loc <- Sys.getlocale("LC_TIME")
  on.exit(suppressWarnings(Sys.setlocale("LC_TIME", loc)), add = TRUE)
  other <- NULL
  for (cand in c("Japanese", "ja_JP.UTF-8", "German", "de_DE.UTF-8", "French", "fr_FR.UTF-8")) {
    set <- suppressWarnings(Sys.setlocale("LC_TIME", cand))
    if (nzchar(set) && !identical(format(as.POSIXct("2026-09-25"), "%b"), "Sep")) {
      other <- cand
      break
    }
  }
  skip_if(is.null(other), "no non-English time locale installed")
  expect_false(identical(format(as.POSIXct("2026-09-25"), "%b"), "Sep"))
  expect_true(grepl("M 25Sep2026  10:05", .render_to_string(doc2), fixed = TRUE))
})

test_that("FT-DC3039 [FR-DC293]: a header row that is a named character vector is matched to the columns by name; a single label is allowed", {
  tb <- rtftable(data.frame(a = 1, b = 2), col_header = rtf_col_header(c(b = "B", a = "A")))
  expect_identical(tb$col_header, list(c("A", "B")))
  rev <- rtftable(data.frame(a = 1, b = 2, c = 3),
                  col_header = rtf_col_header(c(c = "C", a = "A", b = "B")))
  expect_identical(rev$col_header, list(c("A", "B", "C")))
  one <- rtftable(data.frame(a = 1), col_header = rtf_col_header(c(a = "Only")))
  expect_identical(one$col_header, list("Only"))
  txt <- .render_to_string(rtf_tables(rtf_document(), tb))
  expect_true(grepl("A\\cell\\qc\\li0\\ri0 B\\cell", txt, fixed = TRUE))
})

test_that("FT-DC3040 [FR-DC308]: rtf_style() holds every shared key as given and the sections as named lists of the keys set; NULL is unset", {
  st <- rtf_style(font = "Arial", font_size_half_points = 20L, row_height_twips = 300L,
                  cell_padding_left_twips = 1L, cell_padding_right_twips = 2L,
                  cell_padding_top_twips = 3L, cell_padding_bottom_twips = 4L,
                  markup = "relational", color = "#FF0000", title_format = "table",
                  footnote_format = "text", title_width = 0.5, footnote_width = 0.25,
                  titles = list(font = "Arial", row_height_twips = NULL),
                  footnotes = list(markup = "relational"),
                  footer = list(row_height_twips = 300L, font_size_half_points = 16L))
  expect_s3_class(st, "rtf_style")
  expect_identical(
    names(unclass(st)),
    c("font", "font_size_half_points", "row_height_twips", "cell_padding_left_twips",
      "cell_padding_right_twips", "cell_padding_top_twips", "cell_padding_bottom_twips",
      "markup", "color", "title_format", "footnote_format", "title_width",
      "footnote_width", "table", "titles", "footnotes", "header", "footer"))
  expect_identical(st$font, "Arial")
  expect_identical(st$font_size_half_points, 20L)
  expect_identical(st$row_height_twips, 300L)
  expect_identical(c(st$cell_padding_left_twips, st$cell_padding_right_twips,
                     st$cell_padding_top_twips, st$cell_padding_bottom_twips), 1:4)
  expect_identical(st$markup, "relational")
  expect_identical(st$color, "#FF0000")
  expect_identical(st$title_format, "table")
  expect_identical(st$footnote_format, "text")
  expect_identical(st$title_width, 0.5)
  expect_identical(st$footnote_width, 0.25)
  expect_identical(st$titles, list(font = "Arial"))
  expect_identical(st$footnotes, list(markup = "relational"))
  expect_identical(st$footer, list(row_height_twips = 300L, font_size_half_points = 16L))
  expect_identical(st$header, list())
  expect_identical(st$table, list())
  # everything left NULL is unset
  none <- rtf_style()
  expect_s3_class(none, "rtf_style")
  expect_true(all(vapply(unclass(none)[1:13], is.null, NA)))
})

test_that("FT-DC3041 [FR-DC309]: rtf_style() row height, padding, markup and font reach every module: table, title, footnote, header and footer", {
  st <- function(top) {
    rtf_style(row_height_twips = 300L, cell_padding_top_twips = top,
              cell_padding_bottom_twips = 20L, cell_padding_left_twips = 50L,
              markup = "none", font = "Arial", title_format = "table")
  }
  mk <- function(top) {
    rtf_document(style = st(top)) |>
      rtf_section(page = 1, secinfo = list(header = rtf_header(c(l = "H^{2}")),
                                           footer = rtf_footer(c(l = "F^{2}")))) |>
      rtf_tables(.g2_tb(), titles = list("T^{2}"), footnotes = list(list("N^{2}")))
  }
  txt <- .render_to_string(mk(0L))
  rows <- .g2_rows(txt)
  expect_length(rows, 7L)                       # header, footer, title, 3 table rows, footnote
  expect_true(all(grepl("\\trrh300", rows, fixed = TRUE)))
  expect_true(all(grepl("\\clpadb20\\clpadfb3", rows, fixed = TRUE)))
  expect_true(all(grepl("\\li50", rows, fixed = TRUE)))
  # a top padding of 0 writes nothing
  expect_false(grepl("\\clpadl", txt, fixed = TRUE))
  for (k in c("H^\\{2\\}", "F^\\{2\\}", "T^\\{2\\}", "N^\\{2\\}")) {
    row <- .g2_rows_with(txt, k)
    expect_length(row, 1L)
    expect_true(grepl("\\f1 ", row, fixed = TRUE), info = k)
  }
  expect_true(grepl("{\\f1\\fnil\\fcharset0 Arial;}", txt, fixed = TRUE))
  # a top padding of 30 is in every row
  rows30 <- .g2_rows(.render_to_string(mk(30L)))
  expect_true(all(grepl("\\clpadl30\\clpadfl3", rows30, fixed = TRUE)))
})

test_that("FT-DC3042 [FR-DC312]: rtf_style(table = ) reaches every content table: font, exact row height, blank row height, paddings", {
  t1 <- rtftable(.g2_tb(), blank_rows = 1L)
  doc <- rtf_document(style = rtf_style(table = list(
    font = "Arial", row_height_twips = 300L, blank_row_height_twips = 100L,
    cell_padding_right_twips = 5L, cell_padding_top_twips = 6L, cell_padding_bottom_twips = 7L,
    row_height_exact = TRUE))) |> rtf_tables(list(t1, t1))
  txt <- .render_to_string(doc)
  rows <- .g2_rows(txt)
  expect_length(rows, 8L)
  blank <- grepl("trrh-100", rows, fixed = TRUE)
  expect_identical(sum(blank), 2L)
  expect_true(all(grepl("\\trrh-300", rows[!blank], fixed = TRUE)))
  expect_true(all(grepl("\\clpadl6\\clpadfl3\\clpadb7\\clpadfb3", rows[!blank], fixed = TRUE)))
  expect_true(all(grepl("\\ri5\\f1 ", rows[!blank], fixed = TRUE)))
  expect_true(grepl("{\\f1\\fnil\\fcharset0 Arial;}", txt, fixed = TRUE))
})

test_that("FT-DC3043 [FR-DC313]: rtf_style(titles = , footnotes = ) font, row height, align, markup and border reach the title and the footnote block", {
  doc <- rtf_document(style = rtf_style(
    title_format = "table",
    titles = list(font = "Arial", row_height_twips = 300L, align = "left", markup = "none",
                  border = rtf_border(bottom = TRUE)),
    footnotes = list(font = "Verdana", row_height_twips = 310L, align = "right", markup = "none",
                     border = rtf_border(top = TRUE)))) |>
    rtf_tables(.g2_tb(), titles = list("T^{2}"), footnotes = list(list("N^{2}")))
  txt <- .render_to_string(doc)
  ti <- .g2_rows_with(txt, "T^\\{2\\}")
  fn <- .g2_rows_with(txt, "N^\\{2\\}")
  expect_length(ti, 1L)
  expect_length(fn, 1L)
  expect_true(grepl("\\trrh300", ti, fixed = TRUE))
  expect_true(grepl("\\ql\\li0\\ri0\\f1 \\b T^\\{2\\}", ti, fixed = TRUE))
  expect_identical(.g2_sides(ti), "b")
  expect_true(grepl("\\trrh310", fn, fixed = TRUE))
  expect_true(grepl("\\qr\\li0\\ri0\\f2 N^\\{2\\}", fn, fixed = TRUE))
  expect_identical(.g2_sides(fn), "t")
  expect_true(grepl("{\\f1\\fnil\\fcharset0 Arial;}{\\f2\\fnil\\fcharset0 Verdana;}", txt, fixed = TRUE))
})

test_that("FT-DC3044 [FR-DC314]: rtf_style(header = , footer = ) font, row height, markup, paddings, border and colour reach the band of every section", {
  doc <- rtf_document(style = rtf_style(
    header = list(font = "Arial", row_height_twips = 300L, markup = "none",
                  cell_padding_right_twips = 5L, cell_padding_top_twips = 6L,
                  cell_padding_bottom_twips = 7L, border = rtf_border(bottom = TRUE),
                  color = "#FF0000"),
    footer = list(font = "Verdana", row_height_twips = 310L, markup = "none",
                  cell_padding_right_twips = 8L, cell_padding_top_twips = 9L,
                  cell_padding_bottom_twips = 11L))) |>
    rtf_section(page = 1, secinfo = list(header = rtf_header(c(l = "H1^{2}")),
                                         footer = rtf_footer(c(l = "F1^{2}")))) |>
    rtf_section(page = 2, secinfo = list(header = c(l = "H2^{2}"),
                                         footer = rtf_footer(c(l = "F2^{2}")))) |>
    rtf_tables(list(.g2_tb(), .g2_tb()))
  txt <- .render_to_string(doc)
  for (k in c("H1", "H2")) {
    row <- .g2_rows_with(txt, paste0(k, "^"))
    expect_length(row, 1L)
    expect_true(grepl("\\trrh300", row, fixed = TRUE), info = k)
    expect_true(grepl("\\clpadl6\\clpadfl3\\clpadb7\\clpadfb3", row, fixed = TRUE), info = k)
    expect_true(grepl("\\ri5\\f1 \\cf3 ", row, fixed = TRUE), info = k)
    expect_true(grepl(paste0(k, "^\\{2\\}\\cf1 \\cell"), row, fixed = TRUE), info = k)
    expect_identical(.g2_sides(row), "b", info = k)
  }
  for (k in c("F1", "F2")) {
    row <- .g2_rows_with(txt, paste0(k, "^"))
    expect_length(row, 1L)
    expect_true(grepl("\\trrh310", row, fixed = TRUE), info = k)
    expect_true(grepl("\\clpadl9\\clpadfl3\\clpadb11\\clpadfb3", row, fixed = TRUE), info = k)
    expect_true(grepl("\\ri8\\f2 ", row, fixed = TRUE), info = k)
    expect_true(grepl(paste0(k, "^\\{2\\}\\cell"), row, fixed = TRUE), info = k)
  }
})

test_that("FT-DC3045 [FR-DC328]: a band's row_height_twips wins over the height its font size gives; a size of NULL inherits the document's", {
  doc <- function(...) {
    rtf_document(style = rtf_style(font_size_half_points = 20L)) |>
      rtf_section(page = 1, secinfo = list(header = rtf_header(c(l = "H"), ...),
                                           footer = rtf_footer(c(l = "F"), ...))) |>
      rtf_tables(.g2_tb())
  }
  txt <- .render_to_string(doc(font_size_half_points = 24, row_height_twips = 500))
  expect_true(grepl("{\\header \\f0\\fs20\\trowd\\trrh500\\cellx13680\\ql\\li0\\ri0\\fs24 H\\cell\\row}",
                    txt, fixed = TRUE))
  expect_true(grepl("\\trrh500", .g2_rows_with(txt, "F\\cell"), fixed = TRUE))
  expect_true(grepl("\\fs24 F\\cell", txt, fixed = TRUE))
  inh <- .render_to_string(doc())
  expect_true(grepl("{\\header \\f0\\fs20\\trowd\\trrh250\\cellx13680\\ql\\li0\\ri0 H\\cell\\row}",
                    inh, fixed = TRUE))
  expect_true(grepl("{\\footer \\f0\\fs20\\trowd\\trrh250", inh, fixed = TRUE))
  # the size alone: the height follows it
  sz <- .render_to_string(doc(font_size_half_points = 24))
  expect_true(grepl("\\trrh290", .g2_rows_with(sz, "F\\cell"), fixed = TRUE))
})

test_that("FT-DC3046 [FR-DC354]: a band or block width above 1 that is not a whole finite number stops with the message, naming the argument", {
  msg <- "must be \"content\", \"page\", a fraction in (0, 1], or twips."
  for (w in c(1.01, 1.5, Inf)) {
    expect_error(rtf_header(list(c(l = "a")), width = w), paste("`width`", msg), fixed = TRUE)
    expect_error(rtf_footer(list(c(l = "a")), width = w), paste("`width`", msg), fixed = TRUE)
    expect_error(rtf_style(title_width = w), paste("`rtf_style(title_width)`", msg), fixed = TRUE)
    expect_error(rtf_style(footnote_width = w), paste("`rtf_style(footnote_width)`", msg),
                 fixed = TRUE)
  }
  # whole twips and fractions are accepted
  expect_no_error(rtf_header(list(c(l = "a")), width = 5000))
  expect_s3_class(rtf_style(title_width = 0.5), "rtf_style")
})

test_that("FT-DC3047 [FR-DC358]: a row of three cells divides the block's width evenly and its last cell ends at the block's right edge, in a header, a footer and a footnote", {
  doc <- rtf_document(style = rtf_style(footnote_width = 5000)) |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(list(c(l = "a", c = "b", r = "c")), width = 5000),
      footer = rtf_footer(list(c(l = "d", c = "e", r = "f")), width = 5000))) |>
    rtf_tables(.g2_tb(), footnotes = list(list(c(l = "g", c = "h", r = "i"), c(l = "j", r = "k"))))
  txt <- .render_to_string(doc)
  expect_identical(.g2_cellx(.g2_rows_with(txt, "a\\cell")), c(1666L, 3332L, 5000L))
  expect_identical(.g2_cellx(.g2_rows_with(txt, "d\\cell")), c(1666L, 3332L, 5000L))
  expect_identical(.g2_cellx(.g2_rows_with(txt, "g\\cell")), c(1666L, 3332L, 5000L))
  expect_identical(.g2_cellx(.g2_rows_with(txt, "j\\cell")), c(2500L, 5000L))
})

test_that("FT-DC3048 [FR-DC359]: a value of a token of one's own, of a page's column and of the program that holds a token prints it with its braces, in a header, footer, title and footnote", {
  doc <- rtf_document(tokens = list(OWN = "{PAGE}", OWN2 = "{AUTO_PAGE}")) |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(c(l = "h[{OWN}]")), footer = rtf_footer(c(l = "f[{OWN}] {OWN2}")))) |>
    rtf_tables(.g2_tb(), titles = list("t[{OWN}]"), footnotes = list(list("n[{OWN}] {OWN2}")))
  txt <- .render_to_string(doc)
  expect_true(grepl("h[\\{PAGE\\}]", txt, fixed = TRUE))
  expect_true(grepl("f[\\{PAGE\\}] \\{AUTO_PAGE\\}", txt, fixed = TRUE))
  expect_true(grepl("t[\\{PAGE\\}]", txt, fixed = TRUE))
  expect_true(grepl("n[\\{PAGE\\}] \\{AUTO_PAGE\\}", txt, fixed = TRUE))
  expect_false(grepl("\\chpgn", txt, fixed = TRUE))
  # the value of a page's column
  pages <- as_rtftables(data.frame(G = c("{PAGE}", "{AUTO_PAGE}"), V = 1:2),
                        split = "by_value", group_col = "G")
  doc2 <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL,
                                         footer = rtf_footer(list(c(l = "g[{G}]"))))) |>
    rtf_tables(pages)
  txt2 <- .render_to_string(doc2)
  expect_true(grepl("g[\\{PAGE\\}]", txt2, fixed = TRUE))
  expect_true(grepl("g[\\{AUTO_PAGE\\}]", txt2, fixed = TRUE))
  # the program's path
  doc3 <- rtf_document(program = "a/{PAGE}.R") |>
    rtf_section(page = 1, secinfo = list(header = NULL,
      footer = rtf_footer(list(c(l = "p[{PROGRAM}] [{PROGRAM_DIR}]"))))) |>
    rtf_tables(.g2_tb())
  txt3 <- .render_to_string(doc3)
  expect_true(grepl("p[a/\\{PAGE\\}.R] [a]", txt3, fixed = TRUE))
})

test_that("FT-DC3049 [FR-DC362]: rtf_header(border = rtf_border(all = TRUE)) on a two-row band: top on the first row, bottom on the last, left and right on the outer edges of every row", {
  rows <- list(c(l = "r1a", r = "r1b"), c(l = "r2a", c = "r2b", r = "r2c"))
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(rows, border = rtf_border(all = TRUE)),
      footer = rtf_footer(rows, border = rtf_border(all = TRUE)))) |>
    rtf_tables(.g2_tb())
  txt <- .render_to_string(doc)
  hdr <- .g2_headers(txt)
  expect_identical(.g2_sides(.g2_rows_with(hdr, "r1a")), c("tl", "tr"))
  expect_identical(.g2_sides(.g2_rows_with(hdr, "r2a")), c("bl", "b", "br"))
  ftr <- regmatches(txt, regexpr("\\{\\\\footer .*?\\\\row\\}", txt, perl = TRUE))
  ftr <- regmatches(txt, regexpr("\\{\\\\footer [^\n]*", txt))
  expect_identical(.g2_sides(.g2_rows_with(ftr, "r1a")), c("tl", "tr"))
  expect_identical(.g2_sides(.g2_rows_with(ftr, "r2a")), c("bl", "b", "br"))
})

test_that("FT-DC3050 [FR-DC363]: a header / footer / footnote row's own border = rtf_border(inside_v = \"dot\") draws a dotted rule between that row's cells only", {
  rows <- list(list(columns = c(l = "r1a", r = "r1b"), border = rtf_border(inside_v = "dot")),
               c(l = "r2a", r = "r2b"))
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = rtf_header(rows),
                                         footer = rtf_footer(rows, border = NULL))) |>
    rtf_tables(.g2_tb(), footnotes = list(rows))
  txt <- .render_to_string(doc)
  hdr <- .g2_headers(txt)
  r1 <- .g2_rows_with(hdr, "r1a")
  expect_identical(.g2_sides(r1), c("r", "l"))
  expect_identical(lengths(regmatches(r1, gregexpr("\\clbrdrr\\brdrdot", r1, fixed = TRUE))), 1L)
  expect_identical(lengths(regmatches(r1, gregexpr("\\clbrdrl\\brdrdot", r1, fixed = TRUE))), 1L)
  expect_identical(.g2_sides(.g2_rows_with(hdr, "r2a")), c("", ""))
  ftr <- regmatches(txt, regexpr("\\{\\\\footer [^\n]*", txt))
  expect_identical(.g2_sides(.g2_rows_with(ftr, "r1a")), c("r", "l"))
  expect_identical(.g2_sides(.g2_rows_with(ftr, "r2a")), c("", ""))
  fn1 <- .g2_rows_with(txt, "r1a")
  fn1 <- fn1[!grepl("^\\\\trowd[^\n]*\\\\header", fn1)]
  fn <- .g2_rows(sub("^.*\\\\pard\\\\qc\\\\li0\\\\ri0\\\\par", "", txt))
  fn <- fn[grepl("r1a", fn, fixed = TRUE)]
  expect_identical(.g2_sides(fn[length(fn)]), c("r", "l"))
})

test_that("FT-DC3051 [FR-DC365]: rtf_titles(border = ) in the table form: inside_h between rows, left / right on every row, a row's own border merged over it, rtf_style(titles = list(border = )) for a block that sets none, anything else stops", {
  tb <- .g2_tb()
  doc <- function(...) {
    rtf_document(style = rtf_style(title_format = "table")) |> rtf_tables(tb) |> rtf_titles(...)
  }
  b <- rtf_border(inside_h = TRUE, left = TRUE, right = TRUE)
  txt <- .render_to_string(doc(list(c("T1x", "T2x", "T3x")), border = b))
  expect_identical(.g2_sides(.g2_rows_with(txt, "T1x")), "blr")
  expect_identical(.g2_sides(.g2_rows_with(txt, "T2x")), "blr")
  expect_identical(.g2_sides(.g2_rows_with(txt, "T3x")), "lr")
  # top is the rule above the first row, bottom the rule below the last
  tb2 <- .render_to_string(doc(list(c("T1x", "T2x", "T3x")),
                               border = rtf_border(top = TRUE, bottom = TRUE)))
  expect_identical(.g2_sides(.g2_rows_with(tb2, "T1x")), "t")
  expect_identical(.g2_sides(.g2_rows_with(tb2, "T2x")), "")
  expect_identical(.g2_sides(.g2_rows_with(tb2, "T3x")), "b")
  # a row's own border is merged side by side over the block's
  own <- list(list(c("T1x", list(text = "T2x", border = rtf_border(left = "none", bottom = FALSE)),
                     "T3x")))
  own <- list(list("T1x", list(text = "T2x", border = rtf_border(left = "none", top = TRUE)), "T3x"))
  txt2 <- .render_to_string(doc(own, border = b))
  expect_identical(.g2_sides(.g2_rows_with(txt2, "T2x")), "br")
  expect_identical(.g2_sides(.g2_rows_with(txt2, "T1x")), "blr")
  # no border: the output unchanged
  expect_identical(.render_to_string(doc(list(c("T1x", "T2x")))),
                   .render_to_string(doc(list(c("T1x", "T2x")), border = NULL)))
  # a block that sets none takes the style's
  st <- rtf_document(style = rtf_style(title_format = "table", titles = list(border = b))) |>
    rtf_tables(tb, titles = list(c("S1x", "S2x")))
  txt3 <- .render_to_string(st)
  expect_identical(.g2_sides(.g2_rows_with(txt3, "S1x")), "blr")
  expect_identical(.g2_sides(.g2_rows_with(txt3, "S2x")), "lr")
  expect_error(doc(list(c("T1x")), border = "x"),
               "`rtf_titles(border = )` must be NULL or an rtf_border() object.", fixed = TRUE)
  expect_error(doc(list(c("T1x")), border = TRUE),
               "`rtf_titles(border = )` must be NULL or an rtf_border() object.", fixed = TRUE)
})

test_that("FT-DC3052 [FR-DC276]: generate_rtfreport(program = ) overrides the document's own program for one file; NULL uses the document's; {PROGRAM_FULL} is made absolute", {
  doc <- rtf_document(program = "doc/own.R") |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = rtf_footer(list(
      c(l = "P=<{PROGRAM}> N=<{PROGRAM_NAME}> D=<{PROGRAM_DIR}>"))))) |>
    rtf_tables(.g2_tb())
  go <- function(...) {
    f <- tempfile(fileext = ".rtf")
    on.exit(unlink(f), add = TRUE)
    generate_rtfreport(doc, f, ...)
    paste(readLines(f, warn = FALSE), collapse = "\n")
  }
  expect_true(grepl("P=<doc/own.R> N=<own.R> D=<doc>", go(program = NULL), fixed = TRUE))
  expect_true(grepl("P=<doc/own.R> N=<own.R> D=<doc>", go(), fixed = TRUE))
  expect_true(grepl("P=<work/tfl/t_dm.R> N=<t_dm.R> D=<work/tfl>",
                    go(program = "work/tfl/t_dm.R"), fixed = TRUE))
  expect_false(grepl("own.R", go(program = "work/tfl/t_dm.R"), fixed = TRUE))
})
