# test-assemble-toc-layout.R -- the layout of a TOC entry paragraph: the right
# indent before the page-number column and \keep (#1056), and the hanging
# style's number and title (toc_style entry hanging = TRUE).  The helpers are
# in helper-assemble-toc.R; the data are fictional.

# The paragraph properties of a TOC paragraph: what comes before its text
.tl_props <- function(p) sub("^\\{\\\\pard((?:\\\\[a-z]+-?[0-9]*)*) .*$", "\\1", p)

test_that("FT-AS7301 [FR-AS265]: every TOC entry paragraph has a right indent before the page-number column and \\keep, at every level and with the hanging style; the right tab stays at the text width; headings have neither; a long label stays in one paragraph and the estimate counts its lines", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # a long label at levels 2 and 3
  x <- readLines(.tt_case_longlabel(d), warn = FALSE)
  e <- .tt_entries(x)
  expect_length(e, 3L)
  expect_identical(.tl_props(e),
                   c("\\li600\\ri720\\keep\\fs20\\tqr\\tldot\\tx14112",
                     "\\li600\\ri720\\keep\\fs20\\tqr\\tldot\\tx14112",
                     "\\li1200\\ri720\\keep\\fs20\\tqr\\tldot\\tx14112"))
  expect_true(grepl(.tt_long_label(), e[2L], fixed = TRUE))          # one paragraph, the whole label
  expect_true(grepl(.tt_long_label("14.3.2.1"), e[3L], fixed = TRUE))
  expect_identical(sum(grepl("\\\\par\\}$", e)), 3L)
  expect_false(any(grepl("\\\\fi[0-9-]", e)))                       # no hanging indent by default
  # the hanging style: the same right indent, \keep and right tab
  dh <- .tt_dir(); on.exit(unlink(dh, recursive = TRUE), add = TRUE)
  e <- .tt_entries(readLines(.tt_case_hanging(dh), warn = FALSE))
  expect_true(all(grepl("^\\\\li[0-9]+\\\\fi-[0-9]+\\\\ri720\\\\keep\\\\fs20(\\\\tx[0-9]+)*\\\\tqr\\\\tldot\\\\tx14112$",
                        .tl_props(e))))
  expect_true(grepl(sub("^Table 14.3.2.1 ", "", .tt_long_label("14.3.2.1")),
                    e[3L], fixed = TRUE))
  # level 1, headings and the title: an entry has \ri and \keep, nothing else does
  f <- .tt_inputs(d, c(t_14_1_1 = "A", t_14_2_1 = "B", t_14_3_1 = "C"))
  toc <- data.frame(file = f, heading = c("Efficacy", "Safety", NA),
                    label = c("One", "Two", "Three"), level = c(1L, 2L, 3L))
  for (st in list(NULL, list(entry = list(hanging = TRUE)), list(entry = list(hanging = 360)))) {
    p <- .tt_paras(.tt_asm(f, toc = toc, toc_style = st))
    ent <- grepl("PAGEREF", p)
    expect_identical(sum(ent), 3L)
    expect_true(all(grepl("\\\\ri720\\\\keep\\\\fs20", p[ent])))
    expect_true(all(grepl("\\\\tqr\\\\tldot\\\\tx14112 \\{\\\\field", p[ent])))
    expect_false(any(grepl("\\\\ri[0-9]|\\\\keep", p[!ent])))
    expect_identical(sum(grepl("\\\\b\\\\fs22 (Efficacy|Safety)\\\\par", p[!ent])), 2L)
  }
  # the leader off: the right indent stays, the tab stop too
  e <- .tt_entries(.tt_asm(f, toc = toc, toc_leader = "none"))
  expect_true(all(grepl("\\\\ri720\\\\keep\\\\fs20\\\\tqr\\\\tx14112 ", e)))
  # the right indent is 6 characters at the entry size: 864 at 12 pt
  e <- .tt_entries(.tt_asm(f, toc = toc, toc_style = list(entry = list(size = 12))))
  expect_true(all(grepl("\\\\ri864\\\\keep\\\\fs24\\\\tqr", e)))
  # and the digits of the largest page number when it has more than 4: 7 x 120
  d2 <- .tt_dir(); on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  g <- .tt_inputs(d2, c(t_1 = "A", t_2 = "B"), pages = c(10000L, 1L))
  x <- .tt_asm(g, toc = "auto")
  expect_identical(.tt_pages(x), c(2L, 10002L))
  expect_true(all(grepl("^\\{\\\\pard\\\\ri840\\\\keep\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ", .tt_entries(x))))
  # the estimate: at level 2, 14112 - 600 (indent) - 720 (right indent) =
  # 12792 twips = 106 Courier New characters at 10 pt; a page holds 35 lines
  pages_of <- function(len, level) {
    toc <- data.frame(file = f[rep_len(1:2, 35L)], label = c(rep("x", 34L), strrep("y", len)),
                      level = c(rep(2L, 34L), level))
    sum(grepl("\\\\pagebb", .tt_paras(.tt_asm(f[1:2], toc = toc)))) + 1L
  }
  expect_identical(pages_of(106L, 2L), 1L)
  expect_identical(pages_of(107L, 2L), 2L)
  # (level 3: 14112 - 1200 - 720 = 12192 = 101 characters)
  expect_identical(pages_of(101L, 3L), 1L)
  expect_identical(pages_of(102L, 3L), 2L)
})

test_that("FT-AS7302 [FR-AS269]: with hanging = TRUE an entry is <number>\\tab <title> with tab stops at the hang and 2 and 4 em after it, \\li<indent + hang>\\fi-<hang>; the hang is the widest number in the TOC and 1 em; a label that does not split is written whole; toc = \"auto\" and assemble_folder() lay out the same; a whole number of twips writes every label whole; the estimate counts the title's lines", {
  d <- .tt_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .tt_inputs(d, c(t_14_3_2_1 = "A", t_9_1 = "B", app_1 = "C", f_14_1 = "D"))
  toc <- data.frame(file = f, heading = c("Safety", NA, NA, NA),
                    label = c("Table 14.3.2.1 Adverse events by severity",
                              "Table 9.1 Disposition",
                              "Appendix of sites",
                              "Figure 14.1.1a Kaplan-Meier plot"),
                    level = c(2L, 3L, 2L, 1L))
  hang <- list(entry = list(hanging = TRUE))
  x <- .tt_asm(f, toc = toc, toc_style = hang)
  e <- .tt_entries(x)
  # the widest number: "Table 14.3.2.1" and "Figure 14.1.1a", 14 characters
  # x 0.6 em x 10 pt + 1 em = 1680 + 200 = 1880 twips
  expect_identical(.tl_props(e),
                   c("\\li2480\\fi-1880\\ri720\\keep\\fs20\\tx2480\\tx2880\\tx3280\\tqr\\tldot\\tx14112",
                     "\\li3080\\fi-1880\\ri720\\keep\\fs20\\tx3080\\tx3480\\tx3880\\tqr\\tldot\\tx14112",
                     "\\li2480\\fi-1880\\ri720\\keep\\fs20\\tqr\\tldot\\tx14112",
                     "\\li1880\\fi-1880\\ri720\\keep\\fs20\\tx1880\\tx2280\\tx2680\\tqr\\tldot\\tx14112"))
  expect_identical(.tt_labels(x),
                   c("Table 14.3.2.1\\tab Adverse events by severity",
                     "Table 9.1\\tab Disposition",
                     "Appendix of sites",                       # does not split: whole
                     "Figure 14.1.1a\\tab Kaplan-Meier plot"))
  # the heading is as without the hanging style
  p <- .tt_paras(x)
  expect_identical(p[grepl("Safety", p)], "{\\pard\\b\\fs22 Safety\\par}")
  # the widest number decides: "Table 9.1" alone, 9 x 120 + 200 = 1280
  e <- .tt_entries(.tt_asm(f[2:3], toc = toc[2:3, c("file", "label", "level")], toc_style = hang))
  expect_identical(.tl_props(e),
                   c("\\li2480\\fi-1280\\ri720\\keep\\fs20\\tx2480\\tx2880\\tx3280\\tqr\\tldot\\tx14112",
                     "\\li1880\\fi-1280\\ri720\\keep\\fs20\\tqr\\tldot\\tx14112"))
  # no label splits: the hang is 720, every label whole
  e <- .tt_entries(.tt_asm(f[3:4], toc = c("Appendix of sites", "Plot"), toc_style = hang))
  expect_true(all(grepl("^\\{\\\\pard\\\\li720\\\\fi-720\\\\ri720\\\\keep\\\\fs20\\\\tqr\\\\tldot\\\\tx14112 ", e)))
  # a whole number of twips: that hang, the label whole
  x <- .tt_asm(f, toc = toc, toc_style = list(entry = list(hanging = 360)))
  expect_identical(.tl_props(.tt_entries(x))[1L], "\\li960\\fi-360\\ri720\\keep\\fs20\\tqr\\tldot\\tx14112")
  expect_identical(.tt_labels(x)[1L], "Table 14.3.2.1 Adverse events by severity")
  # off (the default): no \fi, the label whole
  x <- .tt_asm(f, toc = toc)
  expect_false(any(grepl("\\\\fi-|\\\\tab [A-Z]", .tt_entries(x))))
  # toc = "auto": the title reader's number; "Table 14.1.1" is the widest,
  # 12 x 120 + 200 = 1640, level 1
  d2 <- .tt_dir(); on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  g <- .tt_inputs(d2, c(t_14_1_1 = "Table 14.1.1 Demographics", t_9_1 = "Table 9.1 Disposition"))
  x <- .tt_asm(g, toc = "auto", toc_style = hang)
  expect_identical(.tl_props(.tt_entries(x)),
                   rep("\\li1640\\fi-1640\\ri720\\keep\\fs20\\tx1640\\tx2040\\tx2440\\tqr\\tldot\\tx14112", 2L))
  expect_identical(.tt_labels(x), c("Table 14.1.1\\tab Demographics", "Table 9.1\\tab Disposition"))
  # assemble_folder(): the same split, at level 2 (its default)
  d4 <- .tt_dir(); on.exit(unlink(d4, recursive = TRUE), add = TRUE)
  .tt_inputs(d4, c(t_14_1_1 = "Table 14.1.1 Demographics", t_9_1 = "Table 9.1 Disposition"))
  out <- file.path(d2, "folder.rtf")
  assemble_folder(d4, output_file = out, toc_style = hang)
  y <- readLines(out, warn = FALSE)
  expect_identical(.tt_labels(y), c("Table 9.1\\tab Disposition", "Table 14.1.1\\tab Demographics"))
  expect_true(all(grepl("^\\\\li2240\\\\fi-1640\\\\ri720\\\\keep\\\\fs20\\\\tx2240\\\\tx2640\\\\tx3040\\\\tqr", .tl_props(.tt_entries(y)))))
  # another font: 0.75 em a character, 12 x 0.75 x 200 + 200 = 2000 (Arial)
  d3 <- .tt_dir(); on.exit(unlink(d3, recursive = TRUE), add = TRUE)
  g <- .tt_inputs(d3, c(t_14_1_1 = "Table 14.1.1 Demographics", t_9_1 = "Table 9.1 Disposition"),
                  font = "Arial")
  expect_true(all(grepl("^\\\\li2000\\\\fi-2000\\\\ri720\\\\keep\\\\fs20\\\\tx2000\\\\tx2400\\\\tx2800\\\\tqr",
                        .tl_props(.tt_entries(.tt_asm(g, toc = "auto", toc_style = hang))))))
  # the estimate: the title's lines, at the width beside the hang.  Level 2
  # with "Table 14.3.2" (hang 1640): 14112 - 600 - 720 - 1640 = 11152 twips =
  # 92 characters of title; a page holds 35 lines
  pages_of <- function(len) {
    toc <- data.frame(file = f[rep_len(1:2, 35L)],
                      label = c(rep("x", 34L), paste("Table 14.3.2", strrep("y", len))),
                      level = 2L)
    sum(grepl("\\\\pagebb", .tt_paras(.tt_asm(f[1:2], toc = toc, toc_style = hang)))) + 1L
  }
  expect_identical(pages_of(92L), 1L)
  expect_identical(pages_of(93L), 2L)
  # a Listing number splits too; the option sets the style as the argument does
  old <- options(rtfreporter.toc_style = hang)
  on.exit(options(old), add = TRUE)
  x <- .tt_asm(f[1:2], toc = c("Listing 16.2.1 Subjects", "Table 9.1 Disposition"))
  expect_identical(.tt_labels(x), c("Listing 16.2.1\\tab Subjects", "Table 9.1\\tab Disposition"))
  expect_true(all(grepl("^\\\\li1880\\\\fi-1880\\\\ri720\\\\keep\\\\fs20\\\\tx1880\\\\tx2280\\\\tx2680\\\\tqr",
                        .tl_props(.tt_entries(x)))))
  options(old)
  # a hanging that is not TRUE, FALSE or a positive whole number, or one under
  # another key, stops naming it
  expect_error(.tt_asm(f, toc = toc, toc_style = list(entry = list(hanging = "yes"))),
               "`toc_style\\$entry\\$hanging` must be TRUE, FALSE or a positive whole number of twips")
  expect_error(.tt_asm(f, toc = toc, toc_style = list(heading = list(hanging = TRUE))),
               "`toc_style\\$heading` has unknown key\\(s\\) 'hanging'")
})

test_that("FT-AS7303 [FR-AS265 FR-AS125 FR-AS130]: the two Word-check books (data-raw/gen_toc_word_check.R) have a PAGEREF and a HYPERLINK to an existing bookmark on every entry, \\ri720\\keep on every entry, cached pages that rise from 4 to 121 with 1, 2 and 3 digits, and no page-number command", {
  for (b in c("default", "hanging")) {
    x <- readLines(test_path("fixtures", paste0("toc-word-check-", b, ".rtf")), warn = FALSE)
    e <- .tt_entries(x)
    expect_length(e, 36L)
    bm <- sub("^\\{\\\\\\*\\\\bkmkstart ([^}]+)\\}.*$", "\\1",
              regmatches(x, regexpr("\\{\\\\\\*\\\\bkmkstart [^}]+\\}", x)))
    ref <- sub(".*PAGEREF ([^ ]+) .*", "\\1", e)
    lnk <- sub('.*HYPERLINK \\\\\\\\l "([^"]+)".*', "\\1", e)
    expect_identical(lnk, ref)
    expect_true(all(ref %in% bm))
    expect_true(all(grepl("\\\\ri720\\\\keep\\\\fs20", e)))
    pg <- .tt_pages(x)
    expect_identical(range(pg), c(4L, 121L))
    expect_false(is.unsorted(pg, strictly = TRUE))
    expect_identical(sort(unique(nchar(pg))), 1:3)
    expect_false(any(grepl("\\\\pgnrestart|\\\\pgnlcrm|\\\\pgndec", x)))
    expect_identical(sum(grepl("\\\\pagebb", e)), 1L)            # a TOC of two pages
    if (b == "hanging") {
      expect_true(all(grepl("\\\\fi-1880\\\\ri720\\\\keep\\\\fs20\\\\tx[0-9]+\\\\tx[0-9]+\\\\tx[0-9]+\\\\tqr", e)))
      expect_true(all(grepl("\\tab ", .tt_labels(x), fixed = TRUE)))
    } else {
      expect_false(any(grepl("\\\\fi-", e)))
    }
  }
})
