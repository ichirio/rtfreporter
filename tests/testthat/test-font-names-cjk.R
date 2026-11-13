# A font family name that is not ASCII is written as Word writes it (#1047):
# the bytes of its East Asian code page as \'xx, the matching \fcharset, and
# {\*\falt <Latin name>} for a common family.

.cjk_df <- function() data.frame(a = c("x", "y"), b = c("1", "2"),
                                 stringsAsFactors = FALSE)
.cjk_ftbl <- function(doc) {
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(rtf_tables(doc, rtftable(.cjk_df())), f, overwrite = TRUE)
  l <- readLines(f, warn = FALSE)
  grep("fonttbl", l, value = TRUE)[1L]
}
.cjk_doc <- function(name, ...) {
  rtf_document(font_table = list(list(name = name)), ...)
}

test_that("FT-TB8001 [FR-DC385]: Yu Mincho in Japanese is written in code page 932 with \\fcharset128 and \\falt Yu Mincho", {
  expect_identical(.cjk_ftbl(.cjk_doc("\u6e38\u660e\u671d")),
    "{\\fonttbl{\\f0\\fnil\\fcharset128 \\'9f\\'e0\\'96\\'be\\'92\\'a9{\\*\\falt Yu Mincho};}}")
})

test_that("FT-TB8002 [FR-DC385]: MS Mincho in fullwidth letters keeps its ASCII space and gets \\falt MS Mincho", {
  expect_identical(.cjk_ftbl(.cjk_doc("\uff2d\uff33 \u660e\u671d")),
    "{\\fonttbl{\\f0\\fnil\\fcharset128 \\'82\\'6c\\'82\\'72 \\'96\\'be\\'92\\'a9{\\*\\falt MS Mincho};}}")
})

test_that("FT-TB8003 [FR-DC385]: a Simplified Chinese and a Korean family take their own charset and code page", {
  expect_identical(.cjk_ftbl(.cjk_doc("\u5b8b\u4f53")),
    "{\\fonttbl{\\f0\\fnil\\fcharset134 \\'cb\\'ce\\'cc\\'e5{\\*\\falt SimSun};}}")
  expect_identical(.cjk_ftbl(.cjk_doc("\ub9d1\uc740 \uace0\ub515")),
    "{\\fonttbl{\\f0\\fnil\\fcharset129 \\'b8\\'bc\\'c0\\'ba \\'b0\\'ed\\'b5\\'f1{\\*\\falt Malgun Gothic};}}")
  # an unknown Simplified Chinese family no Japanese code page writes
  expect_identical(.cjk_ftbl(.cjk_doc("\u534e\u6587\u6977\u4f53")),
    "{\\fonttbl{\\f0\\fnil\\fcharset134 \\'bb\\'aa\\'ce\\'c4\\'bf\\'ac\\'cc\\'e5;}}")
})

test_that("FT-TB8004 [FR-DC385]: an unknown Japanese family is written in code page 932 without \\falt", {
  expect_identical(.cjk_ftbl(.cjk_doc("\u30d2\u30e9\u30ae\u30ce\u660e\u671d")),
    "{\\fonttbl{\\f0\\fnil\\fcharset128 \\'83\\'71\\'83\\'89\\'83\\'4d\\'83\\'6d\\'96\\'be\\'92\\'a9;}}")
  # Han characters only: Japanese first, else the document's east_asian language
  expect_identical(.cjk_ftbl(.cjk_doc("\u660e\u671d")),
    "{\\fonttbl{\\f0\\fnil\\fcharset128 \\'96\\'be\\'92\\'a9;}}")
  expect_identical(.cjk_ftbl(.cjk_doc("\u660e\u671d", font_lang = c(east_asian = "zh-Hant"))),
    "{\\fonttbl{\\f0\\fnil\\fcharset136 \\'a9\\'fa\\'b4\\'c2;}}")
})

test_that("FT-TB8005 [FR-DC385]: an ASCII family and a name no East Asian code page writes are written as before", {
  expect_identical(.cjk_ftbl(.cjk_doc("Arial")),
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Arial;}}")
  expect_identical(.cjk_ftbl(.cjk_doc("Caf\u00e9 Sans")),
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Caf\\u233? Sans;}}")
})

test_that("FT-TB8006 [FR-DC385]: a slot tagged Japanese by font_lang is written in code page 932 and selected with \\dbch\\af", {
  doc <- rtf_document(font_table = list(list(name = c(
    latin = "Times New Roman", east_asian = "\u6e38\u660e\u671d"))),
    font_lang = c(east_asian = "ja"))
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(rtf_tables(doc, rtftable(.cjk_df())), f, overwrite = TRUE)
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(txt, paste0("{\\fonttbl{\\f0\\fnil\\fcharset0 Times New Roman;}",
                           "{\\f1\\fnil\\fcharset128 \\'9f\\'e0\\'96\\'be\\'92\\'a9",
                           "{\\*\\falt Yu Mincho};}}"), fixed = TRUE)
  expect_match(txt, "\\dbch\\af1\\langfe1041", fixed = TRUE)
})
