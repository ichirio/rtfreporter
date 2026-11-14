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

# ---- the files checked in Word, rebuilt byte for byte (helper-word-checked.R) ----

test_that("FT-TB8007 [FR-DC385]: the Yu Mincho file checked in Word (the family named in Japanese) is rebuilt byte for byte (fixtures/fonts/font-ja-yumincho-jpname.rtf)", {
  d <- tempfile("wck-"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .wck_write(.wck_font_doc("\u6e38\u660e\u671d"), d, "out.rtf")
  expect_identical(.wck_bytes(f),
                   .wck_bytes(test_path("fixtures", "fonts", "font-ja-yumincho-jpname.rtf")))
})

test_that("FT-TB8008 [FR-DC385]: the MS Mincho file checked in Word (the family named in Japanese) is rebuilt byte for byte (fixtures/fonts/font-ja-msmincho-jpname.rtf)", {
  d <- tempfile("wck-"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .wck_write(.wck_font_doc("\uff2d\uff33 \u660e\u671d"), d, "out.rtf")
  expect_identical(.wck_bytes(f),
                   .wck_bytes(test_path("fixtures", "fonts", "font-ja-msmincho-jpname.rtf")))
})

test_that("FT-TB8009 [FR-DC103 FR-DC121]: a Latin and a Japanese slot tagged ja are rebuilt byte for byte (fixtures/fonts/font-slots-ja-latin.rtf), Japanese runs selecting the east_asian entry and Latin runs the latin one", {
  d <- tempfile("wck-"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .wck_write(.wck_slot_doc(), d, "out.rtf")
  fx <- test_path("fixtures", "fonts", "font-slots-ja-latin.rtf")
  expect_identical(.wck_bytes(f), .wck_bytes(fx))
  l <- readLines(fx, warn = FALSE)
  expect_identical(l[2L], "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}{\\f1\\fnil\\fcharset128 MS Gothic;}}")
  run <- "\\rtlch\\af0\\ltrch\\hich\\af0\\dbch\\af1\\langfe1041\\loch\\f0 "
  expect_match(grep("Latin text 0123", l, value = TRUE, fixed = TRUE),
               paste0(run, "Latin text 0123\\cell"), fixed = TRUE)
  expect_match(grep("\\u26085?", l, value = TRUE, fixed = TRUE),
               paste0(run, "\\u26085?\\u26412?\\u-30050?\\u12398?\\u-30644?\\cell"), fixed = TRUE)
})

# ---- LibreOffice reads the font names (only where soffice is on PATH) --------------

test_that("FT-TB8010 [FR-DC385]: LibreOffice reads the Japanese family names of the Word-checked files as written: the face and the runs' font are exactly the Japanese name", {
  fx <- test_path("fixtures", "fonts", c("font-ja-yumincho-jpname.rtf",
                                          "font-ja-msmincho-jpname.rtf"))
  x <- .wck_lo_fodt(fx)
  fams <- c("font-ja-yumincho-jpname.rtf" = "\u6e38\u660e\u671d",
            "font-ja-msmincho-jpname.rtf" = "\uff2d\uff33 \u660e\u671d")
  for (b in names(fams)) {
    expect_true(fams[[b]] %in% .wck_families(x[[b]]), info = b)
    # the Japanese, the Latin and the digit runs: the face name LibreOffice
    # parsed (which font draws the Japanese is LibreOffice's own choice)
    for (txt in c("\u65e5\u672c\u8a9e\u306e\u6587\u5b57", "ASCII text 0123", "ABCDE")) {
      expect_identical(.wck_run_font(x[[b]], txt), fams[[b]], info = paste(b, txt))
    }
  }
})

test_that("FT-TB8011 [FR-DC103 FR-DC121]: LibreOffice reads the slots: the Latin runs in Courier New, the Japanese runs' Asian font MS Gothic", {
  x <- .wck_lo_fodt(test_path("fixtures", "fonts", "font-slots-ja-latin.rtf"))[[1L]]
  expect_identical(.wck_run_font(x, "Latin text 0123"), "Courier New")
  expect_identical(.wck_run_font(x, "ABCDE"), "Courier New")
  expect_identical(.wck_run_font(x, "\u65e5\u672c\u8a9e\u306e\u884c", "style:font-name-asian"),
                   "MS Gothic")
  expect_identical(.wck_run_font(x, "\u3042\u3044\u3046\u3048\u304a", "style:font-name-asian"),
                   "MS Gothic")
})
