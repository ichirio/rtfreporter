# Fonts per script slot (latin / east_asian / complex / hansi) and font_lang.

.run_slots <- function(doc) {
  d <- tempfile("font-slots-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "out.rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}
.line_of <- function(l, pat) grep(pat, l, value = TRUE, fixed = TRUE)
.fonttbl  <- function(l) grep("fonttbl", l, value = TRUE)[1L]

.ja <- c(latin = "Times New Roman", east_asian = "MS Mincho")
# "\u5e74\u9f62" is written \u24180?\u40802?
.ja_df <- function() data.frame(a = c("Age", "\u5e74\u9f62"), b = c("1", "2"),
                                stringsAsFactors = FALSE)

# The documents the byte-identity fixtures were written from (by the code
# before font slots existed).  Regenerate the fixtures only when a deliberate
# change elsewhere alters this output.
.slot_base_doc <- function(font = NULL, tfont = NULL, hfont = NULL,
                           wm = NULL, tf = "table") {
  df <- data.frame(a = c("x", "\u65e5\u672c\u8a9e"), b = c("1", "caf\u00e9"),
                   stringsAsFactors = FALSE)
  rtf_document(watermark = wm,
               default_format = rtf_default_format(title_format = tf)) |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(rows = list(c(l = "HDR \u65e5", r = "Page {PAGE}")),
                          font = hfont),
      footer = rtf_footer(rows = list(c(l = "FTR")), font = hfont))) |>
    # (equal widths, as the fixtures were written before default_col_widths())
    rtf_tables(rtftable(df, border = "tfl", font = font, col_rel_width = c(1, 1))) |>
    rtf_titles(list("Title \u8868"), font = tfont) |>
    rtf_footnotes(list("Foot"), font = tfont)
}

# ---- single strings: byte for byte what they always wrote --------------------

test_that("a single-string font writes exactly the pre-slot RTF", {
  fx <- function(n) readLines(test_path("fixtures", "font-slots", n), warn = FALSE)
  expect_identical(.run_slots(.slot_base_doc()), fx("default.rtf"))
  expect_identical(
    .run_slots(.slot_base_doc(font = "Arial", tfont = "Calibri",
                              hfont = "Times New Roman",
                              wm = rtf_watermark("DRAFT", font = "Arial"),
                              tf = "text")),
    fx("single-strings.rtf"))
})

test_that("options(rtfreporter.font = \"Arial\") writes exactly the pre-slot RTF", {
  old <- options(rtfreporter.font = "Arial")
  on.exit(options(old), add = TRUE)
  expect_identical(.run_slots(.slot_base_doc(font = "Times New Roman")),
                   readLines(test_path("fixtures", "font-slots",
                                       "option-arial.rtf"), warn = FALSE))
})

test_that("c(latin = ) alone is that single string", {
  expect_identical(rtfreporter:::.check_font(c(latin = "Arial"), "font"), "Arial")
})

# ---- validation --------------------------------------------------------------

test_that("bad slot vectors are errors with a clear message", {
  expect_error(rtftable(.ja_df(), font = c(latin = "A", asian = "B")),
               "unknown font slot.*\"asian\"")
  expect_error(rtftable(.ja_df(), font = c(east_asian = "MS Mincho")),
               "must name a `latin` font")
  expect_error(rtftable(.ja_df(), font = c(latin = "A", east_asian = "")),
               "font family")
  expect_error(rtftable(.ja_df(), font = c(latin = "A", "B")),
               "named by its slot")
  expect_error(rtftable(.ja_df(), font = c(latin = "A", latin = "B")),
               "only once")
  expect_error(rtf_header(list(c(l = "x")), font = c(complex = "Arial")),
               "`latin`")
  expect_error(rtf_document() |> rtf_tables(rtftable(.ja_df())) |>
                 rtf_titles(list("T"), font = c(foo = "x")),
               "unknown font slot")
  expect_error(rtf_watermark("X", font = c(latin = "A", bad = "B")),
               "unknown font slot")
  expect_error(rtf_document(font_table = list(list(name = c(hansi = "A")))),
               "`latin`")
  old <- options(rtfreporter.font = c(east_asian = "MS Mincho"))
  on.exit(options(old), add = TRUE)
  expect_error(rtf_document(), "rtfreporter.font")
})

test_that("bad font_lang is an error", {
  expect_error(rtf_document(font_lang = "ja"), "named character vector")
  expect_error(rtf_document(font_lang = c(east_asian = "jp")),
               "unknown language.*\"jp\"")
  expect_error(rtf_document(font_lang = c(hansi = "ja")), "distinct slots")
  expect_error(rtf_document(font_lang = c(east_asian = "ja", east_asian = "ko")),
               "distinct slots")
})

# ---- the font table ----------------------------------------------------------

test_that("named fonts get their own entries, with the slot's charset", {
  l <- .run_slots(rtf_document(font_lang = c(east_asian = "ja")) |>
                    rtf_tables(rtftable(.ja_df(), border = "tfl", font = .ja)))
  expect_equal(.fonttbl(l), paste0(
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}",
    "{\\f1\\fnil\\fcharset0 Times New Roman;}",
    "{\\f2\\fnil\\fcharset128 MS Mincho;}}"))
})

test_that("without font_lang every entry stays ANSI", {
  l <- .run_slots(rtf_document() |>
                    rtf_tables(rtftable(.ja_df(), border = "tfl", font = .ja)))
  expect_equal(.fonttbl(l), paste0(
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}",
    "{\\f1\\fnil\\fcharset0 Times New Roman;}",
    "{\\f2\\fnil\\fcharset0 MS Mincho;}}"))
  # and no language word is written anywhere
  expect_false(any(grepl("\\\\(lang|langfe|alang|deflangfe)[0-9]", l[-1L])))
})

test_that("every language maps to its charset", {
  cs <- c("ja" = 128, "zh-Hans" = 134, "zh-Hant" = 136, "ko" = 129)
  for (lg in names(cs)) {
    l <- .run_slots(rtf_document(font_lang = c(east_asian = lg)) |>
                      rtf_tables(rtftable(.ja_df(), font = .ja)))
    expect_match(.fonttbl(l), sprintf("\\fcharset%d MS Mincho;", cs[[lg]]),
                 fixed = TRUE)
  }
  l <- .run_slots(rtf_document(font_lang = c(complex = "he")) |>
                    rtf_tables(rtftable(.ja_df(),
                                        font = c(latin = "Arial",
                                                 complex = "David"))))
  expect_match(.fonttbl(l), "\\fcharset177 David;", fixed = TRUE)
})

test_that("one family in two slots with different charsets is two entries", {
  l <- .run_slots(rtf_document(font_lang = c(east_asian = "ja")) |>
                    rtf_tables(rtftable(.ja_df(),
                                        font = c(latin = "MS Gothic",
                                                 east_asian = "MS Gothic"))))
  expect_match(.fonttbl(l), "{\\f1\\fnil\\fcharset0 MS Gothic;}", fixed = TRUE)
  expect_match(.fonttbl(l), "{\\f2\\fnil\\fcharset128 MS Gothic;}", fixed = TRUE)
  expect_match(.line_of(l, "\\u24180?"), "\\dbch\\af2", fixed = TRUE)
  expect_match(.line_of(l, "\\u24180?"), "\\loch\\f1", fixed = TRUE)
})

# ---- run-level words for Japanese text ----------------------------------------

.ja_run <- "\\rtlch\\af1\\ltrch\\hich\\af1\\dbch\\af2\\langfe1041\\loch\\f1"

test_that("a table cell carries the slot words", {
  l <- .run_slots(rtf_document(font_lang = c(east_asian = "ja")) |>
                    rtf_tables(rtftable(.ja_df(), border = "tfl", font = .ja)))
  expect_match(.line_of(l, "\\u24180?\\u40802?"),
               paste0(.ja_run, " \\u24180?\\u40802?\\cell"), fixed = TRUE)
})

test_that("a title, in either format, carries the slot words", {
  for (tf in c("text", "table")) {
    l <- .run_slots(
      rtf_document(font_lang = c(east_asian = "ja"),
                   default_format = rtf_default_format(title_format = tf)) |>
        rtf_tables(rtftable(.ja_df(), border = "tfl")) |>
        rtf_titles(list("\u8868 14.1"), font = .ja))
    expect_match(.line_of(l, "\\u34920? 14.1"), .ja_run, fixed = TRUE)
  }
})

test_that("a footnote carries the slot words", {
  l <- .run_slots(rtf_document(font_lang = c(east_asian = "ja")) |>
                    rtf_tables(rtftable(.ja_df(), border = "tfl")) |>
                    rtf_footnotes(list("\u6ce8 note"), font = .ja))
  expect_match(.line_of(l, "\\u27880? note"), .ja_run, fixed = TRUE)
})

test_that("a header and a footer band carry the slot words", {
  l <- .run_slots(
    rtf_document(font_lang = c(east_asian = "ja")) |>
      rtf_section(page = 1, secinfo = list(
        header = rtf_header(rows = list(c(l = "\u30d8\u30c3\u30c0")), font = .ja),
        footer = rtf_footer(rows = list(c(l = "\u30d5\u30c3\u30bf")), font = .ja))) |>
      rtf_tables(rtftable(.ja_df(), border = "tfl")))
  expect_match(.line_of(l, "{\\header"), paste0(.ja_run, " \\u12504?"),
               fixed = TRUE)
  expect_match(.line_of(l, "{\\footer"), paste0(.ja_run, " \\u12501?"),
               fixed = TRUE)
})

test_that("an element's missing slot takes the document default's", {
  l <- .run_slots(
    rtf_document(font_table = list(list(name = c(latin = "Courier New",
                                                 east_asian = "MS Gothic")))) |>
      rtf_tables(rtftable(.ja_df(), border = "tfl", font = "Arial")))
  expect_equal(.fonttbl(l), paste0(
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}",
    "{\\f1\\fnil\\fcharset0 MS Gothic;}{\\f2\\fnil\\fcharset0 Arial;}}"))
  expect_match(.line_of(l, "\\u24180?"),
               "\\rtlch\\af2\\ltrch\\hich\\af2\\dbch\\af1\\loch\\f2", fixed = TRUE)
})

test_that("hansi overrides latin for the high-ANSI class only", {
  l <- .run_slots(rtf_document() |>
                    rtf_tables(rtftable(.ja_df(),
                                        font = c(latin = "Arial",
                                                 hansi = "Tahoma"))))
  expect_match(.line_of(l, "\\u24180?"),
               "\\rtlch\\af1\\ltrch\\hich\\af2\\dbch\\af1\\loch\\f1", fixed = TRUE)
})

# ---- font_lang words and document defaults -------------------------------------

test_that("font_lang writes the language words of every slot", {
  l <- .run_slots(
    rtf_document(font_lang = c(latin = "en", east_asian = "ja",
                               complex = "ar")) |>
      rtf_tables(rtftable(.ja_df(), font = c(latin = "Arial",
                                             east_asian = "MS Gothic",
                                             complex = "Arial Unicode"))))
  expect_equal(l[1L], paste0("{\\rtf1\\ansi\\ansicpg1252\\deff0\\deflang1033",
                             "\\deflangfe1041\\adeflang1025\\uc1"))
  expect_match(.fonttbl(l), "\\fcharset178 Arial Unicode;", fixed = TRUE)
  expect_match(.line_of(l, "\\u24180?"), paste0(
    "\\rtlch\\af3\\alang1025\\ltrch\\hich\\af1\\dbch\\af2\\langfe1041",
    "\\loch\\f1\\lang1033\\langnp1033"), fixed = TRUE)
})

test_that("a slotted document default writes the document-level defaults", {
  l <- .run_slots(
    rtf_document(font_table = list(list(name = .ja)),
                 font_lang = c(east_asian = "ja")) |>
      rtf_section(page = 1, secinfo = list(
        header = rtf_header(rows = list(c(l = "\u30d8\u30c3\u30c0"))))) |>
      rtf_tables(rtftable(.ja_df(), border = "tfl")))
  expect_equal(l[1L], paste0(
    "{\\rtf1\\ansi\\ansicpg1252\\deff0\\deflang1033\\adeff0\\stshfdbch1",
    "\\stshfloch0\\stshfhich0\\stshfbi0\\deflangfe1041\\uc1"))
  # an element that names no font still selects the default's slots
  run0 <- "\\rtlch\\af0\\ltrch\\hich\\af0\\dbch\\af1\\langfe1041\\loch\\f0"
  expect_match(.line_of(l, "\\u24180?"), run0, fixed = TRUE)
  expect_match(.line_of(l, "{\\header"), run0, fixed = TRUE)
})

test_that("options(rtfreporter.font =, rtfreporter.font_lang =) set the default", {
  old <- options(rtfreporter.font = .ja,
                 rtfreporter.font_lang = c(east_asian = "ja"))
  on.exit(options(old), add = TRUE)
  l <- .run_slots(rtf_document() |> rtf_tables(rtftable(.ja_df())))
  expect_equal(.fonttbl(l), paste0(
    "{\\fonttbl{\\f0\\fnil\\fcharset0 Times New Roman;}",
    "{\\f1\\fnil\\fcharset128 MS Mincho;}}"))
  expect_match(l[1L], "\\stshfdbch1", fixed = TRUE)
})

test_that("rtf_config() sets and clears font_lang", {
  d <- rtf_document() |> rtf_tables(rtftable(.ja_df(), font = .ja))
  l <- .run_slots(rtf_config(d, font_lang = c(east_asian = "ko")))
  expect_match(l[1L], "\\deflangfe1042", fixed = TRUE)
  d2 <- rtf_config(rtf_config(d, font_lang = c(east_asian = "ko")),
                   font_lang = NA)
  expect_null(d2$document$font_lang)
  expect_error(rtf_config(d, font_lang = c(east_asian = "xx")),
               "unknown language")
})

# ---- watermark -------------------------------------------------------------------

test_that("a slotted watermark font draws in the family its text needs", {
  wf <- rtfreporter:::.watermark_family
  expect_equal(wf(.ja, "DRAFT"), "Times New Roman")
  expect_equal(wf(.ja, "\u793e\u5916\u79d8"), "MS Mincho")
  expect_equal(wf("Arial", "\u793e\u5916\u79d8"), "Arial")
  l <- .run_slots(rtf_document(watermark = rtf_watermark("\u793e\u5916\u79d8",
                                                         font = .ja)) |>
                    rtf_tables(rtftable(.ja_df())))
  expect_true(any(grepl("{\\sn gtextFont}{\\sv MS Mincho}", l, fixed = TRUE)))
})

# ---- width estimation --------------------------------------------------------------

test_that("an east_asian font counts wide characters as two columns", {
  mixed <- "\u65e5\u672c\u8a9eabc\uff21"     # 3 ideographs + abc + fullwidth A
  one   <- text_width_in("0", font = "Courier")
  expect_equal(text_width_in(mixed, font = "Courier"), 7 * one)
  expect_equal(text_width_in(mixed,
                             font = c(latin = "Courier", east_asian = "MS Gothic")),
               11 * one)
  # a slot vector with no east_asian slot measures as its latin font
  expect_equal(text_width_in(mixed, font = c(latin = "Courier",
                                             complex = "Arial")),
               7 * one)
  expect_equal(rtfreporter:::.ea_wide_count(
    c("abc", "\u3042\u30a2\uac00", "\u00e9", NA)), c(0L, 3L, 0L, 0L))
})

test_that("the option's east_asian slot reaches text_width_in() and listings", {
  old <- options(rtfreporter.font = c(latin = "Courier", east_asian = "MS Gothic"))
  on.exit(options(old), add = TRUE)
  one <- text_width_in("0")
  expect_equal(text_width_in("\u65e5\u672cab"), 6 * one)
  # the characters per line come from the latin font, as for "Courier"
  pg <- rtf_page(paper_size = "letter", orientation = "landscape")
  expect_identical(
    rtfreporter:::.listing_total_width(pg, NULL, 18L),
    rtfreporter:::.listing_total_width(pg, "Courier", 18L))
})

# ---- optional: LibreOffice picks the East Asian family -------------------------------

test_that("LibreOffice renders the east_asian slot in its family", {
  skip_on_cran()
  soffice <- Sys.which("soffice")
  pdffonts <- Sys.which("pdffonts")
  skip_if(!nzchar(soffice) || !nzchar(pdffonts), "LibreOffice / pdffonts absent")
  fams <- system2("fc-list", ":lang=ja family", stdout = TRUE, stderr = FALSE)
  skip_if(!length(fams), "no Japanese font installed")
  ja_font <- sub(",.*$", "", fams[[1L]])
  d <- tempfile("font-slots-lo-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "ja.rtf")
  generate_rtfreport(
    rtf_document(font_lang = c(east_asian = "ja")) |>
      rtf_tables(rtftable(.ja_df(), border = "tfl",
                          font = c(latin = "Courier", east_asian = ja_font))),
    f, overwrite = TRUE)
  # R's own LD_LIBRARY_PATH can hide LibreOffice's libraries from it
  suppressWarnings(system2(soffice, c("--headless", "--convert-to", "pdf",
                                      "--outdir", shQuote(d), shQuote(f)),
                           stdout = FALSE, stderr = FALSE,
                           env = "LD_LIBRARY_PATH=", timeout = 120))
  pdf <- file.path(d, "ja.pdf")
  skip_if(!file.exists(pdf), "LibreOffice did not convert")
  used <- system2(pdffonts, shQuote(pdf), stdout = TRUE)
  expect_true(any(grepl(gsub("[^A-Za-z0-9]", "", ja_font),
                        gsub("[^A-Za-z0-9]", "", used), fixed = TRUE)))
})
