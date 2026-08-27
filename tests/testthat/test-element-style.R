# Tests for per-element style: font size, row height, markup, alignment (#292).

.df   <- function() data.frame(a = c("x", "y"), b = c("1", "2"),
                               stringsAsFactors = FALSE)
.W    <- 9360L

# The \fs / \trrh / \q.. of the rows carrying `text` in a rendered document.
.peek <- function(doc, text) {
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  h <- grep(text, readLines(f, warn = FALSE), value = TRUE, fixed = TRUE)
  list(fs    = unique(unlist(regmatches(h, gregexpr("\\\\fs[0-9]+", h)))),
       trrh  = unique(unlist(regmatches(h, gregexpr("\\\\trrh-?[0-9]+", h)))),
       align = unique(unlist(regmatches(h, gregexpr("\\\\q[lrc]", h)))),
       super = any(grepl("\\\\super", h)))
}

# ──────── the resolver: size and height move together ──────────────────────

test_that("nothing set -> the document size and its implied height", {
  m <- rtfreporter:::.resolve_element_metrics(NULL, NULL, 18L, NULL)
  expect_equal(m$fs, 18L)
  expect_equal(m$rh, rtfreporter:::.default_row_height_twips(18L))
})

test_that("an element font size drags the row height with it", {
  m <- rtfreporter:::.resolve_element_metrics(24L, NULL, 18L, NULL)
  expect_equal(m$fs, 24L)
  expect_equal(m$rh, rtfreporter:::.default_row_height_twips(24L))
})

test_that("a document row height does NOT survive an element font size", {
  # the document height was chosen for the document size; the element changed it
  m <- rtfreporter:::.resolve_element_metrics(24L, NULL, 18L, 500L)
  expect_equal(m$rh, rtfreporter:::.default_row_height_twips(24L))
  expect_false(m$rh == 500L)
})

test_that("an explicit element row height always wins", {
  m <- rtfreporter:::.resolve_element_metrics(24L, 400L, 18L, 500L)
  expect_equal(m$fs, 24L)
  expect_equal(m$rh, 400L)
})

test_that("with no element size the document height is inherited", {
  m <- rtfreporter:::.resolve_element_metrics(NULL, NULL, 18L, 500L)
  expect_equal(m$fs, 18L)
  expect_equal(m$rh, 500L)
})

test_that(".fs_cmd_for stays silent when the size matches the document", {
  expect_equal(rtfreporter:::.fs_cmd_for(18L, 18L), "")
  expect_equal(rtfreporter:::.fs_cmd_for(24L, 18L), "\\fs24")
})

test_that("the style checkers reject nonsense", {
  expect_error(rtfreporter:::.check_font_size(0, "fs"), "positive")
  expect_error(rtfreporter:::.check_font_size(c(1, 2), "fs"), "positive")
  expect_error(rtfreporter:::.check_row_height(-1, "rh"), "non-negative")
  expect_error(rtfreporter:::.check_align("middle", "al"), "left")
  expect_null(rtfreporter:::.check_font_size(NULL, "fs"))
})

# ──────── the table body ───────────────────────────────────────────────────

.tbl_metrics <- function(tbl, doc_fs = 18L, doc_rh = NULL) {
  o <- rtfreporter:::.render_rtftable(tbl, .W, font_half_points = doc_fs,
                                      doc_row_height = doc_rh)
  list(fs   = unique(unlist(regmatches(o, gregexpr("\\\\fs[0-9]+", o)))),
       trrh = unique(unlist(regmatches(o, gregexpr("\\\\trrh-?[0-9]+", o)))))
}

test_that("a table with no font size renders exactly as before", {
  m <- .tbl_metrics(rtftable(.df(), border = "none"))
  expect_length(m$fs, 0L)                     # nothing emitted
  expect_equal(m$trrh, "\\trrh230")
})

test_that("rtftable(font_size_half_points = ) sets size and height", {
  m <- .tbl_metrics(rtftable(.df(), border = "none",
                             font_size_half_points = 24L))
  expect_equal(m$fs, "\\fs24")
  expect_equal(m$trrh,
               paste0("\\trrh", rtfreporter:::.default_row_height_twips(24L)))
})

test_that("an explicit table row height beats the font-implied one", {
  m <- .tbl_metrics(rtftable(.df(), border = "none",
                             font_size_half_points = 24L,
                             row_height_twips = 400L))
  expect_equal(m$trrh, "\\trrh400")
})

test_that("the document row height is dropped when the table sets a size", {
  m <- .tbl_metrics(rtftable(.df(), border = "none",
                             font_size_half_points = 24L), doc_rh = 500L)
  expect_equal(m$trrh,
               paste0("\\trrh", rtfreporter:::.default_row_height_twips(24L)))
})

test_that("row_height_twips = 0 still means automatic", {
  m <- .tbl_metrics(rtftable(.df(), border = "none", row_height_twips = 0L))
  expect_length(m$trrh, 0L)                   # no \trrh at all
})

test_that("rtftable() validates the font size", {
  expect_error(rtftable(.df(), font_size_half_points = 0), "positive")
})

# ──────── the title / footnote blocks ──────────────────────────────────────

.doc <- function(...) {
  rtf_document(default_format = rtf_default_format(title_format = "table")) |>
    rtf_tables(rtftable(.df(), border = "tfl")) |>
    rtf_titles(list("MYTITLE"), ...)
}

test_that("the blocks are unchanged when nothing is set", {
  p <- .peek(.doc() |> rtf_footnotes(list("MYFOOT")), "MYTITLE")
  expect_length(p$fs, 0L)
  expect_equal(p$align, "\\qc")               # the title default
  q <- .peek(.doc() |> rtf_footnotes(list("MYFOOT")), "MYFOOT")
  expect_equal(q$align, "\\ql")               # the footnote default
})

test_that("rtf_titles(font_size_half_points = ) sizes the block and its rows", {
  p <- .peek(.doc(font_size_half_points = 28L) |> rtf_footnotes(list("MYFOOT")),
             "MYTITLE")
  expect_equal(p$fs, "\\fs28")
  expect_equal(p$trrh,
               paste0("\\trrh", rtfreporter:::.default_row_height_twips(28L)))
})

test_that("an explicit block row height wins there too", {
  p <- .peek(.doc(font_size_half_points = 28L, row_height_twips = 500L) |>
               rtf_footnotes(list("MYFOOT")), "MYTITLE")
  expect_equal(p$trrh, "\\trrh500")
})

test_that("rtf_footnotes() takes the same arguments", {
  d <- .doc() |> rtf_footnotes(list("MYFOOT"), font_size_half_points = 14L,
                               align = "center")
  q <- .peek(d, "MYFOOT")
  expect_equal(q$fs, "\\fs14")
  expect_equal(q$align, "\\qc")
  expect_equal(q$trrh,
               paste0("\\trrh", rtfreporter:::.default_row_height_twips(14L)))
})

test_that("align changes only the block asked for", {
  d <- .doc() |> rtf_footnotes(list("MYFOOT"), align = "right")
  expect_equal(.peek(d, "MYFOOT")$align,  "\\qr")
  expect_equal(.peek(d, "MYTITLE")$align, "\\qc")   # untouched
})

test_that("a row's own l / c / r slot beats the block default", {
  # Per-row style lists are gone (#296); a row places itself by naming a slot.
  d <- .doc() |> rtf_footnotes(list(list(c(r = "MYFOOT"))), align = "center")
  expect_equal(.peek(d, "MYFOOT")$align, "\\qr")
})

test_that("markup can be turned on for one block only", {
  d <- rtf_document(default_format = rtf_default_format(markup = "none")) |>
    rtf_tables(rtftable(.df(), border = "tfl")) |>
    rtf_footnotes(list("^{a} MYFOOT"), markup = "script")
  expect_true(.peek(d, "MYFOOT")$super)
})

test_that("the block style survives the text (paragraph) form", {
  d <- rtf_document(
      default_format = rtf_default_format(footnote_format = "text")) |>
    rtf_tables(rtftable(.df(), border = "tfl")) |>
    rtf_footnotes(list("MYFOOT"), font_size_half_points = 14L,
                  align = "center")
  p <- .peek(d, "MYFOOT")
  expect_equal(p$fs, "\\fs14")
  expect_equal(p$align, "\\qc")
})

test_that("rtf_titles() / rtf_footnotes() validate their style", {
  d <- rtf_document() |> rtf_tables(rtftable(.df()))
  expect_error(rtf_titles(d, list("t"), font_size_half_points = -1), "positive")
  expect_error(rtf_footnotes(d, list("f"), align = "middle"), "left")
})

# ──────── the header / footer band ─────────────────────────────────────────

.hdr <- function(...) {
  d <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = rtf_header(
      rows = list(c(l = "MYHDR")), ...))) |>
    rtf_tables(rtftable(.df(), border = "tfl"))
  .peek(d, "MYHDR")
}

test_that("the band takes a font size, and its height follows", {
  p <- .hdr(font_size_half_points = 28L)
  expect_true("\\fs28" %in% p$fs)
  expect_equal(p$trrh,
               paste0("\\trrh", rtfreporter:::.default_row_height_twips(28L)))
})

test_that("an explicit band row height wins", {
  expect_equal(.hdr(font_size_half_points = 28L, row_height_twips = 450L)$trrh,
               "\\trrh450")
})

test_that("rtf_footer() carries the same fields", {
  ft <- rtf_footer(list(c(l = "x")), font_size_half_points = 20L,
                   markup = "none")
  expect_equal(ft$font_size_half_points, 20L)
  expect_equal(ft$markup, character(0))       # "none" resolves to no tokens
})

# ──────── nothing set, nothing changes ─────────────────────────────────────

test_that("a document with no per-element style renders byte-identically", {
  mk <- function(styled) {
    d <- rtf_document() |> rtf_tables(rtftable(.df(), border = "tfl"))
    d <- if (styled) rtf_titles(d, list("MYTITLE")) else rtf_titles(d, list("MYTITLE"))
    d <- rtf_footnotes(d, list("MYFOOT"))
    f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
    generate_rtfreport(d, f, overwrite = TRUE)
    readLines(f, warn = FALSE)
  }
  expect_identical(mk(FALSE), mk(TRUE))
})

# ──────── rtf_tables() overrides the table's typography (#299) ─────────────

.ov <- function(...) {
  d <- rtf_document() |> rtf_tables(...)
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(d, f, overwrite = TRUE)
  h <- grep("Mean", readLines(f, warn = FALSE), value = TRUE)
  list(f    = unique(unlist(regmatches(h, gregexpr("\\\\f[0-9]+(?![a-z0-9])", h,
                                                   perl = TRUE)))),
       fs   = unique(unlist(regmatches(h, gregexpr("\\\\fs[0-9]+", h)))),
       trrh = unique(unlist(regmatches(h, gregexpr("\\\\trrh-?[0-9]+", h)))))
}
.ovdf <- function() data.frame(Parameter = c("Age", "  Mean"), A = c("", "45.2"),
                               stringsAsFactors = FALSE)

test_that("rtf_tables(font_size_half_points = ) reaches the table", {
  o <- .ov(rtftable(.ovdf(), border = "tfl"), font_size_half_points = 14L)
  expect_equal(o$fs, "\\fs14")
  expect_equal(o$trrh,
               paste0("\\trrh", rtfreporter:::.default_row_height_twips(14L)))
})

test_that("rtf_tables(font = ) reaches the table", {
  expect_equal(.ov(rtftable(.ovdf(), border = "tfl"), font = "Arial")$f, "\\f1")
})

test_that("the rtf_tables() value wins over the table's own", {
  o <- .ov(rtftable(.ovdf(), border = "tfl", font_size_half_points = 24L),
           font_size_half_points = 14L)
  expect_equal(o$fs, "\\fs14")
})

test_that("the table's own value survives when nothing overrides it", {
  o <- .ov(rtftable(.ovdf(), border = "tfl", font_size_half_points = 24L))
  expect_equal(o$fs, "\\fs24")
})

test_that("an explicit row height still wins at the rtf_tables() level", {
  o <- .ov(rtftable(.ovdf(), border = "tfl"),
           font_size_half_points = 14L, row_height_twips = 300L)
  expect_equal(o$fs, "\\fs14")
  expect_equal(o$trrh, "\\trrh300")
})

test_that("the override reaches every page of an as_rtftables() list", {
  pages <- as_rtftables(.ovdf(), border = "tfl")
  d <- rtf_document() |> rtf_tables(pages, font_size_half_points = 14L)
  expect_true(all(vapply(d$contents,
                         function(p) identical(p$font_size_half_points, 14L),
                         logical(1L))))
})

test_that("rtf_tables() validates them like everything else", {
  d <- rtf_document()
  expect_error(rtf_tables(d, rtftable(.ovdf()), font = 1), "font family")
  expect_error(rtf_tables(d, rtftable(.ovdf()), font_size_half_points = 0),
               "positive")
})
