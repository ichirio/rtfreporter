# rtf_border and rtf_table_style (all S3): construction, derivation, sharing.

test_that("rtf_border is a plain S3 list with class 'rtf_border'", {
  b <- rtf_border(top = TRUE)
  expect_s3_class(b, "rtf_border")
  expect_false(inherits(b, "R6"))
  expect_false(is.null(b$top))
  expect_null(b$bottom)
})

test_that("a border is a plain record, so building another leaves it alone", {
  base    <- rtf_border(top = TRUE)
  derived <- rtf_border(top = TRUE, bottom = rtf_border_side("dot"))
  expect_null(base$bottom)
  expect_identical(derived$bottom$style, "dot")
  expect_identical(derived$top$style,    "single")
})

test_that("sides can differ, and a NULL side stays unset", {
  two <- rtf_border(top    = TRUE,
                    bottom = rtf_border_side("double", 20L),
                    left   = NULL)
  expect_identical(two$top$style,    "single")
  expect_identical(two$bottom$style, "double")
  expect_identical(two$bottom$width, 20L)
  expect_null(two$left)
})

test_that("rtf_border() validates its sides", {
  # A side takes TRUE / FALSE / a style name; anything else names the choices.
  expect_error(rtf_border(top = "not-a-side"), "must be one of")
  expect_error(rtf_border(top = list(1)),      "TRUE, FALSE, a border style")
})

test_that("rtf_border_side() validates style, width, and color", {
  expect_error(rtf_border_side(style = "nope"), "should be one of")
  expect_error(rtf_border_side(width = 0L),     "positive integer")
  expect_error(rtf_border_side(color = "red"),  "6-digit hex")
})

test_that("rtf_table_style is S3 with class 'rtf_table_style'", {
  sty <- rtf_table_style_tfl()
  expect_s3_class(sty, "rtf_table_style")
  expect_false(inherits(sty, "R6"))
  expect_false(sty$header_bold)
})

test_that("rtf_table_style_with() returns a copy with selected fields replaced", {
  sty  <- rtf_table_style_tfl()
  sty2 <- rtf_table_style_with(sty, header_bold = TRUE)
  expect_true(sty2$header_bold)
  expect_false(sty$header_bold)
})

test_that("rtf_table_style_with() rejects unknown field names", {
  sty <- rtf_table_style_tfl()
  expect_error(rtf_table_style_with(sty, nonexistent = 1L),
               "Unknown style field")
})

test_that("style values are snapshotted into each rtftable at construction", {
  sty <- rtf_table_style_tfl()
  df  <- data.frame(A = 1:2, B = c("x", "y"), stringsAsFactors = FALSE)

  t1       <- rtftable(df, style = sty)
  sty_bold <- rtf_table_style_with(sty, header_bold = TRUE)
  t2       <- rtftable(df, style = sty_bold)

  expect_false(t1$col_spec[[1L]]$header_bold)
  expect_true (t2$col_spec[[1L]]$header_bold)
})

test_that("col_spec[[j]]$border applies per-column in the header row", {
  df  <- data.frame(L = "x", N = 1L, V = 2.5)
  tbl <- rtftable(df,
    border = rtfreporter:::.rtf_table_border(header = rtf_border(top = TRUE)),
    col_spec = list(
      list(col = 2, border = rtf_border(top    = rtf_border_side("double", 20L),
                                         bottom = rtf_border_side("double", 20L)))
    ))
  expect_s3_class(tbl$col_spec[[2L]]$border, "rtf_border")
  expect_identical(tbl$col_spec[[2L]]$border$bottom$style, "double")

  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(tbl))
  txt <- .render_to_string(doc)
  expect_match(txt, "\\\\brdrdb")   # \brdrdb = RTF "double" border command
})

test_that("border = 'tfl' produces an rtf_table_border with header-only zones", {
  tbl <- rtftable(data.frame(A = 1), border = "tfl")
  expect_s3_class(tbl$border, "rtf_table_border")
  expect_s3_class(tbl$border$header, "rtf_border")
})

# ──────── Print methods (rtf_border_side / rtf_border / rtf_table_border) ─

test_that("print.rtf_border_side renders style + width + colour", {
  b <- rtf_border_side("double", width = 20L, color = "#FF0000")
  txt <- capture.output(print(b))
  expect_match(paste(txt, collapse = "\n"), "double")
  expect_match(paste(txt, collapse = "\n"), "20 twips")
  expect_match(paste(txt, collapse = "\n"), "#FF0000")
  # No-colour variant: must not print colour string.
  b2 <- rtf_border_side("single")
  expect_false(grepl("color", paste(capture.output(print(b2)), collapse = "")))
})

test_that("print.rtf_border lists all four sides (some none, some set)", {
  b <- rtf_border(top    = rtf_border_side("single"),
                  bottom = rtf_border_side("dot",   width = 10L,
                                            color  = "#00FF00"))
  txt <- paste(capture.output(print(b)), collapse = "\n")
  expect_match(txt, "<rtf_border>")
  expect_match(txt, "top.*single")
  expect_match(txt, "bottom.*dot")
  expect_match(txt, "#00FF00")
  expect_match(txt, "left.*none")
  expect_match(txt, "right.*none")
})

test_that("print.rtf_table_border lists each zone", {
  tb <- rtfreporter:::.rtf_table_border(
    header = rtf_border(top    = rtf_border_side("single"),
                        bottom = rtf_border_side("single",
                                                  color = "#123456")),
    body   = rtf_border(top = rtf_border_side("dot"))
  )
  txt <- paste(capture.output(print(tb)), collapse = "\n")
  expect_match(txt, "<rtf_table_border>")
  expect_match(txt, "header.*single")
  expect_match(txt, "#123456")
  expect_match(txt, "body.*dot")
  expect_match(txt, "spanning.*none")
  expect_match(txt, "last_row.*none")
})

# ──────── Convenience constructors ────────────────────────────────────────

test_that("rtf_border() returns an empty rtf_border", {
  b <- rtf_border()
  expect_s3_class(b, "rtf_border")
  for (s in c("top", "bottom", "left", "right")) expect_null(b[[s]])
})

test_that("a side value carries its own style, weight and colour", {
  bt <- rtf_border(top = rtf_border_side("double", 25L, "#001122"))
  expect_identical(bt$top$style, "double")
  expect_identical(bt$top$width, 25L)
  expect_identical(bt$top$color, "#001122")
  expect_null(bt$bottom)

  bb <- rtf_border(bottom = rtf_border_side("dash", 5L))
  expect_identical(bb$bottom$style, "dash")
  expect_null(bb$top)

  bx <- rtf_border(all = rtf_border_side("thick", 40L))
  for (s in c("top", "bottom", "left", "right")) {
    expect_identical(bx[[s]]$style, "thick")
    expect_identical(bx[[s]]$width, 40L)
  }
})

# ──────── TFL preset ──────────────────────────────────────────────────────

test_that("rtfreporter:::.rtf_border_tfl() returns a header-only rtf_table_border", {
  tb <- rtfreporter:::.rtf_border_tfl("double", width = 30L, color = "#abcdef")
  expect_s3_class(tb, "rtf_table_border")
  expect_s3_class(tb$header, "rtf_border")
  expect_identical(tb$header$top$style, "double")
  expect_identical(tb$header$top$width, 30L)
  expect_identical(tb$header$top$color, "#abcdef")
  for (z in c("spanning", "body", "first_row", "last_row")) {
    expect_null(tb[[z]])
  }
})

# ──────── rtf_table_border validation ─────────────────────────────────────

test_that("the zone map rejects non-rtf_border values in any zone", {
  ztb <- rtfreporter:::.rtf_table_border
  expect_error(ztb(header   = "x"),         "rtf_border object")
  expect_error(ztb(spanning = list(top=1)), "rtf_border object")
  expect_error(ztb(body     = 42),          "rtf_border object")
  expect_error(ztb(first_row = TRUE),       "rtf_border object")
  expect_error(ztb(last_row  = NA),         "rtf_border object")
})

# ──────── rtf_border_with edge cases ──────────────────────────────────────

test_that("a side given as a value is stored verbatim", {
  b <- rtf_border(top = rtf_border_side("dot"))
  expect_s3_class(b, "rtf_border")
  expect_identical(b$top$style, "dot")
})

test_that("rtf_border_with() still guards its base, behind its warning", {
  expect_error(suppressWarnings(rtf_border_with("oops")), "rtf_border object")
})

# ──────── Internal helpers via end-to-end rendering ───────────────────────

test_that(".plain_list_to_table_border accepts legacy plain-list specs", {
  legacy <- list(
    header = list(top = "single", bottom = "double", width = 20L),
    body   = list(top = "none",   bottom = "single")
  )
  out <- rtfreporter:::.plain_list_to_table_border(legacy)
  expect_s3_class(out, "rtf_table_border")
  expect_identical(out$header$top$style,    "single")
  expect_identical(out$header$bottom$style, "double")
  expect_identical(out$header$bottom$width, 20L)
  # `"none"` should NOT produce a side; body has only bottom set.
  expect_null(out$body$top)
  expect_identical(out$body$bottom$style, "single")
})

test_that(".merge_rtf_border returns over when base is NULL", {
  over <- rtf_border(top = TRUE)
  expect_identical(rtfreporter:::.merge_rtf_border(NULL, over), over)
})

test_that(".merge_rtf_border returns base when over is NULL or empty", {
  base <- rtf_border(top = TRUE)
  expect_identical(rtfreporter:::.merge_rtf_border(base, NULL), base)
  expect_identical(rtfreporter:::.merge_rtf_border(base, list()), base)
})

test_that(".merge_rtf_border overrides only non-NULL sides of over", {
  base <- rtf_border(top    = rtf_border_side("single"),
                     bottom = rtf_border_side("single"))
  over <- rtf_border(bottom = rtf_border_side("double"))
  m    <- rtfreporter:::.merge_rtf_border(base, over)
  expect_identical(m$top$style,    "single")        # unchanged
  expect_identical(m$bottom$style, "double")        # overridden
})

test_that(".collect_border_colors gathers every non-NULL colour", {
  b <- rtf_border(top    = rtf_border_side(color = "#aaaaaa"),
                  bottom = rtf_border_side(color = "#bbbbbb"),
                  left   = rtf_border_side(),                # no colour
                  right  = NULL)
  cols <- rtfreporter:::.collect_border_colors(b)
  expect_setequal(cols, c("#aaaaaa", "#bbbbbb"))
  expect_identical(rtfreporter:::.collect_border_colors(NULL), character(0))
})

test_that("rtf_table_style() rejects non-rtf_border zone arguments", {
  expect_error(rtf_table_style(border_header    = "x"), "border_header.*rtf_border")
  expect_error(rtf_table_style(border_spanning  = 1L),  "border_spanning.*rtf_border")
  expect_error(rtf_table_style(border_body      = TRUE),"border_body.*rtf_border")
  expect_error(rtf_table_style(border_first_row = NA),  "border_first_row.*rtf_border")
  expect_error(rtf_table_style(border_last_row  = list()),
               "border_last_row.*rtf_border")
})

test_that("rtf_table_style_with() rejects non-rtf_table_style input", {
  expect_error(rtf_table_style_with("nope", header_bold = TRUE),
               "rtf_table_style object")
})

test_that("print.rtf_table_style prints every zone and inherited header_align", {
  sty <- rtf_table_style_tfl()
  txt <- paste(capture.output(print(sty)), collapse = "\n")
  expect_match(txt, "<rtf_table_style>")
  expect_match(txt, "header.*<rtf_border>")
  expect_match(txt, "spanning.*none")
  expect_match(txt, "header_align.*inherit")
  expect_match(txt, "cell_padding")
})

test_that("print.rtf_table_style shows explicit header_align when set", {
  sty <- rtf_table_style_with(rtf_table_style_tfl(), header_align = "center")
  txt <- paste(capture.output(print(sty)), collapse = "\n")
  expect_match(txt, "header_align : center")
})

test_that(".style_to_table_border copies every zone into rtf_table_border", {
  sty <- rtf_table_style(
    border_header   = rtf_border(top = TRUE),
    border_body     = rtf_border(bottom = TRUE),
    border_last_row = rtf_border(all = TRUE)
  )
  tb <- rtfreporter:::.style_to_table_border(sty)
  expect_s3_class(tb, "rtf_table_border")
  expect_s3_class(tb$header,   "rtf_border")
  expect_s3_class(tb$body,     "rtf_border")
  expect_s3_class(tb$last_row, "rtf_border")
  expect_null(tb$spanning)
  expect_null(tb$first_row)
})

test_that(".collect_table_border_colors walks every zone", {
  tb <- rtfreporter:::.rtf_table_border(
    header = rtf_border(top = rtf_border_side(color = "#111111")),
    body   = rtf_border(bottom = rtf_border_side(color = "#222222"))
  )
  cols <- rtfreporter:::.collect_table_border_colors(tb)
  expect_setequal(cols, c("#111111", "#222222"))
  expect_identical(rtfreporter:::.collect_table_border_colors(NULL),
                   character(0))
})
