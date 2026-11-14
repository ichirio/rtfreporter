# markup argument: control >=/<= (relational) and ^{}/_{} (script) conversion.
# Default "script" -- relational symbol conversion is opt-in (#142).

.render_tbl <- function(df, ...) {
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(rtftable(df, ...))
  .render_to_string(doc)
}

.render_doc_default <- function(df, markup) {
  doc <- rtf_document(style = rtf_style(markup = markup)) |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(rtftable(df))
  .render_to_string(doc)
}


# ── .resolve_markup ──────────────────────────────────────────────────────────

test_that(".resolve_markup expands all/none, validates, and keeps NULL as inherit", {
  rm_ <- rtfreporter:::.resolve_markup
  expect_null(rm_(NULL))                                  # inherit sentinel
  expect_equal(rm_("none"), character(0))
  expect_equal(rm_(character(0)), character(0))
  expect_setequal(rm_("all"), c("script", "relational"))
  expect_equal(rm_("script"), "script")
  expect_setequal(rm_(c("relational", "script")), c("script", "relational"))
  expect_error(rm_("bogus"), "subset of")
})

test_that("rtftable() stores the resolved markup; NULL means inherit", {
  df <- data.frame(A = "x", stringsAsFactors = FALSE)
  expect_null(rtftable(df)$markup)                        # NULL = inherit
  expect_equal(rtftable(df, markup = "none")$markup, character(0))
  expect_setequal(rtftable(df, markup = "all")$markup, c("script", "relational"))
  expect_error(rtftable(df, markup = "nope"), "subset of")
})


# ── default ("script"): relational off, script on ────────────────────────────

test_that("by default >= / <= stay literal but ^{}/_{} render as super/subscript", {
  df  <- data.frame(A = c("Age >= 65", "x^{2}", "H_{2}O"), stringsAsFactors = FALSE)
  txt <- .render_tbl(df)
  expect_false(grepl("u8805", txt))                       # no >= conversion
  expect_match(txt, ">=", fixed = TRUE)                   # literal >=
  expect_match(txt, "\\super", fixed = TRUE)              # ^{2} -> superscript
  expect_match(txt, "\\sub", fixed = TRUE)                # _{2} -> subscript
})


# ── opt in to relational ─────────────────────────────────────────────────────

test_that("markup = 'all' converts >= and <= to RTF Unicode symbols", {
  df  <- data.frame(A = c("Age >= 65", "Dose <= 5"), stringsAsFactors = FALSE)
  txt <- .render_tbl(df, markup = "all")
  expect_match(txt, "\\u8805?", fixed = TRUE)             # >= -> U+2265
  expect_match(txt, "\\u8804?", fixed = TRUE)             # <= -> U+2264
})

test_that("markup = 'relational' enables symbols but not super/subscript", {
  df  <- data.frame(A = c("Age >= 65", "x^{2}"), stringsAsFactors = FALSE)
  txt <- .render_tbl(df, markup = "relational")
  expect_match(txt, "\\u8805?", fixed = TRUE)
  expect_false(grepl("\\super", txt, fixed = TRUE))       # ^{} left literal
})


# ── 'none': everything literal ───────────────────────────────────────────────

test_that("markup = 'none' leaves both >= and ^{} literal", {
  df  <- data.frame(A = c("Age >= 65", "x^{2}"), stringsAsFactors = FALSE)
  txt <- .render_tbl(df, markup = "none")
  expect_false(grepl("u8805", txt))
  expect_false(grepl("\\super", txt, fixed = TRUE))
  expect_match(txt, ">=", fixed = TRUE)
})


# ── resolution: per-table overrides document default ─────────────────────────

test_that("document style markup applies, and a per-table markup overrides it", {
  df <- data.frame(A = "Age >= 65", stringsAsFactors = FALSE)
  # Document opts into relational; the table inherits it.
  expect_match(.render_doc_default(df, "all"), "\\u8805?", fixed = TRUE)
  # Per-table markup wins over the document default.
  doc <- rtf_document(style = rtf_style(markup = "all")) |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(rtftable(df, markup = "none"))
  expect_false(grepl("u8805", .render_to_string(doc)))
})


# ── titles / footnotes honour the document markup ────────────────────────────

test_that("titles and footnotes follow the document markup setting", {
  df  <- data.frame(A = "x", stringsAsFactors = FALSE)
  doc <- rtf_document(style = rtf_style(markup = "all")) |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(rtftable(df),
               titles    = list(list("Age >= 65")),
               footnotes = list(list("Dose <= 5")))
  txt <- .render_to_string(doc)
  expect_match(txt, "\\u8805?", fixed = TRUE)             # title  >=
  expect_match(txt, "\\u8804?", fixed = TRUE)             # footnote <=
})
