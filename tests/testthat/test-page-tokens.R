# Page-number token semantics in the rendered RTF.
#
# Spec recap:
#   {AUTO_PAGE}        -> \chpgn          (DYNAMIC; viewer renders per page)
#   {AUTO_TOTAL_PAGES} -> NUMPAGES field  (DYNAMIC; viewer recomputes)
#   {PAGE}             -> integer literal (STATIC; baked in at render time
#                                          = section's first-page number)
#   {TOTAL_PAGES}      -> integer literal (STATIC; baked in at render time
#                                          = document total page count)

.render_doc_for_token_test <- function(rows, n_pages = 1L) {
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(rows = rows),
      footer = NULL
    )) |>
    rtf_tables(replicate(n_pages,
                          data.frame(A = 1L, B = "x", stringsAsFactors = FALSE),
                          simplify = FALSE))
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# ──────── {AUTO_PAGE} — dynamic ────────────────────────────────────────────

test_that("{AUTO_PAGE} resolves to RTF \\chpgn (dynamic per-page number)", {
  txt <- .render_doc_for_token_test(
    list(c(l = "X", r = "P {AUTO_PAGE}"))
  )
  expect_match(txt, "\\\\chpgn")
  # The literal token must NOT appear in the output
  expect_false(grepl("\\{AUTO_PAGE\\}", txt))
})

# ──────── {AUTO_TOTAL_PAGES} — dynamic NUMPAGES field ─────────────────────

test_that("{AUTO_TOTAL_PAGES} resolves to a NUMPAGES field", {
  txt <- .render_doc_for_token_test(
    list(c(l = "X", r = "P / {AUTO_TOTAL_PAGES}"))
  )
  expect_match(txt, "NUMPAGES")
  expect_false(grepl("\\{AUTO_TOTAL_PAGES\\}", txt))
})

# ──────── {PAGE} — STATIC integer (BUG FIX in v0.0.32) ─────────────────────

test_that("{PAGE} bakes the section's first-page number as a literal integer", {
  # Single section, first page = 1.  Expect literal "1" in the header text.
  txt <- .render_doc_for_token_test(
    list(c(l = "X", r = "Page {PAGE}"))
  )
  # The dynamic RTF field MUST NOT appear for this token.
  expect_false(grepl("\\\\chpgn", txt))
  # Literal "Page 1" must appear (after escaping the space is preserved
  # in the cell content of the header table).
  expect_match(txt, "Page 1")
  expect_false(grepl("\\{PAGE\\}", txt))
})

test_that("{PAGE} and {TOTAL_PAGES} produce the documented 'Page N of M' literal", {
  # 3 sub-pages -> document total = 3.  Section first-page = 1.
  txt <- .render_doc_for_token_test(
    list(c(l = "X", r = "Page {PAGE} of {TOTAL_PAGES}")),
    n_pages = 3L
  )
  expect_match(txt, "Page 1 of 3")
  # Neither token shall leave its literal text behind
  expect_false(grepl("\\{PAGE\\}",        txt))
  expect_false(grepl("\\{TOTAL_PAGES\\}", txt))
  # And no dynamic field code for these static tokens
  expect_false(grepl("\\\\chpgn",         txt))
})

# ──────── {TOTAL_PAGES} — STATIC integer ───────────────────────────────────

test_that("{TOTAL_PAGES} alone resolves to a literal integer", {
  txt <- .render_doc_for_token_test(
    list(c(l = "X", r = "Total: {TOTAL_PAGES}")),
    n_pages = 5L
  )
  expect_match(txt, "Total: 5")
})

# ──────── Side-by-side: {PAGE} static vs {AUTO_PAGE} dynamic ──────────────

test_that("{PAGE} and {AUTO_PAGE} are DIFFERENT (static vs dynamic)", {
  txt <- .render_doc_for_token_test(
    list(c(l = "X", r = "static={PAGE} auto={AUTO_PAGE}")),
    n_pages = 2L
  )
  # The static side becomes literal "static=1"
  expect_match(txt, "static=1")
  # The dynamic side becomes \chpgn (NOT a literal "1")
  # — we should see the \chpgn token somewhere right of "auto="
  # (more robustly, the dynamic field is present in the output)
  expect_match(txt, "\\\\chpgn")
})

# ──────── {PAGE} increments per sub-page (v0.0.34 bug fix) ────────────────
#
# Pre-v0.0.34, an rtf_section with N sub-pages shared a single
# RTF `\header` block, so `{PAGE}` was baked once with the section's
# first-page number -- every page in the document showed "Page 1".
# Now each sub-page is promoted to its own RTF section break, with
# its own header carrying the correct baked-in number.

test_that("{PAGE} increments across sub-pages of one rtf_section", {
  txt <- .render_doc_for_token_test(
    list(c(l = "X", r = "Page {PAGE} of {TOTAL_PAGES}")),
    n_pages = 3L
  )
  # All three baked-in numbers must appear: "Page 1 of 3", "Page 2 of 3", "Page 3 of 3".
  expect_match(txt, "Page 1 of 3")
  expect_match(txt, "Page 2 of 3")
  expect_match(txt, "Page 3 of 3")
})

test_that("{PAGE}-using sub-pages emit one \\sect per boundary", {
  # 3 sub-pages -> 2 internal section breaks (sub-page boundary).
  # Pre-v0.0.34 they were `\page` breaks (no \sect for internal pages).
  txt   <- .render_doc_for_token_test(
    list(c(l = "X", r = "Page {PAGE}")),
    n_pages = 3L
  )
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  n_sect <- sum(grepl("^\\\\sect$", lines))
  # 3 sub-pages with per-page sections -> 2 internal \sect breaks.
  # (The final sub-page is followed by document close, not \sect.)
  expect_gte(n_sect, 2L)
})

test_that("AUTO-only headers stay on the cheap one-header-per-section path", {
  # When the header only uses dynamic AUTO tokens, the old code
  # path (one `\header` per rtf_section, `\page` between sub-pages)
  # must still be used.  Internal `\sect` breaks must NOT appear
  # because no static token needs them.
  txt   <- .render_doc_for_token_test(
    list(c(l = "X", r = "Page {AUTO_PAGE} / {AUTO_TOTAL_PAGES}")),
    n_pages = 3L
  )
  lines  <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  n_sect <- sum(grepl("^\\\\sect$", lines))
  # No internal section breaks (only the closing document structure).
  expect_identical(n_sect, 0L)
})

# ──────── Title / footnote bands (#398) ───────────────────────────────────

# Same three-page document, but the tokens live in the title and footnote
# blocks rather than the running header, and the band format is selectable.
.render_bands_for_token_test <- function(title, footnote,
                                         title_format = "text",
                                         footnote_format = "table") {
  doc <- rtf_document(default_format = rtf_default_format(
    title_format = title_format, footnote_format = footnote_format))
  doc <- rtf_section(doc, page = 1, secinfo = list(
    header = rtf_header(rows = list(c(l = "Protocol RTF-101"))), footer = NULL))
  doc <- rtf_tables(doc,
    replicate(3L, data.frame(A = 1L, B = "x", stringsAsFactors = FALSE),
              simplify = FALSE),
    titles    = list(c(title)),
    footnotes = list(c(footnote)))
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

test_that("page tokens resolve in the title band, in both title formats", {
  for (fmt in c("text", "table")) {
    txt <- .render_bands_for_token_test(
      "Table 6-3.7 (Page {PAGE} of {TOTAL_PAGES})", "note", title_format = fmt)
    # Novartis-style: the number is part of the title line itself.
    expect_match(txt, "Page 1 of 3", fixed = TRUE, info = fmt)
    expect_match(txt, "Page 3 of 3", fixed = TRUE, info = fmt)
    expect_false(grepl("{PAGE}", txt, fixed = TRUE), info = fmt)
    expect_false(grepl("{TOTAL_PAGES}", txt, fixed = TRUE), info = fmt)
  }
})

test_that("page tokens resolve in the footnote band, in BOTH formats (#398)", {
  # The "table" form already worked; "text" printed the token literally.
  for (fmt in c("text", "table")) {
    txt <- .render_bands_for_token_test(
      "T", "Page {AUTO_PAGE} of {SECTION_PAGES}", footnote_format = fmt)
    expect_match(txt, "\\chpgn",     info = fmt)
    expect_match(txt, "SECTIONPAGES",  info = fmt)
    expect_false(grepl("{AUTO_PAGE}",     txt, fixed = TRUE), info = fmt)
    expect_false(grepl("{SECTION_PAGES}", txt, fixed = TRUE), info = fmt)
  }
})

test_that("a static {PAGE} in a body band does not force per-page sections", {
  # `.uses_static_page_token()` inspects the header/footer only, and rightly
  # so: title and footnote are re-emitted for every page, so each page bakes
  # its own number without an extra RTF section.
  txt <- .render_bands_for_token_test("T (Page {PAGE} of {TOTAL_PAGES})", "note")
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  expect_identical(sum(grepl("^\\sect$", lines)), 0L)
  expect_match(txt, "Page 2 of 3", fixed = TRUE)
})
