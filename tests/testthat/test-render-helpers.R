# generate_rtfreport.R internal renderers + escape helpers.
#
# Targets the markup / escape / cellx / border helpers that the
# end-to-end tests do not exercise individually.

# ──────── .rtf_escape_unicode_raw ─────────────────────────────────────────

test_that(".rtf_escape_unicode_raw escapes backslash, braces, newline, unicode", {
  esc <- rtfreporter:::.rtf_escape_unicode_raw
  expect_identical(esc(""),        "")
  expect_identical(esc("hello"),   "hello")
  expect_identical(esc("a\\b"),    "a\\\\b")
  expect_identical(esc("{x}"),     "\\{x\\}")
  expect_identical(esc("a\nb"),    "a\\line b")
  # Unicode code point > 127 -> \uN?
  out <- esc("α")     # Greek alpha = 945
  expect_identical(out, "\\u945?")
})

# ──────── .process_markup -- super/sub/nested/unmatched ───────────────────

test_that(".process_markup wraps ^{...} as \\super and _{...} as \\sub", {
  pm <- rtfreporter:::.process_markup
  expect_match(pm("x^{2}"),  "\\\\super 2")
  expect_match(pm("H_{2}O"), "\\\\sub 2")
})

test_that(".process_markup handles nested braces", {
  pm <- rtfreporter:::.process_markup
  out <- pm("a^{b{c}d}e")
  # The outer ^{...} should wrap as \super, the inner {c} is preserved.
  expect_match(out, "\\\\super")
  expect_match(out, "b\\\\\\{c\\\\\\}d")
})

test_that(".process_markup leaves unmatched markup as escaped plain text", {
  pm <- rtfreporter:::.process_markup
  out <- pm("a^{b")     # no closing brace -> escape the whole thing
  # `^` itself is not RTF-special, so only `{` is escaped.
  expect_identical(out, "a^\\{b")
})

test_that(".process_markup returns plain escape when no markup present", {
  pm <- rtfreporter:::.process_markup
  expect_identical(pm("plain text"), "plain text")
})

test_that(".process_markup chooses the earliest of ^{ vs _{", {
  pm <- rtfreporter:::.process_markup
  # _{ first
  expect_match(pm("a_{1}b^{2}"), "\\\\sub 1.*\\\\super 2")
  # ^{ first
  expect_match(pm("a^{1}b_{2}"), "\\\\super 1.*\\\\sub 2")
})

# ──────── .format_cell_text -- top-level pipeline ─────────────────────────

test_that(".format_cell_text returns '' for NULL and NA", {
  fct <- rtfreporter:::.format_cell_text
  expect_identical(fct(NULL), "")
  expect_identical(fct(NA),   "")
})

test_that(".format_cell_text gates >=/<= on the 'relational' token (default off)", {
  fct <- rtfreporter:::.format_cell_text
  # Default markup is "script": super/subscript on, relational conversion OFF.
  expect_match(fct("a^{1}"), "\\\\super 1")          # script default
  expect_false(grepl("u8805", fct("Age >= 65")))     # >= stays literal
  expect_match(fct("Age >= 65"), ">=", fixed = TRUE)
  # Opt in to the relational conversion (this helper takes resolved tokens).
  expect_match(fct("Age >= 65", markup = c("script", "relational")), "\\\\u8805\\?")
  expect_match(fct("Dose <= 5", markup = "relational"),              "\\\\u8804\\?")
  # No script token: ^{} is NOT turned into \super (stays literal, escaped).
  expect_false(grepl("\\super", fct("a^{1}", markup = character(0)), fixed = TRUE))
})

# ──────── .render_tokens -- AUTO_PAGE / SECTION_PAGES / TOTAL_PAGES ───────

test_that(".render_tokens substitutes every documented token", {
  rt <- rtfreporter:::.render_tokens
  expect_match(rt("Page {AUTO_PAGE}"),       "\\\\chpgn")
  expect_match(rt("Total {AUTO_TOTAL_PAGES}",
                  total_pages = 7L),         "NUMPAGES")
  expect_match(rt("Sec {SECTION_PAGES}"),    "SECTIONPAGES")
  expect_match(rt("Page {PAGE}", current_page = 3L),
                                              "Page 3")
  expect_match(rt("Total {TOTAL_PAGES}", total_pages = 9L),
                                              "Total 9")
})

test_that(".render_tokens returns empty for NULL", {
  expect_identical(rtfreporter:::.render_tokens(NULL), "")
})

# ──────── .compute_cellx -- explicit widths / col_rel / equal ─────────────

test_that(".compute_cellx with column_widths_twips returns cumulative sums", {
  tbl <- list(column_widths_twips = c(1000L, 2000L, 3000L))
  cx  <- rtfreporter:::.compute_cellx(ncols = 3L,
                                      writable_width_twips = 14400L, tbl = tbl)
  expect_identical(cx, c(1000L, 3000L, 6000L))
})

test_that(".compute_cellx with column_widths_twips errors on length mismatch", {
  tbl <- list(column_widths_twips = c(1000L, 2000L))
  expect_error(
    rtfreporter:::.compute_cellx(ncols = 3L,
                                 writable_width_twips = 14400L, tbl = tbl),
    "must match ncol"
  )
})

test_that(".compute_cellx with col_rel_width divides proportionally", {
  tbl <- list(col_rel_width = c(1, 1, 2))
  cx  <- rtfreporter:::.compute_cellx(ncols = 3L,
                                      writable_width_twips = 8000L, tbl = tbl)
  # Total 8000; rel=(1,1,2) -> widths 2000,2000,4000 ; cumsum -> 2000,4000,8000.
  expect_identical(cx, c(2000L, 4000L, 8000L))
})

test_that(".compute_cellx col_rel_width errors on length / non-positive", {
  tbl1 <- list(col_rel_width = c(1, 1))
  expect_error(
    rtfreporter:::.compute_cellx(ncols = 3L,
                                 writable_width_twips = 1000L, tbl = tbl1),
    "must match ncol"
  )
  tbl2 <- list(col_rel_width = c(1, 0, 2))
  expect_error(
    rtfreporter:::.compute_cellx(ncols = 3L,
                                 writable_width_twips = 1000L, tbl = tbl2),
    "must be positive"
  )
})

test_that(".compute_cellx uses table_width_twips when supplied", {
  tbl <- list(table_width_twips = 6000L,
              col_rel_width     = c(1, 2))
  cx  <- rtfreporter:::.compute_cellx(ncols = 2L,
                                      writable_width_twips = 14400L, tbl = tbl)
  # widths: 2000, 4000 -> cumsum 2000, 6000
  expect_identical(cx[2L], 6000L)
})

test_that(".compute_cellx uses table_width_pct_of_writable", {
  tbl <- list(table_width_pct_of_writable = 0.5)
  cx  <- rtfreporter:::.compute_cellx(ncols = 4L,
                                      writable_width_twips = 16000L, tbl = tbl)
  # total 8000 / 4 = 2000 per cell -> cumsum 2000,4000,6000,8000
  expect_equal(cx[4L], 8000)
})

# ──────── .effective_row_border ───────────────────────────────────────────

test_that(".effective_row_border handles all NULL combinations", {
  erb <- rtfreporter:::.effective_row_border
  expect_null(erb(NULL, NULL))

  b <- rtf_border(top = TRUE)
  expect_identical(erb(b, NULL),    b)
  expect_identical(erb(b, list()),  b)   # length 0 override
  expect_identical(erb(NULL, b),    b)
})

test_that(".effective_row_border merges overrides into rtf_border base", {
  base <- rtf_border(top = rtf_border_side("single"))
  over <- rtf_border(bottom = rtf_border_side("double"))
  m <- rtfreporter:::.effective_row_border(base, over)
  expect_identical(m$top$style,    "single")   # unchanged
  expect_identical(m$bottom$style, "double")   # set by override
})

# ──────── .build_border_commands -- old + new style ───────────────────────

test_that(".build_border_commands returns empty string for NULL spec", {
  expect_identical(rtfreporter:::.build_border_commands(NULL), "")
})

test_that(".build_border_commands uses brdrcf<idx> when color is supplied", {
  cmap <- list("#FF0000" = 3L)
  b <- rtf_border(top = rtf_border_side("single", width = 15L,
                                          color = "#FF0000"))
  out <- rtfreporter:::.build_border_commands(b, color_index_map = cmap)
  expect_match(out, "\\\\brdrcf3")
})

test_that(".build_border_commands legacy plain-list form emits border commands", {
  # Old-style: list(top='single', bottom='single', width=15)
  # Renders cell-border prefixes (\clbrdrt, \clbrdrb, ...).
  spec <- list(top = "single", bottom = "single", width = 15L)
  out  <- rtfreporter:::.build_border_commands(spec)
  expect_match(out, "\\\\clbrdrt")
  expect_match(out, "\\\\clbrdrb")
})

test_that(".build_border_commands legacy form ignores 'none' / empty sides", {
  spec <- list(top = "single", bottom = "none", left = "", width = 15L)
  out  <- rtfreporter:::.build_border_commands(spec)
  expect_match(out, "\\\\clbrdrt")
  expect_false(grepl("clbrdrb", out))   # 'none' skipped
  expect_false(grepl("clbrdrl", out))   # '' skipped
})

test_that(".build_border_commands errors on unknown legacy border style", {
  spec <- list(top = "squiggly", width = 15L)
  expect_error(rtfreporter:::.build_border_commands(spec),
               "Unknown border type")
})
