# rtfreport.R helpers: header/footer construction, row updates,
# internal report builders, and validator.

# ──────── rtf_header / rtf_footer construction ────────────────────────────

test_that("rtf_header() accepts a single named vector and wraps it in a list", {
  hdr <- rtf_header(c(l = "Left", r = "Right"))
  expect_type(hdr, "list")
  expect_type(hdr$rows, "list")
  expect_length(hdr$rows, 1L)
  expect_identical(hdr$rows[[1L]][["l"]], "Left")
})

test_that("rtf_header() accepts a list of rows directly", {
  hdr <- rtf_header(rows = list(c(l = "A"), c(r = "B")))
  expect_length(hdr$rows, 2L)
})

test_that("rtf_header() rejects non-list / non-character `rows`", {
  expect_error(rtf_header(rows = 42),  "named character vector")
  expect_error(rtf_header(rows = NA),  "named character vector")
})

test_that("rtf_header() rejects a non-rtf_border `border`", {
  expect_error(rtf_header(rows = c(c = "x"), border = "single"),
               "rtf_border object")
})

test_that("rtf_header() takes border = rtf_border(top = TRUE) for a header rule", {
  h <- rtf_header(rows = c(c = "x"), border = rtf_border(top = TRUE))
  expect_s3_class(h$border, "rtf_border")
})

test_that("rtf_footer() defaults to a top border", {
  ftr <- rtf_footer(c(c = "Source"))
  expect_s3_class(ftr$border, "rtf_border")
  expect_s3_class(ftr$border$top, "rtf_border_side")
})

test_that("rtf_footer() accepts border = NULL to drop the default rule", {
  ftr <- rtf_footer(c(c = "Source"), border = NULL)
  expect_null(ftr$border)
})

test_that("rtf_footer() rejects non-rtf_border `border`", {
  expect_error(rtf_footer(rows = c(c = "x"), border = "dot"),
               "rtf_border object")
})

# ──────── update_header_row / update_footer_row ───────────────────────────

test_that("update_header_row() replaces an existing row", {
  hdr <- rtf_header(rows = list(c(l = "Old", r = "Right")))
  hdr <- update_header_row(hdr, row = 1L, content = c(l = "New", r = "Right"))
  expect_identical(hdr$rows[[1L]][["l"]], "New")
})

test_that("update_header_row() appends rows beyond the current length", {
  hdr <- rtf_header(rows = list(c(l = "A")))
  hdr <- update_header_row(hdr, row = 3L, content = c(l = "C"))
  expect_length(hdr$rows, 3L)
  # Intermediate (row 2) auto-filled as empty centered row.
  expect_identical(hdr$rows[[2L]], c(c = ""))
  expect_identical(hdr$rows[[3L]][["l"]], "C")
})

test_that("update_footer_row() works the same way", {
  ftr <- rtf_footer(rows = list(c(c = "X")))
  ftr <- update_footer_row(ftr, row = 2L, content = c(c = "Y"))
  expect_length(ftr$rows, 2L)
  expect_identical(ftr$rows[[2L]][["c"]], "Y")
})

test_that("update_*_row() rejects non-rtf_header argument", {
  expect_error(update_header_row("nope", row = 1L, content = c(c = "x")),
               "rtf_header.*rtf_footer")
})

test_that("update_*_row() rejects row < 1", {
  hdr <- rtf_header(rows = c(c = "x"))
  expect_error(update_header_row(hdr, row = 0L, content = c(c = "y")),
               ">= 1")
})

test_that("update_*_row() preserves names on character input", {
  hdr <- rtf_header(rows = c(c = "x"))
  hdr <- update_header_row(hdr, row = 1L,
                           content = c(l = "L", c = "C", r = "R"))
  expect_setequal(names(hdr$rows[[1L]]), c("l", "c", "r"))
})

# ──────── Internal helpers ────────────────────────────────────────────────

test_that(".in_to_twips() converts inches to twips", {
  expect_identical(rtfreporter:::.in_to_twips(1),    1440L)
  expect_identical(rtfreporter:::.in_to_twips(0.5),   720L)
  expect_identical(rtfreporter:::.in_to_twips(11),  15840L)
})

test_that(".merge_list() lets override win, NULL override returns base", {
  base <- list(a = 1, b = 2, c = 3)
  ov   <- list(b = 99, d = 4)
  out  <- rtfreporter:::.merge_list(base, ov)
  expect_identical(out$a, 1)
  expect_identical(out$b, 99)
  expect_identical(out$c, 3)
  expect_identical(out$d, 4)
  # NULL override is a no-op.
  expect_identical(rtfreporter:::.merge_list(base, NULL), base)
})

test_that(".assert_index() rejects out-of-range / non-numeric input", {
  expect_error(rtfreporter:::.assert_index("a", 5L, "idx"), "out of range")
  expect_error(rtfreporter:::.assert_index(0L,  5L, "idx"), "out of range")
  expect_error(rtfreporter:::.assert_index(6L,  5L, "idx"), "out of range")
  expect_error(rtfreporter:::.assert_index(NA,  5L, "idx"), "out of range")
  expect_error(rtfreporter:::.assert_index(c(1L, 2L), 5L, "idx"), "out of range")
  expect_identical(rtfreporter:::.assert_index(3L, 5L, "idx"), 3L)
})

test_that(".normalize_content() handles NULL / rtftable / rtfplot / data.frame", {
  expect_null(rtfreporter:::.normalize_content(NULL))
  tb <- rtftable(data.frame(A = 1L))
  expect_identical(rtfreporter:::.normalize_content(tb), tb)
  df <- data.frame(A = 1:2)
  out <- rtfreporter:::.normalize_content(df)
  expect_s3_class(out, "rtftable")
})

test_that(".normalize_content() rejects unsupported types", {
  expect_error(rtfreporter:::.normalize_content(1L),         "rtftable.*rtfplot.*data.frame")
  expect_error(rtfreporter:::.normalize_content("x"),        "rtftable.*rtfplot.*data.frame")
  expect_error(rtfreporter:::.normalize_content(list(1, 2)), "rtftable.*rtfplot.*data.frame")
})

# ──────── Internal rtfreport S3 constructor + ops ─────────────────────────

test_that(".new_rtfreport() builds a default rtfreport with letter landscape", {
  r <- rtfreporter:::.new_rtfreport()
  expect_s3_class(r, "rtfreport")
  expect_identical(r$document$default_page$paper,       "letter")
  expect_identical(r$document$default_page$orientation, "landscape")
  expect_identical(r$document$default_page$width_twips,  15840L)
  expect_identical(r$document$default_page$height_twips, 12240L)
  expect_identical(r$document$default_format$font_size_half_points, 18L)
})

test_that(".rtfreport_set_default_page / format / font / color update the doc", {
  r <- rtfreporter:::.new_rtfreport()
  r <- rtfreporter:::.rtfreport_set_default_page(r,
        list(orientation = "portrait", width_twips = 12240L))
  expect_identical(r$document$default_page$orientation, "portrait")
  expect_identical(r$document$default_page$width_twips, 12240L)
  # Unspecified keys are preserved.
  expect_identical(r$document$default_page$paper, "letter")

  r <- rtfreporter:::.rtfreport_set_default_format(r,
        list(font_size_half_points = 22L))
  expect_identical(r$document$default_format$font_size_half_points, 22L)

  r <- rtfreporter:::.rtfreport_set_font_table(r,
        list(list(name = "Arial"), list(name = "Symbol")))
  expect_length(r$document$font_table, 2L)
  expect_identical(r$document$font_table[[1L]]$name, "Arial")

  r <- rtfreporter:::.rtfreport_set_color_table(r, c("#FF0000", "#00FF00"))
  expect_identical(r$document$color_table, c("#FF0000", "#00FF00"))
})

test_that(".rtfreport_add_page() appends a page; data.frame is normalised", {
  r <- rtfreporter:::.new_rtfreport()
  r <- rtfreporter:::.rtfreport_add_page(r,
        title = "T", content = data.frame(A = 1L), footnote = "F")
  expect_length(r$pages, 1L)
  expect_identical(r$pages[[1L]]$title,    "T")
  expect_identical(r$pages[[1L]]$footnote, "F")
  expect_s3_class(r$pages[[1L]]$content, "rtftable")
})

test_that(".rtfreport_add_section() appends with from_page coerced to int", {
  r <- rtfreporter:::.new_rtfreport()
  r <- rtfreporter:::.rtfreport_add_section(r, from_page = 3)
  expect_length(r$sections, 1L)
  expect_identical(r$sections[[1L]]$from_page, 3L)
})

test_that(".rtfreport_validate() auto-creates a default section + rejects empty pages", {
  r <- rtfreporter:::.new_rtfreport()
  r <- rtfreporter:::.rtfreport_add_page(r, content = data.frame(A = 1))
  expect_message(
    r2 <- rtfreporter:::.rtfreport_validate(r),
    "default.*section"
  )
  expect_length(r2$sections, 1L)

  # No pages -> error.
  empty <- rtfreporter:::.new_rtfreport()
  empty <- rtfreporter:::.rtfreport_add_section(empty)
  expect_error(rtfreporter:::.rtfreport_validate(empty),
               "at least one page")
})

test_that(".rtfreport_validate() rejects a page with an unsupported content type", {
  r <- rtfreporter:::.new_rtfreport()
  r <- rtfreporter:::.rtfreport_add_section(r)
  # Bypass .normalize_content() by stashing raw content directly.
  r$pages[[1L]] <- rtfreporter:::.new_page(content = list(broken = TRUE))
  expect_error(rtfreporter:::.rtfreport_validate(r),
               "rtftable.*rtfplot")
})
