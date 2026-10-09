# Pipe API: argument-validation and deprecated-formatter pass-through.

# ──────── rtf_config validation + branches ────────────────────────────────

test_that("rtf_config rejects non-rtf_document `doc`", {
  expect_error(rtf_config("oops"), "rtf_document")
})

test_that("rtf_config updates only supplied fields, NULLs are no-op", {
  d <- rtf_document() |> rtf_config(
    font_table     = list(list(name = "Arial")),
    color_table    = c("#FF0000"),
    page           = list(orientation = "portrait"),
    default_format = list(font_size_half_points = 22L))
  expect_identical(d$document$font_table[[1L]]$name, "Arial")
  expect_identical(d$document$color_table,           "#FF0000")
  expect_identical(d$document$page$orientation,      "portrait")
  expect_identical(d$document$default_format$font_size_half_points, 22L)
})

# ──────── rtf_tables -- validation branches ───────────────────────────────

test_that("rtf_tables rejects non-rtf_document `doc`", {
  expect_error(rtf_tables("nope", list(data.frame(A = 1L))), "rtf_document")
})

test_that("rtf_tables auto-wraps a single content item and rejects invalid types", {
  d <- rtf_document()
  # A plain character string is not a recognised content item and should error
  # (it gets auto-wrapped into list("not-a-list") then rejected by the item
  # validator, so the error message changed from "must be a list" to "Item 1").
  expect_error(rtf_tables(d, "not-a-list"), "Item 1")
})

test_that("rtf_tables rejects a non-content item", {
  d <- rtf_document()
  expect_error(rtf_tables(d, list(data.frame(A = 1L), "broken")),
               "Item 2")
})

test_that("rtf_tables rejects `titles` / `footnotes` of wrong shape", {
  d <- rtf_document()
  expect_error(
    rtf_tables(d, list(data.frame(A = 1L)), titles = "x"),
    "`titles` must be a list"
  )
  expect_error(
    rtf_tables(d, list(data.frame(A = 1L)), titles = list("a", "b")),
    "length 1"
  )
  expect_error(
    rtf_tables(d, list(data.frame(A = 1L)), footnotes = 42),
    "`footnotes` must be a list"
  )
})

test_that("rtf_tables auto_section wraps named items in rtf_auto_section_item", {
  d <- rtf_document()
  d <- rtf_tables(d,
                  list(Demo = data.frame(A = 1L), AE = data.frame(A = 2L)),
                  auto_section = TRUE)
  cls <- vapply(d$contents, function(c) class(c)[1L], character(1L))
  expect_true(all(cls == "rtf_auto_section_item"))
})

test_that("rtf_tables auto_section ignores items without names", {
  d <- rtf_document()
  d <- rtf_tables(d, list(data.frame(A = 1L), data.frame(A = 2L)),
                  auto_section = TRUE)
  # No names -> the wrap-loop never triggers; items stay rtftable.
  expect_true(inherits(d$contents[[1L]], "rtftable"))
})

# ──────── rtf_figures -- validation branches ──────────────────────────────

test_that("rtf_figures rejects non-rtf_document `doc`", {
  expect_error(rtf_figures("x", list()), "rtf_document")
})

test_that("rtf_figures takes one figure as it is, and refuses what draws nothing", {
  d <- rtf_document()
  # one path, as it is: a figure that is not there says so, as in a list
  expect_error(rtf_figures(d, "x"), "Item 1: Image file not found")
  expect_error(rtf_figures(d, 42), "must be a figure or a list")
  expect_error(rtf_figures(d, c("a.png", "b.png")), "must be a figure or a list")
})

test_that("rtf_figures(doc, plot): one plot object without list() (#589)", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  one <- rtf_figures(rtf_document(), p)
  two <- rtf_figures(rtf_document(), list(p))
  expect_length(one$contents, 1L)
  expect_s3_class(one$contents[[1L]], "rtfplot")
  # the same page as in a list (the drawn file aside)
  strip <- function(d) { d$contents[[1L]]$path <- NULL; d }
  expect_identical(strip(one), strip(two))
  # an rtfplot, as it is
  rp <- two$contents[[1L]]
  expect_identical(rtf_figures(rtf_document(), rp)$contents, list(rp))
  # a list of plots is still several figures
  expect_length(rtf_figures(rtf_document(), list(p, p))$contents, 2L)
})

test_that("rtf_figures rejects elements that are neither rtfplot nor a path", {
  d <- rtf_document()
  expect_error(rtf_figures(d, list(42)), "Item 1")
})

test_that("rtf_figures validates `titles` and `footnotes` shape", {
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  # 74-byte PNG from test-rtfplot.R-style helper -- borrow inline.
  writeBin(as.raw(c(
    0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
    0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44, 0x52,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
    0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53, 0xDE,
    0x00, 0x00, 0x00, 0x0C, 0x49, 0x44, 0x41, 0x54,
    0x08, 0x99, 0x63, 0xF8, 0xFF, 0xFF, 0xFF, 0x3F,
    0x00, 0x05, 0xFE, 0x02, 0xFE, 0xDC, 0xCC, 0x59, 0xE7,
    0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4E, 0x44,
    0xAE, 0x42, 0x60, 0x82)), png)
  d <- rtf_document()
  expect_error(rtf_figures(d, list(png), titles = "x"),
               "`titles` must be a list")
  expect_error(rtf_figures(d, list(png), titles = list("a", "b")),
               "length 1")
  expect_error(rtf_figures(d, list(png), footnotes = 1),
               "`footnotes` must be a list")
})

# ──────── rtf_titles / rtf_footnotes ──────────────────────────────────────

test_that("rtf_titles / rtf_footnotes reject non-rtf_document", {
  expect_error(rtf_titles("x", list()),    "rtf_document")
  expect_error(rtf_footnotes("x", list()), "rtf_document")
})

test_that("rtf_titles / rtf_footnotes error before content is added", {
  d <- rtf_document()
  expect_error(rtf_titles(d,    list("A")), "before any content")
  expect_error(rtf_footnotes(d, list("A")), "before any content")
})

test_that("rtf_titles rejects non-list / wrong-length input", {
  d <- rtf_document() |> rtf_tables(list(data.frame(A = 1L), data.frame(A = 2L)))
  expect_error(rtf_titles(d,    "x"),                  "must be a list")
  expect_error(rtf_titles(d,    list("a", "b", "c")),  "length 2")  # 3 != 2 and != 1
  expect_error(rtf_footnotes(d, "x"),                  "must be a list")
  expect_error(rtf_footnotes(d, list("a", "b", "c")),  "length 2")
  # length 1 is now accepted (common to all pages).
  expect_length(rtf_titles(d, list("common"))$titles, 2L)
})

test_that("rtf_titles / rtf_footnotes successfully attach lists of length n", {
  d <- rtf_document() |>
    rtf_tables(list(data.frame(A = 1L), data.frame(A = 2L))) |>
    rtf_titles(list("T1", "T2")) |>
    rtf_footnotes(list("F1", "F2"))
  expect_identical(d$titles[[1L]], "T1")
  expect_identical(d$footnotes[[2L]], "F2")
})

# ──────── rtf_section -- various code paths ───────────────────────────────

test_that("rtf_section rejects non-rtf_document `doc`", {
  expect_error(rtf_section("x", secinfo = list()), "rtf_document")
})

test_that("rtf_section(page = NULL) stores a _default template", {
  d <- rtf_document() |>
    rtf_section(secinfo = list(header = NULL, footer = NULL))
  expect_true("_default" %in% names(d$sections))
})

test_that("rtf_section(page = single, secinfo bare list) stores it", {
  d <- rtf_document() |> rtf_section(page = 1, secinfo = list(header = NULL))
  expect_true("1" %in% names(d$sections))
})

test_that("rtf_section(page = vector, secinfo per-page) maps each entry", {
  d <- rtf_document() |>
    rtf_section(page = c(1, 3),
                secinfo = list(list(header = NULL), list(header = NULL)))
  expect_setequal(names(d$sections), c("1", "3"))
})

test_that("rtf_section(page = vector) errors on length mismatch", {
  d <- rtf_document()
  expect_error(
    rtf_section(d, page = c(1, 2), secinfo = list(list(header = NULL))),
    "must match"
  )
})

test_that("rtf_section(page = vector, scalar secinfo) errors", {
  d <- rtf_document()
  expect_error(rtf_section(d, page = c(1, 2), secinfo = "oops"),
               "list of section objects")
})
