# Tests for the per-element width vocabulary (#291).

W <- rtfreporter:::.default_writable_twips()

.tbl <- function() {
  rtftable(data.frame(Stat = c("n", "Mean"), A = c("24", "45.2"),
                      stringsAsFactors = FALSE),
           border = "tfl", column_widths_twips = c(1500L, 1200L))
}

# The \cellx values of the row carrying `text`, and whether it has a top rule.
.block <- function(fmt = NULL, text = "Footnote line 1") {
  doc <- rtf_document(page = rtf_page(orientation = "landscape"),
                      default_format = fmt %||% rtf_default_format()) |>
    rtf_tables(.tbl()) |>
    rtf_titles(list("A title")) |>
    rtf_footnotes(list(c("Footnote line 1", "line 2")))
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  hit <- grep(text, readLines(f, warn = FALSE), value = TRUE, fixed = TRUE)
  list(cellx = as.integer(sub("cellx", "", unique(unlist(
         regmatches(hit, gregexpr("cellx[0-9]+", hit)))))),
       rule  = any(grepl("clbrdrt", hit)))
}

# ──────── the resolver ─────────────────────────────────────────────────────

test_that(".resolve_block_width speaks the four-value vocabulary", {
  r <- rtfreporter:::.resolve_block_width
  expect_equal(r("content", 2700L, 13680L), 2700L)
  expect_equal(r("page",    2700L, 13680L), 13680L)
  expect_equal(r(0.5,       2700L, 13680L), 6840L)
  expect_equal(r(1,         2700L, 13680L), 13680L)   # 1 is still a fraction
  expect_equal(r(8000L,     2700L, 13680L), 8000L)
})

test_that("NULL means the element's own default", {
  r <- rtfreporter:::.resolve_block_width
  expect_equal(r(NULL, 2700L, 13680L), 2700L)                      # "content"
  expect_equal(r(NULL, 2700L, 13680L, default = "page"), 13680L)
})

test_that(".check_block_width rejects nonsense", {
  chk <- rtfreporter:::.check_block_width
  expect_null(chk(NULL, "w"))
  expect_error(chk("wide", "w"), "content")
  expect_error(chk(-1, "w"),     "content")
  expect_error(chk(0, "w"),      "content")
  expect_error(chk(c(1, 2), "w"), "content")
})

# ──────── the footnote block ───────────────────────────────────────────────

test_that("the default is unchanged -- the footnote follows the content", {
  b <- .block()
  expect_equal(b$cellx, rtfreporter:::.content_width_twips(.tbl(), W))
  expect_false(b$rule)        # no rule by default since #296
})

test_that("footnote_width = 'page' widens the block", {
  b <- .block(rtf_default_format(footnote_width = "page"))
  expect_equal(b$cellx, W)
  expect_false(b$rule)        # no rule by default since #296
})

test_that("a fraction and an absolute value both work", {
  expect_equal(.block(rtf_default_format(footnote_width = 0.5))$cellx,
               as.integer(round(W * 0.5)))
  expect_equal(.block(rtf_default_format(footnote_width = 8000L))$cellx, 8000L)
})

test_that("footnote_width = 'content' is the default spelled out", {
  expect_equal(.block(rtf_default_format(footnote_width = "content"))$cellx,
               .block()$cellx)
})

test_that("the title block takes the same vocabulary", {
  # the title renders as a table only under title_format = "table"
  b <- .block(rtf_default_format(title_format = "table", title_width = "page"),
              text = "A title")
  expect_equal(b$cellx, W)
  b2 <- .block(rtf_default_format(title_format = "table"), text = "A title")
  expect_equal(b2$cellx, rtfreporter:::.content_width_twips(.tbl(), W))
})

test_that("rtf_default_format() validates the widths", {
  expect_error(rtf_default_format(footnote_width = "wide"), "content")
  expect_error(rtf_default_format(title_width = -1), "content")
})

test_that("rtf_default_format() carries the fields", {
  fmt <- rtf_default_format(title_width = "page", footnote_width = 0.6)
  expect_equal(fmt$title_width, "page")
  expect_equal(fmt$footnote_width, 0.6)
  expect_null(rtf_default_format()$title_width)
  expect_null(rtf_default_format()$footnote_width)
})

test_that("the rtfreporter.* options are honoured", {
  op <- options(rtfreporter.footnote_width = "page")
  on.exit(options(op), add = TRUE)
  expect_equal(rtf_default_format()$footnote_width, "page")
})

# ──────── the header / footer band ─────────────────────────────────────────

.hdr_cellx <- function(hdr) {
  doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
    rtf_section(page = 1, secinfo = list(header = hdr)) |>
    rtf_tables(.tbl())
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  hit <- grep("{\\header", readLines(f, warn = FALSE), value = TRUE, fixed = TRUE)
  as.integer(sub("cellx", "", unique(unlist(
    regmatches(hit, gregexpr("cellx[0-9]+", hit))))))
}

test_that("a header with no width still spans the writable width", {
  expect_equal(max(.hdr_cellx(rtf_header(rows = list(c(l = "L", r = "R"))))), W)
})

test_that("the header takes 'page', a fraction and twips", {
  rows <- list(c(l = "L", r = "R"))
  expect_equal(max(.hdr_cellx(rtf_header(rows, width = "page"))), W)
  expect_equal(max(.hdr_cellx(rtf_header(rows, width = 0.5))),
               as.integer(round(W * 0.5)))
  expect_equal(max(.hdr_cellx(rtf_header(rows, width = 6000L))), 6000L)
})

test_that("the legacy width_twips still wins", {
  rows <- list(c(l = "L", r = "R"))
  expect_equal(max(.hdr_cellx(rtf_header(rows, width_twips = 5000L,
                                         width = "page"))), 5000L)
})

test_that("rtf_footer() takes the same argument", {
  expect_equal(rtf_footer(list(c(l = "x")), width = 0.5)$width, 0.5)
  expect_error(rtf_footer(list(c(l = "x")), width = "wide"), "content")
})

# ──────── nothing changes when nothing is set ──────────────────────────────

test_that("output is byte-identical when no width is given", {
  render <- function(fmt) {
    doc <- rtf_document(page = rtf_page(orientation = "landscape"),
                        default_format = fmt) |>
      rtf_tables(.tbl()) |>
      rtf_titles(list("A title")) |>
      rtf_footnotes(list(c("Footnote line 1", "line 2")))
    f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)
    generate_rtfreport(doc, f, overwrite = TRUE)
    readLines(f, warn = FALSE)
  }
  expect_identical(render(rtf_default_format()),
                   render(rtf_default_format(title_width = "content",
                                             footnote_width = "content")))
})
