## tests/testthat/test-print-methods.R
##
## print() methods for the three central S3 objects (rtftable / rtfplot /
## rtfreport) and the shared metadata-token resolver.  The print methods are
## expected to surface information meaningful to a reporting programmer, so the
## tests assert that the key facts (dimensions, labels, geometry, a body
## preview) actually appear -- not merely that print() runs.

library(testthat)

# ── .resolve_meta_tokens(): the shared resolver behind every adapter ──────────

test_that(".resolve_meta_tokens() resolves FALSE / TRUE / vector / bad token", {
  allowed <- c("col_header", "alignment", "spanning")
  expect_identical(rtfreporter:::.resolve_meta_tokens(FALSE, allowed, "x"),
                   character(0))
  expect_identical(rtfreporter:::.resolve_meta_tokens(NULL,  allowed, "x"),
                   character(0))
  expect_identical(rtfreporter:::.resolve_meta_tokens(TRUE,  allowed, "x"),
                   allowed)
  expect_identical(rtfreporter:::.resolve_meta_tokens(c("alignment"), allowed, "x"),
                   "alignment")
  expect_error(rtfreporter:::.resolve_meta_tokens("nope", allowed, "myadapter"),
               "Unknown myadapter")
  expect_error(rtfreporter:::.resolve_meta_tokens(1L, allowed, "x"),
               "must be FALSE/TRUE")
})

test_that("the per-adapter wrappers delegate to the shared resolver", {
  # gt wrapper (read_meta arg) and the others (read arg) all funnel here.
  expect_identical(rtfreporter:::.resolve_gt_tokens(FALSE), character(0))
  expect_setequal(rtfreporter:::.resolve_gt_tokens(TRUE),
                  rtfreporter:::.GT_META_TOKENS)
  expect_error(rtfreporter:::.resolve_gt_tokens("junk"), "Unknown gt")
})


# ── print.rtftable ───────────────────────────────────────────────────────────

.make_demo_rtftable <- function() {
  df <- data.frame(
    Characteristic = c("Age (years)", "  Mean (SD)", "Sex", "  Female"),
    A = c("", "75.1 (8.2)", "", "53 (54%)"),
    B = c("", "74.4 (7.9)", "", "48 (49%)"),
    check.names = FALSE)
  as_rtftable(df,
    col_header   = c("Characteristic", "Drug A\nN = 98", "Placebo\nN = 98"),
    col_rel_width = c(50, 25, 25))
}

test_that("print.rtftable renders the table body plus a metadata block", {
  rt  <- .make_demo_rtftable()
  out <- paste(capture.output(print(rt)), collapse = "\n")
  # Visual body: rendered header labels and actual cell content.
  expect_match(out, "Characteristic")                  # leaf header rendered
  expect_match(out, "Drug A")                           # header label rendered
  expect_match(out, "75.1 \\(8.2\\)")                   # an actual rendered cell
  expect_match(out, "Mean \\(SD\\)")                    # a row-title cell
  # Metadata block still surfaces the key facts.
  expect_match(out, "<rtftable> 4 rows x 3 columns")
  expect_match(out, "Columns:.*Characteristic")
  expect_match(out, "Widths:.*relative 50:25:25")
  expect_match(out, "Row title:  col 1")
})

test_that("print.rtftable draws horizontal rules where borders are set", {
  rt  <- .make_demo_rtftable()                          # tfl: header top + bottom
  out <- format(rt)
  # At least two full-width rule lines (header top and header bottom).
  rules <- grepl("^[─═━-]+$", out)
  expect_gte(sum(rules), 2L)
})

test_that("print.rtftable renders spanning headers centred with an underline", {
  df <- data.frame(g = c("A", "B"), n1 = c("1", "2"), n2 = c("3", "4"),
                   check.names = FALSE)
  hdr <- list(
    list(list(pos = 1, label = ""), list(pos = c(2, 3), label = "Treated")),
    c("Group", "x", "y"))
  rt <- rtftable(df, col_header = hdr,
    border = rtfreporter:::.rtf_table_border(
      spanning = rtf_border(bottom = rtf_border_side())))
  out <- format(rt)
  expect_true(any(grepl("Treated", out)))
  # The line holding "Treated" is not the leaf label line.
  span_line <- out[grepl("Treated", out)][[1L]]
  expect_false(grepl("Group", span_line))
})

test_that("print.rtftable right-aligns columns per col_spec", {
  df <- data.frame(k = c("a", "b"), v = c("1", "1000"), check.names = FALSE)
  rt <- rtftable(df, col_header = c("k", "v"),
                 col_spec = list(list(col = 2, align = "right")))
  out <- format(rt)
  data_lines <- grep("\\b1\\b", out, value = TRUE)
  # "1" is padded on the left to line up under "1000".
  expect_true(any(grepl("   1$", data_lines)))
})

test_that("format.rtftable honours the ASCII fallback option", {
  rt  <- .make_demo_rtftable()
  old <- options(rtfreporter.print_ascii = TRUE)
  on.exit(options(old), add = TRUE)
  out <- format(rt)
  expect_false(any(grepl("[─═]", out)))       # no box-drawing glyphs
  expect_true(any(grepl("^-+$", out)))                  # ASCII rule instead
})

test_that("print.rtftable truncates long bodies with a 'more rows' note", {
  df <- data.frame(k = as.character(1:20), v = as.character(1:20),
                   check.names = FALSE)
  rt  <- rtftable(df, col_header = c("k", "v"))
  out <- format(rt, n = 5L)
  expect_true(any(grepl("15 more rows", out)))
})

test_that("print.rtftable renders only the first table of a multi-DF rtftable", {
  d1 <- data.frame(x = c("p", "q"), y = c("1", "2"), check.names = FALSE)
  d2 <- data.frame(x = c("r", "s"), y = c("3", "4"), check.names = FALSE)
  rt  <- rtftable(list(d1, d2), col_header = c("x", "y"))
  out <- format(rt)
  expect_true(any(grepl("\\bp\\b", out)))               # first table's cell
  expect_false(any(grepl("\\br\\b", out)))              # second table not shown
  meta <- paste(capture.output(print(rt)), collapse = "\n")
  expect_match(meta, "<rtftable> 2 tables")
})

test_that("print.rtftable reports attached title / footnote line counts", {
  rt <- .make_demo_rtftable()
  attr(rt, "rtf_titles")    <- c("Table 1", "Demographics")
  attr(rt, "rtf_footnotes") <- "Note: ITT."
  out <- paste(capture.output(print(rt)), collapse = "\n")
  expect_match(out, "Titles:     2 line")
  expect_match(out, "Footnotes:  1 line")
  # Titles / footnotes also render around the table body.
  expect_match(out, "Table 1")
  expect_match(out, "Note: ITT")
})

test_that("summary.rtftable prints the metadata block without the body", {
  rt  <- .make_demo_rtftable()
  out <- paste(capture.output(summary(rt)), collapse = "\n")
  expect_match(out, "<rtftable> 4 rows x 3 columns")
  expect_match(out, "Widths:.*relative 50:25:25")
  expect_false(grepl("75.1 \\(8.2\\)", out))            # no rendered cells
  expect_invisible(summary(rt))
})

test_that("print.rtftable returns its argument invisibly", {
  rt <- .make_demo_rtftable()
  expect_invisible(print(rt))
  expect_identical(withVisible(print(rt))$value, rt)
})


# ── print.rtfplot ────────────────────────────────────────────────────────────

.make_demo_png <- function() {
  p  <- tempfile(fileext = ".png")
  ok <- tryCatch({
    grDevices::png(p, width = 120, height = 80)
    graphics::plot.new()
    grDevices::dev.off()
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(p)) skip("no working PNG graphics device")
  p
}

test_that("print.rtfplot shows native px, display size and alignment", {
  fig <- rtfplot(.make_demo_png(), width_twips = 9000L)
  out <- paste(capture.output(print(fig)), collapse = "\n")
  expect_match(out, "<rtfplot> PNG")
  expect_match(out, "Native size:  120 x 80 px")
  expect_match(out, "6.25 in \\(9000 twips\\)")          # explicit display width
  expect_match(out, "Align:        center")
  expect_invisible(print(fig))
})


# ── print.rtfreport ──────────────────────────────────────────────────────────

test_that("print.rtfreport shows page/section counts, geometry and fonts", {
  doc <- rtf_document(page = list(paper_size = "letter", orientation = "landscape")) |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(.make_demo_rtftable())
  rep <- rtfreporter:::.pipe_doc_to_rtfreport(doc)
  out <- paste(capture.output(print(rep)), collapse = "\n")
  expect_match(out, "<rtfreport> 1 page, 1 section")
  expect_match(out, "Page:.*letter landscape, 11.00 x 8.50 in")
  expect_match(out, "Fonts:")
  expect_match(out, "Colors:")
  expect_invisible(print(rep))
})
