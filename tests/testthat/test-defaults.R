# Configurable package defaults: option seeding, resolution precedence,
# snapshot/reset, and explicit header/footer band distance (#111).

.gen <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

.doc1 <- function(page = NULL) {
  d <- if (is.null(page)) rtf_document() else rtf_document(page = page)
  d |> rtf_tables(as_rtftables(data.frame(a = "1", b = "2",
                                          stringsAsFactors = FALSE)))
}

test_that("factory defaults are seeded as rtfreporter.* options at load", {
  expect_identical(getOption("rtfreporter.page.paper_size"), "letter")
  expect_identical(getOption("rtfreporter.font"), "Courier New")
  expect_identical(getOption("rtfreporter.font_size_half_points"), 18L)
})

test_that("rtfreporter_options() snapshots the resolved values", {
  old <- options(rtfreporter.font = "Arial")
  on.exit(options(old), add = TRUE)
  snap <- rtfreporter_options()
  expect_identical(snap$rtfreporter.font, "Arial")            # option override
  expect_identical(snap$rtfreporter.page.margin_left_in, 0.75) # factory fallback
})

test_that("an option override flows into rtf_document() defaults", {
  old <- options(rtfreporter.font = "Arial",
                 rtfreporter.page.paper_size = "A4")
  on.exit(options(old), add = TRUE)
  doc <- rtf_document()
  expect_identical(doc$document$font_table[[1]]$name, "Arial")
  expect_identical(doc$document$page$paper_size, "A4")
  # And it reaches the rendered RTF (Arial in the font table).
  expect_true(grepl("Arial", .gen(.doc1())))
})

test_that("an explicit argument beats the option", {
  old <- options(rtfreporter.font = "Arial")
  on.exit(options(old), add = TRUE)
  doc <- rtf_document(font_table = list(list(name = "Times")))
  expect_identical(doc$document$font_table[[1]]$name, "Times")
})

test_that("rtfreporter_reset_defaults() restores the factory baseline", {
  old <- options(rtfreporter.font = "Arial",
                 rtfreporter.page.margin_top_in = 2.0)
  on.exit(options(old), add = TRUE)
  rtfreporter_reset_defaults()
  expect_identical(getOption("rtfreporter.font"), "Courier New")
  expect_identical(getOption("rtfreporter.page.margin_top_in"), 0.75)
})

# Header / footer band distance -------------------------------------------

test_that("header/footer distance defaults to the full margin when unset", {
  # Default top/bottom margin 0.75in = 1080 twips -> band = full margin = 1080.
  rtf <- .gen(.doc1())
  expect_match(rtf, "\\\\headery1080")
  expect_match(rtf, "\\\\footery1080")
})

test_that("explicit header_dist_in / footer_dist_in override the full-margin rule (#111)", {
  rtf <- .gen(.doc1(list(header_dist_in = 0.5, footer_dist_in = 0.25)))
  expect_match(rtf, "\\\\headery720")   # 0.5in
  expect_match(rtf, "\\\\footery360")   # 0.25in
})

test_that("a header/footer distance option is honoured, and the page key wins (#111)", {
  old <- options(rtfreporter.page.header_dist_in = 0.5)
  on.exit(options(old), add = TRUE)
  expect_match(.gen(.doc1()), "\\\\headery720")             # from option
  # Explicit page key beats the option.
  expect_match(.gen(.doc1(list(header_dist_in = 0.25))), "\\\\headery360")
})

test_that("FT-DC6804 [FR-DC388]: the factory baseline names every rtfreporter.* option the package reads and every one its help names", {
  nms <- names(rtfreporter:::.rtfreporter_factory_defaults())
  opt_re <- "rtfreporter[.][a-z][a-z_.]*[a-z_]"
  # read: each "rtfreporter.<name>" string in the package's functions
  ns <- asNamespace("rtfreporter")
  src <- unlist(lapply(ls(ns, all.names = TRUE), function(n) {
    f <- get(n, envir = ns)
    if (is.function(f)) deparse(f)
  }))
  lit <- unlist(regmatches(src, gregexpr(paste0("\"", opt_re, "\""), src)))
  read <- unique(gsub("\"", "", lit, fixed = TRUE))
  expect_true(all(c("rtfreporter.program", "rtfreporter.tokens",
                    "rtfreporter.print_ascii") %in% read))
  expect_identical(setdiff(read, nms), character(0))
  # the help: each name in the Rd files; a family (rtfreporter.page.*,
  # rtfreporter.cell_padding_*_twips) is read as "rtfreporter.page" and
  # "rtfreporter.cell_padding_", and needs one option of its own
  db <- tryCatch(tools::Rd_db("rtfreporter"), error = function(e) list())
  if (!length(db) && dir.exists(test_path("..", "..", "man"))) {
    db <- tools::Rd_db(dir = test_path("..", ".."))
  }
  skip_if(!length(db), "no help pages to read")
  txt <- vapply(db, function(rd) paste(as.character(rd), collapse = ""), "")
  named <- unique(unlist(regmatches(txt, gregexpr(opt_re, txt))))
  expect_true("rtfreporter.toc_style" %in% named)
  known <- vapply(named, function(p) {
    p %in% nms || any(startsWith(nms, paste0(p, "."))) ||
      (endsWith(p, "_") && any(startsWith(nms, p)))
  }, TRUE)
  expect_identical(named[!known], character(0))
})
