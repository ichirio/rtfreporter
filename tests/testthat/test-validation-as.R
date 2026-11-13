# The assembly area's leaf requirements that had no test (the deviations of
# the first validation version): each test names its FT id and its FRs.

.as_rtf <- function(title, dir = tempdir()) {
  doc <- rtf_tables(rtf_document(), list(data.frame(A = 1:2, B = c("x", "y"))),
                    titles = list(c(title)))
  f <- tempfile("as-", tmpdir = dir, fileext = ".rtf")
  generate_rtfreport(doc, f, overwrite = TRUE)
  f
}

.as_png <- function() testthat::test_path("fixtures", "validation", "dot.png")

test_that("FT-AS118 [FR-AS106]: a cover or contents before a first file with no \\sectd is refused", {
  f <- tempfile(fileext = ".rtf")
  writeLines(c("{\\rtf1\\ansi", "\\pard plain text\\par", "}"), f)
  g <- .as_rtf("Table 14.2.1")
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f, g, out)), add = TRUE)
  expect_error(assemble_rtf(c(f, g), out, toc = c("A", "B"), overwrite = TRUE),
               "File 1 has no \\sectd; cannot insert cover / TOC.", fixed = TRUE)
})

test_that("FT-AS119 [FR-AS123 FR-AS124 FR-AS129]: a toc of a kind assemble_rtf() does not read, and a toc_leader not listed, are refused", {
  f <- c(.as_rtf("Table 14.1.1"), .as_rtf("Table 14.2.1"))
  out <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f, out)), add = TRUE)
  # a list whose element is neither a heading nor an entry
  expect_error(assemble_rtf(f, out, toc = list(1), overwrite = TRUE),
               "`toc` must be \"auto\", a character vector of labels, a table (data.frame) or the path of one.",
               fixed = TRUE)
  # a number, a logical, a function
  msg <- paste0("`toc` must be NULL, \"auto\", a character vector, a table (columns file, ",
                "label, heading, level) or the path of one (.xlsx / .csv).")
  for (bad in list(1, TRUE, identity)) {
    expect_error(assemble_rtf(f, out, toc = bad, overwrite = TRUE), msg, fixed = TRUE)
  }
  expect_error(assemble_rtf(f, out, toc = "auto", toc_leader = "dash", overwrite = TRUE),
               "'arg' should be one of", fixed = TRUE)
})

test_that("FT-AS120 [FR-AS155 FR-AS158]: assemble_folder(recursive = ) reaches the sub-folders; its dots go on to assemble_rtf()", {
  d <- tempfile("asf-")
  dir.create(file.path(d, "sub"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  .as_rtf("Table 14.1.1", d)
  .as_rtf("Table 14.2.1", file.path(d, "sub"))
  expect_identical(nrow(assemble_folder(d)), 1L)
  expect_identical(nrow(assemble_folder(d, recursive = TRUE)), 2L)
  out <- file.path(d, "all.rtf")
  assemble_folder(d, out, recursive = TRUE, cover = list(title = "STUDY-01"))
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("STUDY-01", txt, fixed = TRUE))
  # an argument assemble_rtf() does not have is refused there
  expect_error(assemble_folder(d, file.path(d, "x.rtf"), nope = 1), "nope")
})

test_that("FT-AS121 [FR-AS183]: a table given to rtf_figures() is refused, naming the item", {
  expect_error(rtf_figures(rtf_document(), list(.as_png(), data.frame(a = 1))),
               "Item 2: a 'data.frame' is not a figure; a table goes in rtf_tables().",
               fixed = TRUE)
  expect_error(rtf_figures(rtf_document(), list(rtftable(data.frame(a = 1)))),
               "Item 1: a 'rtftable' is not a figure; a table goes in rtf_tables().",
               fixed = TRUE)
})

test_that("FT-AS122 [FR-AS191 FR-AS192]: rtf_figures(space_before = ) is the empty lines before a figure that shares the page; anything but a whole number of 0 or more is refused", {
  base <- rtf_tables(rtf_document(), list(data.frame(a = 1)))
  doc0 <- rtf_figures(base, .as_png(), page_break = FALSE)
  doc2 <- rtf_figures(base, .as_png(), page_break = FALSE, space_before = 2L)
  expect_identical(doc0$contents[[2L]]$space_before, 0L)
  expect_identical(doc2$contents[[2L]]$space_before, 2L)
  txt <- function(doc) {
    f <- tempfile(fileext = ".rtf")
    on.exit(unlink(f), add = TRUE)
    generate_rtfreport(doc, f, overwrite = TRUE)
    readLines(f, warn = FALSE)
  }
  expect_identical(length(txt(doc2)) - length(txt(doc0)), 2L)
  msg <- "rtf_figures(): `space_before` is a whole number of empty lines, 0 or more."
  for (bad in list(-1, 1.5, NA, "2", c(1, 2))) {
    expect_error(rtf_figures(base, .as_png(), page_break = FALSE, space_before = bad), msg,
                 fixed = TRUE)
  }
})

test_that("FT-AS123 [FR-AS227]: rtfreporter_ai_manual() in an installation without the manual says where the published copy is", {
  # the function as it is, in an installation whose inst/ai/ finds nothing
  e <- new.env(parent = environment(rtfreporter_ai_manual))
  e$system.file <- function(...) ""
  manual <- rtfreporter_ai_manual
  environment(manual) <- e
  expect_error(manual(),
               "`rtfreporter_ai_manual()`: rtfreporter-ai-user-manual.md is not in this installation.",
               fixed = TRUE)
  expect_error(manual(), "https://ichirio.github.io/rtfreporter/ai/", fixed = TRUE)
})
