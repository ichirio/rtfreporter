.ac_rtf <- function(dir, name, title) {
  p <- file.path(dir, name)
  rtf_document() |>
    rtf_tables(as_rtftables(data.frame(a = title, b = 1))) |>
    generate_rtfreport(p, overwrite = TRUE)
  p
}

test_that("a toc entry finds its file however the path is spelled (#767)", {
  d <- tempfile("acheck"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .ac_rtf(d, "t1.rtf", "First"); f2 <- .ac_rtf(d, "t2.rtf", "Second")
  out <- file.path(d, "book.rtf")
  toc <- data.frame(file = c(normalizePath(f1, "\\"), f2), label = c("First", "Second"),
                    level = 2)
  expect_no_error(assemble_rtf(c(f1, f2), out, overwrite = TRUE, toc = toc))
})

test_that("a toc file that is not .xlsx / .csv is said so (#767)", {
  d <- tempfile("acheck"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f1 <- .ac_rtf(d, "t1.rtf", "First"); f2 <- .ac_rtf(d, "t2.rtf", "Second")
  expect_error(assemble_rtf(c(f1, f2), file.path(d, "book.rtf"), toc = "toc.txt"),
               "must end in .xlsx or .csv", fixed = TRUE)
})

test_that("TOC levels are checked (#767)", {
  expect_error(rtfreporter:::.toc_entry("x", level = 4), "from 1 to 3")
  expect_error(rtfreporter:::.toc_entry("x", level = 0), "from 1 to 3")
  expect_error(rtfreporter:::.toc_entry("x", level = NA), "from 1 to 3")
  expect_error(rtfreporter:::.toc_heading("x", level = 3), "from 1 to 2")
  expect_no_error(rtfreporter:::.toc_entry("x", level = 3))
  expect_no_error(rtfreporter:::.toc_heading("x", level = 2))
})
