# The flow from an ARD to an RTF file is drawn once and shown in three places:
# the README, Get started and Tables from an ARD.  The copies stay the same.
# (The source tree only: an installed package has no README or vignettes.)

test_that("the flow figure is the same in the README and the two articles", {
  root <- test_path("..", "..")
  files <- file.path(root, c("README.md", "vignettes/rtfreporter.Rmd",
                             "vignettes/articles/tables-from-ard.Rmd"))
  skip_if_not(all(file.exists(files)), "not the source tree")
  flow <- function(f) {
    x <- readLines(f, encoding = "UTF-8")
    s <- grep("^<!-- flow:", x)
    e <- grep("^<!-- /flow -->", x)
    expect_length(s, 1L)
    expect_length(e, 1L)
    x[s:e]
  }
  blocks <- lapply(files, flow)
  expect_identical(blocks[[2]], blocks[[1]])
  expect_identical(blocks[[3]], blocks[[1]])
  expect_true(any(grepl("generate_rtfreport()", blocks[[1]], fixed = TRUE)))
})
