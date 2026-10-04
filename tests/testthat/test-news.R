# NEWS.md is read by R (news(), R CMD check): a level-1 heading is a
# version.  A line that starts with "# " anywhere -- an R comment in a code
# block included, since a markdown reader that sees the block's fence
# differently takes it as a heading -- must be a version's heading, or the
# check notes "Cannot extract version info".

test_that("every level-1 line of NEWS.md is a version", {
  news <- system.file("NEWS.md", package = "rtfreporter")
  skip_if(!nzchar(news), "NEWS.md is not installed")
  txt <- readLines(news, encoding = "UTF-8", warn = FALSE)
  h1 <- grep("^# ", txt, value = TRUE)
  ok <- grepl("^# rtfreporter ([0-9.]+|[(]development version[)])$", h1)
  expect_identical(h1[!ok], character())
})

test_that("R reads a version from every section of NEWS.md", {
  skip_if_not_installed("commonmark")
  skip_if_not_installed("xml2")
  news <- system.file("NEWS.md", package = "rtfreporter")
  skip_if(!nzchar(news), "NEWS.md is not installed")
  expect_no_warning(tools:::.build_news_db_from_package_NEWS_md(news))
})
