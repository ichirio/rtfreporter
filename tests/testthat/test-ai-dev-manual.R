## tests/testthat/test-ai-dev-manual.R
##
## `pkgdown/assets/rtfreporter-ai-dev-manual.md` is the briefing a contributor
## attaches to an AI chat session before working ON the package.  Like its
## user-facing twin it is only worth having while it is true, and an assistant
## cannot tell a stale manual from a fresh one -- so the claims that can be
## checked mechanically are checked here.
##
## `pkgdown/` is .Rbuildignore'd, so these skip on an installed package.

library(testthat)

.dev_manual_path <- function() {
  test_path("..", "..", "pkgdown", "assets", "rtfreporter-ai-dev-manual.md")
}

.dev_manual_lines <- function() {
  p <- .dev_manual_path()
  skip_if_not(file.exists(p), "AI dev manual not present (installed package)")
  readLines(p, encoding = "UTF-8", warn = FALSE)
}

.backticked <- function(lines, pattern = "`[^`]+`") {
  gsub("`", "", unlist(regmatches(lines, gregexpr(pattern, lines))),
       fixed = TRUE)
}

test_that("every R/ file the manual names exists", {
  lines <- .dev_manual_lines()
  named <- unique(.backticked(lines, "`[A-Za-z_][A-Za-z0-9_]*\\.R`"))
  present <- list.files(test_path("..", ".."), pattern = "[.]R$")   # root-level
  present <- c(present, list.files(test_path("..", "..", "R")),
               list.files(test_path("..", "..", "inst", "resources")))
  missing <- setdiff(named, present)
  expect_identical(missing, character(0),
    info = paste("named in the dev manual but not in the tree:",
                 paste(missing, collapse = ", ")))
})

test_that("the manual's Imports list matches DESCRIPTION", {
  lines <- .dev_manual_lines()
  i <- grep("^2\\. \\*\\*No third-party runtime dependency", lines)
  expect_length(i, 1L)
  claimed <- .backticked(lines[seq(i, i + 2L)], "`[a-zA-Z]+`")
  claimed <- setdiff(claimed, c("Imports:", "Imports", "Suggests:", "Suggests"))

  desc <- read.dcf(test_path("..", "..", "DESCRIPTION"), fields = "Imports")[1L]
  actual <- trimws(strsplit(desc, ",")[[1L]])
  actual <- sub("[[:space:]]*\\(.*\\)$", "", actual)
  actual <- actual[nzchar(actual)]

  expect_setequal(claimed, actual)
})

test_that("the S3-only invariant still holds in R/", {
  r_dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "source tree not available")
  files <- list.files(r_dir, pattern = "[.]R$", full.names = TRUE)
  hits <- Filter(function(f) {
    any(grepl("R6Class|setRefClass|setClass\\(", readLines(f, warn = FALSE)))
  }, files)
  expect_identical(basename(hits), character(0),
    info = "the dev manual states R/ contains no R6/RC/S4 class definitions")
})

test_that("the two manuals point at each other", {
  dev <- .dev_manual_lines()
  user_path <- test_path("..", "..", "pkgdown", "assets",
                         "rtfreporter-ai-user-manual.md")
  skip_if_not(file.exists(user_path), "AI user manual not present")
  usr <- readLines(user_path, encoding = "UTF-8", warn = FALSE)

  expect_true(any(grepl("rtfreporter-ai-user-manual.md", dev, fixed = TRUE)),
              info = "the dev manual should name its user-facing twin")
  # Each declares its own scope, so an assistant knows which one it holds.
  expect_true(any(grepl("^> \\*\\*Scope", dev)))
  expect_true(any(grepl("^> \\*\\*Scope", usr)))
})

test_that("the README links both manuals", {
  readme <- test_path("..", "..", "README.md")
  skip_if_not(file.exists(readme), "README not available")
  L <- readLines(readme, encoding = "UTF-8", warn = FALSE)
  for (f in c("rtfreporter-ai-user-manual.md", "rtfreporter-ai-dev-manual.md")) {
    expect_true(any(grepl(f, L, fixed = TRUE)),
                info = paste(f, "is not linked from the README (the site home page)"))
  }
})

test_that("both manuals are declared in _pkgdown.yml", {
  yml <- test_path("..", "..", "_pkgdown.yml")
  skip_if_not(file.exists(yml), "_pkgdown.yml not available")
  L <- readLines(yml, encoding = "UTF-8", warn = FALSE)
  for (f in c("rtfreporter-ai-user-manual.md", "rtfreporter-ai-dev-manual.md")) {
    expect_true(any(grepl(f, L, fixed = TRUE)),
                info = paste(f, "has no navbar entry"))
  }
})
