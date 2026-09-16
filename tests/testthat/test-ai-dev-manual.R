## tests/testthat/test-ai-dev-manual.R
##
## `inst/ai/rtfreporter-ai-dev-manual.md` is the briefing a contributor
## attaches to an AI chat session before working ON the package.  Like its
## user-facing twin it is only worth having while it is true, and an assistant
## cannot tell a stale manual from a fresh one -- so the claims that can be
## checked mechanically are checked here.
##
## They now ship in inst/ai/ (#463), so they are present in an installed
## package too -- no skip needed for the file itself.

library(testthat)

.dev_manual_path <- function() {
  test_path("..", "..", "inst", "ai", "rtfreporter-ai-dev-manual.md")
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
  user_path <- test_path("..", "..", "inst", "ai",
                         "rtfreporter-ai-user-manual.md")
  skip_if_not(file.exists(user_path), "AI user manual not present")
  usr <- readLines(user_path, encoding = "UTF-8", warn = FALSE)

  expect_true(any(grepl("rtfreporter-ai-user-manual.md", dev, fixed = TRUE)),
              info = "the dev manual should name its user-facing twin")
  # Each declares its own scope, so an assistant knows which one it holds.
  expect_true(any(grepl("^> \\*\\*Scope", dev)))
  expect_true(any(grepl("^> \\*\\*Scope", usr)))
})

test_that("each manual is reachable from the page written for its reader", {
  readme  <- test_path("..", "..", "README.md")
  article <- test_path("..", "..", "vignettes", "articles", "ai-development.Rmd")
  skip_if_not(file.exists(readme) && file.exists(article),
              "README / contributor article not available")

  # The home page is written for people USING the package: it links the user
  # manual, and points contributors at the article rather than carrying the
  # developer manual itself.
  rl <- readLines(readme, encoding = "UTF-8", warn = FALSE)
  # the link carries the version, so match the stem rather than the file name
  expect_true(any(grepl("rtfreporter-ai-user-manual", rl, fixed = TRUE)),
              info = "the user manual is not linked from the home page")
  expect_true(any(grepl("rtfreporter_ai_manual(", rl, fixed = TRUE)),
              info = "the home page should show the accessor, not only a download")
  expect_true(any(grepl("articles/ai-development.html", rl, fixed = TRUE)),
              info = "the home page does not point contributors at the article")

  # That article is written for people working ON the package: it links the
  # developer manual, and warns against attaching both.
  al <- readLines(article, encoding = "UTF-8", warn = FALSE)
  expect_true(any(grepl("rtfreporter-ai-dev-manual.md", al, fixed = TRUE)),
              info = "the developer manual is not linked from its article")
  expect_true(any(grepl("rtfreporter-ai-user-manual.md", al, fixed = TRUE)),
              info = "the article should name the other manual to warn against attaching both")
})

test_that("the contributor article has its Japanese twin, each linking the other", {
  en <- test_path("..", "..", "vignettes", "articles", "ai-development.Rmd")
  ja <- test_path("..", "..", "vignettes", "articles", "ai-development-ja.Rmd")
  skip_if_not(file.exists(en), "contributor article not available")
  expect_true(file.exists(ja),
              info = "the For-contributors group's convention is an -ja twin per article")
  expect_true(any(grepl("ai-development-ja.html",
                        readLines(en, encoding = "UTF-8", warn = FALSE), fixed = TRUE)))
  expect_true(any(grepl("ai-development.html",
                        readLines(ja, encoding = "UTF-8", warn = FALSE), fixed = TRUE)))
})

test_that("both manuals and the article are declared in _pkgdown.yml", {
  yml <- test_path("..", "..", "_pkgdown.yml")
  skip_if_not(file.exists(yml), "_pkgdown.yml not available")
  L <- readLines(yml, encoding = "UTF-8", warn = FALSE)
  for (f in c("rtfreporter-ai-user-manual", "rtfreporter-ai-dev-manual")) {
    expect_true(any(grepl(f, L, fixed = TRUE)),
                info = paste(f, "has no navbar entry"))
  }
  # pkgdown fails the build on an article that is not in the index, so both
  # the article and its Japanese twin must be listed.
  for (a in c("articles/ai-development", "articles/ai-development-ja")) {
    expect_true(any(grepl(paste0("- ", a, "$"), L)),
                info = paste(a, "is not in the articles index"))
  }
})
