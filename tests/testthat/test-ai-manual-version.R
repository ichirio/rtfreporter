## tests/testthat/test-ai-manual-version.R
##
## #463: the manuals ship inside the package so they cannot disagree with the
## code they document -- but only if the version they claim is kept honest.
## An assistant cannot tell a stale manual from a fresh one, so the stamp, the
## accessor and the links that name a version are all asserted here.

library(testthat)

.pkg_version <- function() as.character(utils::packageVersion("rtfreporter"))

.ai_file <- function(which) {
  system.file("ai", sprintf("rtfreporter-ai-%s-manual.md", which),
              package = "rtfreporter")
}

test_that("both manuals are installed with the package", {
  for (w in c("user", "dev")) {
    p <- .ai_file(w)
    expect_true(nzchar(p) && file.exists(p),
                info = paste(w, "manual is not in inst/ai/"))
  }
})

test_that("each manual states the version it was built from", {
  v <- .pkg_version()
  for (w in c("user", "dev")) {
    head5 <- readLines(.ai_file(w), n = 5L, encoding = "UTF-8", warn = FALSE)
    expect_true(any(grepl(v, head5, fixed = TRUE)),
                info = paste0("the ", w, " manual does not state ", v,
                              " in its first five lines -- bump the stamp"))
  }
})

test_that("rtfreporter_ai_manual() returns the installed path", {
  expect_identical(rtfreporter_ai_manual(), .ai_file("user"))
  expect_identical(rtfreporter_ai_manual("dev"), .ai_file("dev"))
  expect_error(rtfreporter_ai_manual("nope"), "should be one of")
})

test_that("rtfreporter_ai_manual(file = ) copies it out", {
  dest <- tempfile(fileext = ".md"); on.exit(unlink(dest), add = TRUE)
  out <- rtfreporter_ai_manual(file = dest)
  expect_identical(out, dest)
  expect_true(file.exists(dest))
  expect_identical(readLines(dest, warn = FALSE),
                   readLines(.ai_file("user"), warn = FALSE))

  # a second copy needs `overwrite`
  expect_error(rtfreporter_ai_manual(file = dest), "overwrite")
  expect_no_error(rtfreporter_ai_manual(file = dest, overwrite = TRUE))

  # a directory keeps the manual's own name
  d <- file.path(tempdir(), "ai-copy"); dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_identical(basename(rtfreporter_ai_manual("dev", file = d)),
                   "rtfreporter-ai-dev-manual.md")
})

test_that("the public links name the newest RELEASE, not the dev version", {
  # A reader following a link from the home page has installed a release, so
  # the link must name the released manual -- which also means it changes once
  # per release rather than every time the development counter moves.
  repo <- test_path("..", "..")
  skip_if_not(dir.exists(file.path(repo, ".git")), "not a source checkout")
  tags <- suppressWarnings(system2("git", c("-C", repo, "tag", "-l", "v*",
                                            "--sort=-v:refname"),
                                   stdout = TRUE, stderr = FALSE))
  skip_if(length(tags) == 0L, "no release tags")
  rel <- sub("^v", "", tags[[1L]])

  for (f in c("README.md", "_pkgdown.yml")) {
    p <- file.path(repo, f)
    skip_if_not(file.exists(p), paste(f, "not available"))
    L <- readLines(p, encoding = "UTF-8", warn = FALSE)
    hit <- grep("rtfreporter-ai-(user|dev)-manual-[0-9]", L, value = TRUE)
    expect_gt(length(hit), 0L)
    stale <- hit[!grepl(rel, hit, fixed = TRUE)]
    expect_identical(stale, character(0),
      info = paste0(f, " links a version that is not the newest release (",
                    rel, ") -- update it when you cut one: ",
                    paste(trimws(stale), collapse = " | ")))
  }
})

test_that("the version-stamped file the links point at is the one published", {
  # The workflow writes docs/ai/<name>-<version>.md from inst/ai/ + DESCRIPTION;
  # this asserts the naming the links assume, so a rename cannot go unnoticed.
  v <- .pkg_version()
  p <- test_path("..", "..", ".github", "workflows", "pkgdown.yaml")
  skip_if_not(file.exists(p), "workflow not available (installed package)")
  L <- readLines(p, encoding = "UTF-8", warn = FALSE)
  expect_true(any(grepl('cp "$f" "docs/ai/${name}-${version}.md"', L, fixed = TRUE)),
              info = "the workflow no longer publishes a version-stamped copy")
  expect_true(any(grepl('cp "$f" "docs/ai/${name}.md"', L, fixed = TRUE)),
              info = "the workflow no longer publishes the stable alias")
  # released copies are regenerated from the tags, not left on the branch --
  # gh-pages is rewritten on every deploy (force_orphan)
  expect_true(any(grepl("git tag -l 'v*' --sort=v:refname", L, fixed = TRUE)),
              info = "the workflow no longer republishes the released manuals from their tags")
  expect_true(any(grepl("fetch-depth: 0", L, fixed = TRUE)),
              info = "reading the tags needs full history in the checkout")
  expect_true(any(grepl("grep '^Version:' DESCRIPTION", L, fixed = TRUE)),
              info = "the workflow no longer reads the version from DESCRIPTION")
  expect_true(nzchar(v))
})
