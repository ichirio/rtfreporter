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
  expect_identical(rtfreporter_ai_manual(which = "dev"), .ai_file("dev"))
  expect_error(rtfreporter_ai_manual(which = "nope"), "should be one of")
})

test_that("rtfreporter_ai_manual(dir = ) writes the kit, or the dev manual", {
  d <- file.path(tempdir(), "ai-kit"); unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out <- rtfreporter_ai_manual(d)
  expect_identical(basename(out), c("00_START_HERE.md", "01_rtfreporter-ai-user-manual.md",
                                    "02_example_orr_subgroups.R", "03_example_ae_soc_pt.R",
                                    "04_example_demographics.R"))
  expect_identical(readLines(file.path(d, "01_rtfreporter-ai-user-manual.md"), warn = FALSE),
                   readLines(.ai_file("user"), warn = FALSE))
  # no ARD: 00 says how to add one
  expect_true(any(grepl("rtfreporter_ai_manual(dir, ard = ard)",
                        readLines(file.path(d, "00_START_HERE.md"), warn = FALSE), fixed = TRUE)))
  # a second kit needs `overwrite`
  expect_error(rtfreporter_ai_manual(d), "overwrite")
  expect_no_error(rtfreporter_ai_manual(d, overwrite = TRUE))
  # the dev manual alone
  dd <- file.path(tempdir(), "ai-dev"); unlink(dd, recursive = TRUE)
  on.exit(unlink(dd, recursive = TRUE), add = TRUE)
  expect_identical(basename(rtfreporter_ai_manual(dd, which = "dev")),
                   "rtfreporter-ai-dev-manual.md")
  # a file is not a folder
  f <- tempfile(); writeLines("x", f); on.exit(unlink(f), add = TRUE)
  expect_error(rtfreporter_ai_manual(f), "not a folder")
})

test_that("rtfreporter_ai_manual(dir, ard = ): the ARD's rows and its shape", {
  skip_if_not_installed("cards")
  adsl <- cards::ADSL
  adsl$ALL <- "All"
  ard <- cards::bind_ard(
    cards::ard_tabulate(adsl, variables = ARM),
    cards::ard_tabulate(adsl, by = c(ARM, ALL), variables = DISCONFL),
    cards::ard_tabulate(adsl, by = c(ARM, SEX), variables = DISCONFL),
    cards::ard_tabulate(adsl, by = c(ARM, AGEGR1, SEX), variables = DISCONFL))
  d <- file.path(tempdir(), "ai-kit-ard"); unlink(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out <- rtfreporter_ai_manual(d, ard = ard)
  expect_true(all(c("05_ard.csv", "06_ard_shape.md") %in% basename(out)))
  csv <- utils::read.csv(file.path(d, "05_ard.csv"))
  expect_identical(nrow(csv), nrow(normalize_ard(ard)))
  expect_true(all(c("ARM", "SEX", "AGEGR1", "variable", "stat_name", "stat") %in% names(csv)))
  shape <- readLines(file.path(d, "06_ard_shape.md"), warn = FALSE)
  expect_true(any(grepl("| `ARM` |", shape, fixed = TRUE)))
  expect_true(any(grepl("parallel subgroups", shape, fixed = TRUE)))
  expect_true(any(grepl('c("SEX", "AGEGR1")', shape, fixed = TRUE)))   # the crossed layer
  expect_true(any(grepl("table_plan(cols = \"ARM\"", shape, fixed = TRUE)))
  expect_true(any(grepl("A key's own counts (`ARM`)", shape, fixed = TRUE)))
})

test_that("the kit's examples are the three-step layout and use only exported functions", {
  kit <- system.file("ai", "kit", package = "rtfreporter")
  skip_if(!nzchar(kit), "the kit is not installed")
  ex <- names(asNamespace("rtfreporter")$.__NAMESPACE__.$exports)
  for (f in c("02_example_orr_subgroups.R", "03_example_ae_soc_pt.R", "04_example_demographics.R")) {
    src <- readLines(file.path(kit, f), warn = FALSE)
    for (step in c("# ---- 1. the table", "# ---- 2. the document", "# ---- 3. the file")) {
      expect_true(any(startsWith(src, step)), info = paste(f, step))
    }
    used <- unique(regmatches(src, gregexpr("\\b(plan_[a-z_]+|rtf_[a-z_]+|normalize_ard|table_plan|generate_rtfreport|cell_rows|col_cell)\\(", src)))
    used <- sub("[(]$", "", unlist(used))
    expect_true(all(used %in% ex), info = paste(f, paste(setdiff(used, ex), collapse = ", ")))
  }
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
  expect_true(any(grepl('cp "inst/ai/${name}.md" "docs/ai/${name}-${version}.md"',
                        L, fixed = TRUE)),
              info = "the workflow no longer publishes the development build under its own version")
  # released copies are regenerated from the tags, not left on the branch --
  # gh-pages is rewritten on every deploy (force_orphan)
  expect_true(any(grepl("git tag -l 'v*' --sort=v:refname", L, fixed = TRUE)),
              info = "the workflow no longer republishes the released manuals from their tags")
  expect_true(any(grepl("fetch-depth: 0", L, fixed = TRUE)),
              info = "reading the tags needs full history in the checkout")
  # the unversioned alias tracks the newest RELEASE, not this build
  expect_true(any(grepl('git show "${latest}:inst/ai/${name}.md" > "docs/ai/${name}.md"',
                        L, fixed = TRUE)),
              info = "the alias no longer tracks the newest release")
  expect_true(any(grepl("grep '^Version:' DESCRIPTION", L, fixed = TRUE)),
              info = "the workflow no longer reads the version from DESCRIPTION")
  expect_true(nzchar(v))
})
