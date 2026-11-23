# Every skip message of the tests is said for a reader in validation/skip-reasons.csv
# (the Quality Report maps a message to its sentence through that file), and no row
# is left for a message that is gone.

# The messages of the skip(), skip_if() and skip_if_not() calls in `files`
.skip_messages <- function(files) {
  out <- character()
  walk <- function(e) {
    if (is.call(e)) {
      fn <- if (is.name(e[[1L]])) as.character(e[[1L]]) else ""
      if (fn %in% c("skip", "skip_if", "skip_if_not")) {
        a <- as.list(e)[-1L]
        nm <- names(a)
        if (is.null(nm)) nm <- rep("", length(a))
        i <- which(nm == "message")
        if (!length(i)) i <- if (fn == "skip") 1L else 2L
        m <- if (length(a) >= i[1L]) a[[i[1L]]] else NULL
        if (is.character(m)) {
          out <<- c(out, m)
        } else if (is.call(m) && identical(m[[1L]], as.name("paste"))) {
          # the one built message: a file's name and a fixed text
          for (f in c("README.md", "_pkgdown.yml")) {
            out <<- c(out, eval(m, list(f = f)))
          }
        }
      }
      for (i in seq_along(e)) {
        # (an empty argument, as in x[, 1], has no value to walk)
        if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
      }
    } else if (is.expression(e) || is.pairlist(e)) {
      for (i in seq_along(e)) walk(e[[i]])
    }
    invisible(NULL)
  }
  for (f in files) walk(parse(f, keep.source = FALSE, encoding = "UTF-8"))
  unique(out)
}

test_that("every skip message of the tests is in validation/skip-reasons.csv", {
  skip_on_cran()
  csv <- test_path("..", "..", "validation", "skip-reasons.csv")
  skip_if_not(file.exists(csv), "needs validation/skip-reasons.csv of the source checkout; runs where the tests run from the checkout")
  tab <- utils::read.csv(csv, colClasses = "character", na.strings = character(), encoding = "UTF-8")
  expect_identical(names(tab), c("reason", "file", "why", "ok"))
  expect_true(all(nzchar(tab$why)))

  files <- list.files(test_path(), pattern = "^(test|helper).*\\.R$", full.names = TRUE)
  msgs <- .skip_messages(files)
  expect_gt(length(msgs), 20L)

  pat <- startsWith(tab$reason, "^")
  covered <- function(m) {
    any(tab$reason[!pat] == m) ||
      any(vapply(tab$reason[pat], function(r) grepl(r, m, perl = TRUE), NA))
  }
  missing_msgs <- msgs[!vapply(msgs, covered, NA)]
  expect_identical(missing_msgs, character())

  # no row for a message that is gone; testthat's own messages ("On Linux" of
  # skip_on_os(), "Installed ..." and "{pkg} is not installed" of
  # skip_if_not_installed()) have no skip call here
  testthat_own <- c("On Linux", "On Mac")
  gone <- tab$reason[!pat & !(tab$reason %in% msgs) & !(tab$reason %in% testthat_own)]
  expect_identical(gone, character())
  gone_pat <- tab$reason[pat][!vapply(tab$reason[pat], function(r) any(grepl(r, msgs, perl = TRUE)), NA)]
  expect_setequal(gone_pat, c("^Installed (\\S+) is version (\\S+); but (\\S+) is required$",
                              "^\\{(.+)\\} is not installed$"))
})
