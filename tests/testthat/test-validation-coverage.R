# Every argument of every exported function, and every rtfreporter.* option
# the package reads, has an active FR that names it.  The gaps known today are
# listed in validation-coverage-gaps.csv; the list may only shrink: a new
# argument or option without an FR fails here, and so does a listed gap that
# an FR now covers (take it off the list).  Deprecated exports are left out.

.cov_requirements <- function() {
  p <- system.file("validation", "requirements.csv", package = "rtfreporter")
  req <- utils::read.csv(p, colClasses = "character", na.strings = character(0))
  req[req$status == "active", , drop = FALSE]
}

# A statement names `x` as a whole word, or through a glob such as
# cell_padding_*_twips or rtfreporter.page.*
.cov_named <- function(x, statements) {
  word <- paste0("(^|[^A-Za-z0-9_.])", gsub(".", "\\.", x, fixed = TRUE),
                 "($|[^A-Za-z0-9_])")
  if (any(grepl(word, statements, perl = TRUE))) return(TRUE)
  globs <- unique(unlist(regmatches(statements,
    gregexpr("[A-Za-z0-9_.]*\\*[A-Za-z0-9_.*]*", statements))))
  # rtfreporter.* (every option) names none of them in particular
  globs <- globs[grepl("[A-Za-z]", globs) & globs != "rtfreporter.*"]
  pats <- paste0("^", gsub("\\*", "[A-Za-z0-9_.]*",
                           gsub(".", "\\.", globs, fixed = TRUE)), "$")
  any(vapply(pats, grepl, logical(1), x = x))
}

.cov_gaps <- function() {
  act <- .cov_requirements()
  fns <- lapply(strsplit(act$functions, ";", fixed = TRUE), trimws)
  ns <- asNamespace("rtfreporter")
  exports <- setdiff(sort(getNamespaceExports(ns)),
                     get(".deprecated_exports", envir = ns))
  gaps <- list()
  for (f in exports) {
    obj <- get(f, envir = ns)
    if (!is.function(obj)) next
    rows <- vapply(fns, function(x) f %in% x, logical(1))
    for (a in setdiff(names(formals(obj)), "...")) {
      if (!any(rows) || !.cov_named(a, act$statement[rows])) {
        gaps[[length(gaps) + 1L]] <- c(what = "argument", name = paste0(f, "(", a, ")"))
      }
    }
  }
  # the options the package's own code reads
  code <- unlist(lapply(ls(ns, all.names = TRUE), function(n) {
    obj <- get(n, envir = ns)
    if (is.function(obj)) deparse(obj) else character(0)
  }))
  opts <- unique(unlist(regmatches(code,
    gregexpr("rtfreporter\\.[A-Za-z0-9_.]*[A-Za-z0-9_]", code))))
  for (o in sort(opts)) {
    if (!.cov_named(o, act$statement)) {
      gaps[[length(gaps) + 1L]] <- c(what = "option", name = o)
    }
  }
  as.data.frame(do.call(rbind, gaps), stringsAsFactors = FALSE)
}

test_that("every exported argument and rtfreporter.* option has an FR (known gaps only shrink)", {
  known <- utils::read.csv(test_path("validation-coverage-gaps.csv"),
                           colClasses = "character")
  gaps <- .cov_gaps()
  new <- setdiff(paste(gaps$what, gaps$name), paste(known$what, known$name))
  closed <- setdiff(paste(known$what, known$name), paste(gaps$what, gaps$name))
  expect(length(new) == 0L, paste0(
    "No active FR names these; add an FR (and its test) to ",
    "inst/validation/requirements.csv:\n", paste(new, collapse = "\n")))
  expect(length(closed) == 0L, paste0(
    "An FR covers these now; take them off validation-coverage-gaps.csv:\n",
    paste(closed, collapse = "\n")))
})

test_that("the coverage check sees a name, a glob, and a missing name", {
  s <- c("rtftable(cell_padding_*_twips = ) pads the cells.",
         "options(rtfreporter.page.*) set the page.",
         "font = is the face.")
  expect_true(.cov_named("cell_padding_left_twips", s))
  expect_true(.cov_named("rtfreporter.page.margin_left_in", s))
  expect_true(.cov_named("font", s))
  expect_false(.cov_named("font_size_half_points", s))
  expect_false(.cov_named("rtfreporter.markup", s))
})
