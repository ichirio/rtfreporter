# Every argument of every exported function, and every rtfreporter.* option
# the package reads, has an active FR that names it.  An argument is named by
# a leaf FR of that function with `arg =`, `arg` bare inside its call form
# `f(...)`, or a glob (see .fr_names_arg).  An option is named as a word or glob.  The gaps known today are
# listed in validation-coverage-gaps.csv; the list may only shrink: a new
# argument or option without an FR fails here, and so does a listed gap that
# an FR now covers (take it off the list).  Deprecated exports are left out.

.cov_requirements <- function() {
  p <- system.file("validation", "requirements.csv", package = "rtfreporter")
  req <- utils::read.csv(p, colClasses = "character", na.strings = character(0))
  req[req$status == "active", , drop = FALSE]
}

# An option is named by a statement as a whole word, or through a glob such as
# rtfreporter.page.*  (an argument follows the stricter .fr_names_arg rule)
.cov_named <- function(x, statements) {
  word <- paste0("(^|[^A-Za-z0-9_.])", gsub(".", "\\.", x, fixed = TRUE),
                 "($|[^A-Za-z0-9_])")
  any(grepl(word, statements, perl = TRUE)) || .fr_glob_names(x, statements)
}

.cov_gaps <- function() {
  act <- .cov_requirements()
  fns <- lapply(strsplit(act$functions, ";", fixed = TRUE), trimws)
  leaf <- !act$id %in% act$parent
  ns <- asNamespace("rtfreporter")
  exports <- sort(getNamespaceExports(ns))
  gaps <- list()
  for (f in exports) {
    obj <- get(f, envir = ns)
    if (!is.function(obj)) next
    rows <- vapply(fns, function(x) f %in% x, logical(1)) & leaf
    for (a in setdiff(names(formals(obj)), c(".plan", "..."))) {
      if (!any(rows) || !.fr_names_arg(f, a, act$statement[rows])) {
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
  expect_true(.cov_named("rtfreporter.page.margin_left_in", s))
  expect_false(.cov_named("rtfreporter.markup", s))
  # arguments: `a =`, a bare argument in the call form, a glob; prose does not count
  expect_true(.fr_names_arg("rtftable", "cell_padding_left_twips", s))
  expect_true(.fr_names_arg("f", "font", s))
  expect_false(.fr_names_arg("f", "font_size_half_points", s))
  call <- "add_header_row(x, row) adds a row; catx(sep, ...) joins (a, b)."
  expect_true(.fr_names_arg("add_header_row", "x", call))
  expect_true(.fr_names_arg("add_header_row", "row", call))
  expect_true(.fr_names_arg("catx", "sep", call))
  expect_false(.fr_names_arg("catx", "x", call))     # x belongs to the other call
  expect_false(.fr_names_arg("catx", "b", call))     # inside someone else's parentheses
  prose <- "The data are written and the font of the x column is kept."
  expect_false(.fr_names_arg("f", "data", prose))
  expect_false(.fr_names_arg("f", "x", prose))
  expect_false(.fr_names_arg("f", "x", "x == 1 is tested"))
})
