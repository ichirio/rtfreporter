# ---------------------------------------------------------------------------
#  Traceability: Functional Requirements (FR) -> Functional Tests (FT)
# ---------------------------------------------------------------------------
#  The FRs are inst/validation/requirements.csv, a hierarchy: each row names
#  its `parent` (FR-PL000 the plan verbs > FR-PL040 plan_blanks > FR-PL041 ...;
#  an id is the area's two letters and a number, never reused).  Tests are
#  mapped to the leaves only; a parent is covered when every leaf under it
#  has a test, partial when some do, none when none do.  A leaf is of a kind:
#    behaviour  a thing the function does
#    argument   an argument, a meaningful value or combination of values
#    data       a data shape it is meant to handle (empty, NA, levels, one or
#               many groups, long or non-ASCII text ...)
#    guard      an unintended use it prevents, and the error or warning it gives
#    interaction  how functions or arguments act together where it is a risk:
#               a precedence or override, an argument that changes another's
#               meaning, a combination that once had a bug, a scenario (data
#               to RTF, compared byte for byte).  These sit apart from the
#               functions' own, under FR-IX000.
#  Only `active` rows count (`draft`: a coarse row not yet split).
#
#  An FT is a test_that() block that checks an FR -- decided by a person who
#  read it, never by the block merely calling the function:
#    * a new test names its id and FRs at its head:
#        test_that("FT-PL123 [FR-PL101 FR-PL102]: table_plan() ...", ...)
#    * an existing test is mapped in inst/validation/test_map.csv
#      (file, test, ft, fr), written by hand.
#  The blocks that call a function in scope but check none of its FRs are
#  listed as candidates (validation/out/candidates.csv): a help to find the
#  tests to read, never counted as coverage.
#
#  Written to validation/out/:
#    traceability.csv / .html   one row an FR: its FTs (the mapping alone)
#    ft.csv                     one row an FT: its file, test, FRs
#    test-results.csv           one row an FT: its result (apart from the matrix)
#    functions.csv              one row a function: FRs by kind, with a test,
#                               arguments an FR names, arguments a test gives
#    candidates.csv             blocks calling a function in scope, not mapped
#    summary.md                 the Development Summary Report (1 page)
#
#  Run from the package root:  Rscript validation/trace.R
# ---------------------------------------------------------------------------

root <- normalizePath(".", winslash = "/")
vdir <- file.path(root, "inst", "validation")
out  <- file.path(root, "validation", "out")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
kinds <- c("behaviour", "argument", "data", "guard", "interaction")

fr <- utils::read.csv(file.path(vdir, "requirements.csv"), colClasses = "character",
                      na.strings = "", stringsAsFactors = FALSE)
fr <- fr[fr$status %in% "active", , drop = FALSE]
fr$parent[is.na(fr$parent)] <- ""
leaf <- !fr$id %in% fr$parent
# a leaf's functions; a parent's are its leaves'
fr_fns <- strsplit(ifelse(is.na(fr$functions), "", fr$functions), ";", fixed = TRUE)
scope <- sort(unique(unlist(fr_fns[leaf])))
# the leaves under each FR (itself, for a leaf)
leaves_of <- function(id) {
  kids <- fr$id[fr$parent == id]
  if (!length(kids)) return(id)
  unique(unlist(lapply(kids, leaves_of)))
}

# ---- the tests: each test_that() block, what it calls and the arguments it gives --
pkgload::load_all(root, quiet = TRUE)
ns <- asNamespace("rtfreporter")
# the arguments given to a function in scope, anywhere in a call tree
given_args <- function(e, acc = list()) {
  if (is.call(e)) {
    f <- e[[1L]]
    nm <- if (is.name(f)) as.character(f) else if (is.call(f) && identical(f[[1L]], as.name("::")))
      as.character(f[[3L]]) else ""
    if (nm %in% scope) {
      a <- names(as.list(e)[-1L])
      acc[[nm]] <- union(acc[[nm]], a[!is.na(a) & nzchar(a)])
    }
    for (x in as.list(e)[-1L]) if (!missing(x)) acc <- given_args(x, acc)
  }
  acc
}
blocks <- function(file) {
  ex <- tryCatch(parse(file, keep.source = TRUE), error = function(e) NULL)
  if (is.null(ex)) return(NULL)
  rows <- lapply(seq_along(ex), function(i) {
    e <- ex[[i]]
    if (!is.call(e) || !identical(e[[1L]], as.name("test_that"))) return(NULL)
    desc <- tryCatch(as.character(eval(e[[2L]], baseenv())), error = function(err) NA_character_)
    ga <- given_args(e[[3L]])
    data.frame(file = basename(file), test = desc,
               calls = paste(unique(all.names(e[[3L]])), collapse = " "),
               given = paste(unlist(lapply(names(ga), function(n) paste0(n, ":", ga[[n]]))),
                             collapse = " "),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
files <- list.files(file.path(root, "tests", "testthat"), "^test-.*[.]R$", full.names = TRUE)
tb <- do.call(rbind, lapply(files, blocks))

# ---- the map: by hand, and the new tests' own names -------------------------------
map <- utils::read.csv(file.path(vdir, "test_map.csv"), colClasses = "character",
                       stringsAsFactors = FALSE)
head_re <- "^(FT-[A-Z]{2}[0-9]{3}) \\[([^]]*)\\]:"
named <- tb[grepl(head_re, tb$test), , drop = FALSE]
if (nrow(named)) {
  map <- rbind(map, data.frame(file = named$file, test = named$test,
                               ft = sub(paste0(head_re, ".*$"), "\\1", named$test),
                               fr = sub(paste0(head_re, ".*$"), "\\2", named$test),
                               stringsAsFactors = FALSE))
}
gone <- !paste(map$file, map$test, sep = "\r") %in% paste(tb$file, tb$test, sep = "\r")
if (any(gone)) {
  stop(sprintf("test_map.csv names %d test(s) not in the tests (renamed?): %s.", sum(gone),
               paste(utils::head(paste(map$file[gone], map$test[gone], sep = ": "), 3),
                     collapse = "; ")), call. = FALSE)
}
if (anyDuplicated(map$ft)) stop("An FT id is used twice: ", paste(unique(map$ft[duplicated(map$ft)]),
                                                                 collapse = ", "), call. = FALSE)
unknown <- setdiff(unlist(strsplit(map$fr, " ", fixed = TRUE)), fr$id)
if (length(unknown)) stop("The map names FRs not in requirements.csv: ",
                          paste(unknown, collapse = ", "), call. = FALSE)
not_leaf <- intersect(unlist(strsplit(map$fr, " ", fixed = TRUE)), fr$id[!leaf])
if (length(not_leaf)) stop("Tests are mapped to the leaves only, not to ",
                           paste(not_leaf, collapse = ", "), call. = FALSE)

# ---- the matrix: FR -> FT, the mapping alone ----------------------------------------
fts <- function(id) map$ft[vapply(strsplit(map$fr, " ", fixed = TRUE), function(x) id %in% x, NA)]
tr <- data.frame(fr = fr$id, parent = fr$parent, kind = fr$kind, statement = fr$statement,
                 functions = fr$functions,
                 ft = vapply(fr$id, function(id) paste(fts(id), collapse = " "), ""),
                 n_ft = vapply(fr$id, function(id) length(fts(id)), 0L),
                 stringsAsFactors = FALSE, row.names = NULL)
# coverage: a leaf with a test is covered; a parent by its leaves
tr$coverage <- vapply(fr$id, function(id) {
  lv <- leaves_of(id)
  n <- sum(tr$n_ft[match(lv, tr$fr)] > 0L)
  if (n == length(lv)) "covered" else if (n > 0L) "partial" else "none"
}, "")
utils::write.csv(tr, file.path(out, "traceability.csv"), row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(map[c("ft", "file", "test", "fr")], file.path(out, "ft.csv"), row.names = FALSE,
                 fileEncoding = "UTF-8")
esc <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE)
# the matrix as a tree: each parent a <details> that folds its children
node <- function(id) {
  r <- tr[tr$fr == id, ]
  kids <- tr$fr[tr$parent == id]
  label <- sprintf("<b>%s</b> %s <span class='%s'>[%s]</span>%s", r$fr, esc(r$statement),
                   r$coverage, r$coverage,
                   if (length(kids)) "" else sprintf(" <i>%s</i> &rarr; %s", r$kind,
                                                       if (nzchar(r$ft)) esc(r$ft) else "<b>no test</b>"))
  if (!length(kids)) return(sprintf("<li>%s</li>", label))
  c(sprintf("<li><details open><summary>%s</summary><ul>", label),
    unlist(lapply(kids, node)), "</ul></details></li>")
}
roots <- tr$fr[!nzchar(tr$parent) | !tr$parent %in% tr$fr]
writeLines(c(
  "<!doctype html><meta charset='utf-8'><title>Traceability matrix</title>",
  "<style>body{font-family:sans-serif;font-size:13px}ul{list-style:none;padding-left:1.2em}.covered{color:#070}.partial{color:#a60}.none{color:#b00}</style>",
  sprintf("<h1>rtfreporter %s: FR &rarr; FT</h1>", utils::packageVersion("rtfreporter")),
  "<p>Each parent folds; a parent is covered when every leaf under it has a test.</p>",
  "<ul>", unlist(lapply(roots, node)), "</ul>"), file.path(out, "traceability.html"),
  useBytes = TRUE)

# ---- candidates: blocks calling a function in scope, not mapped (a help) --------------
cand <- tb[!paste(tb$file, tb$test) %in% paste(map$file, map$test), , drop = FALSE]
cand$functions <- vapply(strsplit(cand$calls, " ", fixed = TRUE), function(cl) {
  paste(intersect(scope, cl), collapse = " ")
}, "")
cand <- cand[nzchar(cand$functions), c("file", "test", "functions"), drop = FALSE]
utils::write.csv(cand, file.path(out, "candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---- per function: FRs by kind, with a test; arguments an FR names, a test gives ---------
fn_rows <- lapply(scope, function(f) {
  mine <- leaf & vapply(fr_fns, function(x) f %in% x, NA)
  formals_ <- setdiff(names(formals(get(f, envir = ns))), c(".plan", "..."))
  named_in_fr <- formals_[vapply(formals_, function(a) {
    any(grepl(sprintf("\\b%s\\s*=", a), fr$statement[mine], perl = TRUE))
  }, NA)]
  given <- unique(unlist(lapply(strsplit(tb$given, " ", fixed = TRUE), function(g) {
    sub("^[^:]*:", "", g[startsWith(g, paste0(f, ":"))])
  })))
  given_in_test <- intersect(formals_, given)
  # (a function that only an interaction names: its arguments are not counted)
  own <- any(mine & fr$kind != "interaction")
  if (!own) named_in_fr <- given_in_test <- formals_ <- character()
  cnt <- vapply(kinds, function(k) sum(mine & fr$kind == k), 0L)
  data.frame(`function` = f, behaviour = cnt[["behaviour"]], argument = cnt[["argument"]],
             data = cnt[["data"]], guard = cnt[["guard"]], interaction = cnt[["interaction"]],
             frs = sum(mine),
             frs_tested = sum(mine & tr$n_ft > 0L), arguments = length(formals_),
             arguments_in_fr = length(named_in_fr), arguments_in_tests = length(given_in_test),
             arguments_in_no_fr = paste(setdiff(formals_, named_in_fr), collapse = " "),
             check.names = FALSE, stringsAsFactors = FALSE)
})
fn_tab <- do.call(rbind, fn_rows)
utils::write.csv(fn_tab, file.path(out, "functions.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---- the tests' results: apart from the matrix -----------------------------------------
run_files <- unique(map$file)
res <- do.call(rbind, lapply(run_files, function(f) {
  r <- testthat::test_file(file.path(root, "tests", "testthat", f),
                           reporter = testthat::ListReporter$new(), package = "rtfreporter")
  d <- as.data.frame(r)
  data.frame(file = f, test = d$test,
             result = ifelse(d$error | d$failed > 0, "fail", ifelse(d$skipped, "skip", "pass")),
             stringsAsFactors = FALSE)
}))
res <- res[!duplicated(paste(res$file, res$test)), , drop = FALSE]
result <- res$result[match(paste(map$file, map$test), paste(res$file, res$test))]
result[is.na(result)] <- "not run"
utils::write.csv(data.frame(ft = map$ft, file = map$file, test = map$test, result = result),
                 file.path(out, "test-results.csv"), row.names = FALSE, fileEncoding = "UTF-8")
fr_ok <- vapply(fr$id[leaf], function(id) {
  r <- result[map$ft %in% fts(id)]
  length(r) > 0L && !any(r %in% c("fail", "not run"))
}, NA)

# ---- the Development Summary Report: one page ---------------------------------------------
tl <- tr[leaf, , drop = FALSE]   # (the counts are of the leaves)
pct <- function(a, b) if (b) sprintf("%d of %d (%.0f%%)", a, b, 100 * a / b) else "0 of 0"
sm <- c(
  sprintf("# rtfreporter %s: Development Summary Report (scope: %s)",
          utils::packageVersion("rtfreporter"), paste(scope, collapse = ", ")),
  "",
  sprintf("Date: %s. Commit: %s.", format(Sys.Date()),
          tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
                   error = function(e) "unknown")),
  sprintf("Environment: %s; %s.", R.version.string, utils::sessionInfo()$running),
  "",
  "## Scope",
  sprintf("%d functional requirements (leaves) of %d functions (inst/validation/requirements.csv): %s.",
          nrow(tl), length(scope), paste(sprintf("%d %s", vapply(kinds, function(k) sum(tl$kind == k), 0L),
                                                 kinds), collapse = ", ")),
  "",
  "## Results",
  "",
  "| Measure | Value |", "|---|---|",
  sprintf("| Requirements with a test (a person mapped it) | %s |", pct(sum(tl$n_ft > 0), nrow(tl))),
  sprintf("| Guard requirements (unintended use stopped) with a test | %s |",
          pct(sum(tl$n_ft > 0 & tl$kind == "guard"), sum(tl$kind == "guard"))),
  sprintf("| Interaction requirements (across functions) with a test | %s |",
          pct(sum(tl$n_ft > 0 & tl$kind == "interaction"), sum(tl$kind == "interaction"))),
  sprintf("| Functional tests | %d: %d pass, %d fail, %d skip |", nrow(map),
          sum(result == "pass"), sum(result == "fail"), sum(result == "skip")),
  sprintf("| Requirements whose tests all pass | %s |", pct(sum(fr_ok), nrow(tl))),
  sprintf("| Arguments a requirement names (functions with their own requirements) | %s |", pct(sum(fn_tab$arguments_in_fr), sum(fn_tab$arguments))),
  sprintf("| Arguments a test gives (an aid, not coverage) | %s |",
          pct(sum(fn_tab$arguments_in_tests), sum(fn_tab$arguments))),
  "",
  sprintf("Upper requirements: %s.", paste(sprintf("%s %s", tr$fr[!leaf], tr$coverage[!leaf]),
                                            collapse = "; ")),
  "",
  "",
  "By function (behaviour / argument / data / guard / interaction; tested):",
  "",
  "| Function | B | A | D | G | I | Tested |", "|---|---|---|---|---|---|---|",
  sprintf("| %s | %d | %d | %d | %d | %d | %d of %d |", fn_tab[["function"]], fn_tab$behaviour,
          fn_tab$argument, fn_tab$data, fn_tab$guard, fn_tab$interaction, fn_tab$frs_tested,
          fn_tab$frs),
  "",
  "A failing test fails the pull request's checks (CI), so it cannot be merged.",
  "",
  "## Deviations",
  if (any(tl$n_ft == 0)) sprintf("- %s (%s): no test", tl$fr[tl$n_ft == 0], tl$kind[tl$n_ft == 0]) else "None.",
  if (any(tl$n_ft > 0 & !fr_ok)) sprintf("- %s: a test fails", tl$fr[tl$n_ft > 0 & !fr_ok]),
  "",
  "## Conclusion",
  if (all(fr_ok)) {
    "Every requirement in scope has tests, and they pass."
  } else "Not every requirement in scope is met by passing tests: see the deviations.",
  "",
  "Attachments: traceability.csv / .html (FR -> FT), ft.csv, test-results.csv, functions.csv, candidates.csv (blocks to read, not counted).")
writeLines(sm, file.path(out, "summary.md"), useBytes = TRUE)
cat(sprintf("FR %d, FT %d, written to %s\n", nrow(tl), nrow(map), out))
