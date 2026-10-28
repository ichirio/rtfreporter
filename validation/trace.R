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
#    traceability.xlsx / .csv   one row a leaf FR: its tests and where (no results)
#    ft.csv                     one row an FT: its file, test, FRs
#    test-log.txt               testthat's output for these tests (the evidence)
#    functions.csv              one row a function: FRs by kind, with a test,
#                               arguments an FR names, arguments a test gives
#    candidates.csv             blocks calling a function in scope, not mapped
#    summary.md                 the Development Summary Report (1 page): the
#                               results, the deviations with why they are
#                               accepted (inst/validation/deviations.csv), the
#                               conclusion and how to read the attachments
#  Needs pkgload, testthat and writexl (Suggests).
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
  sr <- utils::getSrcref(ex)
  rows <- lapply(seq_along(ex), function(i) {
    e <- ex[[i]]
    if (!is.call(e) || !identical(e[[1L]], as.name("test_that"))) return(NULL)
    desc <- tryCatch(as.character(eval(e[[2L]], baseenv())), error = function(err) NA_character_)
    ga <- given_args(e[[3L]])
    data.frame(file = basename(file), test = desc, line = sr[[i]][1L],
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

# ---- the matrix: one row a leaf FR, its tests (no results) --------------------------
fts <- function(id) map$ft[vapply(strsplit(map$fr, " ", fixed = TRUE), function(x) id %in% x, NA)]
where_of <- function(f) {
  i <- match(paste(map$file[map$ft %in% f], map$test[map$ft %in% f]), paste(tb$file, tb$test))
  paste(sprintf("%s:%s", tb$file[i], tb$line[i]), collapse = " ")
}
tr <- data.frame(fr = fr$id, parent = fr$parent, kind = fr$kind, statement = fr$statement,
                 functions = fr$functions,
                 ft = vapply(fr$id, function(id) paste(fts(id), collapse = " "), ""),
                 n_ft = vapply(fr$id, function(id) length(fts(id)), 0L),
                 stringsAsFactors = FALSE, row.names = NULL)
tr$coverage <- vapply(fr$id, function(id) {
  lv <- leaves_of(id)
  n <- sum(tr$n_ft[match(lv, tr$fr)] > 0L)
  if (n == length(lv)) "covered" else if (n > 0L) "partial" else "none"
}, "")
tm <- data.frame(`FR ID` = tr$fr, parent = tr$parent, `function` = tr$functions, type = tr$kind,
                 requirement = tr$statement, `FT ID(s)` = tr$ft,
                 `test file:line` = vapply(tr$fr, function(id) where_of(fts(id)), ""),
                 status = tr$coverage, check.names = FALSE, stringsAsFactors = FALSE)
tm <- tm[leaf, , drop = FALSE]
utils::write.csv(tm, file.path(out, "traceability.csv"), row.names = FALSE, fileEncoding = "UTF-8")
writexl::write_xlsx(list(`FR to FT` = tm,
                         `upper FRs` = data.frame(`FR ID` = tr$fr[!leaf], parent = tr$parent[!leaf],
                                                  requirement = tr$statement[!leaf],
                                                  status = tr$coverage[!leaf], check.names = FALSE)),
                    file.path(out, "traceability.xlsx"))
utils::write.csv(map[c("ft", "file", "test", "fr")], file.path(out, "ft.csv"), row.names = FALSE,
                 fileEncoding = "UTF-8")

# ---- candidates: blocks calling a function in scope, not mapped (a help) --------------
cand <- tb[!paste(tb$file, tb$test) %in% paste(map$file, map$test), , drop = FALSE]
cand$functions <- vapply(strsplit(cand$calls, " ", fixed = TRUE), function(cl) {
  paste(intersect(scope, cl), collapse = " ")
}, "")
cand <- cand[nzchar(cand$functions), c("file", "line", "test", "functions"), drop = FALSE]
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
             frs = sum(mine), frs_tested = sum(mine & tr$n_ft > 0L), arguments = length(formals_),
             arguments_in_fr = length(named_in_fr), arguments_in_tests = length(given_in_test),
             arguments_in_no_fr = paste(setdiff(formals_, named_in_fr), collapse = " "),
             check.names = FALSE, stringsAsFactors = FALSE)
})
fn_tab <- do.call(rbind, fn_rows)
utils::write.csv(fn_tab, file.path(out, "functions.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---- the tests, run: their log is the evidence; FT results stay inside ----------------
run_files <- unique(map$file)
lst <- testthat::ListReporter$new()
log <- utils::capture.output(
  testthat::test_dir(file.path(root, "tests", "testthat"),
                     filter = paste0("^(", paste(gsub("^test-|[.]R$", "", run_files), collapse = "|"), ")$"),
                     reporter = testthat::MultiReporter$new(list(
                       lst, testthat::ProgressReporter$new(show_praise = FALSE))),
                     load_package = "none", stop_on_failure = FALSE),
  type = "output")
writeLines(log, file.path(out, "test-log.txt"), useBytes = TRUE)
d <- as.data.frame(lst$get_results())
res <- data.frame(file = d$file, test = d$test,
                  result = ifelse(d$error | d$failed > 0, "fail", ifelse(d$skipped, "skip", "pass")),
                  stringsAsFactors = FALSE)
res <- res[!duplicated(paste(res$file, res$test)), , drop = FALSE]
result <- res$result[match(paste(map$file, map$test), paste(res$file, res$test))]
result[is.na(result)] <- "not run"
fr_ok <- vapply(fr$id[leaf], function(id) {
  r <- result[map$ft %in% fts(id)]
  length(r) > 0L && !any(r %in% c("fail", "not run"))
}, NA)

# ---- the deviations: the leaves with no test, and why they are accepted -----------------
# inst/validation/deviations.csv, written by a person: one row a group of FRs
# accepted for one reason (ids space-separated), its risk (high / medium /
# low) and the basis of that, the reason it is accepted, the action
dev <- utils::read.csv(file.path(vdir, "deviations.csv"), colClasses = "character",
                       na.strings = "", stringsAsFactors = FALSE)
tl <- tr[leaf, , drop = FALSE]
untested <- tl$fr[tl$n_ft == 0L]
dev_ids <- strsplit(dev$ids, " ", fixed = TRUE)
dev <- dev[vapply(dev_ids, function(x) any(x %in% untested), NA), , drop = FALSE]
dev_ids <- strsplit(dev$ids, " ", fixed = TRUE)
unassessed <- setdiff(untested, unlist(dev_ids))
not_accepted <- dev[dev$risk == "high" & (is.na(dev$reason) | !nzchar(dev$reason)), , drop = FALSE]
failing <- tl$fr[tl$n_ft > 0L & !fr_ok]

# ---- the environments (from the CI validation report, when there) --------------------
envs <- if (file.exists(file.path(out, "environments.rds"))) readRDS(file.path(out, "environments.rds"))
local_runs <- if (file.exists(file.path(out, "local-runs.csv"))) {
  utils::read.csv(file.path(out, "local-runs.csv"), colClasses = "character")
}
env_ok <- !is.null(envs) && nrow(envs) && all(envs$result == "success")

# ---- the Development Summary Report: one page -----------------------------------------
pct <- function(a, b) if (b) sprintf("%d of %d (%.0f%%)", a, b, 100 * a / b) else "0 of 0"
pass_line <- grep("\\[ FAIL [0-9]+ \\| WARN [0-9]+ \\| SKIP [0-9]+ \\| PASS [0-9]+ \\]", log,
                  value = TRUE)
pass_line <- if (length(pass_line)) trimws(pass_line[length(pass_line)]) else "(no summary line)"
ready <- !length(failing) && !length(unassessed) && !nrow(not_accepted)
dev_lines <- if (!nrow(dev) && !length(unassessed)) "None." else c(
  "| FR | Content | Risk | Why accepted | Action |", "|---|---|---|---|---|",
  sprintf("| %s | %s | %s: %s | %s | %s |", gsub(" ", ", ", dev$ids), dev$item, dev$risk,
          dev$risk_basis, ifelse(is.na(dev$reason), "**not accepted: no reason**", dev$reason),
          ifelse(is.na(dev$action), "", dev$action)),
  if (length(unassessed)) sprintf("| %s | not yet assessed | -- | **not accepted** | assess |",
                                  paste(unassessed, collapse = ", ")))
sm <- c(
  sprintf("# rtfreporter %s: Development Summary Report", utils::packageVersion("rtfreporter")),
  "",
  sprintf("Date: %s. Commit: %s. Scope: %s.", format(Sys.Date()),
          tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
                   error = function(e) "unknown"), paste(scope, collapse = ", ")),
  "",
  "## Results",
  "",
  "| Measure | Value |", "|---|---|",
  sprintf("| Requirements (leaves) with a test, mapped by a person | %s |", pct(sum(tl$n_ft > 0), nrow(tl))),
  sprintf("| Guard requirements (unintended use stopped) with a test | %s |",
          pct(sum(tl$n_ft > 0 & tl$kind == "guard"), sum(tl$kind == "guard"))),
  sprintf("| Interaction requirements (across functions) with a test | %s |",
          pct(sum(tl$n_ft > 0 & tl$kind == "interaction"), sum(tl$kind == "interaction"))),
  sprintf("| Requirements whose tests all pass | %s |", pct(sum(fr_ok), nrow(tl))),
  sprintf("| The tests run (test-log.txt) | %s |", pass_line),
  sprintf("| Arguments a requirement names (functions with their own requirements) | %s |",
          pct(sum(fn_tab$arguments_in_fr), sum(fn_tab$arguments))),
  sprintf("| Upper requirements | %s |", paste(sprintf("%s %s", tr$fr[!leaf], tr$coverage[!leaf]),
                                                collapse = "; ")),
  "",
  "## Deviations",
  "",
  "The requirements with no test, grouped by the reason they are accepted. A high risk with no reason is not accepted.",
  "",
  dev_lines,
  if (length(failing)) c("", sprintf("Failing: %s.", paste(failing, collapse = ", "))),
  "",
  "## Conclusion",
  "",
  "What assures the quality, in order:",
  "",
  sprintf("1. **Environments.** %s",
          if (env_ok) sprintf("The same tests pass on every CI environment of this commit (%d: %s)%s.",
                              nrow(envs), paste(envs$environment, collapse = ", "),
                              if (!is.null(local_runs)) sprintf(" and on %d local runs", nrow(local_runs)) else "")
          else "The CI validation report (attached) shows the tests on every environment of the release's commit."),
  sprintf("2. **Tests.** Every requirement mapped to a test has its tests pass (%s); the requirements with no test are the deviations above, each accepted for a stated reason.",
          pct(sum(fr_ok), sum(tl$n_ft > 0))),
  "3. **Process.** Every change is an issue and a pull request, reviewed, with every CI workflow required: a failing test fails the checks and the change cannot be merged (CONTRIBUTING.md, Quality assurance).",
  "4. **Other measures.** Line coverage and the arguments the tests give are aids, not evidence of a requirement met.",
  "",
  if (ready) {
    "Taken together, the package is **validation-ready**: it can be audited against these records. (No third-party audit has been made.)"
  } else {
    "**Not validation-ready**: a test fails, or a deviation is not yet assessed or not accepted (above)."
  },
  "",
  "## Attachments, and how to read them",
  "",
  "- **traceability.xlsx / .csv**: one row a requirement (a leaf): its parent, function, type, the tests that check it and where they are (file:line), and its status. Read it by requirement; a blank FT is a deviation.",
  "- **ft.csv**: one row a test: its file, name and the requirements it checks. Read it by test.",
  "- **test-log.txt**: testthat's own output for these tests, ending in `[ FAIL | WARN | SKIP | PASS ]`. The evidence that they pass.",
  "- **functions.csv**: per function, its requirements by type, how many have a test, and its arguments a requirement names. Read it to see a function's depth.",
  "- **candidates.csv**: tests that call a function in scope but are not mapped: what to read next. Not counted.",
  "- **The CI validation report (PDF)**: the environments (OS, R, the result and tests on each), sessionInfo and the package versions, R CMD check, line coverage and the tests that run each exported function. Read it for where and with what the tests ran.")
writeLines(sm, file.path(out, "summary.md"), useBytes = TRUE)
cat(sprintf("FR %d, FT %d, ready %s, written to %s\n", nrow(tl), nrow(map), ready, out))
