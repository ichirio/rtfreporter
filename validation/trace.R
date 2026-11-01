# ---------------------------------------------------------------------------
#  Traceability: Functional Requirements (FR) -> Functional Tests (FT)
# ---------------------------------------------------------------------------
#  The FRs are inst/validation/requirements.csv, a hierarchy: each row names
#  its `parent` (FR-PL000 the plan verbs > FR-PL004 plan_blanks > FR-PL182 ...;
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
#        test_that("FT-PL<nnn> [FR-PL101 FR-PL102]: table_plan() ...", ...)
#    * an existing test is mapped in inst/validation/test_map.csv
#      (file, test, ft, fr), written by hand.
#  The blocks that call a function in scope but check none of its FRs are
#  listed as candidates (validation/out/candidates.csv): a help to find the
#  tests to read, never counted as coverage.
#
#  Written to validation/out/:
#    traceability.xlsx / .csv   the Traceability Matrix: one row a leaf FR, its
#                               tests and where (no results); the xlsx's
#                               second sheet, the upper FRs' status
#    ft.csv                     one row an FT: its file, test, FRs
#    test-log.txt               testthat's output for these tests (the evidence)
#    functions.csv              one row a function: FRs by kind, with a test,
#                               arguments an FR names, arguments a test gives
#    candidates.csv             blocks calling a function in scope, not mapped
#    quality-report.md          the Quality Report: the results, the
#                               environments, the deviations with why they
#                               are accepted (inst/validation/deviations.csv),
#                               the conclusion and how to read the
#                               attachments
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

# a deviation with no risk stated is not yet assessed either
unassessed <- union(unassessed, intersect(untested, unlist(dev_ids[is.na(dev$risk) | !nzchar(dev$risk)])))

# ---- the environments (gathered by validation/report.R) ---------------------------------
# every CI job's record and the maintainers' local runs; the Quality Plan
# (section 5) names them: 6 in CI, 2 local
expected_env <- c(ci = 6L, local = 2L)
envs <- if (file.exists(file.path(out, "environments.rds"))) readRDS(file.path(out, "environments.rds"))
if (!is.null(envs) && !nrow(envs)) envs <- NULL
n_env <- if (is.null(envs)) c(ci = 0L, local = 0L) else
  c(ci = sum(envs$source == "ci"), local = sum(envs$source != "ci"))
# the tests pass everywhere; R CMD check succeeds on every CI job (a local
# run does not run it)
env_pass <- function(e) {
  grepl("[ FAIL 0 |", e$tests, fixed = TRUE) &
    ifelse(e$source == "ci", e$check %in% "success",
           is.na(e$check) | !nzchar(e$check) | e$check %in% "success")
}
env_failing <- if (is.null(envs)) character() else envs$environment[!env_pass(envs)]
# a record made for another version (a local run not repeated) does not count
this_version <- as.character(utils::packageVersion("rtfreporter"))
env_stale <- if (is.null(envs)) character() else
  envs$environment[!envs$rtfreporter %in% this_version]
env_complete <- all(n_env >= expected_env)
env_ok <- env_complete && !length(env_failing) && !length(env_stale)
env_lines <- if (is.null(envs)) {
  "No environment's record was gathered in this run (the CI run of the release gathers them)."
} else c(
  "| Environment | CI / local | OS | R | rtfreporter | Main dependencies | R CMD check | Tests |",
  "|---|---|---|---|---|---|---|---|",
  sprintf("| %s | %s | %s | %s | %s | %s | %s | %s |", envs$environment, envs$source, envs$os,
          sub("^R version ", "", envs$r), envs$rtfreporter, envs$main_deps,
          ifelse(is.na(envs$check) | !nzchar(envs$check), "--", envs$check),
          ifelse(is.na(envs$tests), "no log", gsub("|", "/", envs$tests, fixed = TRUE))))

# ---- the Quality Report -----------------------------------------------------
# one decimal, floored: 416 of 418 is 99.5%, and 100% only when all
pct <- function(a, b) {
  if (!b) return("0 of 0")
  if (a == b) return(sprintf("%d of %d (100%%)", a, b))
  sprintf("%d of %d (%.1f%%)", a, b, ((1000 * a) %/% b) / 10)
}
# The record is the CI run of a published release; any other run (on a
# maintainer's PC, or CI started by hand) is a draft of it, whatever it finds.
on_ci <- identical(Sys.getenv("GITHUB_ACTIONS"), "true")
draft <- if (on_ci && identical(Sys.getenv("GITHUB_EVENT_NAME"), "release")) {
  NULL
} else if (on_ci) {
  "DRAFT (CI run started by hand; the CI run of the published release is the record)"
} else "DRAFT (local run; the CI run of the published release is the record)"
pass_line <- grep("\\[ FAIL [0-9]+ \\| WARN [0-9]+ \\| SKIP [0-9]+ \\| PASS [0-9]+ \\]", log,
                  value = TRUE)
pass_line <- if (length(pass_line)) trimws(pass_line[length(pass_line)]) else "(no summary line)"
n_dev <- length(untested)
# what keeps the package from being validation-ready
blockers <- c(
  if (length(failing)) sprintf("the tests of %d requirement(s) fail (%s)", length(failing),
                               paste(failing, collapse = ", ")),
  if (length(unassessed)) sprintf("%d requirement(s) with no test are not yet assessed",
                                  length(unassessed)),
  if (nrow(not_accepted)) sprintf("%d high-risk deviation(s) have no reason", nrow(not_accepted)),
  if (!env_complete) sprintf("not every environment was gathered (%d of %d in CI, %d of %d local)",
                             n_env[["ci"]], expected_env[["ci"]], n_env[["local"]],
                             expected_env[["local"]]),
  if (length(env_failing)) sprintf("not every environment passes (%s)",
                                   paste(env_failing, collapse = "; ")),
  if (length(env_stale)) sprintf("the record of %s is of another rtfreporter version than %s",
                                 paste(env_stale, collapse = "; "), this_version))
ready <- !length(blockers)
dev_lines <- if (!nrow(dev) && !length(unassessed)) "None: every leaf requirement has a test." else c(
  "| FR | Content | Risk | Why accepted | Action |", "|---|---|---|---|---|",
  if (nrow(dev)) sprintf("| %s | %s | %s: %s | %s | %s |", gsub(" ", ", ", dev$ids), dev$item,
                         dev$risk, dev$risk_basis,
                         ifelse(is.na(dev$reason), "**not accepted: no reason**", dev$reason),
                         ifelse(is.na(dev$action), "", dev$action)),
  if (length(unassessed)) sprintf("| %s | not yet assessed | -- | **not accepted** | assess |",
                                  paste(unassessed, collapse = ", ")))
n_guard <- sum(tl$kind == "guard")
n_ix <- sum(tl$kind == "interaction")
sm <- c(
  sprintf("# rtfreporter %s: Quality Report", utils::packageVersion("rtfreporter")),
  "",
  if (!is.null(draft)) c(sprintf("**%s**", draft), ""),
  "Against the Quality Plan (inst/validation/quality-plan.md).",
  "",
  sprintf("Version: %s. Date: %s. Commit: %s. Scope: %s.", utils::packageVersion("rtfreporter"),
          format(Sys.Date()),
          tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
                   error = function(e) "unknown"), paste(scope, collapse = ", ")),
  "",
  "## Results",
  "",
  "| Measure | Value |", "|---|---|",
  sprintf("| Leaf requirements with a test, mapped by a person | %s |", pct(sum(tl$n_ft > 0), nrow(tl))),
  sprintf("| Guard requirements (unintended use stopped) with a test | %s |",
          pct(sum(tl$n_ft > 0 & tl$kind == "guard"), n_guard)),
  sprintf("| Interaction requirements (functions or arguments together) with a test | %s |",
          pct(sum(tl$n_ft > 0 & tl$kind == "interaction"), n_ix)),
  sprintf("| Leaf requirements whose tests all pass | %s |", pct(sum(fr_ok), nrow(tl))),
  # (testthat's `|` would split the table's cell: `/` as the environments' table has it)
  sprintf("| The mapped tests, run (test-log.txt) | %s |", gsub("|", "/", pass_line, fixed = TRUE)),
  sprintf("| Arguments a requirement names (functions with their own requirements) | %s |",
          pct(sum(fn_tab$arguments_in_fr), sum(fn_tab$arguments))),
  sprintf("| Upper requirements | %s |", paste(sprintf("%s %s", tr$fr[!leaf], tr$coverage[!leaf]),
                                                collapse = "; ")),
  "",
  "## Environments",
  "",
  sprintf("The same tests, on every environment of the Quality Plan (section 5: %d in CI, %d local), each record of rtfreporter %s; gathered here: %d in CI, %d local.",
          expected_env[["ci"]], expected_env[["local"]], this_version, n_env[["ci"]], n_env[["local"]]),
  "",
  env_lines,
  "",
  "## Deviations",
  "",
  "The leaf requirements with no test, grouped by the reason they are accepted. A high-risk deviation with no reason, or one not yet assessed, is not accepted.",
  "",
  dev_lines,
  if (length(failing)) c("", sprintf("Failing: %s.", paste(failing, collapse = ", "))),
  "",
  "## Conclusion",
  "",
  "What assures the quality, in order:",
  "",
  sprintf("1. **Environments.** %s",
          if (env_ok) {
            sprintf("The same tests pass on every environment of the Quality Plan (%d: %d in CI, where R CMD check succeeds too, and %d local); environments.txt holds each one's sessionInfo and tests' log.",
                    nrow(envs), n_env[["ci"]], n_env[["local"]])
          } else if (is.null(envs)) {
            sprintf("No environment was gathered in this run; the CI run of the release gathers every one (%d in CI, %d local).",
                    expected_env[["ci"]], expected_env[["local"]])
          } else {
            sprintf("**Not every environment is gathered, of this version and passing** (gathered: %d of %d in CI, %d of %d local%s%s): see Environments.",
                    n_env[["ci"]], expected_env[["ci"]], n_env[["local"]], expected_env[["local"]],
                    if (length(env_failing)) paste0("; failing: ", paste(env_failing, collapse = "; ")) else "",
                    if (length(env_stale)) paste0("; of another version: ", paste(env_stale, collapse = "; ")) else "")
          }),
  sprintf("2. **Tests.** %s",
          if (length(failing)) {
            sprintf("**The tests of %d requirement(s) fail**: %s.", length(failing),
                    paste(failing, collapse = ", "))
          } else {
            sprintf("Every leaf requirement mapped to a test has its tests pass: %s. With a test: %s of the guard requirements and %s of the interaction requirements. %s",
                    pct(sum(fr_ok), sum(tl$n_ft > 0)),
                    pct(sum(tl$n_ft > 0 & tl$kind == "guard"), n_guard),
                    pct(sum(tl$n_ft > 0 & tl$kind == "interaction"), n_ix),
                    if (!n_dev) {
                      "Every leaf requirement has a test."
                    } else if (!length(unassessed) && !nrow(not_accepted)) {
                      sprintf("The %d with no test are the deviations above, each accepted for a stated reason.", n_dev)
                    } else {
                      sprintf("Of the %d with no test, not every one is assessed and accepted (Deviations).", n_dev)
                    })
          }),
  "3. **Process.** Every change is made as CONTRIBUTING.md says (its issue-to-merge lifecycle and *Quality assurance*): an issue and a pull request, reviewed, with every CI workflow passing before the merge, so a failing test keeps a change out. GitHub keeps that record (the pull requests, their reviews, the CI runs); it is referred to, not copied.",
  "4. **Other measures.** Line coverage (Attachments, at the end of this report) and the arguments the tests give (functions.csv) are secondary: aids that show where to look, not evidence of a requirement met.",
  "",
  if (is.null(draft) && ready) {
    "Taken together, the package is **validation-ready**: it can be audited against these records. (No third-party audit has been made.)"
  } else if (is.null(draft)) {
    sprintf("**Not validation-ready**: %s.", paste(blockers, collapse = "; "))
  } else {
    sprintf("**%s**: this run concludes nothing. %s Only the CI run of the published release can state the package validation-ready.",
            draft, if (ready) "Every criterion checked here holds." else
              sprintf("Not yet met here: %s.", paste(blockers, collapse = "; ")))
  },
  "",
  "## Attachments, and how to read them",
  "",
  "On the release each file's name carries the tag (`quality-report-<tag>.pdf`, `traceability-<tag>.xlsx`, ...); a CI run started by hand keeps them as its artifact `quality-report`.",
  "",
  "- **traceability.xlsx**, the Traceability Matrix (FR -> FT). Sheet *FR to FT*: one row a leaf requirement -- its FR ID, parent, function, type, the requirement, the FT IDs that check it and where they are (test file:line), its status (`covered` or `none`). Read it by requirement; a blank FT ID is a deviation (above). Sheet *upper FRs*: each parent requirement and whether every leaf under it is covered. It holds no results: those are test-log.txt. (traceability.csv, in the artifact, is the first sheet.)",
  "- **test-log.txt**: testthat's own output for the mapped tests, run in this job, ending in `[ FAIL n | WARN n | SKIP n | PASS n ]`. The evidence that they pass.",
  "- **ft.csv**: one row a functional test: its FT ID, file, name and the requirements it checks. Read it by test.",
  "- **functions.csv**: one row a function in scope: its leaf requirements by type, how many have a test, its arguments, those a requirement names, those a test gives and those no requirement names. Read it for a function's depth (an aid).",
  "- **candidates.csv**: the tests that call a function in scope but are not mapped: what to read next. Never counted.",
  "- **environments.txt**: one part an environment of the table above: its full sessionInfo() (every package's version) and testthat's log. Read it for what, exactly, the tests ran with.",
  "- **Line coverage** (Attachments, at the end of this report): the share of the package's lines the tests run, by file, floored to one decimal. An aid, not evidence of a requirement met.")
writeLines(enc2utf8(sm), file.path(out, "quality-report.md"), useBytes = TRUE)
cat(sprintf("FR %d, FT %d, ready %s%s, written to %s\n", nrow(tl), nrow(map), ready,
            if (is.null(draft)) "" else " (draft)", out))
