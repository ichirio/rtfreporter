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
#  An FT is a test_that() block that checks an FR -- decided by the developer
#  making the change, never by the block merely calling the function:
#    * a new test names its id and FRs at its head:
#        test_that("FT-PL<nnn> [FR-PL101 FR-PL102]: table_plan() ...", ...)
#    * an existing test is mapped in inst/validation/traceability_matrix.csv
#      (file, test, ft, fr), written by hand.
#  Each mapping is checked when it is made: inst/validation/traceability_review.csv
#  (ft, fr, verdict, evidence, fr_covered) records that the test's expectations
#  assert what the FR states, with the expectation lines as the evidence; the
#  matrix shows it in its `checked` and `evidence` columns.
#  The blocks that call a function in scope but check none of its FRs are
#  listed as candidates (validation/out/candidates.csv): a help to find the
#  tests to read, never counted as coverage.
#
#  Written to validation/out/:
#    traceability_matrix.xlsx / traceability_matrix.csv the Traceability Matrix: one row a leaf FR, its
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

# ---- the environments of the Quality Plan (section 5), by tier --------------------------
# The one place that says which environment is which.  A record's name (the
# `environment` of its env.csv: the CI job's name, or the name given to
# validation/env.R by hand) is matched against `name`, a regular expression.
#   required    the verdict (criterion 3) rests on these: each must have a
#               record, its tests passing, of this version and of this code
#   additional  for reference: run and reported in a table of their own, a
#               failure named, never judged
# `role` is the row of the plan's environment strategy it stands for.  A
# record that matches no row is shown with the additional ones, as "not
# named in the Quality Plan".
env_plan <- data.frame(
  tier   = c("required", "required", "required", "required", "required",
             "additional", "additional", "additional"),
  source = c("ci", "ci", "ci", "ci", "local", "ci", "ci", "local"),
  role   = c("minimum", "recommended", "recommended", "recommended", "recommended",
             "oldrel", "devel", "older"),
  name   = c("^ubuntu-[0-9.]+ [(]R [0-9.]+, minimum environment[)]$", # the minimum versions
             "^ubuntu-latest [(]R release[)]$",                       # the recommended versions,
             "^windows-latest [(]R release[)]$",                      #   on the three systems
             "^macos-latest [(]R release[)]$",
             "^Windows 11, R 4[.]6[.]1$",                             #   and on the maintainers' PC
             "^ubuntu-latest [(]R oldrel-1[)]$",                      # the previous release of R
             "^ubuntu-latest [(]R devel[)]$",                         # the next R
             "^Windows 11, R 4[.]4[.]2$"),                            # an older R on the PC
  label  = c("ubuntu (R minimum, minimum environment)", "ubuntu-latest (R release)",
             "windows-latest (R release)", "macos-latest (R release)", "Windows 11, R 4.6.1",
             "ubuntu-latest (R oldrel-1)", "ubuntu-latest (R devel)", "Windows 11, R 4.4.2"),
  stringsAsFactors = FALSE)

fr <- utils::read.csv(file.path(vdir, "requirements.csv"), colClasses = "character",
                      na.strings = "", stringsAsFactors = FALSE)
all_ids <- fr$id
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
map <- utils::read.csv(file.path(vdir, "traceability_matrix.csv"), colClasses = "character",
                       stringsAsFactors = FALSE)
head_re <- "^(FT-[A-Z]{2}[0-9]{3,4}) \\[([^]]*)\\]:"
named <- tb[grepl(head_re, tb$test), , drop = FALSE]
if (nrow(named)) {
  map <- rbind(map, data.frame(file = named$file, test = named$test,
                               ft = sub(paste0(head_re, ".*$"), "\\1", named$test),
                               fr = sub(paste0(head_re, ".*$"), "\\2", named$test),
                               stringsAsFactors = FALSE))
}
gone <- !paste(map$file, map$test, sep = "\r") %in% paste(tb$file, tb$test, sep = "\r")
if (any(gone)) {
  stop(sprintf("traceability_matrix.csv names %d test(s) not in the tests (renamed?): %s.", sum(gone),
               paste(utils::head(paste(map$file[gone], map$test[gone], sep = ": "), 3),
                     collapse = "; ")), call. = FALSE)
}
if (anyDuplicated(map$ft)) stop("An FT id is used twice: ", paste(unique(map$ft[duplicated(map$ft)]),
                                                                 collapse = ", "), call. = FALSE)
unknown <- setdiff(unlist(strsplit(map$fr, " ", fixed = TRUE)), all_ids)
if (length(unknown)) stop("The map names FRs not in requirements.csv: ",
                          paste(unknown, collapse = ", "), call. = FALSE)
# a test of a removed (deprecated) feature stays until the code goes; it
# checks no requirement of the matrix
map$fr <- vapply(strsplit(map$fr, " ", fixed = TRUE),
                 function(x) paste(intersect(x, fr$id), collapse = " "), "")
map <- map[nzchar(map$fr), , drop = FALSE]
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
# the check of each mapping (inst/validation/traceability_review.csv): its
# test's expectations assert what the FR states -- the whole of it ("asserts")
# or a part, the FR's tests together asserting all of it ("part")
rv <- utils::read.csv(file.path(vdir, "traceability_review.csv"), colClasses = "character",
                      na.strings = "", stringsAsFactors = FALSE)
checked_of <- function(id) {
  r <- rv[rv$fr == id & rv$ft %in% fts(id), , drop = FALSE]
  if (!nrow(r) || length(setdiff(fts(id), r$ft))) return("")
  if (any(r$verdict == "asserts")) "by a test" else "by its tests together"
}
evidence_of <- function(id) {
  r <- rv[rv$fr == id & rv$ft %in% fts(id), , drop = FALSE]
  paste(sprintf("%s: %s", r$ft, r$evidence), collapse = "\n")
}
tm <- data.frame(`FR ID` = tr$fr, parent = tr$parent, `function` = tr$functions, type = tr$kind,
                 requirement = tr$statement, `FT ID(s)` = tr$ft,
                 `test file:line` = vapply(tr$fr, function(id) where_of(fts(id)), ""),
                 status = tr$coverage,
                 checked = vapply(tr$fr, checked_of, ""),
                 evidence = vapply(tr$fr, evidence_of, ""),
                 check.names = FALSE, stringsAsFactors = FALSE)
tm <- tm[leaf, , drop = FALSE]
utils::write.csv(tm, file.path(out, "traceability_matrix.csv"), row.names = FALSE, fileEncoding = "UTF-8")
writexl::write_xlsx(list(`FR to FT` = tm,
                         `upper FRs` = data.frame(`FR ID` = tr$fr[!leaf], parent = tr$parent[!leaf],
                                                  requirement = tr$statement[!leaf],
                                                  status = tr$coverage[!leaf], check.names = FALSE)),
                    file.path(out, "traceability_matrix.xlsx"))
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
    ns$.fr_names_arg(f, a, fr$statement[mine])
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
# each skip with its reason: the skip condition's message, as testthat's
# "Skipped tests" section of the log groups them
skip_rows <- lapply(lst$get_results(), function(x) {
  sk <- Filter(function(e) inherits(e, "expectation_skip"), x$results)
  if (!length(sk)) return(NULL)
  data.frame(file = x$file, test = x$test,
             reason = vapply(sk, function(e) trimws(sub("^Reason:\\s*", "", strsplit(conditionMessage(e), "\n")[[1L]][1L])), ""),
             stringsAsFactors = FALSE)
})
skips <- do.call(rbind, skip_rows)
if (is.null(skips)) skips <- data.frame(file = character(), test = character(), reason = character())
skips$where <- tb$line[match(paste(skips$file, skips$test), paste(tb$file, tb$test))]
skips$where <- sprintf("%s:%s", skips$file, skips$where)
result <- res$result[match(paste(map$file, map$test), paste(res$file, res$test))]
result[is.na(result)] <- "not run"
fr_ok <- vapply(fr$id[leaf], function(id) {
  r <- result[map$ft %in% fts(id)]
  length(r) > 0L && !any(r %in% c("fail", "not run"))
}, NA)

# ---- the deviations: the leaves with no test, and why they are accepted -----------------
# inst/validation/deviations.csv, written by the developer: one row a group of FRs
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
# every CI job's record and the maintainers' local runs, each given its tier
# by env_plan (at the top): the required ones the Quality Plan's section 5
# names, 4 in CI and 1 local, and the additional ones, for reference
envs <- if (file.exists(file.path(out, "environments.rds"))) readRDS(file.path(out, "environments.rds"))
if (!is.null(envs) && !nrow(envs)) envs <- NULL
if (!is.null(envs)) {
  envs$plan_row <- vapply(envs$environment, function(n) {
    k <- which(vapply(env_plan$name, grepl, NA, x = n, USE.NAMES = FALSE))
    if (length(k)) k[1L] else NA_integer_
  }, 1L, USE.NAMES = FALSE)
  envs$tier <- ifelse(is.na(envs$plan_row), "additional", env_plan$tier[envs$plan_row])
  envs$role <- ifelse(is.na(envs$plan_row), "", env_plan$role[envs$plan_row])
}
env_req <- env_plan$tier == "required"
expected_env <- c(ci = sum(env_req & env_plan$source == "ci"),
                  local = sum(env_req & env_plan$source == "local"))
# the required environments with a record, and those without
env_have <- if (is.null(envs)) rep(FALSE, nrow(env_plan)) else seq_len(nrow(env_plan)) %in% envs$plan_row
n_env <- c(ci = sum(env_req & env_have & env_plan$source == "ci"),
           local = sum(env_req & env_have & env_plan$source == "local"))
env_missing <- env_plan$label[env_req & !env_have]
env_complete <- !length(env_missing)
# the tests pass (testthat's summary line); R CMD check is shown, and is a
# condition of the release procedure, not of the verdict (a local run does
# not run it)
env_tests_ok <- function(e) grepl("[ FAIL 0 |", e$tests, fixed = TRUE)
env_check_ok <- function(e) {
  ifelse(e$source == "ci", e$check %in% "success",
         is.na(e$check) | !nzchar(e$check) | e$check %in% "success")
}
is_req <- if (is.null(envs)) logical() else envs$tier == "required"
env_failing <- if (is.null(envs)) character() else envs$environment[is_req & !env_tests_ok(envs)]
env_add_failing <- if (is.null(envs)) character() else {
  k <- !is_req & !env_tests_ok(envs)
  sprintf("%s (%s)", envs$environment[k],
          ifelse(is.na(envs$tests[k]), "no tests' summary line",
                 gsub("|", "/", envs$tests[k], fixed = TRUE)))
}
# a record made for another version (a local run not repeated) does not count
this_version <- as.character(utils::packageVersion("rtfreporter"))
env_stale_all <- if (is.null(envs)) character() else
  envs$environment[!envs$rtfreporter %in% this_version]
env_stale <- if (is.null(envs)) character() else envs$environment[is_req & !envs$rtfreporter %in% this_version]
# the commit and date each record was made at: a local record is made by hand
# before the release and may be of an earlier commit of the same version
this_commit <- tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
                        error = function(e) "unknown")
short_sha <- function(x) ifelse(is.na(x) | !nzchar(x), "--", substr(x, 1L, 7L))
# A record is of this code when it was made at this commit, or when nothing
# the tests run changed between its commit and this one: only validation/
# (the records themselves, committed by their own PR, and the report's
# scripts) or .github/ (CI).  Unknown when git cannot tell (no history): then
# it counts as another commit.
same_code <- function(sha) {
  if (is.na(sha) || !nzchar(sha)) return(FALSE)
  if (identical(substr(sha, 1L, 7L), substr(this_commit, 1L, 7L))) return(TRUE)
  changed <- suppressWarnings(tryCatch(
    system2("git", c("diff", "--name-only", sha, "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) NULL))
  if (is.null(changed) || !is.null(attr(changed, "status"))) return(FALSE)
  all(startsWith(changed, "validation/") | startsWith(changed, ".github/"))
}
env_code_ok <- if (is.null(envs)) logical() else if (is.null(envs$commit)) rep(FALSE, nrow(envs)) else
  vapply(envs$commit, same_code, logical(1L), USE.NAMES = FALSE)
env_other_commit <- if (is.null(envs)) character() else envs$environment[!env_code_ok]
env_other_req <- if (is.null(envs)) character() else envs$environment[is_req & !env_code_ok]
env_ok <- env_complete && !length(env_failing) && !length(env_stale) && !length(env_other_req)
# R CMD check, a condition of the release procedure: every CI job of the plan
# has a record, and every CI record's check succeeded
env_ci <- if (is.null(envs)) logical() else envs$source == "ci"
check_missing <- env_plan$label[env_plan$source == "ci" & !env_have]
check_failing <- if (is.null(envs)) character() else
  sprintf("%s (%s)", envs$environment[env_ci & !env_check_ok(envs)],
          ifelse(is.na(envs$check[env_ci & !env_check_ok(envs)]), "no result",
                 envs$check[env_ci & !env_check_ok(envs)]))
check_ok <- !is.null(envs) && !length(check_missing) && !length(check_failing)
env_table <- function(k, note) {
  if (!length(k)) return("No record of these was gathered in this run.")
  e <- envs[k, , drop = FALSE]
  c("| Environment | CI / local | OS | R | rtfreporter (commit, date of the record) | Main dependencies | R CMD check | Tests |",
    "|---|---|---|---|---|---|---|---|",
    sprintf("| %s%s | %s | %s | %s | %s (%s, %s) | %s | %s | %s |", e$environment,
            if (note) ifelse(is.na(e$plan_row), " *(not named in the Quality Plan)*", "") else "",
            e$source, e$os, sub("^R version ", "", e$r), e$rtfreporter,
            short_sha(e$commit), ifelse(is.na(e$date), "--", substr(e$date, 1L, 10L)),
            e$main_deps,
            ifelse(is.na(e$check) | !nzchar(e$check), "--", e$check),
            ifelse(is.na(e$tests), "no log", gsub("|", "/", e$tests, fixed = TRUE))))
}
add_expected <- c(ci = sum(!env_req & env_plan$source == "ci"),
                  local = sum(!env_req & env_plan$source == "local"))
env_lines <- if (is.null(envs)) {
  "No environment's record was gathered in this run (the CI run of the release gathers them)."
} else c(
  "### Required environments",
  "",
  sprintf("The verdict (criterion 3) rests on these: the Quality Plan names %d in CI and %d local; gathered here: %d in CI, %d local%s.",
          expected_env[["ci"]], expected_env[["local"]], n_env[["ci"]], n_env[["local"]],
          if (length(env_missing)) sprintf(" (**missing: %s**)", paste(env_missing, collapse = "; ")) else ""),
  "",
  env_table(which(is_req), FALSE),
  "",
  "### Additional environments (for reference; not judged)",
  "",
  sprintf("Run and reported, not judged: a failure here is named with its tests' summary line, and does not enter the verdict. The Quality Plan names %d in CI and %d local; gathered here: %d in CI, %d local, and %d not named in the Quality Plan.",
          add_expected[["ci"]], add_expected[["local"]],
          sum(!env_req & env_have & env_plan$source == "ci"),
          sum(!env_req & env_have & env_plan$source == "local"), sum(is.na(envs$plan_row))),
  "",
  env_table(which(!is_req), TRUE),
  if (length(env_add_failing)) c("", sprintf("Failing here (reported, not judged): %s.",
                                             paste(env_add_failing, collapse = "; "))),
  if (length(env_other_commit)) c("", sprintf(
    "Recorded at another commit than this report's (%s): %s. Such a record is of this version but not of this code; its test count can differ from this commit's.%s",
    substr(this_commit, 1L, 7L), paste(env_other_commit, collapse = "; "),
    if (length(env_other_req)) " A required one does not count for criterion 3 until it is made again." else "")))

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
  if (!env_complete) sprintf("not every required environment was gathered (%d of %d in CI, %d of %d local; missing: %s)",
                             n_env[["ci"]], expected_env[["ci"]], n_env[["local"]],
                             expected_env[["local"]], paste(env_missing, collapse = "; ")),
  if (length(env_failing)) sprintf("the tests do not pass on every required environment (%s)",
                                   paste(env_failing, collapse = "; ")),
  if (length(env_stale)) sprintf("the record of %s (required) is of another rtfreporter version than %s",
                                 paste(env_stale, collapse = "; "), this_version),
  if (length(env_other_req)) sprintf("the record of %s (required) is not of this code (%s)",
                                     paste(env_other_req, collapse = "; "), substr(this_commit, 1L, 7L)))
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
cell <- function(x) gsub("\\s+", " ", gsub("|", "/", x, fixed = TRUE))

# ---- the results by area (the FR id's area: requirements.csv) ---------------------------
# `checked` is the review of the mappings (traceability_review.csv): "by a test" when a
# test's expectations assert the whole requirement, "by its tests together" when each
# asserts a part and together all of it, blank when not reviewed
tl$area <- fr$area[leaf]
tl$ok <- fr_ok
tl$review <- tm$checked
area_row <- function(x, label) {
  sprintf("| %s | %d | %d | %d | %d | %d | %d |", label, nrow(x), sum(x$n_ft > 0), sum(x$ok),
          sum(x$review == "by a test"), sum(x$review == "by its tests together"),
          sum(x$n_ft > 0 & !nzchar(x$review)))
}
area_lines <- c(
  "| Area | Leaf requirements | With a test | Tests pass | Review: asserts | Review: part only | Not reviewed |",
  "|---|---|---|---|---|---|---|",
  vapply(sort(unique(tl$area)), function(a) area_row(tl[tl$area == a, , drop = FALSE], a), ""),
  gsub("^[|] (.*?) [|]", "| **\\1** |", area_row(tl, "Total"), perl = TRUE))

# ---- the skips, with the reason (the mapped tests' run) ---------------------------------
skip_lines <- if (!nrow(skips)) "No test of the mapped tests' run was skipped." else {
  rs <- table(skips$reason)
  rs <- names(rs)[order(-as.integer(rs), names(rs))]
  c("| Reason | Skips | Where (test file:line) |", "|---|---|---|",
    vapply(rs, function(r) {
      w <- skips$where[skips$reason == r]
      sprintf("| %s | %d | %s |", cell(r), length(w), cell(paste(unique(w), collapse = ", ")))
    }, ""))
}

# ---- the supported environments, and which tested one stands for each range ------------
# The mapping (supported -> stands for it -> why) and the risks accepted are the
# Quality Plan's section "The environment strategy", read here at run time; the rule
# that finds an environment of this run for a row of that table is below.
desc_f <- read.dcf(file.path(root, "DESCRIPTION"), fields = c("Depends", "Imports", "Suggests"))
dep_list <- function(x) {
  x <- trimws(strsplit(ifelse(is.na(x), "", x), ",")[[1L]])
  x[nzchar(x)]
}
r_min <- sub("^R \\(>= ([0-9.]+)\\)$", "\\1", grep("^R \\(", dep_list(desc_f[, "Depends"]), value = TRUE))
imports <- dep_list(desc_f[, "Imports"])
base_pkgs <- rownames(utils::installed.packages(priority = "base"))
sug <- dep_list(desc_f[, "Suggests"])
sug_pkg <- sub("\\s*[(].*$", "", sug)
sug_min <- ifelse(grepl("[(]>=", sug), sub("^.*[(]>=\\s*([^)]+)[)].*$", "\\1", sug), "")
plan <- readLines(file.path(vdir, "quality-plan.md"), encoding = "UTF-8", warn = FALSE)
i0 <- grep("^### The environment strategy", plan)
i1 <- grep("^##", plan)
i1 <- if (length(i0)) min(c(i1[i1 > i0], length(plan) + 1L)) - 1L else 0L
strategy <- if (length(i0)) plan[i0:i1] else character()
# one row: the tier (required or additional), the supported range, what
# stands for it, why that is enough
rows <- strsplit(sub("^[|] *", "", sub(" *[|]$", "", grep("^[|]", strategy, value = TRUE))), " *[|] *")
rows <- Filter(function(x) length(x) == 4L && !grepl("^-+$", x[1L]) && x[1L] != "Tier", rows)
# the risks accepted: the list that follows "What is not tested"
j0 <- grep("^What is not tested", strategy)
risk_items <- character()
if (length(j0)) {
  for (ln in strategy[-seq_len(j0)]) {
    if (grepl("^- ", ln)) risk_items <- c(risk_items, sub("^- ", "", ln))
    else if (grepl("^  +\\S", ln) && length(risk_items)) {
      risk_items[length(risk_items)] <- paste(risk_items[length(risk_items)], trimws(ln))
    } else if (nzchar(trimws(ln)) && length(risk_items)) break
  }
}
# the versions of every suggested package, by environment (packages.csv of each record)
env_dirs <- c(list.dirs(file.path(root, "validation", "env-out"), recursive = FALSE),
              list.dirs(file.path(root, "validation", "env-local"), recursive = FALSE))
env_dirs <- env_dirs[file.exists(file.path(env_dirs, "env.csv"))]
pk <- do.call(rbind, lapply(env_dirs, function(d) {
  p <- file.path(d, "packages.csv")
  if (!file.exists(p)) return(NULL)
  x <- utils::read.csv(p, colClasses = "character", na.strings = c("NA", ""), stringsAsFactors = FALSE)
  x$environment <- utils::read.csv(file.path(d, "env.csv"), colClasses = "character")$environment
  x
}))
# which environment of this run stands for a row of the plan's table: by the
# role env_plan (at the top) gives each record
env_for <- function(supported) {
  if (is.null(envs)) return(list(list(label = NULL, idx = integer())))
  devel <- grepl("Under development|devel", envs$r)
  rv <- suppressWarnings(numeric_version(ifelse(grepl("^R version [0-9]", envs$r),
                                         sub("^R version ([0-9]+[.][0-9]+).*$", "\\1", envs$r), NA)))
  # the minimum environment runs R at DESCRIPTION's minimum
  is_min <- !devel & !is.na(rv) & rv == numeric_version(r_min[1L])[1L, 1:2]
  role_is <- function(r) envs$role == r
  # (the plan asks for a CI release job on each system; a local run does not count for it)
  os_is <- function(re) which(grepl(re, envs$os) & envs$source == "ci" & role_is("recommended"))
  if (grepl("^The minimum versions", supported)) {
    list(list(label = NULL, idx = which(role_is("minimum") & is_min)))
  } else if (grepl("^The recommended versions", supported)) {
    list(list(label = NULL, idx = which(role_is("recommended"))))
  } else if (grepl("^Windows, macOS and Linux", supported)) {
    list(list(label = "Windows", idx = os_is("^Windows")), list(label = "macOS", idx = os_is("^macOS")),
         list(label = "Linux", idx = os_is("Ubuntu|Debian|Linux|Fedora")))
  } else if (grepl("^The previous release of R", supported)) {
    list(list(label = NULL, idx = which(role_is("oldrel"))))
  } else if (grepl("^The next R", supported)) {
    list(list(label = NULL, idx = which(role_is("devel"))))
  } else if (grepl("^An older R", supported)) {
    list(list(label = NULL, idx = which(role_is("older"))))
  } else list(list(label = NULL, idx = NA_integer_))
}
# a gap is a required row with no passing record of this version; an
# additional row's result is shown, not judged
sup_gaps <- character()
sup_rows <- if (!is.null(envs)) unlist(lapply(rows, function(x) {
  required <- identical(tolower(x[1L]), "required")
  lapply(env_for(x[2L]), function(m) {
    what <- if (is.null(m$label)) x[2L] else sprintf("%s: %s", x[2L], m$label)
    if (anyNA(m$idx)) {
      if (required) sup_gaps <<- c(sup_gaps, what)
      return(sprintf("| %s | %s | %s | (no rule in validation/trace.R for this row of the plan) | **not checked** | %s |",
                     x[1L], cell(what), cell(x[3L]), cell(x[4L])))
    }
    bad <- m$idx[m$idx %in% which(!env_tests_ok(envs) | envs$environment %in% env_stale_all)]
    verdict <- if (!length(m$idx)) {
      if (required) sup_gaps <<- c(sup_gaps, what)
      if (required) "**no environment gathered**" else "no environment gathered (not judged)"
    } else if (length(bad)) {
      if (required) sup_gaps <<- c(sup_gaps, what)
      sprintf(if (required) "**failing: %s**" else "failing: %s (not judged)",
              paste(envs$environment[bad], collapse = "; "))
    } else sprintf("pass (%d)", length(m$idx))
    sprintf("| %s | %s | %s | %s | %s | %s |", x[1L], cell(what), cell(x[3L]),
            if (length(m$idx)) cell(paste(envs$environment[m$idx], collapse = "; ")) else "--",
            verdict, cell(x[4L]))
  })
}), use.names = FALSE)
sup_head <- c("| Tier | Supported | Stands for it (Quality Plan) | In this run | Result | Why that is enough (Quality Plan) |",
              "|---|---|---|---|---|---|")
min_run <- function(p) {
  x <- pk[pk$package == p & !is.na(pk$version), , drop = FALSE]
  if (!nrow(x)) return(c(low = "--", high = "--"))
  v <- numeric_version(x$version)
  c(low = as.character(min(v)), high = as.character(max(v)))
}
# the environments where a package was not installed (or its record does not list it)
absent_in <- function(p) {
  have <- pk$environment[pk$package == p & !is.na(pk$version)]
  setdiff(unique(pk$environment), have)
}
sug_show <- if (is.null(pk)) integer() else
  which(nzchar(sug_min) | vapply(sug_pkg, function(p) length(absent_in(p)) > 0L, NA))
sug_lines <- if (is.null(pk)) "No environment's list of package versions was gathered in this run." else c(
  "| Suggested package | Minimum (DESCRIPTION) | Lowest used | Highest used | Not installed in |",
  "|---|---|---|---|---|",
  vapply(sug_show, function(k) {
    m <- min_run(sug_pkg[k])
    gone <- absent_in(sug_pkg[k])
    sprintf("| %s | %s | %s | %s | %s |", sug_pkg[k], if (nzchar(sug_min[k])) sug_min[k] else "--",
            m[["low"]], m[["high"]], if (length(gone)) cell(paste(gone, collapse = "; ")) else "")
  }, ""))
# run at exactly its minimum in some environment; and the environments that ran a
# version older than the minimum (a record made before the minimum was raised)
at_min <- if (is.null(pk)) NA_integer_ else sum(vapply(seq_along(sug_pkg), function(k) {
  v <- pk$version[pk$package == sug_pkg[k] & !is.na(pk$version)]
  nzchar(sug_min[k]) && length(v) &&
    any(numeric_version(v) == numeric_version(sug_min[k]))
}, NA))
below_min <- if (is.null(pk)) character() else unlist(lapply(which(nzchar(sug_min)), function(k) {
  x <- pk[pk$package == sug_pkg[k] & !is.na(pk$version), , drop = FALSE]
  x <- x[numeric_version(x$version) < numeric_version(sug_min[k]), , drop = FALSE]
  if (nrow(x)) sprintf("%s %s (minimum %s) in %s", sug_pkg[k], x$version, sug_min[k], x$environment)
}))
# the tier of the record each is in: an additional record's counts against nothing
below_tier <- if (!length(below_min)) character() else {
  e <- sub("^.* in ", "", below_min)
  t <- if (is.null(envs)) rep(NA_character_, length(e)) else envs$tier[match(e, envs$environment)]
  ifelse(is.na(t), "additional", t)
}
# a required record below a minimum stops the verdict (criterion 3): that
# environment was not made as the Quality Plan says
below_req <- below_min[below_tier == "required"]
if (length(below_req)) {
  env_ok <- FALSE
  ready <- FALSE
  blockers <- c(blockers, sprintf("a required environment ran a version older than DESCRIPTION's minimum (%s)",
                                  paste(below_req, collapse = "; ")))
}
sup_lines <- c(
  sprintf("What the package says it supports, read from DESCRIPTION: R (>= %s); %s; %d suggested packages, %d with a minimum version.",
          r_min[1L],
          if (all(imports %in% base_pkgs)) {
            sprintf("imports only packages that ship with R (%s)", paste(imports, collapse = ", "))
          } else sprintf("imports %s", paste(imports, collapse = ", ")),
          length(sug), sum(nzchar(sug_min))),
  "The operating systems are Windows, macOS and Linux (the Quality Plan, section 5).",
  "",
  "For each supported range, the environment of this run that stands for it, as the Quality Plan's *environment strategy* says, and its result:",
  "",
  if (is.null(envs)) "No environment's record was gathered in this run." else c(sup_head, unlist(sup_rows)),
  if (length(sup_gaps)) c("", sprintf("**Not every required supported range has a passing environment standing for it: %s.**",
                                       paste(sup_gaps, collapse = "; "))) else
    if (!is.null(envs)) c("", "Every required supported range has at least one passing environment standing for it; the additional rows are shown for reference, not judged."),
  "",
  "The suggested packages, as the environments ran them (those with a minimum version, and those not installed in some environment):",
  "",
  sug_lines,
  if (!is.na(at_min)) c("", sprintf("%d of the %d suggested packages with a minimum version were run at that minimum in at least one environment%s.",
                                    at_min, sum(nzchar(sug_min)),
                                    if (at_min < sum(nzchar(sug_min))) "; the others ran only with another version" else "")),
  if (any(below_tier == "required")) c("", sprintf("**Run with a version older than DESCRIPTION's minimum in a required environment** (criterion 3 does not hold until that environment is made again): %s.",
                                                   paste(below_min[below_tier == "required"], collapse = "; "))),
  if (any(below_tier == "additional")) c("", sprintf("Run with a version older than DESCRIPTION's minimum in an additional environment (named only; it counts against nothing): %s.",
                                                     paste(below_min[below_tier == "additional"], collapse = "; "))),
  "",
  "What is not tested, and the risk accepted (the Quality Plan, the *environment strategy*):",
  "",
  if (length(risk_items)) paste("-", risk_items) else "(the list was not found in inst/validation/quality-plan.md)")
sm <- c(
  sprintf("# rtfreporter %s: Quality Report", utils::packageVersion("rtfreporter")),
  "",
  if (!is.null(draft)) c(sprintf("**%s**", draft), ""),
  sprintf("Against the Quality Plan: inst/validation/quality-plan.md as in commit %s.", this_commit),
  "",
  sprintf("Version: %s. Tag: %s. Date: %s. Commit: %s. Scope: %s.", utils::packageVersion("rtfreporter"),
          if (is.null(draft) && nzchar(Sys.getenv("GITHUB_REF_NAME"))) Sys.getenv("GITHUB_REF_NAME") else
            "none (a DRAFT)",
          format(Sys.Date()), this_commit, paste(scope, collapse = ", ")),
  "",
  "## Results",
  "",
  "| Measure | Value |", "|---|---|",
  sprintf("| Leaf requirements with a test, mapped by the developer | %s |", pct(sum(tl$n_ft > 0), nrow(tl))),
  sprintf("| Guard requirements (unintended use stopped) with a test | %s |",
          pct(sum(tl$n_ft > 0 & tl$kind == "guard"), n_guard)),
  sprintf("| Interaction requirements (functions or arguments together) with a test | %s |",
          pct(sum(tl$n_ft > 0 & tl$kind == "interaction"), n_ix)),
  sprintf("| Leaf requirements whose tests all pass | %s |", pct(sum(fr_ok), nrow(tl))),
  # (testthat's `|` would split the table's cell: `/` as the environments' table has it)
  sprintf("| The mapped tests, run (test-log.txt) | %s |", gsub("|", "/", pass_line, fixed = TRUE)),
  sprintf("| Arguments a requirement names (functions with their own requirements) | %s |",
          pct(sum(fn_tab$arguments_in_fr), sum(fn_tab$arguments))),
  # (the upper FRs' own status is the matrix's second sheet; a gap is named here)
  sprintf("| Upper requirements fully covered | %s%s |",
          pct(sum(tr$coverage[!leaf] == "covered"), sum(!leaf)),
          if (all(tr$coverage[!leaf] == "covered")) "" else
            paste0("; not covered: ", paste(sprintf("%s %s", tr$fr[!leaf][tr$coverage[!leaf] != "covered"],
                                                     tr$coverage[!leaf][tr$coverage[!leaf] != "covered"]),
                                             collapse = "; "))),
  "",
  "## Results by area",
  "",
  "The leaf requirements by the area of their FR id; *Review* is the check of each mapping (traceability_review.csv): **asserts**, a test's expectations assert the whole requirement; **part only**, each test asserts a part and together all of it.",
  "",
  area_lines,
  "",
  "## Skipped tests, with the reason",
  "",
  sprintf("The mapped tests' run (test-log.txt): %d skipped, by the reason the test gives. The skip count of each other environment is in its table row; its reasons are in environments.txt.", nrow(skips)),
  "",
  skip_lines,
  "",
  "## Environments",
  "",
  sprintf("The same tests, on the environments of the Quality Plan (section 5), each record of rtfreporter %s: the required ones (%d in CI, %d local), on which the verdict rests, and the additional ones, for reference.",
          this_version, expected_env[["ci"]], expected_env[["local"]]),
  "",
  env_lines,
  "",
  "## Supported environments, and the tested ones that stand for them",
  "",
  sup_lines,
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
  "What assures the quality, in order, against the acceptance criteria of the Quality Plan (section 4):",
  "",
  sprintf("1. **Environments** (criterion 3). %s%s",
          if (env_ok) {
            sprintf("The same tests pass on every required environment of the Quality Plan (%d: %d in CI and %d local), each record of this version and of this code; environments.txt holds each one's sessionInfo and tests' log. The additional environments (%d gathered) are reported, not judged%s.",
                    sum(n_env), n_env[["ci"]], n_env[["local"]], sum(!is_req),
                    if (length(env_add_failing)) paste0("; failing there: ", paste(env_add_failing, collapse = "; ")) else "")
          } else if (is.null(envs)) {
            sprintf("No environment was gathered in this run; the CI run of the release gathers every one (required: %d in CI, %d local).",
                    expected_env[["ci"]], expected_env[["local"]])
          } else {
            sprintf("**Not every required environment is gathered, passing, of this version and of this code, at or above every minimum** (gathered: %d of %d in CI, %d of %d local%s%s%s%s%s): see Environments.",
                    n_env[["ci"]], expected_env[["ci"]], n_env[["local"]], expected_env[["local"]],
                    if (length(env_missing)) paste0("; missing: ", paste(env_missing, collapse = "; ")) else "",
                    if (length(env_failing)) paste0("; failing: ", paste(env_failing, collapse = "; ")) else "",
                    if (length(env_stale)) paste0("; of another version: ", paste(env_stale, collapse = "; ")) else "",
                    if (length(env_other_req)) paste0("; of other code: ", paste(env_other_req, collapse = "; ")) else "",
                    if (length(below_req)) paste0("; below a minimum: ", paste(below_req, collapse = "; ")) else "")
          },
          if (is.null(envs)) "" else
            sprintf(" That R CMD check succeeds on every CI job is a condition of the release procedure (the Quality Plan, section 7), not of this verdict: %s.",
                    if (check_ok) {
                      sprintf("it holds (success on all %d CI jobs)", sum(env_ci))
                    } else {
                      sprintf("**it does not hold**%s%s",
                              if (length(check_failing)) paste0(" (not a success: ", paste(check_failing, collapse = "; "), ")") else "",
                              if (length(check_missing)) paste0(" (no record: ", paste(check_missing, collapse = "; "), ")") else "")
                    })),
  sprintf("2. **Tests** (criteria 1 and 2). %s",
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
  "3. **Process** (criterion 4). Responsibilities are as the Quality Plan says (section 7): the development team evaluates each issue; the developer making a change adds its tests and requirements and runs the tests; the maintainer reviews and approves a contributor's pull request, and merges a change into the release branch only after all tests, of the change and for regression, have passed in all CI environments. GitHub keeps that record (the issues, the pull requests, the CI runs); it is referred to, not copied.",
  "4. **Other measures** (not criteria). Line coverage (Attachments, at the end of this report) and the arguments the tests give (functions.csv) are secondary: aids that show where to look, not evidence of a requirement met.",
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
  "On the release each file's name carries the tag (`quality-report-<tag>.pdf`, `traceability_matrix-<tag>.xlsx`, ...); a CI run started by hand keeps them as its artifact `quality-report`.",
  "",
  "- **traceability_matrix.xlsx**, the Traceability Matrix (FR -> FT). Sheet *FR to FT*: one row a leaf requirement -- its FR ID, parent, function, type, the requirement, the FT IDs that check it and where they are (test file:line), its status (`covered` or `none`), whether the check of its mappings found the requirement asserted (`checked`: by a test, or by its tests together) and the expectation lines that assert it (`evidence`). Read it by requirement; a blank FT ID is a deviation (above). Sheet *upper FRs*: each parent requirement and whether every leaf under it is covered. It holds no results: those are test-log.txt. (traceability_matrix.csv, in the artifact, is the first sheet.)",
  "- **test-log.txt**: testthat's own output for the mapped tests, run in this job, ending in `[ FAIL n | WARN n | SKIP n | PASS n ]`. The evidence that they pass.",
  "- **ft.csv**: one row a functional test: its FT ID, file, name and the requirements it checks. Read it by test.",
  "- **functions.csv**: one row a function in scope: its leaf requirements by type, how many have a test, its arguments, those a requirement names, those a test gives and those no requirement names. Read it for a function's depth (an aid).",
  "- **candidates.csv**: the tests that call a function in scope but are not mapped: what to read next. Never counted.",
  "- **environments.txt**: one part an environment of the two tables above (required, and additional): its sessionInfo() (R, the OS, the locale), the version of every package DESCRIPTION names (Imports and Suggests; NA where one is not installed) and testthat's log. Read it for what, exactly, the tests ran with.",
  "- **Line coverage** (Attachments, at the end of this report): the share of the package's lines the tests run, by file, floored to one decimal. An aid, not evidence of a requirement met.")
writeLines(enc2utf8(sm), file.path(out, "quality-report.md"), useBytes = TRUE)
cat(sprintf("FR %d, FT %d, ready %s%s, written to %s\n", nrow(tl), nrow(map), ready,
            if (is.null(draft)) "" else " (draft)", out))
