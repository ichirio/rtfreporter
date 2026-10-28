# ---------------------------------------------------------------------------
#  Traceability: Functional Requirements (FR) -> Functional Tests (FT)
# ---------------------------------------------------------------------------
#  The FRs are inst/validation/requirements.csv.  An FT is a test_that()
#  block: its id is written at the head of its name ("FT-PLAN-012: ...") or
#  given by inst/validation/test_map.csv (file, test, ft, fr), which --map
#  writes afresh from the tests: each block calling an FR's functions is
#  mapped to that FR.  The tests are run, and the result is
#
#    validation/out/traceability.csv   one row an FR: its FTs (the mapping
#                                      alone; a matrix holds no results)
#    validation/out/ft.csv             one row an FT: its file, test and FRs
#    validation/out/traceability.html  the matrix to read
#    validation/out/test-results.csv   one row an FT: its result
#    validation/out/summary.md         the Development Summary Report (1 page)
#
#  Run from the package root:
#    Rscript validation/trace.R          # the map as it is
#    Rscript validation/trace.R --map    # the map written from the tests first
#  Needs pkgload and testthat (both in Suggests / the development setup).
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(".", winslash = "/")
vdir <- file.path(root, "inst", "validation")
out  <- file.path(root, "validation", "out")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
area <- Sys.getenv("RTFREPORTER_FR_AREA", "PLAN")   # the trial: the plan verbs

fr <- utils::read.csv(file.path(vdir, "requirements.csv"), colClasses = "character",
                      na.strings = "", stringsAsFactors = FALSE)
fr <- fr[fr$status %in% "active" & fr$area %in% area, , drop = FALSE]
fr_fns <- strsplit(fr$functions, ";", fixed = TRUE)

# ---- the tests: each test_that() block, the functions it calls --------------
blocks <- function(file) {
  ex <- tryCatch(parse(file, keep.source = TRUE), error = function(e) NULL)
  if (is.null(ex)) return(NULL)
  sr <- utils::getSrcref(ex)
  rows <- lapply(seq_along(ex), function(i) {
    e <- ex[[i]]
    if (!is.call(e) || !identical(e[[1L]], as.name("test_that"))) return(NULL)
    desc <- tryCatch(as.character(eval(e[[2L]], baseenv())), error = function(err) NA_character_)
    calls <- unique(all.names(e[[3L]]))
    data.frame(file = basename(file), test = desc, line = sr[[i]][1L],
               calls = paste(calls, collapse = " "), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
files <- list.files(file.path(root, "tests", "testthat"), "^test-.*[.]R$", full.names = TRUE)
tb <- do.call(rbind, lapply(files, blocks))
# the tests' own helpers (base_plan <- function() table_plan(...)): a block
# calling one calls what it calls, followed through helpers of helpers
helper_files <- c(files, list.files(file.path(root, "tests", "testthat"), "^helper.*[.]R$",
                                    full.names = TRUE))
helpers <- list()
for (f in helper_files) {
  ex <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
  for (e in ex) {
    if (is.call(e) && (identical(e[[1L]], as.name("<-")) || identical(e[[1L]], as.name("="))) &&
        is.name(e[[2L]]) && is.call(e[[3L]]) && identical(e[[3L]][[1L]], as.name("function"))) {
      helpers[[as.character(e[[2L]])]] <- unique(all.names(e[[3L]]))
    }
  }
}
expand <- function(cl) {
  seen <- character()
  todo <- intersect(cl, names(helpers))
  while (length(todo)) {
    h <- todo[1L]
    todo <- todo[-1L]
    if (h %in% seen) next
    seen <- c(seen, h)
    cl <- union(cl, helpers[[h]])
    todo <- c(todo, setdiff(intersect(helpers[[h]], names(helpers)), seen))
  }
  cl
}
tb$calls <- vapply(strsplit(tb$calls, " ", fixed = TRUE), function(cl) {
  paste(expand(cl), collapse = " ")
}, "")
tb$fr <- vapply(strsplit(tb$calls, " ", fixed = TRUE), function(cl) {
  paste(fr$id[vapply(fr_fns, function(f) any(f %in% cl), NA)], collapse = " ")
}, "")
named_ft <- regmatches(tb$test, regexpr("^FT-[A-Z]+-[0-9]{3}", tb$test))
tb$ft_named <- NA_character_
tb$ft_named[grepl("^FT-[A-Z]+-[0-9]{3}", tb$test)] <- named_ft

map_file <- file.path(vdir, "test_map.csv")
if ("--map" %in% args || !file.exists(map_file)) {
  # every block that calls an FR's functions is an FT of that FR; the ids in
  # the order of the files and the blocks, kept for the blocks already mapped
  old <- if (file.exists(map_file)) utils::read.csv(map_file, colClasses = "character",
                                                    stringsAsFactors = FALSE) else NULL
  hit <- tb[nzchar(tb$fr), , drop = FALSE]
  key <- paste(hit$file, hit$test, sep = "\r")
  ft <- if (!is.null(old)) old$ft[match(key, paste(old$file, old$test, sep = "\r"))] else
    rep(NA_character_, nrow(hit))
  ft[!is.na(hit$ft_named)] <- hit$ft_named[!is.na(hit$ft_named)]
  # (two blocks of one name in a file: the second gets an id of its own)
  ft[duplicated(ft) & !is.na(ft)] <- NA_character_
  used <- suppressWarnings(as.integer(sub(".*-", "", stats::na.omit(ft))))
  nxt <- max(c(0L, used), na.rm = TRUE)
  for (i in which(is.na(ft))) {
    nxt <- nxt + 1L
    ft[i] <- sprintf("FT-%s-%03d", area, nxt)
  }
  map <- data.frame(file = hit$file, test = hit$test, ft = ft, fr = hit$fr,
                    stringsAsFactors = FALSE)
  utils::write.csv(map, map_file, row.names = FALSE, fileEncoding = "UTF-8")
}
map <- utils::read.csv(map_file, colClasses = "character", stringsAsFactors = FALSE)
# a mapped test that is no longer there: the map is out of date
gone <- !paste(map$file, map$test, sep = "\r") %in% paste(tb$file, tb$test, sep = "\r")
if (any(gone)) {
  stop(sprintf("test_map.csv names %d test(s) not in the tests (renamed?): %s. Run with --map.",
               sum(gone), paste(utils::head(paste(map$file[gone], map$test[gone], sep = ": "), 3),
                                collapse = "; ")), call. = FALSE)
}

# ---- the matrix: FR -> FT, the mapping alone (no results) ------------------------
fts <- function(id) map$ft[vapply(strsplit(map$fr, " ", fixed = TRUE), function(x) id %in% x, NA)]
tr <- data.frame(fr = fr$id, statement = fr$statement, functions = fr$functions,
                 ft = vapply(fr$id, function(id) paste(fts(id), collapse = " "), ""),
                 n_ft = vapply(fr$id, function(id) length(fts(id)), 0L),
                 stringsAsFactors = FALSE, row.names = NULL)
utils::write.csv(tr, file.path(out, "traceability.csv"), row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(map[c("ft", "file", "test", "fr")], file.path(out, "ft.csv"), row.names = FALSE,
                 fileEncoding = "UTF-8")

esc <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE)
html <- c("<!doctype html><meta charset='utf-8'><title>Traceability matrix</title>",
          "<style>body{font-family:sans-serif;font-size:13px}td,th{border:1px solid #ccc;padding:3px 6px;vertical-align:top}table{border-collapse:collapse}.no{background:#ffd}</style>",
          sprintf("<h1>rtfreporter %s: FR &rarr; FT</h1>", utils::packageVersion("rtfreporter")),
          "<table><tr><th>FR</th><th>Statement</th><th>Functions</th><th>FT</th></tr>",
          sprintf("<tr class='%s'><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
                  ifelse(tr$n_ft == 0L, "no", ""), tr$fr, esc(tr$statement), esc(tr$functions),
                  esc(tr$ft)),
          "</table>")
writeLines(html, file.path(out, "traceability.html"), useBytes = TRUE)

# ---- the tests' results: apart from the matrix -----------------------------------
pkgload::load_all(root, quiet = TRUE)
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
# an FR met: it has FTs, and they all pass
fr_ok <- vapply(fr$id, function(id) {
  r <- result[map$ft %in% fts(id)]
  length(r) > 0L && !any(r %in% c("fail", "not run"))
}, NA)

# ---- the Development Summary Report: one page -----------------------------------
in_area <- unique(unlist(fr_fns))
called <- unique(unlist(strsplit(tb$calls, " ", fixed = TRUE)))
untested_fns <- setdiff(in_area, called)
sm <- c(
  sprintf("# rtfreporter %s: Development Summary Report (%s)", utils::packageVersion("rtfreporter"),
          if (identical(area, "PLAN")) "trial: the plan verbs" else area),
  "",
  sprintf("Date: %s. Commit: %s.", format(Sys.Date()),
          tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
                   error = function(e) "unknown")),
  sprintf("Environment: %s; %s.", R.version.string, utils::sessionInfo()$running),
  "",
  "## Scope",
  sprintf("The functional requirements of area %s (inst/validation/requirements.csv): %d, over %d exported functions.",
          area, nrow(fr), length(in_area)),
  "",
  "## Results",
  "",
  "| Measure | Value |", "|---|---|",
  sprintf("| Requirements with at least one test (traceability matrix) | %d of %d |",
          sum(tr$n_ft > 0), nrow(tr)),
  sprintf("| Functional tests (test blocks) | %d: %d pass, %d fail, %d skip |", nrow(map),
          sum(result == "pass"), sum(result == "fail"), sum(result == "skip")),
  sprintf("| Requirements whose tests all pass | %d of %d |", sum(fr_ok), nrow(fr)),
  sprintf("| Functions in scope not called by any test | %s |",
          if (length(untested_fns)) paste(untested_fns, collapse = ", ") else "none"),
  "",
  "A failing test fails the pull request's checks (CI), so it cannot be merged.",
  "",
  "## Deviations",
  if (!all(fr_ok)) sprintf("- %s: no passing test", fr$id[!fr_ok]) else "None.",
  "",
  "## Conclusion",
  if (all(fr_ok)) {
    "Every requirement in scope has tests, and they pass."
  } else "Not every requirement in scope is met by passing tests: see the deviations.",
  "",
  "Attachments: traceability.csv / .html (FR -> FT), ft.csv (FT -> FR), test-results.csv (each FT's result); R CMD check, covr and the tests that run each function come from CI.")
writeLines(sm, file.path(out, "summary.md"), useBytes = TRUE)
cat(sprintf("FR %d, FT %d, written to %s\n", nrow(tr), nrow(map), out))
