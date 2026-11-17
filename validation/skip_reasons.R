# ---------------------------------------------------------------------------
#  The skips' reasons, said for a reader (sourced by trace.R and report.R)
# ---------------------------------------------------------------------------
#  A test's skip message is short ("On Linux", "LibreOffice absent").  The
#  Quality Report and environments.txt say instead what the test checks, why
#  it was skipped there and where it runs: validation/skip-reasons.csv, one
#  row a message --
#    reason  the message as testthat gives it, or a pattern (Perl) when it
#            starts with ^ (its groups \1, \2, ... go into `why`)
#    file    the test file it is for; blank: any
#    why     the sentence
#    ok      why skipping it there hides no defect, when a run elsewhere does
#            not show it (the Quality Report finds that itself: (a) the same
#            test ran and passed on another required environment):
#              "b: ..." the behaviour is outside this environment
#              "c: ..." another test checks it (name the test)
#              "d: ..." it runs after the release tag (say where it is recorded)
#  A message the table has not is shown as it is, and the Quality Report
#  names it, so the table is kept up to date.  The tests' own messages are
#  not changed here: tests/ is the package's code.
# ---------------------------------------------------------------------------

skip_table <- function(root) {
  f <- file.path(root, "validation", "skip-reasons.csv")
  if (!file.exists(f)) return(data.frame(reason = character(), file = character(), why = character(), ok = character()))
  x <- utils::read.csv(f, colClasses = "character", na.strings = character(), encoding = "UTF-8")
  if (is.null(x$ok)) x$ok <- ""
  x[nzchar(x$reason) & nzchar(x$why), c("reason", "file", "why", "ok"), drop = FALSE]
}

# The sentence for each skip (`reason` its message, `file` its test file);
# NA where the table has none.  `col = "ok"`: why the skip hides no defect
# (b, c or d below; "" when (a), a run elsewhere, is to show it)
skip_why <- function(reason, file, tab, col = "why") {
  vapply(seq_along(reason), function(i) {
    for (j in seq_len(nrow(tab))) {
      # (a log that names no file, as an R CMD check summary may: the reason alone)
      known <- !is.na(file[i]) && nzchar(file[i])
      if (nzchar(tab$file[j]) && known && !identical(tab$file[j], file[i])) next
      r <- tab$reason[j]
      if (startsWith(r, "^")) {
        if (grepl(r, reason[i], perl = TRUE)) return(sub(r, tab[[col]][j], reason[i], perl = TRUE))
      } else if (identical(r, reason[i])) {
        return(tab[[col]][j])
      }
    }
    NA_character_
  }, "")
}

# The skips of a testthat log ("Skipped tests" section: "• reason (n): 'file:line:col', ..."),
# one row a reason: reason, n, files (the test files, ", " between them; "" when the log
# gives none), at (the skips' "file:line", ", " between them; "" when the log gives none)
skip_log <- function(lines) {
  at <- grep("Skipped tests", lines)
  none <- data.frame(reason = character(), n = integer(), files = character())
  if (!length(at)) return(none)
  rows <- list()
  for (a in at) {
    blk <- character()
    for (l in lines[-seq_len(a)]) {
      if (!nzchar(trimws(l)) || grepl("^\\[ FAIL", l)) break
      blk <- c(blk, l)
    }
    items <- strsplit(paste(blk, collapse = "\n"), "(^|\n)• ")[[1L]]
    for (it in items[nzchar(trimws(items))]) {
      it <- gsub("\\s+", " ", trimws(it))
      m <- regmatches(it, regexec("^(.*?) \\(([0-9]+)\\)(?::\\s*(.*))?$", it, perl = TRUE))[[1L]]
      if (!length(m)) next
      fl <- unique(regmatches(m[4L], gregexpr("test-[^:']+", m[4L]))[[1L]])
      at <- unique(sub(":[0-9]+$", "", regmatches(m[4L], gregexpr("test-[^:']+:[0-9]+:[0-9]+", m[4L]))[[1L]]))
      rows[[length(rows) + 1L]] <- data.frame(reason = m[2L], n = as.integer(m[3L]),
                                              files = paste(fl, collapse = ", "),
                                              at = paste(at, collapse = ", "))
    }
  }
  none$at <- character()
  if (!length(rows)) return(none)
  out <- do.call(rbind, rows)
  out[!duplicated(out$reason), , drop = FALSE]
}

# environments.txt's lines for a log's skips: each reason's sentence, its count and the
# test's own message
skip_log_lines <- function(lines, tab) {
  s <- skip_log(lines)
  if (!nrow(s)) return("No test was skipped.")
  why <- skip_why(s$reason, sub(",.*$", "", s$files), tab)
  sprintf("- %s (%d)%s\n  [the test's message: %s]", ifelse(is.na(why), s$reason, why), s$n,
          ifelse(nzchar(s$files), paste0(": ", s$files), ""), s$reason)
}
