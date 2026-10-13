# ============================================================================
#  rtfreporter_ai_manual() -- hand the AI briefings to the user (#463)
# ============================================================================
#
#  The manuals are shipped in `inst/ai/` rather than only published on the
#  pkgdown site, and this is why: the site serves whatever is on `main`
#  (`development: mode: unreleased`), while a user is on whichever release
#  they installed.  A manual read against the wrong version is worse than no
#  manual, because an assistant quotes a function that will not resolve with
#  exactly the confidence it quotes one that will.  Shipping the file inside
#  the package makes it the same artefact as the code, so the two cannot
#  disagree; this accessor is how it comes back out.

#' The AI assistant manuals that ship with this package
#'
#' rtfreporter is too new to be in any chat model's training data: asked for
#' rtfreporter code, an assistant reaches for `r2rtf`'s verbs or invents
#' arguments, and flags neither as a guess.  The fix is to give it the facts
#' first, and these two files are those facts, each sized to sit in one chat
#' session.
#'
#' `rtfreporter_ai_manual()` returns the path to the copy **installed with
#' this package**, so the manual you attach always describes the version you
#' actually have.  The same files are published on the documentation site, but
#' that copy tracks the development version -- when the two differ, this one is
#' the one that matches your installation.
#'
#' @section Which manual:
#' \describe{
#'   \item{`"user"` (default)}{For *using* rtfreporter: the workflow, every
#'     [as_rtftables()] argument, the four clinical table shapes, headers and
#'     page tokens, listings, figures, borders, tables from a cards / cardx
#'     ARD ([table_plan()] and the `plan_*()` verbs), and the complete export
#'     list.}
#'   \item{`"dev"`}{For working *on* the package: the invariants, the S3 object
#'     model, the rendering pipeline, the adapter contract, the plan engine, and the test /
#'     docs / release conventions.}
#' }
#'
#' Attach **one** of them, never both: the user manual forbids touching
#' internals and the developer manual requires it, so an assistant given both
#' follows neither cleanly.
#'
#' @section The kit: `dir =`:
#' With `dir`, the user manual comes as a **kit**: a flat folder an assistant
#' reads in order, to write one table's R program from a shell (the table's
#' mock-up) and an ARD.
#' \describe{
#'   \item{`00_START_HERE.md`}{The request: what to attach, the three-step
#'     layout every program has (the plan over the ARD; the document; the
#'     file), the rules (only the functions in the manual; the numbers are
#'     the ARD's; the layout is the plan's).}
#'   \item{`01_rtfreporter-ai-user-manual.md`}{The manual (its §17a is how to
#'     write a table's program from a shell and an ARD).}
#'   \item{`02_example_orr_subgroups.R`, `03_example_ae_soc_pt.R`,
#'     `04_example_demographics.R`}{Programs in that layout: an ORR by
#'     subgroup with a section per arm, adverse events by SOC and PT, and
#'     demographics with a Total and a p-value column.}
#'   \item{`05_ard.csv`, `06_ard_shape.md`}{With `ard`: the ARD as
#'     [normalize_ard()] flattens it (every row when there are 2,000 or
#'     fewer, else each analysis' first rows), and its shape -- the keys and
#'     their values, which keys come alone or together, each variable's
#'     statistics, a key's own counts, and a starting [table_plan()] -- so
#'     the program names what the ARD really has.}
#' }
#' Nothing is sent anywhere: the folder is for the user to attach.
#'
#' @param dir A folder to write the manual into (created when it does not
#'   exist).  `NULL` (default): nothing is written, and the installed
#'   manual's path comes back.  For `which = "user"` the folder gets the kit
#'   (see *The kit*); for `"dev"`, the developer manual alone.
#' @param ard An ARD (a cards ARD, or one [normalize_ard()] has flattened)
#'   for the kit's `05_ard.csv` and `06_ard_shape.md`.  `NULL` (default):
#'   `00_START_HERE.md` says how to add them.
#' @param which `"user"` (default) or `"dev"` -- see *Which manual*.
#' @param overwrite Replace files of the same names already in `dir`.
#'   Default `FALSE`.
#'
#' @return The installed manual's path when `dir` is `NULL`; otherwise the
#'   paths written, invisibly.
#'
#' @examples
#' # where the manual lives
#' rtfreporter_ai_manual()
#' writeLines(head(readLines(rtfreporter_ai_manual()), 3))
#'
#' # the kit, with an ARD's shape
#' if (requireNamespace("cards", quietly = TRUE)) {
#'   ard <- cards::ard_stack(cards::ADSL, .by = ARM,
#'                           cards::ard_tabulate(variables = c(SEX, AGEGR1)))
#'   kit <- rtfreporter_ai_manual(file.path(tempdir(), "kit"), ard = ard, overwrite = TRUE)
#'   basename(kit)
#' }
#' @export
rtfreporter_ai_manual <- function(dir = NULL, ard = NULL, which = c("user", "dev"),
                                  overwrite = FALSE) {
  which <- match.arg(which)
  name  <- sprintf("rtfreporter-ai-%s-manual.md", which)
  src   <- system.file("ai", name, package = "rtfreporter")
  if (!nzchar(src) || !file.exists(src)) {
    stop(sprintf(paste0(
      "`rtfreporter_ai_manual()`: %s is not in this installation.\n  It ",
      "ships in inst/ai/; a package built before v0.7.56 does not have it.\n  ",
      "Reinstall, or read the published copy at\n  %s"),
      name, "https://ichirio.github.io/rtfreporter/ai/"), call. = FALSE)
  }
  if (is.null(dir)) return(src)
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    stop("`rtfreporter_ai_manual(dir = )` is one folder.", call. = FALSE)
  }
  if (file.exists(dir) && !dir.exists(dir)) {
    stop(sprintf("`rtfreporter_ai_manual()`: '%s' is a file, not a folder.", dir),
         call. = FALSE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  free <- function(to) {
    dest <- file.path(dir, to)
    if (file.exists(dest) && !isTRUE(overwrite)) {
      stop(sprintf(paste0("`rtfreporter_ai_manual()`: '%s' already exists; ",
                          "pass `overwrite = TRUE` to replace it."), dest), call. = FALSE)
    }
    dest
  }
  copy <- function(to, from) {
    dest <- free(to)
    if (!file.copy(from, dest, overwrite = TRUE)) {
      stop(sprintf("`rtfreporter_ai_manual()`: could not write '%s'.", dest), call. = FALSE)
    }
    dest
  }
  write <- function(to, lines) {
    dest <- free(to)
    con <- file(dest, open = "w", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(lines, con)
    dest
  }
  if (identical(which, "dev")) return(invisible(copy(name, src)))
  kit <- system.file("ai", "kit", package = "rtfreporter")
  start <- readLines(file.path(kit, "00_START_HERE.md"), warn = FALSE, encoding = "UTF-8")
  if (is.null(ard)) {
    start <- c(start, "", "## No ARD attached", "",
               paste0("No ARD came with this kit, so `05_ard.csv` and ",
                      "`06_ard_shape.md` are not here.  The user writes them"),
               "with `rtfreporter_ai_manual(dir, ard = ard)`.  Without them, name the",
               "keys and statistics as the examples do, and list the ones you assumed",
               "as `# QUESTION:` comments.")
  }
  examples <- c("02_example_orr_subgroups.R", "03_example_ae_soc_pt.R",
                "04_example_demographics.R")
  out <- c(write("00_START_HERE.md", start),
           copy("01_rtfreporter-ai-user-manual.md", src),
           vapply(examples, function(f) copy(f, file.path(kit, f)), ""))
  if (!is.null(ard)) {
    d <- if (inherits(ard, "ard_long")) ard else normalize_ard(ard)
    rows <- .ai_kit_ard_rows(d)
    dest <- free("05_ard.csv")
    utils::write.csv(rows, dest, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    out <- c(out, dest, write("06_ard_shape.md", .ai_kit_shape(d, nrow(rows))))
  }
  invisible(unname(out))
}

# The ARD's rows for `05_ard.csv`: the keys, what each row is, its value --
# every row when there are 2,000 or fewer, else each analysis' first 30.
.ai_kit_ard_rows <- function(d) {
  keep <- c(.ai_kit_keys(d), "variable", "variable_level", "context", "stat_name",
            "stat_label", "stat", "stat_fmt", ".label", ".depth", ".overall", ".key_own")
  x <- as.data.frame(d)[, intersect(keep, names(d)), drop = FALSE]
  for (k in names(x)) if (is.factor(x[[k]])) x[[k]] <- as.character(x[[k]])
  if (nrow(x) <= 2000L) return(x)
  grp <- paste(x$variable, x$context, sep = "\r")
  x[stats::ave(seq_along(grp), grp, FUN = seq_along) <= 30L, , drop = FALSE]
}

# The key columns normalize_ard() put first: everything before `variable`.
.ai_kit_keys <- function(d) {
  i <- match("variable", names(d))
  if (is.na(i) || i == 1L) character(0) else names(d)[seq_len(i - 1L)]
}

# `06_ard_shape.md`: what an assistant must know before naming anything.
.ai_kit_shape <- function(d, n_csv) {
  d <- as.data.frame(d)
  keys <- .ai_kit_keys(d)
  cell <- function(x) gsub("|", "\\|", x, fixed = TRUE)
  vals <- function(k) {
    v <- d[[k]]
    lv <- if (is.factor(v)) levels(v) else .ard_first_seen(as.character(v[!is.na(v)]))
    more <- if (length(lv) > 15L) sprintf(" ... (%d values)", length(lv)) else ""
    paste0(cell(paste(utils::head(lv, 15L), collapse = ", ")), more)
  }
  set <- matrix(vapply(keys, function(k) !is.na(d[[k]]), logical(nrow(d))),
                nrow = nrow(d), dimnames = list(NULL, keys))
  combo <- apply(set, 1L, function(r) paste(keys[r], collapse = " + "))
  combo[!nzchar(combo)] <- "(no key)"
  combo_n <- sort(table(combo), decreasing = TRUE)
  every <- keys[colMeans(set) > 0.9]
  alone <- setdiff(keys, every)
  alone <- alone[vapply(alone, function(k) any(combo == k | combo == paste(c(every, k), collapse = " + ")), NA)]
  # rows with two or more of the other keys: a crossed layer
  parts <- lapply(strsplit(names(combo_n), " + ", fixed = TRUE), setdiff, every)
  crossed <- unique(Filter(function(x) length(x) >= 2L, parts))
  own <- if (".key_own" %in% names(d)) d$.key_own %in% TRUE else rep(FALSE, nrow(d))
  vc <- unique(d[!own, c("variable", "context"), drop = FALSE])
  vrows <- vapply(seq_len(nrow(vc)), function(i) {
    at <- !own & d$variable %in% vc$variable[i] & d$context %in% vc$context[i]
    sprintf("| `%s` | %s | %s | %d |", vc$variable[i], vc$context[i],
            cell(paste(unique(d$stat_name[at]), collapse = ", ")), sum(at))
  }, "")
  owns <- unique(d$variable[own])
  start <- c(
    if (length(every)) sprintf("`table_plan(cols = \"%s\", ...)`", every[1L]),
    if (length(alone) + length(crossed) > 1L) sprintf(paste0(
      "`normalize_ard(stack = list(subgroup = %s))` and ",
      "`rows = c(grp1 = \"subgroup\", grp2 = \"subgroup_level\")`"),
      if (length(crossed)) paste0("list(", paste(c(paste0("\"", alone, "\""),
        vapply(crossed, function(x) paste0("c(", paste0("\"", x, "\"", collapse = ", "), ")"), "")),
        collapse = ", "), ")")
      else paste0("c(", paste0("\"", alone, "\"", collapse = ", "), ")")),
    if (length(unique(vc$variable)) > 1L)
      "`rows = c(group = \"variable\")` (several analysis variables)")
  c("# The ARD's shape",
    "",
    sprintf("`05_ard.csv` is the ARD as `normalize_ard()` flattens it: %d rows%s, one statistic each.",
            n_csv, if (n_csv < nrow(d)) sprintf(" of %d (each analysis' first)", nrow(d)) else ""),
    "",
    "## Keys (what `cols` / `rows` may name)",
    "",
    "| key | rows | values (in order) |", "|---|---|---|",
    vapply(keys, function(k) sprintf("| `%s` | %d | %s |", k, sum(!is.na(d[[k]])), vals(k)), ""),
    "",
    "## Which keys come together",
    "",
    "| keys set on a row | rows |", "|---|---|",
    sprintf("| %s | %d |", names(combo_n), as.integer(combo_n)),
    "",
    if (length(alone) > 1L) c(paste0(
      "Keys on some rows only, one at a time (", paste0("`", alone, "`", collapse = ", "),
      "), are parallel subgroups: stack them.  Keys set together are a crossed subgroup ",
      "(a layer `c(...)`) or a nesting (two row keys)."), ""),
    "## Variables and their statistics",
    "",
    "| variable | context | statistics | rows |", "|---|---|---|---|",
    vrows,
    "",
    if (length(owns)) c(paste0(
      "A key's own counts (", paste0("`", owns, "`", collapse = ", "),
      "): the header's `{n}` and `heading = \"... (N={n})\"`, not rows ",
      "(`table_plan(show_keys = )` shows them as rows)."), ""),
    "## A starting point",
    "",
    if (length(start)) paste0("- ", start) else "- no key is on most rows: take `cols` from the shell",
    "",
    "Adjust it to the shell, and name nothing that is not in the tables above.")
}
