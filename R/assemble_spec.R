# ============================================================================
#  Helpers for assemble_rtf(): collect files, read titles, build a TOC /
#  "assembly spec", and run the assembly from that spec.
#
#  The two public entry points:
#    assemble_folder()  folder (+ output)  -> the table of contents (a
#                                             data.frame, "spec"), and the
#                                             assembled file when `output_file`
#    assemble_rtf()     files and / or a table of contents -> assembled file
#  assemble_files() / assemble_spec() / assemble_toc() / assemble_from_spec()
#  are deprecated (0.8.x) wrappers over the internal steps below.
# ============================================================================


# -- RTF title extraction ----------------------------------------------------

# Decode a short run of RTF text into plain text: each `\uN?` becomes its
# character (a non-breaking space, U+00A0, becomes a normal space), and the
# escaped braces / backslash become literals.
.rtf_decode_text <- function(x) {
  pieces <- unique(regmatches(x, gregexpr("\\\\u-?[0-9]+\\?", x))[[1L]])
  for (p in pieces) {
    cp <- as.integer(sub("\\\\u(-?[0-9]+)\\?", "\\1", p))
    ch <- if (is.na(cp) || cp == 160L) " " else intToUtf8(((cp %% 65536L) + 65536L) %% 65536L)
    x  <- gsub(p, ch, x, fixed = TRUE)
  }
  x <- gsub("\\{",  "{",  x, fixed = TRUE)
  x <- gsub("\\}",  "}",  x, fixed = TRUE)
  x <- gsub("\\\\", "\\", x, fixed = TRUE)
  trimws(x)
}

# Extract the `{\header ...}` group from an RTF string (balanced braces).
.rtf_header_block <- function(txt) {
  # Match `\header` as a complete RTF control word (not the page-setting
  # `\headery`, which shares the prefix and now precedes the group).
  pos <- regexpr("\\\\header(?![a-z])", txt, perl = TRUE)
  if (pos < 0L) return("")
  open <- pos
  while (open > 1L && substr(txt, open, open) != "{") open <- open - 1L
  depth <- 0L; i <- open; n <- nchar(txt)
  while (i <= n) {
    ch <- substr(txt, i, i)
    if (ch == "{") depth <- depth + 1L
    else if (ch == "}") { depth <- depth - 1L; if (depth == 0L) break }
    i <- i + 1L
  }
  substr(txt, open, min(i, n))
}

# The centred header cells (rtfreporter's make_header puts the table number,
# title and subtitle in centred `\qc` cells).
.rtf_centered_cells <- function(header_block) {
  if (!nzchar(header_block)) return(character(0))
  hits <- regmatches(
    header_block,
    gregexpr("\\\\qc\\\\li0\\\\ri0 .*?\\\\cell", header_block))[[1L]]
  if (length(hits) == 0L) return(character(0))
  vapply(sub("^\\\\qc\\\\li0\\\\ri0 (.*)\\\\cell$", "\\1", hits),
         .rtf_decode_text, character(1L), USE.NAMES = FALSE)
}

# Read one RTF file's table number / title / TOC label from its running
# header.  Falls back to the file name when no "Table N" header is found.
.rtf_table_label <- function(file) {
  txt    <- paste(readLines(file, warn = FALSE), collapse = " ")
  cells  <- .rtf_centered_cells(.rtf_header_block(txt))
  tabnum <- NA_character_
  for (c in cells) {
    g <- regmatches(c, regexec("^Table\\s+([0-9][0-9.A-Za-z-]*)\\b\\s*(.*)$", c))[[1L]]
    if (length(g) == 3L && nzchar(g[2L])) {
      tabnum <- g[2L]
      title_inline <- trimws(g[3L])
      break
    }
  }
  # Title = a centred cell that is neither the "Table N" line nor a "<...>"
  # subtitle; pick the longest such cell.
  cand <- cells[!grepl("^Table\\s", cells) & !grepl("^<.*>$", cells) &
                  nchar(cells) > 0L]
  title <- if (length(cand)) cand[which.max(nchar(cand))] else NA_character_
  if (!is.na(tabnum) && exists("title_inline") && nzchar(title_inline) &&
      is.na(title)) title <- title_inline

  base <- sub("\\.rtf$", "", basename(file), ignore.case = TRUE)
  label <- if (!is.na(tabnum)) {
    trimws(paste0("Table ", tabnum, if (!is.na(title)) paste0("  ", title)))
  } else if (!is.na(title)) {
    title
  } else {
    base
  }
  list(file = file, table = tabnum, title = title, label = label)
}


# -- Natural sort ------------------------------------------------------------

# Order strings so embedded numbers sort numerically ("t2" < "t10").
.natural_order <- function(x) {
  key <- vapply(x, function(s) {
    parts <- regmatches(s, gregexpr("[0-9]+|[^0-9]+", s))[[1L]]
    paste0(vapply(parts, function(p)
      if (grepl("^[0-9]+$", p)) formatC(as.numeric(p), width = 15, flag = "0",
                                        format = "d")
      else p, character(1L)), collapse = "")
  }, character(1L))
  order(key)
}


#' Collect the RTF files in a folder (deprecated)
#'
#' **Deprecated** in 0.8.x (warns once a session, still works); removed in
#' 0.9.0.  Use [assemble_folder()] (its table's `file` column), or
#' `list.files()`.
#'
#' Lists the `.rtf` files in `dir`, in natural-sorted order (so `t2` comes
#' before `t10`), ready to hand to [assemble_rtf()] or the other assembly
#' helpers.
#'
#' @param dir Directory to scan.
#' @param pattern File-name pattern (default `"[.]rtf$"`, case-insensitive).
#' @param recursive Recurse into sub-directories?  Default `FALSE`.
#' @param sort Natural-sort the result?  Default `TRUE`.
#'
#' @return A character vector of file paths.
#' @seealso [assemble_spec()], [assemble_toc()], [assemble_folder()].
#'
#' @examples
#' # two TFL files in a folder, as a study's output
#' dir <- file.path(tempdir(), "tfl")
#' dir.create(dir, showWarnings = FALSE)
#' for (t in c("14.1.1", "14.2.1")) {
#'   doc <- rtf_document() |>
#'     rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
#'     rtf_titles(list(c(paste("Table", t), "Safety Population")))
#'   generate_rtfreport(doc, file.path(dir, paste0("t", gsub(".", "_", t,
#'     fixed = TRUE), ".rtf")), overwrite = TRUE)
#' }
#' # instead: the table of contents of the folder
#' assemble_folder(dir)$file
#' @export
assemble_files <- function(dir, pattern = "[.]rtf$", recursive = FALSE,
                           sort = TRUE) {
  .deprecate_once(
    "assemble_files",
    paste0("`assemble_files()` is deprecated: `assemble_folder(dir)` gives the ",
           "folder's table of contents (its `file` column).\n  ",
           "Removed in 0.9.0."))
  .assemble_files(dir, pattern, recursive, sort)
}

.assemble_files <- function(dir, pattern = "[.]rtf$", recursive = FALSE,
                            sort = TRUE) {
  if (!dir.exists(dir)) stop("Directory not found: ", dir, call. = FALSE)
  files <- list.files(dir, pattern = pattern, full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
  if (sort && length(files)) files <- files[.natural_order(basename(files))]
  files
}


#' Build an assembly spec (one editable row per RTF) (deprecated)
#'
#' **Deprecated** in 0.8.x (warns once a session, still works); removed in
#' 0.9.0.  It is [assemble_folder()] without an `output_file`.
#'
#' Reads the table number and title from each RTF's running header (the format
#' written by rtfreporter's headers) and returns a `data.frame` -- the
#' **assembly spec** -- with one row per file.  Edit it (rename labels, add
#' section `heading`s, change `level`, reorder rows, or drop rows) and pass it
#' to [assemble_from_spec()].
#'
#' @param dir Directory to scan (ignored if `files` is given).
#' @param files Optional explicit vector of `.rtf` paths (overrides `dir`).
#' @param recursive Passed to [assemble_files()] when scanning `dir`.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{`order`}{integer, the assembly order (editable).}
#'     \item{`file`}{path to the `.rtf`.}
#'     \item{`table`}{table number read from the header (or `NA`).}
#'     \item{`heading`}{section heading to print above this entry in the TOC
#'       (`NA` = none); fill these in to group entries.}
#'     \item{`label`}{the TOC entry text (defaults to `"Table N  <title>"`).}
#'     \item{`level`}{TOC indent level of the entry (default `2`).}
#'     \item{`pages`}{page count of the file (informational).}
#'   }
#' @seealso [assemble_from_spec()], [assemble_folder()].
#'
#' @examples
#' # two TFL files in a folder, as a study's output
#' dir <- file.path(tempdir(), "tfl")
#' dir.create(dir, showWarnings = FALSE)
#' for (t in c("14.1.1", "14.2.1")) {
#'   doc <- rtf_document() |>
#'     rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
#'     rtf_titles(list(c(paste("Table", t), "Safety Population")))
#'   generate_rtfreport(doc, file.path(dir, paste0("t", gsub(".", "_", t,
#'     fixed = TRUE), ".rtf")), overwrite = TRUE)
#' }
#' # instead: the folder's table of contents, edited, then assembled
#' spec <- assemble_folder(dir)
#' spec$heading[spec$table == "14.1.1"] <- "Demographics"   # group entries
#' assemble_rtf(toc = spec, output_file = tempfile(fileext = ".rtf"))
#' @export
assemble_spec <- function(dir = NULL, files = NULL, recursive = FALSE) {
  .deprecate_once(
    "assemble_spec",
    paste0("`assemble_spec()` is deprecated: it is `assemble_folder(dir)` ",
           "(no `output_file`).\n  Removed in 0.9.0."))
  .assemble_spec(dir, files, recursive)
}

.assemble_spec <- function(dir = NULL, files = NULL, recursive = FALSE) {
  if (is.null(files)) {
    if (is.null(dir)) stop("Supply `dir` or `files`.", call. = FALSE)
    files <- .assemble_files(dir, recursive = recursive)
  }
  if (length(files) == 0L) stop("No RTF files found.", call. = FALSE)
  info <- lapply(files, .rtf_table_label)
  tabs <- vapply(info, function(x) x$table %||% NA_character_, character(1L))
  # Default order: by table number (so 14.1.x precedes 14.3.x); files without
  # a detected number keep their incoming (natural file-name) order, last.
  ord  <- .natural_order(ifelse(is.na(tabs), paste0("~", basename(files)), tabs))
  info <- info[ord]; tabs <- tabs[ord]; files <- files[ord]
  data.frame(
    order   = seq_along(files),
    file    = vapply(info, `[[`, character(1L), "file"),
    table   = vapply(info, function(x) x$table %||% NA_character_, character(1L)),
    heading = NA_character_,
    label   = vapply(info, `[[`, character(1L), "label"),
    level   = 2L,
    pages   = vapply(files, function(f)
      .count_rtf_pages(readLines(f, warn = FALSE)), integer(1L)),
    stringsAsFactors = FALSE
  )
}


# Internal: assembly spec (data.frame) -> `toc =` list of toc_heading() /
# toc_entry().  A new heading is emitted whenever the `heading` value changes.
.spec_to_toc <- function(spec) {
  spec <- .check_toc_table(spec)
  toc <- list()
  last_heading <- NULL
  for (i in seq_len(nrow(spec))) {
    h <- if (!is.null(spec$heading)) spec$heading[i] else NA_character_
    if (!is.na(h) && nzchar(trimws(h)) && !identical(h, last_heading)) {
      toc <- c(toc, list(.toc_heading(trimws(h), level = 1L)))
      last_heading <- h
    }
    lvl <- if (!is.null(spec$level) && !is.na(spec$level[i]))
      as.integer(spec$level[i]) else 2L
    toc <- c(toc, list(.toc_entry(spec$label[i], file = spec$file[i],
                                  level = lvl)))
  }
  toc
}

# A table of contents given as a table: the columns it needs, its rows in
# `order` when it has one, and its files present.
.check_toc_table <- function(spec) {
  if (!all(c("file", "label") %in% names(spec))) {
    stop("A table of contents needs the columns `file` and `label` ",
         "(and optionally `heading`, `level`, `order`; see assemble_folder()).",
         call. = FALSE)
  }
  if (!is.null(spec$order)) spec <- spec[order(spec$order), , drop = FALSE]
  spec$file  <- as.character(spec$file)
  spec$label <- as.character(spec$label)
  if (!is.null(spec$heading)) spec$heading <- as.character(spec$heading)
  missing <- !file.exists(spec$file)
  if (any(missing)) {
    stop("The table of contents names missing file(s): ",
         paste(spec$file[missing], collapse = ", "), call. = FALSE)
  }
  spec
}


#' Build a TOC definition from a set of RTF files (deprecated)
#'
#' **Deprecated** in 0.8.x (warns once a session, still works); removed in
#' 0.9.0.  Give [assemble_rtf()] the table of contents itself
#' (`toc = ` the table from [assemble_folder()]).
#'
#' Convenience wrapper that reads the files (via [assemble_spec()]) and returns
#' the `toc =` list of [toc_heading()] / [toc_entry()] objects ready for
#' [assemble_rtf()].  Pass a ready-made `spec` to convert that instead.
#'
#' @param files Vector of `.rtf` paths.
#' @param spec Optional assembly spec (from [assemble_spec()]); when supplied,
#'   `files` is ignored and the spec is converted directly.
#' @param ... Passed to [assemble_spec()] when building from `files`.
#'
#' @return A list suitable for `assemble_rtf(toc = )`.
#' @seealso [assemble_spec()], [assemble_from_spec()].
#'
#' @examples
#' # two TFL files in a folder, as a study's output
#' dir <- file.path(tempdir(), "tfl")
#' dir.create(dir, showWarnings = FALSE)
#' for (t in c("14.1.1", "14.2.1")) {
#'   doc <- rtf_document() |>
#'     rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
#'     rtf_titles(list(c(paste("Table", t), "Safety Population")))
#'   generate_rtfreport(doc, file.path(dir, paste0("t", gsub(".", "_", t,
#'     fixed = TRUE), ".rtf")), overwrite = TRUE)
#' }
#' # instead: the table of contents as the `toc`
#' assemble_rtf(toc = assemble_folder(dir), output_file = tempfile(fileext = ".rtf"))
#' @export
assemble_toc <- function(files = NULL, spec = NULL, ...) {
  .deprecate_once(
    "assemble_toc",
    paste0("`assemble_toc()` is deprecated: give `assemble_rtf(toc = )` the ",
           "table of contents (from `assemble_folder()`).\n  ",
           "Removed in 0.9.0."))
  if (is.null(spec)) {
    if (is.null(files)) stop("Supply `files` or `spec`.", call. = FALSE)
    spec <- .assemble_spec(files = files, ...)
  }
  .spec_to_toc(spec)
}


# -- Spec file I/O (.xlsx via writexl/readxl, or .csv) -----------------------

.write_spec <- function(spec, path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xlsx") {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Writing an .xlsx spec needs the 'writexl' package; ",
           "install it or use a .csv path.", call. = FALSE)
    }
    writexl::write_xlsx(spec, path)
  } else if (ext == "csv") {
    utils::write.csv(spec, path, row.names = FALSE)
  } else {
    stop("`spec_file` must end in .xlsx or .csv.", call. = FALSE)
  }
  invisible(path)
}

.read_spec <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xlsx") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Reading an .xlsx spec needs the 'readxl' package; ",
           "install it or use a .csv path.", call. = FALSE)
    }
    as.data.frame(readxl::read_xlsx(path), stringsAsFactors = FALSE)
  } else if (ext == "csv") {
    utils::read.csv(path, stringsAsFactors = FALSE)
  } else {
    stop("Spec file must end in .xlsx or .csv.", call. = FALSE)
  }
}


#' Assemble RTF files from an assembly spec (deprecated)
#'
#' **Deprecated** in 0.8.x (warns once a session, still works); removed in
#' 0.9.0.  It is `assemble_rtf(toc = spec)`: [assemble_rtf()] takes the table (or
#' its .xlsx / .csv path) as `toc`.
#'
#' Runs [assemble_rtf()] with a Table of Contents built from an assembly spec
#' (a `data.frame` from [assemble_spec()], or the path to a saved `.xlsx` /
#' `.csv` spec).  Rows are ordered by the spec's `order` column when present.
#'
#' @param spec An assembly-spec `data.frame`, or a path to a `.xlsx` / `.csv`
#'   spec file.
#' @param output_file Path of the assembled `.rtf` to write.
#' @param toc_title,toc_leader,toc_page_numbering,overwrite,... Passed to
#'   [assemble_rtf()].
#'
#' @return Invisibly, `output_file`.
#' @seealso [assemble_spec()], [assemble_folder()], [assemble_rtf()].
#'
#' @examples
#' # two TFL files in a folder, as a study's output
#' dir <- file.path(tempdir(), "tfl")
#' dir.create(dir, showWarnings = FALSE)
#' for (t in c("14.1.1", "14.2.1")) {
#'   doc <- rtf_document() |>
#'     rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
#'     rtf_titles(list(c(paste("Table", t), "Safety Population")))
#'   generate_rtfreport(doc, file.path(dir, paste0("t", gsub(".", "_", t,
#'     fixed = TRUE), ".rtf")), overwrite = TRUE)
#' }
#' # instead
#' spec <- assemble_folder(dir)          # review / edit the order
#' assemble_rtf(toc = spec, output_file = tempfile(fileext = ".rtf"),
#'              toc_page_numbering = "decimal")
#' @export
assemble_from_spec <- function(spec, output_file,
                               toc_title = "Table of Contents",
                               toc_leader = "dot",
                               toc_page_numbering = "decimal",
                               overwrite = FALSE, ...) {
  .deprecate_once(
    "assemble_from_spec",
    paste0("`assemble_from_spec()` is deprecated: it is ",
           "`assemble_rtf(toc = spec)` (a table or its .xlsx / .csv path).\n  ",
           "Removed in 0.9.0."))
  .assemble_from_spec(spec, output_file, toc_title = toc_title,
                      toc_leader = toc_leader,
                      toc_page_numbering = toc_page_numbering,
                      overwrite = overwrite, ...)
}

.assemble_from_spec <- function(spec, output_file,
                                toc_title = "Table of Contents",
                                toc_leader = "dot",
                                toc_page_numbering = "decimal",
                                overwrite = FALSE, ...) {
  if (is.character(spec) && length(spec) == 1L) spec <- .read_spec(spec)
  if (!is.data.frame(spec) || !all(c("file", "label") %in% names(spec))) {
    stop("`spec` must be an assembly-spec data.frame (see assemble_spec()) ",
         "or a path to one.", call. = FALSE)
  }
  if (!is.null(spec$order)) spec <- spec[order(spec$order), , drop = FALSE]
  missing <- !file.exists(spec$file)
  if (any(missing)) {
    stop("Spec references missing file(s): ",
         paste(spec$file[missing], collapse = ", "), call. = FALSE)
  }
  toc <- .spec_to_toc(spec)
  assemble_rtf(spec$file, output_file, overwrite = overwrite, toc = toc,
               toc_title = toc_title, toc_leader = toc_leader,
               toc_page_numbering = toc_page_numbering, ...)
}


#' Assemble every RTF in a folder into one TOC deliverable
#'
#' Scans `dir` for `.rtf` files (natural-sorted, so `t2` comes before `t10`)
#' and reads each file's table number and title from its running header into
#' a **table of contents**: a `data.frame` with one row per file.  With an
#' `output_file` it assembles the deliverable with that table of contents
#' ([assemble_rtf()]); without one it only returns the table, to edit
#' (rename labels, fill `heading` to group entries, change `level`, reorder
#' or drop rows) and hand to `assemble_rtf(toc = )`.
#'
#' The table has the columns `order` (the assembly order), `file`, `table`
#' (the table number read from the header, or `NA`), `heading` (a heading
#' printed above the entry whenever it changes; `NA` = none), `label` (the
#' entry text, `"Table N  <title>"`), `level` (the entry's indent, default 2)
#' and `pages` (informational).
#'
#' @param dir Directory of `.rtf` files to assemble.
#' @param output_file Path of the assembled `.rtf` to write, or `NULL`
#'   (default): return the table of contents only.
#' @param spec_file Optional path (`.xlsx` or `.csv`).  When given, the
#'   generated spec is **saved** there (so you can inspect / edit it); when
#'   `NULL` (default) the spec is kept in memory only.
#' @param recursive Recurse into sub-directories when scanning?  Default
#'   `FALSE`.
#' @param toc_title,toc_leader,toc_page_numbering,overwrite,... Passed through
#'   to [assemble_rtf()].
#'
#' @return Without `output_file`, the table of contents.  With one,
#'   invisibly, a list with `output` (the assembled file) and `spec` (the
#'   table of contents used).
#' @seealso [assemble_rtf()].
#'
#' @examples
#' # two TFL files in a folder, as a study's output
#' dir <- file.path(tempdir(), "tfl")
#' dir.create(dir, showWarnings = FALSE)
#' for (t in c("14.1.1", "14.2.1")) {
#'   doc <- rtf_document() |>
#'     rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
#'     rtf_titles(list(c(paste("Table", t), "Safety Population")))
#'   generate_rtfreport(doc, file.path(dir, paste0("t", gsub(".", "_", t,
#'     fixed = TRUE), ".rtf")), overwrite = TRUE)
#' }
#' # One call: scan a folder of TFL .rtf files and assemble them, in catalog
#' # order, into a single deliverable with an auto table of contents.
#' assemble_folder(dir, tempfile(fileext = ".rtf"), toc_title = "Contents")
#'
#' # Or look at the table of contents first, edit it, then assemble
#' spec <- assemble_folder(dir)
#' spec$heading <- "Demographics and safety"
#' assemble_rtf(toc = spec, output_file = tempfile(fileext = ".rtf"))
#' @export
assemble_folder <- function(dir, output_file = NULL, spec_file = NULL,
                            recursive = FALSE,
                            toc_title = "Table of Contents",
                            toc_leader = "dot",
                            toc_page_numbering = "decimal",
                            overwrite = FALSE, ...) {
  spec <- .assemble_spec(dir = dir, recursive = recursive)
  if (!is.null(spec_file)) .write_spec(spec, spec_file)
  if (is.null(output_file)) return(spec)
  .assemble_from_spec(spec, output_file, toc_title = toc_title,
                     toc_leader = toc_leader,
                     toc_page_numbering = toc_page_numbering,
                     overwrite = overwrite, ...)
  invisible(list(output = output_file, spec = spec))
}
