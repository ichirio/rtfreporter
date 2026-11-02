# ============================================================================
#  Helpers for assemble_rtf(): collect files, read titles, build a TOC /
#  "assembly spec", and run the assembly from that spec.
#
#  The two public entry points:
#    assemble_folder()  folder (+ output)  -> the table of contents (a
#                                             data.frame, "spec"), and the
#                                             assembled file when `output_file`
#    assemble_rtf()     files and / or a table of contents -> assembled file
#  (assemble_files() / assemble_spec() / assemble_toc() / assemble_from_spec(),
#  the 0.8.x wrappers over the internal steps below, were removed in 0.9.0.)
# ============================================================================


# -- RTF title extraction ----------------------------------------------------

# Decode a short run of RTF text into plain text: each `\uN?` becomes its
# character (a non-breaking space, U+00A0, becomes a normal space), and the
# escaped braces / backslash become literals.
.rtf_decode_text <- function(x) {
  x <- .rtf_unicode_decode(x)
  x <- gsub(intToUtf8(160L), " ", x, fixed = TRUE)
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

# The centred bold title lines of the title block in the body (what
# rtf_titles() writes): the run of consecutive centred bold lines that
# contains the first "Table N" line, within the first 200 lines and before
# any picture.  Returns character(0) when no such line is found.
.rtf_body_title_lines <- function(lines) {
  n_scan <- min(length(lines), 200L)
  pat <- "\\\\qc\\\\li[0-9]+\\\\ri[0-9]+\\s+\\\\b\\s+(.+?)\\\\b0"
  texts <- rep(NA_character_, n_scan)
  for (i in seq_len(n_scan)) {
    if (grepl("\\\\pict", lines[i])) {
      n_scan <- i - 1L
      break
    }
    g <- regmatches(lines[i], regexec(pat, lines[i]))[[1L]]
    if (length(g) == 2L) texts[i] <- .rtf_decode_text(g[2L])
  }
  texts <- texts[seq_len(n_scan)]
  is_tab <- !is.na(texts) & grepl("^Table\\s+[0-9]", texts)
  if (!any(is_tab)) return(character(0))
  first <- which(is_tab)[1L]
  # Widen to the consecutive centred bold lines around the "Table N" line.
  lo <- first
  while (lo > 1L && !is.na(texts[lo - 1L])) lo <- lo - 1L
  hi <- first
  while (hi < n_scan && !is.na(texts[hi + 1L])) hi <- hi + 1L
  texts[lo:hi]
}

# Table number and title from a set of centred title cells: the first
# "Table N" cell gives the number; the title is the longest cell that is
# neither a "Table N" line nor a "<...>" subtitle, else the text after the
# number on the "Table N" line.
.rtf_cells_table_title <- function(cells) {
  tabnum <- NA_character_
  title_inline <- ""
  for (c in cells) {
    g <- regmatches(c, regexec("^Table\\s+([0-9][0-9.A-Za-z-]*)\\b\\s*(.*)$", c))[[1L]]
    if (length(g) == 3L && nzchar(g[2L])) {
      tabnum <- g[2L]
      title_inline <- trimws(g[3L])
      break
    }
  }
  cand <- cells[!grepl("^Table\\s", cells) & !grepl("^<.*>$", cells) &
                  nchar(cells) > 0L]
  title <- if (length(cand)) cand[which.max(nchar(cand))] else NA_character_
  if (!is.na(tabnum) && nzchar(title_inline) && is.na(title)) {
    title <- title_inline
  }
  list(table = tabnum, title = title)
}

# Read one RTF file's table number / title / TOC label from its running
# header.  When the header has no "Table N" line, the title block in the body
# (rtf_titles()) is read instead (#843).  Falls back to the file name when
# neither has one.
.rtf_table_label <- function(file) {
  lines  <- readLines(file, warn = FALSE)
  txt    <- paste(lines, collapse = " ")
  cells  <- .rtf_centered_cells(.rtf_header_block(txt))
  tt     <- .rtf_cells_table_title(cells)
  if (is.na(tt$table)) {
    body <- .rtf_cells_table_title(.rtf_body_title_lines(lines))
    if (!is.na(body$table)) tt <- body
  }
  tabnum <- tt$table
  title  <- tt$title

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
# Natural sort key: every run of digits zero-padded to 15 places, so "t2"
# sorts before "t10".
.natural_key <- function(x) {
  vapply(x, function(s) {
    parts <- regmatches(s, gregexpr("[0-9]+|[^0-9]+", s))[[1L]]
    paste0(vapply(parts, function(p)
      if (grepl("^[0-9]+$", p)) formatC(as.numeric(p), width = 15, flag = "0",
                                        format = "d")
      else p, character(1L)), collapse = "")
  }, character(1L), USE.NAMES = FALSE)
}

# Natural order compared byte by byte (the C locale) with method = "radix",
# so the order is the same in every locale and on every OS (#827).
.natural_order <- function(x) {
  order(.natural_key(x), method = "radix")
}


.assemble_files <- function(dir, pattern = "[.]rtf$", recursive = FALSE,
                            sort = TRUE) {
  if (!dir.exists(dir)) stop("Directory not found: ", dir, call. = FALSE)
  files <- list.files(dir, pattern = pattern, full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
  if (sort && length(files)) files <- files[.natural_order(basename(files))]
  files
}


.assemble_spec <- function(dir = NULL, files = NULL, recursive = FALSE) {
  if (is.null(files)) {
    if (is.null(dir)) stop("Supply `dir` or `files`.", call. = FALSE)
    files <- .assemble_files(dir, recursive = recursive)
  }
  if (length(files) == 0L) stop("No RTF files found.", call. = FALSE)
  info <- lapply(files, .rtf_table_label)
  tabs <- vapply(info, function(x) x$table %||% NA_character_, character(1L))
  # Default order: by table number (so 14.1.x precedes 14.3.x), then the
  # files without a detected number in natural file-name order.  Radix order
  # compares bytes, so it does not depend on the locale (#827).
  key  <- .natural_key(ifelse(is.na(tabs), basename(files), tabs))
  ord  <- order(is.na(tabs), key, method = "radix")
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


# Internal: assembly spec (data.frame) -> `toc =` list of .toc_heading() /
# .toc_entry().  A new heading is emitted whenever the `heading` value changes.
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
    # (as.numeric(), not as.integer(): a level of 2.5 is refused by
    # .toc_entry(), not cut to 2)
    lvl <- if (!is.null(spec$level) && !is.na(spec$level[i]))
      as.numeric(spec$level[i]) else 2L
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


.assemble_from_spec <- function(spec, output_file,
                                toc_title = "Table of Contents",
                                toc_leader = "dot",
                                toc_page_numbering = "decimal",
                                overwrite = FALSE, ...) {
  if (is.character(spec) && length(spec) == 1L) spec <- .read_spec(spec)
  if (!is.data.frame(spec) || !all(c("file", "label") %in% names(spec))) {
    stop("`spec` must be an assembly-spec data.frame (see assemble_folder()) ",
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
#' and reads each file's table number and title into a **table of
#' contents**: a `data.frame` with one row per file.  The number and title
#' come from the running header (`rtf_header()`) when it has a `"Table N"`
#' line, otherwise from the title block that [rtf_titles()] writes at the top
#' of the body.  With an
#' `output_file` it assembles the deliverable with that table of contents
#' ([assemble_rtf()]); without one it only returns the table, to edit
#' (rename labels, fill `heading` to group entries, change `level`, reorder
#' or drop rows) and hand to `assemble_rtf(toc = )`.
#'
#' The rows come in this order, the same in every locale and on every OS:
#' first the files with a `"Table N"` line (in the header or the titles), by
#' that number in
#' natural order (`14.1.2` before `14.1.10`, `14.1.x` before `14.3.x`,
#' whatever the file names); then the files without one -- listings,
#' figures, any file with no `"Table N"` line -- by file name in
#' natural order.  Names are compared character by character as in the C
#' locale, so upper case comes before lower case (`B` before `a`) and `-`
#' before `_`.  Files with the same table number keep their file-name order.
#'
#' The table has the columns `order` (the assembly order), `file`, `table`
#' (the table number read from the header or the titles, or `NA`), `heading`
#' (a heading
#' printed above the entry whenever it changes; `NA` = none), `label` (the
#' entry text, `"Table N  <title>"`), `level` (the entry's indent, a whole
#' number from 1 to 3, default 2) and `pages` (informational).
#'
#' @param dir Directory of `.rtf` files to assemble.
#' @param output_file Path of the assembled `.rtf` to write, or `NULL`
#'   (default): return the table of contents only.
#' @param spec_file Optional path (`.xlsx` or `.csv`).  When given, the
#'   generated spec is **saved** there (so you can inspect / edit it); when
#'   `NULL` (default) the spec is kept in memory only.  It is written, never
#'   read back: assemble an edited one with `assemble_rtf(toc = spec_file)`.
#' @param recursive Recurse into sub-directories when scanning?  Default
#'   `FALSE`.
#' @param toc_title,toc_leader,toc_page_numbering,overwrite,... Passed through
#'   to [assemble_rtf()]; `toc_page_numbering` defaults to `"decimal"` here
#'   (`"none"` in [assemble_rtf()]).
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
