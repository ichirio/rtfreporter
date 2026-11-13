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

# Decode a short run of RTF text (one title row, one header cell) into plain
# text: each `\uN?` becomes its character (a non-breaking space, U+00A0,
# becomes a normal space), and the escaped braces / backslash become
# literals.  A control word is not text: a line break inside the row
# (`\line`, or `\par`) joins the text before and after it with two spaces, as
# title rows are joined; `\tab` is a space; any other control word (`\i`,
# `\ul`, `\cf3`, ...) is left out (#1021).
.rtf_decode_text <- function(x) {
  brk <- "\u0005"   # a line break inside the row
  bsl <- "\u0007"   # an escaped backslash, kept apart from the control words
  if (!length(x) || is.na(x)) return(x)
  x <- .rtf_label_text(x)
  x <- gsub("\\\\", bsl, x, fixed = TRUE)
  x <- gsub("\\\\(?:line|par)(?![A-Za-z]) ?", brk, x, perl = TRUE)
  x <- gsub("\\\\tab(?![A-Za-z]) ?", " ", x, perl = TRUE)
  x <- .rtf_unicode_decode(x)
  x <- gsub("\\\\[A-Za-z]+-?[0-9]* ?", "", x)
  x <- gsub("\\~", " ", x, fixed = TRUE)
  x <- gsub(intToUtf8(160L), " ", x, fixed = TRUE)
  x <- gsub("\\{",  "{",  x, fixed = TRUE)
  x <- gsub("\\}",  "}",  x, fixed = TRUE)
  x <- gsub(bsl, "\\", x, fixed = TRUE)
  parts <- trimws(strsplit(x, brk, fixed = TRUE)[[1L]])
  paste(parts[nzchar(parts)], collapse = "  ")
}

# The title of a file as the file shows it, the one reader of a contents
# entry's label (assemble_rtf(toc = "auto"), a table toc row with no label,
# assemble_folder()'s label): the title rows rtf_titles() writes at the top
# of the body -- the run of centred bold rows that begins at the first one,
# within the first 200 lines and before any picture, a blank row inside the
# run passed over -- each read by .rtf_decode_text() and joined with two
# spaces ("Table 1  Demographics").  NA when the file has no such row.
.rtf_title_text <- function(lines) {
  n_scan <- min(length(lines), 200L)
  pat   <- "\\\\qc\\\\li[0-9]+\\\\ri[0-9]+\\s+\\\\b\\s+(.+?)\\\\b0"
  blank <- "\\\\q[lcr]\\\\li[0-9]+\\\\ri[0-9]+(?:\\\\[a-z]+-?[0-9]*)*\\s*\\\\(?:par|cell)"
  rows <- character(0)
  gap  <- FALSE
  for (i in seq_len(n_scan)) {
    line <- lines[i]
    if (grepl("\\\\pict", line)) break
    g <- regmatches(line, regexec(pat, line))[[1L]]
    if (length(g) == 2L && nzchar(trimws(g[2L]))) {
      rows <- c(rows, .rtf_decode_text(g[2L]))
      gap  <- FALSE
    } else if (length(rows) && !gap && grepl(blank, line, perl = TRUE)) {
      gap <- TRUE            # a blank title row: the run may go on after it
    } else if (length(rows)) {
      break
    }
  }
  rows <- rows[nzchar(rows)]
  if (!length(rows)) return(NA_character_)
  paste(rows, collapse = "  ")
}

# The label a file's contents entry gets when none is given, the same for
# toc = "auto", a table toc row with no label and assemble_folder(): its
# title (.rtf_title_text()); a file with no title rows, its running header's
# "Table N" line (.rtf_table_label()); else the file's name without ".rtf".
.toc_label_from_title <- function(file) {
  .rtf_table_label(file)$label
}

# A title's RTF as the text of a label (a contents entry, its outline entry,
# the folder table's label): what a page token wrote there is left out -- a
# page number (\chpgn), a NUMPAGES field, the {BOOK_PAGE} slot -- since a
# label is not on that page, and a markup group (`{\sub 2}`, `{\super a}`)
# keeps its text without its formatting.  The escaped braces of the text
# (`\{`) are not groups and stay.
.rtf_label_text <- function(x) {
  if (!length(x) || is.na(x)) return(x)
  gone <- "\u0006"
  x <- gsub("(?<!\\\\)\\{\\\\\\*\\\\[A-Za-z]+\\}", gone, x, perl = TRUE)
  x <- gsub(paste0("(?<!\\\\)\\{\\\\field\\{\\\\\\*\\\\fldinst [^{}]*\\}",
                   "\\{\\\\fldrslt [^{}]*\\}\\}"), gone, x, perl = TRUE)
  x <- gsub("\\\\chpgn(?![A-Za-z]) ?", gone, x, perl = TRUE)
  repeat {
    y <- gsub("(?<!\\\\)\\{\\\\[A-Za-z]+-?[0-9]* ?((?:[^{}\\\\]|\\\\[^{}A-Za-z]|\\\\u-?[0-9]+\\?)*)(?<!\\\\)\\}",
              "\\1", x, perl = TRUE)
    if (identical(y, x)) break
    x <- y
  }
  x <- gsub(paste0(" *", gone, "[", gone, " ]*"), " ", x)
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
# "Table N" cell gives the number; the title is the text after the number on
# that line when there is any, whatever other centred cells follow (a
# population line, "Draft"); else the longest cell that is neither a
# "Table N" line nor a "<...>" subtitle (#1046).
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
  if (!is.na(tabnum) && nzchar(title_inline)) {
    return(list(table = tabnum, title = title_inline))
  }
  cand <- cells[!grepl("^Table\\s", cells) & !grepl("^<.*>$", cells) &
                  nchar(cells) > 0L]
  title <- if (length(cand)) cand[which.max(nchar(cand))] else NA_character_
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

  # The label: the file's title rows, joined (.rtf_title_text(), as
  # toc = "auto" reads them); without title rows, the running header's
  # "Table N" line ("Table N  <title>"); else the file name (#1030).
  base  <- sub("\\.rtf$", "", basename(file), ignore.case = TRUE)
  label <- .rtf_title_text(lines)
  if (is.na(label) || !nzchar(label)) {
    label <- if (!is.na(tabnum)) {
      trimws(paste0("Table ", tabnum, if (!is.na(title)) paste0("  ", title)))
    } else {
      base
    }
  }
  list(file = file, table = tabnum, title = title, label = label)
}


# -- Natural sort ------------------------------------------------------------

# Order strings so embedded numbers sort numerically ("t2" < "t10").
# Natural sort key: every run of digits, without its leading zeros, after its
# length, so "t2" sorts before "t10".  As text: a long run, a date and time in
# a file name, is too large for an integer.
.natural_key <- function(x) {
  vapply(x, function(s) {
    parts <- regmatches(s, gregexpr("[0-9]+|[^0-9]+", s))[[1L]]
    paste0(vapply(parts, function(p)
      if (grepl("^[0-9]+$", p)) {
        p <- sub("^0+(?=[0-9])", "", p, perl = TRUE)
        paste0(formatC(nchar(p), width = 5, flag = "0"), p)
      } else p, character(1L)), collapse = "")
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
    # (no label, or an NA one: the file's own title, as toc = "auto" reads it)
    lab <- spec$label[i]
    if (is.na(lab)) lab <- .toc_label_from_title(spec$file[i])
    toc <- c(toc, list(.toc_entry(lab, file = spec$file[i], level = lvl)))
  }
  toc
}

# A table of contents given as a table: the columns it needs, its rows in
# `order` when it has one, and its files present.
.check_toc_table <- function(spec) {
  if (!"file" %in% names(spec)) {
    stop("A table of contents needs the column `file` (and optionally ",
         "`label`, `heading`, `level`, `order`; see assemble_folder()).",
         call. = FALSE)
  }
  if (!is.null(spec$order)) spec <- spec[order(spec$order), , drop = FALSE]
  spec$file  <- as.character(spec$file)
  # (no `label` column: every label is read from its file's title)
  spec$label <- if (is.null(spec$label)) rep(NA_character_, nrow(spec)) else
    as.character(spec$label)
  if (!is.null(spec$heading)) spec$heading <- as.character(spec$heading)
  missing <- !file.exists(spec$file)
  if (any(missing)) {
    stop("The table of contents names missing file(s): ",
         paste(spec$file[missing], collapse = ", "), call. = FALSE)
  }
  spec
}



# -- Spec file I/O (.xlsx via writexl/readxl, or .csv) -----------------------

# The scalar arguments of assemble_folder() (#862), before any file is read:
# a base-R message ("invalid filename argument", "NA in coercion to
# boolean") does not say which argument is wrong.
.check_folder_args <- function(dir, spec_file, recursive) {
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    stop("`dir` must be a single folder path.", call. = FALSE)
  }
  if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
    stop("`recursive` must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(spec_file)) return(invisible())
  if (length(spec_file) != 1L) {
    stop("`spec_file` must be NULL or a single path ending in .xlsx or .csv.",
         call. = FALSE)
  }
  if (!is.character(spec_file) || is.na(spec_file) ||
      !tolower(tools::file_ext(spec_file)) %in% c("xlsx", "csv")) {
    stop("`spec_file` must end in .xlsx or .csv.", call. = FALSE)
  }
  if (!dir.exists(dirname(spec_file))) {
    stop(sprintf("The folder of `spec_file` does not exist: %s",
                 dirname(spec_file)), call. = FALSE)
  }
  invisible()
}

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
                                overwrite = FALSE, ...) {
  if (is.character(spec) && length(spec) == 1L) spec <- .read_spec(spec)
  if (!is.data.frame(spec) || !"file" %in% names(spec)) {
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
               toc_title = toc_title, toc_leader = toc_leader, ...)
}


#' Assemble every RTF in a folder into one TOC deliverable
#'
#' Scans `dir` for `.rtf` files (natural-sorted, so `t2` comes before `t10`)
#' and reads each file's table number and title into a **table of
#' contents**: a `data.frame` with one row per file.  The number comes from
#' the running header (`rtf_header()`) when it has a `"Table N"` line,
#' otherwise from the title block that [rtf_titles()] writes at the top of
#' the body; the label is the file's title, read as
#' `assemble_rtf(toc = "auto")` reads it.  With an
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
#' entry text: the file's title, read as `assemble_rtf(toc = "auto")` reads
#' it -- its title rows joined with two spaces, a line break inside a row
#' too, as `"Table 14.1.1  Safety Population"`; a figure or a listing is
#' labelled by its title as a table is; a file with no title rows takes its
#' running header's `"Table N"` line, the title written after the number
#' winning over the centred lines below it, and a file with neither, its file
#' name without `.rtf`; one row a file, so a file whose titles change from
#' page to page with `page_by` (`"Summary of {PARAM}"`) is labelled by its
#' first page's title), `level` (the entry's indent, a whole
#' number from 1 to 3, default 2) and `pages` (informational).
#'
#' @param dir Directory of `.rtf` files to assemble: one folder path.  A
#'   value that is not one non-empty string is an error, as is a folder that
#'   does not exist or holds no `.rtf` file.
#' @param output_file Path of the assembled `.rtf` to write, or `NULL`
#'   (default): return the table of contents only.  Assembling needs two or
#'   more files, so with an `output_file` a folder of one `.rtf` file is an
#'   error (without one, its one-row table is returned).
#' @param spec_file Optional path (`.xlsx` or `.csv`).  When given, the
#'   generated spec is **saved** there (so you can inspect / edit it); when
#'   `NULL` (default) the spec is kept in memory only.  It is written, never
#'   read back: assemble an edited one with `assemble_rtf(toc = spec_file)`.
#'   One path in a folder that exists; another value is an error, checked
#'   before any file is read or written.
#' @param recursive Recurse into sub-directories when scanning?  `TRUE` or
#'   `FALSE` (default); another value is an error.
#' @param toc_title,toc_leader,overwrite,... Passed through to
#'   [assemble_rtf()] (`cover`, `book_page`, `toc_style` ... go in `...`).
#'   The assembled file's pages are numbered as [assemble_rtf()] numbers them
#'   (its **Page numbering**): one Arabic sequence from the first page, a
#'   cover and the table of contents included, and the total is all pages.
#'   `toc_page_numbering`, which was removed, is an error, with or without
#'   an `output_file`.
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
                            overwrite = FALSE, ...) {
  .check_assemble_dots(intersect(...names(), "toc_page_numbering"),
                       "assemble_folder")
  .check_folder_args(dir, spec_file, recursive)
  spec <- .assemble_spec(dir = dir, recursive = recursive)
  # one file is a table of contents, not a book (#862): stop before the
  # spec file is written
  if (!is.null(output_file) && nrow(spec) < 2L) {
    stop(sprintf("`dir` holds one .rtf file (%s); assembling needs two or more.",
                 basename(spec$file)), call. = FALSE)
  }
  if (!is.null(spec_file)) .write_spec(spec, spec_file)
  if (is.null(output_file)) return(spec)
  .assemble_from_spec(spec, output_file, toc_title = toc_title,
                     toc_leader = toc_leader,
                     overwrite = overwrite, ...)
  invisible(list(output = output_file, spec = spec))
}
