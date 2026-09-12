# assemble_rtf: merge multiple rtfreporter-generated RTF files into one.
#
# Strategy:
#   * File 1   -> keep all lines except the closing "}"
#   * File N>1 -> extract from the first \sectd onward, drop "}"
#   * Files are joined with a \sect separator (RTF section break)
#   * A single "}" closes the combined document
#
# Optional, layered on top in v0.0.29 / v0.0.30:
#   * `cover  = list(...)`                cover page section at the very front
#   * `toc    = c(...) / "auto" / list()` clickable Table of Contents with
#                                          per-source-file bookmarks
#   * `toc_page_numbering = "roman"`      i, ii, iii on TOC pages; body
#                                          restarts at 1
# ============================================================================


# ── RTF post-processing helpers ────────────────────────────────────────────

# Strip the closing RTF "}" from a line vector.
.rtf_drop_close <- function(lines) {
  n <- length(lines)
  while (n > 0L && !nzchar(trimws(lines[n]))) n <- n - 1L
  if (n > 0L && trimws(lines[n]) == "}") lines[-n] else lines
}

# Extract lines starting from the first \sectd (inclusive), closing "}" removed.
.rtf_extract_section_content <- function(lines) {
  idx <- which(trimws(lines) == "\\sectd")
  if (length(idx) == 0L) {
    stop("No \\sectd found in RTF file. Only rtfreporter-generated files are supported.",
         call. = FALSE)
  }
  .rtf_drop_close(lines[idx[1L]:length(lines)])
}

# Sanitise a string into a valid RTF bookmark name (letters / digits /
# underscores; <= 32 chars; must start with a letter).
.sanitize_bookmark <- function(x) {
  s <- sub("\\.rtf$", "", basename(as.character(x)), ignore.case = TRUE)
  s <- gsub("[^A-Za-z0-9_]", "_", s)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  s <- substr(s, 1L, 32L)
  ifelse(grepl("^[A-Za-z]", s) & nzchar(s), s, paste0("bk_", s))
}

# Inject `{\*\bkmkstart NAME}{\*\bkmkend NAME}` plus, optionally, an
# `\outlinelevel<N>` paragraph carrying `outline_label` so PDF
# converters (LibreOffice, Word) expose the section in the PDF outline
# / bookmark panel — the eCTD-recommended navigation aid.
#
# INSERTION POINT (v0.0.37 -- bug fix):
#   The inserts are placed *after* the new section's preamble
#   (`\sectd`, `\sbkpage...`, `{\header...}`, `{\footer...}`) and
#   *before* the first body paragraph.  Inserting between `\sectd`
#   and the section properties (the v0.0.31--36 layout) put a
#   paragraph in a half-configured section, causing the previous
#   section's lingering paragraph state (e.g. a footnote's `\brdrt`
#   border) to bleed under the new section's header on the first
#   page of the new section -- a "phantom rule" right below the
#   header band.
#
# INVISIBILITY (matches ydisctools / Yenu's r2rtf TOC strategy):
#   \cf2          -> white text colour (rtfreporter reserves
#                    colour-table index 2 for white; see
#                    .build_color_table_rtf()).  White-on-white renders
#                    zero visible pixels regardless of font size.
#   \fs2          -> 1-pt font, in case a converter ignores `\sl...`
#                    and falls back to default size.
#   \sl1\slmult0  -> EXACT line spacing of 1 twip (1/1440 inch ≈ 0 px).
#                    Forces the paragraph's rendered height to ~0
#                    regardless of font size, so the outline label
#                    cannot push a borderline-fitting table from one
#                    page to two.
#   \sa0\sb0      -> no surrounding spacing.
#
# GROUP WRAPPING:
#   The outer `{ ... }` scopes `\plain\cf2\fs2\sl1` so the format
#   state is LOCAL to the group and cannot bleed into the following
#   content (v0.0.33 fix).
.insert_bookmark <- function(content, bookmark_name,
                              outline_label = NULL, outline_level = 0L) {
  sectd_idx <- which(trimws(content) == "\\sectd")[1L]
  if (is.na(sectd_idx)) return(content)

  # Skip past the section preamble.  rtfreporter emits, in order:
  #   \sectd
  #   \sbkpage\lndscpsxn\pgwsxn...   (page geometry)
  #   {\header ...}                  (optional)
  #   {\footer ...}                  (optional)
  # ...and the first BODY paragraph starts at the next \pard line.
  # We insert immediately before that first body paragraph.
  insert_after <- sectd_idx
  i <- sectd_idx + 1L
  while (i <= length(content)) {
    line <- trimws(content[i])
    # Section page-property line (lone command run, not a paragraph).
    is_preamble <- grepl("^\\\\(sbkpage|pgwsxn|pghsxn|marglsxn|margrsxn|margtsxn|margbsxn|lndscpsxn|pgnrestart|pgndec|pgnlcrm)", line) ||
                   # Headers/footers are emitted as a single `{\header ...}` line.
                   grepl("^\\{\\\\header\\b",  line) ||
                   grepl("^\\{\\\\footer\\b",  line)
    if (!is_preamble) break
    insert_after <- i
    i <- i + 1L
  }

  inserts <- character(0)
  inserts <- c(inserts,
    sprintf("{\\*\\bkmkstart %s}{\\*\\bkmkend %s}",
            bookmark_name, bookmark_name))
  if (!is.null(outline_label) && nzchar(outline_label)) {
    inserts <- c(inserts,
      sprintf(paste0("{\\pard\\plain\\cf2\\fs2\\sl1\\slmult0",
                     "\\sa0\\sb0\\outlinelevel%d %s\\par}"),
              as.integer(outline_level), .toc_escape(outline_label)))
  }

  c(content[1L:insert_after], inserts,
    if (insert_after < length(content)) content[(insert_after + 1L):length(content)]
    else character(0))
}

# Count the rendered pages of an RTF file's lines.
#
# Two things start a new page, and a file mixes them:
#   \sbkpage   a section break that begins on a new page.  rtfreporter emits
#              one per `rtf_section` -- NOT one per rendered page.
#   \page      a plain page break, used between the sub-pages *inside* one
#              section (`doc_cmd$page_break`).
# So the page count is "one page for the first section, plus one for every
# further section start, plus one for every in-section break".  Counting
# `\sbkpage` alone reported 1 for any single-section file however long (#401).
#
# `\page` is matched only when the control word ends there, so `\pagebb` and
# friends cannot be mistaken for a break.  `\sbkpage` is not matched by the
# `\page` pattern either: the backslash the pattern requires is not there.
.count_rtf_pages <- function(lines) {
  txt <- paste(lines, collapse = "\n")
  .n  <- function(pattern) {
    m <- gregexpr(pattern, txt, perl = TRUE)[[1L]]
    if (length(m) == 1L && m[1L] == -1L) 0L else length(m)
  }
  sections <- .n("\\\\sbkpage")
  breaks   <- .n("\\\\page(?![a-zA-Z])")
  if (sections == 0L && breaks == 0L) return(0L)
  max(sections, 1L) + breaks
}

# Inject a `\pgnrestart\pgndec` right after the first \sectd, so the body
# pages restart at 1 when the preceding TOC used Roman numbering.
.insert_pgnrestart <- function(content) {
  sectd_idx <- which(trimws(content) == "\\sectd")[1L]
  if (is.na(sectd_idx)) return(content)
  c(content[1L:sectd_idx], "\\pgnrestart\\pgndec",
    if (sectd_idx < length(content)) content[(sectd_idx + 1L):length(content)]
    else character(0))
}

# RTF-escape plain text (no markup, no token replacement).
.toc_escape <- function(x) {
  if (length(x) == 0L || is.na(x)) return("")
  chars <- strsplit(as.character(x), "", fixed = TRUE)[[1L]]
  out <- vapply(chars, function(ch) {
    cp <- utf8ToInt(ch)
    if      (ch == "\\") return("\\\\")
    if      (ch == "{")  return("\\{")
    if      (ch == "}")  return("\\}")
    if      (cp > 127L)  return(sprintf("\\u%d?", cp))
    ch
  }, character(1L))
  paste0(out, collapse = "")
}


# ── TOC entry helpers (exported) ───────────────────────────────────────────

#' Build a structured TOC heading
#'
#' Use inside [assemble_rtf()]'s `toc =` list to insert a section
#' heading (no clickable link, no page number) above a group of
#' [toc_entry()]s.
#'
#' @param label Character; the heading text.
#' @param level Integer; 1 (default) or 2.  Controls indent depth in
#'   the rendered TOC.
#'
#' @return A list of class `"rtf_toc_heading"`.
#'
#' @examples
#' toc_heading("EFFICACY ANALYSES", level = 1)
#'
#' @export
toc_heading <- function(label, level = 1L) {
  structure(
    list(label = as.character(label)[1L], level = as.integer(level)),
    class = "rtf_toc_heading"
  )
}

#' Build a structured TOC entry
#'
#' Use inside [assemble_rtf()]'s `toc =` list to add a clickable TOC
#' entry pointing at one of the `input_files`.
#'
#' @param label Character; the entry text.
#' @param file Either a path that appears in `input_files`, an integer
#'   1-based index into `input_files`, or `NULL` (default: consume the
#'   next file in order).
#' @param level Integer; 1 to 3.  Indent depth in the rendered TOC
#'   (1 = flush left, 2 = small indent, ...).
#'
#' @return A list of class `"rtf_toc_entry"`.
#'
#' @examples
#' toc_entry("Table 14.1.1 Demographics",  file = "t14_1_1.rtf", level = 2)
#' toc_entry("Listing 16.1 Disposition")   # auto-bound to the next file
#'
#' @export
toc_entry <- function(label, file = NULL, level = 2L) {
  structure(
    list(label = as.character(label)[1L],
         file  = file,
         level = as.integer(level)),
    class = "rtf_toc_entry"
  )
}


# ── TOC normalisation ──────────────────────────────────────────────────────

# Extract the first centred-bold title from rtfreporter's title block (now a
# content-width table; see `.render_text_block_table()`).  The title is the
# first single centred-bold cell, emitted before the content; column headers
# are not bold by default, so they do not match.  Returns NA if not found
# within the initial scan window (stops at the first picture / figure).
.extract_first_title <- function(lines) {
  n_scan <- min(length(lines), 200L)
  for (i in seq_len(n_scan)) {
    line <- lines[i]
    if (grepl("\\\\pict", line, fixed = FALSE)) break
    # Title cell from .render_text_block_table():
    #   "...\qc\li0\ri0 \b TEXT\b0 \cell..."
    m <- regexec("\\\\qc\\\\li[0-9]+\\\\ri[0-9]+\\s+\\\\b\\s+(.+?)\\\\b0", line)
    g <- regmatches(line, m)[[1L]]
    if (length(g) == 2L && nzchar(trimws(g[2L]))) {
      # Reverse the RTF escape: \\ -> \, \{ -> {, \} -> }, \uN? -> char
      title <- gsub("\\\\\\\\", "\\\\", g[2L])
      title <- gsub("\\\\\\{",   "{",   title)
      title <- gsub("\\\\\\}",   "}",   title)
      # \uN? -> the character with codepoint N.  base gsub() does NOT accept a
      # function replacement, and this Windows R build has broken \\1 backrefs,
      # so substitute each distinct escape literally via regmatches().
      escs <- regmatches(title, gregexpr("\\\\u[0-9]+\\?", title))[[1L]]
      for (e in unique(escs)) {
        cp <- as.integer(regmatches(e, regexpr("[0-9]+", e)))
        if (!is.na(cp)) title <- gsub(e, intToUtf8(cp), title, fixed = TRUE)
      }
      return(trimws(title))
    }
  }
  NA_character_
}

# Normalise the user-supplied `toc` into a uniform list of:
#   list(label, level, file_idx (NA = heading), type = "heading"|"entry")
.normalize_toc <- function(toc, input_files) {
  if (is.null(toc)) return(NULL)

  # "auto" -> extract title from each input file
  if (is.character(toc) && length(toc) == 1L && toc == "auto") {
    out <- lapply(seq_along(input_files), function(i) {
      ttl <- .extract_first_title(readLines(input_files[i], warn = FALSE))
      if (is.na(ttl) || !nzchar(ttl)) {
        ttl <- sub("\\.rtf$", "", basename(input_files[i]), ignore.case = TRUE)
      }
      list(label = ttl, level = 1L, file_idx = i, type = "entry")
    })
    return(out)
  }

  # Plain character vector -> one entry per file (flat, level 1)
  if (is.character(toc)) {
    if (length(toc) != length(input_files)) {
      stop("`toc` must be a character vector the same length as `input_files`.",
           call. = FALSE)
    }
    return(lapply(seq_along(toc),
                  function(i) list(label = toc[i], level = 1L,
                                    file_idx = i, type = "entry")))
  }

  # Structured list of toc_heading() / toc_entry()
  if (is.list(toc)) {
    used      <- integer(0)
    next_free <- function() {
      candidates <- setdiff(seq_along(input_files), used)
      if (length(candidates) == 0L) NA_integer_ else candidates[1L]
    }
    out <- lapply(toc, function(e) {
      if (inherits(e, "rtf_toc_heading")) {
        return(list(label = e$label, level = e$level,
                    file_idx = NA_integer_, type = "heading"))
      }
      if (inherits(e, "rtf_toc_entry")) {
        if (is.null(e$file)) {
          fi <- next_free()
        } else if (is.numeric(e$file)) {
          fi <- as.integer(e$file)
        } else if (is.character(e$file)) {
          fi <- match(e$file, input_files)
          if (is.na(fi)) {
            stop(sprintf("toc_entry: file '%s' is not in `input_files`.",
                         e$file), call. = FALSE)
          }
        } else {
          stop("toc_entry: `file` must be a character path or integer index.",
               call. = FALSE)
        }
        if (is.na(fi) || fi < 1L || fi > length(input_files)) {
          stop("toc_entry: file index out of range.", call. = FALSE)
        }
        used <<- c(used, fi)
        return(list(label = e$label, level = e$level,
                    file_idx = fi, type = "entry"))
      }
      stop("Each `toc` element must be toc_heading() or toc_entry().",
           call. = FALSE)
    })
    return(out)
  }

  stop("`toc` must be NULL, \"auto\", a character vector, or a list of ",
       "toc_heading() / toc_entry().", call. = FALSE)
}


# ── Cover-page rendering ───────────────────────────────────────────────────

.build_cover_section <- function(cover) {
  if (is.null(cover)) return(character())

  # Each formatted paragraph is wrapped in `{ ... }` so its \fs / \b
  # only applies inside that group.  Closing `\fs0\par` (used pre-
  # v0.0.33) was leaking font-size-0 into following content.
  blocks <- c(
    "\\sectd\\sbkpage\\lndscpsxn",
    "\\pgwsxn15840\\pghsxn12240",
    "\\marglsxn864\\margrsxn864\\margtsxn1296\\margbsxn1296",
    # Push content vertically to roughly the upper third of the page
    "{\\pard\\fs18\\par}{\\pard\\fs18\\par}{\\pard\\fs18\\par}"
  )

  if (!is.null(cover$title) && nzchar(cover$title)) {
    blocks <- c(blocks,
      sprintf("{\\pard\\qc\\b\\fs44 %s\\par}",
              .toc_escape(cover$title)),
      "{\\pard\\fs18\\par}"
    )
  }
  if (!is.null(cover$subtitle) && nzchar(cover$subtitle)) {
    blocks <- c(blocks,
      sprintf("{\\pard\\qc\\fs28 %s\\par}",
              .toc_escape(cover$subtitle)),
      "{\\pard\\fs18\\par}"
    )
  }
  if (!is.null(cover$date) && nzchar(cover$date)) {
    blocks <- c(blocks,
      sprintf("{\\pard\\qc\\fs22 %s\\par}",
              .toc_escape(cover$date))
    )
  }
  if (!is.null(cover$version) && nzchar(cover$version)) {
    blocks <- c(blocks,
      sprintf("{\\pard\\qc\\fs22 %s\\par}",
              .toc_escape(cover$version))
    )
  }
  if (!is.null(cover$meta) && length(cover$meta) > 0L) {
    blocks <- c(blocks, "{\\pard\\fs18\\par}")
    for (line in cover$meta) {
      blocks <- c(blocks,
        sprintf("{\\pard\\qc\\fs20 %s\\par}",
                .toc_escape(line))
      )
    }
  }
  blocks
}


# ── TOC rendering ──────────────────────────────────────────────────────────

# Indent in twips per TOC level (1 = flush left, 2 = +600 twips, ...).
.toc_indent_for_level <- function(level) {
  600L * (max(1L, as.integer(level)) - 1L)
}

# Render the TOC section.  page_numbering = "roman" inserts \pgnlcrm
# (Roman lowercase) so TOC pages are i, ii, iii.  The caller is
# responsible for inserting \pgnrestart\pgndec on the next body section.
.build_toc_section <- function(toc_entries, bookmarks, toc_title,
                                toc_leader = c("dot", "none"),
                                page_numbering = c("none", "roman", "decimal"),
                                entry_pages = NULL) {
  toc_leader     <- match.arg(toc_leader)
  page_numbering <- match.arg(page_numbering)
  leader_cmd <- if (toc_leader == "dot") "\\tldot" else ""
  # Right tab at 14400 twips (10 inches) -- landscape letter writable area.
  tab_pos <- 14400L

  # Section preamble.  \sbkpage forces a new page at the start; landscape +
  # rtfreporter-default margins.  \pgnlcrm switches to Roman lowercase
  # numbering for the TOC pages themselves; \pgnrestart anchors at 1.
  pg_cmd <- switch(page_numbering,
    roman   = "\\pgnrestart\\pgnlcrm",
    decimal = "\\pgnrestart\\pgndec",
    none    = ""
  )
  # Every paragraph below is wrapped in `{ ... }` so character format
  # state (\fs / \b) cannot leak across paragraph boundaries into the
  # following body content.  See v0.0.33 bug fix.
  lines <- c(
    paste0("\\sectd\\sbkpage\\lndscpsxn", pg_cmd),
    "\\pgwsxn15840\\pghsxn12240",
    "\\marglsxn864\\margrsxn864\\margtsxn1296\\margbsxn1296",
    sprintf("{\\pard\\qc\\b\\fs28 %s\\par}",
            .toc_escape(toc_title)),
    "{\\pard\\fs18\\par}"
  )

  for (e in toc_entries) {
    indent     <- .toc_indent_for_level(e$level)
    indent_cmd <- if (indent > 0L) sprintf("\\li%d", indent) else ""

    if (e$type == "heading") {
      # Heading -- bold, no leader, no page number.
      lines <- c(lines,
        sprintf("{\\pard%s\\b\\fs22 %s\\par}",
                indent_cmd, .toc_escape(e$label))
      )
      next
    }

    # Entry -- HYPERLINK + dot leader + PAGEREF.  The PAGEREF field's cached
    # result is the page number we computed for this file (so it shows
    # correctly even before the viewer refreshes fields); Word/LibreOffice
    # update it to the live value on field refresh.
    bm  <- bookmarks[e$file_idx]
    txt <- .toc_escape(e$label)
    pg  <- if (!is.null(entry_pages) && e$file_idx <= length(entry_pages) &&
               !is.na(entry_pages[e$file_idx])) {
      as.integer(entry_pages[e$file_idx])
    } else 1L
    line <- sprintf(
      paste0("{\\pard%s\\fs20\\tqr%s\\tx%d ",
             "{\\field{\\*\\fldinst HYPERLINK \\\\l \"%s\"}",
             "{\\fldrslt %s}}",
             "\\tab",
             "{\\field{\\*\\fldinst PAGEREF %s \\\\h}",
             "{\\fldrslt %d}}",
             "\\par}"),
      indent_cmd, leader_cmd, tab_pos, bm, txt, bm, pg
    )
    lines <- c(lines, line)
  }
  lines
}


# ── Public entry point ────────────────────────────────────────────────────

#' Assemble multiple RTF files into one
#'
#' `assemble_rtf()` concatenates several RTF files generated by
#' [generate_rtfreport()] into a single deliverable.  Each input file
#' becomes one or more sections of the assembled output; the document
#' header (font / colour tables, page settings) is taken from the
#' **first** input.
#'
#' Page-number fields work correctly across the assembled document
#' provided the header/footer text uses dynamic tokens:
#'
#' * \code{\{AUTO_PAGE\}} -- per-page number rendered by the RTF viewer.
#' * \code{\{AUTO_TOTAL_PAGES\}} -- total page count rendered by the
#'   viewer across all assembled files.
#'
#' Static tokens (\code{\{TOTAL_PAGES\}}) reflect only the page count
#' of the individual source file and will be wrong in the assembled
#' output.
#'
#' @section Cover page:
#' Pass `cover = list(...)` to add a cover page section before the TOC.
#' Recognised fields:
#'
#' \describe{
#'   \item{`title`}{large centred bold heading (default font size 22 pt).}
#'   \item{`subtitle`}{medium centred line below the title (14 pt).}
#'   \item{`date`}{centred line (11 pt).}
#'   \item{`version`}{centred line (11 pt).}
#'   \item{`meta`}{character vector of extra centred lines (10 pt).}
#' }
#'
#' Any field that is `NULL` or empty is skipped.
#'
#' @section Table of Contents:
#' `toc` can take any of these shapes:
#'
#' \describe{
#'   \item{`NULL` (default)}{no TOC, no bookmarks.  Byte-for-byte the
#'     pre-v0.0.29 behaviour.}
#'   \item{`"auto"`}{auto-extract each input file's title (the first
#'     centred-bold cell of the title block) and use it as a level-1
#'     TOC entry.  Falls back to the file's basename if no title is
#'     detected.}
#'   \item{a character vector of TOC labels}{one per input file -- same as
#'     `"auto"` but with explicit labels.}
#'   \item{a `list(...)` of `toc_heading()` and `toc_entry()` objects}{for
#'     multi-level (chapter / table) layouts.  `toc_entry(file = ...)`
#'     selects which `input_files` element each row points to;
#'     omitting `file` consumes the next file in order.}
#' }
#'
#' Each TOC entry is an RTF `HYPERLINK` field; clicking it in
#' Word / Pages jumps to a per-source-file bookmark.  Each row also
#' has a `PAGEREF` field so the page number next to the entry is
#' calculated by the viewer when the file opens.
#'
#' @section Page numbering:
#' `toc_page_numbering` controls how the TOC pages (and cover, if
#' present) are numbered:
#'
#' \describe{
#'   \item{`"none"` (default)}{Arabic numbering flows continuously from
#'     the first page; if the source files use \code{\{AUTO_PAGE\}} the
#'     TOC counts as page 1, 2, ...}
#'   \item{`"roman"`}{TOC pages use lowercase Roman numerals (`i`, `ii`,
#'     ...); body pages restart at `1`.}
#'   \item{`"decimal"`}{TOC pages use Arabic numerals starting at `1`;
#'     body pages restart at `1`.}
#' }
#'
#' @param input_files Character vector of paths to RTF files to combine
#'   (at least 2).
#' @param output_file Path for the assembled output RTF file.
#' @param overwrite Logical; whether to overwrite an existing output
#'   file.  Default `FALSE`.
#' @param cover Optional `list(title, subtitle, date, version, meta)`
#'   for a cover page.  `NULL` (default) = no cover page.
#' @param toc Table-of-Contents specification.  `NULL` (default) =
#'   no TOC, no bookmarks; `"auto"` = auto-extract titles; a
#'   character vector = one label per input file; a `list(...)` of
#'   `toc_heading()` / `toc_entry()` = multi-level layout.
#' @param toc_title Centred bold title rendered on the TOC page.
#'   Default `"Table of Contents"`.
#' @param toc_leader `"dot"` (default) draws a dotted leader between
#'   each TOC entry and its page number; `"none"` leaves whitespace.
#' @param toc_page_numbering `"none"` (default) / `"roman"` /
#'   `"decimal"`.  See **Page numbering** above.
#' @param bookmark_prefix String prepended to every auto-generated
#'   bookmark name (default `"tfl_"`).  Helps avoid clashes if you
#'   later concatenate multiple assembled outputs.
#'
#' @return Invisibly returns `output_file`.
#'
#' @examples
#' \dontrun{
#' # Plain concatenation (legacy behaviour)
#' assemble_rtf(c("a.rtf", "b.rtf"), "out.rtf", overwrite = TRUE)
#'
#' # Auto-TOC: one entry per file, label extracted from each title
#' assemble_rtf(c("a.rtf", "b.rtf"), "out.rtf",
#'              toc       = "auto",
#'              overwrite = TRUE)
#'
#' # Multi-level TOC with section headings + cover page + Roman TOC pages
#' assemble_rtf(
#'   input_files = c("t14_1_1.rtf", "t14_2_1.rtf", "l16_1.rtf"),
#'   output_file = "tfl_package.rtf",
#'   cover = list(
#'     title    = "Study XYZ-001",
#'     subtitle = "Final Statistical Report",
#'     date     = "2026-05-28",
#'     version  = "v1.0",
#'     meta     = c("Confidential", "Prepared by ACME Pharma")
#'   ),
#'   toc = list(
#'     toc_heading("EFFICACY ANALYSES"),
#'     toc_entry("Table 14.1.1 Demographics", file = "t14_1_1.rtf"),
#'     toc_heading("SAFETY ANALYSES"),
#'     toc_entry("Table 14.2.1 Adverse Events", file = "t14_2_1.rtf"),
#'     toc_heading("LISTINGS"),
#'     toc_entry("Listing 16.1 Subject Disposition", file = "l16_1.rtf")
#'   ),
#'   toc_page_numbering = "roman",
#'   overwrite           = TRUE
#' )
#' }
#'
#' @export
assemble_rtf <- function(input_files, output_file, overwrite = FALSE,
                          cover              = NULL,
                          toc                = NULL,
                          toc_title          = "Table of Contents",
                          toc_leader         = c("dot", "none"),
                          toc_page_numbering = c("none", "roman", "decimal"),
                          bookmark_prefix    = "tfl_") {
  toc_leader         <- match.arg(toc_leader)
  toc_page_numbering <- match.arg(toc_page_numbering)

  if (!is.character(input_files) || length(input_files) < 2L) {
    stop("`input_files` must be a character vector with at least 2 elements.",
         call. = FALSE)
  }
  for (f in input_files) {
    if (!file.exists(f)) stop(sprintf("Input file not found: %s", f),
                              call. = FALSE)
  }
  if (file.exists(output_file) && !isTRUE(overwrite)) {
    stop("`output_file` already exists. Set overwrite = TRUE.", call. = FALSE)
  }

  toc_entries <- .normalize_toc(toc, input_files)
  use_toc     <- !is.null(toc_entries)
  use_cover   <- !is.null(cover)

  # Build bookmarks for files referenced by any TOC entry.
  bookmarks            <- character(length(input_files))
  file_outline_labels  <- vector("list", length(input_files))
  if (use_toc) {
    bookmarks <- paste0(bookmark_prefix, .sanitize_bookmark(input_files))
    if (anyDuplicated(bookmarks)) {
      tab <- table(bookmarks)
      dups <- names(tab)[tab > 1L]
      for (d in dups) {
        idx <- which(bookmarks == d)
        bookmarks[idx] <- paste0(d, "_", seq_along(idx))
      }
    }
    # Map each input file -> its TOC entry label (used as the PDF
    # outline-entry text).  If a file is not referenced by any
    # toc_entry(), fall back to basename(file) so the PDF outline
    # still shows a useful name.
    for (e in toc_entries) {
      if (e$type == "entry" && !is.na(e$file_idx)) {
        if (is.null(file_outline_labels[[e$file_idx]])) {
          file_outline_labels[[e$file_idx]] <- e$label
        }
      }
    }
    for (i in seq_along(file_outline_labels)) {
      if (is.null(file_outline_labels[[i]])) {
        file_outline_labels[[i]] <- sub("\\.rtf$", "",
                                        basename(input_files[i]),
                                        ignore.case = TRUE)
      }
    }
  }

  # Page on which each input file STARTS in the assembled document, used as
  # the TOC's cached page numbers (the viewer refreshes them to the live value
  # via the PAGEREF fields).  `.count_rtf_pages()` knows how a file's pages are
  # spelled, so the start page of file i = front-matter pages + 1 + pages in
  # files 1..i-1.
  entry_pages <- NULL
  if (use_toc) {
    npages <- vapply(input_files, function(f)
      .count_rtf_pages(readLines(f, warn = FALSE)), integer(1L))
    front <- if (toc_page_numbering == "roman") {
      0L                            # body restarts at 1 (front matter is i, ii)
    } else {
      (if (use_cover) 1L else 0L) + 1L   # cover + TOC, then continuous numbering
    }
    entry_pages <- as.integer(front + 1L + cumsum(c(0L, npages[-length(npages)])))
  }

  # ── File 1 ──────────────────────────────────────────────────────────────
  lines1 <- readLines(input_files[1L], warn = FALSE)
  body   <- .rtf_drop_close(lines1)

  if (use_toc || use_cover) {
    sectd_idx <- which(trimws(body) == "\\sectd")[1L]
    if (is.na(sectd_idx)) {
      stop("File 1 has no \\sectd; cannot insert cover / TOC.", call. = FALSE)
    }
    head_lines <- body[seq_len(sectd_idx - 1L)]
    tail_lines <- body[seq.int(sectd_idx, length(body))]

    front_matter <- character(0)
    if (use_cover) {
      front_matter <- c(front_matter, .build_cover_section(cover))
    }
    if (use_toc) {
      if (length(front_matter)) front_matter <- c(front_matter, "\\sect")
      front_matter <- c(
        front_matter,
        .build_toc_section(toc_entries, bookmarks, toc_title,
                           toc_leader = toc_leader,
                           page_numbering = toc_page_numbering,
                           entry_pages = entry_pages)
      )
    }

    # Roman front matter restarts the body at decimal page 1.  Decimal TOC
    # numbering is continuous, so the body must NOT restart (the TOC is page 1
    # and the first table starts on page 2).
    if (use_toc && toc_page_numbering == "roman") {
      tail_lines <- .insert_pgnrestart(tail_lines)
    }
    if (use_toc) {
      tail_lines <- .insert_bookmark(tail_lines, bookmarks[1L],
                                      outline_label = file_outline_labels[[1L]])
    }
    body <- c(head_lines, front_matter, "\\sect", tail_lines)
  }

  # ── Files 2..N ─────────────────────────────────────────────────────────
  for (i in seq_along(input_files)[-1L]) {
    content <- .rtf_extract_section_content(readLines(input_files[i],
                                                       warn = FALSE))
    if (use_toc) {
      content <- .insert_bookmark(content, bookmarks[i],
                                   outline_label = file_outline_labels[[i]])
    }
    body <- c(body, "\\sect", content)
  }

  body <- c(body, "}")
  writeLines(body, con = output_file, useBytes = TRUE)
  invisible(output_file)
}
