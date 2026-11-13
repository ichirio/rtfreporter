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
#   * page numbers                        one Arabic run from the first
#                                          page (cover and TOC included);
#                                          the total is every page (#1049)
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

# Fill every `{BOOK_PAGE}` slot with the compiled document's page number (#413).
#
# The slot is an empty ignorable destination the deliverable left behind; it
# can sit in any band, because it is only text in a cell.  `book_page` is the
# text to put there and may use the dynamic tokens -- the total it gets is the
# BOOK's page count, which is the whole point of filling this at assembly time
# rather than at generation.
#
# The static `{PAGE}` / `{TOTAL_PAGES}` cannot work here: the slot usually
# lives in a header or footer, one band shared by every page of its section,
# so a single substitution cannot carry a different number per page.
.fill_book_page_slots <- function(lines, book_page, total_pages) {
  if (is.null(book_page)) return(lines)
  book_page <- as.character(book_page)[1L]
  if (grepl("(?:^|[^A-Z_])\\{(PAGE|TOTAL_PAGES)\\}", book_page, perl = TRUE)) {
    stop("`book_page` cannot use the static `{PAGE}` / `{TOTAL_PAGES}` tokens: ",
         "the slot sits in one band shared by every page of a section, so a ",
         "single substitution cannot give each page its own number.\n",
         "  Use `{AUTO_PAGE}` / `{AUTO_TOTAL_PAGES}`, which the reader ",
         "resolves per page.", call. = FALSE)
  }
  slot <- .load_rtf_commands()$fields$book_page_slot
  filled <- .render_tokens(book_page, current_page = NULL,
                           total_pages = total_pages)
  vapply(lines, function(l) gsub(slot, filled, l, fixed = TRUE),
         character(1L), USE.NAMES = FALSE)
}

# Point every NUMPAGES cache at the assembled document's page count (#415).
#
# `{AUTO_TOTAL_PAGES}` bakes the count of the document being written, so each
# input arrives claiming its own old total while sitting in a book of a
# different length.  Word recalculates header/footer fields during layout and
# so shows the right number anyway; what this fixes is the file as written --
# for readers that display the cached result, for anything that parses rather
# than renders, and for a body-placed total, which Word does not recompute
# until the fields are refreshed.
#
# Rewriting a cached RESULT is not the conversion step this design avoids: the
# instruction is untouched, and the cache is only ever a stale answer to it.
.retotal_numpages <- function(lines, total_pages) {
  if (is.null(total_pages) || is.na(total_pages)) return(lines)
  # {\field{\*\fldinst NUMPAGES}{\fldrslt <anything but a brace>}}
  pattern <- paste0("(\\{\\\\field\\{\\\\\\*\\\\fldinst NUMPAGES\\}",
                    "\\{\\\\fldrslt )[^{}]*(\\}\\})")
  vapply(lines,
         function(l) gsub(pattern, paste0("\\1", as.integer(total_pages), "\\2"), l),
         character(1L), USE.NAMES = FALSE)
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
    if      (cp > 127L)  return(.rtf_unicode_escape(cp))
    ch
  }, character(1L))
  paste0(out, collapse = "")
}


# ── TOC entry helpers (exported) ───────────────────────────────────────────

# The scalar arguments of assemble_rtf(): each one value of its kind, so a
# typo is an error instead of a base-R message or a silent default.
.check_assemble_args <- function(output_file, overwrite, cover, toc_title,
                                 bookmark_prefix, book_page) {
  one_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x)
  }
  if (!one_string(output_file) || !nzchar(output_file)) {
    stop("`output_file` must be a single non-empty file path.", call. = FALSE)
  }
  if (!dir.exists(dirname(output_file))) {
    stop(sprintf("The folder of `output_file` does not exist: %s",
                 dirname(output_file)), call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!one_string(toc_title)) {
    stop("`toc_title` must be a single string.", call. = FALSE)
  }
  # (named l / c / r as a header cell is: the title's alignment)
  if (!is.null(names(toc_title))) .check_hf_row_names(list(toc_title), "assemble_rtf")
  if (!one_string(bookmark_prefix) ||
      !grepl("^[A-Za-z0-9_]*$", bookmark_prefix)) {
    stop("`bookmark_prefix` must be a single string of letters, digits ",
         "and `_`.", call. = FALSE)
  }
  if (!is.null(book_page) && !one_string(book_page)) {
    stop("`book_page` must be NULL or a single string.", call. = FALSE)
  }
  if (is.null(cover)) return(invisible(NULL))
  fields <- c("title", "subtitle", "date", "version", "meta")
  if (!is.list(cover) || is.data.frame(cover) ||
      (length(cover) && (is.null(names(cover)) ||
                         !all(names(cover) %in% fields)))) {
    stop("`cover` must be NULL or a list with the fields title, subtitle, ",
         "date, version and meta.", call. = FALSE)
  }
  for (f in fields[1:4]) {
    v <- cover[[f]]
    if (!is.null(v) && !(is.atomic(v) && length(v) <= 1L)) {
      stop(sprintf("`cover$%s` must be a single string.", f), call. = FALSE)
    }
  }
  if (!is.null(cover$meta) && !is.atomic(cover$meta)) {
    stop("`cover$meta` must be a character vector.", call. = FALSE)
  }
  invisible(NULL)
}

# A cover field to print: not NULL, NA or "" (those are skipped)
.cover_has <- function(v) {
  length(v) == 1L && !is.na(v) && nzchar(as.character(v))
}

# A TOC level: a whole number from 1 to `max` (a heading 2, an entry 3)
.check_toc_level <- function(level, max, what) {
  ok <- is.numeric(level) && length(level) == 1L && !is.na(level) &&
    level == round(level) && level >= 1 && level <= max
  if (!ok) {
    stop(sprintf("A TOC %s's `level` must be a whole number from 1 to %d.",
                 sub("toc_", "", what), max), call. = FALSE)
  }
  invisible(level)
}

.toc_heading <- function(label, level = 1L) {
  .check_toc_level(level, 2L, "toc_heading")
  structure(
    list(label = as.character(label)[1L], level = as.integer(level)),
    class = "rtf_toc_heading"
  )
}

.toc_entry <- function(label, file = NULL, level = 2L) {
  .check_toc_level(level, 3L, "toc_entry")
  if (length(label) != 1L || is.na(label)) {
    stop("A TOC entry's `label` must be a single string, not NA.",
         call. = FALSE)
  }
  structure(
    list(label = as.character(label)[1L],
         file  = file,
         level = as.integer(level)),
    class = "rtf_toc_entry"
  )
}


# ── TOC normalisation ──────────────────────────────────────────────────────

# (A file's title, the label of its entry when none is given, is read by
# .toc_label_from_title() in R/assemble_spec.R: the one reader of
# toc = "auto", a table toc row with no label and assemble_folder().)

# Normalise the user-supplied `toc` into a uniform list of:
#   list(label, level, file_idx (NA = heading), type = "heading"|"entry")
.normalize_toc <- function(toc, input_files) {
  if (is.null(toc)) return(NULL)

  # A table of contents as a table (file, label, heading, level)
  if (is.data.frame(toc)) toc <- .spec_to_toc(toc)

  # "auto" -> extract title from each input file
  if (is.character(toc) && length(toc) == 1L && toc == "auto") {
    out <- lapply(seq_along(input_files), function(i) {
      list(label = .toc_label_from_title(input_files[i]), level = 1L,
           file_idx = i, type = "entry")
    })
    return(out)
  }

  # Plain character vector -> one entry per file (flat, level 1)
  if (is.character(toc)) {
    if (length(toc) != length(input_files)) {
      stop("`toc` must be a character vector the same length as `input_files`.",
           call. = FALSE)
    }
    if (anyNA(toc)) stop("`toc` labels must not be NA.", call. = FALSE)
    return(lapply(seq_along(toc),
                  function(i) list(label = toc[i], level = 1L,
                                    file_idx = i, type = "entry")))
  }

  # The internal structured list (.toc_heading() / .toc_entry()) a table
  # becomes in .spec_to_toc()
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
          # (one file however it is spelled: relative / absolute, \ or /)
          norm <- function(p) normalizePath(p, "/", mustWork = FALSE)
          fi <- match(e$file, input_files)
          if (is.na(fi)) fi <- match(norm(e$file), norm(input_files))
          if (is.na(fi)) {
            stop(sprintf("`toc`: file '%s' is not in `input_files`.",
                         e$file), call. = FALSE)
          }
        } else {
          stop("`toc`: `file` must be a character path or integer index.",
               call. = FALSE)
        }
        if (is.na(fi) || fi < 1L || fi > length(input_files)) {
          stop("`toc`: file index out of range.", call. = FALSE)
        }
        used <<- c(used, fi)
        return(list(label = e$label, level = e$level,
                    file_idx = fi, type = "entry"))
      }
      stop("`toc` must be \"auto\", a character vector of labels, a table (data.frame) or the path of one.",
           call. = FALSE)
    })
    return(out)
  }

  stop("`toc` must be NULL, \"auto\", a character vector, a table ",
       "(columns file, label, heading, level) or the path of one (.xlsx / ",
       ".csv).", call. = FALSE)
}


# ── Cover-page rendering ───────────────────────────────────────────────────

# The cover and TOC sections' page: landscape letter with 0.6 in left and
# right margins.  Its text width, 14112 twips, is where the TOC's page numbers
# end and where its labels wrap (#967).
.TOC_PAGE_W    <- 15840L
.TOC_PAGE_H    <- 12240L
.TOC_MARGIN_LR <- 864L
.TOC_TEXT_W    <- .TOC_PAGE_W - 2L * .TOC_MARGIN_LR

.build_cover_section <- function(cover) {
  if (is.null(cover)) return(character())

  # Each formatted paragraph is wrapped in `{ ... }` so its \fs / \b
  # only applies inside that group.  Closing `\fs0\par` (used pre-
  # v0.0.33) was leaking font-size-0 into following content.
  blocks <- c(
    "\\sectd\\sbkpage\\lndscpsxn",
    paste0("\\pgwsxn", .TOC_PAGE_W, "\\pghsxn", .TOC_PAGE_H),
    paste0("\\marglsxn", .TOC_MARGIN_LR, "\\margrsxn", .TOC_MARGIN_LR,
           "\\margtsxn1296\\margbsxn1296"),
    # Push content vertically to roughly the upper third of the page
    "{\\pard\\fs18\\par}{\\pard\\fs18\\par}{\\pard\\fs18\\par}"
  )

  if (.cover_has(cover$title)) {
    blocks <- c(blocks,
      sprintf("{\\pard\\qc\\b\\fs44 %s\\par}",
              .toc_escape(cover$title)),
      "{\\pard\\fs18\\par}"
    )
  }
  if (.cover_has(cover$subtitle)) {
    blocks <- c(blocks,
      sprintf("{\\pard\\qc\\fs28 %s\\par}",
              .toc_escape(cover$subtitle)),
      "{\\pard\\fs18\\par}"
    )
  }
  if (.cover_has(cover$date)) {
    blocks <- c(blocks,
      sprintf("{\\pard\\qc\\fs22 %s\\par}",
              .toc_escape(cover$date))
    )
  }
  if (.cover_has(cover$version)) {
    blocks <- c(blocks,
      sprintf("{\\pard\\qc\\fs22 %s\\par}",
              .toc_escape(cover$version))
    )
  }
  if (!is.null(cover$meta) && length(cover$meta) > 0L) {
    blocks <- c(blocks, "{\\pard\\fs18\\par}")
    for (line in cover$meta[!is.na(cover$meta)]) {
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

# The TOC page each entry goes on (#767).  The TOC section is landscape
# Letter with 0.9" top / bottom margins: 9648 twips to write in.  A line is
# taken as 1.15 x its font size, a label wraps at Courier New's width beside
# its indent and page number, and a page is filled to 90% only, so the page
# breaks are the TOC's own (\pagebb) and its page count is known for the
# cached page numbers.  A heading last on a page goes over with what it heads.
# The title takes `title_lines` lines (default 1: a title of one line; a "\n"
# in it adds a line); the sizes are those of `style` (.toc_style(): 14 pt
# title, 9 pt blank line, 11 pt heading, 10 pt entry by default).
.toc_entry_pages <- function(toc_entries, title_lines = 1L,
                             style = .toc_style(NULL)) {
  if (!length(toc_entries)) return(integer(0))
  room <- 0.9 * (12240 - 2 * 1296)
  line <- function(hp) 1.15 * hp * 10               # half-points -> twips
  used <- title_lines * line(.toc_hp(style$title)) + line(.toc_hp(style$gap))
  page <- 1L
  out  <- integer(length(toc_entries))
  for (i in seq_along(toc_entries)) {
    e  <- toc_entries[[i]]
    hp <- .toc_hp(if (identical(e$type, "heading")) style$heading else style$entry)
    hang <- if (identical(e$type, "heading")) 0L else .toc_hanging(style$entry)
    n  <- .toc_label_lines(e$label, e$level, hp, hang)
    h  <- n * line(hp)
    if (used + h > room && used > 0) {
      page <- page + 1L
      used <- 0
    }
    used   <- used + h
    out[i] <- page
  }
  for (i in rev(seq_along(out))[-1L]) {
    if (identical(toc_entries[[i]]$type, "heading") && out[i + 1L] > out[i]) {
      out[i] <- out[i + 1L]
    }
  }
  out
}

# The lines a label takes at `hp` half-points: it wraps at Courier New's width
# beside its level's indent and its page number; the lines after the first
# have `hang` twips less width (a hanging indent).
.toc_label_lines <- function(label, level, hp, hang = 0L) {
  char  <- .char_width_in("courier_new", 10) * 1440  # a 10 pt character, twips
  width <- .TOC_TEXT_W - .toc_indent_for_level(level) - 720
  len   <- nchar(label %||% "")
  n <- max(1, ceiling(len * char * hp / 20 / width))
  if (hang > 0L && n > 1) {
    first <- floor(width * 20 / (char * hp))
    rest  <- max(1, floor((width - hang) * 20 / (char * hp)))
    n <- max(n, 1 + ceiling(max(0, len - first) / rest))
  }
  n
}

# -- The TOC's text styles (assemble_rtf(toc_style = )) ----------------------

# Today's sizes: the title 14 pt bold, a heading 11 pt bold, an entry 10 pt,
# and the blank line below the title 9 pt (`gap`, a size only).
.TOC_STYLE_DEFAULT <- list(
  title   = list(size = 14, bold = TRUE),
  heading = list(size = 11, bold = TRUE),
  entry   = list(size = 10, bold = FALSE, hanging = FALSE),
  gap     = list(size = 9)
)

# A style item's size in half-points (the `\fs` unit)
.toc_hp <- function(item) as.integer(round(item$size * 2))

# An entry's hanging indent in twips: 0 when off (FALSE), 720 for TRUE, else
# the number of twips given.
.toc_hanging <- function(item) {
  h <- item$hanging
  if (isTRUE(h)) 720L else if (is.numeric(h)) as.integer(h) else 0L
}

# `toc_style` (NULL, or a list of list(size = , bold = ) by title / heading /
# entry / gap) merged over the defaults, key by key; anything else an error.
.toc_style <- function(x = NULL, where = "toc_style") {
  out <- .TOC_STYLE_DEFAULT
  if (is.null(x)) return(out)
  keys <- names(out)
  if (!is.list(x) || is.data.frame(x) ||
      (length(x) && (is.null(names(x)) || any(!nzchar(names(x)))))) {
    stop("`", where, "` must be NULL or a list with the keys ",
         paste(keys, collapse = ", "), ", e.g. list(title = list(size = 12)).",
         call. = FALSE)
  }
  bad <- setdiff(names(x), keys)
  if (length(bad)) {
    stop("`", where, "` has unknown key(s) ", paste0("'", bad, "'", collapse = ", "),
         "; the keys are ", paste(keys, collapse = ", "), ".", call. = FALSE)
  }
  for (k in names(x)) {
    v <- x[[k]]
    w <- sprintf("`%s$%s`", where, k)
    fields <- names(out[[k]])
    if (!is.list(v) || (length(v) && (is.null(names(v)) || any(!nzchar(names(v)))))) {
      stop(w, " must be a list with ", paste(fields, collapse = " and "),
           ", e.g. list(size = 12).", call. = FALSE)
    }
    if (!is.null(v$hanging)) {
      hg <- v$hanging
      ok <- (is.logical(hg) && length(hg) == 1L && !is.na(hg)) ||
        (is.numeric(hg) && length(hg) == 1L && is.finite(hg) && hg > 0 &&
           hg == round(hg))
      if (!ok) {
        stop(sprintf("`%s$%s$hanging`", where, k), " must be TRUE, FALSE or a ",
             "positive whole number of twips (720 is TRUE).", call. = FALSE)
      }
      out[[k]]$hanging <- if (is.numeric(hg)) as.integer(hg) else hg
    }
    badf <- setdiff(names(v), fields)
    if (length(badf)) {
      stop(w, " has unknown key(s) ", paste0("'", badf, "'", collapse = ", "),
           "; it takes ", paste(fields, collapse = ", "), ".", call. = FALSE)
    }
    if (!is.null(v$size)) {
      s <- v$size
      if (!is.numeric(s) || length(s) != 1L || is.na(s) || !is.finite(s) ||
          s <= 0 || s * 2 != round(s * 2)) {
        stop(sprintf("`%s$%s$size`", where, k), " must be a positive number of points, whole or half ",
             "(10, 10.5).", call. = FALSE)
      }
      out[[k]]$size <- as.numeric(s)
    }
    if (!is.null(v$bold)) {
      if (!is.logical(v$bold) || length(v$bold) != 1L || is.na(v$bold)) {
        stop(sprintf("`%s$%s$bold`", where, k), " must be TRUE or FALSE.", call. = FALSE)
      }
      out[[k]]$bold <- v$bold
    }
  }
  out
}

# The control words of a styled paragraph: `\b` (when bold) and `\fs<n>`
.toc_char_cmd <- function(item) {
  paste0(if (isTRUE(item$bold)) "\\b" else "", "\\fs", .toc_hp(item))
}

# The title's alignment: named l / c / r as a header cell is; centred when
# unnamed.
.toc_title_align <- function(toc_title) {
  switch(names(toc_title)[1L] %||% "c", l = "\\ql", r = "\\qr", "\\qc")
}

# The TOC title as RTF text, filled as a page's title is: "\n" becomes a line
# break, and the tokens are filled (the session's own tokens,
# options(rtfreporter.tokens); {DATETIME}, {PROGRAM}; {AUTO_PAGE} ...).  A
# token that is not one stays as text, as in a title.  The page tokens that
# need a page ({PAGE}, {TOTAL_PAGES}) are not filled: the title is on no page
# of its own.  A heading's text is filled the same way.
.toc_title_rtf <- function(toc_title) {
  toc_title <- unname(toc_title)
  .run_ctx$tokens <- .user_tokens()
  # {PROGRAM}: the session's options(rtfreporter.program) first, as for a
  # page; else found when the token needs it
  .run_ctx$program <- .resolve_program()
  on.exit({
    .run_ctx$program <- NULL
    .run_ctx$program_done <- NULL
    .run_ctx$time <- NULL
    .run_ctx$tokens <- NULL
  }, add = TRUE)
  .substitute_page_tokens(.rtf_escape(toc_title))
}

# The lines a rendered TOC title takes: one, and one more per line break (a
# `\line` that is not an escaped backslash followed by "line").
.toc_title_lines <- function(title_rtf) {
  rx <- "(?<!\\\\)(?:\\\\\\\\)*\\\\line(?![A-Za-z])"
  m  <- gregexpr(rx, title_rtf, perl = TRUE)[[1L]]
  1L + if (m[1L] == -1L) 0L else length(m)
}

# Render the TOC section.  It writes no page-number command: its pages carry
# on the document's one Arabic sequence (#1049).
.build_toc_section <- function(toc_entries, bookmarks, toc_title,
                                toc_leader = c("dot", "none"),
                                entry_pages = NULL, title_rtf = NULL,
                                toc_style = NULL) {
  if (is.null(title_rtf)) title_rtf <- .toc_title_rtf(toc_title)
  style <- .toc_style(toc_style)
  toc_leader <- match.arg(toc_leader)
  leader_cmd <- if (toc_leader == "dot") "\\tldot" else ""
  # The page numbers sit on a right tab at the right margin: the TOC
  # section's text width (#967).
  tab_pos <- .TOC_TEXT_W

  # Section preamble.  \sbkpage forces a new page at the start; landscape +
  # rtfreporter-default margins.
  # Every paragraph below is wrapped in `{ ... }` so character format
  # state (\fs / \b) cannot leak across paragraph boundaries into the
  # following body content.  See v0.0.33 bug fix.
  lines <- c(
    "\\sectd\\sbkpage\\lndscpsxn",
    paste0("\\pgwsxn", .TOC_PAGE_W, "\\pghsxn", .TOC_PAGE_H),
    paste0("\\marglsxn", .TOC_MARGIN_LR, "\\margrsxn", .TOC_MARGIN_LR,
           "\\margtsxn1296\\margbsxn1296"),
    sprintf("{\\pard%s%s %s\\par}", .toc_title_align(toc_title),
            .toc_char_cmd(style$title), title_rtf),
    sprintf("{\\pard%s\\par}", .toc_char_cmd(style$gap))
  )

  on_page <- .toc_entry_pages(toc_entries, .toc_title_lines(title_rtf), style)
  for (k in seq_along(toc_entries)) {
    e          <- toc_entries[[k]]
    indent     <- .toc_indent_for_level(e$level)
    indent_cmd <- if (indent > 0L) sprintf("\\li%d", indent) else ""
    hang <- if (e$type == "heading") 0L else .toc_hanging(style$entry)
    if (hang > 0L) indent_cmd <- sprintf("\\li%d\\fi-%d", indent + hang, hang)
    # (the first entry of each later TOC page starts that page)
    if (k > 1L && on_page[k] > on_page[k - 1L]) indent_cmd <- paste0("\\pagebb", indent_cmd)

    if (e$type == "heading") {
      # Heading -- bold, no leader, no page number.
      lines <- c(lines,
        sprintf("{\\pard%s%s %s\\par}", indent_cmd,
                .toc_char_cmd(style$heading), .toc_title_rtf(e$label))
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
      paste0("{\\pard%s%s\\tqr%s\\tx%d ",
             "{\\field{\\*\\fldinst HYPERLINK \\\\l \"%s\"}",
             "{\\fldrslt %s}}",
             "\\tab",
             "{\\field{\\*\\fldinst PAGEREF %s \\\\h}",
             "{\\fldrslt %d}}",
             "\\par}"),
      indent_cmd, .toc_char_cmd(style$entry), leader_cmd, tab_pos, bm, txt, bm, pg
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
#' * \code{\{AUTO_PAGE\}} -- per-page number rendered by the RTF viewer,
#'   counted from the assembled file's first page.
#' * \code{\{AUTO_TOTAL_PAGES\}} -- the assembled file's total page count,
#'   a cover and the table of contents included.
#'
#' See **Page numbering** below.
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
#' Any field that is `NULL`, `NA` or empty is skipped, as is an `NA` line of
#' `meta`.  Another field name, or a `cover` that is not a list, is an error.
#'
#' @section Table of Contents:
#' `toc` can take any of these shapes:
#'
#' \describe{
#'   \item{`NULL` (default)}{no TOC, no bookmarks.  Byte-for-byte the
#'     pre-v0.0.29 behaviour.}
#'   \item{`"auto"`}{each input file's title as a level-1 TOC entry.  The
#'     title is the file's title rows (the centred bold rows [rtf_titles()]
#'     writes at the top of the body), read as the file shows them and
#'     joined into one entry with two spaces between them (`"Table 1"` and
#'     `"Demographics"` give `"Table 1  Demographics"`); a line break inside
#'     a row (`"\n"`) is joined the same way, never printed as `\line`.  A
#'     table, a figure and a listing are each labelled by their title; a
#'     file with no title rows takes its running header's `"Table N"` line
#'     (`"Table 14.1.1  Demographic Characteristics"`: the title written
#'     after the number on that line, whatever centred lines follow it, a
#'     population line for one; a line with the number only takes the
#'     longest other centred line that is not a `<...>` subtitle), and a
#'     file with neither, its file name without `.rtf`.  A file gets one
#'     entry however many pages it has: a file whose titles change from
#'     page to page (`"Summary of {PARAM}"` with `page_by`) is labelled by
#'     its first page's title.  Its tokens
#'     are filled (`{STUDY}`, `{PAGE}`), except that a dynamic page number
#'     (`{AUTO_PAGE}`, `{AUTO_TOTAL_PAGES}`) and the `{BOOK_PAGE}` slot are
#'     left out, and markup keeps its text without its formatting.
#'     [assemble_folder()] labels its rows the same way.}
#'   \item{a character vector of TOC labels}{one per input file -- same as
#'     `"auto"` but with explicit labels.}
#'   \item{a table (`data.frame`)}{one row per file, in order, for
#'     multi-level (chapter / table) layouts: `file` (the path), and
#'     optionally `label` (the entry text; a row with no `label`, or an `NA`
#'     one, takes its file's title, read as `"auto"` reads it), `heading` (a
#'     heading printed above the row's entry whenever it changes; `NA` =
#'     none), `level` (the entry's indent, a whole number from 1 to 3,
#'     default 2) and `order`.  [assemble_folder()] makes one from a
#'     folder, ready to edit.  `input_files` can then be left out: the
#'     table's `file` column is used.}
#'   \item{the path of such a table}{an `.xlsx` or `.csv` file, e.g. the one
#'     `assemble_folder(spec_file = )` wrote and you edited.}
#' }
#'
#' Each TOC entry is an RTF `HYPERLINK` field; clicking it in
#' Word / Pages jumps to a per-source-file bookmark.  Each row also
#' has a `PAGEREF` field so the page number next to the entry is
#' calculated by the viewer when the file opens.
#'
#' @section Page numbering:
#' Every page of the assembled file is numbered in one Arabic sequence from
#' its first page, and the total is the count of all its pages.  The cover,
#' when there is one, is page 1; the table of contents' pages come next, then
#' the input files' pages, with no restart anywhere.  With a cover, a
#' one-page table of contents and 7 pages of tables, the pages read 1 of 9
#' (the cover), 2 of 9 (the table of contents), 3 of 9 (the first table)
#' ... 9 of 9; without the cover, 1 of 8 (the table of contents), 2 of 8
#' (the first table) ... 8 of 8.
#'
#' The inputs' \code{\{AUTO_PAGE\}} / \code{\{AUTO_TOTAL_PAGES\}}, the
#' `book_page` text and the page numbers the table of contents shows all
#' follow this sequence.  The file is written with the total already in
#' place (every `NUMPAGES` field's cached value is the count of all pages)
#' and with each table of contents entry's cached page number, so a viewer
#' that does not refresh fields shows them too.
#'
#' @param input_files Character vector of paths to RTF files to combine
#'   (at least 2).  May be left out when `toc` is a table (or its path):
#'   the table's `file` column, in its order.
#' @param output_file Path for the assembled output RTF file: a single
#'   string, in a folder that exists, and not one of the `input_files`.
#' @param overwrite `TRUE` or `FALSE`; whether to overwrite an existing
#'   output file.  Default `FALSE`.
#' @param cover Optional `list(title, subtitle, date, version, meta)`
#'   for a cover page.  `NULL` (default) = no cover page.
#' @param toc Table-of-Contents specification.  `NULL` (default) =
#'   no TOC, no bookmarks; `"auto"` = auto-extract titles; a
#'   character vector = one label per input file; a table (column `file`,
#'   and optionally `label`, `heading`, `level`) or the path of one (`.xlsx` /
#'   `.csv`) = multi-level layout.  See **Table of Contents** above.
#' @param toc_title Bold title rendered on the TOC page, a single
#'   string (`""` = an empty title line).  Named `l`, `c` or `r` as a header
#'   cell is, it is aligned left, centre or right (`c(l = "Contents")`);
#'   unnamed it is centred.  `"\n"` is a line break in the
#'   title, and the tokens of [rtf_text_tokens()] that need no page are filled
#'   as in a title: the session's own tokens
#'   (`options(rtfreporter.tokens = )`), `{DATETIME}`, `{PROGRAM}`.  A token
#'   that is none of them stays as text.  The same goes for the text of a
#'   `heading` of a table `toc`.  Default `"Table of Contents"`.
#' @param toc_leader `"dot"` (default) draws a dotted leader between
#'   each TOC entry and its page number; `"none"` leaves whitespace.
#' @param bookmark_prefix String prepended to every auto-generated
#'   bookmark name (default `"tfl_"`): letters, digits and `_` only, as a
#'   bookmark name allows; `""` = none.  Helps avoid clashes if you
#'   later concatenate multiple assembled outputs.
#' @param book_page Text for the compiled document's page number, a single
#'   string put into every `{BOOK_PAGE}` slot the inputs reserved.  `NULL` (default) leaves
#'   the slots empty.
#'   \describe{
#'     \item{Why a slot}{A deliverable that will be bound into a book needs
#'       two page numbers: its own (`Page 1 of 3`) and the book's (`Page 4
#'       of 57`).  Written from the start they print the same number twice
#'       on a standalone deliverable, where the two are equal.  So the
#'       deliverable reserves the position instead -- `c(c = "{BOOK_PAGE}")`
#'       in whichever band it wants, header, footer, title or footnote --
#'       and renders it empty, keeping the row's height.  This argument
#'       supplies the content when the book is built, so filling it cannot
#'       shift anything.}
#'     \item{What to give}{Typically `"Page {AUTO_PAGE} of
#'       {AUTO_TOTAL_PAGES}"`.  The total resolves to the **book's** page
#'       count, which is what the deliverable could not know on its own.
#'       Both follow **Page numbering** above: the page is counted from the
#'       book's first page, a cover and the table of contents included, and
#'       the total is all of its pages (the first table after a cover and a
#'       one-page table of contents of a 9-page book reads `Page 3 of 9`).}
#'     \item{Refused}{The static `{PAGE}` / `{TOTAL_PAGES}` are rejected: the
#'       slot sits in one band shared by every page of a section, so a
#'       single substitution cannot give each page its own number.}
#'   }
#' @param toc_style The size and boldness of the TOC's text: `NULL` (the
#'   built-in sizes) or a list with the keys `title`, `heading` and `entry`,
#'   each `list(size = <points>, bold = TRUE/FALSE)`, and `gap`
#'   (`list(size = )`: the blank line below the title).  A list is merged over
#'   the built-in values key by key, so `list(entry = list(size = 9))` changes
#'   the entries' size only.  Built in: title 14 pt bold, heading 11 pt bold,
#'   entry 10 pt, gap 9 pt.  A size is a positive number of points, whole or
#'   half (`10.5`).  `entry` also takes `hanging`: `FALSE` (default) writes each
#'   entry's wrapped lines at the indent of its first line; `TRUE` hangs them
#'   720 twips (half an inch) further in, and a positive whole number is that
#'   many twips (`list(entry = list(hanging = 360))`).  Headings are not
#'   affected.  Default `getOption("rtfreporter.toc_style")`.  The font is
#'   the document's (that of the first input file).  Another key, a size that
#'   is not a positive number, a `bold` that is not `TRUE` / `FALSE` or a
#'   `hanging` that is not `TRUE` / `FALSE` / a positive whole number is an
#'   error.
#'
#'
#' @return Invisibly returns `output_file`.
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
#' a <- file.path(dir, "t14_1_1.rtf")
#' b <- file.path(dir, "t14_2_1.rtf")
#' out <- tempfile(fileext = ".rtf")
#'
#' # Plain concatenation (legacy behaviour)
#' assemble_rtf(c(a, b), out, overwrite = TRUE)
#'
#' # Auto-TOC: one entry per file, label extracted from each title
#' assemble_rtf(c(a, b), out, toc = "auto", overwrite = TRUE)
#'
#' # Multi-level TOC with section headings + cover page (pages 1, 2, 3, 4)
#' assemble_rtf(
#'   input_files = c(a, b),
#'   output_file = out,
#'   cover = list(
#'     title    = "Study XYZ-001",
#'     subtitle = "Final Statistical Report",
#'     date     = "2026-05-28",
#'     version  = "v1.0",
#'     meta     = c("Confidential", "Prepared by ACME Pharma")
#'   ),
#'   toc = data.frame(
#'     file    = c(a, b),
#'     heading = c("DEMOGRAPHICS", "SAFETY ANALYSES"),
#'     label   = c("Table 14.1.1 Demographics", "Table 14.2.1 Adverse Events")
#'   ),
#'   overwrite = TRUE
#' )
#'
#' @export
assemble_rtf <- function(input_files = NULL, output_file, overwrite = FALSE,
                          cover              = NULL,
                          toc                = NULL,
                          toc_title          = "Table of Contents",
                          toc_leader         = c("dot", "none"),
                          bookmark_prefix    = "tfl_",
                          book_page          = NULL,
                          toc_style          = getOption("rtfreporter.toc_style")) {
  toc_leader <- match.arg(toc_leader)
  .check_assemble_args(output_file, overwrite, cover, toc_title,
                       bookmark_prefix, book_page)
  toc_style <- .toc_style(toc_style)

  # A table of contents given as a table, or as the path of one: its rows in
  # `order`, and its files when `input_files` is left out.
  # (any other file: .read_spec() says it must be .xlsx / .csv)
  if (is.character(toc) && length(toc) == 1L && !identical(toc, "auto") &&
      (grepl("[.](xlsx|csv)$", toc, ignore.case = TRUE) || file.exists(toc) ||
       grepl("[.][A-Za-z]{2,5}$", toc))) {
    if (grepl("[.](xlsx|csv)$", toc, ignore.case = TRUE) && !file.exists(toc)) {
      stop(sprintf("`toc` file not found: %s", toc), call. = FALSE)
    }
    toc <- .read_spec(toc)
  }
  if (is.data.frame(toc)) {
    toc <- .check_toc_table(toc)
    if (is.null(input_files)) input_files <- toc$file
  }

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
  norm_path <- function(p) normalizePath(p, "/", mustWork = FALSE)
  if (norm_path(output_file) %in% norm_path(input_files)) {
    stop("`output_file` is one of the `input_files`; writing it would ",
         "destroy that input.", call. = FALSE)
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
    # entry, fall back to basename(file) so the PDF outline
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
  # files 1..i-1: the pages are one sequence from the first, a cover and the
  # TOC's own pages included (#815, #1049).
  entry_pages <- NULL
  title_rtf   <- if (use_toc) .toc_title_rtf(toc_title)
  toc_pages   <- if (use_toc) {
    max(1L, .toc_entry_pages(toc_entries, .toc_title_lines(title_rtf), toc_style))
  } else 0L
  if (use_toc) {
    npages <- vapply(input_files, function(f)
      .count_rtf_pages(readLines(f, warn = FALSE)), integer(1L))
    front <- (if (use_cover) 1L else 0L) + toc_pages
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
                           entry_pages = entry_pages, title_rtf = title_rtf,
                           toc_style = toc_style)
      )
    }

    # No page-number command anywhere: the body runs on from the front matter
    # (#1049).
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

  # The assembled document's own page count, known only here.  Front matter
  # counts: a cover is one page, the TOC its own pages (.toc_entry_pages()).
  body_pages  <- sum(vapply(input_files, function(f)
    .count_rtf_pages(readLines(f, warn = FALSE)), integer(1L)))
  front_pages <- (if (use_cover) 1L else 0L) + toc_pages
  book_pages  <- body_pages + front_pages

  # Fill the reserved slots, then make every NUMPAGES cache -- the inputs' own
  # included -- agree with the book they now sit in.
  if (!is.null(book_page)) {
    body <- .fill_book_page_slots(body, book_page, book_pages)
  }
  body <- .retotal_numpages(body, book_pages)

  body <- c(body, "}")
  writeLines(body, con = output_file, useBytes = TRUE)
  invisible(output_file)
}
