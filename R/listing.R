# ============================================================================
#  Listing preparation -- listing_col() / listing_spec() / build_listing()
# ============================================================================
#
#  A clinical listing is a data.frame problem before it is an RTF problem.
#  What a source data.frame lacks is not styling but SHAPE:
#
#    * several source variables belong in one printed column, joined by "/";
#    * a long cell has to break over several physical rows so it fits a column
#      that is only so many characters wide;
#    * narrow blank columns sit between the printed ones as gutters;
#    * a blank row separates one subject's block from the next; and
#    * a page break must never land inside a subject's block.
#
#  Only the first four are new work.  The last one is `split = "group_safe"`,
#  which is why `build_listing()` emits a hidden record-id column: the
#  `as_rtftables(listing = )` hook points `group_col` at it, asks for a
#  group-safe split and names it in `drop_cols` -- and because drop columns are
#  hidden AFTER pagination (see `as_rtftables()`), the column groups the pages
#  and then disappears.
#
#  The division of labour follows the stub, which answered the same question in
#  #314: `stub_cols()` is a data.frame verb, `stub_spec()` bundles its
#  settings, and `as_rtftables(stub = )` runs it as a hook inside the pipeline.
#  Here that is `build_listing()`, `listing_spec()` and
#  `as_rtftables(listing = )` -- and, as there, the verb and the hook read the
#  SAME spec object, so neither carries a copy of the other's argument list.
#
#  What this file does NOT do is render: it hands `as_rtftables()` a
#  data.frame, and everything from pagination onwards is the machinery that was
#  already there.


# ── Templates ────────────────────────────────────────────────────────────────
#
#  A listing "type" is a named bundle of defaults -- the separator, the gutter
#  columns and their width, the per-record blank row, the wrapping rule, the
#  default alignment.  `listing_spec(type = )` looks the name up here and any
#  argument given explicitly overrides what it finds, exactly as
#  `rtftable(border = "tfl")` resolves a preset and lets `border = ` override
#  it.
#
#  Adding a type is one entry in this registry plus documentation and tests: no
#  signature anywhere changes.  The wrapping rule is part of the entry rather
#  than hard-coded below, so a type whose rule is not "separator first, then
#  words" can be added without touching the one that ships.

.listing_templates <- function() {
  list(
    multiline = list(
      sep              = "/",
      spacer           = TRUE,
      spacer_rel_width = 1,
      blank_row        = TRUE,
      blank_row_first  = TRUE,
      align            = "left",
      layout           = "stack",
      wrap             = .listing_wrap_sep_word
    )
  )
}

.listing_template <- function(type) {
  if (!is.character(type) || length(type) != 1L || is.na(type) ||
      !nzchar(type)) {
    stop("`type` must be a single listing-template name; see ?listing_spec.",
         call. = FALSE)
  }
  reg <- .listing_templates()
  tpl <- reg[[type]]
  if (is.null(tpl)) {
    stop(sprintf("Unknown listing type \"%s\".  Available: %s.", type,
                 paste0("\"", names(reg), "\"", collapse = ", ")),
         call. = FALSE)
  }
  tpl
}


# ── The "multiline" wrapping rule ────────────────────────────────────────────
#
#  Break after the separator first, and only inside a piece that is still too
#  long fall back to word boundaries (a space, a comma or a hyphen, the break
#  taken AFTER the character so the reader can see why the line ended).  A
#  token that is still too wide on its own is hard-split, so **every line this
#  returns fits the column it was measured against** (#364).
#
#  That last rule used to be the opposite -- an over-long token kept its own
#  line, on the grounds that cutting a subject id in half is ugly.  It is, but
#  the line count is what pages the listing: `rel_width` follows `width`, so a
#  token wider than the column is wider than the RENDERED column too, Word
#  wraps it onto a second line, and the record is a row taller than
#  `build_listing()` counted.  A page that overflows in Word is worse than a
#  split id, and it is the same failure as #362 by another route.
#
#  Widths are DISPLAY widths, not character counts: a full-width (CJK) glyph
#  occupies two monospaced columns, so `nchar()` would let a Japanese listing
#  ask for 20 columns and take up to 40.

# Display width, with the character count as the fallback where the width
# cannot be determined (an unknown encoding, a control character).
.listing_disp_width <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  w <- suppressWarnings(nchar(x, type = "width", allowNA = TRUE))
  bad <- is.na(w)
  if (any(bad)) w[bad] <- nchar(x[bad], type = "chars")
  as.integer(w)
}

# The longest prefix of `x` that fits `width`.  Never returns "" for a
# non-empty `x`, so a caller looping on the remainder always makes progress --
# even where one glyph is wider than the whole column.
.listing_take <- function(x, width) {
  n <- nchar(x, type = "chars")
  if (n == 0L) return("")
  best <- 1L
  for (i in seq_len(n)) {
    if (.listing_disp_width(substr(x, 1L, i)) <= width) best <- i else break
  }
  substr(x, 1L, best)
}

.listing_split_after <- function(text, sep) {
  if (is.null(sep) || !nzchar(sep)) return(text)
  # \Q...\E quotes the separator, so a "." or a "|" separator is a literal and
  # the lookbehind stays fixed-width.  (Written this way rather than with an
  # escaping gsub(): backreferences are unreliable in some R builds.)
  parts <- strsplit(text, paste0("(?<=\\Q", sep, "\\E)"), perl = TRUE)[[1L]]
  if (!length(parts)) text else parts
}

.listing_wrap_words <- function(text, width) {
  words <- strsplit(text, "(?<=[ ,-])", perl = TRUE)[[1L]]
  if (!length(words)) words <- text
  out <- character(0L)
  cur <- ""
  for (w in words) {
    # A token wider than the column on its own.  Split it here, before the
    # running line is trimmed to measure it -- trimming would eat the trailing
    # space that separates this word from the next.
    if (.listing_disp_width(trimws(w)) > width) {
      if (nzchar(trimws(cur))) out <- c(out, trimws(cur))
      tok <- sub("^\\s+", "", w)
      while (.listing_disp_width(trimws(tok)) > width) {
        piece <- .listing_take(tok, width)
        out   <- c(out, piece)
        tok   <- substring(tok, nchar(piece, type = "chars") + 1L)
      }
      cur <- tok                       # keeps any trailing separator
      next
    }
    if (!nzchar(cur)) {
      cur <- w
      # Measured WITHOUT trimming the running line, which is what the rule
      # this came from did.  Trimming would let a line end one character
      # wider (the trailing space is invisible) and would silently change
      # every existing listing's line counts; that is a separate decision,
      # not part of fixing #364.
    } else if (.listing_disp_width(cur) + .listing_disp_width(w) <= width) {
      cur <- paste0(cur, w)
    } else {
      out <- c(out, trimws(cur))
      cur <- w
    }
  }
  if (nzchar(trimws(cur))) out <- c(out, trimws(cur))
  out
}

# Refill separator-delimited pieces into lines of at most `width`, so a cell
# whose parts are short keeps them side by side.  This is what turns the
# unconditional break of "stack" into the break OPPORTUNITY of "flow".
.listing_flow <- function(parts, width) {
  out <- character(0L)
  cur <- ""
  for (p in parts) {
    if (!nzchar(cur)) {
      cur <- p
    } else if (.listing_disp_width(trimws(paste0(cur, p))) <= width) {
      cur <- paste0(cur, p)
    } else {
      out <- c(out, cur)
      cur <- p
    }
  }
  if (nzchar(cur)) out <- c(out, cur)
  out
}

.listing_wrap_sep_word <- function(text, width, sep, layout = "stack") {
  if (is.null(text) || length(text) != 1L || is.na(text)) text <- ""
  text <- as.character(text)
  # A "\n" already in the data is a line break the author asked for; honour it
  # before any width is considered.
  chunks <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  if (!length(chunks)) chunks <- ""
  if (is.null(width) || is.na(width)) {
    # No width, no layout: with nothing to lay the column out against, both
    # `layout`s return the text as it stands.  (ydisctools' rule breaks at
    # every separator here even with no width; that would silently make every
    # existing width-less multi-variable column taller, and nothing asks for
    # it -- `width` is what says how the column is laid out.)
    chunks <- trimws(chunks)
    return(if (all(!nzchar(chunks))) "" else chunks)
  }
  out <- character(0L)
  for (ch in chunks) {
    parts <- .listing_split_after(ch, sep)
    # "flow": refill the pieces first, so a break survives only where the
    # line ran out of room.  "stack" keeps every separator break.
    if (identical(layout, "flow")) parts <- .listing_flow(parts, width)
    for (p in parts) {
      p <- trimws(p)
      if (!nzchar(p)) next
      if (.listing_disp_width(p) <= width) {
        out <- c(out, p)
      } else {
        out <- c(out, .listing_wrap_words(p, width))
      }
    }
  }
  if (!length(out)) "" else out
}


# ── One printed column ───────────────────────────────────────────────────────

#' One printed column of a listing
#'
#' Describes a single column of a listing: which source variables it is built
#' from, how they are joined, how wide it may be before its text wraps onto a
#' further physical row, and what its header says.  A list of these is what
#' [listing_spec()] takes, and [build_listing()] turns into a data.frame.
#'
#' `vars` may name **several** columns: their values are joined with `sep`,
#' missing and empty values dropped, so a column reading
#' `"ADENOCARCINOMA/BRCA1/GRADE 3"` is written `listing_col(c("HIST", "BRCA",
#' "HISTGRD"))` rather than pasted by hand upstream.
#'
#' `width` is a **display width in characters** (a full-width CJK glyph
#' counts as two), not a rendered width: it decides
#' where the text of this column breaks onto another physical row, and so how
#' tall each record's block is.  It does not set the column's width in the
#' table -- that is `rel_width`, which defaults to `width` when you give one.
#'
#' @param vars Character.  One or more source column names, joined with `sep`
#'   in the order given.  Missing (`NA`) and empty values are skipped, so a
#'   record missing its middle value does not print a doubled separator.
#' @param sep Separator for `vars`.  `NULL` (default) takes the listing's own
#'   (`"/"` under the `"multiline"` type).
#' @param width Integer or `NULL`.  Maximum characters per physical row before
#'   the cell wraps.  `NULL` (default) never wraps this column.
#' @param label Column header text.  A line break starts a further header row,
#'   as everywhere else in rtfreporter, and what you write is used **exactly**
#'   -- you laid the lines out, so it is never re-wrapped.  `NULL` (default)
#'   **derives** the header from the data: each source column's `label`
#'   attribute when it has one, otherwise its name, joined with `sep` and a
#'   line break, then wrapped to `width` so it cannot be wider than the column
#'   it sits over.  `""` asks for a deliberately empty header.
#' @param layout How a cell lays its parts out: `"stack"` breaks after
#'   **every** separator, so each source column starts its own line -- the
#'   conventional listing look, and it keeps a column reading down the page.
#'   `"flow"` treats the separator as a break *opportunity* and fills each
#'   line as far as `width` allows, so a column of short parts (an age and a
#'   sex, a value and its unit) does not spend two rows on four characters.
#'   `NULL` (default) takes the listing's own (`"stack"` under `"multiline"`).
#'   With no `width` there is nothing to lay out and both behave alike.
#' @param collapse_repeats Logical (default `FALSE`).  Mark this as a **key**
#'   column: its value is carried down every physical row of a record, and
#'   [as_rtftables()] then blanks the repeats.  Carrying and delegating rather
#'   than blanking here is what makes the value **reappear at the top of the
#'   next page** -- the suppression happens per page, after the split, so a
#'   record continued across a page break still shows its subject.  A cell
#'   that already wraps onto several lines is padded as usual: there is no one
#'   value to repeat.
#' @param name Output column name.  `NULL` (default) uses the first entry of
#'   `vars`; [listing_spec()] makes the set unique if two columns collide.
#' @param rel_width Relative width of this column in the rendered table.
#'   `NULL` (default) uses `width` when there is one, otherwise the longest
#'   line of `label`.
#' @param align `"left"`, `"center"` or `"right"`.  `NULL` (default) takes the
#'   listing's own (`"left"` under the `"multiline"` type).
#'
#' @return An object of class `rtf_listing_col`.
#'
#' @seealso [listing_spec()], which collects these; [build_listing()], which
#'   applies them.
#'
#' @examples
#' # One source column, wrapped at 15 characters.
#' listing_col("USUBJID", width = 15, label = "Unique\nSubject ID")
#'
#' # Three source columns in one printed column, joined with "/".
#' listing_col(c("DISPTPD", "BRCA", "HIST"), width = 22,
#'             label = "Primary Diagnosis/\nAny (BRCA) Mutations/\nHistology")
#'
#' # No wrapping, and a header only.
#' listing_col("STAGE", label = "Stage at\nInitial\nDiagnosis")
#'
#' # Header left to the data's own labels; short parts kept side by side.
#' listing_col(c("AGE", "SEX"), width = 12, layout = "flow")
#'
#' # A key column: printed once per record, again atop the next page.
#' listing_col("USUBJID", width = 15, collapse_repeats = TRUE)
#'
#' @export
listing_col <- function(vars,
                        sep       = NULL,
                        width     = NULL,
                        label     = NULL,
                        name      = NULL,
                        rel_width = NULL,
                        align     = NULL,
                        layout    = NULL,
                        collapse_repeats = FALSE) {
  if (missing(vars) || is.null(vars) || !is.character(vars) ||
      length(vars) == 0L || anyNA(vars) || !all(nzchar(vars))) {
    stop("`vars` must be one or more non-empty source column names.",
         call. = FALSE)
  }
  if (!is.null(sep) && (!is.character(sep) || length(sep) != 1L ||
                        is.na(sep))) {
    stop("`sep` must be a single string, or NULL.", call. = FALSE)
  }
  if (!is.null(width)) {
    if (length(width) != 1L || is.na(width) || !is.numeric(width) ||
        width < 1) {
      stop("`width` must be a single positive number of characters, or NULL.",
           call. = FALSE)
    }
    width <- as.integer(width)
  }
  if (!is.null(rel_width)) {
    if (length(rel_width) != 1L || !is.numeric(rel_width) ||
        is.na(rel_width) || rel_width <= 0) {
      stop("`rel_width` must be a single positive number, or NULL.",
           call. = FALSE)
    }
    rel_width <- as.numeric(rel_width)
  }
  if (!is.null(label) && (!is.character(label) || length(label) != 1L ||
                          is.na(label))) {
    stop("`label` must be a single string, or NULL.", call. = FALSE)
  }
  if (!is.null(name) && (!is.character(name) || length(name) != 1L ||
                         is.na(name) || !nzchar(name))) {
    stop("`name` must be a single non-empty string, or NULL.", call. = FALSE)
  }
  if (!is.null(align)) {
    align <- match.arg(align, c("left", "center", "right"))
  }
  if (!is.null(layout)) {
    layout <- match.arg(layout, c("stack", "flow"))
  }
  if (!is.logical(collapse_repeats) || length(collapse_repeats) != 1L ||
      is.na(collapse_repeats)) {
    stop("`collapse_repeats` must be TRUE or FALSE.", call. = FALSE)
  }
  structure(
    list(vars = vars, sep = sep, width = width, label = label,
         name = if (is.null(name)) vars[1L] else name,
         rel_width = rel_width, align = align, layout = layout,
         collapse_repeats = collapse_repeats),
    class = "rtf_listing_col"
  )
}

#' @export
print.rtf_listing_col <- function(x, ...) {
  cat("<rtf_listing_col>\n")
  cat("  name  : ", x$name, "\n", sep = "")
  cat("  vars  : ", paste(x$vars, collapse = ", "), "\n", sep = "")
  cat("  width : ", if (is.null(x$width)) "(no wrap)" else x$width,
      "\n", sep = "")
  if (!is.null(x$label)) {
    cat("  label : ", gsub("\n", " / ", x$label, fixed = TRUE), "\n", sep = "")
  }
  invisible(x)
}


# ── The listing as a whole ───────────────────────────────────────────────────

#' Bundle the settings for one listing
#'
#' Collects the columns of a listing and the settings that apply to all of
#' them, so that [build_listing()] and `as_rtftables(listing = )` take a single
#' argument and read exactly the same object -- neither one carries a copy of
#' the other's argument list.
#'
#' @section Listing types:
#'
#' `type` names a template that supplies every default below, and any argument
#' you pass explicitly overrides it -- the same relationship
#' `rtftable(border = "tfl")` has with its preset.  One type ships:
#'
#' \describe{
#'   \item{`"multiline"`}{The layout a wide clinical listing usually wants: a
#'     `"/"` separator, gutter columns between the printed ones, a blank row
#'     after each record and one at the top of every page, everything left
#'     aligned, and text wrapped at the separator first and at word boundaries
#'     only where a piece is still too long.}
#' }
#'
#' Adding a type later changes no signature: it is one entry in the internal
#' registry, and it brings its own wrapping rule with it.
#'
#' @section The record column:
#'
#' A listing wraps one source row over several physical rows, so a page break
#' must not land inside one.  `record` asks [build_listing()] to append a
#' hidden column holding the source row number; `as_rtftables(listing = )` then
#' points `group_col` at it, splits with `"group_safe"` and lists it in
#' `drop_cols`, so it decides the page breaks and is never printed.  Set
#' `record = FALSE` only if you intend to paginate some other way.
#'
#' @param cols The printed columns, in order: a list of [listing_col()]
#'   objects.  A bare string (or a character vector) stands for
#'   `listing_col()` on it, so `cols = c("USUBJID", "AGE")` means two
#'   unwrapped columns.
#' @param type Listing template name; see *Listing types*.  Default
#'   `"multiline"`.
#' @param sep Default separator for a [listing_col()] that does not set its
#'   own.  `NULL` (default) takes the template's.
#' @param spacer Logical.  Insert a narrow blank column between each pair of
#'   printed columns.  `NULL` (default) takes the template's.
#' @param spacer_rel_width Relative width of those gutter columns.  `NULL`
#'   (default) takes the template's.
#' @param blank_row Logical.  End each record's block with a blank row, so one
#'   subject is visibly separated from the next.  `NULL` (default) takes the
#'   template's.
#' @param blank_row_first Logical.  Start each page with a blank row.  Passed
#'   on as `as_rtftables(blank_row_first = )`, which is what makes it *per
#'   page* rather than once per listing.  `NULL` (default) takes the
#'   template's.
#' @param align Default alignment for columns that do not set their own.
#'   `NULL` (default) takes the template's.
#' @param layout Default cell layout for columns that do not set their own:
#'   `"stack"` breaks after every separator, `"flow"` fills each line as far
#'   as the column's `width` allows.  See [listing_col()].  `NULL` (default)
#'   takes the template's.
#' @param record `TRUE` (default) appends the hidden record column under its
#'   standard name, `FALSE` appends none, or a single string names it
#'   yourself.  See *The record column*.
#'
#' @return An object of class `rtf_listing_spec`.
#'
#' @seealso [listing_col()] for one column; [build_listing()], which applies
#'   this to a data.frame; [as_rtftables()], whose `listing` argument does the
#'   same thing inside the rendering pipeline.
#'
#' @examples
#' spec <- listing_spec(list(
#'   listing_col("USUBJID", width = 15, label = "Unique\nSubject ID"),
#'   listing_col(c("SEX", "AGE"), width = 12, label = "Sex/\nAge"),
#'   listing_col("ARM", width = 20, label = "Treatment Arm")
#' ))
#' spec
#'
#' # Bare names are columns too: two unwrapped columns, no gutters.
#' listing_spec(c("USUBJID", "ARM"), spacer = FALSE)
#'
#' @export
listing_spec <- function(cols,
                         type             = "multiline",
                         sep              = NULL,
                         spacer           = NULL,
                         spacer_rel_width = NULL,
                         blank_row        = NULL,
                         blank_row_first  = NULL,
                         align            = NULL,
                         layout           = NULL,
                         record           = TRUE) {
  tpl <- .listing_template(type)

  if (missing(cols) || is.null(cols)) {
    stop("`cols` is required: the printed columns, in order.", call. = FALSE)
  }
  if (inherits(cols, "rtf_listing_col")) cols <- list(cols)
  if (is.character(cols)) cols <- as.list(cols)
  if (!is.list(cols) || length(cols) == 0L) {
    stop("`cols` must be a non-empty list of listing_col() objects (or ",
         "column names).", call. = FALSE)
  }
  cols <- lapply(seq_along(cols), function(j) {
    cl <- cols[[j]]
    if (is.character(cl)) cl <- listing_col(cl)
    if (!inherits(cl, "rtf_listing_col")) {
      stop(sprintf(paste0("`cols[[%d]]` must be a listing_col() or a ",
                          "character vector of source column names; got '%s'."),
                   j, paste(class(cl), collapse = "/")), call. = FALSE)
    }
    cl
  })
  # Two columns built from the same first variable would otherwise produce two
  # identically named output columns.
  nms <- make.unique(vapply(cols, function(cl) cl$name, character(1L)),
                     sep = "_")
  for (j in seq_along(cols)) cols[[j]]$name <- nms[j]

  chk_lgl <- function(v, nm) {
    if (is.null(v)) return(NULL)
    if (!is.logical(v) || length(v) != 1L || is.na(v)) {
      stop(sprintf("`%s` must be TRUE or FALSE, or NULL.", nm), call. = FALSE)
    }
    v
  }
  spacer          <- chk_lgl(spacer, "spacer")
  blank_row       <- chk_lgl(blank_row, "blank_row")
  blank_row_first <- chk_lgl(blank_row_first, "blank_row_first")
  if (!is.null(spacer_rel_width) &&
      (!is.numeric(spacer_rel_width) || length(spacer_rel_width) != 1L ||
       is.na(spacer_rel_width) || spacer_rel_width <= 0)) {
    stop("`spacer_rel_width` must be a single positive number, or NULL.",
         call. = FALSE)
  }
  if (!is.null(align)) align <- match.arg(align, c("left", "center", "right"))
  if (!is.null(layout)) layout <- match.arg(layout, c("stack", "flow"))

  record_col <-
    if (isTRUE(record)) {
      ".rtf_record"
    } else if (isFALSE(record)) {
      NULL
    } else if (is.character(record) && length(record) == 1L && !is.na(record) &&
               nzchar(record)) {
      record
    } else {
      stop("`record` must be TRUE, FALSE, or a single column name.",
           call. = FALSE)
    }

  structure(
    list(cols             = cols,
         type             = type,
         sep              = if (is.null(sep)) tpl$sep else sep,
         spacer           = if (is.null(spacer)) tpl$spacer else spacer,
         spacer_rel_width = if (is.null(spacer_rel_width)) {
                              tpl$spacer_rel_width
                            } else spacer_rel_width,
         blank_row        = if (is.null(blank_row)) tpl$blank_row else blank_row,
         blank_row_first  = if (is.null(blank_row_first)) {
                              tpl$blank_row_first
                            } else blank_row_first,
         align            = if (is.null(align)) tpl$align else align,
         layout           = if (is.null(layout)) tpl$layout else layout,
         wrap             = tpl$wrap,
         record_col       = record_col),
    class = "rtf_listing_spec"
  )
}

#' @export
print.rtf_listing_spec <- function(x, ...) {
  cat("<rtf_listing_spec>\n")
  cat("  type    : ", x$type, "\n", sep = "")
  cat("  columns : ", length(x$cols),
      if (isTRUE(x$spacer)) " (+ gutters)" else "", "\n", sep = "")
  for (cl in x$cols) {
    cat("    - ", cl$name, "  <- ", paste(cl$vars, collapse = paste0(" ", x$sep, " ")),
        if (is.null(cl$width)) "" else paste0("  [wrap ", cl$width, "]"),
        "\n", sep = "")
  }
  cat("  record  : ",
      if (is.null(x$record_col)) "(none)" else x$record_col, "\n", sep = "")
  fit <- attr(x, "rtf_listing_fit", exact = TRUE)
  if (!is.null(fit)) {
    widths <- vapply(x$cols, function(c1) as.integer(c1$width), integer(1L))
    cat("  fitted  : ", sum(widths), " + ", fit$gutter, " gutter of ",
        fit$total_width, " characters
", sep = "")
    w <- max(nchar(names(fit$demand)))
    for (i in seq_along(x$cols)) {
      cat(sprintf("    %-*s  width = %3d %s (demand %.1f)
", w,
                  names(fit$demand)[i], widths[i],
                  if (fit$fixed[i]) "set" else "fit", fit$demand[i]))
    }
    cat("  Paste listing_code(spec) into your program, then tune by eye.
")
  }
  invisible(x)
}


# ── Layout: the one place output columns are enumerated ──────────────────────
#
#  Both halves of the feature need the same answer to "what are the output
#  columns, in order, and what does each one look like?" -- `build_listing()`
#  to name and fill them, the `as_rtftables()` hook to derive the header, the
#  widths and the alignment.  Neither computes it itself.

.listing_default_rel_width <- function(cl) {
  if (!is.null(cl$rel_width)) return(as.numeric(cl$rel_width))
  if (!is.null(cl$width))     return(as.numeric(cl$width))
  if (!is.null(cl$label)) {
    lines <- strsplit(cl$label, "\n", fixed = TRUE)[[1L]]
    if (length(lines)) return(max(.listing_disp_width(lines), 1))
  }
  10
}

.listing_layout <- function(spec) {
  k     <- length(spec$cols)
  items <- list()
  for (j in seq_len(k)) {
    cl <- spec$cols[[j]]
    items[[length(items) + 1L]] <- list(
      kind      = "col",
      index     = j,
      name      = cl$name,
      label     = if (is.null(cl$label)) "" else cl$label,
      rel_width = .listing_default_rel_width(cl),
      align     = if (is.null(cl$align)) spec$align else cl$align)
    if (isTRUE(spec$spacer) && j < k) {
      items[[length(items) + 1L]] <- list(
        kind      = "spacer",
        index     = NA_integer_,
        name      = paste0(".sp", j),
        label     = "",
        rel_width = as.numeric(spec$spacer_rel_width),
        align     = spec$align)
    }
  }
  items
}

# The narrowest width at which a piece of text can be laid out without a hard
# split: the widest of its unbreakable tokens.  A header can WRAP, so what a
# column owes its header is this, not the header's full length -- otherwise a
# long label ("Time since Initial Diagnosis (months)") would claim a column
# nearly forty characters wide for data that needs ten.
.listing_min_wrap_width <- function(text, sep) {
  if (is.null(text) || !length(text) || is.na(text[[1L]])) return(0L)
  pieces <- unlist(strsplit(as.character(text)[[1L]], "
", fixed = TRUE),
                   use.names = FALSE)
  pieces <- unlist(lapply(pieces, .listing_split_after, sep = sep),
                   use.names = FALSE)
  tokens <- unlist(lapply(pieces, function(p) {
    strsplit(p, "(?<=[ ,-])", perl = TRUE)[[1L]]
  }), use.names = FALSE)
  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) return(0L)
  max(.listing_disp_width(tokens))
}

# One source column's display name: its `label` attribute when it has a usable
# one, otherwise the column name.  This is `stub_cols(label = NULL)`'s rule --
# a listing header and a merged stub label are derived the same way.
#  `labels` is the lookup `fit_listing_widths(labels = )` supplies: the words
#  for each SOURCE variable, written in the program.  It beats the data,
#  because data with no label attributes is exactly the case it exists for,
#  and a label that is wrong in the source should be correctable without
#  touching the data.
.listing_var_label <- function(data, v, labels = NULL) {
  if (!is.null(labels) && v %in% names(labels)) {
    lab <- labels[[v]]
    if (is.character(lab) && length(lab) == 1L && !is.na(lab) &&
        nzchar(lab)) {
      return(lab)
    }
  }
  lab <- attr(data[[v]], "label", exact = TRUE)
  if (is.character(lab) && length(lab) == 1L && !is.na(lab) && nzchar(lab)) {
    lab
  } else {
    v
  }
}

# Resolve one column's header.  A `label` written by hand is used EXACTLY as
# written -- the author laid the lines out and re-wrapping would fight them.
# A derived header is built from every source column and wrapped to the
# column's width, so it can never be wider than the column it sits over.
.listing_resolve_label <- function(data, cl, sep, layout, labels = NULL) {
  if (!is.null(cl$label)) return(cl$label)
  labs <- vapply(cl$vars, function(v) .listing_var_label(data, v, labels),
                 character(1L))
  n <- length(labs)
  pieces <- if (n > 1L) c(paste0(labs[-n], sep), labs[[n]]) else labs
  lines <- unlist(lapply(pieces, function(p) {
    .listing_wrap_sep_word(p, cl$width, sep, layout)
  }), use.names = FALSE)
  paste(lines, collapse = "
")
}

# Header / widths / alignment for a body `build_listing()` produced.  Given in
# the body's own coordinates, record column included, because as_rtftables()
# hides the drop columns AFTER pagination and re-indexes these alongside.
.listing_metadata <- function(spec, body) {
  layout <- .listing_layout(spec)
  hdr <- vapply(layout, function(it) it$label, character(1L))
  rw  <- vapply(layout, function(it) it$rel_width, numeric(1L))
  cs  <- lapply(seq_along(layout), function(j) {
    list(col = j, align = layout[[j]]$align)
  })
  if (!is.null(spec$record_col) && spec$record_col %in% names(body)) {
    hdr <- c(hdr, "")
    rw  <- c(rw, 1)
  }
  list(col_header = hdr, col_rel_width = rw, col_spec = cs)
}


# ── build_listing() ──────────────────────────────────────────────────────────

.listing_combine <- function(data, cl, sep) {
  missing_vars <- setdiff(cl$vars, names(data))
  if (length(missing_vars)) {
    stop(sprintf("Column%s %s not in `data` (listing column \"%s\").",
                 if (length(missing_vars) > 1L) "s" else "",
                 paste0("\"", missing_vars, "\"", collapse = ", "),
                 cl$name), call. = FALSE)
  }
  # The join itself is catx() (#360), so a listing column and a join the user
  # writes by hand cannot drift apart.
  if (nrow(data) == 0L) return(character(0))
  do.call(catx, c(list(sep), unname(as.list(data[cl$vars]))))
}

#' Reshape a data.frame into a listing body
#'
#' Turns source data into the data.frame a listing prints: source variables
#' joined into their printed columns, long cells broken over as many physical
#' rows as they need, gutter columns between the printed ones, a blank row
#' after each record, and a hidden record column so a page break never lands
#' inside a record.
#'
#' This is preparation only -- the result is an ordinary data.frame, and
#' [as_rtftables()] does the rendering.  The result carries the spec as an
#' attribute, so the two halves compose:
#'
#' \preformatted{
#'   build_listing(adsl, spec) |> as_rtftables(max_rows = 40)
#'   as_rtftables(adsl, listing = spec, max_rows = 40)   # the same thing
#' }
#'
#' The second form is the usual one; reach for this one to look at (or patch)
#' the reshaped data before it is rendered.
#'
#' @section How a cell wraps:
#'
#' Under the `"multiline"` type, a cell longer than its column's `width`
#' breaks **after the separator** first, so each source variable starts its own
#' line; a piece that is still too long breaks again at a word boundary (after
#' a space, comma or hyphen); and a token that is *still* too wide is
#' hard-split, so **every line fits the column it was measured against**.  A
#' line break already in the data is honoured before any of this.
#'
#' `width` is a **display width**: a full-width (CJK) glyph occupies two
#' monospaced columns and is counted as two, so a Japanese listing wrapped to
#' 20 really does fit in twenty.
#'
#' Every column of one record is padded to the tallest, so the record's rows
#' stay aligned across columns.
#'
#' @param data A `data.frame` (or tibble) of source data -- one row per record.
#'   An rlistings `listing_df` is **not** accepted: it has already been laid
#'   out by rlistings, and goes straight to [as_rtftables()].
#' @param spec A [listing_spec()].
#'
#' @return A `data.frame`: the printed columns in order, gutter columns between
#'   them, and (unless `record = FALSE`) the hidden record column last.  It
#'   carries the spec as the attribute `rtf_listing`, which [as_rtftables()]
#'   reads.
#'
#' @seealso [listing_spec()] and [listing_col()] for the settings;
#'   [as_rtftables()], whose `listing` argument runs this inside the pipeline.
#'
#' @examples
#' adsl <- data.frame(
#'   USUBJID = c("01-701-1015", "01-701-1023"),
#'   HIST    = c("ADENOCARCINOMA", "SQUAMOUS CELL CARCINOMA"),
#'   BRCA    = c("BRCA1", NA),
#'   ARM     = c("Placebo", "Xanomeline High Dose"),
#'   stringsAsFactors = FALSE
#' )
#'
#' spec <- listing_spec(list(
#'   listing_col("USUBJID", width = 11, label = "Unique\nSubject ID"),
#'   listing_col(c("HIST", "BRCA"), width = 16,
#'               label = "Histology/\nMutation"),
#'   listing_col("ARM", width = 12, label = "Treatment Arm")
#' ))
#'
#' body <- build_listing(adsl, spec)
#' body
#'
#' @export
build_listing <- function(data, spec) {
  if (.is_rlistings_tbl(data)) {
    stop("`data` is an rlistings listing (`listing_df`), which rlistings has ",
         "already laid out -- its display columns, key-column suppression and ",
         "titles are baked in.  Pass it straight to `as_rtftables()` instead ",
         "of building a listing from it.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble; got '",
         paste(class(data), collapse = "/"), "'.", call. = FALSE)
  }
  if (!inherits(spec, "rtf_listing_spec")) {
    stop("`spec` must be a listing_spec(); got '",
         paste(class(spec), collapse = "/"), "'.", call. = FALSE)
  }
  if (!is.null(attr(data, "rtf_listing", exact = TRUE))) {
    stop("`data` has already been through build_listing() -- building it ",
         "again would join columns that are already joined.", call. = FALSE)
  }

  n <- nrow(data)
  k <- length(spec$cols)

  # -- wrap every cell -> one character vector of physical lines per cell ----
  # The header is resolved here too: an omitted `label` is derived from the
  # data's own `label` attributes, which only this side of the pipeline can
  # see (#366).  The resolved spec travels on the body, so as_rtftables() and
  # a second build never have to derive it again.
  lines <- vector("list", k)
  for (j in seq_len(k)) {
    cl   <- spec$cols[[j]]
    sepj <- if (is.null(cl$sep)) spec$sep else cl$sep
    lay  <- if (is.null(cl$layout)) spec$layout else cl$layout
    txt  <- .listing_combine(data, cl, sepj)
    spec$cols[[j]]$label <- .listing_resolve_label(data, cl, sepj, lay)
    lines[[j]] <- lapply(txt, function(s) spec$wrap(s, cl$width, sepj, lay))
  }

  # -- how tall is each record's block --------------------------------------
  nl <- integer(n)
  for (i in seq_len(n)) {
    nl[i] <- max(vapply(lines, function(L) length(L[[i]]), integer(1L)), 1L)
  }
  if (isTRUE(spec$blank_row)) nl <- nl + 1L
  total <- sum(nl)
  start <- if (n == 0L) integer(0) else cumsum(c(1L, nl))[seq_len(n)]

  # -- fill the printed columns ---------------------------------------------
  filled <- vector("list", k)
  for (j in seq_len(k)) {
    v <- rep("", total)
    # A key column marked `collapse_repeats` is CARRIED DOWN its record's
    # rows rather than padded with "", so as_rtftables(collapse_repeats = )
    # has a constant run to suppress -- and, because it suppresses per page,
    # after the split, the value reappears at the top of the next page.  Only
    # a single-line value is carried: a cell that already wraps has no one
    # value to repeat, and printing it in full is never wrong.
    down <- isTRUE(spec$cols[[j]]$collapse_repeats)
    for (i in seq_len(n)) {
      L <- lines[[j]][[i]]
      if (!length(L)) next
      body_rows <- if (isTRUE(spec$blank_row)) nl[i] - 1L else nl[i]
      if (down && length(L) == 1L && body_rows > 1L) {
        v[seq.int(start[i], length.out = body_rows)] <- L
      } else {
        v[seq.int(start[i], length.out = length(L))] <- L
      }
    }
    filled[[j]] <- v
  }

  # -- lay the output columns out, gutters included -------------------------
  layout <- .listing_layout(spec)
  out    <- vector("list", length(layout))
  for (p in seq_along(layout)) {
    it <- layout[[p]]
    out[[p]] <- if (identical(it$kind, "col")) filled[[it$index]] else
      rep("", total)
  }
  names(out) <- vapply(layout, function(it) it$name, character(1L))

  if (!is.null(spec$record_col)) {
    if (spec$record_col %in% names(out)) {
      stop(sprintf(paste0("The record column \"%s\" collides with a printed ",
                          "column of the same name; rename one."),
                   spec$record_col), call. = FALSE)
    }
    out[[spec$record_col]] <- if (n == 0L) integer(0) else rep(seq_len(n), nl)
  }

  body <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  rownames(body) <- NULL
  attr(body, "rtf_listing") <- spec
  body
}


# ── The as_rtftables() side ──────────────────────────────────────────────────
#
#  Resolve the `listing` argument against what the data already carries: a body
#  from `build_listing()` holds its own spec, so `as_rtftables()` must use it
#  without building anything a second time.

.resolve_listing_arg <- function(listing, x) {
  carried <- if (is.data.frame(x)) attr(x, "rtf_listing", exact = TRUE) else NULL
  if (!is.null(listing) && !inherits(listing, "rtf_listing_spec")) {
    stop("`listing` must be a listing_spec(); got '",
         paste(class(listing), collapse = "/"), "'.", call. = FALSE)
  }
  if (!is.null(listing) && !is.null(carried)) {
    stop("`data` was already built by build_listing(), and carries its own ",
         "listing spec.  Drop the `listing` argument, or pass the unbuilt ",
         "source data.", call. = FALSE)
  }
  if (!is.null(carried)) {
    return(list(spec = carried, build = FALSE))
  }
  if (!is.null(listing)) {
    return(list(spec = listing, build = TRUE))
  }
  NULL
}
