# ============================================================================
#  Font table -- several fonts, and per-element selection
# ============================================================================
#
#  The document used to carry exactly one font: the resource template was fixed
#  at a single entry and the renderer only ever read `font_table[[1]]$name`, so
#  there was no `\f1` to select and per-element choice was impossible.
#
#  The table is now variable length, and each element may name a font. The
#  mechanism mirrors the colour table, which already resolves a requested
#  colour to an index and declares it on the way out:
#
#      .collect_fonts()      every family the document mentions, in order
#      .build_font_index_map()   family -> \fN index
#      .build_font_table_rtf()   the {\fonttbl ...} group
#      .f_cmd_for()          the \fN a given element needs, or "" for the default
#
#  Index 0 is the document default, so an element that asks for nothing emits
#  nothing and the output is unchanged.

# Normalise a font_table (a list of list(name=), or a plain character vector)
# to a character vector of family names, first = the document default.
.font_names <- function(font_table) {
  if (is.null(font_table)) return(character(0))
  if (is.character(font_table)) return(font_table[nzchar(font_table)])
  nm <- vapply(font_table, function(f) {
    if (is.character(f) && length(f) == 1L) return(f)
    n <- if (is.character(f)) f else f$name
    if (.is_slotted(n)) n[["latin"]] else as.character(n %||% "")
  }, character(1L))
  nm[nzchar(nm)]
}

# The script slots a font may name.  RTF selects a font per run of characters
# by character class, so one family per class is what a run can carry:
#   latin      -- ASCII and high-ANSI (Latin, Greek, Cyrillic)  \loch / \hich
#   hansi      -- high-ANSI only, overriding `latin` there        \hich
#   east_asian -- CJK (double-byte)                               \dbch
#   complex    -- right-to-left / complex scripts                 \rtlch
.FONT_SLOTS <- c("latin", "hansi", "east_asian", "complex")

# A font argument is either one family name (today's meaning, today's bytes)
# or a named vector of families per slot.  A vector naming `latin` alone is
# the same font as that one string, so it is returned as one.
.check_font <- function(x, arg) {
  if (is.null(x)) return(NULL)
  nm <- names(x)
  if (!is.character(x) || !length(x) || anyNA(x) || !all(nzchar(x)) ||
      (is.null(nm) && length(x) != 1L)) {
    stop(sprintf(paste0("`%s` must be a single font family name, or a named ",
                        "vector of them per script slot, e.g. c(latin = ",
                        "\"Times New Roman\", east_asian = \"MS Mincho\")."),
                 arg), call. = FALSE)
  }
  if (is.null(nm)) return(x)
  if (anyNA(nm) || !all(nzchar(nm))) {
    stop(sprintf("`%s`: every font family must be named by its slot (%s).",
                 arg, paste(.FONT_SLOTS, collapse = ", ")), call. = FALSE)
  }
  bad <- setdiff(nm, .FONT_SLOTS)
  if (length(bad)) {
    stop(sprintf("`%s`: unknown font slot(s) %s; use %s.", arg,
                 paste0("\"", bad, "\"", collapse = ", "),
                 paste(.FONT_SLOTS, collapse = ", ")), call. = FALSE)
  }
  if (anyDuplicated(nm)) {
    stop(sprintf("`%s`: each font slot may be named only once.", arg),
         call. = FALSE)
  }
  if (!"latin" %in% nm) {
    stop(sprintf(paste0("`%s` must name a `latin` font: it is the family for ",
                        "ASCII text and the one every other slot falls back ",
                        "to."), arg), call. = FALSE)
  }
  x <- x[intersect(.FONT_SLOTS, nm)]
  if (length(x) == 1L) return(unname(x))
  x
}

.is_slotted <- function(font) !is.null(names(font)) && length(font) > 1L

# A font for print(): the family, or "latin = A, east_asian = B".
.font_label <- function(font) {
  if (!.is_slotted(font)) return(paste(font, collapse = ", "))
  paste0(names(font), " = ", font, collapse = ", ")
}

# The languages a slot can be tagged with: the font table charset its fonts
# are declared with, and the Windows LCID of the language control words.
.FONT_LANGS <- list(
  "ja"      = c(charset = 128L, lcid = 1041L),
  "zh-Hans" = c(charset = 134L, lcid = 2052L),
  "zh-Hant" = c(charset = 136L, lcid = 1028L),
  "ko"      = c(charset = 129L, lcid = 1042L),
  "ar"      = c(charset = 178L, lcid = 1025L),
  "he"      = c(charset = 177L, lcid = 1037L),
  "en"      = c(charset = 0L,   lcid = 1033L)
)

# `font_lang`: a language per slot, e.g. c(east_asian = "ja").  `hansi` takes
# the `latin` language, so it is not a slot of its own here.
.check_font_lang <- function(x, arg = "font_lang") {
  if (is.null(x)) return(NULL)
  nm   <- names(x)
  slot <- setdiff(.FONT_SLOTS, "hansi")
  if (!is.character(x) || !length(x) || anyNA(x) || is.null(nm) ||
      anyNA(nm) || !all(nzchar(nm))) {
    stop(sprintf(paste0("`%s` must be a named character vector of languages ",
                        "per font slot, e.g. c(east_asian = \"ja\")."), arg),
         call. = FALSE)
  }
  bad <- setdiff(nm, slot)
  if (length(bad) || anyDuplicated(nm)) {
    stop(sprintf("`%s`: names must be distinct slots among %s.", arg,
                 paste(slot, collapse = ", ")), call. = FALSE)
  }
  bad <- setdiff(x, names(.FONT_LANGS))
  if (length(bad)) {
    stop(sprintf("`%s`: unknown language(s) %s; use %s.", arg,
                 paste0("\"", bad, "\"", collapse = ", "),
                 paste(names(.FONT_LANGS), collapse = ", ")), call. = FALSE)
  }
  x[intersect(slot, nm)]
}

# Slot -> charset (0 when the slot has no language) and slot -> LCID (NA).
.slot_lang_info <- function(font_lang, what) {
  slot_lang <- c(latin = "latin", hansi = "latin", east_asian = "east_asian",
                 complex = "complex")
  vapply(slot_lang, function(s) {
    l <- if (is.null(font_lang)) NA_character_ else font_lang[s]
    if (!is.na(l)) return(.FONT_LANGS[[l]][[what]])
    if (what == "charset") 0L else NA_integer_
  }, integer(1L))
}

# A declared font_table: each entry's `name` (or the entry itself, when it is
# a string) may be a slot vector, so check it as any `font =` is checked.
.check_font_table <- function(font_table) {
  if (!is.list(font_table)) return(font_table)
  lapply(font_table, function(f) {
    if (is.list(f) && is.character(f$name) && !is.null(names(f$name))) {
      f$name <- .check_font(f$name, "font_table")
    } else if (is.character(f) && !is.null(names(f))) {
      f <- .check_font(f, "font_table")
    }
    f
  })
}

# A font table entry is identified by family AND charset: the same family
# tagged Japanese and untagged are two entries.  The key of an ANSI entry is
# the bare family, so every untagged document keys and writes as it always
# has.  (`;` cannot occur in an RTF font name: it ends the entry.)
.font_entry_key <- function(name, charset = 0L) {
  ifelse(charset == 0L, name, paste0(name, ";", charset))
}

.split_font_key <- function(key) {
  m <- regmatches(key, regexec("^(.*);([0-9]+)$", key))[[1L]]
  if (length(m)) list(name = m[[2L]], charset = as.integer(m[[3L]]))
  else list(name = key, charset = 0L)
}

# The family each slot of an element's font resolves to.  A slot the element
# leaves out falls back to the document default's slot when the default names
# one, else to the element's own `latin`; `hansi` falls back to `latin`.  A
# single string in a slotted document is that document's slots with `latin`
# replaced.
.resolve_slots <- function(font, default = NULL) {
  dflt <- if (.is_slotted(default)) default else NULL
  if (!.is_slotted(font)) {
    font <- if (is.null(dflt)) c(latin = unname(font))
            else c(latin = unname(font), dflt[setdiff(names(dflt),
                                                      c("latin", "hansi"))])
  }
  out <- stats::setNames(rep(font[["latin"]], length(.FONT_SLOTS)), .FONT_SLOTS)
  for (s in c("east_asian", "complex")) {
    if (!is.null(dflt) && !is.na(dflt[s])) out[[s]] <- dflt[[s]]
  }
  out[names(font)] <- font
  if (is.na(font["hansi"])) out[["hansi"]] <- font[["latin"]]
  out
}

# The table entry keys a font needs: one per slot it uses, each under its
# slot's charset.
.font_spec_keys <- function(font, charsets, default = NULL) {
  if (!.is_slotted(font) && !.is_slotted(default)) {
    return(.font_entry_key(unname(font), charsets[["latin"]]))
  }
  r <- .resolve_slots(font, default)
  unname(.font_entry_key(r, charsets[names(r)]))
}

# Every family the rendered document needs: the declared table first (its first
# entry is the default), then any family an element asked for that is not
# already declared.
.collect_fonts <- function(declared, requested, font_lang = NULL) {
  specs <- .font_specs(declared)
  if (length(specs) == 0L) specs <- list(.opt("rtfreporter.font"))
  charsets <- .slot_lang_info(font_lang, "charset")
  default  <- specs[[1L]]
  if (is.character(requested) && is.null(names(requested))) {
    requested <- as.list(requested)
  } else if (!is.list(requested)) {
    requested <- list(requested)
  }
  requested <- Filter(function(f) length(f) && !anyNA(f) && all(nzchar(f)),
                      requested)
  keys <- c(.font_spec_keys(default, charsets),
            unlist(lapply(c(specs[-1L], requested), .font_spec_keys,
                          charsets = charsets, default = default),
                   use.names = FALSE))
  unique(keys)
}

# The declared table as a list of font specs (a name, or a slot vector).
.font_specs <- function(font_table) {
  if (is.null(font_table)) return(list())
  if (is.character(font_table)) {
    return(as.list(font_table[nzchar(font_table)]))
  }
  out <- lapply(font_table, function(f) {
    if (is.character(f)) f else f$name
  })
  Filter(function(f) is.character(f) && length(f) && all(nzchar(f)), out)
}

# family -> \fN index, 0-based, matching the emitted table.
.build_font_index_map <- function(fonts) {
  if (length(fonts) == 0L) return(list())
  stats::setNames(as.list(seq_along(fonts) - 1L), fonts)
}

# The {\fonttbl ...} group for the collected families.
#   \fnil\fcharset0 -- unknown family, ANSI charset: deterministic glyph
#   mapping across viewers without claiming a specific family.
.build_font_table_rtf <- function(fonts) {
  if (length(fonts) == 0L) fonts <- .opt("rtfreporter.font")
  entries <- vapply(seq_along(fonts), function(i) {
    k <- .split_font_key(fonts[[i]])
    sprintf("{\\f%d\\fnil\\fcharset%d %s;}", i - 1L, k$charset,
            .rtf_escape(k$name))
  }, character(1L))
  paste0("{\\fonttbl", paste(entries, collapse = ""), "}")
}

# Everything the renderer needs to select fonts: the table entries, the
# key -> index map, and (as attributes of the map, so every renderer that is
# already handed the map gets them) the slot charsets and LCIDs and the run
# words an element that names no font writes.  For a single-string default
# that is "" -- the document's \deff0 already selects it -- and the output is
# the same as before slots existed.
.font_render_ctx <- function(declared, requested, font_lang = NULL) {
  specs <- .font_specs(declared)
  if (length(specs) == 0L) specs <- list(.opt("rtfreporter.font"))
  default <- specs[[1L]]
  fonts <- .collect_fonts(declared, requested, font_lang)
  map   <- .build_font_index_map(fonts)
  attr(map, "charsets") <- .slot_lang_info(font_lang, "charset")
  attr(map, "lcids")    <- .slot_lang_info(font_lang, "lcid")
  attr(map, "default")  <- default
  attr(map, "default_cmd") <- if (.is_slotted(default)) {
    .slot_cmd(.resolve_slots(default), map)
  } else ""
  list(fonts = fonts, map = map, default = default)
}

# The run words for a font resolved per slot.  The form Word itself writes:
#   \rtlch\af<C>\ltrch\hich\af<H>\dbch\af<E>\loch\f<L>
# \af is the font associated with the character class just named, and the
# closing \loch\f leaves low-ANSI current, so the plain \f of the run is the
# Latin font a reader that ignores the classes falls back to.  The language
# words follow their slot's font when `font_lang` names one.
.slot_cmd <- function(slots, map) {
  cs   <- attr(map, "charsets", exact = TRUE) %||% .slot_lang_info(NULL, "charset")
  lcid <- attr(map, "lcids", exact = TRUE)    %||% .slot_lang_info(NULL, "lcid")
  idx  <- function(s) {
    i <- match(.font_entry_key(slots[[s]], cs[[s]]), names(map))
    if (is.na(i)) 0L else as.integer(map[[i]])
  }
  lw <- function(words, s) {
    if (is.na(lcid[[s]])) "" else paste0(words, lcid[[s]], collapse = "")
  }
  paste0("\\rtlch\\af", idx("complex"), lw("\\alang", "complex"),
         "\\ltrch\\hich\\af", idx("hansi"),
         "\\dbch\\af", idx("east_asian"), lw("\\langfe", "east_asian"),
         "\\loch\\f", idx("latin"), lw(c("\\lang", "\\langnp"), "latin"))
}

# The document-level words a slotted default adds to the RTF header: the
# associated default (\adeff), the style-sheet default fonts per class
# (\stshfdbch / \stshfloch / \stshfhich / \stshfbi) and the default languages
# (\deflangfe / \adeflang; \deflang replaces the fixed 1033 when `font_lang`
# names a latin language).  An unslotted, untagged document gets its header
# back unchanged.
.font_header_rtf <- function(header_open, map) {
  default <- attr(map, "default", exact = TRUE)
  lcid    <- attr(map, "lcids", exact = TRUE)
  if (is.null(lcid)) return(header_open)
  extra <- ""
  if (.is_slotted(default)) {
    cs <- attr(map, "charsets", exact = TRUE)
    r  <- .resolve_slots(default)
    i  <- function(s) {
      as.integer(map[[match(.font_entry_key(r[[s]], cs[[s]]), names(map))]])
    }
    extra <- sprintf("\\adeff%d\\stshfdbch%d\\stshfloch%d\\stshfhich%d\\stshfbi%d",
                     i("complex"), i("east_asian"), i("latin"), i("hansi"),
                     i("complex"))
  }
  if (!is.na(lcid[["latin"]])) {
    header_open <- sub("\\deflang1033", paste0("\\deflang", lcid[["latin"]]),
                       header_open, fixed = TRUE)
  }
  if (!is.na(lcid[["east_asian"]])) {
    extra <- paste0(extra, "\\deflangfe", lcid[["east_asian"]])
  }
  if (!is.na(lcid[["complex"]])) {
    extra <- paste0(extra, "\\adeflang", lcid[["complex"]])
  }
  if (!nzchar(extra)) return(header_open)
  sub("\\uc1", paste0(extra, "\\uc1"), header_open, fixed = TRUE)
}

# The \fN an element needs.  "" when it wants the document default (index 0),
# so an element that changes nothing emits nothing.
.f_cmd_for <- function(font, font_index_map) {
  default <- attr(font_index_map, "default", exact = TRUE)
  if (is.null(font) || !length(font) || !all(nzchar(font))) {
    return(attr(font_index_map, "default_cmd", exact = TRUE) %||% "")
  }
  if (.is_slotted(font) || .is_slotted(default)) {
    return(.slot_cmd(.resolve_slots(font, default), font_index_map))
  }
  cs <- attr(font_index_map, "charsets", exact = TRUE)
  key <- .font_entry_key(font, if (is.null(cs)) 0L else cs[["latin"]])
  # `[[` on an absent name errors, and a renderer may legitimately be handed an
  # empty map (a direct .render_* call in a test), so look the name up by match
  i <- match(key, names(font_index_map))
  if (is.na(i)) return("")
  idx <- font_index_map[[i]]
  if (is.null(idx) || identical(as.integer(idx), 0L)) return("")
  paste0("\\f", as.integer(idx))
}

# Every font an assembled report asks for, so the table can declare them all
# before the body is written.
.collect_report_fonts <- function(report) {
  # A header/footer may still be the legacy named character vector rather than
  # an rtf_header() list, so only reach for `$font` where there is one to reach.
  # `[["font"]]`, not `$font`: a style list that sets only
  # `font_size_half_points` would otherwise partial-match the size (#496).
  fld <- function(x) if (is.list(x)) x[["font"]] else NULL
  doc <- report$document
  out <- list()
  for (p in report$pages) {
    ct <- p$content
    if (inherits(ct, "rtftable")) out <- c(out, list(fld(ct)))
  }
  for (s in report$sections) {
    if (!is.list(s)) next
    out <- c(out, list(fld(s$header)), list(fld(s$footer)))
  }
  # A page's later segments (rtf_tables(page_break = FALSE)) go last, after
  # everything above: where they add nothing new the table is the one it
  # always was, and where they do, their fonts are declared (once a font
  # only a later segment asked for was left out).
  for (p in report$pages) {
    for (sg in p$segments[-1L]) {
      if (inherits(sg[["content"]], "rtftable")) out <- c(out, list(fld(sg[["content"]])))
    }
  }
  out <- Filter(Negate(is.null), c(list(fld(doc$title_style)),
                                   list(fld(doc$footnote_style)), out))
  # A slot vector must stay whole; a report of single names stays the plain
  # character vector it always was.
  if (any(vapply(out, .is_slotted, logical(1L)))) out
  else unlist(out, use.names = FALSE)
}
