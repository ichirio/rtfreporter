# Validation tests: the title and footnote blocks -- rtf_titles() /
# rtf_footnotes() with rtf_style()'s titles / footnotes sections, the block
# formats and widths -- a pairwise set and decision tables, every case checked
# on the RTF written.  Helpers prefixed .vt_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (rtf_titles(), rtf_footnotes(),
#  rtf_style()) and the code (R/pipe-composition.R rtf_titles(),
#  rtf_footnotes(); R/blocks.R; R/rtf_style.R):
#
#    rows      a block is a character vector (a row per entry, "" a blank
#              row) or a list of rows.  A title row may be a styled
#              list(text =, align =, bold =, italic =, underline =,
#              row_height_twips =); title rows default to centred and bold.
#              A footnote row may be c(l = , c = , r = ) (cells placed left,
#              centre, right whatever the block's align; the table form
#              only) or list(columns = , row_height_twips = ); footnote rows
#              default to left.
#    align     the row's own > rtf_titles(align = ) / rtf_footnotes(align =)
#              > rtf_style(<block> = list(align = )) > the default.
#    format    rtf_style(title_format = , footnote_format = ): "text"
#              (paragraphs, \pard) or "table" (a one-column table, \trowd).
#              Defaults "text" for the title, "table" for the footnote.
#    width     rtf_style(title_width = , footnote_width = ): "content" (the
#              table's width, the default), "page" (the writable width), a
#              fraction in (0, 1] of the writable width, or twips (a whole
#              number above 1); the table form only (\cellx of the block).
#    height    a row's row_height_twips > the block's argument > the
#              section's > the height its font size implies; the table form
#              only (\trrh).
#    markup    the block's argument > the section's > the document's
#              (baseline "script": ^{2} is written {\super 2}).
#    tokens    rtf_document(tokens = ) ({STUDY}) and the page tokens
#              ({AUTO_PAGE} is \chpgn) are filled in both blocks.
#    pages     one block given for two contents is written on both.
#
#  Fixed: a 2-column table of 2000 + 3000 twips (content width 5000) on the
#  default page (letter landscape, 0.75 in margins: writable width 13680).
#  Values chosen apart: align arg "right", section "left" (titles) /
#  "center" (footnotes); height arg 400, section 380, row 500, implied 230;
#  width 0.5 -> 6840, twips 3000.
#
#  (A) FT-DC290, pairwise.  Factors and levels:
#    block   titles, footnotes
#    form    text, table
#    rows    plain (3 rows, the 2nd blank), styled (a styled title row with
#            align = "center", bold = FALSE, italic = TRUE, row height 500;
#            a footnote row list(columns = c(l = , r = ), row_height_twips =
#            500))
#    align   none, arg, section, both (arg wins)
#    width   content, page, 0.5, 3000
#    height  none, arg, section, both (arg wins)
#    markup  inherit, arg "none", section "none"
#    tokens  none, own {STUDY}, page {AUTO_PAGE}
#    pages   1, 2 (one block for both)
#  Constraint: a c(l = , r = ) footnote row needs the table form (FR-IX308),
#  so footnotes x styled x text is not a valid combination.  8064 valid
#  combinations; .allpairs() picks the rows that cover every pair of levels.
#  Oracle per row of the block: its text (tokens filled), \pard or \trowd,
#  \qc / \ql / \qr, \b (titles), \i\ul (the styled title row), \cellx (the
#  width) and \trrh (the height) in the table form, the markup of "^{2}";
#  the block written once per page.  FR-DC189 FR-DC192..195 FR-DC206
#  FR-DC209..211 FR-DC283 FR-DC303 FR-DC305 FR-DC311 FR-DC315 FR-TB176
#  FR-DC357.
#
#  (B) FT-DC291, decision table: the block width.
#    width source                       table form       text form
#    none                               content 5000     no \cellx
#    rtf_style "page"                   13680            no \cellx
#    rtf_style 0.25                     3420             no \cellx
#    rtf_style 1 (a fraction, not twips) 13680           no \cellx
#    rtf_style 2 (twips)                2                no \cellx
#    rtf_style 7000 (wider than content) 7000            no \cellx
#    option rtfreporter.<x>_width "page" 13680           no \cellx
#    option "page" + rtf_style 3000     3000             no \cellx
#  x block (titles, footnotes) x form: 32 rows.  FR-DC311 FR-DC315 FR-DC357.
#
#  (C) FT-DC292, decision table: where a footnote row lands.
#    row                   block align   cells written
#    "s"                   none          \ql s
#    "s"                   center        \qc s
#    "s"                   right         \qr s
#    c(l = "a", r = "b")   right         \ql a | \qr b
#    c(c = "m")            left          \qc m
#    c(l="a",c="m",r="b")  center        \ql a | \qc m | \qr b
#  The cells of a row share the block's width, the last ending at its right
#  edge (three cells over 5000 twips used to end at 4998).  FR-DC206 FR-DC211
#  FR-DC358.
#
#  Not tested here: the font size and font precedence (FT-DC248..254,
#  test-validation-style-precedence.R).  The rtf_titles() example's
#  "{HALF_BLANK_ROW}" is no token (it was written as it is); the example now
#  uses "" for a blank row.
# ---------------------------------------------------------------------------

.VT_W <- c(content = 5000L, page = 13680L)
.VT_ALIGN <- list(titles = c(def = "c", sec = "l", arg = "r"),
                  footnotes = c(def = "l", sec = "c", arg = "r"))
.VT_WORD <- c(l = "left", c = "center", r = "right")

.vt_table <- function() {
  rtftable(data.frame(A = c("a1", "a2"), B = c("b1", "b2")),
           column_widths_twips = c(2000L, 3000L))
}

# The rows of one case: a list of rows to give and the rows expected, each
# with its tag (how it is found in the file), cells and own settings.
.vt_rows <- function(block, rows, tokens) {
  tag <- if (block == "titles") "TTL" else "FNT"
  tok <- switch(tokens, none = "", own = " {STUDY}", page = " p{AUTO_PAGE}")
  tok_out <- switch(tokens, none = "", own = " XYZ-001", page = " p\\chpgn ")
  first <- paste0(tag, "1 x^{2}", tok)
  if (rows == "plain") {
    give <- c(first, "", paste0(tag, "3"))
    want <- list(list(tag = paste0(tag, "1"), text = paste0(tag, "1 x"), tok = tok_out),
                 list(tag = paste0(tag, "3"), text = paste0(tag, "3")))
  } else if (block == "titles") {
    give <- list(first, list(text = "TTL2 s", align = "center", bold = FALSE,
                             italic = TRUE, underline = TRUE,
                             row_height_twips = 500L))
    want <- list(list(tag = "TTL1", text = "TTL1 x", tok = tok_out),
                 list(tag = "TTL2", text = "TTL2 s", align = "c", bold = FALSE,
                      italic = TRUE, height = 500L))
  } else {
    give <- list(first, list(columns = c(l = "FNT2L", r = "FNT2R"),
                             row_height_twips = 500L))
    want <- list(list(tag = "FNT1", text = "FNT1 x", tok = tok_out),
                 list(tag = "FNT2L", cells = c(l = "FNT2L", r = "FNT2R"),
                      height = 500L))
  }
  list(give = give, want = want)
}

# Build one case and return the RTF.
.vt_build <- function(cs, r) {
  b <- cs$block
  al <- .VT_ALIGN[[b]]
  shared <- list()
  shared[[if (b == "titles") "title_format" else "footnote_format"]] <- cs$form
  if (cs$width != "content") {
    w <- switch(cs$width, page = "page", half = 0.5, twips = 3000L)
    shared[[if (b == "titles") "title_width" else "footnote_width"]] <- w
  }
  sec <- list()
  if (cs$align %in% c("section", "both")) sec$align <- .VT_WORD[[al[["sec"]]]]
  if (cs$height %in% c("section", "both")) sec$row_height_twips <- 380L
  if (cs$markup == "section") sec$markup <- "none"
  if (length(sec)) shared[[b]] <- sec
  # the other block is given its default form, so its rows are not read
  doc <- rtf_document(style = do.call(rtf_style, shared),
                      tokens = list(STUDY = "XYZ-001"))
  n <- as.integer(cs$pages)
  doc <- rtf_tables(doc, rep(list(.vt_table()), n))
  args <- list(doc, if (is.list(r$give)) list(r$give) else r$give)
  if (cs$align %in% c("arg", "both")) args$align <- .VT_WORD[[al[["arg"]]]]
  if (cs$height %in% c("arg", "both")) args$row_height_twips <- 400L
  if (cs$markup == "arg") args$markup <- "none"
  doc <- do.call(if (b == "titles") rtf_titles else rtf_footnotes, args)
  .render_to_string(doc)
}

.vt_lines <- function(txt, tag) {
  ln <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  ln[grepl(paste0(tag, "[^0-9A-Z]"), ln) | endsWith(ln, tag)]
}

# Check one case; returns a character vector of problems (empty when right).
.vt_check <- function(cs) {
  r <- .vt_rows(cs$block, cs$rows, cs$tokens)
  txt <- .vt_build(cs, r)
  al <- .VT_ALIGN[[cs$block]]
  blk_align <- if (cs$align %in% c("arg", "both")) al[["arg"]] else
    if (cs$align == "section") al[["sec"]] else al[["def"]]
  blk_h <- if (cs$height %in% c("arg", "both")) 400L else
    if (cs$height == "section") 380L else 230L
  width <- switch(cs$width, content = 5000L, page = 13680L, half = 6840L, twips = 3000L)
  markup_none <- cs$markup != "inherit"
  bad <- character(0)
  for (w in r$want) {
    ln <- .vt_lines(txt, w$tag)
    if (length(ln) != as.integer(cs$pages)) {
      bad <- c(bad, sprintf("%s: %d lines, want %s", w$tag, length(ln), cs$pages))
      next
    }
    l <- ln[1L]
    tbl <- cs$form == "table"
    if (startsWith(l, "\\trowd") != tbl) bad <- c(bad, paste(w$tag, "form"))
    if (tbl) {
      h <- if (!is.null(w$height)) w$height else blk_h
      if (!grepl(sprintf("\\trrh%d\\", h), l, fixed = TRUE)) bad <- c(bad, paste(w$tag, "height"))
      if (!grepl(sprintf("\\cellx%d\\", width), l, fixed = TRUE)) bad <- c(bad, paste(w$tag, "width"))
    } else if (grepl("\\cellx", l, fixed = TRUE) || grepl("\\trrh", l, fixed = TRUE)) {
      bad <- c(bad, paste(w$tag, "text form has a cell"))
    }
    if (!is.null(w$cells)) {
      for (s in names(w$cells)) {
        pat <- sprintf("\\q%s\\li0\\ri0 %s\\cell", s, w$cells[[s]])
        if (!grepl(pat, l, fixed = TRUE)) bad <- c(bad, paste(w$tag, "cell", s))
      }
      next
    }
    a <- if (!is.null(w$align)) w$align else blk_align
    if (!grepl(sprintf("\\q%s\\li0\\ri0 ", a), l, fixed = TRUE)) bad <- c(bad, paste(w$tag, "align"))
    bold <- cs$block == "titles" && !isFALSE(w$bold)
    if (grepl("\\b ", l, fixed = TRUE) != bold) bad <- c(bad, paste(w$tag, "bold"))
    if (isTRUE(w$italic) && !grepl("\\i \\ul ", l, fixed = TRUE)) bad <- c(bad, paste(w$tag, "italic"))
    if (!grepl(w$text, l, fixed = TRUE)) bad <- c(bad, paste(w$tag, "text"))
    if (!is.null(w$tok)) {
      sup <- if (markup_none) "^\\{2\\}" else "{\\super 2}"
      if (!grepl(paste0(w$text, sup, w$tok), l, fixed = TRUE)) bad <- c(bad, paste(w$tag, "markup/tokens"))
    }
  }
  bad
}

test_that("FT-DC290 [FR-DC189 FR-DC195 FR-DC206 FR-DC211 FR-DC283 FR-DC303 FR-DC305 FR-DC311 FR-TB176 FR-DC357]: pairwise -- block x form x rows x align x width x height x markup x tokens x pages", {
  cand <- expand.grid(block = c("titles", "footnotes"), form = c("text", "table"),
                      rows = c("plain", "styled"),
                      align = c("none", "arg", "section", "both"),
                      width = c("content", "page", "half", "twips"),
                      height = c("none", "arg", "section", "both"),
                      markup = c("inherit", "arg", "section"),
                      tokens = c("none", "own", "page"), pages = c("1", "2"),
                      stringsAsFactors = FALSE)
  cand <- cand[!(cand$block == "footnotes" & cand$rows == "styled" & cand$form == "text"), ]
  expect_identical(nrow(cand), 8064L)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 40L)
  set <- ap$rows
  for (i in seq_len(nrow(set))) {
    cs <- as.list(set[i, ])
    expect_identical(.vt_check(cs), character(0),
                     label = paste(unlist(cs), collapse = " / "))
  }
})

test_that("FT-DC291 [FR-DC311 FR-DC315 FR-DC357]: decision table -- the block width from rtf_style(), the option and the default, in the table form only", {
  rows <- list(
    list(style = NULL,    opt = NULL,   want = 5000L),
    list(style = "page",  opt = NULL,   want = 13680L),
    list(style = 0.25,    opt = NULL,   want = 3420L),
    list(style = 1,       opt = NULL,   want = 13680L),
    list(style = 2L,      opt = NULL,   want = 2L),
    list(style = 7000L,   opt = NULL,   want = 7000L),
    list(style = NULL,    opt = "page", want = 13680L),
    list(style = 3000L,   opt = "page", want = 3000L))
  n <- 0L
  for (b in c("titles", "footnotes")) {
    key <- if (b == "titles") "title" else "footnote"
    for (form in c("table", "text")) {
      for (i in seq_along(rows)) {
        rw <- rows[[i]]
        opts <- list()
        if (!is.null(rw$opt)) opts[[paste0("rtfreporter.", key, "_width")]] <- rw$opt
        old <- options(opts)
        sty <- list()
        sty[[paste0(key, "_format")]] <- form
        if (!is.null(rw$style)) sty[[paste0(key, "_width")]] <- rw$style
        doc <- rtf_document(style = do.call(rtf_style, sty)) |>
          rtf_tables(list(.vt_table()))
        doc <- if (b == "titles") rtf_titles(doc, "TTL1 w") else rtf_footnotes(doc, "FNT1 w")
        txt <- .render_to_string(doc)
        options(old)
        l <- .vt_lines(txt, if (b == "titles") "TTL1" else "FNT1")
        lab <- sprintf("%s / %s / row %d", b, form, i)
        expect_length(l, 1L)
        if (form == "table") {
          expect_true(grepl(sprintf("\\cellx%d\\", rw$want), l, fixed = TRUE), label = lab)
        } else {
          expect_false(grepl("\\cellx", l, fixed = TRUE), label = lab)
        }
        n <- n + 1L
      }
    }
  }
  expect_identical(n, 32L)
})

test_that("FT-DC292 [FR-DC206 FR-DC211 FR-DC358]: decision table -- where a footnote row's cells land, with the block's align", {
  rows <- list(
    list(row = "FQ1",                            align = NULL,     want = c(l = "FQ1")),
    list(row = "FQ2",                            align = "center", want = c(c = "FQ2")),
    list(row = "FQ3",                            align = "right",  want = c(r = "FQ3")),
    list(row = c(l = "FQ4a", r = "FQ4b"),           align = "right",  want = c(l = "FQ4a", r = "FQ4b")),
    list(row = c(c = "FQ5m"),                     align = "left",   want = c(c = "FQ5m")),
    list(row = c(l = "FQ6a", c = "FQ6m", r = "FQ6b"), align = "center", want = c(l = "FQ6a", c = "FQ6m", r = "FQ6b")))
  for (i in seq_along(rows)) {
    rw <- rows[[i]]
    doc <- rtf_document() |> rtf_tables(list(.vt_table())) |>
      rtf_footnotes(list(list(rw$row, "FNTEND")), align = rw$align)
    ln <- strsplit(.render_to_string(doc), "\n", fixed = TRUE)[[1L]]
    l <- ln[grepl(rw$want[[1L]], ln, fixed = TRUE)]
    expect_length(l, 1L)
    for (s in names(rw$want)) {
      expect_true(grepl(sprintf("\\q%s\\li0\\ri0 %s\\cell", s, rw$want[[s]]), l, fixed = TRUE),
                  label = sprintf("row %d slot %s", i, s))
    }
    # the cells share the block's width: the last \cellx is the content's
    cx <- as.integer(sub("\\\\cellx", "", regmatches(l, gregexpr("\\\\cellx[0-9]+", l))[[1L]]))
    expect_identical(cx[length(cx)], 5000L, label = sprintf("row %d width", i))
    expect_identical(length(cx), length(rw$want), label = sprintf("row %d cells", i))
  }
})
