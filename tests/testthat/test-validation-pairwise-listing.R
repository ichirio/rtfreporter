# Validation tests: the listing builders -- listing_spec() / listing_col() /
# build_listing() / listing_wrap() / listing_wrap_code() / listing_code() with
# fit_listing_widths() -- a pairwise set checked on the body, the fitted spec,
# the RTF written and the code written out, and a decision table of the key
# column (collapse_repeats).  Helpers prefixed .vl_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (listing_col(), listing_spec(),
#  build_listing(), listing_wrap(), fit_listing_widths(), listing_code()) and
#  the code (R/listing.R, R/listing_fit.R):
#
#    wrap      a cell (its vars joined with the separator, missing values
#              left out) is broken by the listing's rule: after the separator
#              (every one under "stack", where the line runs out under
#              "flow"), then at a word boundary, then hard-split, so every
#              line fits the column's width (a full-width glyph counts as
#              two); no width, no wrap.  The same rule lays out a derived
#              header.  listing_spec(wrap = listing_wrap) and the rule
#              written out by listing_wrap_code() give the same lines as the
#              default; a custom rule's lines are taken as they come.
#    record    every column of a record is padded to the tallest, and a blank
#              row ends the record (blank_row, TRUE in "multiline").
#    key       collapse_repeats = TRUE carries a one-line value down every
#              row of its record (the blank row too); as_rtftables() then
#              blanks the repeats per page.
#    widths    fit_listing_widths() fills the widths not set: the fitted
#              widths and the gutters add up to the budget, floor(writable
#              twips / the width of "0" in the font), every set width is
#              kept, no fitted width under min_width.  A fitted spec carries
#              rel_width = width and the header frozen unwrapped.
#    render    as_rtftables(listing = ) spans the writable width (the last
#              \cellx), each column's share its rel_width (the width, or the
#              longest header line with none), gutters spacer_rel_width;
#              each column's cells take its align (the column's, else the
#              listing's); the page's size and orientation and the style's
#              font and size are written; a non-ASCII glyph as \uN?.
#    code      listing_code() parsed and run gives back the spec, every
#              column's settings and the listing's, the wrap rule apart: a
#              custom wrap (listing_wrap included) is named in a NOTE line.
#
#  Data: three records -- ID (S-01, S-01, S-02), TERM + LOC joined in one
#  column (TERM labelled "Reported Term"), AGE + SEX joined in a third.
#
#  (A) FT-LS160, pairwise.  Factors and levels:
#    widths   none (no width: no wrap), fixed (8, 14, 6), fit
#             (fit_listing_widths() on the page and font), mixed (the first
#             fixed at 8, the others fitted), min (fitted with min_width =
#             12)
#    wrap     default (NULL), listing_wrap, code (listing_wrap_code()
#             evaluated), custom (listing_wrap() then toupper())
#    sep      "/", " | " (several characters), "." (a regex character)
#    layout   stack, flow
#    key      collapse_repeats FALSE, TRUE on ID
#    align    template (left), listing "center", column (the third "right")
#    page     letter landscape, A4 portrait, letter portrait with 1 in
#             side margins
#    font     Courier New 9 pt, Courier New 8 pt, Arial 9 pt
#    data     ascii, long (a 28-character word), nonascii (Japanese and
#             accented Latin)
#    spacer   on (rel 1), half (rel 0.5), off
#  No constraints.  The product is 58,320 rows; the candidates are every
#  97th of them (expand.grid order), checked to hold every pair of levels,
#  and .allpairs() picks the rows covering every pair.  Oracle: each cell's
#  lines (listing_wrap() on the joined text, upper-cased for custom; every
#  line within the width; no character lost), the record heights, the
#  gutters, the key column carried or not, the derived header; the widths
#  (sum and gutters = budget, set ones kept, min_width); in the RTF the paper
#  and font, the \cellx of the header row (last = writable width, each edge
#  within 2 twips of its share), each column's \q, "S-01" printed once
#  (key) or twice, the non-ASCII as \uN?; listing_code() run back.
#  FR-LS102 FR-LS103 FR-LS104 FR-LS105 FR-LS122 FR-LS124 FR-LS127 FR-LS128
#  FR-LS129 FR-LS145 FR-LS149 FR-LS150 FR-LS153 FR-LS154 FR-LS156 FR-LS171
#  FR-LS172 FR-LS173 FR-LS174 FR-LS175 FR-LS177 FR-LS182 FR-LS189 FR-LS191
#  FR-LS195 FR-LS206 FR-LS207 FR-LS208 FR-LS211 FR-LS212 FR-LS213 FR-LS214
#  FR-LS219 FR-LS253.
#
#  (B) FT-LS161, decision table: the key column.
#    key    cell lines  blank_row  other column  key column rows
#    FALSE  1           TRUE       1 line        value, ""
#    TRUE   1           TRUE       1 line        value, value
#    TRUE   1           FALSE      1 line        value
#    TRUE   1           FALSE      2 lines       value, value
#    TRUE   2           TRUE       1 line        line 1, line 2, ""
#    FALSE  2           FALSE      2 lines       line 1, line 2
#  Then rendered with max_rows so the second record of a run starts a new
#  page: the key's value is printed once per run on a page (a repeat blanked
#  to NA), and again at the top of the next page.  FR-LS104 FR-LS156
#  FR-LS157.
# ---------------------------------------------------------------------------

.VL_PAGE <- list(
  letter_land = list(page = list(paper_size = "letter", orientation = "landscape"),
                     paper = c(15840L, 12240L), land = TRUE, w = 13680L),
  a4_port     = list(page = list(paper_size = "A4", orientation = "portrait"),
                     paper = c(11905L, 16838L), land = FALSE, w = 9745L),
  letter_1in  = list(page = list(paper_size = "letter", orientation = "portrait",
                                 margin_left_in = 1, margin_right_in = 1),
                     paper = c(12240L, 15840L), land = FALSE, w = 9360L))
.VL_FONT <- list(courier18 = list(font = "Courier New", size = 18L),
                 courier16 = list(font = "Courier New", size = 16L),
                 arial18   = list(font = "Arial", size = 18L))

.vl_data <- function(kind) {
  term2 <- switch(kind,
                  ascii = "UPPER RESPIRATORY TRACT INFECTION",
                  long = "IMMUNOHISTOCHEMISTRYREACTION",
                  nonascii = "頭痛 (ずつう) café")
  d <- data.frame(ID = c("S-01", "S-01", "S-02"),
                  TERM = c("HEADACHE", term2, "RASH"),
                  LOC = c("FRONTAL", NA, "LEFT ARM"),
                  AGE = c("34", "34", "51"),
                  SEX = c("F", "F", if (kind == "nonascii") "男" else "M"),
                  stringsAsFactors = FALSE)
  attr(d$TERM, "label") <- "Reported Term"
  d
}

# The wrap rule of a case, and the oracle of its lines.
.vl_wrap_fn <- function(kind) {
  switch(kind,
         default = NULL,
         listing_wrap = listing_wrap,
         code = {
           env <- new.env(parent = environment(listing_wrap))
           eval(parse(text = listing_wrap_code("vl_wrap")), envir = env)
           env$vl_wrap
         },
         custom = function(text, width, sep, layout) {
           toupper(listing_wrap(text, width, sep, layout))
         })
}
.vl_lines <- function(text, width, sep, layout, wrap) {
  out <- listing_wrap(text, width, sep, layout)
  if (wrap == "custom") toupper(out) else out
}
.vl_join <- function(v, sep) {
  v <- trimws(v[!is.na(v)])
  paste(v[nzchar(v)], collapse = sep)
}

.vl_spec <- function(cs, d) {
  w <- switch(cs$widths, fixed = c(8L, 14L, 6L), mixed = c(8L, NA, NA),
              c(NA, NA, NA))
  wd <- function(j) if (is.na(w[j])) NULL else w[j]
  cols <- list(
    listing_col("ID", width = wd(1L), collapse_repeats = cs$key == "TRUE"),
    listing_col(c("TERM", "LOC"), width = wd(2L)),
    listing_col(c("AGE", "SEX"), width = wd(3L),
                align = if (cs$align == "column") "right"))
  spec <- listing_spec(cols, sep = cs$sep, layout = cs$layout,
                       align = if (cs$align == "center") "center",
                       spacer = cs$spacer != "off",
                       spacer_rel_width = if (cs$spacer == "half") 0.5,
                       wrap = .vl_wrap_fn(cs$wrap))
  if (cs$widths %in% c("fit", "mixed", "min")) {
    pg <- .VL_PAGE[[cs$page]]; ft <- .VL_FONT[[cs$font]]
    spec <- fit_listing_widths(d, spec, page = do.call(rtf_page, pg$page),
                               font = ft$font, font_size_half_points = ft$size,
                               min_width = if (cs$widths == "min") 12L else 6L)
  }
  spec
}

# The \cellx and \q of every row of an RTF table, one list per row.
.vl_rows <- function(txt) {
  rows <- regmatches(txt, gregexpr("\\\\trowd.*?\\\\row", txt))[[1L]]
  lapply(rows, function(r) {
    cx <- as.integer(regmatches(r, gregexpr("(?<=\\\\cellx)[0-9]+", r, perl = TRUE))[[1L]])
    body <- sub("^.*?\\\\cellx[0-9]+(?!.*\\\\cellx)", "", r, perl = TRUE)
    cells <- strsplit(body, "\\cell", fixed = TRUE)[[1L]]
    cells <- cells[seq_len(max(length(cells) - 1L, 0L))]
    list(cellx = cx,
         q = sub(".*?\\\\q([lcr]).*", "\\1", cells, perl = TRUE),
         text = trimws(sub("^.*?\\\\ri0(\\\\f[0-9]+)? ?", "", cells, perl = TRUE)))
  })
}

.vl_design <- function() {
  lv <- list(widths = c("none", "fixed", "fit", "mixed", "min"),
             wrap = c("default", "listing_wrap", "code", "custom"),
             sep = c("/", " | ", "."),
             layout = c("stack", "flow"),
             key = c("FALSE", "TRUE"),
             align = c("template", "center", "column"),
             page = names(.VL_PAGE),
             font = names(.VL_FONT),
             data = c("ascii", "long", "nonascii"),
             spacer = c("on", "half", "off"))
  full <- expand.grid(lv, stringsAsFactors = FALSE)
  cand <- full[seq(1L, nrow(full), by = 97L), , drop = FALSE]
  rownames(cand) <- NULL
  every <- unlist(lapply(utils::combn(names(lv), 2L, simplify = FALSE), function(p) {
    g <- expand.grid(lv[[p[1L]]], lv[[p[2L]]], stringsAsFactors = FALSE)
    paste(p[1L], g[[1L]], p[2L], g[[2L]], sep = "|")
  }))
  list(pw = .allpairs(cand), every = every, n_full = nrow(full))
}

test_that("FT-LS160 [FR-LS253]: listing builders pairwise -- widths x wrap x separator x layout x key x align x page x font x data x gutters", {
  des <- .vl_design()
  expect_identical(des$n_full, 58320L)
  # every pair of levels of every two factors is in the candidates, and the
  # picked rows cover them all
  expect_setequal(des$pw$need, des$every)
  expect_setequal(des$pw$covered, des$every)
  cases <- des$pw$rows
  expect_lte(nrow(cases), 40L)

  for (r in seq_len(nrow(cases))) {
    cs <- as.list(cases[r, , drop = FALSE])
    lab <- paste0("case ", r, " (", paste(names(cs), unlist(cs), sep = "=", collapse = ", "), ")")
    d <- .vl_data(cs$data)
    pg <- .VL_PAGE[[cs$page]]; ft <- .VL_FONT[[cs$font]]
    spec <- .vl_spec(cs, d)
    width <- lapply(spec$cols, function(cl) cl$width)

    # -- widths --------------------------------------------------------------
    gutter <- if (cs$spacer == "off") 0 else 2 * (if (cs$spacer == "half") 0.5 else 1)
    if (cs$widths == "none") {
      expect_true(all(vapply(width, is.null, NA)), label = lab)
    } else if (cs$widths == "fixed") {
      expect_identical(unlist(width), c(8L, 14L, 6L), label = lab)
    } else {
      budget <- floor(pg$w / (text_width_in("0", font = ft$font,
                                            font_size_half_points = ft$size) * 1440))
      expect_identical(attr(spec, "rtf_listing_fit")$total_width, as.integer(budget), label = lab)
      expect_equal(sum(unlist(width)) + gutter, budget, label = lab)
      if (cs$widths == "mixed") expect_identical(width[[1L]], 8L, label = lab)
      if (cs$widths == "min") expect_true(all(unlist(width) >= 12L), label = lab)
      expect_identical(vapply(spec$cols, function(cl) cl$rel_width, 0),
                       as.numeric(unlist(width)), label = lab)
    }

    # -- the body ------------------------------------------------------------
    body <- build_listing(d, spec)
    vars <- list(ID = "ID", TERM = c("TERM", "LOC"), AGE = c("AGE", "SEX"))
    lines <- lapply(seq_along(vars), function(j) {
      lapply(seq_len(nrow(d)), function(i) {
        .vl_lines(.vl_join(unlist(d[i, vars[[j]]]), cs$sep), width[[j]], cs$sep,
                  cs$layout, cs$wrap)
      })
    })
    nl <- vapply(seq_len(nrow(d)), function(i) max(lengths(lapply(lines, `[[`, i))), 1L) + 1L
    expect_identical(nrow(body), sum(nl), label = lab)
    expect_identical(names(body), c("ID", if (cs$spacer != "off") ".sp1", "TERM",
                                    if (cs$spacer != "off") ".sp2", "AGE", ".rtf_record"),
                     label = lab)
    for (j in seq_along(vars)) {
      want <- unlist(lapply(seq_len(nrow(d)), function(i) {
        L <- lines[[j]][[i]]
        fill <- if (j == 1L && cs$key == "TRUE" && length(L) == 1L) L else ""
        c(L, rep(fill, nl[i] - length(L)))
      }))
      expect_identical(body[[names(vars)[j]]], want, label = paste(lab, names(vars)[j]))
      # every line fits its width, and no character is lost or added
      if (!is.null(width[[j]])) {
        expect_true(all(listing_disp_width(unlist(lines[[j]])) <= width[[j]]),
                    label = paste(lab, "fits", j))
      }
      src <- vapply(seq_len(nrow(d)), function(i) {
        s <- gsub("\\s", "", .vl_join(unlist(d[i, vars[[j]]]), cs$sep))
        if (cs$wrap == "custom") toupper(s) else s
      }, "")
      got <- vapply(lines[[j]], function(L) gsub("\\s", "", paste(L, collapse = "")), "")
      expect_identical(got, src, label = paste(lab, "chars", j))
    }
    expect_identical(body$.rtf_record, rep(seq_len(nrow(d)), nl), label = lab)
    if (cs$spacer != "off") expect_true(all(body$.sp1 == "" & body$.sp2 == ""), label = lab)
    # the derived header, wrapped as the cells are
    hdr_src <- c("ID", paste0("Reported Term", cs$sep, "LOC"), paste0("AGE", cs$sep, "SEX"))
    hdr <- vapply(seq_along(vars), function(j) {
      paste(.vl_lines(hdr_src[j], width[[j]], cs$sep, cs$layout, cs$wrap), collapse = "\n")
    }, "")
    got_hdr <- vapply(attr(body, "rtf_listing")$cols, function(cl) cl$label, "")
    expect_identical(got_hdr, hdr, label = lab)

    # -- the RTF -------------------------------------------------------------
    path <- tempfile(fileext = ".rtf")
    doc <- rtf_document(page = do.call(rtf_page, pg$page),
                        style = rtf_style(font = ft$font, font_size_half_points = ft$size)) |>
      rtf_tables(as_rtftables(d, listing = spec))
    generate_rtfreport(doc, path, overwrite = TRUE)
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    unlink(path)
    expect_true(grepl(sprintf("\\paperw%d\\paperh%d", pg$paper[1L], pg$paper[2L]), txt,
                      fixed = TRUE), label = lab)
    expect_identical(grepl("\\landscape", txt, fixed = TRUE), pg$land, label = lab)
    expect_true(grepl(paste0(ft$font, ";}"), txt, fixed = TRUE), label = lab)
    expect_true(grepl(paste0("\\fs", ft$size), txt, fixed = TRUE), label = lab)
    rows <- .vl_rows(txt)
    hdr_row <- rows[[1L]]
    rel <- vapply(seq_along(vars), function(j) {
      if (!is.null(width[[j]])) as.numeric(width[[j]])
      else max(listing_disp_width(strsplit(hdr[j], "\n", fixed = TRUE)[[1L]]))
    }, 0)
    sp <- if (cs$spacer == "half") 0.5 else 1
    rel <- if (cs$spacer == "off") rel else c(rel[1L], sp, rel[2L], sp, rel[3L])
    expect_identical(length(hdr_row$cellx), length(rel), label = lab)
    expect_identical(utils::tail(hdr_row$cellx, 1L), pg$w, label = lab)
    expect_true(all(abs(hdr_row$cellx - cumsum(rel) / sum(rel) * pg$w) <= 2), label = lab)
    al <- if (cs$align == "center") "c" else "l"
    want_q <- c(al, al, if (cs$align == "column") "r" else al)
    if (cs$spacer != "off") want_q <- c(want_q[1L], al, want_q[2L], al, want_q[3L])
    data_rows <- Filter(function(x) length(x$cellx) == length(rel), rows)[-1L]
    expect_identical(unique(lapply(data_rows, `[[`, "q")), list(want_q), label = lab)
    ids <- vapply(data_rows, function(x) x$text[1L], "")
    expect_identical(sum(ids == "S-01"), if (cs$key == "TRUE") 1L else 2L, label = lab)
    expect_identical(sum(ids == "S-02"), 1L, label = lab)
    if (cs$data == "nonascii") {
      expect_true(grepl("\\u-26579?", txt, fixed = TRUE), label = lab)   # U+982D
      # e acute, upper-cased by the custom rule
      expect_true(grepl(if (cs$wrap == "custom") "\\u201?" else "\\u233?", txt,
                        fixed = TRUE), label = lab)
    }

    # -- the code written out ------------------------------------------------
    code <- listing_code(spec)
    expect_identical(any(grepl("^# NOTE: this listing uses a custom `wrap`", code)),
                     cs$wrap != "default", label = lab)
    back <- eval(parse(text = code), envir = new.env(parent = environment(listing_code)))
    keys <- c("vars", "width", "sep", "layout", "name", "rel_width", "align",
              "collapse_repeats", "label")
    for (j in seq_along(spec$cols)) {
      expect_identical(back$cols[[j]][keys], spec$cols[[j]][keys], label = paste(lab, "code", j))
    }
    for (k in c("sep", "spacer", "spacer_rel_width", "align", "layout", "record_col")) {
      expect_identical(back[[k]], spec[[k]], label = paste(lab, "code", k))
    }
  }
})

test_that("FT-LS161 [FR-LS156 FR-LS157]: listing builders decision table -- the key column against the record's lines, the blank row and the page", {
  dt <- data.frame(
    key       = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE),
    key_lines = c(1L, 1L, 1L, 1L, 2L, 2L),
    blank     = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
    other     = c(1L, 1L, 1L, 2L, 1L, 2L),
    stringsAsFactors = FALSE)
  dt$want <- I(list(c("K-1", ""), c("K-1", "K-1"), "K-1", c("K-1", "K-1"),
                    c("K-1 LONG", "VALUE", ""), c("K-1 LONG", "VALUE")))
  for (r in seq_len(nrow(dt))) {
    cs <- dt[r, ]
    lab <- paste("row", r)
    key_val <- if (cs$key_lines == 1L) "K-1" else "K-1 LONG VALUE"
    oth_val <- if (cs$other == 1L) "AB" else "AB CD"
    d <- data.frame(K = key_val, V = oth_val, stringsAsFactors = FALSE)
    spec <- listing_spec(list(listing_col("K", width = 9, collapse_repeats = cs$key),
                              listing_col("V", width = 3)),
                         blank_row = cs$blank, spacer = FALSE)
    body <- build_listing(d, spec)
    expect_identical(body$K, cs$want[[1L]], label = lab)
  }

  # rendered: a run of three records of one key, two to a page -- the value
  # once per run on a page, and again at the top of the next page
  d <- data.frame(K = c("K-1", "K-1", "K-1", "K-2"), V = c("a", "b", "c", "d"),
                  stringsAsFactors = FALSE)
  for (key in c(TRUE, FALSE)) {
    spec <- listing_spec(list(listing_col("K", width = 5, collapse_repeats = key),
                              listing_col("V", width = 3)), spacer = FALSE,
                         blank_row_first = FALSE)
    pages <- as_rtftables(d, listing = spec, max_rows = 4)
    expect_length(pages, 2L)
    ks <- lapply(pages, function(p) p$data$K)
    if (key) {
      # (a blanked repeat is NA, written as an empty cell)
      expect_identical(ks[[1L]], c("K-1", NA, NA, NA))
      expect_identical(ks[[2L]], c("K-1", NA, "K-2", NA))
    } else {
      expect_identical(ks[[1L]], c("K-1", "", "K-1", ""))
      expect_identical(ks[[2L]], c("K-1", "", "K-2", ""))
    }
  }
})
