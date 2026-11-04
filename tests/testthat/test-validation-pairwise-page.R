# Validation tests: rtf_page() -- how the paper (a preset or explicit sizes),
# the orientation, the margins and the header / footer distances act together
# in the file a document writes: a pairwise set and a decision table of what
# the explicit sizes do to paper_size and orientation.  Helpers prefixed
# .pp_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  Each argument alone (its classes, 0, Inf, NA, a vector, a string) is
#  FT-DC282's, and margins that leave no writable area FT-DC283's, in
#  test-validation-boundaries-document.R; this file does not repeat them.
#  What is left are the arguments that change one another's meaning.  The
#  rules, as read from the help (rtf_page()) and FR-DC152 FR-DC155 FR-DC157
#  FR-DC159 FR-DC161 FR-DC340 FR-IX309 FR-IX310:
#
#    paper    a preset (letter 8.5 x 11 in, legal 8.5 x 14, A4 210 x 297 mm,
#             A3 297 x 420, A5 148 x 210; any case) gives the paper's short
#             and long side; orientation "landscape" (the default) makes the
#             long side the width and writes \landscape, "portrait" the short
#             one and no \landscape.
#    sizes    width_in and height_in WIN over paper_size; the page is as
#             given, the orientation INFERRED (width >= height is landscape:
#             a square page is landscape) and orientation / paper_size, left
#             unset, stay unset.  A paper_size PASSED with sizes warns that
#             the sizes take precedence; an orientation PASSED that contradicts
#             the sizes warns and the sizes are used; one that agrees warns of
#             nothing.
#    margins  the four margins in inches (default 0.75) are \margl \margr
#             \margt \margb in twips (1 in = 1440).
#    bands    header_dist_in / footer_dist_in NULL: the band sits at the full
#             top / bottom margin (\headery = \margt, \footery = \margb); a
#             number: that distance.
#
#  The paper in twips is the inches times 1440.  A4's width is 210 mm =
#  11905.5 twips: the file may hold either neighbour (the factory constant
#  rounds to 11905), so a preset's size is checked within one twip.
#
#  (A) FT-DC296, pairwise.  Factors and levels:
#    paper    letter, "A4", legal, "A3", "a5" (the preset spelled in another
#             case), sizes 10 x 7 in, sizes 7 x 10, sizes 8 x 8 (the square),
#             sizes 10 x 7 with paper_size = "A4" passed
#    orient   not passed, "landscape", "portrait"
#    margins  default, custom (top 1, bottom 0.9, left 0.5, right 0.6), thin
#             (0.01 on all four: the smallest the help allows to be near 0)
#    header   NULL, 0.3
#    footer   NULL, 0.4
#  The product is 9 x 3 x 3 x 2 x 2 = 324 rows; .allpairs() picks the rows
#  covering every pair of levels.  Oracle: the rules above (the warnings
#  checked as well: exactly those the rules name).
#
#  (B) FT-DC297, decision table, what explicit sizes do to paper_size and
#  orientation.  Sizes 10 x 7 (landscape), 7 x 10 (portrait) or 8 x 8 (square,
#  landscape) x paper_size not passed / "A4" passed x orientation not passed /
#  "landscape" / "portrait" = 18 rows.  The page is the sizes in every row; it
#  is landscape when width >= height; a warning of paper_size when it was
#  passed; a warning of orientation exactly when it was passed and differs
#  from the inferred one.
# ---------------------------------------------------------------------------

.pp_df <- data.frame(Param = c("Age", "Weight"), Value = c("65", "70"),
                     stringsAsFactors = FALSE)

# the file the page gives, and the warnings it raised
.pp_write <- function(...) {
  warns <- character(0)
  txt <- withCallingHandlers({
    doc <- rtf_document(page = rtf_page(...))
    f <- tempfile(fileext = ".rtf")
    on.exit(unlink(f), add = TRUE)
    suppressMessages(generate_rtfreport(rtf_tables(doc, .pp_df), f))
    paste(readLines(f, warn = FALSE), collapse = "\n")
  }, warning = function(w) {
    warns <<- c(warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(rtf = txt, warnings = warns)
}

# the first value of a control word, as a number (NA when it is not there)
.pp_num <- function(txt, word) {
  m <- regmatches(txt, regexpr(paste0("\\\\", word, "[0-9]+"), txt))
  if (length(m)) as.integer(sub(paste0("\\\\", word), "", m)) else NA_integer_
}

.pp_has_landscape <- function(txt) grepl("\\landscape", txt, fixed = TRUE)

# paper sides in twips, short then long, from the sizes the help states
.pp_preset <- function(name) {
  mm <- function(x) x / 25.4 * 1440
  switch(tolower(name),
         letter = c(8.5, 11) * 1440,
         legal  = c(8.5, 14) * 1440,
         a4     = mm(c(210, 297)),
         a3     = mm(c(297, 420)),
         a5     = mm(c(148, 210)))
}

.pp_geoms <- list(
  letter  = list(paper_size = "letter"),
  A4      = list(paper_size = "A4"),
  legal   = list(paper_size = "legal"),
  A3      = list(paper_size = "A3"),
  a5      = list(paper_size = "a5"),
  s10x7   = list(width_in = 10, height_in = 7),
  s7x10   = list(width_in = 7, height_in = 10),
  s8x8    = list(width_in = 8, height_in = 8),
  s10x7A4 = list(width_in = 10, height_in = 7, paper_size = "A4"))

.pp_margins <- list(
  default = list(),
  custom  = list(margin_top_in = 1, margin_bottom_in = 0.9,
                 margin_left_in = 0.5, margin_right_in = 0.6),
  thin    = list(margin_top_in = 0.01, margin_bottom_in = 0.01,
                 margin_left_in = 0.01, margin_right_in = 0.01))

test_that("FT-DC296 [FR-DC152 FR-DC155 FR-DC157 FR-DC159 FR-DC161 FR-DC340 FR-IX309 FR-IX310 FR-IX405]: rtf_page(): paper x orientation x margins x header and footer distance, pairwise", {
  cand <- expand.grid(paper = names(.pp_geoms), orient = c("unset", "landscape", "portrait"),
                      margins = names(.pp_margins), header = c("null", "0.3"),
                      footer = c("null", "0.4"), stringsAsFactors = FALSE)
  expect_identical(nrow(cand), 324L)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 45L)
  for (i in seq_len(nrow(ap$rows))) {
    r <- ap$rows[i, ]
    args <- c(.pp_geoms[[r$paper]], .pp_margins[[r$margins]])
    if (r$orient != "unset") args$orientation <- r$orient
    if (r$header != "null") args$header_dist_in <- 0.3
    if (r$footer != "null") args$footer_dist_in <- 0.4
    out <- do.call(.pp_write, args)
    txt <- out$rtf
    label <- paste(r, collapse = "/")
    w <- .pp_num(txt, "paperw")
    h <- .pp_num(txt, "paperh")
    sized <- !is.null(args$width_in)
    if (sized) {
      expect_identical(c(w, h), as.integer(c(args$width_in, args$height_in) * 1440), info = label)
      land <- args$width_in >= args$height_in
      want_warn <- c(
        if (!is.null(args$paper_size)) "Both `paper_size` and `width_in`/`height_in`",
        if (r$orient != "unset" && (r$orient == "landscape") != land) "contradicts the given dimensions")
    } else {
      side <- .pp_preset(args$paper_size)
      land <- r$orient != "portrait"
      expect_lte(abs(min(w, h) - min(side)), 1, label = label)
      expect_lte(abs(max(w, h) - max(side)), 1, label = label)
      expect_identical(w > h, land, info = label)
      want_warn <- character(0)
    }
    expect_identical(.pp_has_landscape(txt), land, info = label)
    expect_length(out$warnings, length(want_warn))
    for (k in seq_along(want_warn)) {
      expect_true(any(grepl(want_warn[k], out$warnings, fixed = TRUE)), info = label)
    }
    m <- list(margin_top_in = 0.75, margin_bottom_in = 0.75,
              margin_left_in = 0.75, margin_right_in = 0.75)
    m[names(.pp_margins[[r$margins]])] <- .pp_margins[[r$margins]]
    got <- c(.pp_num(txt, "margt"), .pp_num(txt, "margb"),
             .pp_num(txt, "margl"), .pp_num(txt, "margr"))
    expect_identical(got, as.integer(round(unlist(m, use.names = FALSE) * 1440)), info = label)
    hd <- if (r$header == "null") m$margin_top_in else 0.3
    fd <- if (r$footer == "null") m$margin_bottom_in else 0.4
    expect_identical(.pp_num(txt, "headery"), as.integer(round(hd * 1440)), info = label)
    expect_identical(.pp_num(txt, "footery"), as.integer(round(fd * 1440)), info = label)
  }
})

test_that("FT-DC297 [FR-DC157 FR-DC340 FR-IX309 FR-IX310 FR-IX405]: rtf_page(): explicit sizes against paper_size and orientation, the decision table", {
  shapes <- list(land = c(10, 7), port = c(7, 10), square = c(8, 8))
  tab <- expand.grid(shape = names(shapes), paper = c("unset", "A4"),
                     orient = c("unset", "landscape", "portrait"),
                     stringsAsFactors = FALSE)
  expect_identical(nrow(tab), 18L)
  for (i in seq_len(nrow(tab))) {
    r <- tab[i, ]
    sz <- shapes[[r$shape]]
    args <- list(width_in = sz[1L], height_in = sz[2L])
    if (r$paper != "unset") args$paper_size <- "A4"
    if (r$orient != "unset") args$orientation <- r$orient
    out <- do.call(.pp_write, args)
    label <- paste(r, collapse = "/")
    land <- sz[1L] >= sz[2L]
    expect_identical(c(.pp_num(out$rtf, "paperw"), .pp_num(out$rtf, "paperh")),
                     as.integer(sz * 1440), info = label)
    expect_identical(.pp_has_landscape(out$rtf), land, info = label)
    paper_warn <- any(grepl("Both `paper_size` and `width_in`/`height_in`", out$warnings, fixed = TRUE))
    orient_warn <- any(grepl("contradicts the given dimensions", out$warnings, fixed = TRUE))
    expect_identical(paper_warn, r$paper != "unset", info = label)
    expect_identical(orient_warn, r$orient != "unset" && (r$orient == "landscape") != land,
                     info = label)
    expect_length(out$warnings, paper_warn + orient_warn)
  }
})
