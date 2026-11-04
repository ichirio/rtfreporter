# Validation tests: auto_col_widths() -- how the header labels, the font and
# its size, the padding, the minimum, a table width and the protected columns
# act together: a pairwise set and a decision table of protect_cols against
# the table width.  Helpers prefixed .aw_.  Fictional data only.
#
# ---------------------------------------------------------------------------
#  Design
# ---------------------------------------------------------------------------
#  The rules, as read from the help (auto_col_widths(), text_width_in()) and
#  FR-TB366 FR-TB369 FR-TB370 FR-TB371 FR-TB372 FR-TB373 FR-TB374 FR-TB375:
#
#    natural  a column's width is its widest line (a multi-line cell or
#             label counts by its longest line, a missing label or cell by
#             nothing) as text_width_in() measures it in the font and size,
#             in twips, plus col_padding_twips, and never below
#             min_col_width_twips.  The header is the labels of col_header
#             (a vector, a pipe-delimited string, or the first row of a list;
#             a missing label counts as empty), else names(data).
#    width    with table_width_twips the widths are scaled so that they add up
#             to exactly that many twips, the last column taking the rounding.
#    protect  protect_cols keep their natural width when a table width is
#             given -- squeezed or scaled up -- and only the others are
#             scaled; with no table width they do nothing; an index outside
#             the columns names nothing; the protection is dropped when it
#             would push the others below the minimum.
#
#  Courier New is measured at a fixed advance, the one family whose text
#  width text_width_in() and auto_col_widths() agree on (see the report of
#  the PR: Arial and Times New Roman are measured at a flat average by
#  auto_col_widths() but per glyph by text_width_in(), and a table width that
#  the minimum overrules does not add up, and a protected set at the edge of
#  the minimum can leave the last column below it).  Those cases are not
#  asserted here; the cases below keep every scaled column above the minimum.
#
#  Data (.aw_df): four columns, ID (12 characters), Arm (12), Result (a
#  two-line cell, 15 and 16 characters) and Visit (17, one missing).
#
#  (A) FT-TB988, pairwise.  Factors and levels:
#    header   NULL (the names), a vector of long labels, a pipe string, a list
#             of two rows (only the first counts), a vector of two labels
#             (the others empty)
#    font     NULL (Courier New), "Courier New", "courier"
#    size     18, 24 half-points
#    width    NULL, wide (1.5 x the natural total), narrow (0.85 x)
#    min      720, 0, 2000 (only with no table width: 2000 is above the
#             scaled widths, where the minimum and the total disagree)
#    padding  288, 0
#    protect  none, 1, c(1, 3), 9 (outside the columns)
#  The product is 5 x 3 x 2 x 3 x 3 x 2 x 4 = 2160 rows less the 2000-with-
#  width rows (480); .allpairs() picks the rows covering every pair of
#  levels.  Oracle: the rules above.
#
#  (B) FT-TB989, decision table, protect_cols against the table width (min
#  720, padding 288, size 18, every natural width above the minimum):
#    protect | table width                         | result
#    none    | any                                 | uniform scaling
#    1       | NULL                                | the natural widths (no effect)
#    9       | wide                                | the same as none
#    1       | wide (1.5 x)                        | column 1 natural, others grow
#    1       | narrow, the others stay above min   | column 1 natural, others shrink
#    1       | one twip under natural(1) + 3 x min | dropped: uniform scaling
# ---------------------------------------------------------------------------

.aw_df <- function() {
  data.frame(ID = c("SUBJ-0001-XX", "S2"),
             Arm = c("Placebo", "Drug A 10 mg"),
             Result = c("Mean\n12.3456789 (4.5)", "x"),
             Visit = c("Week 12 follow-up", NA),
             stringsAsFactors = FALSE)
}

.aw_headers <- list(
  names = NULL,
  long = c("Subject identifier", "Arm", "Result", "Visit"),
  pipe = "Subject|Treatment arm of the study|Result|Visit",
  list = list(c("A", "Treatment arm", "R", "V"),
              c("A second row that is far too long to count", "", "", "")),
  short = c("Subject identifier", "Arm"))

# the labels the rules read, one per column
.aw_labels <- function(spec, d) {
  if (is.null(spec)) return(names(d))
  if (is.list(spec)) spec <- spec[[1L]]
  if (length(spec) == 1L && grepl("|", spec, fixed = TRUE)) spec <- strsplit(spec, "|", fixed = TRUE)[[1L]]
  c(spec, rep("", ncol(d) - length(spec)))
}

.aw_longest <- function(x, font, size) {
  x[is.na(x)] <- ""
  lines <- unlist(strsplit(c(x, ""), "\r\n|\r|\n"))
  lines[which.max(text_width_in(lines, font = font, font_size_half_points = size))]
}

# the natural widths, from text_width_in()
.aw_natural <- function(d, labels, font, size, pad, min) {
  w <- vapply(seq_along(d), function(j) {
    longest <- c(.aw_longest(labels[j], font, size), .aw_longest(d[[j]], font, size))
    round(max(text_width_in(longest, font = font, font_size_half_points = size)) * 1440)
  }, 0)
  pmax(w + pad, min)
}

.aw_check_scaled <- function(got, nat, total, protect, min, label) {
  expect_identical(sum(got), as.integer(total), info = label)
  expect_true(all(got >= min), info = label)
  prot <- intersect(protect, seq_along(nat))
  scal <- setdiff(seq_along(nat), prot)
  expect_identical(got[prot], as.integer(nat[prot]), info = label)
  factor <- (total - sum(nat[prot])) / sum(nat[scal])
  body <- setdiff(scal, scal[length(scal)])
  expect_lte(max(abs(got[body] - nat[body] * factor)), 1, label = label)
}

test_that("FT-TB988 [FR-TB366 FR-TB369 FR-TB370 FR-TB371 FR-TB372 FR-TB373 FR-TB374 FR-TB375 FR-IX406]: auto_col_widths(): header x font x size x table width x minimum x padding x protected columns, pairwise", {
  cand <- expand.grid(header = names(.aw_headers), font = c("NULL", "Courier New", "courier"),
                      size = c(18L, 24L), width = c("NULL", "wide", "narrow"),
                      min = c(720L, 0L, 2000L), pad = c(288L, 0L),
                      protect = c("none", "1", "1,3", "9"), stringsAsFactors = FALSE)
  cand <- cand[!(cand$min == 2000L & cand$width != "NULL"), ]
  expect_identical(nrow(cand), 1680L)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 60L)
  d <- .aw_df()
  for (i in seq_len(nrow(ap$rows))) {
    r <- ap$rows[i, ]
    font <- if (r$font == "NULL") NULL else r$font
    protect <- if (r$protect == "none") integer(0) else as.integer(strsplit(r$protect, ",")[[1L]])
    hdr <- .aw_headers[[r$header]]
    labels <- .aw_labels(hdr, d)
    nat <- .aw_natural(d, labels, font, r$size, r$pad, r$min)
    total <- switch(r$width, "NULL" = NULL, wide = round(sum(nat) * 1.5), narrow = round(sum(nat) * 0.85))
    got <- auto_col_widths(d, col_header = hdr, font = font, font_size_half_points = r$size,
                           table_width_twips = total, min_col_width_twips = r$min,
                           col_padding_twips = r$pad, protect_cols = protect)
    label <- paste(r, collapse = "/")
    expect_type(got, "integer")
    expect_length(got, ncol(d))
    if (is.null(total)) {
      expect_lte(max(abs(got - nat)), 1, label = label)
    } else {
      .aw_check_scaled(got, nat, total, protect, r$min, label)
    }
  }
})

test_that("FT-TB989 [FR-TB371 FR-TB372 FR-TB374 FR-IX406]: auto_col_widths(): protect_cols against the table width, the decision table", {
  d <- .aw_df()
  nat <- as.integer(auto_col_widths(d))
  expect_gt(min(nat), 720L)
  s <- sum(nat)
  uniform <- function(total) {
    w <- as.integer(round(nat[-4L] * total / s))
    c(w, as.integer(total - sum(w)))
  }
  # none: uniform scaling, up and down
  for (f in c(1.5, 0.85)) {
    got <- auto_col_widths(d, table_width_twips = round(s * f))
    expect_identical(got, uniform(round(s * f)))
  }
  # 1 and no table width: no effect
  expect_identical(auto_col_widths(d, protect_cols = 1), as.integer(nat))
  # 9: names nothing
  expect_identical(auto_col_widths(d, table_width_twips = round(s * 1.5), protect_cols = 9),
                   uniform(round(s * 1.5)))
  # 1 with a wide table: column 1 natural, the others grow
  wide <- round(s * 1.5)
  got <- auto_col_widths(d, table_width_twips = wide, protect_cols = 1)
  expect_identical(got[1L], nat[1L])
  expect_identical(sum(got), as.integer(wide))
  expect_true(all(got[-1L] > nat[-1L]))
  # 1 with a narrower table the others survive: column 1 natural, the others shrink
  narrow <- round(s * 0.85)
  got <- auto_col_widths(d, table_width_twips = narrow, protect_cols = 1)
  expect_identical(got[1L], nat[1L])
  expect_identical(sum(got), as.integer(narrow))
  expect_true(all(got[-1L] < nat[-1L]) && all(got[-1L] >= 720L))
  # one twip under what column 1 and three minimums need: the protection is dropped
  edge <- nat[1L] + 3L * 720L - 1L
  got <- auto_col_widths(d, table_width_twips = edge, protect_cols = 1)
  expect_identical(got, uniform(edge))
  expect_lt(got[1L], nat[1L])
})
