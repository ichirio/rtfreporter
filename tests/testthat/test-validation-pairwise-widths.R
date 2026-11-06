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
#             A column the scaling would take below the minimum is held at the
#             minimum and the others are scaled to what is left (repeated until
#             none falls below), so the minimum holds and the sum is exact.
#             When the table width is below the sum of the minima both cannot
#             hold: the widths are scaled in proportion to their natural widths,
#             the minimum ignored, and the sum is still exact.
#    protect  protect_cols keep their natural width when a table width is
#             given -- squeezed or scaled up -- and only the others are
#             scaled; with no table width they do nothing; an index outside
#             the columns names nothing; the protection is dropped when what
#             is left for the others is less than the minimum each.
#
#  Courier New is measured at a fixed advance, the one family whose text
#  width text_width_in() and auto_col_widths() agree on: auto_col_widths()
#  measures a font by one average character width (the help says so), and
#  text_width_in() per glyph for Arial and Times New Roman, so the oracle
#  below, which measures with text_width_in(), is for the Courier names.
#
#  Data: .aw_df, four columns, ID (12 characters), Arm (12), Result (a
#  two-line cell, 15 and 16 characters) and Visit (17, one missing), every
#  natural width above the minimum; and .aw_df_short, ID (12), Arm (12) and
#  two narrow columns, Age and Sex, whose natural width is the minimum, so a
#  table width below the natural total takes them below it and they are held.
#
#  (A) FT-TB988, pairwise.  Factors and levels:
#    data     .aw_df | .aw_df_short
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
#  The product is 2 x 5 x 3 x 2 x 3 x 3 x 2 x 4 = 4320 rows less the 2000-with-
#  width rows (960); .allpairs() picks the rows covering every pair of
#  levels.  Oracle: the rules above -- the sum is the table width; no column is
#  below the minimum (the table width at least the sum of the minima); a
#  protected column (when the protection holds) keeps its natural width; a
#  column the scaling takes under the minimum is at the minimum, and the
#  others are in one proportion to their natural widths (the last takes the
#  rounding).
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
#
#  (C) FT-TB990, decision table, the minimum against the table width (short
#  data: natural 1588 1588 720 720, sum 4616; min 720; n x min = 2880):
#    protect | table width | result
#    none    | NULL        | the natural widths
#    none    | 4616        | the natural widths
#    none    | 4400        | 1480 1480 720 720 (Age and Sex held at 720, ID and
#            |             | Arm scaled to what is left, 2 x 1480)
#    none    | 3616        | 1088 1088 720 720
#    none    | 2880        | 720 720 720 720 (n x min, the edge)
#    none    | 2879        | below n x min: proportional, minimum ignored,
#            |             | 990 990 449 450 (sum 2879)
#    none    | 2000        | proportional: 688 688 312 312
#    1       | 3748        | 1588 720 720 720 (what is left, 2160, is 3 x min:
#            |             | the protection holds, the others are held)
#    1       | 3747        | one twip less: the protection is dropped, the
#            |             | widths scale together and the minimum holds
#    1       | 2000        | below n x min: dropped, proportional
#    1       | scaled up   | column 1 natural, the sum exact, none below min
#    none    | 3616, min 0 | no column is held: the natural widths (with min 0)
#            |             | scaled by one factor
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

.aw_df_short <- function() {
  data.frame(ID = c("SUBJ-0001-XX", "S2"), Arm = c("Placebo", "Drug A 10 mg"),
             Age = c("65", "7"), Sex = c("F", "M"), stringsAsFactors = FALSE)
}

# what the rules give, as properties: the sum is the table width; no column
# is under the minimum when the table width allows it; the protected columns
# keep their natural width when what is left is a minimum each for the
# others; a column the scaling takes under the minimum is at the minimum, the
# others are in one proportion to their natural widths (the last of them
# taking the rounding)
.aw_check_scaled <- function(got, nat, total, protect, min, label) {
  n <- length(nat)
  expect_identical(sum(got), as.integer(total), info = label)
  if (total >= n * min) expect_true(all(got >= min), info = label)
  prot <- intersect(protect, seq_along(nat))
  scal <- setdiff(seq_along(nat), prot)
  holds <- length(prot) > 0L && length(scal) > 0L &&
    total - sum(nat[prot]) >= length(scal) * min
  if (!holds) {
    prot <- integer(0)
    scal <- seq_len(n)
  }
  expect_identical(got[prot], as.integer(nat[prot]), info = label)
  left <- total - sum(nat[prot])
  if (left < length(scal) * min) {
    # the minimum cannot hold: in proportion to the natural widths
    body <- scal[-length(scal)]
    expect_lte(max(abs(got[body] - nat[body] * left / sum(nat[scal]))), 1, label = label)
    return(invisible())
  }
  held <- scal[min > 0 & got[scal] == min]
  free <- setdiff(scal, held)
  factor <- (left - min * length(held)) / sum(nat[free])
  body <- setdiff(free, free[length(free)])
  expect_lte(max(abs(got[body] - nat[body] * factor)), 1, label = label)
  # a held column is one the proportion would have taken under the minimum
  if (length(held)) expect_true(all(nat[held] * factor <= min + 1), info = label)
}

test_that("FT-TB988 [FR-TB366 FR-TB369 FR-TB370 FR-TB371 FR-TB372 FR-TB373 FR-TB374 FR-TB375 FR-TB626 FR-TB627 FR-IX406]: auto_col_widths(): data x header x font x size x table width x minimum x padding x protected columns, pairwise", {
  cand <- expand.grid(data = c("base", "short"),
                      header = names(.aw_headers), font = c("NULL", "Courier New", "courier"),
                      size = c(18L, 24L), width = c("NULL", "wide", "narrow"),
                      min = c(720L, 0L, 2000L), pad = c(288L, 0L),
                      protect = c("none", "1", "1,3", "9"), stringsAsFactors = FALSE)
  cand <- cand[!(cand$min == 2000L & cand$width != "NULL"), ]
  expect_identical(nrow(cand), 3360L)
  ap <- .allpairs(cand)
  expect_setequal(ap$covered, ap$need)
  expect_lt(nrow(ap$rows), 70L)
  for (i in seq_len(nrow(ap$rows))) {
    r <- ap$rows[i, ]
    d <- if (r$data == "short") .aw_df_short() else .aw_df()
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

test_that("FT-TB990 [FR-TB371 FR-TB372 FR-TB374 FR-TB626 FR-TB627 FR-IX406]: auto_col_widths(): the minimum against the table width, the decision table", {
  d <- .aw_df_short()
  nat <- auto_col_widths(d)
  expect_identical(nat, c(1588L, 1588L, 720L, 720L))
  w <- function(total, ...) auto_col_widths(d, table_width_twips = total, ...)
  # no table width, or the natural total: the natural widths
  expect_identical(w(4616), nat)
  # the scaling takes Age and Sex under the minimum: they are held, the other
  # two are scaled to what is left (the sum is exact)
  expect_identical(w(4400), c(1480L, 1480L, 720L, 720L))
  expect_identical(w(3616), c(1088L, 1088L, 720L, 720L))
  # the edge: the table width is the sum of the minima
  expect_identical(w(2880), rep(720L, 4L))
  # below the sum of the minima: in proportion to the natural widths, the
  # minimum ignored, the sum exact
  expect_identical(w(2879), c(990L, 990L, 449L, 450L))
  expect_identical(w(2000), c(688L, 688L, 312L, 312L))
  # protect_cols: what is left for the others is exactly 3 x min -> the
  # protection holds and the others are held
  expect_identical(w(3748, protect_cols = 1), c(1588L, 720L, 720L, 720L))
  # one twip less: dropped, the widths scale together, the minimum holds
  got <- w(3747, protect_cols = 1)
  expect_identical(sum(got), 3747L)
  expect_true(all(got >= 720L))
  expect_lt(got[1L], 1588L)
  # protect_cols below the sum of the minima: dropped, proportional
  got <- w(2000, protect_cols = 1)
  expect_identical(got, w(2000))
  # protect_cols scaled up: column 1 natural, the sum exact, none under the minimum
  got <- w(6000, protect_cols = 1)
  expect_identical(got[1L], 1588L)
  expect_identical(sum(got), 6000L)
  expect_true(all(got >= 720L))
  # with a minimum of 0 no column is held: one factor for all
  got <- auto_col_widths(d, table_width_twips = 3616, min_col_width_twips = 0)
  expect_identical(sum(got), 3616L)
  nat0 <- auto_col_widths(d, min_col_width_twips = 0)
  expect_lt(min(nat0), 720L)
  expect_lte(max(abs(got[-4L] - nat0[-4L] * 3616 / sum(nat0))), 1)
  # the examples of the report: the sums were 3928, 4468; the last column of
  # a protected set was 307 (under the minimum)
  d4 <- data.frame(ID = c("SUBJ-0001-XX", "S2"), Arm = c("Placebo", "Drug A 10 mg"),
                   Age = c("65", "7"), Sex = c("F", "M"))
  for (tw in c(3616, 4400)) expect_identical(sum(auto_col_widths(d4, table_width_twips = tw)), as.integer(tw))
  expect_true(all(auto_col_widths(d4, table_width_twips = 3748, protect_cols = 1) >= 720L))
})
