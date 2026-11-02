# Validation tests: equivalence classes and boundary values of the arguments
# of the document and table constructors, batch 1 -- rtf_document(),
# rtf_page(), rtftable() and as_rtftables().  Helpers prefixed .eb_.
# Fictional data only.
#
# The help is the functional requirement.  For each argument the classes
# are: a valid value of each documented kind, NULL, length 0 / 1 / n / a
# wrong length, NA, a value of another type, the documented minimum and
# maximum with a value just inside and just outside, 0, a negative number,
# a fraction for a whole number, Inf / NaN, "" / a non-ASCII / a long
# string, and an unknown choice / a prefix.  A class another test already
# checks (the help-to-FR audit, FT-DC257..FT-DC280 and the FR guards) is
# not repeated here; pass-through `...` and arguments that take only a
# whole object of one class (rtftable(style = ), rtf_document(style = ))
# are left to their guard tests.  Each table below is one row per case;
# the test of the function runs them in a loop.
#
# ---------------------------------------------------------------------------
#  rtf_document()  -- FT-DC281.  The document is written with one table.
# ---------------------------------------------------------------------------
#  argument       | class                 | value              | expected                         | FR
#  font_table     | valid                 | list(list(name=Arial)) | f0 Arial in \fonttbl         | FR-DC102
#  font_table     | NULL                  | NULL               | f0 Courier New                   | FR-DC102
#  font_table     | non-ASCII name        | Mincho in kanji    | ☒6?☹7? in \fonttbl     | FR-DC102
#  font_table     | long name (120 chars) | "FFF...F"          | written whole                    | FR-DC102
#  font_table     | another type          | "Arial"            | error "`font_table` must be a non-empty list" | FR-DC348
#  font_table     | length 0              | list()             | error, same                      | FR-DC348
#  font_table     | name a number         | list(list(name=1)) | error, same                      | FR-DC348
#  font_table     | name NA               | NA                 | error "`font_table` must be a single font family name" | FR-DC348
#  font_table     | name ""               | ""                 | error, same                      | FR-DC348
#  color_table    | valid, n              | c(#000000, #1F4E79)| \red31\green78\blue121;          | FR-DC105
#  color_table    | lower-case hex        | "#1f4e79"          | the same entry                   | FR-DC347
#  color_table    | length 0              | character(0)       | black and white only             | FR-DC347
#  color_table    | NULL                  | NULL               | black and white only             | FR-DC105
#  color_table    | a colour name         | "red"              | error "`color_table` must be a character vector" | FR-DC347
#  color_table    | 5 digits (just short) | "#1F4E7"           | error, same                      | FR-DC347
#  color_table    | 7 digits (just long)  | "#1F4E790"         | error, same                      | FR-DC347
#  color_table    | NA                    | NA_character_      | error, same                      | FR-DC347
#  color_table    | another type          | 1                  | error, same                      | FR-DC347
#  page           | length 0              | list()             | landscape Letter \paperw15840    | FR-DC106
#  page           | one key               | list(paper_size=A4)| \paperw16838                     | FR-DC106
#  page           | negative margin       | margin_top_in = -1 | error "`margin_top_in` must be a single positive number" | FR-DC349
#  page           | margin a string       | margin_left_in="1" | error "`margin_left_in` must be ..." | FR-DC349
#  page           | Inf                   | width_in = Inf     | error "`width_in` must be ..."   | FR-DC349
#  page           | paper of length 2     | c(A4, A3)          | error "`paper_size` must be a single preset" | FR-DC349
#  page           | unknown orientation   | "land"             | error "`orientation` must be"    | FR-DC349
#  default_format | another type          | "x"                | error "`default_format` must be an rtf_default_format" | FR-DC350
#  watermark      | valid string          | "DRAFT"            | DRAFT in a shape                 | FR-DC109
#  watermark      | empty string          | ""                 | no shape                         | FR-DC109
#  watermark      | NA                    | NA                 | no shape                         | FR-DC109
#  watermark      | length 0              | character(0)       | error "`watermark` must be an rtf_watermark object" | FR-DC110
#  watermark      | length 2              | c("A", "B")        | error, same                      | FR-DC110
#  program        | empty string          | ""                 | error "`program` must be a single string" | FR-DC351
#  program_fallback | empty string        | ""                 | error "`program_fallback` must be a single string" | FR-DC351
#  program_fallback | NA                  | NA_character_      | error, same                      | FR-DC114
#  program_fallback | length 2            | c("a", "b")        | error, same                      | FR-DC114
#  tokens         | empty value           | list(STUDY = "")   | {STUDY} written as nothing       | FR-DC116
#  tokens         | non-ASCII value       | kanji              | \u-30106?\u-26093?               | FR-DC116
#  tokens         | a number              | list(STUDY = 5)    | 5                                | FR-DC116
#  tokens         | long value (300)      | "xxx...x"          | written whole                    | FR-DC116
#  tokens         | NULL value            | list(STUDY = NULL) | error "`tokens`: `STUDY` is one value" | FR-DC120
#  tokens         | NA value              | list(STUDY = NA)   | error, same                      | FR-DC120
#  font_lang      | length 0              | character(0)       | error "`font_lang` must be a named character vector" | FR-DC122
#  font_lang      | NA                    | NA                 | error, same                      | FR-DC122
#  font_lang      | another case          | c(latin = "JA")    | error "`font_lang`: unknown language" | FR-DC122
#  (file: "", NA, a vector and a number are FT-DC265's; style is a whole
#  rtf_style() object, FT-DC231's.)
#
# ---------------------------------------------------------------------------
#  rtf_page()  -- FT-DC282.  The page is given to a document and written.
# ---------------------------------------------------------------------------
#  argument       | class                 | value              | expected                         | FR
#  paper_size     | valid                 | "A4"               | \paperw16838\paperh11905         | FR-DC152
#  paper_size     | lower case            | "a4"               | the same                         | FR-DC152
#  paper_size     | default               | "letter"           | \paperw15840\paperh12240         | FR-DC152
#  paper_size     | NULL                  | NULL               | Letter                           | FR-DC151
#  paper_size     | unknown               | "B5"               | error at writing "Unknown `paper_size` "B5"" | FR-DC154
#  paper_size     | empty string          | ""                 | error at writing, same           | FR-DC154
#  paper_size     | prefix                | "let"              | error at writing, same           | FR-DC154
#  paper_size     | NA                    | NA_character_      | error at writing, same           | FR-DC154
#  paper_size     | length 0              | character(0)       | error "`paper_size` must be a single preset" | FR-DC153
#  orientation    | valid                 | "portrait"         | \paperw12240\paperh15840         | FR-DC155
#  orientation    | NULL                  | NULL               | landscape                        | FR-DC155
#  orientation    | another case          | "Portrait"         | error "`orientation` must be"    | FR-DC156
#  orientation    | prefix                | "land"             | error, same                      | FR-DC156
#  orientation    | NA                    | NA_character_      | error, same                      | FR-DC156
#  orientation    | length 0              | character(0)       | error, same                      | FR-DC156
#  width_in       | valid (with height)   | 8.5 x 14           | \paperw12240\paperh20160         | FR-DC157
#  width_in       | 0 (just outside min)  | 0                  | error "`width_in` must be a single positive number" | FR-DC158
#  width_in       | Inf                   | Inf                | error, same                      | FR-DC352
#  height_in      | NaN                   | NaN                | error "`height_in` must be ..."  | FR-DC352
#  width_in       | NA                    | NA_real_           | error, same                      | FR-DC158
#  width_in       | length 2              | c(8.5, 11)         | error, same                      | FR-DC158
#  width_in       | length 0              | numeric(0)         | error, same                      | FR-DC158
#  width_in       | logical               | TRUE               | error, same                      | FR-DC158
#  margin_left_in | just inside min       | 0.01               | \margl14                         | FR-DC159
#  margin_top_in  | 0                     | 0                  | error "`margin_top_in` must be ..." | FR-DC160
#  margin_bottom_in | Inf                 | Inf                | error, same                      | FR-DC352
#  margin_right_in| NA                    | NA_real_           | error, same                      | FR-DC160
#  margin_left_in | length 2              | c(0.5, 0.5)        | error, same                      | FR-DC160
#  margin_top_in  | a string              | "1"                | error, same                      | FR-DC160
#  header_dist_in | NULL                  | NULL               | \headery1080 (the top margin)    | FR-DC161
#  header_dist_in | valid                 | 0.3                | \headery432                      | FR-DC161
#  header_dist_in | 0                     | 0                  | error "`header_dist_in` must be ..." | FR-DC160
#  header_dist_in | Inf                   | Inf                | error, same                      | FR-DC352
#  footer_dist_in | valid                 | 0.3                | \footery432                      | FR-DC161
#  footer_dist_in | negative              | -0.1               | error "`footer_dist_in` must be ..." | FR-DC160
#  (A margin that leaves no writable width is accepted and writes negative
#  cell positions: #847, not tested here.)
#
# ---------------------------------------------------------------------------
#  rtftable()  -- FT-TB773.  The table is written in a document; "object"
#  means the rtftable is checked.
# ---------------------------------------------------------------------------
#  argument       | class                 | value              | expected                         | FR
#  data           | 0 rows                | df[0, ]            | the header row written           | FR-TB561
#  data           | list of n             | list(df, df)       | written                          | FR-TB561
#  data           | length 0              | list()             | error "`data` must be a data.frame or a non-empty list" | FR-TB561
#  data           | NULL                  | NULL               | error, same                      | FR-TB561
#  data           | another type          | a matrix           | error, same                      | FR-TB561
#  data           | frames of 2 widths    | list(df, df[1])    | error "All data.frames in `data` must have the same number of columns" | FR-TB561
#  col_header     | valid                 | c("Parameter", "Result") | Result in a cell           | FR-TB562
#  col_header     | wrong length          | 3 labels           | error "`col_header` row 1 has 3 labels" | FR-TB562
#  col_header     | another type          | 1:2                | error "`col_header` must be NULL, character, or a list" | FR-TB562
#  col_header_align | unknown             | "bogus"            | error "`col_header_align` values must be" | FR-TB564
#  col_header_align | NA                  | NA                 | error, same                      | FR-TB564
#  col_header_align | length 0            | character(0)       | error "`col_header_align` must have length 1 or 2" | FR-TB564
#  row_title      | 0 (just below)        | 0                  | error "`row_title` indices must be within 1..2" | FR-TB547
#  row_title      | n + 1 (just above)    | 3                  | error, same                      | FR-TB547
#  row_title      | unknown name          | "Z"                | error "`row_title` column(s) not found" | FR-TB547
#  row_title      | NA                    | NA                 | error "`row_title` must be NULL, an integer vector" | FR-TB547
#  border         | another case          | "TFL"              | error "`border` must be"         | FR-TB566
#  border         | NA                    | NA                 | error, same                      | FR-TB566
#  blank_rows     | 0 (before the first)  | 0                  | object: blank_rows 0             | FR-TB567
#  blank_rows     | -1 (after the last)   | -1                 | object: blank_rows 2             | FR-TB567
#  blank_rows     | n + 1                 | 3                  | warning "out of range"           | FR-TB567
#  blank_rows     | NA                    | NA                 | error "Unrecognised `blank_rows` specification" | FR-TB567
#  table_width_twips | valid              | 5000               | last \cellx5000                  | FR-TB568
#  table_width_twips | NA                 | NA                 | error "`table_width_twips` must be a single non-negative integer" | FR-TB571
#  table_width_twips | Inf                | Inf                | error, same                      | FR-TB571
#  table_width_twips | length 2           | c(1, 2)            | error, same                      | FR-TB571
#  table_width_pct_of_writable | max      | 1                  | last \cellx13680 (writable)      | FR-TB548
#  table_width_pct_of_writable | inside   | 0.5                | last \cellx6840                  | FR-TB548
#  table_width_pct_of_writable | above max| 1.01               | error "must be a number in (0, 1]" | FR-TB572
#  table_width_pct_of_writable | min (excl)| 0                 | error, same                      | FR-TB572
#  table_width_pct_of_writable | Inf      | Inf                | error, same                      | FR-TB572
#  table_width_pct_of_writable | NA       | NA                 | error, same                      | FR-TB572
#  table_width_pct | max                  | 100                | last \cellx13680                 | FR-TB572
#  table_width_pct | above max            | 100.1              | error "must be a number in (0, 100]" | FR-TB572
#  table_width_pct | min (excl)           | 0                  | error, same                      | FR-TB572
#  table_align    | valid                 | "center" / "right" | \trqc / \trqr                    | FR-TB549
#  table_align    | another case          | "Center"           | error "`table_align` must be"    | FR-TB592
#  table_align    | prefix                | "cent"             | error, same                      | FR-TB592
#  table_align    | NA                    | NA                 | error, same                      | FR-TB549
#  table_align    | length 2              | c("left", "left")  | error, same                      | FR-TB592
#  table_align    | length 0              | character(0)       | error, same                      | FR-TB592
#  table_align    | NULL                  | NULL               | error, same                      | FR-TB592
#  cell_valign    | valid                 | "top"              | \clvertalt                       | FR-TB555
#  cell_valign    | unknown               | "middle"           | error "`cell_valign` must be"    | FR-TB555
#  cell_valign    | length 2              | c("top", "top")    | error, same                      | FR-TB592
#  cell_valign    | length 0              | character(0)       | error, same                      | FR-TB592
#  row_height_exact | TRUE                | TRUE               | \trrh-                           | FR-TB552
#  row_height_exact | NA                  | NA                 | error "`row_height_exact` must be TRUE or FALSE" | FR-TB593
#  row_height_exact | length 2            | c(TRUE, FALSE)     | error, same                      | FR-TB593
#  row_height_exact | a string            | "TRUE"             | error, same                      | FR-TB593
#  read_attributes | NA                   | NA                 | error "`read_attributes` must be TRUE or FALSE" | FR-TB593
#  read_attributes | a string             | "yes"              | error, same                      | FR-TB593
#  read_attributes | length 2             | c(TRUE, FALSE)     | error, same                      | FR-TB593
#  row_height_twips | 0 (min: automatic)  | 0                  | no \trrh                         | FR-TB569
#  row_height_twips | valid               | 400                | \trrh400                         | FR-TB569
#  row_height_twips | Inf / NaN / NA_character_ / numeric(0) / 3e9 (above the integer range) | error "`row_height_twips` must be a single non-negative integer" | FR-TB571
#  font_size_half_points | 1 (min)        | 1                  | \fs1                             | FR-TB550
#  font_size_half_points | 0 / Inf / NA   |                    | error "`font_size_half_points` must be a single positive integer" | FR-TB550
#  font           | non-ASCII             | Mincho in kanji    | ☒6?☹7? in \fonttbl     | FR-TB551
#  font           | "" / NA / unnamed length 2 / length 0 | | error "`font` must be a single font family name" | FR-TB551
#  markup         | "all"                 | "all"              | >= as 蠅                     | FR-TB558
#  markup         | length 0              | character(0)       | ^{2} left as text                | FR-TB558
#  markup         | prefix / unknown      | "scr" / "bogus"    | error "`markup` must be a subset" | FR-TB558
#  blank_row_normalize | prefix           | "det"              | error "`blank_row_normalize` must be a subset" | FR-TB557
#  blank_row_normalize | a repeat         | c(detect, detect)  | object: "detect"                 | FR-TB557
#  blank_row_normalize | "none"           | "none"             | object: none                     | FR-TB557
#  col_rel_width  | valid                 | c(3, 1)            | \cellx10260                      | FR-TB568
#  col_rel_width  | NA / a string / Inf / 0 |                  | error at writing "`col_rel_width` values must be positive" | FR-TB594
#  col_rel_width  | wrong length          | c(1, 1, 1)         | error "`col_rel_width` length (3) must match ncol (2)" | FR-TB594
#  column_widths_twips | valid            | c(2000, 1000)      | \cellx2000 ... \cellx3000        | FR-TB568
#  column_widths_twips | -1 / NA / 0 / 1.5 / a string / Inf | | error at writing "`column_widths_twips` values must be positive whole numbers" | FR-TB594
#  column_widths_twips | wrong length     | 1000               | error "`column_widths_twips` length (1) must match ncol (2)" | FR-TB594
#  cell_styles    | length 0              | list()             | error "`cell_styles` length (0) must equal" | FR-TB556
#  cell_styles    | another type          | "x"                | error "`cell_styles` must be a list" | FR-TB556
#  (Fractions, strings and TRUE given to a *_twips or font size argument
#  are coerced, and markup = NA is read as "none": #846, not tested here.)
#
# ---------------------------------------------------------------------------
#  as_rtftables()  -- FT-TB774.  The body has two groups of three rows
#  ("Age", "Sex", each with two indented rows); the pages are checked.
# ---------------------------------------------------------------------------
#  argument       | class                 | value              | expected                         | FR
#  x              | NULL / a number / a matrix |               | error "`as_rtftables()` supports" | FR-TB117
#  x              | length 0              | list()             | no page                          | FR-TB114
#  x              | 0 rows                | df[0, ]            | one page of 0 rows               | FR-TB114
#  split          | prefix                | "row"              | as "rows": pages 3, 3            | FR-TB599
#  split          | another case / "" / NA / length 2 |        | error "'arg' should be one of" / "'arg' must be" | FR-TB599
#  max_rows       | 1 (min), group_force  | 1                  | six pages of 1, no "(Cont.)" row | FR-TB596
#  max_rows       | 1, group_safe         | 1                  | six pages of 1                   | FR-TB596
#  max_rows       | valid                 | 2                  | pages 2, 2, 2, 2                 | FR-TB124
#  max_rows       | n (max useful)        | 6                  | one page of 6                    | FR-TB596
#  max_rows       | n + 1                 | 7                  | one page of 6                    | FR-TB596
#  max_rows       | 0 / -1 / 1.5 / NA / Inf / "2" / TRUE / length 2 | | error "`max_rows` must be a single positive whole number" | FR-TB595
#  max_rows       | 0 under split "none"  | 0                  | error, same                      | FR-TB595
#  split_rows     | n (last row)          | 6                  | pages 5, 1                       | FR-TB126
#  split_rows     | 1 / 0 / n + 1         |                    | one page of 6                    | FR-TB597
#  split_rows     | unsorted              | c(6, 4)            | pages 3, 2, 1                    | FR-TB597
#  split_rows     | length 0              | numeric(0)         | one page of 6                    | FR-TB597
#  split_rows     | NA / 1.5 / "4"        |                    | error "`split_rows` must be whole row positions" | FR-TB597
#  group_col      | position              | 1                  | pages 3, 3                       | FR-TB127
#  group_col      | length 2              | c("G", "V")        | error "`group_col` must be one column" | FR-TB598
#  group_col      | 0 / NA                |                    | error "`group_col` index ... out of range" | FR-TB128
#  group_by       | prefix                | "ind"              | as "indent": pages 3, 3          | FR-TB599
#  group_by       | unknown / NA / length 2 |                  | error "'arg' should be one of" / "'arg' must be" | FR-TB599
#  cont_label     | empty string          | ""                 | the label repeated alone         | FR-TB597
#  cont_label     | non-ASCII             | kanji              | "Age" + it                       | FR-TB137
#  cont_label     | NA / NULL / a number / length 2 |          | error "`cont_label` must be a single string" | FR-TB597
#  min_group_rows | 0 (min)               | 0                  | pages 3, 3                       | FR-TB597
#  min_group_rows | -1 / NA / 1.5 / Inf / "2" / length 2 |     | error "`min_group_rows` must be a single non-negative whole number" | FR-TB597
#  blank_row_first| TRUE                  | TRUE               | a blank row at 0                 | FR-TB141
#  blank_row_first| NA / "yes" / length 2 / NULL |             | error "`blank_row_first` must be TRUE or FALSE" | FR-TB598
#  blank_row_end  | NA / 1                |                    | error "`blank_row_end` must be TRUE or FALSE" | FR-TB598
#  count_blank_rows | NA / "yes"          |                    | error "`count_blank_rows` must be TRUE or FALSE" | FR-TB598
#  auto_width     | NA / length 2         |                    | error "`auto_width` must be TRUE or FALSE" | FR-TB598
#  na             | valid                 | "-"                | a missing value written "-"      | FR-TB147
#  na             | NA / length 0         |                    | error "`na` must be a single string" | FR-TB149
#  drop_cols      | position              | 2                  | one column left                  | FR-TB153
#  drop_cols      | 0 / unknown name      |                    | error "`drop_cols` index 0 out of range" / "column 'Z' not found" | FR-TB154
#  sort_desc      | NA / length 2 for one key |                | error "`sort_desc` must be TRUE / FALSE" / "must have length" | FR-TB135
#  page_by        | valid                 | "G"                | six pages                        | FR-TB131
#  page_by        | NA                    | NA                 | error "`page_by` index NA out of range" | FR-TB133
#  collapse_repeats | NA                  | NA                 | error "`collapse_repeats` index NA out of range" | FR-TB152
#
#  Left for batch 2: rtf_titles() / rtf_footnotes(), rtf_header() /
#  rtf_footer() and rtf_style(), whose style arguments share the checks
#  above (and #846); as_rtftables(read_meta = , header_sep = , listing = ,
#  stub = , cell_format = , first_rows = ).
# ---------------------------------------------------------------------------

.eb_df <- data.frame(Param = c("Age", "Weight"), Value = c(">=65^{2}", NA),
                     stringsAsFactors = FALSE)
.eb_grp <- data.frame(G = c("Age", "  Mean", "  SD", "Sex", "  F", "  M"),
                      V = c("40", "41", "3", "x", "5", "6"),
                      stringsAsFactors = FALSE)

.eb_rtf <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  suppressMessages(generate_rtfreport(doc, f))
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# One case: `expect` is "ok:<regex>" (the result matches), "no:<regex>" (it
# does not), "ok" (chk(result) is TRUE), "warning:<regex>" or
# "error:<regex>".  `more` holds the other arguments of the call.
.eb_case <- function(arg, cls, val, expect, chk = NULL, more = list()) {
  list(arg = arg, cls = cls, val = val, expect = expect, chk = chk,
       more = more)
}
.eb_cases <- function(...) {
  x <- list(...)
  d <- data.frame(arg = vapply(x, function(e) e$arg, ""),
                  cls = vapply(x, function(e) e$cls, ""),
                  expect = vapply(x, function(e) e$expect, ""),
                  stringsAsFactors = FALSE)
  d$val  <- lapply(x, function(e) e["val"])
  d$chk  <- lapply(x, function(e) e$chk)
  d$more <- lapply(x, function(e) e$more)
  d
}
# Run every case of `cases` through run(args, case).
.eb_run <- function(cases, run) {
  for (i in seq_len(nrow(cases))) {
    cs   <- cases[i, ]
    args <- c(setNames(cs$val[[1L]], cs$arg), cs$more[[1L]])
    lab  <- sprintf("%s = <%s>", cs$arg, cs$cls)
    kind <- sub(":.*", "", cs$expect)
    pat  <- sub("^[a-z]+:", "", cs$expect)
    if (kind == "error") {
      expect_error(run(args, cs), pat, info = lab)
    } else if (kind == "warning") {
      expect_warning(run(args, cs), pat, info = lab)
    } else {
      res <- run(args, cs)
      if (kind == "no") {
        expect_false(grepl(pat, res), info = lab)
      } else if (identical(cs$expect, "ok")) {
        expect_true(isTRUE(cs$chk[[1L]](res)), info = lab)
      } else {
        expect_true(grepl(pat, res), info = lab)
      }
    }
  }
}

.eb_kanji2 <- "明朝"            # a font family in kanji
.eb_bw <- paste0("\\{\\\\colortbl;\\\\red0\\\\green0\\\\blue0;",
                 "\\\\red255\\\\green255\\\\blue255;\\}")

test_that("FT-DC281 [FR-DC102 FR-DC105 FR-DC106 FR-DC109 FR-DC110 FR-DC114 FR-DC116 FR-DC120 FR-DC122 FR-DC347 FR-DC348 FR-DC349 FR-DC350 FR-DC351]: rtf_document(): equivalence classes and boundaries of its arguments", {
  C <- .eb_case
  cases <- .eb_cases(
    C("font_table", "valid", list(list(name = "Arial")), "ok:\\\\f0\\\\fnil\\\\fcharset0 Arial;"),
    C("font_table", "NULL", NULL, "ok:\\\\f0\\\\fnil\\\\fcharset0 Courier New;"),
    C("font_table", "non-ASCII name", list(list(name = .eb_kanji2)), "ok:\\\\u26126\\?\\\\u26397\\?;"),
    C("font_table", "long name", list(list(name = strrep("F", 120))), paste0("ok:", strrep("F", 120), ";")),
    C("font_table", "another type", "Arial", "error:^`font_table` must be a non-empty list"),
    C("font_table", "length 0", list(), "error:^`font_table` must be a non-empty list"),
    C("font_table", "name a number", list(list(name = 1)), "error:^`font_table` must be a non-empty list"),
    C("font_table", "name NA", list(list(name = NA_character_)), "error:^`font_table` must be a single font family name"),
    C("font_table", "name empty", list(list(name = "")), "error:^`font_table` must be a single font family name"),
    C("color_table", "valid, n", c("#000000", "#1F4E79"), "ok:\\\\red31\\\\green78\\\\blue121;"),
    C("color_table", "lower-case hex", "#1f4e79", "ok:\\\\red31\\\\green78\\\\blue121;"),
    C("color_table", "length 0", character(0), paste0("ok:", .eb_bw)),
    C("color_table", "NULL", NULL, paste0("ok:", .eb_bw)),
    C("color_table", "a colour name", "red", "error:^`color_table` must be a character vector of 6-digit hex"),
    C("color_table", "5 digits", "#1F4E7", "error:^`color_table` must be a character vector"),
    C("color_table", "7 digits", "#1F4E790", "error:^`color_table` must be a character vector"),
    C("color_table", "NA", NA_character_, "error:^`color_table` must be a character vector"),
    C("color_table", "another type", 1, "error:^`color_table` must be a character vector"),
    C("page", "length 0", list(), "ok:\\\\paperw15840\\\\paperh12240"),
    C("page", "one key", list(paper_size = "A4"), "ok:\\\\paperw16838"),
    C("page", "negative margin", list(margin_top_in = -1), "error:^`margin_top_in` must be a single positive number of inches"),
    C("page", "margin a string", list(margin_left_in = "1"), "error:^`margin_left_in` must be a single positive number"),
    C("page", "Inf", list(width_in = Inf), "error:^`width_in` must be a single positive number"),
    C("page", "paper of length 2", list(paper_size = c("A4", "A3")), "error:^`paper_size` must be a single preset name"),
    C("page", "unknown orientation", list(orientation = "land"), "error:^`orientation` must be"),
    C("default_format", "another type", "x", "error:^`default_format` must be an rtf_default_format"),
    C("watermark", "valid", "DRAFT", "ok:DRAFT"),
    C("watermark", "empty string", "", "no:\\\\shp"),
    C("watermark", "NA", NA, "no:\\\\shp"),
    C("watermark", "length 0", character(0), "error:^`watermark` must be an rtf_watermark object"),
    C("watermark", "length 2", c("A", "B"), "error:^`watermark` must be an rtf_watermark object"),
    C("program", "empty string", "", "error:^`program` must be a single string"),
    C("program_fallback", "empty string", "", "error:^`program_fallback` must be a single string"),
    C("program_fallback", "NA", NA_character_, "error:^`program_fallback` must be a single string"),
    C("program_fallback", "length 2", c("a", "b"), "error:^`program_fallback` must be a single string"),
    C("tokens", "empty value", list(STUDY = ""), "ok:S=\\|"),
    C("tokens", "non-ASCII value", list(STUDY = "試験"), "ok:S=\\\\u-30106\\?\\\\u-26093\\?\\|"),
    C("tokens", "a number", list(STUDY = 5), "ok:S=5\\|"),
    C("tokens", "long value", list(STUDY = strrep("x", 300)), paste0("ok:S=", strrep("x", 300), "\\|")),
    C("tokens", "NULL value", list(STUDY = NULL), "error:^`tokens`: `STUDY` is one value"),
    C("tokens", "NA value", list(STUDY = NA), "error:^`tokens`: `STUDY` is one value"),
    C("font_lang", "length 0", character(0), "error:^`font_lang` must be a named character vector"),
    C("font_lang", "NA", NA, "error:^`font_lang` must be a named character vector"),
    C("font_lang", "another case", c(latin = "JA"), "error:^`font_lang`: unknown language")
  )
  .eb_run(cases, function(args, cs) {
    doc <- do.call(rtf_document, args)
    ttl <- if (cs$arg == "tokens") "S={STUDY}|" else "Table 1"
    .eb_rtf(rtf_tables(doc, .eb_df, titles = ttl))
  })
})

test_that("FT-DC282 [FR-DC151 FR-DC152 FR-DC153 FR-DC154 FR-DC155 FR-DC156 FR-DC157 FR-DC158 FR-DC159 FR-DC160 FR-DC161 FR-DC352]: rtf_page(): equivalence classes and boundaries of its arguments", {
  C <- .eb_case
  letter <- "ok:\\\\paperw15840\\\\paperh12240"
  cases <- .eb_cases(
    C("paper_size", "valid", "A4", "ok:\\\\paperw16838\\\\paperh11905"),
    C("paper_size", "lower case", "a4", "ok:\\\\paperw16838\\\\paperh11905"),
    C("paper_size", "default", "letter", letter),
    C("paper_size", "NULL", NULL, letter),
    C("paper_size", "unknown", "B5", "error:^Unknown `paper_size` \"B5\""),
    C("paper_size", "empty string", "", "error:^Unknown `paper_size` \"\""),
    C("paper_size", "prefix", "let", "error:^Unknown `paper_size` \"let\""),
    C("paper_size", "NA", NA_character_, "error:^Unknown `paper_size` \"NA\""),
    C("paper_size", "length 0", character(0), "error:^`paper_size` must be a single preset name"),
    C("orientation", "valid", "portrait", "ok:\\\\paperw12240\\\\paperh15840"),
    C("orientation", "NULL", NULL, letter),
    C("orientation", "another case", "Portrait", "error:^`orientation` must be"),
    C("orientation", "prefix", "land", "error:^`orientation` must be"),
    C("orientation", "NA", NA_character_, "error:^`orientation` must be"),
    C("orientation", "length 0", character(0), "error:^`orientation` must be"),
    C("width_in", "valid", 8.5, "ok:\\\\paperw12240\\\\paperh20160", more = list(height_in = 14)),
    C("width_in", "0", 0, "error:^`width_in` must be a single positive number of inches"),
    C("width_in", "Inf", Inf, "error:^`width_in` must be a single positive number"),
    C("height_in", "NaN", NaN, "error:^`height_in` must be a single positive number"),
    C("width_in", "NA", NA_real_, "error:^`width_in` must be a single positive number"),
    C("width_in", "length 2", c(8.5, 11), "error:^`width_in` must be a single positive number"),
    C("width_in", "length 0", numeric(0), "error:^`width_in` must be a single positive number"),
    C("width_in", "logical", TRUE, "error:^`width_in` must be a single positive number"),
    C("margin_left_in", "just inside min", 0.01, "ok:\\\\margl14\\\\"),
    C("margin_top_in", "0", 0, "error:^`margin_top_in` must be a single positive number"),
    C("margin_bottom_in", "Inf", Inf, "error:^`margin_bottom_in` must be a single positive number"),
    C("margin_right_in", "NA", NA_real_, "error:^`margin_right_in` must be a single positive number"),
    C("margin_left_in", "length 2", c(0.5, 0.5), "error:^`margin_left_in` must be a single positive number"),
    C("margin_top_in", "a string", "1", "error:^`margin_top_in` must be a single positive number"),
    C("header_dist_in", "NULL", NULL, "ok:\\\\headery1080\\\\"),
    C("header_dist_in", "valid", 0.3, "ok:\\\\headery432\\\\"),
    C("header_dist_in", "0", 0, "error:^`header_dist_in` must be a single positive number"),
    C("header_dist_in", "Inf", Inf, "error:^`header_dist_in` must be a single positive number"),
    C("footer_dist_in", "valid", 0.3, "ok:\\\\footery432\\\\"),
    C("footer_dist_in", "negative", -0.1, "error:^`footer_dist_in` must be a single positive number")
  )
  .eb_run(cases, function(args, cs) {
    .eb_rtf(rtf_tables(rtf_document(page = do.call(rtf_page, args)), .eb_df))
  })
})

test_that("FT-TB773 [FR-TB547 FR-TB548 FR-TB549 FR-TB550 FR-TB551 FR-TB552 FR-TB555 FR-TB556 FR-TB557 FR-TB558 FR-TB561 FR-TB562 FR-TB564 FR-TB566 FR-TB567 FR-TB568 FR-TB569 FR-TB571 FR-TB572 FR-TB592 FR-TB593 FR-TB594]: rtftable(): equivalence classes and boundaries of its arguments", {
  C <- .eb_case
  E <- function(arg, cls, val, pat) C(arg, cls, val, paste0("error:", pat))
  rh  <- "^`row_height_twips` must be a single non-negative integer"
  fs  <- "^`font_size_half_points` must be a single positive integer"
  fnt <- "^`font` must be a single font family name"
  rel <- "^`col_rel_width` values must be positive"
  cw  <- "^`column_widths_twips` values must be positive whole numbers"
  cases <- .eb_cases(
    C("data", "0 rows", .eb_df[0, ], "ok:Param\\\\cell"),
    C("data", "list of n", list(.eb_df, .eb_df), "ok:Weight\\\\cell"),
    E("data", "length 0", list(), "^`data` must be a data.frame or a non-empty list"),
    E("data", "NULL", NULL, "^`data` must be a data.frame or a non-empty list"),
    E("data", "a matrix", as.matrix(.eb_df), "^`data` must be a data.frame or a non-empty list"),
    E("data", "frames of 2 widths", list(.eb_df, .eb_df[1]), "^All data.frames in `data` must have the same number of columns"),
    C("col_header", "valid", c("Parameter", "Result"), "ok:Result\\\\cell"),
    E("col_header", "wrong length", c("A", "B", "C"), "^`col_header` row 1 has 3 labels"),
    E("col_header", "another type", 1:2, "^`col_header` must be NULL, character, or a list"),
    E("col_header_align", "unknown", "bogus", "^`col_header_align` values must be"),
    E("col_header_align", "NA", NA, "^`col_header_align` values must be"),
    E("col_header_align", "length 0", character(0), "^`col_header_align` must have length 1 or 2"),
    E("row_title", "0", 0, "^`row_title` indices must be within 1\\.\\.2"),
    E("row_title", "n + 1", 3, "^`row_title` indices must be within 1\\.\\.2"),
    E("row_title", "unknown name", "Z", "^`row_title` column\\(s\\) not found in data: Z"),
    E("row_title", "NA", NA, "^`row_title` must be NULL, an integer vector, or column names"),
    E("border", "another case", "TFL", "^`border` must be"),
    E("border", "NA", NA, "^`border` must be"),
    C("blank_rows", "0", 0, "ok", chk = function(o) identical(o$blank_rows, 0L)),
    C("blank_rows", "-1", -1, "ok", chk = function(o) identical(o$blank_rows, 2L)),
    C("blank_rows", "n + 1", 3, "warning:out of range"),
    E("blank_rows", "NA", NA, "^Unrecognised `blank_rows` specification"),
    C("table_width_twips", "valid", 5000, "ok:\\\\cellx5000[^0-9]"),
    E("table_width_twips", "NA", NA, "^`table_width_twips` must be a single non-negative integer"),
    E("table_width_twips", "Inf", Inf, "^`table_width_twips` must be a single non-negative integer"),
    E("table_width_twips", "length 2", c(1, 2), "^`table_width_twips` must be a single non-negative integer"),
    C("table_width_pct_of_writable", "max", 1, "ok:\\\\cellx13680[^0-9]"),
    C("table_width_pct_of_writable", "inside", 0.5, "ok:\\\\cellx6840[^0-9]"),
    E("table_width_pct_of_writable", "just above max", 1.01, "^`table_width_pct_of_writable` must be a number in \\(0, 1\\]"),
    E("table_width_pct_of_writable", "min (excluded)", 0, "^`table_width_pct_of_writable` must be a number"),
    E("table_width_pct_of_writable", "Inf", Inf, "^`table_width_pct_of_writable` must be a number"),
    E("table_width_pct_of_writable", "NA", NA, "^`table_width_pct_of_writable` must be a number"),
    C("table_width_pct", "max", 100, "ok:\\\\cellx13680[^0-9]"),
    E("table_width_pct", "just above max", 100.1, "^`table_width_pct` must be a number in \\(0, 100\\]"),
    E("table_width_pct", "min (excluded)", 0, "^`table_width_pct` must be a number"),
    C("table_align", "center", "center", "ok:\\\\trqc"),
    C("table_align", "right", "right", "ok:\\\\trqr"),
    E("table_align", "another case", "Center", "^`table_align` must be"),
    E("table_align", "prefix", "cent", "^`table_align` must be"),
    E("table_align", "NA", NA, "^`table_align` must be"),
    E("table_align", "length 2", c("left", "left"), "^`table_align` must be"),
    E("table_align", "length 0", character(0), "^`table_align` must be"),
    E("table_align", "NULL", NULL, "^`table_align` must be"),
    C("cell_valign", "valid", "top", "ok:\\\\clvertalt"),
    E("cell_valign", "unknown", "middle", "^`cell_valign` must be"),
    E("cell_valign", "length 2", c("top", "top"), "^`cell_valign` must be"),
    E("cell_valign", "length 0", character(0), "^`cell_valign` must be"),
    C("row_height_exact", "TRUE", TRUE, "ok:\\\\trrh-[0-9]"),
    E("row_height_exact", "NA", NA, "^`row_height_exact` must be TRUE or FALSE"),
    E("row_height_exact", "length 2", c(TRUE, FALSE), "^`row_height_exact` must be TRUE or FALSE"),
    E("row_height_exact", "a string", "TRUE", "^`row_height_exact` must be TRUE or FALSE"),
    E("read_attributes", "NA", NA, "^`read_attributes` must be TRUE or FALSE"),
    E("read_attributes", "a string", "yes", "^`read_attributes` must be TRUE or FALSE"),
    E("read_attributes", "length 2", c(TRUE, FALSE), "^`read_attributes` must be TRUE or FALSE"),
    C("row_height_twips", "0 (automatic)", 0, "no:\\\\trrh"),
    C("row_height_twips", "valid", 400, "ok:\\\\trrh400[^0-9]"),
    E("row_height_twips", "Inf", Inf, rh),
    E("row_height_twips", "NaN", NaN, rh),
    E("row_height_twips", "NA string", NA_character_, rh),
    E("row_height_twips", "length 0", numeric(0), rh),
    E("row_height_twips", "above the integer range", 3e9, rh),
    C("font_size_half_points", "1 (min)", 1, "ok:\\\\fs1[^0-9]"),
    E("font_size_half_points", "0", 0, fs),
    E("font_size_half_points", "Inf", Inf, fs),
    E("font_size_half_points", "NA", NA, fs),
    C("font", "non-ASCII", .eb_kanji2, "ok:\\\\u26126\\?\\\\u26397\\?;"),
    E("font", "empty string", "", fnt),
    E("font", "NA", NA_character_, fnt),
    E("font", "unnamed length 2", c("Arial", "Courier New"), fnt),
    E("font", "length 0", character(0), fnt),
    C("markup", "all", "all", "ok:\\\\u8805"),
    C("markup", "length 0", character(0), "ok:\\^\\\\\\{2\\\\\\}"),
    E("markup", "prefix", "scr", "^`markup` must be a subset"),
    E("markup", "unknown", "bogus", "^`markup` must be a subset"),
    E("blank_row_normalize", "prefix", "det", "^`blank_row_normalize` must be a subset"),
    C("blank_row_normalize", "a repeat", c("detect", "detect"), "ok",
      chk = function(o) identical(o$blank_row_normalize, "detect")),
    C("blank_row_normalize", "none", "none", "ok",
      chk = function(o) identical(o$blank_row_normalize, character(0))),
    C("col_rel_width", "valid", c(3, 1), "ok:\\\\cellx10260[^0-9]"),
    E("col_rel_width", "NA", c(NA, 1), rel),
    E("col_rel_width", "a string", c("a", "b"), rel),
    E("col_rel_width", "Inf", c(Inf, 1), rel),
    E("col_rel_width", "0", c(0, 1), rel),
    E("col_rel_width", "wrong length", c(1, 1, 1), "^`col_rel_width` length \\(3\\) must match ncol \\(2\\)"),
    C("column_widths_twips", "valid", c(2000, 1000), "ok:\\\\cellx2000[^0-9].*\\\\cellx3000[^0-9]"),
    E("column_widths_twips", "negative", c(-1, 1000), cw),
    E("column_widths_twips", "NA", c(NA, 1000), cw),
    E("column_widths_twips", "0", c(0, 1000), cw),
    E("column_widths_twips", "a fraction", c(1.5, 1000), cw),
    E("column_widths_twips", "a string", c("a", "b"), cw),
    E("column_widths_twips", "Inf", c(Inf, 1000), cw),
    E("column_widths_twips", "wrong length", 1000, "^`column_widths_twips` length \\(1\\) must match ncol \\(2\\)"),
    E("cell_styles", "length 0", list(), "^`cell_styles` length \\(0\\) must equal"),
    E("cell_styles", "another type", "x", "^`cell_styles` must be a list")
  )
  .eb_run(cases, function(args, cs) {
    if (is.null(names(args)) || !"data" %in% names(args)) {
      args <- c(list(data = .eb_df), args)
    }
    tbl <- do.call(rtftable, args)
    if (!is.null(cs$chk[[1L]])) return(tbl)
    .eb_rtf(rtf_tables(rtf_document(), tbl))
  })
})

test_that("FT-TB774 [FR-TB114 FR-TB117 FR-TB124 FR-TB126 FR-TB127 FR-TB128 FR-TB131 FR-TB133 FR-TB135 FR-TB137 FR-TB141 FR-TB147 FR-TB149 FR-TB152 FR-TB153 FR-TB154 FR-TB595 FR-TB596 FR-TB597 FR-TB598 FR-TB599]: as_rtftables(): equivalence classes and boundaries of its arguments", {
  C <- .eb_case
  E <- function(arg, cls, val, pat, more = list()) {
    C(arg, cls, val, paste0("error:", pat), more = more)
  }
  rows <- function(n) function(p) identical(unname(vapply(p, function(x) nrow(x$data), 1L)), as.integer(n))
  first <- function(v) function(p) identical(unname(vapply(p, function(x) x$data[[1L]][1L], "")), v)
  gf <- function(n) list(split = "group_force", max_rows = n)
  gs <- list(split = "group_safe", max_rows = 3, group_by = "indent")
  mr <- "^`max_rows` must be a single positive whole number"
  sr <- "^`split_rows` must be whole row positions"
  cl <- "^`cont_label` must be a single string"
  mg <- "^`min_group_rows` must be a single non-negative whole number"
  cases <- .eb_cases(
    E("x", "NULL", NULL, "^`as_rtftables\\(\\)` supports"),
    E("x", "a number", 1, "^`as_rtftables\\(\\)` supports"),
    E("x", "a matrix", as.matrix(.eb_grp), "^`as_rtftables\\(\\)` supports"),
    C("x", "length 0", list(), "ok", chk = function(p) length(p) == 0L),
    C("x", "0 rows", .eb_grp[0, ], "ok", chk = rows(0)),
    C("split", "prefix", "row", "ok", chk = rows(c(3, 3)), more = list(split_rows = 4)),
    E("split", "another case", "ROWS", "'arg' should be one of"),
    E("split", "empty string", "", "'arg' should be one of"),
    E("split", "NA", NA, "'arg' must be NULL or a character vector"),
    E("split", "length 2", c("rows", "none"), "'arg' must be of length 1"),
    C("max_rows", "1 (min), group_force", 1, "ok", chk = function(p) {
      rows(rep(1, 6))(p) && first(.eb_grp$G)(p) }, more = list(split = "group_force")),
    C("max_rows", "1, group_safe", 1, "ok", chk = rows(rep(1, 6)), more = list(split = "group_safe")),
    C("max_rows", "valid", 2, "ok", chk = rows(c(2, 2, 2, 2)), more = list(split = "group_force")),
    C("max_rows", "n", 6, "ok", chk = rows(6), more = list(split = "group_force")),
    C("max_rows", "n + 1", 7, "ok", chk = rows(6), more = list(split = "group_force")),
    E("max_rows", "0", 0, mr, more = list(split = "group_force")),
    E("max_rows", "negative", -1, mr, more = list(split = "group_force")),
    E("max_rows", "a fraction", 1.5, mr, more = list(split = "group_force")),
    E("max_rows", "NA", NA, mr, more = list(split = "group_force")),
    E("max_rows", "Inf", Inf, mr, more = list(split = "group_safe")),
    E("max_rows", "a string", "2", mr, more = list(split = "group_force")),
    E("max_rows", "logical", TRUE, mr, more = list(split = "group_force")),
    E("max_rows", "length 2", c(2, 3), mr, more = list(split = "group_force")),
    E("max_rows", "0 under split none", 0, mr),
    C("split_rows", "n (last row)", 6, "ok", chk = rows(c(5, 1)), more = list(split = "rows")),
    C("split_rows", "1", 1, "ok", chk = rows(6), more = list(split = "rows")),
    C("split_rows", "0", 0, "ok", chk = rows(6), more = list(split = "rows")),
    C("split_rows", "n + 1", 7, "ok", chk = rows(6), more = list(split = "rows")),
    C("split_rows", "unsorted", c(6, 4), "ok", chk = rows(c(3, 2, 1)), more = list(split = "rows")),
    C("split_rows", "length 0", numeric(0), "ok", chk = rows(6), more = list(split = "rows")),
    E("split_rows", "NA", NA, sr, more = list(split = "rows")),
    E("split_rows", "a fraction", 1.5, sr, more = list(split = "rows")),
    E("split_rows", "a string", "4", sr, more = list(split = "rows")),
    C("group_col", "position", 1, "ok", chk = rows(c(3, 3)), more = gs),
    E("group_col", "length 2", c("G", "V"), "^`group_col` must be one column", more = gs),
    E("group_col", "0", 0, "^`group_col` index 0 out of range", more = gs),
    E("group_col", "NA", NA, "^`group_col` index NA out of range", more = gs),
    C("group_by", "prefix", "ind", "ok", chk = rows(c(3, 3)), more = gs[1:2]),
    E("group_by", "unknown", "bogus", "'arg' should be one of", more = gs[1:2]),
    E("group_by", "NA", NA, "'arg' must be NULL or a character vector", more = gs[1:2]),
    E("group_by", "length 2", c("indent", "value"), "'arg' must be of length 1", more = gs[1:2]),
    C("cont_label", "empty string", "", "ok", chk = first(c("Age", "Age", "Sex", "Sex")), more = gf(2)),
    C("cont_label", "non-ASCII", "続き", "ok",
      chk = first(c("Age", "Age続き", "Sex", "Sex続き")), more = gf(2)),
    E("cont_label", "NA", NA_character_, cl, more = gf(2)),
    E("cont_label", "NULL", NULL, cl, more = gf(2)),
    E("cont_label", "a number", 1, cl, more = gf(2)),
    E("cont_label", "length 2", c("a", "b"), cl, more = gf(2)),
    C("min_group_rows", "0 (min)", 0, "ok", chk = rows(c(3, 3)), more = gs),
    E("min_group_rows", "negative", -1, mg, more = gs),
    E("min_group_rows", "NA", NA, mg, more = gs),
    E("min_group_rows", "a fraction", 1.5, mg, more = gs),
    E("min_group_rows", "Inf", Inf, mg, more = gs),
    E("min_group_rows", "a string", "2", mg, more = gs),
    E("min_group_rows", "length 2", c(1, 2), mg, more = gs),
    C("blank_row_first", "TRUE", TRUE, "ok", chk = function(p) 0L %in% p[[1L]]$blank_rows),
    E("blank_row_first", "NA", NA, "^`blank_row_first` must be TRUE or FALSE"),
    E("blank_row_first", "a string", "yes", "^`blank_row_first` must be TRUE or FALSE"),
    E("blank_row_first", "length 2", c(TRUE, FALSE), "^`blank_row_first` must be TRUE or FALSE"),
    E("blank_row_first", "NULL", NULL, "^`blank_row_first` must be TRUE or FALSE"),
    E("blank_row_end", "NA", NA, "^`blank_row_end` must be TRUE or FALSE"),
    E("blank_row_end", "a number", 1, "^`blank_row_end` must be TRUE or FALSE"),
    E("count_blank_rows", "NA", NA, "^`count_blank_rows` must be TRUE or FALSE"),
    E("count_blank_rows", "a string", "yes", "^`count_blank_rows` must be TRUE or FALSE"),
    E("auto_width", "NA", NA, "^`auto_width` must be TRUE or FALSE"),
    E("auto_width", "length 2", c(TRUE, FALSE), "^`auto_width` must be TRUE or FALSE"),
    C("na", "valid", "-", "ok", chk = function(p) identical(p[[1L]]$data$V[2L], "-"),
      more = list(x = data.frame(G = c("a", "b"), V = c("1", NA)))),
    E("na", "NA", NA_character_, "^`na` must be a single string"),
    E("na", "length 0", character(0), "^`na` must be a single string"),
    C("drop_cols", "position", 2, "ok", chk = function(p) ncol(p[[1L]]$data) == 1L),
    E("drop_cols", "0", 0, "^`drop_cols` index 0 out of range"),
    E("drop_cols", "unknown name", "Z", "^`drop_cols` column 'Z' not found"),
    E("sort_desc", "NA", NA, "^`sort_desc` must be TRUE / FALSE", more = list(sort_by = "V")),
    E("sort_desc", "length 2 for one key", c(TRUE, FALSE), "^`sort_desc` must have length", more = list(sort_by = "V")),
    C("page_by", "valid", "G", "ok", chk = function(p) length(p) == 6L),
    E("page_by", "NA", NA, "^`page_by` index NA out of range"),
    E("collapse_repeats", "NA", NA, "^`collapse_repeats` index NA out of range")
  )
  .eb_run(cases, function(args, cs) {
    if (!"x" %in% names(args)) args <- c(list(x = .eb_grp), args)
    do.call(as_rtftables, args)
  })
})
