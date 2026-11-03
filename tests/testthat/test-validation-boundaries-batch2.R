# Validation tests: equivalence classes and boundary values of the arguments,
# batch 2 -- rtf_titles(), rtf_footnotes(), rtf_header(), rtf_footer(),
# rtf_style(), the as_rtftables() arguments batch 1 left (read_meta,
# header_sep, listing, stub, cell_format, first_rows), and the scalar
# arguments of the most used plan verbs.  Helpers prefixed .eb2_.
# Fictional data only.
#
# The help is the functional requirement.  For each argument the classes
# are: a valid value of each documented kind, NULL, length 0 / 1 / n / a
# wrong length, NA, a value of another type, the documented minimum and
# maximum with a value just inside and just outside, 0, a negative number,
# a fraction for a whole number, Inf / NaN, "" / a non-ASCII / a long
# string, and an unknown choice / a prefix.  A class another test already
# checks (FT-TB775: a fraction, a string, TRUE or NA given to a twips or
# font-size argument, and markup = NA, in every function) is not repeated.
# Each table below is one row per case; the test of the function runs them
# in a loop.  "object" means the returned object is checked, not the file.
#
# ---------------------------------------------------------------------------
#  rtf_titles()  -- FT-DC284.  A document of one table (two for "n").
# ---------------------------------------------------------------------------
#  argument       | class                 | value              | expected                         | FR
#  doc            | another type          | list()             | error "`doc` must be an rtf_document object" | FR-DC197
#  doc            | no content            | rtf_document()     | error "Cannot set titles before any content" | FR-DC198
#  titles         | valid string          | "Table 1"          | Table 1 written                  | FR-DC188
#  titles         | length 1 for 2 pages  | list("Tcommon")    | written twice                    | FR-DC187
#  titles         | n for n               | list("TA", "TB")   | TB written                       | FR-DC186
#  titles         | wrong length          | 2 blocks, 1 page   | error "`titles` has 2 blocks but the document has 1 page" | FR-DC200
#  titles         | length 0              | list()             | error "`titles` has 0 blocks"    | FR-DC200
#  titles         | NULL                  | NULL               | error "`titles` must be a list"  | FR-DC199
#  titles         | another type          | 1                  | error, same                      | FR-DC199
#  titles         | NA (logical)          | NA                 | error, same                      | FR-DC199
#  titles         | empty string          | ""                 | a blank row                      | FR-DC189
#  titles         | character length 0    | character(0)       | no title row                     | FR-DC189
#  titles         | NA row               | c("TT", NA)        | a blank row, no "NA"             | FR-DC355
#  titles         | non-ASCII             | kanji              | \u-30106?\u-26093?               | FR-DC189
#  titles         | long (500 chars)      | "xxx...x"          | written whole                    | FR-DC189
#  font_size_half_points | 1 (min)        | 1                  | object: 1                        | FR-DC192
#  font_size_half_points | 0 / -1 / Inf / length 2 / length 0 | | error "`rtf_titles(font_size_half_points)` must be a single positive integer" | FR-DC201
#  row_height_twips | 0 (min)             | 0                  | object: 0                        | FR-DC193
#  row_height_twips | -1 / Inf / 3e9 / length 0 |              | error "`rtf_titles(row_height_twips)` must be a single non-negative integer" | FR-TB600
#  markup         | "none"                | "none"             | ^{2} left as text                | FR-DC194
#  markup         | "all"                 | "all"              | >= as 蠅                     | FR-DC194
#  markup         | prefix / unknown / "" | "scr" / "bogus" / "" | error "`markup` must be a subset" | FR-DC194
#  font           | non-ASCII             | Mincho in kanji    | ☒6?☹7? in \fonttbl     | FR-DC196
#  font           | "" / NA / length 0 / unnamed length 2 / a number | | error "`rtf_titles(font)` must be a single font family name" | FR-DC196
#  font           | unknown slot          | c(latin=, bogus=)  | error "unknown font slot"        | FR-DC196
#  font           | no latin slot         | c(east_asian = )   | error "must name a `latin` font" | FR-DC196
#  align          | valid                 | "right"            | \qr                              | FR-DC195
#  align          | another case / prefix / NA / length 2 / length 0 / a number / unknown | | error "`rtf_titles(align)` must be" | FR-DC195
#
# ---------------------------------------------------------------------------
#  rtf_footnotes()  -- FT-DC285.  A document of one table (two for "n").
# ---------------------------------------------------------------------------
#  argument       | class                 | value              | expected                         | FR
#  doc            | another type          | list()             | error "`doc` must be an rtf_document object" | FR-DC213
#  doc            | no content            | rtf_document()     | error "Cannot set footnotes before any content" | FR-DC214
#  footnotes      | valid string          | "Source: ADSL"     | written                          | FR-DC205
#  footnotes      | length 1 for 2 pages  | list("Fcommon")    | written twice                    | FR-DC204
#  footnotes      | NULL element          | list(NULL)         | object: no footnote              | FR-DC203
#  footnotes      | l / r cells           | c(l = , r = )      | the right cell written           | FR-DC206
#  footnotes      | a cell named "left"   | c(left = "L")      | error at writing "A footnote row may only be named l / c / r" | FR-DC218
#  footnotes      | non-ASCII / long      |                    | written                          | FR-DC205
#  footnotes      | NULL / a number       |                    | error "`footnotes` must be a list" | FR-DC215
#  footnotes      | length 0 / 3 for 2    |                    | error "`footnotes` has 0 blocks" / "has 3 blocks" | FR-DC216
#  border         | valid                 | rtf_border(top=TRUE) | object: the block's border     | FR-DC207
#  border         | "top" / TRUE / NA / a plain list |         | error "`rtf_footnotes(border = )` must be NULL or an rtf_border() object" | FR-DC217
#  font_size_half_points | 0              | 0                  | error "`rtf_footnotes(font_size_half_points)` must be" | FR-DC219
#  row_height_twips | -1                  | -1                 | error "`rtf_footnotes(row_height_twips)` must be" | FR-TB600
#  align          | unknown / NA          | "middle" / NA      | error "`rtf_footnotes(align)` must be" | FR-DC219
#  font           | ""                    | ""                 | error "`rtf_footnotes(font)` must be" | FR-DC212
#  markup         | unknown               | "bogus"            | error "`markup` must be a subset" | FR-DC210
#
# ---------------------------------------------------------------------------
#  rtf_header()  -- FT-DC286, and rtf_footer()  -- FT-DC287: the same
#  cases, each band written in a document of one table.
# ---------------------------------------------------------------------------
#  argument       | class                 | value              | expected                         | FR
#  rows           | valid                 | c(l = , r = )      | both cells written               | FR-DC325
#  rows           | unnamed string        | "HDRX"             | written                          | FR-DC325
#  rows           | n rows                | list of 3          | object: 3 rows                   | FR-DC325
#  rows           | length 0              | list()             | object: no row                   | FR-DC325
#  rows           | non-ASCII / long (400)|                    | written                          | FR-DC325
#  rows           | NULL / a number       |                    | error "`rows` must be a named character vector" | FR-DC325
#  rows           | a cell named "left"   | c(left = "A")      | error "row 1: a cell may only be named l, c or r" | FR-DC325
#  rows           | a row height of -1    | list(columns=, row_height_twips=-1) | error "row 1's `row_height_twips` must be" | FR-DC302
#  rows           | a row colour "red"    | list(columns=, color="red") | error "row 1's `color` must be" | FR-DC297
#  border         | valid                 | rtf_border(bottom=TRUE) | object: an rtf_border       | FR-DC326
#  border         | NULL                  | NULL               | object: none                     | FR-DC326
#  border         | "top" / TRUE / NA     |                    | error "`border` must be NULL or an rtf_border object" | FR-DC326
#  width_twips    | valid                 | 5000               | \cellx5000                       | FR-DC327
#  width_twips    | -1 / 1.5 / NA / Inf / "5000" / length 2 / length 0 | | error "`width_twips` must be a single non-negative integer" | FR-DC343
#  width          | "page" (default)      | "page"             | \cellx13680 (writable)           | FR-DC327
#  width          | fraction inside       | 0.5                | \cellx6840                       | FR-DC327
#  width          | fraction max          | 1                  | \cellx13680                      | FR-DC327
#  width          | twips, smallest       | 2                  | \cellx2                          | FR-DC327
#  width          | twips                 | 5000               | \cellx5000                       | FR-DC327
#  width          | just above max fraction, not whole | 1.01  | error "`width` must be "content", "page", a fraction in (0, 1], or twips" | FR-DC354
#  width          | fraction of twips / Inf / NaN | 1.5 / Inf / NaN | error, same               | FR-DC354
#  width          | 0 / negative / NA / another case / prefix / length 2 / length 0 / TRUE | | error, same | FR-DC327
#  font_size_half_points | 1 (min)        | 1                  | object: 1                        | FR-DC328
#  font_size_half_points | 0 / Inf        |                    | error "`font_size_half_points` must be" | FR-TB600
#  row_height_twips | 0 (min)             | 0                  | object: 0                        | FR-DC342
#  row_height_twips | -1 / Inf            |                    | error "`row_height_twips` must be" | FR-DC343
#  cell_padding_top_twips | 0 / -1 / Inf  |                    | object: 0 / error "`cell_padding_top_twips` must be" | FR-DC300
#  font           | non-ASCII / ""        |                    | ☒6? / error "`font` must be" | FR-DC341
#  markup         | prefix                | "scr"              | error "`markup` must be a subset" | FR-DC329
#  drop_empty_rows| TRUE                  | TRUE               | object: TRUE                     | FR-DC331
#  drop_empty_rows| NA / "yes" / length 2 / NULL / 1 |         | error "`drop_empty_rows` must be TRUE or FALSE" | FR-DC331
#  color          | lower-case hex        | "#7f7f7f"          | \red127\green127\blue127         | FR-DC295
#  color          | a name / 5 digits / 7 digits / NA / length 2 / length 0 / "" / a number | | error "`color` must be NULL or a 6-digit hex colour" | FR-DC297
#  (rtf_footer() draws a top rule by default; border = NULL takes it away.)
#  (width_twips = 0 writes a band of no width: #851.)
#
# ---------------------------------------------------------------------------
#  rtf_style()  -- FT-DC288.  The style is checked as an object, or given
#  to a document of one table and written.
# ---------------------------------------------------------------------------
#  argument       | class                 | value              | expected                         | FR
#  ...            | unknown name          | fnt = "Arial"      | error "`rtf_style()`: unknown argument `fnt`" | FR-DC321
#  (none)         | all NULL              | rtf_style()        | prints "(nothing set)"           | FR-DC324
#  font           | non-ASCII             | Mincho in kanji    | ☒6?☹7? in \fonttbl     | FR-DC309
#  font           | "" / NA / length 0 / unnamed length 2 | | error "`rtf_style(font)` must be" | FR-DC322
#  font_size_half_points | 1 (min)        | 1                  | \fs1                             | FR-DC309
#  font_size_half_points | 0 / Inf / length 2 |                | error "`rtf_style(font_size_half_points)` must be" | FR-DC322
#  row_height_twips | 0 (min) / -1 / Inf  |                    | object: 0 / error                | FR-DC322
#  cell_padding_left_twips | 0 / -1       |                    | object: 0 / error                | FR-DC322
#  color          | lower-case hex        | "#7f7f7f"          | object: "#7F7F7F"                | FR-DC310
#  color          | a name / NA / "" / length 2 / 5 digits | | error "`rtf_style(color)` must be NULL or a 6-digit hex colour" | FR-DC322
#  title_format   | valid                 | "table"            | object: "table"                  | FR-DC311
#  title_format   | another case / prefix / NA / "" / length 0 / length 2 / a number | | error "`rtf_style(title_format)` must be "text" or "table"" | FR-DC322
#  footnote_format| another case          | "TEXT"             | error, same                      | FR-DC322
#  title_width    | "page" / 1 (max fraction) / 0.5 / 4000 twips | | \cellx13680 / \cellx13680 / \cellx6840 / \cellx4000 | FR-DC311
#  title_width    | 1.01 / 1.5 / Inf      |                    | error "`rtf_style(title_width)` must be" | FR-DC354
#  title_width    | 0 / -1 / NA / another case / length 2 | | error, same                      | FR-DC322
#  footnote_width | Inf                   | Inf                | error "`rtf_style(footnote_width)` must be" | FR-DC354
#  table, titles, footnotes, header, footer | a string | "x"   | error "`rtf_style(<s> = )` must be a named list" | FR-DC322
#    (each section) | unnamed entry       | list(18)           | error "every entry must be named" | FR-DC320
#    (each section) | unknown key         | list(bogus = 1)    | error "unknown key `bogus`"      | FR-DC320
#    (each section) | a key twice         | list(font =, font =) | error "`font` is given twice"  | FR-DC320
#    (each section) | length 0            | list()             | object: nothing set              | FR-DC312
#    (each section) | an rtf_style()      | rtf_style()        | error "must be a named list"     | FR-DC322
#    (each section) | a NULL value        | list(font = NULL)  | object: nothing set              | FR-DC312
#  table          | cell_valign unknown / NA, table_align another case, row_height_exact NA, header_row_height_twips -1 | | error "`rtf_style(table)$<key>` must be" | FR-DC322
#  titles         | align unknown         | "x"                | error "`rtf_style(titles)$align` must be" | FR-DC322
#  titles         | border (not its key)  | rtf_border()       | error "unknown key `border`"     | FR-DC320
#  footnotes      | border a string       | "top"              | error "`rtf_style(footnotes)$border` must be" | FR-DC322
#  header         | color a name          | "red"              | error "`rtf_style(header)$color` must be" | FR-DC322
#
# ---------------------------------------------------------------------------
#  as_rtftables()  -- FT-TB776.  The arguments batch 1 left; the pages are
#  checked.
# ---------------------------------------------------------------------------
#  argument       | class                 | value              | expected                         | FR
#  read_meta      | TRUE (default)        | TRUE               | the label attribute is the header | FR-TB119
#  read_meta      | FALSE / NULL          |                    | no header from the label         | FR-TB118
#  read_meta      | token                 | "labels"           | the label                        | FR-TB119
#  read_meta      | with an adapter token | c("labels", "spanning") | the label (the other ignored) | FR-TB119
#  read_meta      | unknown token / length 0 | "bogus" / character(0) | no label                 | FR-TB119
#  read_meta      | NA / a number / length 2 / a token NA | | error "`read_meta` must be FALSE/TRUE or a character vector of tokens" | FR-TB120
#  header_sep     | valid (default)       | "____"             | two header rows                  | FR-TB162
#  header_sep     | NULL / length 0       |                    | one header row                   | FR-TB162
#  header_sep     | with an NA            | c("____", NA)      | two header rows                  | FR-TB602
#  header_sep     | "" / not found        | "" / "x"           | one header row                   | FR-TB602
#  header_sep     | a number / TRUE       | 1 / TRUE           | error "`header_sep` must be NULL or a character vector" | FR-TB602
#  listing        | valid                 | listing_spec()     | the spec's header                | FR-TB155
#  listing        | a string / list() / TRUE / NA |            | error "`listing` must be a listing_spec()" | FR-TB155
#  listing        | on a built listing    | build_listing()    | error "already built by build_listing()" | FR-IX355
#  stub           | names / positions     | c("SOC", "PT") / 1:2 | an indented stub of 5 rows     | FR-TB158
#  stub           | one column            | "SOC"              | kept as it is                    | FR-TB591
#  stub           | a stub_spec()         | indent = 2         | two-space indent                 | FR-TB158
#  stub           | length 0              | character(0)       | error "`vars` names no column"   | FR-TB203
#  stub           | NA / 0 / n + 1 / unknown name / a repeat | | error "out of range" / "not found" / "distinct" | FR-TB203
#  stub           | indent -1 / 1.5 / "2" |                    | error "`indent` must be a single non-negative integer" | FR-TB204 FR-TB605
#  stub           | group_summary NA / "" |                    | error "`group_summary` must be a subset" | FR-TB605
#  cell_format    | a name / "fmt_" prefix | "count_paren" / "fmt_count_paren" | column 2 formatted, column 1 left | FR-TB145
#  cell_format    | a list                | list(NULL, "right_align") | column 2 right-aligned    | FR-TB145
#  cell_format    | length 0 list / an entry not a name | list() / list(NULL, 1) | unchanged      | FR-TB145
#  cell_format    | a function            | toupper            | applied                          | FR-TB145
#  cell_format    | unknown / "" / another case |              | error "is not a built-in cell format" | FR-TB146
#  cell_format    | NA / a number / TRUE / length 2 / length 0 | | error "`cell_format` must be a function, the name of a built-in" | FR-TB145
#  cell_format    | a function of the wrong length |          | error "must return a vector the same length" | FR-TB146
#  first_rows     | 1 (min) / 2 / n - 1 with group_force, 3 | | pages 1,3,3 / 2,2,3 / 5,2 (the next page opens with a continuation row, #856) | FR-TB150 FR-TB610
#  first_rows     | n / n + 1             | 6 / 7              | one page of 6                    | FR-TB150
#  first_rows     | 0 / -1 / 1.5 / NA / Inf / "2" / TRUE / length 2 / length 0 | | error "as_rtftables(): `first_rows` is a whole number of rows, 1 or more" | FR-TB603
#  first_rows     | with page_by / by_value |                  | error "`first_rows` paginates"   | FR-TB151
#  stub, group_col, collapse_repeats, at | TRUE / 1.5 / a factor | | error "must be a column name or a whole position" | FR-TB606 (FT-TB777, #850)
#
# ---------------------------------------------------------------------------
#  plan_paginate_rows()  -- FT-PL967.  Six records, no groups.
# ---------------------------------------------------------------------------
#  max_rows       | 1 (min) / 2 / n / n + 1 |                  | pages 1 x 6 / 2,2,2 / 6 / 6      | FR-PL944
#  max_rows       | 0 / -1 / 1.5 / NA / Inf / "2" / TRUE / length 2 / length 0 | | error "`max_rows` must be a single positive whole number" | FR-TB595
#  max_rows       | with split "none"     | 2                  | error "has no effect with split = 'none'" | FR-PL446
#  split          | prefix / unknown      | "group_s" / "bogus"| pages 2,2,2 / error "'arg' should be one of" | FR-PL433 FR-TB599
#  min_group_rows | 0 (min) / n           | 0 / 6              | pages 3,3 (two groups of three)  | FR-PL435
#  min_group_rows | -1 / 1.5 / NA / "2"   |                    | error "`min_group_rows` must be a single non-negative whole number" | FR-TB597
#
# ---------------------------------------------------------------------------
#  plan_paginate_cols()  -- FT-PL968.  Six columns, the first kept.
# ---------------------------------------------------------------------------
#  every          | 1 (min) / 2 / n / n + 1 (5 / 6 not-kept columns) | | blocks 2 x 5 / 3,3,2 / 6 / 6 | FR-PL464
#  every          | 0 / -1 / 1.5 / NA / Inf / "2" / length 2 | | error "`every` is a positive number of columns" | FR-PL471 FR-TB604
#  at             | a position / a name   | 4 / "D15"          | blocks 3, 4                      | FR-PL462
#  at             | 1 / 0 / n + 1 / NA / unknown name |        | error "nothing before column 1" / "out of range" / "not found" | FR-TB284
#  fit            | NA / "yes" / length 2 |                    | error "plan_paginate_cols(fit = ) is TRUE" | FR-PL470
#  allow_span_break | NA / "yes"          |                    | error "`paginate_cols(allow_span_break = )` is TRUE or FALSE" | FR-TB604
#  order          | "across" / "down" / unknown / NA |         | blocks 3,3,2 / error "is not an axis" | FR-PL467
#
# ---------------------------------------------------------------------------
#  plan_digits()  -- FT-PL969.  A mean of 61.246 and counts 12 and 8.
# ---------------------------------------------------------------------------
#  ...            | 0 / 1 / 2 decimals    |                    | 61 / 61.2 / 61.25 (counts alike) | FR-PL240
#  ...            | significant digits    | "3s"               | 61.2, 12.0, 8.00                 | FR-PL244
#  ...            | by statistic          | continuous = c(mean = 2) | 61.25, counts open        | FR-PL241
#  ...            | a statistic not there | continuous = c(bogus = 2) | no change                 | FR-PL241
#  ...            | a key that is nothing | ZZZ = 3            | error "names 'ZZZ', which matched nothing" | FR-PL246
#  ...            | two unnamed values    | 1, 2               | error "Give at most one unnamed value" | FR-PL248
#  rounding       | "sas" / unknown / NA  |                    | 61.25 / error "`rounding` must be" | FR-PL243 FR-PL249
#  (A value that is a fraction, NA, TRUE, a string, a vector, Inf or
#  negative: #852.)
#
# ---------------------------------------------------------------------------
#  plan_blanks()  -- FT-PL970.  Two groups of three rows.
# ---------------------------------------------------------------------------
#  where          | "between_groups"      |                    | after row 3                      | FR-PL183
#  where          | 0 / -1 / 2 / n        |                    | 0 / 6 / 2 / 6                    | FR-PL934 FR-PL187
#  where          | n positions           | c(1, 3)            | 1, 3                             | FR-PL187
#  where          | "none" / length 0     |                    | none                             | FR-PL196
#  where          | unknown / prefix / NA / TRUE |             | error "Unrecognised `blank_rows` entry" | FR-PL194
#  where          | "records" with no listing |                | error "separates the records of a listing" | FR-PL193
#  first, counted | NA / "yes"            |                    | error "must be TRUE or FALSE"    | FR-TB598
#  where          | n + 1 / -2 / 1.5 / NA |                    | error "out of range" / "whole numbers" | FR-TB607 (FT-TB778, #853)
#
# ---------------------------------------------------------------------------
#  plan_stub()  -- FT-PL971.  Age (Mean) and Sex (F, M).
# ---------------------------------------------------------------------------
#  indent         | 0 (min) / 1 / 4 (default) / 20 |           | that many non-breaking spaces    | FR-PL363 FR-TB198
#  indent         | -1 / NA / Inf / length 2 / length 0 |      | error "`indent` must be a single non-negative integer" | FR-TB204
#  indent         | 1.5 / "2" / TRUE      |                    | error, same                      | FR-TB605
#  group_summary  | "all" / "none"        |                    | folded                           | FR-PL364
#  group_summary  | unknown               | "bogus"            | error "`group_summary` must be a subset" | FR-TB204
#  group_summary  | NA / ""               |                    | error, same                      | FR-TB605
#  name           | "" / NA / a number / length 2 | | object / error "`label` must be NULL or a single string" | FR-PL362 FR-TB204
#  before         | NA / "yes" / length 2 |                    | error "plan_stub(before = ) is TRUE" | FR-PL958
#  heading        | NA                    | NA                 | error "plan_stub(heading = ) is one text" | FR-PL913
# ---------------------------------------------------------------------------

.eb2_df <- data.frame(Param = c("Age", "Weight"), Value = c(">=65^{2}", NA),
                      stringsAsFactors = FALSE)
.eb2_grp <- data.frame(G = c("Age", "  Mean", "  SD", "Sex", "  F", "  M"),
                       V = c("40", "41", "3", "x", "5", "6"),
                       stringsAsFactors = FALSE)
.eb2_d1 <- rtf_tables(rtf_document(), .eb2_df)
.eb2_d2 <- rtf_tables(rtf_document(), list(.eb2_df, .eb2_df))
.eb2_kanji2 <- "明朝"         # a font family in kanji (Mincho)
.eb2_nb <- function(n) strrep(" ", n)

.eb2_rtf <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  suppressMessages(generate_rtfreport(doc, f))
  paste(readLines(f, warn = FALSE), collapse = "\n")
}
# The first \cellx of the file: the header (or footer) band's width.
.eb2_cellx1 <- function(x) regmatches(x, regexpr("\\\\cellx[0-9]+", x))

# One case: `expect` is "ok:<regex>" (the file matches), "no:<regex>" (it
# does not), "ok" (chk(file) is TRUE), "obj" (chk(object) is TRUE),
# "warning:<regex>" or "error:<regex>".  `more` holds the other arguments.
.eb2_case <- function(arg, cls, val, expect, chk = NULL, more = list()) {
  list(arg = arg, cls = cls, val = val, expect = expect, chk = chk,
       more = more)
}
.eb2_err <- function(arg, cls, val, pat, more = list()) {
  .eb2_case(arg, cls, val, paste0("error:", pat), more = more)
}
.eb2_obj <- function(arg, cls, val, chk, more = list()) {
  .eb2_case(arg, cls, val, "obj", chk = chk, more = more)
}
# Run every case through run(args, obj): `obj` is TRUE when the object,
# not the file, is checked.
.eb2_run <- function(cases, run) {
  for (cs in cases) {
    args <- c(setNames(list(cs$val), cs$arg), cs$more)
    lab  <- sprintf("%s = <%s>", cs$arg, cs$cls)
    kind <- sub(":.*", "", cs$expect)
    pat  <- sub("^[a-z]+:", "", cs$expect)
    obj  <- identical(cs$expect, "obj")
    if (kind == "error") {
      expect_error(run(args, obj), pat, info = lab)
    } else if (kind == "warning") {
      expect_warning(run(args, obj), pat, info = lab)
    } else {
      res <- run(args, obj)
      if (kind == "no") {
        expect_false(grepl(pat, res), info = lab)
      } else if (kind %in% c("ok", "obj") && !grepl(":", cs$expect)) {
        expect_true(isTRUE(cs$chk(res)), info = lab)
      } else {
        expect_true(grepl(pat, res), info = lab)
      }
    }
  }
}

test_that("FT-DC284 [FR-DC186 FR-DC187 FR-DC188 FR-DC189 FR-DC192 FR-DC193 FR-DC194 FR-DC195 FR-DC196 FR-DC197 FR-DC198 FR-DC199 FR-DC200 FR-DC201 FR-DC355 FR-TB600]: rtf_titles(): equivalence classes and boundaries of its arguments", {
  C <- .eb2_case
  E <- .eb2_err
  O <- .eb2_obj
  fs  <- "^`rtf_titles\\(font_size_half_points\\)` must be a single positive integer"
  rh  <- "^`rtf_titles\\(row_height_twips\\)` must be a single non-negative integer"
  fnt <- "^`rtf_titles\\(font\\)` must be a single font family name"
  al  <- "^`rtf_titles\\(align\\)` must be \"left\", \"center\" or \"right\""
  mk  <- "^`markup` must be a subset"
  blank <- "\\\\pard\\\\qc\\\\li0\\\\ri0 \\\\b \\\\b0 \\\\par"
  cases <- list(
    E("doc", "another type", list(), "^`doc` must be an rtf_document object"),
    E("doc", "no content", rtf_document(), "^Cannot set titles before any content has been added"),
    C("titles", "valid string", "Table 1", "ok:Table 1\\\\b0"),
    C("titles", "length 1 for 2 pages", list("Tcommon"), "ok",
      chk = function(x) length(gregexpr("Tcommon", x)[[1L]]) == 2L,
      more = list(doc = .eb2_d2)),
    C("titles", "n for n", list("TA", "TB"), "ok:TB\\\\b0", more = list(doc = .eb2_d2)),
    E("titles", "wrong length", list("A", "B"), "^`titles` has 2 blocks but the document has 1 page:"),
    E("titles", "length 0", list(), "^`titles` has 0 blocks but the document has 1 page"),
    E("titles", "NULL", NULL, "^`titles` must be a list \\(one block per page\\)"),
    E("titles", "another type", 1, "^`titles` must be a list"),
    E("titles", "NA", NA, "^`titles` must be a list"),
    C("titles", "empty string", "", "ok:\\\\pard\\\\qc\\\\li0\\\\ri0\\\\par"),
    C("titles", "character length 0", character(0), "no:\\\\pard\\\\qc"),
    C("titles", "NA row", c("TT", NA), "ok", chk = function(x) {
      grepl(paste0("TT\\\\b0 \\\\par\n", blank), x) && !grepl("\\\\b NA\\\\b0", x) }),
    C("titles", "non-ASCII", "試験", "ok:\\\\u-30106\\?\\\\u-26093\\?"),
    C("titles", "long", strrep("x", 500), paste0("ok:", strrep("x", 500), "\\\\b0")),
    O("font_size_half_points", "1 (min)", 1, function(d) identical(d$title_style$font_size_half_points, 1L)),
    E("font_size_half_points", "0", 0, fs),
    E("font_size_half_points", "negative", -1, fs),
    E("font_size_half_points", "Inf", Inf, fs),
    E("font_size_half_points", "length 2", c(18, 20), fs),
    E("font_size_half_points", "length 0", numeric(0), fs),
    O("row_height_twips", "0 (min)", 0, function(d) identical(d$title_style$row_height_twips, 0L)),
    E("row_height_twips", "negative", -1, rh),
    E("row_height_twips", "Inf", Inf, rh),
    E("row_height_twips", "above the integer range", 3e9, rh),
    E("row_height_twips", "length 0", numeric(0), rh),
    C("markup", "none", "none", "ok:x\\^\\\\\\{2\\\\\\}", more = list(titles = "x^{2}")),
    C("markup", "all", "all", "ok:x\\\\u8805", more = list(titles = "x>=1")),
    E("markup", "prefix", "scr", mk),
    E("markup", "unknown", "bogus", mk),
    E("markup", "empty string", "", mk),
    C("font", "non-ASCII", .eb2_kanji2, "ok:\\\\u26126\\?\\\\u26397\\?;"),
    E("font", "empty string", "", fnt),
    E("font", "NA", NA_character_, fnt),
    E("font", "length 0", character(0), fnt),
    E("font", "unnamed length 2", c("Arial", "Courier New"), fnt),
    E("font", "a number", 1, fnt),
    E("font", "unknown slot", c(latin = "Arial", bogus = "X"), "^`rtf_titles\\(font\\)`: unknown font slot\\(s\\) \"bogus\""),
    E("font", "no latin slot", c(east_asian = "MS Mincho"), "^`rtf_titles\\(font\\)` must name a `latin` font"),
    C("align", "valid", "right", "ok:\\\\pard\\\\qr"),
    E("align", "another case", "Center", al),
    E("align", "prefix", "cent", al),
    E("align", "NA", NA, al),
    E("align", "length 2", c("left", "left"), al),
    E("align", "length 0", character(0), al),
    E("align", "a number", 1, al),
    E("align", "unknown", "justify", al)
  )
  .eb2_run(cases, function(args, obj) {
    if (!"doc" %in% names(args)) args <- c(list(doc = .eb2_d1), args)
    if (!"titles" %in% names(args)) args$titles <- "Table 1"
    d <- do.call(rtf_titles, args)
    if (obj) d else .eb2_rtf(d)
  })
})

test_that("FT-DC285 [FR-DC203 FR-DC204 FR-DC205 FR-DC206 FR-DC207 FR-DC210 FR-DC212 FR-DC213 FR-DC214 FR-DC215 FR-DC216 FR-DC217 FR-DC218 FR-DC219 FR-TB600]: rtf_footnotes(): equivalence classes and boundaries of its arguments", {
  C <- .eb2_case
  E <- .eb2_err
  O <- .eb2_obj
  bd <- "^`rtf_footnotes\\(border = \\)` must be NULL or an rtf_border\\(\\) object"
  al <- "^`rtf_footnotes\\(align\\)` must be \"left\", \"center\" or \"right\""
  cases <- list(
    E("doc", "another type", list(), "^`doc` must be an rtf_document object"),
    E("doc", "no content", rtf_document(), "^Cannot set footnotes before any content has been added"),
    C("footnotes", "valid string", "Source: ADSL", "ok:Source: ADSL"),
    C("footnotes", "length 1 for 2 pages", list("Fcommon"), "ok",
      chk = function(x) length(gregexpr("Fcommon", x)[[1L]]) == 2L,
      more = list(doc = .eb2_d2)),
    O("footnotes", "NULL element", list(NULL), function(d) is.null(d$contents[[1L]]$footnote)),
    C("footnotes", "l / r cells", list(list(c(l = "LEFTX", r = "RIGHTX"))), "ok:RIGHTX"),
    E("footnotes", "a cell named left", list(list(c(left = "L"))), "^A footnote row may only be named l / c / r; got 'left'"),
    C("footnotes", "non-ASCII", "試験", "ok:\\\\u-30106\\?\\\\u-26093\\?"),
    C("footnotes", "long", strrep("y", 500), paste0("ok:", strrep("y", 500))),
    E("footnotes", "NULL", NULL, "^`footnotes` must be a list \\(one block per page\\)"),
    E("footnotes", "another type", 1, "^`footnotes` must be a list"),
    E("footnotes", "length 0", list(), "^`footnotes` has 0 blocks but the document has 1 page"),
    E("footnotes", "3 for 2", list("A", "B", "C"), "^`footnotes` has 3 blocks but the document has 2 pages",
      more = list(doc = .eb2_d2)),
    O("border", "valid", rtf_border(top = TRUE), function(d) inherits(d$footnote_style$border, "rtf_border")),
    E("border", "a string", "top", bd),
    E("border", "TRUE", TRUE, bd),
    E("border", "NA", NA, bd),
    E("border", "a plain list", list(top = TRUE), bd),
    E("font_size_half_points", "0", 0, "^`rtf_footnotes\\(font_size_half_points\\)` must be a single positive integer"),
    E("row_height_twips", "negative", -1, "^`rtf_footnotes\\(row_height_twips\\)` must be a single non-negative integer"),
    E("align", "unknown", "middle", al),
    E("align", "NA", NA, al),
    E("font", "empty string", "", "^`rtf_footnotes\\(font\\)` must be a single font family name"),
    E("markup", "unknown", "bogus", "^`markup` must be a subset")
  )
  .eb2_run(cases, function(args, obj) {
    if (!"doc" %in% names(args)) args <- c(list(doc = .eb2_d1), args)
    if (!"footnotes" %in% names(args)) args$footnotes <- "Source: ADSL"
    d <- do.call(rtf_footnotes, args)
    if (obj) d else .eb2_rtf(d)
  })
})

# The cases of rtf_header() and rtf_footer(), which take the same arguments.
.eb2_band_cases <- function(fn) {
  C <- .eb2_case
  E <- .eb2_err
  O <- .eb2_obj
  wt <- "^`width_twips` must be a single non-negative integer \\(twips\\) or NULL"
  wd <- "^`width` must be \"content\", \"page\", a fraction in \\(0, 1\\], or twips"
  dr <- "^`drop_empty_rows` must be TRUE or FALSE"
  co <- "^`color` must be NULL or a 6-digit hex colour"
  bd <- "^`border` must be NULL or an rtf_border object"
  first <- function(n) function(x) identical(.eb2_cellx1(x), paste0("\\cellx", n))
  list(
    C("rows", "valid", c(l = "Study XYZ", r = "Page {AUTO_PAGE}"), "ok", chk = function(x) {
      grepl("Study XYZ", x) && grepl("Page ", x) }),
    C("rows", "unnamed string", "HDRX", "ok:HDRX"),
    O("rows", "n rows", list(c(l = "A"), c(c = "B"), c(r = "C")), function(b) length(b$rows) == 3L),
    O("rows", "length 0", list(), function(b) length(b$rows) == 0L),
    C("rows", "non-ASCII", c(l = "試験"), "ok:\\\\u-30106\\?\\\\u-26093\\?"),
    C("rows", "long", c(l = strrep("y", 400)), paste0("ok:", strrep("y", 400))),
    E("rows", "NULL", NULL, "^`rows` must be a named character vector or list of named vectors"),
    E("rows", "another type", 1, "^`rows` must be a named character vector or list of named vectors"),
    E("rows", "a cell named left", c(left = "A"), sprintf("^`%s\\(\\)` row 1: a cell may only be named l, c or r", fn)),
    E("rows", "a row height of -1", list(list(columns = c(l = "A"), row_height_twips = -1)),
      sprintf("^`%s\\(\\)` row 1's `row_height_twips` must be", fn)),
    E("rows", "a row colour name", list(list(columns = c(l = "A"), color = "red")),
      sprintf("^`%s\\(\\)` row 1's `color` must be NULL or a 6-digit hex colour", fn)),
    O("border", "valid", rtf_border(bottom = TRUE), function(b) inherits(b$border, "rtf_border")),
    O("border", "NULL", NULL, function(b) is.null(b$border) && identical(b$border_default, FALSE)),
    E("border", "a string", "top", bd),
    E("border", "TRUE", TRUE, bd),
    E("border", "NA", NA, bd),
    C("width_twips", "valid", 5000, "ok", chk = first(5000)),
    E("width_twips", "negative", -1, wt),
    E("width_twips", "a fraction", 1.5, wt),
    E("width_twips", "NA", NA, wt),
    E("width_twips", "Inf", Inf, wt),
    E("width_twips", "a string", "5000", wt),
    E("width_twips", "length 2", c(1, 2), wt),
    E("width_twips", "length 0", numeric(0), wt),
    C("width", "page", "page", "ok", chk = first(13680)),
    C("width", "fraction inside", 0.5, "ok", chk = first(6840)),
    C("width", "fraction max", 1, "ok", chk = first(13680)),
    C("width", "twips, smallest", 2, "ok", chk = first(2)),
    C("width", "twips", 5000, "ok", chk = first(5000)),
    E("width", "just above the max fraction", 1.01, wd),
    E("width", "a fraction of twips", 1.5, wd),
    E("width", "Inf", Inf, wd),
    E("width", "NaN", NaN, wd),
    E("width", "0", 0, wd),
    E("width", "negative", -0.5, wd),
    E("width", "NA", NA_real_, wd),
    E("width", "another case", "Page", wd),
    E("width", "prefix", "pag", wd),
    E("width", "length 2", c(0.5, 1), wd),
    E("width", "length 0", character(0), wd),
    E("width", "TRUE", TRUE, wd),
    O("font_size_half_points", "1 (min)", 1, function(b) identical(b$font_size_half_points, 1L)),
    E("font_size_half_points", "0", 0, "^`font_size_half_points` must be a single positive integer"),
    E("font_size_half_points", "Inf", Inf, "^`font_size_half_points` must be a single positive integer"),
    O("row_height_twips", "0 (min)", 0, function(b) identical(b$row_height_twips, 0L)),
    E("row_height_twips", "negative", -1, "^`row_height_twips` must be a single non-negative integer"),
    E("row_height_twips", "Inf", Inf, "^`row_height_twips` must be a single non-negative integer"),
    O("cell_padding_top_twips", "0 (min)", 0, function(b) identical(b$cell_padding_top_twips, 0L)),
    E("cell_padding_top_twips", "negative", -1, "^`cell_padding_top_twips` must be a single non-negative integer"),
    E("cell_padding_top_twips", "Inf", Inf, "^`cell_padding_top_twips` must be a single non-negative integer"),
    C("font", "non-ASCII", .eb2_kanji2, "ok:\\\\u26126\\?\\\\u26397\\?;"),
    E("font", "empty string", "", "^`font` must be a single font family name"),
    E("markup", "prefix", "scr", "^`markup` must be a subset"),
    O("drop_empty_rows", "TRUE", TRUE, function(b) isTRUE(b$drop_empty_rows)),
    E("drop_empty_rows", "NA", NA, dr),
    E("drop_empty_rows", "a string", "yes", dr),
    E("drop_empty_rows", "length 2", c(TRUE, FALSE), dr),
    E("drop_empty_rows", "NULL", NULL, dr),
    E("drop_empty_rows", "a number", 1, dr),
    C("color", "lower-case hex", "#7f7f7f", "ok:\\\\red127\\\\green127\\\\blue127;"),
    E("color", "a name", "red", co),
    E("color", "5 digits", "#7F7F7", co),
    E("color", "7 digits", "#7F7F7F0", co),
    E("color", "NA", NA_character_, co),
    E("color", "length 2", c("#000000", "#111111"), co),
    E("color", "length 0", character(0), co),
    E("color", "empty string", "", co),
    E("color", "a number", 1, co)
  )
}

.eb2_band_run <- function(fn, where) {
  function(args, obj) {
    if (!"rows" %in% names(args)) args <- c(list(rows = c(l = "Study XYZ")), args)
    band <- do.call(fn, args)
    if (obj) return(band)
    .eb2_rtf(do.call(rtf_section, c(list(.eb2_d1), setNames(list(band), where))))
  }
}

test_that("FT-DC286 [FR-DC295 FR-DC297 FR-DC300 FR-DC302 FR-DC325 FR-DC326 FR-DC327 FR-DC328 FR-DC329 FR-DC331 FR-DC341 FR-DC342 FR-DC343 FR-DC354 FR-TB600]: rtf_header(): equivalence classes and boundaries of its arguments", {
  .eb2_run(.eb2_band_cases("rtf_header"), .eb2_band_run(rtf_header, "header"))
  # No rule by default.
  expect_null(rtf_header(c(l = "A"))$border)
})

test_that("FT-DC287 [FR-DC295 FR-DC297 FR-DC300 FR-DC302 FR-DC325 FR-DC326 FR-DC327 FR-DC328 FR-DC329 FR-DC331 FR-DC341 FR-DC342 FR-DC343 FR-DC354 FR-TB600]: rtf_footer(): equivalence classes and boundaries of its arguments", {
  .eb2_run(.eb2_band_cases("rtf_footer"), .eb2_band_run(rtf_footer, "footer"))
  # A top rule by default.
  expect_true(inherits(rtf_footer(c(l = "A"))$border, "rtf_border"))
})

test_that("FT-DC288 [FR-DC309 FR-DC310 FR-DC311 FR-DC312 FR-DC320 FR-DC321 FR-DC322 FR-DC324 FR-DC354]: rtf_style(): equivalence classes and boundaries of its arguments", {
  C <- .eb2_case
  E <- .eb2_err
  O <- .eb2_obj
  fnt <- "^`rtf_style\\(font\\)` must be a single font family name"
  fs  <- "^`rtf_style\\(font_size_half_points\\)` must be a single positive integer"
  co  <- "^`rtf_style\\(color\\)` must be NULL or a 6-digit hex colour"
  tf  <- "^`rtf_style\\(title_format\\)` must be \"text\" or \"table\""
  tw  <- "^`rtf_style\\(title_width\\)` must be \"content\", \"page\", a fraction in \\(0, 1\\], or twips"
  tbl <- list(title_format = "table")
  ttl <- function(n) function(x) identical(.eb2_cellx1(x), paste0("\\cellx", n))
  cases <- list(
    E("fnt", "unknown name", "Arial", "^`rtf_style\\(\\)`: unknown argument `fnt`"),
    C("font", "non-ASCII", .eb2_kanji2, "ok:\\\\u26126\\?\\\\u26397\\?;"),
    E("font", "empty string", "", fnt),
    E("font", "NA", NA_character_, fnt),
    E("font", "length 0", character(0), fnt),
    E("font", "unnamed length 2", c("Arial", "Courier New"), fnt),
    C("font_size_half_points", "1 (min)", 1, "ok:\\\\fs1[^0-9]"),
    E("font_size_half_points", "0", 0, fs),
    E("font_size_half_points", "Inf", Inf, fs),
    E("font_size_half_points", "length 2", c(18, 20), fs),
    O("row_height_twips", "0 (min)", 0, function(s) identical(s$row_height_twips, 0L)),
    E("row_height_twips", "negative", -1, "^`rtf_style\\(row_height_twips\\)` must be a single non-negative integer"),
    E("row_height_twips", "Inf", Inf, "^`rtf_style\\(row_height_twips\\)` must be a single non-negative integer"),
    O("cell_padding_left_twips", "0 (min)", 0, function(s) identical(s$cell_padding_left_twips, 0L)),
    E("cell_padding_left_twips", "negative", -1, "^`rtf_style\\(cell_padding_left_twips\\)` must be"),
    O("color", "lower-case hex", "#7f7f7f", function(s) identical(s$color, "#7F7F7F")),
    E("color", "a name", "red", co),
    E("color", "NA", NA_character_, co),
    E("color", "empty string", "", co),
    E("color", "length 2", c("#000000", "#111111"), co),
    E("color", "5 digits", "#7F7F7", co),
    O("title_format", "valid", "table", function(s) identical(s$title_format, "table")),
    E("title_format", "another case", "Table", tf),
    E("title_format", "prefix", "tab", tf),
    E("title_format", "NA", NA, tf),
    E("title_format", "empty string", "", tf),
    E("title_format", "length 0", character(0), tf),
    E("title_format", "length 2", c("text", "table"), tf),
    E("title_format", "a number", 1, tf),
    E("footnote_format", "another case", "TEXT", "^`rtf_style\\(footnote_format\\)` must be \"text\" or \"table\""),
    C("title_width", "page", "page", "ok", chk = ttl(13680), more = tbl),
    C("title_width", "fraction max", 1, "ok", chk = ttl(13680), more = tbl),
    C("title_width", "fraction inside", 0.5, "ok", chk = ttl(6840), more = tbl),
    C("title_width", "twips", 4000, "ok", chk = ttl(4000), more = tbl),
    E("title_width", "just above the max fraction", 1.01, tw),
    E("title_width", "a fraction of twips", 1.5, tw),
    E("title_width", "Inf", Inf, tw),
    E("title_width", "0", 0, tw),
    E("title_width", "negative", -1, tw),
    E("title_width", "NA", NA_real_, tw),
    E("title_width", "another case", "Page", tw),
    E("title_width", "length 2", c(1, 2), tw),
    E("footnote_width", "Inf", Inf, "^`rtf_style\\(footnote_width\\)` must be"),
    E("table", "cell_valign unknown", list(cell_valign = "middle"), "^`rtf_style\\(table\\)\\$cell_valign` must be \"top\", \"center\" or \"bottom\""),
    E("table", "cell_valign NA", list(cell_valign = NA), "^`rtf_style\\(table\\)\\$cell_valign` must be"),
    E("table", "table_align another case", list(table_align = "Center"), "^`rtf_style\\(table\\)\\$table_align` must be"),
    E("table", "row_height_exact NA", list(row_height_exact = NA), "^`rtf_style\\(table\\)\\$row_height_exact` must be TRUE or FALSE"),
    E("table", "header_row_height_twips -1", list(header_row_height_twips = -1), "^`rtf_style\\(table\\)\\$header_row_height_twips` must be"),
    E("titles", "align unknown", list(align = "x"), "^`rtf_style\\(titles\\)\\$align` must be"),
    E("titles", "border, not its key", list(border = rtf_border(top = TRUE)), "^`rtf_style\\(titles = \\)`: unknown key `border`"),
    E("footnotes", "border a string", list(border = "top"), "^`rtf_style\\(footnotes\\)\\$border` must be NULL or an rtf_border\\(\\) object"),
    E("header", "color a name", list(color = "red"), "^`rtf_style\\(header\\)\\$color` must be NULL or a 6-digit hex colour")
  )
  for (s in c("table", "titles", "footnotes", "header", "footer")) {
    S <- function(cls, val, pat) E(s, cls, val, sprintf(pat, s))
    cases <- c(cases, list(
      S("a string", "x", "^`rtf_style\\(%s = \\)` must be a named list of"),
      S("unnamed entry", list(18), "^`rtf_style\\(%s = \\)`: every entry must be named"),
      S("unknown key", list(bogus = 1), "^`rtf_style\\(%s = \\)`: unknown key `bogus`"),
      S("a key twice", list(font = "A", font = "B"), "^`rtf_style\\(%s = \\)`: `font` is given twice"),
      S("an rtf_style()", rtf_style(), "^`rtf_style\\(%s = \\)` must be a named list of"),
      O(s, "length 0", list(), function(st) identical(st[[s]], list())),
      O(s, "a NULL value", list(font = NULL), function(st) identical(st[[s]], list()))
    ))
  }
  .eb2_run(cases, function(args, obj) {
    st <- do.call(rtf_style, args)
    if (obj) return(st)
    .eb2_rtf(rtf_tables(rtf_document(style = st), .eb2_df, titles = "Ttl"))
  })
  # Nothing set prints so.
  expect_true(any(grepl("(nothing set)", capture.output(print(rtf_style())),
                        fixed = TRUE)))
})

test_that("FT-TB776 [FR-IX355 FR-TB118 FR-TB119 FR-TB120 FR-TB145 FR-TB146 FR-TB150 FR-TB151 FR-TB155 FR-TB158 FR-TB162 FR-TB203 FR-TB204 FR-TB591 FR-TB602 FR-TB603 FR-TB605]: as_rtftables(): equivalence classes and boundaries of read_meta, header_sep, listing, stub, cell_format and first_rows", {
  C <- .eb2_case
  E <- .eb2_err
  O <- .eb2_obj
  lab <- .eb2_grp
  attr(lab$V, "label") <- "Value label"
  sp <- data.frame(label = "a", `Placebo____n` = "1", `Placebo____%` = "2",
                   check.names = FALSE)
  recs <- data.frame(USUBJID = c("01-001", "01-001", "01-002"),
                     AETERM = c("Headache", "Nausea", "Fatigue"))
  ls <- listing_spec(list(listing_col("USUBJID", label = "Subject"),
                          listing_col("AETERM", label = "Adverse event")))
  h <- data.frame(SOC = c("Gastro", "Gastro", "Nerv"),
                  PT = c("Nausea", "Vomit", "Headache"), N = c("1", "2", "3"))
  cf <- data.frame(L = c("a", "b"), N = c("1 (5.0)", "12 (50.0)"))
  hdr <- function(v) function(p) identical(p[[1L]]$col_header[[length(p[[1L]]$col_header)]][2L], v)
  nolab <- function(p) !identical(unlist(p[[1L]]$col_header)[2L], "Value label")
  nhdr <- function(n) function(p) length(p[[1L]]$col_header) == n
  rows <- function(n) function(p) identical(unname(vapply(p, function(x) nrow(x$data), 1L)), as.integer(n))
  col2 <- function(v) function(p) identical(p[[1L]]$data$N, v) && identical(p[[1L]]$data$L, cf$L)
  rm  <- "^`read_meta` must be FALSE/TRUE or a character vector of tokens"
  hs  <- "^`header_sep` must be NULL or a character vector of separators"
  lsp <- "^`listing` must be a listing_spec\\(\\)"
  cfe <- "^`cell_format` must be a function, the name of a built-in cell format, or a list of either"
  fr  <- "^as_rtftables\\(\\): `first_rows` is a whole number of rows, 1 or more"
  gf3 <- list(split = "group_force", max_rows = 3)
  stub_rows <- function(ind) function(p) {
    identical(p[[1L]]$data[[1L]], c("Gastro", paste0(.eb2_nb(ind), c("Nausea", "Vomit")),
                                    "Nerv", paste0(.eb2_nb(ind), "Headache")))
  }
  cases <- list(
    O("read_meta", "TRUE", TRUE, hdr("Value label"), more = list(x = lab)),
    O("read_meta", "FALSE", FALSE, nolab, more = list(x = lab)),
    O("read_meta", "NULL", NULL, nolab, more = list(x = lab)),
    O("read_meta", "token", "labels", hdr("Value label"), more = list(x = lab)),
    O("read_meta", "with an adapter token", c("labels", "spanning"), hdr("Value label"), more = list(x = lab)),
    O("read_meta", "unknown token", "bogus", nolab, more = list(x = lab)),
    O("read_meta", "length 0", character(0), nolab, more = list(x = lab)),
    E("read_meta", "NA", NA, rm, more = list(x = lab)),
    E("read_meta", "a number", 1, rm, more = list(x = lab)),
    E("read_meta", "length 2", c(TRUE, FALSE), rm, more = list(x = lab)),
    E("read_meta", "a token NA", c("labels", NA), rm, more = list(x = lab)),
    O("header_sep", "valid", "____", nhdr(2), more = list(x = sp)),
    O("header_sep", "NULL", NULL, nhdr(0), more = list(x = sp)),
    O("header_sep", "length 0", character(0), nhdr(0), more = list(x = sp)),
    O("header_sep", "with an NA", c("____", NA), nhdr(2), more = list(x = sp)),
    O("header_sep", "empty string", "", nhdr(0), more = list(x = sp)),
    O("header_sep", "not found", "x", nhdr(0), more = list(x = sp)),
    E("header_sep", "a number", 1, hs, more = list(x = sp)),
    E("header_sep", "TRUE", TRUE, hs, more = list(x = sp)),
    O("listing", "valid", ls, function(p) identical(unlist(p[[1L]]$col_header)[1L], "Subject"), more = list(x = recs)),
    E("listing", "a string", "x", lsp, more = list(x = recs)),
    E("listing", "length 0", list(), lsp, more = list(x = recs)),
    E("listing", "TRUE", TRUE, lsp, more = list(x = recs)),
    E("listing", "NA", NA, lsp, more = list(x = recs)),
    E("listing", "on a built listing", ls, "already built by build_listing\\(\\)",
      more = list(x = build_listing(recs, ls))),
    O("stub", "names", c("SOC", "PT"), stub_rows(4), more = list(x = h)),
    O("stub", "positions", c(1, 2), stub_rows(4), more = list(x = h)),
    O("stub", "one column", "SOC", function(p) ncol(p[[1L]]$data) == 3L && nrow(p[[1L]]$data) == 3L,
      more = list(x = h)),
    O("stub", "a stub_spec()", stub_spec(c("SOC", "PT"), indent = 2), stub_rows(2), more = list(x = h)),
    E("stub", "length 0", character(0), "^`vars` names no column", more = list(x = h)),
    E("stub", "NA", NA, "^`stub\\(vars\\)` index NA out of range", more = list(x = h)),
    E("stub", "0", c(0, 1), "^`stub\\(vars\\)` index 0 out of range", more = list(x = h)),
    E("stub", "n + 1", c(1, 4), "^`stub\\(vars\\)` index 4 out of range \\(1\\.\\.3\\)", more = list(x = h)),
    E("stub", "unknown name", c("SOC", "ZZ"), "^`stub\\(vars\\)` column 'ZZ' not found", more = list(x = h)),
    E("stub", "a repeat", c("SOC", "SOC"), "^`vars` must name distinct columns", more = list(x = h)),
    E("stub", "indent negative", stub_spec(c("SOC", "PT"), indent = -1), "^`indent` must be a single non-negative integer", more = list(x = h)),
    E("stub", "indent a fraction", stub_spec(c("SOC", "PT"), indent = 1.5), "^`indent` must be a single non-negative integer", more = list(x = h)),
    E("stub", "indent a string", stub_spec(c("SOC", "PT"), indent = "2"), "^`indent` must be a single non-negative integer", more = list(x = h)),
    E("stub", "group_summary NA", stub_spec(c("SOC", "PT"), group_summary = NA), "^`group_summary` must be a subset", more = list(x = h)),
    E("stub", "group_summary empty", stub_spec(c("SOC", "PT"), group_summary = ""), "^`group_summary` must be a subset", more = list(x = h)),
    O("cell_format", "a name", "count_paren", col2(fmt_count_paren(cf$N)), more = list(x = cf)),
    O("cell_format", "fmt_ prefix", "fmt_count_paren", col2(fmt_count_paren(cf$N)), more = list(x = cf)),
    O("cell_format", "a list", list(NULL, "right_align"), col2(fmt_right_align(cf$N)), more = list(x = cf)),
    O("cell_format", "length 0 list", list(), col2(cf$N), more = list(x = cf)),
    O("cell_format", "an entry not a name", list(NULL, 1), col2(cf$N), more = list(x = cf)),
    O("cell_format", "a function", toupper, col2(toupper(cf$N)), more = list(x = cf)),
    E("cell_format", "unknown", "bogus", "^`cell_format`: \"bogus\" is not a built-in cell format", more = list(x = cf)),
    E("cell_format", "empty string", "", "^`cell_format`: \"\" is not a built-in cell format", more = list(x = cf)),
    E("cell_format", "another case", "Count_Paren", "^`cell_format`: \"Count_Paren\" is not a built-in", more = list(x = cf)),
    E("cell_format", "NA", NA, cfe, more = list(x = cf)),
    E("cell_format", "a number", 1, cfe, more = list(x = cf)),
    E("cell_format", "TRUE", TRUE, cfe, more = list(x = cf)),
    E("cell_format", "length 2", c("count_paren", "value_paren"), cfe, more = list(x = cf)),
    E("cell_format", "length 0", character(0), cfe, more = list(x = cf)),
    E("cell_format", "a function of the wrong length", function(x) x[1L],
      "^A `cell_format` function must return a vector the same length as the column \\(got 1, expected 2\\)",
      more = list(x = cf)),
    O("first_rows", "1 (min)", 1, rows(c(1, 3, 3)), more = gf3),
    O("first_rows", "valid", 2, rows(c(2, 2, 3)), more = gf3),
    O("first_rows", "n - 1", 5, rows(c(5, 2)), more = gf3),
    O("first_rows", "n", 6, rows(6), more = gf3),
    O("first_rows", "n + 1", 7, rows(6), more = gf3),
    E("first_rows", "0", 0, fr, more = gf3),
    E("first_rows", "negative", -1, fr, more = gf3),
    E("first_rows", "a fraction", 1.5, fr, more = gf3),
    E("first_rows", "NA", NA, fr, more = gf3),
    E("first_rows", "Inf", Inf, fr, more = gf3),
    E("first_rows", "a string", "2", fr, more = gf3),
    E("first_rows", "TRUE", TRUE, fr, more = gf3),
    E("first_rows", "length 2", c(1, 2), fr, more = gf3),
    E("first_rows", "length 0", numeric(0), fr, more = gf3),
    E("first_rows", "with page_by", 2, "^`first_rows` paginates one table by rows or groups: not with `page_by`",
      more = list(page_by = "G")),
    E("first_rows", "with by_value", 2, "^`first_rows` paginates by rows or groups: not split = \"by_value\"",
      more = list(split = "by_value", group_by = "indent"))
  )
  .eb2_run(cases, function(args, obj) {
    if (!"x" %in% names(args)) args <- c(list(x = .eb2_grp), args)
    do.call(as_rtftables, args)
  })
})

# ---- the plan verbs ----------------------------------------------------------

.eb2_subj <- data.frame(Subject = sprintf("01-%03d", 1:6),
                        Age = c("61", "58", "70", "66", "54", "49"))
.eb2_wide <- data.frame(P = c("ALT", "AST"), D1 = "1", D8 = "2", D15 = "3",
                        D22 = "4", D29 = "5")
.eb2_st <- data.frame(
  ARM = rep(c("Placebo", "Active"), each = 3),
  variable = rep(c("AGE", "SEX", "SEX"), 2),
  .label = rep(c(NA, "F", "M"), 2),
  .kind = rep(c("continuous", "categorical", "categorical"), 2),
  stat_name = rep(c("mean", "n", "n"), 2),
  stat = c(61.246, 12, 8, 63.51, 9, 11))
.eb2_plan <- function() {
  table_plan(.eb2_st, cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(Mean = "{mean}"), categorical = "{n}") |>
    plan_labels(AGE = "Age", SEX = "Sex")
}
.eb2_nrows <- function(n) function(p) identical(unname(vapply(p, function(x) nrow(x$data), 1L)), as.integer(n))
.eb2_ncols <- function(n) function(p) identical(unname(vapply(p, function(x) ncol(x$data), 1L)), as.integer(n))

test_that("FT-PL967 [FR-PL433 FR-PL435 FR-PL446 FR-PL944 FR-TB595 FR-TB597 FR-TB599]: plan_paginate_rows(): equivalence classes and boundaries of its arguments", {
  O <- .eb2_obj
  E <- .eb2_err
  mr <- "^`max_rows` must be a single positive whole number \\(rows\\) or NULL"
  mg <- "^`min_group_rows` must be a single non-negative whole number"
  grp <- list(.x = .eb2_grp, max_rows = 4)
  cases <- list(
    O("max_rows", "1 (min)", 1, .eb2_nrows(rep(1, 6))),
    O("max_rows", "valid", 2, .eb2_nrows(c(2, 2, 2))),
    O("max_rows", "n", 6, .eb2_nrows(6)),
    O("max_rows", "n + 1", 7, .eb2_nrows(6)),
    E("max_rows", "0", 0, mr),
    E("max_rows", "negative", -1, mr),
    E("max_rows", "a fraction", 1.5, mr),
    E("max_rows", "NA", NA, mr),
    E("max_rows", "Inf", Inf, mr),
    E("max_rows", "a string", "2", mr),
    E("max_rows", "TRUE", TRUE, mr),
    E("max_rows", "length 2", c(2, 3), mr),
    E("max_rows", "length 0", numeric(0), mr),
    E("max_rows", "with split none", 2, "^plan_paginate_rows\\(max_rows = 2\\) has no effect with split = 'none'",
      more = list(split = "none")),
    O("split", "prefix", "group_s", .eb2_nrows(c(2, 2, 2)), more = list(max_rows = 2)),
    E("split", "unknown", "bogus", "'arg' should be one of", more = list(max_rows = 2)),
    O("min_group_rows", "0 (min)", 0, .eb2_nrows(c(3, 3)), more = grp),
    O("min_group_rows", "n", 6, .eb2_nrows(c(3, 3)), more = grp),
    E("min_group_rows", "negative", -1, mg, more = grp),
    E("min_group_rows", "a fraction", 1.5, mg, more = grp),
    E("min_group_rows", "NA", NA, mg, more = grp),
    E("min_group_rows", "a string", "2", mg, more = grp)
  )
  .eb2_run(cases, function(args, obj) {
    x <- args$.x %||% .eb2_subj
    args$.x <- NULL
    plan_apply(do.call(plan_paginate_rows, c(list(table_plan(x)), args)))
  })
})

test_that("FT-PL968 [FR-PL462 FR-PL464 FR-PL467 FR-PL470 FR-PL471 FR-TB284 FR-TB604]: plan_paginate_cols(): equivalence classes and boundaries of its arguments", {
  O <- .eb2_obj
  E <- .eb2_err
  ev <- "^`every` is a positive number of columns"
  ft <- "^plan_paginate_cols\\(fit = \\) is TRUE"
  sb <- "^`paginate_cols\\(allow_span_break = \\)` is TRUE or FALSE"
  ax <- "^`order`: \"[A-Za-z]+\" is not an axis"
  e2 <- list(every = 2)
  nf <- list(fit = FALSE)
  cases <- list(
    O("every", "1 (min)", 1, .eb2_ncols(rep(2, 5))),
    O("every", "valid", 2, .eb2_ncols(c(3, 3, 2))),
    O("every", "n", 5, .eb2_ncols(6)),
    O("every", "n + 1", 6, .eb2_ncols(6)),
    E("every", "0", 0, ev),
    E("every", "negative", -1, ev),
    E("every", "a fraction", 1.5, ev),
    E("every", "NA", NA, ev),
    E("every", "Inf", Inf, ev),
    E("every", "a string", "2", ev),
    E("every", "length 2", c(2, 3), ev),
    O("at", "a position", 4, .eb2_ncols(c(3, 4)), more = nf),
    O("at", "a name", "D15", .eb2_ncols(c(3, 4)), more = nf),
    E("at", "1", 1, "^`at` must name columns 2\\.\\.ncol -- there is nothing before column 1"),
    E("at", "0", 0, "^`paginate_cols\\(at\\)` index 0 out of range \\(1\\.\\.6\\)"),
    E("at", "n + 1", 7, "^`paginate_cols\\(at\\)` index 7 out of range \\(1\\.\\.6\\)"),
    E("at", "NA", NA, "^`paginate_cols\\(at\\)` index NA out of range"),
    E("at", "unknown name", "ZZ", "^`paginate_cols\\(at\\)` column 'ZZ' not found"),
    E("fit", "NA", NA, ft, more = e2),
    E("fit", "a string", "yes", ft, more = e2),
    E("fit", "length 2", c(TRUE, FALSE), ft, more = e2),
    E("allow_span_break", "NA", NA, sb, more = e2),
    E("allow_span_break", "a string", "yes", sb, more = e2),
    O("order", "across", "across", .eb2_ncols(c(3, 3, 2)), more = e2),
    O("order", "down", "down", .eb2_ncols(c(3, 3, 2)), more = e2),
    E("order", "unknown", "bogus", ax, more = e2),
    E("order", "NA", NA, ax, more = e2)
  )
  .eb2_run(cases, function(args, obj) {
    if (is.null(args$keep)) args$keep <- "P"
    plan_apply(do.call(plan_paginate_cols, c(list(table_plan(.eb2_wide)), args)))
  })
})

test_that("FT-PL969 [FR-PL240 FR-PL241 FR-PL243 FR-PL244 FR-PL246 FR-PL248 FR-PL249]: plan_digits(): equivalence classes and boundaries of its arguments", {
  vals <- function(v) function(t) identical(t$Placebo, v)
  O <- .eb2_obj
  E <- .eb2_err
  cases <- list(
    O("..1", "0 decimals", 0, vals(c("61", "12", "8"))),
    O("..1", "1 decimal", 1, vals(c("61.2", "12.0", "8.0"))),
    O("..1", "2 decimals", 2, vals(c("61.25", "12.00", "8.00"))),
    O("..1", "significant digits", "3s", vals(c("61.2", "12.0", "8.00"))),
    O("continuous", "by statistic", c(mean = 2), vals(c("61.25", "12", "8"))),
    O("continuous", "a statistic not there", c(bogus = 2), vals(c("61.246", "12", "8"))),
    E("ZZZ", "a key that is nothing", 3, "^plan_digits\\(\\) names 'ZZZ', which matched nothing"),
    E("..1", "two unnamed values", 1, "^Give at most one unnamed value", more = list(2)),
    O("rounding", "sas", "sas", vals(c("61.25", "12.00", "8.00")), more = list(2)),
    E("rounding", "unknown", "bogus", "^`rounding` must be \"r\"", more = list(2)),
    E("rounding", "NA", NA, "^`rounding` must be \"r\"", more = list(2))
  )
  .eb2_run(cases, function(args, obj) {
    names(args)[names(args) == "..1"] <- ""
    plan_apply(do.call(plan_digits, c(list(.eb2_plan()), args)), "table")
  })
})

test_that("FT-PL970 [FR-PL183 FR-PL187 FR-PL193 FR-PL194 FR-PL196 FR-PL934 FR-TB598]: plan_blanks(): equivalence classes and boundaries of its arguments", {
  O <- .eb2_obj
  E <- .eb2_err
  at <- function(v) function(p) identical(p[[1L]]$blank_rows, v)
  un <- "^Unrecognised `blank_rows` entry"
  cases <- list(
    O("where", "between_groups", "between_groups", at(3L)),
    O("where", "0 (before the first)", 0, at(0L)),
    O("where", "-1 (after the last)", -1, at(6L)),
    O("where", "valid", 2, at(2L)),
    O("where", "n", 6, at(6L)),
    O("where", "n positions", c(1, 3), at(c(1L, 3L))),
    O("where", "none", "none", function(p) !length(p[[1L]]$blank_rows)),
    O("where", "length 0", character(0), function(p) !length(p[[1L]]$blank_rows)),
    E("where", "unknown", "bogus", un),
    E("where", "prefix", "between", un),
    E("where", "NA", NA, un),
    E("where", "TRUE", TRUE, un),
    E("where", "records with no listing", "records", "^plan_blanks\\(where = \"records\"\\) separates the records of a listing"),
    E("first", "NA", NA, "^`blank_row_first` must be TRUE or FALSE"),
    E("first", "a string", "yes", "^`blank_row_first` must be TRUE or FALSE"),
    E("counted", "NA", NA, "^`count_blank_rows` must be TRUE or FALSE"),
    E("counted", "a string", "yes", "^`count_blank_rows` must be TRUE or FALSE")
  )
  .eb2_run(cases, function(args, obj) {
    plan_apply(do.call(plan_blanks, c(list(table_plan(.eb2_grp)), args)))
  })
})

test_that("FT-PL971 [FR-PL362 FR-PL363 FR-PL364 FR-PL913 FR-PL958 FR-TB198 FR-TB204 FR-TB605]: plan_stub(): equivalence classes and boundaries of its arguments", {
  O <- .eb2_obj
  E <- .eb2_err
  ind <- function(n) function(p) {
    identical(p[[1L]]$data[[1L]], c("Age", paste0(.eb2_nb(n), "Mean"), "Sex",
                                    paste0(.eb2_nb(n), c("F", "M"))))
  }
  ie <- "^`indent` must be a single non-negative integer"
  gs <- "^`group_summary` must be a subset"
  lb <- "^`label` must be NULL or a single string"
  bf <- "^plan_stub\\(before = \\) is TRUE"
  cases <- list(
    O("indent", "0 (min)", 0, ind(0)),
    O("indent", "1", 1, ind(1)),
    O("indent", "4 (default)", 4, ind(4)),
    O("indent", "20", 20, ind(20)),
    E("indent", "negative", -1, ie),
    E("indent", "NA", NA, ie),
    E("indent", "Inf", Inf, ie),
    E("indent", "length 2", c(1, 2), ie),
    E("indent", "length 0", numeric(0), ie),
    E("indent", "a fraction", 1.5, ie),
    E("indent", "a string", "2", ie),
    E("indent", "TRUE", TRUE, ie),
    O("group_summary", "all", "all", ind(4)),
    O("group_summary", "none", "none", ind(4)),
    E("group_summary", "unknown", "bogus", gs),
    E("group_summary", "NA", NA, gs),
    E("group_summary", "empty string", "", gs),
    O("name", "empty string", "", function(p) identical(names(p[[1L]]$data)[1L], "")),
    E("name", "NA", NA, lb),
    E("name", "a number", 1, lb),
    E("name", "length 2", c("a", "b"), lb),
    E("before", "NA", NA, bf),
    E("before", "a string", "yes", bf),
    E("before", "length 2", c(TRUE, FALSE), bf),
    E("heading", "NA", NA, "^plan_stub\\(heading = \\) is one text")
  )
  .eb2_run(cases, function(args, obj) {
    if (!"name" %in% names(args)) args$name <- "Characteristic"
    plan_apply(do.call(plan_stub, c(list(.eb2_plan()), args)), "pages")
  })
})

# ---------------------------------------------------------------------------
#  The fixes for #850 - #853: what the batch left out.
# ---------------------------------------------------------------------------

test_that("FT-TB777 [FR-TB606 FR-TB128 FR-TB203 FR-TB284]: a column position is a whole number in 1..ncol; TRUE, a fraction or a factor stops", {
  h <- data.frame(SOC = c("Gastro", "Gastro", "Nerv"),
                  PT  = c("Nausea", "Vomit", "Headache"),
                  N   = c("1", "2", "3"))
  wide <- data.frame(P = "ALT", D1 = "1", D8 = "2", D15 = "3")
  wp <- "must be a column name or a whole position"
  # stub (stub_cols' vars through as_rtftables)
  expect_error(as_rtftables(h, stub = TRUE),
               "^`stub\\(vars\\)` must be a column name or a whole position \\(1\\.\\.3\\), not TRUE")
  expect_error(as_rtftables(h, stub = 1.5), "not 1\\.5")
  expect_error(as_rtftables(h, stub = factor("SOC")),
               "^`stub\\(vars\\)` must be column names or whole positions")
  expect_error(stub_cols(h, vars = c(1, 2.5)), paste0("^`vars` ", wp))
  # names and whole positions (integer or double) keep working
  expect_identical(as_rtftables(h, stub = c(1, 2))[[1]]$data,
                   as_rtftables(h, stub = c("SOC", "PT"))[[1]]$data)
  expect_identical(as_rtftables(h, stub = 1:2)[[1]]$data,
                   as_rtftables(h, stub = list("SOC", 2L))[[1]]$data)
  # group_col, collapse_repeats, sort_by, drop_cols
  expect_error(as_rtftables(h, split = "group_safe", group_col = 1.5, max_rows = 2),
               paste0("^`group_col` ", wp))
  expect_error(as_rtftables(h, split = "group_safe", group_col = TRUE, max_rows = 2),
               paste0("^`group_col` ", wp))
  expect_length(as_rtftables(h, split = "group_safe", group_col = 1, max_rows = 2), 2L)
  expect_error(as_rtftables(h, collapse_repeats = TRUE), paste0("^`collapse_repeats` ", wp))
  expect_true(is.na(as_rtftables(h, collapse_repeats = 1)[[1]]$data[2, 1]))
  expect_error(as_rtftables(h, sort_by = 2.5), paste0("^`sort_by` ", wp))
  expect_error(as_rtftables(h, drop_cols = TRUE), paste0("^`drop_cols` ", wp))
  # paginate_cols(at = )
  expect_error(paginate_cols(as_rtftables(wide), at = 2.7),
               paste0("^`paginate_cols\\(at\\)` ", wp, " \\(1\\.\\.4\\), not 2\\.7"))
  expect_error(paginate_cols(as_rtftables(wide), at = TRUE),
               paste0("^`paginate_cols\\(at\\)` ", wp))
  # NA and out of range keep their message
  expect_error(as_rtftables(h, stub = NA), "^`stub\\(vars\\)` index NA out of range \\(1\\.\\.3\\)")
  expect_error(as_rtftables(h, stub = 4), "^`stub\\(vars\\)` index 4 out of range \\(1\\.\\.3\\)")
})

test_that("FT-DC289 [FR-DC356 FR-DC343]: rtf_header() / rtf_footer(width_twips = 0) is the full writable width, as NULL is", {
  df <- data.frame(Param = "Age", Value = "61")
  band_x <- function(band, where) {
    doc <- rtf_section(rtf_tables(rtf_document(), df),
                       header = if (where == "header") band,
                       footer = if (where == "footer") band)
    x <- .eb2_rtf(doc)
    as.integer(sub("\\\\cellx", "", regmatches(x, gregexpr("\\\\cellx[0-9]+", x))[[1]]))
  }
  for (fn in c("header", "footer")) {
    mk <- if (fn == "header") rtf_header else rtf_footer
    zero <- band_x(mk(c(l = "Study XYZ"), width_twips = 0), fn)
    full <- band_x(mk(c(l = "Study XYZ")), fn)
    expect_false(any(zero == 0L), info = fn)
    expect_identical(zero, full, info = fn)
    # with `width` given, 0 leaves the width to it
    half <- band_x(mk(c(l = "Study XYZ"), width_twips = 0, width = 0.5), fn)
    expect_identical(half, band_x(mk(c(l = "Study XYZ"), width = 0.5), fn), info = fn)
    # a positive width_twips still wins
    expect_true(4000L %in% band_x(mk(c(l = "Study XYZ"), width_twips = 4000), fn), info = fn)
    expect_error(mk(c(l = "A"), width_twips = -1),
                 "^`width_twips` must be a single non-negative integer", info = fn)
  }
})

test_that("FT-PL972 [FR-PL959 FR-PL240 FR-PL241 FR-PL244]: plan_digits() takes whole decimals (0 or more) or \"<n>s\", and stops on anything else naming the key", {
  st <- data.frame(ARM = rep(c("Placebo", "Active"), each = 3),
                   variable = rep(c("AGE", "SEX", "SEX"), 2),
                   .label = rep(c(NA, "F", "M"), 2),
                   .kind = rep(c("continuous", "categorical", "categorical"), 2),
                   stat_name = rep(c("mean", "n", "n"), 2),
                   stat = c(61.246, 12, 8, 63.51, 9, 11))
  p <- plan_cells(table_plan(st, cols = "ARM", rows = c(group = "variable")),
                  continuous = c(Mean = "{mean}"), categorical = "{n}")
  bad <- "must be a whole number of decimals \\(0 or more\\) or \"<n>s\""
  for (v in list(1.5, TRUE, "2", c(1, 2), NA, numeric(0), -1, Inf, "x", "0s",
                 factor("1"), list(1))) {
    expect_error(plan_digits(p, v), paste0("^plan_digits\\(\\): the unnamed value ", bad),
                 info = paste(format(v), collapse = " "))
  }
  expect_error(plan_digits(p, AGE = 1.5), paste0("^plan_digits\\(\\): `AGE` ", bad, ".*got 1\\.5"))
  expect_error(plan_digits(p, AGE = "2"), "got \"2\"")
  expect_error(plan_digits(p, continuous = c(mean = NA)), paste0("`continuous` ", bad))
  expect_error(plan_digits(p, continuous = c(mean = -1)), paste0("`continuous` ", bad))
  expect_error(plan_digits(p, .rows = c(N = 0, 1)), paste0("`\\.rows` ", bad))
  expect_error(plan_digits(p, AGE = list(A = list(B = 1))), paste0("`AGE` ", bad))
  # what the help documents keeps working
  tab <- function(...) plan_apply(plan_digits(p, ...), "table")$Placebo
  expect_identical(tab(0), c("61", "12", "8"))
  expect_identical(tab(2L), c("61.25", "12.00", "8.00"))
  expect_identical(tab("3s"), c("61.2", "12.0", "8.00"))
  expect_identical(tab(continuous = c(mean = 2)), c("61.25", "12", "8"))
  expect_identical(tab(continuous = list(mean = 2)), c("61.25", "12", "8"))
  # a mixed vector is text after c(): "0" is decimals there
  expect_identical(tab(continuous = c(mean = "3s", n = 0)), c("61.2", "12", "8"))
  expect_s3_class(plan_digits(p, rounding = "sas"), "table_plan")
})

test_that("FT-TB778 [FR-TB607 FR-PL187 FR-TB598]: a blank_rows position past the last row, below -1 or not whole stops, in rtftable(), as_rtftables() and plan_blanks()", {
  grp <- data.frame(G = c("Age", "  Mean", "  SD", "Sex", "  F", "  M"),
                    V = c("40", "41", "3", "x", "5", "6"))
  oor <- function(p) sprintf(paste0("^`blank_rows` position %s out of range: the table has 6 rows ",
                                     "\\(positions 0\\.\\.6, or -1 for after the last row\\)"), p)
  wh  <- "^`blank_rows` positions must be whole numbers"
  run <- list(
    rtftable     = function(b) rtftable(grp, blank_rows = b)$blank_rows,
    as_rtftables = function(b) as_rtftables(grp, blank_rows = b)[[1]]$blank_rows,
    plan_blanks  = function(b) plan_apply(plan_blanks(table_plan(grp), where = b))[[1]]$blank_rows
  )
  for (nm in names(run)) {
    f <- run[[nm]]
    expect_error(f(7), oor("7"), info = nm)
    expect_error(f(-2), oor("-2"), info = nm)
    expect_error(f(c(2, 7, 9)), "^`blank_rows` positions 7, 9 out of range", info = nm)
    expect_error(f(1.5), wh, info = nm)
    expect_error(f(NA_real_), wh, info = nm)
    expect_error(f(list(2, 8)), oor("8"), info = nm)
    # the bounds are kept
    expect_identical(f(c(0, 3, -1)), c(0L, 3L, 6L), info = nm)
    expect_identical(f(6), 6L, info = nm)
  }
  # plan_blanks() says which verb declared it
  expect_error(plan_apply(plan_blanks(table_plan(grp), where = 7)),
               "declared by:\\s+plan_blanks\\(where = 7\\)")
  # checked on the whole body: a position within it, past a shorter page,
  # puts no blank on that page
  # (two pages of three rows each here)
  pg <- as_rtftables(grp, blank_rows = 4, max_rows = 4, split = "group_force")
  expect_identical(lapply(pg, `[[`, "blank_rows"), list(NULL, NULL))
  pg <- as_rtftables(grp, blank_rows = 2, max_rows = 4, split = "group_force")
  expect_identical(lapply(pg, `[[`, "blank_rows"), list(2L, 2L))
})
