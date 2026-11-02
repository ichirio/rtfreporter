# Validation tests: equivalence classes and boundary values of the arguments,
# batch 3 -- assemble_rtf() and assemble_folder(), the listing functions
# (listing_col(), listing_spec(), build_listing(), fit_listing_widths(),
# listing_wrap(), collapse_repeats()), the scalar arguments of the ARD
# helpers (normalize_ard(), widen_ard(), pull_ard()) and the number
# formatters (fmt_round(), fmt_signif(), fmt_numeric(), fmt_count_pct(),
# fmt_value_paren()), and the scalar arguments of the plan verbs batch 2
# left (plan_total(), plan_sort(), plan_filter(), plan_columns(),
# plan_append(), plan_col_header(), plan_levels()).  Helpers prefixed .eb3_.
# Fictional data only.
#
# The help is the functional requirement.  For each argument the classes
# are: a valid value of each documented kind, NULL, length 0 / 1 / n / a
# wrong length, NA, a value of another type, the documented minimum and
# maximum with a value just inside and just outside, 0, a negative number,
# a fraction for a whole number, Inf / NaN, "" / a non-ASCII / a long
# string, and an unknown choice / a prefix.  A class another test already
# checks is not repeated (FT-AR083: stack_sep that is not one text).
# Each table below is one row per case; the test of the function runs them
# in a loop.  "object" means the returned object is checked, not the file.
#
# ---------------------------------------------------------------------------
#  assemble_rtf()  -- FT-AS140.  Two one-page files, Table 14.1.1 / 14.2.1.
# ---------------------------------------------------------------------------
#  argument       | class                 | value              | expected                         | FR
#  input_files    | length 1 / 0 / a number |                  | error "`input_files` must be a character vector with at least 2 elements" | FR-AS102 FR-AS103
#  input_files    | NA / a missing file   | c(a, NA)           | error "Input file not found"     | FR-AS104
#  input_files    | the same file twice   | c(a, a)            | two bookmarks, _1 and _2         | FR-AS134
#  output_file    | non-ASCII name        | "出力.rtf"         | written                          | FR-AS107
#  output_file    | NA / "" / length 2 / a number / NULL |      | error "`output_file` must be a single non-empty file path" | FR-AS244
#  output_file    | folder that is not there |                 | error "The folder of `output_file` does not exist" | FR-AS244
#  output_file    | one of the inputs     | a                  | error "is one of the `input_files`" | FR-AS245
#  output_file    | exists, overwrite FALSE |                  | error "already exists"           | FR-AS108
#  overwrite      | TRUE                  | TRUE               | replaced                         | FR-AS107
#  overwrite      | NA / "yes" / 1 / length 2 |                | error "`overwrite` must be TRUE or FALSE" | FR-AS244
#  cover          | valid                 | list(title = "CVR") | \fs44 CVR                       | FR-AS109
#  cover          | length 0              | list()             | no title line                    | FR-AS110
#  cover          | "" / NA field         | list(title = NA)   | skipped, no empty title line     | FR-AS110
#  cover          | NA line of meta       | c("M1", NA)        | M1 written, no empty line        | FR-AS110
#  cover          | non-ASCII / long      | kanji / 500 chars  | written                          | FR-AS109
#  cover          | a string / a number / TRUE / a data.frame / unknown field |  | error "`cover` must be NULL or a list" | FR-AS244
#  cover          | a field of length 2   | list(title = c("a","b")) | error "`cover$title` must be a single string" | FR-AS244
#  toc            | "auto"                | "auto"             | the titles as entries            | FR-AS112
#  toc            | another case          | "AUTO"             | error "same length as `input_files`" | FR-AS114
#  toc            | length 0 / wrong length | character(0) / "x" | error, same                    | FR-AS114
#  toc            | an NA label           | c("x", NA)         | error "`toc` labels must not be NA" | FR-AS246
#  toc            | a number / TRUE       | 1 / TRUE           | error "`toc` must be NULL, \"auto\"" | FR-AS124
#  toc (table)    | level 1 (min) / 3 (max) | 1 / 3            | written; level 3 indented 1200   | FR-AS116 FR-AS238
#  toc (table)    | level 0 / 4 / 2.5 / -1 / Inf |             | error "`level` must be a whole number from 1 to 3" | FR-AS238
#  toc (table)    | level NA              | NA                 | the default, 2 (indent 600)      | FR-AS115
#  toc (table)    | an NA label           | c(NA, "y")         | error "`label` must be a single string, not NA" | FR-AS246
#  toc (table)    | no label column / 0 rows / a missing file |  | error (FR-AS118 / FR-AS102 / FR-AS119) | FR-AS102 FR-AS118 FR-AS119
#  toc (path)     | a .csv that is not there / a .txt |        | error "`toc` file not found" / "must end in .xlsx or .csv" | FR-AS117 FR-AS246
#  toc_title      | valid / "" / non-ASCII / long |            | written                          | FR-AS127
#  toc_title      | NA / length 2 / length 0 / a number / NULL | | error "`toc_title` must be a single string" | FR-AS244
#  toc_leader     | "none" / prefix "d"   |                    | no leader / a dot leader         | FR-AS128
#  toc_leader     | unknown / NA          |                    | error (match.arg)                | FR-AS129
#  toc_page_numbering | "roman" / prefix "rom" |               | \pgnlcrm                         | FR-AS131
#  toc_page_numbering | another case / "" / NA / length 2 |    | error (match.arg)                | FR-AS129
#  bookmark_prefix | "" / "studyX_"       |                    | the bookmark name                | FR-AS133
#  bookmark_prefix | NA / a space / length 2 / a number / non-ASCII | | error "`bookmark_prefix` must be a single string of letters" | FR-AS244
#  book_page      | valid                 | "Page {AUTO_PAGE}" | written                          | FR-AS138
#  book_page      | static token          | "{PAGE}"           | error "cannot use the static"    | FR-AS139
#  book_page      | NA / length 2 / length 0 / a number |      | error "`book_page` must be NULL or a single string" | FR-AS244
#
# ---------------------------------------------------------------------------
#  assemble_folder()  -- FT-AS141.  A folder of the two files.
# ---------------------------------------------------------------------------
#  dir            | valid / non-ASCII     |                    | object: two rows                 | FR-AS148
#  dir            | not there / no .rtf   |                    | error "Directory not found" / "No RTF files found." | FR-AS156
#  output_file    | NULL                  | NULL               | object: the table                | FR-AS152
#  output_file    | a path                |                    | object: list(output, spec)       | FR-AS152
#  output_file    | exists, overwrite FALSE |                  | error "already exists"           | FR-AS157
#  spec_file      | ".csv" / ".CSV"       |                    | written                          | FR-AS153
#  spec_file      | ".txt" / no extension / "" / NA / a number | | error "`spec_file` must end in .xlsx or .csv." | FR-AS154
#  recursive      | TRUE / FALSE          |                    | three rows / two rows            | FR-AS155
#  toc_page_numbering | default / "roman" / unknown |          | decimal / \pgnlcrm / error       | FR-AS157 FR-AS129
#  ...            | cover / an unknown argument |              | passed on / error "unused argument" | FR-AS158
#  toc levels     | the table's level 0 / 4 | spec$level       | error "`level` must be a whole number" | FR-AS238
#  (dir NA / two folders / a number, recursive NA / "yes", spec_file of two
#   paths or in a folder that is not there: base-R errors, and a folder of
#   one file names `input_files`: issue #862.)
#
# ---------------------------------------------------------------------------
#  listing_col()  -- FT-LS154.  USUBJID / AGE / AETERM, three records.
# ---------------------------------------------------------------------------
#  vars           | one / n               |                    | object: the column               | FR-LS143
#  vars           | length 0 / NA / "" / a number / NULL |      | error "`vars` must be one or more non-empty" | FR-LS158
#  vars           | not in data           | "NOPE"             | error at build "Column \"NOPE\" not in `data`" | FR-LS115
#  sep            | "" / " / " / non-ASCII |                   | joined with it                   | FR-LS144
#  sep            | NA / length 2 / a number |                 | error "`sep` must be a single string" | FR-LS159
#  width          | 1 (min) / 10          |                    | wrapped to it                    | FR-LS145
#  width          | 0 / -1 / NA / "10" / length 2 / length 0 / TRUE | | error "`width` must be a single positive number of characters" | FR-LS160
#  width          | 1.5 / Inf             |                    | error, same                      | FR-LS251
#  label          | valid / "" / n lines / non-ASCII / long |    | object: the label                | FR-LS146 FR-LS151
#  label          | NA / a number / length 0 |                 | error "`label` must be a string" | FR-LS162
#  name           | valid / non-ASCII     |                    | the output column name           | FR-LS152
#  name           | "" / NA / length 2 / a number |            | error "`name` must be a single non-empty string" | FR-LS163
#  rel_width      | a fraction 0.5        | 0.5                | object: 0.5                      | FR-LS153
#  rel_width      | 0 / -1 / NA / "2" / length 2 / TRUE |      | error "`rel_width` must be a single positive number" | FR-LS161
#  rel_width      | Inf                   | Inf                | error, same                      | FR-LS251
#  align          | the three / prefix    |                    | object                           | FR-LS154
#  align          | another case / "" / NA / length 2 |        | error (match.arg)                | FR-LS164
#  layout         | both / prefix         |                    | object                           | FR-LS155
#  layout         | unknown / NA          |                    | error (match.arg)                | FR-LS164
#  collapse_repeats | TRUE / FALSE        |                    | object                           | FR-LS156
#  collapse_repeats | NA / "yes" / 1 / length 2 / length 0 |   | error "must be TRUE or FALSE"    | FR-LS165
#
# ---------------------------------------------------------------------------
#  listing_spec()  -- FT-LS155.
# ---------------------------------------------------------------------------
#  cols           | strings / a duplicate |                    | object: USUBJID, USUBJID_1       | FR-LS119 FR-LS120
#  cols           | NULL / list() / a number / list(1) |       | error (FR-LS133 / 134 / 135)      | FR-LS133 FR-LS134 FR-LS135
#  type           | valid                 | "multiline"        | object                           | FR-LS121
#  type           | prefix / another case / unknown |          | error "Unknown listing type"     | FR-LS136
#  type           | NA / a number / length 2 / "" |            | error "`type` must be a single listing-template name" | FR-LS136
#  sep            | "-" / ""              |                    | joined with it                   | FR-LS122
#  sep            | NA / a number / length 2 |                 | error "`sep` must be a single string" | FR-LS122
#  spacer, blank_row, blank_row_first | TRUE / FALSE |         | gutters / blank rows             | FR-LS123 FR-LS125 FR-LS126
#  spacer, blank_row, blank_row_first | NA / "yes" / 1 / length 2 | | error "must be TRUE or FALSE, or NULL" | FR-LS137
#  spacer_rel_width | a fraction 0.25     |                    | object                           | FR-LS124
#  spacer_rel_width | 0 / -1 / NA / "1" / length 2 |           | error "must be a single positive number" | FR-LS138
#  spacer_rel_width | Inf                 |                    | error, same                      | FR-LS251
#  align / layout | prefix / unknown / NA |                    | object / error (match.arg)       | FR-LS127 FR-LS128 FR-LS139
#  wrap           | listing_wrap / a ... function |            | the same cells                   | FR-LS129 FR-LS171
#  wrap           | two arguments / a string / NA |            | error "must accept four" / "must be a function" | FR-LS140
#  record         | TRUE / FALSE / a name / non-ASCII |        | the record column                | FR-LS131
#  record         | "" / NA / a number / length 2 |            | error "`record` must be TRUE, FALSE, or a single column name" | FR-LS141
#
# ---------------------------------------------------------------------------
#  build_listing()  -- FT-LS156.
# ---------------------------------------------------------------------------
#  data           | 0 rows / 1 row / a tibble / all NA / non-ASCII | | object                      | FR-LS101 FR-LS102 FR-LS109
#  data           | a list / NULL         |                    | error "`data` must be a data.frame or tibble" | FR-LS112
#  data           | already built         |                    | error "already been through build_listing()" | FR-LS114
#  spec           | NULL / a list / a listing_col() |          | error "`spec` must be a listing_spec()" | FR-LS113
#
# ---------------------------------------------------------------------------
#  fit_listing_widths()  -- FT-LS157.  Two columns, gutter 1.
# ---------------------------------------------------------------------------
#  total_width    | 60 / 20 (min that fits) |                  | widths that add up to it         | FR-LS206 FR-LS216
#  total_width    | 1 / TRUE              |                    | error "A total width of 1 character(s)" | FR-LS229
#  total_width    | 0 / -1 / NA / Inf / length 2 |             | error "`total_width` must be a single positive number of characters" | FR-LS228
#  total_width    | 100.7 / "100"         |                    | error, same                      | FR-LS252
#  min_width      | 1 / 6 (default)       |                    | object                           | FR-LS219
#  min_width      | 0 / NA / Inf / length 2 |                  | error "`min_width` must be a single positive integer" | FR-LS226
#  min_width      | 6.5 / "6" / TRUE      |                    | error, same                      | FR-LS252
#  min_width      | 30 (too wide for the budget) |             | error "A total width of 60"      | FR-LS229
#  probs          | 0 / 0.5 / 1           |                    | object                           | FR-LS220
#  probs          | -0.1 / 1.1 / NA / "0.9" / length 2 |       | error "`probs` must be a single number in [0, 1]" | FR-LS227
#  header_lines   | 1 / 2.5 / Inf         |                    | object                           | FR-LS218
#  header_lines   | 0 / 0.5 / -1 / NA / "4" |                  | error "`header_lines` must be a single number >= 1, or Inf" | FR-LS225
#  font_size_half_points | 16 / 1         |                    | a wider budget at a smaller size | FR-LS214
#  font_size_half_points | 0 / -1 / NA / Inf |                 | error "Could not measure the width" | FR-LS232
#  font_size_half_points | 17.5 / "18" / TRUE / length 2 / NULL | | error "`font_size_half_points` must be a single positive integer" | FR-LS252
#  font           | "Arial" / named slots |                    | measured                         | FR-LS213
#  font           | "" / NA / a number / unnamed length 2 |    | error "`font` must be a single font family name" | FR-LS252
#  page           | rtf_page() / list()   |                    | measured                         | FR-LS212
#  page           | a string / a number   |                    | error "`page` must be an rtf_page() object" | FR-LS230
#  labels         | named / a list        |                    | the column's label               | FR-LS217
#  labels         | unnamed / NA name / a number / length 0 |  | error "`labels` must be a named character vector" | FR-LS224
#  data           | 0 rows                |                    | object                           | FR-LS205
#
# ---------------------------------------------------------------------------
#  listing_wrap()  -- FT-LS158.
# ---------------------------------------------------------------------------
#  width          | 1 (min) / 5 / 1.5 / Inf / NULL |           | lines of at most that width; NULL / Inf no limit | FR-LS170 FR-LS174 FR-LS178
#  width          | 0 / -1 / NA / "5" / length 2 / TRUE |      | error "`width` must be a single positive number, or NULL" | FR-LS181
#  sep            | "/" / "" / NULL / " " |                    | broken after it / not split      | FR-LS172
#  sep            | NA / a number / length 2 |                 | error "`sep` must be a single string" | FR-LS180
#  layout         | "flow" / prefix       |                    | filled lines                     | FR-LS177
#  layout         | unknown / NA          |                    | error (match.arg)                | FR-LS177
#  text           | "" / NA / length 0 / n / non-ASCII / long |  | "" / list() / a list / display width | FR-LS175 FR-LS176 FR-LS179
#
# ---------------------------------------------------------------------------
#  collapse_repeats()  -- FT-LS159.  A = x, x, y.
# ---------------------------------------------------------------------------
#  cols           | a name / a position / a list / two names |  | the repeat blanked (NA)          | FR-LS233 FR-LS234 FR-LS235
#  cols           | length 0 / NULL       |                    | nothing blanked                  | FR-LS235
#  cols           | 0 / n + 1 / -1 / NA   |                    | error "index ... out of range"   | FR-LS238
#  cols           | unknown / another case |                   | error "column 'a' not found"     | FR-LS238
#  x              | a list of pages / list() |                 | each page                        | FR-LS236
#  x              | a data.frame / NULL   |                    | error "expects an rtftable"      | FR-LS239
#  (A position of 1.5 or TRUE is read as 1: the shared column resolver, #850.)
#
# ---------------------------------------------------------------------------
#  normalize_ard()  -- FT-AR107.  Two arms, SEX counts, the total N.
# ---------------------------------------------------------------------------
#  drop_contexts  | default               |                    | total_n row gone, ard_total_n 9  | FR-AR119
#  drop_contexts  | length 0 / NULL       |                    | nothing dropped                  | FR-AR121
#  drop_contexts  | one context           | "tabulate"         | those rows gone                  | FR-AR119
#  drop_contexts  | unknown / another case / NA / "" |         | nothing dropped                  | FR-AR119
#  stack_sep      | "" / "-" / non-ASCII / long | crossed layer | joins the names and levels       | FR-AR126
#
# ---------------------------------------------------------------------------
#  widen_ard()  -- FT-AR108.  Drug A has F and M, Placebo has F.
# ---------------------------------------------------------------------------
#  na             | NA (default) / "" / "-" / non-ASCII |      | the empty cell                   | FR-AR180
#  na             | length 2 / length 0 / NULL / a number |    | error "`na` is one text"         | FR-AR250
#  label          | default / a column / named |               | the label column                 | FR-AR156
#  label          | NULL / NA / length 0  |                    | no label column                  | FR-AR157 FR-AR158
#  label          | a formula             | ~ "x{.label}"      | the template                     | FR-AR247
#  label          | unknown / "" / a number |                  | error "no column"                | FR-AR197
#  label          | length 2              | c(".label", "variable") | error "`label` is one column" | FR-AR250
#  stat_groups    | one group / list()    |                    | object                           | FR-AR183
#  stat_groups    | unnamed / NA / a number / a statistic twice / empty |  | error (FR-AR209)      | FR-AR209
#
# ---------------------------------------------------------------------------
#  pull_ard()  -- FT-AR109.
# ---------------------------------------------------------------------------
#  cols           | valid / twice         |                    | Drug A 5, Placebo 4              | FR-AR213 FR-AR216
#  cols           | unknown / NA          |                    | error "`cols`: no key"           | FR-AR224
#  cols           | length 0 / a number   |                    | error "`cols` names one or more column keys" | FR-AR251
#  stat           | "N" / "n"             |                    | that statistic                   | FR-AR218
#  stat           | unknown / ""          |                    | error "No 'x' found"             | FR-AR225
#  stat           | NA / length 2 / a number |                 | error "`stat` is one statistic's name" | FR-AR251
#  variable       | the key / unknown / NA |                   | its own rows / error "No 'N' found" | FR-AR219 FR-AR225
#  context        | valid / unknown       |                    | the value / error "No 'N' found" | FR-AR215 FR-AR225
#  sep            | "-" / ""              |                    | the value                        | FR-AR221
#  sep            | NA / length 2 / a number |                 | error "`sep` is one text"        | FR-AR251
#  levels         | a list / a plain vector / NA |             | ordered / not read               | FR-AR220 FR-AR239
#
# ---------------------------------------------------------------------------
#  fmt_round()  -- FT-FM102, fmt_signif()  -- FT-FM103.
# ---------------------------------------------------------------------------
#  x              | x.5 / -x.5 / 0 / -0   | 0.5, 1.5, 2.5, -0.5 ...| half to even ("r") / away ("sas"), no "-0" | FR-FM117 FR-FM118 FR-FM124
#  x              | very large / very small | 1e15, 123456789.125, 1e-15 | exact digits, no fuzz carry | FR-FM125 FR-FM251
#  x              | just below a half     | 1.00499999         | "1.00" under both                | FR-FM251
#  x              | NA / NaN / Inf / -Inf |                    | na text / "Inf" / "-Inf"         | FR-FM121 FR-FM122
#  x              | length 0 / integer    |                    | character(0) / formatted         | FR-FM114
#  x              | text / logical / factor / NULL / a list |  | error "formats numeric input"    | FR-FM126 FR-FM145
#  digits         | 0 (min) / 15 / 22     |                    | that many decimals               | FR-FM115 FR-FM116
#  digits         | -1 / 1.5 / NA / Inf / length 2 / length 0 / NULL | | error "must be a single non-negative integer" | FR-FM127 FR-FM146
#  digits         | "2" / TRUE            |                    | error, same                      | FR-FM252
#  rounding       | "r" / "sas" / "SAS"   |                    | the family                       | FR-FM117 FR-FM118
#  rounding       | prefix / unknown / NA / a number / length 2 / "" | | error "`rounding` must be"  | FR-FM128
#  na             | "-" / "" / non-ASCII / NULL |              | the text for NA and NaN          | FR-FM121 FR-FM141
#  na             | NA / a number / length 2 / length 0 |      | error "`na` must be a single string" | FR-FM253
#  small          | "fixed" / prefix      |                    | integer counting                 | FR-FM138
#  small          | another case / NA / a number |             | error (match.arg)                | FR-FM147
#  (fmt_signif(digits = 0) prints "0" for 0.012: issue #863.)
#
# ---------------------------------------------------------------------------
#  fmt_numeric()  -- FT-FM104.  g / v (1.25, 2.35, NA) / s.
# ---------------------------------------------------------------------------
#  digits, signif | one of them           |                    | formatted                        | FR-FM153 FR-FM154
#  digits, signif | both / neither        |                    | error "Give exactly one"         | FR-FM165
#  digits         | -1 / 1.5 / NA / "1" / TRUE |               | error "must be a single non-negative integer" | FR-FM173 FR-FM252
#  cols           | name / position / two / length 0 |         | formatted / unchanged            | FR-FM151 FR-FM152
#  cols           | unknown / 0 / n + 1 / NA |                 | error "not found" / "out of range" | FR-FM172
#  by, formats    | valid / no formats / formats without by |  | formatted / error                | FR-FM155 FR-FM165 FR-FM167
#  na             | "-" / NA / a number / length 2 |           | the text / error "`na` must be a single string" | FR-FM162 FR-FM253
#  data           | 0 rows / a list       |                    | object / error "needs a data frame" | FR-FM149 FR-FM164
#
# ---------------------------------------------------------------------------
#  fmt_count_pct()  -- FT-FM105.
# ---------------------------------------------------------------------------
#  count, pct     | 0 / 1 / 10 / 100 / NA / NaN / Inf |       | the four branches                | FR-FM175 FR-FM176 FR-FM177 FR-FM178 FR-FM189
#  pct            | x.x5 halves           | 1/16, 0.9995       | "6.2" / "6.3", 100 branch        | FR-FM187 FR-FM190
#  count, pct     | a bare NA             | NA                 | missing                          | FR-FM255
#  count          | a fraction / text     |                    | error                            | FR-FM192 FR-FM249
#  count, pct     | length 0 / mismatched |                    | character(0) / error             | FR-FM191 FR-FM193
#  pct            | above 1 as a fraction | 5                  | warning, "500"                   | FR-FM194
#  pct_unit       | "percent" / prefix    |                    | as given                         | FR-FM183
#  pct_unit       | another case / NA / a number |             | error (match.arg)                | FR-FM196
#  nbsp           | " " / "" / non-ASCII  |                    | the padding                      | FR-FM185
#  nbsp           | NA / length 2 / a number |                 | error "`nbsp` must be a single string" | FR-FM254
#  pct_sign       | TRUE                  |                    | "%)"                             | FR-FM184
#  pct_sign       | NA / "yes" / 1 / length 2 |                | error "`pct_sign` must be TRUE or FALSE" | FR-FM254
#  na             | "-" / "NE" / NULL     |                    | the token, right-aligned         | FR-FM186
#  na             | NA / a number / length 2 |                 | error "`na` must be a single string" | FR-FM195
#  rounding       | "sas" / unknown / NA  |                    | "6.3" / error                    | FR-FM187 FR-FM196
#
# ---------------------------------------------------------------------------
#  fmt_value_paren()  -- FT-FM106.
# ---------------------------------------------------------------------------
#  x              | a vector of shapes / length 0 |            | aligned / returned as it is      | FR-FM228 FR-FM233 FR-FM235
#  nbsp           | " " / ""              |                    | the padding                      | FR-FM237
#  nbsp           | NA / length 2 / a number |                 | error "`nbsp` must be a single string" | FR-FM254
#  na             | "-" / NULL            |                    | the token                        | FR-FM238
#  na             | NA / a number / length 2 |                 | error "`na` must be a single string" | FR-FM239
#  rounding       | "sas" / unknown / NA  |                    | the value / error                | FR-FM248
#
# ---------------------------------------------------------------------------
#  The plan verbs' scalar arguments not in batch 2 -- FT-PL973 to FT-PL979.
#  Age (Mean) and Sex (F, M) in two arms, the records of no arm for a Total.
# ---------------------------------------------------------------------------
#  plan_total     label    | valid / non-ASCII / long |          | the column's heading             | FR-PL265
#  plan_total     label    | "" / NA / length 2 / a number / length 0 / NULL | | error "is one text" | FR-PL275
#  plan_total     position | "first" / prefix      |             | Total before the arms            | FR-PL265
#  plan_total     position | another case / unknown / NA / length 2 | | error (match.arg)          | FR-PL275
#  plan_total     direction| "rows" / prefix, no margins |       | error "the ARD has no margins"   | FR-PL276
#  plan_total     direction| unknown / NA          |             | error (match.arg)                | FR-PL275
#  plan_sort      keep     | TRUE / FALSE          |             | the sort key kept / hidden       | FR-PL311
#  plan_sort      keep     | NA / "yes" / 1 / length 2 |         | error "plan_sort(keep = ) is TRUE" | FR-PL960
#  plan_sort      stat     | valid                 | "n"         | sorted                           | FR-PL310
#  plan_sort      stat     | NA / length 2 / a number / "" |     | error "is one statistic's name"  | FR-PL960
#  plan_filter    any      | TRUE / FALSE          |             | 3 rows / 1 row                   | FR-PL142 FR-PL143
#  plan_filter    any      | NA / "yes" / 1        |             | error "plan_filter(any = ) is TRUE" | FR-PL150
#  plan_filter    parents  | "drop" / prefix       |             | judged by its own counts         | FR-PL145
#  plan_filter    parents  | unknown / NA          |             | error (match.arg)                | FR-PL151
#  plan_filter    by       | a row key / not a row key |         | judged together / error "not a row key" | FR-PL146 FR-PL152
#  plan_filter    by       | NA / a number / length 2 |          | error "is one row key's name"    | FR-PL960
#  plan_columns   widths   | n / shorter / by name |             | col_rel_width                    | FR-PL157 FR-PL158 FR-PL159
#  plan_columns   widths   | a name not there / a column left out | | error                       | FR-PL160 FR-PL176
#  plan_columns   auto_width | TRUE / FALSE        |             | pages                            | FR-PL166
#  plan_columns   auto_width | NA / "yes" / 1      |             | error "must be TRUE or FALSE"    | FR-PL166
#  plan_columns   sep      | valid / non-ASCII     |             | the value columns                | FR-PL167
#  plan_columns   sep      | "" / NA / a number / length 2 |     | error "is one non-empty string"  | FR-PL179
#  plan_append    name     | valid / non-ASCII / NULL |          | the part appended                | FR-PL945
#  plan_append    name     | "" / NA / a number / length 2 |     | error "is one non-empty string"  | FR-PL945
#  plan_append    absent   | "skip" / prefix       |             | appended                         | FR-PL945
#  plan_append    absent   | unknown / NA          |             | error (match.arg)                | FR-PL945
#  plan_col_header span    | "each" / "one" / a key |            | a cell a column / a spanner      | FR-PL392
#  plan_col_header span    | not a key / NA / a number |         | error                            | FR-PL392
#  plan_col_header header_sep | valid / length 2 |               | pages                            | FR-PL387
#  plan_col_header header_sep | NA / a number    |               | error "must be NULL or a character vector" | FR-PL387
#  plan_col_header col_header_align | valid / unknown / NA / wrong length | | pages / error        | FR-PL388
#  plan_levels    .drop_empty | one / n / NULL     |             | no level dropped (none empty)    | FR-PL256
#  plan_levels    .drop_empty | TRUE / NA / a number |           | error "takes variable names"     | FR-PL262
#  (plan_columns(widths = 0 / -1 / NA / Inf / "3") and decimal = an unknown
#   column pass until the file is written: issue #864.  plan_labels(),
#   plan_nest() and plan_compare() take maps (`...`) rather than scalars and
#   are left for a later batch.)
# ---------------------------------------------------------------------------

# ---- the runner (as in batch 2) ---------------------------------------------

# One case: `expect` is "ok:<regex>" (the result matches), "no:<regex>" (it
# does not), "ok" / "obj" with a `chk` function, "warning:<regex>" or
# "error:<regex>".  `more` holds the other arguments.
.eb3_case <- function(arg, cls, val, expect, chk = NULL, more = list()) {
  list(arg = arg, cls = cls, val = val, expect = expect, chk = chk,
       more = more)
}
.eb3_err <- function(arg, cls, val, pat, more = list()) {
  .eb3_case(arg, cls, val, paste0("error:", pat), more = more)
}
.eb3_obj <- function(arg, cls, val, chk, more = list()) {
  .eb3_case(arg, cls, val, "obj", chk = chk, more = more)
}
.eb3_run <- function(cases, run) {
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

# ---- assemble ----------------------------------------------------------------

.eb3_env <- new.env()
# Two one-page TFL files in a folder of their own, made once.
.eb3_files <- function() {
  if (is.null(.eb3_env$files)) {
    d <- tempfile("eb3asm")
    dir.create(d)
    mk <- function(t, f) {
      doc <- rtf_document() |>
        rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
        rtf_titles(list(c(paste("Table", t), "Safety Population")))
      suppressMessages(generate_rtfreport(doc, file.path(d, f)))
      file.path(d, f)
    }
    .eb3_env$files <- c(mk("14.1.1", "t14_1_1.rtf"), mk("14.2.1", "t14_2_1.rtf"))
    .eb3_env$dir <- d
  }
  .eb3_env$files
}
.eb3_read <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")

test_that("FT-AS140 [FR-AS102 FR-AS103 FR-AS104 FR-AS107 FR-AS108 FR-AS109 FR-AS110 FR-AS112 FR-AS114 FR-AS115 FR-AS116 FR-AS117 FR-AS118 FR-AS119 FR-AS124 FR-AS127 FR-AS128 FR-AS129 FR-AS131 FR-AS133 FR-AS134 FR-AS138 FR-AS139 FR-AS238 FR-AS244 FR-AS245 FR-AS246]: assemble_rtf(): equivalence classes and boundaries of its arguments", {
  C <- .eb3_case
  E <- .eb3_err
  f <- .eb3_files()
  a <- f[1L]
  b <- f[2L]
  tab <- function(...) data.frame(file = f, label = c("L1", "L2"), ...)
  exists_out <- tempfile(fileext = ".rtf")
  writeLines("{\\rtf1 x}", exists_out)
  nf  <- "^Input file not found"
  two <- "^`input_files` must be a character vector with at least 2 elements"
  of  <- "^`output_file` must be a single non-empty file path"
  ow  <- "^`overwrite` must be TRUE or FALSE"
  cv  <- "^`cover` must be NULL or a list with the fields title"
  len <- "^`toc` must be a character vector the same length as `input_files`"
  lvl <- "^A TOC entry's `level` must be a whole number from 1 to 3"
  tt  <- "^`toc_title` must be a single string"
  bp  <- "^`bookmark_prefix` must be a single string of letters, digits and `_`"
  bk  <- "^`book_page` must be NULL or a single string"
  ma  <- "^'arg' should be one of"
  mn  <- "^'arg' must be NULL or a character vector"
  cases <- list(
    E("input_files", "length 1", a, two),
    E("input_files", "length 0", character(0), two),
    E("input_files", "a number", 1:2, two),
    E("input_files", "NA", c(a, NA), nf),
    E("input_files", "a missing file", c(a, file.path(dirname(a), "nope.rtf")), nf),
    C("input_files", "the same file twice", c(a, a), "ok", more = list(toc = "auto"),
      chk = function(x) grepl("bkmkstart tfl_t14_1_1_1", x) && grepl("bkmkstart tfl_t14_1_1_2", x)),
    C("output_file", "non-ASCII name", file.path(tempdir(), "出力.rtf"), "ok:Table 14.2.1"),
    E("output_file", "NA", NA_character_, of),
    E("output_file", "empty string", "", of),
    E("output_file", "length 2", c(tempfile(), tempfile()), of),
    E("output_file", "a number", 1, of),
    E("output_file", "NULL", NULL, of),
    E("output_file", "a folder that is not there", file.path(tempdir(), "no_such_dir", "o.rtf"),
      "^The folder of `output_file` does not exist"),
    E("output_file", "one of the inputs", a, "^`output_file` is one of the `input_files`",
      more = list(overwrite = TRUE)),
    E("output_file", "exists, overwrite FALSE", exists_out, "^`output_file` already exists"),
    C("overwrite", "TRUE", TRUE, "ok:Table 14.1.1", more = list(output_file = exists_out)),
    E("overwrite", "NA", NA, ow),
    E("overwrite", "a string", "yes", ow),
    E("overwrite", "a number", 1, ow),
    E("overwrite", "length 2", c(TRUE, FALSE), ow),
    C("cover", "valid", list(title = "CVR", subtitle = "SUBT"), "ok:\\\\fs44 CVR\\\\par"),
    C("cover", "length 0", list(), "no:\\\\fs44"),
    C("cover", "empty field", list(title = ""), "no:\\\\fs44"),
    C("cover", "NA field", list(title = NA, date = "2026-01-01"), "ok", chk = function(x) {
      !grepl("\\\\fs44", x) && !grepl("\\\\fs22 NA", x) && grepl("\\\\fs22 2026-01-01", x) }),
    C("cover", "NA line of meta", list(meta = c("M1", NA)), "ok", chk = function(x) {
      grepl("\\\\fs20 M1\\\\par", x) && !grepl("\\\\fs20 \\\\par", x) && !grepl("\\\\fs20 NA", x) }),
    C("cover", "non-ASCII", list(title = "試験"), "ok:\\\\fs44 \\\\u-30106\\?\\\\u-26093\\?"),
    C("cover", "long", list(title = strrep("c", 500)), paste0("ok:\\\\fs44 ", strrep("c", 500))),
    E("cover", "a string", "Cover", cv),
    E("cover", "a number", 1, cv),
    E("cover", "TRUE", TRUE, cv),
    E("cover", "a data.frame", data.frame(title = "x"), cv),
    E("cover", "unknown field", list(titel = "x"), cv),
    E("cover", "a field of length 2", list(title = c("a", "b")), "^`cover\\$title` must be a single string"),
    C("toc", "auto", "auto", "ok", chk = function(x) {
      grepl("HYPERLINK", x) && grepl("Table 14.2.1", x) }),
    E("toc", "another case", "AUTO", len),
    E("toc", "length 0", character(0), len),
    E("toc", "wrong length", "only one", len),
    E("toc", "an NA label", c("x", NA), "^`toc` labels must not be NA"),
    E("toc", "a number", 1, "^`toc` must be NULL, \"auto\""),
    E("toc", "TRUE", TRUE, "^`toc` must be NULL, \"auto\""),
    C("toc", "table, level 1 (min)", tab(level = 1), "no:\\\\li600"),
    C("toc", "table, level 3 (max)", tab(level = 3), "ok:\\\\li1200"),
    C("toc", "table, level NA (default 2)", tab(level = NA), "ok:\\\\li600"),
    E("toc", "table, level 0", tab(level = 0), lvl),
    E("toc", "table, level 4", tab(level = 4), lvl),
    E("toc", "table, level 2.5", tab(level = 2.5), lvl),
    E("toc", "table, level -1", tab(level = -1), lvl),
    E("toc", "table, level Inf", tab(level = Inf), lvl),
    E("toc", "table, an NA label", data.frame(file = f, label = c(NA, "y")),
      "^A TOC entry's `label` must be a single string, not NA"),
    E("toc", "table without label", data.frame(file = f),
      "^A table of contents needs the columns `file` and `label`"),
    E("toc", "table of 0 rows", data.frame(file = character(), label = character()), two),
    E("toc", "table naming a missing file", data.frame(file = c(a, "zz.rtf"), label = c("x", "y")),
      "^The table of contents names missing file"),
    E("toc", "a .csv that is not there", file.path(tempdir(), "no_such_toc.csv"), "^`toc` file not found"),
    E("toc", "a .txt path", "toc.txt", "^Spec file must end in .xlsx or .csv"),
    C("toc_title", "valid", "Contents", "ok:Contents", more = list(toc = "auto")),
    C("toc_title", "empty string", "", "no:Table of Contents", more = list(toc = "auto")),
    C("toc_title", "non-ASCII", "目次", "ok:\\\\u30446\\?\\\\u27425\\?", more = list(toc = "auto")),
    C("toc_title", "long", strrep("t", 300), paste0("ok:", strrep("t", 300)), more = list(toc = "auto")),
    E("toc_title", "NA", NA_character_, tt),
    E("toc_title", "length 2", c("a", "b"), tt),
    E("toc_title", "length 0", character(0), tt),
    E("toc_title", "a number", 1, tt),
    E("toc_title", "NULL", NULL, tt),
    C("toc_leader", "none", "none", "no:tldot", more = list(toc = "auto")),
    C("toc_leader", "prefix", "d", "ok:tldot", more = list(toc = "auto")),
    E("toc_leader", "unknown", "bogus", ma),
    E("toc_leader", "NA", NA, mn),
    C("toc_page_numbering", "roman", "roman", "ok:pgnlcrm", more = list(toc = "auto")),
    C("toc_page_numbering", "prefix", "rom", "ok:pgnlcrm", more = list(toc = "auto")),
    E("toc_page_numbering", "another case", "Roman", ma),
    E("toc_page_numbering", "empty string", "", ma),
    E("toc_page_numbering", "NA", NA, mn),
    E("toc_page_numbering", "length 2", c("none", "roman"), "^'arg' must be of length 1"),
    C("bookmark_prefix", "empty string", "", "ok:bkmkstart t14_1_1\\}", more = list(toc = "auto")),
    C("bookmark_prefix", "valid", "studyX_", "ok:bkmkstart studyX_t14_2_1", more = list(toc = "auto")),
    E("bookmark_prefix", "NA", NA_character_, bp),
    E("bookmark_prefix", "a space", "a b", bp),
    E("bookmark_prefix", "length 2", c("a", "b"), bp),
    E("bookmark_prefix", "a number", 1, bp),
    E("bookmark_prefix", "non-ASCII", "試", bp),
    C("book_page", "valid", "Page {AUTO_PAGE}", "ok:Table 14.2.1"),
    E("book_page", "static token", "{PAGE}", "^`book_page` cannot use the static"),
    E("book_page", "NA", NA_character_, bk),
    E("book_page", "length 2", c("a", "b"), bk),
    E("book_page", "length 0", character(0), bk),
    E("book_page", "a number", 1, bk)
  )
  .eb3_run(cases, function(args, obj) {
    if (!"input_files" %in% names(args) && !is.data.frame(args$toc)) {
      args <- c(list(input_files = f), args)
    }
    if (!"output_file" %in% names(args)) args$output_file <- tempfile(fileext = ".rtf")
    out <- do.call(assemble_rtf, args)
    .eb3_read(out)
  })
})

test_that("FT-AS141 [FR-AS129 FR-AS148 FR-AS152 FR-AS153 FR-AS154 FR-AS155 FR-AS156 FR-AS157 FR-AS158 FR-AS238]: assemble_folder(): equivalence classes and boundaries of its arguments", {
  C <- .eb3_case
  E <- .eb3_err
  O <- .eb3_obj
  f <- .eb3_files()
  d <- .eb3_env$dir
  # a folder with a sub-folder (recursive), a folder of no .rtf file, and a
  # folder in kanji
  top <- tempfile("eb3top")
  dir.create(file.path(top, "sub"), recursive = TRUE)
  file.copy(f, top)
  file.copy(f[1L], file.path(top, "sub", "t14_3_1.rtf"))
  empty <- tempfile("eb3empty")
  dir.create(empty)
  kanji <- file.path(tempfile("eb3k"), "試験")
  dir.create(kanji, recursive = TRUE)
  file.copy(f, kanji)
  out_exists <- tempfile(fileext = ".rtf")
  writeLines("{\\rtf1 x}", out_exists)
  sf  <- "^`spec_file` must end in .xlsx or .csv."
  csv <- file.path(tempdir(), "eb3_spec.csv")
  CSV <- file.path(tempdir(), "eb3_spec2.CSV")
  nrows <- function(n) function(x) is.data.frame(x) && nrow(x) == n
  cases <- list(
    O("dir", "valid", d, nrows(2)),
    O("dir", "non-ASCII", kanji, nrows(2)),
    E("dir", "not there", file.path(d, "nope"), "^Directory not found"),
    E("dir", "no .rtf file", empty, "^No RTF files found."),
    O("output_file", "NULL", NULL, nrows(2)),
    O("output_file", "a path", tempfile(fileext = ".rtf"), function(x) {
      identical(names(x), c("output", "spec")) && file.exists(x$output) }),
    E("output_file", "exists, overwrite FALSE", out_exists, "^`output_file` already exists"),
    O("spec_file", ".csv", csv, function(x) file.exists(csv) && nrow(utils::read.csv(csv)) == 2L),
    O("spec_file", ".CSV", CSV, function(x) file.exists(CSV)),
    E("spec_file", ".txt", file.path(tempdir(), "s.txt"), sf),
    E("spec_file", "no extension", file.path(tempdir(), "s"), sf),
    E("spec_file", "empty string", "", sf),
    E("spec_file", "NA", NA_character_, sf),
    E("spec_file", "a number", 1, sf),
    O("recursive", "TRUE", TRUE, nrows(3), more = list(dir = top)),
    O("recursive", "FALSE (default)", FALSE, nrows(2), more = list(dir = top)),
    C("toc_page_numbering", "default (decimal)", "decimal", "no:pgnlcrm",
      more = list(output_file = tempfile(fileext = ".rtf"))),
    C("toc_page_numbering", "roman", "roman", "ok:pgnlcrm",
      more = list(output_file = tempfile(fileext = ".rtf"))),
    E("toc_page_numbering", "unknown", "bogus", "^'arg' should be one of",
      more = list(output_file = tempfile(fileext = ".rtf"))),
    C("cover", "passed on (...)", list(title = "STUDY-EB3"), "ok:STUDY-EB3",
      more = list(output_file = tempfile(fileext = ".rtf"))),
    E("bogus", "an unknown argument (...)", 1, "unused argument",
      more = list(output_file = tempfile(fileext = ".rtf")))
  )
  .eb3_run(cases, function(args, obj) {
    if (!"dir" %in% names(args)) args <- c(list(dir = d), args)
    res <- do.call(assemble_folder, args)
    if (obj) res else .eb3_read(res$output)
  })
  # the table's level, edited and handed back to assemble_rtf()
  spec <- assemble_folder(d)
  for (lv in c(0, 4)) {
    spec$level <- lv
    expect_error(assemble_rtf(toc = spec, output_file = tempfile(fileext = ".rtf")),
                 "^A TOC entry's `level` must be a whole number from 1 to 3", info = lv)
  }
})

# ---- listing -----------------------------------------------------------------

.eb3_ae <- data.frame(USUBJID = c("S-01", "S-01", "S-02"), AGE = c(30, 30, 41),
                      AETERM = c("Headache", "Nausea and vomiting", NA),
                      stringsAsFactors = FALSE)

test_that("FT-LS154 [FR-LS115 FR-LS143 FR-LS144 FR-LS145 FR-LS146 FR-LS151 FR-LS152 FR-LS153 FR-LS154 FR-LS155 FR-LS156 FR-LS158 FR-LS159 FR-LS160 FR-LS161 FR-LS162 FR-LS163 FR-LS164 FR-LS165 FR-LS251]: listing_col(): equivalence classes and boundaries of its arguments", {
  C <- .eb3_case
  E <- .eb3_err
  O <- .eb3_obj
  vr <- "^`vars` must be one or more non-empty source column names"
  sp <- "^`sep` must be a single string, or NULL"
  wd <- "^`width` must be a single positive number of characters, or NULL"
  lb <- "^`label` must be a string, or a character vector of header lines, or NULL"
  nm <- "^`name` must be a single non-empty string, or NULL"
  rw <- "^`rel_width` must be a single positive number, or NULL"
  cr <- "^`collapse_repeats` must be TRUE or FALSE"
  ma <- "^'arg' should be one of"
  mn <- "^'arg' must be NULL or a character vector"
  col1 <- function(field, v) function(x) identical(x$spec$cols[[1L]][[field]], v)
  cell <- function(v) function(x) identical(x$body[[1L]][seq_along(v)], v)
  two  <- list(vars = c("USUBJID", "AGE"))
  ae   <- list(vars = "AETERM")
  cases <- list(
    O("vars", "one", "USUBJID", cell(c("S-01", "S-01", "S-02"))),
    O("vars", "n", c("USUBJID", "AGE"), cell("S-01/30")),
    E("vars", "length 0", character(0), vr),
    E("vars", "NA", NA_character_, vr),
    E("vars", "empty string", "", vr),
    E("vars", "a number", 1, vr),
    E("vars", "NULL", NULL, vr),
    E("vars", "not in data", "NOPE", "^Column \"NOPE\" not in `data`"),
    O("sep", "empty string", "", cell("S-0130"), more = two),
    O("sep", "spaced", " / ", cell("S-01 / 30"), more = two),
    O("sep", "non-ASCII", "／", cell("S-01／30"), more = two),
    E("sep", "NA", NA_character_, sp, more = two),
    E("sep", "length 2", c("/", "-"), sp, more = two),
    E("sep", "a number", 1, sp, more = two),
    O("width", "1 (min)", 1, cell(c("H", "e", "a")), more = ae),
    O("width", "10", 10, cell(c("Headache", "Nausea", "and", "vomiting")), more = ae),
    E("width", "0", 0, wd, more = ae),
    E("width", "negative", -1, wd, more = ae),
    E("width", "NA", NA, wd, more = ae),
    E("width", "a string", "10", wd, more = ae),
    E("width", "length 2", c(5, 6), wd, more = ae),
    E("width", "length 0", numeric(0), wd, more = ae),
    E("width", "TRUE", TRUE, wd, more = ae),
    E("width", "a fraction", 1.5, wd, more = ae),
    E("width", "Inf", Inf, wd, more = ae),
    O("label", "valid", "Subject", col1("label", "Subject")),
    O("label", "empty string", "", col1("label", "")),
    O("label", "n lines", c("Unique", "Subject ID"), col1("label", "Unique\nSubject ID")),
    O("label", "non-ASCII", "被験者", col1("label", "被験者")),
    O("label", "long", strrep("l", 300), col1("label", strrep("l", 300))),
    E("label", "NA", NA_character_, lb),
    E("label", "a number", 1, lb),
    E("label", "length 0", character(0), lb),
    O("name", "valid", "ID", function(x) identical(names(x$body)[1L], "ID")),
    O("name", "non-ASCII", "番号", function(x) identical(names(x$body)[1L], "番号")),
    E("name", "empty string", "", nm),
    E("name", "NA", NA_character_, nm),
    E("name", "length 2", c("a", "b"), nm),
    E("name", "a number", 1, nm),
    O("rel_width", "a fraction", 0.5, col1("rel_width", 0.5)),
    E("rel_width", "0", 0, rw),
    E("rel_width", "negative", -1, rw),
    E("rel_width", "NA", NA, rw),
    E("rel_width", "a string", "2", rw),
    E("rel_width", "length 2", c(1, 2), rw),
    E("rel_width", "TRUE", TRUE, rw),
    E("rel_width", "Inf", Inf, rw),
    O("align", "center", "center", col1("align", "center")),
    O("align", "prefix", "r", col1("align", "right")),
    E("align", "another case", "Center", ma),
    E("align", "empty string", "", ma),
    E("align", "NA", NA, mn),
    E("align", "length 2", c("left", "right"), "^'arg' must be of length 1"),
    O("layout", "flow", "flow", col1("layout", "flow")),
    O("layout", "prefix", "st", col1("layout", "stack")),
    E("layout", "unknown", "bogus", ma),
    E("layout", "NA", NA, mn),
    O("collapse_repeats", "TRUE", TRUE, col1("collapse_repeats", TRUE)),
    O("collapse_repeats", "FALSE", FALSE, col1("collapse_repeats", FALSE)),
    E("collapse_repeats", "NA", NA, cr),
    E("collapse_repeats", "a string", "yes", cr),
    E("collapse_repeats", "a number", 1, cr),
    E("collapse_repeats", "length 2", c(TRUE, TRUE), cr),
    E("collapse_repeats", "length 0", logical(0), cr)
  )
  .eb3_run(cases, function(args, obj) {
    if (!"vars" %in% names(args)) args$vars <- "USUBJID"
    spec <- listing_spec(list(do.call(listing_col, args)), spacer = FALSE,
                         blank_row = FALSE)
    list(spec = spec, body = build_listing(.eb3_ae, spec))
  })
})

test_that("FT-LS155 [FR-LS119 FR-LS120 FR-LS121 FR-LS122 FR-LS123 FR-LS124 FR-LS125 FR-LS126 FR-LS127 FR-LS128 FR-LS129 FR-LS131 FR-LS133 FR-LS134 FR-LS135 FR-LS136 FR-LS137 FR-LS138 FR-LS139 FR-LS140 FR-LS141 FR-LS171 FR-LS251]: listing_spec(): equivalence classes and boundaries of its arguments", {
  E <- .eb3_err
  O <- .eb3_obj
  ty <- "^`type` must be a single listing-template name"
  sp <- "^`sep` must be a single string"
  lg <- function(a) sprintf("^`%s` must be TRUE or FALSE, or NULL", a)
  sr <- "^`spacer_rel_width` must be a single positive number, or NULL"
  rc <- "^`record` must be TRUE, FALSE, or a single column name"
  wf <- "^`wrap` must be a function\\(text, width, sep, layout\\)"
  base <- build_listing(.eb3_ae, listing_spec(list(listing_col(c("USUBJID", "AGE")),
                                                   listing_col("AETERM", width = 8))))
  fld <- function(field, v) function(x) identical(x$spec[[field]], v)
  nms <- function(v) function(x) identical(names(x$body), v)
  same <- function(x) identical(lapply(x$body, as.character), lapply(base, as.character))
  cases <- list(
    O("cols", "strings", c("USUBJID", "AGE"), nms(c("USUBJID", ".sp1", "AGE", ".rtf_record"))),
    O("cols", "a duplicate", list("USUBJID", "USUBJID"), nms(c("USUBJID", ".sp1", "USUBJID_1", ".rtf_record"))),
    E("cols", "NULL", NULL, "^`cols` is required"),
    E("cols", "length 0", list(), "^`cols` must be a non-empty list"),
    E("cols", "a number", 1, "^`cols` must be a non-empty list"),
    E("cols", "a list of a number", list(1), "^`cols\\[\\[1\\]\\]` must be a listing_col()"),
    O("type", "valid", "multiline", fld("type", "multiline")),
    E("type", "prefix", "multi", "^Unknown listing type \"multi\""),
    E("type", "another case", "Multiline", "^Unknown listing type"),
    E("type", "unknown", "bogus", "^Unknown listing type"),
    E("type", "NA", NA, ty),
    E("type", "a number", 1, ty),
    E("type", "length 2", c("multiline", "multiline"), ty),
    E("type", "empty string", "", ty),
    O("sep", "valid", "-", function(x) identical(x$body[[1L]][1L], "S-01-30")),
    O("sep", "empty string", "", function(x) identical(x$body[[1L]][1L], "S-0130")),
    E("sep", "NA", NA_character_, sp),
    E("sep", "a number", 1, sp),
    E("sep", "length 2", c("/", "-"), sp),
    O("spacer", "FALSE", FALSE, nms(c("USUBJID", "AETERM", ".rtf_record"))),
    E("spacer", "NA", NA, lg("spacer")),
    E("spacer", "a string", "yes", lg("spacer")),
    E("spacer", "a number", 1, lg("spacer")),
    E("spacer", "length 2", c(TRUE, FALSE), lg("spacer")),
    O("blank_row", "FALSE", FALSE, function(x) nrow(x$body) == 5L),
    O("blank_row", "TRUE", TRUE, function(x) nrow(x$body) == 8L),
    E("blank_row", "NA", NA, lg("blank_row")),
    O("blank_row_first", "FALSE", FALSE, fld("blank_row_first", FALSE)),
    E("blank_row_first", "NA", NA, lg("blank_row_first")),
    E("blank_row_first", "a number", 1, lg("blank_row_first")),
    O("spacer_rel_width", "a fraction", 0.25, fld("spacer_rel_width", 0.25)),
    E("spacer_rel_width", "0", 0, sr),
    E("spacer_rel_width", "negative", -1, sr),
    E("spacer_rel_width", "NA", NA, sr),
    E("spacer_rel_width", "a string", "1", sr),
    E("spacer_rel_width", "length 2", c(1, 2), sr),
    E("spacer_rel_width", "Inf", Inf, sr),
    O("align", "prefix", "r", fld("align", "right")),
    E("align", "unknown", "bogus", "^'arg' should be one of"),
    E("align", "NA", NA, "^'arg' must be NULL or a character vector"),
    O("layout", "flow", "flow", fld("layout", "flow")),
    E("layout", "unknown", "bogus", "^'arg' should be one of"),
    O("wrap", "listing_wrap", listing_wrap, same),
    O("wrap", "a ... function", function(...) listing_wrap(...), same),
    E("wrap", "two arguments", function(a, b) a, "^`wrap` must accept four arguments"),
    E("wrap", "a string", "x", wf),
    E("wrap", "NA", NA, wf),
    O("record", "TRUE", TRUE, nms(c("USUBJID", ".sp1", "AETERM", ".rtf_record"))),
    O("record", "FALSE", FALSE, nms(c("USUBJID", ".sp1", "AETERM"))),
    O("record", "a name", "REC", nms(c("USUBJID", ".sp1", "AETERM", "REC"))),
    O("record", "non-ASCII", "記録", nms(c("USUBJID", ".sp1", "AETERM", "記録"))),
    E("record", "empty string", "", rc),
    E("record", "NA", NA, rc),
    E("record", "a number", 1, rc),
    E("record", "length 2", c("a", "b"), rc)
  )
  .eb3_run(cases, function(args, obj) {
    if (!"cols" %in% names(args)) {
      args$cols <- list(listing_col(c("USUBJID", "AGE")), listing_col("AETERM", width = 8))
    }
    spec <- do.call(listing_spec, args)
    list(spec = spec, body = build_listing(.eb3_ae, spec))
  })
})

test_that("FT-LS156 [FR-LS101 FR-LS102 FR-LS109 FR-LS112 FR-LS113 FR-LS114]: build_listing(): equivalence classes and boundaries of its arguments", {
  E <- .eb3_err
  O <- .eb3_obj
  spec <- listing_spec(list(listing_col(c("USUBJID", "AGE")), listing_col("AETERM", width = 8)))
  dm <- function(r, c) function(x) identical(dim(x), c(as.integer(r), as.integer(c)))
  dt <- "^`data` must be a data.frame or tibble"
  sp <- "^`spec` must be a listing_spec\\(\\)"
  na_row <- data.frame(USUBJID = NA_character_, AGE = NA_real_, AETERM = NA_character_)
  cases <- list(
    O("data", "0 rows", .eb3_ae[0, ], dm(0, 4)),
    O("data", "1 row", .eb3_ae[1, ], dm(2, 4)),
    O("data", "all NA", na_row, function(x) identical(x$USUBJID[1L], "") && identical(x$AETERM[1L], "")),
    O("data", "non-ASCII", data.frame(USUBJID = "被験者", AGE = 1, AETERM = "頭痛"),
      function(x) identical(x$USUBJID[1L], "被験者/1") && identical(x$AETERM[1L], "頭痛")),
    E("data", "a list", as.list(.eb3_ae), dt),
    E("data", "NULL", NULL, dt),
    E("data", "already built", build_listing(.eb3_ae, spec), "already been through build_listing\\(\\)"),
    E("spec", "NULL", NULL, sp),
    E("spec", "a list", list(), sp),
    E("spec", "a listing_col()", listing_col("AGE"), sp)
  )
  if (requireNamespace("tibble", quietly = TRUE)) {
    cases <- c(cases, list(O("data", "a tibble", tibble::as_tibble(.eb3_ae), dm(8, 4))))
  }
  .eb3_run(cases, function(args, obj) {
    if (!"data" %in% names(args)) args <- c(list(data = .eb3_ae), args)
    if (!"spec" %in% names(args)) args$spec <- spec
    do.call(build_listing, args)
  })
})

test_that("FT-LS157 [FR-LS205 FR-LS206 FR-LS212 FR-LS213 FR-LS214 FR-LS216 FR-LS217 FR-LS218 FR-LS219 FR-LS220 FR-LS224 FR-LS225 FR-LS226 FR-LS227 FR-LS228 FR-LS229 FR-LS230 FR-LS232 FR-LS252]: fit_listing_widths(): equivalence classes and boundaries of its arguments", {
  E <- .eb3_err
  O <- .eb3_obj
  spec <- listing_spec(list(listing_col(c("USUBJID", "AGE")), listing_col("AETERM")))
  tw <- "^`total_width` must be a single positive number of characters"
  mw <- "^`min_width` must be a single positive integer"
  pr <- "^`probs` must be a single number in \\[0, 1\\]"
  hl <- "^`header_lines` must be a single number >= 1, or Inf"
  fs <- "^`font_size_half_points` must be a single positive integer \\(half-points\\)"
  cm <- "^Could not measure the width of one character"
  fn <- "^`font` must be a single font family name"
  lb <- "^`labels` must be a named character vector"
  w  <- function(x) vapply(x$cols, function(cl) cl$width, 1L)
  adds <- function(n) function(x) sum(w(x)) + 1L == n
  wide <- function(x) all(w(x) >= 6L)
  wider <- function(x) sum(w(x)) > 124L      # 18 half-points on the default page: 125
  t60 <- list(total_width = 60)
  lab <- function(v) function(x) identical(x$cols[[1L]]$label, v)
  cases <- list(
    O("total_width", "60", 60, adds(60)),
    O("total_width", "20 (the least that fits two of 6)", 20, adds(20)),
    E("total_width", "1", 1, "^A total width of 1 character\\(s\\)"),
    E("total_width", "TRUE", TRUE, tw),
    E("total_width", "0", 0, tw),
    E("total_width", "negative", -1, tw),
    E("total_width", "NA", NA, tw),
    E("total_width", "Inf", Inf, tw),
    E("total_width", "length 2", c(60, 70), tw),
    E("total_width", "a fraction", 100.7, tw),
    E("total_width", "a string", "100", tw),
    O("min_width", "1 (min)", 1, adds(60), more = t60),
    O("min_width", "6 (default)", 6, wide, more = t60),
    E("min_width", "0", 0, mw, more = t60),
    E("min_width", "NA", NA, mw, more = t60),
    E("min_width", "Inf", Inf, mw, more = t60),
    E("min_width", "length 2", c(6, 7), mw, more = t60),
    E("min_width", "a fraction", 6.5, mw, more = t60),
    E("min_width", "a string", "6", mw, more = t60),
    E("min_width", "TRUE", TRUE, mw, more = t60),
    E("min_width", "30 (too wide for 60)", 30, "^A total width of 60", more = t60),
    O("probs", "0 (min)", 0, adds(60), more = t60),
    O("probs", "0.5", 0.5, adds(60), more = t60),
    O("probs", "1 (max)", 1, adds(60), more = t60),
    E("probs", "just below 0", -0.1, pr, more = t60),
    E("probs", "just above 1", 1.1, pr, more = t60),
    E("probs", "NA", NA, pr, more = t60),
    E("probs", "a string", "0.9", pr, more = t60),
    E("probs", "length 2", c(0.5, 0.9), pr, more = t60),
    O("header_lines", "1 (min)", 1, adds(60), more = t60),
    O("header_lines", "a fraction", 2.5, adds(60), more = t60),
    O("header_lines", "Inf", Inf, adds(60), more = t60),
    E("header_lines", "0", 0, hl, more = t60),
    E("header_lines", "just below 1", 0.5, hl, more = t60),
    E("header_lines", "negative", -1, hl, more = t60),
    E("header_lines", "NA", NA, hl, more = t60),
    E("header_lines", "a string", "4", hl, more = t60),
    O("font_size_half_points", "16", 16L, wider),
    O("font_size_half_points", "1 (min)", 1, wider),
    E("font_size_half_points", "0", 0, cm),
    E("font_size_half_points", "negative", -1, cm),
    E("font_size_half_points", "NA", NA, cm),
    E("font_size_half_points", "Inf", Inf, cm),
    E("font_size_half_points", "a fraction", 17.5, fs),
    E("font_size_half_points", "a string", "18", fs),
    E("font_size_half_points", "TRUE", TRUE, fs),
    E("font_size_half_points", "length 2", c(16, 18), fs),
    E("font_size_half_points", "NULL", NULL, fs),
    O("font", "valid", "Arial", function(x) length(w(x)) == 2L),
    O("font", "named slots", c(latin = "Arial", east_asian = "MS Mincho"), function(x) length(w(x)) == 2L),
    E("font", "empty string", "", fn),
    E("font", "NA", NA_character_, fn),
    E("font", "a number", 1, fn),
    E("font", "unnamed length 2", c("Arial", "Courier New"), fn),
    O("page", "rtf_page()", rtf_page(orientation = "portrait"), function(x) sum(w(x)) < 124L),
    O("page", "an empty list", list(), function(x) length(w(x)) == 2L),
    E("page", "a string", "a4", "^`page` must be an rtf_page\\(\\) object"),
    E("page", "a number", 1, "^`page` must be an rtf_page\\(\\) object"),
    O("labels", "named", c(USUBJID = "Subject"), lab("Subject/AGE"), more = t60),
    O("labels", "a list", list(USUBJID = "S"), lab("S/AGE"), more = t60),
    O("labels", "an unknown name", c(NOPE = "x"), lab("USUBJID/AGE"), more = t60),
    E("labels", "unnamed", "Subject", lb, more = t60),
    E("labels", "NA name", setNames("x", NA), lb, more = t60),
    E("labels", "a number", 1, lb, more = t60),
    E("labels", "length 0", character(0), lb, more = t60),
    O("data", "0 rows", .eb3_ae[0, ], adds(60), more = t60)
  )
  .eb3_run(cases, function(args, obj) {
    if (!"data" %in% names(args)) args <- c(list(data = .eb3_ae), args)
    do.call(fit_listing_widths, c(args, list(spec = spec)))
  })
})

test_that("FT-LS158 [FR-LS170 FR-LS172 FR-LS174 FR-LS175 FR-LS176 FR-LS177 FR-LS178 FR-LS179 FR-LS180 FR-LS181]: listing_wrap(): equivalence classes and boundaries of its arguments", {
  E <- .eb3_err
  O <- .eb3_obj
  wd <- "^`width` must be a single positive number, or NULL"
  sp <- "^`sep` must be a single string, or NULL"
  is <- function(v) function(x) identical(x, v)
  txt <- list(text = "Nausea/and vomiting")
  cases <- list(
    O("width", "1 (min)", 1, function(x) all(nchar(x) == 1L) && length(x) == 18L, more = txt),
    O("width", "5", 5, is(c("Nause", "a/", "and", "vomit", "ing")), more = txt),
    O("width", "a fraction (display width)", 1.5, function(x) all(nchar(x) == 1L), more = txt),
    O("width", "Inf (no limit)", Inf, is(c("Nausea/", "and vomiting")), more = txt),
    O("width", "NULL (no limit)", NULL, is("Nausea/and vomiting"), more = txt),
    E("width", "0", 0, wd, more = txt),
    E("width", "negative", -1, wd, more = txt),
    E("width", "NA", NA, wd, more = txt),
    E("width", "a string", "5", wd, more = txt),
    E("width", "length 2", c(5, 6), wd, more = txt),
    E("width", "TRUE", TRUE, wd, more = txt),
    O("sep", "valid", "/", is(c("Nausea/", "and", "vomiting")), more = c(txt, width = 8)),
    O("sep", "empty string", "", is(c("Nausea/a", "nd", "vomiting")), more = c(txt, width = 8)),
    O("sep", "NULL", NULL, is(c("Nausea/a", "nd", "vomiting")), more = c(txt, width = 8)),
    E("sep", "NA", NA_character_, sp, more = c(txt, width = 8)),
    E("sep", "a number", 1, sp, more = c(txt, width = 8)),
    E("sep", "length 2", c("/", "-"), sp, more = c(txt, width = 8)),
    O("layout", "flow", "flow", is("a/b/c"), more = list(text = "a/b/c", width = 5)),
    O("layout", "prefix", "fl", is("a/b/c"), more = list(text = "a/b/c", width = 5)),
    O("layout", "stack (default)", "stack", is(c("a/", "b/", "c")), more = list(text = "a/b/c", width = 5)),
    E("layout", "unknown", "bogus", "^'arg' should be one of", more = list(text = "a/b/c", width = 5)),
    E("layout", "NA", NA, "^'arg' must be NULL or a character vector", more = list(text = "a/b/c", width = 5)),
    O("text", "empty string", "", is(""), more = list(width = 4)),
    O("text", "NA", NA_character_, is(""), more = list(width = 4)),
    O("text", "length 0", character(0), is(list()), more = list(width = 4)),
    O("text", "n", c("a b c", "dd"), is(list(c("a b", "c"), "dd")), more = list(width = 4)),
    O("text", "non-ASCII (two columns a glyph)", "試験の/頭痛", is(c("試験", "の/", "頭痛")), more = list(width = 4)),
    O("text", "long", strrep("x", 20), is(rep("xxxx", 5)), more = list(width = 4))
  )
  .eb3_run(cases, function(args, obj) do.call(listing_wrap, args))
})

test_that("FT-LS159 [FR-LS233 FR-LS234 FR-LS235 FR-LS236 FR-LS238 FR-LS239]: collapse_repeats(): equivalence classes and boundaries of its arguments", {
  E <- .eb3_err
  O <- .eb3_obj
  tb <- rtftable(data.frame(A = c("x", "x", "y"), B = c("1", "1", "2")))
  a_is <- function(v) function(x) identical(x$data$A, v)
  blank <- a_is(c("x", NA, "y"))
  rg <- "^`collapse_repeats` index"
  cases <- list(
    O("cols", "a name", "A", blank),
    O("cols", "a position", 1L, blank),
    O("cols", "a list of name and position", list("A", 2L), function(x) {
      identical(x$data$A, c("x", NA, "y")) && identical(x$data$B, c("1", NA, "2")) }),
    O("cols", "two names", c("A", "B"), blank),
    O("cols", "length 0", character(0), a_is(c("x", "x", "y"))),
    O("cols", "NULL", NULL, a_is(c("x", "x", "y"))),
    E("cols", "0", 0, rg),
    E("cols", "n + 1", 3, rg),
    E("cols", "negative", -1, rg),
    E("cols", "NA", NA, rg),
    E("cols", "unknown", "NOPE", "^`collapse_repeats` column 'NOPE' not found"),
    E("cols", "another case", "a", "^`collapse_repeats` column 'a' not found"),
    O("x", "a list of pages", list(tb, tb), function(x) length(x) == 2L && identical(x[[2L]]$data$A, c("x", NA, "y"))),
    O("x", "an empty list", list(), function(x) identical(x, list())),
    E("x", "a data.frame", data.frame(A = 1), "^`collapse_repeats\\(\\)` expects an rtftable"),
    E("x", "NULL", NULL, "^`collapse_repeats\\(\\)` expects an rtftable")
  )
  .eb3_run(cases, function(args, obj) {
    if (!"x" %in% names(args)) args <- c(list(x = tb), args)
    if (!"cols" %in% names(args)) args$cols <- "A"
    do.call(collapse_repeats, args)
  })
})

# ---- the ARD helpers -----------------------------------------------------------

.eb3_r <- function(g1, g1l, var, lvl, ctx, sn, st) {
  data.frame(group1 = g1, group1_level = g1l, variable = var,
             variable_level = lvl, context = ctx, stat_name = sn,
             stat_label = sn, stat = st, stringsAsFactors = FALSE)
}
# Drug A: F 3 / M 2 of 5; Placebo: F 4 of 4; the arms' own counts; total 9.
.eb3_ard <- rbind(
  .eb3_r("TRT", "Drug A", "SEX", "F", "tabulate", "n", 3),
  .eb3_r("TRT", "Drug A", "SEX", "M", "tabulate", "n", 2),
  .eb3_r("TRT", "Placebo", "SEX", "F", "tabulate", "n", 4),
  .eb3_r("TRT", "Drug A", "SEX", "F", "tabulate", "N", 5),
  .eb3_r("TRT", "Drug A", "SEX", "M", "tabulate", "N", 5),
  .eb3_r("TRT", "Placebo", "SEX", "F", "tabulate", "N", 4),
  .eb3_r(NA, NA, "TRT", "Drug A", "tabulate", "n", 5),
  .eb3_r(NA, NA, "TRT", "Placebo", "tabulate", "n", 4),
  .eb3_r(NA, NA, "..ard_total_n..", NA, "total_n", "N", 9))

test_that("FT-AR107 [FR-AR119 FR-AR121 FR-AR126]: normalize_ard(): equivalence classes and boundaries of drop_contexts and stack_sep", {
  O <- .eb3_obj
  kept <- function(n, tot) function(x) nrow(x) == n && identical(attr(x, "ard_total_n"), tot)
  # A crossed layer: SEX in group2, AGEGR in group3.
  cr <- .eb3_ard[.eb3_ard$variable == "SEX" & .eb3_ard$stat_name == "n", ]
  cr$group2 <- "SEX"
  cr$group2_level <- cr$variable_level
  cr$group3 <- "AGEGR"
  cr$group3_level <- "<65"
  cr$variable <- "AESER"
  cr$variable_level <- "Y"
  st <- function(sep) function(x) {
    identical(unique(as.character(x$sg)), paste0("SEX", sep, "AGEGR")) &&
      paste0("F", sep, "<65") %in% as.character(x$sg_level)
  }
  sx <- list(.x = cr, stack = list(sg = list(c("SEX", "AGEGR"))))
  cases <- list(
    O("drop_contexts", "default", c("attributes", "total_n"), kept(8, 9)),
    O("drop_contexts", "length 0", character(0), kept(9, NULL)),
    O("drop_contexts", "NULL", NULL, kept(9, NULL)),
    O("drop_contexts", "one context", "tabulate", kept(1, NULL)),
    O("drop_contexts", "unknown", "bogus", kept(9, NULL)),
    O("drop_contexts", "another case", "TOTAL_N", kept(9, NULL)),
    O("drop_contexts", "NA", NA_character_, kept(9, NULL)),
    O("drop_contexts", "empty string", "", kept(9, NULL)),
    O("stack_sep", "default", " / ", st(" / "), more = sx),
    O("stack_sep", "empty string", "", st(""), more = sx),
    O("stack_sep", "valid", "-", st("-"), more = sx),
    O("stack_sep", "non-ASCII", "／", st("／"), more = sx),
    O("stack_sep", "long", strrep("-", 50), st(strrep("-", 50)), more = sx)
  )
  .eb3_run(cases, function(args, obj) {
    x <- args$.x %||% .eb3_ard
    args$.x <- NULL
    do.call(normalize_ard, c(list(x), args))
  })
})

test_that("FT-AR108 [FR-AR156 FR-AR157 FR-AR158 FR-AR180 FR-AR183 FR-AR197 FR-AR209 FR-AR247 FR-AR250]: widen_ard(): equivalence classes and boundaries of na, label and stat_groups", {
  E <- .eb3_err
  O <- .eb3_obj
  d <- normalize_ard(.eb3_ard)
  na <- "^`na` is one text"
  sg <- "^`stat_groups`"
  empty <- function(v) function(x) identical(x[["Placebo"]][x[[1L]] == "M"], v)
  hd <- function(v) function(x) identical(names(x), v)
  cases <- list(
    O("na", "NA (default)", NA_character_, empty(NA_character_)),
    O("na", "empty string", "", empty("")),
    O("na", "valid", "-", empty("-")),
    O("na", "non-ASCII", "該当なし", empty("該当なし")),
    O("na", "a logical NA", NA, empty(NA_character_)),
    E("na", "length 2", c("a", "b"), na),
    E("na", "length 0", character(0), na),
    E("na", "NULL", NULL, na),
    E("na", "a number", 0, na),
    O("label", "default", ".label", hd(c("label", "Drug A", "Placebo"))),
    O("label", "a column", "variable_level", hd(c("variable_level", "Drug A", "Placebo"))),
    O("label", "named", c(Sex = ".label"), hd(c("Sex", "Drug A", "Placebo"))),
    O("label", "NULL", NULL, hd(c("Drug A", "Placebo"))),
    O("label", "NA", NA, hd(c("Drug A", "Placebo"))),
    O("label", "length 0", character(0), hd(c("Drug A", "Placebo"))),
    O("label", "a formula", ~ "x{.label}", function(x) identical(x$label, c("xF", "xM"))),
    E("label", "unknown", "nope", "^`label`: no column 'nope'"),
    E("label", "empty string", "", "^`label`: no column ''"),
    E("label", "a number", 1, "^`label`: no column '1'"),
    E("label", "length 2", c(".label", "variable"), "^`label` is one column"),
    O("stat_groups", "one group", list(cnt = "n"), function(x) nrow(x) == 2L),
    O("stat_groups", "length 0", list(), function(x) nrow(x) == 2L),
    E("stat_groups", "unnamed", list("n"), sg),
    E("stat_groups", "a string", "n", sg),
    E("stat_groups", "NA", NA, sg),
    E("stat_groups", "a number", 1, sg),
    E("stat_groups", "a statistic twice", list(a = "n", b = "n"), "^`stat_groups`: 'n' is in two groups"),
    E("stat_groups", "an empty group", list(n = character(0)), "^`stat_groups`: 'n' names no statistic")
  )
  .eb3_run(cases, function(args, obj) {
    do.call(widen_ard, c(list(d, cols = "TRT", cells = "{n}", notes = FALSE), args))
  })
})

test_that("FT-AR109 [FR-AR213 FR-AR215 FR-AR216 FR-AR218 FR-AR219 FR-AR220 FR-AR221 FR-AR224 FR-AR225 FR-AR239 FR-AR251]: pull_ard(): equivalence classes and boundaries of its arguments", {
  E <- .eb3_err
  O <- .eb3_obj
  is <- function(v) function(x) identical(x, v)
  n54 <- c("Drug A" = 5, Placebo = 4)
  nk <- "^`cols`: no key"
  cl <- "^`cols` names one or more column keys"
  stt <- "^`stat` is one statistic's name"
  sp <- "^`sep` is one text"
  none <- "^No 'N' found"
  cases <- list(
    O("cols", "valid", "TRT", is(n54)),
    O("cols", "twice", c("TRT", "TRT"), is(c("Drug A____Drug A" = 5, "Placebo____Placebo" = 4))),
    E("cols", "unknown", "nope", nk),
    E("cols", "NA", NA_character_, nk),
    E("cols", "length 0", character(0), cl),
    E("cols", "a number", 1, cl),
    O("stat", "N (default)", "N", is(n54)),
    O("stat", "n (the largest of the levels)", "n", is(c("Drug A" = 3, Placebo = 4))),
    E("stat", "unknown", "nope", "^No 'nope' found"),
    E("stat", "empty string", "", "^No '' found"),
    E("stat", "NA", NA_character_, stt),
    E("stat", "length 2", c("N", "n"), stt),
    E("stat", "a number", 1, stt),
    O("variable", "the analysis variable", "SEX", is(n54)),
    E("variable", "the key (its own rows have no N)", "TRT", none),
    E("variable", "unknown", "nope", none),
    E("variable", "NA", NA, none),
    O("context", "valid", "tabulate", is(n54)),
    E("context", "unknown", "nope", none),
    O("sep", "valid", "-", is(n54)),
    O("sep", "empty string", "", is(n54)),
    E("sep", "NA", NA_character_, sp),
    E("sep", "length 2", c("a", "b"), sp),
    E("sep", "a number", 1, sp),
    O("levels", "a named list", list(TRT = c("Placebo", "Drug A")), is(c(Placebo = 4, "Drug A" = 5))),
    O("levels", "a level not there", list(TRT = c("nope", "Placebo")), is(c(Placebo = 4, "Drug A" = 5))),
    O("levels", "a plain vector (not read)", c("Placebo", "Drug A"), is(n54)),
    O("levels", "NA", NA, is(n54))
  )
  .eb3_run(cases, function(args, obj) {
    if (!"cols" %in% names(args)) args$cols <- "TRT"
    do.call(pull_ard, c(list(.eb3_ard), args))
  })
})

# ---- the number formatters -----------------------------------------------------

.eb3_x <- c(0.5, 1.5, 2.5, -0.5, -1.5, -2.5, 0, -0)

test_that("FT-FM102 [FR-FM114 FR-FM115 FR-FM116 FR-FM117 FR-FM118 FR-FM121 FR-FM122 FR-FM124 FR-FM125 FR-FM126 FR-FM127 FR-FM128 FR-FM251 FR-FM252 FR-FM253]: fmt_round(): equivalence classes and boundaries of its arguments, the rounding boundaries", {
  E <- .eb3_err
  O <- .eb3_obj
  is <- function(v) function(x) identical(x, v)
  dg <- "^`fmt_round\\(digits = \\)` must be a single non-negative integer"
  rd <- "^`rounding` must be \"r\""
  na <- "^`na` must be a single string"
  nt <- "^`fmt_round\\(\\)` formats numeric input"
  r0 <- list(digits = 0, rounding = "r")
  s0 <- list(digits = 0, rounding = "sas")
  cases <- list(
    O("x", "halves, r", .eb3_x, is(c("0", "2", "2", "0", "-2", "-2", "0", "0")), more = r0),
    O("x", "halves, sas", .eb3_x, is(c("1", "2", "3", "-1", "-2", "-3", "0", "0")), more = s0),
    O("x", "a half at 2 digits", c(0.125, 2.675, -2.675, 23.445), is(c("0.12", "2.67", "-2.67", "23.44")),
      more = list(rounding = "r")),
    O("x", "a half at 2 digits, sas", c(0.125, 2.675, -2.675, 23.445), is(c("0.13", "2.68", "-2.68", "23.45")),
      more = list(rounding = "sas")),
    O("x", "very large, r", c(1e15, 123456789.125), is(c("1000000000000000", "123456789")), more = r0),
    O("x", "very large, sas", c(1e15, 123456789.125), is(c("1000000000000000", "123456789")), more = s0),
    O("x", "very large at 2, sas", 123456789.125, is("123456789.13"), more = list(rounding = "sas")),
    O("x", "very small", c(1e-15, -1e-300), is(c("0.00", "0.00")), more = list(rounding = "sas")),
    O("x", "just below a half, sas", 1.00499999, is("1.00"), more = list(rounding = "sas")),
    O("x", "just below a half, r", 1.00499999, is("1.00"), more = list(rounding = "r")),
    O("x", "NA / NaN / Inf / -Inf", c(NA, NaN, Inf, -Inf), is(c("", "", "Inf", "-Inf"))),
    O("x", "length 0", numeric(0), is(character(0))),
    O("x", "integer", 1L, is("1.00")),
    E("x", "text", "1", nt),
    E("x", "logical", TRUE, nt),
    E("x", "factor", factor(1), nt),
    E("x", "NULL", NULL, nt),
    E("x", "a list", list(1), nt),
    O("digits", "0 (min)", 0, is("1"), more = list(x = 1.23456)),
    O("digits", "15", 15, is("1.234560000000000"), more = list(x = 1.23456)),
    O("digits", "22", 22, function(x) nchar(sub(".*\\.", "", x)) == 22L, more = list(x = 1.23456)),
    E("digits", "negative", -1, dg),
    E("digits", "a fraction", 1.5, dg),
    E("digits", "NA", NA, dg),
    E("digits", "Inf", Inf, dg),
    E("digits", "length 2", c(1, 2), dg),
    E("digits", "length 0", numeric(0), dg),
    E("digits", "NULL", NULL, dg),
    E("digits", "a string", "2", dg),
    E("digits", "TRUE", TRUE, dg),
    O("rounding", "r", "r", is("0.12"), more = list(x = 0.125)),
    O("rounding", "sas", "sas", is("0.13"), more = list(x = 0.125)),
    O("rounding", "another case", "SAS", is("0.13"), more = list(x = 0.125)),
    E("rounding", "prefix", "s", rd),
    E("rounding", "unknown", "bogus", rd),
    E("rounding", "NA", NA, rd),
    E("rounding", "a number", 1, rd),
    E("rounding", "length 2", c("r", "sas"), rd),
    E("rounding", "empty string", "", rd),
    O("na", "valid", "-", is(c("1.00", "-", "-")), more = list(x = c(1, NA, NaN))),
    O("na", "empty string", "", is(c("1.00", "", "")), more = list(x = c(1, NA, NaN))),
    O("na", "non-ASCII", "欠測", is(c("1.00", "欠測", "欠測")), more = list(x = c(1, NA, NaN))),
    O("na", "NULL", NULL, is(c("1.00", "", "")), more = list(x = c(1, NA, NaN))),
    E("na", "NA", NA, na, more = list(x = c(1, NA))),
    E("na", "NA_character_", NA_character_, na, more = list(x = c(1, NA))),
    E("na", "a number", 1, na, more = list(x = c(1, NA))),
    E("na", "length 2", c("a", "b"), na, more = list(x = c(1, NA))),
    E("na", "length 0", character(0), na, more = list(x = c(1, NA)))
  )
  .eb3_run(cases, function(args, obj) {
    if (!"x" %in% names(args)) args <- c(list(x = 1), args)
    do.call(fmt_round, args)
  })
})

test_that("FT-FM103 [FR-FM129 FR-FM131 FR-FM135 FR-FM136 FR-FM137 FR-FM138 FR-FM139 FR-FM141 FR-FM142 FR-FM145 FR-FM146 FR-FM147 FR-FM251 FR-FM252 FR-FM253]: fmt_signif(): equivalence classes and boundaries of its arguments, the rounding boundaries", {
  E <- .eb3_err
  O <- .eb3_obj
  is <- function(v) function(x) identical(x, v)
  dg <- "^`fmt_signif\\(digits = \\)` must be a single positive integer"
  na <- "^`na` must be a single string"
  r2 <- list(digits = 2, rounding = "r")
  s2 <- list(digits = 2, rounding = "sas")
  cases <- list(
    O("x", "halves, r", .eb3_x, is(c("0.50", "1.5", "2.5", "-0.50", "-1.5", "-2.5", "0.0", "0.0")), more = r2),
    O("x", "x.x5, r / sas", 0.125, is("0.12"), more = r2),
    O("x", "x.x5, sas", 0.125, is("0.13"), more = s2),
    O("x", "very large, sas", c(1e15, 123456789.125), is(c("1000000000000000", "123456789")), more = s2),
    O("x", "very small", 1e-15, is("0.0000000000000010"), more = s2),
    O("x", "very small, fixed", 1e-15, is("0.0"), more = c(s2, small = "fixed")),
    O("x", "just below a half, sas", 1.00499999, is("1.00"), more = list(digits = 3, rounding = "sas")),
    O("x", "NA / NaN / Inf", c(NA, NaN, Inf, -Inf), is(c("-", "-", "Inf", "-Inf")), more = list(na = "-")),
    E("x", "text", "1", "^`fmt_signif\\(\\)` formats numeric input"),
    O("digits", "1", 1, is(c("123", "0.01")), more = list(x = c(123.456, 0.012345))),
    O("digits", "22", 22, function(x) length(x) == 2L, more = list(x = c(123.456, 0.012345))),
    E("digits", "negative", -1, dg),
    E("digits", "a fraction", 1.5, dg),
    E("digits", "NA", NA, dg),
    E("digits", "Inf", Inf, dg),
    E("digits", "length 2", c(1, 2), dg),
    E("digits", "a string", "2", dg),
    E("digits", "TRUE", TRUE, dg),
    O("small", "fixed", "fixed", is("0.0"), more = list(x = 0.012345, digits = 2)),
    O("small", "prefix", "fix", is("0.0"), more = list(x = 0.012345, digits = 2)),
    E("small", "another case", "Fixed", "^'arg' should be one of"),
    E("small", "NA", NA, "^'arg' must be NULL or a character vector"),
    E("small", "a number", 1, "^'arg' must be NULL or a character vector"),
    O("na", "NULL", NULL, is(c("1.00", "")), more = list(x = c(1, NA))),
    E("na", "NA", NA, na, more = list(x = c(1, NA))),
    E("na", "a number", 1, na, more = list(x = c(1, NA))),
    E("na", "length 2", c("a", "b"), na, more = list(x = c(1, NA)))
  )
  .eb3_run(cases, function(args, obj) {
    if (!"x" %in% names(args)) args <- c(list(x = 1), args)
    do.call(fmt_signif, args)
  })
})

test_that("FT-FM104 [FR-FM149 FR-FM151 FR-FM152 FR-FM153 FR-FM154 FR-FM155 FR-FM162 FR-FM164 FR-FM165 FR-FM167 FR-FM172 FR-FM173 FR-FM252 FR-FM253]: fmt_numeric(): equivalence classes and boundaries of its arguments", {
  E <- .eb3_err
  O <- .eb3_obj
  df <- data.frame(g = c("Mean", "SD", "x"), v = c(1.25, 2.35, NA), s = c("a", "b", "c"))
  v_is <- function(v) function(x) identical(x$v, v)
  dg <- "^`fmt_numeric\\(digits = \\)` must be a single non-negative integer"
  sg <- "^`fmt_numeric\\(signif = \\)` must be a single positive integer"
  one <- "^Give exactly one of `signif` or `digits`"
  na <- "^`na` must be a single string"
  d1 <- list(digits = 1)
  cases <- list(
    O("digits", "1", 1, v_is(c("1.2", "2.4", ""))),
    O("signif", "2", 2, v_is(c("1.2", "2.4", ""))),
    E("signif", "both", 2, one, more = d1),
    E("digits", "neither", NULL, one),
    E("digits", "negative", -1, dg),
    E("digits", "a fraction", 1.5, dg),
    E("digits", "NA", NA, dg),
    E("digits", "a string", "1", dg),
    E("digits", "TRUE", TRUE, dg),
    E("signif", "a string", "2", sg),
    O("cols", "a name", "v", v_is(c("1.2", "2.4", "")), more = d1),
    O("cols", "a position", 2, v_is(c("1.2", "2.4", "")), more = d1),
    O("cols", "two, one not numeric", c("v", "s"), function(x) identical(x$s, df$s), more = d1),
    O("cols", "length 0", character(0), v_is(df$v), more = d1),
    E("cols", "unknown", "nope", "^`fmt_numeric\\(cols\\)` column 'nope' not found", more = d1),
    E("cols", "0", 0, "^`fmt_numeric\\(cols\\)` index 0 out of range", more = d1),
    E("cols", "n + 1", 4, "^`fmt_numeric\\(cols\\)` index 4 out of range", more = d1),
    E("cols", "NA", NA, "^`fmt_numeric\\(cols\\)` index NA out of range", more = d1),
    O("by", "valid", "g", v_is(c("1.2", "2.35", "")),
      more = list(formats = list(Mean = list(digits = 1), .default = list(digits = 2)))),
    E("by", "no formats", "g", "^`fmt_numeric\\(by = \\)` needs a named `formats` list"),
    E("formats", "without by", list(Mean = list(digits = 1)), one),
    O("na", "valid", "-", v_is(c("1.2", "2.4", "-")), more = d1),
    E("na", "NA", NA, na, more = d1),
    E("na", "a number", 1, na, more = d1),
    E("na", "length 2", c("a", "b"), na, more = d1),
    O("data", "0 rows", df[0, ], function(x) nrow(x) == 0L && identical(names(x), names(df)), more = d1),
    E("data", "a list", as.list(df), "^`fmt_numeric\\(\\)` needs a data frame", more = d1)
  )
  .eb3_run(cases, function(args, obj) {
    if (!"data" %in% names(args)) args <- c(list(data = df), args)
    if (!"cols" %in% names(args)) args$cols <- "v"
    do.call(fmt_numeric, args)
  })
})

test_that("FT-FM105 [FR-FM175 FR-FM176 FR-FM177 FR-FM178 FR-FM183 FR-FM184 FR-FM185 FR-FM186 FR-FM187 FR-FM189 FR-FM190 FR-FM191 FR-FM192 FR-FM193 FR-FM194 FR-FM195 FR-FM196 FR-FM249 FR-FM254 FR-FM255]: fmt_count_pct(): equivalence classes and boundaries of its arguments, the rounding boundaries", {
  E <- .eb3_err
  O <- .eb3_obj
  C <- .eb3_case
  is <- function(v) function(x) identical(x, v)
  sp <- list(nbsp = " ")
  nb <- "^`nbsp` must be a single string"
  ps <- "^`pct_sign` must be TRUE or FALSE"
  na <- "^`na` must be a single string"
  ma <- "^'arg' should be one of"
  mn <- "^'arg' must be NULL or a character vector"
  rd <- "^`rounding` must be \"r\""
  cases <- list(
    O("count", "0 / 1 / 10 / 100 / NA / NaN / Inf", c(0, 1, 10, 100, NA, NaN, Inf),
      is(c("  0       ", "  1  (6.2)", " 10 (50.0)", "100  (100)", "", "", "Inf       ")),
      more = c(sp, list(pct = c(0, 0.0625, 0.5, 1, 0.5, 0.5, 0.5), rounding = "r"))),
    O("pct", "x.x5, sas", 0.0625, is("  1  (6.3)"), more = c(sp, count = 1, rounding = "sas")),
    O("pct", "99.95 rounds into the 100 branch", 0.9995, is("1999  (100)"), more = c(sp, count = 1999, rounding = "sas")),
    O("pct", "0.05 rounds to 0.0 (r) / 0.1 (sas)", 0.0005, is("  1  (0.0)"), more = c(sp, count = 1, rounding = "r")),
    O("pct", "a bare NA", NA, is("  1       "), more = c(sp, count = 1)),
    O("count", "a bare NA", NA, is("  -       "), more = c(sp, pct = 0.5, na = "-")),
    O("count", "large", 123456, is("123456 (50.0)"), more = c(sp, pct = 0.5)),
    E("count", "a fraction", 1.5, "^`count` must be whole numbers", more = list(pct = 0.1)),
    E("count", "text", "1", "^`count` and `pct` must both be numeric", more = list(pct = 0.1)),
    O("count", "length 0", numeric(0), is(character(0)), more = list(pct = numeric(0))),
    E("count", "mismatched length", c(1, 2, 3), "^`count` and `pct` must have the same length",
      more = list(pct = c(0.1, 0.2))),
    C("pct", "above 1 as a fraction", 5, "warning:`pct` has values above 1", more = c(sp, count = 1)),
    O("pct_unit", "percent", "percent", is("  1 (50.0)"), more = c(sp, count = 1, pct = 50)),
    O("pct_unit", "prefix", "per", is("  1 (50.0)"), more = c(sp, count = 1, pct = 50)),
    E("pct_unit", "another case", "Percent", ma, more = list(count = 1, pct = 50)),
    E("pct_unit", "NA", NA, mn, more = list(count = 1, pct = 50)),
    E("pct_unit", "a number", 1, mn, more = list(count = 1, pct = 50)),
    O("nbsp", "a space", " ", is(c("  1 (50.0)", " 10 (50.0)")), more = list(count = c(1, 10), pct = 0.5)),
    O("nbsp", "empty string", "", is(c("1(50.0)", "10(50.0)")), more = list(count = c(1, 10), pct = 0.5)),
    O("nbsp", "non-ASCII", "　", is("　　1　(50.0)"), more = list(count = 1, pct = 0.5)),
    E("nbsp", "NA", NA, nb, more = list(count = 1, pct = 0.5)),
    E("nbsp", "length 2", c("a", "b"), nb, more = list(count = 1, pct = 0.5)),
    E("nbsp", "a number", 1, nb, more = list(count = 1, pct = 0.5)),
    O("pct_sign", "TRUE", TRUE, is("  1 (50.0%)"), more = c(sp, count = 1, pct = 0.5)),
    E("pct_sign", "NA", NA, ps, more = list(count = 1, pct = 0.5)),
    E("pct_sign", "a string", "yes", ps, more = list(count = 1, pct = 0.5)),
    E("pct_sign", "a number", 1, ps, more = list(count = 1, pct = 0.5)),
    E("pct_sign", "length 2", c(TRUE, FALSE), ps, more = list(count = 1, pct = 0.5)),
    O("na", "valid", "NE", is(c(" NE       ", " 10 (50.0)")), more = c(sp, list(count = c(NA, 10), pct = 0.5))),
    O("na", "NULL", NULL, is(c("", " 10 (50.0)")), more = c(sp, list(count = c(NA, 10), pct = 0.5))),
    E("na", "NA", NA, na, more = list(count = NA_real_, pct = 0.5)),
    E("na", "a number", 1, na, more = list(count = NA_real_, pct = 0.5)),
    E("na", "length 2", c("a", "b"), na, more = list(count = NA_real_, pct = 0.5)),
    E("rounding", "unknown", "bogus", rd, more = list(count = 1, pct = 0.0625)),
    E("rounding", "NA", NA, rd, more = list(count = 1, pct = 0.0625))
  )
  .eb3_run(cases, function(args, obj) do.call(fmt_count_pct, args))
})

test_that("FT-FM106 [FR-FM228 FR-FM233 FR-FM235 FR-FM237 FR-FM238 FR-FM239 FR-FM248 FR-FM254]: fmt_value_paren(): equivalence classes and boundaries of its arguments", {
  E <- .eb3_err
  O <- .eb3_obj
  is <- function(v) function(x) identical(x, v)
  x2 <- list(x = c("12 (3.4)", "1 (0.5)"))
  nb <- "^`nbsp` must be a single string"
  na <- "^`na` must be a single string"
  rd <- "^`rounding` must be \"r\""
  cases <- list(
    O("x", "shapes", c("12 (3.4)", "1 (0.5)", "123", NA, "-1.5 (-0.25)"),
      is(c("  12   (3.4)", "   1   (0.5)", " 123        ", "", "-1.5 (-0.25)")), more = list(nbsp = " ")),
    O("x", "length 0", character(0), is(character(0))),
    O("nbsp", "a space", " ", is(c("12 (3.4)", " 1 (0.5)")), more = x2),
    O("nbsp", "empty string", "", is(c("12(3.4)", "1(0.5)")), more = x2),
    E("nbsp", "NA", NA, nb, more = x2),
    E("nbsp", "length 2", c("a", "b"), nb, more = x2),
    E("nbsp", "a number", 1, nb, more = x2),
    O("na", "valid", "-", is(c("12 (3.4)", " -      ")), more = list(x = c("12 (3.4)", NA), nbsp = " ")),
    O("na", "NULL", NULL, is(c("12 (3.4)", "")), more = list(x = c("12 (3.4)", NA), nbsp = " ")),
    E("na", "NA", NA, na, more = x2),
    E("na", "a number", 1, na, more = x2),
    E("na", "length 2", c("a", "b"), na, more = x2),
    O("rounding", "sas (a half up)", "sas", is("6 (101)"), more = list(x = "6 (100.5)", nbsp = " ")),
    O("rounding", "r (a half to even)", "r", is("6 (100)"), more = list(x = "6 (100.5)", nbsp = " ")),
    E("rounding", "unknown", "bogus", rd, more = x2),
    E("rounding", "NA", NA, rd, more = x2)
  )
  .eb3_run(cases, function(args, obj) do.call(fmt_value_paren, args))
})

# ---- the plan verbs ----------------------------------------------------------

# Age (Mean) and Sex (F, M) in two arms, with the records of no arm a Total
# column reads.
.eb3_st <- data.frame(
  ARM = c(rep(c("Placebo", "Active"), each = 3), NA, NA, NA),
  variable = rep(c("AGE", "SEX", "SEX"), 3),
  .label = rep(c(NA, "F", "M"), 3),
  .kind = rep(c("continuous", "categorical", "categorical"), 3),
  stat_name = rep(c("mean", "n", "n"), 3),
  stat = c(61.246, 12, 8, 63.51, 9, 11, 62, 21, 19))
.eb3_plan <- function() {
  table_plan(.eb3_st, cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(Mean = "{mean}"), categorical = "{n}", notes = FALSE) |>
    plan_labels(AGE = "Age", SEX = "Sex")
}
.eb3_pages <- function(verb, args) {
  plan_apply(do.call(verb, c(list(.eb3_plan()), args)), "pages")
}
.eb3_names <- function(v) function(p) identical(names(p[[1L]]$data), v)

test_that("FT-PL973 [FR-PL265 FR-PL275 FR-PL276]: plan_total(): equivalence classes and boundaries of label, position and direction", {
  E <- .eb3_err
  O <- .eb3_obj
  lb <- "^plan_total\\(label = \\) is one text, the column's heading"
  hd <- function(...) .eb3_names(c("group", "label", ...))
  cases <- list(
    O("label", "default", "Total", hd("Placebo", "Active", "Total")),
    O("label", "non-ASCII", "全体", hd("Placebo", "Active", "全体")),
    O("label", "long", strrep("T", 100), hd("Placebo", "Active", strrep("T", 100))),
    E("label", "empty string", "", lb),
    E("label", "NA", NA_character_, lb),
    E("label", "length 2", c("a", "b"), lb),
    E("label", "a number", 1, lb),
    E("label", "length 0", character(0), lb),
    E("label", "NULL", NULL, lb),
    O("position", "first", "first", hd("Total", "Placebo", "Active")),
    O("position", "prefix", "f", hd("Total", "Placebo", "Active")),
    E("position", "another case", "First", "^'arg' should be one of"),
    E("position", "unknown", "bogus", "^'arg' should be one of"),
    E("position", "NA", NA, "^'arg' must be NULL or a character vector"),
    E("position", "length 2", c("first", "last"), "^'arg' must be of length 1"),
    E("direction", "rows, no margins", "rows", "^plan_total\\(direction = \"rows\"\\): the ARD has no margins"),
    E("direction", "prefix", "r", "^plan_total\\(direction = \"rows\"\\)"),
    E("direction", "unknown", "bogus", "^'arg' should be one of"),
    E("direction", "NA", NA, "^'arg' must be NULL or a character vector")
  )
  .eb3_run(cases, function(args, obj) .eb3_pages(plan_total, args))
})

test_that("FT-PL974 [FR-PL310 FR-PL311 FR-PL960]: plan_sort(): equivalence classes and boundaries of stat and keep", {
  E <- .eb3_err
  O <- .eb3_obj
  kp <- "^plan_sort\\(keep = \\) is TRUE"
  sa <- "^plan_sort\\(stat = \\) is one statistic's name"
  g  <- list("group")
  n  <- list("-n")
  cases <- list(
    O("keep", "TRUE (default)", TRUE, .eb3_names(c("group", "label", "Placebo", "Active")), more = g),
    O("keep", "FALSE", FALSE, .eb3_names(c("label", "Placebo", "Active")), more = g),
    E("keep", "NA", NA, kp, more = g),
    E("keep", "a string", "yes", kp, more = g),
    E("keep", "a number", 1, kp, more = g),
    E("keep", "length 2", c(TRUE, TRUE), kp, more = g),
    O("stat", "valid", "n", function(p) identical(p[[1L]]$data[[2L]], c("Mean", "F", "M")), more = n),
    E("stat", "NA", NA_character_, sa, more = n),
    E("stat", "length 2", c("n", "mean"), sa, more = n),
    E("stat", "a number", 1, sa, more = n),
    E("stat", "empty string", "", sa, more = n)
  )
  .eb3_run(cases, function(args, obj) .eb3_pages(plan_sort, args))
})

test_that("FT-PL975 [FR-PL142 FR-PL143 FR-PL144 FR-PL145 FR-PL146 FR-PL150 FR-PL151 FR-PL152 FR-PL960]: plan_filter(): equivalence classes and boundaries of any, parents and by", {
  E <- .eb3_err
  O <- .eb3_obj
  an <- "^plan_filter\\(any = \\) is TRUE"
  by <- "^plan_filter\\(by = \\) is one row key's name"
  rows <- function(n) function(p) nrow(p[[1L]]$data) == n
  cases <- list(
    O("any", "TRUE (default)", TRUE, rows(3)),
    O("any", "FALSE", FALSE, rows(1)),
    E("any", "NA", NA, an),
    E("any", "a string", "yes", an),
    E("any", "a number", 1, an),
    O("parents", "drop", "drop", rows(3)),
    O("parents", "prefix", "d", rows(3)),
    E("parents", "unknown", "bogus", "^'arg' should be one of"),
    E("parents", "NA", NA, "^'arg' must be NULL or a character vector"),
    O("by", "a row key", "variable", rows(3)),
    E("by", "not a row key", "nope", "^plan_filter\\(by = \"nope\"\\): not a row key"),
    E("by", "NA", NA_character_, by),
    E("by", "a number", 1, by),
    E("by", "length 2", c("variable", "variable"), by)
  )
  .eb3_run(cases, function(args, obj) {
    plan_apply(do.call(plan_filter, c(list(.eb3_plan(), quote(n >= 10)), args)), "pages")
  })
})

test_that("FT-PL976 [FR-PL157 FR-PL158 FR-PL159 FR-PL160 FR-PL166 FR-PL167 FR-PL176 FR-PL179]: plan_columns(): equivalence classes and boundaries of widths, auto_width and sep", {
  E <- .eb3_err
  O <- .eb3_obj
  aw <- "^`auto_width` must be TRUE or FALSE"
  sp <- "^plan_columns\\(sep = \\) is one non-empty string"
  rw <- function(v) function(p) identical(p[[1L]]$col_rel_width, v)
  ok <- function(p) is.list(p) && length(p) == 1L
  cases <- list(
    O("widths", "n, in order", c(3, 1, 1, 1), rw(c(3, 1, 1, 1))),
    O("widths", "shorter (the last repeats)", c(3, 1), rw(c(3, 1, 1, 1))),
    O("widths", "by name", c(group = 2, label = 3, .values = 1), rw(c(2, 3, 1, 1))),
    E("widths", "a name not there", c(nope = 1), "^plan_columns\\(widths = \\) names 'nope'"),
    E("widths", "a printed column left out", c(group = 3, .values = 1), "^The `columns` sheet gives widths, but not for: 'label'"),
    O("auto_width", "TRUE", TRUE, ok),
    O("auto_width", "FALSE", FALSE, ok),
    E("auto_width", "NA", NA, aw),
    E("auto_width", "a string", "yes", aw),
    E("auto_width", "a number", 1, aw),
    O("sep", "valid", "__", .eb3_names(c("group", "label", "Placebo", "Active"))),
    O("sep", "non-ASCII", "／", .eb3_names(c("group", "label", "Placebo", "Active"))),
    E("sep", "empty string", "", sp),
    E("sep", "NA", NA_character_, sp),
    E("sep", "a number", 1, sp),
    E("sep", "length 2", c("a", "b"), sp)
  )
  .eb3_run(cases, function(args, obj) .eb3_pages(plan_columns, args))
})

test_that("FT-PL977 [FR-PL945]: plan_append(): equivalence classes and boundaries of name and absent", {
  E <- .eb3_err
  O <- .eb3_obj
  nm <- "^plan_append\\(name = \\) is one non-empty string"
  ten <- function(p) nrow(p[[1L]]$data) == 10L
  cases <- list(
    O("name", "valid", "Second", ten),
    O("name", "non-ASCII", "第二", ten),
    O("name", "NULL (default)", NULL, ten),
    E("name", "empty string", "", nm),
    E("name", "NA", NA_character_, nm),
    E("name", "a number", 1, nm),
    E("name", "length 2", c("a", "b"), nm),
    O("absent", "skip", "skip", ten),
    O("absent", "prefix", "s", ten),
    E("absent", "unknown", "bogus", "^'arg' should be one of"),
    E("absent", "NA", NA, "^'arg' must be NULL or a character vector")
  )
  .eb3_run(cases, function(args, obj) {
    plan_apply(do.call(plan_append, c(list(.eb3_plan(), .eb3_plan()), args)), "pages")
  })
})

test_that("FT-PL978 [FR-PL387 FR-PL388 FR-PL392]: plan_col_header(): equivalence classes and boundaries of span, header_sep and col_header_align", {
  E <- .eb3_err
  O <- .eb3_obj
  ln <- list(lines = list(c(group = "", label = "", .values = "Treatment")))
  ok <- function(p) is.list(p) && length(p) == 1L
  spn <- function(p) {
    h <- p[[1L]]$col_header[[1L]]
    is.list(h) && identical(h[[3L]][c("from", "to", "label")], list(from = 3L, to = 4L, label = "Treatment"))
  }
  sp <- "^plan_col_header\\(span = \\) is \"each\", \"one\" or a key's name"
  hs <- "^`header_sep` must be NULL or a character vector of separators"
  al <- "^`col_header_align` values must be \"left\", \"center\", or \"right\""
  cases <- list(
    O("span", "each (default)", "each", function(p) identical(p[[1L]]$col_header[[1L]], c("", "", "Treatment", "Treatment")), more = ln),
    O("span", "one", "one", spn, more = ln),
    O("span", "a key", "ARM", ok, more = ln),
    E("span", "not a key", "bogus", "^`col_header`: 'bogus' is not a column key", more = ln),
    E("span", "NA", NA, sp, more = ln),
    E("span", "a number", 1, sp, more = ln),
    O("header_sep", "valid", "__", ok),
    O("header_sep", "length 2", c("__", "--"), ok),
    E("header_sep", "NA", NA, hs),
    E("header_sep", "a number", 1, hs),
    O("col_header_align", "valid", "center", ok),
    E("col_header_align", "unknown", "bogus", al),
    E("col_header_align", "NA", NA, al),
    E("col_header_align", "wrong length", c("left", "center", "center"), "^`col_header_align` must have length 1 or 4")
  )
  .eb3_run(cases, function(args, obj) .eb3_pages(plan_col_header, args))
})

test_that("FT-PL979 [FR-PL256 FR-PL262]: plan_levels(): equivalence classes and boundaries of .drop_empty", {
  E <- .eb3_err
  O <- .eb3_obj
  de <- "^plan_levels\\(.drop_empty = \\) takes variable names"
  three <- function(p) identical(p[[1L]]$data[[2L]], c("Mean", "F", "M"))
  cases <- list(
    O(".drop_empty", "one variable (no empty level)", "SEX", three),
    O(".drop_empty", "n", c("SEX", "AGE"), three),
    O(".drop_empty", "NULL (default)", NULL, three),
    E(".drop_empty", "TRUE", TRUE, de),
    E(".drop_empty", "NA", NA, de),
    E(".drop_empty", "a number", 1, de)
  )
  .eb3_run(cases, function(args, obj) .eb3_pages(plan_levels, args))
})
