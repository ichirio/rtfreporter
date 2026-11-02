# Help-to-FR audit, batch 7 (second file): the listings (area LS) --
# listing_col(), listing_spec(), build_listing(), listing_wrap(),
# listing_wrap_code(), the measures (listing_disp_width(), listing_take(),
# listing_split_after()), fit_listing_widths(), listing_code(), catx() and
# collapse_repeats().  plan_listing() was audited with the plan verbs (batch
# 5, test-validation-audit-plan2.R).  The assembly and the figures are in
# test-validation-audit-assemble.R.
#
# The help is the functional requirement: every statement in a function's Rd
# is an active FR in inst/validation/requirements.csv, and every FR has a
# test.  For each function below, one row per claim of its roxygen: the
# claim (short), the FR that states it, and the test that checks it.  The
# tests below this table close the gaps the audit found.
#
# listing_col()
#   claim                                                   | FR          | test
#   one printed column; an rtf_listing_col                  | FR-LS142    | FT-LS020, FT-LS138
#   vars joined with sep, NA and "" skipped                 | FR-LS143    | FT-LS006, FT-LS119
#   sep: NULL takes the listing's "/"                       | FR-LS144    | FT-LS003
#   width: display width, wraps; NULL never wraps           | FR-LS145    | FT-LS074, FT-LS135
#   label: lines, a string with breaks, words to wrap, ""   | FR-LS146-148, FR-LS151 | FT-LS027, FT-LS034, FT-LS038-FT-LS041, FT-LS047, FT-LS056, FT-LS070
#   label NULL: label attribute, else name, or labels =;    | FR-LS149/150| FT-LS028, FT-LS031, FT-LS032, FT-LS042, FT-LS046, FT-LS061-FT-LS063, FT-LS065
#     follows the layout                                    |             |
#   layout stack / flow; alike with no width                | FR-LS155    | FT-LS024, FT-LS025, FT-LS049, FT-LS074
#   collapse_repeats: carried down, blanked per page; a     | FR-LS156/157| FT-LS026, FT-LS035-FT-LS037, FT-LS043, FT-LS044, FT-LS060
#     wrapped cell padded                                   |             |
#   name; rel_width (width, the label, 10); align           | FR-LS152-154| FT-LS006, FT-LS138, FT-TB221
#   the arguments refused                                   | FR-LS158-165| FT-LS021, FT-LS045, FT-LS048, FT-LS050, FT-LS138
#
# listing_spec()
#   the columns and the settings in one rtf_listing_spec    | FR-LS118    | FT-LS020, FT-LS140
#   cols: listing_col()s, or bare names; unique names       | FR-LS119/120| FT-LS021, FT-LS139
#   the "multiline" type's defaults; each argument over it  | FR-LS121-128| FT-LS003, FT-LS049, FT-LS134, FT-LS140, FT-LS145, FT-LS148, FT-TB221
#   wrap: the contract (four arguments, text, width, sep,   | FR-LS129/130, FR-LS117 | FT-LS029, FT-LS030, FT-LS064, FT-LS077, FT-LS078
#     layout, a non-empty value); listing_wrap the same     |             |
#   record TRUE / FALSE / a name; as_rtftables() splits on  | FR-LS131, FR-LS110 | FT-LS021, FT-LS140, FT-LS142, FT-TB218
#     it and drops it                                       |             |
#   no labels argument                                      | FR-LS132    | FT-LS102
#   the arguments refused                                   | FR-LS133-141| FT-LS021, FT-LS050, FT-LS078, FT-LS140
#
# build_listing()
#   the printed columns, gutters, the record column last    | FR-LS101/105/106 | FT-LS021, FT-LS123, FT-LS124, FT-LS132, FT-LS134, FT-LS135
#   joined, wrapped, padded to the tallest, a blank row     | FR-LS102-104| FT-LS003, FT-LS006, FT-LS119, FT-LS122, FT-LS125, FT-LS126, FT-LS134, FT-LS135
#   the rule: separator, word, hard split; a line break in  | FR-LS172-176| FT-LS120, FT-LS121, FT-LS127, FT-LS129-FT-LS131, FT-LS143, FT-LS144
#     the data first; display width                         |             |
#   labels resolved against the data; a factor's labels     | FR-LS107/108| FT-LS042, FT-LS065, FT-LS128, FT-LS132
#   the spec as attribute rtf_listing; the same as          | FR-LS110    | FT-LS142, FT-TB218
#     as_rtftables(listing = )                              |             |
#   zero rows; an rlistings listing; not a data.frame; not  | FR-LS109, FR-LS111-116 | FT-LS017, FT-LS132, FT-LS133, FT-LS136, FT-LS137, FT-LS146
#     a spec; built twice; vars missing; a record clash     |             |
#
# listing_wrap()
#   lines for one text, a list for several                  | FR-LS170/179| FT-LS055, FT-LS058
#   exactly the "multiline" rule                            | FR-LS171    | FT-LS077
#   separator, word boundary, hard split; every line fits   | FR-LS172-174| FT-LS121, FT-LS127, FT-LS129-FT-LS131
#   display width; a line break first; NA or ""             | FR-LS175/176| FT-LS120, FT-LS143, FT-LS144
#   layout; width NULL returns the text                     | FR-LS177/178| FT-LS025, FT-LS055, FT-LS058, FT-LS141
#   sep / width refused                                     | FR-LS180/181| FT-LS057
#
# listing_wrap_code()
#   the policy as source, rtf_listing_code, runs as it is   | FR-LS182/186| FT-LS066, FT-LS072
#   no rtfreporter:::; calls the exported measures          | FR-LS183    | FT-LS066, FT-LS067
#   name the entry, the helpers prefixed; a header          | FR-LS184/185| FT-LS068, FT-LS069, FT-LS071, FT-LS073
#   name refused                                            | FR-LS187/188| FT-LS059
#
# listing_disp_width(), listing_take(), listing_split_after()
#   display widths, a wide glyph two; NA 0; vectorised      | FR-LS199-201| FT-LS023, FT-LS052, FT-LS143
#   the longest prefix that fits; never "" for a non-empty  | FR-LS166-169| FT-LS054
#     text                                                  |             |
#   after each separator, kept; "" or NULL unsplit; literal | FR-LS202-204| FT-LS053, FT-LS130
#   ... any separator literally, "\E" too (code fixed)      | FR-LS249    | FT-LS152
#   one string (not NA), one width of 0 or more, else stop  | FR-LS248    | FT-LS151
#
# fit_listing_widths()
#   the same spec, a width on every column; the measurement | FR-LS205/211| FT-LS020, FT-LS080, FT-LS093, FT-LS094
#   the budget: page, font, size; gutters and set widths    | FR-LS206-208, FR-LS212-216 | FT-LS019, FT-LS081, FT-LS087, FT-LS088, FT-LS090, FT-LS091, FT-LS109, FT-LS110, FT-LS114, FT-LS118, FT-LS150
#     out first                                             |             |
#   ... 159 characters at 8pt, 142 at 9pt (help fixed)      | FR-LS250    | FT-LS153
#   the demand: quantile, header lines, token, min_width    | FR-LS209/210, FR-LS218-220 | FT-LS079, FT-LS083-FT-LS085, FT-LS095, FT-LS096, FT-LS098, FT-LS103, FT-LS104, FT-LS107
#   labels; their precedence                                | FR-LS217    | FT-LS082, FT-LS089, FT-LS100, FT-LS117
#   the arguments refused; a budget too small; no width     | FR-LS221-232| FT-LS019, FT-LS092, FT-LS097, FT-LS099, FT-LS108, FT-LS147
#
# listing_code()
#   the code as lines of class rtf_listing_code; printed    | FR-LS189    | FT-LS019, FT-LS112, FT-LS116
#   only what differs; a fitted spec in full                | FR-LS190-192| FT-LS086, FT-LS105, FT-LS113, FT-LS115
#   name; indent                                            | FR-LS193/194| FT-LS111, FT-LS149
#   a custom wrap commented                                 | FR-LS195    | FT-LS019, FT-LS051
#   the arguments refused                                   | FR-LS196-198| FT-LS101
#
# catx()
#   joined element by element; NA and "" left out           | FR-LS240/241| FT-LS007, FT-LS009
#   blanks stripped; recycled; a factor by its labels       | FR-LS242-244| FT-LS001, FT-LS004, FT-LS005
#   no values character(0); lengths refused; sep refused    | FR-LS245-247| FT-LS002, FT-LS008
#
# collapse_repeats()
#   repeats NA, the first of each run kept, no row removed  | FR-LS233    | FT-LS010, FT-TB106
#   hierarchical; names and positions                       | FR-LS234/235| FT-LS012, FT-LS014, FT-LS016, FT-TB099, FT-TB105
#   a list of pages, each page afresh; as_rtftables(        | FR-LS236/237| FT-LS013, FT-LS015, FT-LS018, FT-LS022, FT-TB102
#     collapse_repeats = ) the same; each data frame        |             |
#   a column not found; not an rtftable                     | FR-LS238/239| FT-LS011, FT-LS018, FT-TB100

test_that("FT-LS151 [FR-LS248]: listing_take() and listing_split_after() take one string and one width or separator", {
  for (bad in list(c("a", "b"), NA_character_, 1L, character(0))) {
    expect_error(listing_take(bad, 3), "`listing_take()`: `x` must be one string.", fixed = TRUE)
    expect_error(listing_split_after(bad, "/"), "`listing_split_after()`: `text` must be one string.",
                 fixed = TRUE)
  }
  for (bad in list(-1, NA_real_, c(1, 2), "3")) {
    expect_error(listing_take("abc", bad),
                 "`listing_take()`: `width` must be one number of 0 or more (a display width).", fixed = TRUE)
  }
  for (bad in list(c("/", "-"), NA_character_, 1L)) {
    expect_error(listing_split_after("a/b", bad), "`listing_split_after()`: `sep` must be one string, or NULL.",
                 fixed = TRUE)
  }
  expect_identical(listing_take("abc", 0), "a")
})

test_that("FT-LS152 [FR-LS249]: listing_split_after() takes any separator literally", {
  expect_identical(listing_split_after("a\\Eb\\Ec", "\\E"), c("a\\E", "b\\E", "c"))
  expect_identical(listing_split_after("a\\b", "\\"), c("a\\", "b"))
  expect_identical(listing_split_after("a.*b.*", ".*"), c("a.*", "b.*"))
  expect_identical(listing_split_after("AGE / SEX / RACE", " / "), c("AGE / ", "SEX / ", "RACE"))
  expect_identical(listing_split_after("a//b", "/"), c("a/", "/", "b"))
  expect_identical(listing_split_after("頭痛・吐き気", "・"),
                   c("頭痛・", "吐き気"))
  expect_identical(listing_split_after("", "/"), "")
  # the rule built on it breaks after such a separator too
  expect_identical(listing_wrap("HEADACHE\\ENAUSEA", 10, sep = "\\E"), c("HEADACHE\\E", "NAUSEA"))
})

test_that("FT-LS153 [FR-LS250]: a landscape A4 page with half-inch margins holds 159 characters at 8pt, 142 at 9pt", {
  d <- data.frame(USUBJID = c("EX-001", "EX-002"), AETERM = c("HEADACHE", "NAUSEA"))
  spec <- listing_spec(list(listing_col("USUBJID"), listing_col("AETERM")))
  pg <- rtf_page(paper_size = "A4", orientation = "landscape",
                 margin_left_in = 0.5, margin_right_in = 0.5)
  budget <- function(...) attr(fit_listing_widths(d, spec, page = pg, ...), "rtf_listing_fit")$total_width
  expect_equal(budget(font_size_half_points = 16L), 159)
  expect_equal(budget(), 142)
})
