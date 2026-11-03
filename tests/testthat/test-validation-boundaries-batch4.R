# Validation tests: equivalence classes and boundary values of the arguments,
# batch 4 -- the cases batch 3 left for a decision (assemble_folder()'s dir,
# recursive and spec_file, #862; fmt_signif() / fmt_numeric() with zero
# significant digits, #863; plan_columns()' widths and decimal, #864) and
# the arguments of plan_labels(), plan_nest() and plan_compare().  Helpers
# prefixed .eb4_.  Fictional data only.
#
# The help is the functional requirement.  For each argument the classes
# are: a valid value of each documented kind, NULL, length 0 / 1 / n / a
# wrong length, NA, a value of another type, the documented minimum and
# maximum with a value just inside and just outside, 0, a negative number,
# a fraction for a whole number, Inf / NaN, "" / a non-ASCII / a long
# string, and an unknown choice / a prefix.  A class another test already
# checks is not repeated (batch 3: FT-AS141, FT-FM103, FT-FM104, FT-PL976).
# Each table below is one row per case; the test of the function runs them
# in a loop.  "object" means the returned object is checked, not the file.
#
# ---------------------------------------------------------------------------
#  assemble_folder()  -- FT-AS142.  A folder of two files, one of one file.
# ---------------------------------------------------------------------------
#  argument       | class                 | value              | expected                         | FR
#  dir            | NA / "" / length 2 / a number / NULL / TRUE |  | error "`dir` must be a single folder path" | FR-AS247
#  dir            | one .rtf file, no output_file |            | object: one row                  | FR-AS148 FR-AS247
#  dir            | one .rtf file, an output_file |            | error "`dir` holds one .rtf file" | FR-AS247
#  dir            | one .rtf file, a spec_file too |           | error, and no spec file written  | FR-AS247
#  recursive      | NA / "yes" / 1 / length 2 / length 0 / NULL | | error "`recursive` must be TRUE or FALSE" | FR-AS247
#  spec_file      | length 2 / length 0   |                    | error "`spec_file` must be NULL or a single path" | FR-AS247
#  spec_file      | in a folder that is not there |            | error "The folder of `spec_file` does not exist" | FR-AS247
#  spec_file      | ".xlsx" upper case / non-ASCII name |      | written                          | FR-AS153
#  spec_file      | a bad value, a bad dir too |               | the spec_file error first: nothing is read | FR-AS247
#
# ---------------------------------------------------------------------------
#  fmt_signif() / fmt_numeric()  -- FT-FM107.  Zero significant digits.
# ---------------------------------------------------------------------------
#  fmt_signif     digits  | 1 (the minimum)       |             | "123" "0.01"                     | FR-FM146
#  fmt_signif     digits  | 0 / 0L / -0 / 0.5     |             | error "must be a single positive integer" | FR-FM146
#  fmt_numeric    signif  | 1 (the minimum)       |             | "0.01"                           | FR-FM173
#  fmt_numeric    signif  | 0 / a rule's signif 0 |             | error "`fmt_numeric(signif = )` must be a single positive integer" | FR-FM173
#  fmt_numeric    digits  | 0 (the minimum)       |             | "0"                              | FR-FM173
#  fmt_numeric    digits  | -1 / a rule's digits -1 |           | error "must be a single non-negative integer" | FR-FM173
#
# ---------------------------------------------------------------------------
#  plan_columns()  -- FT-PL980.  widths and decimal (#864).
# ---------------------------------------------------------------------------
#  widths         | a fraction / a very small / a large |      | col_rel_width                    | FR-PL157
#  widths         | integer / named, a hidden column |         | col_rel_width                    | FR-PL157 FR-PL160
#  widths         | 0 / a negative / NA / NaN / Inf / "3" / TRUE / a factor / one 0 among n | | error at plan_columns() "plan_columns(widths = ) is positive finite numbers" | FR-PL961
#  widths         | NULL / length 0       |                    | no widths given                  | FR-PL961
#  decimal        | ".values" / a name / two names |           | decimal_split$cols               | FR-PL161 FR-PL162
#  decimal        | NULL / length 0       |                    | no decimal alignment             | FR-PL961
#  decimal        | NA / "" / a number / TRUE |                | error at plan_columns() "plan_columns(decimal = ) is column names" | FR-PL961
#  decimal        | a name not a column / one of two |         | error at plan_apply() "names 'nope', not a column of the table" | FR-PL163
#  decimal        | a hidden column       |                    | allowed, no alignment            | FR-PL163
#  decimal        | another case          | ".Values"          | error, same                      | FR-PL163
#
# ---------------------------------------------------------------------------
#  plan_labels()  -- FT-PL981.  Age (Mean) and Sex (F, M) in two arms.
# ---------------------------------------------------------------------------
#  <variable>     | one text / non-ASCII / long / "" |         | the block's heading              | FR-PL258
#  <key> dict     | c(F = "Female") / one of two / with the heading | | the levels' text             | FR-PL258 FR-PL930
#  <column key>   | c(Active = "Drug X")  |                    | the value column's name          | FR-PL257
#  map whole      | c(AGE = "Age") unnamed |                   | as written out                   | FR-PL932
#  no entry       | plan_labels()         |                    | the table unchanged              | FR-PL962
#  <unknown key>  | NOPE = "x" / a code no row has |           | not used, no message             | FR-PL963
#  <variable>     | NA / NA_character_ / NULL / a number / TRUE / a factor / length 0 / a list / two unnamed | | error "plan_labels(AGE = ) is one text" | FR-PL962
#  <key> dict     | an NA text / an unnamed text among named |  | error, same                      | FR-PL962
#  unnamed        | "x"                   |                    | error "every entry is named"     | FR-PL261
#
# ---------------------------------------------------------------------------
#  plan_nest()  -- FT-PL982.  RACE and ASIANORG (the origin of the Asian rows).
# ---------------------------------------------------------------------------
#  <variable>     | c(RACE = "Asian")     |                    | the origins under "Asian"        | FR-PL318
#  <variable>     | a non-ASCII level     |                    | matched as the table shows it    | FR-PL319
#  <variable>     | a level no row has / "" / NA as text | | error at plan_apply() "RACE has no row" | FR-PL324
#  <variable>     | a parent not there    | c(NOPE = "Asian")  | error at plan_apply()            | FR-PL324
#  <variable>     | not a variable of the table |              | error "the table has no rows of NOPE" | FR-PL324
#  <variable>     | unnamed / NA / a number / length 2 / length 0 / NULL / a list / an NA name | | error "one level of one variable" | FR-PL323 FR-PL964
#  <variable>     | under its own level   | RACE = c(RACE = "Asian") | error "not its own"          | FR-PL964
#  <variable>     | given twice           |                    | error "is given twice"           | FR-PL964
#  no entry / unnamed |                   |                    | error "name the nested variable" | FR-PL323
#
# ---------------------------------------------------------------------------
#  plan_compare()  -- FT-PL983.  AGE by arm and a t-test of the two arms.
# ---------------------------------------------------------------------------
#  ...            | a template / non-ASCII name / two columns |  | the columns                    | FR-PL289 FR-PL290
#  ...            | a map by context / a context no record has |  | 0.031 / NA                    | FR-PL290
#  ...            | none / unnamed / a name twice |            | error "every column has a name of its own" | FR-PL298
#  ...            | a column of the table | Active = ...       | error "is a column of the table already" | FR-PL300
#  position       | "last" / "first" / prefix "f" |            | after / before the value columns | FR-PL292
#  position       | another case / "" / unknown / NA / a number / c("first", "last") | | error (match.arg) | FR-PL299
#  by             | NA / a number / length 2 |                 | error "plan_compare(by = ) is one column" | FR-PL299
#  by             | not a column / ""     |                    | error at plan_apply() "no such column" | FR-PL301
#  (a column's template that is NA, a number, TRUE, "" or length 0 gives an
#   empty or constant column without a message, as plan_cells() does:
#   issue #866, as it is plan_cells()' rule.)
# ---------------------------------------------------------------------------

# ---- the runner (as in batch 3) ---------------------------------------------

# One case: `expect` is "obj" with a `chk` function, or "error:<regex>".
# `more` holds the other arguments.
.eb4_case <- function(arg, cls, val, expect, chk = NULL, more = list()) {
  list(arg = arg, cls = cls, val = val, expect = expect, chk = chk,
       more = more)
}
.eb4_err <- function(arg, cls, val, pat, more = list()) {
  .eb4_case(arg, cls, val, paste0("error:", pat), more = more)
}
.eb4_obj <- function(arg, cls, val, chk, more = list()) {
  .eb4_case(arg, cls, val, "obj", chk = chk, more = more)
}
.eb4_run <- function(cases, run) {
  for (cs in cases) {
    args <- c(setNames(list(cs$val), cs$arg), cs$more)
    lab  <- sprintf("%s = <%s>", cs$arg, cs$cls)
    if (startsWith(cs$expect, "error:")) {
      expect_error(run(args), sub("^error:", "", cs$expect), info = lab)
    } else {
      res <- run(args)
      expect_true(isTRUE(cs$chk(res)), info = lab)
    }
  }
}

# ---- assemble_folder() -------------------------------------------------------

.eb4_env <- new.env()
# Two one-page TFL files in a folder of their own, and one in another, made
# once.
.eb4_dirs <- function() {
  if (is.null(.eb4_env$two)) {
    mk <- function(d, t, f) {
      doc <- rtf_document() |>
        rtf_tables(data.frame(Parameter = "Age", Value = "75.1")) |>
        rtf_titles(list(c(paste("Table", t), "Safety Population")))
      suppressMessages(generate_rtfreport(doc, file.path(d, f)))
    }
    two <- tempfile("eb4two")
    one <- tempfile("eb4one")
    dir.create(two)
    dir.create(one)
    mk(two, "14.1.1", "t14_1_1.rtf")
    mk(two, "14.2.1", "t14_2_1.rtf")
    mk(one, "14.3.1", "t14_3_1.rtf")
    .eb4_env$two <- two
    .eb4_env$one <- one
  }
  c(two = .eb4_env$two, one = .eb4_env$one)
}

test_that("FT-AS142 [FR-AS148 FR-AS153 FR-AS247]: assemble_folder(): equivalence classes and boundaries of dir, recursive and spec_file", {
  E <- .eb4_err
  O <- .eb4_obj
  d <- .eb4_dirs()
  dr <- "^`dir` must be a single folder path."
  rc <- "^`recursive` must be TRUE or FALSE."
  sf <- "^`spec_file` must be NULL or a single path ending in .xlsx or .csv."
  one <- "^`dir` holds one .rtf file \\(t14_3_1.rtf\\); assembling needs two or more."
  nrows <- function(n) function(x) is.data.frame(x) && nrow(x) == n
  out <- function() tempfile(fileext = ".rtf")
  csv_kanji <- file.path(tempdir(), "目次_eb4.csv")
  spec_one <- file.path(tempdir(), "eb4_one.csv")
  cases <- list(
    E("dir", "NA", NA, dr),
    E("dir", "NA_character_", NA_character_, dr),
    E("dir", "empty string", "", dr),
    E("dir", "length 2", unname(c(d["two"], d["two"])), dr),
    E("dir", "length 0", character(0), dr),
    E("dir", "a number", 1, dr),
    E("dir", "TRUE", TRUE, dr),
    E("dir", "NULL", NULL, dr),
    O("dir", "one .rtf file, no output_file", unname(d["one"]), nrows(1)),
    E("dir", "one .rtf file, an output_file", unname(d["one"]), one, more = list(output_file = out())),
    E("dir", "one .rtf file, a spec_file too", unname(d["one"]), one,
      more = list(output_file = out(), spec_file = spec_one)),
    E("recursive", "NA", NA, rc),
    E("recursive", "a string", "yes", rc),
    E("recursive", "a number", 1, rc),
    E("recursive", "length 2", c(TRUE, FALSE), rc),
    E("recursive", "length 0", logical(0), rc),
    E("recursive", "NULL", NULL, rc),
    E("spec_file", "length 2", c("a.csv", "b.csv"), sf),
    E("spec_file", "length 0", character(0), sf),
    E("spec_file", "a folder that is not there", file.path(tempdir(), "eb4_nodir", "t.csv"),
      "^The folder of `spec_file` does not exist: "),
    O("spec_file", "non-ASCII name", csv_kanji, function(x) is.data.frame(x) && file.exists(csv_kanji)),
    E("spec_file", "a bad value, a bad dir too", "s.txt", "^`spec_file` must end in .xlsx or .csv.",
      more = list(dir = file.path(tempdir(), "eb4_nope")))
  )
  .eb4_run(cases, function(args) {
    if (!"dir" %in% names(args)) args <- c(list(dir = unname(d["two"])), args)
    do.call(assemble_folder, args)
  })
  # the one-file folder stopped before its spec file was written
  expect_false(file.exists(spec_one))
  # an upper-case .XLSX, when writexl is there
  skip_if_not_installed("writexl")
  xl <- file.path(tempdir(), "eb4_spec.XLSX")
  assemble_folder(unname(d["two"]), spec_file = xl)
  expect_true(file.exists(xl))
})

# ---- fmt_signif() / fmt_numeric() --------------------------------------------

test_that("FT-FM107 [FR-FM146 FR-FM173]: fmt_signif() and fmt_numeric(): zero significant digits, the minimum of digits and signif", {
  E <- .eb4_err
  O <- .eb4_obj
  is <- function(v) function(x) identical(x, v)
  dg <- "^`fmt_signif\\(digits = \\)` must be a single positive integer."
  x <- c(123.456, 0.012345)
  cases <- list(
    O("digits", "1 (the minimum)", 1, is(c("123", "0.01"))),
    O("digits", "1L", 1L, is(c("123", "0.01"))),
    E("digits", "0", 0, dg),
    E("digits", "0L", 0L, dg),
    E("digits", "-0", -0, dg),
    E("digits", "0.5 (between 0 and 1)", 0.5, dg)
  )
  .eb4_run(cases, function(args) do.call(fmt_signif, c(list(x = x), args)))

  df <- data.frame(g = c("a", "b"), v = c(0.012345, 5.678))
  v_is <- function(v) function(r) identical(r$v, v)
  sg <- "^`fmt_numeric\\(signif = \\)` must be a single positive integer."
  dn <- "^`fmt_numeric\\(digits = \\)` must be a single non-negative integer."
  cases <- list(
    O("signif", "1 (the minimum)", 1, v_is(c("0.01", "6"))),
    E("signif", "0", 0, sg),
    E("signif", "0L", 0L, sg),
    O("digits", "0 (the minimum)", 0, v_is(c("0", "6"))),
    E("digits", "-1", -1, dn),
    E("formats", "a rule's signif 0", list(a = list(signif = 0), b = list(digits = 1)), sg,
      more = list(by = "g")),
    O("formats", "a rule's signif 1", list(a = list(signif = 1), b = list(digits = 0)), v_is(c("0.01", "6")),
      more = list(by = "g")),
    O("formats", "a rule's digits 0", list(a = list(digits = 0), b = list(signif = 2)), v_is(c("0", "5.7")),
      more = list(by = "g")),
    E("formats", "a rule's digits -1", list(a = list(digits = -1), b = list(digits = 1)), dn,
      more = list(by = "g"))
  )
  .eb4_run(cases, function(args) do.call(fmt_numeric, c(list(data = df, cols = "v"), args)))
})

# ---- the plan verbs ----------------------------------------------------------

# Age (Mean) and Sex (F, M) in two arms.
.eb4_st <- data.frame(
  ARM = rep(c("Placebo", "Active"), each = 3),
  variable = rep(c("AGE", "SEX", "SEX"), 2),
  .label = rep(c(NA, "F", "M"), 2),
  .kind = rep(c("continuous", "categorical", "categorical"), 2),
  stat_name = rep(c("mean", "n", "n"), 2),
  stat = c(61.246, 12, 8, 63.51, 9, 11))
.eb4_plan <- function() {
  table_plan(.eb4_st, cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(Mean = "{mean}"), categorical = "{n}", notes = FALSE)
}
.eb4_pages <- function(verb, args, plan = .eb4_plan()) {
  plan_apply(do.call(verb, c(list(plan), args)), "pages")
}

test_that("FT-PL980 [FR-PL157 FR-PL160 FR-PL161 FR-PL162 FR-PL163 FR-PL961]: plan_columns(): equivalence classes and boundaries of widths and decimal", {
  E <- .eb4_err
  O <- .eb4_obj
  wd <- "^plan_columns\\(widths = \\) is positive finite numbers, one a column in order or named by column."
  dc <- "^plan_columns\\(decimal = \\) is column names, \".values\" for every value column."
  nope <- "^plan_columns\\(decimal = \\) names 'nope', not a column of the table."
  rw <- function(v) function(p) identical(p[[1L]]$col_rel_width, v)
  ds <- function(v) function(p) identical(p[[1L]]$decimal_split$cols, v)
  no_ds <- function(p) is.null(p[[1L]]$decimal_split)
  hid <- .eb4_plan() |> plan_hide("group")
  cases <- list(
    O("widths", "a fraction", c(0.5, 0.25), rw(c(0.5, 0.25, 0.25, 0.25))),
    O("widths", "very small", 1e-6, rw(rep(1e-6, 4))),
    O("widths", "large", 1e6, rw(rep(1e6, 4))),
    O("widths", "integer", c(3L, 1L), function(p) isTRUE(all.equal(p[[1L]]$col_rel_width, c(3, 1, 1, 1)))),
    O("widths", "NULL (default)", NULL, function(p) is.list(p) && length(p) == 1L),
    O("widths", "length 0", numeric(0), function(p) is.list(p) && length(p) == 1L),
    E("widths", "0", 0, wd),
    E("widths", "a negative", c(-1, 1, 1), wd),
    E("widths", "one 0 among n", c(3, 0, 1, 1), wd),
    E("widths", "NA", c(3, NA), wd),
    E("widths", "NaN", NaN, wd),
    E("widths", "Inf", c(Inf, 1, 1), wd),
    E("widths", "a string", "3", wd),
    E("widths", "TRUE", TRUE, wd),
    E("widths", "a factor", factor(3), wd),
    E("widths", "named, a 0", c(group = 0, label = 3, .values = 1), wd),
    O("decimal", ".values", ".values", ds(3:4)),
    O("decimal", "a name", "Active", ds(4L)),
    O("decimal", "two names", c("Placebo", "Active"), ds(3:4)),
    O("decimal", "NULL (default)", NULL, no_ds),
    O("decimal", "length 0", character(0), no_ds),
    E("decimal", "NA", NA, dc),
    E("decimal", "NA_character_", NA_character_, dc),
    E("decimal", "empty string", "", dc),
    E("decimal", "a number", 3, dc),
    E("decimal", "TRUE", TRUE, dc),
    E("decimal", "a name not a column", "nope", nope),
    E("decimal", "one of two not a column", c("Active", "nope"), nope),
    E("decimal", "another case", ".Values", "^plan_columns\\(decimal = \\) names '.Values'")
  )
  .eb4_run(cases, function(args) .eb4_pages(plan_columns, args))
  # a hidden column: its width and its name in decimal are allowed
  p <- plan_apply(hid |> plan_columns(widths = c(group = 9, label = 3, .values = 1),
                                      decimal = "group"), "pages")
  expect_identical(p[[1L]]$col_rel_width, c(3, 1, 1))
  expect_null(p[[1L]]$decimal_split)
})

test_that("FT-PL981 [FR-PL257 FR-PL258 FR-PL261 FR-PL930 FR-PL932 FR-PL962 FR-PL963]: plan_labels(): equivalence classes and boundaries of its entries", {
  E <- .eb4_err
  O <- .eb4_obj
  tb <- function(p) plan_apply(p, "table")
  first <- function(v) function(t) identical(as.character(t[[1L]]), v)
  second <- function(v) function(t) identical(as.character(t[[2L]]), v)
  same <- function(t) identical(t, tb(.eb4_plan()))
  long <- strrep("Age in years ", 30)
  lb <- "^plan_labels\\(AGE = \\) is one text, or texts named by code"
  ls <- "^plan_labels\\(SEX = \\) is one text, or texts named by code"
  cases <- list(
    O("AGE", "one text", "Age", first(c("Age", "SEX", "SEX"))),
    O("AGE", "non-ASCII", "年齢", first(c("年齢", "SEX", "SEX"))),
    O("AGE", "long", long, first(c(long, "SEX", "SEX"))),
    O("AGE", "empty string", "", first(c("", "SEX", "SEX"))),
    O("SEX", "a dictionary", c(F = "Female", M = "Male"), second(c("Mean", "Female", "Male"))),
    O("SEX", "one code of two", c(F = "Female"), second(c("Mean", "Female", "M"))),
    O("SEX", "with the heading", c(SEX = "Sex", F = "Female"), first(c("AGE", "Sex", "Sex"))),
    O("ARM", "a column key", c(Active = "Drug X"), function(t) identical(names(t), c("group", "label", "Placebo", "Drug X"))),
    O("NOPE", "a key the table has not", "x", same),
    O("SEX", "a code no row has", c(Z = "Zed"), same),
    E("AGE", "NA", NA, lb),
    E("AGE", "NA_character_", NA_character_, lb),
    E("AGE", "NULL", NULL, lb),
    E("AGE", "a number", 1, lb),
    E("AGE", "TRUE", TRUE, lb),
    E("AGE", "a factor", factor("Age"), lb),
    E("AGE", "length 0", character(0), lb),
    E("AGE", "a list", list("Age"), lb),
    E("AGE", "two unnamed texts", c("Age", "Years"), lb),
    E("SEX", "a dictionary with an NA text", c(F = NA, M = "Male"), ls),
    E("SEX", "an unnamed text among named", c(F = "Female", "Male"), ls)
  )
  .eb4_run(cases, function(args) tb(do.call(plan_labels, c(list(.eb4_plan()), args))))
  # the whole map, no entry, and an unnamed text
  expect_identical(tb(plan_labels(.eb4_plan(), c(AGE = "Age", SEX = "Sex"))),
                   tb(plan_labels(.eb4_plan(), AGE = "Age", SEX = "Sex")))
  expect_true(same(tb(plan_labels(.eb4_plan()))))
  expect_error(plan_labels(.eb4_plan(), "x"), "^plan_labels\\(\\): every entry is named")
})

# RACE, and the Asian subjects' origin as its own variable.
.eb4_race <- data.frame(
  ARM = rep(c("Placebo", "Active"), each = 5),
  variable = rep(c("RACE", "RACE", "RACE", "ASIANORG", "ASIANORG"), 2),
  .label = rep(c("White", "Asian", "Black", "Chinese", "Japanese"), 2),
  .kind = "categorical", stat_name = "n",
  stat = c(10, 6, 4, 2, 4, 9, 7, 4, 3, 4))
.eb4_race_plan <- function(d = .eb4_race) {
  table_plan(d, cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n}", notes = FALSE)
}

test_that("FT-PL982 [FR-PL318 FR-PL319 FR-PL323 FR-PL324 FR-PL964]: plan_nest(): equivalence classes and boundaries of its entries", {
  E <- .eb4_err
  O <- .eb4_obj
  ind <- strrep(" ", 4)
  lab <- function(v) function(t) identical(as.character(t[[2L]]), v)
  one <- "^plan_nest\\(ASIANORG = \\): one level of one variable, named by it"
  norow <- "^plan_nest\\(ASIANORG = c\\(RACE = \"%s\"\\)\\): RACE has no row"
  cases <- list(
    O("ASIANORG", "valid", c(RACE = "Asian"),
      lab(c("White", "Asian", paste0(ind, "Chinese"), paste0(ind, "Japanese"), "Black"))),
    E("ASIANORG", "a level no row has", c(RACE = "Nope"), sprintf(norow, "Nope")),
    E("ASIANORG", "another case", c(RACE = "asian"), sprintf(norow, "asian")),
    E("ASIANORG", "empty level", c(RACE = ""), sprintf(norow, "")),
    E("ASIANORG", "\"NA\" as text", c(RACE = "NA"), sprintf(norow, "NA")),
    E("ASIANORG", "a parent not there", c(NOPE = "Asian"), "^plan_nest\\(ASIANORG = c\\(NOPE = \"Asian\"\\)\\): NOPE has no row"),
    E("NOPE", "not a variable of the table", c(RACE = "Asian"), "^plan_nest\\(NOPE = \\): the table has no rows of NOPE"),
    E("ASIANORG", "unnamed", "Asian", one),
    E("ASIANORG", "NA level", c(RACE = NA_character_), one),
    E("ASIANORG", "NA (logical)", c(RACE = NA), one),
    E("ASIANORG", "a number", c(RACE = 1), one),
    E("ASIANORG", "length 2", c(RACE = "Asian", RACE = "White"), one),
    E("ASIANORG", "length 0", character(0), one),
    E("ASIANORG", "NULL", NULL, one),
    E("ASIANORG", "a list", list(RACE = "Asian"), one),
    E("ASIANORG", "an NA name", stats::setNames("Asian", NA), one),
    E("RACE", "under its own level", c(RACE = "Asian"), "^plan_nest\\(RACE = \\): a variable goes under a level of another variable, not its own."),
    E("ASIANORG", "given twice", c(RACE = "Asian"), "^plan_nest\\(ASIANORG = \\) is given twice",
      more = list(ASIANORG = c(RACE = "White")))
  )
  .eb4_run(cases, function(args) plan_apply(do.call(plan_nest, c(list(.eb4_race_plan()), args)), "table"))
  # a non-ASCII level, matched as the table shows it
  d <- .eb4_race
  d$.label[d$.label == "Asian"] <- "アジア"
  t <- plan_apply(plan_nest(.eb4_race_plan(d), ASIANORG = c(RACE = "アジア")), "table")
  expect_identical(as.character(t[[2L]])[2:3], c("アジア", paste0(ind, "Chinese")))
  # no entry, an unnamed one
  nm <- "^plan_nest\\(\\): name the nested variable"
  expect_error(plan_nest(.eb4_race_plan()), nm)
  expect_error(plan_nest(.eb4_race_plan(), c(RACE = "Asian")), nm)
})

# AGE by arm, and a t-test of the two arms (the records with no ARM).
.eb4_cmp <- data.frame(
  ARM = c(rep(c("Placebo", "Active"), each = 3), NA, NA, NA, NA),
  variable = "AGE",
  context = c(rep("summary", 6), rep("stats_t_test", 4)),
  .label = NA, .kind = "continuous",
  stat_name = c("N", "mean", "sd", "N", "mean", "sd",
                "p.value", "estimate", "conf.low", "conf.high"),
  stat = c(20, 61.2, 9.81, 20, 63.5, 10.2, 0.0312, -2.3, -4.4, -0.2))
.eb4_cmp_plan <- function() {
  table_plan(.eb4_cmp, cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(n = "{N}", "Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
               notes = FALSE)
}

test_that("FT-PL983 [FR-PL289 FR-PL290 FR-PL292 FR-PL298 FR-PL299 FR-PL300 FR-PL301]: plan_compare(): equivalence classes and boundaries of its arguments", {
  E <- .eb4_err
  O <- .eb4_obj
  p3 <- list(p = "{p.value:.3f}")
  nms <- function(v) function(t) identical(names(t), v)
  pv <- function(v) function(t) identical(t$p, v)
  last <- c("group", "label", "Placebo", "Active", "p")
  frst <- c("group", "label", "p", "Placebo", "Active")
  ma <- "^'arg' should be one of"
  mn <- "^'arg' must be NULL or a character vector"
  by <- "^plan_compare\\(by = \\) is one column, the key that splits the comparisons"
  cases <- list(
    O("p", "a template", "{p.value:.3f}", pv(c("0.031", NA))),
    O("p値", "a non-ASCII name", "{p.value:.3f}", nms(c("group", "label", "Placebo", "Active", "p値"))),
    O("diff", "two columns", "{estimate:.1f}", nms(c("group", "label", "Placebo", "Active", "diff", "p")), more = p3),
    O("p", "a map by context", c(stats_t_test = "{p.value:.3f}"), pv(c("0.031", NA))),
    O("p", "a context no record has", c(stats_wilcox_test = "{p.value:.3f}"), pv(c(NA_character_, NA))),
    E("Active", "a column of the table", "{p.value:.3f}", "^plan_compare\\(\\): 'Active' is a column of the table already."),
    O("position", "last (default)", "last", nms(last), more = p3),
    O("position", "first", "first", nms(frst), more = p3),
    O("position", "prefix", "f", nms(frst), more = p3),
    E("position", "another case", "First", ma, more = p3),
    E("position", "empty string", "", ma, more = p3),
    E("position", "unknown", "mid", ma, more = p3),
    E("position", "NA", NA, mn, more = p3),
    E("position", "a number", 1, mn, more = p3),
    E("position", "length 2, not the default", c("first", "last"), "^'arg' must be of length 1", more = p3),
    E("by", "NA", NA, by, more = p3),
    E("by", "a number", 1, by, more = p3),
    E("by", "length 2", c("a", "b"), by, more = p3),
    E("by", "not a column", "NOPE", "^plan_compare\\(by = \"NOPE\"\\): no such column in the comparison records.", more = p3),
    E("by", "empty string", "", "^plan_compare\\(by = \"\"\\): no such column in the comparison records.", more = p3)
  )
  .eb4_run(cases, function(args) plan_apply(do.call(plan_compare, c(list(.eb4_cmp_plan()), args)), "table"))
  # no column, an unnamed one, a name twice
  own <- "^plan_compare\\(\\): every column has a name of its own"
  expect_error(plan_compare(.eb4_cmp_plan()), own)
  expect_error(plan_compare(.eb4_cmp_plan(), "{p.value:.3f}"), own)
  expect_error(plan_compare(.eb4_cmp_plan(), p = "{p.value:.3f}", p = "{estimate}"), own)
})
