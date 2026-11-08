# Help-to-FR audit, batch 6: the ARD helpers (normalize_ard(), widen_ard(),
# pull_ard(), list_ard_keys(), overall_row(), cell_rows()) and the number
# formats (round_num(), fmt_signif(), fmt_round(), fmt_numeric(),
# fmt_count_pct(), fmt_right_align(), fmt_count_paren(), fmt_value_paren()).
#
# The help is the functional requirement: every statement in a function's Rd
# is an active FR in inst/validation/requirements.csv, and every FR has a
# test.  For each function below, one row per claim of its roxygen: the
# claim (short), the FR that states it, and the test that checks it.  The
# tests below this table close the gaps the audit found.
#
# list_ard_keys()
#   claim                                                   | FR          | test
#   prints keys, variables, contexts, statistics; rows only | FR-AR233    | FT-AR038
#   returns invisibly keys / variables / contexts / kinds / | FR-AR234    | FT-AR038
#     stats                                                 |             |
#   the structural kinds, as normalize_ard() decides them   | FR-AR235    | FT-AR038, FT-AR066
#   a key in several group slots, or no key at all          | FR-AR236/237| FT-AR094
#
# overall_row()
#   the cards sentinel rows (from = NULL)                   | FR-AR229    | FT-AR008
#   a separately built block counting the arm (from = )     | FR-AR230/231| FT-AR029
#   label written into the first hierarchy column           | FR-AR228    | FT-AR008, FT-AR029
#   an overall_row object; overall = "x" is overall_row("x")| FR-AR227    | FT-AR008
#   label is one string                                     | FR-AR232    | FT-AR047
#
# cell_rows()
#   a named row whose value is a chain (with guards)        | FR-AR164    | FT-TB507
#   an object of class cell_rows for widen_ard(cells = )    | FR-AR164    | FT-TB507
#   one argument per row; none stops                        | FR-AR238    | FT-AR095
#   an unnamed argument takes its label from `label`        | FR-AR164/248| FT-AR105 (#840, fixed)
#   a named row on a variable with levels stops             | FR-AR249    | FT-AR106 (#840, fixed)
#
# pull_ard()
#   one statistic, keyed like the spread columns; a named   | FR-AR213    | FT-AR049, FT-AR050
#     vector                                                |             |
#   the number the percentages used; agreeing answers only  | FR-AR215    | FT-AR049, FT-AR050
#   stat = "N" by default, or any statistic                 | FR-AR218    | FT-AR050
#   the key variables' own rows left out unless named       | FR-AR214/219| FT-AR050, FT-AR090
#   several answers that disagree stop with the candidates  | FR-AR226    | FT-AR084
#   cols by the grouping variable's own name; unknown stops | FR-AR216/224| FT-AR049-FT-AR051
#   variable / context restrict the pull                    | FR-AR219    | FT-AR090
#   levels: a named list ordering the result                | FR-AR220    | FT-AR014, FT-AR050
#   ... a plain vector is not read (code fixed)             | FR-AR239    | FT-AR096
#   ... with several cols, matched on the joined names      | FR-AR240    | FT-AR097
#   sep as widen_ard()'s                                    | FR-AR221    | FT-AR049
#
# normalize_ard()
#   group pairs to named key columns; lists flattened;      | FR-AR101-105| FT-AR009, FT-AR015, FT-AR022, FT-AR042, FT-AR044
#     stat_fmt; rows read, never attributes                 |             |
#   a plain data frame, worked on before widen_ard()        | FR-AR111/144| FT-AR048, FT-AR063
#   keys = NULL / names                                     | FR-AR113    | FT-AR042
#   hierarchy: folded, .label deepest, .depth 1 / 0 / NA    | FR-AR114/115| FT-AR027, FT-AR040, FT-AR063
#   overall: the sentinel's label, NULL drops it            | FR-AR116/118| FT-AR008, FT-AR029, FT-AR043
#   ... hierarchy is needed with it                         | FR-AR241    | FT-AR098
#   drop_contexts; the total kept as ard_total_n / .total_n | FR-AR119-121| FT-AR067, FT-AR074, FT-AR075, FT-AR081, FT-AR088
#   drop_key_variables FALSE (.key_own) / TRUE              | FR-AR122/123| FT-AR018, FT-AR042
#   stack: layers, short forms, group slots, which row      | FR-AR124-127, FR-AR136-142 | FT-AR076-FT-AR080, FT-AR083
#     where, refused; stack_sep; stack_nest                 |             |
#   ard_ignored: a report nothing builds the table from     | FR-AR110    | FT-AR063
#   ... dropping it only shortens widen_ard()'s notes       | FR-AR242    | FT-AR099
#   factor levels: the label, .label_order, relabelling     | FR-AR103/107| FT-AR013, FT-AR015, FT-AR063
#   a factor key comes back a factor with its levels        | FR-AR108    | FT-AR010, FT-AR014, FT-AR060
#   the columns returned; .kind                             | FR-AR101/106| FT-AR001, FT-AR042, FT-AR063, FT-AR066
#
# widen_ard()
#   one ARD record one row; a plain data frame returned     | FR-AR143    | FT-AR054, FT-AR064
#   cols: one / several keys, sep; the name ignored; left   | FR-AR150-152, FR-AR196 | FT-AR014, FT-AR054, FT-AR064, FT-PL041
#     out: one column All; an ARD with a group needs it     |             |
#   rows: any column, names, NULL default, a formula        | FR-AR153-155| FT-AR003, FT-AR004, FT-AR023, FT-AR025
#   label: .label / NULL / NA / a formula chain             | FR-AR156-159| FT-AR019, FT-AR035, FT-AR064
#   ... a bare formula, a chain of one (code fixed)         | FR-AR247    | FT-AR104
#   cells: one template, a chain, a guard, named rows       | FR-AR160-162| FT-AR016, FT-AR017, FT-AR045, FT-AR056
#   ... a guard may name the caller's variables             | FR-AR243    | FT-AR100
#   cells: a list is a map (variable, context, kind,        | FR-AR163    | FT-AR033, FT-AR034, FT-AR066
#     default)                                              |             |
#   ... a list with no names is a chain                     | FR-AR244    | FT-AR101
#   statistics no template names are not read, reported     | FR-AR146    | FT-AR056, FT-AR067
#   the {token:spec} grammar                                | FR-AR165, FR-AR204 | FT-AR006, FT-AR009, FT-AR053, FT-AR068
#   stat_groups; filter; show_keys; compare                 | FR-AR183-186| FT-PL034, FT-PL036, FT-PL115, FT-PL118
#   stats = "cells" / "rows"; value = "stat" / "stat_fmt"   | FR-AR166/167| FT-AR046, FT-AR057, FT-AR058
#   levels: column key, row key, analysis variable,         | FR-AR168/169| FT-AR011, FT-AR036, FT-AR037, FT-PL107
#     variable; a value the ARD lacks is said               |             |
#   labels: text only, every element named, scoped by      | FR-AR170-172, FR-AR200 | FT-AR002, FT-AR024, FT-AR031, FT-AR036, FT-PL106
#     column or analysis variable                           |             |
#   ... setNames() of different lengths stops               | FR-AR245    | FT-AR102
#   sort: FALSE / TRUE / keys / scoped forms                | FR-AR173-176| FT-AR004, FT-AR005, FT-AR011, FT-AR021, FT-PL123
#   ... "sum(n)" is "n"; "min(n)"; an outer key's level     | FR-AR246    | FT-AR103
#   rounding; sort_stat; na                                 | FR-AR178-180| FT-AR020, FT-AR055, FT-AR089
#   notes TRUE / FALSE / "attr" / "applied"                 | FR-AR181/182| FT-AR030, FT-AR045, FT-AR067
#
# round_num()
#   half to even ("r") or away from zero ("sas"); the fuzz | FR-FM101-104| FT-FM084, FT-FM095
#   every formatter rounds by it; the option set once; an   | FR-FM109, FR-FM119, FR-FM140, FR-FM161, FR-FM187 | FT-FM081, FT-FM085
#     explicit rounding wins                                |             |
#   digits: decimals, 0 by default, a fraction cut          | FR-FM106/107/113 | FT-FM084, FT-FM085, FT-FM095
#   rounding in either case; NULL reads the option          | FR-FM108/109| FT-FM085, FT-FM086
#   a numeric vector the same length                        | FR-FM105    | FT-FM084
#
# fmt_signif()
#   digits counting the integer part; trailing zeros        | FR-FM129/130| FT-FM065, FT-FM066
#   a long integer part prints whole; decade recomputed     | FR-FM131-133| FT-FM052, FT-FM056, FT-FM091
#   small = "signif" / "fixed"; zero with digits - 1        | FR-FM135/137/138 | FT-FM075, FT-FM076, FT-FM080
#   character input stops                                   | FR-FM145    | FT-FM055
#   digits 3 by default; rounding; na for NA and NaN; Inf   | FR-FM136/139-142 | FT-FM048, FT-FM068, FT-FM069, FT-FM081, FT-FM091
#
# fmt_round()
#   every decimal printed, trailing zeros included          | FR-FM114    | FT-FM064
#   digits 2 by default; rounding; na; Inf                  | FR-FM115-122| FT-FM064, FT-FM068, FT-FM069, FT-FM072, FT-FM073, FT-FM091
#
# fmt_numeric()
#   the selected numeric columns to text; others untouched  | FR-FM148/149/152 | FT-FM051, FT-FM059, FT-FM061, FT-FM063
#   by: a carrier column, the stub itself, trimmed          | FR-FM155    | FT-FM077
#   formats: one of signif / digits, small, .default        | FR-FM156-158| FT-FM047, FT-FM050, FT-FM077
#   all-missing rows need no key; an unmatched key stops    | FR-FM159/171| FT-FM049, FT-FM053
#   cols by name or position; signif / digits without by    | FR-FM151/153/154/165 | FT-FM058, FT-FM059, FT-FM060
#   row count, column order unchanged                       | FR-FM149    | FT-FM061
#
# fmt_count_pct()
#   five branches, one width, the ")" aligned               | FR-FM175-179| FT-FM027-FT-FM032, FT-FM035
#   count: whole counts; 0 the count alone; missing: na     | FR-FM178/186| FT-FM028, FT-FM031
#   ... a fraction is an error                              | FR-FM249    | FT-FM100
#   pct: a fraction by default, recycled; above 1 warns     | FR-FM180-183/194 | FT-FM025, FT-FM032, FT-FM033, FT-FM035
#   pct_sign; nbsp                                          | FR-FM184/185| FT-FM031, FT-FM032, FT-FM037, FT-FM038
#   a missing percent: the count alone; Inf counts print    | FR-FM188/189| FT-FM029, FT-FM036
#   rounding once, before the branch                        | FR-FM187/190| FT-FM081, FT-FM082
#
# fmt_right_align()
#   non-empty cells right-justified to the widest           | FR-FM240/241| FT-FM012, FT-FM026
#   nbsp; every space in a padded cell is nbsp              | FR-FM243/244| FT-FM026, FT-FM094
#   na: NA empty, a token right-justified                   | FR-FM245/246| FT-FM013, FT-FM026
#   ... NaN is missing, Inf is not (code fixed)             | FR-FM250    | FT-FM101
#
# fmt_count_paren()
#   counts and inner text right-justified; others unchanged | FR-FM212-214| FT-FM006-FT-FM008
#   any notation inside the parentheses                     | FR-FM213    | FT-FM007
#   na: the token under the ones digit                      | FR-FM220-222| FT-FM003-FT-FM005, FT-FM009
#   ... NaN is missing, Inf is not (code fixed)             | FR-FM250    | FT-FM101
#   bare = TRUE pads a bare count                           | FR-FM216/217/224/226 | FT-FM001, FT-FM010, FT-FM011
#
# fmt_value_paren()
#   the value right-justified; the (...) block to one edge; | FR-FM228/229| FT-FM015, FT-FM017
#     "(100)" flush against its ")"                         |             |
#   a zero count drops its parenthetical; 100 loses decimals| FR-FM230-232| FT-FM017-FT-FM019, FT-FM022, FT-FM023
#   cells it cannot parse unchanged; nested not parsed      | FR-FM234/235| FT-FM020, FT-FM097
#   rounding of a parenthetical of 100 or more             | FR-FM248    | FT-FM099
#   na; nbsp                                                | FR-FM237/238| FT-FM021, FT-FM094
#   ... NaN is missing, Inf is not (code fixed)             | FR-FM250    | FT-FM101

# Invented counts: two arms, SEX tabulated (n and its N), in the shape cards
# writes.
.a6_rows <- function(g1l, var, lvl, sn, st, ctx = "tabulate", g1 = "TRT") {
  data.frame(group1 = g1, group1_level = g1l, variable = var,
             variable_level = lvl, context = ctx, stat_name = sn,
             stat_label = sn, stat = st, stringsAsFactors = FALSE)
}
.a6_sex <- function() {
  rbind(.a6_rows(c("A", "A", "B", "B"), "SEX", c("F", "M", "F", "M"), "n", c(3, 2, 4, 1)),
        .a6_rows(c("A", "A", "B", "B"), "SEX", c("F", "M", "F", "M"), "N", c(5, 5, 6, 6)))
}
# The same counts crossed with SEX as a column key (group2)
.a6_crossed <- function() {
  a <- .a6_sex()
  a <- a[a$stat_name == "n", , drop = FALSE]
  a$group2 <- "SEX"
  a$group2_level <- a$variable_level
  a
}
.a6_wide <- function(...) widen_ard(normalize_ard(.a6_sex()), cols = "TRT", notes = FALSE, ...)

# ---- cell_rows(), pull_ard() ----------------------------------------------------

test_that("FT-AR095 [FR-AR238]: cell_rows() needs at least one row", {
  expect_error(cell_rows(), "`cell_rows()` needs at least one row.", fixed = TRUE)
  expect_s3_class(cell_rows("{n}"), "cell_rows")
})

test_that("FT-AR096 [FR-AR239]: pull_ard(levels = ) is a named list; a plain vector is not read", {
  a <- .a6_sex()
  expect_identical(pull_ard(a, cols = "TRT"), c(A = 5, B = 6))
  expect_identical(pull_ard(a, cols = "TRT", levels = c("B", "A")), c(A = 5, B = 6))
  expect_identical(pull_ard(a, cols = "TRT", levels = list(TRT = c("B", "A"))), c(B = 6, A = 5))
})

test_that("FT-AR097 [FR-AR240]: with several cols, pull_ard(levels = ) is matched on the joined names", {
  pull <- function(lv) pull_ard(.a6_crossed(), cols = c("TRT", "SEX"), stat = "n",
                                variable = "SEX", levels = lv)
  expect_identical(pull(NULL), c(A____F = 3, A____M = 2, B____F = 4, B____M = 1))
  expect_identical(names(pull(list(TRT = c("B____M", "A____F")))),
                   c("B____M", "A____F", "A____M", "B____F"))
  expect_identical(names(pull(list(TRT = c("B", "A")))), names(pull(NULL)))
})

# ---- normalize_ard() -------------------------------------------------------------

test_that("FT-AR098 [FR-AR241]: normalize_ard(overall = ) needs a hierarchy", {
  h <- rbind(.a6_rows("A", "..ard_hierarchical_overall..", NA, "n", 5, ctx = "hierarchical"),
             .a6_rows("A", "AEDECOD", "Headache", "n", 2, ctx = "hierarchical"))
  h$group2 <- c(NA, "AEDECOD")
  h$group2_level <- c(NA, "Headache")
  expect_error(normalize_ard(h, overall = "Any event"),
               "`overall` labels the overall rows of a hierarchy: give `hierarchy` too",
               fixed = TRUE)
})

test_that("FT-AR099 [FR-AR242]: dropping ard_ignored leaves the table and shortens only the notes", {
  a <- rbind(.a6_sex(), .a6_rows(NA, "TRT", "A", "n", 5, g1 = NA))
  d <- normalize_ard(a, drop_key_variables = TRUE)
  expect_identical(attr(d, "ard_ignored")$reason, "a key variable's own tabulation")
  bare <- d
  attr(bare, "ard_ignored") <- NULL
  m1 <- paste(capture_messages(t1 <- widen_ard(d, cols = "TRT", cells = "{n}")), collapse = "")
  m2 <- paste(capture_messages(t2 <- widen_ard(bare, cols = "TRT", cells = "{n}")), collapse = "")
  expect_match(m1, "5 ARD rows were not used", fixed = TRUE)
  expect_match(m1, "a key variable's own tabulation", fixed = TRUE)
  expect_match(m2, "4 ARD rows were not used", fixed = TRUE)
  expect_no_match(m2, "own tabulation", fixed = TRUE)
  expect_identical(t1, t2)
})

# ---- widen_ard() -----------------------------------------------------------------

test_that("FT-AR100 [FR-AR243]: a cells guard may name the caller's variables", {
  lim <- 3
  t <- .a6_wide(cells = c(n > lim ~ "many", "{n}"))
  expect_identical(t$B, c("many", "1"))
  lim <- 0
  expect_identical(.a6_wide(cells = c(n > lim ~ "many", "{n}"))$A, c("many", "many"))
})

test_that("FT-AR101 [FR-AR244]: a cells list with no names is a chain, not a map", {
  expect_identical(.a6_wide(cells = list(n > 3 ~ "big", "{n}")),
                   .a6_wide(cells = c(n > 3 ~ "big", "{n}")))
  expect_identical(.a6_wide(cells = list(n > 3 ~ "big", "{n}"))$B, c("big", "1"))
})

test_that("FT-AR102 [FR-AR245]: labels from setNames() of two lengths stop", {
  lab <- stats::setNames(c("Female", "Male", "Other"), c("F", "M"))
  expect_error(.a6_wide(cells = "{n}", labels = lab),
               "`labels` must name every element; element(s) 3 have no name.", fixed = TRUE)
  expect_error(.a6_wide(cells = "{n}", labels = lab), "check that the two vectors are the same length",
               fixed = TRUE)
})

test_that("FT-AR103 [FR-AR246]: sort = \"sum(n)\" is \"n\"; \"min(n)\"; \"-n[A]\" totals the outer level's columns", {
  lab <- function(sort) .a6_wide(cells = "{n}", sort = sort)$label
  expect_identical(lab("n"), c("M", "F"))                  # 3 against 7
  expect_identical(.a6_wide(cells = "{n}", sort = "sum(n)"), .a6_wide(cells = "{n}", sort = "n"))
  expect_identical(lab("-min(n)"), c("F", "M"))            # 3 against 1
  # rows x / y under two arms crossed with SEX
  a <- rbind(.a6_rows(c("A", "A", "B", "B"), "AE", c("x", "y", "x", "y"), "n", c(1, 5, 9, 2)),
             .a6_rows(c("A", "A", "B", "B"), "AE", c("x", "y", "x", "y"), "n", c(2, 6, 10, 3)))
  a$group2 <- "SEX"
  a$group2_level <- rep(c("F", "M"), each = 4)
  d <- normalize_ard(a)
  srt <- function(s) widen_ard(d, cols = c("TRT", "SEX"), cells = "{n}", sort = s, notes = FALSE)
  expect_identical(srt("-n[A]")$label, c("y", "x"))        # 11 against 3
  expect_identical(srt("-n[B]")$label, c("x", "y"))        # 19 against 5
  expect_identical(srt("-n[A]")$A____F, c("5", "1"))
})

test_that("FT-AR104 [FR-AR247]: label = takes a bare formula and a chain of one formula", {
  age <- normalize_ard(rbind(
    .a6_rows(c("A", "B"), "AGE", NA, "mean", c(50, 52), ctx = "summary")))
  t <- widen_ard(age, cols = "TRT", cells = "{mean}", label = ~ "Age", notes = FALSE)
  expect_identical(t$label, "Age")
  expect_identical(t$A, "50")
  t <- .a6_wide(cells = "{n}", label = c(~ "{.label}!"))
  expect_identical(t$label, c("F!", "M!"))
  expect_identical(.a6_wide(cells = "{n}", label = ~ "{.label}!"), t)
})

test_that("FT-AR105 [FR-AR164 FR-AR248]: an unnamed cell_rows() line is one row per level, labelled from label", {
  t <- .a6_wide(cells = cell_rows("{n}"))
  expect_identical(t$label, c("F", "M"))
  expect_identical(t$A, c("3", "2"))
  expect_identical(t$B, c("4", "1"))
  expect_identical(t, .a6_wide(cells = "{n}"))
  # the guard and the chain are read per level
  t <- .a6_wide(cells = cell_rows(c(n < 2 ~ "few", "{n}/{N}")))
  expect_identical(t$A, c("3/5", "2/5"))
  expect_identical(t$B, c("4/6", "few"))
  # label = and levels = act on those rows as on a bare template's
  expect_identical(.a6_wide(cells = cell_rows("{n}"), label = ~ "{.label}!")$label,
                   c("F!", "M!"))
  expect_identical(as.character(.a6_wide(cells = cell_rows("{n}"),
                            levels = list(SEX = c("M", "F")))$label), c("M", "F"))
  # one label (a continuous summary): the one row, its label from `label`
  age <- normalize_ard(.a6_rows(c("A", "B"), "AGE", NA, "mean", c(50, 52), ctx = "summary"))
  t <- widen_ard(age, cols = "TRT", cells = cell_rows("{mean}"), label = ~ "Age", notes = FALSE)
  expect_identical(t$label, "Age")
  expect_identical(t$B, "52")
})

test_that("FT-AR106 [FR-AR249]: a named row on a variable with several levels stops, naming it", {
  msg <- "`cells` gives 'SEX' named rows ('n'), but 'SEX' has several levels ('F', 'M')."
  expect_error(.a6_wide(cells = c(n = "{n}")), msg, fixed = TRUE)
  expect_error(.a6_wide(cells = cell_rows(n = "{n}")), msg, fixed = TRUE)
  expect_error(.a6_wide(cells = cell_rows("{n}", "%" = "{N}")),
               "`cells` gives 'SEX' named rows ('%')", fixed = TRUE)
  # one level: the named rows read the whole record, as before
  one <- normalize_ard(.a6_rows(c("A", "B"), "SEX", "F", "n", c(3, 4)))
  t <- widen_ard(one, cols = "TRT", cells = cell_rows("1" = "{n}", "2" = "[{n}]"),
                 label = NA, notes = FALSE)
  expect_identical(t$A, c("3", "[3]"))
})

# ---- the number formats ---------------------------------------------------------

test_that("FT-FM100 [FR-FM249]: fmt_count_pct() takes whole counts only", {
  expect_error(fmt_count_pct(2.5, 0.5), "`count` must be whole numbers; got 2.5.", fixed = TRUE)
  expect_identical(fmt_count_pct(2, 0.5, nbsp = " "), "  2 (50.0)")
})

test_that("FT-FM101 [FR-FM250]: a numeric NaN is missing to the cell formats, an Inf is not", {
  expect_identical(fmt_right_align(c(NaN, Inf, 12), na = "-", nbsp = " "), c("  -", "Inf", " 12"))
  expect_identical(fmt_right_align(c(NaN, 12), nbsp = " "), c("", "12"))
  expect_identical(fmt_count_paren(c(NaN, Inf), na = "-", nbsp = " "), c("-", "Inf"))
  expect_identical(fmt_value_paren(c(NaN, -Inf, 12), na = "-", nbsp = " "),
                   c("   -", "-Inf", "  12"))
})
