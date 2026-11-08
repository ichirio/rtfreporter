# Help-to-FR audit, batch 4: the plan verbs (part 1), from table_plan() to
# plan_paginate_group().
#
# The help is the functional requirement: every statement in a function's Rd
# is an active FR in inst/validation/requirements.csv, and every FR has a
# test.  For each function below, one row per claim of its roxygen: the
# claim (short), the FR that states it, and the test that checks it.  The
# tests below this table close the gaps the audit found.  plan_cells(),
# plan_digits() and plan_stub() had their pairwise tests already
# (test-validation-plan-cells.R); only their other claims are new here.
#
# table_plan()
#   claim                                                   | FR          | test
#   deferred: the data held, nothing run until plan_apply() | FR-PL101    | FT-PL001
#   last wins across layers; specificity within one        | FR-IX257    | FT-IX010
#   x: normalized / long / wide / records; kind by columns  | FR-PL102/117-120 | FT-PL009, FT-PL005, FT-PL011, FT-PL019, FT-PL015
#   x: a raw cards ARD refused with the line to write       | FR-PL126    | FT-PL007
#   cols: one key, several keys joined                      | FR-PL105/106| FT-PL004, FT-PL045
#   cols renamed c(new = old): verbs name it by new name    | FR-PL922    | FT-PL932
#   cols: a factor's levels order the columns               | FR-PL122    | FT-PL049
#   cols left out, one arm: one column All                  | FR-PL107    | FT-PL041
#   rows: names are the output columns                      | FR-PL108    | FT-PL010
#   rows left out: the analysis variable, as group          | FR-PL109    | FT-PL027
#   rows unnamed: the key keeps its name (help fixed)       | FR-PL921    | FT-PL931
#   label: .label default, NA none, coalesce, guarded       | FR-PL110-113| FT-PL010, FT-PL037, FT-PL024
#   show_keys: a key's own counts in the body; NULL none    | FR-PL115/116| FT-PL053
#   show_keys: a column key refused                         | FR-PL132    | FT-PL053
#   stat: the frame's own names for the three parts; guards | FR-PL114/128/129 | FT-PL022, FT-PL052
#   value: a records plan; guards                           | FR-PL902    | FT-PL904
#   value: two records of one key and column stop (#835)    | FR-PL943    | FT-PL952
#   roles only: the other settings belong to the verbs      | FR-PL101    | FT-PL001
#   no x / a role not a column / no cols with a group       | FR-PL125/127/131 | FT-PL021, FT-PL008, FT-PL041
#
# plan_cells()  (pairwise: test-validation-plan-cells.R)
#   templates, keyed by specificity, named rows, guards     | FR-PL226-228| FT-PL143, FT-PL916
#   stat groups: split versus .others                       | FR-PL239    | FT-PL917
#   reserved names; stats / value / na / notes              | FR-PL230-235| FT-PL145, FT-PL146, FT-PL147
#   {x:stat_fmt} / an open {x} reads stat_fmt              | FR-IX201/PL917 | FT-PL145, FT-PL921
#   held however the plan is run (rtf_tables(doc, plan))    | FR-PL232    | FT-PL105
#
# plan_digits()  (pairwise: test-validation-plan-cells.R)
#   open tokens only; by key, by statistic, "4s"            | FR-PL240-242| FT-PL148, FT-PL919
#   a later layer wins key by key, falling through          | FR-PL916    | FT-PL920
#   a row key's digits value by value                       | FR-PL908/909| FT-PL912
#   a finished table: a column, .rows; errors               | FR-PL244-247| FT-PL150, FT-PL922
#   rounding: the family, last wins                         | FR-PL243    | FT-PL149
#   rounding NULL follows options(rtfreporter.rounding);    | FR-PL923    | FT-PL933
#     an explicit rounding wins over it                     |             |
#
# plan_stub()  (pairwise: test-validation-plan-cells.R)
#   vars / name / heading / group_summary / before          | FR-PL360-368, FR-PL912 | FT-PL273, FT-PL274, FT-PL914
#   indent: non-breaking spaces, 4 by default               | FR-PL924    | FT-PL934
#   before = TRUE: a folded key no longer pages             | FR-PL925    | FT-PL935
#   before = FALSE refused with a cell style                | FR-IX253    | FT-PL211
#
# plan_columns()
#   widths by name / unnamed / repeated; unknown name       | FR-PL157-160/176 | FT-PL037, FT-PL026, FT-PL043, FT-PL046
#   decimal / row_title by name or position                 | FR-PL161-165/177/178 | FT-PL013, FT-PL028, FT-PL037, FT-PL038
#   auto_width / sep / cell_format / column_widths_twips    | FR-PL166-172| FT-PL028, FT-PL029, FT-PL032, FT-PL047
#   a key value containing sep refused                      | FR-PL180    | FT-PL068
#
# plan_col_header()
#   rtf_col_header(), short rows repeated, {col}, {n}       | FR-PL373-376| FT-PL212-FT-PL216
#   {col1}, {col2}; {n:sum}, {n:<column>}                   | FR-PL376/377| FT-PL215-FT-PL219
#   {n1}, {n2}: the number at a depth of the keys           | FR-PL926    | FT-PL936
#   a data frame of cells / lines and span                  | FR-PL381/382| FT-PL224, FT-PL226
#   a function of n (and the table)                         | FR-PL380    | FT-PL222
#   values: TRUE, page / table, numbers, named tokens, frame| FR-PL383-386| FT-PL220, FT-PL229, FT-PL277
#   values: a function of the data                          | FR-PL927    | FT-PL936
#   NA with a warning; header_sep / col_header_align        | FR-PL378/387/388 | FT-PL220, FT-PL029, FT-PL032
#
# plan_levels()
#   a key's order, the value columns or the rows           | FR-PL251    | FT-PL152
#   variable = : the blocks' order                          | FR-PL252    | FT-PL152
#   variable = c(AGE = "Age (years)"): the block's heading  | FR-PL928    | FT-PL937
#   a named element is the value's text; labels win         | FR-PL253, FR-IX200 | FT-PL108
#   merged one key at a time; a whole map                   | FR-PL254    | FT-PL153
#   .drop_empty; several calls add up; a long frame (fixed) | FR-PL256/929| FT-PL154, FT-PL938
#
# plan_labels()
#   one text: a value / a variable's heading                | FR-PL258    | FT-PL106
#   a dictionary for one key, column or row                 | FR-PL257    | FT-PL153
#   the variable's own name in it is its heading            | FR-PL930    | FT-PL939
#   one code, two axes, two texts                           | FR-PL931    | FT-PL940
#   named by itself or by plan_levels()' text; wins         | FR-PL260, FR-IX200 | FT-PL108
#   a later call replaces an entry whole                    | FR-PL257    | FT-PL153
#   one unnamed named vector is the map; anywhere in pipe   | FR-PL932    | FT-PL941
#
# plan_sort()
#   columns, statistics, [col], @key, max/min, levels(),    | FR-PL304-312| FT-PL122, FT-PL123, FT-PL164, FT-PL165
#     .overall / .depth, TRUE / FALSE, stat, keep           |             |
#   on a listing it sorts the records                       | FR-PL933    | FT-PL942
#
# plan_filter()
#   conditions over statistics and keys, all holding        | FR-PL135/140/141 | FT-PL036, FT-PL057, FT-PL058
#   unanswered records, notes; columns stay; overall stays  | FR-PL136-138| FT-PL054, FT-PL055, FT-PL036
#   any / parents / by                                      | FR-PL142-146| FT-PL036, FT-PL059
#
# plan_blanks()
#   between_groups / records / none / a rule                | FR-PL183/188/189/196 | FT-PL039, FT-PL073, FT-PL031, FT-PL048
#   positions after rows; 0 before the first row           | FR-PL187/934| FT-PL072, FT-PL943
#   -1 after the last row (as_rtftables(), fixed in #833)   | FR-TB139    | FT-TB754
#   first / last on every page                              | FR-PL184/185| FT-PL039
#   a listing's first = FALSE drops the multiline top row   | FR-PL935    | FT-PL944
#   counted = TRUE counts toward max_rows                   | FR-PL186    | FT-PL071
#   counted = FALSE: a page may print more than max_rows    | FR-PL936    | FT-PL945
#
# plan_total()
#   overall rows as a Total column; label / position        | FR-PL264/265| FT-PL156
#   two column keys: the innermost folded                   | FR-PL268    | FT-PL111
#   direction rows / both; only the body's analyses         | FR-PL267/269| FT-PL033, FT-PL114
#   {n} over it the study total                             | FR-PL266    | FT-PL113
#
# plan_titles() / plan_footnotes()
#   one block on every page, attr rtf_titles / rtf_footnotes| FR-PL486    | FT-PL261
#   strings and character vectors make one block, in order  | FR-PL937    | FT-PL946
#   pages = one block per page; not both                    | FR-PL487/491| FT-PL262, FT-PL264
#
# plan_paginate_rows()
#   max_rows / split / break_before / min_group_rows /      | FR-PL432-436| FT-PL284, FT-PL926
#     cont_label                                            |             |
#   split = a function of one's own                         | FR-PL939    | FT-PL948
#   page_by first, the budget within; heading / heading_at  | FR-PL437-439| FT-PL235, FT-PL256, FT-PL257
#   several page_by: a token per column, no {value}         | FR-PL938, FR-PL442 (corrected) | FT-PL947
#   max_rows, split left out: "group_safe" (#836); "none"   | FR-PL944, FR-PL446 | FT-PL953
#     written out with max_rows stops                       |             |
#
# plan_paginate_cols()
#   at / every / keep / fit / order / col_header            | FR-PL461-467| FT-PL258, FT-PL259, FT-PL288
#   cut_by: blocks, a separator, or a key per column        | FR-PL463/940| FT-PL260, FT-PL949
#   keep NULL: the row titles (column 1 or plan_columns())  | FR-PL941    | FT-PL950
#
# plan_paginate_group()
#   a page per value; the section it opens                  | FR-PL449/450| FT-PL251, FT-PL252
#   col NULL: the outermost row key; keep                   | FR-PL451/452| FT-PL287
#   heading {value} {n}; heading_at                         | FR-PL453/454| FT-PL255, FT-PL257
#   heading: {PROGRAM}, {DATETIME}, page tokens left as is  | FR-PL942    | FT-PL951

# Invented statistics of two arms, one row a statistic
.ap_st <- function() {
  data.frame(
    ARM       = rep(c("Drug A", "Placebo"), each = 4),
    variable  = rep(c("AGE", "AGE", "SEX", "SEX"), 2),
    .label    = rep(c(NA, NA, "F", "M"), 2),
    .kind     = rep(c("continuous", "continuous", "categorical", "categorical"), 2),
    stat_name = rep(c("mean", "sd", "n", "n"), 2),
    stat      = c(50.123, 10.456, 3, 4, 52.987, 9.111, 5, 2),
    stringsAsFactors = FALSE)
}
.ap_cells <- function(p) {
  plan_cells(p, continuous = c("Mean (SD)" = "{mean:.1f} ({sd:.2f})"),
             categorical = "{n:d}", notes = FALSE)
}
.ap_plan <- function(d = .ap_st(), ...) {
  .ap_cells(table_plan(d, cols = "ARM", rows = c(group = "variable"), ...))
}
.ap_nrows <- function(pages) vapply(pages, function(pg) nrow(pg$data), integer(1L))

test_that("FT-PL931 [FR-PL921]: table_plan(rows = ) left unnamed keeps the key's column name", {
  d <- .ap_st()
  tab <- function(...) names(plan_apply(.ap_cells(table_plan(d, cols = "ARM", ...)), "table"))
  expect_identical(tab(rows = "variable")[1L], "variable")
  expect_identical(tab(rows = c(block = "variable"))[1L], "block")
  # left out altogether, the analysis variable is the key, as `group`
  expect_identical(tab()[1L], "group")
})

test_that("FT-PL932 [FR-PL922]: table_plan(cols = c(new = old)) lets later verbs name the key by its new name", {
  d <- .ap_st()
  renamed <- .ap_cells(table_plan(d, cols = c(Arm = "ARM"), rows = c(group = "variable"))) |>
    plan_levels(Arm = c("Placebo", "Drug A"))
  plain <- .ap_plan(d) |> plan_levels(ARM = c("Placebo", "Drug A"))
  out <- plan_apply(renamed, "table")
  expect_identical(names(out), c("group", "label", "Placebo", "Drug A"))
  expect_identical(out, plan_apply(plain, "table"))
})

test_that("FT-PL933 [FR-PL923]: plan_digits() follows options(rtfreporter.rounding) unless its rounding = says", {
  d <- .ap_st()
  d$stat[1L] <- 50.25
  p <- table_plan(d, cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(continuous = c(M = "{mean}"), categorical = "{n:d}", notes = FALSE) |>
    plan_digits(1)
  cell <- function(p) plan_apply(p, "table")[["Drug A"]][1L]
  old <- options(rtfreporter.rounding = NULL)
  on.exit(options(old), add = TRUE)
  expect_identical(cell(p), "50.2")                     # half to even, R's own
  options(rtfreporter.rounding = "sas")
  expect_identical(cell(p), "50.3")                     # the study's rule
  expect_identical(cell(p |> plan_digits(rounding = "r")), "50.2")
  options(rtfreporter.rounding = "r")
  expect_identical(cell(p |> plan_digits(rounding = "sas")), "50.3")
})

test_that("FT-PL934 [FR-PL924]: plan_stub() indents each level by four non-breaking spaces by default", {
  p <- .ap_plan()
  nb <- function(k) strrep(" ", k)
  for (before in c(FALSE, TRUE)) {
    stub <- plan_apply(p |> plan_stub(name = "Char", before = before), "pages")[[1L]]$data$Char
    expect_identical(stub, c("AGE", paste0(nb(4), "Mean (SD)"), "SEX",
                             paste0(nb(4), c("F", "M"))), info = before)
  }
  stub <- plan_apply(p |> plan_stub(name = "Char", indent = 2), "pages")[[1L]]$data$Char
  expect_identical(stub[2L], paste0(nb(2), "Mean (SD)"))
})

test_that("FT-PL935 [FR-PL925]: plan_stub(before = TRUE) folds the row keys away, so plan_paginate_group() can no longer page on one", {
  p <- .ap_plan()
  pages <- plan_apply(p |> plan_stub(name = "Char") |> plan_paginate_group(col = "group"))
  expect_identical(names(pages), c("AGE", "SEX"))
  expect_error(plan_apply(p |> plan_stub(name = "Char", before = TRUE) |>
                            plan_paginate_group(col = "group")),
               "`group_col` 'group' not found in the table.", fixed = TRUE)
})

test_that("FT-PL936 [FR-PL926 FR-PL927]: plan_col_header() {n1} / {n2} name a depth of the keys; values may be a function of the data", {
  d <- .ap_st()
  d2 <- rbind(transform(d, GRP = "F"), transform(d, GRP = "M"))
  p <- table_plan(d2, cols = c("ARM", "GRP"), rows = c(group = "variable")) |>
    plan_cells(continuous = c(M = "{mean:.1f}"), categorical = "{n:d}", notes = FALSE)
  nv <- c(`Drug A` = 10, Placebo = 12, `Drug A____F` = 4, `Drug A____M` = 6,
          `Placebo____F` = 5, `Placebo____M` = 7)
  hdr <- plan_apply(p |> plan_col_header(rtf_col_header(c("", "{col1}/{col2}/{n1}/{n2}")),
                                         values = list(n = nv)))[[1L]]$col_header[[1L]]
  expect_identical(hdr[3:6], c("Drug A/F/10/4", "Drug A/M/10/6", "Placebo/F/12/5", "Placebo/M/12/7"))
  # a function of the data, returning the numbers
  p1 <- .ap_plan(d)
  seen <- NULL
  f <- function(data) {
    seen <<- nrow(data)
    c(`Drug A` = 9, Placebo = 11)
  }
  hdr <- plan_apply(p1 |> plan_col_header(rtf_col_header(c("", "{col} N={n}")),
                                          values = f))[[1L]]$col_header[[1L]]
  expect_identical(hdr[3:4], c("Drug A N=9", "Placebo N=11"))
  expect_identical(seen, nrow(d))
  hdr <- plan_apply(p1 |> plan_col_header(rtf_col_header(c("", "{col} N={n}")),
                                          values = list(n = f)))[[1L]]$col_header[[1L]]
  expect_identical(hdr[3:4], c("Drug A N=9", "Placebo N=11"))
})

test_that("FT-PL937 [FR-PL928]: plan_levels(variable = ) with a named element orders the blocks and gives the heading", {
  out <- plan_apply(.ap_plan() |> plan_levels(variable = c(SEX = "Sex", "AGE")), "table")
  expect_identical(as.character(out$group), c("Sex", "Sex", "AGE"))
  expect_identical(out$label, c("F", "M", "Mean (SD)"))
})

test_that("FT-PL938 [FR-PL929]: plan_levels(.drop_empty = ) calls add up, on a long frame built without cards too", {
  d <- data.frame(ARM = rep(c("A", "B"), each = 4),
                  variable = rep(c("SEX", "SEX", "RACE", "RACE"), 2),
                  .label = rep(c("F", "U", "W", "X"), 2), .kind = "categorical",
                  stat_name = "n", stat = c(3, 0, 2, 0, 4, 0, 1, 0))
  p <- table_plan(d, cols = "ARM", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n:d}", notes = FALSE)
  lab <- function(p) plan_apply(p, "table")$label
  expect_identical(lab(p), c("F", "U", "W", "X"))
  # a long frame has no variable_level: the label column is the level
  # (this did nothing before the audit)
  expect_identical(lab(p |> plan_levels(.drop_empty = "SEX")), c("F", "W", "X"))
  expect_identical(lab(p |> plan_levels(.drop_empty = "SEX") |> plan_levels(.drop_empty = "RACE")),
                   c("F", "W"))
  expect_identical(lab(p |> plan_levels(.drop_empty = c("SEX", "RACE"))), c("F", "W"))
})

test_that("FT-PL939 [FR-PL930]: plan_labels(<variable> = c(<variable> = )) is the block's heading beside its levels' text", {
  pg <- plan_apply(.ap_plan() |> plan_labels(SEX = c(SEX = "Sex, n", F = "Female")) |>
                     plan_stub(name = "Char"), "pages")[[1L]]
  nb <- strrep(" ", 4)
  expect_identical(pg$data$Char, c("AGE", paste0(nb, "Mean (SD)"), "Sex, n",
                                   paste0(nb, c("Female", "M"))))
})

test_that("FT-PL940 [FR-PL931]: plan_labels() gives one code two texts on two axes", {
  sh <- data.frame(BL = rep(c("0", "1"), each = 2), variable = "GR",
                   .label = rep(c("0", "1"), 2), .kind = "categorical",
                   stat_name = "n", stat = c(5, 1, 2, 3))
  out <- table_plan(sh, cols = "BL", rows = c(group = "variable")) |>
    plan_cells(categorical = "{n:d}", notes = FALSE) |>
    plan_labels(GR = c("0" = "Grade 0", "1" = "Grade 1"),
                BL = c("0" = "Baseline 0", "1" = "Baseline 1")) |>
    plan_apply("table")
  expect_identical(names(out), c("group", "label", "Baseline 0", "Baseline 1"))
  expect_identical(out$label, c("Grade 0", "Grade 1"))
  expect_identical(out[["Baseline 0"]], c("5", "1"))
})

test_that("FT-PL941 [FR-PL932]: plan_labels(<one named vector>) is the whole map, and may come before plan_cells()", {
  d <- .ap_st()
  labs <- c(AGE = "Age (years)", SEX = "Sex")
  early <- table_plan(d, cols = "ARM", rows = c(group = "variable")) |>
    plan_labels(labs) |> .ap_cells()
  late <- .ap_plan(d) |> plan_labels(AGE = "Age (years)", SEX = "Sex")
  out <- plan_apply(early, "table")
  expect_identical(as.character(out$group), c("Age (years)", "Sex", "Sex"))
  expect_identical(out, plan_apply(late, "table"))
})

test_that("FT-PL942 [FR-PL933]: plan_sort() on a listing sorts the records, a leading - descending", {
  recs <- data.frame(USUBJID = c("02", "01", "01"),
                     AETERM = c("Fatigue", "Nausea", "Headache"))
  lst <- function(...) {
    pg <- plan_apply(table_plan(recs) |> plan_sort(...) |>
                       plan_listing(listing_col("USUBJID"), listing_col("AETERM")))[[1L]]
    x <- pg$data$AETERM
    x[nzchar(trimws(x))]
  }
  expect_identical(lst("USUBJID", "AETERM"), c("Headache", "Nausea", "Fatigue"))
  expect_identical(lst("USUBJID", "-AETERM"), c("Nausea", "Headache", "Fatigue"))
})

test_that("FT-PL943 [FR-PL934]: plan_blanks(where = 0) puts a blank row before the first row of each page", {
  t4 <- data.frame(A = letters[1:4], B = 1:4)
  expect_identical(as.integer(plan_apply(table_plan(t4) |> plan_blanks(where = c(0, 2)))[[1L]]$blank_rows),
                   c(0L, 2L))
  pages <- plan_apply(table_plan(t4) |> plan_paginate_rows(max_rows = 2, split = "group_force") |>
                        plan_blanks(where = 0))
  expect_length(pages, 2L)
  for (pg in pages) expect_identical(as.integer(pg$blank_rows), 0L)
})

test_that("FT-PL944 [FR-PL935]: plan_blanks(first = FALSE) takes away the blank row a multiline listing puts at the top", {
  recs <- data.frame(USUBJID = c("01", "01", "02"),
                     AETERM = c("Headache", "Nausea", "Fatigue"))
  p <- table_plan(recs) |>
    plan_listing(listing_col("USUBJID"), listing_col("AETERM"), type = "multiline")
  expect_identical(as.integer(plan_apply(p)[[1L]]$blank_rows), 0L)
  expect_length(plan_apply(p |> plan_blanks(first = FALSE))[[1L]]$blank_rows, 0L)
})

test_that("FT-PL945 [FR-PL936]: plan_blanks(counted = FALSE), the default, lets a page print more rows than max_rows", {
  t9 <- data.frame(G = rep(c("a", "b", "c"), each = 3), V = 1:9)
  base <- table_plan(t9) |>
    plan_paginate_rows(max_rows = 4, split = "group_force", min_group_rows = 0)
  printed <- function(pages) .ap_nrows(pages) + lengths(lapply(pages, `[[`, "blank_rows"))
  pages <- plan_apply(base |> plan_blanks(where = "between_groups"))
  expect_identical(.ap_nrows(pages), c(4L, 4L, 3L))
  expect_true(any(printed(pages) > 4L))
  pages <- plan_apply(base |> plan_blanks(where = "between_groups", counted = TRUE))
  expect_true(all(printed(pages) <= 4L))
})

test_that("FT-PL946 [FR-PL937]: plan_titles() / plan_footnotes() join strings and character vectors into one block on every page", {
  t9 <- data.frame(G = rep(c("a", "b", "c"), each = 3), V = 1:9)
  pages <- plan_apply(table_plan(t9) |>
                        plan_paginate_rows(max_rows = 5, split = "group_force") |>
                        plan_titles("Table 1", c("Line 2", "Line 3")) |>
                        plan_footnotes(c("Note a", "Note b"), "Note c"))
  expect_gt(length(pages), 1L)
  for (pg in pages) {
    expect_identical(attr(pg, "rtf_titles"), c("Table 1", "Line 2", "Line 3"))
    expect_identical(attr(pg, "rtf_footnotes"), c("Note a", "Note b", "Note c"))
  }
})

test_that("FT-PL947 [FR-PL938 FR-PL442]: plan_paginate_rows() with several page_by columns names its pages by a token per column", {
  lab <- data.frame(Period = rep(c("P1", "P2"), each = 4),
                    Site = rep(c("S1", "S1", "S2", "S2"), 2), ALT = 1:8)
  pages <- plan_apply(table_plan(lab) |>
                        plan_paginate_rows(page_by = c("Period", "Site"),
                                           heading = "Period {Period}, site {Site}",
                                           heading_at = "title"))
  expect_identical(names(pages), c("Period P1, site S1", "Period P1, site S2",
                                   "Period P2, site S1", "Period P2, site S2"))
  expect_identical(attr(pages[[1L]], "rtf_heading_at"), "title")
  msg <- "with several, the heading carries a token for each column"
  expect_error(plan_paginate_rows(table_plan(lab), page_by = c("Period", "Site"),
                                  heading = "{value}"), msg, fixed = TRUE)
  expect_error(plan_paginate_rows(table_plan(lab), page_by = c("Period", "Site"),
                                  heading = "{Period}"), msg, fixed = TRUE)
  # one column: {value}
  pages <- plan_apply(table_plan(lab) |> plan_paginate_rows(page_by = "Period", heading = "Period {value}"))
  expect_identical(names(pages), c("Period P1", "Period P2"))
})

test_that("FT-PL948 [FR-PL939]: plan_paginate_rows(split = <function>) is a split of one's own", {
  t8 <- data.frame(A = letters[1:8], B = 1:8)
  pages <- plan_apply(table_plan(t8) |>
                        plan_paginate_rows(split = function(df, ...) list(df[1:3, ], df[4:8, ])))
  expect_identical(.ap_nrows(pages), c(3L, 5L))
  expect_identical(pages[[2L]]$data$A, letters[4:8])
})

test_that("FT-PL949 [FR-PL940]: plan_paginate_cols(cut_by = <one key per column>) makes each run of one key a block", {
  w <- data.frame(P = c("x", "y"), D1 = 1:2, D2 = 3:4, D3 = 5:6, D4 = 7:8)
  pages <- plan_apply(table_plan(w) |> plan_paginate_cols(cut_by = c("", "a", "a", "b", "b")))
  expect_identical(lapply(pages, function(pg) names(pg$data)),
                   list(c("P", "D1", "D2"), c("P", "D3", "D4")))
})

test_that("FT-PL950 [FR-PL941]: plan_paginate_cols(keep = NULL) repeats the row titles: column 1, or plan_columns(row_title = )", {
  w <- data.frame(P = c("x", "y"), D1 = 1:2, D2 = 3:4, D3 = 5:6, D4 = 7:8)
  cols_of <- function(p) lapply(plan_apply(p), function(pg) names(pg$data))
  expect_identical(cols_of(table_plan(w) |> plan_paginate_cols(every = 2)),
                   list(c("P", "D1", "D2"), c("P", "D3", "D4")))
  expect_identical(cols_of(table_plan(w) |> plan_columns(row_title = 1:2) |>
                             plan_paginate_cols(every = 2)),
                   list(c("P", "D1", "D2", "D3"), c("P", "D1", "D4")))
})

test_that("FT-PL951 [FR-PL942]: plan_paginate_group(heading = ) leaves {PROGRAM}, {DATETIME} and the page tokens for the renderer", {
  lab <- data.frame(PARAM = rep(c("ALT", "AST"), each = 2), V = 1:4)
  pages <- plan_apply(table_plan(lab) |>
                        plan_paginate_group(col = "PARAM",
                                            heading = "{value} {PROGRAM} {DATETIME} {PAGE}"))
  expect_identical(names(pages), c("ALT {PROGRAM} {DATETIME} {PAGE}",
                                   "AST {PROGRAM} {DATETIME} {PAGE}"))
})

test_that("FT-PL952 [FR-PL943]: table_plan(value = ): two records of one key and column stop, naming them", {
  rec <- data.frame(USUBJID = c("S1", "S1", "S2"), AGE = c(30, 30, 41),
                    VIS = c("W1", "W1", "W1"), AVAL = c(1, 2, 3))
  tab <- function(d, rows) {
    plan_apply(table_plan(d, cols = "VIS", rows = rows, value = "AVAL") |>
                 plan_cells("{value}"), "table")
  }
  # a plain key and a template over the record: both stop, the key as resolved
  err <- expect_error(tab(rec, c(subject = "USUBJID")), "Two rows reach one cell")
  expect_match(conditionMessage(err), "subject = 'S1', column 'W1'", fixed = TRUE)
  expect_match(conditionMessage(err), "'1', '2'", fixed = TRUE)
  expect_match(conditionMessage(err), "two records of one key and column", fixed = TRUE)
  err <- expect_error(tab(rec, c(subject = ~ "{USUBJID}/{AGE}")), "Two rows reach one cell")
  expect_match(conditionMessage(err), "subject = 'S1/30', column 'W1'", fixed = TRUE)
  # the same value twice is one value; a column that tells them apart passes
  same <- rec
  same$AVAL <- c(1, 1, 3)
  expect_identical(tab(same, c(subject = "USUBJID"))$W1, c("1", "3"))
  apart <- rec
  apart$VIS <- c("W1", "W2", "W1")
  out <- tab(apart, c(subject = "USUBJID"))
  expect_identical(out$W2[out$subject == "S1"], "2")
})

test_that("FT-PL953 [FR-PL944]: plan_paginate_rows(max_rows = ) with the split left out is group_safe; with \"none\" it stops", {
  t9 <- data.frame(G = rep(c("a", "b", "c"), each = 3), V = 1:9)
  alone <- plan_apply(table_plan(t9) |> plan_paginate_rows(max_rows = 4))
  safe <- plan_apply(table_plan(t9) |> plan_paginate_rows(max_rows = 4, split = "group_safe"))
  expect_gt(length(alone), 1L)
  expect_identical(.ap_nrows(alone), .ap_nrows(safe))
  expect_identical(lapply(alone, `[[`, "data"), lapply(safe, `[[`, "data"))
  expect_identical(rtfreporter:::.plan_rtf_args(
    table_plan(t9) |> plan_paginate_rows(max_rows = 4))$split, "group_safe")
  expect_error(plan_apply(table_plan(t9) |> plan_paginate_rows(max_rows = 4, split = "none")),
               "plan_paginate_rows(max_rows = 4) has no effect with split = 'none'.", fixed = TRUE)
  # no budget, no split: the split stays the default, "none"
  expect_null(rtfreporter:::.plan_rtf_args(
    table_plan(t9) |> plan_paginate_rows(min_group_rows = 0))$split)
})
