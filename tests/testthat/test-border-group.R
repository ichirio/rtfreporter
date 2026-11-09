## tests/testthat/test-border-group.R
##
## plan_style(border_group = ) / rtf_table_style(border_group = ): a rule
## under the last row of each row group, written by as_rtftables().

library(testthat)

# Three groups (Age, Sex, Race), each a heading row and two indented members.
.bg_df <- function() {
  data.frame(
    Item = c("Age", " Mean", " SD", "Sex", " Female", " Male",
             "Race", " Asian", " White"),
    Value = as.character(1:9), stringsAsFactors = FALSE)
}

# RTF text of a list of pages.
.bg_rtf <- function(pages) {
  f <- tempfile(fileext = ".rtf")
  generate_rtfreport(rtf_document() |> rtf_tables(pages), f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# The bottom-rule count (\clbrdrb\brdrs) per \trowd block.
.bg_bottoms <- function(rtf) {
  rows <- unlist(strsplit(rtf, "\\\\trowd"))[-1]
  vapply(rows, function(r)
    length(regmatches(r, gregexpr("\\\\clbrdrb\\\\brdrs", r))[[1]]),
    integer(1L), USE.NAMES = FALSE)
}

.bg_rule <- rtf_border(bottom = TRUE)

test_that("border_group is a zone: NULL or an rtf_border, kept by name", {
  s <- rtf_table_style(border_group = .bg_rule)
  expect_s3_class(s$border_group, "rtf_border")
  expect_null(rtf_table_style()$border_group)
  expect_error(rtf_table_style(border_group = "x"),
               "`border_group` must be NULL or an rtf_border object")
  expect_error(.rtf_table_border(group = 1),
               "`group` must be NULL or an rtf_border object")
  expect_s3_class(.style_to_table_border(s)$group, "rtf_border")
  expect_false("group" %in% names(.style_to_table_border(rtf_table_style())))
  expect_output(print(s), "group")
  expect_s3_class(
    plan_style(table_plan(.bg_df()), border_group = .bg_rule), "table_plan")
})

test_that("a rule goes under the last row of each group but the final one", {
  pg <- as_rtftables(.bg_df(), group_by = "indent",
                     style = rtf_table_style(border_group = .bg_rule))
  expect_length(pg, 1L)
  b <- .bg_bottoms(.bg_rtf(pg))
  # header row, then 9 body rows: the rule is under rows 3 and 6 (SD, Male)
  expect_equal(b, c(0L, 0L, 0L, 2L, 0L, 0L, 2L, 0L, 0L, 0L))
})

test_that("border_last_row wins on the last row, with no double line", {
  pg <- as_rtftables(.bg_df(), group_by = "indent",
                     style = rtf_table_style(
                       border_group = .bg_rule,
                       border_last_row = rtf_border(bottom = TRUE)))
  b <- .bg_bottoms(.bg_rtf(pg))
  expect_equal(b, c(0L, 0L, 0L, 2L, 0L, 0L, 2L, 0L, 0L, 2L))
})

test_that("through plan_style(): the same table, with plan_row_group()", {
  tb <- .bg_df()
  direct <- as_rtftables(tb, group_by = "indent",
                         style = rtf_table_style(border_group = .bg_rule))
  viaplan <- table_plan(tb) |>
    plan_row_group(mode = "indent") |>
    plan_style(border_group = .bg_rule) |>
    plan_apply()
  expect_identical(.bg_rtf(viaplan), .bg_rtf(direct))
})

test_that("default NULL: the RTF is byte-identical to a table without it", {
  tb <- .bg_df()
  hdr <- rtf_border(top = TRUE, bottom = TRUE)
  nul <- table_plan(tb) |> plan_row_group(mode = "indent") |>
    plan_style(border_header = hdr, border_group = NULL) |> plan_apply()
  zone <- table_plan(tb) |> plan_row_group(mode = "indent") |>
    plan_style(border_header = hdr) |> plan_apply()
  expect_identical(.bg_rtf(nul), .bg_rtf(zone))
  expect_identical(
    .bg_rtf(as_rtftables(tb, group_by = "indent",
                         style = rtf_table_style(border_group = NULL))),
    .bg_rtf(as_rtftables(tb, group_by = "indent",
                         style = rtf_table_style())))
})

test_that("no groups said, or one group: nothing changes", {
  tb  <- .bg_df()
  st0 <- rtf_table_style()
  st1 <- rtf_table_style(border_group = .bg_rule)
  # no group_by / group_col: the table has no groups
  expect_identical(.bg_rtf(as_rtftables(tb, style = st1)),
                   .bg_rtf(as_rtftables(tb, style = st0)))
  # one group: a heading with all the rest under it
  one <- data.frame(Item = c("Age", " Mean", " SD"), Value = c("1", "2", "3"),
                    stringsAsFactors = FALSE)
  expect_identical(
    .bg_rtf(as_rtftables(one, group_by = "indent", style = st1)),
    .bg_rtf(as_rtftables(one, group_by = "indent", style = st0)))
})

test_that("a group split across pages has its rule under its real last row only", {
  # Age has five members, so a page of 4 rows cuts inside it
  tb <- data.frame(
    Item = c("Age", " a1", " a2", " a3", " a4", " a5", "Sex", " F", " M"),
    Value = as.character(1:9), stringsAsFactors = FALSE)
  pg <- as_rtftables(tb, group_by = "indent", split = "group_force",
                     max_rows = 4,
                     style = rtf_table_style(border_group = .bg_rule))
  expect_gt(length(pg), 1L)
  bt <- lapply(pg, function(p) .bg_bottoms(.bg_rtf(list(p))))
  # no page break is a rule: page 1 ends inside Age and has none
  expect_equal(sum(bt[[1L]]), 0L)
  # over the whole table, one rule: under Age's real last row (a5); Sex
  # ends the table and takes none
  expect_equal(sum(vapply(bt, function(x) sum(x > 0L), integer(1L))), 1L)
  rows <- unlist(lapply(pg, function(p)
    strsplit(.bg_rtf(list(p)), "\\\\trowd")[[1L]][-1L]))
  has <- grepl("\\\\clbrdrb\\\\brdrs", rows)
  expect_equal(sum(has), 1L)
  expect_match(rows[has], "a5")
})

test_that("a page's last row takes border_last_row, not the group rule", {
  pg <- as_rtftables(.bg_df(), group_by = "indent", split = "group_safe",
                     max_rows = 3,
                     style = rtf_table_style(
                       border_group = rtf_border(
                         bottom = rtf_border_line("double")),
                       border_last_row = rtf_border(bottom = TRUE)))
  rtf <- .bg_rtf(pg)
  # every group ends a page here, so each page's last row is last_row's
  # single rule and the group's double rule is written nowhere
  expect_false(grepl("\\\\clbrdrb\\\\brdrdb", rtf))
  expect_true(grepl("\\\\clbrdrb\\\\brdrs", rtf))
})

test_that("a blank row between groups is not where the rule goes", {
  pg <- as_rtftables(.bg_df(), group_by = "indent",
                     blank_rows = "between_groups",
                     style = rtf_table_style(border_group = .bg_rule))
  rows <- unlist(strsplit(.bg_rtf(pg), "\\\\trowd"))[-1]
  has <- grepl("\\\\clbrdrb\\\\brdrs", rows)
  expect_equal(sum(has), 2L)
  # the rows that carry the rule have text: the groups' last body rows
  expect_true(all(grepl("SD|Male", rows[has])))
})
