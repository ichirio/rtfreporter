## tests/testthat/test-gt-group.R
##
## gt_group support (#214): gt's multi-table container (gt::gt_group() /
## gt::gt_split(); also what tfrmt's print_to_gt() returns under a page_plan)
## expands to one page set per member table.  All tests skip when gt is
## absent.

library(testthat)

# Two small member tables shared by the tests below.
.gt_group_members <- function() {
  list(
    gt::gt(data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)),
    gt::gt(data.frame(a = 4:5, b = letters[4:5], stringsAsFactors = FALSE))
  )
}

# ── detection & expansion ─────────────────────────────────────────────────────

test_that(".is_gt_group() detects gt_group containers only", {
  skip_if_not_installed("gt")
  gg <- gt::gt_group(.list = .gt_group_members())
  expect_true(rtfreporter:::.is_gt_group(gg))
  expect_false(rtfreporter:::.is_gt_group(.gt_group_members()[[1]]))
  expect_false(rtfreporter:::.is_gt_group(list(a = 1)))
})

test_that(".gt_group_tables() pulls every member as a gt_tbl", {
  skip_if_not_installed("gt")
  gg   <- gt::gt_group(.list = .gt_group_members())
  tbls <- rtfreporter:::.gt_group_tables(gg)
  expect_length(tbls, 2L)
  expect_true(all(vapply(tbls, inherits, logical(1), "gt_tbl")))
})

test_that(".gt_group_tables() on an empty gt_group returns an empty list", {
  skip_if_not_installed("gt")
  expect_identical(rtfreporter:::.gt_group_tables(gt::gt_group()), list())
})

# ── as_rtftables() ────────────────────────────────────────────────────────────

test_that("as_rtftables() expands a gt_group into one page per member", {
  skip_if_not_installed("gt")
  members <- .gt_group_members()
  pages   <- as_rtftables(gt::gt_group(.list = members))
  expect_length(pages, 2L)
  expect_true(all(vapply(pages, inherits, logical(1), "rtftable")))
  # Same bodies as converting the member list directly.
  direct <- as_rtftables(members)
  expect_identical(lapply(pages,  `[[`, "data"),
                   lapply(direct, `[[`, "data"))
})

test_that("as_rtftables() accepts gt_split() output", {
  skip_if_not_installed("gt")
  g     <- gt::gt(data.frame(a = 1:10, b = letters[1:10],
                             stringsAsFactors = FALSE))
  pages <- as_rtftables(gt::gt_split(g, row_every_n = 4))
  expect_length(pages, 3L)                      # 4 + 4 + 2 rows
  expect_equal(vapply(pages, function(p) nrow(p$data), integer(1L)),
               c(4L, 4L, 2L))
})

test_that("as_rtftables() pagination args apply per gt_group member", {
  skip_if_not_installed("gt")
  members <- list(
    gt::gt(data.frame(a = 1:4, stringsAsFactors = FALSE)),
    gt::gt(data.frame(a = 5:8, stringsAsFactors = FALSE))
  )
  pages <- as_rtftables(gt::gt_group(.list = members),
                        split = "group_force", max_rows = 2)
  # Each 4-row member -> 2 pages of 2 rows.
  expect_length(pages, 4L)
  expect_equal(vapply(pages, function(p) nrow(p$data), integer(1L)),
               rep(2L, 4L))
})

test_that("as_rtftables() converts an empty gt_group to an empty list", {
  skip_if_not_installed("gt")
  expect_identical(as_rtftables(gt::gt_group()), list())
})

# ── as_rtftable() ─────────────────────────────────────────────────────────────

test_that("as_rtftable() unwraps a single-member gt_group", {
  skip_if_not_installed("gt")
  gg  <- gt::gt_group(.list = .gt_group_members()[1])
  tbl <- as_rtftable(gg)
  expect_s3_class(tbl, "rtftable")
  expect_equal(nrow(tbl$data), 3L)
})

test_that("as_rtftable() rejects a multi-member gt_group informatively", {
  skip_if_not_installed("gt")
  gg <- gt::gt_group(.list = .gt_group_members())
  expect_error(as_rtftable(gg), "holding 2 tables.*as_rtftables")
})

# ── tfrmt page_plan round-trip ────────────────────────────────────────────────

test_that("a tfrmt page_plan render (gt_group) converts page by page", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tfrmt")
  dat <- expand.grid(group  = c("g1", "g2"),
                     label  = paste0("lbl", 1:3),
                     column = c("A", "B"),
                     param  = "n",
                     stringsAsFactors = FALSE)
  dat$value <- seq_len(nrow(dat))
  spec <- tfrmt::tfrmt(
    group = group, label = label, column = column, param = param,
    value = value,
    body_plan = tfrmt::body_plan(
      tfrmt::frmt_structure(group_val = ".default", label_val = ".default",
                            tfrmt::frmt("x"))),
    page_plan = tfrmt::page_plan(
      tfrmt::page_structure(group_val = ".default")))
  pg <- tfrmt::print_to_gt(spec, dat)
  expect_s3_class(pg, "gt_group")              # the motivating case for #214
  pages <- as_rtftables(pg)
  expect_length(pages, 2L)                     # one page per group value
  expect_true(all(vapply(pages, inherits, logical(1), "rtftable")))
})

# ── row-split members (#1005) ─────────────────────────────────────────────────

test_that("FT-TB6601 [FR-TB115]: a row-split gt_group member gives its own rows", {
  skip_if_not_installed("gt")
  d4 <- data.frame(a = as.character(1:4), b = letters[1:4],
                   x = c(1.234, 2.345, 3.456, 4.567), stringsAsFactors = FALSE)
  # the reproduction: page 2 holds rows 3 and 4, not empty cells
  sp <- gt::gt_split(gt::gt(d4[c("a", "b")]), row_every_n = 2)
  pages <- as_rtftables(sp)
  expect_length(pages, 2L)
  expect_identical(pages[[1L]]$data$a, c("1", "2"))
  expect_identical(pages[[2L]]$data$a, c("3", "4"))
  expect_identical(pages[[2L]]$data$b, c("c", "d"))
  # formatted text and a cell style on page-2 rows
  g <- gt::tab_style(gt::fmt_number(gt::gt(d4), "x", decimals = 1),
                     gt::cell_text(weight = "bold"),
                     gt::cells_body(columns = "b", rows = 4))
  sp2 <- gt::gt_split(g, row_every_n = 2)
  pg <- as_rtftables(sp2, read_meta = TRUE)
  expect_identical(pg[[1L]]$data$x, c("1.2", "2.3"))
  expect_identical(pg[[2L]]$data$x, c("3.5", "4.6"))
  jb <- match("b", names(pg[[2L]]$data))
  expect_true(pg[[2L]]$cell_styles[[2L]]$bold[jb])
  expect_false(isTRUE(pg[[2L]]$cell_styles[[1L]]$bold[jb]))
  # a member pulled alone gives the same
  one <- as_rtftables(gt::grp_pull(sp2, 2L), read_meta = TRUE)
  expect_identical(one[[1L]]$data, pg[[2L]]$data)
  expect_identical(one[[1L]]$cell_styles, pg[[2L]]$cell_styles)
  # a column split is unchanged
  cs <- as_rtftables(gt::gt_split(gt::gt(d4[c("a", "b")]), col_slice_at = "a"))
  expect_identical(cs[[1L]]$data$a, as.character(1:4))
  expect_identical(cs[[2L]]$data$b, letters[1:4])
})

# The first-cell text of every table row on each page of an RTF file
# ("~" for a non-breaking space), the header row included.
.t69_page_rows <- function(pages) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(rtf_tables(rtf_document(), pages), f, overwrite = TRUE)
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  lapply(strsplit(txt, "\\page", fixed = TRUE)[[1L]], function(p) {
    m <- regmatches(p, gregexpr("\\\\ql\\\\li0\\\\ri0 [^\n]*?\\\\cell", p))[[1L]]
    m <- sub("^\\\\ql\\\\li0\\\\ri0 ", "", sub("\\\\cell$", "", m))
    gsub("\\u160?", "~", m, fixed = TRUE)
  })
}

test_that("FT-TB6901 [FR-TB115]: a row-split member of a grouped gt keeps its group rows", {
  skip_if_not_installed("gt")
  df <- data.frame(grp  = rep(c("Arm A", "Arm B"), each = 3),
                   subj = sprintf("S%02d", 1:6),
                   age  = c(34, 45, 40, 51, 29, 60), stringsAsFactors = FALSE)
  whole <- gt::gt(df, groupname_col = "grp")
  # the whole table: one heading per group, its rows indented under it
  all_rows <- c("subj", "Arm A", "~~S01", "~~S02", "~~S03",
                "Arm B", "~~S04", "~~S05", "~~S06")
  expect_identical(.t69_page_rows(as_rtftables(whole)), list(all_rows))
  # the reproduction: each page carries its own group rows
  sp <- gt::gt_split(whole, row_every_n = 4)
  expect_identical(.t69_page_rows(as_rtftables(sp)), list(
    c("subj", "Arm A", "~~S01", "~~S02", "~~S03", "Arm B", "~~S04"),
    c("subj", "Arm B", "~~S05", "~~S06")))
  # a group that runs across a page boundary shows its row again, as gt does
  sp2 <- gt::gt_split(whole, row_every_n = 2)
  expect_identical(.t69_page_rows(as_rtftables(sp2)), list(
    c("subj", "Arm A", "~~S01", "~~S02"),
    c("subj", "Arm A", "~~S03", "Arm B", "~~S04"),
    c("subj", "Arm B", "~~S05", "~~S06")))
  # a member pulled alone gives the same page
  one <- as_rtftables(gt::grp_pull(sp, 2L))
  expect_identical(one[[1L]]$data, as_rtftables(sp)[[2L]]$data)
  # rows in an order other than the groups': gt's own order on each page
  du <- df[c(4L, 1L, 5L, 2L, 3L, 6L), ]
  spu <- gt::gt_split(gt::gt(du, groupname_col = "grp"), row_every_n = 4)
  expect_identical(.t69_page_rows(as_rtftables(spu)), list(
    c("subj", "Arm B", "~~S04", "~~S05", "Arm A", "~~S01", "~~S02"),
    c("subj", "Arm B", "~~S06", "Arm A", "~~S03")))
  # an ungrouped split is unchanged: no group rows
  plain <- .t69_page_rows(as_rtftables(gt::gt_split(gt::gt(df[-1L]),
                                                    row_every_n = 4)))
  expect_identical(plain, list(c("subj", sprintf("S%02d", 1:4)),
                               c("subj", sprintf("S%02d", 5:6))))
})
