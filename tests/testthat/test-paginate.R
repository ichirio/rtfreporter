# paginate() — table object → per-page data.frame list (Issue #2)

# ──────── Sample fixtures ─────────────────────────────────────────────────

.demo_df <- function() {
  data.frame(
    label = c("group1", "  row1", "  row2", "  row3",
              "group2", "  rowA", "  rowB",
              "group3", "  rowX"),
    value = c("n (xx)",  "1 (10)", "2 (20)", "3 (30)",
              "n (yy)",  "4 (40)", "5 (50)",
              "n (zz)",  "6 (60)"),
    stringsAsFactors = FALSE
  )
}

# ──────── Generic + default method ────────────────────────────────────────

test_that("paginate() default method errors with a helpful message", {
  expect_error(paginate(42L), "no method for class")
})

# ──────── data.frame method — no-split ─────────────────────────────────────

test_that("split = 'none' returns a single chunk", {
  res <- paginate(.demo_df())
  expect_length(res, 1L)
  expect_equal(nrow(res[[1L]]), nrow(.demo_df()))
  expect_identical(attr(res[[1L]], "rtf_paginate_meta")$strategy, "none")
})

# ──────── Manual row-based split ───────────────────────────────────────────

test_that("split = 'rows' cuts at the supplied positions", {
  res <- paginate(.demo_df(), split = "rows", split_rows = c(5L, 8L))
  expect_length(res, 3L)
  expect_equal(nrow(res[[1L]]), 4L)        # rows 1-4
  expect_equal(nrow(res[[2L]]), 3L)        # rows 5-7
  expect_equal(nrow(res[[3L]]), 2L)        # rows 8-9
})

test_that("split = 'rows' requires split_rows", {
  expect_error(paginate(.demo_df(), split = "rows"),
               "`split_rows` is required")
})

# ──────── group_safe ───────────────────────────────────────────────────────

test_that("group_safe keeps whole groups together when they fit", {
  res <- paginate(.demo_df(), max_rows = 5L, split = "group_safe")
  # Groups are size 4, 3, 2.  Greedy fill: chunk1=g1(4); g2(3) doesn't fit
  # (4+3=7 > 5), so chunk2=g2(3)+g3(2)=5.
  expect_length(res, 2L)
  expect_equal(nrow(res[[1L]]), 4L)
  expect_equal(nrow(res[[2L]]), 5L)
  expect_identical(res[[1L]]$label[1L], "group1")
  expect_identical(res[[2L]]$label[1L], "group2")
})

test_that("group_safe force-splits any single group that exceeds max_rows", {
  # group1 has 4 rows; max_rows = 3 forces a split within group1
  res <- paginate(.demo_df(), max_rows = 3L, split = "group_safe")
  expect_gte(length(res), 2L)
  # The second chunk's first row must be the (Cont.) continuation of group1
  cont_chunks <- vapply(res, function(c) grepl("\\(Cont\\.\\)", c$label[1L]),
                        logical(1L))
  expect_true(any(cont_chunks))
})

test_that("group_safe requires max_rows", {
  expect_error(paginate(.demo_df(), split = "group_safe"),
               "max_rows.*required")
})

# ──────── group_force ──────────────────────────────────────────────────────

test_that("group_force cuts at exactly max_rows with Cont. continuation", {
  # min_group_rows = 0 tests the raw cut mechanic (no widow/orphan nudging).
  res <- paginate(.demo_df(), max_rows = 3L, split = "group_force",
                  min_group_rows = 0L)
  expect_length(res, 4L)
  expect_equal(nrow(res[[1L]]), 3L)
  # Chunk 2 must begin with "group1 (Cont.)"
  expect_match(res[[2L]]$label[1L], "group1 \\(Cont\\.\\)")
  # The value cell on the Cont. row is blanked out
  expect_identical(res[[2L]]$value[1L], "")
})

test_that("group_force min_group_rows moves an orphan group header to the next page", {
  nbsp <- intToUtf8(160L)
  kids <- function(...) paste0(nbsp, nbsp, c(...))
  df <- data.frame(
    label = c("G1", kids("a1", "a2", "a3", "a4"),   # header + 4 children
              "G2", kids("b1", "b2", "b3"),         # header + 3 children
              "G3", kids("c1", "c2")),              # header + 2 children
    value = "x", stringsAsFactors = FALSE)

  # max_rows = 6: G1 fills 5 rows, leaving room for only the G2 header (1 row)
  # at the foot of page 1.
  old <- paginate(df, max_rows = 6L, split = "group_force", min_group_rows = 0L)
  # Old behaviour: page 1 ends on the bare "G2" header.
  expect_identical(old[[1L]]$label[nrow(old[[1L]])], "G2")

  new <- paginate(df, max_rows = 6L, split = "group_force")  # default = 2
  # G2 is pushed whole to page 2; page 1 ends on G1's last child, no "(Cont.)".
  expect_false(any(grepl("\\(Cont\\.\\)", new[[1L]]$label)))
  expect_identical(new[[1L]]$label[nrow(new[[1L]])], paste0(nbsp, nbsp, "a4"))
  expect_identical(new[[2L]]$label[1L], "G2")
})

test_that("group_force min_group_rows avoids leaving a widow tail of one row", {
  nbsp <- intToUtf8(160L)
  kids <- function(...) paste0(nbsp, nbsp, c(...))
  # One big group whose force-split would otherwise spill a single child.
  df <- data.frame(
    label = c("G1", kids(paste0("a", 1:6))),   # header + 6 children = 7 rows
    value = "x", stringsAsFactors = FALSE)

  old <- paginate(df, max_rows = 6L, split = "group_force", min_group_rows = 0L)
  # Raw cut: page 1 = 6 rows, page 2 = "(Cont.)" + 1 child (a widow).
  expect_equal(nrow(old[[2L]]), 2L)

  new <- paginate(df, max_rows = 6L, split = "group_force")  # default = 2
  # Widow control pulls a row back so the continuation has >= 2 children.
  expect_gte(nrow(new[[2L]]) - 1L, 2L)   # minus the (Cont.) header row
  expect_match(new[[2L]]$label[1L], "G1 \\(Cont\\.\\)")
})

test_that("group_safe packs following groups onto a split group's tail", {
  nbsp <- intToUtf8(160L)
  kids <- function(...) paste0(nbsp, nbsp, c(...))
  df <- data.frame(
    label = c("BIG", kids(paste0("a", 1:7)),   # 8 rows -> force-split
              "SMALL", kids("b1", "b2")),       # 3 rows
    value = "x", stringsAsFactors = FALSE)
  res <- paginate(df, max_rows = 6L, split = "group_safe")
  # The SMALL group should ride on the tail page of BIG, not start a new one.
  last <- res[[length(res)]]
  expect_true(any(last$label == "SMALL"))
  expect_match(last$label[1L], "BIG \\(Cont\\.\\)")
})

test_that("group_force preserves cell types in non-character columns", {
  df <- data.frame(
    label = c("g1", "  r1", "  r2", "g2", "  rA"),
    n     = c(10L, 5L, 5L, 8L, 8L),
    stringsAsFactors = FALSE
  )
  res <- paginate(df, max_rows = 2L, split = "group_force")
  # The non-character `n` column should still be NA on the Cont. row.
  cont_idx <- which(grepl("\\(Cont\\.\\)", res[[2L]]$label))
  expect_true(length(cont_idx) >= 1L)
  expect_true(all(is.na(res[[2L]]$n[cont_idx])))
})

# ──────── Auto-detected vs explicit group column ───────────────────────────

test_that("group_col = column name groups by RLE on that column", {
  df <- data.frame(
    grp = rep(c("A", "B", "C"), times = c(3, 2, 4)),
    val = 1:9,
    stringsAsFactors = FALSE
  )
  res <- paginate(df, max_rows = 4L, split = "group_safe",
                   group_col = "grp")
  # A(3) fits alone; B(2) packs with — actually 3+2=5 > 4 → spill.
  # After spill: chunk2=B(2), then C(4): 2+4=6 > 4 → spill, chunk3=C(4)
  expect_length(res, 3L)
  expect_identical(unique(res[[1L]]$grp), "A")
  expect_identical(unique(res[[2L]]$grp), "B")
  expect_identical(unique(res[[3L]]$grp), "C")
})

test_that("group_col with unknown name errors clearly", {
  expect_error(paginate(.demo_df(), max_rows = 5L, split = "group_safe",
                        group_col = "missing"),
               "not found")
})

# ──────── Blank-row attributes ─────────────────────────────────────────────

test_that("blank_rows integer is attached as rtf_blank_rows attribute", {
  res <- paginate(.demo_df(), blank_rows = c(4L, 7L))
  expect_identical(attr(res[[1L]], "rtf_blank_rows"), c(4L, 7L))
})

test_that("blank_rows = 'between_groups' inserts blanks at group transitions", {
  res <- paginate(.demo_df(), blank_rows = "between_groups")
  # Group transitions occur before rows 5 and 8 → blank positions 4 and 7
  expect_identical(attr(res[[1L]], "rtf_blank_rows"), c(4L, 7L))
})

test_that("blank_rows list combines multiple specs", {
  res <- paginate(.demo_df(),
                   blank_rows = list(c(0L), "between_groups"))
  expect_identical(attr(res[[1L]], "rtf_blank_rows"), c(0L, 4L, 7L))
})

test_that("blank_rows positions are clipped to the chunk size", {
  res <- paginate(.demo_df(), max_rows = 4L, split = "group_safe",
                   blank_rows = c(0L, 99L))   # 99 out of range
  expect_identical(attr(res[[1L]], "rtf_blank_rows"), 0L)
})

# ──────── list method ──────────────────────────────────────────────────────

test_that("list method recurses and concatenates page-wise", {
  df1 <- .demo_df()
  df2 <- .demo_df()[1:4, ]   # one group
  res <- paginate(list(df1, df2), max_rows = 5L, split = "group_safe")
  # df1 → 2 chunks (see test above); df2 → 1 chunk.  Total = 3.
  expect_length(res, 3L)
})

test_that("empty list returns an empty list", {
  expect_identical(paginate(list()), list())
})

# ──────── Paginate meta attribute ──────────────────────────────────────────

test_that("each returned chunk carries a paginate_meta attribute", {
  res <- paginate(.demo_df(), max_rows = 3L, split = "group_force")
  meta <- attr(res[[2L]], "rtf_paginate_meta")
  expect_type(meta, "list")
  expect_identical(meta$strategy,    "group_force")
  expect_identical(meta$page_index,  2L)
  expect_identical(meta$total_pages, length(res))
})

# ──────── End-to-end with rtf_tables() consuming the attribute ────────────

test_that("rtftable(read_attributes = TRUE) picks up paginate's blank_rows", {
  res <- paginate(.demo_df(), blank_rows = "between_groups")
  # Single chunk; nrow=9; expected blank positions 4 and 7.
  tbl <- rtftable(res[[1L]])
  expect_identical(tbl$blank_rows, c(4L, 7L))
})

test_that("rtf_tables() can take paginate() output directly", {
  res <- paginate(.demo_df(), max_rows = 4L, split = "group_force",
                   blank_rows = "between_groups")
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, res)
  expect_length(doc$contents, length(res))
  for (i in seq_along(res)) {
    expect_s3_class(doc$contents[[i]]$content, "rtftable")
  }
})

# ──────── gt method (skip when gt isn't installed) ─────────────────────────

test_that("paginate(gt_tbl) extracts the body and delegates to data.frame", {
  skip_if_not_installed("gt")
  df <- data.frame(label = c("A", "B", "C"), n = c(1L, 2L, 3L),
                   stringsAsFactors = FALSE)
  g  <- gt::gt(df)
  res <- paginate(g)
  expect_length(res, 1L)
  expect_equal(nrow(res[[1L]]), 3L)
  expect_true("label" %in% names(res[[1L]]))
})

test_that("paginate(gt_tbl) accepts split / max_rows pass-through", {
  skip_if_not_installed("gt")
  df <- data.frame(label = LETTERS[1:6], n = 1:6, stringsAsFactors = FALSE)
  g  <- gt::gt(df)
  res <- paginate(g, max_rows = 3L, split = "group_force")
  expect_length(res, 2L)
  expect_equal(nrow(res[[1L]]), 3L)
})

# ──────── Class preservation: tibble in → tibble out ──────────────────────

test_that("paginate(gt_tbl) returns tibbles (gt is tibble-native)", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tibble")
  df <- data.frame(
    label = c("A", "  a1", "  a2", "B", "  b1"),
    v     = 1:5,
    stringsAsFactors = FALSE
  )
  g  <- gt::gt(df)
  res <- paginate(g, max_rows = 3L, split = "group_safe")
  expect_gte(length(res), 1L)
  for (chunk in res) {
    expect_s3_class(chunk, "tbl_df")
    expect_s3_class(chunk, "data.frame")
  }
})

test_that("paginate(tibble) preserves the tibble class on every chunk", {
  skip_if_not_installed("tibble")
  tb <- tibble::tibble(
    label = c("A", "  a1", "  a2", "  a3",
              "B", "  b1", "  b2"),
    v     = 1:7
  )
  res <- paginate(tb, max_rows = 3L, split = "group_force",
                   blank_rows      = "between_groups",
                   blank_row_first = TRUE,
                   blank_row_end   = TRUE)
  for (chunk in res) {
    expect_s3_class(chunk, "tbl_df")
    expect_false(identical(class(chunk), "data.frame"),
                 info = paste(class(chunk), collapse = "/"))
  }
})

test_that("paginate(data.frame) still returns plain data.frames (not tibbles)", {
  df <- data.frame(label = c("A", "  a", "B", "  b"),
                    v = 1:4, stringsAsFactors = FALSE)
  res <- paginate(df, max_rows = 2L, split = "group_safe")
  for (chunk in res) {
    expect_identical(class(chunk), "data.frame")
  }
})
