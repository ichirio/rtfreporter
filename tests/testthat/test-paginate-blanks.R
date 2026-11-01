# .paginate_df() — blank_row_first / blank_row_end + indent-based grouping.

# 2 top-level groups with deep sub-row indentation; matches the layout
# described in Issue follow-up: group label, then 4-space sub-rows, then
# 8-space sub-sub-rows.
.deep_indent_df <- function() {
  data.frame(
    label = c(
      "group1",
      "    raw_title1",
      "    raw_title2",
      "        raw_title3",
      "        raw_title4",
      "group2",
      "        raw_title5",
      "        raw_title6",
      "        raw_title7",
      "        raw_title8"
    ),
    value = c("n", "1", "2", "3", "4",
              "n", "5", "6", "7", "8"),
    stringsAsFactors = FALSE
  )
}

# ──────── Indent-based group detection (top-level only) ───────────────────

test_that("only indent level 0 rows open a new group; deeper indents stay in", {
  df   <- .deep_indent_df()
  info <- rtfreporter:::.compute_group_info(df, group_idx = NULL)
  # group_id should be: 1 (group1) for rows 1-5, 2 (group2) for rows 6-10
  expect_identical(info$id, c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L))
  expect_identical(info$headers,
                   c(TRUE, FALSE, FALSE, FALSE, FALSE,
                     TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("NBSP-indented sub-rows (gt/tfrmt style) are not treated as group headers", {
  nbsp <- intToUtf8(160L)
  df <- data.frame(
    label = c("GROUP A",
              paste0(nbsp, nbsp, "child 1"),
              paste0(nbsp, nbsp, "child 2"),
              "GROUP B",
              paste0(nbsp, nbsp, "child 3")),
    value = c("", "1", "2", "", "3"),
    stringsAsFactors = FALSE)
  info <- rtfreporter:::.compute_group_info(df, group_idx = NULL)
  # Only the two non-indented rows are headers -> two groups, not five.
  expect_identical(info$headers, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(info$id, c(1L, 1L, 1L, 2L, 2L))
})

# ──────── Page splitting respects indent-based top-level groups ────────────

test_that("group_safe splits at top-level group boundaries even with nested indents", {
  df  <- .deep_indent_df()
  # max_rows = 6 → group1(5 rows) fits; group2(5 rows) doesn't fit alongside
  # (5+5 = 10 > 6) → spill onto a second page.
  res <- .paginate_df(df, max_rows = 6L, split = "group_safe")
  expect_length(res, 2L)
  expect_identical(unique(rtfreporter:::.compute_group_info(res[[1L]],
                                                              NULL)$id), 1L)
  expect_identical(unique(rtfreporter:::.compute_group_info(res[[2L]],
                                                              NULL)$id), 1L)
})

# ──────── blank_row_first ──────────────────────────────────────────────────

test_that("blank_row_first = TRUE adds position 0 to every page", {
  df  <- .deep_indent_df()
  res <- .paginate_df(df, max_rows = 6L, split = "group_safe",
                   blank_row_first = TRUE)
  for (chunk in res) {
    pos <- attr(chunk, "rtf_blank_rows")
    expect_true(0L %in% pos,
                info = sprintf("page %s missing position 0",
                               paste(pos, collapse = ",")))
  }
})

# ──────── blank_row_end ────────────────────────────────────────────────────

test_that("blank_row_end = TRUE adds the page's last position", {
  df  <- .deep_indent_df()
  res <- .paginate_df(df, max_rows = 6L, split = "group_safe",
                   blank_row_end = TRUE)
  for (chunk in res) {
    pos <- attr(chunk, "rtf_blank_rows")
    expect_true(nrow(chunk) %in% pos,
                info = sprintf("page (n=%d) positions=%s",
                               nrow(chunk), paste(pos, collapse = ",")))
  }
})

# ──────── Combined: between_groups + blank_row_first + blank_row_end ──────

test_that("between_groups + blank_row_first + blank_row_end union as expected", {
  df  <- .deep_indent_df()
  # Single page (max_rows large enough for all 10 rows)
  res <- .paginate_df(df, max_rows = 20L, split = "group_safe",
                   blank_rows      = "between_groups",
                   blank_row_first = TRUE,
                   blank_row_end   = TRUE)
  expect_length(res, 1L)
  pos <- attr(res[[1L]], "rtf_blank_rows")
  # Expected positions:
  #   0  (blank_row_first)
  #   5  (between groups: after row 5 = end of group1)
  #   10 (blank_row_end, after last row)
  expect_identical(pos, c(0L, 5L, 10L))
})

# ──────── Sanity: defaults don't add blanks ────────────────────────────────

test_that("defaults (no blank_rows / no blank_row_first / no blank_row_end) add no blanks", {
  res <- .paginate_df(.deep_indent_df())
  expect_null(attr(res[[1L]], "rtf_blank_rows"))
})

# ──────── rtftable() consumes the merged blank_rows attribute end-to-end ──

test_that("rtftable(read_attributes = TRUE) picks up blank_row_first + between_groups + blank_row_end", {
  df  <- .deep_indent_df()
  res <- .paginate_df(df, blank_rows = "between_groups",
                   blank_row_first = TRUE, blank_row_end = TRUE)
  tbl <- rtftable(res[[1L]])
  expect_identical(tbl$blank_rows, c(0L, 5L, 10L))
})
