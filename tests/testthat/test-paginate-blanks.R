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

# ──────── #1028: blank_rows positions count rows of the whole body ─────────

# The body rows of each printed page, read from the RTF written: a row's
# label ("R03"), or "-" for a blank row.
.t72_rows <- function(pages) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(rtf_tables(rtf_document(), pages), f, overwrite = TRUE)
  s <- paste(readLines(f, warn = FALSE), collapse = "\n")
  lapply(strsplit(s, "\\page", fixed = TRUE)[[1L]], function(pg) {
    ln <- strsplit(pg, "\n", fixed = TRUE)[[1L]]
    ln <- ln[startsWith(ln, "\\trowd") & !grepl("Item\\cell", ln, fixed = TRUE)]
    lab <- grepl("R[0-9]{2}\\\\cell", ln)
    ifelse(lab, sub("^.*(R[0-9]{2})\\\\cell.*$", "\\1", ln), "-")
  })
}
.t72_body <- function(n = 25L) {
  data.frame(Item = sprintf("R%02d", seq_len(n)), V = as.character(seq_len(n)),
             stringsAsFactors = FALSE)
}
# Rows `from`..`to` of the body as their labels, with "-" after each of `after`
.t72_seq <- function(from, to, after = integer(0), before = FALSE) {
  out <- if (before) "-" else character(0)
  for (k in from:to) {
    out <- c(out, sprintf("R%02d", k))
    if (k %in% after) out <- c(out, "-")
  }
  out
}

test_that("FT-TB7201 [FR-TB139]: blank_rows = c(3, 12) over 10-row pages puts one blank after body row 3 and one after body row 12, and none on a page no position falls on", {
  d <- .t72_body()
  want <- list(.t72_seq(1, 10, 3), .t72_seq(11, 20, 12), .t72_seq(21, 25))
  for (sp in c("group_force", "group_safe")) {
    p <- as_rtftables(d, blank_rows = c(3, 12), max_rows = 10, split = sp)
    expect_identical(lapply(p, `[[`, "blank_rows"), list(3L, 2L, NULL), info = sp)
    expect_identical(.t72_rows(p), want, info = sp)
  }
  # the row split, and first_rows, cut elsewhere: the blanks follow the rows
  p <- as_rtftables(d, blank_rows = c(3, 12), split = "rows", split_rows = c(6, 16))
  expect_identical(.t72_rows(p),
                   list(.t72_seq(1, 5, 3), .t72_seq(6, 15, 12), .t72_seq(16, 25)))
  p <- as_rtftables(d, blank_rows = c(3, 12), max_rows = 10, first_rows = 5,
                    split = "group_force")
  expect_identical(lapply(p, `[[`, "blank_rows"), list(3L, 7L, NULL))
  # BY pages: each partition gets the whole body's positions that fall in it
  g <- data.frame(G = rep(c("A", "B", "C"), c(8L, 8L, 9L)), Item = d$Item,
                  stringsAsFactors = FALSE)
  p <- as_rtftables(g, page_by = "G", blank_rows = c(3, 12), drop_cols = "G")
  expect_identical(unname(lapply(p, `[[`, "blank_rows")), list(3L, 4L, NULL))
  # plan_blanks(where = ) passes the positions through unchanged
  p <- plan_apply(table_plan(d) |>
                    plan_paginate_rows(max_rows = 10, split = "group_force") |>
                    plan_blanks(where = c(3, 12)))
  expect_identical(lapply(p, `[[`, "blank_rows"), list(3L, 2L, NULL))
})

test_that("FT-TB7202 [FR-TB139]: blank_rows 0 is before the body's first row and -1 after its last, not on every page", {
  d <- .t72_body()
  p <- as_rtftables(d, blank_rows = c(0, -1), max_rows = 10, split = "group_force")
  expect_identical(lapply(p, `[[`, "blank_rows"), list(0L, NULL, 5L))
  expect_identical(.t72_rows(p), list(.t72_seq(1, 10, before = TRUE),
                                      .t72_seq(11, 20), .t72_seq(21, 25, 25)))
  # BY pages: 0 on the first partition, -1 on the last
  g <- data.frame(G = rep(c("A", "B", "C"), c(8L, 8L, 9L)), Item = d$Item,
                  stringsAsFactors = FALSE)
  p <- as_rtftables(g, page_by = "G", blank_rows = c(0, -1), drop_cols = "G")
  expect_identical(unname(lapply(p, `[[`, "blank_rows")), list(0L, NULL, 9L))
})

test_that("FT-TB7203 [FR-TB139]: a blank after the row that ends a page stays at that page's foot and is not carried to the next page's top", {
  d <- .t72_body()
  p <- as_rtftables(d, blank_rows = 10, max_rows = 10, split = "group_force")
  expect_identical(lapply(p, `[[`, "blank_rows"), list(10L, NULL, NULL))
  r <- .t72_rows(p)
  expect_identical(r[[1L]], .t72_seq(1, 10, 10))
  expect_identical(r[[2L]][1L], "R11")
  # with count_blank_rows = TRUE the blank takes a row: page 1 is full with
  # rows 1..10, so it is dropped, not carried, and no continuation row comes
  p <- as_rtftables(d, blank_rows = 10, max_rows = 10, split = "group_force",
                    count_blank_rows = TRUE)
  expect_identical(lapply(p, `[[`, "blank_rows"), list(NULL, NULL, NULL))
  expect_identical(.t72_rows(p),
                   list(.t72_seq(1, 10), .t72_seq(11, 20), .t72_seq(21, 25)))
})

test_that("FT-TB7204 [FR-TB139]: a set_blank_rows() attribute on the whole data frame names rows of the whole body", {
  d <- set_blank_rows(.t72_body(), blank_rows = c(3, 12))
  expect_identical(attr(d, "rtf_blank_rows"), c(3L, 12L))
  p <- as_rtftables(d, max_rows = 10, split = "group_force")
  expect_identical(lapply(p, `[[`, "blank_rows"), list(3L, 2L, NULL))
  expect_identical(.t72_rows(p), list(.t72_seq(1, 10, 3), .t72_seq(11, 20, 12),
                                      .t72_seq(21, 25)))
  # counted, too
  p <- as_rtftables(d, max_rows = 10, split = "group_force", count_blank_rows = TRUE)
  expect_identical(lapply(p, `[[`, "blank_rows"), list(3L, 3L, NULL))
  expect_identical(.t72_rows(p), list(.t72_seq(1, 9, 3), .t72_seq(10, 18, 12),
                                      .t72_seq(19, 25)))
})

test_that("FT-TB7205 [FR-IX324 FR-TB142]: count_blank_rows = TRUE with blank_rows = c(0, 3) keeps position 0 at the top of page 1 and counts both blanks", {
  d <- .t72_body()
  p <- as_rtftables(d, blank_rows = c(0, 3), max_rows = 10, split = "group_force",
                    count_blank_rows = TRUE)
  expect_identical(lapply(p, `[[`, "blank_rows"), list(c(0L, 3L), NULL, NULL))
  r <- .t72_rows(p)
  expect_identical(r, list(.t72_seq(1, 8, 3, before = TRUE), .t72_seq(9, 18),
                           .t72_seq(19, 25)))
  # every page prints max_rows rows at most; page 1 holds the two blanks
  expect_true(all(lengths(r) <= 10L))
  expect_identical(length(r[[1L]]), 10L)
  # no room for position 0 beside the first group (groups of three, three rows
  # a page): it is dropped, and no page holds only a blank row
  g <- data.frame(G = rep(c("A", "B", "C"), each = 3L), N = as.character(1:9),
                  stringsAsFactors = FALSE)
  p <- as_rtftables(g, split = "group_safe", max_rows = 3, group_col = "G",
                    blank_rows = c(0, 3), count_blank_rows = TRUE)
  expect_identical(unname(vapply(p, function(x) nrow(x$data), 1L)), c(3L, 3L, 3L))
  expect_identical(lapply(p, `[[`, "blank_rows"), list(NULL, NULL, NULL))
})

test_that("FT-TB7206 [FR-TB139]: a page that no position falls on gets no blank row", {
  d <- .t72_body(30L)
  p <- as_rtftables(d, blank_rows = c(2, 25), max_rows = 10, split = "group_force")
  expect_identical(lapply(p, `[[`, "blank_rows"), list(2L, NULL, 5L))
  r <- .t72_rows(p)
  expect_false("-" %in% r[[2L]])
  expect_identical(r[[2L]], .t72_seq(11, 20))
})
