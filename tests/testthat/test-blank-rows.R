# blank_rows: three modes (positions / by_change / by_rule), combinable via list;
# read_attributes fallback; col_header_align cascade; multi-row col_header.

# ──── blank_rows mode 1: integer positions ───────────────────────────────────

test_that("mode 1 positions accept c(0, k, -1) and resolve -1 to nrow", {
  df <- data.frame(Grp = c("A","A","B","B","C"), Val = 1:5,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df, blank_rows = c(0, 2, -1))
  expect_identical(tbl$blank_rows, c(0L, 2L, 5L))
})

test_that("mode 1 out-of-range positions stop, naming them (#853)", {
  df <- data.frame(Grp = c("A","A","B","B","C"), Val = 1:5,
                   stringsAsFactors = FALSE)
  expect_error(rtftable(df, blank_rows = c(2, 99)),
               "`blank_rows` position 99 out of range: the table has 5 rows")
  expect_identical(rtftable(df, blank_rows = c(2, 5))$blank_rows, c(2L, 5L))
})

# ──── blank_rows mode 2: by_change ───────────────────────────────────────────

test_that("by_change inserts blanks at value changes plus before/after", {
  df <- data.frame(Grp = c("A","A","B","B","C"), Val = 1:5,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df, blank_rows = blank_rows_by_change(cols = "Grp"))
  expect_identical(tbl$blank_rows, c(0L, 2L, 4L, 5L))
})

test_that("by_change can disable before-first and after-last", {
  df <- data.frame(Grp = c("A","A","B","B","C"), Val = 1:5,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    blank_rows = blank_rows_by_change("Grp",
                                       include_before_first = FALSE,
                                       include_after_last   = FALSE))
  expect_identical(tbl$blank_rows, c(2L, 4L))
})

# ──── blank_rows mode 3: by_rule (regex) ─────────────────────────────────────

test_that("by_rule inserts blanks based on a regex match", {
  df <- data.frame(
    Param = c("Age",     "  Mean",  "  SD",
              "Weight",  "  Mean",  "  SD",
              "Total"),
    N = c(20, NA, NA, 20, NA, NA, 20),
    stringsAsFactors = FALSE
  )
  tbl <- rtftable(df,
    blank_rows = blank_rows_by_rule(col = "Param",
                                     pattern = "^[^ ]",
                                     where = "before"))
  expect_identical(tbl$blank_rows, c(0L, 3L, 6L))
})

# ──── blank_rows combination ─────────────────────────────────────────────────

test_that("modes 1+2+3 union via list", {
  df <- data.frame(Grp = c("A","A","B","B","C"), Val = 1:5,
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df,
    blank_rows = list(
      c(-1),
      blank_rows_by_change("Grp",
                            include_before_first = FALSE,
                            include_after_last   = FALSE),
      blank_rows_by_rule("Grp", "^C", "before")
    ))
  expect_identical(tbl$blank_rows, c(2L, 4L, 5L))
})

# ──── read_attributes fallback ───────────────────────────────────────────────

test_that("read_attributes default TRUE consumes attr(data, 'rtf_blank_rows')", {
  df <- data.frame(Grp = c("A","A","B","B","C"), Val = 1:5,
                   stringsAsFactors = FALSE)
  attr(df, "rtf_blank_rows") <- c(0L, -1L)
  tbl <- rtftable(df)
  expect_identical(tbl$blank_rows, c(0L, 5L))
})

test_that("an explicit blank_rows argument overrides the attribute", {
  df <- data.frame(Grp = c("A","A","B","B","C"), Val = 1:5,
                   stringsAsFactors = FALSE)
  attr(df, "rtf_blank_rows") <- c(0L, -1L)
  tbl <- rtftable(df, blank_rows = c(2L))
  expect_identical(tbl$blank_rows, 2L)
})

test_that("read_attributes = FALSE ignores attr(data, 'rtf_blank_rows')", {
  df <- data.frame(Grp = c("A","A","B","B","C"), Val = 1:5,
                   stringsAsFactors = FALSE)
  attr(df, "rtf_blank_rows") <- c(0L, -1L)
  tbl <- rtftable(df, read_attributes = FALSE)
  expect_null(tbl$blank_rows)
})

# ──── col_header_align cascade ───────────────────────────────────────────────

test_that("col_header_align = NULL inherits each column's `align`", {
  df  <- data.frame(L = "x", N = 1L, V = 2.5)
  tbl <- rtftable(df, col_spec = list(
    list(col = 1, align = "left"),
    list(col = 2, align = "right"),
    list(col = 3, align = "center")
  ))
  expect_identical(tbl$col_spec[[1L]]$header_align, "left")
  expect_identical(tbl$col_spec[[2L]]$header_align, "right")
  expect_identical(tbl$col_spec[[3L]]$header_align, "center")
})

test_that("col_header_align scalar applies to all columns", {
  df  <- data.frame(L = "x", N = 1L, V = 2.5)
  tbl <- rtftable(df,
    col_spec = list(list(col = 1, align = "left"),
                     list(col = 2, align = "right")),
    col_header_align = "center")
  for (j in 1:3) expect_identical(tbl$col_spec[[j]]$header_align, "center")
})

test_that("col_header_align vector applies per column", {
  df  <- data.frame(L = "x", N = 1L, V = 2.5)
  tbl <- rtftable(df, col_header_align = c("right", "right", "left"))
  expect_identical(tbl$col_spec[[1L]]$header_align, "right")
  expect_identical(tbl$col_spec[[3L]]$header_align, "left")
})

test_that("col_spec entry beats both col_header_align and col_spec$align", {
  df  <- data.frame(L = "x", N = 1L, V = 2.5)
  tbl <- rtftable(df,
    col_spec = list(list(col = 2, header_align = "left")),
    col_header_align = "center")
  expect_identical(tbl$col_spec[[1L]]$header_align, "center")
  expect_identical(tbl$col_spec[[2L]]$header_align, "left")
  expect_identical(tbl$col_spec[[3L]]$header_align, "center")
})

# ──── multi-row col_header with mixed spanning + labels ──────────────────────

test_that("col_header accepts mixed [spanning row, label row]", {
  df <- data.frame(Item = c("Age","Sex"),
                   A_N = c(30,30), A_Mean = c(45.2, NA),
                   B_N = c(30,30), B_Mean = c(46.1, NA),
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df, col_header = list(
    list(
      list(from = 2, to = 3, label = "Drug A (N=30)", underline = TRUE),
      list(from = 4, to = 5, label = "Drug B (N=30)", underline = TRUE)
    ),
    c("Item", "N", "Mean", "N", "Mean")
  ))
  expect_length(tbl$col_header, 2L)
  expect_true(is.list(tbl$col_header[[1L]]))
  expect_false(is.null(tbl$col_header[[1L]][[1L]]$from))
  expect_true(is.character(tbl$col_header[[2L]]))
})

test_that("multi-row col_header (spanning + labels) renders end-to-end", {
  df <- data.frame(Item = c("Age","Sex"),
                   A_N = c(30,30), A_Mean = c(45.2, NA),
                   B_N = c(30,30), B_Mean = c(46.1, NA),
                   stringsAsFactors = FALSE)
  tbl <- rtftable(df, col_header = list(
    list(
      list(from = 2, to = 3, label = "Drug A (N=30)", underline = TRUE),
      list(from = 4, to = 5, label = "Drug B (N=30)", underline = TRUE)
    ),
    c("Item", "N", "Mean", "N", "Mean")
  ))
  doc <- rtf_document()
  doc <- rtf_section(doc, page = 1, secinfo = list(header = NULL, footer = NULL))
  doc <- rtf_tables(doc, list(tbl))
  txt <- .render_to_string(doc)
  expect_match(txt, "Drug A \\(N=30\\)")
  expect_match(txt, "Drug B \\(N=30\\)")
  expect_match(txt, "Item")
})
