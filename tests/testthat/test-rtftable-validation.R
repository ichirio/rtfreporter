# rtftable.R -- argument-validation and edge-case paths.
#
# Targets the error / edge branches of rtftable() that the existing
# end-to-end tests do not exercise.

# ──────── .normalize_table_border ─────────────────────────────────────────

test_that(".normalize_table_border accepts every documented input form", {
  expect_null(rtfreporter:::.normalize_table_border(NULL))
  tb <- rtfreporter:::.rtf_table_border(header = rtf_border(top = TRUE))
  expect_identical(rtfreporter:::.normalize_table_border(tb), tb)
  expect_s3_class(rtfreporter:::.normalize_table_border(rtf_table_style_tfl()),
                  "rtf_table_border")
  expect_s3_class(rtfreporter:::.normalize_table_border("tfl"),
                  "rtf_table_border")
  # Legacy plain-list form
  expect_s3_class(
    rtfreporter:::.normalize_table_border(
      list(header = list(top = "single", bottom = "single", width = 15))),
    "rtf_table_border"
  )
})

test_that(".normalize_table_border rejects unsupported input", {
  expect_error(rtfreporter:::.normalize_table_border(42), "must be")
  expect_error(rtfreporter:::.normalize_table_border("nope"), "must be")
})

test_that("an rtf_border selects the whole table, never silently emptied (#326, #342)", {
  # Before #326 an rtf_border here reached the plain-list fallback, which found
  # no zone names and returned a border with every zone NULL -- the caller asked
  # for a rule and every rule vanished.  #326 made that an error; #342 gave it a
  # meaning instead: the border applies to the whole table.  Either way, the one
  # outcome that must never happen is the rule silently disappearing.
  b  <- rtf_border(top = rtf_border_side("single", 15L))
  tb <- rtfreporter:::.normalize_table_border(b)
  expect_s3_class(tb, "rtf_table_border")
  expect_identical(tb$outer, b)

  df <- data.frame(a = c("x", "y"), b = c("1", "2"), stringsAsFactors = FALSE)
  # The table's top edge is the topmost row's top, and it carries the rule.
  expect_identical(rtftable(df, border = b)$border$first_row$top, b$top)
  expect_silent(rtf_tables(rtf_document(), df, border = b))

  # A zone map still works when built through the supported route.  (Start from
  # "none": the default preset would supply a header bottom rule of its own.)
  expect_identical(style_zone(rtftable(df, border = "none"), header = b)$border$header,
                   b)

  # A cell cannot take a zone map: that direction is still an error.
  expect_error(col_cell(1, "x", border = rtfreporter:::.rtf_table_border(header = b)),
               "rtf_border")
})

# ──────── col_spec validation in rtftable() ───────────────────────────────

test_that("rtftable() rejects non-list col_spec", {
  expect_error(rtftable(data.frame(A = 1L), col_spec = "x"),
               "`col_spec` must be a list")
})

test_that("rtftable() rejects col_spec element missing `col` key", {
  expect_error(
    rtftable(data.frame(A = 1L, B = 2L),
             col_spec = list(list(align = "right"))),
    "must be a list with a `col` key"
  )
})

test_that("rtftable() resolves col_spec col by name (character)", {
  tbl <- rtftable(data.frame(L = 1L, R = "x", stringsAsFactors = FALSE),
                  col_spec = list(list(col = "R", align = "right")))
  expect_identical(tbl$col_spec[[2L]]$align, "right")
})

test_that("rtftable() errors on character col_spec col not in data", {
  expect_error(
    rtftable(data.frame(A = 1L),
             col_spec = list(list(col = "nope", align = "right"))),
    "not found in data"
  )
})

test_that("rtftable() errors on numeric col_spec col out of range", {
  expect_error(
    rtftable(data.frame(A = 1L, B = 2L),
             col_spec = list(list(col = 5L, align = "right"))),
    "out of range"
  )
})

test_that("rtftable() errors on bad per-column border type", {
  expect_error(
    rtftable(data.frame(A = 1L),
             col_spec = list(list(col = 1L, border = "single"))),
    "must be NULL or an rtf_border"
  )
})

# ──────── col_header_align validation ─────────────────────────────────────

test_that("rtftable() expands scalar col_header_align to per-column", {
  tbl <- rtftable(data.frame(A = 1L, B = 2L, C = 3L),
                  col_header_align = "right")
  expect_identical(tbl$col_spec[[1L]]$header_align, "right")
  expect_identical(tbl$col_spec[[2L]]$header_align, "right")
})

test_that("rtftable() accepts per-column col_header_align vector", {
  tbl <- rtftable(data.frame(A = 1L, B = 2L, C = 3L),
                  col_header_align = c("left", "center", "right"))
  expect_identical(tbl$col_spec[[1L]]$header_align, "left")
  expect_identical(tbl$col_spec[[3L]]$header_align, "right")
})

test_that("rtftable() rejects col_header_align of wrong length", {
  expect_error(
    rtftable(data.frame(A = 1L, B = 2L),
             col_header_align = c("left", "center", "right")),
    "must have length 1 or 2"
  )
})

test_that("rtftable() rejects invalid alignment values", {
  expect_error(
    rtftable(data.frame(A = 1L),
             col_header_align = "justify"),
    "left.*center.*right"
  )
})

# ──────── style validation ────────────────────────────────────────────────

test_that("rtftable() rejects a non-rtf_table_style `style`", {
  expect_error(rtftable(data.frame(A = 1L), style = "tfl"),
               "rtf_table_style object")
})

# ──────── col_header normalisation -- error / edge branches ───────────────

test_that(".normalize_col_header_rows accepts pipe-separated shorthand", {
  out <- rtfreporter:::.normalize_col_header_rows("A | B | C", ncol_df = 3L)
  expect_length(out, 1L)
  expect_identical(out[[1L]], c("A", "B", "C"))
})

test_that(".normalize_col_header_rows rejects unsupported scalar types", {
  expect_error(
    rtfreporter:::.normalize_col_header_rows(42, ncol_df = 1L),
    "must be NULL, character, or a list"
  )
})

test_that(".normalize_col_header_rows rejects an empty cell-spec row", {
  expect_error(
    rtfreporter:::.normalize_col_header_rows(list(list()), ncol_df = 1L),
    "non-empty"
  )
})

test_that(".normalize_col_header_rows accepts legacy from/to spanning rows", {
  legacy <- list(
    list(list(from = 1L, to = 2L, label = "AB"),
         list(from = 3L, to = 3L, label = "C"))
  )
  out <- rtfreporter:::.normalize_col_header_rows(legacy, ncol_df = 3L)
  expect_length(out, 1L)
  expect_identical(out[[1L]][[1L]]$from, 1L)
})

test_that(".normalize_col_header_rows rejects rows that are neither chars nor cell specs", {
  expect_error(
    rtfreporter:::.normalize_col_header_rows(
      list(list(list(unknown_key = 1L))), ncol_df = 1L),
    "character vector"
  )
})

# ──────── multi-DF normalisation ──────────────────────────────────────────

test_that(".normalize_multi_col_header replicates a shared rtf_col_header", {
  hd <- rtf_col_header(c("A", "B"))
  out <- rtfreporter:::.normalize_multi_col_header(hd, n_dfs = 3L, ncol_df = 2L)
  expect_length(out, 3L)
  expect_identical(out[[1L]], out[[3L]])
})

test_that(".normalize_multi_col_header replicates a shared character vector", {
  out <- rtfreporter:::.normalize_multi_col_header(c("A", "B"), n_dfs = 2L,
                                                    ncol_df = 2L)
  expect_length(out, 2L)
  expect_identical(out[[1L]], list(c("A", "B")))
})

test_that(".normalize_multi_col_header replicates a shared single-row cell list", {
  hd <- list(col_cell(c(1L, 2L), "Pair"))
  out <- rtfreporter:::.normalize_multi_col_header(hd, n_dfs = 2L, ncol_df = 2L)
  expect_length(out, 2L)
  expect_identical(out[[1L]], out[[2L]])
})

test_that(".normalize_multi_col_header handles per-DF list of specs", {
  hd <- list(c("X", "Y"), c("P", "Q"))   # one per DF
  out <- rtfreporter:::.normalize_multi_col_header(hd, n_dfs = 2L, ncol_df = 2L)
  expect_length(out, 2L)
  expect_identical(out[[1L]], list(c("X", "Y")))
  expect_identical(out[[2L]], list(c("P", "Q")))
})

test_that(".normalize_multi_col_header rejects non-list / wrong-shape input", {
  expect_error(
    rtfreporter:::.normalize_multi_col_header(42, n_dfs = 2L, ncol_df = 2L),
    "must be NULL, a character vector, or a list"
  )
})

# ──────── rtftable() top-level error branches ─────────────────────────────

test_that("rtftable() rejects data that is neither data.frame nor a list of them", {
  expect_error(rtftable("x"), "data.frame or a non-empty list")
  expect_error(rtftable(list()), "data.frame or a non-empty list")
})

test_that("rtftable() rejects a list containing a non-data.frame element", {
  expect_error(
    rtftable(list(data.frame(A = 1L), "broken")),
    "data\\[\\[2\\]\\]"
  )
})

test_that("rtftable() rejects multi-DF input with mismatched column counts", {
  expect_error(
    rtftable(list(data.frame(A = 1L, B = 2L),
                  data.frame(A = 1L))),
    "same number of columns"
  )
})

# ──────── blank_rows validation ───────────────────────────────────────────

test_that("rtftable() rejects multi-DF blank_rows that's not numeric", {
  expect_error(
    rtftable(list(data.frame(A = 1L), data.frame(A = 2L)),
             blank_rows = list(1L, 2L)),
    "integer vector"
  )
})

test_that("rtftable() rejects multi-DF blank_rows with bad integer", {
  expect_error(
    rtftable(list(data.frame(A = 1L), data.frame(A = 2L)),
             blank_rows = c(-2L)),
    "-1, 0, or positive"
  )
})

# ──────── table_width_pct / table_align / cell_valign ─────────────────────

test_that("rtftable() rejects invalid table_width_pct", {
  expect_error(rtftable(data.frame(A = 1L), table_width_pct = 0),     "0.*100")
  expect_error(rtftable(data.frame(A = 1L), table_width_pct = 200),   "0.*100")
  expect_error(rtftable(data.frame(A = 1L), table_width_pct = "x"),   "0.*100")
})

test_that("rtftable() accepts table_width_pct in (0, 100]", {
  tbl <- rtftable(data.frame(A = 1L), table_width_pct = 50)
  expect_equal(tbl$table_width_pct_of_writable, 0.5)
})

test_that("rtftable() rejects invalid table_align / cell_valign / row_height_exact", {
  expect_error(rtftable(data.frame(A = 1L), table_align = "justify"), "table_align")
  expect_error(rtftable(data.frame(A = 1L), cell_valign  = "middle"),  "cell_valign")
  expect_error(rtftable(data.frame(A = 1L), row_height_exact = "yes"), "TRUE or FALSE")
})
