## tests/testthat/test-as-rtftables.R
##
## as_rtftables(): unified table-object -> list-of-rtftable converter,
## plus the paginate() deprecation shim.

library(testthat)

# ── data.frame input ──────────────────────────────────────────────────────────

test_that("as_rtftables(data.frame) returns a list of one rtftable", {
  df  <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  res <- as_rtftables(df)
  expect_type(res, "list")
  expect_length(res, 1L)
  expect_s3_class(res[[1L]], "rtftable")
  expect_identical(res[[1L]]$data$a, 1:3)
})

test_that("as_rtftables(data.frame, split) paginates into multiple rtftables", {
  df  <- data.frame(name = paste0("r", 1:6), v = 1:6, stringsAsFactors = FALSE)
  res <- as_rtftables(df, split = "group_force", max_rows = 3,
                      group_col = "name")
  expect_length(res, 2L)
  expect_true(all(vapply(res, inherits, logical(1L), "rtftable")))
  expect_identical(res[[1L]]$data$name, paste0("r", 1:3))
  expect_identical(res[[2L]]$data$name, paste0("r", 4:6))
})

test_that("as_rtftables(list) flattens and propagates names", {
  l <- list(
    "T1" = data.frame(x = 1:2),
    "T2" = data.frame(x = 3:4)
  )
  res <- as_rtftables(l)
  expect_length(res, 2L)
  expect_identical(names(res), c("T1", "T2"))
})

test_that("as_rtftables errors on unsupported input", {
  expect_error(as_rtftables(42L), "supports")
})

# ── gt input ──────────────────────────────────────────────────────────────────

test_that("as_rtftables(gt) reads metadata into the rtftable", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 3)[, c("mpg", "cyl")]) |>
    gt::cols_label(mpg = "MPG", cyl = "Cyl") |>
    gt::cols_align("right", columns = c(mpg, cyl)) |>
    gt::tab_header(title = "T") |>
    gt::tab_source_note("S")

  res <- as_rtftables(g)
  expect_length(res, 1L)
  rt <- res[[1L]]
  expect_s3_class(rt, "rtftable")
  expect_identical(unlist(rt$col_header[[1L]]), c("MPG", "Cyl"))
  expect_identical(rt$col_spec[[1L]]$align, "right")
  # Page-level blocks travel as attributes.
  expect_identical(attr(rt, "rtf_titles"),    "T")
  expect_identical(attr(rt, "rtf_footnotes"), "S")
})

test_that("as_rtftables(gt) body is clean: no hidden/helper cols, NA->empty", {
  skip_if_not_installed("gt")
  df <- data.frame(a = c("x", "y"), b = c(1L, NA), stringsAsFactors = FALSE)
  g  <- gt::gt(df) |> gt::cols_hide(a)        # hide a column
  rt <- as_rtftables(g)[[1L]]
  # hidden column dropped (extract_body gives visible columns only)
  expect_equal(ncol(rt$data), 1L)
  # no stray newline characters anywhere in the body
  expect_false(any(grepl("\n", as.matrix(rt$data), fixed = TRUE)))
})

test_that("as_rtftables(gt) flows titles/footnotes into rtf_tables()", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 2)) |>
    gt::tab_header(title = "Demo") |>
    gt::tab_source_note("Src")
  pages <- as_rtftables(g)
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(pages)
  expect_identical(doc$titles[[1L]],    "Demo")
  expect_identical(doc$footnotes[[1L]], "Src")
})

test_that("as_rtftables(gt, read_meta = FALSE) ignores metadata", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 2)) |>
    gt::tab_header(title = "Demo")
  res <- as_rtftables(g, read_meta = FALSE)
  expect_null(res[[1L]]$col_header)
  expect_null(attr(res[[1L]], "rtf_titles"))
})

# ── gtsummary input ───────────────────────────────────────────────────────────

test_that("as_rtftables(gtsummary) works end to end", {
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("gt")
  s <- gtsummary::tbl_summary(
    data.frame(age = c(20, 30, 40, 50), grp = c("A", "A", "B", "B")))
  res <- as_rtftables(s)
  expect_true(length(res) >= 1L)
  expect_s3_class(res[[1L]], "rtftable")
})

# ── as_rtftable() single-page wrapper still works ─────────────────────────────

test_that("as_rtftable() delegates to as_rtftables and returns one rtftable", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 2)) |> gt::cols_label(mpg = "MPG")
  rt <- as_rtftable(g)
  expect_s3_class(rt, "rtftable")
  expect_false(is.list(rt) && is.null(attr(rt, "class")))
})

# ── rtf_tables() overrides on as_rtftables() output ───────────────────────────

test_that("explicit rtf_tables() args override as_rtftables(read=FALSE) output", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 2)[, c("mpg", "cyl")]) |>
    gt::cols_label(mpg = "MPG")
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(as_rtftables(g, read_meta = FALSE),
               col_rel_width = c(3, 1), table_align = "center")
  expect_identical(doc$contents[[1L]]$col_rel_width, c(3, 1))
  expect_identical(doc$contents[[1L]]$table_align, "center")
})

test_that("rtf_tables() overrides only explicit fields, keeps gt metadata", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 2)[, c("mpg", "cyl")]) |>
    gt::cols_label(mpg = "MPG", cyl = "Cyl") |>
    gt::cols_align("right")
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(as_rtftables(g, read_meta = TRUE), col_rel_width = c(4, 1))
  # overridden
  expect_identical(doc$contents[[1L]]$col_rel_width, c(4, 1))
  # kept from gt
  expect_identical(unlist(doc$contents[[1L]]$col_header[[1L]]), c("MPG", "Cyl"))
  expect_identical(doc$contents[[1L]]$col_spec[[1L]]$align, "right")
})

test_that("col_spec override merges per-column over gt-derived spec", {
  skip_if_not_installed("gt")
  g <- gt::gt(head(mtcars, 2)[, c("mpg", "cyl")]) |> gt::cols_align("right")
  doc <- rtf_document() |>
    rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
    rtf_tables(as_rtftables(g, read_meta = TRUE),
               col_spec = list(list(col = 1, align = "left")))
  expect_identical(doc$contents[[1L]]$col_spec[[1L]]$align, "left")   # overridden
  expect_identical(doc$contents[[1L]]$col_spec[[2L]]$align, "right")  # kept
})

# ── paginate() deprecation ────────────────────────────────────────────────────

test_that("paginate() is deprecated but still functional", {
  # The deprecation warning fires at most once per session; reset the guard
  # so this test reliably observes it regardless of test execution order.
  depr_env <- get(".paginate_depr_env", envir = asNamespace("rtfreporter"))
  depr_env$warned <- FALSE
  df <- data.frame(a = 1:4)
  expect_warning(out <- paginate(df), "deprecated|as_rtftables")
  expect_type(out, "list")
  expect_s3_class(out[[1L]], "data.frame")
  # Second call in the same session does not warn again.
  expect_no_warning(paginate(df))
})

test_that("as_rtftables(auto_width=TRUE) sets content-based column widths", {
  df <- data.frame(
    Characteristic = c("AMERICAN INDIAN OR ALASKA NATIVE", "WHITE"),
    Placebo = c("1 (2%)", "80 (98%)"),
    Active  = c("0", "75 (100%)"),
    stringsAsFactors = FALSE)
  p <- as_rtftables(df, auto_width = TRUE)[[1L]]
  w <- p$column_widths_twips
  expect_length(w, 3L)
  # The wide row-label column must be the widest.
  expect_true(w[1L] > w[2L] && w[1L] > w[3L])
})

test_that("as_rtftables(auto_width=TRUE, table_width_twips=) fills and protects col 1", {
  df <- data.frame(
    Characteristic = c("AMERICAN INDIAN OR ALASKA NATIVE", "WHITE"),
    A = "1 (2%)", B = "0", C = "5", D = "9", stringsAsFactors = FALSE)
  nat <- as_rtftables(df, auto_width = TRUE)[[1L]]$column_widths_twips
  p   <- as_rtftables(df, auto_width = TRUE, table_width_twips = 12000L)[[1L]]
  w   <- p$column_widths_twips
  expect_equal(sum(w), 12000L)
  expect_equal(w[1L], nat[1L])   # column 1 protected at natural width
})

test_that("as_rtftables() explicit column_widths_twips beats auto_width", {
  df <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  p  <- as_rtftables(df, auto_width = TRUE,
                     column_widths_twips = c(1000L, 2000L))[[1L]]
  expect_equal(p$column_widths_twips, c(1000L, 2000L))
})

test_that("as_rtftables(auto_width=TRUE) caps an over-wide table at the default page width", {
  # A table whose natural width exceeds the default landscape-Letter writable
  # width (13680 twips) is scaled down to fit; a narrow one keeps natural.
  wide <- data.frame(
    label = strrep("X", 120),
    a = strrep("Y", 60), b = strrep("Z", 60), stringsAsFactors = FALSE)
  p <- as_rtftables(wide, auto_width = TRUE)[[1L]]
  expect_equal(sum(p$column_widths_twips), 13680L)

  narrow <- data.frame(label = "A", a = "1", b = "2", stringsAsFactors = FALSE)
  q <- as_rtftables(narrow, auto_width = TRUE)[[1L]]
  expect_lt(sum(q$column_widths_twips), 13680L)   # natural, not stretched
})
