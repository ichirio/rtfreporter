# as_rtftables(stub_vars = ): fold stub_cols() into the extraction pipeline (#242)

nbsp <- intToUtf8(160L)

ae <- data.frame(
  SOC     = c("Cardiac", "Cardiac", "GI"),
  PT      = c("Arrhythmia", "Tachycardia", "Nausea"),
  Placebo = c("3", "2", "5"),
  Active  = c("1", "0", "4"),
  stringsAsFactors = FALSE
)

test_that("stub_vars merges hierarchy columns into one indented stub (data.frame)", {
  pg <- as_rtftables(ae, stub_vars = c("SOC", "PT"), stub_label = "SOC / PT")[[1L]]

  expect_s3_class(pg, "rtftable")
  expect_identical(names(pg$data), c("SOC / PT", "Placebo", "Active"))
  # 3 leaf rows + 2 SOC label rows
  expect_identical(nrow(pg$data), 5L)
  # leaf rows are indented, label rows are not
  expect_true(startsWith(pg$data[[1L]][2L], strrep(nbsp, 4L)))
  expect_false(startsWith(pg$data[[1L]][1L], nbsp))
})

test_that("stub_vars = NULL is a no-op (backward compatible)", {
  a <- as_rtftables(ae)[[1L]]
  expect_identical(names(a$data), c("SOC", "PT", "Placebo", "Active"))
  expect_identical(nrow(a$data), 3L)
})

test_that("stub_cols() output still carries a src attribute without breaking callers", {
  out <- stub_cols(ae, vars = c("SOC", "PT"))
  expect_s3_class(out, "data.frame")
  src <- attr(out, "rtf_stub_src", exact = TRUE)
  expect_length(src, nrow(out))
  # NA marks the two inserted SOC label rows
  expect_identical(sum(is.na(src)), 2L)
})

test_that("stub_vars reindexes a gt spanning header onto the reshaped columns", {
  skip_if_not_installed("gt")
  g <- gt::gt(ae) |>
    gt::tab_spanner("Treatment", c(Placebo, Active)) |>
    gt::cols_align("center", c(Placebo, Active))

  pg <- as_rtftables(g, read_meta = TRUE, stub_vars = c("SOC", "PT"))[[1L]]

  expect_identical(names(pg$data), c("SOC / PT", "Placebo", "Active"))
  # two-row header: spanning row + leaf row
  expect_length(pg$col_header, 2L)
  # spanner shifted to cover the stat columns (now positions 2-3), stub empty
  span_cells <- pg$col_header[[1L]]
  labs <- vapply(span_cells, function(c) c$label %||% "", character(1L))
  expect_true("Treatment" %in% labs)
  trt <- span_cells[[which(labs == "Treatment")]]
  pos <- trt$pos %||% c(trt$from, trt$to)
  expect_identical(as.integer(range(pos)), c(2L, 3L))
  # leaf row carries the stub label first
  expect_identical(pg$col_header[[2L]][[1L]], "SOC / PT")
})

test_that("stub_vars composes with a group-aware split and renders", {
  skip_if_not_installed("gt")
  big <- do.call(rbind, replicate(3, ae, simplify = FALSE))
  big$PT <- paste0(big$PT, seq_len(nrow(big)))         # distinct leaves
  g <- gt::gt(big)

  pages <- as_rtftables(g, read_meta = TRUE, stub_vars = c("SOC", "PT"),
                        split = "group_safe", max_rows = 5)
  expect_gt(length(pages), 1L)
  expect_true(all(vapply(pages, inherits, logical(1L), "rtftable")))

  doc <- rtf_document() |>
    rtf_section(secinfo = list(header = NULL)) |>
    rtf_tables(pages)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  expect_no_error(generate_rtfreport(doc, f, overwrite = TRUE))
  expect_true(file.exists(f))
})

test_that("stub_vars needs at least two columns", {
  expect_error(as_rtftables(ae, stub_vars = "SOC"), "two columns")
})
