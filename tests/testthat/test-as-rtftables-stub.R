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

test_that("by_value + stub_vars works from a gt input and renders (#244)", {
  skip_if_not_installed("gt")
  df <- data.frame(
    grade  = c("Grade 1", "Grade 1", "Grade 2", "Grade 2"),
    group1 = c("Chemistry", "Chemistry", "Chemistry", "Chemistry"),
    label  = c("ALT", "AST", "ALT", "AST"),
    Active = c("3", "2", "1", "4"),
    stringsAsFactors = FALSE
  )
  g <- gt::gt(df)
  pages <- as_rtftables(g, read_meta = TRUE, stub_vars = c("group1", "label"),
                        stub_label = "Parameter", split = "by_value",
                        group_col = "grade", drop_cols = "grade")

  expect_identical(names(pages), c("Grade 1", "Grade 2"))
  expect_true(all(vapply(pages, inherits, logical(1L), "rtftable")))
  expect_identical(names(pages[[1L]]$data), c("Parameter", "Active"))

  doc <- rtf_document() |>
    rtf_section(secinfo = list(header = NULL)) |>
    rtf_tables(pages, auto_section = TRUE)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  expect_no_error(generate_rtfreport(doc, f, overwrite = TRUE))
  expect_true(file.exists(f))
})

test_that("by_value + stub_vars splits by group_col first, then stubs each page (#244)", {
  # Outer grouping column (grade) + inner hierarchy (SOC/PT) folded into a stub.
  # Previously the stub was built once on the whole body, blanking `grade` on
  # the inserted SOC label rows -> one page per row + nameless "group_0".
  df <- data.frame(
    grade = c("Grade 1", "Grade 1", "Grade 1", "Grade 2", "Grade 2"),
    SOC   = c("Cardiac", "Cardiac", "GI", "Cardiac", "GI"),
    PT    = c("Arrhythmia", "Tachycardia", "Nausea", "Arrhythmia", "Nausea"),
    N     = c("3", "2", "5", "1", "4"),
    stringsAsFactors = FALSE
  )
  pages <- as_rtftables(df, stub_vars = c("SOC", "PT"), stub_label = "SOC / PT",
                        split = "by_value", group_col = "grade",
                        drop_cols = "grade")

  # one page per grade value, named by it -- NOT one page per row, no group_0
  expect_identical(names(pages), c("Grade 1", "Grade 2"))
  expect_false(any(grepl("^group_", names(pages))))
  # grade dropped from the printed body; stub + stat remain
  expect_identical(names(pages[[1L]]$data), c("SOC / PT", "N"))
  # page 1 = Grade 1: 3 leaves + 2 SOC labels; page 2 = Grade 2: 2 leaves + 2
  expect_identical(nrow(pages[[1L]]$data), 5L)
  expect_identical(nrow(pages[[2L]]$data), 4L)
  # each page's stub was built independently: a leaf is indented, a SOC label is
  # flush left
  nbsp <- intToUtf8(160L)
  expect_false(startsWith(pages[[1L]]$data[[1L]][1L], nbsp))
  expect_true(startsWith(pages[[1L]]$data[[1L]][2L], strrep(nbsp, 4L)))
})

test_that("by_value + stub_vars handles a constant intermediate hierarchy level (#244)", {
  # The reported case: LBTOX_LBL / group1 / label where group1 is a fixed value
  # shared by every row.  Building the stub before the split would collapse
  # group1 into a single label row spanning both grades; splitting first gives
  # each grade its own group1 header.
  df <- data.frame(
    grade  = c("Grade 1", "Grade 1", "Grade 2", "Grade 2"),
    group1 = c("Chemistry", "Chemistry", "Chemistry", "Chemistry"),
    label  = c("ALT", "AST", "ALT", "AST"),
    N      = c("3", "2", "1", "4"),
    stringsAsFactors = FALSE
  )
  pages <- as_rtftables(df, stub_vars = c("group1", "label"),
                        stub_label = "Parameter",
                        split = "by_value", group_col = "grade",
                        drop_cols = "grade")

  expect_identical(names(pages), c("Grade 1", "Grade 2"))
  nbsp <- intToUtf8(160L)
  # every page repeats the group1 header (flush left) then its two indented labels
  for (pg in pages) {
    expect_identical(nrow(pg$data), 3L)                 # 1 group1 label + 2 leaves
    expect_identical(gsub(nbsp, "", pg$data[[1L]], fixed = TRUE),
                     c("Chemistry", "ALT", "AST"))
    expect_false(startsWith(pg$data[[1L]][1L], nbsp))   # group1 header flush left
    expect_true(startsWith(pg$data[[1L]][2L], nbsp))    # leaves indented
  }
})
