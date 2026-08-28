# stub_spec(): one argument for every stub setting (#314).

ae <- data.frame(SOC = c("Cardiac", "Cardiac", "GI"),
                 PT  = c("AF", "Brady", "Nausea"),
                 N   = c("3 (2.1%)", "1 (0.7%)", "5 (3.5%)"),
                 stringsAsFactors = FALSE)

.cells <- function(o) lengths(regmatches(o, gregexpr("cellx", o, fixed = TRUE)))

# ── the constructor ────────────────────────────────────────────────────────

test_that("stub_spec() carries every stub_cols() setting", {
  sp <- stub_spec(c("SOC", "PT"), label = "S / P", indent = 0L,
                  layout = "columns")
  expect_s3_class(sp, "rtf_stub_spec")
  expect_identical(sp$vars, c("SOC", "PT"))
  expect_identical(sp$label, "S / P")
  expect_identical(sp$indent, 0L)
  expect_identical(sp$layout, "columns")
  expect_false(sp$label_span)
})

test_that("the defaults match stub_cols()", {
  sp <- stub_spec(c("SOC", "PT"))
  expect_identical(sp$layout, "merged")
  expect_identical(sp$indent, 4L)
  expect_null(sp$label)
  expect_false(sp$label_span)
})

test_that("vars is required", {
  expect_error(stub_spec(), "`vars` is required")
  expect_error(stub_spec(NULL), "`vars` is required")
})

test_that("an unknown layout is rejected at construction", {
  expect_error(stub_spec(c("SOC", "PT"), layout = "nope"))
})

test_that("it prints", {
  expect_output(print(stub_spec(c("SOC", "PT"))), "rtf_stub_spec")
  expect_output(print(stub_spec(c("SOC", "PT"))), "SOC, PT")
})

# ── resolution against the superseded family ───────────────────────────────

test_that("the spec form matches the superseded flat form exactly", {
  a <- as_rtftables(ae, stub_vars = c("SOC", "PT"), stub_label = "SOC / PT")
  b <- as_rtftables(ae, stub = stub_spec(c("SOC", "PT"), label = "SOC / PT"))
  expect_identical(a[[1L]]$data, b[[1L]]$data)
  expect_identical(names(a[[1L]]$data), c("SOC / PT", "N"))
})

test_that("a bare vector is shorthand for stub_spec(vars)", {
  a <- as_rtftables(ae, stub = c("SOC", "PT"))
  b <- as_rtftables(ae, stub = stub_spec(c("SOC", "PT")))
  expect_identical(a[[1L]]$data, b[[1L]]$data)
})

test_that("mixing stub with the superseded family is an error", {
  expect_error(as_rtftables(ae, stub = c("SOC", "PT"),
                            stub_vars = c("SOC", "PT")),
               "not both")
  expect_error(as_rtftables(ae, stub = c("SOC", "PT"), stub_label = "x"),
               "not both")
  expect_error(as_rtftables(ae, stub = c("SOC", "PT"), stub_indent = 0L),
               "not both")
})

test_that("no stub at all is still the default", {
  pg <- as_rtftables(ae)
  expect_identical(names(pg[[1L]]$data), c("SOC", "PT", "N"))
})

# ── what the flat family could not reach ───────────────────────────────────

test_that("layout = columns is now reachable from as_rtftables()", {
  pg <- as_rtftables(ae, stub = stub_spec(c("SOC", "PT"), layout = "columns"))
  d  <- pg[[1L]]$data
  expect_identical(names(d), c("SOC", "PT", "N"))
  expect_identical(d$SOC, c("Cardiac", NA, NA, "GI", NA))
  expect_identical(d$N, c(NA, "3 (2.1%)", "1 (0.7%)", NA, "5 (3.5%)"))
})

test_that("layout = columns keeps the source column widths", {
  pg <- as_rtftables(ae, stub = stub_spec(c("SOC", "PT"), layout = "columns"),
                     column_widths_twips = c(2000L, 3000L, 4000L))
  expect_identical(pg[[1L]]$column_widths_twips, c(2000L, 3000L, 4000L))
})

test_that("label_span reaches the renderer through the pipeline", {
  pg <- as_rtftables(ae,
                     stub = stub_spec(c("SOC", "PT"), indent = 0L,
                                      label_span = TRUE),
                     border = "tfl", auto_width = TRUE)
  o <- rtfreporter:::.render_rtftable(pg[[1L]], 9360L)
  n <- .cells(o)
  n <- n[n > 0]
  expect_identical(n[[2L]], 1L)   # Cardiac: merged
  expect_identical(n[[3L]], 2L)   # AF
  expect_identical(n[[5L]], 1L)   # GI: merged
})

test_that("without label_span nothing is merged", {
  pg <- as_rtftables(ae, stub = stub_spec(c("SOC", "PT"), indent = 0L),
                     border = "tfl", auto_width = TRUE)
  n <- .cells(rtfreporter:::.render_rtftable(pg[[1L]], 9360L))
  expect_true(all(n[n > 0] == 2L))
})

# ── the spec composes with the rest of the pipeline ────────────────────────

test_that("a group-aware split still works through the spec", {
  big <- data.frame(SOC = rep(c("A", "B", "C"), each = 4L),
                    PT  = paste0("PT", 1:12),
                    N   = as.character(1:12),
                    stringsAsFactors = FALSE)
  pages <- as_rtftables(big, stub = c("SOC", "PT"),
                        split = "group_safe", max_rows = 6L)
  expect_gt(length(pages), 1L)
})

test_that("the spec survives the list-input recursion", {
  pages <- as_rtftables(list(one = ae, two = ae), stub = c("SOC", "PT"))
  expect_length(pages, 2L)
  for (p in pages) expect_identical(names(p$data), c("SOC / PT", "N"))
})

test_that("by_value + a spec splits by group first, then stubs each page", {
  df <- data.frame(grp = rep(c("G1", "G2"), each = 3L),
                   SOC = rep(c("A", "A", "B"), 2L),
                   PT  = paste0("PT", 1:6),
                   N   = as.character(1:6),
                   stringsAsFactors = FALSE)
  pages <- as_rtftables(df, stub = c("SOC", "PT"), split = "by_value",
                        group_col = "grp")
  expect_length(pages, 2L)
})
