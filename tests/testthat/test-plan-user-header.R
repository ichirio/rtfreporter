# SPIKE (design/plan-resolver): a column header declared on the plan.
#
# Answering "do the other rtftable verbs still work?".  They do -- 12 of the 14
# have .list methods, so they apply to plan_tables() output unchanged, and the
# result reaches the RTF.  But only AFTER resolution, which costs two things:
#
#   * the single pipeline breaks -- you must materialise, apply the verb, then
#     hand the pages to the document;
#   * the caller is forced into POST-stub column coordinates, exactly the
#     confusion set_col_header() already had to be repaired for once.
#
# plan_header() is the plan's answer: declare the header in SOURCE column
# terms and let the one column map place it.

.uh <- function() {
  data.frame(SOC = rep(c("Cardiac", "GI"), each = 3L),
             PT  = paste0("PT", 1:6),
             A   = as.character(1:6),
             B   = as.character(11:16),
             stringsAsFactors = FALSE)
}

.render <- function(x) {
  doc <- rtf_document(page = rtf_page(orientation = "landscape"))
  if (inherits(x, "rtf_plan")) doc <- rtf_tables(doc, x)
  else for (p in x) doc <- rtf_tables(doc, p)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

# ── the existing verbs still work after resolution ─────────────────────────

test_that("set_col_header() applies to plan_tables() output", {
  tb <- plan_tables(rtf_plan(.uh()) |> plan_stub(c("SOC", "PT")) |> plan_style(border = "tfl"))
  out <- set_col_header(tb, c("Term", "A", "B"))
  expect_identical(out[[1L]]$col_header[[1L]], c("Term", "A", "B"))
})

test_that("and reaches the RTF", {
  tb <- set_col_header(plan_tables(rtf_plan(.uh()) |> plan_stub(c("SOC", "PT")) |>
                                     plan_style(border = "tfl")),
                       c("Term", "A", "B"))
  expect_true(any(grepl("Term", .render(tb), fixed = TRUE)))
})

test_that("they do NOT apply to an unresolved plan", {
  p <- rtf_plan(.uh()) |> plan_stub(c("SOC", "PT"))
  expect_error(set_col_header(p, c("a", "b", "c")), "no applicable method")
  expect_error(style_cols(p, cols = 1L, align = "left"), "no applicable method")
})

test_that("after resolution the caller counts POST-stub columns", {
  # unchanged from as_rtftables(): both merge SOC and PT into one column, so a
  # verb applied afterwards sees three, not four
  tb <- plan_tables(rtf_plan(.uh()) |> plan_stub(c("SOC", "PT")))
  a  <- as_rtftables(.uh(), stub_vars = c("SOC", "PT"), border = "tfl")
  expect_identical(ncol(tb[[1L]]$data), 3L)
  expect_identical(ncol(a[[1L]]$data), ncol(tb[[1L]]$data))
})

# ── plan_header(): source coordinates, one pipeline ────────────────────────

test_that("a header is declared in SOURCE columns and placed by the map", {
  t <- plan_tables(rtf_plan(.uh()) |> plan_stub(c("SOC", "PT")) |>
                       plan_style(border = "tfl") |>
                     plan_header(c("SOC", "PT", "Drug A", "Drug B")))[[1L]]
  # four labels in, three out: the stub carries its own name
  expect_identical(t$col_header[[1L]], c("SOC / PT", "Drug A", "Drug B"))
})

test_that("a spanning row declared in source coordinates shifts itself", {
  t <- plan_tables(rtf_plan(.uh()) |> plan_stub(c("SOC", "PT")) |>
                       plan_style(border = "tfl") |>
                     plan_header(list(col_cell(c(3, 4), "Treatment")),
                                 c("SOC", "PT", "A", "B")))[[1L]]
  cells <- t$col_header[[1L]]
  span <- cells[[length(cells)]]
  expect_identical(span$label, "Treatment")
  expect_identical(c(span$from, span$to), c(2L, 3L))   # source 3-4 -> final 2-3
})

test_that("it survives a hidden column too", {
  t <- plan_tables(rtf_plan(.uh()) |> plan_hide("B") |> plan_style(border = "tfl") |>
                     plan_header(c("SOC", "PT", "A", "B")))[[1L]]
  expect_identical(t$col_header[[1L]], c("SOC", "PT", "A"))
})

test_that("the whole thing is one pipeline into the document", {
  lines <- .render(rtf_plan(.uh()) |> plan_stub(c("SOC", "PT")) |>
                       plan_style(border = "tfl") |>
                     plan_header(c("SOC", "PT", "Drug A", "Drug B")))
  expect_true(any(grepl("Drug A", lines, fixed = TRUE)))
})

test_that("a declared header wins over the one the adapter read", {
  d <- .uh()
  attr(d$A, "label") <- "From the source"
  t <- plan_tables(rtf_plan(d) |> plan_style(border = "tfl") |>
                     plan_header(c("SOC", "PT", "Declared", "B")))[[1L]]
  expect_identical(t$col_header[[1L]][[3L]], "Declared")
})

test_that("declaring no rows leaves the source header alone", {
  d <- .uh()
  attr(d$A, "label") <- "From the source"
  t <- plan_tables(rtf_plan(d) |> plan_style(border = "tfl") |> plan_header())[[1L]]
  expect_identical(t$col_header[[1L]][[3L]], "From the source")
})

test_that("last writer wins, as everywhere else", {
  p <- rtf_plan(.uh()) |> plan_style(border = "tfl") |>
    plan_header(c("a", "b", "c", "d")) |>
    plan_header(c("w", "x", "y", "z"))
  t <- plan_tables(p)[[1L]]
  expect_identical(t$col_header[[1L]], c("w", "x", "y", "z"))
})
