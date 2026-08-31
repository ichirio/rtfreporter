# SPIKE (design/plan-resolver): a plan handed straight to rtf_tables().
#
# The last piece of the user-facing shape.  A plan is declarations; it becomes
# pages at the moment it meets a document, not before, so nothing in the
# caller's pipeline has to know about resolution at all.

.dd <- function() {
  readRDS(file.path(system.file("extdata", package = "rtfreporter"),
                    "demog_p1.rds"))
}

.render <- function(doc) {
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

# ── deferred resolution ────────────────────────────────────────────────────

test_that("rtf_tables() accepts an unresolved plan", {
  doc <- rtf_document() |>
    rtf_tables(rtf_plan(.dd()) |> plan_style(border = "tfl"))
  expect_length(doc$contents, 1L)
  expect_s3_class(doc$contents[[1L]], "rtftable")
})

test_that("a paginated plan contributes one content item per page", {
  doc <- rtf_document() |>
    rtf_tables(rtf_plan(.dd()) |> plan_pages(break_before = 8L) |>
                 plan_style(border = "tfl"))
  expect_length(doc$contents, 2L)
})

test_that("resolving through the document matches resolving first", {
  p <- rtf_plan(.dd()) |> plan_group(names(.dd())[1L]) |>
    plan_blanks("between_groups") |> plan_pages(max_rows = 10L) |>
    plan_style(border = "tfl")

  a <- rtf_document(page = rtf_page(orientation = "landscape"))
  a <- rtf_tables(a, p)                       # deferred

  b <- rtf_document(page = rtf_page(orientation = "landscape"))
  for (t in plan_tables(p)) b <- rtf_tables(b, t)   # resolved first

  expect_identical(.render(a), .render(b))
})

test_that("the plan is unchanged by being handed to a document", {
  p <- rtf_plan(.dd()) |> plan_style(border = "tfl")
  before <- p
  invisible(rtf_document() |> rtf_tables(p))
  expect_identical(p, before)
})

# ── it composes with the document verbs ────────────────────────────────────

test_that("titles and footnotes still apply", {
  doc <- rtf_document() |>
    rtf_tables(rtf_plan(.dd()) |> plan_pages(break_before = 8L) |>
                 plan_style(border = "tfl"),
               titles = list("Table 14.1.1", "Table 14.1.2"))
  expect_length(doc$titles, 2L)
})

test_that("auto_title works on a named plan result", {
  # plan_tables() returns an unnamed list, so auto_title is a no-op rather
  # than an error -- worth pinning, since naming pages is the next question
  doc <- rtf_document() |>
    rtf_tables(rtf_plan(.dd()) |> plan_style(border = "tfl"),
               auto_title = TRUE)
  expect_null(doc$titles[[1L]])
})

test_that("a plan renders end to end through generate_rtfreport()", {
  doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
    rtf_tables(rtf_plan(.dd()) |> plan_group(names(.dd())[1L]) |>
                 plan_blanks("between_groups") |> plan_style(border = "tfl"))
  lines <- .render(doc)
  expect_true(length(lines) > 5L)
  expect_true(any(grepl("rtf1", lines, fixed = TRUE)))
})

# ── the acceptance criterion, through the document ─────────────────────────

test_that("a plan through rtf_tables() matches as_rtftables() through it", {
  d <- .dd()
  a <- rtf_document(page = rtf_page(orientation = "landscape"))
  for (t in as_rtftables(d, split = "group_safe", max_rows = 10L,
                         group_col = 1L, border = "tfl")) {
    a <- rtf_tables(a, t)
  }
  b <- rtf_document(page = rtf_page(orientation = "landscape")) |>
    rtf_tables(rtf_plan(d) |> plan_group(names(d)[1L]) |>
                 plan_pages(max_rows = 10L) |> plan_style(border = "tfl"))
  expect_identical(.render(a), .render(b))
})

# ── nothing else changed ───────────────────────────────────────────────────

test_that("an ordinary rtftable is untouched by the hook", {
  d <- .dd()
  doc <- rtf_document() |> rtf_tables(rtftable(d, border = "tfl"))
  expect_length(doc$contents, 1L)
  expect_s3_class(doc$contents[[1L]], "rtftable")
})

test_that("a list of rtftables is untouched by the hook", {
  doc <- rtf_document() |>
    rtf_tables(as_rtftables(.dd(), border = "tfl"))
  expect_length(doc$contents, 1L)
})

test_that("a bare data.frame is untouched by the hook", {
  doc <- rtf_document() |> rtf_tables(.dd())
  expect_length(doc$contents, 1L)
})
