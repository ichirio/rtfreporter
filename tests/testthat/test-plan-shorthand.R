# SPIKE (design/plan-resolver): the shorthand constructor.
#
# Measured against as_rtftables(), the pure layer form cost 16% more characters
# and about twice the lines for an ordinary table -- a bad trade for the common
# case whatever its structural merits.  The constructor now takes the settings
# most tables need and writes the layers itself.
#
# The claim these tests pin: there is NO second code path.  Every shorthand is
# exactly the layer call it stands for, so a plan built either way is the same
# object, and a later layer call composes with a shorthand instead of fighting
# it.

.hd <- function() {
  readRDS(file.path(system.file("extdata", package = "rtfreporter"),
                    "demog_p1.rds"))
}

.ae <- function() {
  data.frame(SOC = rep(c("Cardiac", "GI", "Nervous"), each = 3L),
             PT  = paste0("PT", 1:9),
             N   = as.character(1:9),
             ORD = 9:1,
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

# ── the shorthand IS the layer call ────────────────────────────────────────

test_that("nothing is required -- the bare constructor still works", {
  expect_s3_class(rtf_plan_from(.hd()), "rtf_plan")
  expect_length(plan_tables(rtf_plan_from(.hd())), 1L)
})

test_that("group + max_rows + border equals the three layer calls", {
  a <- rtf_plan_from(.hd(), group = "Label", max_rows = 10L, border = "tfl")
  b <- rtf_plan_from(.hd()) |> plan_group("Label") |>
    plan_pages(max_rows = 10L) |> plan_style(border = "tfl")
  expect_identical(a$layers, b$layers)
  expect_identical(.render(a), .render(b))
})

test_that("stub + group_mode + blanks equals its layer calls", {
  a <- rtf_plan_from(.ae(), stub = c("SOC", "PT"), group = "SOC",
                     group_mode = "indent", blanks = "between_groups",
                     max_rows = 8L, border = "tfl")
  b <- rtf_plan_from(.ae()) |> plan_stub(c("SOC", "PT")) |>
    plan_group("SOC", mode = "indent") |> plan_blanks("between_groups") |>
    plan_pages(max_rows = 8L) |> plan_style(border = "tfl")
  expect_identical(a$layers, b$layers)
  expect_identical(.render(a), .render(b))
})

test_that("hide and carry equal their layer calls", {
  a <- rtf_plan_from(.ae(), hide = "ORD", carry = "SOC")
  b <- rtf_plan_from(.ae()) |> plan_hide("ORD") |>
    plan_roles(SOC = role("carry"))
  expect_identical(a$layers, b$layers)
})

test_that("sort keeps the declared order", {
  a <- rtf_plan_from(.ae(), sort = "ORD")
  expect_identical(resolve_plan(a)$rows$body$PT[[1L]], "PT9")
})

# ── the shorthand matches as_rtftables ─────────────────────────────────────

test_that("the shorthand renders identically to as_rtftables()", {
  expect_identical(
    .render(rtf_plan_from(.hd(), group = "Label", max_rows = 10L,
                          border = "tfl")),
    .render(as_rtftables(.hd(), split = "group_safe", max_rows = 10L,
                         group_col = "Label", border = "tfl")))
})

test_that("the stub shorthand renders identically to as_rtftables()", {
  expect_identical(
    .render(rtf_plan_from(.ae(), hide = "ORD", stub = c("SOC", "PT"),
                          group = "SOC", group_mode = "indent",
                          blanks = "between_groups", max_rows = 8L,
                          border = "tfl")),
    .render(as_rtftables(.ae(), drop_cols = "ORD",
                         stub_vars = c("SOC", "PT"), split = "group_safe",
                         max_rows = 8L, group_by = "indent",
                         blank_rows = "between_groups", border = "tfl")))
})

# ── layers compose with the shorthand, last writer wins ────────────────────

test_that("a later layer overrides one shorthand field and keeps the rest", {
  p <- rtf_plan_from(.hd(), max_rows = 10L, border = "tfl") |>
    plan_pages(max_rows = 5L)
  expect_identical(p$layers$pages$max_rows, 5L)
  expect_identical(p$layers$style$border, "tfl")
  expect_gt(length(plan_tables(p)),
            length(plan_tables(rtf_plan_from(.hd(), max_rows = 10L,
                                             border = "tfl"))))
})

test_that("a later layer adds a setting the shorthand does not reach", {
  p <- rtf_plan_from(.ae(), stub = c("SOC", "PT")) |>
    plan_stub(c("SOC", "PT"), label_span = TRUE)
  expect_true(p$layers$roles$SOC$opts$label_span)
})

test_that("a shorthand role and a later role accumulate", {
  p <- rtf_plan_from(.ae(), stub = c("SOC", "PT")) |> plan_group("SOC")
  expect_setequal(p$layers$roles$SOC$roles, c("stub", "group"))
})

# ── it composes with the document verb ─────────────────────────────────────

test_that("the shorthand goes straight into rtf_tables()", {
  doc <- rtf_document() |>
    rtf_tables(rtf_plan_from(.hd(), max_rows = 10L, border = "tfl"))
  expect_gt(length(doc$contents), 1L)
})
