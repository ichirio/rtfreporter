# SPIKE (design/plan-resolver): the style layer, and the two views of the
# column projection.
#
# Claims:
#
#   1. table-wide styling is declared, not supplied at build time, and merges
#      last-writer-wins per field like every other layer;
#   2. per-column styling is addressed BY NAME and lands at the column's FINAL
#      position -- so `col_spec`, the reason .reindex_col_spec() exists, never
#      needs re-indexing;
#   3. a hidden column survives resolution (the clinical carrier idiom: group
#      on a column that is never printed) and leaves at build time, through one
#      projection with two named views rather than two coordinate systems.

.yd <- function() {
  data.frame(SOC  = rep(c("Cardiac", "GI"), each = 3L),
             PT   = paste0("PT", 1:6),
             HIDE = as.character(1:6),
             N    = as.character(11:16),
             stringsAsFactors = FALSE)
}

# ── the style layer ────────────────────────────────────────────────────────

test_that("table styling is declared and reaches the built table", {
  t <- rtf_plan(.yd()) |> plan_stub(c("SOC", "PT")) |> plan_hide("HIDE") |>
    plan_style(border = "tfl", widths = c(6000L, 4000L)) |>
    plan_tables()
  expect_identical(t[[1L]]$column_widths_twips, c(6000L, 4000L))
})

test_that("style merges last writer wins, per field", {
  p <- rtf_plan(.yd()) |>
    plan_style(border = "tfl", widths = c(1L, 2L)) |>
    plan_style(border = "none")
  expect_identical(p$layers$style$border, "none")
  expect_identical(p$layers$style$column_widths_twips, c(1L, 2L))
})

test_that("an argument passed at build time still wins", {
  t <- rtf_plan(.yd()) |> plan_hide("HIDE") |>
    plan_style(border = "tfl") |>
    plan_tables(border = "none")
  expect_null(t[[1L]]$border$body$top)
})

test_that("no style layer at all still builds", {
  t <- rtf_plan(.yd()) |> plan_hide("HIDE") |> plan_tables()
  expect_s3_class(t[[1L]], "rtftable")
})

# ── per-column styling, by name ────────────────────────────────────────────

test_that("a column styled by name lands at its final position", {
  t <- rtf_plan(.yd()) |> plan_stub(c("SOC", "PT")) |> plan_hide("HIDE") |>
    plan_roles(N = role("display", align = "center", bold = TRUE)) |>
    plan_style(border = "tfl") |>
    plan_tables()
  # N is source column 4; after the stub merge and the hide it is final 2
  expect_length(t[[1L]]$col_spec, 2L)
  expect_identical(t[[1L]]$col_spec[[2L]]$align, "center")
  expect_true(t[[1L]]$col_spec[[2L]]$bold)
})

test_that("styling a hidden column is silently dropped, not misplaced", {
  # "right" is distinctive: no column defaults to it, so if the hidden
  # column's style leaked onto a neighbour the assertion below would catch it
  t <- rtf_plan(.yd()) |> plan_hide("HIDE") |>
    plan_roles(HIDE = role("display", align = "right")) |>
    plan_tables()
  expect_length(t[[1L]]$col_spec, 3L)          # SOC, PT, N
  aligns <- vapply(t[[1L]]$col_spec, function(e) e$align %||% "", character(1L))
  expect_false("right" %in% aligns)
})

test_that("two merged hierarchy columns share one final entry", {
  res <- resolve_plan(rtf_plan(.yd()) |> plan_stub(c("SOC", "PT")) |>
                        plan_roles(SOC = role("stub", align = "left"),
                                   PT  = role("stub", bold = TRUE)))
  # both map to position 1, so they collapse into a single col_spec entry
  expect_length(res$style$col_spec, 1L)
  expect_identical(res$style$col_spec[[1L]]$col, 1L)
})

# ── one projection, two views ──────────────────────────────────────────────

test_that("the body view keeps a hidden column and the final view drops it", {
  res <- resolve_plan(rtf_plan(.yd()) |> plan_stub(c("SOC", "PT")) |>
                        plan_hide("HIDE"))
  expect_identical(res$columns$body_names, c("SOC / PT", "HIDE", "N"))
  expect_identical(res$columns$names, c("SOC / PT", "N"))
})

test_that("both views come from one projection and agree on the merge", {
  res <- resolve_plan(rtf_plan(.yd()) |> plan_stub(c("SOC", "PT")) |>
                        plan_hide("HIDE"))
  expect_identical(unname(res$columns$body_map), c(1L, 1L, 2L, 3L))
  expect_identical(unname(res$columns$map), c(1L, 1L, NA_integer_, 2L))
})

test_that("the built table carries the final view", {
  t <- rtf_plan(.yd()) |> plan_stub(c("SOC", "PT")) |> plan_hide("HIDE") |>
    plan_tables()
  expect_identical(names(t[[1L]]$data), c("SOC / PT", "N"))
})

# ── the hidden carrier idiom ───────────────────────────────────────────────

test_that("a hidden column can group the table it is not printed in", {
  d <- data.frame(CARRIER = rep(c("g1", "g2"), each = 3L),
                  LBL = paste0("L", 1:6),
                  N = as.character(1:6), stringsAsFactors = FALSE)
  res <- resolve_plan(rtf_plan(d) |> plan_group("CARRIER") |>
                        plan_hide("CARRIER") |> plan_blanks("between_groups"))
  expect_length(unique(res$groups$id), 2L)
  expect_identical(res$blanks, 3L)
})

test_that("that matches what as_rtftables(drop_cols) prints", {
  d <- data.frame(CARRIER = rep(c("g1", "g2"), each = 3L),
                  LBL = paste0("L", 1:6),
                  N = as.character(1:6), stringsAsFactors = FALSE)
  a <- as_rtftables(d, group_col = "CARRIER", drop_cols = "CARRIER",
                    blank_rows = "between_groups", border = "tfl")[[1L]]
  t <- rtf_plan(d) |> plan_group("CARRIER") |> plan_hide("CARRIER") |>
    plan_blanks("between_groups") |> plan_style(border = "tfl") |>
    plan_tables()
  expect_identical(names(t[[1L]]$data), names(a$data))
  expect_identical(unname(as.matrix(t[[1L]]$data)), unname(as.matrix(a$data)))
  expect_identical(as.integer(t[[1L]]$blank_rows), as.integer(a$blank_rows))
})

test_that("a hidden stub or carry column is still a contradiction", {
  expect_error(resolve_plan(rtf_plan(.yd()) |> plan_stub(c("SOC", "PT")) |>
                              plan_hide("SOC")),
               "hidden and printed")
  expect_error(resolve_plan(rtf_plan(.yd()) |>
                              plan_roles(N = role("carry")) |> plan_hide("N")),
               "hidden and printed")
})
