# SPIKE (design/plan-resolver): column roles, resolved by name.
#
# The claim under test: if every column reference is held BY NAME and positions
# are computed once, from a single projection of the final layout, there is
# nothing left to re-index -- and the eight reindexers in the package collapse
# to one map.

.rd <- function() {
  data.frame(SOC     = rep(c("Cardiac", "GI"), each = 3L),
             PT      = paste0("PT", 1:6),
             CARRIER = as.character(1:6),
             N       = as.character(11:16),
             stringsAsFactors = FALSE)
}

# ── role() ─────────────────────────────────────────────────────────────────

test_that("role() separates role names from options", {
  r <- role("stub", "group", order = 2L, mode = "indent")
  expect_identical(r$roles, c("stub", "group"))
  expect_identical(r$opts, list(order = 2L, mode = "indent"))
})

test_that("an unknown role is rejected with the valid list", {
  expect_error(role("groop"), "Unknown role")
  expect_error(role("groop"), "Valid roles")
})

test_that("a bare string is promoted to a role", {
  p <- rtf_plan(.rd()) |> plan_roles(SOC = "group")
  expect_identical(p$layers$roles$SOC$roles, "group")
})

# ── the merge rule ─────────────────────────────────────────────────────────

test_that("roles accumulate, so a column can play two parts", {
  # the case that broke the first draft: grouping must not erase the stub
  p <- rtf_plan(.rd()) |> plan_stub(c("SOC", "PT")) |> plan_group("SOC")
  expect_setequal(p$layers$roles$SOC$roles, c("stub", "group"))
})

test_that("options are last writer wins, per field", {
  p <- rtf_plan(.rd()) |>
    plan_roles(SOC = role("group", mode = "value", order = 1L)) |>
    plan_roles(SOC = role("group", mode = "indent"))
  expect_identical(p$layers$roles$SOC$opts$mode, "indent")
  expect_identical(p$layers$roles$SOC$opts$order, 1L)   # untouched
})

test_that("plan_unset() takes a declaration back", {
  p <- rtf_plan(.rd()) |> plan_hide("CARRIER") |> plan_unset("CARRIER")
  expect_null(p$layers$roles$CARRIER)
  expect_identical(resolve_plan(p)$columns$hidden, character(0))
})

test_that("every argument must name a column", {
  expect_error(plan_roles(rtf_plan(.rd()), "group"), "must be named")
})

test_that("a column that does not exist is rejected", {
  expect_error(resolve_plan(rtf_plan(.rd()) |> plan_roles(NOPE = "group")),
               "No such column")
})

# ── the projection ─────────────────────────────────────────────────────────

test_that("a merged stub projects the same columns as as_rtftables()", {
  res <- resolve_plan(rtf_plan(.rd()) |> plan_stub(c("SOC", "PT")) |>
                        plan_hide("CARRIER"))
  a <- as_rtftables(.rd(), stub_vars = c("SOC", "PT"),
                    drop_cols = "CARRIER")[[1L]]
  expect_identical(res$columns$names, names(a$data))
})

test_that("layout = columns leaves every column where it was", {
  res <- resolve_plan(rtf_plan(.rd()) |>
                        plan_stub(c("SOC", "PT"), layout = "columns"))
  expect_identical(res$columns$names, c("SOC", "PT", "CARRIER", "N"))
})

test_that("hiding removes a column from the projection", {
  res <- resolve_plan(rtf_plan(.rd()) |> plan_hide("CARRIER"))
  expect_identical(res$columns$names, c("SOC", "PT", "N"))
  expect_identical(res$columns$hidden, "CARRIER")
})

test_that("no roles at all projects the source layout unchanged", {
  res <- resolve_plan(rtf_plan(.rd()))
  expect_identical(res$columns$names, names(.rd()))
})

# ── the one map ────────────────────────────────────────────────────────────

test_that("one map answers where every column ended up", {
  res <- resolve_plan(rtf_plan(.rd()) |> plan_stub(c("SOC", "PT")) |>
                        plan_group("SOC") |> plan_hide("CARRIER"))
  m <- plan_position(res, c("SOC", "PT", "CARRIER", "N"))
  expect_identical(unname(m), c(1L, 1L, NA_integer_, 2L))
})

test_that("a merged hierarchy column answers with the stub's position", {
  res <- resolve_plan(rtf_plan(.rd()) |> plan_stub(c("SOC", "PT")))
  expect_identical(unname(plan_position(res, "SOC")), 1L)
  expect_identical(unname(plan_position(res, "PT")), 1L)
})

test_that("a hidden column answers NA rather than a stale position", {
  res <- resolve_plan(rtf_plan(.rd()) |> plan_hide("CARRIER"))
  expect_true(is.na(plan_position(res, "CARRIER")))
})

test_that("carry positions are final positions, not source ones", {
  res <- resolve_plan(rtf_plan(.rd()) |> plan_stub(c("SOC", "PT")) |>
                        plan_hide("CARRIER") |> plan_roles(N = role("carry")))
  expect_identical(res$columns$carry, 2L)
})

test_that("the map covers every source column exactly once", {
  res <- resolve_plan(rtf_plan(.rd()) |> plan_stub(c("SOC", "PT")) |>
                        plan_hide("CARRIER"))
  expect_identical(names(res$columns$map), names(.rd()))
})

# ── contradictions ─────────────────────────────────────────────────────────

test_that("a column cannot be hidden and printed at once", {
  expect_error(
    resolve_plan(rtf_plan(.rd()) |> plan_stub(c("SOC", "PT")) |>
                   plan_hide("SOC")),
    "hidden and printed"
  )
  expect_error(
    resolve_plan(rtf_plan(.rd()) |> plan_roles(N = role("carry")) |>
                   plan_hide("N")),
    "hidden and printed"
  )
})

test_that("two grouping columns are refused rather than silently ranked", {
  expect_error(
    resolve_plan(rtf_plan(.rd()) |> plan_roles(SOC = "group", PT = "group")),
    "single column"
  )
})

# ── grouping now lives in the role table ───────────────────────────────────

test_that("grouping declared as a role drives the blank rows", {
  d <- data.frame(PT  = paste0("PT", 1:12),
                  SOC = rep(c("A", "B", "C"), each = 4L),
                  stringsAsFactors = FALSE)
  res <- resolve_plan(rtf_plan(d) |> plan_roles(SOC = "group") |>
                        plan_blanks("between_groups"))
  expect_identical(res$blanks, c(4L, 8L))
})

test_that("plan_group() and plan_roles() write to the same place", {
  a <- rtf_plan(.rd()) |> plan_group("SOC")
  b <- rtf_plan(.rd()) |> plan_roles(SOC = "group")
  expect_identical(a$layers$roles$SOC$roles, b$layers$roles$SOC$roles)
})

test_that("the grouping mode survives as a role option", {
  d <- data.frame(lbl = c("A", "  a1", "  a2", "B", "  b1"),
                  N = as.character(1:5), stringsAsFactors = FALSE)
  res <- resolve_plan(rtf_plan(d) |>
                        plan_roles(lbl = role("group", mode = "indent")) |>
                        plan_blanks("between_groups"))
  expect_identical(res$blanks, 3L)
})
