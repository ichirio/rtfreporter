# SPIKE (design/plan-resolver): a listing declared with the table's own layers.
#
# The claim: once a plan can reshape, a listing needs no vocabulary of its own
# beyond the reshape.  Everything else -- hiding, grouping, pagination, column
# alignment, repeated-value suppression, page furniture, the header -- is the
# SAME layer a table uses, addressed by the printed column's name.
#
# Each case that has an as_rtftables() equivalent is required to agree with it
# at both levels, the criterion the rest of the spike is held to.

.adsl <- function(n = 18L) {
  hist <- c("ADENOCARCINOMA", "SQUAMOUS CELL CARCINOMA", "LARGE CELL CARCINOMA")
  data.frame(
    USUBJID = sprintf("01-701-1%03d", seq_len(n)),
    ARM     = rep(c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"),
                  length.out = n),
    HIST    = rep(hist, length.out = n),
    BRCA    = rep(c("BRCA1", NA, "BRCA2"), length.out = n),
    AGE     = as.character(50L + seq_len(n)),
    stringsAsFactors = FALSE
  )
}

.cols <- function() list(
  listing_col("USUBJID", width = 11, label = "Unique Subject ID"),
  listing_col(c("HIST", "BRCA"), width = 16, label = "Histology/Mutation"),
  listing_col("ARM", width = 12, label = "Treatment Arm"),
  listing_col("AGE", width = 3, label = "Age")
)

.same <- function(a, b, label) {
  obj <- plan_compare_objects(a, b)
  expect_identical(nrow(obj), 0L,
                   info = paste0(label, ": objects differ -- ",
                                 paste(obj$field, collapse = ", ")))
  rtf <- plan_compare_rtf(plan_render_lines(a), plan_render_lines(b))
  expect_true(rtf$identical,
              info = paste0(label, ": RTF differs at line(s) ",
                            paste(utils::head(rtf$diffs$line, 5L),
                                  collapse = ", ")))
}

# ── the acceptance criterion ───────────────────────────────────────────────

test_that("a plan listing renders exactly what as_rtftables(listing =) does", {
  d <- .adsl()
  for (mr in list(NULL, 12L, 20L)) {
    old <- as_rtftables(d, listing = listing_spec(.cols()), max_rows = mr,
                        border = "tfl")
    new <- rtf_plan(d) |>
      plan_listing(.cols()) |>
      plan_blanks(first = TRUE) |>          # listing_spec's blank_row_first
      plan_pages(max_rows = mr) |>
      plan_style(border = "tfl") |>
      rtf_pages()
    .same(old, new, paste("max_rows =", mr %||% "NULL"))
  }
})

test_that("the pre-built body is the same plan as the declared one", {
  d    <- .adsl()
  spec <- listing_spec(.cols(), blank_row_first = FALSE)
  a <- rtf_plan(build_listing(d, spec)) |>
    plan_blanks(first = TRUE) |>
    plan_pages(max_rows = 12L) |> plan_style(border = "tfl") |> rtf_pages()
  b <- rtf_plan(d) |> plan_listing(.cols()) |>
    plan_blanks(first = TRUE) |>
    plan_pages(max_rows = 12L) |> plan_style(border = "tfl") |> rtf_pages()
  .same(a, b, "pre-built vs declared")
})

# ── the record column is an ordinary hidden grouping carrier ───────────────

test_that("the record column groups the body and is not printed", {
  res <- resolve_plan(rtf_plan(.adsl()) |> plan_listing(.cols()))
  expect_identical(res$columns$hidden, ".rtf_record")
  expect_identical(res$columns$group, ".rtf_record")
  expect_true(is.na(unname(plan_position(res, ".rtf_record"))))
  expect_false(".rtf_record" %in% res$columns$names)
  # one group per source record
  expect_identical(length(unique(res$groups$id)), nrow(.adsl()))
})

test_that("keeping records whole is plan_pages(), not a hidden strategy name", {
  p <- rtf_plan(.adsl()) |> plan_listing(.cols()) |>
    plan_pages(max_rows = 12L)                     # groups = "keep" by default
  res <- resolve_plan(p)
  rec <- res$rows$body[[".rtf_record"]]
  for (idx in res$pages) {
    # no record straddles a page boundary
    whole <- vapply(unique(rec[idx]), function(r)
      sum(rec[idx] == r) == sum(rec == r), logical(1L))
    expect_true(all(whole))
  }
})

test_that("a grouping the caller declares wins, and the record stays hidden", {
  res <- resolve_plan(rtf_plan(.adsl()) |> plan_listing(.cols()) |>
                        plan_group("ARM"))
  expect_identical(res$columns$group, "ARM")
  expect_true(".rtf_record" %in% res$columns$hidden)
})

test_that("one page per record is plan_pages(), with nothing listing-specific", {
  pages <- rtf_plan(.adsl(4L)) |> plan_listing(.cols()) |>
    plan_pages(per_group = TRUE) |> rtf_pages()
  expect_identical(length(pages), 4L)          # the record IS the grouping
})

test_that("a listing cannot yet be grouped by one of its printed columns", {
  # KNOWN GAP, recorded rather than worked around, and it is not the plan's:
  # build_listing() emits printed columns, gutters and the record column, and
  # nothing else.  A printed column is LAID OUT -- wrapped, padded with "" on
  # the rows below the first, and followed by the record's own blank row -- so
  # its runs are broken and grouping on it counts far more groups than there
  # are values.  Grouping a listing by treatment arm therefore needs an
  # unwrapped carrier column that build_listing() does not produce, and this
  # is exactly as true of as_rtftables(listing = ) today.
  cols <- .cols()
  cols[[3L]] <- listing_col("ARM", width = 22, label = "Treatment Arm")
  res <- resolve_plan(rtf_plan(.adsl()) |> plan_listing(cols) |>
                        plan_roles(ARM = role("sort")) |>
                        plan_group("ARM", mode = "value"))
  expect_gt(length(unique(res$groups$id)), 3L)
})

# ── the shared layers reach the printed columns ────────────────────────────

test_that("a column style is declared the way a table declares one", {
  res <- resolve_plan(rtf_plan(.adsl()) |> plan_listing(.cols()) |>
                        plan_roles(AGE = role("display", align = "right")))
  pos <- unname(plan_position(res, "AGE"))
  entry <- Filter(function(e) identical(e$col, as.integer(pos)),
                  res$style$col_spec)
  expect_length(entry, 1L)
  expect_identical(entry[[1L]]$align, "right")     # beats the listing's own
})

test_that("a printed column can be hidden by name", {
  res <- resolve_plan(rtf_plan(.adsl()) |> plan_listing(.cols()) |>
                        plan_hide("AGE"))
  expect_false("AGE" %in% res$columns$names)
  expect_true(is.na(unname(plan_position(res, "AGE"))))
})

test_that("a header declared on the plan beats the one the listing derived", {
  res <- resolve_plan(rtf_plan(.adsl()) |> plan_listing(.cols()) |>
                        plan_header(c("ID", "", "Histology", "", "Arm", "",
                                      "Age")))
  expect_identical(res$header$col_header[[1L]][[1L]], "ID")
  expect_identical(res$header$col_header[[1L]][[7L]], "Age")
})

test_that("plan_blanks() is what puts a blank atop every page", {
  mk <- function(...) rtf_plan(.adsl()) |> plan_listing(.cols()) |>
    plan_pages(max_rows = 12L) |> (\(p) p)() |> rtf_pages()
  bare <- rtf_plan(.adsl()) |> plan_listing(.cols()) |>
    plan_pages(max_rows = 12L) |> rtf_pages()
  furn <- rtf_plan(.adsl()) |> plan_listing(.cols()) |>
    plan_blanks(first = TRUE) |> plan_pages(max_rows = 12L) |> rtf_pages()
  expect_gt(length(furn), 1L)
  for (i in seq_along(bare)) expect_false(0L %in% bare[[i]]$blank_rows)
  for (i in seq_along(furn)) expect_true(0L %in% furn[[i]]$blank_rows)
})

# ── repeated values ────────────────────────────────────────────────────────

test_that("collapse_repeats becomes a role, and repeats per page", {
  cols <- list(
    listing_col("ARM", width = 12, label = "Treatment Arm",
                collapse_repeats = TRUE),
    listing_col("USUBJID", width = 11, label = "Unique Subject ID"))
  d <- .adsl(9L)
  d <- d[order(d$ARM), ]
  rownames(d) <- NULL

  res <- resolve_plan(rtf_plan(d) |> plan_listing(cols) |> plan_group("ARM"))
  expect_true("ARM" %in% res$columns$collapse)

  old <- as_rtftables(d, listing = listing_spec(cols), max_rows = 6L,
                      border = "tfl")
  new <- rtf_plan(d) |> plan_listing(cols) |> plan_blanks(first = TRUE) |>
    plan_pages(max_rows = 6L) |> plan_style(border = "tfl") |> rtf_pages()
  .same(old, new, "collapse_repeats")

  # The point of suppressing per PAGE: a run continued across a break shows
  # its value again at the top of the next page.
  expect_gt(length(new), 1L)
  for (pg in new) expect_true(nzchar(pg$data[[1L]][[1L]]))
})

# ── the settings that moved out, and the ones that stayed ──────────────────

test_that("plan_listing() does not accept the settings a layer already owns", {
  fm <- names(formals(plan_listing))
  expect_false("blank_row_first" %in% fm)          # -> plan_blanks(first = )
  expect_false("align" %in% fm)                    # -> plan_roles(role(align=))
  expect_true("blank_row" %in% fm)                 # part of the reshape
  # What is left is the reshape and nothing else.
  expect_setequal(setdiff(fm, c("plan", "...")),
                  c("type", "sep", "spacer", "spacer_rel_width", "blank_row",
                    "layout", "wrap", "record"))
})

test_that("a spec's own blank_row_first cannot come in through the back door", {
  res <- resolve_plan(rtf_plan(.adsl()) |> plan_listing(.cols()))
  expect_false(isTRUE(res$listing$blank_row_first))
  expect_false(isTRUE(unname(res$blank_edges[["first"]])))
})

# ── declaration errors ─────────────────────────────────────────────────────

test_that("columns are required", {
  expect_error(resolve_plan(rtf_plan(.adsl()) |> plan_listing()),
               "at least one column")
})

test_that("a body that is already a listing cannot be declared again", {
  body <- build_listing(.adsl(), listing_spec(.cols()))
  expect_error(rtf_plan(body) |> plan_listing(.cols()),
               "already produced")
})

test_that("columns may be spread over `...` or passed as one list", {
  a <- resolve_plan(rtf_plan(.adsl()) |> plan_listing(.cols()))
  b <- resolve_plan(rtf_plan(.adsl()) |> plan_listing(
    listing_col("USUBJID", width = 11, label = "Unique Subject ID"),
    listing_col(c("HIST", "BRCA"), width = 16, label = "Histology/Mutation"),
    listing_col("ARM", width = 12, label = "Treatment Arm"),
    listing_col("AGE", width = 3, label = "Age")))
  expect_identical(a$rows$body, b$rows$body)
  expect_identical(a$header$col_header, b$header$col_header)
})

test_that("a plain column name is accepted, as listing_spec() accepts one", {
  res <- resolve_plan(rtf_plan(.adsl()) |> plan_listing("USUBJID", "ARM"))
  expect_identical(res$columns$names, c("USUBJID", ".sp1", "ARM"))
})

# ── the row map survives the reshape ───────────────────────────────────────

test_that("per-source-row data still reaches the page it belongs to", {
  d   <- .adsl(9L)
  res <- resolve_plan(rtf_plan(d) |> plan_listing(.cols()) |>
                        plan_pages(max_rows = 12L))
  # One style per SOURCE record; the map spreads it over that record's lines.
  styles <- as.list(seq_len(nrow(d)))
  mapped <- plan_row_map(res, styles)
  expect_length(mapped, res$nrow)
  rec <- res$rows$body[[".rtf_record"]]
  expect_identical(unlist(mapped), as.integer(rec))
  # and each page gets exactly its own records
  for (i in seq_along(res$pages)) {
    expect_setequal(unique(plan_page_source_rows(res, i)),
                    unique(rec[res$pages[[i]]]))
  }
})


# ── sorting a listing ──────────────────────────────────────────────────────

test_that("a listing sorts its RECORDS, not its physical rows", {
  d <- .adsl(9L)
  res <- resolve_plan(rtf_plan(d) |> plan_listing(.cols()) |>
                        plan_roles(ARM = role("sort")))
  arm <- res$rows$body[["ARM"]]
  rec <- res$rows$body[[".rtf_record"]]
  # Every record's lines stay together ...
  expect_identical(rec, sort(rec))
  # ... and the records themselves come out in the source column's order.
  first <- arm[!duplicated(rec)]
  first <- first[nzchar(first)]
  expect_identical(first, sort(first))
})

test_that("a sort carrier the listing does not print still sorts it", {
  d <- .adsl(9L)
  d$ORD <- rev(seq_len(nrow(d)))            # not one of the printed columns
  res <- resolve_plan(rtf_plan(d) |> plan_listing(.cols()) |>
                        plan_roles(ORD = role("sort")))
  ids <- res$rows$body[["USUBJID"]]
  ids <- ids[nzchar(ids)]
  expect_identical(ids[[1L]], d$USUBJID[[nrow(d)]])
  expect_false("ORD" %in% names(res$rows$body))
})

test_that("a role on a column the listing consumed is refused, by name", {
  expect_error(
    resolve_plan(rtf_plan(.adsl()) |> plan_listing(.cols()) |>
                   plan_roles(BRCA = role("display", align = "right"))),
    "consumed the source column")
})

# ── rlistings ──────────────────────────────────────────────────────────────

test_that("an rlistings listing is read as one, not as a plain data.frame", {
  skip_if_not_installed("rlistings")
  d <- data.frame(SOC = c("A", "A", "B"), PT = c("p1", "p2", "p3"),
                  N = c("1", "2", "3"), stringsAsFactors = FALSE)
  l <- rlistings::as_listing(d, key_cols = "SOC", disp_cols = c("PT", "N"))
  # A listing_df IS a data.frame subclass, so a dispatch that forgets it does
  # not error -- it silently drops disp_cols, key suppression and titles (#322).
  a <- as_rtftables(l, border = "tfl")
  b <- rtf_plan(l) |> plan_style(border = "tfl") |> rtf_pages()
  .same(a, b, "rlistings source")
})

test_that("plan_listing() is refused on a listing rlistings already laid out", {
  skip_if_not_installed("rlistings")
  d <- data.frame(SOC = c("A", "B"), PT = c("p1", "p2"),
                  stringsAsFactors = FALSE)
  l <- rlistings::as_listing(d, key_cols = "SOC")
  expect_error(rtf_plan(l) |> plan_listing("PT"), "already laid it out")
})
