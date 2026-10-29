## tests/testthat/test-sort-by.R
##
## as_rtftables(sort_by=, sort_desc=): order body rows before pagination.
## Covers single / multi-key sorts, direction, stability, NA placement, the
## sort-key-carrier pattern (sort_by + drop_cols), feeding grouping, and
## validation.

library(testthat)

df4 <- function() {
  data.frame(
    grp   = c("B", "A", "B", "A"),
    ord   = c(2, 1, 1, 2),
    Label = c("b2", "a1", "b1", "a2"),
    N     = c("20", "10", "30", "40"),
    stringsAsFactors = FALSE
  )
}

# ── basic ordering ────────────────────────────────────────────────────────────

test_that("sort_by orders rows ascending by a single column", {
  res <- as_rtftables(df4(), sort_by = "Label")[[1L]]
  expect_identical(res$data$Label, c("a1", "a2", "b1", "b2"))
})

test_that("sort_by accepts an integer column index", {
  res <- as_rtftables(df4(), sort_by = 3L)[[1L]]      # Label
  expect_identical(res$data$Label, c("a1", "a2", "b1", "b2"))
})

test_that("sort_desc = TRUE sorts descending", {
  res <- as_rtftables(df4(), sort_by = "N", sort_desc = TRUE)[[1L]]
  expect_identical(res$data$N, c("40", "30", "20", "10"))
})

test_that("multi-key sort applies keys in priority order with per-key direction", {
  res <- as_rtftables(df4(), sort_by = c("grp", "ord"),
                      sort_desc = c(FALSE, TRUE))[[1L]]
  expect_identical(paste0(res$data$grp, res$data$ord),
                   c("A2", "A1", "B2", "B1"))
})

test_that("a list mixes name and index sort keys", {
  res <- as_rtftables(df4(), sort_by = list("grp", 2L))[[1L]]
  expect_identical(paste0(res$data$grp, res$data$ord),
                   c("A1", "A2", "B1", "B2"))
})

# ── stability + NA ────────────────────────────────────────────────────────────

test_that("sort is stable: equal keys keep input order", {
  df <- data.frame(k = c("x", "x", "x"),
                   v = c("first", "second", "third"),
                   stringsAsFactors = FALSE)
  res <- as_rtftables(df, sort_by = "k")[[1L]]
  expect_identical(res$data$v, c("first", "second", "third"))
})

test_that("NA keys sort last (ascending and descending)", {
  df <- data.frame(k = c("b", NA, "a"), v = c("B", "X", "A"),
                   stringsAsFactors = FALSE)
  expect_identical(as_rtftables(df, sort_by = "k")[[1L]]$data$v,
                   c("A", "B", "X"))
  expect_identical(
    as_rtftables(df, sort_by = "k", sort_desc = TRUE)[[1L]]$data$v,
    c("B", "A", "X"))
})

# ── interaction with drop_cols / grouping ─────────────────────────────────────

test_that("sort on a hidden carrier column (sort_by + drop_cols)", {
  res <- as_rtftables(df4(), sort_by = "ord", drop_cols = "ord")[[1L]]
  expect_false("ord" %in% names(res$data))
  # ord = c(2,1,1,2); stable asc -> rows with ord 1 first (a1,b1), then ord 2
  expect_identical(res$data$Label, c("a1", "b1", "b2", "a2"))
})

test_that("sort feeds group detection / by_value pagination", {
  res <- as_rtftables(df4(), split = "by_value", group_col = "grp",
                      sort_by = "grp", drop_cols = "grp")
  expect_identical(names(res), c("A", "B"))
  expect_identical(res[["A"]]$data$Label, c("a1", "a2"))
})

test_that("cell_styles follow the sorted row order", {
  cs <- list(
    list(bold = c(TRUE,  FALSE, FALSE, FALSE)),   # row1 (Label b2)
    list(bold = c(FALSE, TRUE,  FALSE, FALSE)),   # row2 (Label a1)
    list(bold = c(FALSE, FALSE, TRUE,  FALSE)),   # row3 (Label b1)
    list(bold = c(FALSE, FALSE, FALSE, TRUE))     # row4 (Label a2)
  )
  res <- as_rtftables(df4(), sort_by = "Label", cell_styles = cs)[[1L]]
  # sorted order a1,a2,b1,b2 -> original rows 2,4,3,1
  expect_identical(res$data$Label, c("a1", "a2", "b1", "b2"))
  expect_identical(res$cell_styles[[1L]]$bold, c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(res$cell_styles[[2L]]$bold, c(FALSE, FALSE, FALSE, TRUE))
})

# ── no-op + validation ────────────────────────────────────────────────────────

test_that("sort_by = NULL keeps the input order", {
  res <- as_rtftables(df4(), sort_by = NULL)[[1L]]
  expect_identical(res$data$Label, c("b2", "a1", "b1", "a2"))
})

test_that("sort_by is honoured through a list input", {
  res <- as_rtftables(list(t1 = df4()), sort_by = "Label")
  expect_identical(res[[1L]]$data$Label, c("a1", "a2", "b1", "b2"))
})

test_that("as_rtftable() inherits sort_by", {
  rt <- as_rtftable(df4(), sort_by = "Label")
  expect_identical(rt$data$Label, c("a1", "a2", "b1", "b2"))
})

test_that("sort_by rejects unknown names and out-of-range indices", {
  expect_error(as_rtftables(df4(), sort_by = "nope"), "not found")
  expect_error(as_rtftables(df4(), sort_by = 99L), "out of range")
})

test_that("sort_desc length must be 1 or length(sort_by)", {
  expect_error(
    as_rtftables(df4(), sort_by = c("grp", "ord"),
                 sort_desc = c(TRUE, FALSE, TRUE)),
    "length 1 or 2")
})

test_that("a listing is sorted record by record: each keeps its lines and its blank row (#745)", {
  d <- data.frame(USUBJID = c("S2", "S1", "S3"),
                  AEDECOD = c("Rash", "A term long enough to wrap", "Nausea"))
  spec <- listing_spec(list(listing_col("USUBJID", label = "Subject"),
                            listing_col("AEDECOD", label = "Term", width = 10)))
  tbl <- as_rtftables(d, listing = spec, sort_by = "USUBJID")
  if (!inherits(tbl, "rtftable")) tbl <- tbl[[1L]]
  ids <- tbl$data$USUBJID
  terms <- tbl$data$AEDECOD
  # S1's record first, its wrapped lines and blank row under it; no blank
  # row gathered at the top
  expect_identical(ids[nzchar(ids)], c("S1", "S2", "S3"))
  expect_true(nzchar(ids[1L]))
  s1 <- which(ids == "S1")
  s2 <- which(ids == "S2")
  expect_true(all(nzchar(terms[(s1 + 1L):(s2 - 2L)])))   # S1's wrapped lines
  expect_identical(terms[s2 - 1L], "")                   # its blank row
  expect_identical(utils::tail(terms, 1L), "")           # S3's blank row last
  # descending: whole records too
  dn <- as_rtftables(d, listing = spec, sort_by = "USUBJID", sort_desc = TRUE)
  if (!inherits(dn, "rtftable")) dn <- dn[[1L]]
  idn <- dn$data$USUBJID
  expect_identical(idn[nzchar(idn)], c("S3", "S2", "S1"))
  expect_true(nzchar(idn[1L]))
})

test_that("plan_sort() on a listing plan sorts the records whole (#745)", {
  d <- data.frame(USUBJID = c("S2", "S1", "S3"), AEDECOD = c("Rash", "Cough", "Nausea"))
  p <- table_plan(d) |>
    plan_listing(listing_col("USUBJID", label = "Subject"), listing_col("AEDECOD", label = "Term")) |>
    plan_sort("USUBJID")
  r <- plan_apply(p)
  if (!inherits(r, "rtftable")) r <- r[[1L]]
  expect_identical(r$data$USUBJID, c("S1", "", "S2", "", "S3", ""))
})

test_that("-n@group1 keeps each group's rows together when two groups tie (#760)", {
  ae <- expand.grid(ARM = c("Placebo", "Drug A 20 mg"), AEBODSYS = c("Cardiac", "Nervous", "Skin"),
                    AEDECOD = c("t1", "t2"), stringsAsFactors = FALSE)
  ae$AEDECOD <- paste(substr(ae$AEBODSYS, 1, 1), ae$AEDECOD)
  ae$n <- c(3, 4, 1, 1, 2, 2, 2, 1, 0, 4, 1, 3)
  ae$n[ae$ARM == "Drug A 20 mg"] <- c(3, 1, 2, 2, 4, 1)   # Cardiac 5, Nervous 5, Skin 3
  soc <- aggregate(n ~ ARM + AEBODSYS, ae, sum)
  soc$AEDECOD <- NA
  x <- rbind(ae, soc[names(ae)])
  x$variable <- ifelse(is.na(x$AEDECOD), "AEBODSYS", "AEDECOD")
  x$.label <- ifelse(is.na(x$AEDECOD), x$AEBODSYS, x$AEDECOD)
  x$.kind <- "categorical"
  x$stat_name <- "n"
  x$stat <- x$n
  x$n <- NULL
  x$.depth <- ifelse(x$variable == "AEBODSYS", 1, 2)
  p <- table_plan(x, cols = "ARM", rows = c(group1 = "AEBODSYS")) |> plan_cells(categorical = "{n}")
  a <- p |> plan_sort(".overall", "-n@group1[Drug A 20 mg]", ".depth", "-n[Drug A 20 mg]", "label") |>
    plan_apply("table")
  expect_identical(a$label, c("Cardiac", "C t1", "C t2", "Nervous", "N t2", "N t1",
                              "Skin", "S t1", "S t2"))
  # the same as with the group written after it
  b <- p |> plan_sort(".overall", "-n@group1[Drug A 20 mg]", "group1", ".depth",
                      "-n[Drug A 20 mg]", "label") |> plan_apply("table")
  expect_identical(a$label, b$label)
})
