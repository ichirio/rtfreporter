# stub_cols(): merge hierarchy columns into one indented stub column

nbsp <- intToUtf8(160L)

ae <- data.frame(
  soc = c("Cardiac disorders", "Cardiac disorders",
          "Gastrointestinal disorders"),
  pt  = c("Atrial fibrillation", "Bradycardia", "Nausea"),
  n   = c("3 (2.1%)", "1 (0.7%)", "5 (3.5%)"),
  stringsAsFactors = FALSE
)

test_that("two-level merge inserts label rows and indents leaf rows", {
  out <- stub_cols(ae, vars = c("soc", "pt"), label = "SOC / PT")

  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("SOC / PT", "n"))
  # 3 leaf rows + 2 SOC label rows
  expect_identical(nrow(out), 5L)
  expect_identical(
    out[[1L]],
    c("Cardiac disorders",
      paste0(strrep(nbsp, 4L), "Atrial fibrillation"),
      paste0(strrep(nbsp, 4L), "Bradycardia"),
      "Gastrointestinal disorders",
      paste0(strrep(nbsp, 4L), "Nausea")))
  # label rows carry NA in the non-stub columns
  expect_identical(out$n, c(NA, "3 (2.1%)", "1 (0.7%)", NA, "5 (3.5%)"))
})

test_that("a repeated parent later in the data starts a new run (RLE)", {
  scattered <- ae[c(1L, 3L, 2L), ]
  out <- stub_cols(scattered, vars = c("soc", "pt"))
  # Cardiac appears twice -> two label rows (sort first to avoid this)
  expect_identical(sum(out[[1L]] == "Cardiac disorders"), 2L)
})

test_that("an empty / NA parent emits no label row and no indent", {
  ae2 <- rbind(
    data.frame(soc = "", pt = "Any adverse event", n = "9 (6.3%)",
               stringsAsFactors = FALSE),
    ae
  )
  out <- stub_cols(ae2, vars = c("soc", "pt"))
  expect_identical(out[[1L]][1L], "Any adverse event")   # flush left, no label row
  expect_identical(nrow(out), 6L)                        # 4 leaves + 2 labels

  ae2$soc[1L] <- NA
  out_na <- stub_cols(ae2, vars = c("soc", "pt"))
  expect_identical(out_na[[1L]], out[[1L]])
})

test_that("three levels nest the indentation", {
  df <- data.frame(
    a = c("A", "A", "A"),
    b = c("b1", "b1", "b2"),
    c = c("x", "y", "z"),
    v = 1:3,
    stringsAsFactors = FALSE
  )
  out <- stub_cols(df, vars = c("a", "b", "c"), indent = 2L)
  expect_identical(
    out[[1L]],
    c("A",
      paste0(strrep(nbsp, 2L), "b1"),
      paste0(strrep(nbsp, 4L), "x"),
      paste0(strrep(nbsp, 4L), "y"),
      paste0(strrep(nbsp, 2L), "b2"),
      paste0(strrep(nbsp, 4L), "z")))
  # numeric column: label rows are NA, leaf rows keep their values
  expect_identical(out$v, c(NA, NA, 1L, 2L, NA, 3L))
})

test_that("a change in a higher level restarts the lower level's run", {
  df <- data.frame(
    a = c("A", "B"),
    b = c("b1", "b1"),     # same value, but a changed -> new b1 label row
    c = c("x", "y"),
    stringsAsFactors = FALSE
  )
  out <- stub_cols(df, vars = c("a", "b", "c"), indent = 1L)
  expect_identical(sum(out[[1L]] == paste0(nbsp, "b1")), 2L)
})

test_that("default label joins the display names with ' / '", {
  out <- stub_cols(ae, vars = c("soc", "pt"))
  expect_identical(names(out)[1L], "soc / pt")

  # a `label` attribute on a merged column feeds the default
  ae_lab <- ae
  attr(ae_lab$soc, "label") <- "System Organ Class"
  attr(ae_lab$pt,  "label") <- "Preferred Term"
  out_lab <- stub_cols(ae_lab, vars = c("soc", "pt"))
  expect_identical(names(out_lab)[1L], "System Organ Class / Preferred Term")
})

test_that("label attributes on the kept columns are preserved", {
  ae_lab <- ae
  attr(ae_lab$n, "label") <- "Subjects, n (%)"
  out <- stub_cols(ae_lab, vars = c("soc", "pt"))
  expect_identical(attr(out$n, "label", exact = TRUE), "Subjects, n (%)")
})

test_that("integer and mixed list vars are accepted", {
  out_int <- stub_cols(ae, vars = c(1L, 2L))
  out_mix <- stub_cols(ae, vars = list("soc", 2L))
  ref     <- stub_cols(ae, vars = c("soc", "pt"))
  expect_identical(out_int[[1L]], ref[[1L]])
  expect_identical(out_mix[[1L]], ref[[1L]])
})

test_that("validation errors are raised", {
  expect_error(stub_cols(list(a = 1), vars = c("a", "b")),
               "must be a data.frame")
  expect_error(stub_cols(ae, vars = "soc"), "at least two columns")
  expect_error(stub_cols(ae, vars = c("soc", "soc")), "distinct")
  expect_error(stub_cols(ae, vars = c("soc", "nope")), "not found")
  expect_error(stub_cols(ae, vars = c(1L, 9L)), "out of range")
  expect_error(stub_cols(ae, vars = c("soc", "pt"), label = c("a", "b")),
               "single string")
  expect_error(stub_cols(ae, vars = c("soc", "pt"), indent = -1L),
               "non-negative")
})

test_that("zero-row input yields a zero-row stub layout", {
  out <- stub_cols(ae[0L, ], vars = c("soc", "pt"))
  expect_identical(nrow(out), 0L)
  expect_identical(names(out), c("soc / pt", "n"))
})

test_that("output feeds the group-aware pagination via indent detection", {
  big <- data.frame(
    soc = rep(c("SOC A", "SOC B"), each = 4L),
    pt  = paste0("PT", 1:8),
    n   = as.character(1:8),
    stringsAsFactors = FALSE
  )
  tbl   <- stub_cols(big, vars = c("soc", "pt"))
  pages <- as_rtftables(tbl, split = "group_safe", max_rows = 6L)
  expect_identical(length(pages), 2L)
  # group_safe keeps each SOC (label row + its 4 leaves) on one page
  p1 <- pages[[1L]]$data[[1L]]
  expect_identical(p1[1L], "SOC A")
  expect_false(any(p1 == "SOC B"))
})
