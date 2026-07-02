# data.frame column `label` attributes -> header labels ("labels" read_meta
# token, enabled by the default read_meta = TRUE)

make_df <- function() {
  df <- data.frame(
    param = c("Age", "Sex"),
    val   = c("75.1", "53%"),
    stringsAsFactors = FALSE
  )
  attr(df$param, "label") <- "Parameter"
  df
}

test_that("label attributes become header labels by default", {
  rt <- as_rtftables(make_df())[[1L]]
  # mixed: labelled column uses the label, unlabelled keeps its name
  expect_identical(rt$col_header[[1L]], c("Parameter", "val"))
})

test_that("read_meta = FALSE keeps the plain names", {
  rt <- as_rtftables(make_df(), read_meta = FALSE)[[1L]]
  # no col_header extracted -> renderer falls back to names(df)
  expect_null(rt$col_header)
})

test_that("read_meta token control is lenient for data.frames", {
  # the "labels" token enables the read ...
  rt <- as_rtftables(make_df(), read_meta = "labels")[[1L]]
  expect_identical(rt$col_header[[1L]], c("Parameter", "val"))
  # ... an adapter-only token is ignored (no error), labels not read
  rt2 <- as_rtftables(make_df(), read_meta = "titles")[[1L]]
  expect_null(rt2$col_header)
})

test_that("an explicit col_header always wins", {
  rt <- as_rtftables(make_df(), col_header = c("P", "V"))[[1L]]
  expect_identical(rt$col_header[[1L]], c("P", "V"))
})

test_that("label attributes feed the header_sep spanning reconstruction", {
  df <- data.frame(
    param = c("Age", "Sex"),
    a     = c("1", "2"),
    b     = c("3", "4"),
    stringsAsFactors = FALSE
  )
  attr(df$a, "label") <- "Active____n"
  attr(df$b, "label") <- "Active____pct"
  rt <- as_rtftables(df)[[1L]]
  # two header rows: a spanning "Active" cell over the labelled columns
  expect_identical(length(rt$col_header), 2L)
  spans <- rt$col_header[[1L]]
  labs  <- vapply(spans, function(c1) c1$label, character(1L))
  expect_true("Active" %in% labs)
  expect_identical(rt$col_header[[2L]], c("param", "n", "pct"))
})

test_that("unusable label attributes are skipped", {
  df <- make_df()
  attr(df$param, "label") <- ""            # empty -> ignored
  attr(df$val,   "label") <- c("a", "b")   # length > 1 -> ignored
  rt <- as_rtftables(df)[[1L]]
  expect_null(rt$col_header)               # nothing usable -> plain names
})

test_that("a list mixing data.frames shares one read_meta without error", {
  res <- as_rtftables(list(a = make_df(), b = make_df()),
                      read_meta = c("titles", "labels"))
  expect_identical(length(res), 2L)
  expect_identical(res[[1L]]$col_header[[1L]], c("Parameter", "val"))
})
