# as_rtftables() is an S3 generic (#487): a package that depends on
# rtfreporter makes its own class a source by registering a method, and
# rtf_tables() then accepts that object directly.  rtfreporter names no such
# class; these tests register one the way a package's NAMESPACE would.

.register_mock_source <- function() {
  registerS3method("as_rtftables", "rtfr_mock_source",
                   function(x, ...) as_rtftables(x$df, ...),
                   envir = asNamespace("rtfreporter"))
}

.mock_source <- function() {
  structure(list(df = data.frame(Parameter = c("A", "B", "C", "D"),
                                 Value = c("1", "2", "3", "4"),
                                 stringsAsFactors = FALSE)),
            class = "rtfr_mock_source")
}

test_that("as_rtftables() dispatches, and the default is the old function", {
  # Checked by behaviour, not by body(): covr instruments function bodies,
  # so a body comparison fails under test-coverage only.
  expect_true(is.function(rtfreporter:::as_rtftables.default))
  expect_identical(utils::getS3method("as_rtftables", "default"),
                   rtfreporter:::as_rtftables.default)

  d <- data.frame(x = c("a", "b"), y = c("1", "2"), stringsAsFactors = FALSE)
  expect_identical(as_rtftables(d),
                   rtfreporter:::as_rtftables.default(d))
})

test_that("a registered method is used, and forwards the arguments", {
  .register_mock_source()
  pg <- as_rtftables(.mock_source(), split = "rows", split_rows = 3)
  expect_length(pg, 2L)
  expect_true(all(vapply(pg, inherits, logical(1L), "rtftable")))
  expect_identical(pg[[1L]]$data$Parameter, c("A", "B"))
})

test_that("only a class of its own counts as having a method", {
  .register_mock_source()
  expect_true(rtfreporter:::.has_rtftables_method(.mock_source()))
  expect_false(rtfreporter:::.has_rtftables_method(data.frame(a = 1)))
  expect_false(rtfreporter:::.has_rtftables_method(list(1, 2)))
  expect_false(rtfreporter:::.has_rtftables_method("text"))
})

test_that("rtf_tables() accepts an object with a method, as it does a gt_tbl", {
  .register_mock_source()
  src <- .mock_source()
  via_obj   <- rtf_document() |> rtf_tables(src)
  via_pages <- rtf_document() |> rtf_tables(as_rtftables(src))

  f1 <- tempfile(fileext = ".rtf"); f2 <- tempfile(fileext = ".rtf")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  generate_rtfreport(via_obj, f1)
  generate_rtfreport(via_pages, f2)
  expect_identical(readLines(f1), readLines(f2))
})
