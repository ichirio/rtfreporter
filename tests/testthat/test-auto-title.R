# rtf_tables(auto_title = TRUE) -- the page name as the last title row (#310).

.pages <- function() {
  d <- data.frame(G = c("Cohort A", "Cohort A", "Cohort B"),
                  Stat = c("n", "Mean", "n"),
                  Value = c("24", "1.5", "18"),
                  stringsAsFactors = FALSE)
  as_rtftables(d, split = "by_value", group_col = "G", border = "tfl")
}

# the text of each stored title row, in order
.title_texts <- function(doc, i) {
  vapply(doc$titles[[i]],
         function(r) if (is.list(r)) r$text else as.character(r),
         character(1L))
}

.render <- function(doc, fmt) {
  doc$default_format <- rtf_default_format(title_format = fmt)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  generate_rtfreport(doc, f, overwrite = TRUE)
  readLines(f, warn = FALSE)
}

# ── the label ──────────────────────────────────────────────────────────────

test_that("the page name becomes the title of an untitled table", {
  doc <- rtf_document() |> rtf_tables(.pages(), auto_title = TRUE)
  expect_identical(.title_texts(doc, 1L), "Cohort A")
  expect_identical(.title_texts(doc, 2L), "Cohort B")
})

test_that("an existing title keeps its place and the name goes last", {
  doc <- rtf_document() |>
    rtf_tables(.pages(), titles = list("Table 14.1.1", "Table 14.1.2"),
               auto_title = TRUE)
  expect_identical(.title_texts(doc, 1L), c("Table 14.1.1", "Cohort A"))
  expect_identical(.title_texts(doc, 2L), c("Table 14.1.2", "Cohort B"))
})

test_that("a multi-row existing title keeps every row", {
  doc <- rtf_document() |>
    rtf_tables(.pages(),
               titles = list(c("Table 14.1.1", "Demographics"),
                             c("Table 14.1.2", "Demographics")),
               auto_title = TRUE)
  expect_identical(.title_texts(doc, 1L),
                   c("Table 14.1.1", "Demographics", "Cohort A"))
})

test_that("the label is left-aligned by default", {
  doc <- rtf_document() |> rtf_tables(.pages(), auto_title = TRUE)
  expect_identical(doc$titles[[1L]][[1L]]$align, "left")
})

test_that("title_label_align moves it", {
  for (a in c("left", "center", "right")) {
    doc <- rtf_document() |>
      rtf_tables(.pages(), auto_title = TRUE, title_label_align = a)
    expect_identical(doc$titles[[1L]][[1L]]$align, a)
  }
})

test_that("an invalid alignment is rejected", {
  expect_error(
    rtf_document() |> rtf_tables(.pages(), auto_title = TRUE,
                                 title_label_align = "middle"),
    "must be \"left\", \"center\", or \"right\""
  )
  expect_error(
    rtf_document() |> rtf_tables(.pages(), auto_title = TRUE,
                                 title_label_align = c("left", "right")),
    "must be \"left\", \"center\", or \"right\""
  )
})

# ── both title formats ─────────────────────────────────────────────────────

test_that("the label renders under either title_format", {
  doc <- rtf_document() |> rtf_tables(.pages(), auto_title = TRUE)
  for (fmt in c("text", "table")) {
    txt <- paste(.render(doc, fmt), collapse = "\n")
    expect_true(grepl("Cohort A", txt, fixed = TRUE))
    expect_true(grepl("Cohort B", txt, fixed = TRUE))
  }
})

test_that("the label carries \\ql in the rendered output", {
  doc <- rtf_document() |> rtf_tables(.pages(), auto_title = TRUE)
  for (fmt in c("text", "table")) {
    hit <- grep("Cohort A", .render(doc, fmt), value = TRUE)
    # the title row, not the body row that also holds the group value
    hit <- hit[!grepl("cellx[0-9]+\\\\?[a-z]*\\\\cellx", hit)]
    expect_true(any(grepl("\\\\ql", hit)))
  }
})

test_that("a right-aligned label carries \\qr", {
  doc <- rtf_document() |>
    rtf_tables(.pages(), auto_title = TRUE, title_label_align = "right")
  hit <- grep("Cohort A", .render(doc, "text"), value = TRUE)
  expect_true(any(grepl("\\\\qr", hit)))
})

# ── gating ─────────────────────────────────────────────────────────────────

test_that("the default leaves titles untouched", {
  doc <- rtf_document() |> rtf_tables(.pages())
  expect_null(doc$titles[[1L]])
  expect_null(doc$titles[[2L]])
})

test_that("an unnamed list is a no-op", {
  doc <- rtf_document() |> rtf_tables(unname(.pages()), auto_title = TRUE)
  expect_null(doc$titles[[1L]])
  expect_null(doc$titles[[2L]])
})

test_that("only the named elements of a partly named list get a label", {
  pages <- .pages()
  names(pages) <- c("Cohort A", "")
  doc <- rtf_document() |> rtf_tables(pages, auto_title = TRUE)
  expect_identical(.title_texts(doc, 1L), "Cohort A")
  expect_null(doc$titles[[2L]])
})

# ── composition ────────────────────────────────────────────────────────────

test_that("auto_title composes with auto_section", {
  doc <- rtf_document() |>
    rtf_tables(.pages(), auto_section = TRUE, auto_title = TRUE)
  # the names survive the auto_section rewrap, which drops list names
  expect_identical(.title_texts(doc, 1L), "Cohort A")
  expect_identical(.title_texts(doc, 2L), "Cohort B")
  n_sec <- sum(vapply(doc$contents, inherits, logical(1L),
                      "rtf_auto_section_item"))
  expect_identical(n_sec, 2L)
})

test_that("a title carried on the object as an attribute is kept", {
  pages <- .pages()
  attr(pages[[1L]], "rtf_titles") <- "From the source object"
  doc <- rtf_document() |> rtf_tables(pages, auto_title = TRUE)
  expect_identical(.title_texts(doc, 1L),
                   c("From the source object", "Cohort A"))
})

test_that("the document renders end to end", {
  doc <- rtf_document() |>
    rtf_tables(.pages(), titles = list("Table 14.1.1", "Table 14.1.2"),
               auto_title = TRUE)
  f <- tempfile(fileext = ".rtf")
  on.exit(unlink(f), add = TRUE)
  expect_silent(generate_rtfreport(doc, f, overwrite = TRUE))
  expect_true(file.exists(f))
})
