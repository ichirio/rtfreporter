# rtf_tables(auto_title = TRUE) -- the page name as the last title row (#310).

# rtf_default_format() / default_format = are deprecated (#804): these tests
# check what they still do, not the warning (test-rtf-style.R does).
silence_deprecation("rtf_default_format")

.pages <- function() {
  d <- data.frame(G = c("Cohort A", "Cohort A", "Cohort B"),
                  Stat = c("n", "Mean", "n"),
                  Value = c("24", "1.5", "18"),
                  stringsAsFactors = FALSE)
  as_rtftables(d, split = "by_value", group_col = "G", border = "tfl")
}

# the text of each stored title row, in order
.title_texts <- function(doc, i) {
  vapply(doc$contents[[i]]$title,
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
  expect_identical(doc$contents[[1L]]$title[[1L]]$align, "left")
})

test_that("title_label_align moves it", {
  for (a in c("left", "center", "right")) {
    doc <- rtf_document() |>
      rtf_tables(.pages(), auto_title = TRUE, title_label_align = a)
    expect_identical(doc$contents[[1L]]$title[[1L]]$align, a)
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
  expect_null(doc$contents[[1L]]$title)
  expect_null(doc$contents[[2L]]$title)
})

test_that("an unnamed list is a no-op", {
  doc <- rtf_document() |> rtf_tables(unname(.pages()), auto_title = TRUE)
  expect_null(doc$contents[[1L]]$title)
  expect_null(doc$contents[[2L]]$title)
})

test_that("only the named elements of a partly named list get a label", {
  pages <- .pages()
  names(pages) <- c("Cohort A", "")
  doc <- rtf_document() |> rtf_tables(pages, auto_title = TRUE)
  expect_identical(.title_texts(doc, 1L), "Cohort A")
  expect_null(doc$contents[[2L]]$title)
})

# ── composition ────────────────────────────────────────────────────────────

test_that("auto_title composes with auto_section", {
  doc <- rtf_document() |>
    rtf_tables(.pages(), auto_section = TRUE, auto_title = TRUE)
  # the names survive the auto_section rewrap, which drops list names
  expect_identical(.title_texts(doc, 1L), "Cohort A")
  expect_identical(.title_texts(doc, 2L), "Cohort B")
  n_sec <- sum(!vapply(doc$contents, function(b) is.null(b[["section"]]), NA))
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

test_that("a page that opens an auto section keeps the title carried on it (as_rtftables)", {
  pages <- .pages()
  attr(pages[[1L]], "rtf_titles") <- "Title of cohort A"
  attr(pages[[2L]], "rtf_footnotes") <- "Footnote of cohort B"
  doc <- rtf_document() |> rtf_tables(pages, auto_section = TRUE)
  expect_identical(unname(.title_texts(doc, 1L)), "Title of cohort A")
  x <- paste(.render(doc, NULL), collapse = " ")
  expect_match(x, "Title of cohort A", fixed = TRUE)
  expect_match(x, "Footnote of cohort B", fixed = TRUE)
})

test_that("rtf_titles() keeps the page's heading under the titles it sets (#748)", {
  doc <- rtf_document() |>
    rtf_tables(.pages(), titles = list("Table 14.1.1"), auto_title = TRUE) |>
    rtf_titles(list(c("Table 14.1.1", "Demographics")))
  expect_identical(.title_texts(doc, 1L), c("Table 14.1.1", "Demographics", "Cohort A"))
  expect_identical(.title_texts(doc, 2L), c("Table 14.1.1", "Demographics", "Cohort B"))
  # a second rtf_titles() keeps it once
  doc2 <- rtf_titles(doc, list("Only"))
  expect_identical(.title_texts(doc2, 1L), c("Only", "Cohort A"))
  # a page with no heading (auto_title = FALSE): the titles as given
  doc3 <- rtf_document() |>
    rtf_tables(.pages(), titles = list("T"), auto_title = FALSE) |>
    rtf_titles(list("New"))
  expect_identical(unname(.title_texts(doc3, 1L)), "New")
  expect_identical(unname(.title_texts(doc3, 2L)), "New")
})


# ── {PAGE_HEADING}: where the heading goes in the title block (#922) ───────

.named <- function() {
  list(`Heading Alpha` = data.frame(Item = c("row a1", "row a2"), Val = c("1", "2"),
                                    stringsAsFactors = FALSE),
       `Heading Beta`  = data.frame(Item = c("row b1", "row b2"), Val = c("3", "4"),
                                    stringsAsFactors = FALSE))
}
.mk <- function() list(text = "{PAGE_HEADING}", align = "right", italic = TRUE)
.shell <- function(marker = .mk())
  list("Table 14.3.4.2", "Shift Table", marker, "(Safety Analysis Set)")
# the first line each text is on, in the order given
.at <- function(lines, texts) {
  vapply(texts, function(t) {
    h <- grep(t, lines, fixed = TRUE)
    if (length(h)) h[[1L]] else NA_integer_
  }, 1L, USE.NAMES = FALSE)
}
.in_order <- function(lines, texts) {
  p <- .at(lines, texts)
  expect_false(anyNA(p))
  expect_identical(order(p), seq_along(p))
}

test_that("{PAGE_HEADING} via rtf_titles() puts the heading at the marker's place, with its align", {
  doc <- rtf_document() |>
    rtf_tables(.named(), auto_title = TRUE) |>
    rtf_titles(list(.shell()))
  for (i in 1:2) {
    nm <- c("Heading Alpha", "Heading Beta")[[i]]
    expect_identical(.title_texts(doc, i),
                     c("Table 14.3.4.2", "Shift Table", nm, "(Safety Analysis Set)"))
    row <- doc$contents[[i]]$title[[3L]]
    expect_identical(row$align, "right")
    expect_true(isTRUE(row$italic))
    expect_true(isTRUE(row$heading))
  }
  for (fmt in c("text", "table")) {
    x <- .render(doc, fmt)
    .in_order(x, c("Table 14.3.4.2", "Shift Table", "Heading Alpha",
                   "(Safety Analysis Set)", "row a1"))
    hit <- grep("Heading Alpha", x, value = TRUE)[1L]
    expect_true(grepl("\\\\qr", hit))
    expect_false(any(grepl("PAGE_HEADING", x, fixed = TRUE)))
  }
})

test_that("{PAGE_HEADING} given to rtf_tables(titles = ), and carried as the rtf_titles attribute", {
  doc <- rtf_document() |>
    rtf_tables(.named(), titles = list(.shell()), auto_title = TRUE)
  expect_identical(.title_texts(doc, 2L),
                   c("Table 14.3.4.2", "Shift Table", "Heading Beta", "(Safety Analysis Set)"))
  expect_identical(doc$contents[[2L]]$title[[3L]]$align, "right")
  # the attribute as_rtftables() puts on a page
  pages <- lapply(.named(), function(d) as_rtftables(d)[[1L]])
  for (i in 1:2) attr(pages[[i]], "rtf_titles") <- .shell()
  doc2 <- rtf_document() |> rtf_tables(pages, auto_title = TRUE)
  expect_identical(.title_texts(doc2, 1L),
                   c("Table 14.3.4.2", "Shift Table", "Heading Alpha", "(Safety Analysis Set)"))
  # a plain string row takes title_label_align
  doc3 <- rtf_document() |>
    rtf_tables(.named(), titles = list(c("T", "{PAGE_HEADING}", "U")),
               auto_title = TRUE, title_label_align = "center")
  expect_identical(.title_texts(doc3, 1L), c("T", "Heading Alpha", "U"))
  expect_identical(doc3$contents[[1L]]$title[[2L]]$align, "center")
  expect_true(isTRUE(doc3$contents[[1L]]$title[[2L]]$heading))
})

test_that("a title block without the marker keeps the heading last, as before", {
  doc <- rtf_document() |>
    rtf_tables(.named(), titles = list(c("T1", "T2")), auto_title = TRUE)
  expect_identical(.title_texts(doc, 1L), c("T1", "T2", "Heading Alpha"))
  expect_identical(doc$contents[[1L]]$title[[3L]],
                   list(text = "Heading Alpha", align = "left", heading = TRUE))
  # the same blocks, composed through rtf_titles()
  doc2 <- rtf_document() |>
    rtf_tables(.named(), auto_title = TRUE) |>
    rtf_titles(list(c("T1", "T2")))
  expect_identical(doc2$contents, doc$contents)
  expect_identical(.render(doc2, "text"), .render(doc, "text"))
})

test_that("a second rtf_titles() moves the heading to its marker, or puts it last", {
  doc <- rtf_document() |>
    rtf_tables(.named(), auto_title = TRUE) |>
    rtf_titles(list(.shell()))
  moved <- rtf_titles(doc, list(list(list(text = "{PAGE_HEADING}", bold = FALSE), "Other", "Last")))
  expect_identical(.title_texts(moved, 1L), c("Heading Alpha", "Other", "Last"))
  expect_false(isTRUE(moved$contents[[1L]]$title[[1L]]$bold))
  expect_identical(.title_texts(moved, 2L), c("Heading Beta", "Other", "Last"))
  last <- rtf_titles(doc, list(c("Other", "Last")))
  expect_identical(.title_texts(last, 1L), c("Other", "Last", "Heading Alpha"))
  # the heading's place is kept through a marker given again
  again <- rtf_titles(doc, list(.shell()))
  expect_identical(.title_texts(again, 2L), .title_texts(doc, 2L))
})

test_that("a page with no heading loses the {PAGE_HEADING} row, on every path", {
  chk <- function(doc, n_rows) {
    for (i in seq_along(doc$contents)) {
      expect_length(doc$contents[[i]]$title, n_rows)
      expect_false(any(grepl("PAGE_HEADING", .title_texts(doc, i), fixed = TRUE)))
    }
    for (fmt in c("text", "table")) {
      expect_false(any(grepl("PAGE_HEADING", .render(doc, fmt), fixed = TRUE)))
    }
  }
  # a table with no name at all
  plain <- rtf_document() |>
    rtf_tables(list(.named()[[1L]]), titles = list(.shell()))
  chk(plain, 3L)
  expect_identical(.title_texts(plain, 1L), c("Table 14.3.4.2", "Shift Table", "(Safety Analysis Set)"))
  # auto_title = FALSE
  off <- rtf_document() |> rtf_tables(.named(), titles = list(.shell()), auto_title = FALSE)
  chk(off, 3L)
  # ... and through rtf_titles() on a document with none
  off2 <- rtf_document() |>
    rtf_tables(.named(), auto_title = FALSE) |>
    rtf_titles(list(.shell()))
  chk(off2, 3L)
  # the page whose name is empty, beside one that has a heading
  half <- .named()
  names(half) <- c("Heading Alpha", "")
  mixed <- rtf_document() |> rtf_tables(half, titles = list(.shell()), auto_title = TRUE)
  expect_length(mixed$contents[[1L]]$title, 4L)
  expect_length(mixed$contents[[2L]]$title, 3L)
  expect_false(any(grepl("PAGE_HEADING", .render(mixed, "text"), fixed = TRUE)))
  # a marker alone: no title row is left
  only <- rtf_document() |> rtf_tables(list(.named()[[1L]]), titles = list("{PAGE_HEADING}"))
  expect_length(only$contents[[1L]]$title, 0L)
  expect_false(any(grepl("PAGE_HEADING", .render(only, "text"), fixed = TRUE)))
  # heading_at = "section": the heading is the section's, the title has no row for it
  sec <- rtf_document() |>
    rtf_tables(.named(), titles = list(.shell()), auto_title = FALSE, auto_section = TRUE)
  chk(sec, 3L)
})

test_that("{PAGE_HEADING} must be a row's whole text, once per block", {
  expect_error(rtf_document() |>
                 rtf_tables(.named(), titles = list("Parameter: {PAGE_HEADING}"), auto_title = TRUE),
               "cannot share a row with other text")
  expect_error(rtf_document() |>
                 rtf_tables(.named(), titles = list(list("T", "Line 1\n{PAGE_HEADING}")),
                            auto_title = TRUE),
               "multi-line string")
  expect_error(rtf_document() |>
                 rtf_tables(.named(), titles = list(list(list(text = "x {PAGE_HEADING}"))),
                            auto_title = FALSE),
               "cannot share a row")
  expect_error(rtf_document() |>
                 rtf_tables(.named(), titles = list(c("{PAGE_HEADING}", "T", "{PAGE_HEADING}")),
                            auto_title = TRUE),
               "2 rows of one title block")
  doc <- rtf_document() |> rtf_tables(.named(), auto_title = TRUE)
  expect_error(rtf_titles(doc, list(c("A: {PAGE_HEADING}"))), "cannot share a row")
  expect_error(rtf_titles(doc, list(c("{PAGE_HEADING}", "{PAGE_HEADING}"))), "2 rows")
  # the figure path is checked, too
  expect_error(rtf_titles(rtf_document() |> rtf_tables(.named()),
                          list("a\n{PAGE_HEADING}")), "multi-line")
})

test_that("cont_title comes after the block; a paginated table's later pages keep the heading", {
  big <- list(`Heading Alpha` = data.frame(Item = sprintf("row %02d", 1:60),
                                            Val = as.character(1:60),
                                            stringsAsFactors = FALSE))
  pages <- as_rtftables(big[[1L]], split = "rows", split_rows = 25)
  names(pages) <- rep("Heading Alpha", length(pages))
  doc <- rtf_document() |>
    rtf_tables(pages, titles = list(.shell()), auto_title = TRUE, cont_title = "(continued)")
  expect_gt(length(doc$contents), 1L)
  pg <- .pipe_doc_to_rtfreport(doc)$pages
  for (p in pg) {
    expect_identical(vapply(p$title, function(r) if (is.list(r)) r$text else r, ""),
                     if (identical(p, pg[[1L]]))
                       c("Table 14.3.4.2", "Shift Table", "Heading Alpha", "(Safety Analysis Set)")
                     else
                       c("Table 14.3.4.2", "Shift Table", "Heading Alpha", "(Safety Analysis Set)",
                         "(continued)"))
  }
  x <- .render(doc, "text")
  .in_order(x, c("Table 14.3.4.2", "Heading Alpha", "(Safety Analysis Set)", "(continued)"))
  expect_false(any(grepl("PAGE_HEADING", x, fixed = TRUE)))
})

test_that("plan_paginate_rows(heading =, heading_at = ) goes the same way", {
  d <- data.frame(grp = c("A", "A", "B", "B"), label = c("n", "Mean", "n", "Mean"),
                  X = c("1", "2.5", "3", "4.5"), stringsAsFactors = FALSE)
  pg <- suppressMessages(
    table_plan(d) |>
      plan_paginate_rows(page_by = "grp", heading = "Group {value}", heading_at = "title") |>
      plan_apply("pages"))
  doc <- rtf_document() |> rtf_tables(pg, titles = list(.shell()))
  expect_identical(.title_texts(doc, 1L),
                   c("Table 14.3.4.2", "Shift Table", "Group A", "(Safety Analysis Set)"))
  expect_identical(.title_texts(doc, 2L)[[3L]], "Group B")
  expect_identical(doc$contents[[1L]]$title[[3L]]$align, "right")
  # the same block, with the heading in the section: no row left
  pg2 <- suppressMessages(
    table_plan(d) |>
      plan_paginate_rows(page_by = "grp", heading = "Group {value}", heading_at = "section") |>
      plan_apply("pages"))
  doc2 <- rtf_document() |> rtf_tables(pg2, titles = list(.shell()))
  expect_length(doc2$contents[[1L]]$title, 3L)
  expect_false(any(grepl("PAGE_HEADING", .render(doc2, "text"), fixed = TRUE)))
  # and rtf_titles() after the plan's pages
  doc3 <- rtf_document() |> rtf_tables(pg) |> rtf_titles(list(.shell()))
  expect_identical(.title_texts(doc3, 2L)[[3L]], "Group B")
})

test_that("a heading with C_{max} prints its subscript at the marker", {
  nm <- list(`Parameter: C_{max} (ng/mL)` = .named()[[1L]])
  doc <- rtf_document() |> rtf_tables(nm, titles = list(.shell()), auto_title = TRUE)
  for (fmt in c("text", "table")) {
    x <- .render(doc, fmt)
    expect_true(any(grepl("{\\sub max}", x, fixed = TRUE)), label = fmt)
    expect_false(any(grepl("PAGE_HEADING", x, fixed = TRUE)))
  }
})

test_that("the marker is listed in rtf_text_tokens(), and a header or column header does not fill it", {
  tk <- rtf_text_tokens()
  expect_true("{PAGE_HEADING}" %in% tk$token)
  expect_identical(tk$kind[tk$token == "{PAGE_HEADING}"], "title")
  expect_false("PAGE_HEADING" %in% rtfreporter:::.RENDER_TOKENS)
})

test_that("figure titles: a marker left in a figure page's title is dropped by rtf_titles()", {
  skip_if_not_installed("grid")
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  grDevices::png(png, width = 300, height = 300)
  graphics::plot(1)
  grDevices::dev.off()
  doc <- rtf_document() |> rtf_figures(png) |> rtf_titles(list(.shell()))
  expect_length(doc$contents[[1L]]$title, 3L)
  expect_false(any(grepl("PAGE_HEADING", .render(doc, "text"), fixed = TRUE)))
})
