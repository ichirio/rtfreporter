## tests/testthat/test-plan-preset-look.R
##
## plan_style(border = <preset>, <look field> = ): the preset's rules
## survive.  The look fields make an rtf_table_style(), and rtftable() reads
## its default border = "tfl" as "take the style's rules"; the plan passes the
## preset as an object, so the rules are the preset's.  "none" stays none.

library(testthat)

.pl_df <- function() {
  data.frame(Parameter = c("Subjects", "Age, mean (SD)", "Female, n (%)"),
             Placebo = c("86", "75.2 (8.59)", "53 (61.6)"),
             Active = c("84", "75.7 (8.29)", "50 (59.5)"),
             stringsAsFactors = FALSE)
}

# An rtftable keeps a `col_header = NULL` entry or leaves it out depending on
# how it was built; the same table either way, so it is dropped before
# comparing.
.pl_strip <- function(pages) {
  lapply(pages, function(pg) {
    if ("col_header" %in% names(pg) && is.null(pg$col_header)) pg["col_header"] <- NULL
    pg
  })
}

.pl_pages <- function(p) .pl_strip(suppressMessages(plan_apply(p, "pages")))

# the direct call
.pl_direct <- function(...) {
  out <- suppressMessages(as_rtftables(.pl_df(), read_meta = FALSE, ...))
  .pl_strip(if (inherits(out, "rtftable")) list(out) else out)
}

.pl_rtf <- function(pages) {
  dir <- tempfile("pl-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  f <- file.path(dir, "t.rtf")
  generate_rtfreport(rtf_document() |> rtf_tables(pages), f)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

# the \trowd block of the header row
.pl_header_row <- function(rtf) unlist(strsplit(rtf, "\\\\trowd"))[2L]

test_that("plan_style(border = \"tfl\", <look field> = ) keeps the preset's rules", {
  # header_bold
  got <- .pl_pages(table_plan(.pl_df()) |> plan_style(border = "tfl", header_bold = TRUE))
  want <- .pl_direct(border = "tfl") |> style_cols(header_bold = TRUE)
  expect_equal(got, want)
  expect_identical(.pl_rtf(got), .pl_rtf(want))
  # with a font field
  got2 <- .pl_pages(table_plan(.pl_df()) |>
                      plan_style(border = "tfl", header_bold = TRUE, font = "Arial"))
  want2 <- .pl_direct(border = "tfl", font = "Arial") |> style_cols(header_bold = TRUE)
  expect_equal(got2, want2)
  expect_identical(.pl_rtf(got2), .pl_rtf(want2))
  # another look field: the body's alignment, which the header inherits
  got3 <- .pl_pages(table_plan(.pl_df()) |>
                      plan_style(border = "tfl", align = "right", italic = TRUE))
  want3 <- .pl_direct(border = "tfl") |>
    style_cols(align = "right", italic = TRUE, header_align = "right")
  expect_equal(got3, want3)
  expect_identical(.pl_rtf(got3), .pl_rtf(want3))
})

test_that("the RTF has the preset's rules over and under the header", {
  for (args in list(list(header_bold = TRUE),
                    list(header_bold = TRUE, font = "Arial"),
                    list(align = "right"))) {
    pg <- .pl_pages(do.call(plan_style, c(list(table_plan(.pl_df()), border = "tfl"), args)))
    hdr <- .pl_header_row(.pl_rtf(pg))
    label <- paste(names(args), collapse = "+")
    expect_identical(lengths(regmatches(hdr, gregexpr("\\\\clbrdrt\\\\brdrs", hdr))), 3L,
                     info = label)
    expect_identical(lengths(regmatches(hdr, gregexpr("\\\\clbrdrb\\\\brdrs", hdr))), 3L,
                     info = label)
  }
})

test_that("border = \"none\" with a look field stays without rules, and other plans are as before", {
  got <- .pl_pages(table_plan(.pl_df()) |> plan_style(border = "none", header_bold = TRUE))
  want <- .pl_direct(border = "none") |> style_cols(header_bold = TRUE)
  expect_equal(got, want)
  expect_identical(.pl_rtf(got), .pl_rtf(want))
  expect_no_match(.pl_rtf(got), "clbrdr", fixed = TRUE)
  got2 <- .pl_pages(table_plan(.pl_df()) |>
                      plan_style(border = "none", italic = TRUE, font = "Arial"))
  expect_no_match(.pl_rtf(got2), "clbrdr", fixed = TRUE)
  # the preset alone, and the preset with a font only: unchanged
  expect_equal(.pl_pages(table_plan(.pl_df()) |> plan_style(border = "tfl")),
               .pl_direct(border = "tfl"))
  expect_equal(.pl_pages(table_plan(.pl_df()) |> plan_style(border = "tfl", font = "Arial")),
               .pl_direct(border = "tfl", font = "Arial"))
})

test_that("no border said: the default preset keeps its rules with a look field", {
  # header_bold, a font field, another look field
  got <- .pl_pages(table_plan(.pl_df()) |> plan_style(header_bold = TRUE))
  want <- .pl_direct() |> style_cols(header_bold = TRUE)
  expect_equal(got, want)
  expect_identical(.pl_rtf(got), .pl_rtf(want))
  got2 <- .pl_pages(table_plan(.pl_df()) |> plan_style(header_bold = TRUE, font = "Arial"))
  want2 <- .pl_direct(font = "Arial") |> style_cols(header_bold = TRUE)
  expect_equal(got2, want2)
  expect_identical(.pl_rtf(got2), .pl_rtf(want2))
  got3 <- .pl_pages(table_plan(.pl_df()) |> plan_style(align = "right", italic = TRUE))
  want3 <- .pl_direct() |> style_cols(align = "right", italic = TRUE, header_align = "right")
  expect_equal(got3, want3)
  # the same rules as the plan with border = "tfl"
  expect_identical(.pl_rtf(got),
                   .pl_rtf(.pl_pages(table_plan(.pl_df()) |>
                                       plan_style(border = "tfl", header_bold = TRUE))))
  hdr <- .pl_header_row(.pl_rtf(got3))
  expect_identical(lengths(regmatches(hdr, gregexpr("\\\\clbrdrt\\\\brdrs", hdr))), 3L)
  # zones without a border are still the table's rules (no preset added)
  z <- rtf_border(bottom = TRUE)
  expect_equal(.pl_pages(table_plan(.pl_df()) |> plan_style(border_last_row = z, header_bold = TRUE)),
               .pl_strip(.pl_direct(style = rtf_table_style(border_last_row = z, header_bold = TRUE))))
})
