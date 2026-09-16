## tests/testthat/test-ai-user-manual.R
##
## `pkgdown/assets/rtfreporter-ai-user-manual.md` is the briefing users attach to an
## AI chat session so the assistant can write rtfreporter code.  Its whole value
## is that it is TRUE: an assistant cannot tell a stale manual from a fresh one,
## so every claim in it has to be checked here rather than reviewed by eye.
##
## Three things are checked:
##   1. its function list is exactly the package's export list,
##   2. it never puts a deprecated export in code a reader would copy,
##   3. every idiom it teaches actually runs.
##
## The manual lives under `pkgdown/`, which is .Rbuildignore'd, so these tests
## skip on an installed package and run from the source tree (devtools::test(),
## and the R-CMD-check job that checks the source directory).

library(testthat)

.manual_path <- function() {
  test_path("..", "..", "pkgdown", "assets", "rtfreporter-ai-user-manual.md")
}

.manual_lines <- function() {
  p <- .manual_path()
  skip_if_not(file.exists(p), "AI manual not present (installed package)")
  readLines(p, encoding = "UTF-8", warn = FALSE)
}

## Code fences are `r ... ` blocks; the API list is the run of bold-led
## paragraphs under the "Complete public API" heading (section 17).
.manual_code <- function(lines) {
  fence <- grepl("^```", lines)
  inside <- cumsum(fence) %% 2L == 1L & !fence
  lines[inside]
}

.manual_api_names <- function(lines) {
  from <- grep("^## 17\\. Complete public API", lines)
  to   <- grep("^## 18\\.", lines)
  expect_length(from, 1L)
  expect_length(to, 1L)
  block <- lines[seq(from + 1L, to - 1L)]
  ## Drop the parenthetical note listing the deprecated names: they are
  ## documented as deprecated, not advertised as the API to use.
  unique(unlist(regmatches(block, gregexpr("`[A-Za-z_][A-Za-z0-9_.]*`", block))))
}

test_that("the manual's API list is exactly the package's exports", {
  lines <- .manual_lines()
  listed <- gsub("`", "", .manual_api_names(lines), fixed = TRUE)
  exported <- getNamespaceExports(asNamespace("rtfreporter"))
  ## S3 methods are exported as `generic.class`; the manual lists generics.
  exported <- exported[!grepl("\\.", exported) |
                         exported %in% c("as_rtftable", "as_rtftables")]

  missing_from_manual <- setdiff(exported, listed)
  expect_identical(missing_from_manual, character(0),
    info = paste("exported but not in the manual's API list:",
                 paste(missing_from_manual, collapse = ", ")))

  invented <- setdiff(listed, exported)
  expect_identical(invented, character(0),
    info = paste("in the manual's API list but not exported:",
                 paste(invented, collapse = ", ")))
})

test_that("the manual never puts a deprecated export in copyable code", {
  lines <- .manual_lines()
  code  <- .manual_code(lines)
  dep   <- rtfreporter:::.deprecated_exports
  hits  <- dep[vapply(dep, function(f)
    any(grepl(paste0("\\b", f, "\\("), code)), logical(1))]
  expect_identical(hits, character(0),
    info = paste("deprecated function used in a manual code block:",
                 paste(hits, collapse = ", ")))
})

test_that("the manual documents every deprecated export as deprecated", {
  lines <- .manual_lines()
  dep <- rtfreporter:::.deprecated_exports
  from <- grep("^\\*\\*Deprecated", lines)
  expect_length(from, 1L)
  block <- lines[seq(from, min(from + 20L, length(lines)))]
  for (f in dep) {
    expect_true(any(grepl(f, block, fixed = TRUE)),
                info = paste(f, "is deprecated but not in the manual's table"))
  }
})

test_that("every idiom the manual teaches runs", {
  skip_if_not(file.exists(.manual_path()),
              "AI manual not present (installed package)")
  f <- tempfile(fileext = ".rtf"); on.exit(unlink(f), add = TRUE)

  df <- data.frame(USUBJID = c("001-001", "001-002", "001-003"),
                   TRT     = c("Placebo", "Active", "Active"),
                   AVAL    = c(12.3, 14.1, 11.7))

  ## S2 -- the starter
  doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
    rtf_section(page = 1, secinfo = list(
      header = rtf_header(rows = list(
        c(l = "Protocol XYZ-001", r = "Confidential"),
        c(l = "Table 14.1.1",     r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"))),
      footer = rtf_footer(rows = list(c(c = "ACME Pharma, Inc."))))) |>
    rtf_tables(as_rtftables(df, border = "tfl", row_height_twips = 280L),
               titles    = list(c("Subject Summary", "Safety Population")),
               footnotes = list(c("Source: ADaM ADSL")))
  expect_no_error(generate_rtfreport(doc, f, overwrite = TRUE))

  ## S3 -- the program skeleton: body -> pages -> render, as six stages
  ARMS <- c("Placebo", "HOGE-001", "Total"); DAYS <- paste("Day", 1:3)
  study_header <- function(title_lines) {
    rtf_header(rows = c(
      list(c(l = "Protocol XYZ-001", r = "Confidential"),
           c(l = "Phase III", r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}")),
      lapply(title_lines, function(t) c(c = t))))
  }
  study_footer <- function(notes = character()) {
    rtf_footer(rows = c(lapply(notes, function(t) c(l = t)),
                        list(c(l = "ACME Pharma", r = "CONFIDENTIAL"))))
  }
  body_ae <- function(arms, days) {
    stub <- data.frame(period = rep(c("P1", "P2"), each = 4),
                       SOC = rep(c("Pain", "Rash"), each = 2, times = 2),
                       PT  = rep(c("Mild", "Severe"), times = 4),
                       stringsAsFactors = FALSE)
    g <- expand.grid(day = days, arm = arms, stringsAsFactors = FALSE)
    num <- as.data.frame(matrix("3 (5.0)", nrow = nrow(stub), ncol = nrow(g)),
                         stringsAsFactors = FALSE)
    names(num) <- paste(g$arm, g$day, sep = "____")
    cbind(stub, num, stringsAsFactors = FALSE)
  }
  denominators <- function(arms) {
    d <- data.frame(group = c("P1", "P2"), stringsAsFactors = FALSE)
    for (a in arms) d[[make.names(a)]] <- c(120L, 118L)
    d
  }
  pages_ae <- function(arms, days) {
    hdr <- rtf_col_header(
      c(list(col_cell(1L, "")),
        lapply(arms, function(a)
          col_cell(col_key(a),
                   sprintf("%s\n(N={%s})\nn(%%)", a, make.names(a))))),
      c("SOC\n  PT", rep(days, length(arms))))
    as_rtftables(
      body_ae(arms, days), read_meta = FALSE, split = "by_value",
      group_col = "period", drop_cols = "period",
      stub = stub_spec(c("SOC", "PT"), label = "row_label", indent = 2L),
      blank_rows = "between_groups",
      blank_row_first = TRUE, blank_row_end = TRUE,
      cell_format = fmt_value_paren,
      col_rel_width = c(5.5, rep(2, length(days) * length(arms)))) |>
      set_col_header(hdr, values = denominators(arms)) |>
      paginate_cols(by = "____", carry = 1,
                    page_order = c("cols", "group", "rows"))
  }
  render <- function(pages, titles, notes, file) {
    doc <- rtf_document(page = rtf_page(orientation = "landscape")) |>
      rtf_section(secinfo = list(header = study_header(titles),
                                 footer = study_footer(notes))) |>
      rtf_tables(pages, auto_section = TRUE)
    generate_rtfreport(doc, file, overwrite = TRUE)
  }
  sk <- pages_ae(ARMS, DAYS)
  expect_gt(length(sk), 1L)
  # the per-page denominator token was filled, not left as "{...}"
  expect_true(any(grepl("(N=120)", header_map(sk[[1]])$text, fixed = TRUE)))
  expect_false(any(grepl("{", header_map(sk[[1]])$text, fixed = TRUE)))
  skf <- tempfile(fileext = ".rtf"); on.exit(unlink(skf), add = TRUE)
  expect_no_error(render(sk, c("Table 14.3.1", "AE"), "Note.", skf))
  expect_true(file.exists(skf))
  # the simple tier: a named row, inline, no separate header object
  expect_no_error(
    as_rtftables(data.frame(Statistic = "n", A = "60", B = "58",
                            stringsAsFactors = FALSE), border = "tfl") |>
      set_col_header(c(Statistic = "Statistic", A = "Drug A", B = "Drug B")))

  ## S5a -- DM
  dm <- data.frame(
    Characteristic = c(rep("Age (years)", 3), rep("Sex", 2)),
    Statistic = c("n", "Mean (SD)", "Median", "Male, n (%)", "Female, n (%)"),
    `Drug A` = c("60", "54.2 (11.3)", "55.0", "31 (51.7%)", "29 (48.3%)"),
    `Drug B` = c("58", "56.8 (10.1)", "57.5", "27 (46.6%)", "31 (53.4%)"),
    check.names = FALSE, stringsAsFactors = FALSE)
  expect_no_error(as_rtftables(dm, group_col = "Characteristic",
                               split = "group_safe", max_rows = 20,
                               border = "tfl"))

  ## S5b -- AE, both the shorthand and the stub_spec() form
  ae <- data.frame(
    SOC = c(rep("Cardiac disorders", 2), rep("Gastrointestinal disorders", 3)),
    PT  = c("Atrial fibrillation", "Bradycardia", "Nausea", "Vomiting", "Diarrhoea"),
    `Drug A` = c("3 (5.0%)", "1 (1.7%)", "8 (13.3%)", "4 (6.7%)", "2 (3.3%)"),
    `Drug B` = c("2 (3.4%)", "0", "6 (10.3%)", "3 (5.2%)", "5 (8.6%)"),
    check.names = FALSE, stringsAsFactors = FALSE)
  expect_no_error(as_rtftables(ae, stub_vars = c("SOC", "PT"),
                               group_by = "indent", blank_rows = "between_groups",
                               split = "group_safe", max_rows = 20, border = "tfl"))
  expect_no_error(as_rtftables(ae, stub = stub_spec(c("SOC", "PT"),
                                                    label = "SOC / Preferred Term"),
                               group_by = "indent", blank_rows = "between_groups",
                               split = "group_safe", max_rows = 20, border = "tfl"))

  ## S5c -- PK: decimal split then a column split
  pk <- data.frame(
    Time = c(rep("1 h", 3), rep("2 h", 3)),
    Statistic = rep(c("n", "Mean", "SD"), 2),
    `Day 1`  = c("24", "1104.5", "233.41"),
    `Day 7`  = c("24", "88.012", "19.223"),
    `Day 14` = c("24", "9.0125", "2.1044"),
    `Day 28` = c("24", "1234.5", "301.22"),
    check.names = FALSE, stringsAsFactors = FALSE)
  pk_pages <- as_rtftables(pk, stub_vars = c("Time", "Statistic"),
                           group_by = "indent", blank_rows = "between_groups",
                           column_widths_twips = c(2000L, rep(1800L, 4)),
                           border = "tfl") |>
    set_decimal_split(cols = 2:5) |>
    paginate_cols(at = 4L, carry = 1L)
  expect_gt(length(pk_pages), 1L)

  ## S5d -- LB: a grouping column that is never printed
  lb <- data.frame(PARAMCD = c(rep("ALT", 3), rep("AST", 3)),
                   Baseline = rep(c("Normal", "Grade 1", "Grade 2"), 2),
                   Normal = c("40", "5", "1", "38", "6", "2"),
                   `Grade 1` = c("8", "12", "3", "9", "11", "4"),
                   check.names = FALSE, stringsAsFactors = FALSE)
  lb_pages <- as_rtftables(lb, group_col = "PARAMCD", drop_cols = "PARAMCD",
                           blank_rows = "between_groups", border = "tfl")
  expect_false("PARAMCD" %in% rtf_columns(lb_pages))

  ## S7 -- spanning header, then editing a finished header
  df_shift <- data.frame(Baseline = c("Low", "Normal", "High"),
                         A_Low = c(4L, 0L, 0L), A_Norm = c(1L, 13L, 1L),
                         B_Low = c(3L, 0L, 0L), B_Norm = c(1L, 14L, 0L))
  expect_no_error(rtftable(
    data = df_shift,
    col_header = c("Baseline", "Low", "Normal", "Low", "Normal"),
    spanning_header = list(
      list(from = 2L, to = 3L, label = "Treatment A  (N=24)", underline = TRUE),
      list(from = 4L, to = 5L, label = "Treatment B  (N=24)", underline = TRUE)),
    column_widths_twips = c(2160L, rep(900L, 4)),
    col_spec = lapply(seq_len(5L), function(j)
      list(col = j, align = if (j == 1L) "left" else "center")),
    border = "tfl", row_height_twips = 280L))

  dfh <- data.frame(row_label = c("A", "B"), g1 = 1:2, g2 = 3:4, Total = 5:6)
  hpages <- as_rtftables(dfh, border = "tfl")
  expect_identical(rtf_columns(hpages), c("row_label", "g1", "g2", "Total"))
  ## a bare data.frame page has no header object -- the manual's gotcha
  expect_error(style_header(hpages, row = 1, cols = 2, bold = TRUE),
               "no column header")
  hpages <- hpages |> set_col_header(
    list(col_cell("row_label", ""), col_cell(c("g1", "g2"), "Treatment")),
    c(row_label = "Category", g1 = "Low", g2 = "High", Total = "Total"))
  expect_no_error(hpages |> add_header_row(c("", "A", "B", "")))
  expect_no_error(hpages |> style_header(row = 2, cols = 2:3, bold = TRUE))
  ## a NAMED row is a patch and may be shorter than the table (#453);
  ## an UNNAMED short row is still an error, not a partial fill
  expect_no_error(set_col_header(hpages, c(g1 = "Low")))
  expect_no_error(set_col_header(hpages, c(g1 = "Low", g2 = "High")))
  expect_error(set_col_header(hpages, c("Low", "High")),
               "the label row has 2 labels")
  expect_no_error(col_cell(pos = col_key("g1"), label = "Drug A"))
  expect_no_error(col_header_from_names(dfh))
  expect_s3_class(header_map(hpages[[1]]), "data.frame")

  ## S8 -- widths
  w <- auto_col_widths(df, col_header = c("Subject", "Arm", "Value"),
                       table_width_twips = 14400L)
  expect_length(w, 3L)
  expect_no_error(rtftable(df, col_rel_width = c(2, 1, 1),
                           table_width_pct = 70, table_align = "center"))

  ## S9 -- titles / footnotes, on the call and afterwards
  expect_no_error(rtf_document() |>
    rtf_tables(list(df, df),
               titles = list(c("Table 14.1.1", "", "Demographics"),
                             c("Table 14.1.2", "", "By region")),
               footnotes = list(c("Source: ADSL.", "", "N = 160."), NULL)))
  expect_no_error(rtf_document() |> rtf_tables(list(df, df)) |>
    rtf_titles(list("Page One", "Page Two")) |>
    rtf_footnotes(list(NULL, "Source: ADSL.")))

  ## S10 -- listings
  adsl <- data.frame(
    USUBJID = c("63016-204-1015", "63016-204-1023", "63016-205-100028"),
    DISPTPD = c("COMPLETED", "COMPLETED", "DISCONTINUED"),
    BRCA    = c("BRCA1", NA, "BRCA2"),
    HIST    = c("ADENOCARCINOMA", "SQUAMOUS CELL CARCINOMA OF THE LUNG", "SMALL CELL"),
    STAGE   = c("IIIB", "IV", "IIIA"), stringsAsFactors = FALSE)
  spec <- listing_spec(list(
    listing_col("USUBJID", width = 15, label = "Unique\nSubject ID",
                collapse_repeats = TRUE),
    listing_col(c("DISPTPD", "BRCA", "HIST"), width = 22,
                label = "Disposition/\nAny (BRCA) Mutations/\nHistology"),
    listing_col("STAGE", label = "Stage at\nInitial\nDiagnosis")))
  expect_no_error(as_rtftables(adsl, listing = spec, max_rows = 8))
  expect_s3_class(build_listing(adsl, spec), "data.frame")
  fitted <- fit_listing_widths(adsl,
    listing_spec(list(listing_col("USUBJID"), listing_col("STAGE"))),
    page = rtf_page(paper_size = "A4", orientation = "landscape"),
    size_half_points = 16L)
  expect_no_error(listing_code(fitted, name = "listing"))

  ## S11 -- figures
  png_path <- tempfile(fileext = ".png"); on.exit(unlink(png_path), add = TRUE)
  grDevices::png(png_path, width = 700, height = 450); plot(1:10); grDevices::dev.off()
  expect_no_error(rtf_document() |>
    rtf_tables(list(rtfplot(png_path, width_twips = 9000L))))
  expect_no_error(rtf_document() |>
    rtf_figures(list(rtfplot(png_path)), titles = list("Figure 14.1.1")))

  ## S12 -- borders and styling
  expect_no_error(rtftable(df, border = rtf_border(
    top = TRUE, bottom = TRUE, left = TRUE, right = TRUE, inside_h = TRUE)))
  expect_no_error(rtftable(df, border = rtf_border()) |>
    style_zone(header   = rtf_border(top = TRUE, bottom = TRUE),
               last_row = rtf_border(bottom = rtf_border_side("double", 10L))))
  expect_no_error(rtftable(df, style = rtf_table_style_tfl()))
  pages <- as_rtftables(df, border = "tfl") |>
    style_cols(cols = 1, align = "left", indent_twips = 120)
  ## a page list rejects integer rows; a formula works, and one page takes both
  expect_error(style_body(pages, rows = 2:3, cols = 2, bold = TRUE), "ambiguous")
  expect_no_error(style_body(pages, rows = ~ TRT == "Active", cols = 2, bold = TRUE))
  expect_no_error(style_body(pages[[1]], rows = 2:3, cols = 2, bold = TRUE))

  ## S14 -- assembly
  f2 <- tempfile(fileext = ".rtf"); on.exit(unlink(f2), add = TRUE)
  book <- tempfile(fileext = ".rtf"); on.exit(unlink(book), add = TRUE)
  generate_rtfreport(doc, f2, overwrite = TRUE)
  expect_no_error(assemble_rtf(c(f, f2), book, overwrite = TRUE, toc = "auto",
                               book_page = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"))

  ## S15 -- the formatting helpers, with the shapes the manual states
  ## the cell reads " 31 (51.7)" and the padding is a non-breaking space,
  ## as the manual states
  expect_identical(format_count_pct(31, 0.517, nbsp = " "), " 31 (51.7)")
  expect_true(grepl(intToUtf8(160), format_count_pct(31, 0.517), fixed = TRUE))
  expect_no_error(fmt_count_paren(c("31 (51.7)", "5 (8.3)")))
  expect_no_error(fmt_value_paren(c("54.2 (11.3)", "9.0 (1.2)")))
  expect_no_error(realign_count_pct(c("5 (33.3)", "12 (80.0)")))
  expect_no_error(fmt_right_align(c("1", "22", "333")))
  expect_identical(catx(" ", "a", "", "b"), "a b")
  expect_no_error(fmt_numeric(data.frame(x = c(1.234, 22.5)), cols = "x", digits = 1))
  expect_s3_class(blank_rows_by_change("Characteristic"), "rtf_blank_rows_by_change")
  expect_s3_class(blank_rows_by_rule("Characteristic", "^Total", where = "before"),
                  "rtf_blank_rows_by_rule")
})
