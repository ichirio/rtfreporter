# Non-ASCII text as RTF \uN? escapes (#829).  RTF reads N as a signed 16-bit
# number: a UTF-16 unit above 32767 is written as unit - 65536, and a code
# point above U+FFFF as its surrogate pair (two escapes).

.ue_hyo   <- "\u8868"        # U+8868, 34920 -> -30616
.ue_eacu  <- "\u00e9"        # U+00E9, 233 stays 233
.ue_smile <- "\U0001F600"    # U+1F600 -> D83D DE00 -> -10179, -8704

test_that("FT-DC255 [FR-DC338]: a character is escaped as a signed \\uN?, a code point above U+FFFF as a surrogate pair", {
  esc <- rtfreporter:::.rtf_unicode_escape
  expect_identical(esc(0x8868L),  "\\u-30616?")
  expect_identical(esc(0xE9L),    "\\u233?")
  expect_identical(esc(0x1F600L), "\\u-10179?\\u-8704?")
  expect_identical(esc(32767L),   "\\u32767?")
  expect_identical(esc(32768L),   "\\u-32768?")
  expect_identical(esc(65535L),   "\\u-1?")

  raw <- rtfreporter:::.rtf_escape_unicode_raw
  expect_identical(raw(.ue_hyo),   "\\u-30616?")
  expect_identical(raw(.ue_eacu),  "\\u233?")
  expect_identical(raw(.ue_smile), "\\u-10179?\\u-8704?")
  expect_identical(raw(paste0("caf", .ue_eacu, " ", .ue_hyo)),
                   "caf\\u233? \\u-30616?")
})

test_that("FT-DC256 [FR-DC338]: generate_rtfreport() writes the signed form in titles and cells", {
  d <- tempfile("ue-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- file.path(d, "out.rtf")
  df <- data.frame(a = c(.ue_hyo, .ue_smile), b = c(.ue_eacu, "x"),
                   stringsAsFactors = FALSE)
  doc <- rtf_document() |>
    rtf_tables(list(df), titles = list(paste("Title", .ue_hyo)))
  generate_rtfreport(doc, f, overwrite = TRUE)
  rtf <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_true(grepl("Title \\u-30616?", rtf, fixed = TRUE))
  expect_true(grepl("\\u-10179?\\u-8704?", rtf, fixed = TRUE))
  expect_true(grepl("\\u233?", rtf, fixed = TRUE))
  expect_false(grepl("\\\\u(3276[89]|327[7-9][0-9]|32[89][0-9]{2}|3[3-9][0-9]{3}|[4-9][0-9]{4})\\?",
                     rtf))
})

test_that("FT-AS128 [FR-AS137]: a table-of-contents label is escaped in the signed form", {
  te <- rtfreporter:::.toc_escape
  expect_identical(te(.ue_hyo),   "\\u-30616?")
  expect_identical(te(.ue_eacu),  "\\u233?")
  expect_identical(te(.ue_smile), "\\u-10179?\\u-8704?")
  expect_identical(te("a{b}\\"),  "a\\{b\\}\\\\")
})

test_that("FT-AS129 [FR-AS112 FR-AS150]: a title written by generate_rtfreport() reads back as the original characters, and assemble_rtf(toc = \"auto\") writes it signed", {
  d <- tempfile("ue-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  ttl <- paste("Table 1", .ue_hyo, paste0("caf", .ue_eacu), .ue_smile)
  f <- file.path(d, "t1.rtf")
  generate_rtfreport(
    rtf_document() |>
      rtf_tables(list(data.frame(A = 1, stringsAsFactors = FALSE)),
                 titles = list(ttl)),
    f, overwrite = TRUE)

  # Readers: the toc = "auto" title and the assemble_folder() text decoder
  got <- rtfreporter:::.rtf_title_text(readLines(f, warn = FALSE))
  expect_identical(got, ttl)
  dec <- rtfreporter:::.rtf_decode_text
  expect_identical(dec(rtfreporter:::.rtf_escape_unicode_raw(ttl)), ttl)
  # the unsigned form of older files still reads back
  expect_identical(dec("\\u34920?\\u55357?\\u56832?"),
                   paste0(.ue_hyo, .ue_smile))
  # a lone surrogate is the replacement character
  expect_identical(dec("a\\u-10179?b"), "a\ufffdb")

  out <- file.path(d, "book.rtf")
  assemble_rtf(c(f, f), out, toc = "auto", overwrite = TRUE)
  rtf <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("Table 1 \\u-30616? caf\\u233? \\u-10179?\\u-8704?",
                    rtf, fixed = TRUE))
})
