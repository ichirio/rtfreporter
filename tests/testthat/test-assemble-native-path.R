# A path the native encoding cannot hold (a Japanese name in a C locale):
# assemble_rtf() and assemble_folder() say so before any file is touched,
# instead of base R's "unable to translate ... to native encoding".

test_that("FT-AS7307 [FR-AS274]: a path the native encoding cannot hold stops with what to do, before any file is read or written", {
  d <- tempfile("anp")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  mk <- function(f) {
    doc <- rtf_document() |> rtf_tables(data.frame(Parameter = "Age", Value = "75.1"))
    suppressMessages(generate_rtfreport(doc, file.path(d, f)))
    file.path(d, f)
  }
  f <- c(mk("t1.rtf"), mk("t2.rtf"))
  kanji <- intToUtf8(c(35430L, 39443L))   # two kanji
  out_k <- file.path(d, paste0(kanji, ".rtf"))
  dir_k <- file.path(d, kanji)
  csv_k <- file.path(d, paste0(kanji, ".csv"))
  msg <- function(a) {
    paste0("^`", a, "` has a character the session's native encoding \\(LC_CTYPE \"C\"\\) ",
           "cannot hold, so R cannot open it: run R in a UTF-8 locale, or give ",
           "the file or folder an ASCII name\\.$")
  }
  with_c_ctype({
    expect_error(assemble_rtf(f, out_k), msg("output_file"))
    expect_error(assemble_rtf(c(f[1L], out_k), file.path(d, "all.rtf")), msg("input_files"))
    expect_error(assemble_folder(dir_k), msg("dir"))
    expect_error(assemble_folder(d, spec_file = csv_k), msg("spec_file"))
    # an ASCII path is not touched by the check
    expect_true(is.data.frame(assemble_folder(d)))
  })
  # nothing was written
  expect_identical(sort(list.files(d)), c("t1.rtf", "t2.rtf"))
})
