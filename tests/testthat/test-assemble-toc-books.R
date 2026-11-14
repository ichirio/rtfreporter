# The two table-of-contents books a person checked in Word (#1056, #1068),
# rebuilt from the code by .tb_build_books() (helper-toc-books.R, which
# data-raw/gen_toc_word_check.R also runs) and compared with the committed
# fixtures, so a change that alters a checked book fails here.  The figures
# are PNGs whose bytes depend on the device (ragg or grDevices::png, and
# ggplot2 or base graphics) and its version, so the bytes are compared with
# each picture's data replaced by a token, and the pictures are checked
# separately: their count, their RTF size, and the pixel size in their PNG.

test_that("FT-AS7306 [FR-AS265 FR-AS125 FR-AS130 FR-AS269]: the two Word-check books (default and hanging TOC) are rebuilt from the code byte for byte, but for the pictures' data; each has the same 3 PNG pictures of 960 x 504 pixels at \\picwgoal11520\\pichgoal6048", {
  skip_on_cran()
  d <- tempfile("tb")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out <- .tb_build_books(d)
  expect_named(out, c("default", "hanging"))
  for (b in names(out)) {
    fx <- test_path("fixtures", paste0("toc-word-check-", b, ".rtf"))
    expect_identical(.tb_bytes(out[[b]]), .tb_bytes(fx))
    p <- .tb_pictures(out[[b]])
    pf <- .tb_pictures(fx)
    expect_identical(nrow(p), 3L)
    expect_identical(p$header, rep("\\pict\\pngblip\\picw960\\pich504\\picwgoal11520\\pichgoal6048", 3L))
    expect_identical(p$header, pf$header)
    expect_true(all(p$png))
    expect_identical(p$width, rep(960L, 3L))
    expect_identical(p$height, rep(504L, 3L))
    expect_identical(p[c("png", "width", "height")], pf[c("png", "width", "height")])
  }
})
