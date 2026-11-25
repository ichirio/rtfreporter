# The two examples in inst/rtf-examples/ a person checked in Word, rebuilt
# from the code in helper-word-checked-examples.R (the code their data-raw
# generators run) and compared with the shipped files (#1071).

test_that("FT-IX6807 [FR-IX131]: watermark-example.rtf (checked in Word) is rebuilt byte for byte from .wck_watermark_doc()", {
  skip_on_cran()
  shipped <- .wck_example("watermark-example.rtf")
  skip_if_not(file.exists(shipped), "needs inst/rtf-examples/watermark-example.rtf; runs wherever the package is installed with its examples, as on every CI job")
  d <- tempfile("wck-ex-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- .wck_write(.wck_watermark_doc(), d, "watermark-example.rtf")
  expect_identical(.wck_bytes(f), .wck_bytes(shipped))
})

test_that("FT-IX6808 [FR-IX133]: figure-example.rtf (checked in Word) is rebuilt from .wck_figure_doc() byte for byte with the picture data blanked out (the PNG bytes depend on the platform's device)", {
  skip_on_cran()
  shipped <- .wck_example("figure-example.rtf")
  skip_if_not(file.exists(shipped), "needs inst/rtf-examples/figure-example.rtf; runs wherever the package is installed with its examples, as on every CI job")
  skip_if_not(isTRUE(capabilities("png")) || isTRUE(capabilities("cairo")),
              "needs a PNG graphics device (capabilities png or cairo) to draw the figure example; runs wherever R has PNG support, as on every CI job")
  d <- tempfile("wck-ex-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  png <- file.path(d, "figure-example.png")
  drawn <- tryCatch({
    .wck_figure_png(png)
    file.exists(png)
  }, error = function(e) FALSE)
  skip_if_not(drawn, "needs a PNG device that draws the figure example file; runs wherever R has PNG support, as on every CI job")
  f <- .wck_write(.wck_figure_doc(png), d, "figure-example.rtf")

  new <- .wck_blank_pict(f)
  old <- .wck_blank_pict(shipped)
  # one picture in each, its data blanked, its size control words kept
  pict <- "\\pict\\pngblip\\picw2400\\pich1500\\picwgoal11520\\pichgoal7200\n<pict data>}"
  for (x in list(new, old)) {
    expect_identical(lengths(regmatches(x, gregexpr("<pict data>", x, fixed = TRUE))), 1L)
    expect_match(x, pict, fixed = TRUE)
  }
  expect_identical(new, old)
})
