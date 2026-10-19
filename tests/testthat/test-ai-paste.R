# rtfreporter_ai_manual(): PASTE.md -- the kit as one file, the shell in it,
# a short size, the ARD's numbers only when asked (#687)

heads_of <- function(f) sub("^##### ", "", grep("^##### ", readLines(f, encoding = "UTF-8"), value = TRUE))

test_that("PASTE.md: every file of the kit in its order; the numbers only with values = TRUE", {
  skip_if_not_installed("cards")
  ard <- cards::ard_stack(cards::ADSL, .by = ARM, cards::ard_tabulate(variables = c(SEX, AGEGR1)))
  d <- withr::local_tempdir()
  out <- rtfreporter_ai_manual(d, ard = ard, overwrite = TRUE)
  expect_true("PASTE.md" %in% basename(out))
  p <- file.path(d, "PASTE.md")
  expect_identical(heads_of(p), c("00_START_HERE.md", "01_rtfreporter-ai-user-manual.md",
                                  "02_example_orr_subgroups.R", "03_example_ae_soc_pt.R",
                                  "04_example_demographics.R", "06_ard_shape.md"))
  expect_true(file.exists(file.path(d, "05_ard.csv")))          # the folder has it
  rtfreporter_ai_manual(d, ard = ard, values = TRUE, overwrite = TRUE)
  expect_true("05_ard.csv" %in% heads_of(p))
  # the dev manual: no kit, no PASTE.md
  d2 <- withr::local_tempdir()
  expect_identical(basename(rtfreporter_ai_manual(d2, which = "dev")), "rtfreporter-ai-dev-manual.md")
})

test_that("size = \"short\": the rules, the plan sections, the exports and the closest example", {
  skip_if_not_installed("cards")
  dm <- cards::ard_stack(cards::ADSL, .by = ARM, cards::ard_tabulate(variables = c(SEX, AGEGR1)),
                         cards::ard_summary(variables = AGE))
  d <- withr::local_tempdir()
  rtfreporter_ai_manual(d, ard = dm, overwrite = TRUE)
  full <- readLines(file.path(d, "PASTE.md"), encoding = "UTF-8")
  rtfreporter_ai_manual(d, ard = dm, size = "short", overwrite = TRUE)
  p <- file.path(d, "PASTE.md")
  short <- readLines(p, encoding = "UTF-8")
  expect_lt(sum(nchar(short)), sum(nchar(full)) * 0.6)
  expect_identical(heads_of(p), c("00_START_HERE.md",
                                  "01_rtfreporter-ai-user-manual.md (sections 0, 17, 17a and 18)",
                                  "04_example_demographics.R", "06_ard_shape.md"))
  for (h in c("^## 0[.] ", "^## 17[.] ", "^## 17a[.] ", "^## 18[.] ")) {
    expect_true(any(grepl(h, short)), info = h)
  }
  for (h in c("^## 5[.] ", "^## 11[.] ", "^## 19[.] ")) expect_false(any(grepl(h, short)), info = h)
  # the version line of the manual's head stays
  expect_true(any(grepl("This manual documents rtfreporter", short, fixed = TRUE)))
  # the folder keeps every file
  expect_true(all(file.exists(file.path(d, c("02_example_orr_subgroups.R", "03_example_ae_soc_pt.R")))))
})

test_that("the closest example: AE for a hierarchical ARD, ORR for a confidence interval, else DM", {
  skip_if_not_installed("cards")
  ae <- cards::ard_stack_hierarchical(cards::ADAE, variables = c(AESOC, AEDECOD), by = TRTA,
                                      denominator = cards::ADSL, id = USUBJID)
  expect_identical(.ai_kit_example_for(normalize_ard(ae)), "03_example_ae_soc_pt.R")
  dm <- cards::ard_stack(cards::ADSL, .by = ARM, cards::ard_tabulate(variables = SEX))
  expect_identical(.ai_kit_example_for(normalize_ard(dm)), "04_example_demographics.R")
  expect_identical(.ai_kit_example_for(NULL), "04_example_demographics.R")
  skip_if_not_installed("cardx")
  skip_if_not_installed("broom")
  adsl <- cards::ADSL
  adsl$RESP <- adsl$AGE > 75
  orr <- cardx::ard_categorical_ci(adsl, by = ARM, variables = RESP, method = "clopper-pearson")
  expect_identical(.ai_kit_example_for(normalize_ard(orr)), "02_example_orr_subgroups.R")
})

test_that("shell = : its text or its file, as 07_shell.md and at the end of PASTE.md, in UTF-8", {
  d <- withr::local_tempdir()
  sh <- c("Table 14.1.1  Demographics", "                Placebo   Drug X", "性別  n (%)")
  rtfreporter_ai_manual(d, shell = sh, overwrite = TRUE)
  p <- file.path(d, "PASTE.md")
  expect_identical(utils::tail(heads_of(p), 1L), "07_shell.md")
  txt <- readLines(p, encoding = "UTF-8")
  expect_true("性別  n (%)" %in% txt)
  expect_identical(readLines(file.path(d, "07_shell.md"), encoding = "UTF-8"), sh)
  # a file of it
  f <- file.path(d, "shell.txt")
  writeLines(enc2utf8(sh), f, useBytes = TRUE)
  rtfreporter_ai_manual(d, shell = f, overwrite = TRUE)
  expect_identical(readLines(file.path(d, "07_shell.md"), encoding = "UTF-8"), sh)
  expect_error(rtfreporter_ai_manual(d, shell = 1, overwrite = TRUE), "shell's text")
})
