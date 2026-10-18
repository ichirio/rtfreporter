## tests/testthat/test-ai-kit-runs.R
##
## The kit `rtfreporter_ai_manual(dir, ard = )` writes is what a chat AI
## copies from, and the article `ai-plan-code(-ja)` is what the person
## asking it follows.  Both are only useful while every line of their code
## runs on the package as it is, so it is run here:
##   1. each kit example (02-04), as the AI would copy it,
##   2. each code block of the two articles, in order, with an AI answer,
##   3. the kit's files are UTF-8 whatever the locale (Japanese labels),
##   4. every argument of the plan verbs is named in the manual's section 17.

library(testthat)

.kit_src <- function() {
  kit <- system.file("ai", "kit", package = "rtfreporter")
  skip_if(!nzchar(kit), "the kit is not installed")
  kit
}

# An environment that sees the package (the test's own) and the exports of
# `pkgs`, so a script's `library()` lines can be dropped instead of
# attaching packages for the rest of the suite.
.kit_env <- function(pkgs, parent) {
  imp <- new.env(parent = parent)
  for (p in pkgs) {
    ns <- asNamespace(p)
    for (f in getNamespaceExports(ns)) {
      if (!exists(f, envir = imp, inherits = FALSE)) {
        assign(f, getExportedValue(p, f), envir = imp)
      }
    }
    lazy <- getNamespaceInfo(ns, "lazydata")       # cards::ADSL and the like
    for (f in ls(lazy)) assign(f, get(f, envir = lazy), envir = imp)
  }
  new.env(parent = imp)
}

# Run a script as the tester does, by source() -- so the program its
# {PROGRAM_FULL} names is known -- into `env`, its library() lines dropped.
# A source() inside it (the article's run block) sources into `env` too.
.run_script <- function(lines, env, name = "script.R") {
  lines <- lines[!grepl("^\\s*library\\(", lines)]
  env$source <- function(file, ...) base::source(file, local = env, ...)
  f <- file.path(tempfile("run"), name)
  dir.create(dirname(f))
  on.exit(unlink(dirname(f), recursive = TRUE), add = TRUE)
  con <- file(f, open = "wb")
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  close(con)
  base::source(f, local = env, encoding = "UTF-8")
}

test_that("every kit example runs and writes its RTF", {
  skip_on_cran()                                  # three ARDs from ADaM: slow
  for (p in c("cards", "cardx", "dplyr", "pharmaverseadam")) skip_if_not_installed(p)
  kit <- .kit_src()
  out <- tempfile("kit-out"); dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  for (f in c("02_example_orr_subgroups.R", "03_example_ae_soc_pt.R",
              "04_example_demographics.R")) {
    env <- .kit_env(c("cards", "dplyr"), environment())
    env$output_path <- out
    env$report_name <- sub("\\.R$", "", f)
    src <- readLines(file.path(kit, f), warn = FALSE, encoding = "UTF-8")
    expect_no_error(suppressWarnings(suppressMessages(.run_script(src, env, f))),
                    message = f)
    expect_true(file.exists(file.path(out, paste0(env$report_name, ".rtf"))), info = f)
    expect_s3_class(env$tbl_plan, "table_plan")
    tab <- suppressWarnings(suppressMessages(plan_apply(env$tbl_plan, "table")))
    expect_gt(nrow(tab), 3L)
  }
})

# The answer an AI gives for the article's ARD: what the tester saves as
# T-14-1-1.R.  Japanese titles, written as UTF-8 bytes.
.ai_answer <- function() {
  c("# QUESTION: none",
    "library(rtfreporter)",
    "",
    "# ---- 1. the table: the plan over the ARD -----------------------------------",
    "tbl_plan <- ard |>",
    "  normalize_ard() |>",
    "  table_plan(cols = \"ARM\", rows = c(group = \"variable\")) |>",
    "  plan_cells(",
    "    continuous  = c(\"n\" = \"{N:.0f}\", \"Mean (SD)\" = \"{mean:.1f} ({sd:.2f})\",",
    "                    \"Min, Max\" = \"{min:.0f}, {max:.0f}\"),",
    "    categorical = c(n == 0 ~ \"0\", \"{n:.0f} ({p:.1f%})\"), notes = FALSE) |>",
    paste0("  plan_labels(c(AGE = \"", "年齢", "\", AGEGR1 = \"Age group\", SEX = \"Sex\")) |>"),
    "  plan_stub(name = \"Characteristic\") |>",
    "  plan_col_header(values = list(n = TRUE),",
    "                  header = rtf_col_header(c(\"\", \"{col}\"), c(\"Characteristic\", \"(N={n})\")))",
    "",
    "# ---- 2. the document: section, titles, footnotes ---------------------------",
    "rep <- rtf_document() |>",
    "  rtf_section(",
    paste0("    header = rtf_header(list(c(l = \"Protocol: CDISCPILOT01\", r = \"Page {PAGE} of {TOTAL_PAGES}\"), c(c = \"",
           "表 14.1.1 人口統計", "\"))),"),
    "    footer = rtf_footer(list(c(l = \"{PROGRAM_FULL}    Generated on: {DATETIME}\")))) |>",
    "  rtf_tables(tbl_plan)",
    "",
    "# ---- 3. the file ------------------------------------------------------------",
    "generate_rtfreport(rep, file.path(output_path, paste0(report_name, \".rtf\")), overwrite = TRUE)")
}

.article_blocks <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  open <- grep("^```r\\s*$", lines)
  lapply(open, function(i) {
    end <- i + which(grepl("^```\\s*$", lines[-seq_len(i)]))[1L]
    lines[seq(i + 1L, end - 1L)]
  })
}

test_that("every code block of the ai-plan-code articles runs, in order", {
  skip_on_cran()
  skip_if_not_installed("cards")
  for (lang in c("ai-plan-code.Rmd", "ai-plan-code-ja.Rmd")) {
    rmd <- test_path("..", "..", "vignettes", "articles", lang)
    skip_if_not(file.exists(rmd), "articles not present (installed package)")
    blocks <- .article_blocks(normalizePath(rmd))
    expect_length(blocks, 3L)                    # the kit, the paste, the run
    wd <- tempfile("article"); dir.create(wd)
    old <- setwd(wd)
    on.exit(setwd(old), add = TRUE)
    env <- .kit_env("cards", environment())
    con <- file("T-14-1-1.R", open = "wb")
    writeLines(enc2utf8(.ai_answer()), con, useBytes = TRUE)
    close(con)
    for (i in seq_along(blocks)) {
      expect_no_error(suppressWarnings(suppressMessages(.run_script(blocks[[i]], env))),
                      message = paste(lang, "block", i))
    }
    expect_true(file.exists(file.path("kit", "T-14-1-1", "06_ard_shape.md")), info = lang)
    expect_true(file.exists(file.path("kit", "T-14-1-1", "PASTE.md")), info = lang)
    # PASTE.md: the kit but 05, each file once
    paste_md <- readLines(file.path("kit", "T-14-1-1", "PASTE.md"), encoding = "UTF-8")
    heads <- sub("^##### ", "", grep("^##### 0", paste_md, value = TRUE))
    expect_identical(heads, c("00_START_HERE.md", "01_rtfreporter-ai-user-manual.md",
                              "02_example_orr_subgroups.R", "03_example_ae_soc_pt.R",
                              "04_example_demographics.R", "06_ard_shape.md"))
    # the size the articles quote ("about 90,000 characters")
    expect_true(abs(sum(nchar(paste_md)) - 90000) < 20000, info = sum(nchar(paste_md)))
    rtf <- file.path("output", "T-14-1-1.rtf")
    expect_true(file.exists(rtf), info = lang)
    # the Japanese title reached the RTF (as \uN escapes)
    expect_true(grepl("\\u20154", paste(readLines(rtf, warn = FALSE), collapse = ""),
                      fixed = TRUE), info = lang)
    setwd(old)
    unlink(wd, recursive = TRUE)
  }
})

.jp_ard <- function() {
  data.frame(group1 = "TRT01A",
             group1_level = I(list("プラセボ", "実薬")),
             variable = "SEX", variable_level = I(list("女性", "女性")),
             context = "tabulate", stat_name = "n", stat_label = "n",
             stat = I(list(3, 4)), stringsAsFactors = FALSE)
}

.expect_kit_utf8 <- function(d) {
  for (f in list.files(d, full.names = TRUE)) {
    b <- readLines(f, warn = FALSE, encoding = "UTF-8")
    expect_true(all(validUTF8(b)), info = basename(f))
  }
  raw <- function(f) readBin(file.path(d, f), "raw", file.size(file.path(d, f)))
  has <- function(f, s) {
    grepl(s, rawToChar(raw(f)), fixed = TRUE, useBytes = TRUE)
  }
  placebo <- enc2utf8("プラセボ")
  Encoding(placebo) <- "bytes"
  for (f in c("05_ard.csv", "06_ard_shape.md")) expect_true(has(f, placebo), info = f)
  dash <- enc2utf8("—"); Encoding(dash) <- "bytes"
  expect_true(has("00_START_HERE.md", dash))   # the request's own text survives
  csv <- utils::read.csv(file.path(d, "05_ard.csv"), encoding = "UTF-8")
  expect_identical(enc2utf8(csv$TRT01A), enc2utf8(c("プラセボ", "実薬")))
  expect_identical(csv$stat, c(3L, 4L))
}

test_that("the kit's files are UTF-8, Japanese labels kept", {
  d <- tempfile("kit-jp")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rtfreporter_ai_manual(d, ard = .jp_ard())
  .expect_kit_utf8(d)
})

test_that("the kit's files are UTF-8 in a non-UTF-8 locale too", {
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if(!nzchar(ok) || isTRUE(l10n_info()[["UTF-8"]]), "cannot switch to a non-UTF-8 locale")
  d <- tempfile("kit-c")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rtfreporter_ai_manual(d, ard = .jp_ard())
  Sys.setlocale("LC_CTYPE", old)
  .expect_kit_utf8(d)
})

test_that("the kit tells what to paste, what never, and the answer's form", {
  d <- tempfile("kit-req")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  rtfreporter_ai_manual(d)
  start <- paste(readLines(file.path(d, "00_START_HERE.md"), warn = FALSE,
                           encoding = "UTF-8"), collapse = "\n")
  for (s in c("## The answer: one R block", "**exactly one** fenced code block",
              "**Paste or attach**", "`06_ard_shape.md` -- the ARD's summary",
              "**shell of this one table as text**", "**Never paste** subject-level data",
              "**Run the answer**", "source(\"T-14-1-1.R\", encoding = \"UTF-8\")")) {
    expect_true(grepl(s, start, fixed = TRUE), info = s)
  }
})

test_that("every argument of the plan verbs is named in the manual's section 17", {
  p <- test_path("..", "..", "inst", "ai", "rtfreporter-ai-user-manual.md")
  skip_if_not(file.exists(p), "AI manual not present (installed package)")
  lines <- readLines(p, warn = FALSE, encoding = "UTF-8")
  from <- grep("^## 17\\. Tables from a cards", lines)
  to   <- grep("^## 18\\.", lines)
  text <- paste(lines[seq(from, to)], collapse = "\n")
  ns <- asNamespace("rtfreporter")
  verbs <- c("table_plan", "normalize_ard",
             grep("^plan_", getNamespaceExports(ns), value = TRUE))
  # plan_style() lists its rtf_table_style() fields with an ellipsis
  verbs <- setdiff(verbs, "plan_style")
  missing <- character()
  for (v in verbs) {
    args <- setdiff(names(formals(get(v, envir = ns))), c(".plan", "...", "x"))
    for (a in args) {
      pat <- paste0("(?<![A-Za-z0-9_.])", gsub(".", "\\.", a, fixed = TRUE), "(?![A-Za-z0-9_])")
      if (!grepl(pat, text, perl = TRUE)) {
        missing <- c(missing, paste0(v, "(", a, ")"))
      }
    }
  }
  expect_identical(missing, character(0),
                   info = paste("not in the manual's section 17:", paste(missing, collapse = ", ")))
})
