# Lint one file for the lint-changed.sh hook; lints go to stderr, exit 2
f <- commandArgs(TRUE)[1]
l <- lintr::lint(f)
if (length(l)) {
  message(paste(utils::capture.output(print(l)), collapse = "\n"))
  quit(status = 2)
}
