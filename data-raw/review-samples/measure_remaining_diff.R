setwd("C:/Users/ichir/AppData/Local/Temp/claude/C--Users-ichir/0e8953d4-534f-4543-9b50-71c7b61ba96a/scratchpad/plan-wt")
suppressMessages(pkgload::load_all(".", quiet = TRUE))
src <- new.env()
code <- readLines("data-raw/review-samples/generate.R")
stop_at <- grep("^# ------------------------------------------------------------- render ----", code)
eval(parse(text = paste(code[seq_len(stop_at - 1L)], collapse = "\n")), envir = src)

cat("Is every remaining difference the trailing blank row?\n\n")
for (cs in src$cases) {
  o <- cs$old(); n <- cs$new()
  extra <- 0L; other <- character(0)
  for (i in seq_along(o)) {
    ob <- o[[i]]$blank_rows; nb <- n[[i]]$blank_rows
    ob <- if (is.null(ob)) integer(0) else as.integer(ob)
    nb <- if (is.null(nb)) integer(0) else as.integer(nb)
    nrow_i <- nrow(o[[i]]$data)
    only_extra <- setdiff(ob, nb)
    if (!identical(sort(nb), sort(setdiff(ob, nrow_i)))) {
      other <- c(other, sprintf("page %d old=%s new=%s (rows=%d)", i,
                                paste(ob, collapse=","), paste(nb, collapse=","),
                                nrow_i))
    }
    extra <- extra + length(only_extra)
    # every extra must be the LAST row of that page
    if (length(only_extra) && !all(only_extra == nrow_i)) {
      other <- c(other, sprintf("page %d extra blank NOT at end: %s", i,
                                paste(only_extra, collapse=",")))
    }
  }
  cat(sprintf("  %-14s extra blanks in old: %d   %s\n", cs$id, extra,
              if (length(other)) paste("OTHER:", paste(other, collapse="; "))
              else "all of them at the page's last row"))
}
