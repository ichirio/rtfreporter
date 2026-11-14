# The hand-written validation tables are kept in one fixed order, so that a
# new row has its own place and two pull requests that each add rows touch
# different lines instead of both appending at the end:
#   traceability_review.csv  by fr, in the order the FR ids have in
#                            requirements.csv, then by ft in natural order
#                            (FT-AR063 before FT-AR3000); a row whose fr is not
#                            in requirements.csv goes at the end, by fr, then ft
#   reference_review.csv     by file
# requirements.csv keeps its own order (parents before children), and
# traceability_matrix.csv, which new tests no longer add to (they name their
# FT and FRs in their titles), keeps the order it has.

.ord_read <- function(f) {
  p <- system.file("validation", f, package = "rtfreporter")
  if (!nzchar(p)) p <- test_path("..", "..", "inst", "validation", f)
  utils::read.csv(p, colClasses = "character", na.strings = character(0),
                  stringsAsFactors = FALSE)
}

# The first row that sorts before the row above it, as a message naming both
# by their line in the file (line 1 is the header); "" when all are in order.
.ord_first_out <- function(keys, label, file) {
  o <- do.call(order, c(unname(keys), list(method = "radix")))
  pos <- integer(length(o))
  pos[o] <- seq_along(o)
  i <- which(diff(pos) < 0L)
  if (!length(i)) return("")
  i <- i[1L] + 1L
  sprintf("%s: line %d (%s) belongs before line %d (%s).", file, i + 1L, label[i], i, label[i - 1L])
}

test_that("traceability_review.csv is in order: fr as in requirements.csv, then ft", {
  req <- .ord_read("requirements.csv")
  rv <- .ord_read("traceability_review.csv")
  pos <- match(rv$fr, req$id)
  known <- !is.na(pos)
  num <- suppressWarnings(as.integer(sub("^\\D*", "", rv$ft)))
  keys <- list(ifelse(known, pos, nrow(req) + 1L), ifelse(known, "", rv$fr),
               sub("[0-9]+$", "", rv$ft), ifelse(is.na(num), -1L, num), rv$ft)
  msg <- .ord_first_out(keys, paste(rv$ft, rv$fr), "traceability_review.csv")
  expect(!nzchar(msg), paste(msg, "A row goes to its place: by fr in the order of",
                             "requirements.csv, then by ft (the number as a number); never to the end."))
})

test_that("reference_review.csv is in order by file", {
  rr <- .ord_read("reference_review.csv")
  msg <- .ord_first_out(list(rr$file), rr$file, "reference_review.csv")
  expect(!nzchar(msg), paste(msg, "A row goes to its place by file; never to the end."))
})
