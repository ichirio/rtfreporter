# Greedy all-pairs for the pairwise validation tests (test-validation-*.R).
#
# From `cand` (the valid full combinations, one per row, a column per
# factor), pick rows until every pair of levels of every two factors that
# occurs in `cand` is covered.  Deterministic: the first row with the
# largest gain.  Returns the rows picked, the pairs needed and the pairs the
# rows cover, so a test can check that every pair is covered.
#
# The pairs are worked out once, as a matrix of ids (a row a candidate, a
# column a pair of factors), so that a grid of tens of thousands of rows
# takes well under a second; the rows picked are the same as one pair at a
# time would pick.
.allpairs <- function(cand) {
  pr <- utils::combn(names(cand), 2L, simplify = FALSE)
  lab <- vapply(pr, function(p) paste(p[1L], cand[[p[1L]]], p[2L], cand[[p[2L]]], sep = "|"),
                character(nrow(cand)))
  lab <- matrix(lab, nrow = nrow(cand))
  # row by row, as each row's pairs are listed
  all_lab <- unique(as.vector(t(lab)))
  id <- matrix(match(lab, all_lab), nrow = nrow(cand))
  need <- rep(TRUE, length(all_lab))
  pick <- integer(0)
  while (any(need)) {
    i <- which.max(rowSums(matrix(need[id], nrow = nrow(id))))
    pick <- c(pick, i)
    need[id[i, ]] <- FALSE
  }
  list(rows = cand[pick, , drop = FALSE], need = all_lab,
       covered = unique(as.vector(t(lab[pick, , drop = FALSE]))))
}

# ---- on CRAN: a representative subset -------------------------------------
# The pairwise and decision-table sets, and the slow validation scenarios,
# run in full in CI (NOT_CRAN=true) and in part on CRAN, which asks for a
# short check time:
#   * a loop over a set calls .cran_cut(i) first: its first .cran_keep rows
#     run everywhere; past them the test stops on CRAN (skip_on_cran()), so
#     the rows after them and the rest of the block are CI only;
#   * a slow scenario (a test of the tm-thicken / tm-gaps / validation
#     files that takes more than about 0.4 s) starts with skip_on_cran().
# The checks of the set itself (every pair covered) run before the loop, on
# CRAN too.  Nothing is dropped: NOT_CRAN=true runs every row.
.cran_keep <- 3L
.cran_cut <- function(i) {
  if (i > .cran_keep) testthat::skip_on_cran()
  invisible()
}
