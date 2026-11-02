# Greedy all-pairs for the pairwise validation tests (test-validation-*.R).
#
# From `cand` (the valid full combinations, one per row, a column per
# factor), pick rows until every pair of levels of every two factors that
# occurs in `cand` is covered.  Deterministic: the first row with the
# largest gain.  Returns the rows picked, the pairs needed and the pairs the
# rows cover, so a test can check that every pair is covered.
.allpairs <- function(cand) {
  pr <- utils::combn(names(cand), 2L, simplify = FALSE)
  cov <- lapply(seq_len(nrow(cand)), function(i) {
    vapply(pr, function(p) paste(p[1L], cand[i, p[1L]], p[2L], cand[i, p[2L]], sep = "|"), "")
  })
  need <- unique(unlist(cov))
  pick <- integer(0)
  while (length(need)) {
    i <- which.max(vapply(cov, function(k) sum(k %in% need), 0L))
    pick <- c(pick, i)
    need <- setdiff(need, cov[[i]])
  }
  list(rows = cand[pick, , drop = FALSE], need = unique(unlist(cov)),
       covered = unique(unlist(cov[pick])))
}
