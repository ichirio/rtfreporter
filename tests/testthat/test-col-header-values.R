# set_col_header(values = ): {tokens} filled per page from a keyed table (#449)

.vt_pages <- function(periods = c("Per 1", "Per 2"), pages = c("1", "2")) {
  arms <- c("Placebo", "TAK-003")
  days <- paste("Day", 1:2)
  d <- do.call(rbind, lapply(periods, function(per)
    do.call(rbind, lapply(pages, function(pg) {
      x <- data.frame(period = per, page = pg, row_grp1 = "SOC",
                      label = c("Mild", "Severe"), stringsAsFactors = FALSE)
      for (a in arms) for (v in days) x[[paste0(a, "____", v)]] <- "1"
      x
    }))))
  as_rtftables(d, split = "by_value", group_col = "period", page_by = "page",
               drop_cols = c("page", "period"),
               stub_vars = c("row_grp1", "label"), stub_label = "Sev")
}

.vt_hdr <- function() {
  rtf_col_header(
    list(col_cell(1L, ""),
         col_cell(c(2L, 3L), "Placebo\n(N={n_pbo})"),
         col_cell(c(4L, 5L), "TAK-003\n(N={n_trt})")),
    c("Sev", rep(paste("Day", 1:2), 2L)))
}

.vt_vals <- function() {
  data.frame(group = c("Per 1", "Per 2"), n_pbo = c(120, 118),
             n_trt = c(115, 112), stringsAsFactors = FALSE)
}

.span <- function(page, k = 2L) {
  Filter(function(c1) nzchar(c1$label %||% ""), page$col_header[[1L]])[[k - 1L]]
}

test_that("each page takes the row its own group key names", {
  pg <- set_col_header(.vt_pages(), .vt_hdr(), values = .vt_vals())
  expect_identical(.span(pg[[1L]])$label, "Placebo\n(N=120)")   # Per 1
  expect_identical(.span(pg[[3L]])$label, "Placebo\n(N=118)")   # Per 2
  expect_identical(.span(pg[[1L]], 3L)$label, "TAK-003\n(N=115)")
  expect_identical(.span(pg[[3L]], 3L)$label, "TAK-003\n(N=112)")
})

test_that("pages of one group share its row", {
  pg <- set_col_header(.vt_pages(), .vt_hdr(), values = .vt_vals())
  expect_identical(.span(pg[[1L]])$label, .span(pg[[2L]])$label)
})

test_that("the keys survive drop_cols and the column split", {
  pg <- set_col_header(.vt_pages(), .vt_hdr(), values = .vt_vals()) |>
    paginate_cols(by = "____", carry = 1, width = "keep")
  # the group column was dropped, yet every page still found its N
  labs <- vapply(pg, function(p) .span(p)$label, character(1L))
  expect_true(all(grepl("N=(120|118|115|112)", labs)))
  expect_identical(sum(grepl("N=120", labs)), 2L)   # Per 1, both page values
})

test_that("by = \"rows\" matches the page_by value", {
  vals <- data.frame(rows = c("1", "2"), n_pbo = c(10, 20), n_trt = c(30, 40),
                     stringsAsFactors = FALSE)
  pg <- set_col_header(.vt_pages(), .vt_hdr(), values = vals, by = "rows")
  expect_identical(.span(pg[[1L]])$label, "Placebo\n(N=10)")    # page 1
  expect_identical(.span(pg[[2L]])$label, "Placebo\n(N=20)")    # page 2
})

test_that("by = \"name\" matches the unique page name", {
  x    <- .vt_pages()
  vals <- data.frame(name = names(x), n_pbo = 1:4, n_trt = 5:8,
                     stringsAsFactors = FALSE)
  pg <- set_col_header(x, .vt_hdr(), values = vals, by = "name")
  expect_identical(.span(pg[[4L]])$label, "Placebo\n(N=4)")
})

test_that("a compound key matches on both axes", {
  vals <- expand.grid(group = c("Per 1", "Per 2"), rows = c("1", "2"),
                      stringsAsFactors = FALSE)
  vals$n_pbo <- seq_len(nrow(vals))
  vals$n_trt <- seq_len(nrow(vals)) * 10L
  pg <- set_col_header(.vt_pages(), .vt_hdr(), values = vals,
                       by = c("group", "rows"))
  expect_length(pg, 4L)
  expect_true(all(nzchar(vapply(pg, function(p) .span(p)$label, character(1L)))))
})

# ──────── the guards ───────────────────────────────────────────────────────

test_that("a page with no row in `values` is an error that names the key", {
  vals <- .vt_vals()[1L, , drop = FALSE]
  expect_error(set_col_header(.vt_pages(), .vt_hdr(), values = vals),
               "no row for page")
  expect_error(set_col_header(.vt_pages(), .vt_hdr(), values = vals),
               "Per 2")
})

test_that("a row of `values` that no page used is an error", {
  vals <- rbind(.vt_vals(),
                data.frame(group = "Per 3", n_pbo = 1, n_trt = 2))
  expect_error(set_col_header(.vt_pages(), .vt_hdr(), values = vals),
               "matched no page")
})

test_that("a token with no value is an error naming it", {
  hdr <- rtf_col_header(
    list(col_cell(1L, ""), col_cell(c(2L, 3L), "{nope}"),
         col_cell(c(4L, 5L), "x")),
    c("Sev", rep(paste("Day", 1:2), 2L)))
  expect_error(set_col_header(.vt_pages(), hdr, values = .vt_vals()),
               "no value for")
  expect_error(set_col_header(.vt_pages(), hdr, values = .vt_vals()), "nope")
})

test_that("a render-time token is left for the renderer", {
  hdr <- rtf_col_header(
    list(col_cell(1L, ""), col_cell(c(2L, 3L), "p. {PAGE} (N={n_pbo})"),
         col_cell(c(4L, 5L), "x")),
    c("Sev", rep(paste("Day", 1:2), 2L)))
  pg <- set_col_header(.vt_pages(), hdr, values = .vt_vals())
  expect_identical(.span(pg[[1L]])$label, "p. {PAGE} (N=120)")
})

test_that("{{ is a literal brace", {
  hdr <- rtf_col_header(
    list(col_cell(1L, ""), col_cell(c(2L, 3L), "{{n_pbo}} = {n_pbo}"),
         col_cell(c(4L, 5L), "x")),
    c("Sev", rep(paste("Day", 1:2), 2L)))
  pg <- set_col_header(.vt_pages(), hdr, values = .vt_vals())
  expect_identical(.span(pg[[1L]])$label, "{n_pbo} = 120")
})

test_that("`values` without the key column says which one it wants", {
  vals <- .vt_vals()
  names(vals)[1L] <- "period"
  expect_error(set_col_header(.vt_pages(), .vt_hdr(), values = vals),
               "no key column")
})

test_that("tokens are filled in the label row too", {
  hdr <- rtf_col_header(
    list(col_cell(1L, ""), col_cell(c(2L, 3L), "A"), col_cell(c(4L, 5L), "B")),
    c("{n_pbo}", rep(paste("Day", 1:2), 2L)))
  pg <- set_col_header(.vt_pages(), hdr, values = .vt_vals())
  expect_identical(pg[[1L]]$col_header[[2L]][1L], "120")
})

test_that("no `values` leaves the broadcast behaviour alone", {
  hdr <- rtf_col_header(
    list(col_cell(1L, ""), col_cell(c(2L, 3L), "A"), col_cell(c(4L, 5L), "B")),
    c("Sev", rep(paste("Day", 1:2), 2L)))
  pg <- set_col_header(.vt_pages(), hdr)
  expect_identical(.span(pg[[1L]])$label, .span(pg[[3L]])$label)
})

# ──────── header_map() ─────────────────────────────────────────────────────

test_that("header_map() shows one row per header cell, with the keys", {
  pg <- set_col_header(.vt_pages(), .vt_hdr(), values = .vt_vals())
  hm <- header_map(pg)
  expect_true(all(c("page", "name", "group", "rows", "row", "cell",
                    "from", "to", "text") %in% names(hm)))
  spans <- hm[hm$row == 1L & nzchar(hm$text), ]
  expect_identical(spans$text[spans$page == 1L],
                   c("Placebo\n(N=120)", "TAK-003\n(N=115)"))
  expect_identical(unique(spans$group[spans$page == 3L]), "Per 2")
  expect_identical(spans$from[spans$page == 1L], c(2L, 4L))
})

test_that("header_map() takes a single table and an empty header", {
  df  <- data.frame(a = "1", b = "2", stringsAsFactors = FALSE)
  tbl <- rtftable(df) |> set_col_header(c("A", "B"))
  expect_equal(nrow(header_map(tbl)), 2L)
  expect_equal(nrow(header_map(rtftable(df))), 0L)
})
