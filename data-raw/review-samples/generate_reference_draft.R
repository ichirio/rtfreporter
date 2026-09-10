# Run from the repository root (the package worktree).

# Read the CURRENT reference index straight out of _pkgdown.yml, so the draft
# below is a diff against what is actually published rather than my memory.
y <- readLines("_pkgdown.yml", warn = FALSE)
start <- grep("^reference:", y)
end   <- grep("^articles:", y)
ref   <- y[start:(end - 1L)]

secs <- list(); cur <- NULL
for (l in ref) {
  if (grepl("^  - title:", l)) {
    if (!is.null(cur)) secs[[length(secs) + 1L]] <- cur
    cur <- list(title = trimws(sub("^  - title:", "", l)), items = character(0))
  } else if (grepl("^      - ", l) && !is.null(cur)) {
    cur$items <- c(cur$items, trimws(sub("^      - ", "", l)))
  }
}
if (!is.null(cur)) secs[[length(secs) + 1L]] <- cur

# The five page_split_*() factories are already gone from main (#334), so they
# no longer appear in _pkgdown.yml and no longer need marking here.
RETIRE <- c("as_rtftables", "as_rtftable", "stub_spec")

PLAN <- c("rtf_plan", "plan_roles", "role", "plan_group", "plan_stub",
          "plan_hide", "plan_unset", "plan_listing", "plan_blanks",
          "plan_pages", "plan_style", "plan_header", "plan_tables",
          "rtf_pages")

out <- c(
"# Reference 索引の草案 — plan 系を採用した場合",
"",
"現行の `_pkgdown.yml` から機械的に生成した**変更後の姿**です。",
"公開中の [Reference](https://ichirio.github.io/rtfreporter/reference/) と",
"見比べてください。",
"",
"凡例  **NEW** = 新設 / ~~取消線~~ = Deprecated へ移動 / 無印 = 変更なし",
"", "---", "")

for (s in secs) {
  items <- s$items
  if (length(items) == 0L) next
  ttl <- s$title
  if (grepl("^Importing tables", ttl)) {
    out <- c(out, sprintf("### %s", ttl),
             "",
             "> `as_rtftables()` / `as_rtftable()` / `stub_spec()` は plan 系に置き換わるため Deprecated へ移動。",
             "> `combine_sections()` と `stub_cols()` は残ります（`stub_cols()` は plan が内部で使用）。",
             "")
  } else if (grepl("^Listings", ttl)) {
    out <- c(out, sprintf("### %s", ttl),
             "",
             "> **listing 系はそのまま残ります。** `listing_col()` / `listing_spec()` /",
             "> `build_listing()` は「並べ方」を決める関数で、plan が置き換えるのは",
             "> 入口の `as_rtftables(listing = )` だけです（→ `plan_listing()`）。",
             "> `plan_listing()` は `listing_spec()` の11設定のうち8つだけを持ちます。",
             "> `blank_row_first` は `plan_blanks(first = )`、`align` は",
             "> `plan_roles(role(align = ))` に寄せたためです。",
             "")
  } else if (grepl("^Pagination strategies", ttl)) {
    out <- c(out, sprintf("### %s", ttl),
             "",
             "> `page_split_*()` は #334 で廃止済み。plan 系では `plan_pages()` が",
             "> その役目を持ちます。`paginate_cols()` と `add_cont_label()` は残ります",
             "> （plan 未対応のため必須）。",
             "")
  } else {
    out <- c(out, sprintf("### %s", ttl), "")
  }
  for (i in items) {
    mark <- if (i %in% RETIRE) sprintf("~~`%s()`~~", i) else sprintf("`%s()`", i)
    out <- c(out, paste0("- ", mark))
  }
  out <- c(out, "")

  # the new section slots in right after page content
  if (grepl("^Page content", ttl)) {
    out <- c(out,
      "### Building a table plan  **NEW**", "",
      "> 何を作るかを宣言し、解決は最後に1回だけ行う。",
      "> 列は**名前**で指定し、位置は解決時に1つの写像が決めます。",
      "",
      paste0("- `", PLAN, "()`  **NEW**"), "")
  }
}

# Deprecated section, rebuilt
out <- c(out, "---", "",
  "### Deprecated  （変更後）", "",
  "> いずれも動作は変わりません。ドキュメント上で後継を案内するだけです。",
  "",
  "| 関数 | 後継 |",
  "|---|---|",
  "| `as_rtftables()` | `rtf_plan()` + レイヤー |",
  "| `as_rtftable()` | `rtf_plan()`（単数形は `rtf_pages(p)[[1]]`） |",
  "| `stub_spec()` | `plan_stub()` |",
  "| `paginate()` | `plan_pages()`（既に Deprecated） |",
  "",
  "**3関数のみ**が Deprecated になります（`page_split_*()` の5つは #334 で",
  "廃止済みなので、この表には残っていません）。",
  "listing 系は1つも Deprecated になりません: `as_rtftables(listing = )` という",
  "**引数**が `plan_listing()` に置き換わるだけです。",
  "`set_col_header()` / `set_header_cell()` / `set_blank_rows()` /",
  "`collapse_repeats()` は plan でも宣言できますが、**完成テーブルへの",
  "後付け調整**という別の役割があるため、すべて残します。",
  "")

writeLines(out, "data-raw/review-samples/REFERENCE_DRAFT.md")
cat("wrote data-raw/review-samples/REFERENCE_DRAFT.md  (",
    length(out), "lines )\n")
cat("sections:", length(secs), "  retired marked:", length(RETIRE), "\n")
