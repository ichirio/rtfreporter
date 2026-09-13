# テーブルオブジェクトアダプタの追加

[In
English](https://ichirio.github.io/rtfreporter/articles/extending-adapters.md)

rtfreporter は、他パッケージ（gt, gtsummary, rtables/tern, flextable,
huxtable）が生成したテーブルを読み込み、メタデータ（カラムラベル、整列、
スパニングヘッダー、タイトル、脚注、セル単位スタイル）を保ったまま RTF
に 変換できます。この記事では、既存の
`gt_adapter.R`・`rtables_adapter.R`・
`flextable_adapter.R`・`huxtable_adapter.R` を雛形に、**新しいテーブル
オブジェクト型**を認識させる方法を示します。

まずは全体像のために
[アーキテクチャと内部構造](https://ichirio.github.io/rtfreporter/articles/architecture-ja.md)
を読んでください。

## 契約（contract）

アダプタの仕事は 1 つだけです。あなたのオブジェクトを、
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
が消費する共通の **kwargs リスト**へ変換することです。`data`
以外のフィールドはすべて任意です。

``` r
list(
  data            = <data.frame>,   # 必須: 描画済み本体（文字セルで可）
  col_header      = <文字ベクタ | 多段スパニング行>,
  col_spec        = list(list(col = 1L, align = "left"), ...),
  cell_styles     = <データ行ごとに 1 要素のリスト, または NULL>,
  titles_block    = <文字ベクタ | NULL>,   # ページタイトルになる
  footnotes_block = <文字ベクタ | NULL>    # ページ脚注になる
)
```

それ以外（ページ分割、ページごとの `cell_styles`
スライス、共有メタデータの 複製、タイトル/脚注の付与）はすべて
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
が行います。 `rtftable` を自分で組み立てる必要は**ありません**。

## ステップ 1 — 判定関数（detector）

軽量・依存の少ない述語を追加します。副作用を持たせないこと。

``` r

# R/mypkg_adapter.R
.is_mypkg_tbl <- function(x) inherits(x, "mypkg_table")
# （S4 クラスなら: isS4(x) && methods::is(x, "MyClass")）
```

## ステップ 2 — トークン解決

トークンにより、ユーザーは `read =`
でメタデータの取り込みを取捨選択できます。 gt / rtables
のリゾルバを踏襲してください。

``` r

.MYPKG_TOKENS_ALL <- c("col_header", "alignment", "titles", "footnotes")

.resolve_mypkg_tokens <- function(read) {
  if (is.null(read) || isFALSE(read)) return(character(0))
  if (isTRUE(read))                    return(.MYPKG_TOKENS_ALL)
  if (!is.character(read))
    stop("`read` must be FALSE/TRUE or a character vector of tokens.",
         call. = FALSE)
  bad <- setdiff(read, .MYPKG_TOKENS_ALL)
  if (length(bad))
    stop("Unknown token(s): ", paste(bad, collapse = ", "), call. = FALSE)
  read
}
```

## ステップ 3 — 抽出関数（extractor）

kwargs
リストを返します。重いパッケージは必ず遅延ロード（`requireNamespace`）にします。

``` r

.mypkg_to_rtftable_kwargs <- function(x, tokens = .MYPKG_TOKENS_ALL) {
  if (!requireNamespace("mypkg", quietly = TRUE))
    stop("Reading a mypkg table requires the `mypkg` package.", call. = FALSE)

  out <- list()
  out$data <- mypkg::as_body_data_frame(x)          # 本体の取り出し方は各パッケージ次第

  if ("col_header" %in% tokens)
    out$col_header <- mypkg::column_labels(x)

  if ("alignment" %in% tokens)
    out$col_spec <- lapply(seq_len(ncol(out$data)), function(j)
      list(col = j, align = mypkg::col_align(x, j)))

  if ("titles" %in% tokens)
    out$titles_block <- mypkg::title_lines(x)

  if ("footnotes" %in% tokens)
    out$footnotes_block <- mypkg::footnote_lines(x)

  out
}
```

既存アダプタから得られるコツ：

- **スパニングヘッダー**は多段の `col_header` です。上段の要素は
  `list(col_cell(pos = c(from, to), label = ...), ...)`、最終要素が
  葉ラベルの文字ベクタになります。`.extract_spanners()`（gt）と
  `.rtables_header_segments()`（rtables）を参照。
- **セル単位スタイル**は `cell_styles` に入れます。データ行ごとに 1
  要素の リストで、各要素は `NULL` か
  `list(bold=, italic=, underline=, indent_twips=)`。各値は長さ `ncol`
  の ベクタで、`NA` は「`col_spec` から継承」を意味します。
- **テキストに埋め込み済みの脚注マーク。** ソースがマークをインラインで
  描画する場合（gt は `<sup>N</sup>`、rtables は `{N}`）、rtfreporter の
  上付き記法 `^{N}` に変換します。このとき **`gsub`
  の後方参照を使わない** こと（一部のロケール不全な R ビルドは `\\1`
  を落とします）。 `.convert_footnote_marks()` /
  `.convert_rtables_marks()` のように
  [`regmatches()`](https://rdrr.io/r/base/regmatches.html)
  を使ってください。

## ステップ 4 — `as_rtftables()` への組み込み

`R/as_rtftables.R` に小さな編集を 3 箇所：

``` r
# (a) リスト入力判定が、あなたのオブジェクトを「要素のリスト」と
#     誤認しないようにする
if (is.list(x) && !is.data.frame(x) && !isS4(x) &&
    !.is_gt_tbl(x) && !.is_gtsummary_tbl(x) &&
    !.is_rtables_tbl(x) && !.is_mypkg_tbl(x)) { ... }

# (b) 本体＋メタデータを解決するディスパッチ
} else if (.is_mypkg_tbl(x)) {
  tokens          <- .resolve_mypkg_tokens(read)
  kw              <- .mypkg_to_rtftable_kwargs(x, tokens = tokens)
  body            <- kw$data
  cell_styles     <- kw$cell_styles
  titles_block    <- kw$titles_block
  footnotes_block <- kw$footnotes_block
}
```

必要なら、単一ラッパーの
[`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md)
の判定も緩め、あなたの型を 受け付けるようにします。

## ステップ 5 — 依存の宣言

`DESCRIPTION` の `Suggests:` に `mypkg` を追加します。`Imports:` には
**絶対に入れない**こと — ハード依存ゼロは意図的な方針です。ステップ 3 の
遅延 [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html)
が、未インストール時に親切なエラーを出します。

## ステップ 6 — テストとドキュメント

- `tests/testthat/test-mypkg-adapter.R` を追加し、各テストを
  `skip_if_not_installed("mypkg")` で包みます。
- 新しい `read`
  トークンを文書化（[`?as_rtftables`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
  を拡張）。
- `NEWS.md` に項目を追加。メジャー/マイナー変更なら `CHANGELOG.md`
  も更新。
- `devtools::document()`, `devtools::test()`, `devtools::check()`
  を実行。

## 参考になる実装

rtables アダプタ（`R/rtables_adapter.R`）は、契約のすべての要素 —
多段スパニングヘッダー、行ごとのインデントを `cell_styles` として、
インラインの `{N}` -\> `^{N}` マーク変換、タイトル/フッター抽出 — を、
たった 1 回の
[`formatters::matrix_form()`](https://rdrr.io/pkg/formatters/man/matrix_form.html)
呼び出しから実現する、
コンパクトで完全な実例です。模倣する雛形として最適です。
