# アーキテクチャと内部構造（コントリビューター向け）

[In
English](https://ichirio.github.io/rtfreporter/articles/architecture.md)

この記事は、rtfreporter
を**拡張・コントリビュート**したい方のための日本語の入口です。
全体像を解説します。詳細設計書は末尾にリンクしています（各ページは英語で開き、
日本語トグルがあります）。

> 🌐 **詳細設計書：**
> [内部クラス設計書（S3）](https://ichirio.github.io/rtfreporter/articles/internal-design-ja.md)
> ·
> [外部インターフェース仕様書](https://ichirio.github.io/rtfreporter/articles/external-api-ja.md)

## 設計原則

- **全面 S3。** すべてのオブジェクトは素の S3 リストで、外部 OOP
  フレームワークは 使いません。そのため
  [`dput()`](https://rdrr.io/r/base/dput.html)
  可能で、[`saveRDS()`](https://rdrr.io/r/base/readRDS.html)
  でも素直にシリアライズ できます。
- **ハード依存ゼロ。** `Imports:` は `methods`（R
  同梱）だけです。任意連携 （gt, gtsummary, rtables/tern, ggplot2
  …）はすべて `Suggests:` に置き、
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html)
  で確認し、未インストール時は親切なエラーを出します。
- **長さの単位は twips のみ**（1 inch = 1440 twips）。
- **不変・関数型のヘルパー。**
  内部の更新処理は、その場で書き換えるのではなく
  *新しいコピー*を返します。
- **「none」は省略を意味する。** ボーダーの辺が `"none"`
  の場合、ゼロ幅の ボーダーを明示するのではなく、RTF
  コマンドを一切出力しません。

## リポジトリ構成

    rtfreporter/
    ├── R/
    │   ├── pipe-composition.R   # 公開パイプ API: rtf_document/section/tables/figures
    │   ├── rtftable.R           # rtftable() S3 オブジェクト + 正規化ヘルパー
    │   ├── rtfplot.R            # rtfplot() S3 オブジェクト
    │   ├── rtfreport.R          # 内部 rtfreport 文書オブジェクト + rtf_header/footer
    │   ├── generate_rtfreport.R # RTF レンダラー（S3 文書 -> .rtf テキスト）
    │   ├── rtf_border.R         # ボーダー階層（S3）
    │   ├── rtf_table_style.R    # 共有テーブルスタイル（S3、スナップショット意味論）
    │   ├── col_header.R         # 統一カラムヘッダー API（col_cell / rtf_col_header）
    │   ├── as_rtftables.R       # テーブルオブジェクト -> rtftable ページのリスト（ハブ）
    │   ├── as_rtftable.R        # 単一ページ用の便利ラッパー
    │   ├── gt_adapter.R         # gt / gtsummary のメタデータ抽出
    │   ├── rtables_adapter.R    # rtables / tern のメタデータ抽出
    │   ├── paginate.R           # （非推奨）ページ分割ジェネリック + 内部コア
    │   ├── blank_rows.R         # 空行指定 + 属性リーダー
    │   └── ...
    ├── inst/resources/          # 管理者が調整可能な RTF コマンドテンプレート・既定値
    ├── tests/testthat/          # テストスイート（S3, edition 3）
    └── vignettes/               # ユーザーガイド（直下）+ articles/（サイト専用）

## オブジェクトモデル

    rtf_document          （公開 S3、パイプ API で構築）
      ├── contents[]      1 ページ 1 要素: rtftable | rtfplot
      ├── titles[]        ページごとのタイトルブロック（文字ベクタ）
      ├── footnotes[]     ページごとの脚注ブロック
      └── sections[]      ページ範囲ごとのヘッダー/フッター（from_page で指定）

    rtftable              1 つのテーブル（単一 or 複数 data.frame）
      ├── data / data_list
      ├── col_header(_list)   文字ベクタ | 多段スパニング行
      ├── col_spec            列ごとの align / bold / italic / indent / border
      ├── cell_styles         セルごとの上書き（bold/italic/underline/indent）
      ├── border              rtf_table_border（ゾーン）
      └── width / height / padding / valign フィールド

    rtfplot               1 つの埋め込み図（PNG/JPEG）

`rtf_document` は公開・パイプ向けの表層です。内部では
`.pipe_doc_to_rtfreport()` で `rtfreport` オブジェクトに変換され、
`.rtfreport_validate()` で検証され、レンダラーに渡されます。

## レンダリングパイプライン

    rtf_document()  |>  rtf_section()  |>  rtf_tables() / rtf_figures()
            │
            ▼   .pipe_doc_to_rtfreport()         （アダプタ: パイプ API -> rtfreport）
       rtfreport
            │   .rtfreport_validate()            （既定セクション補完・各種検証）
            ▼
       generate_rtfreport(doc, "out.rtf")        （generate_rtfreport.R のレンダラー）
            │   .render_rtftable() / .render_rtfplot()
            ▼
       .rtf  テキストファイル

レンダラーは `inst/resources/rtf_commands.R`
のテンプレートに基づく純粋な 文字列組み立てです。サイズ・余白の既定値は
`inst/resources/rtfreporter_defaults.R`
にあり、コードを触らず調整できます。

## テーブルオブジェクトのハブ：`as_rtftables()`

[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
は、外部の*テーブル オブジェクト*を `rtftable`
ページのリストへ変換する単一の入口です。 `gt_tbl`、**gtsummary**
テーブル、**rtables/tern**（`VTableTree`）、 素の
`data.frame`/tibble（およびそれらのリスト）に対応します。

どの入力も、1 つの共通中間形式 — **kwargs リスト** — に還元され、
以降のパイプラインは同一に処理します。

``` r
list(
  data            = <data.frame>,   # 描画済み本体
  col_header      = <chr | 多段スパニング行>,
  col_spec        = <list(list(col=, align=, ...), ...)>,
  cell_styles     = <行ごとのリスト, または NULL>,
  titles_block    = <chr | NULL>,   # -> ページタイトル
  footnotes_block = <chr | NULL>    # -> ページ脚注
)
```

`gt_adapter.R` と `rtables_adapter.R` は、まさにこの形を生成します。
その後
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
は、必要に応じて分割前に本体を**並べ替え** （`sort_by` /
`sort_desc`。グループ化が並べ替え後の行を見るようにするため）、
本体をページ分割し（`.paginate_df()`）、`cell_styles`
をページごとにスライスし、
共有のヘッダー/幅/スパニングメタデータを各ページに複製し、タイトル/脚注ブロックを
`rtf_titles` / `rtf_footnotes`
属性として付与します（[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
が読み取ります）。
ある列はグループ化・並べ替えのために分割を通過させたうえで、描画ページからは
**非表示**にできます（`drop_cols`）。対象列は `.paginate_df()`
まで残し、各ページの
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
構築直前に `.apply_col_drop()` が列を削除し、位置インデックスを持つ
全引数（スパニング含むヘッダー、`col_spec`、幅、`row_title`、`cell_styles`）を
残った列へ振り直します。

アダプタがすべて 1 つの形に収束するため、**新しい入力型の追加は小さく
閉じた作業**で済みます —
[テーブルオブジェクトアダプタの追加](https://ichirio.github.io/rtfreporter/articles/extending-adapters.md)
を参照してください。

## さらに読むには

- 関数ごとのリファレンス：[Reference](https://ichirio.github.io/rtfreporter/reference/index.md)
  索引。
- 公開 S3
  の正確な契約：[外部インターフェース仕様書](https://ichirio.github.io/rtfreporter/articles/external-api-ja.md)。
- 内部クラス設計とヘルパー一覧：[内部クラス設計書（S3）](https://ichirio.github.io/rtfreporter/articles/internal-design-ja.md)。
- コントリビュートの流れとコードスタイル：
  [CONTRIBUTING.md](https://github.com/ichirio/rtfreporter/blob/main/CONTRIBUTING.md)。
