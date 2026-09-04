# 外部インターフェース仕様書

[In
English](https://ichirio.github.io/rtfreporter/articles/external-api.md)

rtfreporter の**公開
API**に関するコントリビューター向けリファレンスです。
エクスポートされた関数、**それらが返す S3
クラスのオブジェクト**、そして全体を
つなぐパイプの流れを説明します。全体像は [Architecture &
internals](https://ichirio.github.io/rtfreporter/articles/architecture.md)、内部データ構造は
[内部クラス設計書](https://ichirio.github.io/rtfreporter/articles/internal-design-ja.md)
を参照してください。

> rtfreporter は**純粋な S3**
> 実装で、実行時のハード依存はありません。公開 オブジェクトはすべて S3
> `class` 属性を持つ素の `list`
> なので、[`dput()`](https://rdrr.io/r/base/dput.html) 可能で、
> [`str()`](https://rdrr.io/r/utils/str.html)
> で中身を確認でき、シリアライズも安全です。

## 用語

「S3 関数」は曖昧なので、本書では次のように区別します。

- **コンストラクタ関数** — **S3
  クラスのオブジェクト**を生成して返すエクスポート
  関数（例：[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  はクラス `"rtftable"` のオブジェクトを返す）。以下の公開 API
  の大半がこれにあたります。
- **S3 ジェネリック / メソッド** —
  オブジェクトのクラスでディスパッチする
  [`print()`](https://rdrr.io/r/base/print.html) /
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html)（例：[`print.rtftable()`](https://ichirio.github.io/rtfreporter/reference/print.rtftable.md)）。

本書で「`f()`
が返すオブジェクト」と書く場合、それはコンストラクタが生成する **S3
クラスのオブジェクト**を指し、ジェネリックではありません。

## パイプの流れ

レポートは `rtf_document`
をコンテンツ／セクション呼び出しでパイプし、最後に
レンダリングして構築します。

``` r

library(rtfreporter)

rtf_document() |>                                  # -> rtf_document
  rtf_tables(as_rtftables(df)) |>                  # コンテンツページを追加
  rtf_section(page = 1, secinfo = list(            # ヘッダー/フッターを割当
    header = rtf_header(...), footer = rtf_footer(...))) |>
  generate_rtfreport("out.rtf", overwrite = TRUE)  # ファイルへ出力
```

[`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
および `rtf_*()` パイプ動詞は**新しい** `rtf_document` を返します
（コピーオンモディファイ。その場で破壊的変更はしません）。

## 公開クラスとコンストラクタ

| クラス | コンストラクタ | 役割 |
|----|----|----|
| `rtf_document` | [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md) | 組み立て中の文書（ページ＋セクション＋文書全体設定）。 |
| `rtftable` | [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md), [`as_rtftable()`](https://ichirio.github.io/rtfreporter/reference/as_rtftable.md), [`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md) | 1 つのテーブル（または分割後の 1 ページ）。 |
| `rtfplot` | [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md) | 埋め込み図 1 つ。 |
| `rtf_border` | [`rtf_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_border.md), `TRUE`, `rtf_border(top = TRUE)` / `_bottom()` / `_box()` / `_none()`, [`rtf_border_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_with.md) | ボーダー指定（辺ごとの線種/太さ/色）。 |
| `rtf_table_border` | [`rtf_table_border()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_border.md), [`rtf_border_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_border_tfl.md) — **どちらも非推奨** | 5ゾーンの割り当て表。表全体なら `rtftable(border = rtf_border(...))`、行の種類ごとなら [`style_zone()`](https://ichirio.github.io/rtfreporter/reference/style_header.md)、TFL 既定は `border = "tfl"` または [`rtf_table_style_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_tfl.md) に置き換わりました。 |
| `rtf_table_style` | [`rtf_table_style()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style.md), [`rtf_table_style_with()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_with.md), [`rtf_table_style_tfl()`](https://ichirio.github.io/rtfreporter/reference/rtf_table_style_tfl.md) | 再利用可能なスタイル（ボーダー＋パディング＋行高の既定値）。構築時にスナップショット取得。 |
| `rtf_col_header` | [`rtf_col_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_col_header.md), [`col_cell()`](https://ichirio.github.io/rtfreporter/reference/col_cell.md), [`add_col_header_row()`](https://ichirio.github.io/rtfreporter/reference/add_col_header_row.md) | 複数行／スパニングの列ヘッダー。 |

公開関数がすべてコンストラクタというわけではありません。[`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
は
**コンストラクタではなく設定関数（configurator）**です。`rtf_document`
を受け取り、 非 `NULL` のフィールドだけ更新して、**同じ** `rtf_document`
クラスのコピーを返します
（新しいクラスは設定しません）。他のパイプ動詞（[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)、[`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)
…） と同じ位置づけです。同様に
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
はヘッダー／フッターを
表す素の（クラス無し）リストを返し、[`update_header_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
/
[`update_footer_row()`](https://ichirio.github.io/rtfreporter/reference/update_header_row.md)
は更新したコピーを返します。

> **`rtf_document` と `rtfreport`。** `rtf_document`
> はパイプで構築・保持する *公開*
> オブジェクトです。レンダリング時（[`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)）にのみ、別の
> *内部* クラス `rtfreport`
> へ変換されます（[内部クラス設計書（S3）](https://ichirio.github.io/rtfreporter/articles/internal-design-ja.md)
> 参照）。`rtfreport` を直接構築・操作することはありません。

## コンテンツ・セクション・レンダリング

- **コンテンツ** — `rtf_tables(doc, tables, ...)` と
  `rtf_figures(doc, figures, ...)` がコンテンツページを追加（1 要素 = 1
  ページ）。[`rtf_titles()`](https://ichirio.github.io/rtfreporter/reference/rtf_titles.md)
  /
  [`rtf_footnotes()`](https://ichirio.github.io/rtfreporter/reference/rtf_footnotes.md)
  でページ単位のタイトル／脚注を付与。いずれも更新後の `rtf_document`
  を返す。
- **セクション** — `rtf_section(doc, page, secinfo)`
  がページ範囲にヘッダー／
  フッターを割り当てる（フラットなページ列へのオーバーレイ。`NULL`
  は前セクション を継承）。
- **レンダリング** — `generate_rtfreport(doc, file, overwrite)` が
  `.rtf` を書き出し、 パスを invisible で返す。

## テーブルの取り込みと分割

`as_rtftables(x, ...)` は統一入口で、`gt` / gtsummary / rtables の
`VTableTree` / `data.frame` / tibble（およびそれらのリスト）を
**`rtftable` ページオブジェクトの
リスト**へ変換します。メタデータ読み取りと分割を 1
回の呼び出しで行います。 `as_rtftable(x, ...)`
は単一ページ版（`rtftable` 1 つ）。分割戦略は名前で指定し
（`split = "group_safe"` など。設定は同じ階層の引数で渡します）、自作の
`split=` 関数も受け付けます。空行は `blank_rows` /
[`set_blank_rows()`](https://ichirio.github.io/rtfreporter/reference/set_blank_rows.md)
で制御し、`count_blank_rows = TRUE` で `max_rows`
にカウントできます。さらに、分割の前に本体を**並べ替え**（`sort_by` /
`sort_desc`）、 指定列で**グループ化**（`group_col` /
`group_by`、連続重複の抑制は `collapse_repeats`）、
そのグループ化・並べ替えに使いつつ帳票には出さない**列の非表示**（`drop_cols`、例えば
ソートキー専用列）が可能です。いずれも入力本体の列座標を共有します。詳細は
[`as_rtftables()`
によるページ分割](https://ichirio.github.io/rtfreporter/articles/pagination.md)
を参照してください。

## 出力ヘルパー

- [`rtf_replace_text()`](https://ichirio.github.io/rtfreporter/reference/rtf_replace_text.md)
  — 生成済み `.rtf` への後処理（検索置換）。
- [`assemble_rtf()`](https://ichirio.github.io/rtfreporter/reference/assemble_rtf.md)（および
  [`assemble_files()`](https://ichirio.github.io/rtfreporter/reference/assemble_files.md)
  /
  [`assemble_folder()`](https://ichirio.github.io/rtfreporter/reference/assemble_folder.md)
  /
  [`assemble_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_spec.md)
  /
  [`assemble_from_spec()`](https://ichirio.github.io/rtfreporter/reference/assemble_from_spec.md)
  /
  [`assemble_toc()`](https://ichirio.github.io/rtfreporter/reference/assemble_toc.md),
  [`toc_heading()`](https://ichirio.github.io/rtfreporter/reference/toc_heading.md)
  /
  [`toc_entry()`](https://ichirio.github.io/rtfreporter/reference/toc_entry.md)）—
  複数 RTF を目次・ブックマーク付きで 1 つに結合。

## S3 ジェネリック

文書・スペックオブジェクトに
[`print()`](https://rdrr.io/r/base/print.html)
メソッドがあり、[`plot()`](https://rdrr.io/r/graphics/plot.default.html)
メソッドは
ボーダー／テーブル／文書のワイヤーフレームをベースグラフィックスで描画し、
レンダリング前のレイアウト確認に使えます。`rtftable` の
[`print()`](https://rdrr.io/r/base/print.html) は、列見出し
（スパニング見出しを含む）・セル内容・罫線・列ごとのアラインメントを反映した
テーブルのプレビューをコンソールに表示するので、内容をそのまま目視確認できます
（[`format()`](https://rdrr.io/r/base/format.html)
はその行を、[`summary()`](https://rdrr.io/r/base/summary.html)
はメタデータの要約を返します）。各関数の詳細は
[関数リファレンス](https://ichirio.github.io/rtfreporter/reference/index.md)
を参照してください。
