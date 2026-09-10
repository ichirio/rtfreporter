# rtfreporter — 2バージョンを切り替えて比較する環境

レビュー用に、**本機能実装前（main）**と**実装ブランチ（design/plan-resolver）**の
rtfreporter を同じ PC に共存させ、切り替えて実行する手順です。

R のライブラリを2つに分け、`lib.loc` で読み分けます。既存の
`R CMD INSTALL` を上書きしないので、普段の環境は壊れません。

---

## 1. 作業ツリーを2つ用意する

ブランチを切り替えずに済むよう、git worktree を使います。

```bash
cd C:/Yrepo/rtfreporter
git fetch origin
git worktree add C:/Yrepo/rtfreporter-main main
git worktree add C:/Yrepo/rtfreporter-plan design/plan-resolver
```

すでに `C:/Yrepo/rtfreporter` がブランチ側なら、そこをそのまま使い、
main の worktree だけ作れば足ります。

## 2. ライブラリを2つ作り、それぞれにインストールする

```bash
mkdir -p C:/Yrepo/rtflibs/main C:/Yrepo/rtflibs/plan

cd C:/Yrepo/rtfreporter-main
R CMD INSTALL --no-multiarch --no-docs --library=C:/Yrepo/rtflibs/main .

cd C:/Yrepo/rtfreporter-plan
R CMD INSTALL --no-multiarch --no-docs --library=C:/Yrepo/rtflibs/plan .
```

## 3. R から切り替えて使う

```r
LIB_OLD <- "C:/Yrepo/rtflibs/main"
LIB_NEW <- "C:/Yrepo/rtflibs/plan"

# 旧
library(rtfreporter, lib.loc = LIB_OLD)

# 新に切り替えるときは、いったん外してから読み直す
detach("package:rtfreporter", unload = TRUE)
library(rtfreporter, lib.loc = LIB_NEW)
```

`detach()` が効かない場合（他パッケージが掴んでいる等）は **R を再起動**して
ください。これが最も確実です。RStudio なら Ctrl+Shift+F10。

### plan 系の関数は export していません

このブランチは NAMESPACE に手を入れていません。main の
`tests/testthat/test-api-surface.R` が export 数を数えており、spike の
関数がそこに混ざると「公開 API がいくつか」という main 側の測定が狂うためです。
したがって `library(rtfreporter)` だけでは `rtf_plan()` は見えません。

**呼び出し方は2通りあります。**

```r
# (a) 名前空間から直接 --- R CMD INSTALL した版で使えます
rtfreporter:::rtf_plan(df) |> rtfreporter:::plan_stub(c("SOC", "PT"))

# (b) load_all --- こちらが実際のレビューでは楽です（内部が全部見えます）
pkgload::load_all("C:/Yrepo/rtfreporter-plan")
rtf_plan(df) |> plan_stub(c("SOC", "PT"))
```

同梱の `00_code.R` と `measure_*.R` はすべて (b) を前提に書いてあります。

### いま「どちら」を読んでいるか確かめる

`DESCRIPTION` の Version は両者とも同じです（ブランチは DESCRIPTION に
触れていないので、main をリベースするたび main の版数をそのまま引き継ぎます）。
判定は**関数の有無**で行ってください。

```r
exists("rtf_plan", envir = asNamespace("rtfreporter"))   # FALSE = 旧 / TRUE = 新
find.package("rtfreporter")
```

## 4. 1つのセッションで両方使いたい場合

同じ名前空間を同時にロードすることはできません。
片方ずつ実行し、生成した RTF を後で比較するのが確実です。

```r
run <- function(lib, expr) {
  callr::r(function(lib, expr) {
    library(rtfreporter, lib.loc = lib)
    eval(expr)
  }, args = list(lib = lib, expr = substitute(expr)))
}
```

`callr` を使えば、別プロセスでそれぞれ実行できます（`install.packages("callr")`）。

---

## 5. 同梱のサンプル

`00_code.R` に、6帳票ぶんの **旧コードと新コードの対** が入っています。
`01_DM_old.rtf` / `01_DM_new.rtf` のように、生成済み RTF も対で置いてあります。

| ファイル | 内容 |
|---|---|
| `01_DM` | グループ化 + group-safe ページ分割（4ページ） |
| `02_AE` | SOC / PT stub + SOC 間の空行（2ページ） |
| `03_PK` | Time / Statistic stub、VISIT を列方向（4ページ） |
| `04_LB` | 印字しない列でグループ化（1ページ） |
| `05_AE_by_SOC` | SOC ごとに1ページ、ページ名付き（4ページ） |
| `06_LISTING` | 被験者リスティング。折り返したセル、レコードを分断しないページ分割（4ページ） |

6組すべて **RTF がバイト単位で一致**しています。

以前は 01_DM / 02_AE / 03_PK の3件で、旧側だけがページ最終行の後ろに空行を
出していました（計7箇所）。main の #362 が `count_blank_rows = TRUE` の下で
ページ端の空行を数えるようになり、この差は消えています。
`measure_remaining_diff.R` で確認できます。

### 一致の確認方法

```bash
fc 01_DM_old.rtf 01_DM_new.rtf
```

R からなら

```r
identical(readLines("01_DM_old.rtf", warn = FALSE),
          readLines("01_DM_new.rtf", warn = FALSE))
```

---

## 6. 後片付け

```bash
git worktree remove C:/Yrepo/rtfreporter-main
git worktree remove C:/Yrepo/rtfreporter-plan
rm -rf C:/Yrepo/rtflibs
```

普段お使いのライブラリには一切触れていないので、これで元通りです。
