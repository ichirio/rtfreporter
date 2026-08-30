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

### いま「どちら」を読んでいるか確かめる

`DESCRIPTION` の Version は両者とも `0.4.74` です（ブランチは
リベース衝突を避けるため DESCRIPTION に触れていません）。
判定は**関数の有無**で行ってください。

```r
exists("rtf_plan_from")   # FALSE = 旧 / TRUE = 新
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

`00_code.R` に、5帳票ぶんの **旧コードと新コードの対** が入っています。
`01_DM_old.rtf` / `01_DM_new.rtf` のように、生成済み RTF も対で置いてあります。

| ファイル | 内容 |
|---|---|
| `01_DM` | グループ化 + group-safe ページ分割（3ページ） |
| `02_AE` | SOC / PT stub + SOC 間の空行（2ページ） |
| `03_PK` | Time / Statistic stub、VISIT を列方向（3ページ） |
| `04_LB` | 印字しない列でグループ化（1ページ） |
| `05_AE_by_SOC` | SOC ごとに1ページ、ページ名付き（4ページ） |

5組すべて **RTF がバイト単位で一致**しています。

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
