# Articles

### Gallery

Clinical tables, a listing and figures made with rtfreporter from the
CDISC pilot data, each with the code that wrote it.

- [Gallery](https://ichirio.github.io/rtfreporter/articles/gallery.md):

  Clinical tables, a listing and figures made with rtfreporter from the
  CDISC pilot data, each with the code that wrote it.

### Tables from an ARD (recommended)

From a cards / cardx analysis results dataset to RTF pages with a plan:
normalize_ard(), table_plan() and the plan\_\*() verbs, rtf_tables() and
generate_rtfreport(). Start with Get started.

- [Get started with
  rtfreporter](https://ichirio.github.io/rtfreporter/articles/rtfreporter.md):

  The recommended workflow end to end: an ARD from cards, a table plan,
  a document with running headers, and the RTF file. Then the second
  path: bringing a table you already have.

- [Tables from an
  ARD](https://ichirio.github.io/rtfreporter/articles/tables-from-ard.md):

  From a cards / cardx analysis results dataset to RTF pages:
  normalize_ard(), table_plan() and the plan\_\*() verbs, and
  rtf_tables().

- [The plan
  verbs](https://ichirio.github.io/rtfreporter/articles/plan-verbs.md):

  What each plan\_\*() verb declares, grouped by job, with the rule that
  a later layer wins.

- [From as_rtftables() to a
  plan](https://ichirio.github.io/rtfreporter/articles/plan-and-as-rtftables.md):

  Which plan verb takes over each as_rtftables() argument, and how to
  see the call a plan resolves to.

### Bring your own table

A table already built with gt, gtsummary, rtables / tern, tfrmt,
flextable, huxtable or as a data frame, read by as_rtftables() – labels,
spanning headers and footnotes included – and paginated.

- [Importing tables with
  as_rtftables()](https://ichirio.github.io/rtfreporter/articles/importing-tables.md):
- [gt, gtsummary & rtables -\> rtfreporter: building RTF
  tables](https://ichirio.github.io/rtfreporter/articles/gt-integration.md):
- [Tables from data frames: a feature
  tour](https://ichirio.github.io/rtfreporter/articles/rtfreporter-quickstart.md):
- [Paginating tables with
  as_rtftables()](https://ichirio.github.io/rtfreporter/articles/pagination.md):
- [A tfrmt table with several stub columns: one for sections, the rest
  as a
  stub](https://ichirio.github.io/rtfreporter/articles/multistub-section-split.md):
- [Same report, every framework: Demographics
  (DM)](https://ichirio.github.io/rtfreporter/articles/showcase-dm.md):
- [Same report, every framework: Adverse events
  (AE)](https://ichirio.github.io/rtfreporter/articles/showcase-ae.md):
- [From Pharmaverse Example tables to RTF
  reports](https://ichirio.github.io/rtfreporter/articles/tlg-catalog.md):

### Listings

Subject listings, written as a plan or from a listing spec.

- [Listings with a
  plan](https://ichirio.github.io/rtfreporter/articles/plan-listings.md):

  A subject listing written as a plan: plan_listing(), the page verbs
  and plan_titles(), and the same listing written as one as_rtftables()
  call.

- [Listings end to end: from source data to the written
  RTF](https://ichirio.github.io/rtfreporter/articles/listings.md):

### Figures

A plot object, or an image file, placed on a page.

- [Figures: from a plot object to a page of the
  report](https://ichirio.github.io/rtfreporter/articles/figures.md):

### Assembling a deliverable

The document around the tables, listings and figures – page setup,
running headers and footers with their tokens, sections, titles and
footnotes, borders – and joining finished files into one deliverable.

- [Page and document
  setup](https://ichirio.github.io/rtfreporter/articles/page-setup.md):
- [Headers, footers and
  tokens](https://ichirio.github.io/rtfreporter/articles/headers-footers.md):
- [Splitting a report into sections, by table
  object](https://ichirio.github.io/rtfreporter/articles/section-splitting.md):
- [Adding tables and
  figures](https://ichirio.github.io/rtfreporter/articles/adding-content.md):
- [Borders and
  rules](https://ichirio.github.io/rtfreporter/articles/borders.md):
- [Rendering, post-processing and
  assembly](https://ichirio.github.io/rtfreporter/articles/output.md):

### For contributors

How rtfreporter is built and how to extend it. Most work now happens
with a chat assistant in the loop, so that article comes first; the
architecture overview and the adapter how-to behind it are the authority
either way, and the route to follow when working by hand. Each article
opens in English with an “In Japanese” toggle at the top.

- [Developing with an AI assistant (for
  contributors)](https://ichirio.github.io/rtfreporter/articles/ai-development.md):
- [Architecture & internals (for
  contributors)](https://ichirio.github.io/rtfreporter/articles/architecture.md):
- [Adding a table-object
  adapter](https://ichirio.github.io/rtfreporter/articles/extending-adapters.md):
- [External API
  specification](https://ichirio.github.io/rtfreporter/articles/external-api.md):

### コントリビューター向け資料（日本語 / Japanese）

Japanese versions of the contributor articles, reachable from the “In
Japanese” toggle at the top of each English page.
日本語のコントリビューター向け資料です。

- [AI
  アシスタントを使った開発（コントリビューター向け）](https://ichirio.github.io/rtfreporter/articles/ai-development-ja.md):
- [アーキテクチャと内部構造（コントリビューター向け）](https://ichirio.github.io/rtfreporter/articles/architecture-ja.md):
- [テーブルオブジェクトアダプタの追加](https://ichirio.github.io/rtfreporter/articles/extending-adapters-ja.md):
- [外部インターフェース仕様書](https://ichirio.github.io/rtfreporter/articles/external-api-ja.md):

### Internals & S3 (advanced)

Design internals and an R OOP / S3 primer. Not in the navbar menu. S3
内部設計と R のオブジェクト指向／S3
の解説。ナビゲーションメニューには出しません。

- [Internal class design
  (S3)](https://ichirio.github.io/rtfreporter/articles/internal-design.md):
- [Rのオブジェクト指向とS3 ―
  rtfreporterの設計思想](https://ichirio.github.io/rtfreporter/articles/internal-design-ja.md):
