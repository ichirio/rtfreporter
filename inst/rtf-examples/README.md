# Example RTF output

These `.rtf` files are the rendered output of the article
[*From pharmaverse tables to RTF reports*](https://ichirio.github.io/rtfreporter/articles/tlg-catalog.html).

Each file is produced by taking a table object built **verbatim** by the
official [pharmaverse examples](https://pharmaverse.github.io/examples/) and
running it through rtfreporter -- the statistics and formatting are the
pharmaverse example's; rtfreporter only adds the final RTF rendering step.

| File | Source table object | pharmaverse example |
|------|--------------------|---------------------|
| `pharmaverse-demographic-tern.rtf` | tern + rtables `TableTree` | [demographic](https://pharmaverse.github.io/examples/tlg/demographic.html) |
| `pharmaverse-demographic-gtsummary.rtf` | gtsummary + cards `gt_tbl` | demographic |
| `pharmaverse-demographic-tfrmt.rtf` | tfrmt + cards `gt_tbl` | demographic |
| `pharmaverse-adverse-events-tern.rtf` | tern + rtables `TableTree` (paginated) | [adverse events](https://pharmaverse.github.io/examples/tlg/adverse_events.html) |
| `pharmaverse-adverse-events-tfrmt.rtf` | tfrmt + cards `gt_tbl` (paginated) | adverse events |
| `pharmaverse-assembled.rtf` | the gtsummary demographics + multi-page tern adverse-events table, joined with `assemble_rtf()` (with a Table of Contents) | both |

Open them in Word / LibreOffice (or batch-convert to PDF).  To regenerate,
run `Rscript data-raw/gen_tlg_catalog_rtf.R` from the repository root.

## Pagination on both axes

| File | What it shows | Article |
|------|---------------|---------|
| `pk-concentration.rtf` | a PK concentration summary with the **visits across the columns** -- 12 visits sized to their content make the table 1.73x too wide for the sheet, so it is split by column (`paginate_cols()`) as well as by row, with each visit column rendered as two cells (`set_decimal_split()`) so the decimal points line up | [Paginating tables](https://ichirio.github.io/rtfreporter/articles/pagination.html) |

Regenerate with `Rscript data-raw/gen_pk_conc.R` from the repository root.

## Screenshots

Screenshots of the first page of each `.rtf` are kept here next to the file,
with the same base name and a `.png` extension (e.g.
`pharmaverse-demographic-tern.png`).  The article copies the one it needs into
its own `figures/` folder at build time and displays it.  For the assembled
deliverable there are two: `pharmaverse-assembled-toc.png` (the Table of
Contents page) and `pharmaverse-assembled.png` (a body page).

`pk-concentration.rtf` likewise needs two, because the horizontal split is the
whole point and one page cannot show it:

| Screenshot | Page |
|------------|------|
| `pk-concentration.png` | page 1 -- the first visit block, Day 1 .. Day 56 |
| `pk-concentration-cols2.png` | page 2 -- the second block, Day 70 .. Day 196, with the same `Nominal Time (h)` stub repeated |

Both are captured by hand from Word, like the others.
