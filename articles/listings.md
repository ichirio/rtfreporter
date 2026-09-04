# Listings end to end: from source data to the written RTF

``` r

library(rtfreporter)
```

A table and a listing want different things from the same package. A
table is already a table by the time it reaches `rtfreporter`: some
framework counted, summarised and labelled it, and what is left is
rendering. A listing is different – it is one row of source data per
subject, and the work is **shape**:

- several source variables belong in one printed column, joined by `/`;
- a long cell has to break over several physical rows so it fits a
  column that is only so many characters wide;
- narrow blank columns sit between the printed ones as gutters;
- one blank row separates one subject’s block from the next; and
- **a page break must never land inside a subject’s block.**

[`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md),
[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
and
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
do the first four. The fifth is
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
which could already do it. This article runs the whole thing, source
data to `.rtf`.

## The source data

A small ADSL, one row per subject, shaped like a
baseline-characteristics listing. Note the values that are longer than
the column they will print in, and the missing ones:

``` r

adsl <- data.frame(
  USUBJID  = c("63016-204-1015", "63016-204-1023", "63016-205-100028",
               "63016-206-1034", "63016-206-1045"),
  DISPTPD  = c("COMPLETED", "COMPLETED", "DISCONTINUED",
               "ONGOING", "COMPLETED"),
  BRCA     = c("BRCA1", NA, "BRCA2", "BRCA1", NA),
  HIST     = c("ADENOCARCINOMA", "SQUAMOUS CELL CARCINOMA OF THE LUNG",
               "SMALL CELL", "HIGH GRADE SEROUS CARCINOMA", "ADENOCARCINOMA"),
  STAGE    = c("IIIB", "IV", "IIIA", "IV", "IIB"),
  HISTGRD  = c("GRADE 3", "GRADE 2", "GRADE 3", "GRADE 1", NA),
  PRRAD    = c("Y", "N", "Y", "N", "Y"),
  CMBRFST  = c("PARTIAL RESPONSE", "STABLE DISEASE", "COMPLETE RESPONSE",
               "PROGRESSIVE DISEASE", "PARTIAL RESPONSE"),
  CMBRLST  = c("STABLE DISEASE", "PROGRESSIVE DISEASE", "STABLE DISEASE",
               NA, "STABLE DISEASE"),
  ECOGPS   = c("0", "1", "1", "0", "2"),
  stringsAsFactors = FALSE
)
str(adsl)
#> 'data.frame':    5 obs. of  10 variables:
#>  $ USUBJID: chr  "63016-204-1015" "63016-204-1023" "63016-205-100028" "63016-206-1034" ...
#>  $ DISPTPD: chr  "COMPLETED" "COMPLETED" "DISCONTINUED" "ONGOING" ...
#>  $ BRCA   : chr  "BRCA1" NA "BRCA2" "BRCA1" ...
#>  $ HIST   : chr  "ADENOCARCINOMA" "SQUAMOUS CELL CARCINOMA OF THE LUNG" "SMALL CELL" "HIGH GRADE SEROUS CARCINOMA" ...
#>  $ STAGE  : chr  "IIIB" "IV" "IIIA" "IV" ...
#>  $ HISTGRD: chr  "GRADE 3" "GRADE 2" "GRADE 3" "GRADE 1" ...
#>  $ PRRAD  : chr  "Y" "N" "Y" "N" ...
#>  $ CMBRFST: chr  "PARTIAL RESPONSE" "STABLE DISEASE" "COMPLETE RESPONSE" "PROGRESSIVE DISEASE" ...
#>  $ CMBRLST: chr  "STABLE DISEASE" "PROGRESSIVE DISEASE" "STABLE DISEASE" NA ...
#>  $ ECOGPS : chr  "0" "1" "1" "0" ...
```

## Describing the columns

[`listing_col()`](https://ichirio.github.io/rtfreporter/reference/listing_col.md)
describes **one printed column**: which source variables it is built
from, how wide it may be before its text wraps, and what its header
says.

``` r

listing_col(c("DISPTPD", "BRCA", "HIST"), width = 22,
            label = "Disposition/\nAny (BRCA) Mutations/\nHistology")
#> <rtf_listing_col>
#>   name  : DISPTPD
#>   vars  : DISPTPD, BRCA, HIST
#>   width : 22
#>   label : Disposition/ / Any (BRCA) Mutations/ / Histology
```

Four things are worth reading twice.

**`vars` may name several columns.** Their values are joined with the
listing’s separator (`/` by default), and **missing and empty values are
skipped** – a subject with no `BRCA` prints `COMPLETED/SQUAMOUS...`, not
`COMPLETED//SQUAMOUS...`.

That join is
[`catx()`](https://ichirio.github.io/rtfreporter/reference/catx.md),
which the package exports for the columns you build yourself – so a
listing needs nothing outside rtfreporter:

``` r

catx("/", "COMPLETED", NA, "ADENOCARCINOMA")
#> [1] "COMPLETED/ADENOCARCINOMA"
```

**`width` is a number of characters, not a rendered width.** It decides
where the text breaks onto another physical row, and therefore how tall
the subject’s block is. What the column *measures* in the table is
`rel_width`, which defaults to `width`.

**The output column takes the first variable’s name** – `DISPTPD` above
– which is why the reshaped data below has a `DISPTPD` column holding
the joined text. It is an internal name, not a header; pass
`name = "COL01"` if you would rather read the columns positionally.

**`label` may contain a line break**, which starts another header row –
the same convention as everywhere else in rtfreporter, and what you
write is used exactly as written. Leave it out and the header is
**derived from the data**: each source column’s `label` attribute when
it has one, otherwise its name, joined with the separator and wrapped to
the column. ADaM read through haven already carries those labels, so
most listings need not write a single header by hand. `label = ""` asks
for a deliberately empty one.

Two more per-column settings matter before the widths: `layout = "flow"`
fills each line as far as `width` allows instead of breaking after every
separator – for a column like `AGE/SEX`, which would otherwise spend two
rows on four characters – and `collapse_repeats = TRUE` marks a **key**
column, printed once per record and again at the top of the next page.

[`listing_spec()`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md)
collects the columns, in the order they print, and the settings that
apply to all of them:

``` r

spec <- listing_spec(list(
  listing_col("USUBJID", width = 15, label = "Unique\nSubject ID"),
  listing_col(c("DISPTPD", "BRCA", "HIST"), width = 22,
              label = "Disposition/\nAny (BRCA) Mutations/\nHistology"),
  listing_col("STAGE", label = "Stage at\nInitial\nDiagnosis"),
  listing_col(c("HISTGRD", "PRRAD"), width = 18,
              label = "Histologic Grade/\nPrior Radiation\nTherapy"),
  listing_col(c("CMBRFST", "CMBRLST"), width = 20,
              label = "Best Response to\nthe 1st Line/\nthe Last Line"),
  listing_col("ECOGPS", label = "ECOG\nPerformance\nStatus")
))
spec
#> <rtf_listing_spec>
#>   type    : multiline
#>   columns : 6 (+ gutters)
#>     - USUBJID  <- USUBJID  [wrap 15]
#>     - DISPTPD  <- DISPTPD / BRCA / HIST  [wrap 22]
#>     - STAGE  <- STAGE
#>     - HISTGRD  <- HISTGRD / PRRAD  [wrap 18]
#>     - CMBRFST  <- CMBRFST / CMBRLST  [wrap 20]
#>     - ECOGPS  <- ECOGPS
#>   record  : .rtf_record
```

## Letting the page choose the widths

Every `width` above was picked by hand. That is the tedious part of
writing a listing – too narrow and a cell wraps into a tall ragged
block, too wide and the listing runs off the sheet – and it is not a
matter of taste: the answer follows from the paper, the margins, the
font and the data.

[`fit_listing_widths()`](https://ichirio.github.io/rtfreporter/reference/fit_listing_widths.md)
works it out. Describe the columns without widths:

``` r

bare <- listing_spec(list(
  listing_col("USUBJID", collapse_repeats = TRUE),
  listing_col(c("DISPTPD", "BRCA", "HIST")),
  listing_col("STAGE"),
  listing_col(c("HISTGRD", "PRRAD")),
  listing_col(c("CMBRFST", "CMBRLST")),
  listing_col("ECOGPS")
))

fitted <- fit_listing_widths(
  adsl, bare,
  page = rtf_page(paper_size = "A4", orientation = "landscape",
                  margin_left_in = 0.5, margin_right_in = 0.5),
  size_half_points = 16L
)
fitted
#> <rtf_listing_spec>
#>   type    : multiline
#>   columns : 6 (+ gutters)
#>     - USUBJID  <- USUBJID  [wrap 21]
#>     - DISPTPD  <- DISPTPD / BRCA / HIST  [wrap 60]
#>     - STAGE  <- STAGE  [wrap 8]
#>     - HISTGRD  <- HISTGRD / PRRAD  [wrap 12]
#>     - CMBRFST  <- CMBRFST / CMBRLST  [wrap 45]
#>     - ECOGPS  <- ECOGPS  [wrap 8]
#>   record  : .rtf_record
#>   fitted  : 154 + 5 gutter of 159 characters
#>     USUBJID  width =  21 fit (demand 15.2)
#>     DISPTPD  width =  60 fit (demand 43.4)
#>     STAGE    width =   8 fit (demand 6.0)
#>     HISTGRD  width =  12 fit (demand 9.0)
#>     CMBRFST  width =  45 fit (demand 33.2)
#>     ECOGPS   width =   8 fit (demand 6.0)
#>   Paste listing_code(spec) into your program, then tune by eye.
```

Read the budget line first. The page’s writable width, divided by the
width of one character in the listing’s font, is how many characters
there are to spend; the gutters come out of it, because they print too.

Each column’s **demand** is the 90th percentile of the display widths of
its cells – a quantile rather than the maximum, so one unusually long
value wraps instead of pushing every other column narrow – floored by
the widest token its header cannot break. That floor is why a label like
`"Stage at Initial Diagnosis"` asks for nine characters (`"Diagnosis"`)
and not twenty-six: a header wraps, so a long one should not claim a
column the data does not need.

A `width` you set yourself is never touched. It comes out of the budget
first and the rest fit around it:

``` r

pinned <- listing_spec(list(
  listing_col("USUBJID", width = 15),          # this one is a decision
  listing_col(c("DISPTPD", "BRCA", "HIST")),
  listing_col("STAGE")
))
vapply(fit_listing_widths(adsl, pinned, total_width = 60)$cols,
       function(cl) cl$width, integer(1))
#> [1] 15 37  6
```

### Paste it back into the program

The measurement is a starting point, not a verdict, so it comes back as
source:

``` r

listing_code(fitted, name = "listing")
#> listing <- listing_spec(list(
#>   listing_col("USUBJID", width = 21, rel_width = 21, collapse_repeats = TRUE,
#>     label = "USUBJID"),
#>   listing_col(c("DISPTPD", "BRCA", "HIST"), width = 60, rel_width = 60,
#>     label = "DISPTPD/\nBRCA/\nHIST"),
#>   listing_col("STAGE", width = 8, rel_width = 8,
#>     label = "STAGE"),
#>   listing_col(c("HISTGRD", "PRRAD"), width = 12, rel_width = 12,
#>     label = "HISTGRD/\nPRRAD"),
#>   listing_col(c("CMBRFST", "CMBRLST"), width = 45, rel_width = 45,
#>     label = "CMBRFST/\nCMBRLST"),
#>   listing_col("ECOGPS", width = 8, rel_width = 8,
#>     label = "ECOGPS")
#> ))
```

Paste that into the program and tune it there. A column whose header
must not break, or one you want roomy, is a judgement the program should
record – not something recomputed on every run from data that may
change. Only what differs from the listing’s defaults is written out, so
it reads like something a person wrote.

## What `build_listing()` does

``` r

body <- build_listing(adsl, spec)
body
#>           USUBJID .sp1               DISPTPD .sp2 STAGE .sp3  HISTGRD .sp4             CMBRFST .sp5
#> 1  63016-204-1015                 COMPLETED/       IIIB      GRADE 3/        PARTIAL RESPONSE/     
#> 2                                     BRCA1/                        Y           STABLE DISEASE     
#> 3                             ADENOCARCINOMA                                                       
#> 4                                                                                                  
#> 5  63016-204-1023                 COMPLETED/         IV      GRADE 2/          STABLE DISEASE/     
#> 6                              SQUAMOUS CELL                        N      PROGRESSIVE DISEASE     
#> 7                      CARCINOMA OF THE LUNG                                                       
#> 8                                                                                                  
#> 9      63016-205-              DISCONTINUED/       IIIA      GRADE 3/       COMPLETE RESPONSE/     
#> 10         100028                     BRCA2/                        Y           STABLE DISEASE     
#> 11                                SMALL CELL                                                       
#> 12                                                                                                 
#> 13 63016-206-1034                   ONGOING/         IV      GRADE 1/      PROGRESSIVE DISEASE     
#> 14                                    BRCA1/                        N                              
#> 15                         HIGH GRADE SEROUS                                                       
#> 16                                 CARCINOMA                                                       
#> 17                                                                                                 
#> 18 63016-206-1045                 COMPLETED/        IIB             Y        PARTIAL RESPONSE/     
#> 19                            ADENOCARCINOMA                                    STABLE DISEASE     
#> 20                                                                                                 
#>    ECOGPS .rtf_record
#> 1       0           1
#> 2                   1
#> 3                   1
#> 4                   1
#> 5       1           2
#> 6                   2
#> 7                   2
#> 8                   2
#> 9       1           3
#> 10                  3
#> 11                  3
#> 12                  3
#> 13      0           4
#> 14                  4
#> 15                  4
#> 16                  4
#> 17                  4
#> 18      2           5
#> 19                  5
#> 20                  5
```

Read that against the source data:

- **`USUBJID` wrapped.** `63016-205-100028` is 16 characters and the
  column takes 15, so it breaks after a hyphen –
  [`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
  breaks at word boundaries (a space, a comma, a hyphen) and takes the
  break *after* the character, so the reader can see why the line ended.
- **The joined column breaks at the separator first.** Each source
  variable starts its own line, which is what makes a listing of this
  shape readable at all. Only a piece that is *still* too long breaks
  again at a word boundary – `SQUAMOUS CELL CARCINOMA OF THE LUNG`
  becomes two lines under a 22-character column.
- **Every column of one subject is padded to the tallest**, so the block
  stays aligned across columns, and **a blank row closes each block**.
- **`.sp1` … `.sp5` are the gutters**, the narrow blank columns between
  the printed ones.
- **`.rtf_record` is bookkeeping**, not a printed column: one id per
  source row. It is what keeps a subject whole across a page break,
  below.

A token that is *still* too wide – one with nowhere to break – is
hard-split at the column width. Cutting a subject id in half is ugly,
but the alternative is worse: the column’s relative width follows
`width`, so an overflowing token is one Word wraps onto a second line,
making the record a row taller than
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
counted and the page taller than `max_rows` allowed for. Every line this
produces fits the column it was measured against.

Widths are **display widths**: a full-width (CJK) glyph occupies two
monospaced columns and counts as two, so a Japanese listing wrapped to
20 really does fit in twenty.

## Rendering it

[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
is preparation only – it hands back an ordinary `data.frame`, and
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md)
renders. The body carries its spec, so the two compose:

``` r

pages <- as_rtftables(body)
pages[[1]]
#> ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#>                    Disposition/              Stage at      Histologic Grade/     Best Response to        ECOG       
#> Unique             Any (BRCA) Mutations/     Initial       Prior Radiation       the 1st Line/           Performance
#> Subject ID         Histology                 Diagnosis     Therapy               the Last Line           Status     
#> ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> 63016-204-1015     COMPLETED/                IIIB          GRADE 3/              PARTIAL RESPONSE/       0          
#>                    BRCA1/                                  Y                     STABLE DISEASE                     
#>                    ADENOCARCINOMA                                                                                   
#>                                                                                                                     
#> 63016-204-1023     COMPLETED/                IV            GRADE 2/              STABLE DISEASE/         1          
#>                    SQUAMOUS CELL                           N                     PROGRESSIVE DISEASE                
#>                    CARCINOMA OF THE LUNG                                                                            
#>                                                                                                                     
#> 63016-205-         DISCONTINUED/             IIIA          GRADE 3/              COMPLETE RESPONSE/      1          
#> 100028             BRCA2/                                  Y                     STABLE DISEASE                     
#> … (10 more rows)
#> 
#> <rtftable> 20 rows x 11 columns
#>   Columns:    Unique Subject ID |  | Disposition/ Any (BRCA) Mutations/ Histo…
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     relative 15:1:22:1:9:1:18:1:20:1:11
```

The **header, the relative widths and the left alignment all came from
the spec.** None of the three had to be written out: no vector of header
strings with an empty entry for every gutter, no
`col_rel_width = c(15, 1, 22, 1, ...)` to keep in step with the column
list, no `col_spec` repeating `align = "left"` ten times.

Usually you do not want the intermediate object at all, and pass the
spec straight to
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md):

``` r

pages <- as_rtftables(adsl, listing = spec)
length(pages)
#> [1] 1
```

The two forms produce the same tables. Reach for
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
when you want to look at – or patch – the reshaped data before it is
rendered; reach for `listing =` the rest of the time.

## Keeping a subject whole across a page break

Give it a row budget and it paginates, **never splitting a subject’s
block**:

``` r

pages <- as_rtftables(adsl, listing = spec, max_rows = 8)
length(pages)
#> [1] 3
vapply(pages, function(p) nrow(p$data), integer(1))
#> [1] 8 4 8
pages[[2]]
#> ───────────────────────────────────────────────────────────────────────────────────────────────────────────────
#>                Disposition/              Stage at      Histologic Grade/     Best Response to       ECOG       
#> Unique         Any (BRCA) Mutations/     Initial       Prior Radiation       the 1st Line/          Performance
#> Subject ID     Histology                 Diagnosis     Therapy               the Last Line          Status     
#> ───────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> 63016-205-     DISCONTINUED/             IIIA          GRADE 3/              COMPLETE RESPONSE/     1          
#> 100028         BRCA2/                                  Y                     STABLE DISEASE                    
#>                SMALL CELL                                                                                      
#>                                                                                                                
#> 
#> <rtftable> 4 rows x 11 columns
#>   Columns:    Unique Subject ID |  | Disposition/ Any (BRCA) Mutations/ Histo…
#>   Row title:  col 1
#>   Borders:    set
#>   Widths:     relative 15:1:22:1:9:1:18:1:20:1:11
```

Nothing new implements that.
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
emitted the `.rtf_record` column, and the hook pointed the machinery
that was already there at it:

``` r

# what as_rtftables(listing = spec, max_rows = 8) sets for you
as_rtftables(body, max_rows = 8,
             group_col = ".rtf_record",   # a group is one subject's block
             group_by  = "value",         # a new value starts a new group
             split     = "group_safe",    # fill the page, never split a group
             drop_cols = ".rtf_record")   # ... and never print it
```

`drop_cols` columns are hidden **after** pagination, which is why a
column can decide the page breaks and still never appear. Every one of
those is a *default*: pass `split`, `group_col` or `drop_cols` yourself
and the hook leaves your choice alone.

## The whole report

Landscape, because a listing is wide; a title block, a footnote block,
and the file:

``` r

pages <- as_rtftables(adsl, listing = spec, max_rows = 8)

doc <- rtf_document(
  page = list(orientation = "landscape", paper_size = "A4",
              margin_top_in = 0.5, margin_bottom_in = 0.5,
              margin_left_in = 0.5, margin_right_in = 0.5),
  default_format = list(font_size_half_points = 16L)
) |>
  rtf_section(secinfo = list(
    header = rtf_header(list(
      c("Listing 16.2.4.2.1.2"),
      c("Baseline Characteristics"),
      c("<Safety Analysis Set>")
    )),
    footer = rtf_footer(list(
      c(l = "ECOG: Eastern Cooperative Oncology Group")
    ))
  )) |>
  rtf_tables(pages)

path <- file.path(tempdir(), "listing-16-2-4-2-1-2.rtf")
generate_rtfreport(doc, path, overwrite = TRUE)
file.exists(path)
#> [1] TRUE
```

That is the whole pipeline: `adsl` in, one `.rtf` out, with the column
description as the only thing written by hand.

## Listing types

`listing_spec(type = )` names a template that supplies the defaults –
separator, gutters and their width, the blank row after each record, the
default alignment, and the wrapping rule itself. One ships:

| `type` | what it means |
|----|----|
| `"multiline"` | `/` separator, gutter columns, a blank row after each record and one at the top of every page, left aligned, and text wrapped at the separator first and at word boundaries only where a piece is still too long. |

Any argument you pass explicitly overrides the template, the same
relationship `rtftable(border = "tfl")` has with its preset:

``` r

listing_spec(c("USUBJID", "STAGE"),
             sep       = " | ",   # join with something else
             spacer    = FALSE,   # no gutter columns
             blank_row = FALSE)   # no blank row between records
#> <rtf_listing_spec>
#>   type    : multiline
#>   columns : 2
#>     - USUBJID  <- USUBJID
#>     - STAGE  <- STAGE
#>   record  : .rtf_record
```

`record = FALSE` drops the hidden record column too – do that only if
you intend to paginate some other way, because it is what a page break
is told to respect.

Because the wrapping rule belongs to the template rather than to
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md),
a future type can bring its own without disturbing this one.

## The hand-written version this replaces

Before this existed, the same listing was a page of pipeline:
[`catx()`](https://ichirio.github.io/rtfreporter/reference/catx.md)
calls to join the columns, a `split_string()` that broke a cell at `/`
and then at word boundaries, a `get_max_element_counts()` to find each
record’s height, a `pad_list_elements()` to pad every column to it,
`unnest_longer()`, a manually interleaved `S00`…`S08`, a blank row bound
on top, and a `split_df_by_blank_rows()` to cut pages at record
boundaries – plus the three hand-maintained vectors (`col_rel_width`,
the header strings, the `col_spec`) that had to stay in step with all of
it.

The package’s test suite runs that pipeline – its three functions
verbatim – beside
[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
on the same data and compares the result cell for cell, so this
article’s claim is checked rather than asserted. Four differences are
deliberate:

- **A token wider than the column is hard-split.** The hand-written rule
  pushed an empty accumulator first *and* let the token overflow – so
  such a cell opened with a blank line, and Word then wrapped the token
  onto a row nobody had counted.
- **Widths are display widths.** The hand-written rule counted
  characters, so a Japanese cell asked for 8 columns and occupied 16.
- **A `"\n"` already in the data is honoured** as a line break, before
  any width is considered.
- **An empty cell still occupies its row.** The hand-written
  `split_string("")` returned no lines at all, so a record whose wrapped
  columns were all empty lost its blank row – and with it the boundary a
  page split needs.

One more difference is a matter of placement rather than behaviour: the
blank row that used to be bound on top of the body is now
`as_rtftables(blank_row_first = )`, which the `"multiline"` template
turns on, so it appears at the top of **every** page rather than only
the first.

## When not to use this

If your listing comes from **rlistings**, it is already laid out:
`disp_cols` has been applied, key-column repeat suppression is baked
into the strings, and the titles and footers travel with it. Pass it
straight to
[`as_rtftables()`](https://ichirio.github.io/rtfreporter/reference/as_rtftables.md),
which reads all of that:

``` r

lsting <- rlistings::as_listing(adsl, key_cols = "USUBJID")
as_rtftables(lsting, max_rows = 20)     # no `listing =` argument
```

[`build_listing()`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
and `as_rtftables(listing = )` refuse a `listing_df` for that reason,
with a message pointing here. (An rlistings listing is a `data.frame`
subclass, so without that guard it would quietly be treated as raw
source data.)

## See also

- [`?listing_col`](https://ichirio.github.io/rtfreporter/reference/listing_col.md),
  [`?listing_spec`](https://ichirio.github.io/rtfreporter/reference/listing_spec.md),
  [`?build_listing`](https://ichirio.github.io/rtfreporter/reference/build_listing.md)
  – every setting, one page each.
- [`?fit_listing_widths`](https://ichirio.github.io/rtfreporter/reference/fit_listing_widths.md),
  [`?listing_code`](https://ichirio.github.io/rtfreporter/reference/listing_code.md)
  – the widths, and the source they come back as.
- [Paginating tables with
  `as_rtftables()`](https://ichirio.github.io/rtfreporter/articles/pagination.md)
  – the split strategies, `group_col` / `group_by`, and `drop_cols` in
  general.
- [Importing
  tables](https://ichirio.github.io/rtfreporter/articles/importing-tables.md)
  – reading a table object, rlistings included, when the layout is
  already done.
