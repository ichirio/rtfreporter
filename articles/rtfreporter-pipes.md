# Pipe-Based API

## Overview

The rtfreporter package provides two complementary APIs for building RTF
reports:

1.  **S3 Methods API** (v0.0.6): Mutable object manipulation with
    `add_section()`, `add_page()`, etc.
2.  **Tidyverse Pipe API** (v0.0.6+): Immutable functional composition
    with `%>%` pipes

This vignette demonstrates the **Tidyverse Pipe API**, which is inspired
by ggplot2 and dplyr, providing a clean, composable interface for
building complex RTF reports.

## Quick Start

The basic workflow is:

1.  Initialize a document with
    [`rtf_document()`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md)
2.  Configure document settings with
    [`rtf_config()`](https://ichirio.github.io/rtfreporter/reference/rtf_config.md)
    (optional)
3.  Add content (tables/figures) with
    [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
    /
    [`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md)
    — formatting arguments (`col_rel_width`, `border`,
    `row_height_twips`, …) can be supplied here and apply to bare
    `data.frame` / file-path elements
4.  Define sections with
    [`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md)
5.  Generate RTF with
    [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md)

``` r

library(rtfreporter)

# Create a simple report
doc <- rtf_document() %>%
  rtf_tables(list(mtcars)) %>%
  rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL))

# Generate RTF file
generate_rtfreport(doc, "report.rtf")
```

## API Functions

### Initialization and Configuration

#### `rtf_document()`

Creates a new RTF document object with clinical trial defaults.

**Defaults:** - Page: Landscape letter (11” × 8.5”) - Margins: 0.75”
top/bottom, 0.5” left/right - Font: Courier - Colors: Black

``` r

doc <- rtf_document()

# With custom defaults
doc <- rtf_document(
  font_table = list(list(name = "Arial")),
  page = list(
    orientation = "portrait",
    width_in = 8.5,
    height_in = 11
  )
)
```

#### `rtf_config()`

Update document-level settings. Typically called once after creating a
document.

**Parameters:** - `font_table`: Override font table - `color_table`:
Override color table - `page`: Override page settings -
`default_format`: Override default formatting

All parameters are **optional** (NULL = no change), making it safe for
repeated calls:

``` r

doc <- rtf_document() %>%
  rtf_config(page = list(orientation = "portrait"))
```

### Content Addition

#### `rtf_tables()`

Add one or more contents as pages. **Each element of the `tables` list
becomes exactly one page**, holding a single table or figure.

Each element must be a `data.frame`, an
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
object, or an
[`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
object. Table-formatting arguments (`col_rel_width`, `border`,
`row_height_twips`, …) supplied to
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
apply **only to bare `data.frame` elements**; pre-built
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
objects keep their own settings.

``` r

# Single content per page — shared formatting applies to bare data.frames
doc <- rtf_document() %>%
  rtf_tables(
    list(df1, df2, rtftable(df3, border = "none")),
    col_rel_width    = c(2, 1, 1),
    border           = "tfl",
    row_height_twips = 280L
  )
# df1 / df2 inherit col_rel_width / border / row_height_twips.
# rtftable(df3, border="none") keeps border="none".
```

When `auto_section = TRUE` and the list is **named**, each name is
appended as an extra heading row in the header for that page, and RTF
sections are split automatically — no manual loop needed. See
[Auto-section: multi-page lab shift
table](#auto-section-multi-page-lab-shift-table) for a complete example.

``` r

# Named list + auto_section = TRUE  →  one RTF section per element
doc <- rtf_document() |>
  rtf_section(secinfo = list(header = common_hdr, footer = common_ftr)) |>
  rtf_tables(
    list("Parameter A" = tbl_a, "Parameter B" = tbl_b),
    auto_section = TRUE            # label_align = "left" (default)
  )
```

#### Titles and footnotes

Each page can carry a **title** (above the content) and a **footnote**
(below). Both are character vectors — one element per line. Empty
strings `""` produce blank lines.

- Title default (`NULL`): one centred blank line above the content.
- Title `character(0)`: title block suppressed entirely.
- Title default style: centred, bold for non-empty lines.
- Footnote: left-aligned paragraphs; the first paragraph carries a top
  border as the visual separator from the content above.

All text inherits the document font size (one font per document, 9pt by
default).

``` r

rtf_document() %>%
  rtf_tables(list(df1, df2),
    titles = list(
      c("Table 14.1.1", "", "Safety Population"),
      "Table 14.1.2"
    ),
    footnotes = list(
      c("Note 1: foo.", "", "Note 2: bar."),
      NULL
    )
  )

# Or assign separately after content has been added
doc %>%
  rtf_tables(list(df1, df2)) %>%
  rtf_titles(list("Page 1", "Page 2")) %>%
  rtf_footnotes(list(NULL, "Footer for page 2."))
```

------------------------------------------------------------------------

#### `rtf_figures()`

Add image files as figure pages. Each element becomes exactly one page.
Elements may be character file paths (PNG/JPEG) or pre-built
[`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
objects. `width_twips` / `height_twips` / `align` apply to bare paths
only.

``` r

doc <- rtf_document() %>%
  rtf_figures(
    list("plot1.png", "plot2.png", rtfplot("plot3.png", align = "left")),
    width_twips = 9000L
  )
```

### Structure Definition

#### `rtf_section()`

Map page numbers to sections with headers and footers.

``` r

# Single section starting at page 1
doc <- rtf_document() %>%
  rtf_tables(list(df1, df2, df3)) %>%
  rtf_section(page = 1, secinfo = list(
    header = rtf_header(rows = list(l = "Analysis", r = "Table 1")),
    footer = rtf_footer(rows = list(c = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"))
  ))

# Multiple sections
doc <- rtf_document() %>%
  rtf_tables(list(df1, df2, df3)) %>%
  rtf_section(
    page = c(1, 3),
    secinfo = list(
      list(header = h1, footer = f1),  # Section 1 (pages 1-2)
      list(header = h2, footer = f2)   # Section 2 (pages 3+)
    )
  )
```

**Template section (for `auto_section`):** Omit `page` to register a
common header/footer template that `rtf_tables(auto_section = TRUE)`
will use as a base.

``` r

doc <- rtf_document() |>
  rtf_section(secinfo = list(header = common_hdr, footer = common_ftr))
  # page= is omitted → stored as "_default" template
```

### Formatting

Apply formatting directly via the constructor functions:

- For tables: use
  [`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
  per-element for full control, or pass shared formatting arguments to
  [`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md).
- For figures: use
  [`rtfplot()`](https://ichirio.github.io/rtfreporter/reference/rtfplot.md)
  per-element, or pass shared arguments to
  [`rtf_figures()`](https://ichirio.github.io/rtfreporter/reference/rtf_figures.md).
- For headers/footers: use
  [`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  /
  [`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
  together with
  [`rtf_section()`](https://ichirio.github.io/rtfreporter/reference/rtf_section.md).

``` r

# Per-page control via rtftable() — full set of formatting arguments
doc <- rtf_document() %>%
  rtf_tables(list(
    rtftable(df1, border = "tfl",  row_height_twips = 280L, col_rel_width = c(2,1,1)),
    rtftable(df2, border = "none", row_height_twips = 320L)
  ))

# Shared defaults for multiple bare data.frames in one call
doc <- rtf_document() %>%
  rtf_tables(list(df1, df2, df3),
             border = "tfl", row_height_twips = 280L,
             col_rel_width = c(2, 1, 1))
```

**廃止予定**: かつて存在した `rtf_table_format()`,
`rtf_header_format()`, `rtf_footer_format()`, `rtf_figure_format()` の 4
関数は実際の出力に何も影響 しない dead-code だったため deprecate
されました。書式は上記のように
[`rtf_tables()`](https://ichirio.github.io/rtfreporter/reference/rtf_tables.md)
/
[`rtftable()`](https://ichirio.github.io/rtfreporter/reference/rtftable.md)
/
[`rtf_header()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
/
[`rtf_footer()`](https://ichirio.github.io/rtfreporter/reference/rtf_header.md)
の引数として 直接指定してください。

## Complete Example

``` r

library(rtfreporter)
library(magrittr)  # For %>% pipe

# Prepare data
df_safety <- data.frame(
  Event = c("Headache", "Nausea", "Dizziness"),
  Mild = c(5, 3, 2),
  Moderate = c(2, 1, 1),
  Severe = c(0, 0, 1)
)

df_efficacy <- data.frame(
  Response = c("Complete", "Partial", "No Response"),
  Count = c(25, 15, 10),
  Percent = c("50%", "30%", "20%")
)

# Create report
report <- rtf_document() %>%
  # Configure document (one-time)
  rtf_config(page = list(
    orientation = "landscape",
    width_in = 11,
    height_in = 8.5
  )) %>%
  # Add content with shared formatting
  rtf_tables(list(df_safety, df_efficacy),
             border = "tfl", row_height_twips = 280L) %>%
  # Define sections with headers/footers
  rtf_section(page = 1, secinfo = list(
    header = rtf_header(rows = list(
      l = "Clinical Study Report",
      r = "Safety Analysis"
    )),
    footer = rtf_footer(rows = list(
      c = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"
    ))
  )) %>%
  rtf_section(page = 2, secinfo = list(
    header = rtf_header(rows = list(
      l = "Clinical Study Report",
      r = "Efficacy Analysis"
    )),
    footer = rtf_footer(rows = list(
      c = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"
    ))
  ))

# Generate RTF file
generate_rtfreport(report, "clinical_report.rtf", overwrite = TRUE)
```

## Auto-section: Multi-page Lab Shift Table

A common clinical reporting pattern is a **shift table** repeated for
several laboratory parameters (e.g. RBC, WBC, Haemoglobin) — each on its
own page, with identical layout but a different heading line identifying
the parameter.

With the `auto_section` feature this requires **no manual loop**.

### Setup: shared table builder

``` r

library(rtfreporter)

data_dir <- system.file("extdata", package = "rtfreporter")
lab_rbc <- readRDS(file.path(data_dir, "lab_rbc.rds"))
lab_wbc <- readRDS(file.path(data_dir, "lab_wbc.rds"))
lab_hgb <- readRDS(file.path(data_dir, "lab_hgb.rds"))

# Spanning header and column-header row (identical for all parameters)
spanning_hdr <- list(
  list(from = 2L,  to = 7L,  label = "Drug A (N=30)", underline = TRUE),
  list(from = 8L,  to = 13L, label = "Drug B (N=30)", underline = TRUE),
  list(from = 14L, to = 19L, label = "Total (N=60)",  underline = TRUE)
)
col_hdr <- c("Baseline\nGrade", rep(c("0","1","2","3","4","Total"), 3L))

make_shift_tbl <- function(df) {
  rtftable(
    df,
    col_header          = col_hdr,
    spanning_header     = spanning_hdr,
    column_widths_twips = c(1200L, rep(733L, 18L)),
    col_spec = c(
      list(list(col = 1, align = "left")),
      lapply(2:19, function(j) list(col = j, align = "center"))
    ),
    row_height_twips = 360L,
    border           = "tfl",
    table_align      = "left"
  )
}
```

### Setup: common header and footer

The header has **5 fixed rows** common to all pages. Row 6 — the
parameter name — is added automatically by `auto_section`.

``` r

common_hdr <- rtf_header(rows = list(
  c(l = "Protocol: STUDY001",
    r = "DRAFT"),
  c(l = "Drug Co., Ltd.",
    r = "Page {AUTO_PAGE} of {AUTO_TOTAL_PAGES}"),
  c(c = "Table 14.x.x.x  Shift table of Toxicity Grade"),
  c(c = "<Safety Analysis Set>"),
  c(c = "")   # blank row — row 6 appended automatically per section
))

common_ftr <- rtf_footer(rows = list(
  c(l = paste0(
    "Note: Percentages are based on the number of subjects in the Safety ",
    "Analysis Set (N) for each treatment group. ",
    "Run date: ", Sys.Date()
  ))
))
```

### Build the report — the new way

``` r

doc <- rtf_document() |>
  rtf_config(page = list(
    orientation    = "landscape",
    width_in       = 11,    height_in      = 8.5,
    margin_top_in  = 0.75,  margin_bottom_in = 0.75,
    margin_left_in = 0.5,   margin_right_in  = 0.5
  )) |>
  # Register the common header/footer as the "_default" template
  # (page= is intentionally omitted)
  rtf_section(secinfo = list(header = common_hdr, footer = common_ftr)) |>
  # Named list → one RTF section per element
  # The element name is appended as a left-aligned row 6 in the header
  rtf_tables(
    list(
      "Red Blood Cell Count (10^6/uL)"   = make_shift_tbl(lab_rbc),
      "White Blood Cell Count (10^3/uL)" = make_shift_tbl(lab_wbc),
      "Hemoglobin (g/dL)"                = make_shift_tbl(lab_hgb)
    ),
    auto_section = TRUE          # section_label_align = "left" by default
  )

generate_rtfreport(doc, "lab_shift.rtf", overwrite = TRUE)
```

### Comparison: old API (manual loop)

Before `auto_section`, the same result required explicitly building each
section header and looping:

``` r

lab_params <- list(
  list(param = "Red Blood Cell Count (10^6/uL)",   data = lab_rbc),
  list(param = "White Blood Cell Count (10^3/uL)", data = lab_wbc),
  list(param = "Hemoglobin (g/dL)",                data = lab_hgb)
)

make_lab_hdr <- function(label) {
  update_header_row(
    rtf_header(rows = common_hdr_rows),
    row = 6L, content = c(l = label)
  )
}

doc <- rtf_document() |>
  rtf_config(page = list(...))

for (i in seq_along(lab_params)) {
  doc <- doc |>
    rtf_tables(list(make_shift_tbl(lab_params[[i]]$data))) |>
    rtf_section(page    = i,
                secinfo = list(header = make_lab_hdr(lab_params[[i]]$param),
                               footer = common_ftr))
}
```

`auto_section = TRUE` replaces the entire loop with the named list — the
label is taken directly from the list element name.

### Key parameters

| Parameter | Default | Description |
|----|----|----|
| `auto_section` | `FALSE` | Enable automatic section splitting for named lists |
| `section_label_align` | `"left"` | Alignment of the appended label row: `"left"`, `"center"`, or `"right"` |

> **Note:** `auto_section` only activates for elements whose name is a
> non-empty string. Unnamed elements (or elements whose name is `""`)
> fall through to the inherited section from the previous page.

## Pipe vs. Methods API

### Using Pipes (Recommended)

``` r

report <- rtf_document() %>%
  rtf_tables(list(df1, df2), border = "tfl") %>%
  rtf_section(page = 1, secinfo = sec_info)
```

### Using Methods (v0.0.6)

``` r

report <- rtfreport()
sec <- report$add_section(header = h1, footer = f1)
report$add_page(section_index = sec, content = list(rtftable(df1)))
report$add_page(section_index = sec, content = list(rtftable(df2)))
report$set_default_page(...)
```

Both APIs work and produce identical RTF output. Choose based on your
preference: - **Pipes**: Functional style, easier to compose, modern R
idiom - **Methods**: Direct object manipulation, more explicit control

## Object Structure

The S3 `rtf_document` object contains:

``` r

list(
  document = list(
    font_table = ...,
    color_table = ...,
    page = list(orientation, width_in, height_in, margins),
    default_format = ...
  ),
  contents = list(...),    # Pages: each element is rtftable | rtfplot
  sections = list(...)     # Page → section info mapping
)
```

Each function returns a modified copy, enabling immutable composition.

## See Also

- [`vignette("rtfreporter-quickstart")`](https://ichirio.github.io/rtfreporter/articles/rtfreporter-quickstart.md) -
  S3 Methods API (v0.0.6)
- [`help(rtf_document)`](https://ichirio.github.io/rtfreporter/reference/rtf_document.md) -
  Detailed function documentation
- [`generate_rtfreport()`](https://ichirio.github.io/rtfreporter/reference/generate_rtfreport.md) -
  Render document to RTF file

## Where next

- [Adding
  content](https://ichirio.github.io/rtfreporter/articles/adding-content.md)
  — titles, footnotes, figures
- [Headers and
  footers](https://ichirio.github.io/rtfreporter/articles/headers-footers.md)
  — running text and page numbers
- [Borders](https://ichirio.github.io/rtfreporter/articles/borders.md) —
  the four-edge model and the TFL preset

The four recipes (`?rtfreporter-recipes`) are the same ground covered as
runnable help-page examples: demographics, adverse events, PK and
laboratory, each data-in to RTF-out.
