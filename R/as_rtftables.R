# Internal: writable width (twips) of rtfreporter's default page -- landscape
# Letter (11in) with the default 0.75in left/right margins.  Used as the cap for
# `auto_width` so an over-wide table is scaled to fit the page by default.
.default_writable_twips <- function() as.integer((11 - 2 * 0.75) * 1440)

# Internal: flatten a (possibly multi-row, possibly spanning) col_header into a
# plain character vector of length `ncols`, where each element is the LONGEST
# label seen at that column across every header row.  Used by `auto_width` so
# that column sizing accounts for the column headers, not just the data.
#
# Accepts the same shapes the rtftable col_header may take:
#   * a character vector            -> a single header row
#   * a list of header rows, each of which is either a character vector or a
#     list of cells with `$label` and a position (`$pos`, or `$from`/`$to`).
# Spanning cells (from != to) are ignored for width purposes (they do not force
# any single column to be wide).
.flatten_col_header_labels <- function(col_header, ncols) {
  if (is.null(col_header) || ncols < 1L) return(NULL)
  rows <- if (is.character(col_header)) list(col_header) else col_header
  best <- rep("", ncols)
  bump <- function(j, lab) {
    if (!is.null(j) && !is.na(j) && j >= 1L && j <= ncols &&
        nchar(lab) > nchar(best[j])) best[j] <<- lab
  }
  for (row in rows) {
    if (is.character(row)) {
      for (j in seq_len(min(length(row), ncols))) bump(j, row[j] %||% "")
    } else if (is.list(row)) {
      for (cell in row) {
        if (!is.list(cell)) next
        lab <- as.character(cell$label %||% "")
        pos <- cell$pos
        if (is.null(pos)) {
          f <- cell$from; t <- cell$to
          if (!is.null(f) && !is.null(t) && length(f) && length(t) && f == t)
            pos <- f
        }
        bump(pos, lab)
      }
    }
  }
  best
}

# Is the "labels" metadata token enabled for a plain data.frame input?
# data.frame token resolution is deliberately LENIENT (unknown tokens are
# ignored, not an error): a `list(gt_tbl, df)` input shares one `read_meta`,
# so adapter-only tokens must not error on the data.frame elements.
.df_labels_enabled <- function(read_meta) {
  isTRUE(read_meta) || (is.character(read_meta) && "labels" %in% read_meta)
}

# Display name per data.frame column: the column's `label` attribute (the
# haven / labelled / xportr convention) when the "labels" token is enabled
# and the attribute is a usable single string, else the column name.
.df_display_names <- function(df, read_meta) {
  nms <- names(df)
  if (!.df_labels_enabled(read_meta)) return(nms)
  for (j in seq_along(df)) {
    lb <- attr(df[[j]], "label", exact = TRUE)
    if (!is.null(lb) && length(lb) == 1L && !is.na(lb) && nzchar(lb)) {
      nms[j] <- as.character(lb)
    }
  }
  nms
}

# Resolve a `read_meta` request to the concrete vector of enabled metadata
# tokens.  Shared by every table-object adapter (gt, rtables, flextable,
# huxtable), each of which wraps this with its own allowed-token set and label:
#   FALSE / NULL      -> character(0) (read no metadata)
#   TRUE              -> `allowed`    (read everything the adapter offers)
#   character vector  -> itself, after validating it is a subset of `allowed`
# `label` names the adapter in the "unknown token" error.
.resolve_meta_tokens <- function(read, allowed, label) {
  if (is.null(read) || isFALSE(read)) return(character(0))
  if (isTRUE(read))                   return(allowed)
  if (!is.character(read)) {
    stop("`read_meta` must be FALSE/TRUE or a character vector of tokens.",
         call. = FALSE)
  }
  bad <- setdiff(read, allowed)
  if (length(bad)) {
    stop(sprintf("Unknown %s `read_meta` token(s): %s.  Allowed: %s",
                 label,
                 paste(sQuote(bad),     collapse = ", "),
                 paste(sQuote(allowed), collapse = ", ")),
         call. = FALSE)
  }
  read
}

#' Convert a table object into rtfreporter table pages
#'
#' `as_rtftables()` is the single entry point for turning a *table object*
#' into a list of [rtftable()] objects -- one per RTF page -- ready to hand
#' straight to [rtf_tables()].  It unifies two jobs that used to be split
#' across `paginate()` (page splitting) and `rtf_tables(read_gt = )`
#' (metadata extraction):
#'
#' 1. **Read the table's metadata** (only the parts the RTF renderer can use;
#'    see *What is carried* below).
#' 2. **Paginate.**  The rendered body is split into per-page chunks using
#'    the same strategies the old `paginate()` offered (`split`, `max_rows`,
#'    `group_col`, blank-row controls, ...).  The shared header / width /
#'    spanning metadata is replicated onto every page.
#'
#' The page-level title / source-note blocks travel with each returned
#' rtftable as the attributes `rtf_titles` / `rtf_footnotes`, which
#' [rtf_tables()] consumes automatically.
#'
#' Supported inputs: `gt_tbl`, `gt_group` (gt's multi-table container from
#' [gt::gt_group()] / [gt::gt_split()] -- and what tfrmt's `print_to_gt()`
#' returns when the spec has a `page_plan`; expanded to one page set per
#' member table), gtsummary tables (including a `tbl_split` container from
#' `gtsummary::tbl_split_by_rows()` / `tbl_split_by_columns()`, likewise
#' expanded member by member), rtables/tern `VTableTree`
#' tables, rlistings `listing_df` listings,
#' `flextable` tables, `huxtable` tables, plain `data.frame` / tibble,
#' or a `list` of any of these (the list is flattened, names propagated as
#' `name`, `name.1`, `name.2`, ...).  Figures are out of scope -- use
#' [rtf_figures()] for those.
#'
#' @section What is carried, by source:
#'
#' The body is always the table's *rendered* body -- for gt/gtsummary via
#' `gt::extract_body()`, for rtables/tern and rlistings via
#' `formatters::matrix_form()`.
#' Only visible columns appear (hidden / helper columns such as tfrmt's
#' `..tfrmt_row_grp_lbl` are dropped), row-group / stub rows are already
#' interleaved, and indentation is rendered into the label text.  (A gt table
#' with more than one stub column -- e.g. a tfrmt
#' `row_grp_plan(label_loc = element_row_grp_loc(location = "column"))` layout --
#' is read from the table's own `_data` / `_boxhead` slots instead, as
#' `gt::extract_body()` does not support multiple stub columns; the group and
#' label columns then come through as ordinary columns.)  On top of that body the
#' following *metadata* is read:
#'
#' \tabular{lllll}{
#'   **Metadata** \tab **gt / gtsummary** \tab **rtables / tern** \tab **flextable** \tab **huxtable** \cr
#'   Column (leaf) labels        \tab yes \tab yes \tab yes \tab yes \cr
#'   Per-column alignment        \tab yes \tab yes \tab yes \tab yes \cr
#'   Spanning headers            \tab yes \tab yes \tab yes \tab yes \cr
#'   Column widths               \tab yes (px/pct) \tab -- \tab -- \tab -- \cr
#'   Title + subtitle            \tab yes \tab yes \tab yes (caption) \tab yes (caption) \cr
#'   Footnotes / source notes    \tab yes \tab yes \tab yes (footer) \tab -- \cr
#'   In-cell footnote marks      \tab yes (superscript) \tab yes (superscript) \tab -- \tab -- \cr
#'   Row-group rows + indent     \tab yes (rendered) \tab yes (rendered) \tab yes (rendered) \tab yes (rendered) \cr
#'   `tab_style()` cell styles   \tab yes (see below) \tab -- \tab -- \tab -- \cr
#' }
#'
#' **Explicit `gt::tab_style()` declarations are carried** (the `"styles"`
#' token, on by default): `cell_borders()` and the `cell_text()` bold /
#' italic / underline / align properties on column spanners, column labels
#' and body/stub cells, plus body text colour -- everything the `rtftable`
#' class can hold.  A border or underline on a column-label cell promotes
#' the label row to single-column cells (the same mechanism
#' [style_header()] uses).  The [style_header()] / [style_body()] /
#' [style_cols()] verbs still override what was read -- adapters populate
#' first, verbs win per side / per field.  *Not* carried: `cell_fill()`,
#' fonts and sizes, header text colour, and gt's **theme** borders
#' (`tab_options()`) -- those describe gt's own default look; rtfreporter's
#' `border` presets govern the frame instead.
#'
#' For flextable the *displayed* text is read (header labels set via
#' `set_header_labels()` and `colformat_*()` formatting included), not the raw
#' `$body$dataset`.  Cells composed of images / equations and `footnote()`
#' reference marks are not carried.  For huxtable the displayed text is likewise
#' read (its `number_format` applied); huxtable has no footnote concept, so only
#' the caption (a page title) is carried.
#'
#' **Not carried** (RTF cannot reproduce these, or they belong to the
#' source's own theme): cell background colours / fills, font and size
#' styling, gt theme borders (`tab_options()`), header text colour, and
#' Markdown formatting inside labels or titles.  For a plain `data.frame` / tibble two pieces of
#' metadata are read: column `label` attributes become header labels (the
#' `"labels"` token, see `read_meta`), and delimited column names are
#' reconstructed into a spanning header (see `header_sep`); everything else
#' (`col_spec`, widths, ...) you set yourself.  See also [stub_cols()] for
#' finishing a tidy hierarchy-column data.frame into the indented stub
#' layout beforehand.
#'
#' @param x A `gt_tbl`, a `gt_group`, a gtsummary table (or `tbl_split`
#'   container), an rtables/tern `VTableTree`, an rlistings `listing_df`, a
#'   `flextable`, a `huxtable`, a `data.frame` / tibble, or a `list` of these.
#' @param read_meta Controls metadata extraction from the source table:
#'   `TRUE` (default, read everything in the table above), `FALSE` (use only
#'   the rendered body -- equivalent to the old `paginate()`), or a character
#'   vector of tokens.  Tokens for
#'   gt/gtsummary: `"col_header"`, `"alignment"`, `"spanning"`, `"widths"`,
#'   `"titles"`, `"footnotes"`, `"styles"` (explicit `tab_style()` borders
#'   and text styles; see *What is carried*).  For rtables/tern: `"col_header"`,
#'   `"alignment"`, `"spanning"`, `"titles"`, `"footnotes"`, `"indent"`,
#'   `"footnote_marks"`; **rlistings takes the same tokens**, because the two
#'   share one `MatrixPrintForm` reader.  For flextable: `"col_header"`, `"alignment"`,
#'   `"spanning"`, `"titles"`, `"footnotes"`.  For huxtable: `"col_header"`,
#'   `"alignment"`, `"spanning"`, `"titles"`.  For a plain data.frame /
#'   tibble the single token is `"labels"`: a column's `label` attribute (the
#'   haven / labelled / xportr convention) is used as its header label
#'   (columns without one keep their name).  Unknown tokens are *ignored* for
#'   data.frames -- a `list()` mixing table objects and data.frames shares one
#'   `read_meta`, so adapter tokens must not error on the data.frame elements.
#' @param split How to break the body into pages. A strategy name:
#'   \describe{
#'     \item{`"none"`}{(default) one page; no row limit checked.}
#'     \item{`"rows"`}{fixed chunk size; requires `split_rows`.}
#'     \item{`"group_safe"`}{fill up to `max_rows` but never split a group
#'       (defined by `group_col`) across a page; requires `max_rows`.}
#'     \item{`"group_force"`}{like `"group_safe"`, but a single group larger than
#'       `max_rows` may span pages with a continuation label; requires `max_rows`.}
#'     \item{`"by_value"`}{one page per distinct value of `group_col`; the pages
#'       are named by that value.}
#'   }
#'   `split` may also be a **custom function** for bespoke page-break rules.
#'   It is called as
#'   `split(df, max_rows = , group_col = , group_by = , cont_label = , min_group_rows = )`
#'   on the (cell-formatted) body and must return a **non-empty list of
#'   data.frames** -- one per page.  Named list elements become page names (as
#'   with `"by_value"`).  Your function implements only the split; the shared
#'   pipeline (blank rows, metadata, per-page assembly, and header / width /
#'   style replication) is applied to its output unchanged.  Write the function
#'   with a `...` so it tolerates the context arguments it does not use, and see
#'   [add_cont_label()] for re-creating the `" (Cont.)"` continuation row.
#' @param max_rows Integer or `NULL`.  Maximum body rows per page for the
#'   `"group_safe"` / `"group_force"` splits (required by them).  Ignored by
#'   `"none"`, `"rows"` (which uses `split_rows`) and `"by_value"`.
#' @param split_rows Integer or `NULL`.  Rows per page for `split = "rows"`
#'   (required by it; ignored otherwise).
#' @param group_col Character, integer, or `NULL`.  The column the group-aware
#'   splits (`"group_safe"`, `"group_force"`, `"by_value"`) detect groups on,
#'   given by name or position.  `NULL` (default) uses **column 1**.  This
#'   selects only the *column*; how a group boundary is found on it is set by
#'   `group_by`.  Note that for gt / gtsummary the body keeps gt's column
#'   **ids** (e.g. `"label"`, `"stat_1"`), and for rtables / flextable /
#'   huxtable the columns are renamed `V1`, `V2`, ... -- so an **integer** index
#'   is the most portable.  A **factor** group column is rendered as its labels:
#'   it is coerced to character just before the split, so the group-aware splits
#'   can write the `cont_label` suffix into it.  This happens *after* `sort_by`,
#'   which still orders a factor by its **levels**.
#' @param group_by How a group boundary is found on `group_col`:
#'   \describe{
#'     \item{`"auto"`}{(default) pick from the column content: leading
#'       indentation present -> `"indent"`; else interspersed empty cells ->
#'       `"filled"`; else -> `"value"`.}
#'     \item{`"indent"`}{a row starts a group when its `group_col` cell is
#'       non-empty and does **not** begin with whitespace (space / tab /
#'       non-breaking space); indented or empty cells are members. The typical
#'       clinical row-label layout (gt / tfrmt bake indentation as NBSP).}
#'     \item{`"value"`}{each maximal run of rows sharing the same `group_col`
#'       value is one group.}
#'     \item{`"filled"`}{a row starts a group when its `group_col` cell is
#'       non-empty; only `NA` / `""` cells are members (the label appears once,
#'       on the group's first row).}
#'   }
#' @param sort_by Columns to **order the body rows by, before pagination**, or
#'   `NULL` (default, keep the input order).  A character / integer vector (or a
#'   `list()` to mix names and indices) in the **input body's** coordinates --
#'   the same space as `group_col`, `collapse_repeats` and `drop_cols`.  The sort
#'   is applied *first*, so group detection, `(Cont.)` labels and `blank_rows`
#'   positions all see the sorted order.  A sort key may also be listed in
#'   `drop_cols` to order on a column that is not printed (the classic hidden
#'   "sort-key" carrier column).  The sort is **stable** (rows that compare equal
#'   keep their input order) and `NA` keys sort **last** regardless of
#'   `sort_desc`.  Note that `sort_by` reorders rendered body rows as-is, so it
#'   suits flat / tabular bodies (a `data.frame`, or a pre-flattened table); on a
#'   gt / rtables body whose group-label and child rows are already interleaved
#'   it would scramble that visual hierarchy -- sort the source data before
#'   building such a table instead.
#' @param sort_desc Sort direction for `sort_by`: `NULL`/`FALSE` (default, all
#'   ascending), a single `TRUE` (all `sort_by` columns descending), or a logical
#'   vector the same length as `sort_by` (one direction per key).  Ignored when
#'   `sort_by` is `NULL`.
#' @param cont_label Character (default `" (Cont.)"`).  Suffix appended to a
#'   group's label on the second and later pages it continues onto (the
#'   group-aware splits), marking a continued group.
#' @param blank_rows Where to insert blank separator rows in the body.  `NULL`
#'   (default) inserts none (but an `rtf_blank_rows` attribute already on the
#'   input is still honoured).  Accepts any of -- or a `list()` combining:
#'   \describe{
#'     \item{an integer vector of positions}{a blank row is inserted *after* each
#'       given data-row index; `0` = before the first row, `-1` = after the last
#'       row (e.g. `c(0, 5, -1)`).}
#'     \item{`"between_groups"`}{a blank at every group transition on `group_col`,
#'       using this call's `group_by` detection (auto / indent / value / filled).}
#'     \item{a [blank_rows_by_change()] object}{insert a blank whenever the group
#'       of one or more columns changes -- the spec carries its own `group_by`.}
#'     \item{a [blank_rows_by_rule()] object}{insert a blank before / after rows
#'       whose column matches a regular expression.}
#'   }
#'   For example
#'   `blank_rows = list(c(-1), blank_rows_by_change("Visit"))` adds a trailing
#'   blank and a blank at every change of `Visit`.  By default these blanks are
#'   added *after* the split and do not count toward `max_rows`; set
#'   `count_blank_rows = TRUE` to make `max_rows` mean the rows the page
#'   actually prints.
#' @param blank_row_first,blank_row_end Logical (default `FALSE`).  Add a single
#'   blank row at the very top (`blank_row_first`) or bottom (`blank_row_end`)
#'   of **every** page, as page furniture.  They are applied after the split,
#'   and whether they count toward `max_rows` is `count_blank_rows`: under
#'   `TRUE` they do, so `max_rows` is the number of rows the page prints;
#'   under `FALSE` (the default) they do not, and the printed page can be up
#'   to two rows taller than `max_rows`.
#' @param align_count_pct Logical (default `FALSE`).  Shorthand to realign
#'   `"n (xx.x)"` count/percent cells to a uniform width before pagination (the
#'   built-in [realign_count_pct()]).  Ignored when `cell_format` is supplied,
#'   which takes precedence.  Neither one controls whether missing values are
#'   substituted -- that is `na` -- only whether the substituted token is
#'   aligned with the counts.
#' @param min_group_rows Integer (default `2`).  Widow/orphan control for the
#'   group-aware splits (`"group_force"`, `"group_safe"`, `"by_value"`): when a
#'   page would end on a group that *starts* on that page while showing fewer
#'   than `min_group_rows` of the group's child rows, the whole group is moved
#'   to the next page.  This prevents a lone group header being stranded at the
#'   foot of a page with none (or too few) of its members.  Set to `0` to
#'   disable (the previous behaviour).
#' @param count_blank_rows Logical (default `FALSE`).  What `max_rows` counts.
#'   \describe{
#'     \item{`TRUE`}{`max_rows` is the number of rows the page **prints**:
#'       body rows, blank rows already in the data, the blanks `blank_rows`
#'       inserts between groups, **and the `blank_row_first` /
#'       `blank_row_end` page edges**.  A page never overflows the budget.
#'       The edges are counted as they actually render: a page-edge blank
#'       merges with a blank row it sits against (see `blank_row_normalize`),
#'       so it costs a row only where there is no blank row there already --
#'       which is why a listing, whose every record block ends in a blank
#'       row, does not lose a row at the foot of each page.}
#'     \item{`FALSE`}{(default) `max_rows` counts only the rows that were in
#'       the input.  Nothing inserted counts, at the page edges or between
#'       groups, so the printed page can be taller than `max_rows`.}
#'   }
#'   Under `TRUE` the blank positions resolved from `blank_rows` (and from any
#'   `rtf_blank_rows` attribute already on the input) are materialised before
#'   the split and re-attached per page afterwards, with a leading blank
#'   suppressed at the top of each page.
#' @param cell_format Optional cell re-formatter applied column-by-column to
#'   the body **before** pagination, for monospaced alignment.  Either a single
#'   function -- applied to every data column (columns 2..N; the row-label
#'   column 1 is left alone) -- or a list of functions taken positionally
#'   (`cell_format[[j]]` for column `j`; non-function entries are skipped).
#'   Each function takes one column (a character vector) and returns a
#'   character vector of the same length; see [fmt_count_paren()] /
#'   [fmt_right_align()] for built-ins and the contract for writing your own.
#'   When supplied it takes precedence over `align_count_pct`.
#' @param na Text to print for a **missing value** (default `""`, an empty
#'   cell -- the previous behaviour).  Applied to every column, the row-label
#'   column included, and **independently of `align_count_pct` / `cell_format`**
#'   -- those decide whether the substituted token is also *aligned*, not
#'   whether the substitution happens.  With one of them set, the token is
#'   right-justified in the count field so its right edge lands on the ones
#'   digit:
#'   ```
#'     1 ( 1.2%)
#'   108 (35.3%)
#'     -            <- na = "-"
#'    NE            <- na = "NE", right edge still under the ones digit
#'   ```
#'   Text already in the data that is *not* missing (`"NE"`, `"n/a"`, a literal
#'   `"-"` you typed yourself) does not match the count pattern and passes
#'   through unchanged, with no padding.
#'
#'   `NaN` counts as missing (R's own `is.na()` says so, and in a TFL a `NaN`
#'   is a `0/0` percentage).  `Inf` / `-Inf` do **not**: they print as `"Inf"` /
#'   `"-Inf"`, because an infinity means a division by zero upstream and
#'   rendering it as `"-"` would hide the bug.  The *strings* `"NA"` / `"NaN"`
#'   are never touched -- `"NA"` can be legitimate data.
#'
#'   The substitution happens before the split, so the `NA`s `collapse_repeats`
#'   writes per page to blank out repeated values stay blank.
#' @param collapse_repeats Columns in which to blank **consecutive repeated
#'   values** (repeat suppression), or `NULL` (default, off).  A character /
#'   integer vector naming the columns in priority order.  Within each column,
#'   only the first value of a run is kept; the rest of the run is replaced with
#'   `NA` (which renders as an empty cell -- no row is removed, only the display
#'   text is suppressed).  When several columns are given the suppression is
#'   **hierarchical**: the first column is collapsed on its own value, and each
#'   later column on the *combination* of itself with all earlier listed columns
#'   (a change in any higher column restarts the lower column's run).  This runs
#'   **per page, after the split**, so the pagination still sees the original
#'   repeated values -- group boundaries and `(Cont.)` labels stay correct, and a
#'   group continued onto the next page shows its label again at the top.  (In
#'   `group_by` terms: value-based grouping happens first, then the column is
#'   collapsed to a `"filled"`-style display.)
#' @param drop_cols Columns to **hide from the printed table while still using
#'   them for pagination / grouping**, or `NULL` (default, drop nothing).  A
#'   character / integer vector (or a `list()` to mix names and indices) naming
#'   the columns in the **input body's** coordinates -- the same coordinate
#'   space as `group_col` and `collapse_repeats`.  The named columns stay
#'   present through the split (so
#'   `group_col`, `collapse_repeats` and [blank_rows_by_change()] can reference
#'   them), then are removed from **every page** before that page's table is
#'   rendered.  This makes a column usable as a hidden grouping / sort-key /
#'   carrier column without it appearing in the report.  Position-indexed
#'   metadata (`col_spec`, column widths, `col_header_align`, `row_title`, and
#'   per-cell `cell_styles`) is reindexed automatically to the remaining
#'   columns.  A **user-supplied `col_header`** is the exception: it is applied
#'   against the **final printed columns** (see the `col_header` argument and
#'   [set_col_header()]), so you write its positions / names for the columns
#'   that remain -- not the pre-drop layout.  `drop_cols` must leave at least
#'   one column to display.  Note that for `split = "by_value"` the page
#'   **names** come from the `group_col` value, so that column may be dropped and
#'   the pages are still named by it; but under `"group_safe"` / `"group_force"`
#'   the `" (Cont.)"` marker is written into the `group_col` cell, so if
#'   `group_col` is itself dropped the marker is not shown (group on the visible
#'   label column if the marker is wanted).
#' @param listing Listing settings, as a [listing_spec()] object, or `NULL`
#'   (default, no listing preparation).  Applies to a **data.frame / tibble**
#'   source: [build_listing()] reshapes it into a listing body -- source
#'   variables joined into their printed columns, long cells wrapped over
#'   several physical rows, gutter columns, a blank row after each record --
#'   and the spec's columns then supply the `col_header`, the relative widths
#'   and the (left) alignment, so none of the three has to be written out by
#'   hand.  The hidden record column is used to keep a record whole across a
#'   page break: `group_col` points at it, `split` becomes `"group_safe"` when
#'   `max_rows` is set, and it is added to `drop_cols`.  Every one of those is
#'   a **default** -- an argument you pass yourself is never overridden.  A
#'   body that has already been through [build_listing()] carries its own spec,
#'   so `as_rtftables()` picks it up and passing `listing` as well is an error.
#'   An rlistings `listing_df` is rejected: it is already laid out, and needs
#'   no `listing` argument.
#' @param stub Row-stub settings, as a [stub_spec()] object -- or, as a
#'   shorthand for `stub_spec(vars)`, a bare vector of hierarchy columns
#'   (parent first, leaf last; at least two).  `NULL` (default) builds no
#'   stub.  This is the **one** argument for everything [stub_cols()] can do,
#'   including `layout` and `label_span`, which the superseded `stub_vars`
#'   family cannot reach.  Passing both `stub` and any of the `stub_vars`
#'   family is an error.
#'
#'   The stub is applied to the **extracted body** -- after the table is pulled
#'   out of its source, before pagination -- so any input that carries the
#'   hierarchy as *separate columns* (a plain `data.frame`, a `gt(df)`, a
#'   multi-group-column table, or a tfrmt `row_grp_plan(location = "column")`
#'   table) gains a clinical stub.  With `layout = "merged"` the named columns
#'   are consumed and replaced by one stub column at position 1 and the
#'   remaining columns keep their order; with `layout = "columns"` every column
#'   stays where it is.  Either way `drop_cols` / `group_col` / `sort_by` (plus
#'   `group_by = "indent"` detection and the group-aware splits) then refer to
#'   the reshaped body.  **Exception: with `split = "by_value"` the body is
#'   split by `group_col` *first* and the stub is built per page afterwards** --
#'   each value becomes an independent section, so `group_col` here refers to
#'   the **pre-stub** columns and is not folded into the stub.  This keeps a
#'   fixed intermediate hierarchy level (e.g. `LBTOX_LBL / group1 / label` with
#'   a constant `group1`) from collapsing into a single stub label row that
#'   spans every group; put the inner levels in the stub and the outer level in
#'   `group_col`.  Position-indexed metadata (`col_spec`, `col_header_align`,
#'   per-cell `cell_styles`) is reindexed automatically; a user-supplied
#'   `col_header` is instead resolved against the final columns (see
#'   [set_col_header()]).  Sources that already render an indented stub
#'   (rtables / tern, tfrmt indented, gtsummary) come out pre-merged, so a stub
#'   does not apply to them.  Under `layout = "merged"` the source column
#'   widths are **not** carried through the merge -- use `auto_width = TRUE` or
#'   pass explicit widths; `layout = "columns"` keeps them.
#' @param stub_vars **Superseded** by `stub`; still supported and not
#'   deprecated, but it cannot reach `layout` or `label_span`, and new stub
#'   settings are added to [stub_spec()] only.
#'   Hierarchy columns to merge into a single **indented stub**
#'   column (parent first, leaf last; at least two), or `NULL` (default, no
#'   stub built).  Applies [stub_cols()] to the **extracted body** -- after the
#'   table is pulled out of its source, before pagination -- so any input that
#'   carries the hierarchy as *separate columns* (a plain `data.frame`, a
#'   `gt(df)`, a multi-group-column table, or a tfrmt
#'   `row_grp_plan(location = "column")` table) gains a clinical stub.  The
#'   named columns are consumed and replaced by one stub column at position 1;
#'   the remaining columns keep their order, and `drop_cols` / `group_col` /
#'   `sort_by` (plus `group_by = "indent"` detection and the group-aware
#'   splits) then refer to the reshaped, **post-stub** columns.  **Exception:
#'   with `split = "by_value"` the body is split by `group_col` *first* and the
#'   stub is built per page afterwards** -- each value becomes an independent
#'   section, so `group_col` here refers to the **pre-stub** columns and is not
#'   folded into the stub.  This keeps a fixed intermediate hierarchy level
#'   (e.g. `LBTOX_LBL / group1 / label` with a constant `group1`) from
#'   collapsing into a single stub label row that spans every group; put the
#'   inner levels in `stub_vars` and the outer level in `group_col`.  Position-indexed
#'   metadata (`col_spec`, `col_header_align`, per-cell `cell_styles`) is
#'   reindexed automatically; a user-supplied `col_header` is instead resolved
#'   against the final columns (which already include the stub at position 1 --
#'   see [set_col_header()]).  Sources that already
#'   render an indented stub (rtables / tern, tfrmt indented, gtsummary) come
#'   out pre-merged, so `stub_vars` does not apply to them.  Column widths from
#'   the source are **not** carried through the merge -- use `auto_width = TRUE`
#'   or pass explicit widths.
#' @param stub_label,stub_indent,stub_group_summary **Superseded** by `stub`
#'   (see [stub_spec()]).  Forwarded to [stub_cols()]
#'   when `stub_vars` is set: the merged stub column's name / header
#'   (`NULL` joins the merged columns' display names with `" / "`), the
#'   non-breaking spaces per nesting level (default `4`), and which leaf values
#'   fold a row onto its group label row (default `c("empty", "parent")`).
#'   Ignored when `stub_vars` is `NULL`.
#' @param header_sep Separator(s) used to reconstruct a **spanning (multi-row)
#'   column header** from a plain **data.frame**'s column names.  A bare
#'   data.frame carries no spanning metadata, so the nesting is parsed out of the
#'   names: each name is split on `header_sep` into segments that become stacked
#'   header rows, and horizontally adjacent columns sharing a label *and* the
#'   same ancestor path are merged into one spanning cell.  Columns with no
#'   separator (e.g. id columns such as `label`) keep their name on the bottom
#'   (leaf) row with blank cells above.  The default recognizes **both** built-in
#'   delimiters: `"____"` (from `ydisctools::pivot_stats_wider()`) and
#'   `"___tlang_delim___"` (tfrmt's column delimiter).  Pass your own
#'   separator(s) to override, or `NULL` to disable (use the plain `names(data)`
#'   single-row header).  Only applies to plain data.frame input -- gt /
#'   gtsummary / rtables / tern / flextable / huxtable already carry real
#'   spanning metadata.  With the `"____"` separator a doubled separator
#'   (`"________"`) yields an empty middle segment, i.e. a **blank cell** at that
#'   header level (handy to align a column that skips an intermediate spanner
#'   level).  An explicit `col_header` (passed via `...`) always wins.
#' @param auto_width Logical (default `FALSE`).  When `TRUE`, each column is
#'   sized to its widest content (column header label or data cell) via
#'   [auto_col_widths()], so long row labels and column headers do not wrap.
#'   The widths are computed once on the full table and applied to every page,
#'   keeping paginated pages aligned.  Ignored if you pass an explicit
#'   `column_widths_twips` or `col_rel_width`.
#' @param table_width_twips Optional total table width in twips, used only when
#'   `auto_width = TRUE`.  When supplied, the auto-sized columns are scaled so
#'   their widths sum to this value (e.g. to fill, or fit within, the writable
#'   page width).  `NULL` (default) uses each column's natural content width,
#'   but **capped at the default page's writable width** (landscape Letter,
#'   0.75in margins) so a naturally over-wide table is scaled down to fit the
#'   page without you having to compute the width.
#' @param border,style Passed to [rtftable()] for every page.  `border`
#'   defaults to `"tfl"`.
#' @param ... Further arguments forwarded to [rtftable()] for every page
#'   (e.g. `col_header`, `col_spec`, `row_title`, `col_rel_width`,
#'   `row_height_twips`).  `row_title` names the row-heading columns (default:
#'   column 1) and sets the per-column default alignment (heading columns left,
#'   others centre).  Explicit values always win over the gt-extracted ones.
#'
#'   A user-supplied `col_header` is resolved against the **final printed
#'   columns** -- after `stub_vars` folding and `drop_cols` removal -- so its
#'   [col_cell()] positions and names refer to the columns you actually see
#'   (position 1 = first printed column; the `stub_vars` stub is that column).
#'   This is equivalent to building the pages and then calling
#'   [set_col_header()]; use [rtf_columns()] to list the final names first.
#'   (`col_spec` / `col_rel_width` / `row_title` still use the pre-drop input
#'   coordinates.)
#'
#' @section Superseded arguments:
#' These `...` arguments still work and are handy for simple one-shot tables,
#' but dedicated post-hoc verbs supersede them for anything beyond a single
#' label row -- they address the **final** printed columns (by name or visible
#' position), are inspectable, and compose in a pipe:
#'
#' \itemize{
#'   \item `col_header` -> [set_col_header()] (multi-row / spanning headers,
#'     name-based; [rtf_columns()] lists the columns and [rtf_header_source()]
#'     shows the current header as editable source).
#'   \item column text styling / added header rows -> [style_cols()],
#'     [style_header()], [add_header_row()].
#' }
#'
#' Prefer the verbs for complex headers; the arguments are kept for
#' convenience and backward compatibility.
#'
#' @return A list of `rtftable` objects, one per page.  When the split is
#'   value-based (or the input was a named list) the list is named.
#'
#' @seealso [set_col_header()] / [rtf_columns()] / [rtf_header_source()] and the
#'   styling verbs [style_cols()] / [style_header()] / [add_header_row()] (which
#'   supersede the header / styling `...` arguments); [as_rtftable()] for the
#'   single-page convenience wrapper; [rtf_tables()] to append the result to a
#'   document.
#'
#' @examples
#' # A plain data.frame: one page, no splitting.
#' df <- data.frame(
#'   Parameter = c("Age", "  Mean", "  SD", "Sex", "  F", "  M"),
#'   Value     = c("", "75.1", "8.2", "", "53%", "47%"),
#'   stringsAsFactors = FALSE
#' )
#' pages <- as_rtftables(df)               # length-1 list of rtftable
#' length(pages)
#'
#' # Fixed-size pagination: 3 body rows per page.
#' pages <- as_rtftables(df, split = "rows", split_rows = 3)
#' length(pages)                           # 2 pages
#'
#' # Blank separator rows: after row 3 and after the last row.
#' pages <- as_rtftables(df, blank_rows = c(3, -1))
#' pages[[1]]$blank_rows
#'
#' \dontrun{
#' library(gtsummary)
#' tbl <- trial |>
#'   tbl_summary(by = trt) |>
#'   as_rtftables()                       # list of rtftable pages
#'
#' doc <- rtf_document() |>
#'   rtf_section(page = 1, secinfo = list(header = NULL, footer = NULL)) |>
#'   rtf_tables(tbl)                       # titles / footnotes flow through
#' generate_rtfreport(doc, "out.rtf", overwrite = TRUE)
#' }
#'
#' @export
as_rtftables <- function(x,
                         read_meta       = TRUE,
                         max_rows        = NULL,
                         split           = c("none", "rows", "group_safe",
                                             "group_force", "by_value"),
                         split_rows      = NULL,
                         group_col       = NULL,
                         group_by        = c("auto", "indent", "value",
                                             "filled"),
                         sort_by         = NULL,
                         sort_desc       = NULL,
                         cont_label      = " (Cont.)",
                         min_group_rows  = 2L,
                         blank_rows      = NULL,
                         blank_row_first = FALSE,
                         blank_row_end   = FALSE,
                         count_blank_rows = FALSE,
                         align_count_pct = FALSE,
                         cell_format     = NULL,
                         na              = "",
                         collapse_repeats = NULL,
                         drop_cols       = NULL,
                         listing         = NULL,
                         stub            = NULL,
                         stub_vars       = NULL,
                         stub_label      = NULL,
                         stub_indent     = 4L,
                         stub_group_summary = c("empty", "parent"),
                         header_sep      = .default_header_seps(),
                         auto_width        = FALSE,
                         table_width_twips = NULL,
                         border          = "tfl",
                         style           = NULL,
                         ...) {
  # `split` is a built-in strategy name OR a custom pagination function.
  #
  # It used to also accept a configured page_split_*() spec, and because such a
  # spec carried its own `group_col`, the group column could be declared in two
  # places -- which is what made #328 possible.  #334 retired the factories, so
  # every setting now has exactly one declaration site and the machinery that
  # reconciled the two is gone with them.
  # Whether the caller SET these decides whether a `listing` may fill them in
  # below: the listing hook supplies pagination defaults, but never overrides a
  # decision the caller made.
  split_given           <- !missing(split)
  group_by_given        <- !missing(group_by)
  blank_row_first_given <- !missing(blank_row_first)

  if (!is.function(split)) split <- match.arg(split)
  group_by <- match.arg(group_by)
  na <- .check_na_text(na)
  user_args <- list(...)

  # One spec from `stub =` or the superseded flat family (#314).  `stub_spec`
  # is the only place new stub settings are added, so this signature stops
  # growing with stub_cols().
  stub_spec_obj <- .resolve_stub_spec(
    stub, stub_vars, stub_label, stub_indent, stub_group_summary,
    c(!is.null(stub_vars), !is.null(stub_label),
      !missing(stub_indent), !missing(stub_group_summary)))

  # ---- gt_group input: expand to its member gt_tbl list -----------------
  # A gt_group (gt::gt_group() / gt::gt_split(); also what tfrmt's
  # print_to_gt() returns under a page_plan) is itself a plain S3 list of
  # internal slots, so it must be expanded BEFORE the list-recursion guard
  # below -- otherwise the guard would iterate the slots, not the tables.
  if (.is_gt_group(x)) x <- .gt_group_tables(x)

  # ---- tbl_split input: drop the container class ------------------------
  # A gtsummary tbl_split (tbl_split_by_rows() / tbl_split_by_columns()) IS
  # a list of member gtsummary tables; unwrap it explicitly so the support
  # is by design, not an accident of the list branch below.
  if (.is_gtsummary_split(x)) x <- .gtsummary_split_tables(x)

  # ---- list input: recurse, concatenate, propagate names ----------------
  if (is.list(x) && !is.data.frame(x) && !isS4(x) &&
      !.is_gt_tbl(x) && !.is_gtsummary_tbl(x) && !.is_rtables_tbl(x) &&
      !.is_flextable_tbl(x)) {
    if (length(x) == 0L) return(list())
    in_names <- names(x)
    out <- list()
    for (i in seq_along(x)) {
      chunks <- as_rtftables(
        x[[i]], read_meta = read_meta, max_rows = max_rows, split = split,
        split_rows = split_rows, group_col = group_col, group_by = group_by,
        sort_by = sort_by, sort_desc = sort_desc,
        cont_label = cont_label, min_group_rows = min_group_rows,
        blank_rows = blank_rows, blank_row_first = blank_row_first,
        blank_row_end = blank_row_end, count_blank_rows = count_blank_rows,
        align_count_pct = align_count_pct,
        cell_format = cell_format, na = na,
        collapse_repeats = collapse_repeats,
        drop_cols = drop_cols, listing = listing,
        # The flat family has already been folded into the spec, so only the
        # spec is forwarded -- passing both would trip its own guard.
        stub = stub_spec_obj,
        header_sep = header_sep,
        auto_width = auto_width, table_width_twips = table_width_twips,
        border = border, style = style, ...)
      if (!is.null(in_names) && nzchar(in_names[i])) {
        base <- in_names[i]
        if (length(chunks) == 1L) {
          names(chunks) <- base
        } else if (is.null(names(chunks)) ||
                   all(!nzchar(names(chunks) %||% ""))) {
          names(chunks) <- paste0(base, ".", seq_along(chunks))
        } else {
          names(chunks) <- paste0(base, ".", names(chunks))
        }
      }
      out <- c(out, chunks)
    }
    return(out)
  }

  # ---- listing preparation (#241) ---------------------------------------
  # Reshape source data into a listing body BEFORE anything else looks at it,
  # so the rest of the pipeline sees an ordinary data.frame and needs to know
  # nothing about listings.  Two things then come from the spec:
  #
  #   * the header, the relative widths and the (left) alignment, which are
  #     put where an adapter would put them -- `kw`, in the data.frame branch
  #     below -- so an explicit `col_header` / `col_rel_width` in `...` still
  #     wins, exactly as it does over a gt or rtables source; and
  #   * the pagination that keeps a record whole: `group_col` on the hidden
  #     record column, a `"group_safe"` split, and the column named in
  #     `drop_cols` so it is hidden AFTER pagination has used it.
  #
  # Every one of those is a DEFAULT: an argument the caller actually passed is
  # left alone.
  listing_res  <- .resolve_listing_arg(listing, x)
  listing_meta <- NULL
  if (!is.null(listing_res)) {
    if (.is_rlistings_tbl(x)) {
      stop("`listing` does not apply to an rlistings listing (`listing_df`): ",
           "rlistings has already laid it out.  Drop `listing` and pass it to ",
           "`as_rtftables()` as it is.", call. = FALSE)
    }
    if (!is.data.frame(x)) {
      stop("`listing` applies to a data.frame or tibble source; got '",
           paste(class(x), collapse = "/"), "'.", call. = FALSE)
    }
    lspec <- listing_res$spec
    if (isTRUE(listing_res$build)) {
      x <- build_listing(x, lspec)
      # build_listing() resolves the headers against the data's own `label`
      # attributes and hands the resolved spec back on the body (#366).
      lspec <- attr(x, "rtf_listing", exact = TRUE)
    }
    listing_meta <- .listing_metadata(lspec, x)

    # A column marked `collapse_repeats` carries its value down the record's
    # rows; blanking the repeats is as_rtftables()'s own job, per page, so a
    # run continued across a break still shows its value at the top.
    keys <- vapply(lspec$cols, function(cl) isTRUE(cl$collapse_repeats),
                   logical(1L))
    if (any(keys) && is.null(collapse_repeats)) {
      collapse_repeats <- vapply(lspec$cols[keys], function(cl) cl$name,
                                 character(1L))
    }

    if (!blank_row_first_given && isTRUE(lspec$blank_row_first)) {
      blank_row_first <- TRUE
    }
    rec <- lspec$record_col
    if (!is.null(rec) && rec %in% names(x)) {
      if (is.null(group_col)) group_col <- rec
      if (!group_by_given)    group_by  <- "value"
      if (!split_given && !is.function(split) && identical(split, "none") &&
          !is.null(max_rows)) {
        split <- "group_safe"
      }
      # A list, not c(): a caller's `drop_cols` may be integer positions, and
      # c(1L, ".rtf_record") would coerce them to strings.
      drop_cols <- if (is.null(drop_cols)) rec else {
        c(as.list(drop_cols), list(rec))
      }
    }
  }

  # ---- gtsummary -> gt --------------------------------------------------
  if (.is_gtsummary_tbl(x)) x <- .gtsummary_to_gt(x)

  # ---- resolve body + metadata ------------------------------------------
  if (.is_gt_tbl(x)) {
    tokens          <- .resolve_gt_tokens(read_meta)
    kw              <- .gt_to_rtftable_kwargs(x, tokens = tokens)
    body            <- kw$data
    cell_styles     <- kw$cell_styles
    titles_block    <- kw$titles_block
    footnotes_block <- kw$footnotes_block
  } else if (.is_rtables_tbl(x)) {
    tokens          <- .resolve_rtables_tokens(read_meta)
    kw              <- .rtables_to_rtftable_kwargs(x, tokens = tokens)
    body            <- kw$data
    cell_styles     <- kw$cell_styles
    titles_block    <- kw$titles_block
    footnotes_block <- kw$footnotes_block
  } else if (.is_rlistings_tbl(x)) {
    # NB: an rlistings listing_df is a data.frame subclass; it must be tested
    # before the plain-data.frame branch below, or it renders with its
    # disp_cols, key-column suppression, titles and footers silently
    # discarded (#322).
    tokens          <- .resolve_rlistings_tokens(read_meta)
    kw              <- .rlistings_to_rtftable_kwargs(x, tokens = tokens)
    body            <- kw$data
    cell_styles     <- kw$cell_styles
    titles_block    <- kw$titles_block
    footnotes_block <- kw$footnotes_block
  } else if (.is_flextable_tbl(x)) {
    tokens          <- .resolve_flextable_tokens(read_meta)
    kw              <- .flextable_to_rtftable_kwargs(x, tokens = tokens)
    body            <- kw$data
    cell_styles     <- kw$cell_styles
    titles_block    <- kw$titles_block
    footnotes_block <- kw$footnotes_block
  } else if (.is_huxtable_tbl(x)) {
    # NB: a huxtable IS a data.frame subclass, so this branch MUST come before
    # the plain-data.frame branch below.
    tokens          <- .resolve_huxtable_tokens(read_meta)
    kw              <- .huxtable_to_rtftable_kwargs(x, tokens = tokens)
    body            <- kw$data
    cell_styles     <- kw$cell_styles
    titles_block    <- kw$titles_block
    footnotes_block <- kw$footnotes_block
  } else if (is.data.frame(x)) {
    body            <- x
    kw              <- list()
    cell_styles     <- NULL
    titles_block    <- NULL
    footnotes_block <- NULL
    # A listing spec IS this body's metadata source, the way an adapter is for
    # a gt or rtables object: it names the columns, sizes them and aligns them.
    # It is written into `kw` for exactly that reason -- `.assemble_page_rtftable()`
    # lets a user `...` argument beat anything in `kw`, so an explicit
    # `col_header` / `col_rel_width` still wins here too.
    if (!is.null(listing_meta)) {
      kw$col_header <- listing_meta$col_header
      kw$col_spec   <- listing_meta$col_spec
      if (is.null(user_args$col_rel_width) &&
          is.null(user_args$column_widths_twips)) {
        kw$col_rel_width <- listing_meta$col_rel_width
      }
    }

    # Header from the column display names: a column's `label` attribute
    # (read_meta "labels" token, on by default) wins over its name, and the
    # resulting names feed the spanning-header reconstruction of delimited
    # names (e.g. ydisctools "cohort1____trt1" or tfrmt
    # "cohort1___tlang_delim___trt1").  An explicit user `col_header`
    # (via ...) always wins, so skip then.
    if (is.null(kw$col_header) && is.null(user_args$col_header)) {
      disp     <- .df_display_names(body, read_meta)
      auto_hdr <- .split_names_to_col_header(disp, header_sep)
      if (!is.null(auto_hdr)) {
        kw$col_header <- auto_hdr
      } else if (!identical(disp, names(body))) {
        kw$col_header <- disp
      }
    }
  } else {
    stop("`as_rtftables()` supports gt_tbl, gt_group, gtsummary, ",
         "rtables/tern, rlistings, flextable, huxtable, data.frame/tibble, ",
         "or a list of these; got '",
         paste(class(x), collapse = "/"), "'.", call. = FALSE)
  }

  # Per-body page builder: build the stub, resolve drops, sort, auto-size,
  # paginate, then assemble one rtftable per page.  Factored into a closure
  # (capturing the resolved arguments) so it can run either ONCE on the whole
  # body -- the usual path -- or PER GROUP for a `by_value` + `stub_vars` split,
  # where the stub must be built after the split (see the branch below).
  # `split_mode` defaults to the requested `split`; the per-group calls override
  # it to "none" so each group's sub-body is not split again.
  build_pages <- function(body, kw, cell_styles, split_mode = split) {
    # ---- build the indented stub (stub_cols) on this body ---------------
    # Merge the `stub_vars` hierarchy columns into one indented stub column
    # BEFORE pagination, so any source that exposes the hierarchy as separate
    # columns (data.frame, plain gt, multi-group-column / tfrmt
    # `location = "column"` tables) gains a clinical stub.  Everything
    # downstream (`drop_cols`, `group_col`, `sort_by`, `group_by = "indent"`,
    # the group-aware splits) then operates on the reshaped, post-stub columns.
    if (!is.null(stub_spec_obj)) {
      st          <- .apply_stub_vars(body, kw, cell_styles, stub_spec_obj)
      body        <- st$body
      kw          <- st$kw
      cell_styles <- st$cell_styles
    }

    # ---- resolve hidden (drop) columns ----------------------------------
    # Columns to remove from the printed pages AFTER pagination (so they can be
    # used by group_col / collapse_repeats / blank_rows_by_change first, then
    # hidden).  Resolved on this body's coordinates, before the sidx helper
    # column is appended below.
    drop_idx <- .resolve_drop_cols(drop_cols, body)

    # ---- sort body rows -------------------------------------------------
    # Order the body BEFORE pagination so group detection, (Cont.) labels and
    # blank_rows positions all see the sorted order.  cell_styles (per original
    # row) is reordered in lockstep so per-cell styling stays with its row.
    ord <- .resolve_sort_order(sort_by, sort_desc, body)
    if (!is.null(ord)) {
      body <- body[ord, , drop = FALSE]
      rownames(body) <- NULL
      if (!is.null(cell_styles)) cell_styles <- cell_styles[ord]
      if (!is.null(user_args$cell_styles) &&
          length(user_args$cell_styles) == length(ord)) {
        user_args$cell_styles <- user_args$cell_styles[ord]
      }
    }

    # ---- auto column widths ---------------------------------------------
    # Size each column to its widest content (header label or data cell) so
    # long row labels / headers do not wrap.  Widths are computed on this body
    # and applied to every page it yields.  An explicit width always wins.
    if (isTRUE(auto_width) &&
        is.null(user_args$column_widths_twips) &&
        is.null(user_args$col_rel_width)) {
      flat_hdr <- .flatten_col_header_labels(kw$col_header, ncol(body))
      tw <- table_width_twips
      if (is.null(tw)) {
        nat <- tryCatch(auto_col_widths(body, col_header = flat_hdr),
                        error = function(e) NULL)
        if (!is.null(nat) && sum(nat) > .default_writable_twips()) {
          tw <- .default_writable_twips()
        }
      }
      aw <- tryCatch(
        auto_col_widths(body, col_header = flat_hdr,
                        table_width_twips = tw, protect_cols = 1L),
        error = function(e) NULL)
      if (!is.null(aw)) user_args$column_widths_twips <- aw
    }

    # ---- paginate (tracking original rows so per-cell styles can be sliced)
    have_styles <- !is.null(cell_styles)
    sidx_col    <- ".__rtf_sidx__"
    if (have_styles) body[[sidx_col]] <- seq_len(nrow(body))

    pages <- .paginate_df(
      body, max_rows = max_rows, split = split_mode, split_rows = split_rows,
      group_col = group_col, group_by = group_by, cont_label = cont_label,
      min_group_rows = min_group_rows, blank_rows = blank_rows,
      blank_row_first = blank_row_first, blank_row_end = blank_row_end,
      count_blank_rows = count_blank_rows,
      # The drop columns are invisible on the page, so a row carrying only
      # them is blank as far as the page-edge accounting goes (#362).
      blank_ignore = names(body)[drop_idx],
      align_count_pct = align_count_pct, cell_format = cell_format, na = na,
      collapse_repeats = collapse_repeats)
    page_names <- names(pages)

    out <- lapply(seq_along(pages), function(i) {
      pg         <- pages[[i]]
      blank_attr <- attr(pg, "rtf_blank_rows", exact = TRUE)

      cs_slice <- NULL
      if (have_styles) {
        oidx <- pg[[sidx_col]]
        pg[[sidx_col]] <- NULL
        cs_slice <- lapply(oidx, function(r) {
          if (is.na(r)) NULL else cell_styles[[as.integer(r)]]
        })
        if (all(vapply(cs_slice, is.null, logical(1L)))) cs_slice <- NULL
      }

      rt <- .assemble_page_rtftable(pg, kw, cs_slice, user_args,
                                     border, style, blank_attr, drop_idx)
      if (!is.null(titles_block))    attr(rt, "rtf_titles")    <- titles_block
      if (!is.null(footnotes_block)) attr(rt, "rtf_footnotes") <- footnotes_block
      rt
    })
    if (!is.null(page_names)) names(out) <- page_names
    out
  }

  # ---- by_value + stub_vars: split first, build the stub per page --------
  # A `by_value` split makes each group its own independent section, so the
  # indented stub must be built AFTER the split -- once per page.  Otherwise a
  # constant intermediate hierarchy level (e.g. LBTOX_LBL / group1 / label with
  # a fixed group1) collapses into a single stub label row that spans every
  # group and cannot be divided, and the outer value grouping fragments to one
  # page per row.  So here we split the raw body by the `group_col` value first,
  # then run build_pages(split = "none") on each group.  For every OTHER split
  # the table stays one logical table paginated across pages, so the stub is
  # built once on the full body (the plain build_pages() call below).
  if (!is.function(split) && identical(split, "by_value") &&
      !is.null(stub_spec_obj)) {
    gidx <- if (is.null(group_col)) 1L
            else .resolve_col_indices(list(group_col), body, "group_col")
    gval <- as.character(body[[gidx]])
    gval[is.na(gval)] <- ""
    lv   <- unique(gval)                    # one section per value, in order
    out  <- list()
    for (k in seq_along(lv)) {
      rows     <- which(gval == lv[k])
      sub_body <- body[rows, , drop = FALSE]
      rownames(sub_body) <- NULL
      sub_cs   <- if (!is.null(cell_styles)) cell_styles[rows] else NULL
      pgs      <- build_pages(sub_body, kw, sub_cs, split_mode = "none")
      nm       <- if (nzchar(lv[k])) lv[k] else paste0("group_", k)
      names(pgs) <- rep(nm, length(pgs))    # split_mode "none" => one page
      out <- c(out, pgs)
    }
    return(out)
  }

  build_pages(body, kw, cell_styles)
}


# Build one page's rtftable from (page data.frame, gt kwargs, sliced
# cell_styles, user overrides).  Shared by as_rtftables() and as_rtftable().
# User-supplied `...` values always beat the gt-extracted ones.
.assemble_page_rtftable <- function(data, kw, cell_styles, user_args,
                                     border, style, blank_attr,
                                     drop_idx = integer(0)) {
  call_args <- list(data = data, border = border, style = style,
                    read_attributes = TRUE)

  # col_header: the AUTO (adapter-derived) header travels with the body through
  # `drop_cols` / `stub_vars` reindexing below.  A USER-supplied `col_header`
  # is instead applied AFTER the table is built, against the FINAL printed
  # columns (via set_col_header()), so its positions and names refer to what is
  # actually shown -- no intermediate/pre-drop coordinates.
  if (is.null(user_args$col_header) && !is.null(kw$col_header)) {
    call_args$col_header <- kw$col_header
  }

  # col_spec: deep merge (user fields win per column)
  if (!is.null(kw$col_spec) || !is.null(user_args$col_spec)) {
    call_args$col_spec <- .merge_col_spec(user_args$col_spec, kw$col_spec)
  }

  # widths: user > gt
  for (k in c("column_widths_twips", "col_rel_width")) {
    if (!is.null(user_args[[k]])) {
      call_args[[k]] <- user_args[[k]]
    } else if (!is.null(kw[[k]])) {
      call_args[[k]] <- kw[[k]]
    }
  }

  # cell_styles: user > sliced-gt
  if (!is.null(user_args$cell_styles)) {
    call_args$cell_styles <- user_args$cell_styles
  } else if (!is.null(cell_styles)) {
    call_args$cell_styles <- cell_styles
  }

  # remaining user args pass through verbatim
  consumed <- c("data", "col_header", "col_spec", "column_widths_twips",
                "col_rel_width", "cell_styles", "border", "style")
  for (k in setdiff(names(user_args), consumed)) {
    call_args[[k]] <- user_args[[k]]
  }

  # Re-attach the blank-row attribute (column removal / subsetting can drop
  # custom attributes); rtftable(read_attributes = TRUE) consumes it.
  if (!is.null(blank_attr)) {
    attr(call_args$data, "rtf_blank_rows") <- blank_attr
  }

  # Hide the `drop_cols` columns: remove them from the (now fully resolved)
  # body + every position-indexed argument, so a carrier / grouping column can
  # be used for pagination above and yet never printed.
  if (length(drop_idx)) {
    call_args <- .apply_col_drop(call_args, drop_idx)
  }

  tbl <- do.call(rtftable, call_args)

  # Apply a USER `col_header` against the final table (final-column coordinates,
  # name-aware).  Deferred to here so `drop_cols` / `stub_vars` do not shift the
  # positions the author wrote.
  if (!is.null(user_args$col_header)) {
    tbl <- set_col_header(tbl, user_args$col_header)
  }
  tbl
}


#' Combine table page lists into one auto-sectioned list
#'
#' Assembles several converted tables -- each an [rtftable()] or a list of them
#' (typically an [as_rtftables()] result) -- into a single flat list ready for
#' `rtf_tables(..., auto_section = TRUE)`, where every argument becomes **one RTF
#' section**.
#'
#' `auto_section = TRUE` cuts a new section at each *named* element of the list
#' and drops that name into the section header; empty / unnamed elements fall
#' through into the current section.  So to give a multi-page table a single
#' heading you name its **first** page and leave the rest unnamed.
#' `combine_sections()` does exactly that bookkeeping: each argument's name is
#' placed on its first page and the remaining pages are blanked, so one logical
#' (possibly paginated) table renders as one clean section.
#'
#' This is a thin, format-agnostic convenience -- it only manipulates the names
#' of a list of `rtftable`s, with no knowledge of the source object.  It is not
#' required: leaving every page named (or unnamed) simply yields one section per
#' page, which is harmless (it matches the common one-page-one-section
#' convention); `combine_sections()` is for the tidier one-section-per-table
#' layout.
#'
#' For a *single* table whose grouping variable is a real **column** of the
#' body, you usually do not need this at all: `as_rtftables(x, split =
#' "by_value", group_col = ...)` already names each page by the group value, so
#' `auto_section = TRUE` gives one section per group directly.
#'
#' @param ... Named arguments, each either an `rtftable` or a list of
#'   `rtftable`s (e.g. the result of [as_rtftables()]).  Each argument **name**
#'   becomes a section heading.  An unnamed argument is appended to the previous
#'   section (its pages all fall through).
#'
#' @return A single flat, named list of `rtftable` objects suitable for
#'   `rtf_tables(result, auto_section = TRUE)`.
#'
#' @seealso [as_rtftables()] (which produces the page lists, and whose
#'   `split = "by_value"` already names pages per group), [rtf_tables()] for the
#'   `auto_section` argument.
#'
#' @examples
#' df <- data.frame(
#'   Characteristic = c("Age", "Sex", "Race", "Region"),
#'   Value          = c("75", "F", "White", "US"),
#'   stringsAsFactors = FALSE)
#' dm <- as_rtftables(df)                                  # one page
#' ae <- as_rtftables(df, split = "rows", split_rows = 2)  # two pages
#'
#' # One clean section per table (the AE table's two pages share one section).
#' sections <- combine_sections(Demographics = dm, `Adverse Events` = ae)
#' names(sections)            # "Demographics" "Adverse Events" ""
#'
#' \dontrun{
#' doc <- rtf_document() |>
#'   rtf_section(secinfo = list(header = my_header)) |>
#'   rtf_tables(sections, auto_section = TRUE)
#' }
#'
#' @export
combine_sections <- function(...) {
  groups <- list(...)
  if (length(groups) == 0L) return(list())
  labels <- names(groups)
  if (is.null(labels)) labels <- rep("", length(groups))

  out <- list()
  for (i in seq_along(groups)) {
    g     <- groups[[i]]
    label <- labels[[i]] %||% ""
    if (inherits(g, "rtftable")) g <- list(g)
    if (!is.list(g)) {
      stop("Each argument to `combine_sections()` must be an rtftable or a ",
           "list of rtftables.", call. = FALSE)
    }
    if (length(g) == 0L) next
    if (!all(vapply(g, inherits, logical(1L), "rtftable"))) {
      where <- if (nzchar(label)) sprintf("'%s'", label) else sprintf("#%d", i)
      stop(sprintf(
        "`combine_sections()` argument %s must contain only rtftable objects.",
        where), call. = FALSE)
    }
    # Name the first page with the section label; blank the rest so they fall
    # through into the same section under auto_section.
    names(g) <- c(label, rep("", length(g) - 1L))
    out <- c(out, g)
  }
  out
}
