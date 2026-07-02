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

# Default separators understood when reconstructing a spanning column header
# from a plain data.frame's column names (see `.split_names_to_col_header()`):
#   "____"              -- ydisctools::pivot_stats_wider()
#   "___tlang_delim___" -- tfrmt's column delimiter
.default_header_seps <- function() c("____", "___tlang_delim___")

# Reconstruct a multi-row spanning column header by splitting delimited column
# names on `seps`.  A bare data.frame carries no spanning metadata, so the
# nesting is parsed out of the names: each name becomes a stack of segments,
# horizontally adjacent columns that share a label AND the same ancestor path
# are merged into a spanning cell, and columns with fewer segments (e.g. id
# columns with no separator) are bottom-aligned so their label sits on the leaf
# row with blank cells above.  A doubled "____" (i.e. "________") splits to an
# empty middle segment -> a blank cell at that header level.
#
# Returns the same shape the rtables adapter emits -- upper rows as lists of
# `col_cell()`, bottom row as a character vector -- so `drop_cols` reindexing
# and the rest of the pipeline treat it like any adapter spanning header.
# Returns NULL when no name splits into more than one segment (caller then
# falls back to the plain `names(data)` single-row header).
.split_names_to_col_header <- function(col_names, seps) {
  if (is.null(seps)) return(NULL)
  seps <- seps[!is.na(seps) & nzchar(seps)]
  if (length(seps) == 0L || length(col_names) == 0L) return(NULL)

  # Literal-quote each separator (PCRE \Q...\E) and try longest first, so a
  # longer separator wins over a shorter one that is its prefix.  \Q...\E avoids
  # gsub-style backreference escaping (unreliable in this R build).
  seps    <- seps[order(-nchar(seps))]
  pattern <- paste0("\\Q", seps, "\\E", collapse = "|")

  segs  <- strsplit(col_names, pattern, perl = TRUE)
  segs  <- lapply(segs, function(s) if (length(s) == 0L) "" else s)
  depth <- max(lengths(segs))
  if (depth <= 1L) return(NULL)

  ncol <- length(col_names)
  # Bottom-align each column's segments into a depth x ncol label matrix; cells
  # above a short column's top segment are NA ("no cell"), distinct from an
  # explicit empty-string segment ("" -> a blank labelled cell).
  M <- matrix(NA_character_, nrow = depth, ncol = ncol)
  for (j in seq_len(ncol)) {
    s <- segs[[j]]
    M[(depth - length(s) + 1L):depth, j] <- s
  }

  same_label <- function(a, b) {
    (is.na(a) && is.na(b)) || (!is.na(a) && !is.na(b) && a == b)
  }

  # Upper rows (1 .. depth-1): merge adjacent columns sharing this row's label
  # and the identical ancestor path (rows above).  Each run -> one col_cell.
  upper_rows <- vector("list", depth - 1L)
  for (r in seq_len(depth - 1L)) {
    cells <- list()
    j <- 1L
    while (j <= ncol) {
      k <- j
      while (k + 1L <= ncol &&
             same_label(M[r, k + 1L], M[r, j]) &&
             identical(M[seq_len(r - 1L), k + 1L], M[seq_len(r - 1L), j])) {
        k <- k + 1L
      }
      lab <- if (is.na(M[r, j])) "" else M[r, j]
      cells <- c(cells,
                 list(col_cell(pos = if (j == k) j else c(j, k), label = lab)))
      j <- k + 1L
    }
    upper_rows[[r]] <- cells
  }

  # Bottom (leaf) row: one label per column, never merged.
  bottom <- M[depth, ]
  bottom[is.na(bottom)] <- ""

  c(upper_rows, list(bottom))
}

# Resolve a column spec (character names and/or integer indices, or a list
# mixing the two) to integer column indices into `df`, preserving the supplied
# order.  Shared by `sort_by`, `drop_cols` and stub_cols()'s `vars`; `arg`
# names the argument in the error messages.
.resolve_col_indices <- function(cols, df, arg) {
  vapply(cols, function(c1) {
    if (is.character(c1)) {
      m <- match(c1, names(df))
      if (is.na(m)) {
        stop(sprintf("`%s` column '%s' not found in the table.", arg, c1),
             call. = FALSE)
      }
      as.integer(m)
    } else {
      i <- as.integer(c1)
      if (is.na(i) || i < 1L || i > ncol(df)) {
        stop(sprintf("`%s` index %s out of range (1..%d).", arg, c1, ncol(df)),
             call. = FALSE)
      }
      i
    }
  }, integer(1L), USE.NAMES = FALSE)
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
#' Supported inputs: `gt_tbl`, gtsummary tables, rtables/tern `VTableTree`
#' tables, `flextable` tables, `huxtable` tables, plain `data.frame` / tibble,
#' or a `list` of any of these (the list is flattened, names propagated as
#' `name`, `name.1`, `name.2`, ...).  Figures are out of scope -- use
#' [rtf_figures()] for those.
#'
#' @section What is carried, by source:
#'
#' The body is always the table's *rendered* body -- for gt/gtsummary via
#' `gt::extract_body()`, for rtables/tern via `formatters::matrix_form()`.
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
#' }
#'
#' For flextable the *displayed* text is read (header labels set via
#' `set_header_labels()` and `colformat_*()` formatting included), not the raw
#' `$body$dataset`.  Cells composed of images / equations and `footnote()`
#' reference marks are not carried.  For huxtable the displayed text is likewise
#' read (its `number_format` applied); huxtable has no footnote concept, so only
#' the caption (a page title) is carried.
#'
#' **Not carried** (RTF cannot reproduce these, so they are intentionally
#' ignored): per-cell bold / italic / underline from `gt::tab_style()`, cell
#' background colours / fills, font and size styling, and Markdown formatting
#' inside labels or titles.  For a plain `data.frame` / tibble two pieces of
#' metadata are read: column `label` attributes become header labels (the
#' `"labels"` token, see `read_meta`), and delimited column names are
#' reconstructed into a spanning header (see `header_sep`); everything else
#' (`col_spec`, widths, ...) you set yourself.  See also [stub_cols()] for
#' finishing a tidy hierarchy-column data.frame into the indented stub
#' layout beforehand.
#'
#' @param x A `gt_tbl`, a gtsummary table, an rtables/tern `VTableTree`, a
#'   `flextable`, a `huxtable`, a `data.frame` / tibble, or a `list` of these.
#' @param read_meta Controls metadata extraction from the source table:
#'   `TRUE` (default, read everything in the table above), `FALSE` (use only
#'   the rendered body -- equivalent to the old `paginate()`), or a character
#'   vector of tokens.  Tokens for
#'   gt/gtsummary: `"col_header"`, `"alignment"`, `"spanning"`, `"widths"`,
#'   `"titles"`, `"footnotes"`.  For rtables/tern: `"col_header"`,
#'   `"alignment"`, `"spanning"`, `"titles"`, `"footnotes"`, `"indent"`,
#'   `"footnote_marks"`.  For flextable: `"col_header"`, `"alignment"`,
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
#'   is the most portable.
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
#'     \item{a [blank_rows_by_change()] object}{insert a blank whenever the value
#'       of one or more columns changes.}
#'     \item{a [blank_rows_by_rule()] object}{insert a blank before / after rows
#'       whose column matches a regular expression.}
#'   }
#'   For example
#'   `blank_rows = list(c(-1), blank_rows_by_change("Visit"))` adds a trailing
#'   blank and a blank at every change of `Visit`.  By default these blanks are
#'   added *after* the split and do not count toward `max_rows`; see
#'   `count_blank_rows` to count them.
#' @param blank_row_first,blank_row_end Logical (default `FALSE`).  Add a single
#'   blank row at the very top (`blank_row_first`) or bottom (`blank_row_end`)
#'   of **every** page, as page furniture.  These are applied after the split
#'   and are never counted toward `max_rows`.
#' @param align_count_pct Logical (default `FALSE`).  Shorthand to realign
#'   `"n (xx.x)"` count/percent cells to a uniform width before pagination (the
#'   built-in [realign_count_pct()]).  Ignored when `cell_format` is supplied,
#'   which takes precedence.
#' @param min_group_rows Integer (default `2`).  Widow/orphan control for the
#'   group-aware splits (`"group_force"`, `"group_safe"`, `"by_value"`): when a
#'   page would end on a group that *starts* on that page while showing fewer
#'   than `min_group_rows` of the group's child rows, the whole group is moved
#'   to the next page.  This prevents a lone group header being stranded at the
#'   foot of a page with none (or too few) of its members.  Set to `0` to
#'   disable (the previous behaviour).
#' @param count_blank_rows Logical (default `FALSE`).  When `TRUE`, blank
#'   separator rows are **counted toward `max_rows`** during pagination, so a
#'   page (data rows + blanks) does not overflow the budget.  The blank
#'   positions resolved from `blank_rows` (and from any `rtf_blank_rows`
#'   attribute already on the input) are materialised before the split and
#'   re-attached per page afterwards, with a leading blank suppressed at the top
#'   of each page.  `blank_row_first` / `blank_row_end` remain page furniture
#'   added after the split and are **not** counted (so they may still exceed
#'   `max_rows`).  When `FALSE` (default) blank rows are added after the split
#'   and do not affect the row count.
#' @param cell_format Optional cell re-formatter applied column-by-column to
#'   the body **before** pagination, for monospaced alignment.  Either a single
#'   function -- applied to every data column (columns 2..N; the row-label
#'   column 1 is left alone) -- or a list of functions taken positionally
#'   (`cell_format[[j]]` for column `j`; non-function entries are skipped).
#'   Each function takes one column (a character vector) and returns a
#'   character vector of the same length; see [fmt_count_paren()] /
#'   [fmt_right_align()] for built-ins and the contract for writing your own.
#'   When supplied it takes precedence over `align_count_pct`.
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
#'   metadata (`col_header` incl. spanning headers, `col_spec`, column widths,
#'   `col_header_align`, `row_title`, and per-cell `cell_styles`) is reindexed
#'   automatically to the remaining columns.  `drop_cols` must leave at least
#'   one column to display.  Note that for `split = "by_value"` the page
#'   **names** come from the `group_col` value, so that column may be dropped and
#'   the pages are still named by it; but under `"group_safe"` / `"group_force"`
#'   the `" (Cont.)"` marker is written into the `group_col` cell, so if
#'   `group_col` is itself dropped the marker is not shown (group on the visible
#'   label column if the marker is wanted).
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
#' @return A list of `rtftable` objects, one per page.  When the split is
#'   value-based (or the input was a named list) the list is named.
#'
#' @seealso [as_rtftable()] for the single-page convenience wrapper,
#'   [rtf_tables()] to append the result to a document.
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
                         collapse_repeats = NULL,
                         drop_cols       = NULL,
                         header_sep      = .default_header_seps(),
                         auto_width        = FALSE,
                         table_width_twips = NULL,
                         border          = "tfl",
                         style           = NULL,
                         ...) {
  # `split` is a built-in strategy name OR a custom pagination function.
  if (!is.function(split)) split <- match.arg(split)
  group_by <- match.arg(group_by)
  user_args <- list(...)

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
        cell_format = cell_format, collapse_repeats = collapse_repeats,
        drop_cols = drop_cols, header_sep = header_sep,
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
    # Header from the column display names: a column's `label` attribute
    # (read_meta "labels" token, on by default) wins over its name, and the
    # resulting names feed the spanning-header reconstruction of delimited
    # names (e.g. ydisctools "cohort1____trt1" or tfrmt
    # "cohort1___tlang_delim___trt1").  An explicit user `col_header`
    # (via ...) always wins, so skip then.
    if (is.null(user_args$col_header)) {
      disp     <- .df_display_names(body, read_meta)
      auto_hdr <- .split_names_to_col_header(disp, header_sep)
      if (!is.null(auto_hdr)) {
        kw$col_header <- auto_hdr
      } else if (!identical(disp, names(body))) {
        kw$col_header <- disp
      }
    }
  } else {
    stop("`as_rtftables()` supports gt_tbl, gtsummary, rtables/tern, ",
         "flextable, huxtable, data.frame/tibble, or a list of these; got '",
         paste(class(x), collapse = "/"), "'.", call. = FALSE)
  }

  # ---- resolve hidden (drop) columns ------------------------------------
  # Columns to remove from the printed pages AFTER pagination (so they can be
  # used by group_col / collapse_repeats / blank_rows_by_change first, then
  # hidden).  Resolved here, on the input body's coordinates, before the sidx
  # helper column is appended below.
  drop_idx <- .resolve_drop_cols(drop_cols, body)

  # ---- sort body rows ---------------------------------------------------
  # Order the body BEFORE pagination so group detection, (Cont.) labels and
  # blank_rows positions all see the sorted order.  cell_styles (per original
  # row) is reordered in lockstep so per-cell styling stays attached to its row.
  ord <- .resolve_sort_order(sort_by, sort_desc, body)
  if (!is.null(ord)) {
    body <- body[ord, , drop = FALSE]
    rownames(body) <- NULL
    # Adapter-extracted cell_styles (per original row) follow the new order.
    if (!is.null(cell_styles)) cell_styles <- cell_styles[ord]
    # An explicitly supplied cell_styles (one per body row) likewise reorders so
    # it stays attached to its row through the sort.
    if (!is.null(user_args$cell_styles) &&
        length(user_args$cell_styles) == length(ord)) {
      user_args$cell_styles <- user_args$cell_styles[ord]
    }
  }

  # ---- auto column widths -----------------------------------------------
  # When requested, size each column to its widest content (header label or
  # data cell) so that long row labels and column headers do not wrap.  The
  # widths are computed once on the full body and applied to every page, so
  # paginated pages stay aligned.  An explicit `column_widths_twips` /
  # `col_rel_width` from the user always wins.
  if (isTRUE(auto_width) &&
      is.null(user_args$column_widths_twips) &&
      is.null(user_args$col_rel_width)) {
    flat_hdr <- .flatten_col_header_labels(kw$col_header, ncol(body))
    tw <- table_width_twips
    # With no explicit width, use the natural content widths but cap them at
    # the default page's writable width -- so a table that is naturally too
    # wide (e.g. a tfrmt demographics table) is scaled down to fit the page,
    # while narrower tables keep their natural widths.
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
    body, max_rows = max_rows, split = split, split_rows = split_rows,
    group_col = group_col, group_by = group_by, cont_label = cont_label,
    min_group_rows = min_group_rows, blank_rows = blank_rows,
    blank_row_first = blank_row_first, blank_row_end = blank_row_end,
    count_blank_rows = count_blank_rows,
    align_count_pct = align_count_pct, cell_format = cell_format,
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


# Build one page's rtftable from (page data.frame, gt kwargs, sliced
# cell_styles, user overrides).  Shared by as_rtftables() and as_rtftable().
# User-supplied `...` values always beat the gt-extracted ones.
.assemble_page_rtftable <- function(data, kw, cell_styles, user_args,
                                     border, style, blank_attr,
                                     drop_idx = integer(0)) {
  call_args <- list(data = data, border = border, style = style,
                    read_attributes = TRUE)

  # col_header: user > gt
  if (!is.null(user_args$col_header)) {
    call_args$col_header <- user_args$col_header
  } else if (!is.null(kw$col_header)) {
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

  do.call(rtftable, call_args)
}


# Resolve a `drop_cols` spec (character names and/or integer indices, or NULL)
# to a sorted vector of unique integer column indices into `df`.  Errors on
# unknown names / out-of-range indices, and refuses to drop every column.
.resolve_drop_cols <- function(cols, df) {
  if (is.null(cols) || length(cols) == 0L) return(integer(0))
  idx <- sort(unique(.resolve_col_indices(cols, df, "drop_cols")))
  if (length(idx) >= ncol(df)) {
    stop("`drop_cols` must leave at least one column to display.", call. = FALSE)
  }
  idx
}


# Resolve a `sort_by` / `sort_desc` request to an integer row permutation of
# `df` (or NULL for "keep input order").  Columns are given as names and/or
# integer indices (a vector, or a list to mix the two), in body coordinates.
# The sort is stable and NA keys are placed last; descending keys are handled
# via `-xtfrm()` so mixed column types (character, numeric, factor) all work.
.resolve_sort_order <- function(sort_by, sort_desc, df) {
  if (is.null(sort_by) || length(sort_by) == 0L) return(NULL)
  idx  <- .resolve_sort_cols(sort_by, df)
  desc <- .normalize_sort_desc(sort_desc, length(idx))
  keys <- lapply(seq_along(idx), function(k) {
    r <- xtfrm(df[[idx[k]]])           # rank-transform: comparable as numeric
    if (isTRUE(desc[k])) -r else r
  })
  do.call(order, c(keys, list(na.last = TRUE)))
}

# Resolve `sort_by` columns (names / integers / a mixing list) to integer
# indices into `df`, preserving the supplied order (sort priority).
.resolve_sort_cols <- function(cols, df) {
  .resolve_col_indices(cols, df, "sort_by")
}

# Normalise `sort_desc` to a logical vector of length `n` (one per sort key).
.normalize_sort_desc <- function(sort_desc, n) {
  if (is.null(sort_desc)) return(rep(FALSE, n))
  d <- as.logical(sort_desc)
  if (anyNA(d)) stop("`sort_desc` must be TRUE / FALSE.", call. = FALSE)
  if (length(d) == 1L) return(rep(d, n))
  if (length(d) != n) {
    stop(sprintf("`sort_desc` must have length 1 or %d (one per `sort_by`).", n),
         call. = FALSE)
  }
  d
}


# Remove the columns `drop_idx` (positions into the resolved body) from a
# `rtftable()` call-args list, reindexing every position-indexed argument to the
# kept columns.  Mirrors the internal "carry a helper column through pagination,
# strip it before rendering" pattern so a grouping / sort-key column can be used
# by the split and then hidden.  Handled args: data, col_header (flat leaf rows
# and spanning / pos cells), col_spec (integer + name `col`), col_rel_width,
# column_widths_twips, col_header_align, row_title, and per-row cell_styles
# (each per-column vector).  The `rtf_blank_rows` attribute survives the subset.
.apply_col_drop <- function(call_args, drop_idx) {
  data <- call_args$data
  n0   <- ncol(data)
  drop_idx <- sort(unique(as.integer(drop_idx)))
  drop_idx <- drop_idx[drop_idx >= 1L & drop_idx <= n0]
  if (length(drop_idx) == 0L) return(call_args)
  keep <- setdiff(seq_len(n0), drop_idx)
  if (length(keep) == 0L) {
    stop("`drop_cols` must leave at least one column to display.", call. = FALSE)
  }
  old_names  <- names(data)
  drop_names <- old_names[drop_idx]

  # data (preserve the blank-row attribute across the column subset)
  blank_attr <- attr(data, "rtf_blank_rows", exact = TRUE)
  data <- data[keep]
  if (!is.null(blank_attr)) attr(data, "rtf_blank_rows") <- blank_attr
  call_args$data <- data

  # length-n0 positional numeric vectors
  for (k in c("col_rel_width", "column_widths_twips")) {
    v <- call_args[[k]]
    if (!is.null(v) && length(v) == n0) call_args[[k]] <- v[keep]
  }

  # col_header_align: length-n0 vector reindexes; length-1 (scalar) untouched
  cha <- call_args$col_header_align
  if (!is.null(cha) && length(cha) == n0) call_args$col_header_align <- cha[keep]

  # col_header (flat leaf row(s) and spanning / pos cells)
  if (!is.null(call_args$col_header)) {
    call_args$col_header <- .reindex_col_header(call_args$col_header, keep, n0)
  }

  # col_spec: drop entries on a removed column, remap integer `col` to the new
  # position (name `col` survives unchanged -- names are not reused)
  if (!is.null(call_args$col_spec)) {
    call_args$col_spec <- .reindex_col_spec(call_args$col_spec, keep, drop_names)
  }

  # row_title: integer positions remap; column names survive unless dropped
  if (!is.null(call_args$row_title)) {
    call_args$row_title <- .reindex_row_title(call_args$row_title, keep,
                                              drop_names)
  }

  # cell_styles: per-row list; subset each length-n0 per-column vector
  if (!is.null(call_args$cell_styles)) {
    call_args$cell_styles <- lapply(call_args$cell_styles, function(r) {
      if (is.null(r) || !is.list(r)) return(r)
      lapply(r, function(v) if (length(v) == n0) v[keep] else v)
    })
  }

  call_args
}


# Reindex a col_header argument (raw rtftable form) onto the kept columns.
.reindex_col_header <- function(ch, keep, n0) {
  if (is.character(ch)) {
    if (length(ch) == n0) return(ch[keep])
    # pipe-delimited single string: split, reindex if it matches the width
    if (length(ch) == 1L && grepl("|", ch, fixed = TRUE)) {
      parts <- trimws(strsplit(ch, "|", fixed = TRUE)[[1L]])
      if (length(parts) == n0) return(parts[keep])
    }
    return(ch)
  }
  if (is.list(ch)) {
    rows <- lapply(ch, function(row) .reindex_header_row(row, keep, n0))
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0L) return(NULL)
    return(rows)
  }
  ch
}

# One header row: a character leaf row (length n0) or a list of spanning / pos
# cells.  Returns the reindexed row, or NULL if the row becomes empty.
.reindex_header_row <- function(row, keep, n0) {
  if (is.character(row)) {
    if (length(row) == n0) return(row[keep])
    return(row)
  }
  if (is.list(row)) {
    cells <- lapply(row, function(cell) .reindex_header_cell(cell, keep))
    cells <- Filter(Negate(is.null), cells)
    if (length(cells) == 0L) return(NULL)
    return(cells)
  }
  row
}

# One header cell with a `$pos` (single or c(min,max)) or legacy `$from`/`$to`.
# Returns the cell with positions remapped to kept-column coordinates, or NULL
# if every column it covered was dropped.
.reindex_header_cell <- function(cell, keep) {
  if (!is.list(cell)) return(cell)
  remap <- function(p) {
    span   <- if (length(p) <= 1L) p else seq.int(min(p), max(p))
    inside <- intersect(span, keep)
    if (length(inside) == 0L) return(NULL)
    np <- match(inside, keep)
    if (length(np) == 1L) np else c(min(np), max(np))
  }
  if (!is.null(cell$pos)) {
    np <- remap(cell$pos)
    if (is.null(np)) return(NULL)
    cell$pos <- np
    return(cell)
  }
  if (!is.null(cell$from) && !is.null(cell$to)) {
    np <- remap(c(cell$from, cell$to))
    if (is.null(np)) return(NULL)
    cell$from <- min(np)
    cell$to   <- max(np)
    return(cell)
  }
  cell
}

# Reindex a col_spec list onto kept columns.  Integer `col` is remapped to its
# new position (entry dropped if its column was removed); a character `col`
# survives unless it names a dropped column.
.reindex_col_spec <- function(col_spec, keep, drop_names) {
  out <- list()
  for (spec in col_spec) {
    col <- spec$col
    if (is.null(col)) { out[[length(out) + 1L]] <- spec; next }
    if (is.character(col)) {
      if (col %in% drop_names) next
    } else {
      nw <- match(as.integer(col), keep)
      if (is.na(nw)) next
      spec$col <- nw
    }
    out[[length(out) + 1L]] <- spec
  }
  if (length(out) == 0L) return(NULL)
  out
}

# Reindex a row_title argument onto kept columns.  Integer positions are
# remapped; column names survive unless dropped.  Returns NULL (-> rtftable's
# default of column 1) if nothing remains.
.reindex_row_title <- function(rt, keep, drop_names) {
  if (is.null(rt)) return(NULL)
  if (is.character(rt)) {
    rt2 <- rt[!(rt %in% drop_names)]
    if (length(rt2) == 0L) return(NULL)
    return(rt2)
  }
  m <- match(as.integer(rt), keep)
  m <- m[!is.na(m)]
  if (length(m) == 0L) return(NULL)
  m
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
