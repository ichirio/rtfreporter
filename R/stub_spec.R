# ============================================================================
#  stub_spec() -- one object for every stub setting (#314)
# ============================================================================
#
#  `as_rtftables()` grew a flat `stub_vars` / `stub_label` / `stub_indent` /
#  `stub_group_summary` family, and `stub_cols()` has since gained `layout` and
#  `label_span`.  Carrying each new option across as another argument would
#  keep widening a signature that is already long.
#
#  The package already has the answer in three places -- `blank_rows =
#  blank_rows_by_change(...)`, `page = rtf_page(...)`, `default_format =
#  rtf_default_format(...)`: fold a bundle of settings into a constructor and
#  take one argument.  `stub_spec()` is that constructor for the stub, so
#  `as_rtftables()` needs a single `stub =` and never has to grow again when
#  `stub_cols()` learns something new.

#' Bundle the row-stub settings for `as_rtftables()`
#'
#' Collects everything [stub_cols()] accepts into one object, so
#' [as_rtftables()] takes a single `stub =` argument instead of one argument
#' per setting.  The arguments are `stub_cols()`'s own, so the two never drift
#' apart.
#'
#' The `stub_vars` / `stub_label` / `stub_indent` / `stub_group_summary`
#' arguments of [as_rtftables()] are **superseded** by this: they still work
#' and are not deprecated, but they cannot reach `layout` or `label_span`, and
#' new settings will only be added here.
#'
#' @param vars The hierarchy columns, **parent first, leaf last** -- at least
#'   two.  See [stub_cols()].
#' @param label,indent,group_summary,layout,label_span Passed to [stub_cols()];
#'   see there for the full description of each.
#'
#' @return An object of class `rtf_stub_spec`.
#'
#' @seealso [stub_cols()], which does the work and documents every setting;
#'   [as_rtftables()], which consumes this through its `stub` argument.
#'
#' @examples
#' # The clinical indented stub, as as_rtftables(stub_vars = ) builds it.
#' stub_spec(c("SOC", "PT"), label = "System Organ Class / Preferred Term")
#'
#' # Keep the hierarchy columns instead, with the group value on its own row.
#' stub_spec(c("SOC", "PT"), layout = "columns")
#'
#' # A merged stub whose group rows span the table.
#' stub_spec(c("SOC", "PT"), indent = 0, label_span = TRUE)
#'
#' @export
stub_spec <- function(vars,
                      label = NULL,
                      indent = 4L,
                      group_summary = c("empty", "parent"),
                      layout = c("merged", "columns"),
                      label_span = FALSE) {
  if (missing(vars) || is.null(vars)) {
    stop("`vars` is required: the hierarchy columns, parent first.",
         call. = FALSE)
  }
  structure(
    list(vars = vars, label = label, indent = indent,
         group_summary = group_summary, layout = match.arg(layout),
         label_span = label_span),
    class = "rtf_stub_spec"
  )
}

#' @export
print.rtf_stub_spec <- function(x, ...) {
  cat("<rtf_stub_spec>\n")
  cat("  vars       :", paste(as.character(x$vars), collapse = ", "), "\n")
  cat("  layout     :", x$layout, "\n")
  cat("  indent     :", x$indent, "\n")
  if (!is.null(x$label)) cat("  label      :", x$label, "\n")
  if (isTRUE(x$label_span)) cat("  label_span : TRUE\n")
  invisible(x)
}

# Resolve `as_rtftables(stub = )` plus the superseded flat arguments into one
# spec, or NULL when no stub was asked for.  Mixing the two is an error rather
# than a silent precedence rule.
.resolve_stub_spec <- function(stub, stub_vars, stub_label, stub_indent,
                               stub_group_summary, flat_given) {
  if (!is.null(stub) && any(flat_given)) {
    stop("Pass either `stub` or the superseded `stub_vars` family, not both.",
         call. = FALSE)
  }
  if (!is.null(stub)) {
    if (inherits(stub, "rtf_stub_spec")) return(stub)
    # A bare vector of column names / positions is the common case.
    return(stub_spec(stub))
  }
  if (is.null(stub_vars)) return(NULL)
  stub_spec(stub_vars, label = stub_label, indent = stub_indent,
            group_summary = stub_group_summary)
}
