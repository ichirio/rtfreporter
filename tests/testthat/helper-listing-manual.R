# The hand-written listing pipeline build_listing() replaces.
#
# Taken from Discussion #356 (and the body of #241): the code a statistical
# programmer writes today to lay a listing out -- join source variables with
# "/", break a long cell at the separator and then at word boundaries, pad
# every column of a record to the tallest, add a blank row after each record,
# and interleave the narrow gutter columns.
#
# `split_string()`, `get_max_element_counts()` and `pad_list_elements()` are
# reproduced VERBATIM: they are the actual logic, and they were already base R.
# Only the dplyr / tidyr glue around them (`mutate()`, `across()`,
# `unnest_longer()`, `bind_rows()`) is rewritten in base R, because the package
# has no hard dependencies -- see `.manual_listing()` below, which is commented
# line by line against the pipeline it mirrors.
#
# test-listing-vs-manual.R asserts that build_listing() reproduces this, so the
# claim "the same processing, done for you" is checked rather than asserted.


# ── Verbatim from Discussion #356 ────────────────────────────────────────────

split_string <- function(text, max_length) {
  # 第1優先："/"で分割
  parts <- unlist(strsplit(text, "(?<=/)", perl = TRUE))

  result <- list()

  for (i in seq_along(parts)) {
    part <- trimws(parts[i])

    # 長さがmax_length以下ならそのまま追加
    if (nchar(part) <= max_length) {
      result <- append(result, list(part))
    } else {
      # 第2優先：ワード単位で分割（スペース、カンマ、ハイフンなど）
      split_pattern <- "(?<=\\s|,|-)"
      words <- unlist(strsplit(part, split_pattern, perl = TRUE))

      temp <- ""
      temp_list <- list()

      for (word in words) {
        if (nchar(temp) + nchar(word) <= max_length) {
          temp <- paste0(temp, word)
        } else {
          temp_list <- append(temp_list, list(trimws(temp)))
          temp <- word
        }
      }
      if (nchar(temp) > 0) {
        temp_list <- append(temp_list, list(trimws(temp)))
      }

      result <- append(result, temp_list)
    }
  }

  return(result)
}

get_max_element_counts <- function(...) {
  lists <- list(...)

  lengths <- sapply(lists, length)
  if (length(unique(lengths)) != 1) {
    stop("all lists must have the same length")
  }

  n <- lengths[1]
  result <- numeric(n)

  for (i in seq_len(n)) {
    counts <- sapply(lists, function(lst) {
      elem <- lst[[i]]
      if (is.list(elem)) {
        length(elem)
      } else {
        1
      }
    })
    result[i] <- max(counts)
  }

  return(result)
}

pad_list_elements <- function(list_input, num_vector) {
  if (length(list_input) != length(num_vector)) {
    stop("list and length vector do not match")
  }

  result <- vector("list", length(list_input))

  for (i in seq_along(list_input)) {
    elem <- list_input[[i]]

    if (!is.list(elem)) {
      elem <- list(elem)
    }

    pad_count <- num_vector[i] - length(elem)

    if (pad_count > 0) {
      elem <- c(elem, rep("", pad_count))
    }

    result[[i]] <- elem
  }

  return(result)
}


# ── The glue, rewritten in base R ────────────────────────────────────────────

# ydisctools::catx() -- join with `sep`, skipping missing and empty values.
# Named apart from rtfreporter's own catx() (#360) so the package function is
# not masked during the test run, and left NON-trimming, which is what
# ydisctools does: rtfreporter's strips blanks around each value, as SAS CATX
# does.  The #356 data carries no padding, so the two agree on it.
.ydisc_catx <- function(sep, ...) {
  parts <- lapply(list(...), function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  n <- max(vapply(parts, length, integer(1L)))
  vapply(seq_len(n), function(i) {
    p <- vapply(parts, function(v) v[[i]], character(1L))
    paste(p[nzchar(p)], collapse = sep)
  }, character(1L))
}

# The pipeline of Discussion #356, up to `raw_df` (the unnested body) and then
# `result` (that body with one blank row bound on top).
#
#   `cols`  -- named list: output column -> character vector of source columns,
#              in the order they are printed.
#   `wrap`  -- named numeric: output column -> its `split_string()` width.
#              Columns not named here are never wrapped, as in the source.
.manual_listing <- function(data, cols, wrap, blank_first = TRUE) {
  nm <- names(cols)

  # mutate(COL01 = catx("/", DISPTPD, BRCA, HIST), ...)
  joined <- lapply(nm, function(k) {
    do.call(.ydisc_catx, c(list("/"), unname(as.list(data[cols[[k]]]))))
  })
  names(joined) <- nm

  # mutate(COL01 = lapply(COL01, split_string, 22), ...)
  split_cols <- lapply(nm, function(k) {
    if (is.null(wrap[[k]])) as.list(joined[[k]]) else {
      lapply(joined[[k]], split_string, wrap[[k]])
    }
  })
  names(split_cols) <- nm

  # mutate(ROW_NUM = get_max_element_counts(<the wrapped columns>))
  wrapped <- split_cols[intersect(nm, names(wrap))]
  row_num <- do.call(get_max_element_counts, unname(wrapped))

  # mutate(across(everything(), ~ pad_list_elements(.x, ROW_NUM + 1)))
  padded <- lapply(split_cols, function(cl) pad_list_elements(cl, row_num + 1))
  names(padded) <- nm

  # unnest_longer(everything())
  out <- lapply(padded, function(cl) as.character(unlist(cl, use.names = FALSE)))
  body <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)

  # The S00 / S01 / ... gutter columns, interleaved between the printed ones.
  n_out <- length(nm)
  cells <- list()
  for (j in seq_len(n_out)) {
    cells[[length(cells) + 1L]] <- body[[nm[j]]]
    names(cells)[length(cells)]  <- nm[j]
    if (j < n_out) {
      cells[[length(cells) + 1L]] <- rep(NA_character_, nrow(body))
      names(cells)[length(cells)]  <- paste0("S", sprintf("%02d", j))
    }
  }
  res <- as.data.frame(cells, stringsAsFactors = FALSE, check.names = FALSE)

  # result <- bind_rows(df_blank, raw_df)
  if (isTRUE(blank_first)) {
    blank <- res[1L, , drop = FALSE]
    blank[] <- NA_character_
    res <- rbind(blank, res)
  }
  rownames(res) <- NULL
  res
}


# ── Test data ────────────────────────────────────────────────────────────────

# A synthetic ADSL shaped like the one Discussion #356 lists from: the same
# variables, and values chosen to exercise every branch of the reshaping --
# a subject id that has to wrap, cells that wrap at the separator only, cells
# whose piece is still too long and wraps again at a word boundary, missing
# values in the middle and at the end of a joined column, and one record whose
# joined column is empty throughout.
#
# Deliberately absent: a single token longer than its column's width, and an
# embedded "\n".  The verbatim rule and rtfreporter's differ there on purpose
# (see test-listing-vs-manual.R), so the shared cases are kept clean.
.listing_adsl <- function() {
  data.frame(
    USUBJID  = c("63016-204-1015", "63016-204-1023", "63016-205-100028",
                 "63016-206-1034", "63016-206-1045", "63016-207-1052"),
    DISPTPD  = c("COMPLETED", "COMPLETED", "DISCONTINUED",
                 "ONGOING", "COMPLETED", "DISCONTINUED"),
    BRCA     = c("BRCA1", NA, "BRCA2", "BRCA1", NA, NA),
    HIST     = c("ADENOCARCINOMA", "SQUAMOUS CELL CARCINOMA OF THE LUNG",
                 "SMALL CELL", "HIGH GRADE SEROUS CARCINOMA",
                 "ADENOCARCINOMA", NA),
    INIDGCAT = c("12.5", "36.2", "4.1", "22.0", "8.7", NA),
    STAGE    = c("IIIB", "IV", "IIIA", "IV", "IIB", "IV"),
    HISTGRD  = c("GRADE 3", "GRADE 2", "GRADE 3", "GRADE 1", NA, "GRADE 2"),
    PRRAD    = c("Y", "N", "Y", "N", "Y", NA),
    PRANTNM2 = c("2", "1", "3", "1", "2", NA),
    CMBRFST  = c("PARTIAL RESPONSE", "STABLE DISEASE", "COMPLETE RESPONSE",
                 "PROGRESSIVE DISEASE", "PARTIAL RESPONSE", NA),
    CMBRLST  = c("STABLE DISEASE", "PROGRESSIVE DISEASE", "STABLE DISEASE",
                 NA, "STABLE DISEASE", NA),
    PRSRG    = c("Y", "Y", "N", "Y", "N", NA),
    PPLATFI  = c(12.34, 6.5, 18.02, 3.75, 24.1, NA),
    PLATFI   = c(9.87, 4.25, 11.5, 2.5, 19.75, NA),
    ECOGPS   = c("0", "1", "1", "0", "2", NA),
    FOLREVAL = c("75", "120", "45", "200", "60", NA),
    stringsAsFactors = FALSE
  )
}

# The nine printed columns of that listing, source columns and order as in
# Discussion #356.
.listing_cols_356 <- function() {
  list(
    USUBJID = "USUBJID",
    COL01   = c("DISPTPD", "BRCA", "HIST"),
    COL02   = "INIDGCAT",
    COL03   = "STAGE",
    COL04   = c("HISTGRD", "PRRAD", "PRANTNM2"),
    COL05   = c("CMBRFST", "CMBRLST"),
    COL06   = "PRSRG",
    COL07   = c("PPLATFI", "PLATFI"),
    COL08   = "ECOGPS",
    COL09   = "FOLREVAL"
  )
}

# ... and the widths it wraps them at.
.listing_wrap_356 <- function() {
  list(USUBJID = 15, COL01 = 22, COL04 = 18, COL05 = 20, COL07 = 18)
}
