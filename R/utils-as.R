# takes a table_body and a character rows expression, and returns the resulting row numbers
.rows_expr_to_row_numbers <- function(table_body, rows, return_when_null = NA) {
  rows_evaluated <- rlang::eval_tidy(rows, data = table_body)

  # if a single lgl value, then expand it to the length of the table_body
  if (is_scalar_logical(rows_evaluated)) {
    rows_evaluated <- rep_len(rows_evaluated, length.out = nrow(table_body))
  }

  if (is.null(rows_evaluated)) {
    return(return_when_null)
  }
  which(rows_evaluated)
}

.cols_to_show <- function(x) {
  x$table_styling$header %>%
    dplyr::filter(!.data$hide) %>%
    dplyr::pull("column")
}


# 1. Converts row expressions to row numbers, and only keeps the most recent.
# 2. Deletes NA footnote, text_formatting undoings, etc. as they will not be used


#' Object Convert Helper
#'
#' Ahead of a gtsummary object being converted to an output type,
#' each logical expression saved in `x$table_styling` is converted to a
#' list of row numbers.
#'
#' @param x a gtsummary object
#'
#' @return a gtsummary object
#' @keywords internal
#' @export
#'
#' @examples
#' tbl <-
#'   trial %>%
#'   tbl_summary(include = c(age, grade)) %>%
#'   .table_styling_expr_to_row_number()
.table_styling_expr_to_row_number <- function(x) {
  set_cli_abort_call()
  # values reused across the styling tables below. The header is not modified in
  # this function, so the set of visible columns is stable throughout; computing
  # it (and the table body row count) once avoids repeated re-derivation.
  cols_to_show <- .cols_to_show(x)
  table_body <- x$table_body
  n_row_body <- nrow(table_body)

  # text_format ----------------------------------------------------------------
  tf <- x$table_styling$text_format
  if (nrow(tf) == 0L) {
    x$table_styling$text_format <- dplyr::tibble(
      column = character(),
      row_numbers = list(),
      format_type = character(),
      undo_text_format = logical()
    )
  } else {
    tf <- tf[tf$column %in% cols_to_show, , drop = FALSE]
    if (nrow(tf) == 0L) {
      x$table_styling$text_format <- dplyr::tibble(
        column = character(),
        row_numbers = list(),
        format_type = character(),
        undo_text_format = logical()
      )
    } else {
      rows_list <- lapply(
        tf$rows,
        function(r) .rows_expr_to_row_numbers(table_body, r, return_when_null = seq_len(n_row_body))
      )
      lens <- vapply(rows_list, length, integer(1L))
      if (sum(lens) == 0L) {
        x$table_styling$text_format <- dplyr::tibble(
          column = character(),
          row_numbers = list(),
          format_type = character(),
          undo_text_format = logical()
        )
      } else {
        col_rep <- rep(tf$column, lens)
        row_rep <- unlist(rows_list, use.names = FALSE)
        fmt_rep <- rep(tf$format_type, lens)
        undo_rep <- rep(tf$undo_text_format, lens)

        # Keep last occurrence per (column, row_numbers, format_type)
        keep <- !duplicated(paste(col_rep, row_rep, fmt_rep, sep = "\r"), fromLast = TRUE)
        col_rep <- col_rep[keep]
        row_rep <- row_rep[keep]
        fmt_rep <- fmt_rep[keep]
        undo_rep <- undo_rep[keep]

        # Filter out undo_text_format == TRUE
        keep2 <- !undo_rep
        col_rep <- col_rep[keep2]
        row_rep <- row_rep[keep2]
        fmt_rep <- fmt_rep[keep2]

        if (length(col_rep) == 0L) {
          x$table_styling$text_format <- dplyr::tibble(
            column = character(),
            row_numbers = list(),
            format_type = character(),
            undo_text_format = logical()
          )
        } else {
          grp_key <- paste(col_rep, fmt_rep, sep = "\r")
          unique_keys <- unique(grp_key)
          split_rows <- split(row_rep, factor(grp_key, levels = unique_keys))
          first_idx <- match(unique_keys, grp_key)

          x$table_styling$text_format <- dplyr::tibble(
            column = col_rep[first_idx],
            row_numbers = unname(split_rows),
            format_type = fmt_rep[first_idx],
            undo_text_format = rep(FALSE, length(unique_keys))
          )
        }
      }
    }
  }

  # source_note ----------------------------------------------------------------
  x$table_styling$source_note <-
    if (nrow(x$table_styling$source_note) == 0L) {
      x$table_styling$source_note
    } else {
      x$table_styling$source_note[x$table_styling$source_note$remove == FALSE, , drop = FALSE]
    }

  # indentation ----------------------------------------------------------------
  ind <- x$table_styling$indent
  if (nrow(ind) == 0L) {
    x$table_styling$indent <- dplyr::tibble(
      column = character(),
      n_spaces = integer(),
      row_numbers = list()
    )
  } else {
    ind <- ind[ind$column %in% cols_to_show, , drop = FALSE]
    if (nrow(ind) == 0L) {
      x$table_styling$indent <- dplyr::tibble(
        column = character(),
        n_spaces = integer(),
        row_numbers = list()
      )
    } else {
      rows_list <- lapply(
        ind$rows,
        function(r) .rows_expr_to_row_numbers(table_body, r, return_when_null = seq_len(n_row_body))
      )
      lens <- vapply(rows_list, length, integer(1L))
      if (sum(lens) == 0L) {
        x$table_styling$indent <- dplyr::tibble(
          column = character(),
          n_spaces = integer(),
          row_numbers = list()
        )
      } else {
        col_rep <- rep(ind$column, lens)
        row_rep <- unlist(rows_list, use.names = FALSE)
        spaces_rep <- rep(ind$n_spaces, lens)

        # Keep last occurrence per (column, row_numbers)
        keep <- !duplicated(paste(col_rep, row_rep, sep = "\r"), fromLast = TRUE)
        col_rep <- col_rep[keep]
        row_rep <- row_rep[keep]
        spaces_rep <- spaces_rep[keep]

        # Filter out n_spaces == 0
        keep2 <- spaces_rep != 0L
        col_rep <- col_rep[keep2]
        row_rep <- row_rep[keep2]
        spaces_rep <- spaces_rep[keep2]

        if (length(col_rep) == 0L) {
          x$table_styling$indent <- dplyr::tibble(
            column = character(),
            n_spaces = integer(),
            row_numbers = list()
          )
        } else {
          grp_key <- paste(col_rep, spaces_rep, sep = "\r")
          unique_keys <- unique(grp_key)
          split_rows <- split(row_rep, factor(grp_key, levels = unique_keys))
          first_idx <- match(unique_keys, grp_key)

          x$table_styling$indent <- dplyr::tibble(
            column = col_rep[first_idx],
            n_spaces = spaces_rep[first_idx],
            row_numbers = unname(split_rows)
          )
        }
      }
    }
  }

  # fmt_missing ----------------------------------------------------------------
  fm <- x$table_styling$fmt_missing
  if (nrow(fm) == 0L) {
    x$table_styling$fmt_missing <- dplyr::tibble(
      column = character(),
      symbol = character(),
      row_numbers = list()
    )
  } else {
    fm <- fm[fm$column %in% cols_to_show, , drop = FALSE]
    if (nrow(fm) == 0L) {
      x$table_styling$fmt_missing <- dplyr::tibble(
        column = character(),
        symbol = character(),
        row_numbers = list()
      )
    } else {
      rows_list <- lapply(
        fm$rows,
        function(r) .rows_expr_to_row_numbers(table_body, r)
      )
      lens <- vapply(rows_list, length, integer(1L))
      if (sum(lens) == 0L) {
        x$table_styling$fmt_missing <- dplyr::tibble(
          column = character(),
          symbol = character(),
          row_numbers = list()
        )
      } else {
        col_rep <- rep(fm$column, lens)
        row_rep <- unlist(rows_list, use.names = FALSE)
        symbol_rep <- rep(fm$symbol, lens)

        keep <- !duplicated(paste(col_rep, row_rep, sep = "\r"), fromLast = TRUE)
        col_rep <- col_rep[keep]
        row_rep <- row_rep[keep]
        symbol_rep <- symbol_rep[keep]

        grp_key <- paste(col_rep, symbol_rep, sep = "\r")
        unique_keys <- unique(grp_key)
        split_rows <- split(row_rep, factor(grp_key, levels = unique_keys))
        first_idx <- match(unique_keys, grp_key)

        x$table_styling$fmt_missing <- dplyr::tibble(
          column = col_rep[first_idx],
          symbol = symbol_rep[first_idx],
          row_numbers = unname(split_rows)
        )
      }
    }
  }

  # spanning_header ------------------------------------------------------------
  sh <- x$table_styling$spanning_header
  if (nrow(sh) > 0L) {
    sh$remove <- ifelse(is.na(sh$spanning_header), TRUE, sh$remove)
    # keep last entry per (column, level)
    keep <- !duplicated(paste(sh$column, sh$level, sep = "\r"), fromLast = TRUE)
    sh <- sh[keep, , drop = FALSE]
    sh <- sh[!sh$remove & sh$column %in% cols_to_show, , drop = FALSE]
    if (nrow(sh) > 0L) {
      sh <- sh[order(sh$level), , drop = FALSE]
    }
    x$table_styling$spanning_header <- sh

    if (nrow(sh) > 0L &&
      !setequal(unique(sh$level), seq_len(max(sh$level)))) {
      max_level <- max(sh$level)
      missing_lvls <- seq_len(max_level) |>
        setdiff(unique(sh$level))

      cli::cli_abort(
        c(
          "!" = "There is an error in the spanning headers structure.",
          "!" = "Each spanning header level must be defined, that is, no levels may be skipped.",
          "i" = "The {cli::qty(length(missing_lvls))} spanning header{?s} for level{?s}
          {.val {missing_lvls}} {cli::qty(length(missing_lvls))} {?is/are} not present,
          but level {.val {max_level}} is present."
        ),
        call = get_cli_abort_call()
      )
    }
  }

  # footnote_header ------------------------------------------------------------
  fh <- x$table_styling$footnote_header
  if (nrow(fh) > 0L) {
    fh$remove <- ifelse(is.na(fh$footnote), TRUE, fh$remove)
    fh <- .filter_row_with_subsequent_replace_or_removal(fh)
    fh <- fh[!fh$remove & fh$column %in% cols_to_show, , drop = FALSE]
    x$table_styling$footnote_header <- fh
  }

  # footnote_body --------------------------------------------------------------
  fb <- x$table_styling$footnote_body
  if (nrow(fb) == 0L) {
    x$table_styling$footnote_body <- dplyr::tibble(
      column = character(),
      row_numbers = integer(),
      text_interpret = character(),
      footnote = character()
    )
  } else {
    fb$remove <- ifelse(is.na(fb$footnote), TRUE, fb$remove)
    rows_list <- lapply(fb$rows, function(r) .rows_expr_to_row_numbers(table_body, r))
    lens <- vapply(rows_list, length, integer(1L))
    if (sum(lens) == 0L) {
      x$table_styling$footnote_body <- dplyr::tibble(
        column = character(),
        row_numbers = integer(),
        text_interpret = character(),
        footnote = character()
      )
    } else {
      col_rep <- rep(fb$column, lens)
      row_rep <- as.integer(unlist(rows_list, use.names = FALSE))
      fn_rep <- rep(fb$footnote, lens)
      ti_rep <- rep(fb$text_interpret, lens)
      repl_rep <- rep(fb$replace, lens)
      rem_rep <- rep(fb$remove, lens)

      unnested_fb <- dplyr::tibble(
        column = col_rep,
        row_numbers = row_rep,
        footnote = fn_rep,
        text_interpret = ti_rep,
        replace = repl_rep,
        remove = rem_rep
      )
      unnested_fb <- .filter_row_with_subsequent_replace_or_removal(unnested_fb)
      unnested_fb <- unnested_fb[!unnested_fb$remove & unnested_fb$column %in% cols_to_show, , drop = FALSE]
      x$table_styling$footnote_body <- dplyr::tibble(
        column = unnested_fb$column,
        row_numbers = as.integer(unnested_fb$row_numbers),
        text_interpret = unnested_fb$text_interpret,
        footnote = unnested_fb$footnote
      )
    }
  }

  # footnote_spanning_header ---------------------------------------------------
  fsh <- x$table_styling$footnote_spanning_header
  if (nrow(fsh) > 0L) {
    fsh$remove <- ifelse(is.na(fsh$footnote), TRUE, fsh$remove)
    fsh <- .filter_row_with_subsequent_replace_or_removal(fsh)
    fsh <- fsh[!fsh$remove & fsh$column %in% cols_to_show, , drop = FALSE]
    x$table_styling$footnote_spanning_header <- fsh
  }

  # abbreviation ---------------------------------------------------------------
  abbreviation_cols <-
    cols_to_show |>
    union(discard(x$table_styling$cols_merge$pattern, is.na) |> .extract_glue_elements()) |>
    union(NA_character_)
  abbr <- x$table_styling$abbreviation
  if (nrow(abbr) > 0L) {
    abbr <- abbr[abbr$column %in% abbreviation_cols, , drop = FALSE]
    if (nrow(abbr) > 0L) {
      keep <- !duplicated(abbr$abbreviation, fromLast = TRUE)
      abbr <- abbr[keep, , drop = FALSE]
      abbr <- abbr[order(abbr$abbreviation, method = "radix"), , drop = FALSE]
    }
    x$table_styling$abbreviation <- abbr
  }

  # fmt_fun --------------------------------------------------------------------
  ff <- x$table_styling$fmt_fun
  if (nrow(ff) == 0L) {
    x$table_styling$fmt_fun <- dplyr::tibble(
      column = character(),
      fmt_fun = list(),
      row_numbers = list()
    )
  } else {
    rows_list <- lapply(
      ff$rows,
      function(r) .rows_expr_to_row_numbers(table_body, r, return_when_null = seq_len(n_row_body))
    )
    lens <- vapply(rows_list, length, integer(1L))
    if (sum(lens) == 0L) {
      x$table_styling$fmt_fun <- dplyr::tibble(
        column = character(),
        fmt_fun = list(),
        row_numbers = list()
      )
    } else {
      orig_idx <- rep(seq_len(nrow(ff)), lens)
      col_rep <- rep(ff$column, lens)
      row_rep <- unlist(rows_list, use.names = FALSE)

      keep <- !duplicated(paste(col_rep, row_rep, sep = "\r"), fromLast = TRUE)
      orig_idx <- orig_idx[keep]
      row_rep <- row_rep[keep]

      sub_df <- dplyr::tibble(
        column = ff$column[orig_idx],
        fmt_fun = ff$fmt_fun[orig_idx]
      )
      grp <- vctrs::vec_group_loc(sub_df)
      res <- grp$key
      res$row_numbers <- lapply(grp$loc, function(i) row_rep[i])
      x$table_styling$fmt_fun <- res
    }
  }

  # post_fmt_fun ---------------------------------------------------------------
  pff <- x$table_styling$post_fmt_fun
  if (nrow(pff) == 0L) {
    x$table_styling$post_fmt_fun <- dplyr::tibble(
      column = character(),
      fmt_fun = list(),
      row_numbers = list()
    )
  } else {
    rows_list <- lapply(
      pff$rows,
      function(r) .rows_expr_to_row_numbers(table_body, r, return_when_null = seq_len(n_row_body))
    )
    lens <- vapply(rows_list, length, integer(1L))
    if (sum(lens) == 0L) {
      x$table_styling$post_fmt_fun <- dplyr::tibble(
        column = character(),
        fmt_fun = list(),
        row_numbers = list()
      )
    } else {
      orig_idx <- rep(seq_len(nrow(pff)), lens)
      col_rep <- rep(pff$column, lens)
      row_rep <- unlist(rows_list, use.names = FALSE)

      keep <- !duplicated(paste(col_rep, row_rep, sep = "\r"), fromLast = TRUE)
      orig_idx <- orig_idx[keep]
      row_rep <- row_rep[keep]

      sub_df <- dplyr::tibble(
        column = pff$column[orig_idx],
        fmt_fun = pff$fmt_fun[orig_idx]
      )
      grp <- vctrs::vec_group_loc(sub_df)
      res <- grp$key
      res$row_numbers <- lapply(grp$loc, function(i) row_rep[i])
      x$table_styling$post_fmt_fun <- res
    }
  }

  # cols_merge -----------------------------------------------------------------
  cm <- x$table_styling$cols_merge
  if (nrow(cm) == 0L) {
    x$table_styling$cols_merge <- dplyr::rowwise(
      dplyr::tibble(column = character(), pattern = character(), rows = list()),
      "column"
    )
  } else {
    cm <- cm[!is.na(cm$pattern), , drop = FALSE]
    if (nrow(cm) == 0L) {
      x$table_styling$cols_merge <- dplyr::rowwise(
        dplyr::tibble(column = character(), pattern = character(), rows = list()),
        "column"
      )
    } else {
      keep <- !duplicated(cm$column, fromLast = TRUE)
      cm <- cm[keep, , drop = FALSE]
      rows_list <- lapply(
        cm$rows,
        function(r) .rows_expr_to_row_numbers(table_body, r, return_when_null = seq_len(n_row_body))
      )
      res <- dplyr::tibble(
        column = cm$column,
        pattern = cm$pattern,
        rows = rows_list
      )
      x$table_styling$cols_merge <- dplyr::rowwise(res, "column")
    }
  }

  class(x) <- "list"
  x
}


# this function processes the footnotes and removes footnotes that have
# later been replaced or removed from the table
.filter_row_with_subsequent_replace_or_removal <- function(x) {
  if (nrow(x) == 0L) {
    return(x)
  }

  # within a column/row, if a later entry contains `replace=TRUE` or `remove=TRUE`, then mark the row for removal.
  # `rev(cumany(rev(.)))` is a suffix-OR (TRUE from the first flagged row to the
  # end of the group); `lead()` shifts it so a row is dropped only when a *later*
  # row is flagged. This is the vectorized equivalent of the previous per-row scan.
  dplyr::filter(
    .data = x,
    .by = any_of(c("column", "level", "row_numbers")),
    !dplyr::lead(
      dplyr::cumany(rev(.data$replace | .data$remove)) |> rev(),
      default = FALSE
    )
  )
}

# this function orders the footnotes by where they first appear in the table,
# and assigns them a sequential ID
.number_footnotes <- function(x, type, start_with = 0L) {
  # if empty, return empty data frame
  if (nrow(type) == 0L) {
    return(dplyr::tibble(
      footnote_id = integer(), footnote = character(), column = character(),
      column_id = integer(), row_numbers = integer()
    ))
  }

  # adding the footnote number to assign to each of the footnotes
  dplyr::inner_join(
    x$table_styling$header |>
      select("column", column_id = "id") |>
      dplyr::filter(!is.na(.data$column_id)),
    type,
    by = "column"
  ) |>
    dplyr::arrange(dplyr::pick(any_of(c("column_id", "row_numbers")))) |>
    dplyr::group_by(.data$footnote) |>
    tidyr::nest() |>
    dplyr::ungroup() |>
    dplyr::mutate(footnote_id = dplyr::row_number() + .env$start_with) |>
    tidyr::unnest(cols = "data") |>
    dplyr::select(any_of(c("footnote_id", "footnote", "column", "column_id", "row_numbers")))
}



# resolve the ordered footnote reference symbols for a gtsummary object.
# precedence: value set via `modify_footnote_symbol()` > theme element > NULL.
# returns `NULL` when no custom symbols are set (engines use default numbering).
.resolve_footnote_symbols <- function(x) {
  x$table_styling$footnote_symbol %||%
    get_theme_element("pkgwide-chr:footnote_symbol", default = NULL)
}

# given a vector of 1-based footnote ids and an ordered symbol vector, return the
# symbol assigned to each id, recycling the symbols when ids exceed their length.
.map_footnote_symbols <- function(footnote_id, symbol) {
  idx <- ((footnote_id - 1L) %% length(symbol)) + 1L
  symbol[idx]
}

# map a validated `text_interpret` value ("md", "html", or "none") to the
# function string applied by the print engines. "none" maps to `identity` so
# the text is passed through uninterpreted (gt has no `none` interpreter). (#1987)
.interpret_fun <- function(text_interpret) {
  if (isTRUE(text_interpret == "none")) {
    return("identity")
  }
  paste0("gt::", text_interpret)
}

# this function takes a list expressions and evaluates them with a `%>%` between them
.eval_list_of_exprs <- function(exprs, env = rlang::caller_env()) {
  exprs %>%
    # removing NULL elements
    unlist() %>%
    compact() %>%
    # concatenating expressions with %>% between each of them
    reduce(function(x, y) rlang::inject(!!x %>% !!y, env = env))
}
