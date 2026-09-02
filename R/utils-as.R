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

# converts the `rows` list column of a styling table into a list of row-number
# vectors, one element per row of the styling table
.rows_expr_list <- function(df, table_body, return_when_null = NA) {
  lapply(
    df$rows,
    \(rows) .rows_expr_to_row_numbers(table_body, rows, return_when_null = return_when_null)
  )
}

# `TRUE` for the last occurrence of each distinct combination of `...`, i.e. the
# vectorized form of `group_by(...) |> filter(row_number() == n())`. Grouping on
# an integer ID rather than a pasted string key keeps this independent of
# whatever characters happen to appear in the column names.
.last_of_each <- function(...) {
  !duplicated(vctrs::vec_group_id(vctrs::data_frame(...)), fromLast = TRUE)
}

# nests `row_numbers` under the distinct combinations of `keys`, in the order
# those combinations first appear. This is `tidyr::nest()` without the tibble
# metadata reconstruction, and it accepts list columns as keys (`fmt_fun`).
# A zero-row `keys` yields the correctly typed zero-row result.
.nest_row_numbers <- function(keys, row_numbers) {
  grp <- vctrs::vec_group_loc(keys)
  res <- grp$key
  res$row_numbers <- lapply(grp$loc, \(loc) row_numbers[loc])
  res
}

# `fmt_fun` and `post_fmt_fun` share a schema and are reduced identically: the
# most recently added function wins within a column/row, and the result is
# nested by the (column, function) pair.
.fmt_fun_expr_to_row_number <- function(df, table_body, n_row_body) {
  rows_list <- .rows_expr_list(df, table_body, return_when_null = seq_len(n_row_body))
  lens <- lengths(rows_list)
  row_rep <- unlist(rows_list, use.names = FALSE)
  # `rows` is expanded before de-duplication, so carry an index back to the
  # original rows instead of the `fmt_fun` list column itself
  orig_idx <- rep(seq_len(nrow(df)), lens)
  keep <- which(.last_of_each(column = rep(df$column, lens), row = row_rep))

  .nest_row_numbers(
    dplyr::tibble(
      column = df$column[orig_idx[keep]],
      fmt_fun = df$fmt_fun[orig_idx[keep]]
    ),
    row_rep[keep]
  )
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

  # Each styling table below is expanded to one row per (column, row number),
  # reduced to the most recently added instruction, and then re-nested. Working
  # on plain vectors instead of grouped tibbles avoids the
  # unnest -> group_by -> filter -> nest cycle this function used to run for
  # every styling table on every conversion.
  #
  # Rows are always subset with `which()` rather than a bare logical: the
  # predicates below can be `NA` (e.g. a table assembled by `tbl_merge()` from
  # an object missing one of these columns), and base `[` keeps an `NA`
  # predicate as an all-`NA` row where `dplyr::filter()` dropped it.

  # text_format ----------------------------------------------------------------
  tf <- x$table_styling$text_format
  tf <- tf[which(tf$column %in% cols_to_show), , drop = FALSE]
  rows_list <- .rows_expr_list(tf, table_body, return_when_null = seq_len(n_row_body))
  lens <- lengths(rows_list)
  row_rep <- unlist(rows_list, use.names = FALSE)
  col_rep <- rep(tf$column, lens)
  fmt_rep <- rep(tf$format_type, lens)
  # within a column/row/format type the most recent instruction wins; the
  # undoings are then dropped, as there is nothing left for them to undo
  keep <- which(
    .last_of_each(column = col_rep, row = row_rep, format_type = fmt_rep) &
      !rep(tf$undo_text_format, lens)
  )
  nested <- .nest_row_numbers(
    dplyr::tibble(column = col_rep[keep], format_type = fmt_rep[keep]),
    row_rep[keep]
  )
  x$table_styling$text_format <- dplyr::tibble(
    column = nested$column,
    row_numbers = nested$row_numbers,
    format_type = nested$format_type,
    undo_text_format = rep(FALSE, nrow(nested))
  )

  # source_note ----------------------------------------------------------------
  sn <- x$table_styling$source_note
  x$table_styling$source_note <- sn[which(!sn$remove), , drop = FALSE]

  # indentation ----------------------------------------------------------------
  ind <- x$table_styling$indent
  ind <- ind[which(ind$column %in% cols_to_show), , drop = FALSE]
  rows_list <- .rows_expr_list(ind, table_body, return_when_null = seq_len(n_row_body))
  lens <- lengths(rows_list)
  row_rep <- unlist(rows_list, use.names = FALSE)
  col_rep <- rep(ind$column, lens)
  spaces_rep <- rep(ind$n_spaces, lens)
  # most recent instruction per column/row wins, then zero-width indents are
  # dropped since they have no effect on the rendered table
  keep <- which(.last_of_each(column = col_rep, row = row_rep) & spaces_rep != 0L)
  x$table_styling$indent <- .nest_row_numbers(
    dplyr::tibble(column = col_rep[keep], n_spaces = spaces_rep[keep]),
    row_rep[keep]
  )

  # fmt_missing ----------------------------------------------------------------
  fm <- x$table_styling$fmt_missing
  fm <- fm[which(fm$column %in% cols_to_show), , drop = FALSE]
  rows_list <- .rows_expr_list(fm, table_body)
  lens <- lengths(rows_list)
  row_rep <- unlist(rows_list, use.names = FALSE)
  col_rep <- rep(fm$column, lens)
  symbol_rep <- rep(fm$symbol, lens)
  keep <- which(.last_of_each(column = col_rep, row = row_rep))
  x$table_styling$fmt_missing <- .nest_row_numbers(
    dplyr::tibble(column = col_rep[keep], symbol = symbol_rep[keep]),
    row_rep[keep]
  )

  # spanning_header ------------------------------------------------------------
  sh <- x$table_styling$spanning_header
  # this is a hold-over from old syntax where NA removed headers
  sh$remove <- ifelse(is.na(sh$spanning_header), TRUE, sh$remove)
  # within a column and level, utilize the most recently added
  sh <- sh[.last_of_each(column = sh$column, level = sh$level), , drop = FALSE]
  # finally, remove the row if it's marked for removal or if the column is not printed in final table
  sh <- sh[which(!sh$remove & sh$column %in% cols_to_show), , drop = FALSE]
  x$table_styling$spanning_header <- sh[order(sh$level), , drop = FALSE]

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

  # footnote_header ------------------------------------------------------------
  fh <- x$table_styling$footnote_header
  # this is a hold-over from old syntax where NA removed footnotes.
  fh$remove <- ifelse(is.na(fh$footnote), TRUE, fh$remove)
  # within a column, if a later entry contains `replace=TRUE` or `remove=TRUE`, then mark the row for removal
  fh <- .filter_row_with_subsequent_replace_or_removal(fh)
  # finally, remove the row if it's marked for removal or if the column is not printed in final table
  x$table_styling$footnote_header <-
    fh[which(!fh$remove & fh$column %in% cols_to_show), , drop = FALSE]

  # footnote_body --------------------------------------------------------------
  fb <- x$table_styling$footnote_body
  # this is a hold-over from pre-v2.0.0 syntax where NA removed footnotes.
  fb$remove <- ifelse(is.na(fb$footnote), TRUE, fb$remove)
  rows_list <- .rows_expr_list(fb, table_body)
  lens <- lengths(rows_list)
  # `.filter_row_with_subsequent_replace_or_removal()` groups on row numbers, so
  # the predicates must be expanded to row numbers before it can be applied
  fb <- dplyr::tibble(
    column = rep(fb$column, lens),
    row_numbers = as.integer(unlist(rows_list, use.names = FALSE)),
    footnote = rep(fb$footnote, lens),
    text_interpret = rep(fb$text_interpret, lens),
    replace = rep(fb$replace, lens),
    remove = rep(fb$remove, lens)
  ) |>
    .filter_row_with_subsequent_replace_or_removal()
  keep <- which(!fb$remove & fb$column %in% cols_to_show)
  x$table_styling$footnote_body <- dplyr::tibble(
    column = fb$column[keep],
    row_numbers = fb$row_numbers[keep],
    text_interpret = fb$text_interpret[keep],
    footnote = fb$footnote[keep]
  )

  # footnote_spanning_header ---------------------------------------------------
  fsh <- x$table_styling$footnote_spanning_header
  # this is a hold-over from old syntax where NA removed footnotes.
  fsh$remove <- ifelse(is.na(fsh$footnote), TRUE, fsh$remove)
  # within a column/level, if a later entry contains `replace=TRUE` or `remove=TRUE`, then mark the row for removal
  fsh <- .filter_row_with_subsequent_replace_or_removal(fsh)
  # finally, remove the row if it's marked for removal or if the column is not printed in final table
  x$table_styling$footnote_spanning_header <-
    fsh[which(!fsh$remove & fsh$column %in% cols_to_show), , drop = FALSE]

  # abbreviation ---------------------------------------------------------------
  abbreviation_cols <-
    cols_to_show |>
    union(discard(x$table_styling$cols_merge$pattern, is.na) |> .extract_glue_elements()) |>
    union(NA_character_)
  abbr <- x$table_styling$abbreviation
  abbr <- abbr[which(abbr$column %in% abbreviation_cols), , drop = FALSE]
  abbr <- abbr[!duplicated(abbr$abbreviation, fromLast = TRUE), , drop = FALSE]
  # `method = "radix"` sorts in the C locale, matching `dplyr::arrange()`. Base
  # `order()` would otherwise collate in the user's locale, so the rendered
  # abbreviation order would vary from machine to machine.
  x$table_styling$abbreviation <-
    abbr[order(as.character(abbr$abbreviation), method = "radix"), , drop = FALSE]

  # fmt_fun / post_fmt_fun -----------------------------------------------------
  x$table_styling$fmt_fun <-
    .fmt_fun_expr_to_row_number(x$table_styling$fmt_fun, table_body, n_row_body)
  x$table_styling$post_fmt_fun <-
    .fmt_fun_expr_to_row_number(x$table_styling$post_fmt_fun, table_body, n_row_body)

  # cols_merge -----------------------------------------------------------------
  cm <- x$table_styling$cols_merge
  # The most recently added instruction per column wins. An `NA` pattern in that
  # position is the hold-over syntax for undoing a merge, so the reduction has to
  # happen *before* the `NA`s are dropped -- otherwise an older, undone pattern
  # would resurface.
  cm <- cm[.last_of_each(column = cm$column), , drop = FALSE]
  cm <- cm[which(!is.na(cm$pattern)), , drop = FALSE]
  x$table_styling$cols_merge <- dplyr::rowwise(
    dplyr::tibble(
      column = cm$column,
      pattern = cm$pattern,
      rows = .rows_expr_list(cm, table_body, return_when_null = seq_len(n_row_body))
    ),
    "column"
  )

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
