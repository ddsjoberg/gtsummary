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
  x$table_styling$text_format <-
    x$table_styling$text_format %>%
    dplyr::filter(.data$column %in% .env$cols_to_show) %>%
    dplyr::mutate(
      row_numbers =
        map(
          .data$rows,
          \(rows) .rows_expr_to_row_numbers(
            table_body, rows,
            return_when_null = seq_len(n_row_body)
          )
        )
    ) %>%
    dplyr::select(-"rows") %>%
    tidyr::unnest("row_numbers") %>%
    dplyr::group_by(.data$column, .data$row_numbers, .data$format_type) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::filter(.data$undo_text_format == FALSE) %>%
    # dropping undoing cmds
    dplyr::group_by(.data$column, .data$format_type) %>%
    tidyr::nest(row_numbers = "row_numbers") %>%
    dplyr::mutate(row_numbers = map(.data$row_numbers, ~ unlist(.x) %>% unname())) %>%
    dplyr::select("column", "row_numbers", everything()) %>%
    dplyr::ungroup()

  # source_note ----------------------------------------------------------------
  x$table_styling$source_note <-
    x$table_styling$source_note |>
    dplyr::filter(.data$remove == FALSE)

  # indentation ----------------------------------------------------------------
  x$table_styling$indent <-
    x$table_styling$indent %>%
    dplyr::filter(.data$column %in% .env$cols_to_show) %>%
    dplyr::mutate(
      row_numbers =
        map(
          .data$rows,
          \(rows) .rows_expr_to_row_numbers(
            table_body, rows,
            return_when_null = seq_len(n_row_body)
          )
        )
    ) %>%
    dplyr::select(-"rows") %>%
    tidyr::unnest("row_numbers") %>%
    dplyr::group_by(.data$column, .data$row_numbers) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::select("column", "row_numbers", "n_spaces") %>%
    dplyr::ungroup() %>%
    tidyr::nest(row_numbers = "row_numbers") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(row_numbers = unlist(.data$row_numbers) %>% unname() %>% list()) %>%
    dplyr::ungroup() |>
    dplyr::filter(.data$n_spaces != 0)

  # fmt_missing ----------------------------------------------------------------
  x$table_styling$fmt_missing <-
    x$table_styling$fmt_missing %>%
    dplyr::filter(.data$column %in% .env$cols_to_show) %>%
    dplyr::mutate(
      row_numbers = map(.data$rows, \(rows) .rows_expr_to_row_numbers(table_body, rows))
    ) %>%
    dplyr::select(-"rows") %>%
    tidyr::unnest("row_numbers") %>%
    dplyr::group_by(.data$column, .data$row_numbers) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::select("column", "row_numbers", "symbol") %>%
    dplyr::ungroup() %>%
    tidyr::nest(row_numbers = "row_numbers") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(row_numbers = unlist(.data$row_numbers) %>% unname() %>% list()) %>%
    dplyr::ungroup()

  # spanning_header ------------------------------------------------------------
  x$table_styling$spanning_header <-
    x$table_styling$spanning_header |>
    dplyr::mutate(
      # this is a hold-over from old syntax where NA removed headers
      remove = ifelse(is.na(.data$spanning_header), TRUE, .data$remove),
    ) |>
    # within a column and level, utilize the most recently added
    dplyr::filter(.by = c("column", "level"), dplyr::n() == dplyr::row_number()) |>
    # finally, remove the row if it's marked for removal or if the column is not printed in final table
    dplyr::filter(!remove, .data$column %in% .env$cols_to_show) |>
    dplyr::arrange(.data$level)

  if (nrow(x$table_styling$spanning_header) > 0L &&
      !setequal(unique(x$table_styling$spanning_header$level),
               seq_len(max(x$table_styling$spanning_header$level)))) {
    max_level <- max(x$table_styling$spanning_header$level)
    missing_lvls <- seq_len(max_level) |>
      setdiff(unique(x$table_styling$spanning_header$level))

    cli::cli_abort(
      c("!" = "There is an error in the spanning headers structure.",
        "!" = "Each spanning header level must be defined, that is, no levels may be skipped.",
        "i" = "The {cli::qty(length(missing_lvls))} spanning header{?s} for level{?s}
        {.val {missing_lvls}} {cli::qty(length(missing_lvls))} {?is/are} not present,
        but level {.val {max_level}} is present."),
      call = get_cli_abort_call()
    )
  }

  # footnote_header ------------------------------------------------------------
  x$table_styling$footnote_header <-
    x$table_styling$footnote_header |>
    dplyr::mutate(
      # this is a hold-over from old syntax where NA removed footnotes.
      remove = ifelse(is.na(.data$footnote), TRUE, .data$remove),
    ) |>
    # within a column, if a later entry contains `replace=TRUE` or `remove=TRUE`, then mark the row for removal
    .filter_row_with_subsequent_replace_or_removal() |>
    # finally, remove the row if it's marked for removal or if the column is not printed in final table
    dplyr::filter(!remove, .data$column %in% .env$cols_to_show)

  # footnote_body --------------------------------------------------------------
  x$table_styling$footnote_body <-
    x$table_styling$footnote_body |>
    dplyr::mutate(
      remove = ifelse(is.na(.data$footnote), TRUE, .data$remove), # this is a hold-over from pre-v2.0.0 syntax where NA removed footnotes.
      # convert rows predicate expression to row numbers
      row_numbers =
        map(
          .data$rows,
          \(rows) .rows_expr_to_row_numbers(table_body, rows)
        )
    ) |>
    tidyr::unnest(cols = "row_numbers") |>
    # within a column/row, if a later entry contains `replace=TRUE` or `remove=TRUE`, then mark the row for removal
    .filter_row_with_subsequent_replace_or_removal() |>
    #finally, remove the row if it's marked for removal or if the column is not printed in final table
    dplyr::filter(!remove, .data$column %in% .env$cols_to_show) |>
    dplyr::select(all_of(c("column", "row_numbers", "text_interpret", "footnote"))) |>
    dplyr::mutate(row_numbers = as.integer(.data$row_numbers)) # when there are no body footnotes, this ensures expected type/class

  # footnote_spanning_header ---------------------------------------------------
  x$table_styling$footnote_spanning_header <-
    x$table_styling$footnote_spanning_header |>
    dplyr::mutate(
      # this is a hold-over from old syntax where NA removed footnotes.
      remove = ifelse(is.na(.data$footnote), TRUE, .data$remove),
    ) |>
    # within a column/level, if a later entry contains `replace=TRUE` or `remove=TRUE`, then mark the row for removal
    .filter_row_with_subsequent_replace_or_removal() |>
    # finally, remove the row if it's marked for removal or if the column is not printed in final table
    dplyr::filter(!remove, .data$column %in% .env$cols_to_show)

  # abbreviation ---------------------------------------------------------------
  abbreviation_cols <-
    cols_to_show |>
    union(discard(x$table_styling$cols_merge$pattern, is.na) |> .extract_glue_elements()) |>
    union(NA_character_)
  x$table_styling$abbreviation <-
    x$table_styling$abbreviation |>
    dplyr::filter(.data$column %in% .env$abbreviation_cols) |>
    dplyr::slice_tail(n = 1L, by = "abbreviation") |>
    dplyr::arrange(.data$abbreviation)

  # fmt_fun --------------------------------------------------------------------
  x$table_styling$fmt_fun <-
    x$table_styling$fmt_fun %>%
    dplyr::mutate(
      row_numbers =
        map(
          .data$rows,
          \(rows) .rows_expr_to_row_numbers(
            table_body, rows,
            return_when_null = seq_len(n_row_body)
          )
        )
    ) %>%
    dplyr::select(-"rows") %>%
    tidyr::unnest("row_numbers") %>%
    dplyr::group_by(.data$column, .data$row_numbers) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::nest(row_numbers = "row_numbers") %>%
    dplyr::mutate(row_numbers = map(.data$row_numbers, ~ unlist(.x) %>% unname()))

  # post_fmt_fun --------------------------------------------------------------------
  x$table_styling$post_fmt_fun <-
    x$table_styling$post_fmt_fun %>%
    dplyr::mutate(
      row_numbers =
        map(
          .data$rows,
          \(rows) .rows_expr_to_row_numbers(
            table_body, rows,
            return_when_null = seq_len(n_row_body)
          )
        )
    ) %>%
    dplyr::select(-"rows") %>%
    tidyr::unnest("row_numbers") %>%
    dplyr::group_by(.data$column, .data$row_numbers) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::nest(row_numbers = "row_numbers") %>%
    dplyr::mutate(row_numbers = map(.data$row_numbers, ~ unlist(.x) %>% unname()))

  # cols_merge -----------------------------------------------------------------
  x$table_styling$cols_merge <-
    x$table_styling$cols_merge %>%
    dplyr::group_by(.data$column) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n(), !is.na(.data$pattern)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      row_numbers =
        switch(nrow(.) == 0,
          integer(0)
        ) %||%
          .rows_expr_to_row_numbers(
            x$table_body, .data$rows,
            return_when_null = seq_len(nrow(x$table_body))
          ) %>%
          list(),
    ) %>%
    dplyr::select(-"rows", rows = "row_numbers")

  class(x) <- "list"
  x
}


# this function processes the footnotes and removes footnotes that have
# later been replaced or removed from the table
.filter_row_with_subsequent_replace_or_removal <- function(x) {
  if (nrow(x) == 0L) return(x)

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
