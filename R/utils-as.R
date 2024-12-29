# takes a table_body and a character rows expression, and returns the resulting row numbers
.rows_expr_to_row_numbers <- function(table_body, rows, return_when_null = NA) {
  rows_evaluated <- rlang::eval_tidy(rows, data = table_body)

  # if a single lgl value, then expand it to the length of the tabel_body
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
  # text_format ----------------------------------------------------------------
  x$table_styling$text_format <-
    x$table_styling$text_format %>%
    dplyr::filter(.data$column %in% .cols_to_show(x)) %>%
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
    dplyr::filter(.data$column %in% .cols_to_show(x)) %>%
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
    dplyr::filter(.data$column %in% .cols_to_show(x)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      row_numbers =
        switch(nrow(.) == 0,
          integer(0)
        ) %||%
          .rows_expr_to_row_numbers(x$table_body, .data$rows) %>% list(),
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
    dplyr::filter(!remove, .data$column %in% x$table_styling$header$column[!x$table_styling$header$hide]) |>
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
    dplyr::filter(!remove, .data$column %in% x$table_styling$header$column[!x$table_styling$header$hide])

  # footnote_body --------------------------------------------------------------
  x$table_styling$footnote_body <-
    x$table_styling$footnote_body |>
    dplyr::mutate(
      remove = ifelse(is.na(.data$footnote), TRUE, .data$remove), # this is a hold-over from pre-v2.0.0 syntax where NA removed footnotes.
      # convert rows predicate expression to row numbers
      row_numbers =
        map(
          .data$rows,
          \(rows) .rows_expr_to_row_numbers(x$table_body, rows)
        )
    ) |>
    tidyr::unnest(cols = "row_numbers") |>
    # within a column/row, if a later entry contains `replace=TRUE` or `remove=TRUE`, then mark the row for removal
    .filter_row_with_subsequent_replace_or_removal() |>
    #finally, remove the row if it's marked for removal or if the column is not printed in final table
    dplyr::filter(!remove, .data$column %in% x$table_styling$header$column[!x$table_styling$header$hide]) |>
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
    dplyr::filter(!remove, .data$column %in% x$table_styling$header$column[!x$table_styling$header$hide])

  # abbreviation ---------------------------------------------------------------
  abbreviation_cols <-
    x$table_styling$header$column[!x$table_styling$header$hide] |>
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
    # filter(.data$column %in% .cols_to_show(x)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      row_numbers =
        switch(nrow(.) == 0,
          integer(0)
        ) %||%
          .rows_expr_to_row_numbers(x$table_body, .data$rows,
            return_when_null = seq_len(nrow(x$table_body))
          ) %>% list()
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

  x
}


# this function processes the footnotes and removes footnotes that have
# later been replaced or removed from the table
.filter_row_with_subsequent_replace_or_removal <- function(x) {
  if (nrow(x) == 0L) return(x)

  # within a column/row, if a later entry contains `replace=TRUE` or `remove=TRUE`, then mark the row for removal
  dplyr::filter(
    .data = x,
    .by = any_of(c("column", "level", "row_numbers")),
    !unlist(
      pmap(
        list(.data$replace, .data$remove, dplyr::row_number()),
        function(row_replace, row_remove, row_number) {
          # if this is the last row in the group, there will be now removal indications below
          if (row_number == dplyr::n()) return(FALSE)
          # if a subsequent call to replace or remove a footnote appear below,
          # then the current row can be deleted.
          any(.data$replace[seq(row_number + 1L, dplyr::n())]) ||
            any(.data$remove[seq(row_number + 1L, dplyr::n())])
        }
      )
    )
  )
}

# this function orders the footnotes by where they first appear in the table,
# and assigns them an sequential ID
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



# this function takes a list expressions and evaluates them with a `%>%` between them
.eval_list_of_exprs <- function(exprs, env = rlang::caller_env()) {
  exprs %>%
    # removing NULL elements
    unlist() %>%
    compact() %>%
    # concatenating expressions with %>% between each of them
    reduce(function(x, y) rlang::inject(!!x %>% !!y, env = env))
}
