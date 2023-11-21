.convert_header_to_rows_one_column <- function(x, column) {
  if (column %in% c("footnote", "footnote_abbrev")) {
    x$table_styling[[column]] <-
      x$table_header %>%
      dplyr::select(all_of(c("column", .env$column))) %>%
      set_names(c("column", "footnote")) %>%
      dplyr::filter(!is.na(.data$footnote)) %>%
      dplyr::mutate(
        rows = list(expr(NULL)),
        text_interpret = "gt::md"
      ) %>%
      dplyr::select(all_of(c("column", "rows", "text_interpret", "footnote")))
  } else if (column %in% c("indent", "bold", "italic")) {
    x$table_styling$text_format <-
      x$table_header %>%
      dplyr::select(all_of(c("column", .env$column))) %>%
      dplyr::filter(!is.na(.data[[column]])) %>%
      set_names(c("column", "rows")) %>%
      dplyr::mutate(
        rows = map(.data$rows, ~ parse_expr(.x)),
        format_type = .env$column,
        undo_text_format = FALSE
      ) %>%
      dplyr::bind_rows(x$table_styling$text_format)
  } else if (column %in% "missing_emdash") {
    x$table_styling[["fmt_missing"]] <-
      x$table_header %>%
      dplyr::select(all_of(c("column", .env$column))) %>%
      dplyr::filter(!is.na(.data[[column]])) %>%
      set_names(c("column", "rows")) %>%
      dplyr::mutate(
        rows = map(.data$rows, ~ parse_expr(.x)),
        symbol = get_theme_element("tbl_regression-str:ref_row_text",
          default = "\U2014"
        )
      )
  } else if (column %in% "fmt_fun") {
    x$table_styling[[column]] <-
      x$table_header %>%
      dplyr::select(all_of(c("column", .env$column))) %>%
      dplyr::filter(!map_lgl(.data[[column]], is.null)) %>%
      dplyr::mutate(rows = list(expr(NULL))) %>%
      dplyr::select(all_of(c("column", "rows", .env$column)))
  }

  # return gtsummary table
  x
}

# takes a table_body and a character rows expression, and returns the resulting row numbers
.rows_expr_to_row_numbers <- function(table_body, rows, return_when_null = NA) {
  rows_evaluated <- rlang::eval_tidy(rows, data = table_body)

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

  # footnote -------------------------------------------------------------------
  x$table_styling$footnote <-
    .table_styling_expr_to_row_number_footnote(x, "footnote")

  # footnote_abbrev ------------------------------------------------------------
  x$table_styling$footnote_abbrev <-
    .table_styling_expr_to_row_number_footnote(x, "footnote_abbrev")

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

.table_styling_expr_to_row_number_footnote <- function(x, footnote_type) {
  df_clean <-
    x$table_styling[[footnote_type]] %>%
    dplyr::filter(.data$column %in% .cols_to_show(x))
  if (nrow(df_clean) == 0) {
    return(dplyr::tibble(
      column = character(0), tab_location = character(0), row_numbers = logical(0),
      text_interpret = character(0), footnote = character(0)
    ))
  }

  df_clean <-
    df_clean %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      row_numbers =
        switch(nrow(.) == 0,
          integer(0)
        ) %||%
          .rows_expr_to_row_numbers(x$table_body, .data$rows) %>% list(),
      tab_location = ifelse(identical(.data$row_numbers, NA), "header", "body")
    ) %>%
    dplyr::select(-"rows") %>%
    tidyr::unnest(cols = "row_numbers") %>%
    dplyr::group_by(.data$column, .data$tab_location, .data$row_numbers) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    # keeping the most recent addition
    dplyr::filter(!is.na(.data$footnote)) # keep non-missing additions

  if (footnote_type == "footnote_abbrev") {
    # order the footnotes by where they first appear in the table,
    df_clean <-
      df_clean %>%
      dplyr::inner_join(
        x$table_styling$header %>%
          select("column") %>%
          mutate(column_id = dplyr::row_number()),
        by = "column"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$tab_location), .data$column_id, .data$row_numbers) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(footnote = paste(unique(.data$footnote), collapse = ", "))
  }

  df_clean %>%
    dplyr::select(all_of(c("column", "tab_location", "row_numbers", "text_interpret", "footnote")))
}

# this function orders the footnotes by where they first appear in the table,
# and assigns them an sequential ID
.number_footnotes <- function(x) {
  if (nrow(x$table_styling$footnote) == 0 &&
    nrow(x$table_styling$footnote_abbrev) == 0) {
    return(dplyr::tibble(
      footnote_id = integer(), footnote = character(), column = character(),
      tab_location = character(), row_numbers = integer()
    ))
  }
  dplyr::bind_rows(
    x$table_styling$footnote,
    x$table_styling$footnote_abbrev
  ) %>%
    dplyr::inner_join(
      x$table_styling$header %>%
        select("column") %>%
        mutate(column_id = dplyr::row_number()),
      by = "column"
    ) %>%
    dplyr::arrange(desc(.data$tab_location), .data$column_id, .data$row_numbers) %>%
    dplyr::group_by(.data$footnote) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(footnote_id = dplyr::row_number()) %>%
    tidyr::unnest(cols = "data") %>%
    dplyr::select(
      "footnote_id", "footnote", "column",
      "tab_location", "row_numbers"
    )
}

.table_styling_cols_merge <- function(x) {
  if (is.null(x$table_styling$cols_merge) || nrow(x$table_styling$cols_merge) == 0) {
    return(x)
  }
  x_cleaned <- .table_styling_expr_to_row_number(x)
  if (nrow(x_cleaned$table_styling$cols_merge) == 0) {
    return(x)
  }

  # replacing numeric columns with formatted/character,merged columns
  merging_columns <- x_cleaned$table_styling$cols_merge$column
  df_merged <- dplyr::as_tibble(x, include = c("tibble", "fmt", "cols_merge"))
  x$table_body[merging_columns] <- df_merged[merging_columns]

  # removing merging instructions ----------------------------------------------
  x$table_styling$cols_merge <-
    x$table_styling$cols_merge %>%
    dplyr::filter(FALSE)

  # replacing formatting functions for merged columns --------------------------
  x <- modify_fmt_fun(x, update = inject(!!merging_columns ~ as.character))

  # return merged gtsummary table ----------------------------------------------
  x
}


# this function takes a list expressions and evaluates them with a `%>%` between them
.eval_list_of_exprs <- function(exprs, env = rlang::caller_env()) {
  exprs %>%
    # removing NULL elements
    unlist() %>%
    purrr::compact() %>%
    # concatenating expressions with %>% between each of them
    purrr::reduce(function(x, y) rlang::inject(!!x %>% !!y, env = env))
}
