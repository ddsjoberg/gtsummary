# this function cleans up table_header (i.e. removes formatting for hidden columns, etc.)
.clean_table_header <- function(x) {
  # removing instructions for hidden columns
  dplyr::mutate_at(
    x,
    vars(any_of(c("bold", "italic", "missing_emdash", "indent", "footnote_abbrev", "footnote"))),
    ~ifelse(.data$hide, NA_character_, .)
  )
}

# convert columns that use row and values to format ----------------------------
.convert_table_header_to_styling <- function(x) {
  x$table_body_styling$header <-
    x$table_header %>%
    mutate(interpret_spanning_header = "gt::md") %>%
    select(.data$column, .data$hide, .data$align,
           interpret_label = .data$text_interpret, .data$label,
           .data$interpret_spanning_header, .data$spanning_header)

  x <- x %>%
    .convert_header_to_rows_one_column("footnote") %>%
    .convert_header_to_rows_one_column("footnote_abbrev") %>%
    .convert_header_to_rows_one_column("missing_emdash") %>%
    .convert_header_to_rows_one_column("indent") %>%
    .convert_header_to_rows_one_column("bold") %>%
    .convert_header_to_rows_one_column("italic") %>%
    .convert_header_to_rows_one_column("fmt_fun")

  # x$table_header <- NULL
  x
}


.convert_header_to_rows_one_column <- function(x, column) {

  if (column %in% c("footnote", "footnote_abbrev")) {
    x$table_body_styling[[column]] <-
      x$table_header %>%
      select(all_of(c("column", .env$column))) %>%
      set_names(c("column", "footnote")) %>%
      filter(!is.na(.data$footnote)) %>%
      mutate(rows = NA_character_,
             text_interpret = "gt::md") %>%
      select(all_of(c("column", "rows", "text_interpret", "footnote")))
  }
  else if (column %in% c("indent", "bold", "italic")) {
    x$table_body_styling$text_format <-
      x$table_header %>%
      select(all_of(c("column", .env$column))) %>%
      filter(!is.na(.data[[column]])) %>%
      set_names(c("column", "rows")) %>%
      mutate(
        format_type = .env$column,
        undo_text_format = FALSE
      ) %>%
      bind_rows(x$table_body_styling$text_format)
  }
  else if (column %in% "missing_emdash") {
    x$table_body_styling[["fmt_missing"]] <-
      x$table_header %>%
      select(all_of(c("column", .env$column))) %>%
      filter(!is.na(.data[[column]])) %>%
      set_names(c("column", "rows")) %>%
      mutate(symbol = get_theme_element("tbl_regression-str:ref_row_text",
                                        default = "---"))
  }
  else if (column %in% "fmt_fun") {
    x$table_body_styling[[column]] <-
      x$table_header %>%
      select(all_of(c("column", .env$column))) %>%
      filter(!map_lgl(.data[[column]], is.null)) %>%
      mutate(rows = NA_character_)  %>%
      select(all_of(c("column", "rows", .env$column)))
  }

  # return gtsummary table
  x
}

# takes a table_body and a character rows expression, and returns the resulting row numbers
.rows_expr_to_row_numbers <- function(table_body, rows) {
  if (is.na(rows)) return(NA)

  df <- as.data.frame(table_body) %>% tibble::remove_rownames()

  expr(subset(df, !!rlang::parse_expr(rows))) %>%
    eval() %>%
    rownames() %>%
    as.integer()
}

.cols_to_show <- function(x) {
  x$table_body_styling$header %>%
    filter(!.data$hide) %>%
    pull(column)
}


# 1. Converts row expressions to row numbers, and only keeps the most recent.
# 2. Deletes NA footnote, text_formatting undoings, etc. as they will not be used
.clean_table_body_stylings <- function(x) {
  # text_format ----------------------
  x$table_body_styling$text_format <-
    x$table_body_styling$text_format %>%
    filter(.data$column %in% .cols_to_show(x)) %>%
    rowwise() %>%
    mutate(
      row_numbers = .rows_expr_to_row_numbers(x$table_body, rows) %>% list()
    ) %>%
    select(-.data$rows) %>%
    unnest(.data$row_numbers) %>%
    group_by(.data$column, .data$row_numbers, .data$format_type) %>%
    dplyr::slice_tail() %>%
    filter(.data$undo_text_format == FALSE) %>% # dropping undoing cmds
    group_by(column, format_type) %>%
    nest(row_numbers = row_numbers) %>%
    mutate(row_numbers = map(row_numbers, ~unlist(.x) %>% unname())) %>%
    select(.data$column, .data$row_numbers, everything()) %>%
    ungroup()

  # footnote ---------------------------------
  x$table_body_styling$footnote <-
    .clean_table_body_stylings_footnote(x, "footnote")

  # footnote_abbrev ---------------------------------
  x$table_body_styling$footnote_abbrev <-
    .clean_table_body_stylings_footnote(x, "footnote_abbrev")

  # fmt_fun --------------------------------------
  x$table_body_styling$fmt_fun <-
    x$table_body_styling$fmt_fun %>%
    filter(.data$column %in% .cols_to_show(x)) %>%
    rowwise() %>%
    mutate(
      tab_location = ifelse(is.na(rows), "header", "body"),
      row_numbers = .rows_expr_to_row_numbers(x$table_body, rows) %>% list()
    ) %>%
    select(-.data$rows) %>%
    unnest(row_numbers) %>%
    group_by(column, tab_location, row_numbers) %>%
    dplyr::slice_tail() %>%
    ungroup()

  x
}

.clean_table_body_stylings_footnote <- function(x, footnote_type) {
  df_clean <-
    x$table_body_styling[[footnote_type]] %>%
    filter(.data$column %in% .cols_to_show(x))
  if (nrow(df_clean) == 0)
    return(tibble(column = character(0), tab_location = character(0), row_numbers = logical(0),
                  text_interpret = character(0), footnote = character(0)))

  df_clean <-
    df_clean %>%
    rowwise() %>%
    mutate(
      tab_location = ifelse(is.na(rows), "header", "body"),
      row_numbers = .rows_expr_to_row_numbers(x$table_body, rows) %>% list(),
    ) %>%
    select(-.data$rows) %>%
    unnest(cols = .data$row_numbers) %>%
    group_by(column, tab_location, row_numbers) %>%
    dplyr::slice_tail() %>% # keeping the most recent addition
    filter(!is.na(footnote)) # keep non-missing additions

  if (footnote_type == "footnote_abbrev") {
    df_clean$footnote <- paste(df_clean$footnote, collapse = ", ")
  }

  df_clean %>%
    select(all_of(c("column", "tab_location", "row_numbers", "text_interpret", "footnote")))
}



