


# creating kable calls from table_header ---------------------------------------
# kable table_header to gt cols_hide code
table_header_to_kable_cols_hide <- function(table_header) {
  table_header %>%
    filter(.data$hide == TRUE) %>%
    pull(.data$column) %>%
    {
      paste0("\"", ., "\"", collapse = ", ")
    } %>%
    {
      glue("dplyr::select(-c({.}))")
    }
}

# kable table_header to formatted columns and bolding code
table_header_to_kable_fmt <- function(table_header) {
  code_vct <-
    table_header %>%
    filter(map_lgl(.data$fmt_fun, ~!is.null(.x))) %>%
    mutate(
      col_code = case_when(
        is.na(bold) ~ glue("dplyr::mutate({column} = x$fmt_fun${column}({column}))"),
        TRUE ~ glue(
          "dplyr::mutate({column} = dplyr::case_when(",
          "{column} <= {bold} ~ paste0('__', x$fmt_fun${column}({column}), '__'), ",
          "TRUE ~ x$fmt_fun${column}({column})",
          "))"
        )
      )
    ) %>%
    pull("col_code")

  if (length(code_vct) == 0) {
    return(NULL)
  }

  code_vct %>%
    glue_collapse(sep = " %>% ")
}

