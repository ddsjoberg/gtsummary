#' Modify column alignment
#'
#' Update column alignment/justification in a gtsummary table.
#'
#' @inheritParams modify_table_styling
#'
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' lm(age ~ marker + grade, trial) %>%
#'   tbl_regression() %>%
#'   modify_column_alignment(columns = everything(), align = "left")
modify_column_alignment <- function(x, columns, align = c("left", "right", "center")) {
  check_class(x, "gtsummary")
  updated_call_list <- c(x$call_list, list(modify_column_hide = match.call()))
  align <- arg_match(align)

  x <-
    modify_table_styling(
      x = x,
      columns = {{ columns }},
      align = align
    )

  x$call_list <- updated_call_list
  x
}

.modify_column_alignment <- function(x, columns, align) {
  # update alignment -----------------------------------------------------------
  x$table_styling$header <-
    x$table_styling$header |>
    dplyr::mutate(align = ifelse(.data$column %in% .env$columns, .env$align, .data$align))

  # return gtsummary table -----------------------------------------------------
  x
}
