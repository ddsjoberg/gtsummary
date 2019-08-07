#' Add column with N
#'
#' For each variable in a `tbl_summary` table, the `add_n` function adds a column with the
#' total number of non-missing (or missing) observations
#'
#' @param x object with class `tbl_summary` from the [tbl_summary] function
#' @param missing logical argument indicating whether to print N (`missing = FALSE`),
#' or N missing (`missing = TRUE`).  Default is `FALSE`
#' @param last logical indicator to include N column last in table.
#' Default is `FALSE`, which will display N column first.
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg
#' @export
#' @return A `tbl_summary` object
#' @examples
#' tbl_n_ex <-
#'   trial %>%
#'   dplyr::select(trt, age, grade, response) %>%
#'   tbl_summary(by = trt) %>%
#'   add_n()
#' @section Example Output:
#' \if{html}{\figure{tbl_n_ex.png}{options: width=50\%}}

add_n <- function(x, missing = FALSE, last = FALSE) {
  # checking that input is class tbl_summary
  if (class(x) != "tbl_summary") stop("x must be class 'tbl_summary'")

  # counting non-missing N (or missing N)
  counts <-
    x$meta_data %>%
    select(c("variable")) %>%
    mutate(
      row_type = "label",
      n_var = map_chr(
        .data$variable,
        ~ case_when(
          missing == FALSE ~ sum(!is.na(x$inputs$data[[.x]])),
          missing == TRUE ~ sum(is.na(x$inputs$data[[.x]]))
        )
      )
    ) %>%
    set_names(c("variable", "row_type", ifelse(missing == FALSE, "n", "n_missing")))

  # merging result with existing tbl_summary
  if (last == FALSE) {
    table_body <-
      x$table_body %>%
      select(c("variable", "row_type", "label")) %>%
      left_join(counts, by = c("variable", "row_type")) %>%
      left_join(x$table_body, by = c("variable", "row_type", "label"))
  }
  else if (last == TRUE) {
    table_body <-
      x$table_body %>%
      left_join(counts, by = c("variable", "row_type"))
  }

  # replacing old table_body with new
  x$table_body <- table_body

  x$table_header <-
    tibble(column = names(table_body)) %>%
    left_join(x$table_header, by = "column") %>%
    table_header_fill_missing()

  # updating header
  if (missing == FALSE) {
    x <- modify_header_internal(x, n = "**N**")
  }
  else if (missing == TRUE) {
    x <- modify_header_internal(x, n_missing = "**N Missing**")
  }

  # updating gt and kable calls with data from table_header
  x <- update_calls_from_table_header(x)

  # adding indicator to output that add_n was run on this data
  x$call_list <- c(x$call_list, list(add_n = match.call()))

  # returning tbl_summary object
  return(x)
}
