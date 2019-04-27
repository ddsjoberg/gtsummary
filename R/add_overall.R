#' Add column with overall summary statistics
#'
#' This function adds a column with overall summary statistics to tables
#' created by `tbl_summary`.
#'
#' @param x object with class `tbl_summary` from the [tbl_summary] function
#' @param last logical indicator to include overall  column last. Default is `FALSE`
#' @family tbl_summary tools
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' tbl_overall_ex <-
#'   trial %>%
#'   dplyr::select(age, response, grade, trt) %>%
#'   tbl_summary(by = "trt") %>%
#'   add_overall()
#' @section Example Output:
#' \if{html}{\figure{tbl_overall_ex.png}{options: width=50\%}}
#'
add_overall <- function(x, last = FALSE) {
  # checking that input is class tbl_summary
  if (class(x) != "tbl_summary") stop("x must be class 'tbl_summary'")
  # checking that input x has a by var
  if (is.null(x$inputs[["by"]])) {
    stop("Cannot add Overall column when no 'by' variable in original tbl_summary")
  }

  x_copy <- x

  # removing 'by' variable from data (so it won't show up in the overall tbl_summary)
  x_copy$inputs[["data"]] <- x$inputs[["data"]] %>% select(-c(x[["by"]]))

  # replacing the function call by variable to NULL to get results overall
  x_copy$inputs[["by"]] <- NULL

  # calculating stats overall, and adding header row
  overall <-
    do.call(tbl_summary, x_copy$inputs) %>%
    pluck("table_body")

  # checking the original tbl_summary and the added overall, are the same before binding (excluding headers)
  if (!identical(
    x$table_body %>%
      select(c("row_type", "variable", "label")),
    overall %>%
      select(c("row_type", "variable", "label"))
  )) {
    stop("An error occured in 'add_overall()', cannot merge overall statistics")
  }

  # adding overall stat to the table1 data frame
  if (last == FALSE) {
    x$table_body <-
      bind_cols(
        overall,
        x$table_body %>% select(-c("variable", "row_type", "label"))
      )
  }
  if (last == TRUE) {
    x$table_body <-
      bind_cols(
        x$table_body,
        overall %>% select(c("stat_0"))
      )
  }

  # adding indicator to output that add_overall was run on this data
  x$call_list <- c(x$call_list, list(add_overall = match.call()))

  # adding header
  x <- cols_label_summary(x, stat_overall = md("**Overall**, N = {N}"))

  x
}
