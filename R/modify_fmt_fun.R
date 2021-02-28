#' Modify Formatting Functions
#'
#' \lifecycle{experimental}
#' Use this function to update the way numeric columns/row in `.$table_body`
#' are formatted
#'
#' @param update list of formulas or a single formula specifying the updated
#' formatting function.
#' The LHS specifies the column(s) to be updated,
#' and the RHS is the updated formatting function.
#' @param rows predicate expression to select rows in `x$table_body`. Default
#' is `NULL`, which applies the formatting function to the entire column.
#' @inheritParams modify_table_styling
#'
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' # show 'grade' p-values to 3 decimal places
#' modify_fmt_fun_ex1 <-
#'   lm(age ~ marker + grade, trial) %>%
#'   tbl_regression() %>%
#'   modify_fmt_fun(
#'     update = p.value ~ function(x) style_pvalue(x, digits = 3),
#'     rows = variable == 'grade'
#'   )
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{modify_fmt_fun_ex1.png}{options: width=45\%}}
modify_fmt_fun <- function(x, update, rows = NULL) {
  # converting update arg to a tidyselect list ---------------------------------
  update <-
    .formula_list_to_named_list(
      x = update,
      data = x$table_body,
      arg_name = "update"
    )

  # updating formatting functions ----------------------------------------------
  modify_table_styling(
    x = x,
    columns = names(update),
    rows = {{ rows }},
    fmt_fun = update
  )
}
