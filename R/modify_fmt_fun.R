#' Modify Formatting Functions
#'
#' \lifecycle{maturing}
#' Use this function to update the way numeric columns and rows of `.$table_body`
#' are formatted
#'
#' @param update list of formulas or a single formula specifying the updated
#' formatting function.
#' The LHS specifies the column(s) to be updated,
#' and the RHS is the updated formatting function.
#' @param rows predicate expression to select rows in `x$table_body`.
#' Default is `NULL`. See details below.
#' @inheritParams modify_table_styling
#'
#' @inheritSection modify_table_styling rows argument
#' @family Advanced modifiers
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @export
# #' @examples
# #' \donttest{
# #' # Example 1 ----------------------------------
# #' # show 'grade' p-values to 3 decimal places
# #' modify_fmt_fun_ex1 <-
# #'   lm(age ~ marker + grade, trial) %>%
# #'   tbl_regression() %>%
# #'   modify_fmt_fun(
# #'     update = p.value ~ function(x) style_pvalue(x, digits = 3),
# #'     rows = variable == "grade"
# #'   )
# #' }
modify_fmt_fun <- function(x, update, rows = NULL) {
  updated_call_list <- c(x$call_list, list(modify_column_unhide = match.call()))
  assert_class(x, "gtsummary")

  # converting update arg to a tidyselect list ---------------------------------
  cards::process_formula_selectors(
    data = x$table_body,
    udpate = update
  )
  cards::check_list_elements(
    check = function(x) is_function(x),
    error_msg = list(
      check = "The value passed in {.arg {arg_name}} for variable {.val {variable}} must be a function."
    )
  )


  # updating formatting functions ----------------------------------------------
  x <-
    modify_table_styling(
      x = x,
      columns = names(update),
      rows = {{ rows }},
      fmt_fun = update
    )

  x$call_list <- updated_call_list
  x
}
