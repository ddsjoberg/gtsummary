#' Modify Formatting Functions
#'
#' \lifecycle{experimental}
#' Use this function to update the way numeric columns/row in `.$table_body`
#' are formatted
#'
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
#'     columns = p.value,
#'     rows = variable == 'grade',
#'     fmt_fun = function(x) style_pvalue(x, digits = 3)
#'   )
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{modify_fmt_fun_ex1.png}{options: width=45\%}}
modify_fmt_fun <- function(x, columns, rows, fmt_fun) {
  modify_table_styling(
    x = x,
    columns = {{ columns }},
    rows = {{ rows }},
    fmt_fun = fmt_fun
  )
}
