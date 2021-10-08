#' Summarize a continuous variable
#'
#' Summarize a continuous variable by one or more categorical variables
#'
#' @param variable Variable name of the continuous column to be summarized
#' @param include Vector of categorical variable names
#' @param by Optional name of single categorical variable. Default it `NULL`
#' @param statistic String specifying the continuous summary statistics to
#' display.  The default is `"{median} ({p25}, {p75})"`. See
#' `tbl_summary(statistic=)` argument for details.
#' @param digits An integer specifying the number of decimal places to round
#' the variable to. Default is `NULL` and the number of decimal places is guessed
#' based on the distribution of the continuous variable.
#' @inheritParams tbl_summary
#'
#' @return a gtsummary table
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' tbl_continuous_ex1 <-
#'   tbl_continuous(
#'     data = trial,
#'     variable = age,
#'     by = trt,
#'     include = grade
#'   )
#'
#' # Example 2 ----------------------------------
#' tbl_continuous_ex2 <-
#'   tbl_continuous(
#'     data = trial,
#'     variable = age,
#'     include = c(trt, grade)
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_continuous_ex1.png}{options: width=38\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_continuous_ex2.png}{options: width=35\%}}

tbl_continuous <- function(data,
                           variable,
                           include,
                           digits = NULL,
                           by = NULL,
                           statistic = everything() ~ "{median} ({p25}, {p75})",
                           label = NULL) {
  # evaluate inputs ------------------------------------------------------------
  variable <- .select_to_varnames(data = data, select = {{ variable }},
                                  select_single = TRUE, arg_name = "variable")
  include <- .select_to_varnames(data = data, select = {{ include }},
                                 select_single = FALSE, arg_name = "include")
  by <- .select_to_varnames(data = data, select = {{ by }},
                            select_single = TRUE, arg_name = "by")
  digits <-
    digits %||% continuous_digits_guess(data, variable, "continuous")

  # calculate tbl_continuous tables --------------------------------------------
  tbl_custom_summary(
    data, data,
    by = by,
    stat_fns = everything() ~ continuous_summary(variable),
    statistic = statistic,
    digits = everything() ~ digits,
    label = label,
    include = include,
    type = all_of(include %>% setdiff(c(variable, by))) ~ "categorical"
  )
}

