#' Summarize a continuous variable
#'
#' \lifecycle{experimental}
#' Summarize a continuous variable by one or more categorical variables
#'
#' @param variable Variable name of the continuous column to be summarized
#' @param digits List of formulas specifying the number of decimal places
#' to round continuous summary statistics. If not specified, an appropriate
#' number of decimals to round statistics will be guessed based on the
#' the variable's distribution.
#' @param statistic List of formulas specifying types of summary statistics
#' to display for each variable. The default is
#' `everything() ~ {median} ({p25}, {p75})`
#' @inheritParams tbl_summary
#'
#' @return a gtsummary table
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @family tbl_continuous tools
#' @export
#'
#' @examples
#' \donttest{
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
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_continuous_ex1.png", width = "38")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_continuous_ex2.png", width = "35")`
#' }}

tbl_continuous <- function(data,
                           variable,
                           include = everything(),
                           digits = NULL,
                           by = NULL,
                           statistic = NULL,
                           label = NULL) {
  # evaluate inputs ------------------------------------------------------------
  variable <- .select_to_varnames(
    data = data, select = {{ variable }},
    select_single = TRUE, arg_name = "variable"
  )
  by <- .select_to_varnames(
    data = data, select = {{ by }},
    select_single = TRUE, arg_name = "by"
  )
  include <-
    .select_to_varnames(
      data = data, select = {{ include }},
      select_single = FALSE, arg_name = "include"
    ) %>%
    setdiff(c(variable, by))

  digits <-
    rep_len(
      list(continuous_digits_guess(data, variable, "continuous")),
      length.out = length(include)
    ) %>%
    rlang::set_names(include) %>%
    purrr::list_modify(
      !!!.formula_list_to_named_list(
        digits,
        data = data, arg_name = "digits",
        type_check = chuck(type_check, "digits", "fn"),
        type_check_msg = chuck(type_check, "digits", "msg")
      )
    )
  statistic <-
    rep_len(list("{median} ({p25}, {p75})"), length.out = length(include)) %>%
    rlang::set_names(include) %>%
    purrr::list_modify(
      !!!.formula_list_to_named_list(statistic,
        data = data,
        arg_name = "statistic",
        type_check = chuck(type_check, "is_string", "fn"),
        type_check_msg = chuck(type_check, "is_string", "msg")
      )
    )

  label <-
    .formula_list_to_named_list(
      label,
      data = data,
      arg_name = "label",
      type_check = chuck(type_check, "is_string", "fn"),
      type_check_msg = chuck(type_check, "is_string", "msg")
    )

  # saving function inputs
  tbl_continuous_inputs <- as.list(environment())

  # calculate tbl_continuous tables --------------------------------------------
  variable_label <-
    label[[variable]] %||% attr(data[[variable]], "label") %||% variable
  statistic_footnote <-
    stat_label_match(statistic) %>%
    unlist() %>%
    unique() %>%
    paste(collapse = "; ")
  result <-
    tbl_custom_summary(
      data, data,
      by = all_of(by),
      stat_fns = everything() ~ continuous_summary(variable),
      statistic = statistic,
      digits = digits,
      label = label,
      include = all_of(include),
      type = all_of(include %>% setdiff(c(variable, by))) ~ "categorical"
    ) %>%
    modify_footnote(
      all_stat_cols() ~ glue("{variable_label}: {statistic_footnote}")
    )

  result[["inputs"]] <- tbl_continuous_inputs
  result[["call_list"]] <- list(tbl_continuous = match.call())

  # return result --------------------------------------------------------------
  class(result) <- c("tbl_continuous", "gtsummary")
  result
}
