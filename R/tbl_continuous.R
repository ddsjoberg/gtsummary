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
                           include = everything(),
                           digits = NULL,
                           by = NULL,
                           statistic = NULL,
                           label = NULL) {
  # evaluate inputs ------------------------------------------------------------
  variable <- .select_to_varnames(data = data, select = {{ variable }},
                                  select_single = TRUE, arg_name = "variable")
  by <- .select_to_varnames(data = data, select = {{ by }},
                            select_single = TRUE, arg_name = "by")
  include <-
    .select_to_varnames(data = data, select = {{ include }},
                        select_single = FALSE, arg_name = "include") %>%
    setdiff(c(variable, by))

  digits <-
    rep_len(
      list(continuous_digits_guess(data, variable, "continuous")),
      length.out = length(include)
    ) %>%
    rlang::set_names(include) %>%
    purrr::list_modify(
      !!!.formula_list_to_named_list(digits, data = data, arg_name = "digits")
    )
  statistic <-
    rep_len(list("{median} ({p25}, {p75})"), length.out = length(include)) %>%
    rlang::set_names(include) %>%
    purrr::list_modify(
      !!!.formula_list_to_named_list(statistic, data = data, arg_name = "statistic")
    )

  label <-
    .formula_list_to_named_list(label, data = data, arg_name = "label")

  # saving function inputs
  tbl_continuous_inputs <- as.list(environment())

  # calculate tbl_continuous tables --------------------------------------------
  result <-
    tbl_custom_summary(
      data, data,
      by = by,
      stat_fns = everything() ~ continuous_summary(variable),
      statistic = statistic,
      digits = digits,
      label = label,
      include = include,
      type = all_of(include %>% setdiff(c(variable, by))) ~ "categorical"
    )
  result[["inputs"]] <- tbl_continuous_inputs
  result[["call_list"]] <- list(tbl_continuous = match.call())

  # return result --------------------------------------------------------------
  class(result) <- c("tbl_continuous", "gtsummary")
  result
}

#' @rdname add_overall
#' @export
add_overall.tbl_continuous <- function(x, last = FALSE, col_label = NULL) {
  updated_call_list <- c(x$call_list, list(add_overall = match.call()))
  # checking that input x has a by var
  if (is.null(x$inputs[["by"]])) {
    stop(
      "Cannot add Overall column when no 'by' variable in original tbl_custom_summary"
    )
  }

  x_copy <- x

  # removing 'by' variable from data
  # (so it won't show up in the overall tbl_summary)
  x_copy$inputs[["data"]] <- select(x$inputs[["data"]], -x[["by"]])
  x_copy$inputs$include <- x_copy$inputs$include %>% setdiff(x[["by"]])

  # replacing the function call by variable to NULL to get results overall
  x_copy$inputs[["by"]] <- NULL

  # calculating stats overall, and adding header row
  tbl_overall <- do.call(tbl_continuous, x_copy$inputs)

  # merging overall results
  x <- add_overall_merge(x, tbl_overall, last, col_label)

  x$call_list <- updated_call_list
  x
}
