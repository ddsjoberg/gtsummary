#' Select helper functions
#'
#' @description Set of functions to supplement the {tidyselect} set of
#' functions for selecting columns of data frames (and other items as well).
#' - `all_continuous()` selects continuous variables
#' - `all_continuous2()` selects only type `"continuous2"`
#' - `all_categorical()` selects categorical (including `"dichotomous"`) variables
#' - `all_dichotomous()` selects only type `"dichotomous"`
#' - `all_interaction()` selects interaction terms from a regression model
#' - `all_intercepts()` selects intercept terms from a regression model
#' - `all_contrasts()` selects variables in regression model based on their type of contrast
#' @param tests character
#' @name select_helpers
#' @return A character vector of column names selected
#' @examples
#' select_ex1 <-
#'   trial %>%
#'   select(age, response, grade) %>%
#'   tbl_summary(
#'     statistic = all_continuous() ~ "{mean} ({sd})",
#'     type = all_dichotomous() ~ "categorical"
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{select_ex1.png}{options: width=55\%}}
NULL

#' @rdname select_helpers
#' @export
all_continuous2 <- function() {
  .generic_selector("variable", "var_type",
                    .data$var_type %in% "continuous2",
                    fun_name = "all_continuous")
}

#' @rdname select_helpers
#' @export
all_tests <- function(tests = NULL) {
  if (is.null(tests) || !is.character(tests) || any(!tests %in% df_add_p_tests$test_name)){
    paste("The `tests=` argument must be one or more of the following:",
          paste(shQuote(df_add_p_tests$test_name), collapse = ", ")) %>%
      stop(call. = FALSE)
  }

  .generic_selector("variable", "test_name",
                    .data$test_name %in% .env$tests,
                    fun_name = "all_tests")
}
