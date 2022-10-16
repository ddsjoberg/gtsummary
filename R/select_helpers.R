#' Select helper functions
#'
#' @description Set of functions to supplement the {tidyselect} set of
#' functions for selecting columns of data frames (and other items as well).
#' - `all_continuous()` selects continuous variables
#' - `all_continuous2()` selects only type `"continuous2"`
#' - `all_categorical()` selects categorical (including `"dichotomous"`) variables
#' - `all_dichotomous()` selects only type `"dichotomous"`
#' - `all_tests()` selects variables by the name of the test performed
#' - `all_stat_cols()` selects columns from `tbl_summary`/`tbl_svysummary` object with summary statistics (i.e. `"stat_0"`, `"stat_1"`, `"stat_2"`, etc.)
#' - `all_interaction()` selects interaction terms from a regression model
#' - `all_intercepts()` selects intercept terms from a regression model
#' - `all_contrasts()` selects variables in regression model based on their type of contrast
#' @param tests string indicating the test type of the variables to select, e.g.
#' select all variables being compared with `"t.test"`
#' @param stat_0 When `FALSE`, will not select the `"stat_0"` column. Default is `TRUE`
#' @param continuous2 Logical indicating whether to include continuous2 variables. Default is `TRUE`
#' @param dichotomous Logical indicating whether to include dichotomous variables.
#' Default is `TRUE`
#' @param contrasts_type type of contrast to select. When `NULL`, all variables with a
#' contrast will be selected. Default is `NULL`.  Select among contrast types
#' `c("treatment", "sum", "poly", "helmert", "other")`
#' @name select_helpers
#' @return A character vector of column names selected
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
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
#' \if{html}{\out{
#' `r man_create_image_tag(file = "select_ex1.png", width = "55")`
#' }}
NULL

#' @rdname select_helpers
#' @export
all_continuous <- function(continuous2 = TRUE) {
  if (continuous2 == TRUE) {
    return(broom.helpers::all_continuous())
  } else {
    return(
      .generic_selector("variable", "var_type",
        .data$var_type %in% "continuous",
        fun_name = "all_continuous"
      )
    )
  }
}

#' @rdname select_helpers
#' @export
all_continuous2 <- function() {
  .generic_selector("variable", "var_type",
    .data$var_type %in% "continuous2",
    fun_name = "all_continuous"
  )
}

#' @rdname select_helpers
#' @export
all_categorical <- broom.helpers::all_categorical

#' @rdname select_helpers
#' @export
all_dichotomous <- broom.helpers::all_dichotomous

#' @rdname select_helpers
#' @export
all_tests <- function(tests = NULL) {
  if (is.null(tests) || !is.character(tests) || any(!tests %in% df_add_p_tests$test_name)) {
    paste(
      "The `tests=` argument must be one or more of the following:",
      paste(shQuote(df_add_p_tests$test_name), collapse = ", ")
    ) %>%
      stop(call. = FALSE)
  }

  .generic_selector("variable", "test_name",
    .data$test_name %in% .env$tests,
    fun_name = "all_tests"
  )
}

#' @rdname select_helpers
#' @export
all_stat_cols <- function(stat_0 = TRUE) {
  # finds stat_0, stat_1, stat_2, etc.
  if (stat_0 == TRUE) {
    return(
      union(
        dplyr::matches("^stat_\\d+$"),
        dplyr::matches("^stat_\\d+_.*$")
      )
    )
  }
  # finds stat_1, stat_2, etc.
  if (stat_0 == FALSE) {
    return(
      union(
        dplyr::matches("^stat_\\d*[1-9]\\d*$"),
        dplyr::matches("^stat_\\d*[1-9]\\d*_.*$")
      )
    )
  }
}

#' @rdname select_helpers
#' @export
all_interaction <- broom.helpers::all_interaction

#' @rdname select_helpers
#' @export
all_intercepts <- broom.helpers::all_intercepts

#' @rdname select_helpers
#' @export
all_contrasts <- broom.helpers::all_contrasts
