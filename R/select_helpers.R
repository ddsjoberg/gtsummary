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
all_numeric <- function() {
  which(data_env$numeric)
}

#' @rdname select_helpers
#' @export
all_character <- function() {
  which(data_env$character)
}

#' @rdname select_helpers
#' @export
all_integer <- function() {
  which(data_env$integer)
}

#' @rdname select_helpers
#' @export
all_double <- function() {
  which(data_env$double)
}

#' @rdname select_helpers
#' @export
all_logical <- function() {
  which(data_env$logical)
}

#' @rdname select_helpers
#' @export
all_factor <- function() {
  which(data_env$factor)
}
