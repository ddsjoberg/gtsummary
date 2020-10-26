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
#' @param dichotomous Logical indicating whether to include dichotomous variables.
#' Default is `TRUE`
#' @param continuous2 Logical indicating whether to include continuous2 variables.
#' Default is `TRUE`
#' @param type type of contrast to select. Must be one of
#' `c("treatment", "sum", "poly", "helmert")`
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
all_continuous <- function(continuous2 = TRUE) {
  if (continuous2) con_types <- c("continuous", "continuous2")
  else con_types <- "continuous"

  .generic_selector("variable", "var_type",
                    .data$var_type %in% con_types,
                    fun_name = "all_continuous")
}

#' @rdname select_helpers
#' @export
all_continuous2 <- function() {
  .generic_selector("variable", "var_type",
                    .data$var_type %in% "continuous2",
                    fun_name = "all_continuous")
}

#' @rdname select_helpers
#' @export
all_dichotomous <- function() {
  .generic_selector("variable", "var_type",
                    .data$var_type %in% "dichotomous",
                    fun_name = "all_dichotomous")
}

#' @rdname select_helpers
#' @export
all_categorical <- function(dichotomous = TRUE) {
  types <- switch(dichotomous, c("categorical", "dichotomous")) %||% "categorical"

  .generic_selector("variable", "var_type",
                    .data$var_type %in% types,
                    fun_name = "all_categorical")
}

#' @rdname select_helpers
#' @export
all_interaction <- function() {
  .generic_selector("variable", "var_type",
                    .data$var_type %in% "interaction",
                    fun_name = "all_interaction")
}

#' @rdname select_helpers
#' @export
all_intercepts <- function() {
  .generic_selector("variable", "var_type",
                    .data$var_type %in% "intercept",
                    fun_name = "all_intercepts")
}

#' @rdname select_helpers
#' @export
all_contrasts <- function(type = c("treatment", "sum", "poly", "helmert")) {
  type <- match.arg(type)
  contr.type <- switch(type,
                       "treatment" = "contr.treatment",
                       "sum" = "contr.sum",
                       "poly" = "contr.poly",
                       "helmert" = "contr.helmert")

  .generic_selector("variable", "contrasts",
                    .data$contrasts %in% contr.type,
                    fun_name = "all_contrasts")
}
