#' Select helper functions
#'
#' @description Set of functions to supplement the {tidyselect} set of
#' functions for selecting columns of data frames (and other items as well).
#' - `all_continuous()` selects continuous variables
#' - `all_categorical()` selects categorical (including `"dichotomous"`) variables
#' - `all_dichotomous()` selects only type `"dichotomous"`
#' - `all_interaction()` selects interaction terms from a regression model
#' - `all_intercepts()` selects intercept terms from a regression model
#' - `all_contrasts()` selects variables in regression model based on their type of contrast
#' @name select_helpers
#' @rdname select_helpers
#' @param dichotomous Logical indicating whether to include dichotomous variables.
#' Default is `TRUE`
#' @param contrasts_type type of contrast to select. When `NULL`, all variables with a
#' contrast will be selected. Default is `NULL`.  Select among contrast types
#' `c("treatment", "sum", "poly", "helmert", "other")`
#'
#' @return A character vector of column names selected
#' @examples
#' glm(response ~ age * trt + grade, gtsummary::trial, family = binomial) %>%
#'   tidy_plus_plus(exponentiate = TRUE, include = all_categorical())
#'
#' glm(response ~ age + trt + grade + stage,
#'     gtsummary::trial,
#'     family = binomial,
#'     contrasts = list(trt = contr.SAS, grade = contr.sum, stage = contr.poly)) %>%
#' tidy_plus_plus(exponentiate = TRUE,
#'                include = all_contrasts(c("treatment", "sum")))
NULL

#' @rdname select_helpers
#' @export
all_continuous <- function() {
  .generic_selector("variable", "var_type",
                    startsWith(.data$var_type, "continuous"),
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
                    .data$var_type %in% .env$types,
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
all_contrasts <- function(contrasts_type = NULL) {
  # if no types specified, select all contrasts
  if (is.null(contrasts_type))
    return(
      .generic_selector("variable", "contrasts_type",
                        !is.na(.data$contrasts_type),
                        fun_name = "all_contrasts")
    )
  # otherwise, select those specified in `contrasts_type=`
  else {
    contrasts_type <-
      match.arg(contrasts_type,
                c("treatment", "sum", "poly", "helmert", "other"),
                several.ok = TRUE)
    return(
      .generic_selector("variable", "contrasts_type",
                        .data$contrasts_type %in% .env$contrasts_type,
                        fun_name = "all_contrasts")
    )
  }
}
