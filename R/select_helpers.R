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
#'
#' @param stat_0 (scalar `logical`)\cr
#'   When `FALSE`, will not select the `"stat_0"` column. Default is `TRUE`
#' @param continuous2 (scalar `logical`)\cr
#'   Logical indicating whether to include continuous2 variables. Default is `TRUE`
#' @param dichotomous (scalar `logical`)\cr
#'   Logical indicating whether to include dichotomous variables. Default is `TRUE`
#' @param tests (`character`)\cr
#'   character vector indicating the test type of the variables to select, e.g.
#'   select all variables being compared with `"t.test"`.
#'
#' @name select_helpers
#' @return A character vector of column names selected
#'
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @examples
#' select_ex1 <-
#'   trial |>
#'   select(age, response, grade) |>
#'   tbl_summary(
#'     statistic = all_continuous() ~ "{mean} ({sd})",
#'     type = all_dichotomous() ~ "categorical"
#'   )
NULL

#' @rdname select_helpers
#' @export
all_continuous <- function(continuous2 = TRUE) {
  types <- if (continuous2) c("continuous", "continuous2") else "continuous"

  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% types))
}

#' @rdname select_helpers
#' @export
all_continuous2 <- function() {
  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% "continuous2"))
}

#' @rdname select_helpers
#' @export
all_categorical <- function(dichotomous = TRUE) {
  types <- if (dichotomous) c("categorical", "dichotomous") else "categorical"

  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% types))
}

#' @rdname select_helpers
#' @export
all_dichotomous <- function() {
  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% "dichotomous"))
}

#' @rdname select_helpers
#' @export
all_tests <- function(tests) {
  where(function(x) isTRUE(attr(x, "gtsummary.test_name") %in% tests))
}

# #' @rdname select_helpers
# #' @export
# all_tests <- function(tests = NULL) {
#   if (is.null(tests) || !is.character(tests) || any(!tests %in% df_add_p_tests$test_name)) {
#     paste(
#       "The `tests=` argument must be one or more of the following:",
#       paste(shQuote(df_add_p_tests$test_name), collapse = ", ")
#     ) %>%
#       stop(call. = FALSE)
#   }
#
#   .generic_selector("variable", "test_name",
#     .data$test_name %in% .env$tests,
#     fun_name = "all_tests"
#   )
# }

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


# #' @rdname select_helpers
# #' @export
# all_interaction <- broom.helpers::all_interaction

# #' @rdname select_helpers
# #' @export
# all_intercepts <- broom.helpers::all_intercepts

# #' @rdname select_helpers
# #' @export
# all_contrasts <- broom.helpers::all_contrasts



#' Table Body Select Prep
#'
#' This function uses the information in `.$table_body` and adds them
#' as attributes to `data` (if passed). Once they've been assigned as
#' proper gtsummary attributes, gtsummary selectors like `all_continuous()`
#' will work properly.
#'
#' @param table_body a data frame from `.$table_body`
#' @param data an optional data frame the attributes will be added to
#'
#' @return a data frame
#' @keywords internal
select_prep <- function(table_body, data = NULL) {
  # if data not passed, use table_body to construct one
  if (is.null(data)) {
    data <- dplyr::tibble(!!!rep_named(unique(table_body$variable), logical(0L)))
  }

  # only keeping rows that have corresponding column names in data
  table_body <- table_body |> dplyr::filter(.data$variable %in% names(data))

  # if table_body passed, add columns as attr to data
  if (!is.null(table_body)) {
    attr_cols <- intersect(names(table_body), c("var_type", "test_name"))
    for (v in attr_cols) {
      df_attr <- table_body[c("variable", v)] |> unique() |> tidyr::drop_na()
      for (i in seq_len(nrow(df_attr))) {
        attr(data[[df_attr$variable[i]]], paste0("gtsummary.", v)) <- df_attr[[v]][i]
      }
    }
  }

  data
}


#' Convert character vector to data frame
#'
#' This is used in some of the selecting we allow for, for example in
#' `as_gt(include=)` you can use tidyselect to select among the call
#' names to include.
#'
#' @param x character vector
#'
#' @return data frame
#' @keywords internal
vec_to_df <- function(x) {
  matrix(ncol = length(x), nrow = 0) |>
    data.frame() |>
    stats::setNames(x)
}


# converts a named list to a table_body format.
# the result of this fn will often be passed to `select_prep()`
#' Convert Named List to Table Body
#'
#' Many arguments in 'gtsummary' accept named lists. This function converts
#' a named list to the `.$table_body` format expected in `select_prep()`
#'
#' @param x named list
#' @param colname string of column name to assign. Default is `caller_arg(x)`
#'
#' @return `.$table_body` data frame
#' @keywords internal
#' @examples
#' type <- list(age = "continuous", response = "dichotomous")
#' gtsummary:::.list2tb(type, "var_type")
.list2tb <- function(x, colname = caller_arg(x)) {
  enframe(unlist(x), "variable", colname)
}
