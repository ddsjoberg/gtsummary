#' Assign Default Summary Type
#'
#' Function inspects data and assigns a summary type when not specified
#' in the `type` argument.
#'
#' @param data (`data.frame`)\cr
#'   a data frame
#' @param variables (`character`)\cr
#'   character vector of column names in `data`
#' @param value (`named list`)\cr
#'   named list of values to show for dichotomous variables, where
#'   the names are the variables
#' @param type (`named list`)\cr
#'   named list of summary types, where names are the variables
#' @param cat_threshold (`integer`)\cr
#'   for base R numeric classes with fewer levels than
#'   this threshold will default to a categorical summary. Default is `10L`
#'
#' @return named list
#' @export
#'
#' @examples
#' assign_summary_type(
#'   data = trial,
#'   variables = c("age", "grade", "response"),
#'   value = NULL
#' )
assign_summary_type <- function(data, variables, value, type = NULL, cat_threshold = 10L) {
  set_cli_abort_call()
  # base classes that can be summarized as continuous
  base_numeric_classes <- c("numeric", "integer", "difftime", "Date", "POSIXt", "double")

  # assign a type
  type <-
    map(
      variables,
      function(variable) {
        # return specified type if passed by user
        if (!is.null(type[[variable]])) {
          return(type[[variable]])
        }

        # if user supplied a dichotomous value, make it dichotomous
        if (!is.null(value[[variable]])) {
          return("dichotomous")
        }

        # if a type with a default dichotomous value, make it dichotomous
        if (!is.null(.get_default_dichotomous_value(data[[variable]]))) {
          return("dichotomous")
        }

        # factors are categorical
        if (inherits(data[[variable]], "factor")) {
          return("categorical")
        }

        # if all missing, the continuous
        if (all(is.na(data[[variable]]))) {
          return("continuous")
        }

        # characters are categorical
        if (inherits(data[[variable]], "character")) {
          return("categorical")
        }

        # numeric variables with fewer than 'cat_threshold' levels will be categorical
        if (inherits(data[[variable]], base_numeric_classes) &&
          length(unique(stats::na.omit(data[[variable]]))) < cat_threshold) {
          return("categorical")
        }

        # all other numeric classes are continuous
        if (inherits(data[[variable]], base_numeric_classes)) {
          return(get_theme_element("tbl_summary-str:default_con_type", default = "continuous"))
        }

        # finally, summarize as categorical if none of the above criteria were met
        return("categorical")
      }
    ) |>
    stats::setNames(variables)

  # return type
  type
}

.get_default_dichotomous_value <- function(x) {
  # logical variables are dichotomous
  if (inherits(x, "logical")) {
    return(TRUE)
  }

  # numeric variables that are 0 and 1 only, will be dichotomous
  if (inherits(x, c("integer", "numeric")) &&
    setequal(unique(stats::na.omit(x)), c(0, 1))) {
    return(stats::na.omit(x) |> unique() |> sort() |> dplyr::last())
  }

  # factor variables that are "No" and "Yes" only, will be dichotomous
  if (inherits(x, "factor") &&
    length(levels(x)) == 2L &&
    setequal(toupper(levels(x)), c("NO", "YES"))) {
    return(levels(x)[toupper(levels(x)) %in% "YES"])
  }

  # character variables that are "No" and "Yes" only, will be dichotomous
  if (inherits(x, "character") &&
    setequal(toupper(stats::na.omit(x)), c("NO", "YES")) &&
    length(unique(stats::na.omit(x))) == 2L) {
    return(unique(x)[toupper(unique(x)) %in% "YES"])
  }

  # otherwise, return NULL
  NULL
}
