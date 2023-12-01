#' Assign Default Summary Type
#'
#' Function inspects data and assigns a summary type when not specified
#' in the `type` argument.
#'
#' @param data a data frame
#' @param variables character vector of column names in `data`
#' @param value named list of values to show for dichotomous variables, where
#' the names are the variables
#' @param type named list of summary types, where names are the variables
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
assign_summary_type <- function(data, variables, value, type = NULL) {
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

        # logical variables are dichotomous
        if (inherits(data[[variable]], "logical")) {
          return("dichotomous")
        }

        # numeric variables that are 0 and 1 only, will be dichotomous
        if (inherits(data[[variable]], c("integer", "numeric")) &&
            length(setdiff(stats::na.omit(data[[variable]]), c(0, 1))) == 0) {
          return("dichotomous")
        }

        # factor variables that are "No" and "Yes" only, will be dichotomous
        if (inherits(data[[variable]], "factor") &&
            length(levels(data[[variable]])) == 2L &&
            setequal(toupper(levels(data[[variable]])), c("NO", "YES"))) {
          return("dichotomous")
        }

        # character variables that are "No" and "Yes" only, will be dichotomous
        if (inherits(data[[variable]], "character") &&
            setequal(toupper(stats::na.omit(data[[variable]])), c("NO", "YES")) &&
            length(stats::na.omit(data[[variable]])) == 2L) {
          return("dichotomous")
        }

        # factors and characters are categorical
        if (inherits(data[[variable]], c("factor", "character"))) {
          return("categorical")
        }

        # numeric variables with fewer than 10 levels will be categorical
        if (inherits(data[[variable]], base_numeric_classes) &&
            length(unique(stats::na.omit(data[[variable]]))) < 10) {
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

.add_summary_type_as_attr <- function(data, type) {
  type <- type[names(type) %in% names(data)]
  type_names <- names(type)

  for (i in seq_along(type)) {
    attr(data[[type_names[i]]], "gtsummary.type") <- type[[i]]
  }

  data
}
