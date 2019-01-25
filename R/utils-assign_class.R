#' Extract class of variable
#'
#' Some packages append non-base classes to data frame columns, e.g.
#' if data is labeled with the `Hmisc` package the class of a string will
#' be `c("labelled", "character")` rather than `c("character")` only.  This
#' simple function extracts the base R class.
#'
#' @param data data frame
#' @param variable string vector of column names from data
#' @keywords internal

assign_class <- function(data, variable) {
  classes_expected <- c("character", "factor", "numeric", "logical", "integer", "double")

  # extracing the base R class
  classes_return <-
    purrr::map(
      variable,
      ~ class(data[[.x]]) %>% intersect(classes_expected)
    )

  # checking all columns returned a class
  class_error <- purrr::map_lgl(classes_return, ~ identical(.x, character(0)))
  if (any(class_error)) {
    stop(glue::glue(
      "Class of variable '{paste(variable[class_error], collapse = ', ')}' not supported"
    ))
  }

  # if column is all missing, return class NA
  purrr::map2_chr(
    variable, classes_return,
    ~ ifelse(data[[.x]] %>% is.na() %>% all(),
      NA_character_, .y
    )
  )
}
