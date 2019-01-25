#' Guesses how many digits to use in rounding continuous variables
#' or summary statistics
#'
#' @param x vector containing the values of a continuous variable. This can be
#' raw data values or a vector of summary statistics themselves
#'
#' @return the rounded values
#'
#' @keywords internal
#'

# takes as the input a vector of variable and summary types
continuous_digits_guess <- function(data,
                                    variable,
                                    summary_type,
                                    class,
                                    digits = NULL) {
  purrr::pmap_dbl(
    list(variable, summary_type, class),
    ~ continuous_digits_guess_one(data, ..1, ..2, ..3, digits)
  )
}

# runs for a single var
continuous_digits_guess_one <- function(data,
                                        variable,
                                        summary_type,
                                        class,
                                        digits = NULL) {
  # if class is NA (meaning all values are NA), returning NA
  if (is.na(class)) return(NA)

  # if the number of digits is specified, return specified number
  if (!is.null(digits[[variable]])) return(digits[[variable]])

  # if the variable is not continuous type, return NA
  if (summary_type != "continuous") return(NA)

  # if class is integer, then round everythng to nearest integer
  if (class == "integer") return(0)

  # calculate the spread of the variable
  var_spread <- stats::quantile(data[[variable]], probs = c(0.95), na.rm = TRUE) -
    stats::quantile(data[[variable]], probs = c(0.05), na.rm = TRUE)

  # otherwise guess the number of dignits to use based on the spread
  dplyr::case_when(
    var_spread < 0.01 ~ 4,
    var_spread >= 0.01 & var_spread < 0.1 ~ 3,
    var_spread >= 0.1 & var_spread < 10 ~ 2,
    var_spread >= 10 & var_spread < 20 ~ 1,
    var_spread >= 20 ~ 0
  )
}
