#' For dichotomous data, returns that value that will be printed in table.
#'
#' @param data data frame
#' @param variable character variable name in \code{data} that will be tabulated
#' @param summary_type the type of summary statistics that will be calculated
#' @param class class of \code{variable}
#' @return value that will be printed in table for dichotomous data
#'
#' @keywords internal

# wrapper for assign_dichotomous_value_one() function
assign_dichotomous_value <- function(data, variable, summary_type, class) {
  purrr::pmap(list(variable, summary_type, class), ~ assign_dichotomous_value_one(data, ..1, ..2, ..3))
}

assign_dichotomous_value_one <- function(data, variable, summary_type, class) {

  # only assign value for dichotomous data
  if (summary_type != "dichotomous") {
    return(NULL)
  }

  # removing all NA values
  var_vector <- data[[variable]][!is.na(data[[variable]])]

  # if class is logical, then value will be TRUE
  if (class == "logical") {
    return(TRUE)
  }

  # if column provided is a factor with "Yes" and "No" (or "yes" and "no") then
  # the value is "Yes" (or "yes")
  if (class == "factor") {
    if (setequal(var_vector, c("Yes", "No")) |
      setequal(var_vector, "Yes") |
      setequal(var_vector, "No")) {
      return("Yes")
    }
    if (setequal(var_vector, c("yes", "no")) |
      setequal(var_vector, "yes") |
      setequal(var_vector, "no")) {
      return("yes")
    }
    if (setequal(var_vector, c("YES", "NO")) |
      setequal(var_vector, "YES") |
      setequal(var_vector, "NO")) {
      return("YES")
    }
  }

  # if column provided is all zeros and ones (or exclusively either one), the the value is one
  if (setequal(var_vector, c(0, 1)) |
    setequal(var_vector, 0) |
    setequal(var_vector, 1)) {
    return(1)
  }

  # otherwise, the value that will be displayed on is the largest value
  return(max(data[[variable]], na.rm = TRUE))
}

# assign_dichotomous_value_one(mtcars, "am", "dichotomous", "double")
