#' determine the appropriate test type given two variables
#'
#' @param data input data set
#' @param var categorical or continuous variable for which a test with \code{by_var} is desired
#' @param var_summary_type summary_type from meta data
#' @param by_var categorical variable
#' @param test list of user defined statistical tests and corresponding variables
#' @return most appropriate test as text of the test function
#' @keywords internal
#' @author Daniel Sjoberg

assign_test <- function(data, var, var_summary_type, by_var, test, group) {
  purrr::map2_chr(
    var, var_summary_type,
    ~ assign_test_one(
      data = data,
      var = .x,
      var_summary_type = .y,
      by_var = by_var,
      test = test,
      group = group
    )
  )
}

assign_test_one <- function(data, var, var_summary_type, by_var, test, group) {
  # if the 'by' variable is null, no tests will be performed
  if (is.null(by_var)) return(NA_character_)

  # if user specifed test to be performed, do that test.
  if (!is.null(test[[var]])) return(test[[var]])

  # if group variable supplied, fit a random effects model
  if (!is.null(group) & length(unique(data[[by_var]])) == 2) return("re")

  # unless by_var has >2 levels, then return NA with a message
  if (!is.null(group) & length(unique(data[[by_var]])) > 2) {
    message(paste0(var, ": P-value calculation for clustered data when by variables have >2 levels is not currently supported"))
    return(NA_character_)
  }

  # for continuous data, default to non-parametric tests
  if (var_summary_type == "continuous" & length(unique(data[[by_var]])) == 2) {
    return("wilcox.test")
  }
  if (var_summary_type == "continuous") {
    return("kruskal.test")
  }

  # calculate expected counts
  min_exp <-
    expand.grid(table(data[[var]]), table(data[[by_var]])) %>%
    dplyr::mutate_(exp = ~ Var1 * Var2 /
      sum(table(data[[var]], data[[by_var]]))) %>%
    dplyr::pull(exp) %>%
    min()

  # if expected counts >= 5 for all cells, chisq, otherwise Fishers exact
  if (min_exp >= 5) return("chisq.test")
  return("fisher.test")
}

# assign_test(data = mtcars, var = c("hp", "mpg"), var_summary_type = c("continuous","continuous"),
#             by_var = NULL, test = NULL, group = NULL)
# assign_test(data = mtcars, var = c("hp", "mpg"), var_summary_type = c("continuous","continuous"),
#             by_var = "am", test = NULL, group = NULL)
# assign_test(data = mtcars, var = c("hp", "mpg", "cyl","vs"),
#             var_summary_type = c("continuous","continuous", "categorical", "dichotomous"),
#             by_var = "am", test = NULL, group = NULL)

# assign_test(data = mtcars, var = c("hp", "mpg", "cyl","vs"),
#             var_summary_type = c("continuous","continuous", "categorical", "dichotomous"),
#             by_var = "gear", test = NULL, group = "am")
