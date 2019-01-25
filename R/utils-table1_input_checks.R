
# function that checks the inputs to \code{\link{fmt_table1}}
# this should include EVERY input of \code{\link{fmt_table1}} in the same order
# copy and paste them from \code{\link{fmt_table1}}
fmt_table1_input_checks <- function(data, by, label, type,
                                    statistic, digits, missing, id) {
  # data -----------------------------------------------------------------------
  # data is a data frame
  if (!is.data.frame(data)) {
    stop(glue::glue(
      "'data' input must be a data frame."
    ))
  }

  # cannot be empty data frame
  if (nrow(data) == 0) {
    stop(glue::glue(
      "Expecting 'data' to have at least 1 row."
    ))
  }

  # by -------------------------------------------------------------------------
  # by is a variable in data
  if (!is.null(by)) {
    if (!(by %in% names(data))) {
      stop(glue::glue(
        "'{by}' not a column in 'data'."
      ))
    }

    # by levels cannot be missing
    if (sum(is.na(data[[by]])) > 0) {
      stop("'by' variable cannot have missing values.")
    }
  }

  # type -----------------------------------------------------------------------
  if (!is.null(type)) {
    # checking that all names in list are variable names from data.
    summary_type_not_in_data <- setdiff(names(type), names(data))
    if (length(summary_type_not_in_data) > 0) {
      message(glue::glue(
        "The following names from 'summary_type' are not found in 'data' and ",
        "were ignored: {paste0(summary_type_not_in_data, collapse = ', ')}"
      ))
    }

    # checking all inputs are continuous, categorial, or dichotomous
    summary_type_value_not_valid <-
      setdiff(
        type %>% unlist() %>% unique(),
        c("categorical", "dichotomous", "continuous")
      )
    if (length(summary_type_value_not_valid) > 0) {
      stop(glue::glue(
        "'summary_type' values must be 'continuous', 'categorical', or 'dichotomous'. ",
        "'{paste0(summary_type_value_not_valid, collapse = ', ')}' not valid."
      ))
    }
  }

  # label ----------------------------------------------------------------------
  if (!is.null(label)) {
    # checking that all names in list are variable names from data.
    var_label_not_in_data <- setdiff(names(label), names(data))
    if (length(var_label_not_in_data) > 0) {
      message(glue::glue(
        "The following names from 'var_label' are not found in 'data' and ",
        "were ignored: {paste0(var_label_not_in_data, collapse = ', ')}"
      ))
    }
  }

  # statistic ------------------------------------------------------------------
  if (!is.null(statistic)) {
    # checking that all names in list are continuous or categorical
    stat_display_names_not_valid <- setdiff(names(statistic), c("continuous", "categorical"))
    if (length(stat_display_names_not_valid) > 0) {
      message(glue::glue(
        "Expecting list names 'continuous' and 'categorical'. ",
        "The following names from 'stat_display' are not valid and ",
        "were ignored: {paste0(stat_display_names_not_valid, collapse = ', ')}"
      ))
    }
  }

  # digits ---------------------------------------------------------------------
  if (!is.null(digits)) {
    # checking that all names in list are variable names from data.
    digits_not_in_data <- setdiff(names(digits), names(data))
    if (length(digits_not_in_data) > 0) {
      message(glue::glue(
        "The following names from 'digits' are not found in 'data' and ",
        "were ignored: {paste0(digits_not_in_data, collapse = ', ')}"
      ))
    }

    # specified digits must be a non-negative integer
    digits_value_not_valid <-
      setdiff(digits %>% unlist() %>% unique(), 0:100)
    if (length(digits_value_not_valid) > 0) {
      stop(glue::glue(
        "'digits' values must be non-negative integers. ",
        "'{paste0(digits_value_not_valid, collapse = ', ')}' not valid input."
      ))
    }
  }

  # id -------------------------------------------------------------------------
  if (length(id) > 1) {
    stop(
      "'id' must be `NULL` or length 1."
    )
  }
}

# # data
#   # not putting in a data frame
#   fmt_table1_input_checks(
#     data = list(not = "a", proper = "data.frame"), by = NULL, summary_type = NULL, var_label = NULL,
#     stat_display = NULL, digits = NULL, pvalue_fun = NULL
#   )
#
# # by
#   # by var not in dataset
#   fmt_table1_input_checks(
#     data = mtcars, by = "Petal.Width", summary_type = NULL, var_label = NULL,
#     stat_display = NULL, digits = NULL, pvalue_fun = NULL
#   )
#
# # summary_type
#   # names not a variables name, and input type not valid
#   fmt_table1_input_checks(
#     data = mtcars, by = NULL, summary_type = list(hp = "continuous", length = "catttegorical"), var_label = NULL,
#     stat_display = NULL, digits = NULL, pvalue_fun = NULL
#   )
#
# # var_label
#   # names not a variables name, and input type not valid
#   fmt_table1_input_checks(
#     data = mtcars, by = NULL, summary_type = NULL, var_label = list(hp = "Horsepower", wrong_name = "Not a variable"),
#     stat_display = NULL, digits = NULL, pvalue_fun = NULL
#   )
#
# # stat_display
#   fmt_table1_input_checks(
#     data = mtcars, by = NULL, summary_type = NULL, var_label = NULL,
#     stat_display = list(continuous = "{mean}", dichot_not = "nope"), digits = NULL, pvalue_fun = NULL
#   )
#
# # digits
#   fmt_table1_input_checks(
#     data = mtcars, by = NULL, summary_type = NULL, var_label = NULL,
#     stat_display = NULL, digits = list(hp = 2, wrong_name = 2.1, am = 5.4), pvalue_fun = NULL
#   )

# fmt_table1_input_checks(
#   data = NULL, by = NULL, summary_type = NULL, var_label = NULL,
#   stat_display = NULL, digits = NULL, pvalue_fun = NULL
# )
