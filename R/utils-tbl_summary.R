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
#' @noRd
#' @author Daniel D. Sjoberg

assign_class <- function(data, variable) {
  classes_expected <- c("character", "factor", "numeric", "logical", "integer")

  # extracing the base R class
  classes_return <-
    map(
      variable,
      ~ class(data[[.x]]) %>% intersect(classes_expected)
    )

  # checking all columns returned a class
  class_error <- map_lgl(classes_return, ~ identical(.x, character(0)))
  if (any(class_error)) {
    stop(glue("Class of variable '{paste(variable[class_error], collapse = ', ')}' not supported"))
  }

  # if column is all missing, return class NA
  map2_chr(
    variable, classes_return,
    ~ ifelse(data[[.x]] %>% is.na() %>% all(),
      NA_character_, .y
    )
  )
}

#' For dichotomous data, returns that value that will be printed in table.
#'
#' @param data data frame
#' @param variable character variable name in \code{data} that will be tabulated
#' @param summary_type the type of summary statistics that will be calculated
#' @param class class of \code{variable}
#' @return value that will be printed in table for dichotomous data
#' @keywords internal
#' @noRd
#' @author Daniel D. Sjoberg

# wrapper for assign_dichotomous_value_one() function
assign_dichotomous_value <- function(data, variable, summary_type, class, value) {
  pmap(
    list(variable, summary_type, class),
    ~ assign_dichotomous_value_one(data, ..1, ..2, ..3, value)
  )
}

assign_dichotomous_value_one <- function(data, variable, summary_type, class, value) {

  # only assign value for dichotomous data
  if (summary_type != "dichotomous") {
    return(NULL)
  }

  # removing all NA values
  var_vector <- data[[variable]][!is.na(data[[variable]])]

  # if 'value' provided, then dichotomous_value is the provided one
  if (!is.null(value[[variable]])) {
    return(value[[variable]])
  }

  # if class is logical, then value will be TRUE
  if (class == "logical") {
    return(TRUE)
  }

  # if column provided is a factor with "Yes" and "No" (or "yes" and "no") then
  # the value is "Yes" (or "yes")
  if (class %in% c("factor", "character")) {
    if (setdiff(var_vector, c("Yes", "No")) %>% length() == 0) {
      return("Yes")
    }
    if (setdiff(var_vector, c("yes", "no")) %>% length() == 0) {
      return("yes")
    }
    if (setdiff(var_vector, c("YES", "NO")) %>% length() == 0) {
      return("YES")
    }
  }

  # if column provided is all zeros and ones (or exclusively either one), the the value is one
  if (setdiff(var_vector, c(0, 1)) %>% length() == 0) {
    return(1)
  }

  # otherwise, the value must be passed from the values argument to tbl_summary
  stop(glue(
    "'{variable}' is dichotomous, but I was unable to determine the ",
    "level to display. Use the 'value = list({variable} = <level>)' argument ",
    "to specify level."
  ))
}

# assign_dichotomous_value_one(mtcars, "am", "dichotomous", "double", NULL)



#' Assign type of summary statistic
#'
#' Function that assigns default statistics to display, or if specified,
#' assigns the user-defined statistics for display.
#'
#' @param variable Vector of variable names
#' @param summary_type A list that includes specified summary types
#' @param stat_display List with up to two named elements.  Names must be
#' continuous or categorical. Can be \code{NULL}.
#' @return vector of stat_display selections for each variable
#' @keywords internal
#' @noRd
#' @author Daniel D. Sjoberg

assign_stat_display <- function(variable, summary_type, stat_display) {
  # dichotomous and categorical are treated in the same fashion here
  summary_type <- ifelse(summary_type == "dichotomous", "categorical", summary_type)

  # otherwise, return defaults
  return(
    map2_chr(
      variable, summary_type,
      ~ case_when(
        .y == "continuous" ~
        stat_display[[.x]] %||%
          stat_display[["..continuous.."]] %||%
          "{median} ({p25}, {p75})",
        .y %in% c("categorical", "dichotomous") ~
        stat_display[[.x]] %||%
          stat_display[["..categorical.."]] %||%
          "{n} ({p}%)"
      )
    )
  )
}

# assign_stat_display("continuous", NULL)
# assign_stat_display(c("continuous", "dichotomous"), NULL)
# assign_stat_display(c("continuous", "dichotomous"), stat_display = list(continuous = "{median}"))

# assign_stat_display("continuous", NULL)
# assign_stat_display("continuous", list(dichotomous = "{n}/{N} ({p}%)"))
# assign_stat_display("dichotomous", list(dichotomous = "{n}/{N} ({p}%)"))



#' Assigns summary type (e.g. continuous, categorical, or dichotomous).
#'
#' For variables where the summary type was not specified in the function
#' call of `tbl_summary`, `assign_summary_type` assigns a type based on class and
#' number of unique levels.
#'
#' @param data Data frame.
#' @param variable Vector of column name.
#' @param class Vector of classes (e.g. numeric, character, etc.)
#' corresponding one-to-one with the names in `variable`.
#' @param summary_type list that includes specified summary types,
#' e.g. \code{summary_type = list(age = "continuous")}
#' @return Vector summary types `c("continuous", "categorical", "dichotomous")`.
#' @keywords internal
#' @noRd
#' @author Daniel D. Sjoberg
#' @examples
#' gtsummary:::assign_summary_type(
#'   data = mtcars,
#'   variable = names(mtcars),
#'   class = apply(mtcars, 2, class),
#'   summary_type = NULL, value = NULL
#' )
assign_summary_type <- function(data, variable, class, summary_type, value) {
  map2_chr(
    variable, class,
    ~ summary_type[[.x]] %||%
      case_when(
        # if a value to display was supplied, then dichotomous
        !is.null(value[[.x]]) &
          length(intersect(value[[.x]], data[[.x]]))
        ~ "dichotomous",

        # logical variables will be dichotmous
        .y == "logical" ~ "dichotomous",

        # numeric variables that are 0 and 1 only, will be dichotomous
        .y %in% c("integer", "numeric") & length(setdiff(na.omit(data[[.x]]), c(0, 1))) == 0
        ~ "dichotomous",

        # factor variables that are "No" and "Yes" only, will be dichotomous
        .y %in% c("factor") & length(setdiff(na.omit(data[[.x]]), c("No", "Yes"))) == 0
        ~ "dichotomous",
        .y %in% c("factor") & length(setdiff(na.omit(data[[.x]]), c("no", "yes"))) == 0
        ~ "dichotomous",
        .y %in% c("factor") & length(setdiff(na.omit(data[[.x]]), c("NO", "YES"))) == 0
        ~ "dichotomous",

        # factors and characters are categorical
        .y %in% c("factor", "character") ~
        "categorical",

        # numeric variables with fewer than 10 levels will be categorical
        .y %in% c("integer", "numeric") & length(unique(na.omit(data[[.x]]))) < 10
        ~ "categorical",

        # everything else is assigned to continuous
        TRUE ~ "continuous"
      )
  )
}


# n = 50
# dta = tibble(
#   age = rnorm(n) + 35,
#   female = sample(c(T, F), size = n, replace = T),
#   male = as.numeric(female),
#   sex = ifelse(female == T, "Female", "Male"),
#   country = sample(c("USA", "Canada"), size = n, replace = T),
#   shoe_size = sample(1:25, size = n, replace = T),
#   family_size = sample(1:5, size = n, replace = T)
# )
# #adding missing values
# dta = mutate_all(dta, funs( ifelse(runif(n) < 0.25, NA, .)) ) %>%
#   mutate(
#     sex = as.factor(sex),
#     male_fct = ifelse(female == TRUE, "No", "Yes") %>% factor()
#   )
# dta
#
# # creating base meta data dataframe
# meta_data =
#   tibble(
#     variable = names(dta),
#     class = map_chr(variable, ~ class(dta[[.x]]))
#   )
# meta_data
#
# # tesing function's guessing ability
# meta_data %>%
#   mutate(
#     assign_summary_type = assign_summary_type(dta, variable, class, NULL)
#   )
#
#
# # tesing function's ability when type assigned
# meta_data %>%
#   mutate(
#     assign_summary_type = assign_summary_type(dta, variable, class, list(shoe_size = "categorical"))
#   )



#' determine the appropriate test type given two variables
#'
#' @param data input data set
#' @param var categorical or continuous variable for which a test with \code{by_var} is desired
#' @param var_summary_type summary_type from meta data
#' @param by_var categorical variable
#' @param test list of user defined statistical tests and corresponding variables
#' @return most appropriate test as text of the test function
#' @noRd
#' @keywords internal
#' @author Daniel D. Sjoberg

assign_test <- function(data, var, var_summary_type, by_var, test, group) {
  map2_chr(
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
  if (is.null(by_var)) {
    return(NA_character_)
  }

  # if user specifed test to be performed, do that test.
  if (!is.null(test[[var]])) {
    return(test[[var]])
  }

  # if user specifed test to be performed for ..continuous.. or
  # ..categorical.., do that test for that class of variable
  if (!is.null(test[["..continuous.."]]) & var_summary_type == "continuous") {
    return(test[["..continuous.."]])
  }
  if (!is.null(test[["..categorical.."]]) &
    var_summary_type %in% c("categorical", "dichotomous")) {
    return(test[["..categorical.."]])
  }

  # if group variable supplied, fit a random effects model
  if (!is.null(group) & length(unique(data[[by_var]])) == 2) {
    return("lme4")
  }

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
    mutate(exp = .data$Var1 * .data$Var2 /
      sum(table(data[[var]], data[[by_var]]))) %>%
    pull(exp) %>%
    min()

  # if expected counts >= 5 for all cells, chisq, otherwise Fishers exact
  if (min_exp >= 5) {
    return("chisq.test")
  }
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


#' Assigns variable label to display.
#'
#' Preference is given to labels specified in `fmt_table1(..., var_label = list())`
#' argument, then to a label attribute attached to the data frame
#' (i.e. attr(data$var, "label)), then to the variable name.
#'
#' @param data Data frame.
#' @param variable Vector of column name.
#' @param var_label list that includes specified variable labels,
#' e.g. `var_label = list(age = "Age, yrs")`
#' @return Vector variable labels.
#' @keywords internal
#' @author Daniel D. Sjoberg
#' @examples
#' gtsummary:::assign_var_label(mtcars, names(mtcars), list(hp = "Horsepower"))
assign_var_label <- function(data, variable, var_label) {
  map_chr(
    variable,
    ~ var_label[[.x]] %||%
      attr(data[[.x]], "label") %||%
      .x
  )
}


#' This function takes in the meta data table, and calls the appropriate summarize function.
#'
#' @param data Data frame
#' @param variable Character variable name in \code{data} that will be tabulated
#' @param by Character variable name in\code{data} that Summary statistics for
#' \code{variable} are stratified
#' @param summary_type A list that includes specified summary types.
#' @param var_label String label
#' @param dichotomous_value If the output is dichotomous, then this is the value
#' @param stat_display String that specifies the format of the displayed statistics.
#' The syntax follows \code{glue::glue()} inputs with n, N, and p as input options.
#' of the variable that will be displayed.
#' @param digits integer indicating the number of decimal places to be used.
#' @param class variable class.  If class is NA, then all values are NA, and no
#' summary statistics will be calculated.
#' @param missing whether to include NA values in the table. `missing` controls
#' if the table includes counts of NA values: the allowed values correspond to
#' never ("no"), only if the count is positive ("ifany") and even for
#' zero counts ("always"). Default is "ifany".
#' @param missing_text String to display for count of missing observations.
#' @param sort string indicating whether to sort categorical
#' variables by 'alphanumeric' or 'frequency'
#' @param percent indicates the type of percentage to return. Must be one of
#' `"column"`, `"row"`, or `"cell"`. Default is `"column"`
#' @noRd
#' @keywords internal
#' @author Daniel D. Sjoberg

calculate_summary_stat <- function(data, variable, by, summary_type,
                                   dichotomous_value, var_label, stat_display,
                                   digits, class, missing, missing_text, sort,
                                   percent) {

  # if class is NA, then do not calculate summary statistics
  if (is.na(class)) {
    # empty results table when no by variable
    if (is.null(by)) {
      return(
        tibble(
          row_type = c("label", "missing"),
          label = c(var_label, missing_text),
          stat_0 = c(NA_character_, as.character(nrow(data)))
        )
      )
    }
    # empty results table when there is a by variable
    if (!is.null(by)) {
      stat_col_names <- df_by(data, by)[["by_col"]]
      return(
        tibble(
          row_type = c("label", "missing"),
          label = c(var_label, missing_text)
        ) %>%
          left_join(
            table(data[[by]]) %>%
              as.matrix() %>%
              t() %>%
              as_tibble() %>%
              mutate_all(as.character) %>%
              set_names(stat_col_names) %>%
              mutate(row_type = "missing")
          )
      )
    }
  }

  # return data table with continuous summary stats
  if (summary_type == "continuous") {
    return(
      summarize_continuous(
        data, variable, by, digits,
        var_label, stat_display, missing, missing_text
      )
    )
  }

  # return data table with categorical or dichotomous summary stats
  if (summary_type %in% c("categorical", "dichotomous")) {
    return(
      summarize_categorical(
        data, variable, by, var_label, stat_display, dichotomous_value,
        missing, missing_text, sort, percent
      )
    )
  }
}

# calculate_summary_stat(data = mtcars, variable = "hp", by = "am",
#                        summary_type = "continuous", dichotomous_value = NULL,
#                        var_label = "Horsepower", stat_display = "{median} ({q1}, {q3})",
#                        digits = 2, pvalue = "<0.0001", class = NA)
# calculate_summary_stat(data = mtcars, variable = "hp", by = NULL,
#                        summary_type = "continuous", dichotomous_value = NULL,
#                        var_label = "Horsepower", stat_display = "{median} ({q1}, {q3})",
#                        digits = 2, pvalue = "<0.0001", class = NA)
# calculate_summary_stat(data = mtcars, variable = "hp", by = "am",
#                        summary_type = "continuous", dichotomous_value = NULL,
#                        var_label = "Horsepower", stat_display = "{median} ({q1}, {q3})",
#                        digits = 2, pvalue = "<0.0001")
#
# calculate_summary_stat(data = mtcars, variable = "hp", by = NULL,
#                        summary_type = "continuous", dichotomous_value = NULL,
#                        var_label = "Horsepower", stat_display = "{median} ({q1}, {q3})",
#                        digits = 2, pvalue = "<0.0001")
#
#
# calculate_summary_stat(data = mtcars, variable = "cyl", by = NULL,
#                        summary_type = "categorical", dichotomous_value = NULL,
#                        var_label = "Horsepower", stat_display = "{n}/{N} ({p}%)",
#                        digits = NULL, pvalue = "<0.0001")



#' Guesses how many digits to use in rounding continuous variables
#' or summary statistics
#'
#' @param x vector containing the values of a continuous variable. This can be
#' raw data values or a vector of summary statistics themselves
#' @return the rounded values
#' @noRd
#' @keywords internal
#' @author Emily Zabor, Daniel D. Sjoberg

# takes as the input a vector of variable and summary types
continuous_digits_guess <- function(data,
                                    variable,
                                    summary_type,
                                    class,
                                    digits = NULL) {
  pmap(
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
  if (is.na(class)) {
    return(NA)
  }

  # if the variable is not continuous type, return NA
  if (summary_type != "continuous") {
    return(NA)
  }

  # if the number of digits is specified for a variable, return specified number
  if (!is.null(digits[[variable]])) {
    return(digits[[variable]])
  }

  # if the number of digits is specified for a all continuous variables,
  # return specified number
  if (!is.null(digits[["..continuous.."]])) {
    return(digits[["..continuous.."]])
  }

  # if class is integer, then round everythng to nearest integer
  if (class == "integer") {
    return(0)
  }

  # calculate the spread of the variable
  var_spread <- stats::quantile(data[[variable]], probs = c(0.95), na.rm = TRUE) -
    stats::quantile(data[[variable]], probs = c(0.05), na.rm = TRUE)

  # otherwise guess the number of dignits to use based on the spread
  case_when(
    var_spread < 0.01 ~ 4,
    var_spread >= 0.01 & var_spread < 0.1 ~ 3,
    var_spread >= 0.1 & var_spread < 10 ~ 2,
    var_spread >= 10 & var_spread < 20 ~ 1,
    var_spread >= 20 ~ 0
  )
}

#' Simple utility function to get extract and calculate additional information
#' about the 'by' variable in \code{\link{tbl_summary}}
#'
#' Given a dataset and the name of the 'by' variable, this function returns a
#' data frame with unique levels of the by variable, the by variable ID, a character
#' version of the levels, and the column name for each level in the \code{\link{tbl_summary}}
#' output data frame.
#'
#' @param data data frame
#' @param by character name of the `by` variable found in data
#' @noRd
#' @keywords internal
#' @author Daniel D. Sjoberg

df_by <- function(data, by) {
  data %>%
    select(c(by)) %>%
    set_names("by") %>%
    count(!!sym("by")) %>%
    mutate(N = sum(.data$n), p = .data$n / .data$N) %>%
    arrange(!!sym("by")) %>%
    mutate(
      by_id = 1:n(), # 'by' variable ID
      by_chr = as.character(.data$by), # Character version of 'by' variable
      by_col = paste0("stat_", .data$by_id) # Column name of in fmt_table1 output
    ) %>%
    select(starts_with("by"), everything())
}
# > df_by(mtcars, "am")
# # A tibble: 2 x 7
#      by     n     N     p by_id by_chr by_col
#   <dbl> <int> <int> <dbl> <int> <chr>  <chr>
# 1     0    19    32 0.594     1 0      stat_1
# 2     1    13    32 0.406     2 1      stat_2

# > df_by(iris, "Species")
# # A tibble: 3 x 7
#   by             n     N     p by_id by_chr     by_col
#   <fct>      <int> <int> <dbl> <int> <chr>      <chr>
# 1 setosa        50   150 0.333     1 setosa     stat_1
# 2 versicolor    50   150 0.333     2 versicolor stat_2
# 3 virginica     50   150 0.333     3 virginica  stat_3


#' Calculates and formats N's and percentages for categorical and dichotomous data
#'
#' @param data Data frame
#' @param variable Character variable name in `data` that will be tabulated
#' @param by Character variable name in `data` that Summary statistics for
#' `variable` are stratified
#' @param var_label String label
#' @param stat_display String that specifies the format of the displayed statistics.
#' The syntax follows \code{\link[glue]{glue}} inputs with n, N, and p as input options.
#' @param dichotomous_value If the output is dichotomous, then this is the value
#' of the variable that will be displayed.
#' @param missing whether to include `NA` values in the table. `missing` controls
#' if the table includes counts of `NA` values: the allowed values correspond to
#' never (`"no"`), only if the count is positive (`"ifany"`) and even for
#' zero counts (`"always"`). Default is `"ifany"`.
#' @param missing_text String to display for count of missing observations.
#' @param sort string indicating whether to sort categorical
#' variables by 'alphanumeric' or 'frequency'
#' @param percent indicates the type of percentage to return. Must be one of
#' `"column"`, `"row"`, or `"cell"`. Default is `"column"`
#' @param percent_fun function to round and format percentages.  Default
#' is `style_percent()`
#' @return formatted summary statistics in a tibble.
#' @noRd
#' @keywords internal
#' @author Daniel D. Sjoberg

summarize_categorical <- function(data, variable, by, var_label,
                                  stat_display, dichotomous_value, missing,
                                  missing_text, sort, percent) {
  percent_fun <-
    getOption("gtsummary.tbl_summary.percent_fun",
      default = style_percent
    )
  if (!rlang::is_function(percent_fun)) {
    stop(paste0(
      "'percent_fun' is not a valid function.  Please pass only a function\n",
      "object. For example, to round percentages to 2 decimal places, \n\n",
      "'options(gtsummary.tbl_summary.percent_fun = function(x) sprintf(\"%.2f\", 100 * x))'"
    ))
  }

  # counting total missing
  tot_n_miss <- sum(is.na(data[[variable]]))

  # tidyr::complete throws warning `has different attributes on LHS and RHS of join`
  # when variable has label.  So deleting it.
  attr(data[[variable]], "label") <- NULL
  if (!is.null(by)) attr(data[[by]], "label") <- NULL
  # same thing when the class "labelled" is included when labeled with the Hmisc package
  class(data[[variable]]) <- setdiff(class(data[[variable]]), "labelled")
  if (!is.null(by)) {
    class(data[[by]]) <- setdiff(class(data[[by]]), "labelled")
  }

  # grouping by var
  if (!is.null(by)) {
    data <-
      data %>%
      select(c(variable, by)) %>%
      set_names(c("variable", "by")) %>%
      left_join(df_by(data, by), by = "by") %>%
      select(c(variable, "by_col"))
  }
  else {
    data <-
      data %>%
      select(c(variable)) %>%
      set_names(c("variable")) %>%
      mutate(by_col = "stat_0") %>%
      select(c(variable, "by_col"))
  }

  # row or column percents
  # for column percent, group by 'by_col'
  # for row percents, group by 'variable'
  percent_group_by_var <-
    case_when(
      percent == "column" ~ "by_col",
      percent == "row" ~ "variable",
      percent == "cell" ~ ""
    )

  # nesting data and changing by variable
  tab0 <-
    data %>%
    stats::na.omit() %>%
    group_by(!!sym("by_col")) %>%
    count(!!sym("variable")) %>%
    ungroup()

  # if there is a dichotomous value supplied, merging it in to ensure it gets counted (when unobserved)
  if (!is.null(dichotomous_value)) {
    # making factors character here
    if (is.factor(tab0$variable)) {
      tab0 <-
        tab0 %>%
        mutate(variable = as.character(variable)) %>%
        full_join(tibble(variable = as.character(dichotomous_value)), by = "variable")
    }
    else {
      tab0 <-
        tab0 %>%
        full_join(tibble(variable = dichotomous_value), by = "variable")
    }
  }

  tab <-
    tab0 %>%
    complete(!!sym("by_col"), !!sym("variable"), fill = list(n = 0)) %>%
    stats::na.omit() %>% # this is needed when the dichot value is unobserved in dataset
    group_by(!!sym("variable")) %>%
    mutate(var_level_freq = sum(.data$n)) %>%
    group_by(!!sym(percent_group_by_var)) %>%
    mutate(
      N = sum(.data$n),
      p = percent_fun(.data$n / .data$N),
      stat = as.character(glue(stat_display))
    ) %>%
    select(c("by_col", "var_level_freq", "variable", "stat")) %>%
    group_by(!!sym("by_col")) %>%
    spread(!!sym("by_col"), !!sym("stat")) %>%
    mutate(
      row_type = "level",
      label = .data$variable %>% as.character()
    )

  # if sort == "frequency", then sort data before moving forward
  if (sort == "frequency") {
    tab <-
      tab %>%
      arrange(desc(.data$var_level_freq))
  }
  else if (sort == "alphanumeric") {
    tab <-
      tab %>%
      arrange(.data$variable)
  }

  # keeping needed vars
  tab <-
    tab %>%
    select(c("variable", "row_type", "label", starts_with("stat_")))

  # number of missing observations
  missing_count <-
    data %>%
    group_by(!!sym("by_col")) %>%
    nest() %>%
    mutate(
      missing_count = map_chr(data, ~ .x[[1]] %>%
        is.na() %>%
        sum())
    ) %>%
    select(c("by_col", "missing_count")) %>%
    spread(!!sym("by_col"), !!sym("missing_count")) %>%
    mutate(
      row_type = "missing",
      label = missing_text
    )

  # formatting for dichotomous variables
  if (!is.null(dichotomous_value)) {
    results <-
      tab %>%
      filter(!!parse_expr("variable == dichotomous_value")) %>%
      mutate(
        row_type = "label",
        label = var_label
      ) %>%
      select(-c(variable)) %>%
      bind_rows(missing_count)
  }
  # formatting for categorical variables
  else {
    results <-
      tibble(
        row_type = "label",
        label = var_label
      ) %>%
      bind_rows(tab %>% select(-c("variable"))) %>%
      bind_rows(missing_count)
  }

  # excluding missing row if indicated
  if (missing == "no" | (missing == "ifany" & tot_n_miss == 0)) {
    results <-
      results %>%
      filter(!!parse_expr("row_type != 'missing'"))
  }

  results
}

# summarize_categorical(
#   data = lung, variable = "ph.karno", by = "sex", var_label = "WTF",
#   stat_display = "{n}/{N} ({p}%)", dichotomous_value = 50, missing = "ifany",
#   percent = "column"
# )
# summarize_categorical(
#   data = lung, variable = "ph.karno", by = "sex", var_label = "WTF",
#   stat_display = "{n}/{N} ({p}%)", dichotomous_value = NULL, missing = "ifany",
#   percent = "column"
# )
# summarize_categorical(
#   data = lung, variable = "ph.karno", by = NULL, var_label = "WTF",
#   stat_display = "{n}/{N} ({p}%)", dichotomous_value = 50
# )
#
# summarize_categorical(
#   data = mtcars, variable = "cyl", by = NULL, var_label = "WTF",
#   stat_display = "{n} ({p}%)", dichotomous_value = NULL
# )
#
# summarize_categorical(
#   data = mtcars, variable = "cyl", by = "am", var_label = "WTF",
#   stat_display = "{n} ({p}%)", dichotomous_value = NULL, missing = "ifany",
#   percent = "column"
# )




#' Calculates and formats summary statistics for continuous data
#'
#' @param data data frame
#' @param variable Character variable name in `data` that will be tabulated
#' @param by Character variable name in `data` that Summary statistics for
#' `variable` are stratified
#' @param digits vector of integers (or single integer) indicating the number
#' of decimal places to be used for rounding.
#' @param var_label string label
#' @param stat_display String that specifies the format of the displayed statistics.
#' The syntax follows \code{\link[glue]{glue}} inputs with n, N, and p as input options.
#' @param missing whether to include `NA` values in the table. `missing` controls
#' if the table includes counts of `NA` values: the allowed values correspond to
#' never (`"no"`), only if the count is positive (`"ifany"`) and even for
#' zero counts (`"always"`). Default is `"ifany"`.
#' @param missing_text String to display for count of missing observations.
#' @return formatted summary statistics in a tibble.
#' @noRd
#' @keywords internal
#' @author Daniel D. Sjoberg
#' @importFrom stringr str_extract_all str_remove_all fixed

summarize_continuous <- function(data, variable, by, digits,
                                 var_label, stat_display, missing, missing_text) {

  # counting total missing
  tot_n_miss <- sum(is.na(data[[variable]]))

  # grouping by var
  if (!is.null(by)) {
    # left_join throws warning `has different attributes on LHS and RHS of join`
    # when variable has label.  So deleting it.
    df_by <- df_by(data, by)
    attr(df_by[["by"]], "label") <- NULL
    attr(data[[by]], "label") <- NULL

    data <-
      data %>%
      select(c(variable, by)) %>%
      set_names(c("variable", "by")) %>%
      left_join(df_by, by = "by") %>%
      select(c(variable, "by_col"))
  }
  else {
    data <-
      data %>%
      select(c(variable)) %>%
      set_names(c("variable")) %>%
      mutate(by_col = "stat_0") %>%
      select(c(variable, "by_col"))
  }

  # nesting data and changing by variable
  data <-
    data %>%
    group_by(!!sym("by_col")) %>%
    nest()

  # nesting data and calculating descriptive stats
  stats <-
    data %>%
    mutate(
      # extracting list of statisitcs that need to be calculated
      stat_name_list = str_extract_all(stat_display, "\\{.*?\\}") %>%
        map(str_remove_all, pattern = fixed("}")) %>%
        map(str_remove_all, pattern = fixed("{")),
      # calculating statistics
      stat_result_list = map2(
        data, .data$stat_name_list,
        ~ calculate_single_stat(.x[[1]], .y)
      ),
      # getting a vector indicating the number of digits to round each requested statistic
      round_digits = map(.data$stat_result_list, ~ rep(digits, length.out = length(.x))),
      # rounding each statistic
      stat_result_list_fmt = map2(
        .data$stat_result_list, .data$round_digits,
        ~ map2_chr(.x, .y, function(stat, digit) sprintf(glue("%.{digit}f"), stat))
      ),
      # converting stats into a tibble with names as the type of statistic (i.e. mean column is called mean)
      df_result = map2(
        .data$stat_name_list, .data$stat_result_list_fmt,
        ~ .y %>%
          t() %>%
          as_tibble(.name_repair = "minimal") %>%
          set_names(.x)
      ),
      # rounding statistics and concatenating results
      stat = map_chr(
        .data$df_result,
        ~ .x %>%
          mutate(
            stat = as.character(glue(stat_display))
          ) %>%
          pull("stat")
      )
    ) %>%
    select(c("by_col", "stat")) %>%
    spread(!!sym("by_col"), !!sym("stat")) %>%
    mutate(
      row_type = "label",
      label = var_label
    ) %>%
    select(c("row_type", "label", starts_with("stat_")))

  # number of missing observations
  missing_count <-
    data %>%
    mutate(
      missing_count =
        map_chr(
          data,
          ~ .x[[1]] %>%
            is.na() %>%
            sum()
        )
    ) %>%
    select(c("by_col", "missing_count")) %>%
    spread(!!sym("by_col"), !!sym("missing_count")) %>%
    mutate(
      row_type = "missing",
      label = missing_text
    )

  # stacking stats and missing row
  result <-
    stats %>%
    bind_rows(missing_count)

  # excluding missing row if indicated
  if (missing == "no" | (missing == "ifany" & tot_n_miss == 0)) {
    result <-
      result %>%
      filter(!!parse_expr("row_type != 'missing'"))
  }

  result
}

#' Assigns categorical variables sort type ("alphanumeric" or "frequency")
#'
#' @param variable variable name
#' @param summary_type the type of variable ("continuous", "categorical", "dichotomous")
#' @param sort named list indicating the type of sorting to perform. Default is NULL.
#' The names of the list elements are variable names or '..categorical..' for assigning
#' all variables of that type.  If both a variable name and '..categorical..' are
#' specified, the variable name takes precedent
#' @noRd
#' @keywords internal
#' @author Daniel D. Sjoberg

# this function assigns categorical variables sort type ("alphanumeric" or "frequency")
assign_sort <- function(variable, summary_type, sort) {
  purrr::map2_chr(
    variable, summary_type,
    function(variable, summary_type) {
      # only assigning sort type for caegorical data
      if (summary_type == "dichotomous") {
        return("alphanumeric")
      }
      if (summary_type != "categorical") {
        return(NA_character_)
      }

      # if variable was specified, then use that
      if (!is.null(sort[[variable]])) {
        return(sort[[variable]])
      }

      # if the sort list has ..categorical.. name, then use that for all categorical variables
      if (!is.null(sort[["..categorical.."]])) {
        return(sort[["..categorical.."]])
      }

      # otherwise, return "alphanumeric"
      return("alphanumeric")
    }
  )
}

# assign_sort(variable = c("trt", "grade", "stage", "age"),
#             summary_type = c("categorical", "categorical", "categorical", "continuous"),
#             sort = list(..categorical.. = "frequency",
#                         stage = "alphanumeric"))


# stat_name that are accepted
calculate_single_stat <- function(x, stat_name) {
  map_dbl(
    stat_name,
    function(name) {
      # calculating percentiles if requested
      if (name %in% paste0("p", 0:100)) {
        do.call(
          "quantile",
          list(
            x,
            probs = as.numeric(gsub("[^0-9\\.]", "", name)) / 100,
            na.rm = TRUE
          )
        )
      }
      # calculating summary stats, input MUST be a function name
      # first argument is x and must take argument 'na.rm = TRUE'
      else {
        do.call(name, list(stats::na.omit(x)))
        # do.call(name, list(x, na.rm = TRUE))
      }
    }
  )
}


# calculate_single_stat(mtcars$mpg, c("p50", "p70"))
#
# summarize_continuous(
#   data = mtcars, variable = "mpg", by = "vs", digits = 0,
#   var_label = "MPG!", stat_display = "{p30} ({p98})", missing = "no"
# )






# function that checks the inputs to \code{\link{tbl_summary}}
# this should include EVERY input of \code{\link{tbl_summary}} in the same order
# copy and paste them from \code{\link{tbl_summary}}

tbl_summary_input_checks <- function(data, by, label, type, value, statistic,
                                     digits, missing, missing_text, sort) {
  # data -----------------------------------------------------------------------
  # data is a data frame
  if (!is.data.frame(data)) {
    stop("'data' input must be a data frame.")
  }

  # cannot be empty data frame
  if (nrow(data) == 0L) {
    stop("Expecting 'data' to have at least 1 row.")
  }

  # cannot include variables named ..continuous.. or ..categorical..
  if (c("..continuous..", "..categorical..") %in% names(data) %>% any()) {
    stop("Column names '..continuous..' and '..categorical..' are not allowed.")
  }

  # by -------------------------------------------------------------------------
  # by is a variable in data
  if (!is.null(by)) {
    if (!(by %in% names(data))) {
      stop(glue("'{by}' not a column in 'data'."))
    }

    # by levels cannot be missing
    if (sum(is.na(data[[by]])) > 0) {
      stop("'by' variable cannot have missing values.")
    }

    # by must be charactst of length 1
    if (!is.character(by) | length(by) > 1) {
      stop("'by' must be a character vector of length 1.")
    }
  }

  # type -----------------------------------------------------------------------
  if (!is.null(type) & is.null(names(type))) { # checking names for deprecated named list input

    # checking input type: must be a list of formulas, or one formula
    if (!class(type) %in% c("list", "formula")) {
      stop(glue(
        "'type' argument must be a list of formulas. ",
        "LHS of the formula is the variable specification, ",
        "and the RHS is the type specification: ",
        "list(vars(age, marker) ~ \"continuous\")"
      ))
    }
    if ("list" %in% class(type)) {
      if (some(type, negate(rlang::is_bare_formula))) {
        stop(glue(
          "'type' argument must be a list of formulas. ",
          "LHS of the formula is the variable specification, ",
          "and the RHS is the type specification: ",
          "list(vars(age, marker) ~ \"continuous\")"
        ))
      }
    }

    # all sepcifed types are continuous, categorical, or dichotomous
    if ("formula" %in% class(type)) type <- list(type)
    if (!every(type, ~ eval(rlang::f_rhs(.x)) %in% c("continuous", "categorical", "dichotomous")) |
      !every(type, ~ rlang::is_string(eval(rlang::f_rhs(.x))))) {
      stop(glue(
        "The RHS of the formula in the 'type'  argument must of one and only one of ",
        "\"continuous\", \"categorical\", or \"dichotomous\""
      ))
    }

    # functions all_continuous, all_categorical, and all_dichotomous cannot be used for type
    if (some(
      type,
      ~ deparse(.x) %>% # converts a formula to a string
        stringr::str_detect(c("all_continuous()", "all_categorical()", "all_dichotomous()")) %>%
        any()
    )) {
      stop(glue(
        "Select functions all_continuous(), all_categorical(), all_dichotomous() ",
        "cannot be used in the 'type' argument."
      ))
    }
  }

  # value ----------------------------------------------------------------------
  if (!is.null(value) & is.null(names(value))) { # checking names for deprecated named list input

    # checking input type: must be a list of formulas, or one formula
    if (!class(value) %in% c("list", "formula")) {
      stop(glue(
        "'value' argument must be a list of formulas. ",
        "LHS of the formula is the variable specification, ",
        "and the RHS is the value specification: ",
        "list(vars(stage) ~ \"T1\")"
      ))
    }
    if ("list" %in% class(value)) {
      if (some(value, negate(rlang::is_bare_formula))) {
        stop(glue(
          "'value' argument must be a list of formulas. ",
          "LHS of the formula is the variable specification, ",
          "and the RHS is the value specification: ",
          "list(vars(stage) ~ \"T1\")"
        ))
      }
    }

    # functions all_continuous, all_categorical, and all_dichotomous cannot be used for value
    if (some(
      value,
      ~ deparse(.x) %>% # converts a formula to a string
        stringr::str_detect(c("all_continuous()", "all_categorical()", "all_dichotomous()")) %>%
        any()
    )) {
      stop(glue(
        "Select functions all_continuous(), all_categorical(), all_dichotomous() ",
        "cannot be used in the 'value' argument."
      ))
    }
  }

  # label ----------------------------------------------------------------------
  if (!is.null(label) & is.null(names(label))) { # checking names for deprecated named list input

    # checking input type: must be a list of formulas, or one formula
    if (!class(label) %in% c("list", "formula")) {
      stop(glue(
        "'label' argument must be a list of formulas. ",
        "LHS of the formula is the variable specification, ",
        "and the RHS is the label specification: ",
        "list(vars(stage) ~ \"T Stage\")"
      ))
    }
    if ("list" %in% class(label)) {
      if (purrr::some(label, negate(rlang::is_bare_formula))) {
        stop(glue(
          "'label' argument must be a list of formulas. ",
          "LHS of the formula is the variable specification, ",
          "and the RHS is the label specification: ",
          "list(vars(stage) ~ \"T Stage\")"
        ))
      }
    }

    # all sepcifed labels must be a string of length 1
    if ("formula" %in% class(label)) label <- list(label)
    if (!every(label, ~ rlang::is_string(eval(rlang::f_rhs(.x))))) {
      stop(glue(
        "The RHS of the formula in the 'label' argument must be a string."
      ))
    }
  }

  # statistic ------------------------------------------------------------------
  if (!is.null(statistic) & is.null(names(statistic))) { # checking names for deprecated named list input

    # checking input type: must be a list of formulas, or one formula
    if (!class(statistic) %in% c("list", "formula")) {
      stop(glue(
        "'statistic' argument must be a list of formulas. ",
        "LHS of the formula is the variable specification, ",
        "and the RHS is the statistic specification: ",
        "list(all_categorical() ~ \"{n} / {N}\")"
      ))
    }
    if ("list" %in% class(statistic)) {
      if (some(statistic, negate(rlang::is_bare_formula))) {
        stop(glue(
          "'statistic' argument must be a list of formulas. ",
          "LHS of the formula is the variable specification, ",
          "and the RHS is the statistic specification: ",
          "list(all_categorical() ~ \"{n} / {N}\")"
        ))
      }
    }

    # all sepcifed statistics must be a string of length 1
    if ("formula" %in% class(statistic)) statistic <- list(statistic)
    if (!every(statistic, ~ rlang::is_string(eval(rlang::f_rhs(.x))))) {
      stop(glue(
        "The RHS of the formula in the 'statistic' argument must be a string."
      ))
    }
  }

  # digits ---------------------------------------------------------------------
  if (!is.null(digits) & is.null(names(digits))) { # checking names for deprecated named list input

    # checking input type: must be a list of formulas, or one formula
    if (!class(digits) %in% c("list", "formula")) {
      stop(glue(
        "'digits' argument must be a list of formulas. ",
        "LHS of the formula is the variable specification, ",
        "and the RHS is the digits specification: ",
        "list(vars(age, marker) ~ 1)"
      ))
    }
    if ("list" %in% class(digits)) {
      if (some(digits, negate(rlang::is_bare_formula))) {
        stop(glue(
          "'digits' argument must be a list of formulas. ",
          "LHS of the formula is the variable specification, ",
          "and the RHS is the digits specification: ",
          "list(vars(age, marker) ~ 1)"
        ))
      }
    }

    # # specified digits must be a non-negative integer
    # digits_value_not_valid <-
    #   setdiff(digits %>% unlist() %>% unique(), 0:100)
    # if (length(digits_value_not_valid) > 0) {
    #   stop(glue(
    #     "'digits' values must be non-negative integers. ",
    #     "'{paste0(digits_value_not_valid, collapse = ', ')}' not valid input."
    #   ))
    # }
  }

  # missing_text ---------------------------------------------------------------
  # input must be character
  if (!"character" %in% class(missing_text)) {
    stop("Argument 'missing_text' must be a character string.")
  }
  # checking the length is one
  if (length(missing_text) != 1) {
    stop("Argument 'missing_text' must be a character string of length 1.")
  }

  # sort -----------------------------------------------------------------------
  if (!is.null(sort) & is.null(names(sort))) { # checking names for deprecated named list input

    # checking input type: must be a list of formulas, or one formula
    if (!class(sort) %in% c("list", "formula")) {
      stop(glue(
        "'sort' argument must be a list of formulas. ",
        "LHS of the formula is the variable specification, ",
        "and the RHS is the sort specification: ",
        "list(vars(age, marker) ~ 1)"
      ))
    }
    if ("list" %in% class(sort)) {
      if (some(sort, negate(rlang::is_bare_formula))) {
        stop(glue(
          "'sort' argument must be a list of formulas. ",
          "LHS of the formula is the variable specification, ",
          "and the RHS is the sort specification: ",
          "list(vars(stage, marker) ~ \"frequency\")"
        ))
      }
    }

    # all sepcifed types are frequency or alphanumeric
    if ("formula" %in% class(sort)) sort <- list(sort)
    if (!every(sort, ~ eval(rlang::f_rhs(.x)) %in% c("frequency", "alphanumeric")) |
      !every(sort, ~ rlang::is_string(eval(rlang::f_rhs(.x))))) {
      stop(glue(
        "The RHS of the formula in the 'sort' argument must of one and only one of ",
        "\"frequency\" or \"alphanumeric\""
      ))
    }
  }
}

# # data
#   # not putting in a data frame
#   tbl_summary_input_checks(
#     data = list(not = "a", proper = "data.frame"), by = NULL, summary_type = NULL, var_label = NULL,
#     stat_display = NULL, digits = NULL, pvalue_fun = NULL
#   )
#
# # by
#   # by var not in dataset
#   tbl_summary_input_checks(
#     data = mtcars, by = "Petal.Width", summary_type = NULL, var_label = NULL,
#     stat_display = NULL, digits = NULL, pvalue_fun = NULL
#   )
#
# # summary_type
#   # names not a variables name, and input type not valid
#   tbl_summary_input_checks(
#     data = mtcars, by = NULL, summary_type = list(hp = "continuous", length = "catttegorical"), var_label = NULL,
#     stat_display = NULL, digits = NULL, pvalue_fun = NULL
#   )
#
# # var_label
#   # names not a variables name, and input type not valid
#   tbl_summary_input_checks(
#     data = mtcars, by = NULL, summary_type = NULL, var_label = list(hp = "Horsepower", wrong_name = "Not a variable"),
#     stat_display = NULL, digits = NULL, pvalue_fun = NULL
#   )
#
# # stat_display
#   tbl_summary_input_checks(
#     data = mtcars, by = NULL, summary_type = NULL, var_label = NULL,
#     stat_display = list(continuous = "{mean}", dichot_not = "nope"), digits = NULL, pvalue_fun = NULL
#   )
#
# # digits
#   tbl_summary_input_checks(
#     data = mtcars, by = NULL, summary_type = NULL, var_label = NULL,
#     stat_display = NULL, digits = list(hp = 2, wrong_name = 2.1, am = 5.4), pvalue_fun = NULL
#   )

# tbl_summary_input_checks(
#   data = NULL, by = NULL, summary_type = NULL, var_label = NULL,
#   stat_display = NULL, digits = NULL, pvalue_fun = NULL
# )

# provide a vector of stat_display and get labels back i.e. {mean} ({sd}) gives Mean (SD)
stat_label_match <- function(stat_display, iqr = TRUE) {
  labels <-
    tibble::tribble(
      ~stat, ~label,
      "{min}", "minimum",
      "{max}", "maximum",
      "{median}", "median",
      "{mean}", "mean",
      "{sd}", "SD",
      "{var}", "variance",
      "{n}", "n",
      "{N}", "N",
      "{p}%", "%",
      "{p}", "%"
    ) %>%
    # adding in quartiles
    bind_rows(
      tibble(stat = paste0("{p", 0:100, "}")) %>%
        mutate(label = paste0(gsub("[^0-9\\.]", "", .data$stat), "%"))
    ) %>%
    # if function does not appear in above list, the print the function name
    bind_rows(
      tibble(
        stat = str_extract_all(stat_display, "\\{.*?\\}") %>%
          unlist() %>%
          unique(),
        label = .data$stat %>%
          str_remove_all(pattern = fixed("}")) %>%
          str_remove_all(pattern = fixed("{"))
      )
    )

  # adding IQR replacements if indicated
  if (iqr == TRUE) {
    labels <-
      bind_rows(
        tibble::tribble(
          ~stat, ~label,
          "{p25}, {p75}", "IQR"
        ),
        labels
      )
  }

  # replacing statistics in {}, with their labels
  for (i in seq_len(nrow(labels))) {
    stat_display <-
      stringr::str_replace_all(
        stat_display,
        stringr::fixed(labels$stat[i]),
        labels$label[i]
      )
  }

  stat_display
}

# stat_label footnote maker
footnote_stat_label <- function(meta_data) {
  meta_data %>%
    select(c("summary_type", "stat_label")) %>%
    mutate(
      summary_type = case_when(
        summary_type == "dichotomous" ~ "categorical",
        TRUE ~ .data$summary_type
      ),
      message = glue("{stat_label}")
    ) %>%
    distinct() %>%
    pull("message") %>%
    paste(collapse = "; ") %>%
    paste0("Statistics presented: ", .)
}

# the by variable is supplied is a bare, and ocnverted to a string.
# when a NULL is passed, it is returned as a NULL
enquo_to_string <- function(by_enquo, arg_name) {
  # returning NULL if NULL was passed
  if (rlang::quo_is_null(by_enquo)) {
    return(NULL)
  }

  # converting enquo to string
  by_quo_text <- rlang::quo_text(by_enquo)

  # is user supplied string, then stopping with error
  if (startsWith(by_quo_text, "\"") && endsWith(by_quo_text, "\"")) {
    stop(glue(
      "\nPassing the '{arg_name}' argument as a string is defunct.\n",
      "Please pass the {arg_name} argument without quotes. For example, \n\n",
      "foo({arg_name} = varname)"
    ))
  }

  # if user passed odd name quoted with back ticks, removing them
  if (startsWith(by_quo_text, "`") && endsWith(by_quo_text, "`")) {
    by_quo_text <- stringr::str_sub(by_quo_text, 2, -2)
  }

  by_quo_text
}
