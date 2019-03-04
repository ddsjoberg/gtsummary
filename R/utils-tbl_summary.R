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
#' @author Daniel Sjoberg

assign_class <- function(data, variable) {
  classes_expected <- c("character", "factor", "numeric", "logical", "integer", "double")

  # extracing the base R class
  classes_return <-
    map(
      variable,
      ~ class(data[[.x]]) %>% intersect(classes_expected)
    )

  # checking all columns returned a class
  class_error <- map_lgl(classes_return, ~ identical(.x, character(0)))
  if (any(class_error)) {
    stop(glue(
      "Class of variable '{paste(variable[class_error], collapse = ', ')}' not supported"
    ))
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
#' @author Daniel Sjoberg

# wrapper for assign_dichotomous_value_one() function
assign_dichotomous_value <- function(data, variable, summary_type, class) {
  pmap(list(variable, summary_type, class), ~ assign_dichotomous_value_one(data, ..1, ..2, ..3))
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



#' Assign type of summary statistic
#'
#' Function that assigns default statistics to display, or if specified,
#' assigns the user-defined statistics for display.
#'
#' @param summary_type A list that includes specified summary types
#' @param stat_display List with up to two named elements.  Names must be
#' continuous or categorical. Can be \code{NULL}.
#' @return vector of stat_display selections for each variable
#' @keywords internal
#' @author Daniel Sjoberg

assign_stat_display <- function(summary_type, stat_display) {
  # dichotomous and categorical are treated in the same fashion here
  summary_type <- ifelse(summary_type == "dichotomous", "categorical", summary_type)

  # otherwise, return defaults
  return(
    map_chr(
      summary_type,
      ~ case_when(
        .x == "continuous" ~ stat_display[[.x]] %||% "{median} ({p25}, {p75})",
        .x %in% c("categorical", "dichotomous") ~
          stat_display[[.x]] %||% "{n} ({p}%)"
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
#' @author Daniel Sjoberg
#' @examples
#' # assign_summary_type(data = mtcars,
#' #                     variable =  names(mtcars),
#' #                     class = apply(mtcars, 2, class),
#' #                     summary_type = NULL)
assign_summary_type <- function(data, variable, class, summary_type) {
  map2_chr(
    variable, class,
    ~ summary_type[[.x]] %||%
      case_when(
        # logical variables will be dichotmous
        .y == "logical" ~
          "dichotomous",

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
# dta = dplyr::data_frame(
#   age = rnorm(n) + 35,
#   female = sample(c(T, F), size = n, replace = T),
#   male = as.numeric(female),
#   sex = ifelse(female == T, "Female", "Male"),
#   country = sample(c("USA", "Canada"), size = n, replace = T),
#   shoe_size = sample(1:25, size = n, replace = T),
#   family_size = sample(1:5, size = n, replace = T)
# )
# #adding missing values
# dta = dplyr::mutate_all(dta, dplyr::funs( ifelse(runif(n) < 0.25, NA, .)) ) %>%
#   dplyr::mutate(
#     sex = as.factor(sex),
#     male_fct = ifelse(female == TRUE, "No", "Yes") %>% factor()
#   )
# dta
#
# # creating base meta data dataframe
# meta_data =
#   dplyr::data_frame(
#     variable = names(dta),
#     class = purrr::map_chr(variable, ~ class(dta[[.x]]))
#   )
# meta_data
#
# # tesing function's guessing ability
# meta_data %>%
#   dplyr::mutate(
#     assign_summary_type = assign_summary_type(dta, variable, class, NULL)
#   )
#
#
# # tesing function's ability when type assigned
# meta_data %>%
#   dplyr::mutate(
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
#' @author Daniel Sjoberg
#' @examples
#' # assign_var_label(mtcars, names(mtcars), list(hp = "Horsepower"))
assign_var_label <- function(data, variable, var_label) {
  map_chr(
    variable,
    ~ var_label[[.x]] %||%
      attr(data[[.x]], "label") %||%
      .x
  )
}

# n = 10
# dta = dplyr::data_frame(
#   age = rnorm(n),
#   sex = 1
# )
# attr(dta[["age"]], "label") = "Patient Age, yrs"
#
# var_label_one(dta, "age", NULL)
#
# meta =
#   dplyr::data_frame(
#     variable = names(dta)
#   )
#
# dplyr::mutate(meta, var_label = var_label(dta, variable, NULL))
# dplyr::mutate(meta, var_label = var_label(dta, variable, list(sex = "Gender")))



#' This function takes in the meta-data and calculates an appropriate p-value
#'
#' @param data input data set
#' @param variable categorical or continuous variable for which a test with \code{by_var} is desired
#' @param by categorical variable
#' @param group the group variable for clustered data
#' @param type the type of variable, one of categorical or continuous, from the metadata
#' @param test list of statistical tests from meta data
#' @return a table of p-values for each variable
#' @keywords internal
#' @author Emily Zabor

calculate_pvalue <- function(data, variable, by, test, type, group) {
  purrr::pmap_dbl(
    list(variable, by, test, type),
    ~ calculate_pvalue_one(data, ..1, ..2, ..3, ..4, group)
  )
}

calculate_pvalue_one <- function(data, variable, by, test, type, group) {

  # if there is no by variable, and thus test is NA, return NA
  if (is.na(test)) return(NA)

  # convert by variables to factor (some functions don't allow strings)
  data[[by]] <- data[[by]] %>% factor()

  # omitting missing values before calculating pvalues
  data <- data %>% select(c(group, variable, by)) %>% stats::na.omit()

  # Wilcoxon and Kruskal-Wallis tests
  if (test %in% c("wilcox.test", "kruskal.test")) {
    tryCatch(
      pval <- stats::kruskal.test(data[[variable]], data[[by]])$p.value,
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
        pval <<- NA
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
        pval <<- NA
      },
      finally = {
        return(pval)
      }
    )
  }

  # T-test
  if (test == "t.test") {
    tryCatch(
      pval <- stats::t.test(data[[variable]] ~ data[[by]])$p.value,
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
        pval <<- NA
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
        pval <<- NA
      },
      finally = {
        return(pval)
      }
    )
  }

  # Chi-squared test
  if (test == "chisq.test") {
    tryCatch(
      pval <- stats::chisq.test(data[[variable]], data[[by]])$p.value,
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
        pval <<- NA
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
        pval <<- NA
      },
      finally = {
        return(pval)
      }
    )
  }

  # Fisher's exact test
  if (test == "fisher.test") {
    tryCatch(
      pval <- stats::fisher.test(data[[variable]], data[[by]])$p.value,
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
        pval <<- NA
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
        pval <<- NA
      },
      finally = {
        return(pval)
      }
    )
  }

  # Random effects - continuous or dichotomous
  if (test == "re" & type %in% c("continuous", "dichotomous")) {
    form1 <- get("as.formula")(paste0(by, " ~ ", variable, " + (1 | ", group, ")"))
    mod1 <- tryCatch(
      lme4::glmer(form1, data = get("na.omit")(data), family = get("binomial")),
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
      }
    )
    if (class(mod1) != "glmerMod") {
      return(NA)
    } else {
      return(summary(mod1)$coefficients[2, "Pr(>|z|)"])
    }
  }

  # Random effects - categorical
  if (test == "re" & type == "categorical") {
    form0 <- get("as.formula")(paste0(by, " ~ 1 + (1 | ", group, ")"))
    form1 <- get("as.formula")(paste0(
      by, " ~ factor(", variable,
      ") + (1 | ", group, ")"
    ))
    mod0 <- tryCatch(
      lme4::glmer(form0, data = get("na.omit")(data), family = get("binomial")),
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
      }
    )
    mod1 <- tryCatch(
      lme4::glmer(form1, data = get("na.omit")(data), family = get("binomial")),
      warning = function(w) {
        message(paste0("For variable ", variable, ": ", w))
      },
      error = function(e) {
        message(paste0("For variable ", variable, ": ", e))
      }
    )
    if (class(mod1) != "glmerMod") {
      return(NA)
    } else {
      return(get("anova")(mod0, mod1)$"Pr(>Chisq)"[2])
    }
  }
}


# calculate_pvalue_one(data = mtcars, variable = "hp", by = "am",
#                      test = "wilcox.test", group = NULL, type = "continuous")
# calculate_pvalue_one(data = mtcars, variable = "hp", by = "am",
#                      test = "kruskal.test", group = NULL, type = "continuous")
# calculate_pvalue_one(data = mtcars, variable = "hp", by = "am",
#                      test = "t.test", group = NULL, type = "continuous")
# calculate_pvalue_one(data = mtcars, variable = "gear", by = "am",
#                      test = "chisq.test", group = NULL, type = "categorical")
# calculate_pvalue_one(data = mtcars, variable = "gear", by = "am",
#                      test = "fisher.test", group = NULL, type = "categorical")
# calculate_pvalue_one(data = mtcars, variable = "hp", by = "am",
#                      group = "gear", test = "re", type = "continuous")
# calculate_pvalue_one(data = mtcars, variable = "hp", by = "am",
#                      group = "gear", test = "re", type = "categorical")
# calculate_pvalue_one(data = mtcars, variable = "mpg", by = "am",
#                      group = "gear", test = "re", type = "continuous")




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
#' @keywords internal
#' @author Daniel Sjoberg

calculate_summary_stat <- function(data, variable, by, summary_type,
                                   dichotomous_value, var_label, stat_display,
                                   digits, class, missing) {

  # if class is NA, then do not calculate summary statistics
  if (is.na(class)) {
    # empty results table when no by variable
    if (is.null(by)) {
      return(
        tibble(
          row_type = c("label", "missing"),
          label = c(var_label, "Unknown"),
          stat_overall = c(NA_character_, as.character(nrow(data)))
        )
      )
    }
    # empty results table when there is a by variable
    if (!is.null(by)) {
      stat_col_names <- df_by(data, by)[["by_col"]]
      return(
        dplyr::data_frame(
          row_type = c("label", "missing"),
          label = c(var_label, "Unknown")
        ) %>%
          dplyr::left_join(
            table(data[[by]]) %>%
              as.matrix() %>%
              t() %>%
              as_tibble() %>%
              mutate_all(as.character) %>%
              set_names(stat_col_names) %>%
              mutate_(row_type = ~"missing")
          )
      )
    }
  }

  # return data table with continuous summary stats
  if (summary_type == "continuous") {
    return(
      summarize_continuous(
        data, variable, by, digits,
        var_label, stat_display, missing
      )
    )
  }

  # return data table with categorical or dichotomous summary stats
  if (summary_type %in% c("categorical", "dichotomous")) {
    return(
      summarize_categorical(
        data, variable, by, var_label,
        stat_display, dichotomous_value, missing
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
#' @keywords internal
#' @author Emily Zabor, Daniel Sjoberg

# takes as the input a vector of variable and summary types
continuous_digits_guess <- function(data,
                                    variable,
                                    summary_type,
                                    class,
                                    digits = NULL) {
  pmap_dbl(
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
#' @keywords internal
#' @author Daniel Sjoberg

df_by <- function(data, by) {
  data %>%
    select(c(by)) %>%
    set_names("by") %>%
    count_("by") %>%
    mutate_(N = ~sum(n), p = ~n / N) %>%
    arrange_("by") %>%
    mutate_(
      by_id = ~ 1:n(), # 'by' variable ID
      by_chr = ~ as.character(by), # Character version of 'by' variable
      by_col = ~ paste0("stat_", by_id) # Column name of in fmt_table1 output
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
#' @return formatted summary statistics in a tibble.
#' @keywords internal
#' @author Daniel Sjoberg

summarize_categorical <- function(data, variable, by, var_label,
                                  stat_display, dichotomous_value, missing) {

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
      mutate_(by_col = ~"stat_0") %>%
      select(c(variable, "by_col"))
  }

  # nesting data and changing by variable
  tab <-
    data %>%
    stats::na.omit() %>%
    group_by_("by_col") %>%
    count_("variable") %>%
    complete_("variable", fill = list(n = 0)) %>%
    mutate_(
      N = ~sum(n),
      p = ~style_percent(n / N),
      stat = ~as.character(glue(stat_display))
    ) %>%
    select(c("by_col", "variable", "stat")) %>%
    spread_("by_col", "stat") %>%
    mutate_(
      row_type = ~"level",
      label = ~variable %>% as.character()
    ) %>%
    select(c("variable", "row_type", "label", starts_with("stat_")))

  # number of missing observations
  missing_count <-
    data %>%
    group_by_("by_col") %>%
    nest() %>%
    mutate_(
      missing_count = ~map_chr(data, ~.x[[1]] %>% is.na() %>% sum())
    ) %>%
    select(c("by_col", "missing_count")) %>%
    spread_("by_col", "missing_count") %>%
    mutate_(
      row_type = ~"missing",
      label = ~"Unknown"
    )


  # formatting for dichotomous variables
  if (!is.null(dichotomous_value)) {
    results <-
      tab %>%
      filter_("variable == dichotomous_value") %>%
      mutate_(
        row_type = ~"label",
        label = ~var_label
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
      filter_("row_type != 'missing'")
  }

  results
}

# summarize_categorical(
#   data = lung, variable = "ph.karno", by = "sex", var_label = "WTF",
#   stat_display = "{n}/{N} ({p}%)", dichotomous_value = 50, missing = "ifany"
# )
# summarize_categorical(
#   data = lung, variable = "ph.karno", by = "sex", var_label = "WTF",
#   stat_display = "{n}/{N} ({p}%)", dichotomous_value = NULL
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
#   stat_display = "{n} ({p}%)", dichotomous_value = NULL
# )




#' Calculates and formats summary statistics for continuous data
#'
#' @param data data frame
#' @param variable Character variable name in `data` that will be tabulated
#' @param by Character variable name in `data` that Summary statistics for
#' `variable` are stratified
#' @param digits integer indicating the number of decimal places to be used.
#' @param var_label string label
#' @param stat_display String that specifies the format of the displayed statistics.
#' The syntax follows \code{\link[glue]{glue}} inputs with n, N, and p as input options.
#' @param missing whether to include `NA` values in the table. `missing` controls
#' if the table includes counts of `NA` values: the allowed values correspond to
#' never (`"no"`), only if the count is positive (`"ifany"`) and even for
#' zero counts (`"always"`). Default is `"ifany"`.
#' @return formatted summary statistics in a tibble.
#' @keywords internal
#' @author Daniel Sjoberg
#' @importFrom stringr str_extract_all str_remove_all fixed

summarize_continuous <- function(data, variable, by, digits,
                                 var_label, stat_display, missing) {

  # counting total missing
  tot_n_miss <- sum(is.na(data[[variable]]))

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
      mutate_(by_col = ~"stat_0") %>%
      select(c(variable, "by_col"))
  }

  # nesting data and changing by variable
  data <-
    data %>%
    group_by_("by_col") %>%
    nest(.key = "data")

  # nesting data and calculating descriptive stats
  stats <-
    data %>%
    mutate_(
      # extracting list of statisitcs that need to be calculated
      stat_name_list = ~str_extract_all(stat_display, "\\{.*?\\}") %>%
        map(str_remove_all, pattern = fixed("}")) %>%
        map(str_remove_all, pattern = fixed("{")),
      # calculating statistics
      stat_result_list = ~map2(
        data, stat_name_list,
        ~ calculate_single_stat(.x[[1]], .y)
      ),
      # converting stats into a tibble with names as the type of statistic (i.e. mean column is called mean)
      df_result = ~map2(
        stat_name_list, stat_result_list,
        ~ .y %>% t() %>% as_tibble() %>% set_names(.x)
      ),
      # rounding statistics and concatenating results
      stat = ~map_chr(
        df_result,
        ~.x %>%
          mutate_all(~sprintf(glue("%.{digits}f"), .)) %>%
          mutate_(
            stat = ~ as.character(glue(stat_display))
          ) %>%
          pull("stat")
      )
    ) %>%
    select(c("by_col", "stat")) %>%
    spread_("by_col", "stat") %>%
    mutate_(
      row_type = ~"label",
      label = ~var_label
    ) %>%
    select(c("row_type", "label", starts_with("stat_")))

  # number of missing observations
  missing_count <-
    data %>%
    mutate_(
      missing_count =
        ~map_chr(
          data,
          ~.x[[1]] %>% is.na() %>% sum()
        )
    ) %>%
    select(c("by_col", "missing_count")) %>%
    spread_("by_col", "missing_count") %>%
    mutate_(
      row_type = ~"missing",
      label = ~"Unknown"
    )

  # stacking stats and missing row
  result <-
    stats %>%
    bind_rows(missing_count)

  # excluding missing row if indicated
  if (missing == "no" | (missing == "ifany" & tot_n_miss == 0)) {
    result <-
      result %>%
      filter_("row_type != 'missing'")
  }

  result
}

# stat_name that are accepted
calculate_single_stat <- function(x, stat_name) {

  map_dbl(stat_name,
          function(name) {
            # calculating percentiles if requested
            if (name %in% paste0("p", 0:100)) {
              do.call(
                "quantile",
                list(
                  x,
                  probs = as.numeric(gsub("[^0-9\\.]", "", name))/100,
                  na.rm = TRUE
                )
              )
            }
            # calculating summary stats, input MUST be a function name
            # first argument is x and must take argument 'na.rm = TRUE'
            else do.call(name, list(x, na.rm = TRUE))
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
tbl_summary_input_checks <- function(data, by, label, type,
                                     statistic, digits, missing, group) {
  # data -----------------------------------------------------------------------
  # data is a data frame
  if (!is.data.frame(data)) {
    stop(glue(
      "'data' input must be a data frame."
    ))
  }

  # cannot be empty data frame
  if (nrow(data) == 0) {
    stop(glue(
      "Expecting 'data' to have at least 1 row."
    ))
  }

  # by -------------------------------------------------------------------------
  # by is a variable in data
  if (!is.null(by)) {
    if (!(by %in% names(data))) {
      stop(glue(
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
      message(glue(
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
      stop(glue(
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
      message(glue(
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
      message(glue(
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
      message(glue(
        "The following names from 'digits' are not found in 'data' and ",
        "were ignored: {paste0(digits_not_in_data, collapse = ', ')}"
      ))
    }

    # specified digits must be a non-negative integer
    digits_value_not_valid <-
      setdiff(digits %>% unlist() %>% unique(), 0:100)
    if (length(digits_value_not_valid) > 0) {
      stop(glue(
        "'digits' values must be non-negative integers. ",
        "'{paste0(digits_value_not_valid, collapse = ', ')}' not valid input."
      ))
    }
  }

  # group ----------------------------------------------------------------------
  if (length(group) > 1) {
    stop(
      "'group' must be `NULL` or length 1."
    )
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

