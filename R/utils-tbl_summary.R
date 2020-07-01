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

assign_class <- function(data, variable, classes_expected) {
  # extracting the base R class
  classes_return <-
    map(variable, ~class(data[[.x]]) %>% intersect(classes_expected))
  classes_return
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
  ), call. = FALSE)
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
          get_theme_element("tbl_summary-str:continuous_stat") %||%
          "{median} ({p25}, {p75})",
        .y %in% c("categorical", "dichotomous") ~
          stat_display[[.x]] %||%
          get_theme_element("tbl_summary-str:categorical_stat") %||%
          "{n} ({p}%)"
      )
    )
  )
}

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
  # checking if user requested type = "categorical" for variable that is all missing
  if (!is.null(summary_type)) {
    summary_type <- purrr::imap(
      summary_type,
      function(.x, .y) {
        categorical_missing <-
          .x == "categorical" &&
          length(data[[.y]]) == sum(is.na(data[[.y]])) &&
          !"factor" %in% class(data[[.y]]) # factor can be summarized with categorical
        if(categorical_missing == FALSE) return(.x)
        message(glue(
          "Variable '{.y}' is `NA` for all observations and cannot be summarized as 'categorical'.\n",
          "Using `{.y} ~ \"dichotomous\"` instead."
        ))
        return("dichotomous")
      }
    )
  }

  # assigning types ------------------------------------------------------------
  type <- map2_chr(
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
        .y %in% c("integer", "numeric") &
          length(setdiff(na.omit(data[[.x]]), c(0, 1))) == 0 &
          nrow(data) != sum(is.na(data[[.x]])) ~
          "dichotomous",

        # factor variables that are "No" and "Yes" only, will be dichotomous
        .y %in% c("factor") & setequal(attr(data[[.x]], "levels"), c("No", "Yes")) ~
          "dichotomous",
        .y %in% c("factor") & setequal(attr(data[[.x]], "levels"), c("no", "yes")) ~
          "dichotomous",
        .y %in% c("factor") & setequal(attr(data[[.x]], "levels"), c("NO", "YES")) ~
          "dichotomous",

        # character variables that are "No" and "Yes" only, will be dichotomous
        .y %in% c("character") & setequal(na.omit(data[[.x]]), c("No", "Yes")) ~
          "dichotomous",
        .y %in% c("character") & setequal(na.omit(data[[.x]]), c("no", "yes")) ~
          "dichotomous",
        .y %in% c("character") & setequal(na.omit(data[[.x]]), c("NO", "YES")) ~
          "dichotomous",

        # factors and characters are categorical (except when all missing)
        .y == "character" & nrow(data) == sum(is.na(data[[.x]])) ~ "dichotomous",
        .y %in% c("factor", "character") ~ "categorical",

        # numeric variables with fewer than 10 levels will be categorical
        .y %in% c("integer", "numeric", "difftime") &
          length(unique(na.omit(data[[.x]]))) < 10 &
          nrow(data) != sum(is.na(data[[.x]])) ~
          "categorical",

        # everything else is assigned to continuous
        TRUE ~ "continuous"
      )
  )

  # checking user did not request a factor or character variable be summarized
  # as a continuous variable
  purrr::pwalk(
    list(type, class, variable),
    ~ if(..1 == "continuous" && ..2 %in% c("factor", "character"))
      stop(glue(
        "Column '{..3}' is class \"{..2}\" and cannot be summarized as a continuous variable."
      ), call. = FALSE)
  )


  type
}


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
#' @noRd
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
  # if all values are NA, returning 0
  if (nrow(data) == sum(is.na(data[[variable]]))) {
    return(0)
  }

  # if the variable is not continuous type, return NA
  if (summary_type != "continuous") {
    return(NA)
  }

  # if the number of digits is specified for a variable, return specified number
  if (!is.null(digits[[variable]])) {
    return(digits[[variable]])
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
  if (is.null(by)) return(NULL)

  if (inherits(data[[by]], "factor"))
    result <- tibble(by = attr(data[[by]], "levels") %>%
                       factor(x = ., levels= ., labels = .))
  else result <- data %>% select(by) %>% dplyr::distinct() %>% set_names("by")

  result <-
    result %>%
    arrange(!!sym("by")) %>%
    mutate(
      n = purrr::map_int(.data$by, ~ sum(data[[!!by]] == .x)),
      N = sum(.data$n),
      p = .data$n / .data$N,
      by_id = 1:n(), # 'by' variable ID
      by_chr = as.character(.data$by), # Character version of 'by' variable
      by_col = paste0("stat_", .data$by_id) # Column name of in fmt_table1 output
    ) %>%
    select(starts_with("by"), everything())

  attr(result$by, "label") <- NULL
  result
}


#' Assigns categorical variables sort type ("alphanumeric" or "frequency")
#'
#' @param variable variable name
#' @param summary_type the type of variable ("continuous", "categorical", "dichotomous")
#' @param sort named list indicating the type of sorting to perform. Default is NULL.
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

      # otherwise, return "alphanumeric"
      return("alphanumeric")
    }
  )
}


# function that checks the inputs to \code{\link{tbl_summary}}
# this should include EVERY input of \code{\link{tbl_summary}} in the same order
# copy and paste them from \code{\link{tbl_summary}}
tbl_summary_data_checks <- function(data) {
  # data -----------------------------------------------------------------------
  # data is a data frame
  if (!is.data.frame(data)) {
    stop("'data' input must be a data frame.", call. = FALSE)
  }

  # cannot be empty data frame
  if (nrow(data) == 0L) {
    stop("Expecting 'data' to have at least 1 row.", call. = FALSE)
  }

  # must have at least one column
  if (ncol(data) == 0L) {
    stop("Expecting 'data' to have at least 1 column", call. = FALSE)
  }
}

tbl_summary_input_checks <- function(data, by, label, type, value, statistic,
                                     digits, missing, missing_text, sort) {
  # data -----------------------------------------------------------------------
  tbl_summary_data_checks(data)

  # by -------------------------------------------------------------------------
  if (!is.null(by) && !by %in% names(data)) {
    stop(glue(
      "`by = '{by}'` is not a column in `data=`. Did you misspell the column name, ",
      "or omit the column with the `include=` argument?"), call. = FALSE)
  }

  # type -----------------------------------------------------------------------
  if (!is.null(type) & is.null(names(type))) { # checking names for deprecated named list input

    # checking input type: must be a list of formulas, or one formula
    if (!inherits(type, c("list", "formula"))) {
      stop(glue(
        "'type' argument must be a list of formulas. ",
        "LHS of the formula is the variable specification, ",
        "and the RHS is the type specification: ",
        "list(c(age, marker) ~ \"continuous\")"
      ), call. = FALSE)
    }
    if (inherits(type, "list")) {
      if (some(type, negate(rlang::is_bare_formula))) {
        stop(glue(
          "'type' argument must be a list of formulas. ",
          "LHS of the formula is the variable specification, ",
          "and the RHS is the type specification: ",
          "list(c(age, marker) ~ \"continuous\")"
        ), call. = FALSE)
      }
    }

    # all sepcifed types are continuous, categorical, or dichotomous
    if (inherits(type, "formula")) type <- list(type)
    if (!every(type, ~ eval(rlang::f_rhs(.x)) %in% c("continuous", "categorical", "dichotomous")) |
        !every(type, ~ rlang::is_string(eval(rlang::f_rhs(.x))))) {
      stop(glue(
        "The RHS of the formula in the 'type'  argument must of one and only one of ",
        "\"continuous\", \"categorical\", or \"dichotomous\""
      ), call. = FALSE)
    }
  }

  # value ----------------------------------------------------------------------
  if (!is.null(value) & is.null(names(value))) { # checking names for deprecated named list input

    # checking input type: must be a list of formulas, or one formula
    if (!inherits(value, c("list", "formula"))) {
      stop(glue(
        "'value' argument must be a list of formulas. ",
        "LHS of the formula is the variable specification, ",
        "and the RHS is the value specification: ",
        "list(stage ~ \"T1\")"
      ), call. = FALSE)
    }
    if (inherits(value, "list")) {
      if (some(value, negate(rlang::is_bare_formula))) {
        stop(glue(
          "'value' argument must be a list of formulas. ",
          "LHS of the formula is the variable specification, ",
          "and the RHS is the value specification: ",
          "list(stage ~ \"T1\")"
        ), call. = FALSE)
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
      ), call. = FALSE)
    }
  }

  # label ----------------------------------------------------------------------
  if (!is.null(label) & is.null(names(label))) { # checking names for deprecated named list input

    # all sepcifed labels must be a string of length 1
    if (inherits(label, "formula")) label <- list(label)
    if (!every(label, ~ rlang::is_string(eval(rlang::f_rhs(.x))))) {
      stop(glue(
        "The RHS of the formula in the 'label' argument must be a string."
      ), call. = FALSE)
    }
  }

  # statistic ------------------------------------------------------------------
  if (!is.null(statistic) & is.null(names(statistic))) { # checking names for deprecated named list input

    # checking input type: must be a list of formulas, or one formula
    if (!inherits(statistic, c("list", "formula"))) {
      stop(glue(
        "'statistic' argument must be a list of formulas. ",
        "LHS of the formula is the variable specification, ",
        "and the RHS is the statistic specification: ",
        "list(all_categorical() ~ \"{n} / {N}\")"
      ), call. = FALSE)
    }
    if (inherits(statistic, "list")) {
      if (some(statistic, negate(rlang::is_bare_formula))) {
        stop(glue(
          "'statistic' argument must be a list of formulas. ",
          "LHS of the formula is the variable specification, ",
          "and the RHS is the statistic specification: ",
          "list(all_categorical() ~ \"{n} / {N}\")"
        ), call. = FALSE)
      }
    }

    # all sepcifed statistics must be a string of length 1
    if (inherits(statistic, "formula")) statistic <- list(statistic)
    if (!every(statistic, ~ rlang::is_string(eval(rlang::f_rhs(.x))))) {
      stop(glue(
        "The RHS of the formula in the 'statistic' argument must be a string."
      ), call. = FALSE)
    }
  }

  # digits ---------------------------------------------------------------------
  if (!is.null(digits) & is.null(names(digits))) { # checking names for deprecated named list input

    # checking input type: must be a list of formulas, or one formula
    if (!inherits(digits, c("list", "formula"))) {
      stop(glue(
        "'digits' argument must be a list of formulas. ",
        "LHS of the formula is the variable specification, ",
        "and the RHS is the digits specification: ",
        "list(c(age, marker) ~ 1)"
      ), call. = FALSE)
    }
    if (inherits(digits, "list")) {
      if (some(digits, negate(rlang::is_bare_formula))) {
        stop(glue(
          "'digits' argument must be a list of formulas. ",
          "LHS of the formula is the variable specification, ",
          "and the RHS is the digits specification: ",
          "list(c(age, marker) ~ 1)"
        ), call. = FALSE)
      }
    }
  }

  # missing_text ---------------------------------------------------------------
  # input must be character
  if (!rlang::is_string(missing_text)) {
    stop("Argument 'missing_text' must be a character string of length 1.", call. = FALSE)
  }

  # sort -----------------------------------------------------------------------
  if (!is.null(sort) & is.null(names(sort))) { # checking names for deprecated named list input

    # checking input type: must be a list of formulas, or one formula
    if (!inherits(sort, c("list", "formula"))) {
      stop(glue(
        "'sort' argument must be a list of formulas. ",
        "LHS of the formula is the variable specification, ",
        "and the RHS is the sort specification: ",
        "c(vars(age, marker) ~ 1)"
      ), call. = FALSE)
    }
    if (inherits(sort, "list")) {
      if (some(sort, negate(rlang::is_bare_formula))) {
        stop(glue(
          "'sort' argument must be a list of formulas. ",
          "LHS of the formula is the variable specification, ",
          "and the RHS is the sort specification: ",
          "list(c(stage, marker) ~ \"frequency\")"
        ), call. = FALSE)
      }
    }

    # all sepcifed types are frequency or alphanumeric
    if (inherits(sort, "formula")) sort <- list(sort)
    if (!every(sort, ~ eval(rlang::f_rhs(.x)) %in% c("frequency", "alphanumeric")) |
        !every(sort, ~ rlang::is_string(eval(rlang::f_rhs(.x))))) {
      stop(glue(
        "The RHS of the formula in the 'sort' argument must of one and only one of ",
        "\"frequency\" or \"alphanumeric\""
      ), call. = FALSE)
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

# stat_label_match -------------------------------------------------------------
# provide a vector of stat_display and get labels back i.e. {mean} ({sd}) gives Mean (SD)
stat_label_match <- function(stat_display, iqr = TRUE) {
  language <- get_theme_element("pkgwide-str:language", default = "en")
  labels <-
    tibble::tribble(
      ~stat, ~label,
      "{min}", translate_text("minimum", language),
      "{max}", translate_text("maximum", language),
      "{median}", translate_text("median", language),
      "{mean}", translate_text("mean", language),
      "{sd}", translate_text("SD", language),
      "{var}", translate_text("variance", language),
      "{n}", translate_text("n", language),
      "{N}", translate_text("N", language),
      "{p}%", translate_text("%", language),
      "{p_miss}%", translate_text("% missing", language),
      "{p_nonmiss}%", translate_text("% not missing", language),
      "{p}", translate_text("%", language),
      "{p_miss}", translate_text("% missing", language),
      "{p_nonmiss}", translate_text("% not missing", language),
      "{N_miss}", translate_text("N missing", language),
      "{N_nonmiss}", translate_text("N", language),
      "{N_obs}", translate_text("no. obs.", language)
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
          "{p25}, {p75}", translate_text("IQR", language),
          "{p25} - {p75}", translate_text("IQR", language)
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

# footnote_stat_label ----------------------------------------------------------
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
    paste0(translate_text("Statistics presented"), ": ", .)
}

# summarize_categorical --------------------------------------------------------
summarize_categorical <- function(data, variable, by, class, dichotomous_value, sort, percent) {
  # grabbing percent formatting function
  percent_fun <-
    get_theme_element("tbl_summary-fn:percent_fun") %||%
    getOption("gtsummary.tbl_summary.percent_fun", default = style_percent)
  N_fun <-
    get_theme_element("tbl_summary-fn:N_fun",
                      default = function(x) sprintf("%.0f", x))

  # stripping attributes/classes that cause issues -----------------------------
  # tidyr::complete throws warning `has different attributes on LHS and RHS of join`
  # when variable has label.  So deleting it.
  attr(data[[variable]], "label") <- NULL
  if (!is.null(by)) attr(data[[by]], "label") <- NULL
  # same thing when the class "labelled" is included when labeled with the Hmisc package
  class(data[[variable]]) <- setdiff(class(data[[variable]]), "labelled")
  if (!is.null(by)) class(data[[by]]) <- setdiff(class(data[[by]]), "labelled")

  # tabulating data ------------------------------------------------------------
  df_by <- df_by(data, by)
  variable_by_chr <- c("variable", switch(!is.null(by), "by"))
  data <- data %>%
    select(c(variable, by)) %>%
    # renaming variables to c("variable", "by") (if there is a by variable)
    set_names(variable_by_chr)

  df_tab <-
    data %>%
    mutate(
      # converting to factor, if not already factor
      variable = switch(class, factor = .data$variable) %||% factor(.data$variable),
      # adding dichotomous level (in case it is unobserved)
      variable = forcats::fct_expand(.data$variable, as.character(dichotomous_value)),
      # # re-leveling by alphanumeric order or frequency
      variable = switch(sort,
                        "alphanumeric" = .data$variable,
                        "frequency" = forcats::fct_infreq(.data$variable))
    ) %>%
    {suppressWarnings(count(., !!!syms(variable_by_chr)))} %>%
    stats::na.omit() %>%
    # if there is a by variable, merging in all levels
    {switch(
      !is.null(by),
      full_join(.,
                list(by = df_by$by,
                     variable = factor(attr(.$variable, "levels"),
                                       levels = attr(.$variable, "levels"))) %>%
                  purrr::cross_df(),
                by = c("by", "variable"))[c("by", "variable", "n")]) %||% .} %>%
    tidyr::complete(!!!syms(variable_by_chr), fill = list(n = 0))

  # calculating percent
  group_by_percent <- switch(
    percent,
    "cell" = "",
    "column" = ifelse(!is.null(by), "by", ""),
    "row" = "variable"
  )

  result <- df_tab %>%
    group_by(!!!syms(group_by_percent)) %>%
    mutate(
      N = sum(.data$n),
      # if the Big N is 0, there is no denom so making percent NA
      p = ifelse(.data$N == 0, NA, .data$n / .data$N)
    ) %>%
    ungroup() %>%
    rename(variable_levels = .data$variable) %>%
    mutate(variable = !!variable) %>%
    select(c(by, variable, "variable_levels", everything()))

  if (!is.null(dichotomous_value)) {
    result <- result %>%
      filter(.data$variable_levels == !!dichotomous_value) %>%
      select(-.data$variable_levels)
  }

  # adding percent_fun as attr to p column
  attr(result$p, "fmt_fun") <- percent_fun
  attr(result$N, "fmt_fun") <- N_fun
  attr(result$n, "fmt_fun") <- N_fun

  result
}

# summarize_continuous ---------------------------------------------------------
summarize_continuous <- function(data, variable, by, stat_display, digits) {
  # stripping attributes/classes that cause issues -----------------------------
  # tidyr::complete throws warning `has different attributes on LHS and RHS of join`
  # when variable has label.  So deleting it.
  attr(data[[variable]], "label") <- NULL
  if (!is.null(by)) attr(data[[by]], "label") <- NULL
  # same thing when the class "labelled" is included when labeled with the Hmisc package
  class(data[[variable]]) <- setdiff(class(data[[variable]]), "labelled")
  if (!is.null(by)) class(data[[by]]) <- setdiff(class(data[[by]]), "labelled")

  # extracting function calls
  fns_names_chr <-
    str_extract_all(stat_display, "\\{.*?\\}") %>%
    map(str_remove_all, pattern = fixed("}")) %>%
    map(str_remove_all, pattern = fixed("{")) %>%
    unlist() %>%
    # removing elements protected as other items
    setdiff(c("p_miss", "p_nonmiss", "N_miss", "N_nonmiss", "N_obs"))

  # defining shortcut quantile functions, if needed
  if (any(fns_names_chr %in% paste0("p", 0:100))) {
    fns_names_chr[fns_names_chr %in% paste0("p", 0:100)] %>%
      set_names(.) %>%
      imap(~purrr::partial(
        quantile,
        probs = as.numeric(stringr::str_replace(.x, pattern = "^p", "")) / 100
      )) %>%
      list2env(envir = rlang::env_parent())
  }

  if (length(fns_names_chr) == 0) stop(glue(
    "No summary function found in `{stat_display}` for variable '{variable}'.\n",
    "Did you wrap the function name in curly brackets?"
  ), call. = FALSE)

  if (any(c("by", "variable") %in% fns_names_chr)) {
    stop(paste(
      "'by' and 'variable' are protected names, and continuous variables",
      "cannot be summarized with functions by the these name."), call. = FALSE)
  }

  # prepping data set
  variable_by_chr <- c("variable", switch(!is.null(by), "by"))
  df_by <- df_by(data, by)
  data <-
    data %>%
    select(c(variable, by)) %>%
    stats::na.omit() %>%
    # renaming variables to c("variable", "by") (if there is a by variable)
    set_names(variable_by_chr)

  # calculating stats for each var and by level
  if (!is.null(by)) {
    df_stats <-
      list(
        fn = fns_names_chr,
        by = df_by$by
      ) %>%
      cross_df() %>%
      mutate(
        variable = variable,
        value = purrr::map2_dbl(
          .data$fn, .data$by,
          function(x, y) {
            var_vctr <- filter(data, .data$by == y) %>% pull(.data$variable)
            if (length(var_vctr) == 0) return(NA)
            do.call(what = x, args = list(x = var_vctr))
          }
        )
      ) %>%
      tidyr::pivot_wider(id_cols = c("by", "variable"), names_from = "fn")
  }
  else if (is.null(by)) {
    df_stats <-
      list(fn = fns_names_chr) %>%
      cross_df() %>%
      mutate(
        variable = variable,
        value = map_dbl(
          .data$fn,
          ~do.call(what = .x, args = list(x = pull(data, .data$variable)))
        )
      ) %>%
      tidyr::pivot_wider(id_cols = c("variable"), names_from = "fn")
  }

  # adding formatting function as attr to summary statistics columns
  fmt_fun <- as.list(rep(digits, length.out = length(fns_names_chr))) %>%
    set_names(fns_names_chr)

  df_stats <- purrr::imap_dfc(
    df_stats,
    function(column, colname) {
      if(is.null(fmt_fun[[colname]])) return(column)
      fmt <- glue("%.{fmt_fun[[colname]]}f")
      attr(column, "fmt_fun") <- purrr::partial(sprintf, fmt = !!fmt)
      column
    }
  )

  # returning final object
  df_stats
}

# df_stats_to_tbl --------------------------------------------------------------
df_stats_to_tbl <- function(data, variable, summary_type, by, var_label, stat_display,
                            df_stats, missing, missing_text) {

  # styling the statistics -----------------------------------------------------
  for (v in (names(df_stats) %>% setdiff(c("by", "variable", "variable_levels")))) {
    df_stats[[v]] <- df_stats[[v]] %>% attr(df_stats[[v]], "fmt_fun")()
  }

  # calculating the statistic to be displayed in the cell in the table.
  tryCatch(
    df_stats$statistic <- glue::glue_data(df_stats, stat_display) %>% as.character(),
    error = function(e) {
      stop(glue(
        "There was an error assembling the summary statistics for '{{variable}}'\n",
        "  with summary type '{{summary_type}}'.\n\n",
        "There are 2 common sources for this error.\n",
        "1. You have requested summary statistics meant for continuous\n",
        "   variables for a variable being as summarized as categorical.\n",
        "   To change the summary type to continuous, add the argument\n",
        "  `type = list({{variable}} ~ 'continuous')`\n",
        "2. One of the functions or statistics from the `statistic=` argument is not valid.",
        .open = "{{", .close = "}}"
      ))
    })

  # reshaping table to wide ----------------------------------------------------
  if (!is.null(by)) {
    df_stats_wide <-
      df_stats %>%
      select(any_of(c("by", "variable", "variable_levels", "statistic"))) %>%
      # merging in new column header names
      left_join(df_by(data, by)[c("by", "by_col")], by = "by") %>%
      tidyr::pivot_wider(id_cols = any_of(c("variable", "variable_levels")),
                         names_from = "by_col",
                         values_from = "statistic")
  }
  else {
    df_stats_wide <-
      df_stats %>%
      select(any_of(c("by", "variable", "variable_levels", "statistic"))) %>%
      rename(stat_0 = .data$statistic) %>%
      select(any_of(c("variable", "variable_levels", "stat_0")))
  }

  # setting up structure for table_body
  if (summary_type == "categorical") {
    # adding a label row for categorical variables
    result <-
      df_stats_wide %>%
      mutate(
        row_type = "level",
        label = as.character(.data$variable_levels)
      ) %>%
      select(-.data$variable_levels)

    # adding label row
    result <-
      tibble(
        variable = variable,
        row_type = "label",
        label = var_label
      ) %>%
      bind_rows(result)
  }
  else if (summary_type %in% c("continuous", "dichotomous")) {
    result <-
      df_stats_wide %>%
      mutate(
        row_type = "label",
        label = var_label
      )
  }

  # add rows for missing -------------------------------------------------------
  if (missing == "always" || (missing == "ifany" & sum(is.na(data[[variable]])) > 0)) {
    result <-
      result %>%
      bind_rows(
        calculate_missing_row(data = data, variable = variable,
                              by = by, missing_text = missing_text)
      )
  }

  # returning final object formatted for table_body ----------------------------
  # selecting stat_* cols (in the correct order)
  stat_vars <- switch(!is.null(by), df_by(data, by)$by_col) %||% "stat_0"
  result %>% select(all_of(c("variable", "row_type", "label", stat_vars)))
}

# calculate_missing_row --------------------------------------------------------
calculate_missing_row <- function(data, variable, by, missing_text) {
  # converting variable to TRUE/FALSE for missing
  data <-
    data %>%
    select(c(variable, by)) %>%
    mutate(
      !!variable := is.na(.data[[variable]])
    )

  # passing the T/F variable through the functions to format as we do in
  # the tbl_summary output
  summarize_categorical(
    data = data, variable = variable, by = by, class = "logical",
    dichotomous_value = TRUE, sort = "alphanumeric", percent = "column"
  ) %>%
    {df_stats_to_tbl(
      data = data, variable = variable, summary_type = "dichotomous", by = by,
      var_label = missing_text, stat_display = "{n}", df_stats = .,
      missing = "no", missing_text = "Doesn't Matter -- Text should never appear")} %>%
    # changing row_type to missing
    mutate(row_type = "missing")
}

# df_stats_fun -----------------------------------------------------------------
# this function creates df_stats in the tbl_summary meta data table
# and includes the number of missing values
df_stats_fun <- function(summary_type, variable, class, dichotomous_value, sort,
                         stat_display, digits, data, by, percent) {
  # first table are the standard stats
  t1 <- switch(
    summary_type,
    "continuous" = summarize_continuous(data = data, variable = variable,
                                        by = by, stat_display = stat_display,
                                        digits = digits),
    "categorical" = summarize_categorical(data = data, variable = variable,
                                          by = by, class = class,
                                          dichotomous_value = dichotomous_value,
                                          sort = sort, percent = percent),
    "dichotomous" = summarize_categorical(data = data, variable = variable,
                                          by = by, class = class,
                                          dichotomous_value = dichotomous_value,
                                          sort = sort, percent = percent)
  )

  # adding the N_obs and N_missing, etc
  t2 <- summarize_categorical(data = mutate_at(data, vars(all_of(variable)), is.na),
                              variable = variable,
                              by = by, class = "logical",
                              dichotomous_value = TRUE,
                              sort = "alphanumeric", percent = percent) %>%
    rename(p_miss = .data$p, N_obs = .data$N, N_miss = .data$n) %>%
    mutate(N_nonmiss = .data$N_obs - .data$N_miss,
           p_nonmiss = 1 - .data$p_miss)

  # returning table will all stats
  merge_vars <- switch(!is.null(by), c("by", "variable")) %||% "variable"
  return <- left_join(t1, t2, by = merge_vars)

  # setting fmt_fun for percents and integers
  attr(return$p_nonmiss, "fmt_fun") <- attr(return$p_miss, "fmt_fun")
  attr(return$N_nonmiss, "fmt_fun") <- attr(return$N_miss, "fmt_fun")

  return
}

# translation function ---------------------------------------------------------
translate_text <- function(x, language = get_theme_element("pkgwide-str:language", default = "en")) {
  if (language == "en") return(x)

  # sub-setting on row of text to translate
  df_text <- filter(df_translations, .data$en == x)

  # if no rows selected OR translation is not provided return x, otherwise the translated text
  ifelse(nrow(df_text) != 1 || is.na(df_text[[language]]), x, df_text[[language]])
}
