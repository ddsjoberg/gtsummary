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
assign_dichotomous_value <- function(data, variable, summary_type, value) {
  pmap(
    list(variable, summary_type),
    ~ assign_dichotomous_value_one(data, ..1, ..2, value)
  )
}

assign_dichotomous_value_one <- function(data, variable, summary_type, value) {
  # only assign value for dichotomous data
  if (!summary_type %in% "dichotomous") {
    return(NULL)
  }

  # removing all NA values
  var_vector <- data[[variable]] %>% stats::na.omit()

  # if 'value' provided, then dichotomous_value is the provided one
  if (!is.null(value[[variable]])) {
    return(value[[variable]])
  }

  # if class is logical, then value will be TRUE
  if (inherits(var_vector, "logical")) {
    return(TRUE)
  }

  # if column provided is a factor with "Yes" and "No" (or "yes" and "no") then
  # the value is "Yes" (or "yes")
  if (inherits(data[[variable]], c("factor", "character"))) {
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
  glue(
    "Variable '{variable}' is dichotomous, and I was unable to determine the ",
    "level to display. Use the 'value = list({variable} = <level>)' argument ",
    "to specify level."
  ) %>%
    str_wrap() %>%
    stop(call. = FALSE)
}

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

assign_stat_display <- function(data, variable, summary_type, stat_display) {
  # dichotomous and categorical are treated in the same fashion here
  summary_type <- ifelse(summary_type == "dichotomous", "categorical", summary_type)

  # returning the stats to display
  stat_display <-
    map2(
      variable, summary_type,
      ~ switch(inherits(data[[.x]], c("POSIXt", "Date")) &&
        .y %in% c("continuous", "continuous2"),
      stat_display[[.x]] %||% "{min} to {max}"
      ) %||%
        switch(.y,
          "categorical" = stat_display[[.x]] %||%
            get_theme_element("tbl_summary-str:categorical_stat") %||%
            "{n} ({p}%)",
          "dichotomous" = stat_display[[.x]] %||%
            get_theme_element("tbl_summary-str:categorical_stat") %||%
            "{n} ({p}%)",
          "continuous" = stat_display[[.x]] %||%
            get_theme_element("tbl_summary-str:continuous_stat") %||%
            "{median} ({p25}, {p75})",
          "continuous2" = stat_display[[.x]] %||%
            get_theme_element("tbl_summary-str:continuous_stat") %||%
            "{median} ({p25}, {p75})"
        )
    )

  # checking the stat_display is correct
  pmap(
    list(variable, summary_type, stat_display),
    function(variable, summary_type, stat_display) {
      if (summary_type != "continuous2" && length(stat_display) > 1) {
        if (summary_type == "continuous") {
          glue(
            "The statistic requested for continuous variable '{variable}' ",
            "has length greater than 1. If you would like a multi-line ",
            "summary update the summary type to 'continuous2', e.g. ",
            "`type = list({variable} ~ 'continuous2')`"
          ) %>%
            str_wrap() %>%
            stop(call. = FALSE)
        }
        glue(
          "The statistic requested for variable '{variable}' ",
          "cannot have length greater than 1."
        ) %>%
          str_wrap() %>%
          stop(call. = FALSE)
      }
    }
  )

  stat_display
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
#' @return Vector summary types `c("continuous", "continuous2", "categorical", "dichotomous")`.
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
assign_summary_type <- function(data, variable, summary_type, value,
                                # types are NOT checked on the first pass at guessing the type
                                check_assignment = FALSE) {
  # base classes that can be summarized as continuous
  base_numeric_classes <- c("numeric", "integer", "difftime", "Date", "POSIXt", "double")

  # assigning the summary type for each variable -------------------------------
  assigned_summary_type <-
    map_chr(
      variable,
      function(variable) {
        # checking if user requested type = "categorical" for variable that is all missing
        if (identical(summary_type[[variable]], "categorical") && sum(is.na(data[[variable]])) == nrow(data)) {
          glue(
            "Variable '{variable}' is `NA` for all observations and cannot be summarized as 'categorical'. ",
            "Using `{variable} ~ \"dichotomous\"` instead."
          ) %>%
            str_wrap() %>%
            inform()
          return("dichotomous")
        }

        # return type if specified by user
        if (!is.null(summary_type[[variable]])) {
          return(summary_type[[variable]])
        }

        # return dichotomous if dichotomous value passed
        if (!is.null(value[[variable]])) {
          return("dichotomous")
        }

        # logical variables are dichotomous
        if (inherits(data[[variable]], "logical")) {
          return("dichotomous")
        }

        # if all missing
        if (sum(is.na(data[[variable]])) == nrow(data)) {
          if (inherits(data[[variable]], base_numeric_classes)) {
            return("continuous")
          }
          if (inherits(data[[variable]], "character")) {
            return("dichotomous")
          }
          if (inherits(data[[variable]], "factor") &&
            !rlang::is_empty(attr(data[[variable]], "levels"))) {
            return("categorical")
          }
          if (inherits(data[[variable]], "factor") &&
            rlang::is_empty(attr(data[[variable]], "levels"))) {
            return("dichotomous")
          }
        }

        # numeric variables that are 0 and 1 only, will be dichotomous
        if (inherits(data[[variable]], c("integer", "numeric")) &&
          length(setdiff(stats::na.omit(data[[variable]]), c(0, 1))) == 0) {
          return("dichotomous")
        }

        # factor variables that are "No" and "Yes" only, will be dichotomous
        if (inherits(data[[variable]], "factor") &&
          setequal(attr(data[[variable]], "levels"), c("No", "Yes"))) {
          return("dichotomous")
        }
        if (inherits(data[[variable]], "factor") &&
          setequal(attr(data[[variable]], "levels"), c("no", "yes"))) {
          return("dichotomous")
        }
        if (inherits(data[[variable]], "factor") &&
          setequal(attr(data[[variable]], "levels"), c("NO", "YES"))) {
          return("dichotomous")
        }

        # character variables that are "No" and "Yes" only, will be dichotomous
        if (inherits(data[[variable]], "character") &&
          setequal(stats::na.omit(data[[variable]]), c("No", "Yes"))) {
          return("dichotomous")
        }
        if (inherits(data[[variable]], "character") &&
          setequal(stats::na.omit(data[[variable]]), c("no", "yes"))) {
          return("dichotomous")
        }
        if (inherits(data[[variable]], "character") &&
          setequal(stats::na.omit(data[[variable]]), c("NO", "YES"))) {
          return("dichotomous")
        }

        # factors and characters are categorical (except when all missing)
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

        # otherwise return NA (which will print an informative message later)
        return(NA_character_)
      }
    )

  # checking for variables that were not assigned a summary type
  if (check_assignment == TRUE && sum(is.na(assigned_summary_type))) {
    vars_with_no_type <- variable[is.na(assigned_summary_type)]
    glue(
      "Assign summary types for variables ",
      "{quoted_list(vars_with_no_type)} (e.g. 'continuous', 'categorical', or 'continuous'). ",
      "Specify the summary type using the `type=` argument. For example, ",
      "`type = list(c({paste(vars_with_no_type, collapse = ', ')}) ~ \"categorical\")`"
    ) %>%
      str_wrap() %>%
      {
        c(paste("Unable to determine a summary type!", ., sep = "\n\n"))
      } %>%
      abort()
  }

  # checking user did not request a factor or character variable be summarized
  # as a continuous variable
  purrr::pwalk(
    list(assigned_summary_type, variable),
    ~ if (..1 %in% c("continuous", "continuous2") && inherits(data[[..2]], c("factor", "character"))) {
      glue("Column '{..2}' is factor or character class and cannot be summarized as continuous.") %>%
        abort()
    }
  )

  assigned_summary_type
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
#' e.g. `var_label = list(age = "Age")`
#' @return Vector variable labels.
#' @keywords internal
#' @noRd
#' @author Daniel D. Sjoberg
#' @examples
#' gtsummary:::assign_var_label(mtcars, names(mtcars), list(hp = "Horsepower"))
assign_var_label <- function(data, variable, var_label) {
  map_chr(
    variable,
    function(.x) {
      # if user-specified, then return
      if (!is.null(var_label[[.x]])) {
        return(var_label[[.x]])
      }

      # there is a variable label, return it
      attr_label <- attr(data[[.x]], "label")
      if (!is.null(attr_label) && length(attr_label) == 1) {
        return(attr_label)
      }

      .x
    }
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

continuous_digits_guess <- function(data,
                                    variable,
                                    summary_type) {
  # if all values are NA, returning 0
  if (!is_survey(data) && nrow(data) == sum(is.na(data[[variable]]))) {
    return(0)
  }
  if (is_survey(data) && length(data$variables[[variable]]) == sum(is.na(data$variables[[variable]]))) {
    return(0)
  }

  # if the variable is not continuous type, return NA
  if (!summary_type %in% c("continuous", "continuous2")) {
    return(NA)
  }

  # if class is integer, then round everything to nearest integer
  if (!is_survey(data) && inherits(data[[variable]], "integer")) {
    return(0)
  }
  if (is_survey(data) && inherits(data$variables[[variable]], "integer")) {
    return(0)
  }

  # calculate the spread of the variable
  if (!is_survey(data)) {
    var_spread <-
      stats::quantile(data[[variable]], probs = c(0.95), na.rm = TRUE) -
      stats::quantile(data[[variable]], probs = c(0.05), na.rm = TRUE)
  }
  if (is_survey(data)) {
    var_spread <-
      compute_survey_stat(data = data, variable = variable, by = NULL, f = "p95")$p95 -
      compute_survey_stat(data = data, variable = variable, by = NULL, f = "p5")$p5
  }

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
  if (is.null(by)) {
    return(NULL)
  }

  if (!is_survey(data)) {
    # classic data.frame
    result <-
      data %>%
      select(by = all_of(by)) %>%
      count(!!sym("by"), .drop = FALSE) %>%
      arrange(!!sym("by")) %>%
      mutate(
        N = sum(.data$n),
        p = .data$n / .data$N,
        by_id = 1:n(), # 'by' variable ID
        by_chr = as.character(.data$by), # Character version of 'by' variable
        by_fct = # factor version of 'by' variable
          switch(inherits(.data$by, "factor"),
            factor(.data$by, levels = attr(.data$by, "levels"), ordered = FALSE)
          ) %||%
            factor(.data$by),
        by_col = paste0("stat_", .data$by_id) # Column name of in fmt_table1 output
      ) %>%
      select(starts_with("by"), everything())
  } else {
    # survey object
    svy_table <- survey::svytable(c_form(right = by), data, round = TRUE) %>%
      as_tibble() %>%
      set_names("by", "n") %>%
      mutate(
        N = sum(.data$n),
        p = .data$n / .data$N
      )

    result <- df_by(data$variables, by) %>%
      rename(n_unweighted = "n", N_unweighted = "N", p_unweighted = "p") %>%
      left_join(svy_table, by = "by")

    result
  }

  attr(result$by, "label") <- NULL
  result
}


#' Assigns categorical variables sort type ("alphanumeric" or "frequency")
#'
#' @param variable variable name
#' @param summary_type the type of variable ("continuous", "continuous2", "categorical", "dichotomous")
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

tbl_summary_input_checks <- function(data, by, missing_text, include) {
  # data -----------------------------------------------------------------------
  tbl_summary_data_checks(data)
  check_haven_labelled(data, variables = c(include, by))

  # missing_text ---------------------------------------------------------------
  # input must be character
  if (!rlang::is_string(missing_text)) {
    stop("Argument 'missing_text' must be a character string of length 1.", call. = FALSE)
  }
}

# stat_label_match -------------------------------------------------------------
# provide a vector of stat_display and get labels back i.e. {mean} ({sd}) gives Mean (SD)
stat_label_match <- function(stat_display, iqr = TRUE, range = TRUE) {
  language <- get_theme_element("pkgwide-str:language", default = "en")
  labels <-
    tibble::tribble(
      ~stat, ~label,
      "{min}", "Minimum",
      "{max}", "Maximum",
      "{median}", "Median",
      "{mean}", "Mean",
      "{sd}", "SD",
      "{var}", "Variance",
      "{sum}", "Sum",
      "{n}", "n",
      "{N}", "N",
      "{p}%", "%",
      "{p}", "%",
      "{p_miss}%", "% missing",
      "{p_miss}", "% missing",
      "{p_nonmiss}%", "% not missing",
      "{p_nonmiss}", "% not missing",
      "{N_miss}", "N missing",
      "{N_nonmiss}", "N",
      "{N_obs}", "No. obs.",
      "{mean.std.error}", "SE",
      "{p.std.error}%", "SE(%)",
      "{p.std.error}", "SE(%)",
      "{deff}", "Design effect",
      "{N_unweighted}", "N (unweighted)",
      "{n_unweighted}", "n (unweighted)",
      "{N_obs_unweighted}", "Total N (unweighted)",
      "{N_miss_unweighted}", "N Missing (unweighted)",
      "{N_nonmiss_unweighted}", "N not Missing (unweighted)",
      "{p_unweighted}%", "% (unweighted)",
      "{p_unweighted}", "% (unweighted)",
      "{p_miss_unweighted}%", "% Missing (unweighted)",
      "{p_miss_unweighted}", "% Missing (unweighted)",
      "{p_nonmiss_unweighted}%", "% not Missing (unweighted)",
      "{p_nonmiss_unweighted}", "% not Missing (unweighted)"
    ) %>%
    # adding in quartiles
    bind_rows(
      tibble(stat = paste0("{p", 0:100, "}")) %>%
        mutate(label = paste0(gsub("[^0-9\\.]", "", .data$stat), "%"))
    ) %>%
    # translating statistic names
    mutate(label = map_chr(.data$label, ~ translate_text(.x, language))) %>%
    # if function does not appear in above list, the print the function name
    bind_rows(
      tibble(
        stat = str_extract_all(unlist(stat_display), "\\{.*?\\}") %>%
          unlist() %>%
          unique(),
        label = .data$stat %>%
          str_remove_all(pattern = fixed("}")) %>%
          str_remove_all(pattern = fixed("{"))
      )
    )

  # adding IQR replacements if indicated
  has_iqr_translation <- !is.na(filter(df_translations, .data$en == "IQR")[[language]])
  if (iqr == TRUE && has_iqr_translation) {
    labels <-
      bind_rows(
        tibble::tribble(
          ~stat, ~label,
          "{p25}, {p75}", translate_text("IQR", language),
          "{p25} \U2013 {p75}", translate_text("IQR", language),
          "{p25} - {p75}", translate_text("IQR", language)
        ),
        labels
      )
  }

  # adding range replacements if indicated
  has_range_translation <- !is.na(filter(df_translations, .data$en == "Range")[[language]])
  if (range == TRUE && has_range_translation) {
    labels <-
      bind_rows(
        tibble::tribble(
          ~stat, ~label,
          "{min}, {max}", translate_text("Range", language),
          "{min} \U2013 {max}", translate_text("Range", language),
          "{min} to {max}", translate_text("Range", language),
          "{min} - {max}", translate_text("Range", language)
        ),
        labels
      )
  }

  # replacing statistics in {}, with their labels
  map(
    stat_display,
    function(.x) {
      for (i in seq_len(nrow(labels))) {
        .x <- stringr::str_replace_all(
          .x,
          stringr::fixed(labels$stat[i]),
          labels$label[i]
        )
      }
      .x
    }
  )
}

# footnote_stat_label ----------------------------------------------------------
# stat_label footnote maker
footnote_stat_label <- function(meta_data) {
  meta_data %>%
    select(c("summary_type", "stat_label")) %>%
    filter(!.data$summary_type %in% "continuous2") %>% # removing multiline stats
    mutate(
      summary_type = case_when(
        summary_type == "dichotomous" ~ "categorical",
        TRUE ~ .data$summary_type
      ),
      message = glue("{stat_label}")
    ) %>%
    distinct() %>%
    pull("message") %>%
    stats::na.omit() %>%
    {
      .purrr_when(
        rlang::is_empty(.) ~ NA_character_,
        TRUE ~ paste(., collapse = "; ")
      )
    }
}

# summarize_categorical --------------------------------------------------------
summarize_categorical <- function(data, variable, by, class, dichotomous_value,
                                  sort, percent, stat_display) {
  # tabulating data ------------------------------------------------------------
  df_by <- df_by(data, by)
  variable_by_chr <- c("variable", switch(!is.null(by),
    "by"
  ))
  data <- data %>%
    select(any_of(c(variable, by))) %>%
    # renaming variables to c("variable", "by") (if there is a by variable)
    set_names(variable_by_chr)

  df_tab <-
    data %>%
    # converting to factor, if not already factor
    mutate_at(
      vars(any_of(c("variable", "by"))),
      ~ switch(inherits(., "factor"),
        factor(., levels = attr(., "levels"), ordered = FALSE)
      ) %||%
        factor(.)
    ) %>%
    mutate(
      # adding dichotomous level (in case it is unobserved)
      variable = forcats::fct_expand(.data$variable, as.character(dichotomous_value)),
      # re-leveling by alphanumeric order or frequency
      variable = switch(sort,
        "alphanumeric" = .data$variable,
        "frequency" = forcats::fct_infreq(.data$variable)
      )
    ) %>%
    filter(!is.na(.data$variable)) %>%
    count(!!!syms(variable_by_chr), .drop = FALSE)

  # replacing factor by with original version
  if ("by" %in% variable_by_chr) {
    df_tab <-
      df_tab %>%
      select(by_fct = "by", everything()) %>%
      left_join(df_by[c("by", "by_fct")], by = "by_fct") %>%
      select(-"by_fct")
  }

  # calculating percent
  group_by_percent <- switch(percent,
    "cell" = "",
    "column" = ifelse(!is.null(by), "by", ""),
    "row" = "variable"
  )

  result <-
    df_tab %>%
    group_by(!!!syms(group_by_percent)) %>%
    mutate(
      N = sum(.data$n),
      # if the Big N is 0, there is no denom so making percent NA
      p = ifelse(.data$N == 0, NA, .data$n / .data$N)
    ) %>%
    ungroup() %>%
    rename(variable_levels = "variable") %>%
    mutate(variable = !!variable) %>%
    select(any_of(c("by", "variable", "variable_levels")), everything())

  if (!is.null(dichotomous_value)) {
    result <- result %>%
      filter(.data$variable_levels == !!dichotomous_value) %>%
      select(-"variable_levels")
  }

  result <-
    result %>%
    mutate(stat_display = .env$stat_display) %>%
    select(any_of(c("by", "variable", "variable_levels", "stat_display")), everything())

  result
}

# summarize_continuous ---------------------------------------------------------
summarize_continuous <- function(data, variable, by, stat_display, summary_type) {
  # extracting function calls
  fns_names_chr <- extracting_function_calls_from_stat_display(stat_display, variable)

  # if there are no continuous summary functions, return tibble early ----------
  if (length(fns_names_chr) == 0) {
    if (!is.null(by)) {
      df_stats <- tibble(
        by = unique(data[[by]]) %>% sort(),
        variable = variable,
        stat_display = .env$stat_display
      )
    } else {
      df_stats <- tibble(
        variable = variable,
        stat_display = .env$stat_display
      )
    }
    return(df_stats)
  }

  # defining shortcut quantile functions, if needed
  if (any(fns_names_chr %in% paste0("p", 0:100))) {
    fns_names_chr[fns_names_chr %in% paste0("p", 0:100)] %>%
      set_names(.) %>%
      imap(~function(x) stats::quantile(x, probs = as.numeric(stringr::str_replace(.x, pattern = "^p", "")) / 100)) %>%
      list2env(envir = rlang::env_parent())
  }

  # prepping data set
  variable_by_chr <- c("variable", switch(!is.null(by),
    "by"
  ))
  df_by <- df_by(data, by)
  data <-
    data %>%
    select(any_of(c(variable, by))) %>%
    # renaming variables to c("variable", "by") (if there is a by variable)
    set_names(variable_by_chr)

  # calculating stats for each var and by level
  fns <- map(fns_names_chr, ~ rlang::sym(.) %>% eval()) %>% set_names(fns_names_chr)
  df_stats <-
    data %>%
    mutate_at(
      vars(any_of("by")),
      ~ switch(inherits(., "factor"),
        factor(., levels = attr(., "levels"), ordered = FALSE)
      ) %||%
        factor(.)
    ) %>%
    stats::na.omit() %>%
    dplyr::group_by_at(switch(!is.null(by),
      "by"
    ), .drop = FALSE) %>%
    safe_summarise_at(variable = variable, fns = fns) %>%
    # dplyr::summarise_at(vars(.data$variable), tibble::lst(!!!fns_names_expr)) %>%
    mutate(variable = .env$variable) %>%
    select(any_of(c("by", "variable")), everything())

  # replacing by variable with original (non-factor version)
  if (!is.null(by)) {
    df_stats <-
      df_stats %>%
      select(by_fct = "by", everything()) %>%
      left_join(df_by[c("by", "by_fct")], by = "by_fct") %>%
      select(-"by_fct")
  }

  # adding stat_display to the data frame
  if (summary_type == "continuous2") {
    return <-
      dplyr::cross_join(
        df_stats,
        tibble(
          variable_levels = map_chr(stat_display, ~ stat_label_match(.x) %>% unlist()),
          stat_display = stat_display
        )
      ) %>%
      select(any_of(c("by", "variable", "variable_levels", "stat_display")), everything())
  } else {
    return <-
      df_stats %>%
      mutate(stat_display = .env$stat_display) %>%
      select(any_of(c("by", "variable", "variable_levels", "stat_display")), everything())
  }

  # returning final object
  return
}

safe_summarise_at <- function(data, variable, fns) {
  tryCatch(
    {
      # ref for all this `.keep_attr()` nonsense stackoverflow.com/questions/67291199
      dplyr::summarise_at(
        data,
        vars("variable"),
        map(
          fns,
          function(.x) {
            if (identical(.x, stats::median)) {
              return(rlang::inject(function(x) .keep_attr(x, .f = !!.x)))
            } else {
              return(.x)
            }
          }
        )
      )
    },
    error = function(e) {
      # replace p[0:100] stats with `quantile`
      fns_names <- stringr::str_replace(names(fns), "^p\\d+$", "quantile") %>% unique()
      paste(
        "There was an error calculating the summary statistics",
        "for {.val {variable}}. Is this variable's class",
        "supported by {.code {fns_names}}?"
      ) %>%
        cli::cli_alert_danger()

      abort(e)
    }
  )
}

.keep_attr <- function(x, .f) {
  x_att <- attributes(x)
  res <- .f(x)
  tryCatch(
    attributes(res) <- x_att,
    error = function(e) invisible()
  )

  res
}


# extracting_function_calls_from_stat_display ---------------------
extracting_function_calls_from_stat_display <- function(stat_display, variable) {
  fns_names_chr <-
    stat_display %>%
    paste(collapse = " ") %>%
    str_extract_all("\\{.*?\\}") %>%
    map(~ str_remove_all(.x, pattern = fixed("}"))) %>%
    map(~ str_remove_all(.x, pattern = fixed("{"))) %>%
    unlist()

  if (length(fns_names_chr) == 0) {
    stop(glue(
      "No summary function found in `{stat_display}` for variable '{variable}'.\n",
      "Did you wrap the function name in curly brackets?"
    ), call. = FALSE)
  }

  # removing elements protected as other items
  fns_names_chr <- fns_names_chr %>%
    setdiff(c("p_miss", "p_nonmiss", "N_miss", "N_nonmiss", "N_obs"))

  if (any(c("by", "variable") %in% fns_names_chr)) {
    stop(paste(
      "'by' and 'variable' are protected names, and continuous variables",
      "cannot be summarized with functions by the these name."
    ), call. = FALSE)
  }

  fns_names_chr
}

# adding_formatting_as_attr ----------------------------------------------------
adding_formatting_as_attr <- function(df_stats, data, variable, summary_type,
                                      stat_display, digits) {
  # setting the default formatting ---------------------------------------------
  percent_fun <-
    get_theme_element("tbl_summary-fn:percent_fun") %||%
    .get_deprecated_option("gtsummary.tbl_summary.percent_fun", default = style_percent)

  # extracting statistics requested
  fns_names_chr <-
    str_extract_all(stat_display, "\\{.*?\\}") %>%
    map(~ str_remove_all(.x, pattern = fixed("}"))) %>%
    map(~ str_remove_all(.x, pattern = fixed("{"))) %>%
    unlist()
  base_stats <- c(
    "p_miss", "p_nonmiss", "N_miss", "N_nonmiss", "N_obs",
    "N_obs_unweighted", "N_miss_unweighted", "N_nonmiss_unweighted",
    "p_miss_unweighted", "p_nonmiss_unweighted"
  )
  percent_stats <- c("p_miss", "p_nonmiss", "p_miss_unweighted", "p_nonmiss_unweighted")

  # converting the digits input to a list
  if (is.numeric(digits[[variable]])) {
    digits[[variable]] <- as.list(digits[[variable]])
  } else if (rlang::is_function(digits[[variable]])) digits[[variable]] <- list(digits[[variable]])

  # if user supplied number of digits to round, use them
  if (!is.null(digits[[variable]])) {
    digits[[variable]] <-
      # making the digits passed the same length as stat vector
      rep(digits[[variable]], length.out = length(fns_names_chr)) %>%
      rlang::set_names(fns_names_chr) %>%
      # converting digits to fns
      imap(
        # scale percents by 100
        ~ switch(is.numeric(.x) & .y %in% percent_stats,
                 function(x) style_number(x, digits = .x, scale = 100)
        ) %||%
          switch(is.numeric(.x) & summary_type %in% c("categorical", "dichotomous") & .y %in% c("p", "p_unweighted"),
                 function(x) style_number(x, digits = .x, scale = 100)
          ) %||%
          # all other stats are not scaled
          switch(is.numeric(.x),
                 function(x) style_number(x, digits = .x)
          ) %||%
          .x # if user passed a function, then return the function
      )
  }

  # if variable is a date, then convert to character
  else if ((inherits(data[[variable]], c("POSIXt", "Date")) ||
    inherits(data[["variables"]][[variable]], c("POSIXt", "Date"))) &&
    summary_type %in% c("continuous", "continuous2")) {
    digits[[variable]] <-
      rep(list(as.character), length.out = length(fns_names_chr %>% setdiff(base_stats))) %>%
      rlang::set_names(fns_names_chr %>% setdiff(base_stats))
  }

  # if no digits supplied and variable is continuous, guess how to summarize
  else if (summary_type %in% c("continuous", "continuous2")) {
    digits[[variable]] <-
      continuous_digits_guess(
        data = data,
        variable = variable,
        summary_type = summary_type
      ) %>%
      rep(length.out = length(fns_names_chr %>% setdiff(base_stats))) %>%
      as.list() %>%
      rlang::set_names(fns_names_chr %>% setdiff(base_stats)) %>%
      map(~ function(x) style_number(x = x, digits = .x))
  }

  # adding the formatting function as an attribute
  df_stats <-
    purrr::imap_dfc(
      df_stats,
      function(column, colname) {
        if (colname %in% c("by", "variable", "variable_levels", "stat_display", "label", "col_name")) {
          return(column)
        } # if the fmt function is already defined, then add it as attribute
        else if (!is.null(digits[[variable]][[colname]])) {
          attr(column, "fmt_fun") <- digits[[variable]][[colname]]
        }

        # if the variable is categorical and a percent, use `style_percent`
        else if (summary_type %in% c("categorical", "dichotomous") &
          colname %in% c(
            "p", "p_unweighted", "p_miss", "p_nonmiss",
            "p_miss_unweighted", "p_nonmiss_unweighted"
          )) {
          attr(column, "fmt_fun") <- percent_fun
        }

        # if the variable is categorical and an N, use `style_number`
        else if (summary_type %in% c("categorical", "dichotomous") &
          colname %in% c(
            "N", "n", "n_unweighted", "N_unweighted",
            "N_obs", "N_miss", "N_nonmiss",
            "N_obs_unweighted", "N_miss_unweighted",
            "N_nonmiss_unweighted"
          )) {
          attr(column, "fmt_fun") <- style_number
        }

        # if the column is one of the remaining percent cols, use `style_percent`
        else if (colname %in% percent_stats) {
          attr(column, "fmt_fun") <- percent_fun
        }

        # if the column is one of the remaining N cols, use `style_number`
        else if (colname %in% base_stats) {
          attr(column, "fmt_fun") <- style_number
        }

        # that should cover everything in `tbl_summary()`,
        # but these are sometimes used in `tbl_custom_summary()`
        else if (is.numeric(column)) {
          attr(column, "fmt_fun") <- style_sigfig
        } else {
          attr(column, "fmt_fun") <- as.character
        }

        # return column
        column
      }
    )

  df_stats
}

# df_stats_to_tbl --------------------------------------------------------------
df_stats_to_tbl <- function(data, variable, summary_type, by, var_label, stat_display,
                            df_stats, missing, missing_text) {
  # styling the statistics -----------------------------------------------------
  df_stats_original <- df_stats
  for (v in (names(df_stats) %>% setdiff(c("by", "variable", "variable_levels", "stat_display")))) {
    if (is.function(attr(df_stats[[v]], "fmt_fun"))) {
      df_stats[[v]] <- df_stats[[v]] %>% attr(df_stats[[v]], "fmt_fun")()
    }
  }

  # calculating the statistic to be displayed in the cell in the table.
  tryCatch(
    df_stats <-
      df_stats %>%
      dplyr::rowwise() %>%
      mutate(statistic = glue::glue(.data$stat_display) %>% as.character()) %>%
      ungroup() %>%
      select(-"stat_display"),
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
    }
  )

  # reshaping table to wide ----------------------------------------------------
  if (!is.null(by)) {
    df_stats_wide <-
      df_stats %>%
      select(any_of(c("by", "variable", "variable_levels", "statistic"))) %>%
      # merging in new column header names
      left_join(df_by(data, by)[c("by", "by_col")], by = "by") %>%
      tidyr::pivot_wider(
        id_cols = any_of(c("variable", "variable_levels")),
        names_from = "by_col",
        values_from = "statistic"
      )
  } else {
    df_stats_wide <-
      df_stats %>%
      select(any_of(c("by", "variable", "variable_levels", "statistic"))) %>%
      rename(stat_0 = "statistic") %>%
      select(any_of(c("variable", "variable_levels", "stat_0")))
  }

  # setting up structure for table_body
  if (summary_type %in% c("categorical", "continuous2")) {
    # adding a label row for categorical variables
    result <-
      df_stats_wide %>%
      mutate(
        row_type = "level",
        label = as.character(.data$variable_levels)
      ) %>%
      select(-"variable_levels")

    # adding label row
    result <-
      tibble(
        variable = variable,
        row_type = "label",
        label = var_label
      ) %>%
      bind_rows(result)
  } else if (summary_type %in% c("continuous", "dichotomous")) {
    result <-
      df_stats_wide %>%
      mutate(
        row_type = "label",
        label = var_label
      )
  }

  # add rows for missing -------------------------------------------------------
  if (missing == "always" || (missing == "ifany" & has_na(data, variable))) {
    # browser()

    missing_stat <- get_theme_element("tbl_summary-str:missing_stat", default = "{N_miss}")
    result <-
      result %>%
      bind_rows(
        df_stats_original %>%
          select(any_of(c(
            "by", "variable", "N_miss", "N_obs", "p_miss", "N_nonmiss", "p_nonmiss",
            "N_obs_unweighted", "N_miss_unweighted", "N_nonmiss_unweighted",
            "p_miss_unweighted", "p_nonmiss_unweighted"
          ))) %>%
          distinct() %>%
          mutate(stat_display = .env$missing_stat) %>%
          {
            df_stats_to_tbl(
              data = data, variable = variable, summary_type = "dichotomous", by = by,
              var_label = missing_text, stat_display = missing_stat, df_stats = .,
              missing = "no", missing_text = "Doesn't Matter -- Text should never appear"
            )
          } %>%
          # changing row_type to missing
          mutate(row_type = "missing")
      )
  }

  # returning final object formatted for table_body ----------------------------
  # selecting stat_* cols (in the correct order)
  stat_vars <- switch(!is.null(by),
    df_by(data, by)$by_col
  ) %||% "stat_0"
  result %>% select(all_of(c("variable", "row_type", "label", stat_vars)))
}


# df_stats_fun -----------------------------------------------------------------
# this function creates df_stats in the tbl_summary meta data table
# and includes the number of missing values
df_stats_fun <- function(summary_type, variable, dichotomous_value, sort,
                         stat_display, data, by, percent, digits, var_label) {
  # first table are the standard stats
  t1 <- switch(summary_type,
    "continuous" = summarize_continuous(
      data = data, variable = variable,
      by = by, stat_display = stat_display,
      summary_type = summary_type
    ),
    "continuous2" = summarize_continuous(
      data = data, variable = variable,
      by = by, stat_display = stat_display,
      summary_type = summary_type
    ),
    "categorical" = summarize_categorical(
      data = data, variable = variable,
      by = by,
      dichotomous_value = dichotomous_value,
      sort = sort, percent = percent,
      stat_display = stat_display
    ),
    "dichotomous" = summarize_categorical(
      data = data, variable = variable,
      by = by,
      dichotomous_value = dichotomous_value,
      sort = sort, percent = percent,
      stat_display = stat_display
    )
  )

  # adding the N_obs and N_missing, etc
  t2 <- summarize_categorical(
    data = mutate_at(data, vars(all_of(variable)), is.na),
    variable = variable,
    by = by,
    dichotomous_value = TRUE,
    sort = "alphanumeric", percent = "column",
    stat_display = "{n}"
  ) %>%
    select(-"stat_display") %>%
    rename(p_miss = "p", N_obs = "N", N_miss = "n") %>%
    mutate(
      N_nonmiss = .data$N_obs - .data$N_miss,
      p_nonmiss = 1 - .data$p_miss
    )

  # returning table will all stats
  merge_vars <- switch(!is.null(by),
    c("by", "variable")
  ) %||% "variable"
  return <- left_join(t1, t2, by = merge_vars)

  # adding underlying column name
  if ("by" %in% names(return)) {
    return <-
      return %>%
      left_join(df_by(data, by)[c("by", "by_col")], by = "by") %>%
      rename(col_name = "by_col")
  } else {
    return$col_name <- "stat_0"
  }

  # adding label column
  if ("variable_levels" %in% names(return)) {
    return$label <- as.character(return$variable_levels)
  } else {
    return$label <- var_label
  }

  # adding formatting function as attr to summary statistics columns
  return <- adding_formatting_as_attr(
    df_stats = return, data = data, variable = variable,
    summary_type = summary_type, stat_display = stat_display, digits = digits
  )

  return
}

# translation function ---------------------------------------------------------
translate_text <- function(x, language = get_theme_element("pkgwide-str:language", default = "en")) {
  if (language == "en" || identical(x, character(0))) {
    return(x)
  }

  # sub-setting on row of text to translate
  df_text <- filter(df_translations, .data$en == x)

  # if no rows selected OR translation is not provided return x, otherwise the translated text
  ifelse(nrow(df_text) != 1 || is.na(df_text[[language]]), x, df_text[[language]])
}

# test if a variable has some NA -------------------------------------
has_na <- function(data, variable) {
  if (is_survey(data)) {
    sum(is.na(data$variables[[variable]])) > 0
  } else {
    sum(is.na(data[[variable]])) > 0
  }
}

# convert a tbl_summary meta_data object to a var_info tibble
meta_data_to_var_info <- function(meta_data) {
  var_info <-
    meta_data %>%
    select(any_of(c("variable", "summary_type", "class", "var_label")))

  if ("class" %in% names(var_info)) {
    var_info <-
      var_info %>%
      mutate(var_class = map_chr(.data$class, ~ pluck(.x, 1))) %>%
      select(-"class")
  }
  if ("summary_type" %in% names(var_info)) {
    var_info <- select(var_info, var_type = "summary_type", everything())
  }

  var_info
}

# simple function to evaluate the RHS of a formula in the formula's environment
eval_rhs <- function(x) {
  rlang::f_rhs(x) %>% rlang::eval_tidy(env = rlang::f_env(x))
}

check_haven_labelled <- function(data, variables) {
  # extract data frame
  data <- switch(is_survey(data),
    data$variables
  ) %||% data

  if (purrr::some(data[variables], ~ inherits(., "haven_labelled"))) {
    # list of columns with haven_labelled
    haven_labelled_vars <-
      purrr::map_lgl(data, ~ inherits(.x, "haven_labelled")) %>%
      keep(identity) %>%
      names()

    cnvt_funs <-
      c("haven::as_factor()", "labelled::to_factor()", "labelled::unlabelled()", "unclass()")
    hyperlinks <-
      c(
        "https://haven.tidyverse.org/articles/semantics.html",
        "https://larmarange.github.io/labelled/articles/intro_labelled.html#unlabelled"
      )

    paste(
      "Column(s) {.field {haven_labelled_vars}} are class {.val haven_labelled}.",
      "This is an intermediate datastructure not meant for analysis.",
      "Convert columns with {.code {cnvt_funs}}.",
      "{.val haven_labelled} value labels are ignored when columns are not converted.",
      "Failure to convert may have unintended consequences or result in error."
    ) %>%
      cli::cli_alert_info()
    cli::cli_ul(hyperlinks)
  }
  return(invisible(NULL))
}
