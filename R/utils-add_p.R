#' Convert test arg input to function
#'
#' This function accepts the RHS of the test argument formula, and
#' returns a list including the function that will run to calculate
#' the p-value. List also includes the description of the test, and
#' whether the test accept `test.args()`
#'
#' @param class string indicating the class of test to select from.
#' Must be one of `c("tbl_summary", "tbl_svysummary", "tbl_survfit")`
#' @param test test indicated on the RHS of the formula
#' @param env environment where formula was created. When a test is passed
#' as a character, this helps ensure the character is converted to the test
#' function object in the correct environment.
#' @param parent_fun String indicating either "add_p" or "add_difference",
#' the parent function.
#'
#' @noRd
#' @author Daniel D. Sjoberg
.get_add_p_test_fun <- function(class, test, env = NULL, parent_fun = "add_p") {
  # if no test, then return NULL
  if (is.null(test)) {
    return(NULL)
  }

  # keep class of tests
  df <-
    df_add_p_tests %>%
    filter(.data$class %in% .env$class)

  # if test is character, then subset based on test name
  if (rlang::is_string(test)) { # character test name --------------------------
    df <-
      df %>%
      filter(.data$test_name %in% .env$test)
  } else if (rlang::is_function(test)) { # test function passed ------------------
    df <-
      df %>%
      # now select test object equivalent to the passed function
      filter(map_lgl(
        .data$test_fun,
        ~ tryCatch(identical(eval(.x), test),
          error = function(e) FALSE
        )
      ))
  }

  # return info from df if internal test selected
  if (nrow(df) == 1) {
    if (parent_fun == "add_p" && df$add_p == FALSE) {
      glue(
        "You've selected test '{df$test_name}', which does not ",
        "return p-values. See `?tests` for details."
      ) %>%
        rlang::inform()
    }
    if (parent_fun == "add_difference" && df$add_difference == FALSE) {
      glue(
        "You've selected test '{df$test_name}', which does not ",
        "return a difference. See `?tests` for details."
      ) %>%
        rlang::inform()
    }

    return(
      df %>%
        select(any_of(c("test_name", "fun_to_run", "accept_dots"))) %>%
        mutate_at(vars("fun_to_run"), ~ map(.x, eval)) %>%
        as.list() %>%
        purrr::flatten()
    )
  }
  if (rlang::is_string(test) && nrow(df) == 0) {
    return(
      list(
        test_name = "user-defined",
        fun_to_run = rlang::parse_expr(test) %>% rlang::eval_tidy(env = env),
        accept_dots = FALSE
      )
    )
  }
  if (rlang::is_function(test) && nrow(df) == 0) {
    return(
      list(
        test_name = "user-defined",
        fun_to_run = test,
        accept_dots = FALSE
      )
    )
  }
  abort("Something went wrong in the test selection....")
}

#' Calculate p-values
#'
#' @param x list returned from `.get_add_p_test_fun()`
#' @param data data frame/object to calculate p-values from
#' @param by character by variable name
#' @param variable character variable name
#' @param group optional group variable
#' @param type optional type variable
#' @param test.args named list of additional arguments to pass to `test=`
#' @noRd
.run_add_p_test_fun <- function(x, data, variable, by = NULL, group = NULL,
                                type = NULL, test.args = NULL, conf.level = 0.95,
                                adj.vars = NULL, tbl = NULL,
                                continuous_variable = NULL) {
  # if x is NULL, return NULL
  if (is.null(x)) {
    return(NULL)
  }

  # calculating test function
  test_fun_result <-
    tryCatch(
      withCallingHandlers(
        {
          # calculating p-value
          do.call(x$fun_to_run, list(
            data = data, variable = variable, by = by,
            group = group, type = type, test.args = test.args,
            conf.level = conf.level, tbl = tbl,
            adj.vars = adj.vars,
            continuous_variable = continuous_variable
          ))
        },
        # printing warning and errors as message
        warning = function(w) {
          w <- as.character(w)
          # hidden warnings from survey::svychisq() should not be returned
          w <- stringr::str_subset(w, "chisq.test\\(svytable\\(", negate = TRUE)
          if (length(w) > 0) {
            message(glue(
              "Warning for variable '{variable}':\n ", w
            ))
          }
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        message(glue(
          "There was an error in 'add_p()/add_difference()' for variable '{variable}', ",
          "p-value omitted:\n", as.character(e)
        ))
        return(NULL)
      }
    )

  # saving test function results into list 'x'
  if (is.data.frame(test_fun_result)) {
    x$df_result <- test_fun_result
  } # these list inputs were deprecated and deleted from documentation in v1.3.6
  else if (is.list(test_fun_result) &&
    setequal(names(test_fun_result), c("p", "test"))) {
    x$df_result <-
      tibble::as_tibble(test_fun_result) %>%
      dplyr::rename(p.value = "p", method = "test")
  } else if (rlang::is_scalar_double(test_fun_result)) {
    x$df_result <- tibble(p.value = test_fun_result, method = NA_character_)
  } else if (is.null(test_fun_result)) {
    x$df_result <- tibble(p.value = NA_real_, method = NA_character_)
  }

  x$df_result <- x$df_result %>%
    select(
      any_of(c(
        "estimate", "std.error", "statistic", "parameter",
        "conf.low", "conf.high", "p.value", "method"
      )),
      everything()
    )
  x
}

#' Assign test in `add_p.tbl_summary()`
#'
#' Returns either the user-supplied test, or the default test given the variable
#' type
#'
#' @param data a data frame
#' @param variable character variable name
#' @param summary_type summary type
#' @param by character by variable
#' @param group character grouping variable
#' @param test named list of user-supplied tests
#' @noRd
#' @keywords internal
.assign_test_tbl_summary <- function(data, variable, summary_type, by, group, test) {
  # if user supplied a test, use that test -------------------------------------
  if (!is.null(test[[variable]])) {
    return(test[[variable]])
  }

  # if all obs are missing, return NULL ----------------------------------------
  # if (length(data[[variable]]) == sum(is.na(data[[variable]]))) {
  #   return(NULL)
  # }

  # if no test supplied, setting defaults --------------------------------------
  # if by var has 3 or more levels, return error...no default test.
  if (!is.null(group) && length(unique(data[[by]])) > 2) {
    stop("There is no default test for correlated data when `by=` variable has >2 levels.", call. = FALSE)
  }

  # if group variable supplied, fit a random effects model
  if (!is.null(group) & length(unique(data[[by]])) == 2) {
    if (summary_type %in% c("continuous", "continuous2")) {
      test_func <-
        get_theme_element("add_p.tbl_summary-attr:test.continuous.group_by2") %||%
        .get_deprecated_option("gtsummary.add_p.test.continuous.group_by2", default = "lme4")
      return(test_func)
    }
    if (summary_type %in% c("categorical", "dichotomous")) {
      test_func <-
        get_theme_element("add_p.tbl_summary-attr:test.categorical.group_by2") %||%
        .get_deprecated_option("gtsummary.add_p.test.categorical.group_by2", default = "lme4")
      return(test_func)
    }
  }

  # for continuous data, default to non-parametric tests
  if (summary_type %in% c("continuous", "continuous2") & length(unique(data[[by]])) == 2) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.continuous_by2") %||%
      .get_deprecated_option("gtsummary.add_p.test.continuous_by2", default = "wilcox.test")
    return(test_func)
  }
  if (summary_type %in% c("continuous", "continuous2")) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.continuous") %||%
      .get_deprecated_option("gtsummary.add_p.test.continuous", default = "kruskal.test")
    return(test_func)
  }

  # calculate expected counts to select between chisq and fisher
  min_exp <-
    suppressWarnings(
      table(data[[by]], data[[variable]]) %>%
        proportions() %>%
        {expand.grid(rowSums(.), colSums(.))} %>%
        mutate(
          exp = .data$Var1 * .data$Var2 *
            sum(!is.na(data[[variable]]) & !is.na(data[[by]]))
        ) %>%
        pull(exp) %>%
        min()
    )

  # if expected counts >= 5 for all cells, chisq, otherwise Fishers exact
  if (isTRUE(min_exp >= 5 || is.nan(min_exp))) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.categorical") %||%
      .get_deprecated_option("gtsummary.add_p.test.categorical", default = "chisq.test.no.correct")
    return(test_func)
  }
  test_func <-
    get_theme_element("add_p.tbl_summary-attr:test.categorical.low_count") %||%
    .get_deprecated_option("gtsummary.add_p.test.categorical.low_count", default = "fisher.test")
  return(test_func)
}


#' Assign test in `add_p.tbl_svysummary()`
#'
#' Returns either the user-supplied test, or the default test given the variable
#' type
#'
#' @param data a survey object
#' @param variable character variable name
#' @param summary_type summary type
#' @param by character by variable
#' @param test named list of user-supplied tests
#' @noRd
#' @keywords internal
.assign_test_tbl_svysummary <- function(data, variable, summary_type, by, test) {
  # if user supplied a test, use that test -------------------------------------
  if (!is.null(test[[variable]])) {
    return(test[[variable]])
  }

  # for continuous data, default to non-parametric tests
  if (summary_type %in% c("continuous", "continuous2")) {
    test_func <-
      get_theme_element("add_p.tbl_svysummary-attr:test.continuous", default = "svy.wilcox.test")
    return(test_func)
  }

  # for categorical data, default to chi-squared with Rao & Scott correction
  if (summary_type %in% c("categorical", "dichotomous")) {
    test_func <-
      get_theme_element("add_p.tbl_svysummary-attr:test.categorical", default = "svy.chisq.test")
    return(test_func)
  }
}

.assign_test_add_diff <- function(data, variable, summary_type, by, group, test,
                                  adj.vars) {
  # if user supplied a test, use that test -------------------------------------
  if (!is.null(test[[variable]])) {
    return(test[[variable]])
  }

  if (summary_type %in% c("continuous", "continuous2") &&
    is.null(group) && is.null(adj.vars) && is.data.frame(data)) {
    return("t.test")
  }
  if (summary_type %in% c("continuous", "continuous2") &&
    is.null(group) && is.data.frame(data)) {
    return("ancova")
  }
  if (summary_type %in% "dichotomous" && is.null(group) &&
    is.null(adj.vars) && is.data.frame(data)) {
    return("prop.test")
  }
  if (summary_type %in% "categorical" && is.null(group) &&
    is.null(adj.vars) && is.data.frame(data)) {
    return("smd")
  }

  if (summary_type %in% c("continuous", "continuous2") &&
    !is.null(group) && is.data.frame(data)) {
    return("ancova_lme4")
  }

  if (summary_type %in% c("continuous", "continuous2", "dichotomous") &&
    is.null(group) && is_survey(data)) {
    return("emmeans")
  }

  if (summary_type %in% c("categorical") &&
    is.null(adj.vars) && is.null(group) && is_survey(data)) {
    return("smd")
  }

  glue(
    "There is no default test for variable '{variable}'. Please specify method in `test=` ",
    "or exclude it with `include = -c({variable})`"
  ) %>%
    stringr::str_wrap() %>%
    stop(call. = FALSE)
}


.assign_test_tbl_continuous <- function(data, continuous_variable,
                                        variable, by, group, test) {
  # if user supplied a test, use that test -------------------------------------
  if (!is.null(test[[variable]])) {
    return(test[[variable]])
  }

  # if not by variable, can calculate the test the same way as in `add_p.tbl_summary`
  if (is.null(by)) {
    return(
      .assign_test_tbl_summary(
        data = data, variable = continuous_variable, summary_type = "continuous",
        by = variable, group = group, test = test
      )
    )
  }

  # no default test for correlated data
  if (!is.null(group)) {
    stop("There is no default test for correlated data when `by=` is specified.", call. = FALSE)
  }

  # otherwise, use 2-way ANOVA
  return("anova_2way")
}
