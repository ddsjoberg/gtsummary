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
#'
#' @noRd
#' @author Daniel D. Sjoberg
.get_add_p_test_fun <- function(class, test, env = NULL) {
  # if no test, then return NULL
  if (is.null(test)) return(NULL)

  # keep class of tests
  df <-
    df_add_p_tests %>%
    filter(.data$class %in% .env$class)

  # if test is character, then subset based on test name
  if (rlang::is_string(test)) { # character test name ------------------------------
    df <-
      df %>%
      filter(.data$test_name %in% .env$test)
  }
  else if (rlang::is_function(test)) { # test function passed -------------------------
    df <-
      df %>%
      # now select test object equivalent to the passed function
      filter(map_lgl(.data$test_fun, ~identical(eval(.x), test)))
  }

  # return info from df if internal test selected
  if (nrow(df) == 1)
    return(
      df %>%
        select(any_of(c("test_name", "fun_to_run", "accept_dots"))) %>%
        mutate_at(vars(.data$fun_to_run), ~map(.x, eval)) %>%
        as.list() %>%
        purrr::flatten()
    )
  if (rlang::is_string(test) && nrow(df) == 0)
    return(
      list(test_name = "user-defined",
           fun_to_run = rlang::parse_expr(test) %>% rlang::eval_tidy(env = env),
           accept_dots = FALSE)
    )
  if (rlang::is_function(test) && nrow(df) == 0)
    return(
      list(test_name = "user-defined",
           fun_to_run = test,
           accept_dots = FALSE)
    )
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
                                type = NULL, test.args = NULL, conf.level = 0.95) {
  # if x is NULL, return NULL
  if (is.null(x)) return(NULL)

  # calculating test function
  test_fun_result <-
    tryCatch(
      withCallingHandlers(
        {
          # calculating p-value
          do.call(x$fun_to_run, list(data = data, variable = variable, by = by,
                                     group = group, type = type, test.args = test.args,
                                     conf.level = conf.level, x = x))
        },
        # printing warning and errors as message
        warning = function(w) {
          w <- as.character(w)
          # hidden warnings from survey::svychisq() should not be returned
          w <- stringr::str_subset(w, "chisq.test\\(svytable\\(", negate = TRUE)
          if (length(w) > 0) {
            message(glue(
              "Warning in 'add_p()' for variable '{variable}':\n ", w
            ))
          }
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        message(glue(
          "There was an error in 'add_p()' for variable '{variable}', ",
          "p-value omitted:\n", as.character(e)
        ))
        return(NULL)
      }
    )

  # saving test function results into list 'x'
  if (is.data.frame(test_fun_result))
    x$df_result <- test_fun_result
  # these list inputs were deprecated and deleted from documentation in v1.3.6
  else if (is.list(test_fun_result) &&
           identical(names(test_fun_result), c("p", "test")))
    x$df_result <-
    tibble::as_tibble(test_fun_result) %>%
    set_names(c("p.value", "method"))
  else if (is.list(test_fun_result) &&
           identical(names(test_fun_result), c("test", "p")))
    x$df_result <-
    tibble::as_tibble(test_fun_result) %>%
    set_names(c("method", "p.value"))
  else if (rlang::is_scalar_double(test_fun_result))
    x$df_result <- tibble(p.value = test_fun_result, method = NA_character_)
  else if (is.null(test_fun_result))
    x$df_result <- tibble(p.value = NA_real_, method = NA_character_)

  x$df_result <- x$df_result %>%
    select(any_of(c("estimate", "conf.low", "conf.high", "p.value", "statistics", "method")),
           everything())
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
.assign_test_tbl_summary <- function(data, variable, summary_type, by, group, test) {
  # if user supplied a test, use that test -------------------------------------
  if (!is.null(test[[variable]])) return(test[[variable]])

  # if all obs are missing, return NULL ----------------------------------------
  if (length(data[[variable]]) == sum(is.na(data[[variable]]))) return(NULL)

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
        getOption("gtsummary.add_p.test.continuous.group_by2", default = "lme4")
      return(test_func)
    }
    if (summary_type %in% c("categorical", "dichotomous")) {
      test_func <-
        get_theme_element("add_p.tbl_summary-attr:test.categorical.group_by2") %||%
        getOption("gtsummary.add_p.test.categorical.group_by2", default = "lme4")
      return(test_func)
    }
  }

  # for continuous data, default to non-parametric tests
  if (summary_type %in% c("continuous", "continuous2") & length(unique(data[[by]])) == 2) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.continuous_by2") %||%
      getOption("gtsummary.add_p.test.continuous_by2", default = "wilcox.test")
    return(test_func)
  }
  if (summary_type %in% c("continuous", "continuous2")) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.continuous") %||%
      getOption("gtsummary.add_p.test.continuous", default = "kruskal.test")
    return(test_func)
  }

  # calculate expected counts to select between chisq and fisher
  min_exp <-
    expand.grid(
      table(data[[variable]]) / sum(!is.na(data[[variable]])),
      table(data[[by]]) / sum(!is.na(data[[by]]))
    ) %>%
    mutate(
      exp = .data$Var1 * .data$Var2 *
        sum(!is.na(data[[variable]]) & !is.na(data[[by]]))
    ) %>%
    pull(exp) %>%
    min()

  # if expected counts >= 5 for all cells, chisq, otherwise Fishers exact
  if (min_exp >= 5) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.categorical") %||%
      getOption("gtsummary.add_p.test.categorical", default = "chisq.test")
    return(test_func)
  }
  test_func <-
    get_theme_element("add_p.tbl_summary-attr:test.categorical.low_count") %||%
    getOption("gtsummary.add_p.test.categorical.low_count", default = "fisher.test")
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
.assign_test_tbl_svysummary <- function(data, variable, summary_type, by, test) {
  # if user supplied a test, use that test -------------------------------------
  if (!is.null(test[[variable]])) return(test[[variable]])

  # if all obs are missing, return NULL ----------------------------------------
  if (length(data$variables[[variable]]) == sum(is.na(data$variables[[variable]]))) return(NULL)

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

.assign_test_add_diff <- function(data, variable, summary_type, by, group, test) {
  # if user supplied a test, use that test -------------------------------------
  if (!is.null(test[[variable]])) return(test[[variable]])

  return("t.test")
}
