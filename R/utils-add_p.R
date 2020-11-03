# creating a tibble of all possible tests to select from all `add_p.*()` functions
df_add_p_tests <- tibble::tribble(
  ~class,           ~test_package, ~test_name,              ~test_fun,                 ~fun_to_run,                       ~accept_dots, ~description,
  "tbl_summary",    "stats",       "t.test",                expr(stats::t.test),       add_p_test_t.test,                  TRUE,         "t-test",
  "tbl_summary",    "stats",       "aov",                   expr(stats::aov),          add_p_test_aov,                     FALSE,        "One-way ANOVA",
  "tbl_summary",    "stats",       "kruskal.test",          expr(stats::kruskal.test), add_p_test_kruskal.test,            FALSE,        "Kruskal-Wallis test",
  "tbl_summary",    "stats",       "wilcox.test",           expr(stats::wilcox.test),  add_p_test_wilcox.test,             TRUE,         "Wilcoxon rank-sum test",
  "tbl_summary",    "stats",       "chisq.test",            expr(stats::chisq.test),   add_p_test_chisq.test,              TRUE,         "chi-square test of independence",
  "tbl_summary",    "stats",       "chisq.test.no.correct", NULL,                      add_p_test_chisq.test.no.correct,   FALSE,        "chi-square test of independence",
  "tbl_summary",    "stats",       "fisher.test",           expr(stats::fisher.test),  add_p_test_fisher.test,             TRUE,         "Fisher's exact test",
  "tbl_summary",    "lme4",        "lme4",                  expr(lme4::glmer),         add_p_test_lme4,                    FALSE,        "random intercept logistic regression"
)



#' Convert test arg input to function
#'
#' This function accepts the RHS of the test argument formula, and
#' returns a list that includes the function that will run to calculate
#' the p-value. List also includes
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
      # first subset on packages that are installed
      filter(map_lgl(.data$test_package, requireNamespace)) %>%
      # now select test object equivalent to the passed function
      filter(map_lgl(.data$test_fun, ~identical(eval(.x), test)))
  }

  # return info from df if internal test selected
  if (nrow(df) == 1)
    return(
      df %>%
        select(any_of("test_name", "fun_to_run", "accept_dots", "description")) %>%
        as.list()
    )
  if (rlang::is_string(test) && nrow(df) == 0)
    return(
      list(test_name = "user-defined",
           fun_to_run = rlang::parse_expr(x) %>% rlang::eval_tidy(env = env),
           accept_dots = TRUE)
    )
  if (rlang::is_function(test) && nrow(df) == 0)
    return(
      list(test_name = "user-defined",
           fun_to_run = test,
           accept_dots = TRUE)
    )
  abort("Something went wrong in the test selection....")
}


.run_add_p_test_fun <- function(x, data, by, variable, group = NULL, type = NULL) {

}



tibble::tribble(
  ~fun_chr, ~fun_base_r, ~fun_add_p,
  "t.test", stats::t.test, add_p_test_t.test,
  "aov", stats::aov, add_p_test_aov,
  "kruskal.test", stats::kruskal.test, add_p_test_kruskal.test,
  "wilcox.test", stats::wilcox.test, add_p_test_wilcox.test,
  "chisq.test", stats::chisq.test, add_p_test_chisq.test,
  "chisq.test.no.correct", NULL, add_p_test_chisq.test.no.correct,
  "fisher.test", stats::fisher.test, add_p_test_fisher.test,
  "lme4", NULL, add_p_test_lme4
)

#' determine the appropriate test type given two variables
#'
#' @param data input data set
#' @param var categorical or continuous variable for which a test with \code{by_var} is desired
#' @param var_summary_type summary_type from meta data
#' @param by_var categorical variable
#' @param test list of user defined statistical tests and corresponding variables
#' @param assign_test_one function for assigning test to one specific variable
#' @return most appropriate test as text of the test function
#' @noRd
#' @keywords internal
#' @author Daniel D. Sjoberg

assign_test <- function(data, var, var_summary_type, by_var, test, group, env, assign_test_one_fun = assign_test_one) {
  map2(
    var, var_summary_type,
    ~ assign_test_one_fun(
      data = data,
      var = .x,
      var_summary_type = .y,
      by_var = by_var,
      test = test,
      group = group,
      env = env
    )
  )
}

# test_match_to_fn -------------------------------------------------------------
# little function to return the function from the shortcut name,
# this is a helper function for assign_test_one()
test_match_to_fn <- function(x, env, survey = FALSE) {
  if (is.null(x)) return(NULL)

  # lookup table
  df_lookup <-
    tibble::tribble(
      ~fun_chr, ~fun_base_r, ~fun_add_p,
      "t.test", stats::t.test, add_p_test_t.test,
      "aov", stats::aov, add_p_test_aov,
      "kruskal.test", stats::kruskal.test, add_p_test_kruskal.test,
      "wilcox.test", stats::wilcox.test, add_p_test_wilcox.test,
      "chisq.test", stats::chisq.test, add_p_test_chisq.test,
      "chisq.test.no.correct", NULL, add_p_test_chisq.test.no.correct,
      "fisher.test", stats::fisher.test, add_p_test_fisher.test,
      "lme4", NULL, add_p_test_lme4
    )
  if (requireNamespace("survey", quietly = TRUE) & survey) {
    if (rlang::is_string(x) && x %in% df_lookup$fun_chr) {
      stop(paste0(
        "\"", x, "\" is not a test adapted to survey objects.\n",
        "See ?add_p.tbl_svysummary for a list of available statistical tests."
      ), call. = FALSE)
    }

    df_lookup <- tibble::tribble(
      ~fun_chr, ~fun_base_r, ~fun_add_p,
      "svy.t.test", survey::svyttest, add_p_test_svy.t.test,
      "svy.wilcox.test", survey::svyranktest, add_p_test_svy.wilcox.test,
      "svy.kruskal.test", NULL, add_p_test_svy.kruskal.test,
      "svy.vanderwaerden.test", NULL, add_p_test_svy.vanderwaerden.test,
      "svy.median.test", NULL, add_p_test_svy.median.test,
      "svy.chisq.test", survey::svychisq, add_p_test_svy.chisq.test,
      "svy.adj.chisq.test", NULL, add_p_test_svy.adj.chisq.test,
      "svy.wald.test", NULL, add_p_test_svy.wald.test,
      "svy.adj.wald.test", NULL, add_p_test_svy.adj.wald.test,
      "svy.lincom.test", NULL, add_p_test_svy.lincom.test,
      "svy.saddlepoint.test", NULL, add_p_test_svy.saddlepoint.test
    )
  }

  # if string was passed, match `fun_chr`
  if (rlang::is_string(x) && x %in% df_lookup$fun_chr) {
      return(df_lookup$fun_add_p[df_lookup$fun_chr %in% x] %>% pluck(1))
  }

  # if fn was passed, match `fun_base_r`
  if (rlang::is_function(x)) {
    lgl <- map_lgl(df_lookup$fun_base_r, function(fn) identical(fn, x))
    if (any(lgl))
      return(df_lookup$fun_add_p[lgl] %>% pluck(1))
  }

  # if the input is a function, return it
  if (rlang::is_function(x)) return(x)

  # if the user passed a string, return the function that is the string
  if (rlang::is_string(x))
    return(rlang::parse_expr(x) %>% rlang::eval_tidy(env = env))

  # if not a match, return NULL
  return(NULL)
}

# assign_test_one() ------------------------------------------------------------
assign_test_one <- function(data, var, var_summary_type, by_var, test, group, env) {
  # if user specified test to be performed, do that test. -----------------------
  # matching an internal test by name or base R function match
  # or returning the appropriate test based on what is passed
  test_func <- test_match_to_fn(test[[var]], env = env)
  if (!is.null(test_func)) return(test_func)

  # if no test supplied, setting defaults --------------------------------------
  # if by var hs 3 or more levels, return error...no default test.
  if (!is.null(group) && length(unique(data[[by_var]])) > 2) {
    stop("There is no default test for correlated data when `by=` variable has >2 levels.", call. = FALSE)
  }

  # if group variable supplied, fit a random effects model
  if (!is.null(group) & length(unique(data[[by_var]])) == 2) {
    if (var_summary_type %in% c("continuous", "continuous2")) {
      test_func <-
        get_theme_element("add_p.tbl_summary-attr:test.continuous.group_by2") %||%
        getOption("gtsummary.add_p.test.continuous.group_by2", default = "lme4") %>%
        test_match_to_fn()
      return(test_func)
    }
    if (var_summary_type %in% c("categorical", "dichotomous")) {
      test_func <-
        get_theme_element("add_p.tbl_summary-attr:test.categorical.group_by2") %||%
        getOption("gtsummary.add_p.test.categorical.group_by2", default = "lme4") %>%
        test_match_to_fn()
      return(test_func)
    }
  }

  # for continuous data, default to non-parametric tests
  if (var_summary_type %in% c("continuous", "continuous2") & length(unique(data[[by_var]])) == 2) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.continuous_by2") %||%
      getOption("gtsummary.add_p.test.continuous_by2", default = "wilcox.test") %>%
      test_match_to_fn()
    return(test_func)
  }
  if (var_summary_type %in% c("continuous", "continuous2")) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.continuous") %||%
      getOption("gtsummary.add_p.test.continuous", default = "kruskal.test") %>%
      test_match_to_fn()
    return(test_func)
  }

  # if all obs are missing, assigning chisq
  if (length(data[[var]]) == sum(is.na(data[[var]]))) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.categorical") %||%
      getOption("gtsummary.add_p.test.categorical", default = "chisq.test") %>%
      test_match_to_fn()
    return(test_func)
  }

  # calculate expected counts
  min_exp <-
    expand.grid(
      table(data[[var]]) / sum(!is.na(data[[var]])),
      table(data[[by_var]]) / sum(!is.na(data[[by_var]]))
    ) %>%
    mutate(
      exp = .data$Var1 * .data$Var2 *
        sum(!is.na(data[[var]]) & !is.na(data[[by_var]]))
    ) %>%
    pull(exp) %>%
    min()

  # if expected counts >= 5 for all cells, chisq, otherwise Fishers exact
  if (min_exp >= 5) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.categorical") %||%
      getOption("gtsummary.add_p.test.categorical", default = "chisq.test") %>%
      test_match_to_fn()
    return(test_func)
  }
  test_func <-
    get_theme_element("add_p.tbl_summary-attr:test.categorical.low_count") %||%
    getOption("gtsummary.add_p.test.categorical.low_count", default = "fisher.test") %>%
    test_match_to_fn()
  return(test_func)
}


# Tests used in add_p

add_p_test_t.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- stats::t.test(data[[variable]] ~ as.factor(data[[by]]))$p.value
  result$test <- translate_text("t-test")
  result
}

add_p_test_aov <- function(data, variable, by, ...) {
  result <- list()
  result$p <- broom::glance(stats::lm(data[[variable]] ~ as.factor(data[[by]])))$p.value
  result$test <- translate_text("One-way ANOVA")
  result
}

add_p_test_kruskal.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- stats::kruskal.test(data[[variable]], as.factor(data[[by]]))$p.value
  result$test <- translate_text("Kruskal-Wallis test")
  result
}

add_p_test_wilcox.test <- function(data, variable, by, ...) {
  result <- list()
  if (length(unique(data[[by]])) > 2) {
    stop("Wilcoxon rank-sum test cannot be calculated with more than 2 groups")
  }
  result$p <- stats::wilcox.test(data[[variable]] ~ as.factor(data[[by]]))$p.value
  result$test <- translate_text("Wilcoxon rank-sum test")

  result
}

add_p_test_chisq.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- stats::chisq.test(data[[variable]], as.factor(data[[by]]))$p.value
  result$test <- translate_text("chi-square test of independence")
  result
}

add_p_test_chisq.test.no.correct <- function(data, variable, by, ...) {
  result <- list()
  result$p <- stats::chisq.test(data[[variable]], as.factor(data[[by]]), correct = FALSE)$p.value
  result$test <- translate_text("chi-square test of independence")
  result
}

add_p_test_fisher.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- stats::fisher.test(data[[variable]], as.factor(data[[by]]))$p.value
  result$test <- translate_text("Fisher's exact test")
  result
}

add_p_test_lme4 <- function(data, variable, by, group, type, ...) {
  result <- list()
  # input checks for lme4 tests
  if (data[[by]] %>% unique() %>% length() != 2) {
    # only allowing logistic regression models for now
    stop("'by' variable must have two and only two levels.")
  }
  if (is.null(group)) {
    # lme4 requires a group to account for the correlated data
    stop("'group' argument cannot be NULL for 'test = \"lme4\"'")
  }

  # creating formulas for base model (without variable) and full model
  formula0 <- glue("as.factor({by}) ~ 1 + (1 | {group})")
  if (type %in% c("continuous", "continuous2")) {
    formula1 <- glue("as.factor({by}) ~ {variable} + (1 | {group})")
  } else {
    formula1 <- glue("as.factor({by}) ~ as.factor({variable}) + (1 | {group})")
  }

  # building base and full models
  mod0 <- lme4::glmer(stats::as.formula(formula0),
    data = data, family = stats::binomial
  )
  mod1 <- lme4::glmer(stats::as.formula(formula1),
    data = data, family = stats::binomial
  )

  # returning p-value
  result$p <- stats::anova(mod0, mod1)$"Pr(>Chisq)"[2]
  result$test <- translate_text("random intercept logistic regression")
  result
}


# this is a safe wrapper for add_p_test().  It does some brief prepping before
# passing the arguments along to add_p_test()
add_p_test_safe <- function(data, variable, by, group, test, include = NULL, type) {
  # omitting variables not in include
  if (!variable %in% include) {
    return(NULL)
  }

  # keeping non-missing values
  # note: this syntax is compatible with survey objects
  data <- stats::na.omit(data[, c(variable, by, group)])

  # calculating pvalue
  tryCatch(
    withCallingHandlers(
      {
        # initializing to NA
        pval <- NA_real_
        pval <- do.call(test, list(
          data = data, variable = variable,
          by = by, group = group, test = test,
          type = type
        ))
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

  pval
}

# vectorized version of the functions that calculate a single pvalue
calculate_pvalue <- function(data, variable, by, test, type, group, include) {
  pmap(
    list(variable, by, test, type),
    ~ add_p_test_safe(
      data = data, variable = ..1, by = ..2,
      group = group, test = ..3, include = include, type = ..4
    )
  )
}


# assign_test_one() ------------------------------------------------------------
assign_test_one_survey <- function(data, var, var_summary_type, by_var, test, group, env) {
  # if user specified test to be performed, do that test. -----------------------
  # matching an internal test by name or base R function match
  # or returning the appropriate test based on what is passed
  test_func <- test_match_to_fn(test[[var]], env = env, survey = TRUE)
  if (!is.null(test_func)) return(test_func)


  # for continuous data, default to non-parametric tests
  if (var_summary_type %in% c("continuous", "continuous2")) {
    test_func <-
      get_theme_element("add_p.tbl_svysummary-attr:test.continuous", default = "svy.wilcox.test") %>%
      test_match_to_fn(survey = TRUE)
    return(test_func)
  }

  # for categorical data, default to chi-squared with Rao & Scott correction
  if (var_summary_type %in% c("categorical", "dichotomous")) {
    test_func <-
      get_theme_element("add_p.tbl_svysummary-attr:test.categorical", default = "svy.chisq.test") %>%
      test_match_to_fn(survey = TRUE)
    return(test_func)
  }
}

# tests for tbl_svysummary ------------------------------------------------------------

add_p_test_svy.chisq.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- survey::svychisq(c_form(right = c(variable, by)), data, statistic = "F")$p.value
  result$test <- translate_text("chi-squared test with Rao & Scott's second-order correction")
  result
}

add_p_test_svy.adj.chisq.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- survey::svychisq(c_form(right = c(variable, by)), data, statistic = "Chisq")$p.value
  result$test <- translate_text("chi-squared test adjusted by a design effect estimate")
  result
}

add_p_test_svy.wald.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- survey::svychisq(c_form(right = c(variable, by)), data, statistic = "Wald")$p.value
  result$test <- translate_text("Wald test of independence for complex survey samples")
  result
}

add_p_test_svy.adj.wald.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- survey::svychisq(c_form(right = c(variable, by)), data, statistic = "adjWald")$p.value
  result$test <- translate_text("adjusted Wald test of independence for complex survey samples")
  result
}

add_p_test_svy.lincom.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- survey::svychisq(c_form(right = c(variable, by)), data, statistic = "lincom")$p.value
  result$test <- translate_text("test of independence using the exact asymptotic distribution for complex survey samples")
  result
}

add_p_test_svy.saddlepoint.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- survey::svychisq(c_form(right = c(variable, by)), data, statistic = "saddlepoint")$p.value
  result$test <- translate_text("test of independence using a saddlepoint approximation for complex survey samples")
  result
}

add_p_test_svy.t.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- survey::svyttest(c_form(variable, by), data)$p.value
  result$test <- translate_text("t-test adapted to complex survey samples")
  result
}

add_p_test_svy.wilcox.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- survey::svyranktest(c_form(variable, by), data, test = "wilcoxon")$p.value
  result$test <- translate_text("Wilcoxon rank-sum test for complex survey samples")
  result
}

add_p_test_svy.kruskal.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- survey::svyranktest(c_form(variable, by), data, test = "KruskalWallis")$p.value
  result$test <- translate_text("Kruskal-Wallis rank-sum test for complex survey samples")
  result
}

add_p_test_svy.vanderwaerden.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- survey::svyranktest(c_form(variable, by), data, test = "vanderWaerden")$p.value
  result$test <- translate_text("van der Waerden's normal-scores test for complex survey samples")
  result
}

add_p_test_svy.median.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- survey::svyranktest(c_form(variable, by), data, test = "median")$p.value
  result$test <- translate_text("Mood's test for the median for complex survey samples")
  result
}
