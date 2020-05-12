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

assign_test <- function(data, var, var_summary_type, by_var, test, group, env) {
  map2(
    var, var_summary_type,
    ~ assign_test_one(
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
test_match_to_fn <- function(x, env) {
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
  # if user specifed test to be performed, do that test. -----------------------
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
    if (var_summary_type == "continuous") {
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
  if (var_summary_type == "continuous" & length(unique(data[[by_var]])) == 2) {
    test_func <-
      get_theme_element("add_p.tbl_summary-attr:test.continuous_by2") %||%
      getOption("gtsummary.add_p.test.continuous_by2", default = "wilcox.test") %>%
      test_match_to_fn()
    return(test_func)
  }
  if (var_summary_type == "continuous") {
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
  result$test <- "t-test"
  result
}

add_p_test_aov <- function(data, variable, by, ...) {
  result <- list()
  result$p <- broom::glance(stats::lm(data[[variable]] ~ as.factor(data[[by]])))$p.value
  result$test <- "One-way ANOVA"
  result
}

add_p_test_kruskal.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- stats::kruskal.test(data[[variable]], as.factor(data[[by]]))$p.value
  result$test <- "Kruskal-Wallis test"
  result
}

add_p_test_wilcox.test <- function(data, variable, by, ...) {
  result <- list()
  if (length(unique(data[[by]])) > 2) {
    stop("Wilcoxon rank-sum test cannot be calculated with more than 2 groups")
  }
  result$p <- stats::wilcox.test(data[[variable]],
                                 as.numeric(as.factor(data[[by]])))$p.value
  result$test <- "Wilcoxon rank-sum test"
  result
}

add_p_test_chisq.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- stats::chisq.test(data[[variable]], as.factor(data[[by]]))$p.value
  result$test <- "chi-square test of independence"
  result
}

add_p_test_chisq.test.no.correct <- function(data, variable, by, ...) {
  result <- list()
  result$p <- stats::chisq.test(data[[variable]], as.factor(data[[by]]), correct = FALSE)$p.value
  result$test <- "chi-square test of independence"
  result
}

add_p_test_fisher.test <- function(data, variable, by, ...) {
  result <- list()
  result$p <- stats::fisher.test(data[[variable]], as.factor(data[[by]]))$p.value
  result$test <- "Fisher's exact test"
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
  if (type == "continuous") {
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
  result$test <- "random intercept logistic regression"
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
  data <- stats::na.omit(data[c(variable, by, group)])

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
        message(glue(
          "Warning in 'add_p()' for variable '{variable}':\n ", as.character(w)
        ))
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

