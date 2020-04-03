#' Display regression model results in table
#'
#' This function takes a regression model object and returns a formatted table
#' that is publication-ready. The function is highly customizable
#' allowing the user to obtain a bespoke summary table of the
#' regression model results. Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html}{tbl_regression vignette}
#' for detailed examples.
#'
#' @section Setting Defaults:
#' If you prefer to consistently use a different function to format p-values or
#' estimates, you can set options in the script or in the user- or
#' project-level startup file, '.Rprofile'.  The default confidence level can
#' also be set.
#' \itemize{
#'   \item `options(gtsummary.pvalue_fun = new_function)`
#'   \item `options(gtsummary.tbl_regression.estimate_fun = new_function)`
#'   \item `options(gtsummary.conf.level = 0.90)`
#' }
#'
#' @section Note:
#' The N reported in the output is the number of observations
#' in the data frame `model.frame(x)`. Depending on the model input, this N
#' may represent different quantities. In most cases, it is the number of people or
#' units in your model.  Here are some common exceptions.
#' 1. Survival regression models including time dependent covariates.
#' 2. Random- or mixed-effects regression models with clustered data.
#' 3. GEE regression models with clustered data.
#'
#' This list is not exhaustive, and care should be taken for each number reported.
#' @param x Regression model object
#' @param exponentiate Logical indicating whether to exponentiate the
#' coefficient estimates. Default is `FALSE`.
#' @param label List of formulas specifying variables labels,
#' e.g. `list(age ~ "Age, yrs", stage ~ "Path T Stage")`
#' @param include Variables to include in output. Input may be a vector of
#' quoted variable names, unquoted variable names, or tidyselect select helper
#' functions. Default is `everything()`.
#' @param conf.level Must be strictly greater than 0 and less than 1.
#' Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param intercept Logical argument indicating whether to include the intercept
#' in the output.  Default is `FALSE`
#' @param show_single_row By default categorical variables are printed on
#' multiple rows.  If a variable is dichotomous (e.g. Yes/No) and you wish to print
#' the regression coefficient on a single row, include the variable name(s)
#' here--quoted and unquoted variable name accepted.
#' @param estimate_fun Function to round and format coefficient estimates.
#' Default is [style_sigfig] when the coefficients are not transformed, and
#' [style_ratio] when the coefficients have been exponentiated.
#' @param pvalue_fun Function to round and format p-values.
#' Default is [style_pvalue].
#' The function must have a numeric vector input (the numeric, exact p-value),
#' and return a string that is the rounded/formatted p-value (e.g.
#' `pvalue_fun = function(x) style_pvalue(x, digits = 2)` or equivalently,
#'  `purrr::partial(style_pvalue, digits = 2)`).
#' @param tidy_fun Option to specify a particular tidier function if the
#' model is not a [vetted model][vetted_models] or you need to implement a
#' custom method. Default is `NULL`
#' @param exclude DEPRECATED
#' @param show_yesno DEPRECATED
#' @author Daniel D. Sjoberg
#' @seealso See tbl_regression \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html}{vignette} for detailed examples
#' @family tbl_regression tools
#' @export
#' @return A `tbl_regression` object
#' @examples
#' library(survival)
#' tbl_regression_ex1 <-
#'   coxph(Surv(ttdeath, death) ~ age + marker, trial) %>%
#'   tbl_regression(exponentiate = TRUE)
#'
#' tbl_regression_ex2 <-
#'   glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE)
#'
#' library(lme4)
#' tbl_regression_ex3 <-
#'   glmer(am ~ hp + (1 | gear), mtcars, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE)
#'
#' # for convenience, you can also pass named lists to any arguments
#' # that accept formulas (e.g label, etc.)
#' glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE, label = list(age = "Patient Age"))
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_regression_ex1.png}{options: width=64\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_regression_ex2.png}{options: width=50\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{tbl_regression_ex3.png}{options: width=50\%}}

tbl_regression <- function(x, label = NULL, exponentiate = FALSE,
                           include = everything(), show_single_row = NULL,
                           conf.level = NULL, intercept = FALSE,
                           estimate_fun = NULL, pvalue_fun = NULL,
                           tidy_fun = NULL,
                           show_yesno = NULL, exclude = NULL) {
  # deprecated arguments -------------------------------------------------------
  if (!is.null(show_yesno)) {
    lifecycle::deprecate_stop(
      "1.2.2", "tbl_regression(show_yesno = )",
      "tbl_regression(show_single_row = )"
    )
  }

  if (!rlang::quo_is_null(rlang::enquo(exclude))) {
    lifecycle::deprecate_warn(
      "1.2.5",
      "gtsummary::tbl_regression(exclude = )",
      "tbl_regression(include = )",
      details = paste0(
        "The `include` argument accepts quoted and unquoted expressions similar\n",
        "to `dplyr::select()`. To exclude variable, use the minus sign.\n",
        "For example, `include = -c(age, stage)`"
      )
    )
  }

  # setting defaults -----------------------------------------------------------
  pvalue_fun <-
    pvalue_fun %||%
    getOption("gtsummary.pvalue_fun", default = style_pvalue)
  estimate_fun <-
    estimate_fun %||%
    getOption(
      "gtsummary.tbl_regression.estimate_fun",
      default = ifelse(exponentiate == TRUE, style_ratio, style_sigfig)
    )
  conf.level <-
    conf.level %||%
    getOption("gtsummary.conf.level", default = 0.95)

  # checking estimate_fun and pvalue_fun are functions
  if (!purrr::every(list(estimate_fun, pvalue_fun, tidy_fun %||% pvalue_fun), is.function)) {
    stop("Inputs `estimate_fun`, `pvalue_fun`, `tidy_fun` must be functions.",
         call. = FALSE)
  }

  include <- rlang::enquo(include)
  exclude <- rlang::enquo(exclude)
  show_single_row <- rlang::enquo(show_single_row)

  # will return call, and all object passed to in tbl_regression call
  # the object func_inputs is a list of every object passed to the function
  func_inputs <- as.list(environment())

    # converting tidyselect formula lists to named lists
  # extracting model frame
  model_frame <- tryCatch({
      stats::model.frame(x)
    },
    error = function(e) {
      usethis::ui_oops(paste0(
        "There was an error calling {usethis::ui_code('stats::model.frame(x)')}.\n\n",
        "Most likely, this is because the argument passed in {usethis::ui_code('x =')} ",
        "was\nmisspelled, does not exist, or is not a regression model.\n\n",
        "Rarely, this error may occur if the model object was created within\na ",
        "functional programming framework (e.g. using {usethis::ui_code('lappy()')}, ",
        "{usethis::ui_code('purrr::map()')}, etc.).\n",
        "Review the GitHub issue linked below for a possible solution."
      ))
      usethis::ui_code_block("https://github.com/ddsjoberg/gtsummary/issues/231")
      stop(as.character(e), call. = FALSE)
    }
  )

  # using broom to tidy up regression results, and
  # then reversing order of data frame
  tidy_model <-
    tidy_wrap(x, exponentiate, conf.level, tidy_fun)

  # parsing the terms from model and variable names
  # outputing a tibble of the parsed model with
  # rows for reference groups, and headers for
  table_body <- parse_fit(x, tidy_model, label, !!show_single_row)

  # saving evaluated `label`, and `show_single_row`
  func_inputs$label <- attr(table_body, "label")
  func_inputs$show_single_row <- attr(table_body, "show_single_row")

  # adding character CI
  if (all(c("conf.low", "conf.high") %in% names(table_body))) {
    table_body <-
      table_body %>%
      # adding character CI
      mutate(
        ci = if_else(
          !is.na(.data$conf.low),
          paste0(estimate_fun(.data$conf.low), ", ", estimate_fun(.data$conf.high)),
          NA_character_
        )
      )
  }

  # moving pvalue col to end of df
  if ("p.value" %in% names(table_body)) {
    table_body <- select(table_body, -.data$p.value, .data$p.value)
  }

  # including and excluding variables/intercept indicated
  include <- var_input_to_string(data = vctr_2_tibble(unique(table_body$variable)),
                                 arg_name = "include", select_input = !!include)
  exclude <- var_input_to_string(data = vctr_2_tibble(unique(table_body$variable)),
                                 arg_name = "exclude", select_input = !!exclude)

  if (intercept == FALSE) include <- include %>% setdiff("(Intercept)")
  include <- include %>% setdiff(exclude)

  # keeping variables indicated in `include`
  table_body <-
    table_body %>%
    filter(.data$variable %in% include)

  # model N
  n <- nrow(model_frame)

  # table of column headers
  table_header <-
    tibble(column = names(table_body)) %>%
    table_header_fill_missing() %>%
    table_header_fmt_fun(estimate = estimate_fun)

  if ("p.value" %in% names(table_body)) {
    table_header <- table_header_fmt_fun(table_header, p.value = pvalue_fun)
  }
  if (all(c("conf.low", "conf.high") %in% names(table_body))) {
    table_header <- table_header_fmt_fun(
      table_header,
      conf.low = estimate_fun,
      conf.high = estimate_fun
    )
  }

  table_header <- table_header %>%
    # adding footnotes to table_header tibble
    mutate(
      footnote_abbrev = case_when(
        .data$column == "estimate" ~
          estimate_header(x, exponentiate) %>% attr("footnote") %||% NA_character_,
        .data$column == "ci" ~ "CI = Confidence Interval",
        TRUE ~ .data$footnote_abbrev
      ),
      missing_emdash = case_when(
        .data$column %in% c("estimate", "ci") ~ "row_ref == TRUE",
        TRUE ~ .data$missing_emdash
      )
    )

  # saving the evaluated lists (named lists) as the function inputs
  func_inputs$include <- include
  func_inputs$exclude <- NULL # making this NULL since it's deprecated

  results <- list(
    table_body = table_body,
    table_header = table_header,
    n = n,
    model_obj = x,
    inputs = func_inputs,
    call_list = list(tbl_regression = match.call())
  )

  # setting column headers
  results <- modify_header_internal(
    results,
    label = "**Characteristic**",
    estimate = glue("**{estimate_header(x, exponentiate)}**")
  )
  if ("p.value" %in% names(table_body)) {
    results <- modify_header_internal(
      results, p.value = "**p-value**"
    )
  }
  if (all(c("conf.low", "conf.high") %in% names(table_body))) {
    results <- modify_header_internal(
      results, ci = glue("**{style_percent(conf.level, symbol = TRUE)} CI**")
    )
  }

  # assigning a class of tbl_regression (for special printing in R markdown)
  class(results) <- c("tbl_regression", "gtsummary")

  results
}

# identifies headers for common models (logistic, poisson, and PH regression)
estimate_header <- function(x, exponentiate) {
  # first identify the type ----------------------------------------------------
  model_type <- "generic"
  # GLM and GEE models
  if (inherits(x, c("glm", "geeglm")) &&
    x$family$family == "binomial" &&
    x$family$link == "logit") {
    model_type <- "logistic"
  } else if (inherits(x, "clogit")) {
    model_type <- "logistic"
  } else if (inherits(x, c("glm", "geeglm")) &&
    x$family$family == "poisson" &&
    x$family$link == "log") {
    model_type <- "poisson"
  } # Cox Models
  else if (inherits(x, "coxph")) {
    model_type <- "prop_hazard"
  } # LME4 models
  else if (inherits(x, "glmerMod") &&
    attr(class(x), "package") == "lme4" &&
    x@resp$family$family == "binomial" &&
    x@resp$family$link == "logit") {
    model_type <- "logistic"
  } else if (inherits(x, "glmerMod") &&
    attr(class(x), "package") == "lme4" &&
    x@resp$family$family == "poisson" &&
    x@resp$family$link == "log") {
    model_type <- "poisson"
  }

  # assigning header and footer ------------------------------------------------
  if (model_type == "logistic") {
    header <- ifelse(exponentiate == TRUE, "OR", "log(OR)")
    attr(header, "footnote") <- "OR = Odds Ratio"
  }
  else if (model_type == "poisson") {
    header <- ifelse(exponentiate == TRUE, "IRR", "log(IRR)")
    attr(header, "footnote") <- "IRR = Incidence Rate Ratio"
  }
  else if (model_type == "prop_hazard") {
    header <- ifelse(exponentiate == TRUE, "HR", "log(HR)")
    attr(header, "footnote") <- "HR = Hazard Ratio"
  }
  else {
    header <- ifelse(exponentiate == TRUE, "exp(Beta)", "Beta")
  }

  header
}
