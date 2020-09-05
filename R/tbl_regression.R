#' Display regression model results in table
#'
#' This function takes a regression model object and returns a formatted table
#' that is publication-ready. The function is highly customizable
#' allowing the user to obtain a bespoke summary table of the
#' regression model results. Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html}{tbl_regression vignette}
#' for detailed examples.
#'
#' @section Methods:
#'
#' The default method for `tbl_regression()` model summary uses `broom::tidy(x)`
#' to perform the initial tidying of the model object. There are, however,
#' a few [vetted model][vetted_models] that use [modifications][tbl_regression_methods].
#'
#' - `"lmerMod"` or `"glmerMod"`: These mixed effects models use `broom.mixed::tidy(x, effects = "fixed")`
#' - `"survreg"`: The scale parameter is removed, `broom::tidy(x) %>% dplyr::filter(term != "Log(scale)")`
#' - `"multinom"`: This multinomial outcome is complex, and the returned object is a `tbl_stack()` object with the parameters for each outcome stacked into a final object
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
#' e.g. `list(age ~ "Age", stage ~ "Path T Stage")`
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
#' @param ... Not used
#' @param exclude DEPRECATED
#' @param show_yesno DEPRECATED
#' @author Daniel D. Sjoberg
#' @seealso See tbl_regression \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html}{vignette} for detailed examples
#' @family tbl_regression tools
#' @export
#' @rdname tbl_regression
#' @return A `tbl_regression` object
#' @examples
#' # Example 1 ----------------------------------
#' library(survival)
#' tbl_regression_ex1 <-
#'   coxph(Surv(ttdeath, death) ~ age + marker, trial) %>%
#'   tbl_regression(exponentiate = TRUE)
#'
#' # Example 2 ----------------------------------
#' tbl_regression_ex2 <-
#'   glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE)
#'
#' # Example 3 ----------------------------------
#' suppressMessages(library(lme4))
#' tbl_regression_ex3 <-
#'   glmer(am ~ hp + (1 | gear), mtcars, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE)
#'
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

tbl_regression <- function(x, ...) {
  UseMethod("tbl_regression")
}

#' @export
#' @rdname tbl_regression
tbl_regression.default <- function(x, label = NULL, exponentiate = FALSE,
                                   include = everything(), show_single_row = NULL,
                                   conf.level = NULL, intercept = FALSE,
                                   estimate_fun = NULL, pvalue_fun = NULL,
                                   tidy_fun = broom::tidy,
                                   show_yesno = NULL, exclude = NULL, ...) {
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
    get_theme_element("tbl_regression-arg:pvalue_fun") %||%
    get_theme_element("pkgwide-fn:pvalue_fun") %||%
    getOption("gtsummary.pvalue_fun", default = style_pvalue) %>%
    gts_mapper("tbl_regression(pvalue_fun=)")
  estimate_fun <-
    estimate_fun %||%
    get_theme_element("tbl_regression-arg:estimate_fun") %||%
    getOption(
      "gtsummary.tbl_regression.estimate_fun",
      default = ifelse(exponentiate == TRUE, style_ratio, style_sigfig)
    ) %>%
    gts_mapper("tbl_regression(estimate_fun=)")
  conf.level <-
    conf.level %||%
    get_theme_element("tbl_regression-arg:conf.level") %||%
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

  table_body <-
    tidy_prep(x, tidy_fun = tidy_fun, exponentiate = exponentiate,
              conf.level = conf.level, intercept = intercept,
              label = label, show_single_row = !!show_single_row)

  # saving evaluated `label`, and `show_single_row`
  func_inputs$label <-
    unique(table_body$variable) %>%
    vctr_2_tibble() %>%
    tidyselect_to_list(x = {{ label }}, arg_name = "label")
  func_inputs$show_single_row <-
    unique(table_body$variable) %>%
    vctr_2_tibble() %>%
    var_input_to_string(arg_name = "show_single_row", select_input = {{show_single_row}})

  # including and excluding variables indicated
  include <- var_input_to_string(data = vctr_2_tibble(unique(table_body$variable)),
                                 arg_name = "include", select_input = !!include)
  exclude <- var_input_to_string(data = vctr_2_tibble(unique(table_body$variable)),
                                 arg_name = "exclude", select_input = !!exclude)

  include <- include %>% setdiff(exclude)

  # saving the evaluated lists (named lists) as the function inputs
  func_inputs$include <- include
  func_inputs$exclude <- NULL # making this NULL since it's deprecated

  # keeping variables indicated in `include`
  table_body <- table_body %>% filter(.data$variable %in% include)

  # model N
  n <- table_body$N[1]

  # adding character CI
  if (all(c("conf.low", "conf.high") %in% names(table_body))) {
    ci.sep <- get_theme_element("pkgwide-str:ci.sep", default = ", ")
    table_body <-
      table_body %>%
      mutate( # adding character CI
        ci = if_else(
          !is.na(.data$conf.low),
          paste0(estimate_fun(.data$conf.low), ci.sep, estimate_fun(.data$conf.high)),
          NA_character_
        )
      ) %>%
      dplyr::relocate(any_of("ci"), .after = "conf.high")
  }

  # re-ordering columns
  table_body <-
    table_body %>%
    dplyr::relocate(any_of(c("conf.low", "conf.high", "ci", "p.value")), .after = last_col())

  # table of column headers
  table_header <-
    tibble(column = names(table_body)) %>%
    table_header_fill_missing() %>%
    table_header_fmt_fun(estimate = estimate_fun)

  # if ("N" %in% names(table_body)) {
  #   table_header <-
  #     table_header_fmt_fun(
  #       table_header,
  #       N = function(x) style_number(x, digits = 0)
  #     )
  # }
  # if ("p.value" %in% names(table_body)) {
  #   table_header <- table_header_fmt_fun(table_header, p.value = pvalue_fun)
  # }
  # if (all(c("conf.low", "conf.high") %in% names(table_body))) {
  #   table_header <- table_header_fmt_fun(
  #     table_header,
  #     conf.low = estimate_fun,
  #     conf.high = estimate_fun
  #   )
  # }

  # # default 'fmt_fun' for numeric columns is `style_sigfig`
  # col_needs_fmt_fun <-
  #   purrr::map2_lgl(table_header$column, table_header$fmt_fun,
  #                   ~is.numeric(table_body[[.x]]) & is.null(.y))
  # table_header$fmt_fun[col_needs_fmt_fun] <-
  #   list(purrr::partial(style_sigfig, digits = 3))


  # table_header <- table_header %>%
  #   # adding footnotes to table_header tibble
  #   mutate(
  #     footnote_abbrev = case_when(
  #       .data$column == "estimate" ~
  #         estimate_header(x, exponentiate) %>% attr("footnote") %||% NA_character_,
  #       .data$column == "ci" ~ translate_text("CI = Confidence Interval"),
  #       TRUE ~ .data$footnote_abbrev
  #     ),
  #     missing_emdash = case_when(
  #       .data$column %in% c("estimate", "ci", "std.error", "statistic") ~ "reference_row == TRUE",
  #       TRUE ~ .data$missing_emdash
  #     )
  #   )


  # constructing return object
  results <- list(
    table_body = table_body,
    table_header = table_header,
    n = n,
    model_obj = x,
    inputs = func_inputs,
    call_list = list(tbl_regression = match.call())
  )

  # assigning a class of tbl_regression (for special printing in R markdown)
  class(results) <- c("tbl_regression", "gtsummary")

  # setting column headers, and print instructions
  tidy_columns_to_report <-
    get_theme_element("tbl_regression-chr:tidy_columns",
                      default = c("conf.low", "conf.high", "p.value")) %>%
    union("estimate") %>%
    intersect(names(table_body))


  results <- results %>%
    modify_table_header(
      column = "label",
      label = paste0("**", translate_text("Characteristic"), "**"),
      hide = FALSE
    )


  if ("estimate" %in% names(results$table_body))
    results <- modify_table_header(
      results,
      column = "estimate",
      label = glue("**{estimate_header(x, exponentiate)}**") %>% as.character(),
      hide = !"estimate" %in% tidy_columns_to_report,
      missing_emdash = "reference_row == TRUE",
      footnote_abbrev =
        estimate_header(x, exponentiate) %>% attr("footnote") %||% NA_character_,
      fmt_fun = estimate_fun
    )

  if ("N" %in% names(results$table_body))
    results <- modify_table_header(
      results, column = "N", label = "**N**", fmt_fun = style_number
    )

  if (all(c("conf.low", "conf.high") %in% names(results$table_body))) {
    results <- modify_table_header(
      results,
      column = "ci",
      label = glue("**{style_percent(conf.level, symbol = TRUE)} CI**") %>% as.character(),
      hide = !all(c("conf.low", "conf.high") %in% tidy_columns_to_report),
      missing_emdash = "reference_row == TRUE",
      footnote_abbrev = translate_text("CI = Confidence Interval")
    )
    results <- modify_table_header(results,
                                   column = c("conf.low", "conf.high"),
                                   fmt_fun = estimate_fun)
  }

  if ("p.value" %in% names(results$table_body))
    results <- modify_table_header(
      results,
      column = "p.value",
      label = paste0("**", translate_text("p-value"), "**"),
      fmt_fun = pvalue_fun,
      hide = !"p.value" %in% tidy_columns_to_report
    )

  if ("std.error" %in% names(results$table_body))
    results <- modify_table_header(
      results,
      column = "std.error",
      label = paste0("**", translate_text("SE"), "**"),
      footnote_abbrev = translate_text("SE = Standard Error"),
      fmt_fun = purrr::partial(style_sigfig, digits = 3),
      hide = !"std.error" %in% tidy_columns_to_report
    )

  if ("statistic" %in% names(results$table_body))
    results <- modify_table_header(
      results,
      column = "statistic",
      label = paste0("**", translate_text("Statistic"), "**"),
      fmt_fun = purrr::partial(style_sigfig, digits = 3),
      hide = !"statistic" %in% tidy_columns_to_report
    )

  # # setting column headers
  # results <- modify_header_internal(
  #   results,
  #   label = paste0("**", translate_text("Characteristic"), "**"),
  #   estimate = glue("**{estimate_header(x, exponentiate)}**")
  # )
  # if ("p.value" %in% names(table_body)) {
  #   results <- modify_header_internal(
  #     results, p.value = paste0("**", translate_text("p-value"), "**")
  #   )
  # }
  # if (all(c("conf.low", "conf.high") %in% names(table_body))) {
  #   results <- modify_header_internal(
  #     results, ci = glue("**{style_percent(conf.level, symbol = TRUE)} CI**")
  #   )
  # }

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
  language <- get_theme_element("pkgwide-str:language", default = "en")
  if (model_type == "logistic") {
    header <- ifelse(exponentiate == TRUE, "OR", "log(OR)") %>% translate_text(language)
    attr(header, "footnote") <- translate_text("OR = Odds Ratio", language)
  }
  else if (model_type == "poisson") {
    header <- ifelse(exponentiate == TRUE, "IRR", "log(IRR)") %>% translate_text(language)
    attr(header, "footnote") <- translate_text("IRR = Incidence Rate Ratio", language)
  }
  else if (model_type == "prop_hazard") {
    header <- ifelse(exponentiate == TRUE, "HR", "log(HR)") %>% translate_text(language)
    attr(header, "footnote") <- translate_text("HR = Hazard Ratio", language)
  }
  else {
    header <-
      get_theme_element("tbl_regression-str:coef_header") %||%
      ifelse(exponentiate == TRUE, "exp(Beta)", "Beta") %>%
      as.character() %>%
      translate_text(language)
  }

  header
}
