#' Display regression model results in table
#'
#' This function uses [broom::tidy](https://broom.tidyverse.org/reference/brms_tidiers.html) and
#' [broom.mixed::tidy](https://github.com/bbolker/broom.mixed)
#' to perform the initial model formatting. Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html}{tbl_regression vignette}
#' for detailed examples.
#'
#' @section Note:
#' The N reported in the `tbl_regression()` output is the number of observations
#' in the data frame `model.frame(x)`. Depending on the model input, this N
#' may represent different quantities. In most cases, it is the total number of
#' observations in your model; however, the precise definition of an observation,
#' or unit of analysis, may differ across models. Here are some common examples.
#' 1. Survival regression models including time dependent covariates.
#' 2. Random- or mixed-effects regression models with clustered data.
#' 3. GEE regression models with clustered data.
#'
#' This list is not exhaustive, and care should be taken for each number reported.
#'
#' @param x regression model object
#' @param exponentiate logical indicating whether or not to exponentiate the
#' coefficient estimates. Default is `FALSE`.
#' @param label list of formulas specifying variables labels,
#' e.g. `list("age" ~ "Age, yrs", "ptstage" ~ "Path T Stage")`
#' @param include names of variables to include in output.
#' @param exclude names of variables to exclude from output.
#' @param conf.level must be strictly greater than 0 and less than 1.
#' Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param intercept logical argument indicating whether to include the intercept
#' in the output.  Default is `FALSE`
#' @param show_yesno By default yes/no categorical variables are printed on a
#' single row, when the 'No' category is the reference group.  To print both
#' levels in the output table, include the variable name in the show_yesno
#' vector, e.g. `show_yesno = c("var1", "var2")``
#' @param estimate_fun function to round and format beta coefficient estimates.
#' Default is [style_sigfig] when the coefficients are not transformed, and
#' [style_ratio] when the coefficients have been exponentiated.
#' @param pvalue_fun function to round and format p-values.
#' Default is \code{\link{style_pvalue}}.
#' The function must have a numeric vector input (the numeric, exact p-value),
#' and return a string that is the rounded/formatted p-value (e.g.
#' \code{pvalue_fun = function(x) style_pvalue(x, digits = 2)} or equivalently,
#'  \code{purrr::partial(style_pvalue, digits = 2)}).
#' @author Daniel D. Sjoberg
#' @seealso See tbl_regression \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html}{vignette} for detailed examples
#' @family tbl_regression tools
#' @export
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
#'
tbl_regression <- function(x, label = NULL,
                           exponentiate = FALSE,
                           include = NULL,
                           exclude = NULL,
                           show_yesno = NULL,
                           conf.level = 0.95, intercept = FALSE,
                           estimate_fun = ifelse(exponentiate == TRUE, style_ratio, style_sigfig),
                           pvalue_fun = style_pvalue) {
  # checking estimate_fun and pvalue_fun are functions
  if (!is.function(estimate_fun) | !is.function(pvalue_fun)) {
    stop("Inputs 'estimate_fun' and 'pvalue_fun' must be functions.")
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
      if (some(label, negate(rlang::is_bare_formula))) {
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

  # converting tidyselect formula lists to named lists
  label <- tidyselect_to_list(stats::model.frame(x), label)




  # will return call, and all object passed to in tbl_regression call
  # the object func_inputs is a list of every object passed to the function
  func_inputs <- as.list(environment())

  # using broom and broom.mixed to tidy up regression results, and
  # then reversing order of data frame
  tidy_model <-
    tidy_wrap(x, exponentiate, conf.level)

  # parsing the terms from model and variable names
  # outputing a tibble of the parsed model with
  # rows for reference groups, and headers for
  # categorical variables
  table_body <- parse_fit(x, tidy_model, label, show_yesno)

  # including and excluding variables/intercept indicated
  # Note, include = names(stats::model.frame(mod_nlme))
  # has an error for nlme because it is "reStruct"
  if (!is.null(include)) {
    include_err <- include %>% setdiff(table_body$variable %>% unique())
    if (length(include_err) > 0) {
      stop(glue(
        "'include' must be be a subset of '{paste(table_body$variable %>% unique(), collapse = ', ')}'"
      ))
    }
  }
  if (is.null(include)) include <- table_body$variable %>% unique()
  if (intercept == FALSE) include <- include %>% setdiff("(Intercept)")
  include <- include %>% setdiff(exclude)

  # keeping variables indicated in `include`
  table_body <-
    table_body %>%
    filter(.data$variable %in% include)

  # model N
  n <- stats::model.frame(x) %>% nrow()

  # footnote abbreviation details
  footnote_abbr <-
    estimate_header(x, exponentiate) %>%
    attr("footnote") %>%
    c("CI = Confidence Interval") %>%
    paste(collapse = ", ")
  footnote_location <- ifelse(
    is.null(attr(estimate_header(x, exponentiate), "footnote")),
    "vars(conf.low)",
    "vars(estimate, conf.low)"
  )

  results <- list(
    table_body = table_body,
    n = n,
    model_obj = x,
    inputs = func_inputs,
    call_list = list(tbl_regression = match.call()),
    gt_calls = eval(gt_tbl_regression)
  )

  # assigning a class of tbl_regression (for special printing in Rmarkdown)
  class(results) <- "tbl_regression"

  results
}

# gt function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
gt_tbl_regression <- quote(list(
  # first call to the gt function
  gt = "gt(data = x$table_body)" %>%
    glue(),

  # label column indented and left just
  gt_calls = glue(
    "cols_align(align = 'center') %>% ",
    "cols_align(align = 'left', columns = vars(label))"
  ),

  # do not print columns variable or row_type columns
  # here i do a setdiff of the variables i want to print by default
  cols_hide = "cols_hide(columns = vars(variable, row_ref, row_type, var_type, N))" %>%
    glue(),

  # NAs do not show in table
  fmt_missing = "fmt_missing(columns = everything(), missing_text = '')" %>%
    glue(),

  # Show "---" for reference groups
  fmt_missing_ref =
    "fmt_missing(columns = vars(estimate, conf.low, conf.high), rows = row_type == 'level', missing_text = '---')" %>%
      glue(),

  # column headers
  cols_label = glue(
    "cols_label(",
    "label = md('**N = {n}**'), ",
    "estimate = md('**{estimate_header(x, exponentiate)}**'), ",
    "conf.low = md('**{style_percent(conf.level, symbol = TRUE)} CI**'), ",
    "p.value = md('**p-value**')",
    ")"
  ),

  # column headers abbreviations footnote
  footnote_abbreviation = glue(
    "tab_footnote(",
    "footnote = '{footnote_abbr}',",
    "locations = cells_column_labels(",
    "columns = {footnote_location})",
    ")"
  ),

  # adding p-value formatting (evaluate the expression with eval() function)
  fmt_pvalue =
    "fmt(columns = vars(p.value), rows = !is.na(p.value), fns = x$inputs$pvalue_fun)" %>%
      glue(),

  # ceof and confidence interval formatting
  fmt_estimate =
    "fmt(columns = vars(estimate, conf.low, conf.high), rows = !is.na(estimate), fns = x$inputs$estimate_fun)" %>%
      glue(),

  # combining conf.low and conf.high to print confidence interval
  cols_merge_ci =
    "cols_merge(col_1 = vars(conf.low), col_2 = vars(conf.high), pattern = '{1}, {2}')" %>%
      glue::as_glue(),

  # indenting levels and missing rows
  tab_style_text_indent = glue(
    "tab_style(",
    "style = cells_styles(text_indent = px(10), text_align = 'left'),",
    "locations = cells_data(",
    "columns = vars(label),",
    "rows = row_type != 'label'",
    "))"
  )
))

# identifies headers for common models (logistic, poisson, and cox regression)
estimate_header <- function(x, exponentiate) {
  if (
    (class(x)[1] %in% c("glm", "geeglm")) | # generalized linear models, and GEE GLMs
      (class(x)[1] == "glmerMod" & attr(class(x), "package") %||% "NULL" == "lme4") # mixed effects models (from lme4 package)
  ) {
    if (class(x)[1] %in% c("glm", "geeglm")) {
      family <- x$family
    } else if (class(x)[1] == "glmerMod" & attr(class(x), "package") %||% "NULL" == "lme4") {
      family <- x@resp$family
    } else {
      stop("Error occured in 'estimate_header' function")
    }

    # logistic regression
    if (exponentiate == TRUE & family$family == "binomial" & family$link == "logit") {
      header <- "OR"
      attr(header, "footnote") <- "OR = Odds Ratio"
    }
    else if (exponentiate == FALSE & family$family == "binomial" & family$link == "logit") {
      header <- "log(OR)"
      attr(header, "footnote") <- "OR = Odds Ratio"
    }

    # poisson regression with log link
    else if (exponentiate == TRUE & family$family == "poisson" & family$link == "log") {
      header <- "IRR"
      attr(header, "footnote") <- "IRR = Incidence Rate Ratio"
    }
    else if (exponentiate == FALSE & family$family == "poisson" & family$link == "log") {
      header <- "log(IRR)"
      attr(header, "footnote") <- "IRR = Incidence Rate Ratio"
    }

    # Other models
    else if (exponentiate == TRUE) {
      header <- "exp(Coefficient)"
    } else {
      header <- "Coefficient"
    }
  }
  # Cox PH Regression
  else if (class(x)[1] == "coxph" & exponentiate == TRUE) {
    header <- "HR"
    attr(header, "footnote") <- "HR = Hazard Ratio"
  }
  else if (class(x)[1] == "coxph" & exponentiate == FALSE) {
    header <- "log(HR)"
    attr(header, "footnote") <- "HR = Hazard Ratio"
  }

  # Other models
  else if (exponentiate == TRUE) {
    header <- "exp(Coefficient)"
  } else {
    header <- "Coefficient"
  }

  header
}
