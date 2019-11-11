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
#' e.g. `list("age" ~ "Age, yrs", "ptstage" ~ "Path T Stage")`
#' @param include Character vector of variable names to include from output.
#' @param exclude Character vector of variable names to exclude from output.
#' @param conf.level Must be strictly greater than 0 and less than 1.
#' Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param intercept Logical argument indicating whether to include the intercept
#' in the output.  Default is `FALSE`
#' @param show_single_row By default categorical variables are printed on
#' multiple rows.  If a variable is binary (e.g. Yes/No) and you wish to print
#' the regression coefficient on a single row, include the variable name here,
#' e.g. `show_single_row = c("var1", "var2")`
#' @param estimate_fun Function to round and format coefficient estimates.
#' Default is [style_sigfig] when the coefficients are not transformed, and
#' [style_ratio] when the coefficients have been exponentiated.
#' @param pvalue_fun Function to round and format p-values.
#' Default is [style_pvalue].
#' The function must have a numeric vector input (the numeric, exact p-value),
#' and return a string that is the rounded/formatted p-value (e.g.
#' `pvalue_fun = function(x) style_pvalue(x, digits = 2)` or equivalently,
#'  `purrr::partial(style_pvalue, digits = 2)`).
#' @param show_yesno deprecated
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
#'  glm(response ~ age + grade, trial, family = binomial(link = "logit")) %>%
#'     tbl_regression(exponentiate = TRUE, label = list(age = "Patient Age"))
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

tbl_regression <- function(x, label = NULL, exponentiate = FALSE,
                           include = NULL, exclude = NULL,
                           show_single_row = NULL, conf.level = NULL, intercept = FALSE,
                           estimate_fun = NULL, pvalue_fun = NULL, show_yesno = NULL) {
  # deprecated arguments -------------------------------------------------------
  if (!is.null(show_yesno)) {
    lifecycle::deprecate_stop("1.2.2", "tbl_regression(show_yesno = )",
                              "tbl_regression(show_single_row = )")
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
  if (!is.function(estimate_fun) | !is.function(pvalue_fun)) {
    stop("Inputs 'estimate_fun' and 'pvalue_fun' must be functions.")
  }

  # converting tidyselect formula lists to named lists
  # extracting model frame
  model_frame <- tryCatch({
    stats::model.frame(x)
  },
  warning = function(w) {
    warning(x)
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
    stop(e)
  })
  label <- tidyselect_to_list(model_frame, label, input_type = "label")
  # all sepcifed labels must be a string of length 1
  if (!every(label, ~ rlang::is_string(.x))) {
    stop("Each `label` specified must be a string of length 1.")
  }

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
  table_body <-
    parse_fit(x, tidy_model, label, show_single_row) %>%
    # adding character CI
    mutate(
      ci = if_else(
        !is.na(.data$conf.low),
        paste0(estimate_fun(.data$conf.low), ", ", estimate_fun(.data$conf.high)),
        NA_character_
      )
    ) %>%
    # moving pvalue col to end of df
    select(-.data$p.value, .data$p.value)

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

  # table of column headers
  table_header <-
    tibble(column = names(table_body)) %>%
    table_header_fill_missing() %>%
    table_header_fmt(
      p.value = "x$inputs$pvalue_fun",
      estimate = "x$inputs$estimate_fun",
      conf.low = "x$inputs$estimate_fun",
      conf.high = "x$inputs$estimate_fun"
    ) %>%
    # adding footnotes to table_header tibble
    mutate(
      footnote_abbrev = map2(
        .data$column, .data$footnote_abbrev,
        function(x1, y1) {
          if (x1 == "estimate")
            return(c(y1, estimate_header(x, exponentiate) %>% attr("footnote")))
          else if (x1 == "ci") return(c(y1, "CI = Confidence Interval"))
          return(y1)
        }
      )
    )

  results <- list(
    table_body = table_body,
    table_header = table_header,
    n = n,
    model_obj = x,
    inputs = func_inputs,
    call_list = list(tbl_regression = match.call()),
    gt_calls = eval(gt_tbl_regression),
    kable_calls = eval(kable_tbl_regression)
  )

  # setting column headers
  results <- modify_header_internal(
    results,
    label = "**Characteristic**",
    estimate = glue("**{estimate_header(x, exponentiate)}**"),
    ci = glue("**{style_percent(conf.level, symbol = TRUE)} CI**"),
    p.value = "**p-value**"
  )

  # writing additional gt and kable calls with data from table_header
  results <- update_calls_from_table_header(results)

  # assigning a class of tbl_regression (for special printing in Rmarkdown)
  class(results) <- "tbl_regression"

  results
}

# gt function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
gt_tbl_regression <- quote(list(
  # first call to the gt function
  gt = "gt::gt(data = x$table_body)" %>%
    glue(),

  # label column indented and left just
  cols_align = glue(
    "gt::cols_align(align = 'center') %>% ",
    "gt::cols_align(align = 'left', columns = gt::vars(label))"
  ),

  # NAs do not show in table
  fmt_missing = "gt::fmt_missing(columns = gt::everything(), missing_text = '')" %>%
    glue(),

  # Show "---" for reference groups
  fmt_missing_ref =
    "gt::fmt_missing(columns = gt::vars(estimate, ci), rows = row_ref == TRUE, missing_text = '---')" %>%
      glue(),

  # indenting levels and missing rows
  tab_style_text_indent = glue(
    "gt::tab_style(",
    "style = gt::cell_text(indent = gt::px(10), align = 'left'),",
    "locations = gt::cells_data(",
    "columns = gt::vars(label), ",
    "rows = row_type != 'label'",
    "))"
  )
))


# kable function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
kable_tbl_regression <- quote(list(
  # first call to the gt function
  kable = glue("x$table_body"),

  #  placeholder, so the formatting calls are performed other calls below
  fmt = NULL,

  # Show "---" for reference groups
  fmt_missing_ref = glue(
    "dplyr::mutate_at(dplyr::vars(estimate, conf.low), ",
    "~ dplyr::case_when(row_ref == TRUE ~ '---', TRUE ~ .))"
  )
))



# identifies headers for common models (logistic, poisson, and PH regression)
estimate_header <- function(x, exponentiate) {
  # first identify the type ----------------------------------------------------
  model_type = "generic"
  # GLM and GEE models
  if (class(x)[1] %in% c("glm", "geeglm") &&
      x$family$family == "binomial" &&
      x$family$link == "logit")
    model_type = "logistic"
  else if (class(x)[1] %in% c("glm", "geeglm") &&
           x$family$family == "poisson" &&
           x$family$link == "log")
    model_type = "poisson"
  # Cox Models
  else if (class(x)[1] == "coxph")
    model_type = "prop_hazard"
  # LME4 models
  else if (class(x)[1] == "glmerMod" &&
           attr(class(x), "package") == "lme4" &&
           x@resp$family$family == "binomial" &&
           x@resp$family$link == "logit")
    model_type = "logistic"
  else if (class(x)[1] == "glmerMod" &&
           attr(class(x), "package") == "lme4" &&
           x@resp$family$family == "poisson" &&
           x@resp$family$link == "log")
    model_type = "poisson"

  # assigning header and footer ------------------------------------------------
  if (model_type == "logistic") {
    header <- ifelse(exponentiate == TRUE ,"OR", "log(OR)")
    attr(header, "footnote") <- "OR = Odds Ratio"
  }
  else if (model_type == "poisson") {
    header <- ifelse(exponentiate == TRUE ,"IRR", "log(IRR)")
    attr(header, "footnote") <- "IRR = Incidence Rate Ratio"
  }
  else if (model_type == "prop_hazard") {
    header <- ifelse(exponentiate == TRUE ,"HR", "log(HR)")
    attr(header, "footnote") <- "HR = Hazard Ratio"
  }
  else {
    header <- ifelse(exponentiate == TRUE ,"exp(Beta)", "Beta")
  }

  header
}
