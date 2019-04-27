#' Display univariate regression model results in table
#'
#' The `tbl_uvregression` function arguments are similar to the [tbl_regression]
#' arguments. Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{tbl_uvregression vignette}
#' for detailed examples.
#'
#' @section Note:
#' The N reported in the `tbl_uvregression()` output is the number of observations
#' in the data frame `model.frame(x)`. Depending on the model input, this N
#' may represent different quantities. In most cases, it is the number of people or
#' units in your model.  Here are some common exceptions.
#' 1. Survival regression models including time dependent covariates.
#' 2. Random- or mixed-effects regression models with clustered data.
#' 3. GEE regression models with clustered data.
#'
#' This list is not exhaustive, and care should be taken for each number reported.
#'
#' @param data Data frame to be used in univariate regression modeling.  Data
#' frame includes the outcome variable(s) and the independent variables.
#' @param method Regression method (e.g. \code{\link[stats]{lm}},
#' \code{\link[stats]{glm}}, \code{\link[survival]{coxph}}, and more).
#' @param y model outcome as a string (e.g. `y = recurrence` or `y = Surv(time, recur)`)
#' @param formula String that becomes the model formula.
#' Uses \code{\link[glue]{glue}} syntax. Default is `"{y} ~ {x}"`, where `{y}`
#' is the dependent variable, and `{x}` represents a single covariate. For a
#' random intercept, the formula may be `formula = "{y} ~ {x} + (1 | gear)"`.
#' @param method.args List of additional arguments passed on to the regression
#' function defined by method.
#' @param hide_n Hide N column. Default is `FALSE`
#' @inheritParams tbl_regression
#' @importFrom stringr word str_detect fixed
#' @author Daniel D. Sjoberg
#' @family tbl_uvregression tools
#' @export
#' @examples
#' tbl_uv_ex1 <-
#'  tbl_uvregression(
#'    trial %>% dplyr::select(response, age, grade),
#'    method = glm,
#'    y = response,
#'    method.args = list(family = binomial),
#'    exponentiate = TRUE
#'  )
#'
#' # rounding pvalues to 2 decimal places
#' library(survival)
#' tbl_uv_ex2 <-
#'   tbl_uvregression(
#'     trial %>% dplyr::select(ttdeath, death, age, grade, response),
#'     method = coxph,
#'     y = Surv(ttdeath, death),
#'     label = list(grade = "Grade"),
#'     exponentiate = TRUE,
#'     pvalue_fun = function(x) style_pvalue(x, digits = 2)
#'   )
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_uv_ex1.png}{options: width=50\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_uv_ex2.png}{options: width=50\%}}
#'
tbl_uvregression <- function(data, method, y, method.args = NULL,
                             formula = "{y} ~ {x}",
                             exponentiate = FALSE, label = NULL,
                             hide_n = FALSE,
                             show_yesno = NULL, conf.level = 0.95,
                             coef_fun = ifelse(exponentiate == TRUE, style_ratio, style_sigfig),
                             pvalue_fun = style_pvalue) {
  # bare to string -------------------------------------------------------------
  # updated method and y inputs to be bare, and converting them to strings
  # to be compatible with the rest of the function that assumes character input
  method <- deparse(substitute(method)) %>% as.character()
  y <- deparse(substitute(y)) %>% as.character()

  # data -----------------------------------------------------------------------
  # data is a data frame
  if (!is.data.frame(data)) {
    stop(glue(
      "'data' input must be a data frame."
    ))
  }

  # will return call, and all object passed to in table1 call
  # the object func_inputs is a list of every object passed to the function
  tbl_uvregression_inputs <- as.list(environment())

  # checking that '{x}' appears on RHS of formula
  if(word(formula, start = 2L, sep = fixed("~")) %>%
    str_detect(pattern = fixed("{x}")) == FALSE) {
    stop("'{x}' must appear on RHS of '~' in formula argument")
  }

  # get all x vars
  x_vars <- names(data) %>%
    setdiff( # removing outcome variable(s)
      paste0(y, "~1") %>% stats::as.formula() %>% all.vars()
    ) %>%
    setdiff( # removing potential variables added to model formula (e.g. random intercepts)
      all.vars(stats::as.formula(formula)[[3]]) %>% remove_one_x() # the one x removed is the {x}
    )

  # bulding regression models
  model_obj_list <-
    map(
      x_vars,
      function(x)
        do.call(
        what = method,
        args = c(
          list(
            formula = glue(formula) %>% stats::as.formula(),
            data = data
          ),
          method.args
        )
      )
    )
  names(model_obj_list) <- x_vars

  # formatting regression models
  tbl_regression_list <-
    imap(
      model_obj_list,
      ~ tbl_regression(
        .x,
        exponentiate = exponentiate,
        conf.level = conf.level,
        label = label,
        show_yesno = show_yesno
      )
    )

  # extracting model tables and stacking
  table_body <-
    map_dfr(
      tbl_regression_list,
      ~ .x %>% pluck("table_body")
    ) %>%
    mutate(
      N = if_else(.data$row_type == "label", .data$N, NA_integer_)
    )

  # creating a meta_data table (this will be used in subsequent functions, eg add_global)
  meta_data <-
    table_body %>%
    filter(!!parse_expr('row_type == "label"')) %>%
    select(c("variable", "var_type", "label", "N"))

  # returning named list of results
  results <- list(
    inputs = tbl_uvregression_inputs,
    tbl_regression_list = tbl_regression_list,
    meta_data = meta_data,
    table_body = table_body,
    call_list = list(tbl_uvregression = match.call()),
    gt_calls = eval(gt_tbl_uvregression)
  )

  # hiding N column if requested
  if(hide_n ==TRUE) {
    results$gt_calls[["cols_hide_n"]] <-
      glue("cols_hide(columns = vars(N))")
  }


  class(results) <- "tbl_uvregression"
  results
}


# gt function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
gt_tbl_uvregression <- quote(list(
  # first call to the gt function
  gt = "gt(data = x$table_body)" %>%
    glue(),

  # label column indented and left just
  cols_align = glue(
    "cols_align(align = 'center') %>% ",
    "cols_align(align = 'left', columns = vars(label))"
  ),

  # do not print columns variable or row_type columns
  # here i do a setdiff of the variables i want to print by default
  cols_hide =
    "cols_hide(columns = vars(variable, row_ref, row_type, var_type))" %>%
    glue(),

  # NAs do not show in table
  fmt_missing =
    "fmt_missing(columns = everything(), missing_text = '')" %>%
    glue(),

  # Show "---" for reference groups
  fmt_missing_ref =
    "fmt_missing(columns = vars(coef, ll, ul), rows = row_type == 'level', missing_text = '---')" %>%
    glue(),

  # column headers
  cols_label = glue(
    "cols_label(",
    "label = md('**Characteristic**'), ",
    "N = md('**N**'), ",
    "coef = md('**{coef_header(model_obj_list[[1]], exponentiate)}**'), ",
    "ll = md('**{style_percent(conf.level, symbol = TRUE)} CI**'), ",
    "p.value = md('**p-value**')",
    ")"
  ),

  # adding p-value formatting (evaluate the expression with eval() function)
  fmt_pvalue =
    "fmt(columns = vars(p.value), rows = !is.na(p.value), fns = x$inputs$pvalue_fun)" %>%
    glue(),

  # ceof and confidence interval formatting
  fmt_coef =
    "fmt(columns = vars(coef, ll, ul), rows = !is.na(coef), fns = x$inputs$coef_fun)" %>%
    glue(),

  # combining ll and ul to print confidence interval
  cols_merge_ci =
    "cols_merge(col_1 = vars(ll), col_2 = vars(ul), pattern = '{1}, {2}')" %>%
    glue::as_glue(),

  # indenting levels and missing rows
  tab_style_text_indent = glue(
    "tab_style(",
    "style = cells_styles(text_indent = px(10), text_align = 'left'),",
    "locations = cells_data(",
    "columns = vars(label),",
    "rows = row_type != 'label'",
    "))"
  ),

  # column headers abbreviations footnote
  # extracting from the first variable regression model
  footnote_abbreviation =
    tbl_regression_list %>%
    pluck(1, "gt_calls", "footnote_abbreviation")
))

# helper function to remove one value of "x" from a vector
remove_one_x <- function(v) {
  index_remove <-
    (v == "x") %>%
    which() %>%
    min()

  v[-index_remove]
}
