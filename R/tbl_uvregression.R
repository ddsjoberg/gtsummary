#' Creates table of univariate regression results
#'
#' The `tbl_uvregression` function arguments are similar to the \code{\link{tbl_regression}}
#' arguments. Review the `tbl_uvregression`
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{vignette}
#' for detailed examples.
#'
#' @param data Data frame to be used in univariate regression modeling.  Data frame
#' includes the outcome variable(s) and the independent variables.
#' @param method Regression method (e.g. \code{\link[stats]{lm}}, \code{\link[stats]{glm}},
#' \code{\link[survival]{coxph}}, and more).
#' @param y model outcome as a string (e.g. `y = 'recurrence'` or `y = 'Surv(time, recur)'`)
#' @param formula String that becomes the model formula.  Uses \code{\link[glue]{glue}} syntax.
#' Default is `"{y} ~ {.x}"`, where `{y}` is the dependent variable, and `{.x}`
#' represents a single covariate. For a random intercept, the formula may be
#' `formula = "{y} ~ {.x} + (1 | gear)"`.
#' @param method.args List of additional arguments passed on to the regression function defined by method.
#' @param exponentiate logical argument passed directly to `broom::tidy()`.
#' Default is `FALSE`
#' @param label list of labels to write in the output. `list(age60 = "Age > 60")`
#' @param show_yesno Vector of names of categorical and factor variables that
#' are `c("No", "Yes")`, `c("no", "yes")`, or `c("NO", "YES")` default to dichotomous printing
#' (i.e. only Yes shown). To force both levels to be shown include the column
#' name in `show_yesno`, e.g. `show_yesno = c("highgrade", "female")`
#' @param conf.level confidence level passed directly to \code{broom::tidy}.
#' Default is 0.95.
#' @param coef_fun function to round and format beta coefficients.  Default is \code{\link{style_sigfig}}
#' @param pvalue_fun function to round and format p-values.  Default is \code{\link{style_pvalue}}
#' @author Daniel Sjoberg
#' @export
#' @examples
#' tbl_uvregression(
#'   trial,
#'   method = "glm",
#'   y = "response",
#'   method.args = list(family = binomial),
#'   exponentiate = TRUE
#' )
#'
#' # rounding pvalues to 2 decimal places, and adding global p-values
#' tbl_uvregression(
#'   trial,
#'   method = "glm",
#'   y = "response",
#'   method.args = list(family = binomial),
#'   exponentiate = TRUE,
#'   pvalue_fun = function(x) style_pvalue(x, digits = 2)
#' ) %>%
#'   add_global()
tbl_uvregression <- function(data, method, y, method.args = NULL,
                             formula = "{y} ~ {.x}",
                             exponentiate = FALSE, label = NULL,
                             show_yesno = NULL, conf.level = 0.95,
                             coef_fun = style_sigfig, pvalue_fun = style_pvalue) {

  # data -----------------------------------------------------------------------
  # data is a data frame
  if (!is.data.frame(data)) {
    stop(glue(
      "'data' input must be a data frame."
    ))
  }

  # varnames -------------------------------------------------------------------
  # ".x" cannot be a variable name
  if (".x" %in% names(data)) {
    stop("'.x' is reserved and cannot be a column name in data frame.")
  }

  # will return call, and all object passed to in table1 call
  # the object func_inputs is a list of every object passed to the function
  tbl_uvregression_inputs <- as.list(environment())

  # get all x vars
  x_vars <- names(data) %>%
    setdiff( # removing outcome variable(s)
      paste0(y, "~1") %>% stats::as.formula() %>% all.vars()
    ) %>%
    setdiff( # removing potential variables added to model formula (e.g. random intercepts)
      all.vars(stats::as.formula(formula)[[3]])
    )

  # bulding regression models
  model_obj_list <-
    map(
      x_vars,
      ~ do.call(
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


  class(results) <- "tbl_uvregression"
  results
}


# gt function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
gt_tbl_uvregression <- quote(list(
  # first call to the gt function
  gt = "gt(data = x$table_body)",

  # label column indented and left just
  cols_align = glue(
    "cols_align(align = 'center') %>% ",
    "cols_align(align = 'left', columns = vars(label))"
  ),

  # do not print columns variable or row_type columns
  cols_hide =
    "cols_hide(columns = vars(variable, row_type, var_type))",

  # NAs do not show in table
  fmt_missing =
    "fmt_missing(columns = everything(), missing_text = '')",

  # Show "---" for reference groups
  fmt_missing_ref =
    "fmt_missing(columns = vars(coef, ll, ul), rows = row_type == 'level', missing_text = '---')",

  # column headers
  cols_label = glue(
    "cols_label(",
    "label = md('**Characteristic**'), ",
    "N = md('**N**'), ",
    "coef = md('**{coef_header(model_obj_list[1][[1]], exponentiate)}**'), ",
    "ll = md('**{style_percent(conf.level, symbol = TRUE)} CI**'), ",
    "pvalue = md('**p-value**')",
    ")"
  ),

  # adding p-value formatting (evaluate the expression with eval() function)
  fmt_pvalue =
    "fmt(columns = vars(pvalue), rows = !is.na(pvalue), fns = x$inputs$pvalue_fun)",

  # ceof and confidence interval formatting
  fmt_coef =
    "fmt(columns = vars(coef, ll, ul), rows = !is.na(coef), fns = x$inputs$coef_fun)",

  # combining ll and ul to print confidence interval
  cols_merge_ci =
    "cols_merge(col_1 = vars(ll), col_2 = vars(ul), pattern = '{1}, {2}')",

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
