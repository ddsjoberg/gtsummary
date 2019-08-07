#' Display univariate regression model results in table
#'
#' The `tbl_uvregression` function arguments are similar to the [tbl_regression]
#' arguments. Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{tbl_uvregression vignette}
#' for detailed examples.
#'
#' @section Setting Defaults:
#' If you like to consistently use a different function to format p-values or
#' estimates, you can set options in the script or in the user- or
#' project-level startup file, '.Rprofile'.  The default confidence level can
#' also be set. Please note the default option for the estimate is the same
#' as it is for `tbl_regression()`.
#' \itemize{
#'   \item `options(gtsummary.pvalue_fun = new_function)`
#'   \item `options(gtsummary.tbl_regression.estimate_fun = new_function)`
#'   \item `options(gtsummary.conf.level = 0.90)`
#' }
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
#' @param data data frame to be used in univariate regression modeling.  Data
#' frame includes the outcome variable(s) and the independent variables.
#' @param method regression method (e.g. \code{\link[stats]{lm}},
#' \code{\link[stats]{glm}}, \code{\link[survival]{coxph}}, and more).
#' @param y model outcome as a string (e.g. `y = recurrence` or `y = Surv(time, recur)`)
#' @param formula string that becomes the model formula.
#' Uses \code{\link[glue]{glue}} syntax. Default is `"{y} ~ {x}"`, where `{y}`
#' is the dependent variable, and `{x}` represents a single covariate. For a
#' random intercept, the formula may be `formula = "{y} ~ {x} + (1 | gear)"`.
#' @param method.args list of additional arguments passed on to the regression
#' function defined by `method`.
#' @param hide_n hide N column. Default is `FALSE`
#' @inheritParams tbl_regression
#' @importFrom stringr word str_detect fixed
#' @author Daniel D. Sjoberg
#' @seealso See tbl_regression \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{vignette}  for detailed examples
#' @family tbl_uvregression tools
#' @export
#' @return A `tbl_uvregression` object
#' @examples
#' tbl_uv_ex1 <-
#'   tbl_uvregression(
#'     trial %>% dplyr::select(response, age, grade),
#'     method = glm,
#'     y = response,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE
#'   )
#'
#' # rounding pvalues to 2 decimal places
#' library(survival)
#' tbl_uv_ex2 <-
#'   tbl_uvregression(
#'     trial %>% dplyr::select(ttdeath, death, age, grade, response),
#'     method = coxph,
#'     y = Surv(ttdeath, death),
#'     label = list("grade" ~ "Grade"),
#'     exponentiate = TRUE,
#'     pvalue_fun = function(x) style_pvalue(x, digits = 2)
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_uv_ex1.png}{options: width=50\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_uv_ex2.png}{options: width=50\%}}

tbl_uvregression <- function(data, method, y, method.args = NULL,
                             formula = "{y} ~ {x}",
                             exponentiate = FALSE, label = NULL,
                             hide_n = FALSE, show_yesno = NULL, conf.level = NULL,
                             estimate_fun = NULL, pvalue_fun = NULL) {
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

  # bare to string -------------------------------------------------------------
  # updated method and y inputs to be bare, and converting them to strings
  # to be compatible with the rest of the function that assumes character input
  method <- deparse(substitute(method)) %>% as.character()
  y <- deparse(substitute(y)) %>% as.character()

  # checking estimate_fun and pvalue_fun are functions
  if (!is.function(estimate_fun) | !is.function(pvalue_fun)) {
    stop("Inputs 'estimate_fun' and 'pvalue_fun' must be functions.")
  }

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
      if (purrr::some(label, negate(rlang::is_bare_formula))) {
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
  if (word(formula, start = 2L, sep = fixed("~")) %>%
    str_detect(pattern = fixed("{x}")) == FALSE) {
    stop("'{x}' must appear on RHS of '~' in formula argument")
  }

  # get all x vars
  x_vars <- names(data) %>%
    setdiff( # removing outcome variable(s)
      paste0(y, "~1") %>%
        stats::as.formula() %>%
        all.vars()
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

  # column labels
  # table of column headers
  table_header <-
    tibble(column = names(table_body)) %>%
    table_header_fill_missing() %>%
    table_header_fmt(
      p.value = "x$inputs$pvalue_fun",
      estimate = "x$inputs$estimate_fun",
      conf.low = "x$inputs$estimate_fun",
      conf.high = "x$inputs$estimate_fun"
    )

  # creating a meta_data table (this will be used in subsequent functions, eg add_global_p)
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
    table_header = table_header,
    call_list = list(tbl_uvregression = match.call()),
    gt_calls = eval(gt_tbl_uvregression),
    kable_calls = eval(kable_tbl_uvregression)
  )

  # column headers
  results <- modify_header_internal(
    results,
    label = "**Characteristic**",
    estimate = glue("**{estimate_header(model_obj_list[[1]], exponentiate)}**"),
    conf.low = glue("**{style_percent(conf.level, symbol = TRUE)} CI**"),
    p.value = "**p-value**"
  )

  # unhiding N column and assigning label, if requested
  if (hide_n == FALSE) {
    results <- modify_header_internal(
      results,
      N = "**N**",
    )
  }

  # writing additional gt and kable calls with data from table_header
  results <- update_calls_from_table_header(results)

  class(results) <- "tbl_uvregression"
  results
}


# gt function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
gt_tbl_uvregression <- quote(list(
  # first call to the gt function
  gt = "gt::gt(data = x$table_body)" %>%
    glue(),

  # label column indented and left just
  cols_align = glue(
    "gt::cols_align(align = 'center') %>% ",
    "gt::cols_align(align = 'left', columns = gt::vars(label))"
  ),

  # NAs do not show in table
  fmt_missing =
    "gt::fmt_missing(columns = gt::everything(), missing_text = '')" %>%
      glue(),

  # Show "---" for reference groups
  fmt_missing_ref =
    "gt::fmt_missing(columns = gt::vars(estimate, conf.low, conf.high), rows = row_ref == TRUE, missing_text = '---')" %>%
      glue(),

  # combining conf.low and conf.high to print confidence interval
  cols_merge_ci =
    "gt::cols_merge(col_1 = gt::vars(conf.low), col_2 = gt::vars(conf.high), pattern = '{1}, {2}')" %>%
      glue::as_glue(),

  # indenting levels and missing rows
  tab_style_text_indent = glue(
    "gt::tab_style(",
    "style = gt::cell_text(indent = gt::px(10), align = 'left'),",
    "locations = gt::cells_data(",
    "columns = gt::vars(label), ",
    "rows = row_type != 'label'",
    "))"
  ),

  # column headers abbreviations footnote
  # extracting from the first variable regression model
  footnote_abbreviation =
    tbl_regression_list %>%
      pluck(1, "gt_calls", "footnote_abbreviation")
))

# kable function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
kable_tbl_uvregression <- quote(list(
  # first call to the gt function
  kable = glue("x$table_body"),

  #  placeholder, so the formatting calls are performed other calls below
  fmt = NULL,

  # combining conf.low and conf.high to print confidence interval
  cols_merge_ci =
    "dplyr::mutate(conf.low = ifelse(is.na(estimate), NA, glue::glue('{conf.low}, {conf.high}') %>% as.character()))" %>% glue::as_glue(),

  # Show "---" for reference groups
  fmt_missing_ref = glue(
    "dplyr::mutate_at(dplyr::vars(estimate, conf.low), ",
    "~ dplyr::case_when(row_ref == TRUE ~ '---', TRUE ~ .))"
  )
))


# helper function to remove one value of "x" from a vector
remove_one_x <- function(v) {
  index_remove <-
    (v == "x") %>%
    which() %>%
    min()

  v[-index_remove]
}
