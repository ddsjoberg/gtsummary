#' Display univariate regression model results in table
#'
#' @description
#' This function estimates univariate regression models and returns them in
#' a publication-ready table.  It can create univariate regression models holding
#' either a covariate or outcome constant.
#'
#' For models holding outcome constant, the function takes as arguments a data frame,
#' the type of regression model, and the outcome variable `y=`. Each column in the
#' data frame is regressed on the specified outcome. The `tbl_uvregression`
#' function arguments are similar to the [tbl_regression] arguments. Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{tbl_uvregression vignette}
#' for detailed examples.
#'
#' You may alternatively hold a single covariate constant. For this, pass a data
#' frame, the type of regression model, and a single
#' covariate in the `x=` argument. Each column of the data frame will serve as
#' the outcome in a univariate regression model. Take care using the `x` argument
#' that each of the columns in the data frame are appropriate for the same type
#' of model, e.g. they are all continuous variables appropriate for [lm], or
#' binary variables appropriate for logistic regression with [glm].
#'
#' @inheritSection tbl_regression Setting Defaults
#' @inheritSection tbl_regression Note
#'
#' @param data Data frame to be used in univariate regression modeling.  Data
#' frame includes the outcome variable(s) and the independent variables.
#' @param method Regression method (e.g. [lm], [glm], [survival::coxph], and more).
#' @param y Model outcome (e.g. `y = recurrence` or `y = Surv(time, recur)`).
#' All other column in `data` will be regressed on `y`.
#' Specify one and only one of `y` or `x`
#' @param x Model covariate (e.g. `x = trt`).
#' All other columns in `data` will serve as the outcome in a regression model
#' with `x` as a covariate.  Output table is best when `x` is a continuous or
#' binary variable displayed on a single row.
#' Specify one and only one of `y` or `x`
#' @param formula String of the model formula.
#' Uses [glue::glue] syntax. Default is `"{y} ~ {x}"`, where `{y}`
#' is the dependent variable, and `{x}` represents a single covariate. For a
#' random intercept model, the formula may be `formula = "{y} ~ {x} + (1 | gear)"`.
#' @param method.args List of additional arguments passed on to the regression
#' function defined by `method`.
#' @param hide_n Hide N column. Default is `FALSE`
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
#'     label = list(vars(grade) ~ "Grade"),
#'     exponentiate = TRUE,
#'     pvalue_fun = function(x) style_pvalue(x, digits = 2)
#'   )
#'
#' # for convenience, you can also pass named lists to any arguments
#' # that accept formulas (e.g label, etc.)
#' library(survival)
#' trial %>%
#'    dplyr::select(ttdeath, death, age, grade, response) %>%
#'    tbl_uvregression(
#'      method = coxph,
#'      y = Surv(ttdeath, death),
#'      label = list(grade = "Grade"),
#'      exponentiate = TRUE)
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_uv_ex1.png}{options: width=50\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_uv_ex2.png}{options: width=50\%}}

tbl_uvregression <- function(data, method, y = NULL, x = NULL, method.args = NULL,
                             formula = "{y} ~ {x}",
                             exponentiate = FALSE, label = NULL,
                             include = NULL, exclude = NULL,
                             hide_n = FALSE, show_single_row = NULL, conf.level = NULL,
                             estimate_fun = NULL, pvalue_fun = NULL, show_yesno = NULL) {
  # deprecated arguments -------------------------------------------------------
  if (!is.null(show_yesno)) {
    lifecycle::deprecate_stop("1.2.2", "tbl_uvregression(show_yesno = )",
                              "tbl_uvregression(show_single_row = )")
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

  # bare to string -------------------------------------------------------------
  # updated method and y inputs to be bare, and converting them to strings
  # to be compatible with the rest of the function that assumes character input
  method <- deparse(substitute(method)) %>% as.character()
  y_name <- deparse(substitute(y)) %>% as.character()
  x_name <- deparse(substitute(x)) %>% as.character()
  if (!rlang::quo_is_null(rlang::enquo(y))) y <- y_name
  if (!rlang::quo_is_null(rlang::enquo(x))) x <- x_name
  if (is.null(x) + is.null(y) != 1L) {
    stop("Specify one, and only one, of `x` and `y`. This function can
         create univariate regression models holding either a covariate or outcome
         constant.")
  }

  # checking formula correctly specified ---------------------------------------
  # checking that '{x}' appears on RHS of formula
  if (word(formula, start = 2L, sep = fixed("~")) %>%
      str_detect(pattern = fixed("{x}")) == FALSE) {
    stop("'{x}' must appear on RHS of '~' in formula argument")
  }
  # checking that '{y}' appears on LHS of formula
  if (word(formula, start = 1L, sep = fixed("~")) %>%
      str_detect(pattern = fixed("{y}")) == FALSE) {
    stop("'{y}' must appear on LHS of '~' in formula argument")
  }

  # checking estimate_fun and pvalue_fun are functions -------------------------
  if (!is.function(estimate_fun) | !is.function(pvalue_fun)) {
    stop("Arguments 'estimate_fun' and 'pvalue_fun' must be functions.")
  }

  # converting tidyselect formula lists to named lists -------------------------
  label <- tidyselect_to_list(data, label, .meta_data = NULL, input_type = "label")
  # all sepcifed labels must be a string of length 1
  if (!every(label, ~ rlang::is_string(.x))) {
    stop("Each `label` specified must be a string of length 1.")
  }

  # data -----------------------------------------------------------------------
  # data is a data frame
  if (!is.data.frame(data)) {
    stop("'data' input must be a data frame.")
  }

  # will return call, and all object passed to in table1 call
  # the object func_inputs is a list of every object passed to the function
  tbl_uvregression_inputs <- as.list(environment())
  tbl_uvregression_inputs <-
    tbl_uvregression_inputs[!names(tbl_uvregression_inputs) %in% c("x_name", "y_name")]

  # get all vars not specified -------------------------------------------------
  all_vars <-
    names(data) %>%
    # removing x or y variable
    setdiff(paste(c(y, x), "~ 1") %>% stats::as.formula() %>% all.vars()) %>%
    # removing any other variables listed in the formula
    setdiff(all.vars(stats::as.formula(formula), unique = FALSE)) %>%
    # removing {y} and {x}
    setdiff(c("x", "y"))

  if (!is.null(include)) all_vars <- intersect(all_vars, include)
  all_vars <- all_vars %>% setdiff(exclude)
  if (length(all_vars) == 0) {
    stop("There were no covariates selected.")
  }

  # bulding regression models --------------------------------------------------
  if (is.null(x)) {
    model_obj_list <-
      map(
        all_vars,
        function(x)
          do.call(what = method,
                  args = c(list(formula = glue(formula) %>% stats::as.formula(),
                                data = data),
                           method.args))
      )
  }
  else if (is.null(y)) {
    model_obj_list <-
      map(
        all_vars,
        function(y)
          do.call(what = method,
                  args = c(list(formula = glue(formula) %>% stats::as.formula(),
                                data = data),
                           method.args))
      )
  }
  names(model_obj_list) <- all_vars

  # formatting regression models with tbl_regression ---------------------------
  if (is.null(x)) {
    tbl_regression_list <-
      imap(
        model_obj_list,
        ~ tbl_regression(
          .x,
          exponentiate = exponentiate,
          conf.level = conf.level,
          label = label,
          show_single_row = intersect(.y, show_single_row)
        )
      )
  }
  else if (is.null(y)) {
    tbl_regression_list <-
      imap(
        model_obj_list,
        function(tbl, var) {
          tbl_uv <-
            tbl_regression(
              tbl,
              label = list(label[[var]] %||% attr(data[[var]], "label") %||% var) %>% set_names(x),
              exponentiate = exponentiate,
              conf.level = conf.level,
              include = x,
              show_single_row = show_single_row
            )
          tbl_uv$table_body$variable = var
          tbl_uv$table_body$var_type = NA_character_
          tbl_uv
        }
      )
  }

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
    left_join(tbl_regression_list %>% pluck(1, "table_header"),
              by = "column")

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
    label = ifelse(!is.null(y), "**Covariate**", "**Outcome**"),
    estimate = glue("**{estimate_header(model_obj_list[[1]], exponentiate)}**"),
    ci = glue("**{style_percent(conf.level, symbol = TRUE)} CI**"),
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
kable_tbl_uvregression <- quote(list(
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


# helper function to remove one value of "x" from a vector
remove_one_x <- function(v) {
  index_remove <-
    (v == "x") %>%
    which() %>%
    min()

  v[-index_remove]
}
