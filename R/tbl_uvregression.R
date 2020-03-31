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
#' dichotomous variables appropriate for logistic regression with [glm].
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
#' dichotomous variable displayed on a single row.
#' Specify one and only one of `y` or `x`
#' @param formula String of the model formula.
#' Uses [glue::glue] syntax. Default is `"{y} ~ {x}"`, where `{y}`
#' is the dependent variable, and `{x}` represents a single covariate. For a
#' random intercept model, the formula may be `formula = "{y} ~ {x} + (1 | gear)"`.
#' @param method.args List of additional arguments passed on to the regression
#' function defined by `method`.
#' @param hide_n Hide N column. Default is `FALSE`
#' @inheritParams tbl_regression
#' @author Daniel D. Sjoberg
#' @seealso See tbl_regression \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{vignette}  for detailed examples
#' @family tbl_uvregression tools
#' @export
#' @return A `tbl_uvregression` object
#' @examples
#' tbl_uv_ex1 <-
#'   tbl_uvregression(
#'     trial[c("response", "age", "grade")],
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
#'     trial[c("ttdeath", "death", "age", "grade", "response")],
#'     method = coxph,
#'     y = Surv(ttdeath, death),
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

tbl_uvregression <- function(data, method, y = NULL, x = NULL, method.args = NULL,
                             formula = "{y} ~ {x}",
                             exponentiate = FALSE, label = NULL,
                             include = everything(), exclude = NULL,
                             hide_n = FALSE, show_single_row = NULL, conf.level = NULL,
                             estimate_fun = NULL, pvalue_fun = NULL, show_yesno = NULL,
                             tidy_fun = NULL) {
  # deprecated arguments -------------------------------------------------------
  if (!is.null(show_yesno)) {
    lifecycle::deprecate_stop(
      "1.2.2", "tbl_uvregression(show_yesno = )",
      "tbl_uvregression(show_single_row = )"
    )
  }

  if (!rlang::quo_is_null(rlang::enquo(exclude))) {
    lifecycle::deprecate_warn(
      "1.2.5",
      "gtsummary::tbl_uvregression(exclude = )",
      "tbl_uvregression(include = )",
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

  # bare to string/enexpr ------------------------------------------------------
  # updated method and y inputs to be bare, and converting them to strings
  # to be compatible with the rest of the function that assumes character input
  method <- rlang::enexpr(method)
  method.args <- rlang::enexpr(method.args)

  # converting to string, or keeping as NULL.  Using the standard
  # variable selector, but users may also pass `Surv(ttdeath, death)`,
  # which is not a column header, rather a function.  In that case,
  # converting the bare input to a string.
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  x <-
    tryCatch({
      var_input_to_string(data = data, select_input = !!x, arg_name = "x")
    }, error = function(e) {
      rlang::expr_text(x)
    })
  y <-
    tryCatch({
      var_input_to_string(data = data, select_input = !!y, arg_name = "y")
    }, error = function(e) {
      rlang::expr_text(y)
    })

  # checking selections of x and y
  if (is.null(x) + is.null(y) != 1L) {
    stop("Specify one, and only one, of `x` and `y`. This function can
         create univariate regression models holding either a covariate or outcome
         constant.", call. = FALSE)
  }
  if ((!is.null(x) && length(x) != 1) | (!is.null(y) && length(y) != 1)) {
    stop("Select only a single column in argument `x=` or `y=`.", call. = FALSE)
  }

  include <- var_input_to_string(data = data, select_input = !!rlang::enquo(include),
                                 arg_name = "include")
  exclude <- var_input_to_string(data = data, select_input = !!rlang::enquo(exclude),
                                 arg_name = "exclude")
  show_single_row <- var_input_to_string(data = data,
                                         select_input = !!rlang::enquo(show_single_row),
                                         arg_name = "show_single_row")

  # checking formula correctly specified ---------------------------------------
  if (!rlang::is_string(formula)) {
    stop('`formula` must be passed as a string, e.g. `formula = "{y} ~ {x}"`',
         call. = FALSE)
  }
  # checking that '{x}' appears on RHS of formula
  if (word(formula, start = 2L, sep = fixed("~")) %>%
    str_detect(pattern = fixed("{x}")) == FALSE) {
    stop("'{x}' must appear on RHS of '~' in formula argument", call. = FALSE)
  }
  # checking that '{y}' appears on LHS of formula
  if (word(formula, start = 1L, sep = fixed("~")) %>%
    str_detect(pattern = fixed("{y}")) == FALSE) {
    stop("'{y}' must appear on LHS of '~' in formula argument", call. = FALSE)
  }

  # checking estimate_fun and pvalue_fun are functions -------------------------
  if (!is.function(estimate_fun) | !is.function(pvalue_fun)) {
    stop("Arguments 'estimate_fun' and 'pvalue_fun' must be functions.",
         call. = FALSE)
  }

  # converting tidyselect formula lists to named lists -------------------------
  label <- tidyselect_to_list(data, label, .meta_data = NULL, arg_name = "label")
  # all sepcifed labels must be a string of length 1
  if (!every(label, ~ rlang::is_string(.x))) {
    stop("Each `label` specified must be a string of length 1.", call. = FALSE)
  }

  # data -----------------------------------------------------------------------
  # data is a data frame
  if (!is.data.frame(data)) {
    stop("`data` argument must be a data frame.", call. = FALSE)
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
    stop("There were no covariates selected.", call. = FALSE)
  }

  # bulding regression models --------------------------------------------------
  df_model <-
    tibble(vars = all_vars) %>%
    set_names(ifelse(!is.null(y), "x", "y")) %>%
    mutate(
      formula_chr = glue(formula),
      model = map(
        .data$formula_chr,
        ~list(method, formula = as.formula(.x), data = data) %>%
          c(as.list(method.args)[-1]) %>%
          as.call() %>%
          eval()
      )
    )

  # convert model to tbl_regression object -------------------------------------
  if (!is.null(y)) {
    df_model <-
      df_model %>%
      mutate(
        tbl = map2(
          .data$model, .data$x,
          ~tbl_regression(
            .x,
            exponentiate = exponentiate,
            conf.level = conf.level,
            label = label,
            show_single_row = intersect(.y, show_single_row),
            tidy_fun = tidy_fun
          )
        )
      )
  }
  if (!is.null(x)) {
    df_model <-
      df_model %>%
      mutate(
        tbl = map2(
          .data$model, .data$y,
          function(model, y) {
            tbl_uv <-
              tbl_regression(
                model,
                label = list(label[[y]] %||% attr(data[[y]], "label") %||% y) %>% set_names(x),
                exponentiate = exponentiate,
                conf.level = conf.level,
                include = x,
                show_single_row = show_single_row,
                tidy_fun = tidy_fun
              )
            tbl_uv$table_body$variable <- y
            tbl_uv$table_body$var_type <- NA_character_
            tbl_uv
          }
        )
      )
  }

  # adding N to table ----------------------------------------------------------
  if (hide_n == FALSE) {
    df_model <-
      df_model %>%
      mutate(
        tbl = map(
          .data$tbl,
          function(tbl) {
            tbl <- modify_header(tbl, N = "**N**")
            # only display N on label row
            tbl$table_body$N <- ifelse(tbl$table_body$row_type == "label",
                                       tbl$table_body$N, NA)
            tbl
          }
        )
      )
  }

  # stacking results to return -------------------------------------------------
  results <- tbl_stack(df_model$tbl)
  names(results$tbls) <- all_vars
  class(results) <- c("tbl_uvregression", "gtsummary")

  # creating a meta_data table -------------------------------------------------
  # (this will be used in subsequent functions, eg add_global_p)
  results$meta_data <-
    results$table_body %>%
    filter(.data$row_type == "label") %>%
    select(c("variable", "var_type", "label", "N"))

  # exporting results ----------------------------------------------------------
  results$inputs <- tbl_uvregression_inputs
  results$call_list = list(tbl_uvregression = match.call())

  results
}

