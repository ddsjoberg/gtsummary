#' Univariable regression model summary
#'
#' @description
#' This function estimates univariable regression models and returns them in
#' a publication-ready table.
#' It can create regression models holding
#' either a covariate or an outcome constant.
#'
#' @section `x` and `y` arguments:
#' For models holding outcome constant, the function takes as arguments a data frame,
#' the type of regression model, and the outcome variable `y=`. Each column in the
#' data frame is regressed on the specified outcome. The `tbl_uvregression()`
#' function arguments are similar to the [`tbl_regression()`] arguments. Review the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{tbl_uvregression vignette}
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
#' @inheritSection tbl_regression Methods
#'
#' @param data (`data.frame`, `survey.design`)\cr
#'   A data frame or a survey design object.
#' @param method (`string`/`function`)\cr
#'   Regression method or function, e.g. [lm], [glm], [survival::coxph], `survey::svyglm`, etc.
#'   Methods may be passed as functions (`method=lm`) or as strings (`method='lm'`).
#' @param y,x (`expression`, `string`)\cr
#'   Model outcome (e.g. `y=recurrence` or `y=Surv(time, recur)`) or
#'   covariate (e.g. `x=trt`.
#'   All other column specified in `include` will be regressed against the constant `y` or `x`.
#'   Specify one and only one of `y` or `x`.
#' @param formula (`string`)\cr
#'   String of the model formula.
#'   Uses [`glue::glue()`] syntax. Default is `"{y} ~ {x}"`, where `{y}`
#'   is the dependent variable, and `{x}` represents a single covariate. For a
#'   random intercept model, the formula may be `formula = "{y} ~ {x} + (1 | gear)"`.
#' @param method.args (named `list`)\cr
#'   Named list of arguments assed to `method`.
#' @param hide_n (scalar `logical`)\cr
#'   Hide N column. Default is `FALSE`
#' @inheritParams tbl_regression
#' @author Daniel D. Sjoberg
#'
#' @seealso See tbl_regression \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html#tbl_uvregression}{vignette}  for detailed examples
#' @name tbl_uvregression
#'
#' @return A `tbl_uvregression` object
#'
#' @examplesIf gtsummary:::is_pkg_installed(c("cardx", "broom", "broom.helpers"), reference_pkg = "gtsummary")
#' # Example 1 ----------------------------------
#' tbl_uvregression(
#'   trial,
#'   method = glm,
#'   y = response,
#'   method.args = list(family = binomial),
#'   exponentiate = TRUE,
#'   include = c("age", "grade")
#' )
#'
#' # Example 2 ----------------------------------
#' # rounding pvalues to 2 decimal places
#' library(survival)
#'
#' tbl_uvregression(
#'   trial,
#'   method = coxph,
#'   y = Surv(ttdeath, death),
#'   exponentiate = TRUE,
#'   include = c("age", "grade", "response"),
#'   pvalue_fun = label_style_pvalue(digits = 2)
#' )
NULL

#' @export
#' @name tbl_uvregression
tbl_uvregression <- function(data, ...) {
  check_not_missing(data)
  UseMethod("tbl_uvregression")
}

#' @export
#' @name tbl_uvregression
tbl_uvregression.data.frame <- function(data,
                                        y = NULL,
                                        x = NULL,
                                        method,
                                        method.args = list(),
                                        exponentiate = FALSE,
                                        label = NULL,
                                        include = everything(),
                                        tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
                                        hide_n = FALSE,
                                        show_single_row = NULL,
                                        conf.level = 0.95,
                                        estimate_fun = ifelse(exponentiate, label_style_ratio(), label_style_sigfig()),
                                        pvalue_fun = label_style_pvalue(digits = 1),
                                        formula = "{y} ~ {x}",
                                        add_estimate_to_reference_rows = FALSE,
                                        conf.int = TRUE, ...) {
  set_cli_abort_call()
  y <- enquo(y)
  x <- enquo(x)
  method.args <- enquo(method.args)

  # setting default values -----------------------------------------------------
  if (missing(pvalue_fun)) {
    pvalue_fun <-
      get_deprecated_theme_element("tbl_regression-arg:pvalue_fun") %||%
      get_theme_element("pkgwide-fn:pvalue_fun", default = pvalue_fun)
  }
  pvalue_fun <- as_function(pvalue_fun, arg = "pvalue_fun")

  check_scalar_logical(exponentiate)
  if (missing(estimate_fun)) {
    estimate_fun <-
      get_theme_element("tbl_regression-arg:estimate_fun", default = estimate_fun)
  }
  estimate_fun <- as_function(estimate_fun, arg = "estimate_fun")

  if (missing(conf.int)) {
    conf.int <- get_theme_element("tbl_regression-arg:conf.int", default = conf.int)
  }
  if (missing(conf.level)) {
    conf.level <- get_theme_element("tbl_regression-arg:conf.level", default = conf.level)
  }
  if (missing(add_estimate_to_reference_rows)) {
    add_estimate_to_reference_rows <-
      get_theme_element("tbl_regression-arg:add_estimate_to_reference_rows",
                        default = add_estimate_to_reference_rows)
  }

  # check inputs ---------------------------------------------------------------
  check_not_missing(method)
  check_scalar_logical(hide_n)
  check_scalar_logical(add_estimate_to_reference_rows)
  check_scalar_logical(conf.int)
  check_scalar_range(conf.level, range = c(0, 1))
  check_uvregression_formula(formula)

  # check that only one of arguments x and y is specified
  if ((!is_quo_empty(x) && !is_quo_empty(y)) || (is_quo_empty(x) && is_quo_empty(y))) {
    cli::cli_abort(
      "Must specify one and only one of arguments {.arg x} and {.arg y}.",
      call = get_cli_abort_call()
    )
  }

  # process inputs -------------------------------------------------------------
  x <- .process_x_and_y_args_as_string(data, x)
  y <- .process_x_and_y_args_as_string(data, y)
  check_scalar(x, allow_empty = TRUE)
  check_scalar(y, allow_empty = TRUE)

  cards::process_selectors(
    as.data.frame(data),
    include = {{ include }},
    show_single_row = {{ show_single_row }}
  )

  # styler: off
  # remove any variables specified in arguments `x`/`y` from include
  include <- include |>
    # remove the x/y variable from the list
    setdiff(tryCatch(stats::reformulate(c(x, y)) |> all.vars(), error = \(e) character(0L))) |>
    # remove any other columns listed in the formula
    setdiff(tryCatch(glue::glue_data(.x = list(y = 1, x = 1), formula) |> stats::as.formula() |> all.vars(), error = \(e) character(0L)))
  if (is_empty(include)) {
    cli::cli_abort("The {.arg include} argument cannot be empty.", call = get_cli_abort_call())
  }

  # remove any variables not in include
  show_single_row <-
    if (is_empty(x)) intersect(show_single_row, include)
    else intersect(show_single_row, x)
  #styler: on

  cards::process_formula_selectors(
    as.data.frame(data)[include],
    label = label
  )
  cards::check_list_elements(
    x = label,
    predicate = \(x) is_string(x),
    error_msg = "Each value passed in the {.arg label} argument must be a string of length {.val {1}}."
  )

  .check_haven_labelled(as.data.frame(data)[include])

  # fill in labels
  label <-
    map(include, ~label[[.x]] %||% attr(as.data.frame(data)[[.x]], 'label') %||% .x) |>
    set_names(include)

  # will return call, and all object passed to in table1 call
  # the object func_inputs is a list of every object passed to the function
  tbl_uvregression_inputs <- as.list(environment())

  # construct models -----------------------------------------------------------
  lst_models <-
    include |>
    # construct a formula for each model
    .construct_uvregression_formulas(formula = formula, x = x, y = y) |>
    # build models
    .construct_uvregression_models(data = data, method = method, method.args = !!method.args)

  # summarize each regression model with `tbl_regression()` --------------------
  lst_tbls <-
    lst_models |>
    .construct_uvregression_tbls(
      label = label, exponentiate = exponentiate, tidy_fun = tidy_fun,
      show_single_row = show_single_row, conf.level = conf.level,
      estimate_fun = estimate_fun, pvalue_fun = pvalue_fun,
      add_estimate_to_reference_rows = add_estimate_to_reference_rows,
      conf.int = conf.int, x = x, ...
    )

  # if the outcome varied, then replace the variable names within tbls
  if (is_empty(y)) {
    lst_tbls <- lst_tbls |>
      imap(
        function(tbl, variable) {
          tbl$table_body$variable <- variable
          tbl$table_body$var_type <- NA_character_
          tbl
        }
      )
  }

  # stacking results to return -------------------------------------------------
  results <- tbl_stack(lst_tbls)
  class(results) <- c("tbl_uvregression", "gtsummary")

  # update column header if `x=` was used --------------------------------------
  if (!is_empty(x)) {
    results <- modify_table_styling(results, columns = "label", label = "**Outcome**")
  }

  # removing modify_stat_* columns ---------------------------------------------
  results$table_styling$header <-
    results$table_styling$header %>%
    select(-starts_with("modify_stat_"))

  # adding column of N ---------------------------------------------------------
  if (hide_n == FALSE) results <- add_n(results, location = "label") # styler: off

  # exporting results ----------------------------------------------------------
  results$inputs <- tbl_uvregression_inputs
  results$call_list <- list(tbl_uvregression = match.call())

  results
}

#' @export
#' @name tbl_uvregression
tbl_uvregression.survey.design <- tbl_uvregression.data.frame

is_quo_empty <- function(x) {
  tryCatch(is_empty(eval_tidy(x)), error = \(e) FALSE)
}

.construct_uvregression_tbls <- function(models, label, exponentiate, tidy_fun,
                                         show_single_row, conf.level,
                                         estimate_fun, pvalue_fun,
                                         add_estimate_to_reference_rows,
                                         conf.int, x, ...) {
  imap(
    models,
    function(model, variable) {
      tbl_i <-
        cards::eval_capture_conditions(
          tbl_regression(
            x = model,
            include = ifelse(is_empty(x), variable, x),
            label =
              # styler: off
              if (is_empty(x)) label[variable]
              else label[variable] |> set_names(x),
              # styler: on
            exponentiate = exponentiate,
            tidy_fun = tidy_fun,
            show_single_row =
              # styler: off
              if (is_empty(x)) intersect(variable, show_single_row)
              else intersect(x, show_single_row),
              # styler: on
            conf.level = conf.level,
            estimate_fun = estimate_fun,
            pvalue_fun = pvalue_fun,
            add_estimate_to_reference_rows = add_estimate_to_reference_rows,
            conf.int = conf.int,
            ...
          )
        )

      if (!is_empty(tbl_i[["error"]])) {
        cli::cli_abort(
          c("There was an {cli::col_red('error')} running {.fun tbl_regression} for variable {.val {variable}}. See message below.",
            "x" = tbl_i[["error"]]),
          call = get_cli_abort_call()
        )
      }
      if (!is_empty(tbl_i[["warning"]])) {
        cli::cli_inform(
          c("There was a {cli::col_yellow('warning')} running {.fun tbl_regression} for variable {.val {variable}}. See message below.",
            "!" = tbl_i[["warning"]])
        )
      }

      tbl_i[["result"]]
    }
  )

}

.construct_uvregression_models <- function(formulas, data, method, method.args) {
  method.args <- enquo(method.args)
  imap(
    formulas,
    \(formula, variable) {
      model_i <- cards::eval_capture_conditions({
        cardx::construct_model(data, formula = formula, method = method, method.args = !!method.args)
      })
      if (!is_empty(model_i[["error"]])) {
        cli::cli_abort(
          c("There was an {cli::col_red('error')} constructing the model for variable {.val {variable}}. See message below.",
            "x" = model_i[["error"]]),
          call = get_cli_abort_call()
        )
      }
      if (!is_empty(model_i[["warning"]])) {
        cli::cli_inform(
          c("There was a {cli::col_yellow('warning')} constructing the model for variable {.val {variable}}. See message below.",
            "!" = model_i[["warning"]])
        )
      }

      model_i[["result"]]
    }
  )
}


.construct_uvregression_formulas <- function(include, formula, x, y) {
  include |>
    # first, construct formula
    map(
      \(variable) {
        formula_i <-
          cards::eval_capture_conditions(
            glue(
              formula,
              .envir = list(y = ifelse(is_empty(y), variable, y),
                            x = ifelse(is_empty(x), variable, x))
            ) |>
              stats::as.formula()
          )
        if (!is_empty(formula_i[["error"]])) {
          cli::cli_abort(
            c("There was an error constructing the formula for variable {.val {variable}}. See message below.",
              "x" = formula_i[["error"]]),
            call = get_cli_abort_call()
          )
        }
        formula_i[["result"]]
      }
    ) |>
    set_names(include)
}


check_uvregression_formula <- function(formula) {
  # first formula must be a string
  check_string(formula)
  formula_split <- strsplit(formula, split = "~", fixed = TRUE)[[1]]
  if (length(formula_split) != 2L) {
    cli::cli_abort(
      "The {.arg formula} argument must be have structure of a standard formula, e.g. {.val {{y}} ~ {{x}}}.",
      call = get_cli_abort_call()
    )
  }

  # {y} must appear once in the string and on the LHS of the formula
  if (length(unlist(regmatches(formula, m = gregexpr("{y}", formula, fixed = TRUE)))) != 1L ||
      length(unlist(regmatches(formula_split[[1]], m = gregexpr("{y}", formula_split[[1]], fixed = TRUE)))) != 1L) {
    cli::cli_abort(
      c("Error in argument {.arg formula} structure.",
        i = "The substring {.val {{y}}} must appear once in the string and it must be on the LHS of the formula."),
      call = get_cli_abort_call()
    )
  }

  # {x} must appear once in the string and on the RHS of the formula
  if (length(unlist(regmatches(formula, m = gregexpr("{x}", formula, fixed = TRUE)))) != 1L ||
      length(unlist(regmatches(formula_split[[2]], m = gregexpr("{x}", formula_split[[2]], fixed = TRUE)))) != 1L) {
    cli::cli_abort(
      c("Error in argument {.arg formula} structure.",
        i = "The substring {.val {{x}}} must appear once in the string and it must be on the RHS of the formula."),
      call = get_cli_abort_call()
    )
  }

  invisible(formula)
}

# convert whatever is passed in `x` and `y` to a string
.process_x_and_y_args_as_string <- function(data, x, arg_name = rlang::caller_arg(x)) {
  # if quo is empty, then return NULL
  if (is_quo_empty(x)) return(NULL) # styler: off

  # if a character was passed, return it as it
  if (tryCatch(is.character(eval_tidy(x)), error = \(e) FALSE)) return(eval_tidy(x)) # styler: off

  # try tidy evaluation, and if that doesn't work, then return string of input
  tryCatch(
    cards::cards_select(data = as.data.frame(data), expr = x) |> cardx::bt(),
    error = function(e) {
      tryCatch(
        # lastly, convert quosure to a string
        expr_deparse(quo_get_expr(x)),
        error = function(e) {
          cli::cli_abort(
            "There was a problem processing argument {.arg {arg_name}}.",
            call = get_cli_abort_call()
          )
        }
      )
    }
  )
}
