#' Regression model summary
#'
#' @description
#' This function takes a regression model object and returns a formatted table
#' that is publication-ready. The function is customizable
#' allowing the user to create bespoke regression model summary tables.
#' Review the
#' [`tbl_regression()` vignette](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
#' for detailed examples.
#'
#' @param x (regression model)\cr
#'   Regression model object
#' @param exponentiate (scalar `logical`)\cr
#'   Logical indicating whether to exponentiate the coefficient estimates.
#'   Default is `FALSE`.
#' @param label ([`formula-list-selector`][syntax])\cr
#'   Used to change variables labels, e.g. `list(age = "Age", stage = "Path T Stage")`
#' @param include ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to include in output. Default is `everything()`.
#' @param conf.level (scalar `real`)\cr
#'   Confidence level for confidence interval/credible interval. Defaults to `0.95`.
#' @param intercept (scalar `logical`)\cr
#'   Indicates whether to include the intercept in the output.  Default is `FALSE`
#' @param show_single_row ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   By default categorical variables are printed on multiple rows.
#'   If a variable is dichotomous (e.g. Yes/No) and you wish to print the
#'   regression coefficient on a single row, include the variable name(s) here.
#' @param estimate_fun (`function`)\cr
#'   Function to round and format coefficient estimates.
#'   Default is [`label_style_sigfig()`] when the coefficients are not transformed, and
#'   [`label_style_ratio()`] when the coefficients have been exponentiated.
#' @param pvalue_fun (`function`)\cr
#'   Function to round and format p-values. Default is [`label_style_pvalue()`].
#' @param tidy_fun (`function`)\cr
#'   Tidier function for the model. Default is to use `broom::tidy()`.
#'   If an error occurs, the tidying of the model is attempted with
#'   `parameters::model_parameters()`, if installed.
#' @param add_estimate_to_reference_rows (scalar `logical`)\cr
#'   Add a reference value. Default is `FALSE`.
#' @param conf.int (scalar `logical`)\cr
#'   Logical indicating whether or not to include a confidence
#'   interval in the output. Default is `TRUE`.
#' @param ... Additional arguments passed to [`broom.helpers::tidy_plus_plus()`].
#'
#' @section Methods:
#'
#' The default method for `tbl_regression()` model summary uses `broom::tidy(x)`
#' to perform the initial tidying of the model object. There are, however,
#' a few models that use [modifications][tbl_regression_methods].
#'
#' - `"parsnip/workflows"`: If the model was prepared using parsnip/workflows,
#'   the original model fit is extracted and the original `x=` argument
#'   is replaced with the model fit. This will typically go unnoticed; however,if you've
#'   provided a custom tidier in `tidy_fun=` the tidier will be applied to the model
#'   fit object and not the parsnip/workflows object.
#'
#' - `"survreg"`: The scale parameter is removed, `broom::tidy(x) %>% dplyr::filter(term != "Log(scale)")`
#'
#' - `"multinom"`: This multinomial outcome is complex, with one line per covariate per outcome (less the reference group)
#'
#' - `"gam"`: Uses the internal tidier `tidy_gam()` to print both parametric and smooth terms.
#'
#' - `"lmerMod"`, `"glmerMod"`, `"glmmTMB"`, `"glmmadmb"`, `"stanreg"`, `"brmsfit"`: These mixed effects
#'   models use `broom.mixed::tidy(x, effects = "fixed")`. Specify `tidy_fun = broom.mixed::tidy`
#'   to print the random components.
#'
#' @author Daniel D. Sjoberg
#'
#' @name tbl_regression
#' @return A `tbl_regression` object
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")) && gtsummary:::is_pkg_installed(c("cardx", "broom", "broom.helpers"))
#' # Example 1 ----------------------------------
#' glm(response ~ age + grade, trial, family = binomial()) |>
#'   tbl_regression(exponentiate = TRUE)
NULL

#' @rdname tbl_regression
#' @export
tbl_regression <- function(x, ...) {
  check_pkg_installed(c("broom", "broom.helpers"))
  check_not_missing(x)
  UseMethod("tbl_regression")
}

#' @rdname tbl_regression
#' @export
tbl_regression.default <- function(x,
                                   label = NULL,
                                   exponentiate = FALSE,
                                   include = everything(),
                                   show_single_row = NULL,
                                   conf.level = 0.95,
                                   intercept = FALSE,
                                   estimate_fun = ifelse(exponentiate, label_style_ratio(), label_style_sigfig()),
                                   pvalue_fun = label_style_pvalue(digits = 1),
                                   tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
                                   add_estimate_to_reference_rows = FALSE,
                                   conf.int = TRUE, ...) {
  set_cli_abort_call()

  # setting theme defaults -----------------------------------------------------
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

  if (missing(conf.level)) {
    conf.level <- get_theme_element("tbl_regression-arg:conf.level", default = conf.level)
  }

  if (missing(conf.int)) {
    conf.int <- get_theme_element("tbl_regression-arg:conf.int", default = conf.int)
  }

  if (missing(add_estimate_to_reference_rows)) {
    add_estimate_to_reference_rows <-
      get_theme_element("tbl_regression-arg:add_estimate_to_reference_rows", default = add_estimate_to_reference_rows)
  }

  # check inputs ---------------------------------------------------------------
  check_scalar_logical(intercept)
  check_scalar_logical(add_estimate_to_reference_rows)
  check_scalar_logical(conf.int)
  check_scalar_range(conf.level, range = c(0, 1))

  # quote inputs and save argument values --------------------------------------
  include <- enquo(include)
  show_single_row <- enquo(show_single_row)
  func_inputs <- as.list(environment())

  # build table_body -----------------------------------------------------------
  table_body <-
    tidy_prep(
      x,
      tidy_fun = tidy_fun,
      exponentiate = exponentiate,
      conf.level = conf.level,
      intercept = intercept,
      label = label,
      show_single_row = !!show_single_row,
      include = !!include,
      add_estimate_to_reference_rows = add_estimate_to_reference_rows,
      conf.int = conf.int,
      ...
    ) |>
    dplyr::relocate(any_of(c("conf.low", "conf.high", "p.value")), .after = last_col())

  # saving evaluated `label`, `show_single_row`, and `include` -----------------
  cards::process_selectors(
    data = scope_table_body(table_body),
    show_single_row = !!show_single_row,
    include = !!include
  )
  cards::process_formula_selectors(
    data = scope_table_body(table_body),
    label = label
  )
  func_inputs <-
    utils::modifyList(
      func_inputs,
      val = list(include = include, show_single_row = show_single_row, label = label),
      keep.null = TRUE
    )

  # construct initial gtsummary object -----------------------------------------
  res <-
    .create_gtsummary_object(table_body) |>
    utils::modifyList(
      list(
        N = table_body[["N_obs"]][1],
        N_event = table_body[["N_event"]][1],
        inputs = func_inputs,
        call_list = list(tbl_regression = match.call())
      ),
      keep.null = FALSE
    ) |>
    structure(class = c("tbl_regression", "gtsummary"))

  # setting column headers, and print instructions
  tidy_columns_to_report <-
    get_deprecated_theme_element("tbl_regression-chr:tidy_columns",
                                 default = c("conf.low", "conf.high", "p.value")) |>
    union("estimate") |>
    intersect(names(table_body))

  # setting default table_header values
  res <-
    .tbl_regression_default_table_header(
      res,
      exponentiate = exponentiate,
      tidy_columns_to_report = tidy_columns_to_report,
      estimate_fun = estimate_fun,
      pvalue_fun = pvalue_fun,
      conf.level = conf.level
    )

  # adding the Ns to the `x$table_styling$header`
  if (!rlang::is_empty(res[c("N", "N_event")] |> compact())) {
    res$table_styling$header <-
      res[c("N", "N_event")] |>
      compact() |>
      dplyr::as_tibble() |>
      dplyr::rename_with(.fn = ~ vec_paste0("modify_stat_", .), .cols = everything()) |>
      dplyr::cross_join(
        res$table_styling$header
      ) |>
      dplyr::relocate(starts_with("modify_stat_"), .after = last_col())
  }


  # running any additional mods ------------------------------------------------
  res <-
    get_theme_element("tbl_regression-fn:addnl-fn-to-run", default = identity) |>
    do.call(list(res))

  # return results -------------------------------------------------------------
  res <- res |>
    modify_table_styling(
      columns = "label",
      rows = .data$row_type %in% c("level", "missing"),
      indent = 4
    )
  res$call_list <- list(tbl_regression = match.call())
  res
}
