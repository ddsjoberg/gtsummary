#' Display regression model results in table
#'
#' This function takes a regression model object and returns a formatted table
#' that is publication-ready. The function is highly customizable
#' allowing the user to obtain a bespoke summary table of the
#' regression model results. Review the
#' \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html}{tbl_regression vignette}
#' for detailed examples.
#'
#' @section Methods:
#'
#' The default method for `tbl_regression()` model summary uses `broom::tidy(x)`
#' to perform the initial tidying of the model object. There are, however,
#' a few models that use [modifications][tbl_regression_methods].
#'
#' - `"parsnip/workflows"`: If the model was prepared using parsnip/workflows,
#' the original model fit is extracted and the original `x=` argument
#' is replaced with the model fit. This will typically go unnoticed; however,if you've
#' provided a custom tidier in `tidy_fun=` the tidier will be applied to the model
#' fit object and not the parsnip/workflows object.
#' - `"survreg"`: The scale parameter is removed, `broom::tidy(x) %>% dplyr::filter(term != "Log(scale)")`
#' - `"multinom"`: This multinomial outcome is complex, with one line per covariate per outcome (less the reference group)
#' - `"gam"`: Uses the internal tidier `tidy_gam()` to print both parametric and smooth terms.
#' - `"tidycrr"`: Uses the tidier `tidycmprsk::tidy()` to print the model terms.
#' - `"lmerMod"`, `"glmerMod"`, `"glmmTMB"`, `"glmmadmb"`, `"stanreg"`, `"brmsfit"`: These mixed effects
#' models use `broom.mixed::tidy(x, effects = "fixed")`. Specify `tidy_fun = broom.mixed::tidy`
#' to print the random components.
#'
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
#' @param tidy_fun Option to specify a particular tidier function for the
#' model. Default is to use `broom::tidy()`, but if an error occurs
#' then tidying of the model is attempted with `parameters::model_parameters()`,
#' if installed.
#' @param add_estimate_to_reference_rows add a reference value. Default is FALSE
#' @param conf.int Logical indicating whether or not to include a confidence
#' interval in the output. Defaults to `TRUE`.
#' @param ... \lifecycle{experimental}Additional arguments passed to [broom.helpers::tidy_plus_plus()].
#' See `?tidy_plus_plus_dots` for details.
#' @author Daniel D. Sjoberg
#' @seealso See tbl_regression \href{https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html}{vignette} for detailed examples
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @family tbl_regression tools
#' @export
#' @rdname tbl_regression
#' @return A `tbl_regression` object
#' @examples
#' \donttest{
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
#' # round all estimates to 3 decimal places
#' suppressMessages(library(lme4))
#' tbl_regression_ex3 <-
#'   lmer(hp ~ am + (1 | gear), data = mtcars) %>%
#'   tbl_regression(estimate_fun = function(x) style_number(x, digits = 3))
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_regression_ex1.png", width = "64")`
#' }}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_regression_ex2.png", width = "50")`
#' }}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "tbl_regression_ex3.png", width = "50")`
#' }}

tbl_regression <- function(x, ...) {
  UseMethod("tbl_regression")
}

#' @export
#' @rdname tbl_regression
tbl_regression.default <- function(x, label = NULL, exponentiate = FALSE,
                                   include = everything(), show_single_row = NULL,
                                   conf.level = NULL, intercept = FALSE,
                                   estimate_fun = NULL, pvalue_fun = NULL,
                                   tidy_fun = NULL,
                                   add_estimate_to_reference_rows = FALSE,
                                   conf.int = NULL, ...) {
  # setting defaults -----------------------------------------------------------
  tidy_fun <- tidy_fun %||% broom.helpers::tidy_with_broom_or_parameters
  pvalue_fun <-
    pvalue_fun %||%
    get_theme_element("tbl_regression-arg:pvalue_fun") %||%
    get_theme_element("pkgwide-fn:pvalue_fun") %||%
    .get_deprecated_option("gtsummary.pvalue_fun", default = style_pvalue) %>%
    gts_mapper("tbl_regression(pvalue_fun=)")
  estimate_fun <-
    estimate_fun %||%
    get_theme_element("tbl_regression-arg:estimate_fun") %||%
    .get_deprecated_option(
      "gtsummary.tbl_regression.estimate_fun",
      default = ifelse(exponentiate == TRUE, style_ratio, style_sigfig)
    ) %>%
    gts_mapper("tbl_regression(estimate_fun=)")
  conf.level <-
    conf.level %||%
    get_theme_element("tbl_regression-arg:conf.level") %||%
    .get_deprecated_option("gtsummary.conf.level", default = 0.95)
  conf.int <-
    conf.int %||%
    get_theme_element("tbl_regression-arg:conf.int", default = TRUE)
  add_estimate_to_reference_rows <-
    add_estimate_to_reference_rows %||%
    get_theme_element("tbl_regression-arg:add_estimate_to_reference_rows", default = FALSE)


  # checking estimate_fun and pvalue_fun are functions
  if (!purrr::every(list(estimate_fun, pvalue_fun, tidy_fun %||% pvalue_fun), is.function)) {
    stop("Inputs `estimate_fun`, `pvalue_fun`, `tidy_fun` must be functions.",
      call. = FALSE
    )
  }

  include <- rlang::enquo(include)
  show_single_row <- rlang::enquo(show_single_row)

  # will return call, and all object passed to in tbl_regression call
  # the object func_inputs is a list of every object passed to the function
  func_inputs <- as.list(environment())

  table_body <-
    tidy_prep(x,
      tidy_fun = tidy_fun, exponentiate = exponentiate,
      conf.level = conf.level, intercept = intercept,
      label = label, show_single_row = !!show_single_row,
      include = !!include,
      add_estimate_to_reference_rows = add_estimate_to_reference_rows,
      conf.int = conf.int,
      ...
    )

  # saving evaluated `label`, `show_single_row`, and `include` -----------------
  func_inputs$label <-
    .formula_list_to_named_list(
      x = label,
      var_info = table_body,
      arg_name = "label",
      type_check = chuck(type_check, "is_string", "fn"),
      type_check_msg = chuck(type_check, "is_string", "msg")
    )

  func_inputs$show_single_row <-
    .select_to_varnames(
      select = !!show_single_row,
      var_info = table_body,
      arg_name = "show_single_row"
    )

  func_inputs$include <- unique(table_body$variable)

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
  x <-
    .create_gtsummary_object(table_body = table_body) %>%
    purrr::list_modify(
      N = pluck(table_body, "N_obs", 1),
      n = pluck(table_body, "N_obs", 1), # i want to remove this eventually
      N_event = pluck(table_body, "N_event", 1), model_obj = x,
      inputs = func_inputs,
      call_list = list(tbl_regression = match.call())
    ) %>%
    purrr::discard(is.null)

  # assigning a class of tbl_regression (for special printing in R markdown)
  class(x) <- c("tbl_regression", "gtsummary")

  # setting column headers, and print instructions
  tidy_columns_to_report <-
    get_theme_element("tbl_regression-chr:tidy_columns",
      default = c("conf.low", "conf.high", "p.value")
    ) %>%
    union("estimate") %>%
    intersect(names(table_body))

  # setting default table_header values
  x <-
    .tbl_regression_default_table_header(
      x,
      exponentiate = exponentiate,
      tidy_columns_to_report = tidy_columns_to_report,
      estimate_fun = estimate_fun,
      pvalue_fun = pvalue_fun,
      conf.level = conf.level
    )

  # adding the Ns to the `x$table_styling$header`
  if (!rlang::is_empty(x[c("N", "n", "N_event")] %>% purrr::compact())) {
    x$table_styling$header <-
      x[c("N", "n", "N_event")] %>%
      purrr::compact() %>%
      as_tibble() %>%
      dplyr::rename_with(.fn = ~ vec_paste0("modify_stat_", .), .cols = everything()) %>%
      dplyr::cross_join(
        x$table_styling$header
      ) %>%
      dplyr::relocate(starts_with("modify_stat_"), .after = last_col())
  }


  # running any additional mods ------------------------------------------------
  x <-
    get_theme_element("tbl_regression-fn:addnl-fn-to-run", default = identity) %>%
    do.call(list(x))

  # return results -------------------------------------------------------------
  x
}
