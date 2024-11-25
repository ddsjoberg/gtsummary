#' Custom tidiers
#'
#' @description
#' \lifecycle{maturing}
#' Collection of tidiers that can be utilized in gtsummary. See details below.
#'
#' # Regression Model Tidiers
#'
#' These tidiers are passed to `tbl_regression()` and `tbl_uvregression()` to
#' obtain modified results.
#'
#' - `tidy_standardize()` tidier to report standardized coefficients. The
#' [parameters](https://easystats.github.io/parameters/reference/standardize_parameters.html)
#' package includes a wonderful function to estimate standardized coefficients.
#' The tidier uses the output from `parameters::standardize_parameters()`, and
#' merely takes the result and puts it in `broom::tidy()` format.
#'
#' - `tidy_bootstrap()` tidier to report bootstrapped coefficients. The
#' [parameters](https://easystats.github.io/parameters/reference/model_parameters.default.html)
#' package includes a wonderful function to estimate bootstrapped coefficients.
#' The tidier uses the output from `parameters::bootstrap_parameters(test = "p")`, and
#' merely takes the result and puts it in `broom::tidy()` format.
#'
#' - `tidy_robust()` tidier to report robust standard errors, confidence intervals,
#' and p-values. The [parameters](https://easystats.github.io/parameters/reference/model_parameters.default.html)
#' package includes a wonderful function to calculate robust standard errors, confidence intervals, and p-values
#' The tidier uses the output from `parameters::model_parameters()`, and
#' merely takes the result and puts it in `broom::tidy()` format. To use this
#' function with `tbl_regression()`, pass a function with the arguments for
#' `tidy_robust()` populated.
#'
#' - `pool_and_tidy_mice()` tidier to report models resulting from multiply imputed data
#' using the mice package. Pass the mice model object *before* the model results
#' have been pooled. See example.
#'
#' # Other Tidiers
#'
#' - `tidy_wald_test()` tidier to report Wald p-values, wrapping the
#'   `aod::wald.test()` function.
#'   Use this tidier with `add_global_p(anova_fun = tidy_wald_test)`
#'
#' @inheritParams tbl_regression
#' @inheritParams broom::tidy.glm
#'
#' @param x (`model`)\cr
#'   Regression model object
#' @param pool.args (named `list`)\cr
#'   Named list of arguments passed to `mice::pool()` in
#'   `pool_and_tidy_mice()`. Default is `NULL`
#' @param vcov,vcov_args
#'  - `tidy_robust()`: Arguments passed to `parameters::model_parameters()`.
#'                     At least one of these arguments **must** be specified.
#'  - `tidy_wald_test()`: `vcov` is the covariance matrix of the model with default `stats::vcov()`.
#' @param quiet `r lifecycle::badge("deprecated")`
#' @param ...
#' Arguments passed to method;
#' - `pool_and_tidy_mice()`: `mice::tidy(x, ...)`
#' - `tidy_standardize()`: `parameters::standardize_parameters(x, ...)`
#' - `tidy_bootstrap()`: `parameters::bootstrap_parameters(x, ...)`
#' - `tidy_robust()`: `parameters::model_parameters(x, ...)`
#'
#' @name custom_tidiers
#' @rdname custom_tidiers
#' @export
#' @examplesIf gtsummary:::is_pkg_installed(c("effectsize", "mice", "parameters"))
#' # Example 1 ----------------------------------
#' mod <- lm(age ~ marker + grade, trial)
#'
#' tbl_stnd <- tbl_regression(mod, tidy_fun = tidy_standardize)
#' tbl <- tbl_regression(mod)
#'
#' tidy_standardize_ex1 <-
#'   tbl_merge(
#'     list(tbl_stnd, tbl),
#'     tab_spanner = c("**Standardized Model**", "**Original Model**")
#'   )
#'
#' # Example 2 ----------------------------------
#' # use "posthoc" method for coef calculation
#' tbl_regression(mod, tidy_fun = \(x, ...) tidy_standardize(x, method = "posthoc", ...))
#'
#' # Example 3 ----------------------------------
#' # Multiple Imputation using the mice package
#' set.seed(1123)
#' pool_and_tidy_mice_ex3 <-
#'   suppressWarnings(mice::mice(trial, m = 2)) |>
#'   with(lm(age ~ marker + grade)) |>
#'   tbl_regression()
tidy_standardize <- function(x,
                             exponentiate = FALSE,
                             conf.level = 0.95,
                             conf.int = TRUE,
                             ...,
                             quiet = FALSE) {
  set_cli_abort_call()
  check_pkg_installed(c("parameters", "effectsize"))
  check_scalar_logical(exponentiate)
  check_scalar_logical(conf.int)
  check_scalar_range(conf.level, range = c(0, 1))

  if (!missing(quiet)) {
    lifecycle::deprecate_stop(
      when = "2.0.0",
      what = "gtsummary::tidy_standardize(quiet)",
      details = "Argument has been ignored."
    )
  }

  # calculating standardize coefs
  std_coef <- parameters::standardize_parameters(model = x, ci = conf.level, ...)

  # converting output to broom::tidy format
  tidy <-
    as_tibble(std_coef) %>%
    select(
      term = "Parameter", estimate = "Std_Coefficient",
      conf.low = "CI_low", conf.high = "CI_high"
    )

  # exponentiate, if requested
  if (exponentiate) {
    tidy <- dplyr::mutate_at(tidy, dplyr::vars("estimate", "conf.low", "conf.high"), exp)
  }

  # removing conf int, if requested
  if (!conf.int) tidy <- dplyr::select(tidy, -any_of(c("conf.low", "conf.high")))

  tidy
}

#' @rdname custom_tidiers
#' @export
tidy_bootstrap <- function(x,
                           exponentiate = FALSE,
                           conf.level = 0.95,
                           conf.int = TRUE,
                           ...,
                           quiet = FALSE) {
  set_cli_abort_call()
  check_pkg_installed("parameters")
  check_scalar_logical(exponentiate)
  check_scalar_logical(conf.int)
  check_scalar_range(conf.level, range = c(0, 1))
  if (!missing(quiet)) {
    lifecycle::deprecate_stop(
      when = "2.0.0",
      what = "gtsummary::tidy_standardize(quiet)",
      details = "Argument has been ignored."
    )
  }

  # calculating bootstrapped coefs
  boot_coef <- parameters::bootstrap_parameters(model = x, ci = conf.level, test = "p", ...)

  # converting output to broom::tidy format
  tidy <-
    as_tibble(boot_coef) %>%
    select(
      term = "Parameter", estimate = "Coefficient",
      conf.low = "CI_low", conf.high = "CI_high", p.value = "p"
    )

  # exponentiate, if requested
  if (exponentiate) {
    tidy <- dplyr::mutate_at(tidy, vars("estimate", "conf.low", "conf.high"), exp)
  }

  # removing conf int, if requested
  if (!conf.int) tidy <- select(tidy, -any_of(c("conf.low", "conf.high")))

  tidy
}


#' @rdname custom_tidiers
#' @export
tidy_robust <- function(x,
                        exponentiate = FALSE,
                        conf.level = 0.95,
                        conf.int = TRUE,
                        vcov = NULL, # type of robust estimation
                        vcov_args = NULL, # specify the cluster-structure
                        ...,
                        quiet = FALSE) {
  set_cli_abort_call()
  check_pkg_installed(c("parameters", "insight"))
  check_scalar_logical(exponentiate)
  check_scalar_logical(conf.int)
  check_scalar_range(conf.level, range = c(0, 1))
  if (!missing(quiet)) {
    lifecycle::deprecate_stop(
      when = "2.0.0",
      what = "gtsummary::tidy_standardize(quiet)",
      details = "Argument has been ignored."
    )
  }

  if (is.null(vcov) && is.null(vcov_args)) {
    cli::cli_inform(
      c(i = "Arguments {.code vcov} and {.code vcov_args} have not been specified
        in {.code tidy_robust()}. Specify at least one to obtain robust standard errors.")
    )
  }

  dots <- rlang::dots_list(...)
  lst_model_parameters_args <-
    inject(list(
      ci = !!conf.level,
      vcov = !!vcov,
      vcov_args = !!vcov_args,
      !!!dots
    )) |>
    compact()

  # calculating robust coefs
  robust_coef <-
    inject(parameters::model_parameters(model = x, !!!lst_model_parameters_args))

  # converting output to broom::tidy format ------------------------------------
  tidy <- insight::standardize_names(robust_coef, style = "broom")

  # exponentiate, if requested -------------------------------------------------
  if (exponentiate) {
    tidy <-
      dplyr::mutate_at(tidy, vars("estimate", "conf.low", "conf.high"), exp)
  }

  # removing conf int, if requested --------------------------------------------
  if (!conf.int) {
    tidy <- select(tidy, -any_of(c("conf.low", "conf.high")))
  }

  # return results -------------------------------------------------------------
  tidy
}


#' @rdname custom_tidiers
#' @export
pool_and_tidy_mice <- function(x, pool.args = NULL, ..., quiet = FALSE) {
  set_cli_abort_call()
  check_pkg_installed("mice")

  if (!missing(quiet)) {
    lifecycle::deprecate_stop(
      when = "2.0.0",
      what = "gtsummary::tidy_standardize(quiet)",
      details = "Argument has been ignored."
    )
  }

  if (!inherits(x, "mira")) {
    cli::cli_abort(
      "Argument {.arg x} must be class {.cls mira}.",
      call = get_cli_abort_call()
    )
  }

  dots <- list(...)

  # evaluating tidy expression
  inject(mice::pool(!!x, !!!pool.args) %>% mice::tidy(!!!dots))
}

#' @rdname custom_tidiers
#' @export
tidy_gam <- function(x, conf.int = FALSE, exponentiate = FALSE, conf.level = 0.95, ...) {
  set_cli_abort_call()
  check_scalar_logical(exponentiate)
  check_scalar_logical(conf.int)
  check_scalar_range(conf.level, range = c(0, 1))

  dplyr::bind_rows(
    suppressWarnings(
      broom::tidy(x, exponentiate = exponentiate, conf.int = conf.int,
                  conf.level = conf.level, parametric = TRUE, ...)
    ) |>
      dplyr::mutate(parametric = TRUE),
    broom::tidy(x, parametric = FALSE, ...)  |>
      dplyr::mutate(parametric = FALSE)
  ) |>
    dplyr::relocate("parametric", .after = dplyr::last_col())
}

#' @rdname custom_tidiers
#' @export
tidy_wald_test <- function(x, tidy_fun = NULL, vcov = stats::vcov(x), ...) {
  set_cli_abort_call()
  check_pkg_installed(c("aod", "broom.helpers"))

  tidy_fun <- tidy_fun %||% broom.helpers::tidy_with_broom_or_parameters

  # match model terms to the variable
  broom.helpers::tidy_and_attach(
    model = x,
    tidy_fun = tidy_fun
  ) %>%
    broom.helpers::tidy_identify_variables() %>%
    dplyr::select(term = "variable", model_terms = "term") %>%
    dplyr::mutate(term_id = dplyr::row_number()) %>%
    # reduce to one line per variable in model
    tidyr::nest(data = -"term") %>%
    dplyr::rowwise() %>%
    # calculate Wald test
    dplyr::mutate(
      model_terms = unlist(.data$data[["model_terms"]]) %>% list(),
      model_terms_id = rlang::set_names(.data$data[["term_id"]]) %>% list(),
      wald_test =
        aod::wald.test(
          Sigma = vcov,
          b = stats::coef(x),
          Terms = .data$model_terms_id
        ) %>%
        list(),
      df = .data$wald_test$result$chi2 %>% getElement("df"),
      statistic = .data$wald_test$result$chi2 %>% getElement("chi2"),
      p.value = .data$wald_test$result$chi2 %>% getElement("P"),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("term", "df", "statistic", "p.value")
}
