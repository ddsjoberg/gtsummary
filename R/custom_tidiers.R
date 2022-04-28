#' Collection of custom tidiers
#'
#' \lifecycle{maturing}
#' Collection of tidiers that can be passed to `tbl_regression()`
#' and `tbl_uvregression()` to obtain modified results. See examples below.
#'
#' @section Details:
#'
#' - `tidy_standardize()` tidier to report standardized coefficients. The
#' [effectsize](https://easystats.github.io/effectsize/reference/standardize_parameters.html)
#' package includes a wonderful function to estimate standardized coefficients.
#' The tidier uses the output from `effectsize::standardize_parameters()`, and
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
#' `tidy_robust()` populated. This is easily done using `purrr::partial()` e.g.
#' `tbl_regression(tidy_fun = partial(tidy_robust, vcov_estimation = "CL"))`
#'
#' - `pool_and_tidy_mice()` tidier to report models resulting from multiply imputed data
#' using the mice package. Pass the mice model object *before* the model results
#' have been pooled. See example.
#'
#' - `tidy_wald_test()` tidier to report Wald p-values, wrapping the
#'   `aod::wald.test()` function.
#'   Use this tidier with `add_global_p(anova_fun = tidy_wald_test)`
#'
#' Ensure your model type is compatible with the methods/functions used to estimate
#' the model parameters before attempting to use the tidier with `tbl_regression()`
#' @inheritParams broom::tidy.glm
#' @inheritParams tbl_regression
#' @inheritParams add_global_p
#' @param pool.args named list of arguments passed to `mice::pool()` in
#' `pool_and_tidy_mice()`. Default is `NULL`
#' @param vcov_estimation,vcov_type,vcov_args arguments passed to
#' `parameters::model_parameters()`
#' @param ... arguments passed to method;
#' - `pool_and_tidy_mice()`: `mice::tidy(x, ...)`
#' - `tidy_standardize()`: `effectsize::standardize_parameters(x, ...)`
#' - `tidy_bootstrap()`: `parameters::bootstrap_parameters(x, ...)`
#' - `tidy_robust()`: `parameters::model_parameters(x, ...)`
#'
#' @param x a regression model object
#' @name custom_tidiers
#' @rdname custom_tidiers
#' @export
#' @examplesIf broom.helpers::.assert_package("effectsize", pkg_search = "gtsummary", boolean = TRUE) && broom.helpers::.assert_package("parameters", pkg_search = "gtsummary", boolean = TRUE) && broom.helpers::.assert_package("mice", pkg_search = "gtsummary", boolean = TRUE)
#' \donttest{
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
#' tidy_standardize_ex2 <-
#'   tbl_regression(mod, tidy_fun = purrr::partial(tidy_standardize, method = "posthoc"))
#'
#' # Example 3 ----------------------------------
#' # Multiple Imputation using the mice package
#' set.seed(1123)
#' pool_and_tidy_mice_ex3 <-
#'   suppressWarnings(mice::mice(trial, m = 2)) %>%
#'   with(lm(age ~ marker + grade)) %>%
#'   tbl_regression()
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tidy_standardize_ex1.png}{options: width=65\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tidy_standardize_ex2.png}{options: width=47\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{pool_and_tidy_mice_ex3.png}{options: width=47\%}}

tidy_standardize <- function(x, exponentiate = FALSE,
                             conf.level = 0.95,
                             conf.int = TRUE,
                             ..., quiet = FALSE) {
  assert_package("effectsize", "tidy_standardize()")
  dots <- list(...)

  # calculating standardize coefs
  std_coef_expr <- expr(effectsize::standardize_parameters(model = x, ci = !!conf.level, !!!dots))
  if (quiet == FALSE) {
    inform(glue("tidy_standardize(): Estimating standardized coefs with\n  `{deparse(std_coef_expr, width.cutoff = 500L)}`"))
  }
  std_coef <-
    expr(effectsize::standardize_parameters(model = !!x, ci = !!conf.level, !!!dots)) %>%
    eval()

  # converting output to broom::tidy format
  tidy <-
    as_tibble(std_coef) %>%
    select(
      term = .data$Parameter, estimate = .data$Std_Coefficient,
      conf.low = .data$CI_low, conf.high = .data$CI_high
    )

  # exponentiate, if requested
  if (exponentiate) {
    tidy <- mutate_at(tidy, vars(.data$estimate, .data$conf.low, .data$conf.high), exp)
  }

  # removing conf int, if requested
  if (!conf.int) tidy <- select(tidy, -any_of(c("conf.low", "conf.high")))

  tidy
}

#' @rdname custom_tidiers
#' @export
tidy_bootstrap <- function(x, exponentiate = FALSE,
                           conf.level = 0.95,
                           conf.int = TRUE, ..., quiet = FALSE) {
  assert_package("parameters", "tidy_bootstrap()")
  dots <- list(...)

  # calculating bootstrapped coefs
  boot_coef_expr <- expr(parameters::bootstrap_parameters(model = x, ci = !!conf.level, test = "p", !!!dots))
  if (quiet == FALSE) {
    inform(glue("tidy_bootstrap(): Estimating bootstrapped coefs with\n  `{deparse(boot_coef_expr, width.cutoff = 500L)}`"))
  }
  boot_coef <-
    expr(parameters::bootstrap_parameters(model = !!x, ci = !!conf.level, test = "p", !!!dots)) %>%
    eval()

  # converting output to broom::tidy format
  tidy <-
    as_tibble(boot_coef) %>%
    select(
      term = .data$Parameter, estimate = .data$Coefficient,
      conf.low = .data$CI_low, conf.high = .data$CI_high, p.value = .data$p
    )

  # exponentiate, if requested
  if (exponentiate) {
    tidy <- mutate_at(tidy, vars(.data$estimate, .data$conf.low, .data$conf.high), exp)
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
                        vcov_estimation = NULL, #type of covariance matrix
                        vcov_type = NULL, #type of robust estimation
                        vcov_args = NULL, #specify the cluster-structure
                        ...,
                        quiet = FALSE) {
  assert_package("parameters", "tidy_robust()")
  assert_package("insight", "tidy_robust()")

  dots <- rlang::dots_list(...)
  lst_model_parameters_args <-
    rlang::inject(list(ci = !!conf.level,
                       robust = TRUE,
                       vcov_estimation = !!vcov_estimation,
                       vcov_type = !!vcov_type,
                       !!!dots)) %>%
    purrr::compact()

  # calculating robust coefs
  robust_coef_expr <-
    expr(parameters::model_parameters(model = x, !!!lst_model_parameters_args))
  if (quiet == FALSE) {
    glue("tidy_robust(): Robust estimation with\n",
         "  `{deparse(robust_coef_expr, width.cutoff = 500L)}`\n\n") %>%
      inform()
  }
  robust_coef <- eval(robust_coef_expr)

  # converting output to broom::tidy format ------------------------------------
  tidy <- insight::standardize_names(robust_coef, style = "broom")

  # exponentiate, if requested -------------------------------------------------
  if (exponentiate) {
    tidy <-
      mutate_at(tidy, vars(.data$estimate, .data$conf.low, .data$conf.high), exp)
  }

  # removing conf int, if requested --------------------------------------------
  if (!conf.int) {
    tidy <- select(tidy,-any_of(c("conf.low", "conf.high")))
  }

  # return results -------------------------------------------------------------
  tidy
}


#' @rdname custom_tidiers
#' @export
pool_and_tidy_mice <- function(x, pool.args = NULL, ..., quiet = FALSE) {
  assert_package("mice", "pool_and_tidy_mice()")
  if (!inherits(x, "mira")) stop("Object `x=` must be of class 'mira'.", call. = FALSE)

  dots <- list(...)

  # printing code that will run
  mice_expr <- expr(mice::pool(x, !!!pool.args) %>% mice::tidy(!!!dots))
  if (quiet == FALSE) {
    inform(glue("pool_and_tidy_mice(): Tidying mice model with\n  `{deparse(mice_expr, width.cutoff = 500L)}`"))
  }

  # evaluating tidy expression
  expr(mice::pool(!!x, !!!pool.args) %>% mice::tidy(!!!dots)) %>% eval()
}

#' @rdname custom_tidiers
#' @export
tidy_gam <- function(x, conf.int = FALSE, exponentiate = FALSE, conf.level = 0.95, ...) {
  broom::tidy(x,
              conf.int = conf.int,
              conf.level = conf.level,
              parametric = TRUE, ...
  ) %>%
    # exponentiate coefs (GAM tidier does not have an `exponentiate=` argument)
    dplyr::mutate_at(
      vars(any_of(c("estimate", "conf.low", "conf.high"))),
      ~ switch(exponentiate == TRUE,
               exp(.)
      ) %||% .
    ) %>%
    dplyr::mutate(parametric = TRUE) %>%
    dplyr::bind_rows(
      broom::tidy(x, parametric = FALSE, ...) %>%
        dplyr::mutate(parametric = FALSE)
    ) %>%
    dplyr::relocate(.data$parametric, .after = dplyr::last_col())
}

#' @rdname custom_tidiers
#' @export
tidy_wald_test <- function(x, tidy_fun = NULL, ...) {
  assert_package("aod", "tidy_wald_test()")

  tidy_fun <- tidy_fun %||% broom.helpers::tidy_with_broom_or_parameters

  # match model terms to the variable
  broom.helpers::tidy_and_attach(
    model = x,
    tidy_fun = tidy_fun
  ) %>%
    broom.helpers::tidy_identify_variables() %>%
    dplyr::select(term = .data$variable, model_terms = .data$term) %>%
    dplyr::mutate(term_id = dplyr::row_number()) %>%
    # reduce to one line per variable in model
    tidyr::nest(data = -.data$term) %>%
    dplyr::rowwise() %>%
    # calculate Wald test
    dplyr::mutate(
      model_terms = unlist(.data$data[["model_terms"]]) %>% list(),
      model_terms_id = rlang::set_names(.data$data[["term_id"]]) %>% list(),
      wald_test =
        aod::wald.test(
          Sigma = stats::vcov(x),
          b = stats::coef(x),
          Terms = .data$model_terms_id
        ) %>%
        list(),
      df = .data$wald_test$result$chi2 %>% purrr::pluck("df"),
      statistic = .data$wald_test$result$chi2 %>% purrr::pluck("chi2"),
      p.value = .data$wald_test$result$chi2 %>% purrr::pluck("P"),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$term, .data$df, .data$statistic, .data$p.value)
}
