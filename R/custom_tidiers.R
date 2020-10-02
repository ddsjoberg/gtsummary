#' Collection of custom tidiers
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
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
#' Ensure your model type is compatible with the methods/functions used to estimate
#' the model parameters before attempting to use the tidier with `tbl_regression()`
#' @inheritParams broom::tidy.glm
#' @inheritParams add_global_p.tbl_regression
#' @param ... arguments passed to method;
#' - `tidy_standardize()`: `effectsize::standardize_parameters(x, ...)`
#' - `tidy_bootstrap()`: `parameters::bootstrap_parameters(x, ...)`
#'
#' @param x a regression model object
#' @name custom_tidiers
#' @rdname custom_tidiers
#' @export
#' @examples
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
#'
#' # Example 3 ----------------------------------
#' tidy_bootstrap_ex3 <-
#'   tbl_regression(mod, tidy_fun = tidy_bootstrap)
#'
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
#' \if{html}{\figure{tidy_bootstrap_ex3.png}{options: width=47\%}}

tidy_standardize <- function(x, exponentiate = FALSE,
                             conf.level = 0.95,
                             conf.int = TRUE,
                             quiet = FALSE, ...) {
  assert_package("effectsize", "tidy_standardize")
  dots <- list(...)

  # calculating standardize coefs
  std_coef_expr <- expr(effectsize::standardize_parameters(model = x, ci = !!conf.level, !!!dots))
  if (quiet == FALSE)
    inform(glue("tidy_standardize: Estimating standardized coefs with\n  `{deparse(std_coef_expr)}`"))
  std_coef <-
    expr(effectsize::standardize_parameters(model = !!x, ci = !!conf.level, !!!dots)) %>%
    eval()

  # converting output to broom::tidy format
  tidy <-
    as_tibble(std_coef) %>%
    select(term = .data$Parameter, estimate = .data$Std_Coefficient,
           conf.low = .data$CI_low, conf.high = .data$CI_high)

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
  assert_package("parameters", "bootstrap_parameters")
  dots <- list(...)

  # calculating bootstrapped coefs
  boot_coef_expr <- expr(parameters::bootstrap_parameters(model = x, ci = !!conf.level, test = "p", !!!dots))
  if (quiet == FALSE)
    inform(glue("tidy_bootstrap: Estimating bootstrapped coefs with\n  `{deparse(boot_coef_expr)}`"))
  boot_coef <-
    expr(parameters::bootstrap_parameters(model = !!x, ci = !!conf.level, test = "p", !!!dots)) %>%
    eval()

  # converting output to broom::tidy format
  tidy <-
    as_tibble(boot_coef) %>%
    select(term = .data$Parameter, estimate = .data$Coefficient,
           conf.low = .data$CI_low, conf.high = .data$CI_high, p.value = .data$p)

  # exponentiate, if requested
  if (exponentiate) {
    tidy <- mutate_at(tidy, vars(.data$estimate, .data$conf.low, .data$conf.high), exp)
  }

  # removing conf int, if requested
  if (!conf.int) tidy <- select(tidy, -any_of(c("conf.low", "conf.high")))

  tidy
}
