#' Custom tidiers
#'
#' @section Details:
#'
#' - `tidy_standardize()` tidier to report standardized coefficients. The effectsize
#' package includes a wonderful function to estimate standardized coefficients.
#' The tidier uses the output from `effectsize::standardize_parameters()`, and
#' merely takes the result and puts it in `broom::tidy()` format.
#'
#' - `tidy_bootstrap()` tidier to report bootstrapped coefficients. The parameters
#' package includes a wonderful function to estimate bootstrapped coefficients.
#' The tidier uses the output from `parameters::bootstrap_parameters(test = "p")`, and
#' merely takes the result and puts it in `broom::tidy()` format.
#'
#' Ensure your model type is compatible with the methods/functions used to estimate
#' the model parameters before attempting to use the tidier with `tbl_regression()`
#' @inheritParams broom::tidy.glm
#' @param ... arguments passed to method; `effectsize::standardize_parameters()`
#' for `tidy_standardize()`, and `parameters::bootstrap_parameters()` for
#' `tidy_bootstrap()`
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
#' tidy_bootstrap_ex2 <-
#'   tbl_regression(mod, tidy_fun = tidy_bootstrap)
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tidy_standardize_ex1.png}{options: width=65\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tidy_bootstrap_ex2.png}{options: width=47\%}}

tidy_standardize <- function(x, exponentiate = FALSE,
                             conf.level = 0.95,
                             conf.int = TRUE, ...) {
  assert_package("effectsize", "tidy_standardize")

  # converting output to broom::tidy format
  tidy <-
    effectsize::standardize_parameters(x, ci = conf.level, ...) %>%
    as_tibble() %>%
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
                           conf.int = TRUE, ...) {
  assert_package("parameters", "bootstrap_parameters")

  # converting output to broom::tidy format
  tidy <-
    parameters::bootstrap_parameters(x, ci = conf.level, test = "p", ...) %>%
    as_tibble() %>%
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
