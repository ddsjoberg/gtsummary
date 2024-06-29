#' @title Methods for tbl_regression
#'
#' @description Most regression models are handled by [`tbl_regression()`],
#' which uses [`broom::tidy()`] to perform initial tidying of results. There are,
#' however, some model types that have modified default printing behavior.
#' Those methods are listed below.
#'
#' @param ... arguments passed to `tbl_regression()`
#' @inheritParams tbl_regression
#' @inheritParams tbl_stack
#'
#' @name tbl_regression_methods
#' @keywords internal
#'
#' @inheritSection tbl_regression Methods
NULL

#' @export
#' @rdname tbl_regression_methods
tbl_regression.model_fit <- function(x, ...) {
  message("Extracting {parsnip} model fit with `tbl_regression(x = x$fit, ...)`")
  tbl_regression(x = x$fit, ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.workflow <- function(x, ...) {
  check_pkg_installed("workflows", reference_pkg = "gtsummary")

  if (isTRUE(!x$pre$actions$formula$blueprint$indicators %in% "none")) {
    cli::cli_inform(
      c( i = "To take full advantage of model formatting, e.g. grouping categorical
      variables, please add the following argument to the {.fun workflows::add_model} call:",
        "*" = "{.code blueprint = hardhat::default_formula_blueprint(indicators = 'none')}")
    )
  }

  tbl_regression(x = workflows::extract_fit_parsnip(x), ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.survreg <- function(x, tidy_fun = function(x, ...) broom::tidy(x, ...) |> dplyr::filter(.data$term != "Log(scale)"), ...) {
  tbl_regression.default(x = x, tidy_fun = tidy_fun, ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.mira <- function(x, tidy_fun = pool_and_tidy_mice, ...) {
  tbl <- tbl_regression.default(x = x, tidy_fun = tidy_fun, ...)

  # adding outcome levels to multinomial models
  if (inherits(x$analyses[[1]], "multinom")) {
    tbl <- .multinom_modifations(tbl)
  }

  tbl
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.mipo <- function(x, ...) {
  cli::cli_inform(
    c("i" = "Pass the {.cls mice} model to {.code tbl_regression()} before
             models have been combined with {.fun mice::pool}.",
       "*" = "The default tidier, {.fun pool_and_tidy_mice},
              will both pool and tidy the regression model.",
      "*" = "{.run mice::mice(trial, m = 2) |> with(lm(age ~ marker + grade)) |> tbl_regression()}")
  )
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.lmerMod <- function(x, tidy_fun = \(x, ...) broom.mixed::tidy(x, ..., effects = "fixed"), ...) {
  check_pkg_installed("broom.mixed", reference_pkg = "gtsummary")
  tbl_regression.default(x = x, tidy_fun = tidy_fun, ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.glmerMod <- tbl_regression.lmerMod

#' @export
#' @rdname tbl_regression_methods
tbl_regression.glmmTMB <- tbl_regression.lmerMod

#' @export
#' @rdname tbl_regression_methods
tbl_regression.glmmadmb <- tbl_regression.lmerMod

#' @export
#' @rdname tbl_regression_methods
tbl_regression.stanreg <- tbl_regression.lmerMod

#' @export
#' @rdname tbl_regression_methods
tbl_regression.brmsfit <- tbl_regression.lmerMod

#' @export
#' @rdname tbl_regression_methods
tbl_regression.gam <- function(x, tidy_fun = tidy_gam, ...) {
  tbl_regression.default(x = x, tidy_fun = tidy_fun, ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.tidycrr <- function(x, tidy_fun = tidycmprsk::tidy, ...) {
  check_pkg_installed("tidycmprsk", reference_pkg = "gtsummary")
  tbl_regression.default(x = x, tidy_fun = tidy_fun, ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.crr <- function(x, ...) {
  cli::cli_inform(
    c("For better summary support, build model with {.fun tidycmprsk::crr}.",
      "Visit {.url https://mskcc-epi-bio.github.io/tidycmprsk/} for details.")
  )

  tbl_regression.default(x = x, ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.multinom <- function(x, ...) {
  result <- tbl_regression.default(x = x, ...)

  # grouping by outcome, and printing warning message
  .multinom_modifations(result)
}

.multinom_modifations <- function(x) {
  # adding a grouped header for the outcome levels
  x$table_body <-
    x$table_body |>
    mutate(groupname_col = .data$y.level, .before = 1L)

  x <- modify_table_styling(
      x = x,
      columns = all_of("groupname_col"),
      hide = FALSE,
      label = "**Outcome**",
      align = "left"
    )

  # warning about multinomial models
  cli::cli_inform(
    c("i" = "Multinomial models have a different underlying structure than the
             models gtsummary was designed for.",
      "*" = "Functions designed to work with {.fun tbl_regression} objects may yield unexpected results.")
  )

  x
}
