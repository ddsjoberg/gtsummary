#' @title Methods for tbl_regression
#'
#' @description Most regression models are handled by [tbl_regression.default()],
#' which uses [broom::tidy()] to perform initial tidying of results. There are,
#' however, some model types that have modified default printing behavior.
#' Those methods are listed below.
#'
#' @inheritSection tbl_regression Methods
#' @name tbl_regression_methods
#' @keywords internal
#' @rdname tbl_regression_methods
#' @param ... arguments passed to `tbl_regression.default()`
#' @inheritParams tbl_regression
#' @inheritParams tbl_stack
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
  assert_package("workflows", "tbl_regression.workflow()")

  if (isTRUE(!x$pre$actions$formula$blueprint$indicators %in% "none")) {
    paste(
      "To take full advantage of model formatting, e.g. grouping categorical",
      "variables, please add the following argument to the `workflows::add_model()` call:"
    ) %>%
      stringr::str_wrap() %>%
      paste("`blueprint = hardhat::default_formula_blueprint(indicators = 'none')`", sep = "\n") %>%
      paste("\n") %>%
      rlang::inform()
  }

  paste(
    "Extracting {workflows} model fit with",
    "`workflows::extract_fit_parsnip(x) %>% tbl_regression(...)`"
  ) %>%
    message()

  tbl_regression(x = workflows::extract_fit_parsnip(x), ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.survreg <- function(x, tidy_fun = function(x, ...) broom::tidy(x, ...) %>% dplyr::filter(.data$term != "Log(scale)"), ...) {
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
  paste(
    "Please pass the 'mice' model to {.code tbl_regression()} before ",
    "models have been combined with {.code mice::pool()}.",
    "The default tidier, {.code pool_and_tidy_mice()}, ", "
        will both pool and tidy the regression model."
  ) %>%
    stringr::str_wrap() %>%
    cli_alert_danger()
  paste("\n\nmice::mice(trial, m = 2) %>%",
    "with(lm(age ~ marker + grade)) %>%",
    "tbl_regression()",
    sep = "\n  "
  ) %>%
    cli_code()
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.lmerMod <- function(x, tidy_fun = function(x, ...) broom.mixed::tidy(x, ..., effects = "fixed"), ...) {
  assert_package("broom.mixed", "tbl_regression.lmerMod()")
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
  assert_package("tidycmprsk", "tbl_regression.tidycrr()")
  tbl_regression.default(x = x, tidy_fun = tidy_fun, ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.crr <- function(x, ...) {
  cli::cli_alert_info("For better summary support, build model with {.code tidycmprsk::crr()}.")
  cli::cli_ul("Visit {.url https://mskcc-epi-bio.github.io/tidycmprsk/} for details.")
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
    x$table_body %>%
    mutate(groupname_col = .data$y.level, .before = 1)
  x <-
    modify_table_styling(
      x = x,
      columns = all_of("groupname_col"),
      hide = FALSE,
      label = "**Outcome**",
      align = "left"
    )

  # warning about multinomial models
  paste(
    "Multinomial models have a different underlying structure than",
    "the models gtsummary was designed for.",
    "Other gtsummary functions designed to work with",
    "{.field tbl_regression} objects may yield unexpected",
    "results."
  ) %>%
    str_wrap() %>%
    cli_alert_info()

  x
}
