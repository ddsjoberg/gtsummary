#' @title Methods for tbl_regression
#'
#' @description Most regression models are handled by [tbl_regression.default],
#' which uses [broom::tidy] to perform initial tidying of results. There are,
#' however, some model types that have modified default printing behavior.
#' Those methods are listed below.
#'
#' @inheritSection tbl_regression Methods
#' @name tbl_regression_methods
#' @rdname tbl_regression_methods
#' @param ... arguments passed to `tbl_regression.default()`
#' @inheritParams tbl_regression
#' @inheritParams tbl_stack
NULL

#' @export
#' @rdname tbl_regression_methods
tbl_regression.lmerMod <- function(
  x, tidy_fun = function(x, ...) broom.mixed::tidy(x, ..., effects = "fixed"), ...) {
  assert_package("broom.mixed", "tbl_regression.lmerMod")
  tbl_regression.default(x = x, tidy_fun = tidy_fun, ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.glmerMod <- tbl_regression.lmerMod

#' @export
#' @rdname tbl_regression_methods
tbl_regression.survreg <- function(
  x, tidy_fun = function(x, ...) broom::tidy(x, ...) %>% dplyr::filter(.data$term != "Log(scale)"), ...) {
  tbl_regression.default(x = x, tidy_fun = tidy_fun, ...)
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.multinom <- function(x, group_header = NULL, ...) {

  inform("Constructing `tbl_regression()` objects for each outcome")
  df_tidy <-
    broom::tidy(x, exponentiate = exponentiate,
                conf.level = conf.level, conf.int = TRUE) %>%
    dplyr::nest_by(.data$y.level) %>%
    rowwise() %>%
    mutate(
      # creating a tidy function that just returns the piece needed of this tidy data frame
      tidy_data = list(function(x, ...) .data$data),
      # running tbl_regression on each piece of the tidy tibble (for each outcome)
      tbl_regression =
        tbl_regression.default(x = x, tidy_fun = tidy_fun, ...) %>%
        list()
    )

  inform("Stacking `tbl_regression()` objects for each outcome using `tbl_stack()`")
  tbl_stack(df_tidy$tbl_regression, group_header = group_header %||% df_tidy$y.level)
}


#' #' @export
#' #' @rdname tbl_regression_methods
#' tbl_regression.mipo <- function(x, tidy_fun = mice::tidy, ...) {
#'     tbl_regression.default(x = x, tidy_fun = tidy_fun, ...)
#' }
