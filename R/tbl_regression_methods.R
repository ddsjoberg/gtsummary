#' @title Methods for tbl_regression
#'
#' @description Most regression models are handled by [tbl_regression.default],
#' which uses [broom::tidy] to perform initial tidying of results. There are,
#' however, some model types that have modified default printing behavior.
#' Those methods are listed below.
#' @name tbl_regression_methods
#' @rdname tbl_regression_methods
#' @inheritParams tbl_regression
#' @inheritParams tbl_stack
NULL

#' @export
#' @rdname tbl_regression_methods
tbl_regression.lmerMod <- function(x, label = NULL, exponentiate = FALSE,
                                   include = everything(), show_single_row = NULL,
                                   conf.level = NULL, intercept = FALSE,
                                   estimate_fun = NULL, pvalue_fun = NULL,
                                   tidy_fun = function(x, ...) broom.mixed::tidy(x, ..., effects = "fixed"),
                                   show_yesno = NULL, exclude = NULL, ...) {
  assert_package("broom.mixed", "tbl_regression.lmerMod")

  tbl_regression.default(
    x = x, label = label, exponentiate = exponentiate,
    include = {{ include }}, show_single_row = {{ show_single_row }},
    conf.level = conf.level, intercept = intercept,
    estimate_fun = estimate_fun, pvalue_fun = pvalue_fun,
    tidy_fun = tidy_fun, show_yesno = {{ show_yesno }}, exclude = {{ exclude }}
  )
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.glmerMod <- tbl_regression.lmerMod

#' @export
#' @rdname tbl_regression_methods
tbl_regression.survreg <- function(x, label = NULL, exponentiate = FALSE,
                                   include = everything(), show_single_row = NULL,
                                   conf.level = NULL, intercept = FALSE,
                                   estimate_fun = NULL, pvalue_fun = NULL,
                                   tidy_fun = function(x, ...) broom::tidy(x, ...) %>% dplyr::filter(.data$term != "Log(scale)"),
                                   show_yesno = NULL, exclude = NULL, ...) {
  tbl_regression.default(
    x = x, label = label, exponentiate = exponentiate,
    include = {{ include }}, show_single_row = {{ show_single_row }},
    conf.level = conf.level, intercept = intercept,
    estimate_fun = estimate_fun, pvalue_fun = pvalue_fun,
    tidy_fun = tidy_fun, show_yesno = {{ show_yesno }}, exclude = {{ exclude }}
  )
}

#' @export
#' @rdname tbl_regression_methods
tbl_regression.multinom <- function(x, label = NULL, exponentiate = FALSE,
                                    include = everything(), show_single_row = NULL,
                                    conf.level = NULL, intercept = FALSE,
                                    estimate_fun = NULL, pvalue_fun = NULL,
                                    show_yesno = NULL, exclude = NULL,
                                    group_header = NULL, ...) {

  df_tidy <-
    broom::tidy(x, exponentiate = exponentiate,
                conf.level = conf.level, conf.int = TRUE) %>%
    dplyr::nest_by(.data$y.level) %>%
    rowwise() %>%
    mutate(
      # creating a tidy function that just returns the piece needed of this tidy data frame
      tidy_data = list(function(x, ...) data),
      # running tbl_regression on each piece of the tidy tibble (for each outcome)
      tbl_regression =
        tbl_regression.default(
          x = x, tidy_fun = tidy_data, label = label, exponentiate = exponentiate,
          include = {{ include }}, show_single_row = {{ show_single_row }},
          conf.level = conf.level, intercept = intercept,
          estimate_fun = estimate_fun, pvalue_fun = pvalue_fun,
          show_yesno = {{ show_yesno }}, exclude = {{ exclude }}
        ) %>%
        list()
    )

  inform("Stacking `tbl_regression()` objects for each outcome using `tbl_stack()`")
  tbl_stack(df_tidy$tbl_regression, group_header = group_header %||% df_tidy$y.level)
}


#' #' @export
#' #' @rdname tbl_regression_methods
#' tbl_regression.mipo <- function(x, label = NULL, exponentiate = FALSE,
#'                                 include = everything(), show_single_row = NULL,
#'                                 conf.level = NULL, intercept = FALSE,
#'                                 estimate_fun = NULL, pvalue_fun = NULL,
#'                                 tidy_fun = mice::tidy,
#'                                 show_yesno = NULL, exclude = NULL, ...) {
#'   tbl_regression.default(
#'     x = x, label = label, exponentiate = exponentiate,
#'     include = {{ include }}, show_single_row = {{ show_single_row }},
#'     conf.level = conf.level, intercept = intercept,
#'     estimate_fun = estimate_fun, pvalue_fun = pvalue_fun,
#'     tidy_fun = tidy_fun, show_yesno = {{ show_yesno }}, exclude = {{ exclude }}
#'   )
#' }
