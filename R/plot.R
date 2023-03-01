#' Plot Regression Coefficients
#'
#' The `plot()` function extracts `x$table_body` and passes the it to
#' `ggstats::ggcoef_plot()` along with a formatting options.
#'
#' \lifecycle{experimental}
#' @param x 'tbl_regression' or 'tbl_uvregression' object
#' @param remove_header_rows logical indicating whether to remove header rows
#' for categorical variables. Default is `TRUE`
#' @param remove_reference_rows logical indicating whether to remove reference rows
#' for categorical variables. Default is `FALSE`.
#' @param ... arguments passed to `ggstats::ggcoef_plot(...)`
#'
#' @return a ggplot
#' @name plot
#'
#' @examplesIf broom.helpers::.assert_package("ggstats", pkg_search = "gtsummary", boolean = TRUE)
#' glm(response ~ marker + grade, trial, family = binomial) %>%
#'   tbl_regression(
#'     add_estimate_to_reference_rows = TRUE,
#'     exponentiate = TRUE
#'   ) %>%
#'   plot()
NULL

#' @rdname plot
#' @export
plot.tbl_regression <- function(x,
                                remove_header_rows = TRUE,
                                remove_reference_rows = FALSE, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  assert_package("ggstats", fn = "plot.tbl_regression()")

  df_coefs <- x$table_body
  if (isTRUE(remove_header_rows)) {
    df_coefs <- df_coefs %>% filter(!.data$header_row %in% TRUE)
  }
  if (isTRUE(remove_reference_rows)) {
    df_coefs <- df_coefs %>% filter(!.data$reference_row %in% TRUE)
  }

  df_coefs %>%
    ggstats::ggcoef_plot(exponentiate = x$inputs$exponentiate, ...)
}

#' @rdname plot
#' @export
plot.tbl_uvregression <- plot.tbl_regression
