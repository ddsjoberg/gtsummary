#' Combine terms
#'
#' The function combines terms from a regression model, and replaces the terms
#' with a single row in the output table.  The p-value is calculated using
#' [`stats::anova()`].
#'
#' @param x (`tbl_regression`)\cr
#'   A `tbl_regression` object
#' @param formula_update (`formula`)\cr
#'   formula update passed to the [`stats::update()`].
#'   This updated formula is used to construct a reduced model, and is
#'   subsequently passed to [stats::anova()] to calculate the p-value for the
#'   group of removed terms.  See the [`stats::update()`] function's `formula.=`
#'   argument for proper syntax.
#' @param label (`string`)\cr
#'   Optional string argument labeling the combined rows
#' @param quiet `r lifecycle::badge("deprecated")`
#' @param ... Additional arguments passed to [stats::anova]
#'
#' @author Daniel D. Sjoberg
#' @return `tbl_regression` object
#' @export
#'
#' @examplesIf gtsummary:::is_pkg_installed(c('cardx', 'broom.helpers'))
#' # Example 1 ----------------------------------
#' # Logistic Regression Example, LRT p-value
#' glm(response ~ marker + I(marker^2) + grade,
#'     trial[c("response", "marker", "grade")] |> na.omit(), # keep complete cases only!
#'     family = binomial) |>
#'   tbl_regression(label = grade ~ "Grade", exponentiate = TRUE) |>
#'   # collapse non-linear terms to a single row in output using anova
#'   combine_terms(
#'     formula_update = . ~ . - marker - I(marker^2),
#'     label = "Marker (non-linear terms)",
#'     test = "LRT"
#'   )
combine_terms <- function(x, formula_update, label = NULL, quiet, ...) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(combine_terms = match.call()))
  check_pkg_installed("cardx", reference_pkg = "gtsummary")

  # deprecation ----------------------------------------------------------------
  if (!missing(quiet)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "gtsummary::combine_terms(quiet)"
    )
  }

  # process inputs -------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(formula_update)
  check_class(x, "tbl_regression")
  check_class(formula_update, "formula")
  check_string(label, allow_empty = TRUE)

  # perform calculation --------------------------------------------------------
  reduced_model <-
    cards::eval_capture_conditions(stats::update(x$inputs$x, formula. = formula_update)) |>
    getElement("result")
  anova <-
    cards::eval_capture_conditions(
      stats::anova(x$inputs$x, reduced_model, ...)
    )
  if (!is_empty(anova[["error"]])) {
    cli::cli_abort(
      c("There was an error calculating the combined terms p-value.",
        i = "There are two common causes for an error. See error message below.",
        "*" = "The model type is not supported by {.fun stats::anova}.",
        "*" = "The number of observations used to estimate the full and reduced models is different.",
        "x" = anova[["error"]]
        ),
      call = get_cli_abort_call()
    )
  }

  x$cards$combine_terms <- cardx::ard_stats_anova(anova[["result"]])

  # if no column was selected, print error
  if (dplyr::filter(x$cards$combine_terms, .data$stat_name %in% "p.value") |> nrow() == 0) {
    cli::cli_abort(
      c("The output from {.fun anova} did not contain a p-value.",
      "i" = "This may happen when there is no default method.
             Use {.arg test} argument to specify the method, e.g. {.code test = \"LRT\"}."),
      call = get_cli_abort_call()
    )
  }
  anova_p <-
    dplyr::filter(x$cards$combine_terms, .data$stat_name %in% "p.value") |>
    dplyr::pull("stat") |>
    unlist()

  # tbl'ing the new model object -----------------------------------------------
  new_model_tbl <-
    rlang::call2(
      "tbl_regression",
      x = reduced_model, # updated model object
      label = x$inputs$label,
      exponentiate = x$inputs$exponentiate,
      include = rlang::expr(intersect(any_of(!!x$inputs$include), everything())),
      show_single_row = rlang::expr(intersect(any_of(!!x$inputs$show_single_row), everything())),
      conf.level = x$inputs$conf.level,
      intercept = x$inputs$intercept,
      estimate_fun = x$inputs$estimate_fun,
      pvalue_fun = x$inputs$pvalue_fun,
      tidy_fun = x$inputs$tidy_fun
    ) |>
    eval()

  # updating original tbl object -----------------------------------------------
  # adding p-value column, if it is not already there
  if (!"p.value" %in% names(x$table_body)) {
    # adding p.value to table_body
    x$table_body <- dplyr::mutate(x$table_body, p.value = NA_real_)
    x <-
      modify_table_styling(
        x,
        columns = "p.value",
        label = "**p-value**",
        hide = FALSE,
        fmt_fun = x$inputs$pvalue_fun
      )
  }
  # replacing the combined rows with a single row
  table_body <-
    x$table_body %>%
    dplyr::left_join(
      new_model_tbl$table_body %>%
        dplyr::select(
          "variable", "var_type", "reference_row",
          "row_type", "label"
        ) %>%
        dplyr::mutate(collapse_row = FALSE),
      by = c("variable", "var_type", "row_type", "reference_row", "label")
    ) %>%
    # marking rows on tbl that will be reduced to a single row
    dplyr::mutate(collapse_row = ifelse(is.na(.data$collapse_row), TRUE, .data$collapse_row)) %>%
    dplyr::group_by(.data$collapse_row) %>%
    dplyr::filter(.data$collapse_row == FALSE |
             (dplyr::row_number() == 1 & .data$collapse_row == TRUE)) %>%
    # updating column values for collapsed rows
    dplyr::mutate_at(
      dplyr::vars("estimate", "conf.low", "conf.high"),
      ~ ifelse(.data$collapse_row == TRUE, NA, .)
    ) %>%
    dplyr::mutate(
      p.value = ifelse(.data$collapse_row == TRUE, anova_p, .data$p.value),
      row_type = ifelse(.data$collapse_row == TRUE, "label", .data$row_type)
    ) %>%
    dplyr::ungroup()

  # adding variable label, if specified ----------------------------------------
  if (!is.null(label)) {
    table_body <-
      table_body %>%
      dplyr::mutate(label = ifelse(.data$collapse_row == TRUE, .env$label, .data$label))
  }

  # writing over the table_body in x -------------------------------------------
  x$table_body <-
    table_body %>%
    dplyr::select(-"collapse_row")

  # returning updated tbl object -----------------------------------------------
  x$call_list <- updated_call_list
  x
}
