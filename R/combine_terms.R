#' Combine terms in a regression model
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' The function combines terms from a regression model, and replaces the terms
#' with a single row in the output table.  The p-value is calculated using
#' [stats::anova()].
#'
#' @param x a `tbl_regression` object
#' @param formula_update formula update passed to the [stats::update].
#' This updated formula is used to construct a reduced model, and is
#' subsequently passed to [stats::anova()] to calculate the p-value for the
#' group of removed terms.  See the [stats::update] help file for proper syntax.
#' function's `formula.=` argument
#' @param label Option string argument labeling the combined rows
#' @param ... Additional arguments passed to [stats::anova]
#' @author Daniel D. Sjoberg
#' @family tbl_regression tools
#' @return `tbl_regression` object
#' @export
#'
#' @examples
#' # fit model with nonlinear terms for marker
#' nlmod1 <- lm(
#'   age ~ marker + I(marker^2) + grade,
#'   trial[c("age", "marker", "grade")] %>% na.omit() # keep complete cases only!
#' )
#'
#' combine_terms_ex1 <-
#'   tbl_regression(nlmod1, label = grade ~ "Grade") %>%
#'   # collapse non-linear terms to a single row in output using anova
#'   combine_terms(
#'     formula_update = . ~ . - marker - I(marker^2),
#'     label = "Marker (non-linear terms)"
#'   )
#'
#' # Example with Cubic Splines
#' library(Hmisc)
#' mod2 <- lm(
#'   age ~ rcspline.eval(marker, inclx = TRUE) + grade,
#'   trial[c("age", "marker", "grade")] %>% na.omit() # keep complete cases only!
#' )
#'
#' combine_terms_ex2 <-
#'   tbl_regression(mod2, label = grade ~ "Grade") %>%
#'   combine_terms(
#'     formula_update = . ~ . -rcspline.eval(marker, inclx = TRUE),
#'     label = "Marker (non-linear terms)"
#'   )
#'
#' # Logistic Regression Example, LRT p-value
#' combine_terms_ex3 <-
#'   glm(
#'     response ~ marker + I(marker^2) + grade,
#'     trial[c("response", "marker", "grade")] %>% na.omit(), # keep complete cases only!
#'     family = binomial
#'   ) %>%
#'   tbl_regression(label = grade ~ "Grade", exponentiate = TRUE) %>%
#'   # collapse non-linear terms to a single row in output using anova
#'   combine_terms(
#'     formula_update = . ~ . - marker - I(marker^2),
#'     label = "Marker (non-linear terms)",
#'     test = "LRT"
#'   )
#'
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{combine_terms_ex1.png}{options: width=45\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{combine_terms_ex2.png}{options: width=45\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{combine_terms_ex3.png}{options: width=45\%}}

combine_terms <- function(x, formula_update, label = NULL, ...) {
  # checking input -------------------------------------------------------------
  if (!inherits(x, "tbl_regression")) {
    stop("`x` input must be class `tbl_regression`", call. = FALSE)
  }

  if(!is.null(label) && !rlang::is_string(label)) {
    stop(paste(
      "`label` argument must be a string of length one."
    ), call. = FALSE)
  }

  # creating updated model object ----------------------------------------------
  new_model_obj <- stats::update(x$model_obj, formula. = formula_update)
  tryCatch({
    anova <- stats::anova(x$model_obj, new_model_obj, ...)
    },
    error = function(e) {
      err_msg <-
        paste(
          "There was error calculating the p-value in the",
          "'anova()' function.\n",
          "There are two common causes for an error during the calculation:\n",
          "1. The model type is not supported by 'anova()'.\n",
          "2. The number of observations used to estimate the full and reduced",
          "models is different.\n\n",
          as.character(e)
        )
      stop(err_msg, call. = FALSE)
    })
  # extracting p-value from anova object ---------------------------------------
  df_anova <- as_tibble(anova) %>%
    select(starts_with("Pr(>"),  starts_with("P(>"))
  # if not column was selected, print error
  if (ncol(df_anova) == 0) {
    stop(paste(
      "The output from `anova()` did not contain a p-value.\n",
      "A common source of this error is not specifying the `test=` argument.\n",
      "For example, to get the LRT p-value for a logistic regression estimated with `glm()`,\n",
      "include the argument `test = \"LRT\"` in the `combine_terms()` call."
    ), call. = FALSE)
  }

  anova_p <- df_anova %>%
    slice(n()) %>%
    pull()

  # tbl'ing the new model object -----------------------------------------------
  # getting call from original tbl_regression call, and updating with new model object
  call_aslist <- x$inputs
  # replacing model with updated model in call
  call_aslist$x <- new_model_obj
  # running tbl_regression with new model
  new_model_tbl <- do.call(tbl_regression, call_aslist)

  # updating original tbl object -----------------------------------------------
  # replacing the combined rows with a single row
  table_body <-
    x$table_body %>%
    left_join(
      new_model_tbl$table_body %>%
        select(.data$variable, .data$var_type, .data$row_ref,
               .data$row_type, .data$label) %>%
        mutate(collapse_row = FALSE),
      by = c("variable", "var_type", "row_type", "row_ref", "label")
    ) %>%
    # marking rows on tbl that will be reduced to a single row
    mutate(collapse_row = ifelse(is.na(.data$collapse_row), TRUE, .data$collapse_row)) %>%
    group_by(.data$collapse_row) %>%
    filter(.data$collapse_row == FALSE |
             (dplyr::row_number() == 1 & .data$collapse_row == TRUE)) %>%
    # updating column values for collapsed rows
    mutate_at(vars(.data$estimate, .data$conf.low, .data$conf.high, .data$ci),
              ~ifelse(.data$collapse_row == TRUE, NA, .)) %>%
    mutate(
      p.value = ifelse(.data$collapse_row == TRUE, !!anova_p, .data$p.value),
      row_type = ifelse(.data$collapse_row == TRUE, "label", .data$row_type)
    ) %>%
    ungroup()

  # adding variable label, if specified ----------------------------------------
  if(!is.null(label)) {
    table_body <-
      table_body %>%
      mutate(label = ifelse(.data$collapse_row == TRUE, !!label, .data$label))
  }

  # writing over the table_body in x -------------------------------------------
  x$table_body <-
    table_body %>%
    select(-.data$collapse_row)

  # returning updated tbl object -----------------------------------------------
  x$call_list <- c(x$call_list, list(combine_terms = match.call()))

  x
}
