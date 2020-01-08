#' Combine terms
#'
#' @param x a `tbl_regregression` object
#' @param formula_update formula update passed to the [stats::update]
#' function's `formula.=` argument
#'
#' @return `tbl_regression` object
#' @export
#'
#' @examples
#' combine_terms_ex1 <-
#'   # fit model with nonlinear terms for marker
#'   lm(
#'     age ~ marker + I(marker^2) + grade,
#'     trial[c("age", "marker", "grade")] %>% na.omit() # keep complete cases only!
#'   ) %>%
#'   tbl_regression(label = grade ~ "Grade") %>%
#'   # collapse non-linear terms to a single row in output using anova
#'   combine_terms(
#'     formula_update = . ~ . - marker - I(marker^2),
#'     label = "Marker (non-linear terms)"
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{combine_terms_ex1.png}{options: width=45\%}}

combine_terms <- function(x, formula_update, label = NULL) {
  # checking input -------------------------------------------------------------
  if (!is(x, "tbl_regression")) {
    stop("`x` input must be class `tbl_regression`", call. = FALSE)
  }

  if (names(x$call_list) != "tbl_regression") {
    stop(paste(
      "Call `combine_terms()` directly after `tbl_regression()`,",
      "prior to any other related functions."
    ), call. = FALSE)
  }

  # creating updated model object ----------------------------------------------
  new_model_obj <- stats::update(x$model_obj, formula. = formula_update)
  tryCatch({
    anova_p <-
      stats::anova(x$model_obj, new_model_obj) %>%
      as_tibble() %>%
      select(starts_with("Pr(>")) %>%
      slice(n()) %>%
      pull()},
    error = function(e) {
      usethis::ui_oops(paste(
        "There was error  calculating the p-value in the",
        "{usethis::ui_code('anova()')} function (error printed below).\n",
        "There are two common causes for an error during the calculation:\n",
        "1. The model type is not supported by {usethis::ui_code('anova()')}.\n",
        "2. The number of observations used to estimate the full and reduced",
        "models is different."
      ))
      stop(as.character(e), call. = FALSE)
    })

  # tbl'ing the new model object -----------------------------------------------
  # getting call from original tbl_regression call, and updating with new model object
  call_aslist <- x$call_list$tbl_regression %>% as.list()
  # replacing model with updated model in call
  call_aslist$x <- new_model_obj
  # running tbl_regression with new model
  new_model_tbl <- as.call(call_aslist) %>% eval()

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
    select(-collapse_row)

  # returning updated tbl object -----------------------------------------------
  x$call_list <- c(x$call_list, list(combine_terms = match.call()))

  x
}
