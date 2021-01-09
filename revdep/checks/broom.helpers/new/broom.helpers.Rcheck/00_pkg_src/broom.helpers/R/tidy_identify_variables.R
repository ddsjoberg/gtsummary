#' Identify the variable corresponding to each model coefficient
#'
#' `tidy_identify_variables()` will add to the tidy tibble
#' three additional columns: `variable`, `var_class`, `var_type` and `var_nlevels`.
#'
#' It will also identify interaction terms and intercept(s).
#'
#' `var_type` could be:
#'
#' * `"continuous"`,
#' * `"dichotomous"` (categorical variable with 2 levels),
#' * `"categorical"` (categorical variable with 3 levels or more),
#' * `"intercept"`
#' * `"interaction"`
#' * `"unknown"` in the rare cases where `tidy_identify_variables()`
#'   will fail to identify the list of variables
#'
#' For dichotomous and categorical variables, `var_nlevels` corresponds to the number
#' of original levels in the corresponding variables.
#' @param x a tidy tibble
#' @param model the corresponding model, if not attached to `x`
#' @inheritParams tidy_plus_plus
#' @export
#' @seealso [model_identify_variables()]
#' @family tidy_helpers
#' @examples
#' Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
#'   glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) %>%
#'   tidy_and_attach() %>%
#'   tidy_identify_variables()
#'
#' lm(
#'   Sepal.Length ~ poly(Sepal.Width, 2) + Species,
#'   data = iris,
#'   contrasts = list(Species = contr.sum)
#' ) %>%
#'   tidy_and_attach(conf.int = TRUE) %>%
#'   tidy_identify_variables()
tidy_identify_variables <- function(x, model = tidy_get_model(x),
                                    quiet = FALSE, strict = FALSE) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if ("header_row" %in% names(x)) {
    stop("`tidy_identify_variables()` cannot be applied after `tidy_add_header_rows().`")
  }

  .attributes <- .save_attributes(x)

  if ("variable" %in% names(x)) {
    x <- x %>% dplyr::select(
      -any_of(c("variable", "var_class", "var_type", "var_nlevels"))
    )
  }

  variables_list <- model_identify_variables(model)

  if (nrow(variables_list) > 0) {
    x %>%
      dplyr::left_join(variables_list, by = "term") %>%
      dplyr::mutate(
        var_type = dplyr::if_else(
          is.na(.data$variable),
          "intercept",
          .data$var_type
        ),
        variable = dplyr::if_else(
          .data$var_type == "intercept",
          .data$term,
          .data$variable
        )
      ) %>%
      tidy_attach_model(model = model, .attributes = .attributes)
  } else {
    if (!quiet)
      usethis::ui_oops(paste0(
        "Unable to identify the list of variables.\n\n",
        "This is usually due to an error calling {usethis::ui_code('stats::model.frame(x)')}",
        "or {usethis::ui_code('stats::model.matrix(x)')}.\n",
        "It could be the case if that type of model does not implement these methods.\n",
        "Rarely, this error may occur if the model object was created within\na ",
        "functional programming framework (e.g. using {usethis::ui_code('lappy()')}, ",
        "{usethis::ui_code('purrr::map()')}, etc.)."
      ))
    if (strict) stop("Cannot identify variables. Quitting execution.", call. = FALSE)
    x %>%
      dplyr::mutate(
        variable = .data$term,
        var_class = NA_integer_,
        var_type = "unknown",
        var_nlevels = NA_integer_
      ) %>%
      tidy_attach_model(model = model, .attributes = .attributes)
  }


}
