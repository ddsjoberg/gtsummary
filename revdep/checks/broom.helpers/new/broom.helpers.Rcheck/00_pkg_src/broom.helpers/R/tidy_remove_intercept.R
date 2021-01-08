#' Remove intercept(s)
#'
#' Will remove terms where `var_type == "intercept"`.
#'
#' @details
#' If the `variable` column is not yet available in `x`,
#' [tidy_identify_variables()] will be automatically applied.
#' @param x a tidy tibble
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @family tidy_helpers
#' @examples
#' Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived)) %>%
#'   glm(Survived ~ Class + Age + Sex, data = ., weights = .$n, family = binomial) %>%
#'   tidy_and_attach() %>%
#'   tidy_remove_intercept()
tidy_remove_intercept <- function(x, model = tidy_get_model(x)) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  .attributes <- .save_attributes(x)

  if (!"var_type" %in% names(x)) {
    x <- x %>% tidy_identify_variables(model = model)
  }

  x %>%
    dplyr::filter(.data$var_type != "intercept") %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
