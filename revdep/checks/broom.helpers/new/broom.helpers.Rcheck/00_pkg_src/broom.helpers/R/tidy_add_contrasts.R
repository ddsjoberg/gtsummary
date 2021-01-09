#' Add contrasts type for categorical variables
#'
#' Add a `contrasts` column corresponding to contrasts used for a
#' categorical variable and a `contrasts_type` column equal to
#' "treatment", "sum", "poly", "helmert" or "other".
#'
#' @details
#' If the `variable` column is not yet available in `x`,
#' [tidy_identify_variables()] will be automatically applied.
#'
#' @param x a tidy tibble
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @family tidy_helpers
#' @examples
#' df <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
#'
#' df %>%
#'   glm(
#'     Survived ~ Class + Age + Sex,
#'     data = ., weights = .$n, family = binomial,
#'     contrasts = list(Age = contr.sum, Class = "contr.helmert")
#'   ) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_contrasts()
tidy_add_contrasts <- function(x, model = tidy_get_model(x)) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  .attributes <- .save_attributes(x)

  if ("contrasts" %in% names(x)) {
    x <- x %>% dplyr::select(-.data$contrasts)
  }

  if (!"variable" %in% names(x)) {
    x <- x %>% tidy_identify_variables()
  }

  contrasts_list <- model_list_contrasts(model)
  if (is.null(contrasts_list)) {
    x$contrasts <- NA_character_
    x$contrasts_type <- NA_character_
  } else {
    x <- x %>%
      dplyr::left_join(
        contrasts_list %>%
          dplyr::select(.data$variable, .data$contrasts, .data$contrasts_type),
        by = "variable"
      )
  }
  x %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
