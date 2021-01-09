#' Get contrasts used in the model
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @examples
#' glm(
#'   am ~ mpg + factor(cyl),
#'   data = mtcars,
#'   family = binomial,
#'   contrasts = list(`factor(cyl)` = contr.sum)
#' ) %>%
#'   model_get_contrasts()
model_get_contrasts <- function(model) {
  UseMethod("model_get_contrasts")
}

#' @export
#' @rdname model_get_contrasts
model_get_contrasts.default <- function(model) {
  tryCatch(
    purrr::chuck(model, "contrasts"),
    error = function(e) {
      # if first approach is not working, try second one
      attr(model_get_model_matrix(model), "contrasts")
    }
  )
}

