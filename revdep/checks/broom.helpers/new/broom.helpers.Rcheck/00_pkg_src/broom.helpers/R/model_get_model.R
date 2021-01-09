#' Get the model from model objects
#'
#' Most model objects are proper R model objects. There are, however, some
#' model objects that store the proper object internally (e.g. mice models).
#' This function extracts that model object in those cases.
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @examples
#' lm(hp ~ mpg + factor(cyl), mtcars) %>%
#'   model_get_model()
model_get_model <- function(model) {
  UseMethod("model_get_model")
}

#' @export
#' @rdname model_get_model
model_get_model.default <- function(model) model

#' @export
#' @rdname model_get_model
model_get_model.mira <- function(model) model$analyses[[1]]
