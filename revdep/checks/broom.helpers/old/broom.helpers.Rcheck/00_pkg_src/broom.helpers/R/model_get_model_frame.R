#' Get the model frame of a model
#'
#' The structure of the object returned by [stats::model.frame()]
#' could slightly differ for certain types of models.
#' `model_get_model_frame()` will always return an object
#' with the same data structure or `NULL` if it is not possible
#' to compute model frame from `model`.
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @seealso [stats::model.frame()]
#' @examples
#' lm(hp ~ mpg + factor(cyl), mtcars) %>%
#'   model_get_model_frame() %>%
#'   head()
model_get_model_frame <- function(model) {
  UseMethod("model_get_model_frame")
}

#' @export
#' @rdname model_get_model_frame
model_get_model_frame.default <- function(model) {
  tryCatch(
    stats::model.frame(model),
    error = function(e) {
      NULL
    }
  )
}

#' @export
#' @rdname model_get_model_frame
model_get_model_frame.coxph <- function(model) {
  tryCatch(
    stats::model.frame.default(model),
    error = function(e) {
      NULL
    }
  )
}

#' @export
#' @rdname model_get_model_frame
model_get_model_frame.survreg <- function(model) {
  tryCatch(
    stats::model.frame.default(model),
    error = function(e) {
      NULL # nocov
    }
  )
}
