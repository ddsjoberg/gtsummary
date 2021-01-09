#' Get the model matrix of a model
#'
#' The structure of the object returned by [stats::model.matrix()]
#' could slightly differ for certain types of models.
#' `model_get_model_matrix()` will always return an object
#' with the same structure as [stats::model.matrix.default()].
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @seealso [stats::model.matrix()]
#' @examples
#' lm(hp ~ mpg + factor(cyl), mtcars) %>%
#'   model_get_model_matrix() %>%
#'   head()
model_get_model_matrix <- function(model) {
  UseMethod("model_get_model_matrix")
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.default <- function(model) {
  tryCatch(
    stats::model.matrix(model),
    error = function(e) {
      tryCatch( # test second approach
        stats::model.matrix(stats::terms(model), model$model),
        error = function(e) {
          NULL
        }
      )
    }
  )
}

#' @export
#' @rdname model_get_model_matrix
# For multinom models, names of the model matrix are not
# consistent with the terms names when contrasts other
# than treatment are used, resulting in an issue for
# the identification of variables
model_get_model_matrix.multinom <- function(model) {
  mm <- stats::model.matrix(model)
  colnames(mm) <- colnames(stats::coef(model))
  mm
}

#' @export
#' @rdname model_get_model_matrix
model_get_model_matrix.clm <- function(model) {
  stats::model.matrix(model)[[1]]
}
