#' Predict from a `{{model}}`
#'
#' @param object A `{{model}}` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' train <- mtcars[1:20,]
#' test <- mtcars[21:32, -1]
#'
#' # Fit
#' mod <- {{model}}(mpg ~ cyl + log(drat), train)
#'
#' # Predict, with preprocessing
#' predict(mod, test)
#'
#' @export
predict.{{model}} <- function(object, new_data, type = "numeric", ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_{{model}}_predict_types())
  predict_{{model}}_bridge(type, object, forged$predictors)
}

valid_{{model}}_predict_types <- function() {
  c("numeric")
}

# ------------------------------------------------------------------------------
# Bridge

predict_{{model}}_bridge <- function(type, model, predictors) {
  predictors <- as.matrix(predictors)

  predict_function <- get_{{model}}_predict_function(type)
  predictions <- predict_function(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

get_{{model}}_predict_function <- function(type) {
  switch(
    type,
    numeric = predict_{{model}}_numeric
  )
}

# ------------------------------------------------------------------------------
# Implementation

predict_{{model}}_numeric <- function(model, predictors) {
  predictions <- rep(1L, times = nrow(predictors))
  hardhat::spruce_numeric(predictions)
}
