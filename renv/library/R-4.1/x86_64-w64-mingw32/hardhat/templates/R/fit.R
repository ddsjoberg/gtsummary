#' Fit a `{{model}}`
#'
#' `{{model}}()` fits a model.
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return
#'
#' A `{{model}}` object.
#'
#' @examples
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#'
#' # XY interface
#' mod <- {{model}}(predictors, outcome)
#'
#' # Formula interface
#' mod2 <- {{model}}(mpg ~ ., mtcars)
#'
#' # Recipes interface
#' library(recipes)
#' rec <- recipe(mpg ~ ., mtcars)
#' rec <- step_log(rec, disp)
#' mod3 <- {{model}}(rec, mtcars)
#'
#' @export
{{model}} <- function(x, ...) {
  UseMethod("{{model}}")
}

#' @export
#' @rdname {{model}}
{{model}}.default <- function(x, ...) {
  stop("`{{model}}()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname {{model}}
{{model}}.data.frame <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  {{model}}_bridge(processed, ...)
}

# XY method - matrix

#' @export
#' @rdname {{model}}
{{model}}.matrix <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  {{model}}_bridge(processed, ...)
}

# Formula method

#' @export
#' @rdname {{model}}
{{model}}.formula <- function(formula, data, ...) {
  processed <- hardhat::mold(formula, data)
  {{model}}_bridge(processed, ...)
}

# Recipe method

#' @export
#' @rdname {{model}}
{{model}}.recipe <- function(x, data, ...) {
  processed <- hardhat::mold(x, data)
  {{model}}_bridge(processed, ...)
}

# ------------------------------------------------------------------------------
# Bridge

{{model}}_bridge <- function(processed, ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  fit <- {{model}}_impl(predictors, outcome)

  new_{{model}}(
    coefs = fit$coefs,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation

{{model}}_impl <- function(predictors, outcome) {
  list(coefs = 1)
}
