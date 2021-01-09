#' Get coefficient type
#'
#' Indicate the type of coefficient among "generic", "logistic",
#' "poisson" and "prop_hazard".
#'
#' @param model a model object
#' @export
#' @family model_helpers
#' @examples
#' lm(hp ~ mpg + factor(cyl), mtcars) %>%
#'   model_get_coefficients_type()
#'
#' Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
#'   glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) %>%
#'   model_get_coefficients_type()
model_get_coefficients_type <- function(model) {
  UseMethod("model_get_coefficients_type")
}

#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.default <- function(model) {
  "generic"
}

#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.glm <- function(model) {
  if (model$family$family == "binomial" && model$family$link == "logit")
    return("logistic")
  if (model$family$family == "poisson" && model$family$link == "log")
    return("poisson")
  if (model$family$family == "quasibinomial" && model$family$link == "logit")
    return("logistic")
  if (model$family$family == "quasipoisson" && model$family$link == "log")
    return("poisson")
  "generic"
}

#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.negbin <- function(model) {
  "poisson"
}

#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.geeglm <- model_get_coefficients_type.glm

#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.glmerMod <- function(model) {
  if (model@resp$family$family == "binomial" && model@resp$family$link == "logit")
    return("logistic")
  if (model@resp$family$family == "poisson" && model@resp$family$link == "log")
    return("poisson")
  # "quasi" families cannot be used with in glmer
  "generic"
}

#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.clogit <- function(model) {
  "logistic"
}

#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.polr <- function(model) {
  if (model$method == "logistic")
    return("logistic")
  "generic"
}

#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.multinom <- function(model) {
  "logistic"
}

#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.svyolr <- function(model) {
  "logistic"
}


#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.clm <- function(model) {
  "logistic"
}

#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.clmm <- function(model) {
  "logistic"
}

#' @export
#' @rdname model_get_coefficients_type
model_get_coefficients_type.coxph <- function(model) {
  "prop_hazard"
}
