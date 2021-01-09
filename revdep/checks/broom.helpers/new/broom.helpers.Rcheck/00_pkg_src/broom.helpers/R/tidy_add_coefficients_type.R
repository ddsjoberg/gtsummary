#' Add coefficients type and label as attributes
#'
#' Add the type of coefficients ("generic", "logistic", "poisson" or "prop_hazard")
#' and the corresponding coefficient labels, as attributes to `x` (respectively
#' named `coefficients_type` and `coefficients_label`).
#'
#' @param x a tidy tibble
#' @param exponentiate logical indicating whether or not to exponentiate the
#' coefficient estimates. It should be consistent with the original call to
#' [broom::tidy()]
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @family tidy_helpers
#' @examples
#' ex1 <- lm(hp ~ mpg + factor(cyl), mtcars) %>%
#'   tidy_and_attach() %>%
#'   tidy_add_coefficients_type()
#' attr(ex1, "coefficients_type")
#' attr(ex1, "coefficients_label")
#'
#' ex2 <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) %>%
#'   glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) %>%
#'   tidy_and_attach(exponentiate = TRUE) %>%
#'   tidy_add_coefficients_type()
#' attr(ex2, "coefficients_type")
#' attr(ex2, "coefficients_label")
tidy_add_coefficients_type <- function(
  x, exponentiate = attr(x, "exponentiate"),
  model = tidy_get_model(x)
) {
  if (is.null(exponentiate) | !is.logical(exponentiate))
    stop("'exponentiate' is not provided. You need to pass it explicitely.")

  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  .attributes <- .save_attributes(x)
  .attributes$exponentiate <- exponentiate

  coefficients_type <- model_get_coefficients_type(model)
  if (exponentiate)
    coefficients_label <- dplyr::case_when(
      coefficients_type == "logistic" ~ "OR",
      coefficients_type == "poisson" ~ "IRR",
      coefficients_type == "prop_hazard" ~ "HR",
      TRUE ~ "exp(Beta)"
    )
  else
    coefficients_label <- dplyr::case_when(
      coefficients_type == "logistic" ~ "log(OR)",
      coefficients_type == "poisson" ~ "log(IRR)",
      coefficients_type == "prop_hazard" ~ "log(HR)",
      TRUE ~ "Beta"
    )

  attr(x, "coefficients_type") <- coefficients_type
  attr(x, "coefficients_label") <- coefficients_label

  x %>%
    tidy_attach_model(model = model, .attributes = .attributes)
}
