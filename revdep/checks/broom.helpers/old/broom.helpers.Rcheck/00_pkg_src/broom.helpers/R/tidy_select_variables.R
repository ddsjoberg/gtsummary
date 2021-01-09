#' Select variables to keep/drop
#'
#' Will remove unselected variables from the results.
#' To remove the intercept, use [tidy_remove_intercept()].
#'
#' @details
#' If the `variable` column is not yet available in `x`,
#' [tidy_identify_variables()] will be automatically applied.
#' @param x a tidy tibble
#' @param include variables to include. Accepts [tidyselect][dplyr::select]
#' syntax. Use `-` to remove a variable. Default is `everything()`.
#' See also [all_continuous()], [all_categorical()], [all_dichotomous()]
#' and [all_interaction()]
#'
#' @param model the corresponding model, if not attached to `x`
#' @export
#' @family tidy_helpers
#' @examples
#' res <- Titanic %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::mutate(Survived = factor(Survived)) %>%
#'   glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) %>%
#'   tidy_and_attach() %>%
#'   tidy_identify_variables()
#'
#' res
#' res %>% tidy_select_variables()
#' res %>% tidy_select_variables(include = "Class")
#' res %>% tidy_select_variables(include = -c("Age", "Sex"))
#' res %>% tidy_select_variables(include = starts_with("A"))
#' res %>% tidy_select_variables(include = all_categorical())
#' res %>% tidy_select_variables(include = all_dichotomous())
#' res %>% tidy_select_variables(include = all_interaction())
#' res %>% tidy_select_variables(
#'   include = c("Age", all_categorical(dichotomous = FALSE), all_interaction())
#' )

tidy_select_variables <- function(
  x, include = everything(), model = tidy_get_model(x)
) {
  if (is.null(model)) {
    stop("'model' is not provided. You need to pass it or to use 'tidy_and_attach()'.")
  }

  if (!"variable" %in% names(x)) {
    x <- x %>% tidy_identify_variables(model = model)
  }
  .attributes <- .save_attributes(x)

  # obtain character vector of selected variables
  include <- .select_to_varnames({{ include }}, var_info = x, arg_name = "include")

  x %>%
    dplyr::filter(
      .data$var_type == "intercept" |
        .data$variable %in% include
    ) %>%
    tidy_attach_model(model = model, .attributes = .attributes)

}
