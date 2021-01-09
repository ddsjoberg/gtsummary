#' Get the number of levels for each factor used in `xlevels`
#'
#' @param model a model object
#' @return a tibble with two columns: `"variable"` and `"var_nlevels"`
#' @export
#' @family model_helpers
#' @examples
#' lm(hp ~ mpg + factor(cyl), mtcars) %>%
#'   model_get_nlevels()
model_get_nlevels <- function(model) {
  UseMethod("model_get_nlevels")
}

#' @export
#' @rdname model_get_nlevels
model_get_nlevels.default <- function(model) {
  nlevels <- model_get_xlevels(model) %>% lapply(length)
  if (length(nlevels) == 0)
    return(
      dplyr::tibble(variable = NA_character_, var_nlevels = NA_integer_) %>%
        dplyr::filter(FALSE) # empty tibble
    )
  dplyr::tibble(
    variable = names(nlevels),
    var_nlevels = unlist(nlevels)
  )
}
