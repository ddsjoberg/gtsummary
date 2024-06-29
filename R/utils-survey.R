# script contains code to assist with survey objects

#' @export
as.data.frame.survey.design <- function(x, ...) {
  x$variables
}

#' Test if data is a survey object
#' @noRd
is_survey <- function(data) {
  return(inherits(data, "survey.design") | inherits(data, "svyrep.design"))
}
