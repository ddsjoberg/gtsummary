#' Deprecated functions
#'
#' Some functions have been deprecated and are no longer being actively
#' supported. Please use these functions instead:
#' [add_p], [add_global_p].
#'
#' @name deprecated
#' @keywords internal
NULL

#' @rdname deprecated
#' @export

add_comparison <- function(...) {
  signal_soft_deprecated(
    "add_comparison() is deprecated. Please use add_p() instead."
  )
  add_p(...)
}

#' @rdname deprecated
#' @export

add_global <- function(...) {
  signal_soft_deprecated(
    "add_global() is deprecated. Please use add_global_p() instead."
  )
  add_global_p(...)
}
