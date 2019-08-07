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

cols_label_summary <- function(stat_overall = NULL, ...) {
  signal_soft_deprecated(
    "cols_label_summary() is deprecated. Please use modify_header() instead."
  )
  if (!is.null(stat_overall)) {
    modify_header(stat_0 = stat_overall, ...)
  } else {
    modify_header(...)
  }
}

#' @rdname deprecated
#' @export

add_global <- function(...) {
  signal_soft_deprecated(
    "add_global() is deprecated. Please use add_global_p() instead."
  )
  add_global_p(...)
}

#' @rdname deprecated
#' @export

tab_style_bold_p <- function(...) {
  signal_soft_deprecated(
    "tab_style_bold_p() is deprecated. Please use bold_p() instead."
  )
  bold_p(...)
}

#' @rdname deprecated
#' @export

tab_style_bold_labels <- function(...) {
  signal_soft_deprecated(
    "tab_style_bold_labels() is deprecated. Please use bold_labels() instead."
  )
  bold_labels(...)
}

#' @rdname deprecated
#' @export

tab_style_italicize_levels <- function(...) {
  signal_soft_deprecated(
    "tab_style_italicize_levels() is deprecated. Please use italicize_levels() instead."
  )
  italicize_levels(...)
}

#' @rdname deprecated
#' @export

tab_style_italicize_labels <- function(...) {
  signal_soft_deprecated(
    "tab_style_italicize_labels() is deprecated. Please use italicize_labels() instead."
  )
  italicize_labels(...)
}

#' @rdname deprecated
#' @export

tab_style_bold_levels <- function(...) {
  signal_soft_deprecated(
    "tab_style_bold_levels() is deprecated. Please use bold_levels() instead."
  )
  bold_levels(...)
}
