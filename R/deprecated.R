#' Deprecated functions
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

#' @rdname deprecated
#' @export

add_comparison <- function(...) {
  lifecycle::deprecate_warn("1.1.0", "gtsummary::add_comparison()", "add_p()")
  add_p(...)
}

#' @rdname deprecated
#' @export

cols_label_summary <- function(stat_overall = NULL, ...) {
  lifecycle::deprecate_warn("1.0.0", "gtsummary::cols_label_summary()", "modify_header()")
  if (!is.null(stat_overall)) {
    modify_header(stat_0 = stat_overall, ...)
  } else {
    modify_header(...)
  }
}

#' @rdname deprecated
#' @export

add_global <- function(...) {
  lifecycle::deprecate_warn("1.1.0", "gtsummary::add_global()", "add_global_p()")
  add_global_p(...)
}

#' @rdname deprecated
#' @export

tab_style_bold_p <- function(...) {
  lifecycle::deprecate_warn("1.2.0", "gtsummary::tab_style_bold_p()", "bold_p()")
  bold_p(...)
}

#' @rdname deprecated
#' @export

tab_style_bold_labels <- function(...) {
  lifecycle::deprecate_warn("1.2.0", "gtsummary::tab_style_bold_labels()", "bold_labels()")
  bold_labels(...)
}

#' @rdname deprecated
#' @export

tab_style_italicize_levels <- function(...) {
  lifecycle::deprecate_warn("1.2.0", "gtsummary::tab_style_italicize_levels()", "italicize_levels()")
  italicize_levels(...)
}

#' @rdname deprecated
#' @export

tab_style_italicize_labels <- function(...) {
  lifecycle::deprecate_warn("1.2.0", "gtsummary::tab_style_italicize_labels()", "italicize_labels()")
  italicize_labels(...)
}

#' @rdname deprecated
#' @export

tab_style_bold_levels <- function(...) {
  lifecycle::deprecate_warn("1.2.0", "gtsummary::tab_style_bold_levels()", "bold_levels()")
  bold_levels(...)
}

#' @rdname deprecated
#' @export

fmt_uni_regression <- function(...) {
  lifecycle::deprecate_stop("1.0.0", "gtsummary::fmt_uni_regression()", "tbl_uvregression()")
}

#' @rdname deprecated
#' @export

fmt_table1 <- function(...) {
  lifecycle::deprecate_stop("1.0.0", "gtsummary::fmt_table1()", "tbl_summary()")
}

#' @rdname deprecated
#' @export

fmt_regression <- function(...) {
  lifecycle::deprecate_stop("1.0.0", "gtsummary::fmt_regression()", "tbl_regression()")
}

#' @rdname deprecated
#' @export

fmt_beta <- function(...) {
  lifecycle::deprecate_stop("1.0.0", "gtsummary::fmt_beta()", "style_sigfig()")
}

#' @rdname deprecated
#' @export

fmt_pvalue <- function(...) {
  lifecycle::deprecate_stop("1.0.0", "gtsummary::fmt_pvalue()", "style_pvalue()")
}
