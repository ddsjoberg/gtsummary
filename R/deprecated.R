#' Deprecated functions
#'
#' \lifecycle{deprecated}
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

# tentative deprecation schedule, `Sys.Date() - months(18)`
# "warn" for 18 months
#   1.7.2 2023-07-13
#   1.7.1 2023-04-27
#   1.7.0 2023-01-13
#   1.6.3 2022-12-06
#   1.6.2 2022-09-30
#   1.6.1 2022-06-22
#   1.6.0 2022-04-25

# "stop" for 18 months, then delete from pkg, `Sys.Date() - months(36)`
#   1.5.2 2022-01-29
#   1.5.1 2022-01-20
#   1.5.0 2021-10-16
#   1.4.2 2021-07-13
#   1.4.1 2021-05-19
#   1.4.0 2021-04-13
#   1.3.7 2021-02-26
#   1.3.6 2021-01-08
#   1.3.5 2020-09-29
#   1.3.4 2020-08-27
#   1.3.3 2020-08-11


# v1.3.3 (2020-08-11) ----------------------------------------------------------
#' @rdname deprecated
#' @export
as_flextable <- function(...) {
  lifecycle::deprecate_stop(
    "1.3.3", "gtsummary::as_flextable()", "as_flex_table()",
    details = paste(
      "The `as_flextable()` function graduated",
      "from 'Experimental' status in v1.3.3. The function's name was changed",
      "to avoid a name conflict with `flextable::as_flextable()`.",
      "If you are trying to use the function",
      "from {flextable}, for the time being, use the double colon notation",
      "when both {gtsummary} and {flextable}",
      "are loaded, e.g. `flextable::as_flextable(...)`."
    ) %>%
      stringr::str_wrap()
  )
}

# v1.3.6 -----------------------------------------------------------------------
#' @rdname deprecated
#' @export
all_numeric <- function() {
  lifecycle::deprecate_stop(
    "1.3.6", "gtsummary::all_numeric()",
    details = paste0(
      "The {tidyselect} and {dplyr} packages have implemented functions to ",
      "select variables by class and type, and the {gtsummary} version is ",
      "now deprecated.\n\n",
      "Use `where(is.numeric)` instead."
    )
  )
}


#' @rdname deprecated
#' @export
all_character <- function() {
  lifecycle::deprecate_stop(
    "1.3.6", "gtsummary::all_character()",
    details = paste0(
      "The {tidyselect} and {dplyr} packages have implemented functions to ",
      "select variables by class and type, and the {gtsummary} version is ",
      "now deprecated.\n\n",
      "Use `where(is.character)` instead."
    )
  )
}

#' @rdname deprecated
#' @export
all_integer <- function() {
  lifecycle::deprecate_stop(
    "1.3.6", "gtsummary::all_integer()",
    details = paste0(
      "The {tidyselect} and {dplyr} packages have implemented functions to ",
      "select variables by class and type, and the {gtsummary} version is ",
      "now deprecated.\n\n",
      "Use `where(is.integer)` instead."
    )
  )
}

#' @rdname deprecated
#' @export
all_double <- function() {
  lifecycle::deprecate_stop(
    "1.3.6", "gtsummary::all_double()",
    details = paste0(
      "The {tidyselect} and {dplyr} packages have implemented functions to ",
      "select variables by class and type, and the {gtsummary} version is ",
      "now deprecated.\n\n",
      "Use `where(is.double)` instead."
    )
  )
}

#' @rdname deprecated
#' @export
all_logical <- function() {
  lifecycle::deprecate_stop(
    "1.3.6", "gtsummary::all_logical()",
    details = paste0(
      "The {tidyselect} and {dplyr} packages have implemented functions to ",
      "select variables by class and type, and the {gtsummary} version is ",
      "now deprecated.\n\n",
      "Use `where(is.logical)` instead."
    )
  )
}

#' @rdname deprecated
#' @export
all_factor <- function() {
  lifecycle::deprecate_stop(
    "1.3.6", "gtsummary::all_factor()",
    details = paste0(
      "The {tidyselect} and {dplyr} packages have implemented functions to ",
      "select variables by class and type, and the {gtsummary} version is ",
      "now deprecated.\n\n",
      "Use `where(is.factor)` instead."
    )
  )
}

# this is a copy of the tidyselect where function. it can be deleted after the
# all_factor, all_character, etc. functions are fully deprecated
where <- function(fn) {
  predicate <- rlang::as_function(fn)

  function(x, ...) {
    out <- predicate(x, ...)

    if (!rlang::is_bool(out)) {
      abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }

    out
  }
}


# v1.6.1 ----------------------------------------------------------
#' @rdname deprecated
#' @export
modify_cols_merge <- function(...) {
  lifecycle::deprecate_warn("1.6.1", "gtsummary::modify_cols_merge()", "modify_column_merge()")
  modify_column_merge(...)
}
