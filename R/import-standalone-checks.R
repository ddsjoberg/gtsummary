# Standalone file: do not edit by hand
# Source: <https://github.com/ddsjoberg/standalone/blob/main/R/standalone-checks.R>
# ----------------------------------------------------------------------
#
# ---
# repo: ddsjoberg/standalone
# file: standalone-checks.R
# last-updated: 2024-01-24
# license: https://unlicense.org
# imports: [rlang, cli]
# ---
#
# This file provides a minimal functions to check argument values and types
# passed by users to functions in packages.
#
# ## Changelog
# nocov start
# styler: off

#' Check Class
#'
#' @param class (`character`)\cr
#'   character vector or string indicating accepted classes.
#'   Passed to `inherits(what=class)`
#' @param x `(object)`\cr
#'   object to check
#' @param allow_empty (`logical(1)`)\cr
#'   Logical indicating whether an empty value will pass the test.
#'   Default is `FALSE`
#' @param arg_name (`string`)\cr
#'   string indicating the label/symbol of the object being checked.
#'   Default is `rlang::caller_arg(x)`
#' @inheritParams cli::cli_abort
#' @keywords internal
#' @noRd
check_class <- function(x, class, allow_empty = FALSE,
                        message = "The {.arg {arg_name}} argument must be class
                                   {.cls {class}}, not {.obj_type_friendly {x}}.",
                        arg_name = rlang::caller_arg(x),
                        call = parent.frame()) {
  # if empty, skip test
  if (isTRUE(allow_empty) && rlang::is_empty(x)) {
    return(invisible())
  }

  if (!inherits(x, class)) {
    cli::cli_abort(message, call = call)
  }
  invisible()
}

#' Check Class Data Frame
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_data_frame <- function(x, allow_empty = FALSE,
                             message = "The {.arg {arg_name}} argument must be class
                                              {.cls {class}}, not {.obj_type_friendly {x}}.",
                             arg_name = rlang::caller_arg(x), call = parent.frame()) {
  check_class(
    x = x, class = "data.frame", allow_empty = allow_empty,
    message = message, arg_name = arg_name, call = call
  )
}

#' Check Class Logical
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_logical <- function(x, allow_empty = FALSE,
                          message = "The {.arg {arg_name}} argument must be class
                                              {.cls {class}}, not {.obj_type_friendly {x}}.",
                          arg_name = rlang::caller_arg(x), call = parent.frame()) {
  check_class(
    x = x, class = "logical", allow_empty = allow_empty,
    message = message, arg_name = arg_name, call = call
  )
}

#' Check Class Logical and Scalar
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_scalar_logical <- function(x, allow_empty = FALSE,
                                 message = "The {.arg {arg_name}} argument must be a scalar with class
                                              {.cls {class}}, not {.obj_type_friendly {x}}.",
                                 arg_name = rlang::caller_arg(x), call = parent.frame()) {
  check_logical(
    x = x, allow_empty = allow_empty,
    message = message, arg_name = arg_name,
    call = call
  )

  check_scalar(
    x = x, allow_empty = allow_empty,
    message = message, arg_name = arg_name,
    call = call
  )
}

#' Check Argument not Missing
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_not_missing <- function(x,
                              message = "The {.arg {arg_name}} argument cannot be missing.",
                              arg_name = rlang::caller_arg(x), call = parent.frame()) {
  if (missing(x)) {
    cli::cli_abort(message, call = call)
  }
  invisible()
}

#' Check Length
#'
#' @param msg (`string`)\cr
#'   string passed to `cli::cli_abort(message=)`
#' @param length (`integer(1)`)\cr
#'   integer specifying the required length
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_length <- function(x, length,
                         message =
                           ifelse(
                             allow_empty,
                             "The {.arg {arg_name}} argument must be length {.val {length}} or empty.",
                             "The {.arg {arg_name}} argument must be length {.val {length}}."
                           ),
                         allow_empty = FALSE,
                         arg_name = rlang::caller_arg(x), call = parent.frame()) {
  # if empty, skip test
  if (isTRUE(allow_empty) && rlang::is_empty(x)) {
    return(invisible())
  }

  # check length
  if (length(x) != length) {
    cli::cli_abort(message, call = call)
  }

  invisible()
}

#' Check is Scalar
#'
#' @param msg (`string`)\cr
#'   string passed to `cli::cli_abort(message=)`
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_scalar <- function(x,
                         message =
                           ifelse(
                             allow_empty,
                             "The {.arg {arg_name}} argument must be length {.val {length}} or empty.",
                             "The {.arg {arg_name}} argument must be length {.val {length}}."
                           ),
                         allow_empty = FALSE,
                         arg_name = rlang::caller_arg(x), call = parent.frame()) {
  check_length(
    x = x, length = 1L, message = message,
    allow_empty = allow_empty, arg_name = arg_name, call = call
  )
}

#' Check Range
#'
#' @param x numeric scalar to check
#' @param range numeric vector of length two
#' @param include_bounds logical of length two indicating whether to allow
#'   the lower and upper bounds
#' @param scalar logical indicating whether `x` must be a scalar
#' @param msg string passed to `cli::cli_abort(message=)`
#'
#' @return invisible
#' @keywords internal
#' @noRd
check_range <- function(x,
                        range,
                        include_bounds = c(FALSE, FALSE),
                        message =
                          paste0(
                            "The {.arg {arg_name}} argument must be in the interval
                           {.code {ifelse(include_bounds[1], '[', '(')}{range[1]},
                           {range[2]}{ifelse(include_bounds[2], ']', ')')}}",
                           ifelse(scalar, " and length {.val {1}}", ""),
                           "."),
                        scalar = FALSE,
                        allow_empty = FALSE,
                        arg_name = rlang::caller_arg(x),
                        call = parent.frame()) {
  # if empty, skip test
  if (isTRUE(allow_empty) && rlang::is_empty(x)) {
    return(invisible())
  }

  if (isTRUE(scalar)) {
    check_scalar(x, message = message, arg_name = arg_name, call = call)
  }

  print_error <- FALSE
  # check input is numeric
  if (!is.numeric(x)) {
    print_error <- TRUE
  }

  # check the lower bound of range
  if (isFALSE(print_error) && isTRUE(include_bounds[1]) && any(x < range[1])) {
    print_error <- TRUE
  }
  if (isFALSE(print_error) && isFALSE(include_bounds[1]) && any(x <= range[1])) {
    print_error <- TRUE
  }

  # check upper bound of range
  if (isFALSE(print_error) && isTRUE(include_bounds[2]) && any(x > range[2])) {
    print_error <- TRUE
  }
  if (isFALSE(print_error) && isFALSE(include_bounds[2]) && any(x >= range[2])) {
    print_error <- TRUE
  }

  # print error
  if (print_error) {
    cli::cli_abort(message, call = call)
  }

  invisible()
}


#' Check Binary
#'
#' Checks if a column in a data frame is binary,
#' that is, if the column is class `<logical>` or
#' `<numeric/integer>` and coded as `c(0, 1)`
#'
#' @param x a vector
#' @param call call environment
#'
#' @return invisible
#' @keywords internal
#' @noRd
check_binary <- function(x,
                         message =
                           "Expecting {.arg {arg_name}} to be either {.cls logical}
                            or {.cls {c('numeric', 'integer')}} coded as {.val {c(0, 1)}}.",
                         allow_empty = FALSE,
                         arg_name = rlang::caller_arg(x), call = parent.frame()) {
  # if empty, skip test
  if (isTRUE(allow_empty) && rlang::is_empty(x)) {
    return(invisible())
  }

  # first check x is either logical or numeric
  check_class(x, class = c("logical", "numeric", "integer"),
              arg_name = arg_name, message = message, call = call)

  # if "numeric" or "integer", it must be coded as 0, 1
  if (!is.logical(x) && !(rlang::is_integerish(x) && rlang::is_empty(setdiff(x, c(0, 1, NA))))) {
    cli::cli_abort(message, call = call)
  }

  invisible()
}

# nocov end
# styler: on
