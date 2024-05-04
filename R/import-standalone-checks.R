# Standalone file: do not edit by hand
# Source: <https://github.com/ddsjoberg/standalone/blob/main/R/standalone-checks.R>
# ----------------------------------------------------------------------
#
# ---
# repo: ddsjoberg/standalone
# file: standalone-checks.R
# last-updated: 2024-04-10
# license: https://unlicense.org
# dependencies: standalone-cli_call_env.R
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
#' @param cls (`character`)\cr
#'   character vector or string indicating accepted classes.
#'   Passed to `inherits(what=cls)`
#' @param x `(object)`\cr
#'   object to check
#' @param message (`character`)\cr
#'   string passed to `cli::cli_abort(message)`
#' @param allow_empty (`logical(1)`)\cr
#'   Logical indicating whether an empty value will pass the test.
#'   Default is `FALSE`
#' @param arg_name (`string`)\cr
#'   string indicating the label/symbol of the object being checked.
#'   Default is `rlang::caller_arg(x)`
#' @inheritParams cli::cli_abort
#' @inheritParams rlang::abort
#' @keywords internal
#' @noRd
check_class <- function(x,
                        cls,
                        allow_empty = FALSE,
                        message =
                          ifelse(
                            allow_empty,
                            "The {.arg {arg_name}} argument must be class
                             {.cls {cls}} or empty, not {.obj_type_friendly {x}}.",
                            "The {.arg {arg_name}} argument must be class
                             {.cls {cls}}, not {.obj_type_friendly {x}}."
                          ),
                        arg_name = rlang::caller_arg(x),
                        class = "check_class",
                        call = get_cli_abort_call()) {
  # if empty, skip test
  if (isTRUE(allow_empty) && rlang::is_empty(x)) {
    return(invisible(x))
  }

  if (!inherits(x, cls)) {
    cli::cli_abort(message, class = c(class, "standalone-checks"), call = call)
  }
  invisible(x)
}

#' Check Class Data Frame
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_data_frame <- function(x,
                             allow_empty = FALSE,
                             message =
                               ifelse(
                                 allow_empty,
                                 "The {.arg {arg_name}} argument must be class
                                  {.cls {cls}} or empty, not {.obj_type_friendly {x}}.",
                                 "The {.arg {arg_name}} argument must be class
                                  {.cls {cls}}, not {.obj_type_friendly {x}}."
                               ),
                             arg_name = rlang::caller_arg(x),
                             class = "check_data_frame",
                             call = get_cli_abort_call()) {
  check_class(
    x = x, cls = "data.frame", allow_empty = allow_empty,
    message = message, arg_name = arg_name, class = class, call = call
  )
}

#' Check Class Logical
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_logical <- function(x,
                          allow_empty = FALSE,
                          message =
                            ifelse(
                              allow_empty,
                              "The {.arg {arg_name}} argument must be class
                               {.cls {cls}} or empty, not {.obj_type_friendly {x}}.",
                              "The {.arg {arg_name}} argument must be class
                               {.cls {cls}}, not {.obj_type_friendly {x}}."
                            ),
                          arg_name = rlang::caller_arg(x),
                          class = "check_logical",
                          call = get_cli_abort_call()) {
  check_class(
    x = x, cls = "logical", allow_empty = allow_empty,
    message = message, arg_name = arg_name, class = class, call = call
  )
}

#' Check Class Logical and Scalar
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_scalar_logical <- function(x,
                                 allow_empty = FALSE,
                                 message =
                                   ifelse(
                                     allow_empty,
                                     "The {.arg {arg_name}} argument must be a scalar with class
                                      {.cls {cls}} or empty, not {.obj_type_friendly {x}}.",
                                     "The {.arg {arg_name}} argument must be a scalar with class
                                      {.cls {cls}}, not {.obj_type_friendly {x}}."
                                   ),
                                 arg_name = rlang::caller_arg(x),
                                 class = "check_scalar_logical",
                                 call = get_cli_abort_call()) {
  check_logical(
    x = x, allow_empty = allow_empty,
    message = message, arg_name = arg_name,
    class = class, call = call
  )

  check_scalar(
    x = x, allow_empty = allow_empty,
    message = message, arg_name = arg_name,
    call = call
  )
}

#' Check String
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_string <- function(x,
                         allow_empty = FALSE,
                         message =
                           ifelse(
                             allow_empty,
                             "The {.arg {arg_name}} argument must be a string or empty,
                              not {.obj_type_friendly {x}}.",
                             "The {.arg {arg_name}} argument must be a string,
                              not {.obj_type_friendly {x}}."
                           ),
                         arg_name = rlang::caller_arg(x),
                         class = "check_string",
                         call = get_cli_abort_call()) {
  check_class(
    x = x, cls = "character", allow_empty = allow_empty,
    message = message, arg_name = arg_name,
    class = class, call = call
  )

  check_scalar(
    x = x, allow_empty = allow_empty,
    message = message, arg_name = arg_name,
    class = class, call = call
  )
}

#' Check Argument not Missing
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_not_missing <- function(x,
                              message = "The {.arg {arg_name}} argument cannot be missing.",
                              arg_name = rlang::caller_arg(x),
                              class = "check_not_missing",
                              call = get_cli_abort_call()) {
  if (missing(x)) {
    cli::cli_abort(message, class = c(class, "standalone-checks"), call = call)
  }

  # can't return 'x' because it may be an unevaluable obj, eg a bare tidyselect
  invisible()
}

#' Check Length
#'
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
                         arg_name = rlang::caller_arg(x),
                         class = "check_length",
                         call = get_cli_abort_call()) {
  # if empty, skip test
  if (isTRUE(allow_empty) && rlang::is_empty(x)) {
    return(invisible(x))
  }

  # check length
  if (length(x) != length) {
    cli::cli_abort(message, class = c(class, "standalone-checks"), call = call)
  }

  invisible(x)
}

#' Check is Scalar
#'
#' @inheritParams check_class
#' @keywords internal
#' @noRd
check_scalar <- function(x,
                         allow_empty = FALSE,
                         message =
                           ifelse(
                             allow_empty,
                             "The {.arg {arg_name}} argument must be length {.val {length}} or empty.",
                             "The {.arg {arg_name}} argument must be length {.val {length}}."
                           ),
                         arg_name = rlang::caller_arg(x),
                         class = "check_scalar",
                         call = get_cli_abort_call()) {
  check_length(
    x = x, length = 1L, message = message,
    allow_empty = allow_empty, arg_name = arg_name,
    class = class, call = call
  )
}

#' Check Range
#'
#' @param x numeric scalar to check
#' @param range numeric vector of length two
#' @param include_bounds logical of length two indicating whether to allow
#'   the lower and upper bounds
#' @inheritParams check_class
#'
#' @return invisible
#' @keywords internal
#' @noRd
check_range <- function(x,
                        range,
                        include_bounds = c(FALSE, FALSE),
                        message =
                          "The {.arg {arg_name}} argument must be in the interval
                           {.code {ifelse(include_bounds[1], '[', '(')}{range[1]},
                           {range[2]}{ifelse(include_bounds[2], ']', ')')}}.",
                        allow_empty = FALSE,
                        arg_name = rlang::caller_arg(x),
                        class = "check_range",
                        call = get_cli_abort_call()) {
  # if empty, skip test
  if (isTRUE(allow_empty) && rlang::is_empty(x)) {
    return(invisible(x))
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
    cli::cli_abort(message, class = c(class, "standalone-checks"), call = call)
  }

  invisible(x)
}

#' Check Scalar Range
#'
#' @param x numeric scalar to check
#' @param range numeric vector of length two
#' @param include_bounds logical of length two indicating whether to allow
#'   the lower and upper bounds
#' @inheritParams check_class
#'
#' @return invisible
#' @keywords internal
#' @noRd
check_scalar_range <- function(x,
                               range,
                               include_bounds = c(FALSE, FALSE),
                               allow_empty = FALSE,
                               message =
                                 "The {.arg {arg_name}} argument must be in the interval
                                  {.code {ifelse(include_bounds[1], '[', '(')}{range[1]},
                                  {range[2]}{ifelse(include_bounds[2], ']', ')')}}
                                  and length {.val {1}}.",
                               arg_name = rlang::caller_arg(x),
                               class = "check_scalar_range",
                               call = get_cli_abort_call()) {
  check_scalar(x, message = message, arg_name = arg_name,
               allow_empty = allow_empty, class = class, call = call)

  check_range(x = x, range = range, include_bounds = include_bounds,
              message = message, allow_empty = allow_empty,
              arg_name = arg_name, class = class, call = call)
}

#' Check Binary
#'
#' Checks if a column in a data frame is binary,
#' that is, if the column is class `<logical>` or
#' `<numeric/integer>` and coded as `c(0, 1)`
#'
#' @param x a vector
#' @inheritParams check_class
#'
#' @return invisible
#' @keywords internal
#' @noRd
check_binary <- function(x,
                         allow_empty = FALSE,
                         message =
                           ifelse(
                             allow_empty,
                             "Expecting {.arg {arg_name}} to be either {.cls logical},
                              {.cls {c('numeric', 'integer')}} coded as {.val {c(0, 1)}}, or empty.",
                             "Expecting {.arg {arg_name}} to be either {.cls logical}
                             or {.cls {c('numeric', 'integer')}} coded as {.val {c(0, 1)}}."
                           ),
                         arg_name = rlang::caller_arg(x),
                         class = "check_binary",
                         call = get_cli_abort_call()) {
  # if empty, skip test
  if (isTRUE(allow_empty) && rlang::is_empty(x)) {
    return(invisible(x))
  }

  # first check x is either logical or numeric
  check_class(x, cls = c("logical", "numeric", "integer"),
              arg_name = arg_name, message = message, class = class, call = call)

  # if "numeric" or "integer", it must be coded as 0, 1
  if (!is.logical(x) && !(rlang::is_integerish(x) && rlang::is_empty(setdiff(x, c(0, 1, NA))))) {
    cli::cli_abort(message, class = c(class, "standalone-checks"), call = call)
  }

  invisible(x)
}


#' Check Formula-List Selector
#'
#' Checks the structure of the formula-list selector used throughout the
#' cards, cardx, and gtsummary packages.
#'
#' @param x formula-list selecting object
#' @inheritParams check_class
#'
#' @return invisible
#' @keywords internal
#' @noRd
check_formula_list_selector <- function(x,
                                        allow_empty = FALSE,
                                        message =
                                          c(
                                            ifelse(
                                              allow_empty,
                                              "The {.arg {arg_name}} argument must be a named list, list of formulas, a single formula, or empty.",
                                              "The {.arg {arg_name}} argument must be a named list, list of formulas, or a single formula."
                                            ),
                                            "i" = "Review {.help [?syntax](cards::syntax)} for examples and details."
                                          ),
                                        arg_name = rlang::caller_arg(x),
                                        class = "check_formula_list_selector",
                                        call = get_cli_abort_call()) {
  # if empty, skip test
  if (isTRUE(allow_empty) && rlang::is_empty(x)) {
    return(invisible(x))
  }

  # first check the general structure; must be a list or formula
  check_class(
    x = x, cls = c("list", "formula"), allow_empty = allow_empty,
    message = message, arg_name = arg_name, class = class, call = call
  )

  # if it's a list, then check each element is either named or a formula
  if (inherits(x, "list")) {
    for (i in seq_along(x)) {
      if (!rlang::is_named(x[i]) && !inherits(x[[i]], "formula")) {
        cli::cli_abort(message, class = c(class, "standalone-checks"), call = call)
      }
    }
  }

  invisible(x)
}

# nocov end
# styler: on
