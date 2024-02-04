# ---
# repo: ddsjoberg/gtsummary
# file: standalone-assertions.R
# last-updated:
# license: https://unlicense.org
# dependencies: rlang, cli
# ---
#
# This file provides a minimal shim to provide checks or assertions,
#   typically of function argument inputs.
#
# ## Changelog
#
# nocov start

#' Check Argument Class
#'
#' @param x object whose class will be checked
#' @param class character vector or string indicating accepted classes.
#' Passed to `inherits(what=class)`
#' @param length if not `NULL`, the `x` object must be length specified
#' @param allow_empty logical indicating whether empty arguments are allowed.
#' Empty is defined by `rlang::is_empty()`, where empty elements include
#' `NULL`, `list()`, `character()`, etc. Default is `FALSE`
#' @param arg_name string indicating the argument name. Default is `rlang::caller_arg(x)`
#' @param allow_null Logical indicating whether a NULL value will pass the test.
#' Default is `FALSE`
#' @inheritParams cli::cli_abort
#' @keywords internal
#' @name check_class
NULL

#' @rdname check_class
check_class <- function(x, class, length = NULL, allow_empty = FALSE,
                        arg_name = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (isTRUE(allow_empty) && rlang::is_empty(x)) {
    return(invisible())
  }
  if (!is.null(length)) {
    check_length(x = x, length = length, arg_name = arg_name, call = call)
  }

  if (!inherits(x, class)) {
    cli::cli_abort(
      c("Argument {.arg {arg_name}} must be class {.cls {class}}.",
        "i" = "The class of {.arg {arg_name}} is {.cls {class(x)}}."),
      call = call
    )
  }
  invisible()
}

#' @rdname check_class
check_class_data_frame <- function(x, arg_name = rlang::caller_arg(x), call = parent.frame()) {
  check_class(x = x, class = "data.frame", arg_name = arg_name, call = call)
}

#' Check Argument not Missing
#'
#' @param x argument to check
#' @param arg_name string indicating the name of the argument. Used in the error messaging.
#' @inheritParams check_class
#' @keywords internal
check_not_missing <- function(x, arg_name = rlang::caller_arg(x), call = parent.frame()) {
  if (missing(x)) {
    cli::cli_abort("The {.arg {arg_name}} argument cannot be missing.", call = call)
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
#' @name check_length
NULL

#' @rdname check_length
check_length <- function(x, length, arg_name = caller_arg(x), call = parent.frame()) {
  if (length(x) != length) {
    cli::cli_abort("The {.arg {arg_name}} argument must be length {.val {length}}.", call = call)
  }
  invisible()
}

#' @rdname check_length
check_scalar <- function(x, arg_name = caller_arg(x), call = parent.frame()) {
  check_length(x = x, length = 1L, arg_name = arg_name, call = call)
}
