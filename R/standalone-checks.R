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

assert_class <- function(x, class, arg_name = rlang::caller_arg(x), env = rlang::caller_env()) {
  if (!inherits(x, class)) {
    cli::cli_abort(
      c("Argument {.arg {arg_name}} must be class {.cls {class}}.",
        "i" = "The class of {.arg {arg_name}} is {.cls {class(x)}}."),
      call = env
    )
  }
}
