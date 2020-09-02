#' Copy of tidyselect's unexported `where()` function
#'
#' Need this function when we do checks if the select helpers are wrapped in `var()`.
#' If it is not present, users cannot use `where(is.numeric)` type selectors.
#' @noRd

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
