
#' Check that a package is installed, stopping otherwise
#'
#' @param pkg Package required
#' @param fn Calling function from the user perspective
#'
#' @return Returns NULL or not at all.
#'
#' @noRd
#' @keywords internal
#' @author David Hugh-Jones
assert_package <- function(pkg, fn) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(glue::glue(
      "The '{pkg}' package is required for '{fn}'.\n",
      "Install with install.packages('{pkg}')"
    ), call. = FALSE)
  }
}
