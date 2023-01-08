#' Check a package installation
#'
#' The function checks whether a package is installed and returns an error
#' or `FALSE` if not available. If a package search is provided, the function
#' will check whether a minimum version of a package is required.
#'
#' @inheritParams broom.helpers::.assert_package
#' @noRd
#' @return Returns NULL or not at all.
#' @examples
#' assert_package("gt", boolean = TRUE)
#'
assert_package <- function(pkg, fn = NULL, boolean = FALSE) {
  broom.helpers::.assert_package(
    pkg = pkg,
    fn = fn,
    pkg_search = "gtsummary",
    boolean = boolean
  )
}
