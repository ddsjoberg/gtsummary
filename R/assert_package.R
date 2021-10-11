#' Check a package installation
#'
#' The function checks whether a package is installed and returns an error
#' or `FALSE` if not available. If a package search is provided, the function
#' will check whether a minimum version of a package is required.
#'
#' @param pkg Package required
#' @param fn Calling function from the user perspective
#' @param pkg_search the package the function will search for a minimum
#' required version from.
#' @param boolean logical indicating whether to return a TRUE/FALSE, rather
#' than error when package/package version not available.
#'
#' @return Returns NULL or not at all.
#' @export
#' @examples
#' assert_package("gt", boolean = TRUE)

assert_package <- function(pkg, fn = NULL, pkg_search = "gtsummary", boolean = FALSE) {
  version <- get_min_version_required(pkg, pkg_search)
  if (is.null(version) && !requireNamespace(pkg, quietly = TRUE)) {
    if (!is.null(fn)) {
      cli_alert_danger("The {.val {pkg}} package is required for function {.code {fn}}.")
    }
    cli_ul("Install {.val {pkg}} with the code below.")
    cli_code(glue('install.packages("{pkg}")'))
    if (isTRUE(boolean)) return(FALSE)
    stop("Install required package", call. = FALSE)
  }

  if (!is.null(version) &&
      (!requireNamespace(pkg, quietly = TRUE) ||
       (requireNamespace(pkg, quietly = TRUE) && utils::packageVersion(pkg) < version))) {
    if (!is.null(fn)) {
      paste("The {.val {pkg}} package {.field v{version}} or greater is",
            "required for function {.code {fn}}.") %>%
        cli_alert_danger()
    }
    cli_ul("Install/update {.val {pkg}} with the code below.")
    cli_code(glue('install.packages("{pkg}")'))
    if (isTRUE(boolean)) return(FALSE)
    stop("Install required package", call. = FALSE)
  }

  if (isTRUE(boolean)) return(TRUE)
  invisible()
}

# get min version required for a Suggested package in gtsummary
get_min_version_required <- function(pkg, pkg_search) {
  if (is.null(pkg_search)) return(NULL)
  utils::packageDescription(pkg_search, fields = "Suggests") %>%
    stringr::str_remove_all("[\r\n]") %>%
    stringr::str_split(pattern = fixed(",")) %>%
    unlist() %>%
    {.[stringr::word(.) == pkg]} %>%
    stringr::word(start = 2, end = -1) %>%
    stringr::str_remove_all(pattern = " ") %>%
    stringr::str_remove_all(pattern = "^\\(>=|\\)$") %>%
    {switch(!rlang::is_empty(.) && !is.na(.), .)}
}
