# Standalone file: do not edit by hand
# Source: <https://github.com/insightsengineering/standalone/blob/main/R/standalone-check_pkg_installed.R>
# ----------------------------------------------------------------------
#
# ---
# repo: insightsengineering/standalone
# file: standalone-check_pkg_installed.R
# last-updated: 2024-04-19
# license: https://unlicense.org
# dependencies: standalone-cli_call_env.R
# imports: [rlang, dplyr, tidyr]
# ---
#
# This file provides functions to check package installation.
#
# ## Changelog
# nocov start
# styler: off

#' Check Package Installation
#'
#' @description
#' - `check_pkg_installed()`: checks whether a package is installed and
#'   returns an error if not available, or interactively asks user to install
#'   missing dependency. If a package search is provided,
#'   the function will check whether a minimum version of a package is required and installed.
#'
#' - `is_pkg_installed()`: checks whether a package is installed and
#'   returns `TRUE` or `FALSE` depending on availability. If a package search is provided,
#'   the function will check whether a minimum version of a package is required and installed.
#'
#' - `get_pkg_dependencies()` returns a tibble with all
#'   dependencies of a specific package.
#'
#' - `get_min_version_required()` will return, if any, the minimum version of `pkg` required by `ref`.
#'
#' @param pkg (`character`)\cr
#'   vector of package names to check.
#' @param call (`environment`)\cr
#'   frame for error messaging. Default is [get_cli_abort_call()].
#' @param ref (`string`)\cr
#'   name of the package the function will search for a minimum required version from.
#' @param lib.loc (`path`)\cr
#'   location of `R` library trees to search through, see [utils::packageDescription()].
#'
#' @details
#' The `ref` argument (`pkg` in `get_pkg_dependencies`) uses `utils::packageName()` as a default, which returns the package in
#' which the current environment or function is run from. The current environment is determined via `parent.frame()`.
#'
#' If, for example, `get_min_version_required("dplyr", ref = utils::packageName())` is run within a `cards` function, and this
#' function is then called within a function of the `cardx` package, the minimum version returned by the
#' `get_min_version_required` call will return the version required by the `cards` package. If run within a test file,
#' `utils::packageName()` returns the package of the current test. Within Roxygen `@examplesIf` calls, `utils::packageName()` will
#' returns the package of the current example.
#'
#' @return `is_pkg_installed()` and `check_pkg_installed()` returns a logical or error,
#' `get_min_version_required()` returns a data frame with the minimum version required,
#' `get_pkg_dependencies()` returns a tibble.
#'
#' @examples
#' check_pkg_installed("dplyr")
#'
#' is_pkg_installed("dplyr")
#'
#' get_pkg_dependencies()
#'
#' get_min_version_required("dplyr")
#'
#' @name check_pkg_installed
#' @noRd
NULL

#' @inheritParams check_pkg_installed
#' @keywords internal
#' @noRd
check_pkg_installed <- function(pkg,
                                ref = utils::packageName(),
                                call = get_cli_abort_call()) {
  if (!is.character(ref) && !is.null(ref)) cli::cli_abort("{.arg ref} must be a string.")

  # get min version data -------------------------------------------------------
  df_pkg_min_version <-
    get_min_version_required(pkg = pkg, ref = ref)

  # prompt user to install package ---------------------------------------------
  rlang::check_installed(
    pkg = df_pkg_min_version$pkg,
    version = df_pkg_min_version$version,
    compare = df_pkg_min_version$compare,
    call = call
  ) |>
    # this can be removed after this issue is resolved https://github.com/r-lib/rlang/issues/1694
    suppressWarnings()
}

#' @inheritParams check_pkg_installed
#' @keywords internal
#' @noRd
is_pkg_installed <- function(pkg,
                             ref = utils::packageName()) {
  if (!is.character(ref) && !is.null(ref)) cli::cli_abort("{.arg ref} must be a string.")

  # get min version data -------------------------------------------------------
  df_pkg_min_version <-
    get_min_version_required(pkg = pkg, ref = ref)

  # check installation TRUE/FALSE ----------------------------------------------
  rlang::is_installed(
    pkg = df_pkg_min_version$pkg,
    version = df_pkg_min_version$version,
    compare = df_pkg_min_version$compare
  ) |>
    # this can be removed after this issue is resolved https://github.com/r-lib/rlang/issues/1694
    suppressWarnings()
}

#' @inheritParams check_pkg_installed
#' @keywords internal
#'
#' @param pkg (`string`)\cr
#'   name of the package the function will search for dependencies from.
#'
#' @noRd
get_pkg_dependencies <- function(pkg = utils::packageName(), lib.loc = NULL) {
  if (!is.character(pkg) && !is.null(pkg)) cli::cli_abort("{.arg pkg} must be a string.")

  if (rlang::is_empty(pkg)) {
    return(.empty_pkg_deps_df())
  }

  description <- utils::packageDescription(pkg, lib.loc = lib.loc) |> suppressWarnings()
  if (identical(description, NA)) {
    return(.empty_pkg_deps_df())
  }
  description |>
    unclass() |>
    dplyr::as_tibble() |>
    dplyr::select(
      dplyr::any_of(c(
        "Package", "Version", "Imports", "Depends",
        "Suggests", "Enhances", "LinkingTo"
      ))
    ) |>
    dplyr::rename(
      reference_pkg = "Package",
      reference_pkg_version = "Version"
    ) |>
    tidyr::pivot_longer(
      -dplyr::all_of(c("reference_pkg", "reference_pkg_version")),
      values_to = "pkg",
      names_to = "dependency_type",
    ) |>
    tidyr::separate_rows("pkg", sep = ",") |>
    dplyr::mutate(
      pkg = trimws(
        x = gsub(x = .data$pkg, pattern = "\\s+", replacement = " "),
        which = "both", whitespace = "[ \t\r\n]"
      )
    ) |>
    dplyr::filter(!is.na(.data$pkg)) |>
    tidyr::separate(
      .data$pkg,
      into = c("pkg", "version"),
      sep = " ", extra = "merge", fill = "right"
    ) |>
    dplyr::mutate(
      compare = .data$version |> str_extract(pattern = "[>=<]+"),
      version = .data$version |> str_remove_all(pattern = "[\\(\\) >=<]")
    )
}

.empty_pkg_deps_df <- function() {
  dplyr::tibble(
    reference_pkg = character(0L), reference_pkg_version = character(0L),
    dependency_type = character(0L), pkg = character(0L),
    version = character(0L), compare = character(0L)
  )
}

#' @inheritParams check_pkg_installed
#' @keywords internal
#' @noRd
get_min_version_required <- function(pkg, ref = utils::packageName(), lib.loc = NULL) {
  if (!is.character(ref) && !is.null(ref)) cli::cli_abort("{.arg ref} must be a string.")

  # if no package reference, return a df with just the pkg names
  if (rlang::is_empty(ref)) {
    return(
      .empty_pkg_deps_df() |>
        dplyr::full_join(
          dplyr::tibble(pkg = pkg),
          by = "pkg"
        )
    )
  }

  # get the package_ref deps and subset on requested pkgs, also supplement df with pkgs
  # that may not be proper deps of the reference package (these pkgs don't have min versions)
  res <-
    get_pkg_dependencies(ref, lib.loc = lib.loc) |>
    dplyr::filter(.data$pkg %in% .env$pkg) |>
    dplyr::full_join(
      dplyr::tibble(pkg = pkg),
      by = "pkg"
    )

  res
}

# nocov end
# styler: on
