# Standalone file: do not edit by hand
# Source: <https://github.com/ddsjoberg/standalone/blob/main/R/standalone-tibble.R>
# ----------------------------------------------------------------------
#
# ---
# file: standalone-tibble.R
# last-updated: 2024-03-09
# license: https://unlicense.org
# imports: [dplyr]
# ---
#
# This file provides a minimal shim to provide a tibble-like API on top of
# base R functions. They are not drop-in replacements but allow a similar style
# of programming.
#
# ## Changelog
#
# nocov start
# styler: off

enframe <- function(x, name = "name", value = "value") {
  if (!is.null(names(x))) {
    lst <- list(names(x), unname(x)) |> stats::setNames(c(name, value))
  }
  else {
    lst <- list(seq_along(x), unname(x)) |> stats::setNames(c(name, value))
  }
  dplyr::tibble(!!!lst)
}

remove_rownames <- function(.data) {
  rownames(.data) <- NULL
  .data
}

rownames_to_column <- function(.data, var = "rowname") {
  .data[[var]] <- rownames(.data)

  dplyr::relocate(.data, dplyr::all_of(var), .before = 1L)
}

# nocov end
# styler: on
