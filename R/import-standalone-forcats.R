# Standalone file: do not edit by hand
# Source: <https://github.com/ddsjoberg/standalone/blob/main/R/standalone-forcats.R>
# ----------------------------------------------------------------------
#
# ---
# file: standalone-forcats.R
# last-updated: 2024-01-24
# license: https://unlicense.org
# imports:
# ---
#
# This file provides a minimal shim to provide a forcats-like API on top of
# base R functions. They are not drop-in replacements but allow a similar style
# of programming.
#
# ## Changelog
#
# nocov start
# styler: off

fct_infreq <- function(f, ordered = NA) {
  # reorder by frequency
  factor(
    f,
    levels = table(f) |> sort(decreasing = TRUE) |> names(),
    ordered = ifelse(is.na(ordered), is.ordered(f), ordered)
  )
}

fct_inorder <- function(f, ordered = NA) {
  factor(
    f,
    levels = stats::na.omit(unique(f)) |> union(levels(f)),
    ordered = ifelse(is.na(ordered), is.ordered(f), ordered)
  )
}

# nocov end
# styler: on
