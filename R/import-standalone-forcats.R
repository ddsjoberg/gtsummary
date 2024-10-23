# Standalone file: do not edit by hand
# Source: <https://github.com/insightsengineering/standalone/blob/main/R/standalone-forcats.R>
# ----------------------------------------------------------------------
#
# ---
# repo: insightsengineering/standalone
# file: standalone-forcats.R
# last-updated: 2024-06-05
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

fct_rev <- function(f) {
  if (!inherits(f, "factor")) f <- factor(f)

  factor(
    f,
    levels = rev(levels(f)),
    ordered = is.ordered(f)
  )
}

fct_expand <- function(f, ..., after = Inf) {
  if (!inherits(f, "factor")) f <- factor(f)

  old_levels <- levels(f)
  new_levels <-
    old_levels |>
    append(values = setdiff(c(...), old_levels), after = after)
  factor(f, levels = new_levels)
}

fct_na_value_to_level <- function(f, level = NA) {
  if (!inherits(f, "factor")) f <- factor(f)

  # make NA an explicit level
  f <- addNA(f, ifany = FALSE)

  # replace NA level with the string passed in `level` argument
  if (!is.na(level)) levels(f)[is.na(levels(f))] <- level

  f
}



# nocov end
# styler: on
