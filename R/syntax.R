#' Syntax and Notation
#'
#' @name syntax
#' @keywords internal
#' @description
#'
#' # Selectors
#'
#' The gtsummary package also utilizes selectors: selectors from the tidyselect
#' package and custom selectors. Review their help files for details.
#'
#' - **tidy selectors**
#'
#'     `everything()`, `all_of()`, `any_of()`, `starts_with()`, `ends_with()`,
#'     `contains()`, `matches()`, `num_range()`, `last_col()`
#'
#' - **gtsummary selectors**
#'
#'     `all_continuous()`, `all_categorical()`, `all_dichotomous()`,
#'     `all_continuous2()`, `all_tests()`, `all_stat_cols()`,
#'     `all_interaction()`, `all_intercepts()`, `all_contrasts()`
#'
#' # Formula and List Selectors
#'
#' Many arguments throughout the gtsummary package accept list and
#' formula notation, e.g. `tbl_summary(statistic=)`. Below enumerates a few
#' tips and shortcuts for using the list and formulas.
#'
#' 1. **List of Formulas**
#'
#'     Typical usage includes a list of formulas, where the LHS is a variable
#'     name or a selector.
#'
#'     ```r
#'     tbl_summary(statistic = list(age ~ "{mean}", all_categorical() ~ "{n}"))
#'     ```
#'
#' 1. **Named List**
#'
#'     You may also pass a named list; however, the tidyselect and gtsummary selectors
#'     are not supported with this syntax.
#'
#'     ```r
#'     tbl_summary(statistic = list(age = "{mean}", response = "{n}"))
#'     ```
#'
#' 1. **Hybrid Named List/List of Formulas**
#'
#'     Pass a combination of formulas and named elements
#'
#'     ```r
#'     tbl_summary(statistic = list(age = "{mean}", all_categorical() ~ "{n}"))
#'     ```
#' 1. **Shortcuts**
#'
#'     You can pass a single formula, which is equivalent to passing the formula
#'     in a list.
#'
#'     ```r
#'     tbl_summary(statistic = all_categorical() ~ "{n}")
#'     ```
#'     As a shortcut to select all variables, you can omit the LHS of the formula.
#'     The two calls below are equivalent.
#'
#'     ```r
#'     tbl_summary(statistic = ~"{n}")
#'     tbl_summary(statistic = everything() ~ "{n}")
#'     ```
#'
#' 1. **Combination Selectors**
#'
#'     Selectors can be combined using the `c()` function.
#'
#'     ```r
#'     tbl_summary(statistic = c(everything(), -grade) ~ "{n}")
#'     ```
NULL
