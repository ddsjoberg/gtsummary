# Syntax and Notation

Syntax and Notation

## Selectors

The gtsummary package also utilizes selectors: selectors from the
tidyselect package and custom selectors. Review their help files for
details.

- **tidy selectors**

  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html),
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html),
  [`any_of()`](https://tidyselect.r-lib.org/reference/all_of.html),
  [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`matches()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`num_range()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`last_col()`](https://tidyselect.r-lib.org/reference/everything.html)

- **gtsummary selectors**

  [`all_continuous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  [`all_categorical()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  [`all_dichotomous()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  [`all_continuous2()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  [`all_tests()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  [`all_stat_cols()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  [`all_interaction()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  [`all_intercepts()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md),
  [`all_contrasts()`](https://www.danieldsjoberg.com/gtsummary/dev/reference/select_helpers.md)

## Formula and List Selectors

Many arguments throughout the gtsummary package accept list and formula
notation, e.g. `tbl_summary(statistic=)`. Below enumerates a few tips
and shortcuts for using the list and formulas.

1.  **List of Formulas**

    Typical usage includes a list of formulas, where the LHS is a
    variable name or a selector.

        tbl_summary(statistic = list(age ~ "{mean}", all_categorical() ~ "{n}"))

2.  **Named List**

    You may also pass a named list; however, the tidyselect and
    gtsummary selectors are not supported with this syntax.

        tbl_summary(statistic = list(age = "{mean}", response = "{n}"))

3.  **Hybrid Named List/List of Formulas**

    Pass a combination of formulas and named elements

        tbl_summary(statistic = list(age = "{mean}", all_categorical() ~ "{n}"))

4.  **Shortcuts**

    You can pass a single formula, which is equivalent to passing the
    formula in a list.

        tbl_summary(statistic = all_categorical() ~ "{n}")

    As a shortcut to select all variables, you can omit the LHS of the
    formula. The two calls below are equivalent.

        tbl_summary(statistic = ~"{n}")
        tbl_summary(statistic = everything() ~ "{n}")

5.  **Combination Selectors**

    Selectors can be combined using the
    [`c()`](https://rdrr.io/r/base/c.html) function.

        tbl_summary(statistic = c(everything(), -grade) ~ "{n}")
