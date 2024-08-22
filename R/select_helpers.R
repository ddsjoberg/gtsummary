#' Select helper functions
#'
#' @description Set of functions to supplement the \{tidyselect\} set of
#' functions for selecting columns of data frames (and other items as well).
#' - `all_continuous()` selects continuous variables
#' - `all_continuous2()` selects only type `"continuous2"`
#' - `all_categorical()` selects categorical (including `"dichotomous"`) variables
#' - `all_dichotomous()` selects only type `"dichotomous"`
#' - `all_tests()` selects variables by the name of the test performed
#' - `all_stat_cols()` selects columns from `tbl_summary`/`tbl_svysummary` object with summary statistics (i.e. `"stat_0"`, `"stat_1"`, `"stat_2"`, etc.)
#' - `all_interaction()` selects interaction terms from a regression model
#' - `all_intercepts()` selects intercept terms from a regression model
#' - `all_contrasts()` selects variables in regression model based on their type of contrast
#'
#' @param stat_0 (scalar `logical`)\cr
#'   When `FALSE`, will not select the `"stat_0"` column. Default is `TRUE`
#' @param continuous2 (scalar `logical`)\cr
#'   Logical indicating whether to include continuous2 variables. Default is `TRUE`
#' @param dichotomous (scalar `logical`)\cr
#'   Logical indicating whether to include dichotomous variables. Default is `TRUE`
#' @param tests (`character`)\cr
#'   character vector indicating the test type of the variables to select, e.g.
#'   select all variables being compared with `"t.test"`.
#' @param contrasts_type (`character`)\cr
#'   type of contrast to select. Select among contrast types `c("treatment", "sum", "poly", "helmert", "sdif", "other")`.
#'   Default is all contrast types.
#' @name select_helpers
#' @return A character vector of column names selected
#'
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @examples
#' select_ex1 <-
#'   trial |>
#'   select(age, response, grade) |>
#'   tbl_summary(
#'     statistic = all_continuous() ~ "{mean} ({sd})",
#'     type = all_dichotomous() ~ "categorical"
#'   )
NULL

#' @rdname select_helpers
#' @export
all_continuous <- function(continuous2 = TRUE) {
  types <- if (continuous2) c("continuous", "continuous2") else "continuous"

  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% types))
}

#' @rdname select_helpers
#' @export
all_continuous2 <- function() {
  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% "continuous2"))
}

#' @rdname select_helpers
#' @export
all_categorical <- function(dichotomous = TRUE) {
  types <- if (dichotomous) c("categorical", "dichotomous") else "categorical"

  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% types))
}

#' @rdname select_helpers
#' @export
all_dichotomous <- function() {
  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% "dichotomous"))
}

#' @rdname select_helpers
#' @export
all_tests <- function(tests) {
  check_class(tests, "character")
  where(function(x) isTRUE(attr(x, "gtsummary.test_name") %in% tests))
}

#' @rdname select_helpers
#' @export
all_intercepts <- function() {
  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% "intercept"))
}

#' @rdname select_helpers
#' @export
all_interaction <- function() {
  where(function(x) isTRUE(attr(x, "gtsummary.var_type") %in% "interaction"))
}

#' @rdname select_helpers
#' @export
all_contrasts <- function(contrasts_type = c("treatment", "sum", "poly", "helmert", "sdif", "other")) {
  contrasts_type <- arg_match(contrasts_type, multiple = TRUE)
  where(function(x) isTRUE(attr(x, "gtsummary.contrasts_type") %in% contrasts_type))
}

#' @rdname select_helpers
#' @export
all_stat_cols <- function(stat_0 = TRUE) {
  # finds stat_0, stat_1, stat_2, etc.
  if (stat_0 == TRUE) {
    return(
      union(
        dplyr::matches("^stat_\\d+$"),
        dplyr::matches("^stat_\\d+_.*$")
      )
    )
  }
  # finds stat_1, stat_2, etc.
  if (stat_0 == FALSE) {
    return(
      union(
        dplyr::matches("^stat_\\d*[1-9]\\d*$"),
        dplyr::matches("^stat_\\d*[1-9]\\d*_.*$")
      )
    )
  }
}


#' Scoping for Table Body and Header
#'
#' @description
#' ## `scope_table_body()`
#'
#' This function uses the information in `.$table_body` and adds them
#' as attributes to `data` (if passed). Once they've been assigned as
#' proper gtsummary attributes, gtsummary selectors like `all_continuous()`
#' will work properly.
#'
#' Columns `c("var_type", "test_name", "contrasts_type")` and columns that
#' begin with `"selector_*"` are scoped. The values of these columns are
#' added as attributes to a data frame. For example, if `var_type='continuous'`
#' for variable `"age"`, then the attribute
#' `attr(.$age, 'gtsummary.var_type') <- 'continuous'` is set.
#' That attribute is then used in a selector like `all_continuous()`.
#'
#' ## `scope_header()`
#'
#' This function takes information from `.$table_styling$header` and adds it
#' to `table_body`. Columns that begin with `'modify_selector_'` and the `hide`
#' column.
#'
#' @param table_body a data frame from `.$table_body`
#' @param data an optional data frame the attributes will be added to
#' @param header the header data frame from `.$table_styling$header`
#'
#' @return a data frame
#' @name scoping_gtsummary
#'
#' @keywords internal
#'
#' @examples
#' tbl <- tbl_summary(trial, include = c(age, grade))
#'
#' scope_table_body(tbl$table_body) |> select(all_continuous()) |> names()
NULL

#' @rdname scoping_gtsummary
#' @export
scope_table_body <- function(table_body, data = NULL) {
  if (!"variable" %in% names(table_body)) {
    cli::cli_abort(
      "The {.code .$table_body} data frame does not have the required {.val variable} column.",
      call = get_cli_abort_call()
    )
  }

  # if data not passed, use table_body to construct one
  if (is_empty(data)) {
    data <- dplyr::tibble(!!!rep_named(unique(table_body$variable), logical(0L)))
  }

  # only keeping rows that have corresponding column names in data
  table_body <- table_body |> dplyr::filter(.data$variable %in% names(data))

  # if table_body passed, add columns as attr to data
  base_attr_cols <- c("var_type", "test_name", "contrasts_type")
  attr_cols <- table_body |>
    dplyr::select(any_of(base_attr_cols), starts_with("selector_")) |>
    names()

  # add attributes
  for (v in attr_cols) {
    df_attr <- table_body[c("variable", v)] |>
      unique() |>
      tidyr::drop_na()
    for (i in seq_len(nrow(df_attr))) {
      attr(data[[df_attr$variable[i]]], paste0("gtsummary.", v)) <- df_attr[[v]][i]
    }
  }

  # return data frame with attributes
  data
}

#' @rdname scoping_gtsummary
#' @export
scope_header <- function(table_body, header = NULL) {
  # if header not passed, return table_body unaltered
  if (is_empty(header)) return(table_body) # styler: off

  header <- header |>
    dplyr::select("column", "hide", starts_with("modify_selector_")) |>
    dplyr::rename_with(.fn = ~str_remove(.x, "^modify_selector_"), .cols = starts_with("modify_selector_"))

  # add information as attributes to table_body
  for (modify_selector_column in names(header)[-1]) {
    # cycle over the values where the modify selector column is not NA
    for (column in header$column[!is.na(header[[modify_selector_column]])]) {
        attr(table_body[[column]], paste0("gtsummary.", modify_selector_column)) <-
          header[header$column == column, modify_selector_column][[1]]
    }
  }

  table_body
}

#' Convert character vector to data frame
#'
#' This is used in some of the selecting we allow for, for example in
#' `as_gt(include=)` you can use tidyselect to select among the call
#' names to include.
#'
#' @param x character vector
#'
#' @return data frame
#' @keywords internal
vec_to_df <- function(x) {
  matrix(ncol = length(x), nrow = 0) |>
    data.frame() |>
    stats::setNames(x)
}


# converts a named list to a table_body format.
# the result of this fn will often be passed to `scope_table_body()`
#' Convert Named List to Table Body
#'
#' Many arguments in 'gtsummary' accept named lists. This function converts
#' a named list to the `.$table_body` format expected in `scope_table_body()`
#'
#' @param x named list
#' @param colname string of column name to assign. Default is `caller_arg(x)`
#'
#' @return `.$table_body` data frame
#' @keywords internal
#' @examples
#' type <- list(age = "continuous", response = "dichotomous")
#' gtsummary:::.list2tb(type, "var_type")
.list2tb <- function(x, colname = caller_arg(x)) {
  enframe(unlist(x), "variable", colname)
}
