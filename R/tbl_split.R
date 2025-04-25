#' Split gtsummary table
#'
#' `r lifecycle::badge('experimental')`\cr
#' The `tbl_split` function splits a single gtsummary table into multiple tables.
#' Both horizontal (column-wise, (that is, splits by columns in
#' `x$table_body`) and vertical (row-wise) splits are possible.
#' Updates to the print method are expected.
#'
#' @param x (`gtsummary` or `list`)\cr
#'   gtsummary table.
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables at which to split the gtsummary table rows (tables
#'   will be separated after each of these variables).
#' @param keys ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to be repeated in each table split.
#' @param groups (list of `character` vectors)\cr
#'   list of column names that appear in `x$table_body`.
#'   Each group of column names represent.
#' @inheritParams rlang::args_dots_empty
#'
#' @return `tbl_split` object
#'
#' @details
#' Run `gtsummary::show_header_names()` to print all column names to split by.
#'
#' @examples
#' # create standard table
#' tbl <- tbl_summary(trial, by = trt)
#'
#' # split by rows
#' tbl_list_rows <- tbl |>
#'   tbl_split(variables = c(marker, grade))
#'
#' # print column names
#' gtsummary::show_header_names(tbl)
#'
#' # split by columns
#' tbl_list_cols <- tbl |>
#'   tbl_split(key = c(stat_1, stat_2))
#'
#' # take the list of splitted rows and split again by columns
#' tbl_list_rows_cols <- tbl_list_rows |>
#'   tbl_split(key = c(stat_1, stat_2))
#'
#' @name tbl_split
NULL

#' @export
#' @rdname tbl_split
tbl_split <- function(x, ...) {
  UseMethod("tbl_split")
}

#' @export
#' @rdname tbl_split
tbl_split.gtsummary <- function(x, keys, variables, groups, ...) {
  set_cli_abort_call()
  check_dots_empty()

  out_list <- NULL
  # split columns first (vertically) -------------------------------------------
  if (!missing(groups)) {

    # do the split
    out_list <- tbl_split_by_columns(
      x = x,
      keys = {{ keys }},
      groups = groups
    )
  }

  # split rows then ------------------------------------------------------------
  if (!missing(variables)) {

    # if already split, nest lists
    if (!is.null(out_list)) {
      out_list <- tbl_split(out_list, variables = {{ variables }})
    # otherwise split only horizontally
    } else {
      out_list <- tbl_split_by_rows(
        x = x,
        variables = {{ variables }}
      )
    }
  }

  out_list |>
    structure(class = c("tbl_split", "list"))
}

#' @export
#' @rdname tbl_split
tbl_split.list <- function(x, keys, variables, groups, ...) {
  set_cli_abort_call()
  check_dots_empty()

  out <- map(
    .x = x,
    .f = tbl_split,
    keys = {{ keys }},
    variables = {{ variables }},
    groups = groups
  )

  # return list of tbls --------------------------------------------------------
  out
}

#' @export
#' @rdname tbl_split
tbl_split_by_rows <- function(x, variables) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")

  # process inputs -------------------------------------------------------------
  cards::process_selectors(
    data = scope_table_body(x$table_body),
    variables = {{ variables }}
  )
  # adding last variable
  variables <- variables |> union(dplyr::last(x$table_body$variable))

  # merging split points -------------------------------------------------------
  # convert list of table_body into list of gtsummary objects
  x$table_body %>%
    dplyr::left_join(
      dplyr::tibble(variable = variables, ..group.. = variables),
      by = "variable"
    ) |>
    tidyr::fill("..group..", .direction = "up") |>
    tidyr::nest(data = -"..group..") |>
    dplyr::pull("data") |>
    map(
      ~ list(.) |>
        set_names("table_body") |>
        c(utils::modifyList(x, val = list(table_body = NULL))) %>% # add the other parts of the gtsummary table
        `class<-`(class(x)) # add original class from `x`
    ) %>%
    `class<-`(c("tbl_split", "list")) # assign class (can't assign gtsummary because of print definitions)
}

#' @export
#' @rdname tbl_split
print.tbl_split <- function(x, ...) {
  check_dots_empty()
  walk(x, print)
}

#' @export
#' @name tbl_split
tbl_split_by_columns <- function(x, keys, groups) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  cards::process_selectors(x$table_body, keys = {{ keys }})
  check_class(groups, "list")
  cards::check_list_elements(
    groups,
    predicate = is.character,
    error_msg = "Each element of the {.arg groups} argument's list must be a {.cls character} vector."
  )
  if (missing(keys)) {
    keys <- first_column(x)
  }

  # check all variables in groups appear in the table
  columns_do_not_exist <-
    unlist(groups) |>
    setdiff(x$table_styling$header$column)
  if (!is_empty(columns_do_not_exist)) {
    cli::cli_abort()
  }

  # check that every un-hidden column is present
  missing_cols <-
    x$table_styling$header[!x$table_styling$header$hide,][["column"]] |>
    setdiff(c(keys, unlist(groups)))
  if (!is_empty(missing_cols)) {
    cli::cli_inform(
      c("The following columns were not listed in either {.arg keys} or {.arg groups} argument: {.val {missing_cols}}",
        "i" = "These columns have been added to the end of {.arg groups}.",
        "*" = "Run {.fun gtsummary::show_header_names} for a list of all column names.")
    )
    groups <- c(groups, list(missing_cols))
  }

  # splitting table ------------------------------------------------------------
  result <- vector(mode = "list", length = length(groups))
  for (i in seq_along(groups)) {
    result[[i]] <- gtsummary::modify_column_hide(x, columns = -all_of(union(keys, groups[[i]])))
  }

  # return list of tbls --------------------------------------------------------
  result |>
    structure(class = c("tbl_split", "list"))
}

#' @export
#' @name tbl_split
first_column <- function(x) {
  set_cli_abort_call()
  check_class(x, "gtsummary")

  x$table_styling$header[!x$table_styling$header$hide,][["column"]][1]
}
