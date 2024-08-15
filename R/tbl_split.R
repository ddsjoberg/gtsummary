#' Split gtsummary table
#'
#' \lifecycle{experimental}\cr
#' The `tbl_split` function splits a single gtsummary table into multiple tables.
#' Updates to the print method are expected.
#'
#' @param x (`gtsummary`)\cr
#'   gtsummary table
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   variables at which to split the gtsummary table rows (tables
#'   will be separated after each of these variables)
#' @inheritParams rlang::args_dots_empty
#'
#' @return `tbl_split` object
#' @name tbl_split
#'
#' @examples
#' tbl <-
#'   tbl_summary(trial) |>
#'   tbl_split(variables = c(marker, grade))
NULL

#' @export
#' @rdname tbl_split
tbl_split <- function(x, ...) {
  UseMethod("tbl_split")
}

#' @export
#' @rdname tbl_split
tbl_split.gtsummary <- function(x, variables, ...) {
  set_cli_abort_call()
  check_dots_empty()

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
