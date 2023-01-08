#' Split gtsummary table
#'
#' \lifecycle{experimental}
#' The `tbl_split` function splits a single gtsummary table into multiple tables.
#' Updates to the print method are expected.
#'
#' @param x gtsummary table
#' @param variables variables at which to split the gtsummary table rows (tables
#'  will be separated after each of these variables)
#' @param ... not used
#'
#' @return `tbl_split` object
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @family tbl_summary tools
#' @family tbl_survfit tools
#' @family tbl_svysummary tools
#'
#' @examples
#' tbl <-
#'   tbl_summary(trial) %>%
#'   tbl_split(variables = c(marker, grade))
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
tbl_split.gtsummary <- function(x, variables, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  # check/parse inputs ---------------------------------------------------------
  variables <-
    broom.helpers::.select_to_varnames(
      {{ variables }},
      var_info = x$table_body,
      arg_name = "variable"
    ) %>%
    # adding last variable
    union(x$table_body$variable %>% rev() %>% purrr::pluck(1))

  # merging split points -------------------------------------------------------
  # convert list of table_body into list of gtsummary objects
  x$table_body %>%
    dplyr::left_join(
      tibble(variable = variables, ..group.. = variables),
      by = "variable"
    ) %>%
    tidyr::fill("..group..", .direction = "up") %>%
    tidyr::nest(data = -"..group..") %>%
    dplyr::pull("data") %>%
    purrr::map(
      ~ list(.) %>%
        purrr::set_names("table_body") %>%
        c(purrr::list_modify(x, "table_body" = NULL)) %>% # add the other parts of the gtsummary table
        `class<-`(class(x)) # add original class from `x`
    ) %>%
    `class<-`("tbl_split") # assign class (can't assign gtsummary because of print definitions)
}

#' @export
#' @rdname tbl_split
print.tbl_split <- function(x, ...) {
  check_dots_empty(error = function(e) inform(c(e$message, e$body)))
  purrr::walk(x, print)
}
