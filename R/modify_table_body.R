#' Modify Table Body
#'
#' @description
#' Function is for advanced manipulation of gtsummary tables.
#' It allow users to modify the `.$table_body` data frame included
#' in each gtsummary object.
#'
#' If a new column is added to the table, default printing instructions will then
#' be added to `.$table_styling`. By default, columns are hidden.
#' To show a column, add a column header with `modify_header()`.
#'
#' @param x gtsummary object
#' @param fun A function or formula. If a _function_, it is used as is.
#' If a _formula_, e.g. `fun = ~ .x %>% arrange(variable)`,
#' it is converted to a function. The argument passed to `fun=` is `x$table_body`.
#' @param ... Additional arguments passed on to the mapped function
#'
#' @export
#'
#' @examples
#' # Example 1 --------------------------------
#' # # Add number of cases and controls to regression table
#' # modify_table_body_ex1 <-
#' #  trial %>%
#' #  select(response, age, marker) %>%
#' #  tbl_uvregression(
#' #    y = response,
#' #    method = glm,
#' #    method.args = list(family = binomial),
#' #    exponentiate = TRUE,
#' #    hide_n = TRUE
#' #  ) %>%
#' #  # adding number of non-events to table
#' #  modify_table_body(
#' #    ~ .x %>%
#' #      dplyr::mutate(N_nonevent = N_obs - N_event) %>%
#' #      dplyr::relocate(c(N_event, N_nonevent), .before = estimate)
#' #  ) %>%
#' #  # assigning header labels
#' #  modify_header(N_nonevent = "**Control N**", N_event = "**Case N**") %>%
#' #  modify_fmt_fun(c(N_event, N_nonevent) ~ style_number)
#' @export
#' @family Advanced modifiers
modify_table_body <- function(x, fun, ...) {
  set_cli_abort_call()
  check_class(x, "gtsummary")
  updated_call_list <- c(x$call_list, list(modify_table_body = match.call()))

  # execute function on x$table_body -------------------------------------------
  x$table_body <- map(.x = list(x$table_body), .f = fun, ...)[[1]]

  # update table_styling -------------------------------------------------------
  x <- .update_table_styling(x)

  # return gtsummary object ----------------------------------------------------
  x$call_list <- updated_call_list
  x
}
