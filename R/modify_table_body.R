#' Modify Table Body
#'
#' @description
#' \lifecycle{maturing}
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
#' @seealso `modify_table_styling()`
#' @seealso See \href{https://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary internals vignette}
#'
#' @examples
#' \donttest{
#' # Example 1 --------------------------------
#' # Add number of cases and controls to regression table
#' modify_table_body_ex1 <-
#'   trial %>%
#'   select(response, age, marker) %>%
#'   tbl_uvregression(
#'     y = response,
#'     method = glm,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE,
#'     hide_n = TRUE
#'   ) %>%
#'   # adding number of non-events to table
#'   modify_table_body(
#'     ~ .x %>%
#'       dplyr::mutate(N_nonevent = N_obs - N_event) %>%
#'       dplyr::relocate(c(N_event, N_nonevent), .before = estimate)
#'   ) %>%
#'   # assigning header labels
#'   modify_header(N_nonevent = "**Control N**", N_event = "**Case N**") %>%
#'   modify_fmt_fun(c(N_event, N_nonevent) ~ style_number)
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\out{
#' `r man_create_image_tag(file = "modify_table_body_ex1.png", width = "65")`
#' }}
#' @export
#' @family Advanced modifiers
modify_table_body <- function(x, fun, ...) {
  .assert_class(x, "gtsummary")
  updated_call_list <- c(x$call_list, list(modify_table_body = match.call()))

  # execute function on x$table_body -------------------------------------------
  x$table_body <-
    list(x$table_body) %>%
    map_dfr(fun, ...)

  # update table_styling -------------------------------------------------------
  x <- .update_table_styling(x)

  # return gtsummary object ----------------------------------------------------
  x$call_list <- updated_call_list
  x
}
