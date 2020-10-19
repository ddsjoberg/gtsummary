#' Execute functions functions on table_body
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' Function is for advanced manipulation of gtsummary tables.
#' It allow users to modify the `.$table_body` data frame included
#' in each gtsummary object.
#'
#' If a new column is added to the table, default printing instructions will then
#' be added to `.$table_header`. By default, columns are hidden.
#' To show a column, add a column header with `modify_header()`.
#'
#' @param x gtsummary object
#' @param fun unquoted (bare) function name
#' @param ... arguments passed to `fun()` function. First argument of `fun()`
#' must be `x$table_body`
#'
#' @export
#' @seealso `modify_table_header()`
#' @seealso See \href{http://www.danieldsjoberg.com/gtsummary/articles/gtsummary_definition.html}{gtsummary internals vignette}
#'
#' @examples
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
#'   add_nevent() %>%
#'   # adding number of non-events to table
#'   modify_table_body(dplyr::mutate, n_nonevent = N - nevent) %>%
#'   modify_table_body(dplyr::relocate, n_nonevent, .before = nevent) %>%
#'   modify_header(n_nonevent = "**Control N**", nevent = "**Case N**")
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{modify_table_body_ex1.png}{options: width=65\%}}
#' @export
modify_table_body <- function(x, fun, ...) {
  if (!inherits(x, "gtsummary")) stop("`x=` must be class 'gtsummary'", call. = FALSE)

  # execute function on x$table_body
  x$table_body <- fun(x$table_body, ...)

  # update table_header
  x$table_header <- table_header_fill_missing(x$table_header, x$table_body)

  # return gtsummary object
  x
}




