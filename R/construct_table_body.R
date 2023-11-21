#' Construct Table Body
#'
#' @param x gtsummary object
#'
#' @return gtsummary object
#' @name construct_gtsummary
#'
#' @examples
#' # TODO: Add example
NULL

#' @rdname construct_gtsummary
#' @export
construct_gtsummary <- function(x) {
  if (!inherits(x, "gtsummary"))
    cli::cli_abort("Object must be class {.cls gtsummary}.")
  UseMethod("construct_gtsummary")
}

#' @rdname construct_gtsummary
#' @export
construct_gtsummary.default <- function(x) {
  cli::cli_abort("There is no {.fun construct_table_body} method for object of class {.cls {class(x)}}.")
}

#' @rdname construct_gtsummary
#' @export
construct_gtsummary.tbl_summary <- function(x) {
  # build the table body pieces with bridge functions and stack them -----------
  x$table_body <-
    dplyr::left_join(
      dplyr::tibble(variable = x$calls$tbl_summary$include),
      dplyr::bind_rows(
        bridge_summary_continuous(
          x,
          variables = .get_variables_by_type(x$calls$tbl_summary$type, type = "continuous")
        ),
        bridge_summary_continuous2(
          x,
          variables = .get_variables_by_type(x$calls$tbl_summary$type, type = "continuous2")
        ),
        bridge_summary_categorical(
          x,
          variables = .get_variables_by_type(x$calls$tbl_summary$type, type = "categorical")
        ),
        bridge_summary_dichotomous(
          x,
          variables = .get_variables_by_type(x$calls$tbl_summary$type, type = "dichotomous"),
          value = x$calls$tbl_summary$value
        )
      ),
      by = "variable"
    )

  # construct default table_styling --------------------------------------------
  x <- construct_initial_table_styling(x)

  # update table_styling -------------------------------------------------------
  x <- x |>
    modify_header(
      label = "**Characteristic**",
      all_stat_cols() ~
        .ifelse1(is.null(x$calls$tbl_summary$by), "**N = {N}**", "**{level}**  \nN = {N}")
    )

  # return object with table_body included -------------------------------------
  x
}
