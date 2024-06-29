#' Sort/filter by p-values
#'
#' @param x (`gtsummary`)\cr
#'   An object created using gtsummary functions
#' @param t (scalar `numeric`)\cr
#'   Threshold below which values will be retained. Default is 0.05.
#' @param q (scalar `logical`)\cr
#'   When `TRUE` will check the q-value column rather
#'   than the p-value. Default is `FALSE`.
#'
#' @author Karissa Whiting, Daniel D. Sjoberg
#' @name sort_filter_p
#'
#' @examples
#' # Example 1 ----------------------------------
#' trial %>%
#'   select(age, grade, response, trt) %>%
#'   tbl_summary(by = trt) %>%
#'   add_p() %>%
#'   filter_p(t = 0.8) %>%
#'   sort_p()
#'
#' # Example 2 ----------------------------------
#' glm(response ~ trt + grade, trial, family = binomial(link = "logit")) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   sort_p()
NULL


#' @export
#' @rdname sort_filter_p
sort_p <- function(x, q = FALSE) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(bold_p = match.call()))

  # checking inputs ------------------------------------------------------------
  check_class(x, "gtsummary")
  check_scalar_logical(q)

  # storing column name to bold
  col_name <- ifelse(q == FALSE, "p.value", "q.value")

  # checking input table has a p.value/q.value column
  if (!col_name %in% names(x$table_body)) {
    cli::cli_abort(
      "There is no column named {.val {col_name}} in {.code x$table_body}.",
      call = get_cli_abort_call()
    )
  }

  # sorting table by p.value or q.value
  x$table_body <-
    x$table_body |>
    dplyr::mutate(.by = "variable", ..gtsummary_sorting_column.. = .min_no_inf(.data[[col_name]])) |>
    dplyr::arrange(.data$..gtsummary_sorting_column..) |>
    dplyr::select(-"..gtsummary_sorting_column..")

  # returning results ----------------------------------------------------------
  x$call_list <- updated_call_list
  x
}

#' @export
#' @rdname sort_filter_p
filter_p <- function(x, q = FALSE, t = 0.05) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(bold_p = match.call()))

  # checking inputs ------------------------------------------------------------
  check_class(x, "gtsummary")
  check_scalar_logical(q)

  # storing column name to bold
  col_name <- ifelse(q == FALSE, "p.value", "q.value")

  # checking input table has a p.value/q.value column
  if (!col_name %in% names(x$table_body)) {
    cli::cli_abort(
      "There is no column named {.val {col_name}} in {.code x$table_body}.",
      call = get_cli_abort_call()
    )
  }

  # sorting table by p.value or q.value
  x$table_body <-
    x$table_body |>
    dplyr::mutate(.by = "variable", ..gtsummary_sorting_column.. = .min_no_inf(.data[[col_name]])) |>
    dplyr::filter(.data$..gtsummary_sorting_column.. <= .env$t) |>
    dplyr::select(-"..gtsummary_sorting_column..")

  # returning results ----------------------------------------------------------
  x$call_list <- updated_call_list
  x
}

.min_no_inf <- function(x) {
  if (all(is.na(x)) || length(x) == 0L) return(NA)
  min(x, na.rm = TRUE)
}
