#' Bold significant p-values
#'
#' Bold values below a chosen threshold (e.g. <0.05)
#' in a gtsummary tables.
#'
#' @param x (`gtsummary`)\cr
#'   Object created using gtsummary functions
#' @param t (scalar `numeric`)\cr
#'   Threshold below which values will be bold. Default is 0.05.
#' @param q (scalar `logical`)\cr
#'   When `TRUE` will bold the q-value column rather
#'   than the p-value. Default is `FALSE`.
#' @author Daniel D. Sjoberg, Esther Drill
#' @export
#' @examples
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_summary(by = trt, include = c(response, marker, trt), missing = "no") |>
#'   add_p() |>
#'   bold_p(t = 0.1)
#'
#' # Example 2 ----------------------------------
#' glm(response ~ trt + grade, trial, family = binomial(link = "logit")) |>
#'   tbl_regression(exponentiate = TRUE) |>
#'   bold_p(t = 0.65)
bold_p <- function(x, t = 0.05, q = FALSE) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(bold_p = match.call()))

  # checking inputs ------------------------------------------------------------
  check_class(x, "gtsummary")
  check_scalar_range(t, range = c(0, 1), include_bounds = c(TRUE, TRUE))
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

  # update table_styling -------------------------------------------------------
  # modifying table_styling with bold threshold
  x <-
    .modify_text_format(
      x,
      columns = col_name,
      rows = !!expr(!!sym(col_name) <= !!t),
      text_format = "bold"
    )

  # returning results ----------------------------------------------------------
  x$call_list <- updated_call_list
  x
}
