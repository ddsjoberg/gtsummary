#' ARD add overall column
#'
#' Adds a column with overall summary statistics to tables
#' created by `tbl_ard_summary()`.
#'
#' @param x (`tbl_ard_summary`)\cr
#'   A stratified 'gtsummary' table
#' @param cards (`card`)\cr
#'   An ARD object of class `"card"` typically created with `cards::ard_*()` functions.
#' @param last (scalar `logical`)\cr
#'   Logical indicator to display overall column last in table.
#'   Default is `FALSE`, which will display overall column first.
#' @param col_label (`string`)\cr
#'   String indicating the column label. Default is `"**Overall**"`
#' @param statistic ([`formula-list-selector`][syntax])\cr
#'   Override the statistic argument in initial `tbl_*` function
#'   call. Default is `NULL`.
#' @param digits ([`formula-list-selector`][syntax])\cr
#'   Override the digits argument in initial `tbl_*` function
#'   call. Default is `NULL`.
#' @inheritParams rlang::args_dots_empty
#'
#' @author Daniel D. Sjoberg
#' @name add_overall_ard
#' @return A `gtsummary` of same class as `x`
#' @examples
#' # Example 1 ----------------------------------
#' # build primary table
#' tbl <-
#'   cards::ard_stack(
#'     trial,
#'     .by = trt,
#'     cards::ard_continuous(variables = age),
#'     cards::ard_categorical(variables = grade),
#'     .missing = TRUE,
#'     .attributes = TRUE,
#'     .total_n = TRUE
#'   ) |>
#'   tbl_ard_summary(by = trt)
#'
#' # create ARD with overall results
#' ard_overall <-
#'   cards::ard_stack(
#'     trial,
#'     cards::ard_continuous(variables = age),
#'     cards::ard_categorical(variables = grade),
#'     .missing = TRUE,
#'     .attributes = TRUE,
#'     .total_n = TRUE
#'   )
#'
#' # add an overall column
#' tbl |>
#'   add_overall(cards = ard_overall)
NULL

#' @rdname add_overall_ard
#' @export
add_overall.tbl_ard_summary <- function(x,
                                        cards,
                                        last = FALSE,
                                        col_label = "**Overall**",
                                        statistic = NULL,
                                        digits = NULL, ...) {
  set_cli_abort_call()
  check_dots_empty()
  check_not_missing(cards)
  check_class(cards, "card")

  # translating the col_label, if nothing passed by user
  if (missing(col_label)) {
    paste0("**", translate_string("Overall"), "**")
  }

  # replace the original cards with the new passed cards object in the inputs.
  x$inputs$cards <- cards

  add_overall_generic(
    x = x,
    last = last,
    col_label = col_label,
    statistic = statistic,
    digits = digits,
    call = c(x$call_list, list(add_overall = match.call())),
    calling_fun = names(x$call_list)[1]
  )
}
