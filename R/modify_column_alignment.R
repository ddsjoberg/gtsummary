#' Modify Column Alignment
#'
#' \lifecycle{maturing}
#' Update column alignment/justification in a gtsummary table.
#'
#' @inheritParams modify_table_styling
#'
#' @family Advanced modifiers
#' @seealso Review [list, formula, and selector syntax][syntax] used throughout gtsummary
#' @export
#' @examples
#' \donttest{
#' # Example 1 ----------------------------------
#' tbl <-
#'   lm(age ~ marker + grade, trial) %>%
#'   tbl_regression() %>%
#'   modify_column_alignment(columns = everything(), align = "left")
#' }
modify_column_alignment <- function(x, columns, align = c("left", "right", "center")) {
  .assert_class(x, "gtsummary")
  updated_call_list <- c(x$call_list, list(modify_column_hide = match.call()))
  align <- match.arg(align)

  x <-
    modify_table_styling(
      x = x,
      columns = {{ columns }},
      align = align
    )

  x$call_list <- updated_call_list
  x
}
