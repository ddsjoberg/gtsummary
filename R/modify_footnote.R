#' Update gtsummary table footnote
#'
#' @param x a gtsummary object
#' @param update list of formulas or a single formula specifying the footnote
#'  update. The LHS selects the variables
#' whose footnote will be updated, and the RHS is the new footnote. For example,
#' `update = stat_0 ~ "New footnote!"` or
#' `update = starts_with("stat_") ~ "New footnote!"`. To delete the footnote,
#' update the text to `NA`. Columns from `x$table_body` may be selected.
#' @param abbreviation Logical indicating if an abbreviation is being updated.
#' Abbreviation footnotes are handled differently. See examples below.
#'
#' @return gtsummary object
#' @family tbl_summary tools
#' @family tbl_regression tools
#' @family tbl_uvregression tools
#' @export
#' @examples
#' tbl_summary <-
#'   trial %>%
#'   select(trt, age, grade) %>%
#'   tbl_summary(by = trt)
#'
#' # update footnote
#' modify_footnote_ex1 <-
#'   tbl_summary %>%
#'   modify_footnote(
#'     update = starts_with("stat_") ~
#'       "median (IQR) for continuous variables; n (%) categorical variables"
#'   )
#'
#' # deleting all footnote
#' modify_footnote_ex2 <-
#'   tbl_summary %>%
#'   modify_footnote(update = everything() ~ NA)
#'
#' # updating the footnote abbreviation for CI
#' modify_footnote_ex3 <-
#'   glm(response ~ age + grade, trial, family = binomial) %>%
#'   tbl_regression(exponentiate = TRUE) %>%
#'   modify_footnote(ci ~ "CI = Credible Interval", abbreviation = TRUE)
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{modify_footnote_ex1.png}{options: width=60\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{modify_footnote_ex2.png}{options: width=55\%}}
#'
#' \if{html}{Example 3}
#'
#' \if{html}{\figure{modify_footnote_ex3.png}{options: width=45\%}}

modify_footnote <- function(x, update, abbreviation = FALSE) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Argument `x=` must be an object with 'gtsummary' class", call. = FALSE)
  }

  # converting update arg to a tidyselect list ---------------------------------
  update <- tidyselect_to_list(x$table_body, {{ update }}, arg_name = "update")

  # updating footnote ----------------------------------------------------------
  footnote_column_name <- ifelse(abbreviation == TRUE, "footnote_abbrev", "footnote")

  # convert named list to a tibble
  table_header_update <-
    update %>%
    unlist() %>%
    tibble::enframe(name = "column", value = footnote_column_name) %>%
    # ensuring the column is a character
    mutate_at(vars(any_of(footnote_column_name)), as.character) %>%
    # performing inner join to put the edits in the same order as x$table_header
    {dplyr::inner_join(
      x$table_header %>% select(.data$column),
      .,
      by = "column"
    )}

  # updating table_header
  rows <- x$table_header$column %in% table_header_update$column
  x$table_header[rows, c("column", footnote_column_name)] <-
    table_header_update

  # return updated gtsummary object --------------------------------------------
  x[["call_list"]] <- list(x[["call_list"]], add_p = match.call())
  x
}

