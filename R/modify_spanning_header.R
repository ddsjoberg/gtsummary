#' Update gtsummary table spanning header
#'
#' @param x a gtsummary object
#' @param update list of formulas or a single formula specifying the update.
#' The LHS selects the variables
#' whose spanning header will be updated, and the RHS is the new spanning header.
#' For example, `update = starts_with("stat_") ~ "New spanning header!"`.
#' Columns from `x$table_body` may be selected.
#' To remove all spanning headers, use `update = everything() ~ NA`.
#'
#'
#' @return gtsummary object
#' @export
#' @examples
#' # add header above summary statistics
#' spanning_header_ex1 <-
#'   trial %>%
#'   select(trt, age, grade) %>%
#'   tbl_summary(by = trt) %>%
#'   modify_spanning_header(starts_with("stat_") ~ "**Randomization Assignment**")
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{spanning_header_ex1.png}{options: width=70\%}}

modify_spanning_header <- function(x, update) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) {
    stop("Argument `x=` must be an object with 'gtsummary' class", call. = FALSE)
  }

  # converting update arg to a tidyselect list ---------------------------------
  update <- tidyselect_to_list(x$table_body, {{ update }}, arg_name = "update")

  # updating footnote ----------------------------------------------------------
  # convert named list to a tibble
  table_header_update <-
    update %>%
    unlist() %>%
    tibble::enframe(name = "column", value = "spanning_header") %>%
    # ensuring the column is a character
    mutate_at(vars(.data$spanning_header), as.character) %>%
    # performing inner join to put the edits in the same order as x$table_header
    {dplyr::inner_join(
      x$table_header %>% select(.data$column),
      .,
      by = "column"
    )}

  # updating table_header
  rows <- x$table_header$column %in% table_header_update$column
  x$table_header[rows, c("column", "spanning_header")] <-
    table_header_update

  # return updated gtsummary object --------------------------------------------
  x[["call_list"]] <- list(x[["call_list"]], add_p = match.call())
  x
}
