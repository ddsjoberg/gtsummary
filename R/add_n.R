#' Adds a column with N (or N missing) for each variable
#'
#' The function assumes the DEFAULT header are in use. Only modify header rows
#' after all columns has been added.
#'
#' @param x object with class `tbl_summary` from the \code{\link{tbl_summary}} function
#' @param missing logical argument indicating whether to print N (`missing = FALSE`),
#' or N missing (`missing = TRUE`).  Default is `FALSE`
#' @param last logical indicator to include overall  column last. Default is `FALSE`
#' @family tbl_summary
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' tbl_n <- trial %>% tbl_summary(by = "trt") %>% add_n()
add_n <- function(x, missing = FALSE, last = FALSE) {
  # checking that input is class tbl_summary
  if (class(x) != "tbl_summary") stop("x must be class 'tbl_summary'")

  # counting non-missing N (or missing N)
  counts <-
    x$meta_data %>%
    select(c("variable")) %>%
    mutate(
      row_type = "label",
      n_var = map_chr(
        .data$variable,
        ~ case_when(
          missing == FALSE ~ sum(!is.na(x$inputs$data[[.x]])),
          missing == TRUE ~ sum(is.na(x$inputs$data[[.x]]))
        )
      )
    ) %>%
    set_names(c("variable", "row_type", ifelse(missing == FALSE, "n", "n_missing")))

  # merging result with existing tbl_summary
  if (last == FALSE) {
    table_body <-
      x$table_body %>%
      select(c("variable", "row_type", "label")) %>%
      left_join(counts, by = c("variable", "row_type")) %>%
      left_join(x$table_body, by = c("variable", "row_type", "label"))
  }
  else if (last == TRUE) {
    table_body <-
      x$table_body %>%
      left_join(counts, by = c("variable", "row_type"))
  }

  # column headers
  x[["gt_calls"]][[glue("cols_label:{ifelse(missing == FALSE, 'n', 'n_missing')}")]] <-
    glue(
      "cols_label({ifelse(missing == FALSE, 'n', 'n_missing')} = ",
      "md('**{ifelse(missing == FALSE, 'N', 'N Missing')}**'))"
    )

  # replacing old table_body with new
  x$table_body <- table_body

  # adding indicator to output that add_overall was run on this data
  x$call_list <- c(x$call_list, list(add_overall = match.call()))

  # returning tbl_summary object
  return(x)
}
