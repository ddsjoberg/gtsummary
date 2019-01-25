#' Adds a column with overall summary statistics to an existing `fmt_table1` object where descriptive
#' statistics are split by a variable
#'
#' The function assumes the DEFAULT headers are in use. Only modify header rows
#' after Overall column has been added.
#'
#' @param x object with class `fmt_table1` from the \code{\link{fmt_table1}} function
#' @param last logical indicator to include overall  column last. Default is `FALSE`
#' @export
#' @examples
#' trial %>% fmt_table1(by = "trt") %>% add_overall()
add_overall <- function(x, last = FALSE) {
  # checking that input is class fmt_table1
  if (class(x) != "fmt_table1") stop("x must be class 'fmt_table1'")
  # checking that input x has a by var
  if (is.null(x$inputs[["by"]])) stop("Cannot add Overall column when no 'by' variable in original fmt_table1")

  x_copy <- x

  # removing by variable from data (so it won't show up in the overall fmt_table1)
  x_copy$inputs[["data"]] <- x_copy$inputs[["data"]] %>% dplyr::select(-dplyr::one_of(x_copy$inputs[["by"]]))

  # replacing the function call by variable to NULL to get results overall
  x_copy$inputs[["by"]] <- NULL

  # calculating stats overall, and adding header row
  header_n <- x$table1 %>%
    dplyr::filter_("startsWith(row_type, 'header')") %>%
    dplyr::pull("row_type") %>%
    length()
  if (header_n < 2) {
    stop(glue::glue(
      "Header must be at least two rows to accommodate Overall header. ",
      "Run 'add_overall' before any modifications are made to header rows."
    ))
  }

  overall <-
    do.call(fmt_table1, x_copy$inputs) %>%
    modify_header(
      stat_overall = fill_blanks(c("Overall", "N = {N}"), header_n),
      label = fill_blanks(c("Variable", " "), header_n)
    ) %>%
    purrr::pluck("table1")

  # checking the original fmt_table1 and the added overall, are the same before binding (excluding headers)
  if (!identical(
    x$table1 %>%
      dplyr::select(dplyr::one_of(c("row_type", ".variable", "label"))) %>%
      dplyr::filter_("!startsWith(row_type, 'header')"),
    overall %>%
      dplyr::select(dplyr::one_of(c("row_type", ".variable", "label"))) %>%
      dplyr::filter_("!startsWith(row_type, 'header')")
  )) {
    stop("An error occured in 'add_overall()'")
  }

  # adding overall stat to the table1 data frame
  if (last == FALSE) {
    x$table1 <-
      dplyr::bind_cols(
        overall,
        x$table1 %>% dplyr::select(-dplyr::one_of(c(".variable", "row_type", "label")))
      )
  }
  if (last == TRUE) {
    x$table1 <-
      dplyr::bind_cols(
        x$table1,
        overall %>% dplyr::select(dplyr::one_of("stat_overall"))
      )
  }

  # adding indicator to output that add_overall was run on this data
  x$call_list <- c(x$call_list, list(add_overall = match.call()))

  return(x)
}
